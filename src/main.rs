use anyhow::Context;
use askama::Template;
use axum::{
    extract::{Form, State},
    http::StatusCode,
    response::{Html, IntoResponse, Response},
    routing::{get, post},
    Router,
};
use regex::Regex;
use serde_json::Value;
use tokio::net::TcpListener;
use tower_http::services::ServeDir;
use tracing::info;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

#[derive(Clone)]
struct Backend {
    cards: Vec<Value>,
    banlist: Value,
    sets: Vec<Value>,
}

fn import_json(file: &str) -> anyhow::Result<Value> {
    Ok(serde_json::from_str::<Value>(&std::fs::read_to_string(
        file,
    )?)?)
}

fn init_backend() -> anyhow::Result<Backend> {
    Ok(Backend {
        cards: import_json("assets/cards.json")?
            .as_array()
            .unwrap()
            .to_vec(),
        banlist: import_json("assets/banned.json")?,
        sets: import_json("assets/sets.json")?
            .as_array()
            .unwrap()
            .to_vec(),
    })
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "cataloguer=debug".into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();

    info!("initializing router...");
    let backend = init_backend()?;
    let api_router = Router::new()
        .route("/search", post(search))
        .with_state(backend);
    let assets_path = std::env::current_dir().unwrap();
    let router = Router::new()
        .route("/", get(index))
        .route("/syntax", get(syntax))
        .nest("/api", api_router)
        .nest_service(
            "/assets",
            ServeDir::new(format!("{}/assets", assets_path.to_str().unwrap())),
        );
    let listener = TcpListener::bind("0.0.0.0:8000").await.unwrap();

    info!("router initialized, listening on localhost:8000");

    axum::serve(listener, router)
        .await
        .context("error while staring server")?;

    Ok(())
}

async fn index() -> impl IntoResponse {
    let template = IndexTemplate {};
    HtmlTemplate(template)
}

async fn syntax() -> impl IntoResponse {
    let template = SyntaxTemplate {};
    HtmlTemplate(template)
}

/// A wrapper type that we'll use to encapsulate HTML parsed by askama into valid HTML for axum to serve.
struct HtmlTemplate<T>(T);

/// Allows us to convert Askama HTML templates into valid HTML for axum to serve in the response.
impl<T> IntoResponse for HtmlTemplate<T>
where
    T: Template,
{
    fn into_response(self) -> Response {
        // Attempt to render the template with askama
        match self.0.render() {
            // If we're able to successfully parse and aggregate the template, serve it
            Ok(html) => Html(html).into_response(),
            // If we're not, return an error or some bit of fallback HTML
            Err(err) => (
                StatusCode::INTERNAL_SERVER_ERROR,
                format!("Failed to render template. Error: {}", err),
            )
                .into_response(),
        }
    }
}

#[derive(Template)]
#[template(path = "index.html")]
struct IndexTemplate;

#[derive(Template)]
#[template(path = "syntax.html")]
struct SyntaxTemplate;

#[derive(Template)]
#[template(path = "cards.html")]
struct CardsList {
    query: String,
    cards: Vec<String>,
}

#[derive(serde::Deserialize)]
struct SearchForm {
    search: String,
}

fn as_operator<T: std::cmp::PartialEq + std::cmp::PartialOrd + std::fmt::Debug>(
    operand: &str,
    c1: T,
    c2: T,
) -> bool {
    //println!("{:?} {} {:?}",c1, operand, c2);
    match operand {
        ":" => c1 == c2,
        "<" => c1 < c2,
        ">" => c1 > c2,
        "<=" => c1 <= c2,
        ">=" => c1 >= c2,
        "!=" => c1 != c2,
        _ => false,
    }
}

async fn search(
    State(backend): State<Backend>,
    Form(query): Form<SearchForm>,
) -> impl IntoResponse {
    let mut temp: Vec<String> = vec![];
    let results: Option<Vec<Value>> = search_cards(&query.search, &backend, backend.cards.clone());
    if results.is_some() {
        for card in results.unwrap() {
            let code: Option<String> = Some(
                card["code"]
                    .as_array().unwrap()
                    .last().unwrap()
                    .as_str().unwrap()
                    .to_owned(),
            );
            temp.push(code.unwrap());
        }
    }
    let response = CardsList { query:query.search, cards: temp };
    HtmlTemplate(response)
}

fn search_cards(query: &str, backend: &Backend, card_pool: Vec<Value>) -> Option<Vec<Value>> {
    let mut remaining: Vec<Value> = card_pool.clone();
    //currently ignoring regex
    let mut bracket_flag = false; //whether reading a bracketed set of info
    let mut or_flag = false; //whether last instruction was an or
    let mut resolving_bracket = false; // used to skip instructions on brackets
    let mut back_looking = card_pool.clone(); //used to resolve or
    let mut or_buffer: Vec<Value> = vec![]; //stores result of instruction before an or
    let mut buffer = "".to_owned(); //part is added into this, used for quotation marks and brackets
    let mut buffering = false; //needs to exist to allow quotation marks to function

    for part in query.split(" ") {
        let part = part.to_lowercase();
        // (!)-term-operator-value
        let part_results: Vec<Value>;
        let mut inverse = false;

        buffer.push_str(&part);
 
        //horrific operator logic for "", () and or
        if part.contains("\"") && !buffering {
            buffer = buffer.replace("\"", "");
            buffering = true;
        } else if part.contains("\"") && buffering {
            buffer = buffer.replace("\"", "");
            buffering = false;
        } 
        if part.contains("(") && !buffering && !bracket_flag  {
            bracket_flag = true;
            buffer = buffer.replace("(", "");
        } //get back to this
        if part.contains(")") && !buffering && bracket_flag {
            bracket_flag = false;
            buffer = buffer.replace(")", "");
            resolving_bracket = true;
        }
        if bracket_flag || buffering {
            buffer.push_str(" ");
            continue;
        }
        if part == "or" {
            or_buffer = remaining;
            remaining = back_looking.clone();
            or_flag = true;
            buffer = "".to_owned();
            continue;
        }
        if !or_flag {
            back_looking = remaining.clone();
        }
        let pre_query = remaining.clone(); //here for inverse, get rid of this later
        if buffer.starts_with('-') {
            println!("inverting statement");
            inverse = true;
            buffer = buffer.replace("-", "")
        } //the ! probably needs to be removed from buffer?
        
        //then find the operator with code that looks really bad, I'm yet to think of a smarter answer
        if !resolving_bracket {
        let operator = match &buffer {
            x if x.contains(":") => ":",
            x if x.contains("=") => ":",
            x if x.contains("<=") => "<=",
            x if x.contains("<") => "<",
            x if x.contains(">=") => ">=",
            x if x.contains(">") => ">",
            x if x.contains("!=") => "!=",
            _ => { //if none then must be name:
                buffer = "_:".to_owned() + &buffer;
                ":"
            }
        };

        let value = buffer.split(operator).nth(1)?;
        
        println!( "{} {} {}", buffer.split(operator).next()?, &operator, &value); //debug print
        part_results = match buffer.split(operator).next()? {
            "a" | "artist" => remaining.into_iter().filter(
                    |x| if x["illustrator"].is_array() {
                        x["illustrator"].as_array().unwrap().into_iter().find(|x| x.as_str().unwrap().to_lowercase().contains(value)).is_some()
                    } else {false}
                ).collect(), //show that printing?, currently no preference
            "adv"| "g" | "advancement" => {
                if value.parse::<u64>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                remaining.into_iter().filter(|x| as_operator(operator, x["advancement_cost"].as_u64(), value.parse::<u64>().ok())).collect()
            },
            "b" | "banned" => {
                if !backend.banlist[value].is_array() {continue;}
                remaining.into_iter().filter(|x| backend.banlist[value].as_array().unwrap().contains(&x["stripped_title"])).collect() //maybe shouldn't panic on invalid formats, otherwise fine
            },
            "c" | "cost" | "rez" => {
                if value.parse::<u64>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                remaining.into_iter().filter(|x| as_operator(operator, x["cost"].as_u64(), value.parse::<u64>().ok())).collect()
            },
            "cy" | "cyc" | "cycle" => {
                if value.parse::<u64>().is_err() {
                    if backend.sets.iter().find(|x| x["cycle"] == value).is_none() {vec!()} else {
                        let code = &backend.sets.iter().find(|x| x["cycle"] == value).unwrap()["start_num"].as_str().unwrap()[0..2];
                        remaining.into_iter().filter(|x| x["code"].as_array().unwrap().into_iter().find(|x| &x.as_str().unwrap()[0..2] == code      ).is_some()).collect()
                    }
                }
                else {
                    remaining.into_iter().filter(|x| x["code"].as_array().unwrap().into_iter().find(|x| &x.as_str().unwrap()[0..2] == value).is_some()).collect()
                } //clean this up a bit, shouldn't need 2 filter statements
            },
            "d" | "date" | "year" => { 
                let sets: Vec<&Value> = match value {
                    x if Regex::new(r"\d{2}/\d{2}/\d{2}").unwrap().is_match(x) => continue, //think aboutthis one
                    x if Regex::new(r"\d{4}").unwrap().is_match(x) => backend.sets.iter().filter(|x| &x["date"].as_str().unwrap().to_owned()[6..8]==&value.to_owned()[2..4]).collect(),
                    x if Regex::new(r"\w{2,4}").unwrap().is_match(x) => backend.sets.iter().filter(|x| x["code"].as_str().unwrap()==value || x["cycle"].as_str().unwrap()==value).collect(), //works
                    _ => continue,
                };
                if sets.len() == 0 {
                    return None//fails on years like 2025, or 2001, then currently shows no cards
                }
                println!("{:?}", sets);
                match operator {
                //if a cycle and equal just grab that cycle
                    x if sets.len() == 1 && x==":" => search_cards(&format!("set:{value}"),backend, remaining)?,
                    ":" => {
                        let start = sets.iter().fold("99999", |acc, x| if acc.parse::<u64>().ok() > x["start_num"].as_str().unwrap().parse::<u64>().ok() {x["start_num"].as_str().unwrap()} else {acc});
                        let end = sets.iter().fold("00000", |acc, x| if acc.parse::<u64>().ok() < x["end_num"].as_str().unwrap().parse::<u64>().ok() {x["end_num"].as_str().unwrap()} else {acc});
                        search_cards(&format!("nrdb<{end} nrdb>{start}"), backend, remaining)?
                    },
                    "!=" => {
                        let start = sets.iter().fold("99999", |acc, x| if acc.parse::<u64>().ok() > x["start_num"].as_str().unwrap().parse::<u64>().ok() {x["start_num"].as_str().unwrap()} else {acc});
                        let end = sets.iter().fold("00000", |acc, x| if acc.parse::<u64>().ok() < x["end_num"].as_str().unwrap().parse::<u64>().ok() {x["end_num"].as_str().unwrap()} else {acc});
                        search_cards(&format!("!(nrdb<{end} nrdb>{start})"), backend, remaining)?
                    },
                //otherwise grab the oldest or newest date depending on operator
                    "<=" | ">" => {
                        let cy_end = sets.into_iter().fold("00000", |acc, x| if acc.parse::<u64>().ok() < x["end_num"].as_str().unwrap().parse::<u64>().ok() {x["end_num"].as_str().unwrap()} else {acc});
                        search_cards(&format!("nrdb{operator}{cy_end}"),backend, remaining)?
                    },
                    ">=" | "<" => {
                        let cy_start = sets.into_iter().fold("99999", |acc, x| if acc.parse::<u64>().ok() > x["start_num"].as_str().unwrap()    .parse::<u64>().ok() {x["start_num"].as_str().unwrap()} else {acc});
                        search_cards(&format!("nrdb{operator}{cy_start}"),backend, remaining)?
                    },
                    _ => continue,
                }
            },
            "ep" => {
                if value.parse::<u64>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                remaining.into_iter().filter(|x| as_operator(operator, x["eternal_points"].as_u64(), value.parse::<u64>().ok())).collect()
        },
            "f" | "faction" => {
                let temp = match value {
                    "a" => "anarch",
                    "c" | "crim" => "criminal",
                    "s" => "shaper",
                    "h" | "hb" => "haas-bioroid",
                    "j" | "jin" => "jinteki",
                    "n" => "nbn",
                    "w" | "wey" | "weyland" => "weyland-consortium",
                    _ => value
                }; //probably could be smarter but this is simple
                if temp == "neutral" {search_cards("f:neutral-runner or f:neutral-corp", backend, remaining)?} else {
                    remaining.into_iter().filter(|x| x["faction_code"].as_str() == Some(temp)).collect()
                }},
            "fmt" | "format" | "z" => match value {
                "startup" => search_cards("cy:lib or cy:sg or cy:su21 -banned:startup", backend, remaining)?,
                "neo" => search_cards("is:nsg -banned:neo", backend, remaining)?,
                "standard" => search_cards("cy:kit or cy:rs or is:nsg or cy:mor -banned:standard", backend, remaining)?,
                "sunset" => search_cards("cy:kit or cy:rs or is:nsg or cy:mor -banned:sunset", backend, remaining)?,
                "eternal" => search_cards("-banned:eternal", backend, remaining)?,
                _ => vec!(),
            },
            "ft" | "flavor" | "flavour" => remaining.into_iter().filter(
                |x| if x["flavor"].is_array() {
                    x["flavor"].as_array().unwrap().into_iter().find(|x| x.as_str().unwrap().to_lowercase().contains(value)).is_some()
                } else {false}
            ).collect(),
            "i" | "inf" | "influence" => {
                if value.parse::<u64>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                remaining.into_iter().filter(|x| as_operator(operator, x["faction_cost"].as_u64(), value.parse::<u64>().ok())).collect()
            },
            "inf_lim" | "il" => {
                if value.parse::<u64>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                remaining.into_iter().filter(|x| as_operator(operator, x["influence_limit"].as_u64(), value.parse::<u64>().ok())).collect()
            }, //currently nulls can be found with il<0 t:identity
            "is" => match value {
                "advanceable" => search_cards("o:\"you can advance this\"", backend, remaining)?,
                "corp" => search_cards("f:neutral-corp or f:n or f:j or f:h or f:w", backend, remaining)?,
                "dfc" => search_cards("hoshiko or (sync ev) or (jinteki biotech)", backend, remaining)?,
                "ffg" => search_cards("nrdb<24002", backend, remaining)?,
                "guest" => search_cards("ft:\"Designed by\" -pavilion", backend, remaining)?,
                "nsg" => search_cards("nrdb>26000 (-cy:mor -cy:sm) or (sansan city grid) or (subliminal messaging)", backend, remaining)?,
                "nearprinted" => remaining.into_iter().filter(|x| x["nearprint"].as_str().is_some()).collect(),
                "reprint" => remaining.into_iter().filter(|x| x["code"].as_array().unwrap().len() > 1).collect(),
                "runner" => search_cards("f:anarch or f:shaper or f:criminal or f:adam or f:sunny-lebeau or f:apex or f:neutral-runner", backend, remaining)?,
                "trap" => search_cards("o:\"when the runner accesses this\"", backend, remaining)?,
                "unique" => remaining.into_iter().filter(|x| if x["uniqueness"].is_boolean() {x["uniqueness"].as_bool().unwrap()} else {false}).collect(),
                _ => vec!()
            },
            "l" | "link" => {
                if value.parse::<u64>().is_err() {println!("{} was an invalid search term", buffer); continue;}    
                remaining.into_iter().filter(|x| as_operator(operator, x["base_link"].as_u64(), value.parse::<u64>().ok())).collect()
            },
            "m" | "mem" | "memory" => { 
                if value.parse::<u64>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                remaining.into_iter().filter(|x| as_operator(operator, x["memory_cost"].as_u64(), value.parse::<u64>().ok())).collect()
            },
            "md" | "min_deck" => {
                if value.parse::<u64>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                remaining.into_iter().filter(|x| as_operator(operator, x["minimum_deck_size"].as_u64(), value.parse::<u64>().ok())).collect()
            },
            //"n" | "number" =>
            //"new" =>
            "not" => search_cards(&("-is:".to_owned() + value), backend, remaining)?,
            "nrdb" => { //mutates remaining so that cards that have reprints don't get double
                        //counted with multiple nrdb terms (eg. su21 cards tend to be both sides of
                        //n < 20003 n > 20003 )
                let mut temp: Vec<Value> = vec!();
                for mut x in remaining {
                    x["code"] = serde_json::json!(
                        x["code"].as_array().unwrap().into_iter()
                        .filter(|y| as_operator(operator,
                                y.as_str().unwrap().parse::<u64>().ok(),
                                value.parse::<u64>().ok())
                    ).collect::<Vec<&Value>>());
                temp.push(x);
                };
                temp.into_iter()
                .filter(|x| x["code"].as_array().unwrap().len() > 0)
                .collect()
            },
            // "n" | "number" => 
            "o" | "x" | "text" | "oracle" => remaining.into_iter().filter(|x| 
                    if x["stripped_text"].is_string() {x["stripped_text"].as_str().unwrap().to_lowercase().contains(&value)} else {false}
                ).collect(),
            //"order" =>
            "p" | "v" | "points" => {
                if value.parse::<u64>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                remaining.into_iter().filter(|x| as_operator(operator, x["agenda_points"].as_u64(), value.parse::<u64>().ok())).collect()
            },
            //"pro" | "pronouns" =>, //needs some cards.json edits
            "s" | "sub" | "subtype" => remaining.into_iter().filter(|x| x["keywords"].as_str().unwrap().to_lowercase().contains(&value)).collect(),
            "set" | "e" | "edition" => {
                let set = backend.sets.iter().find(|x| x["code"] == value)?;
                let start = set["start_num"].as_str().unwrap();
                let end = set["end_num"].as_str().unwrap();
                search_cards(&format!("nrdb<={end} nrdb>={start}"), backend, remaining)?
            },
            //"st" =>
            "str" | "strength" => {
                if value.parse::<u64>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                remaining.into_iter().filter(|x| as_operator(operator, x["strength"].as_u64(), value.parse::<u64>().ok())).collect()
            },
            "t" | "type" => remaining.into_iter().filter(|x| x["type_code"].as_str().unwrap() == value).collect(),
            "trash" | "bin" | "h" => {
                if value.parse::<u64>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                remaining.into_iter().filter(|x| as_operator(operator, x["trash_cost"].as_u64(), value.parse::<u64>().ok())).collect()
            },
            "_" | "name" => remaining.into_iter().filter(|x: &Value| {x["stripped_title"].as_str().unwrap().to_lowercase().contains(&part)}).collect(),
            _ => vec!(), //skip if invalid mode
        };
        } else {
            println!("resolving a bracket of {}", buffer);
            part_results = search_cards(&buffer, backend, remaining)?;
            resolving_bracket = false;
        }
        remaining = if inverse {
            pre_query
                .clone()
                .into_iter()
                .filter(|x| !part_results.contains(x))
                .collect()
        } else {
            part_results
        };
        //println!("{:?}", &remaining);
        buffer = "".to_owned();
        if or_flag {
            let to_add: Vec<Value> = or_buffer
                .clone()
                .into_iter()
                .filter(|x| !remaining.contains(x))
                .collect();
            remaining.extend(to_add);
        }
    }
    println!("finished search"); //another debug print line
    Some(remaining)
}

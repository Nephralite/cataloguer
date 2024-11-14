use anyhow::Context;
use askama::Template;
use axum::{
    extract::{Query, State, Path}, 
    http::StatusCode, 
    response::{Html, IntoResponse, Response}, 
    routing::get,
    Router
};
use regex::Regex;
use serde_json::{Map, Value, json};
use std::{env, collections::HashMap};
use rand::thread_rng;
use rand::seq::SliceRandom;
use tokio::net::TcpListener;
use tower_http::services::ServeDir;
use tracing::info;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

#[derive(Clone)]
struct Backend {
    cards: Vec<Card>,
    banlist: Map<String, Value>,
    sets: Vec<Value>,
}

fn init_backend() -> anyhow::Result<Backend> {
    Ok(Backend {
        cards: serde_json::from_str::<Vec<Card>>(&std::fs::read_to_string("assets/cards.json")?)?,
        banlist: serde_json::from_str::<Map<String, Value>>(&std::fs::read_to_string("assets/banned.json")?)?,
        sets: serde_json::from_str::<Vec<Value>>(&std::fs::read_to_string("assets/sets.json")?)?,
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
    let router = Router::new()
        .route("/", get(search))
        .route("/cards/:id/:printing", get(cardpage))
        .route("/syntax", get(syntax))
        .with_state(backend)
        .nest_service(
            "/assets",
            ServeDir::new(format!("{}/assets", env::current_dir().unwrap().to_str().unwrap())),
        );
    let port = match env::var("PORT"){
        Ok(port) => port,
        Err(_) => "8080".to_owned()
    };
    let listener = TcpListener::bind(format!("0.0.0.0:{port}")).await.unwrap();

    info!("router initialized, listening on localhost:{port}");

    axum::serve(listener, router)
        .await
        .context("error while starting server")?;

    Ok(())
}

async fn syntax() -> impl IntoResponse {
    Templates::SyntaxTemplate(SyntaxTemplate {query:"".to_owned()})
}

#[derive(Template)]
#[template(path = "index.html")]
struct IndexTemplate {query: String}

#[derive(Template)]
#[template(path = "syntax.html")]
struct SyntaxTemplate {query: String}

#[derive(Template)]
#[template(path = "cards.html")]
struct CardsList {
    query: String,
    cards: Vec<String>,
}

#[derive(Template)]
#[template(path = "cardpage.html")]
struct CardPageTemplate {
    query: String,
    card: Card,
    x: usize,
    legality: Legality,
}



struct Legality {
    startup: &'static str,
    standard: &'static str,
    eternal: String,
}

#[derive(serde::Deserialize, Debug)]
struct SearchForm {
    search: Option<String>,
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

enum Templates {
    CardsList(CardsList),
    SyntaxTemplate(SyntaxTemplate),
    IndexTemplate(IndexTemplate),
    CardPageTemplate(CardPageTemplate)
}

impl IntoResponse for Templates {
    fn into_response(self) -> Response {
        // Attempt to render the template with askama
        let inner = match self {
            Templates::CardsList(c) => c.render(),
            Templates::IndexTemplate(c) => c.render(),
            Templates::SyntaxTemplate(c) => c.render(),
            Templates::CardPageTemplate(c) => c.render(),
        }; 
        match inner {
       // If we're able to successfully parse and aggregate the template, serve it
        Ok(html) => Html(html).into_response(),
       // If we're not, return an error or some bit of fallback HTML
        Err(err) => (
            StatusCode::INTERNAL_SERVER_ERROR,
            format!("Failed to render template. Error: {}", err),
        ).into_response(),
        }
    }
}

async fn cardpage(Path(params): Path<HashMap<String, String>>, State(backend): State<Backend>) -> impl IntoResponse {
    println!("testing that we get here");
    let printing = params.get("printing").unwrap().parse().unwrap();
    let id = params.get("id").unwrap();
    println!("{}, {}", id, printing);
    let card = backend.cards.iter().find(|x| x.stripped_title.to_lowercase() == *id).unwrap();
    
    let startup = if search_cards("z:startup", &backend, vec!(card.clone())) == Some(vec!(card.clone())) {"legal"} else if search_cards("banned:startup", &backend, vec!(card.clone())) == Some(vec!(card.clone())) {"banned"} else {"not legal"};
    let standard= if search_cards("z:standard", &backend, vec!(card.clone())) == Some(vec!(card.clone())) {"legal"} else if search_cards("banned:standard", &backend, vec!(card.clone())) == Some(vec!(card.clone())) {"banned"} else {"not legal"};
    let eternal = if search_cards("ep>0", &backend, vec!(card.clone())) == Some(vec!(card.clone())) {format!("{} Points", card.eternal_points.unwrap())} else if search_cards("z:eternal", &backend, vec!(card.clone())) == Some(vec!(card.clone())) {"legal".to_owned()} else if search_cards("banned:eternal", &backend, vec!(card.clone())) == Some(vec!(card.clone())) {"banned".to_owned()} else {"not legal".to_owned()};
    
    Templates::CardPageTemplate(CardPageTemplate{ query:"".to_owned(), card: card.clone(), x: printing, legality: Legality {startup, standard, eternal}})
}

async fn search(
    State(backend): State<Backend>,
    Query(params): Query<SearchForm>,
) -> impl IntoResponse {
    if let Some(query) = params.search {
        let mut temp: Vec<String> = vec![];
        let results: Option<Vec<Card>> = search_cards(&query, &backend, backend.cards.clone());
        if results.is_some() {
            if query.contains("prefer:oldest") {
                for card in results.unwrap() {
                    temp.push(card.printings.first().unwrap().code.clone());
                }
            } else { //prefer:newest is just the default
                for card in results.unwrap() {
                    temp.push(card.printings.last().unwrap().code.clone());
            
                }
            }
        }
        Templates::CardsList(CardsList { query, cards: temp })
    } else {
        Templates::IndexTemplate(IndexTemplate {query:"".to_owned()})
    }
}


#[derive(serde::Deserialize, Clone)]
struct Printing {
    artist: Option<String>,
    flavour: Option<String>, 
    code: String,
    img_type: String,
    set: String,
}

#[derive(serde::Deserialize, Clone)]
struct Card {
    printings: Vec<Printing>,
    faction: String,
    type_code: String,
    subtypes: Option<String>,
    title: String,
    stripped_title: String,
    text: Option<String>,
    stripped_text: Option<String>,
    uniqueness: bool,
    influence: Option<u8>,
    influence_limit: Option<u8>,
    minimum_deck_size: Option<u8>,
    strength: Option<u8>,
    base_link: Option<u8>,
    cost: Option<u8>,
    trash_cost: Option<u8>,
    memory_cost: Option<u8>,
    advancement_cost: Option<u8>,
    agenda_points: Option<u8>,
    eternal_points: Option<u8>,
    nearprint: Option<String>,
}

impl PartialEq for Card {
    fn eq(&self, other: &Card) -> bool {
        self.title == other.title
    }

    fn ne(&self, other: &Card) -> bool {
        self.title != other.title
    }
}

fn faction_order(faction:&str) -> usize {
    let order = ["anarch", "criminal", "shaper", "neutral-runner", "apex", "adam", "sunny-lebeau", "haas-bioroid", "jinteki", "nbn", "weyland-consortium", "neutral-corp"];
    let answer = order.iter().position(|&r| r == faction);
    if answer.is_some() {answer.unwrap()} else {255}
}

fn search_cards(query: &str, backend: &Backend, card_pool: Vec<Card>) -> Option<Vec<Card>> {
    let mut remaining: Vec<Card> = card_pool.clone();
    //currently ignoring regex
    let mut bracket_flag = false; //whether reading a bracketed set of info
    let mut or_flag = false; //whether last instruction was an or
    let mut resolving_bracket = false; // used to skip instructions on brackets
    let mut back_looking = card_pool.clone(); //used to resolve or
    let mut or_buffer: Vec<Card> = vec![]; //stores result of instruction before an or
    let mut buffer = "".to_owned(); //part is added into this, used for quotation marks and brackets
    let mut buffering = false; //needs to exist to allow quotation marks to function
    let mut order = "released".to_owned();
    let mut order_dir = "asc".to_owned();

        
    for part in query.split(" ") {
        let part = part.to_lowercase();
        // (!)-term-operator-value
        let part_results: Vec<Card>;
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
        }
        
        //then find the operator with code that looks really bad, I'm yet to think of a smarter answer
        if !resolving_bracket {
        let operator = match &buffer {
            x if x.contains(":") => ":",
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
                |x| x.printings.iter().find(
                    |y| if y.artist.is_some() {
                        y.artist.clone().unwrap().to_lowercase().contains(value)
                    } else {false}
                ).is_some()).collect(), //show that printing?, currently no preference
            "adv"| "g" | "advancement" => {
                if value.parse::<u64>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                remaining.into_iter().filter(|x| as_operator(operator, x.advancement_cost, value.parse::<u8>().ok())).collect()
            },
            "b" | "banned" => {
                if !backend.banlist[value].is_array() {continue;}
                remaining.into_iter().filter(|x| backend.banlist[value].as_array().unwrap().contains(&json!(x.stripped_title))).collect() //maybe shouldn't panic on invalid formats, otherwise fine
            },
            "c" | "cost" | "rez" => {
                if value.parse::<u8>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                remaining.into_iter().filter(|x| as_operator(operator, x.cost, value.parse::<u8>().ok())).collect()
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
            "dir" | "direction" => {order_dir=value.to_owned(); remaining}
            "ep" => {
                if value.parse::<u8>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                remaining.into_iter().filter(|x| as_operator(operator, x.eternal_points, value.parse::<u8>().ok())).collect()
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
                    remaining.into_iter().filter(|x| x.faction == temp).collect()
                }},
            "fmt" | "format" | "z" | "legal" => match value {
                "startup" => search_cards("(cy:lib or cy:sg or cy:su21) -banned:startup -o:\"starter game only\"", backend, remaining)?,
                "neo" => search_cards("is:nsg -banned:neo -o:\"starter game only\"", backend, remaining)?,
                "rig" | "postgateway" | "librealis" | "twocycle" => search_cards("date>=sg -banned:rig -o:\"starter game only\"", backend, remaining)?,
                "standard" => search_cards("(cy:kit or cy:rs or is:nsg or cy:mor or set:rar) -banned:standard -o:\"starter game only\"", backend, remaining)?,
                "sunset" => search_cards("(cy:kit or cy:rs or is:nsg or cy:mor) -banned:sunset -o:\"starter game only\"", backend, remaining)?,
                "eternal" => search_cards("-banned:eternal -o:\"starter game only\" -set:tdc -cy:00 -cy:24", backend, remaining)?,
                _ => vec!(),
            },
            "ft" | "flavor" | "flavour" => remaining.into_iter().filter(
                |x| x.printings.iter().find(|y| if y.flavour.is_some() {y.flavour.clone().unwrap().to_lowercase().contains(value)} else {false}).is_some()).collect(),
            "i" | "inf" | "influence" => {
                if value.parse::<u8>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                remaining.into_iter().filter(|x| as_operator(operator, x.influence, value.parse::<u8>().ok())).collect()
            },
            "inf_lim" | "il" => {
                if value == "null" || value == "inf" { return search_cards("il<0 t:identity", backend, remaining);}
                if value.parse::<u8>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                remaining.into_iter().filter(|x| as_operator(operator, x.influence_limit, value.parse::<u8>().ok())).collect()
            },
            "is" => match value {
                "advanceable" => search_cards("o:\"you can advance this\" or o:\"can be advanced\" -o:\"that can be advanced\"", backend, remaining)?,
                "corp" => search_cards("f:neutral-corp or f:n or f:j or f:h or f:w", backend, remaining)?,
                "dfc" => search_cards("hoshiko or (sync ev) or (jinteki biotech)", backend, remaining)?,
                "ffg" => search_cards("nrdb<24002", backend, remaining)?,
                "guest" => search_cards("ft:\"Designed by\" -pavilion", backend, remaining)?,
                "nsg" => search_cards("nrdb>26000 (-cy:mor -cy:sm) or (sansan city grid) or (subliminal messaging)", backend, remaining)?,
                "nearprinted" => remaining.into_iter().filter(|x| x.nearprint.is_some()).collect(),
                "reprint" => remaining.into_iter().filter(|x| x.printings.len() > 1).collect(), //needs to change
                "runner" => search_cards("f:anarch or f:shaper or f:criminal or f:adam or f:sunny-lebeau or f:apex or f:neutral-runner", backend, remaining)?,
                "trap" => search_cards("o:\"when the runner accesses this\"", backend, remaining)?,
                "unique" => remaining.into_iter().filter(|x| x.uniqueness).collect(),
                _ => vec!()
            },
            "l" | "link" => {
                if value.parse::<u8>().is_err() {println!("{} was an invalid search term", buffer); continue;}    
                remaining.into_iter().filter(|x| as_operator(operator, x.base_link, value.parse::<u8>().ok())).collect()
            },
            "m" | "mem" | "memory" => { 
                if value.parse::<u8>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                remaining.into_iter().filter(|x| as_operator(operator, x.memory_cost, value.parse::<u8>().ok())).collect()
            },
            "md" | "min_deck" => {
                if value.parse::<u8>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                remaining.into_iter().filter(|x| as_operator(operator, x.minimum_deck_size, value.parse::<u8>().ok())).collect()
            },
            //"n" | "number" =>
            //"new" =>
            "not" => search_cards(&("-is:".to_owned() + value), backend, remaining)?,
            "nrdb" => { //mutates remaining so that cards that have reprints don't get double
                        //counted with multiple nrdb terms (eg. su21 cards tend to be both sides of
                        //n < 20003 n > 20003 )
                let mut temp: Vec<Card> = vec!();
                for mut x in remaining {
                    x.printings = x.printings.into_iter().filter(|y| as_operator(operator,
                                y.code.parse::<u64>().ok(), value.parse::<u64>().ok())
                    ).collect::<Vec<Printing>>();
                temp.push(x);
                };
                temp.into_iter()
                .filter(|x| x.printings.len() > 0)
                .collect()
            },
            // "n" | "number" => 
            "o" | "x" | "text" | "oracle" => remaining.into_iter().filter(|x| 
                    if x.text.is_some() {x.stripped_text.clone().unwrap().to_lowercase().contains(&value)} else {false}
                ).collect(),
            "order" => {order = value.to_owned(); remaining}
            "p" | "v" | "points" => {
                if value.parse::<u8>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                remaining.into_iter().filter(|x| as_operator(operator, x.agenda_points, value.parse::<u8>().ok())).collect()
            },
            //"pro" | "pronouns" =>, //needs some cards.json edits
            "s" | "sub" | "subtype" => remaining.into_iter().filter(|x| if x.subtypes.is_some() {x.subtypes.clone().unwrap().to_lowercase().contains(&value)} else {false}).collect(),
            "set" | "e" | "edition" | "cycle" | "cyc" | "cy" => {
                let set = backend.sets.iter().find(|x| x["code"] == value)?;
                let start = set["start_num"].as_str().unwrap();
                let end = set["end_num"].as_str().unwrap();
                search_cards(&format!("nrdb<={end} nrdb>={start}"), backend, remaining)?
            },
            //"st" =>
            "str" | "strength" => {
                if value.parse::<u8>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                remaining.into_iter().filter(|x| as_operator(operator, x.strength, value.parse::<u8>().ok())).collect()
            },
            "t" | "type" => remaining.into_iter().filter(|x| x.type_code.contains(value)).collect(),
            "trash" | "bin" | "h" => {
                if value.parse::<u64>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                remaining.into_iter().filter(|x| as_operator(operator, x.trash_cost, value.parse::<u8>().ok())).collect()
            },
            "_" | "name" => remaining.into_iter().filter(|x: &Card| {x.stripped_title.to_lowercase().contains(&part)}).collect(),
            _ => remaining, //skip if invalid mode
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
            let to_add: Vec<Card> = or_buffer
                .clone()
                .into_iter()
                .filter(|x| !remaining.contains(x))
                .collect();
            remaining.extend(to_add);
        }
    }
    remaining.sort_by_key(|card| card.title.clone()); //break alphabetical in ties for now
    match order.as_str() {
            "artist" => remaining.sort_by_key(|card| card.printings.last()?.artist.clone()),
            "cost" => remaining.sort_by_key(|card| card.cost),
            "faction" => remaining.sort_by_key(|card| faction_order(&card.faction)),
            "released" => remaining.sort_by_key(|card| card.printings.first().unwrap().code.clone()),
            "random" => remaining.shuffle(&mut thread_rng()),
            //"set" => ,
            "strength" => remaining.sort_by_key(|card| card.strength),
            "type" => remaining.sort_by_key(|card| card.type_code.clone()),
            _ => {},
    }
    if order_dir == "desc" {remaining.reverse()}
    println!("finished search"); //another debug print line
    Some(remaining)
}

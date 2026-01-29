use std::collections::HashSet;
use std::fmt::Debug;
use serde_json::{json, Map, Value};
use rand::thread_rng;
use rand::seq::SliceRandom;
use regex::Regex;
use thiserror::Error;
use crate::structs::{Card, Backend, Set, Printing};
use crate::parse::{IsFilterType, NumericKey, ParseError, PrintingPreference, QueryNode, SearchDirection, SearchOrder, SearchSettings, TextKey, TextValue, parse_query};

fn as_operator<T: PartialEq + PartialOrd + Debug>(
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


pub fn faction_order(faction:&str) -> usize {
    let order = ["anarch", "criminal", "shaper", "neutral-runner", "apex", "adam", "sunny-lebeau", "haas-bioroid", "jinteki", "nbn", "weyland-consortium", "neutral-corp"];
    let answer = order.iter().position(|&r| r == faction);
    if answer.is_some() {answer.unwrap()} else {255}
}

// Struct used only inside the search.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct SearchPrinting<'a> {
    printing: &'a Printing,
    card: &'a Card,
}

// TODO: move everything out of here and into do_search
pub fn search_cards(query: &str, backend: &Backend, card_pool: Vec<Card>) -> Option<Vec<Card>> {
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
                "agenda" => search_cards(&format!("adv:{} p:{}", value.split("/").next()?, value.split("/").last()?), backend, remaining)?,
                "adv"| "g" | "advancement" => {
                    if value.parse::<u64>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                    remaining.into_iter().filter(|x| as_operator(operator, x.advancement_cost, value.parse::<u8>().ok())).collect()
                },
                "b" | "banned" => {
                    if !backend.banlist[value].is_array() {continue;}
                    remaining.into_iter().filter(|x| backend.banlist[value].as_array().unwrap().contains(&json!(x.stripped_title))).collect() //maybe shouldn't panic on invalid formats, otherwise fine
                },
                "c" | "cost" | "rez" => {
                    if value == "x" {return search_cards("c<0 -t:id -t:agenda", backend, remaining);}
                    if value.parse::<u8>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                    remaining.into_iter().filter(|x| as_operator(operator, x.cost, value.parse::<u8>().ok())).collect()
                },
                "d" | "date" | "year" => {
                    let sets: Vec<&Set> = match value {
                        x if Regex::new(r"\d{2}/\d{2}/\d{2}").unwrap().is_match(x) => continue, //think aboutthis one
                        x if Regex::new(r"\d{4}").unwrap().is_match(x) => backend.sets.iter().filter(|x| &x.date[6..8]==&value.to_owned()[2..4]).collect(),
                        x if Regex::new(r"\w{2,4}").unwrap().is_match(x) => backend.sets.iter().filter(|x| &x.code==value).collect(), //works
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
                            let start = sets.iter().fold("99999", |acc, x| if acc.parse::<u64>().ok() > x.start_num.parse::<u64>().ok() {&x.start_num} else {acc});
                            let end = sets.iter().fold("00000", |acc, x| if acc.parse::<u64>().ok() < x.end_num.parse::<u64>().ok() {&x.end_num} else {acc});
                            search_cards(&format!("nrdb<{end} nrdb>{start}"), backend, remaining)?
                        },
                        "!=" => {
                            let start = sets.iter().fold("99999", |acc, x| if acc.parse::<u64>().ok() > x.start_num.parse::<u64>().ok() {&x.start_num} else {acc});
                            let end = sets.iter().fold("00000", |acc, x| if acc.parse::<u64>().ok() < x.end_num.parse::<u64>().ok() {&x.end_num} else {acc});
                            search_cards(&format!("!(nrdb<{end} nrdb>{start})"), backend, remaining)?
                        },
                        //otherwise grab the oldest or newest date depending on operator
                        "<=" | ">" => {
                            let cy_end = sets.into_iter().fold("00000", |acc, x| if acc.parse::<u64>().ok() < x.end_num.parse::<u64>().ok() {&x.end_num} else {acc});
                            search_cards(&format!("nrdb{operator}{cy_end}"),backend, remaining)?
                        },
                        ">=" | "<" => {
                            let cy_start = sets.into_iter().fold("99999", |acc, x| if acc.parse::<u64>().ok() > x.start_num.parse::<u64>().ok() {&x.start_num} else {acc});
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
                    "startup" | "sup" => search_cards("(cy:lib or cy:sg or cy:ele) -banned:startup -o:\"starter game only\"", backend, remaining)?,
                    "neo" => search_cards("is:nsg -set:su21 -banned:neo -o:\"starter game only\"", backend, remaining)?,
                    //"rig" | "postgateway" | "librealis" | "twocycle" => search_cards("date>=sg -banned:rig -o:\"starter game only\"", backend, remaining)?,
                    "standard" | "current" | "std" | "25.12" => search_cards("is:nsg -set:su21 -banned:standard -o:\"starter game only\"", backend, remaining)?,
                    "24.12" => search_cards("cy:kit or cy:rs or (nrdb>26000 -cy:sm -cy:ele) -banned:24.12 -o:\"starter game only\"", backend, remaining)?,
                    "sunset" => search_cards("-banned:sunset -o:\"starter game only\" cy:kit or cy:rs or (nrdb>26000 -cy:sm -cy:ele) or cy:mor", backend, remaining)?,
                    "eternal" => search_cards("-banned:eternal -o:\"starter game only\" -set:tdc -cy:draft -cy:napd", backend, remaining)?,
                    "pawnshop" => search_cards("-o:\"starter game only\" -set:tdc -cy:draft -cy:napd (is:corp tob>532) or (is:runner tob>442)", backend, remaining)?,
                    "pawnshopprev" => search_cards("-o:\"starter game only\" -set:tdc -cy:draft -cy:napd (is:corp tobold>532) or (is:runner tobold>442)", backend, remaining)?,
                    "throwback" => search_cards("-banned:throwback -o:\"starter game only\" -set:tdc -cy:draft -cy:napd", backend, remaining)?,
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
                    "corp" | "c" => search_cards("f:neutral-corp or f:n or f:j or f:h or f:w", backend, remaining)?,
                    "dfc" => search_cards("o:flip", backend, remaining)?,
                    "ffg" => search_cards("nrdb<24002", backend, remaining)?,
                    "guest" => search_cards("ft:\"Designed by\" -pavilion", backend, remaining)?,
                    "nsg" => search_cards("nrdb>26000 -cy:mor -cy:sm", backend, remaining)?,
                    "nearprinted" => remaining.into_iter().filter(|x| x.nearprint.is_some()).collect(),
                    "reprint" => remaining.into_iter().filter(|x| x.printings.len() > 1).collect(), //needs to change
                    "runner" | "r" => search_cards("f:anarch or f:shaper or f:criminal or f:adam or f:sunny-lebeau or f:apex or f:neutral-runner", backend, remaining)?,
                    "space" => search_cards("o:\"rez cost is lowered\"", backend, remaining)?,
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
                "order" | "sort" => {order = value.to_owned(); remaining}
                "pronouns" => remaining.into_iter().filter(|c| c.pronouns.is_some() && (c.pronouns.clone().unwrap().split("/").any(|i| i==value) || c.pronouns.clone().unwrap().contains("any"))).collect(),
                "p" | "v" | "points" => {
                    if value.parse::<u8>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                    remaining.into_iter().filter(|x| as_operator(operator, x.agenda_points, value.parse::<u8>().ok())).collect()
                },
                //"pro" | "pronouns" =>, //needs some cards.json edits
                "s" | "sub" | "subtype" => remaining.into_iter().filter(|x| if x.subtypes.is_some() {x.subtypes.clone().unwrap().to_lowercase().contains(&value)} else {false}).collect(),
                "set" | "e" | "edition" | "cycle" | "cyc" | "cy" => {
                    let set = backend.sets.iter().find(|x| x.code == value)?;
                    let start = &set.start_num;
                    let end = &set.end_num;
                    search_cards(&format!("nrdb<={end} nrdb>={start}"), backend, remaining)?
                },
                //"st" =>
                "str" | "strength" => {
                    if value.parse::<u8>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                    remaining.into_iter().filter(|x| as_operator(operator, x.strength, value.parse::<u8>().ok())).collect()
                },
                "t" | "type" => remaining.into_iter().filter(|x| x.type_code.contains(value)).collect(),
                "tob" => {
                    //these are only imported as needed to minimize load on general cataloguer searches, might lower performance for this search though
                    let ranks = serde_json::from_str::<Map<String, Value>>(&std::fs::read_to_string("assets/trashobusto.json").unwrap()).unwrap();
                    remaining.into_iter().filter(
                        |x| if ranks.contains_key(&x.title) {
                            as_operator(operator, ranks[&x.title].as_u64(), value.parse::<u64>().ok())
                        } else {
                            println!("card not in trasho: {}", &x.title); false
                        }).collect()},
                "trash" | "bin" | "h" => {
                    if value.parse::<u64>().is_err() {println!("{} was an invalid search term", buffer); continue;}
                    remaining.into_iter().filter(|x| as_operator(operator, x.trash_cost, value.parse::<u8>().ok())).collect()
                },
                "tobold" => {
                    //these are only imported as needed to minimize load on general cataloguer searches, might lower performance for this search though
                    let ranks = serde_json::from_str::<Map<String, Value>>(&std::fs::read_to_string("assets/tob_old.json").unwrap()).unwrap();
                    remaining.into_iter().filter(
                        |x| if ranks.contains_key(&x.title) {
                            as_operator(operator, ranks[&x.title].as_u64(), value.parse::<u64>().ok())
                        } else {
                            println!("card not in trasho: {}", &x.title); false
                        }).collect()},
                "_" | "name" => remaining.into_iter().filter(|x: &Card| {x.stripped_title.to_lowercase().contains(&part)}).collect(),
                _ => remaining, //skip if invalid mode
            };
        } else {
            println!("resolving a bracket of {}", buffer);
            part_results = search_cards(&buffer, backend, remaining)?;
            resolving_bracket = false;
        }
        remaining = if inverse {
            pre_query.into_iter()
                .map( |x| { let mut temp = x.clone(); temp.printings = x.printings.into_iter().filter(
                    |y| match part_results.iter().find(|&z| z.title == x.title) {
                        Some(k) => !k.printings.contains(y),
                        None => true,
                    }).collect(); temp}
                ).filter(|x| x.printings.len() > 0).collect()
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
        "cost" | "c" => remaining.sort_by_key(|card| card.cost),
        "faction" | "f" => remaining.sort_by_key(|card| faction_order(&card.faction)),
        "inf" => remaining.sort_by_key(|card| card.influence),
        "released" => remaining.sort_by_key(|card| card.printings.first().unwrap().code.clone()),
        "random" => remaining.shuffle(&mut thread_rng()),
        "alphabetical" | "alph" => remaining.sort_by_key(|card| card.stripped_title.clone()),
        //"set" => ,
        "tob" => {
            let ranks: Map<String, Value> = serde_json::from_str::<Map<String, Value>>(&std::fs::read_to_string("assets/trashobusto.json").unwrap()).unwrap();
            remaining = remaining.into_iter().filter(|c| ranks.contains_key(&c.title)).collect::<Vec<Card>>();
            remaining.sort_by_key(|card| ranks.get(&card.title).unwrap().as_u64())
        },
        "strength" => remaining.sort_by_key(|card| card.strength),
        "type" => remaining.sort_by_key(|card| card.type_code.clone()),
        _ => {},
    }
    if order_dir == "desc" {remaining.reverse()}
    println!("finished search"); //another debug print line
    Some(remaining)
}

#[derive(Error, Debug)]
pub enum SearchError {
    #[error("invalid search query: {0}")]
    ParseError(#[from] ParseError),
    #[error("invalid search query: {0}")]
    QueryError(String),
    #[error("internal error while searching: {0}")]
    InternalError(String),
    #[error("feature not yet implemented: {0}")]
    NotYetImplemented(String),
}

pub(crate) fn do_search(query: &str, backend: &Backend, form_settings: SearchSettings) -> Result<Vec<Printing>, SearchError> {
    println!("Begin query: '{query}'");
    let (node, settings) = parse_query(query)?;
    let card_pool: HashSet<SearchPrinting> = backend.cards.iter()
        .flat_map(
            |card| card.printings.iter().map(
                |printing| SearchPrinting {printing, card}
            )
        )
        .collect();
    let results: HashSet<SearchPrinting> = search_impl(node, backend, &card_pool, 0)?;
    
    // After searching, collect back into a HashSet of Cards
    let mut result_cards: HashSet<Card> = HashSet::new();
    for printing in results {
        if let Some(mut existing_card) = result_cards.take(printing.card) {
            existing_card.printings.push(printing.printing.clone());
            result_cards.insert(existing_card);
        } else {
            let mut new_card = printing.card.clone();
            new_card.printings = vec![printing.printing.clone()];
            result_cards.insert(new_card);
        }
    }

    //...and then into a Vec of cards, so we can sort them
    let mut results: Vec<&Card> = result_cards.iter().collect();

    // Safety: when building this list, we only insert a Card if we have a printing for it - so
    // card.printings is always nonempty, so it's safe to call unwrap() on .first() and .last()

    // By default, sort the cards by their latest NRDB printing.
    results.sort_by_key(|card| card.printings.last().unwrap().code.parse::<u64>().ok()); 

    let search_order = settings.sort.or(form_settings.sort).unwrap_or(SearchOrder::Released);
    match search_order {
        SearchOrder::Name => results.sort_by_cached_key(|card| &card.stripped_title),
        SearchOrder::Artist => results.sort_by_key(|card| &card.printings.last().unwrap().artist),
        SearchOrder::Cost => results.sort_by_key(|card| card.cost),
        SearchOrder::Type => results.sort_by_key(|card| &card.type_code),
        SearchOrder::Faction => results.sort_by_key(|card| faction_order(&card.faction)),
        SearchOrder::Influence => results.sort_by_key(|card| card.influence),
        SearchOrder::Released => results.sort_by_key(|card| &card.printings.first().unwrap().code),
        SearchOrder::Random => results.shuffle(&mut thread_rng()),
        SearchOrder::TrashOrBusto => {
            let ranks: Map<String, Value> = serde_json::from_str::<Map<String, Value>>(&std::fs::read_to_string("assets/trashobusto.json").unwrap()).unwrap();
            results = results.into_iter().filter(|c| ranks.contains_key(&c.title)).collect::<Vec<&Card>>();
            results.sort_by_key(|card| ranks.get(&card.title).unwrap().as_u64())
        },
        // TODO: handle not-implemented search types (e.g. strength, type) as well
        other => return Err(SearchError::NotYetImplemented(format!("search order {other:?}"))),
    };

    let search_direction = settings.direction.or(form_settings.direction).unwrap_or(SearchDirection::Ascending);
    if matches!(search_direction, SearchDirection::Descending) {
        results.reverse();
    }

    let printing_preference = settings.prefer.or(form_settings.prefer).unwrap_or(PrintingPreference::Newest);
    println!("End query: '{query}'");
    match printing_preference {
        PrintingPreference::Oldest => Ok(results.into_iter().map(|c| c.printings.first().unwrap().clone()).collect()),
        PrintingPreference::Newest => Ok(results.into_iter().map(|c| c.printings.last().unwrap().clone()).collect()),
    }
}

fn search_impl<'a>(node: QueryNode, backend: &Backend, card_pool: &HashSet<SearchPrinting<'a>>, depth: usize) -> Result<HashSet<SearchPrinting<'a>>, SearchError> {
    println!("{} {:?}", ">".repeat(depth), &node);
    let results = match node {
        QueryNode::OrGroup(inner_nodes) => {
            let mut hs = HashSet::new();
            // TODO: there's probably a way to do this nicely with a flat_map or something
            for inner in inner_nodes {
                let results = search_impl(*inner, backend, card_pool, depth+1)?;
                hs.extend(results);
            }
            Ok(hs)
        },
        QueryNode::AndGroup(inner_nodes) => {
            // TODO: surely I can do this without the clone
            let mut pool = card_pool.clone();
            for inner in inner_nodes {
                pool = search_impl(*inner, backend, &pool, depth+1)?;
            }
            Ok(pool)
        },
        QueryNode::TextFilter(text_filter) => {
            let text_value = match text_filter.value {
                TextValue::Regex(_) => {
                    return Err(SearchError::NotYetImplemented("regex searching".to_string()))
                }
                TextValue::Exact(_) => {
                    return Err(SearchError::NotYetImplemented("exact-string searching".to_string()))
                }
                TextValue::Plain(ref s) => s.clone()
            };
            let results: HashSet<SearchPrinting> = match text_filter.key {
                TextKey::Artist => card_pool.iter()
                    .filter(
                        |x| x.printing.artist.as_ref().is_some_and(|s| s.to_lowercase().contains(&text_value))
                    )
                    .copied()
                    .collect(),
                TextKey::Agenda => {
                    let parts: Vec<&str> = text_value.split('/').collect();
                    // reusable error message
                    let bad_agenda = || {
                        SearchError::QueryError(format!(
                            "not a valid 'agenda' filter: '{}'",
                            &text_value
                        ))
                    };
                    let [adv_str, points_str] = &parts[..] else {
                        return Err(bad_agenda());
                    };
                    let adv_req: i32 = adv_str.parse().map_err(|_| bad_agenda())?;
                    let points: i32 = points_str.parse().map_err(|_| bad_agenda())?;
                    inner_search(&format!("advancement:{adv_req} points:{points}"), backend, card_pool, depth+1)?
                }   
                TextKey::Banned => {
                    let Some(banlist_arr) = backend.banlist
                        .get(&text_value)
                        .and_then(|a| a.as_array())
                    else {
                        return Err(SearchError::QueryError(format!("not a banlist: '{text_value}'")))
                    };
                    card_pool.iter()
                    .filter(
                        |x| banlist_arr.contains(&json!(x.card.stripped_title))
                    )
                    .copied()
                    .collect()
                },
                TextKey::Date => {
                    let sets: Vec<&Set> = match &text_value {
                        x if Regex::new(r"^\d{2}/\d{2}/\d{4}$").unwrap().is_match(x) => return Err(SearchError::NotYetImplemented("dates of the form xx/xx/xxxx".to_string())), //think aboutthis one
                        x if Regex::new(r"^\d{2}/\d{2}/\d{2}$").unwrap().is_match(x) => return Err(SearchError::NotYetImplemented("dates of the form xx/xx/xx".to_string())), //think aboutthis one
                        x if Regex::new(r"^\d{4}$").unwrap().is_match(x) => backend.sets.iter().filter(|x| !x.date.is_empty() && x.date[6..8]==text_value[2..4]).collect(),
                        x if Regex::new(r"^\w{2,4}$").unwrap().is_match(x) => backend.sets.iter().filter(|x| x.code==text_value).collect(), //works
                        _ => return Err(SearchError::QueryError(format!("invalid date: '{text_value}'"))),
                    };
                    if sets.is_empty() {
                        //fails on years like 2025, or 2001, then currently shows no cards
                        return Ok(HashSet::new());
                    } else if let [the_set] = &sets[..] {
                        // if it's a single set, then just get that set
                        card_pool.iter()
                            .filter(
                                |x| x.printing.set == the_set.code
                            )
                            .copied()
                            .collect()
                    } else {
                        // TODO: why are we not just using the set matching code above??
                        let start = sets.iter().fold("99999", |acc, x| if acc.parse::<u64>().ok() > x.start_num.parse::<u64>().ok() {&x.start_num} else {acc});
                        let end = sets.iter().fold("00000", |acc, x| if acc.parse::<u64>().ok() < x.end_num.parse::<u64>().ok() {&x.end_num} else {acc});
                        let query_string = format!("nrdb<{end} nrdb>{start}");
                        let Ok((node, _)) = parse_query(&query_string) else {
                            // TODO: should this maybe be a QueryError instead?
                            return Err(SearchError::InternalError("failed to parse internally-generated query".to_string()))
                        };
                        search_impl(node, backend, card_pool, depth+1)?
                    }
                },
                TextKey::Faction => {
                    let faction = match text_value.as_str() {
                        "a" => "anarch",
                        "c" | "crim" => "criminal",
                        "s" => "shaper",
                        "h" | "hb" => "haas-bioroid",
                        "j" | "jin" => "jinteki",
                        "n" => "nbn",
                        "w" | "wey" | "weyland" => "weyland-consortium",
                        other => other
                    }; //probably could be smarter but this is simple
                    if faction == "neutral" {
                        card_pool.iter()
                            .filter(
                                |x| x.card.faction == "neutral-runner" || x.card.faction == "neutral-corp"
                            )
                            .copied()
                            .collect()
                    } else {
                        card_pool.iter()
                            .filter(
                                |x| x.card.faction == faction
                            )
                            .copied()
                            .collect()
                    }
                },
                TextKey::Format => {
                    let query_str = match text_value.as_str() {
                        "startup" | "sup" => "(cy:lib or cy:sg or cy:ele) -banned:startup -o:\"starter game only\"",
                        "neo" => "is:nsg -set:su21 -banned:neo -o:\"starter game only\"",
                        // "rig" | "postgateway" | "librealis" | "twocycle" => "date>=sg -banned:rig -o:\"starter game only\"",
                        "standard" | "current" | "std" | "25.12" => "is:nsg -set:su21 -banned:standard -o:\"starter game only\"",
                        "24.12" => "cy:kit or cy:rs or (nrdb>26000 -cy:sm -cy:ele) -banned:24.12 -o:\"starter game only\"",
                        "sunset" => "-banned:sunset -o:\"starter game only\" cy:kit or cy:rs or (nrdb>26000 -cy:sm -cy:ele) or cy:mor",
                        "eternal" => "-banned:eternal -o:\"starter game only\" -set:tdc -cy:draft -cy:napd",
                        "pawnshop" => "-o:\"starter game only\" -set:tdc -cy:draft -cy:napd (is:corp tob>532) or (is:runner tob>442)",
                        "pawnshopprev" => "-o:\"starter game only\" -set:tdc -cy:draft -cy:napd (is:corp tobold>532) or (is:runner tobold>442)",
                        "throwback" => "-banned:throwback -o:\"starter game only\" -set:tdc -cy:draft -cy:napd",
                        _ => return Err(SearchError::QueryError(format!("not a format: '{text_value}'"))),
                    };
                    let Ok((node, _)) = parse_query(query_str) else {
                        return Err(SearchError::InternalError("failed to parse internally-generated query".to_string()))
                    };
                    search_impl(node, backend, card_pool, depth+1)?
                },
                TextKey::FlavourText => card_pool.iter()
                    .filter(
                        |x| x.printing.flavour.as_ref().is_some_and(|s| s.to_lowercase().contains(&text_value))
                    )
                    .copied()
                    .collect(),
                TextKey::OracleText => card_pool.iter()
                    .filter(
                        |x| x.card.stripped_text.as_ref().is_some_and(|s| s.to_lowercase().contains(&text_value))
                    )
                    .copied()
                    .collect(),
                TextKey::Pronouns => card_pool.iter()
                    .filter(
                        |x| x.card.pronouns.as_ref().is_some_and(|s| s.to_lowercase().contains(&text_value))
                    )
                    .copied()
                    .collect(),
                TextKey::Set => {
                    let set = backend.sets.iter()
                        .find(|x| x.code == text_value)
                        .ok_or_else(|| SearchError::QueryError(format!("not a set: '{text_value}'")))?;
                    let start = &set.start_num;
                    let end = &set.end_num;
                    let Ok((node, _)) = parse_query(&format!("nrdb<={end} nrdb>={start}")) else {
                        return Err(SearchError::InternalError("failed to parse internally-generated query".to_string()))
                    };
                    search_impl(node, backend, card_pool, depth+1)?
                },
                TextKey::Subtype => card_pool.iter()
                    .filter(
                        |x| x.card.subtypes.as_ref().is_some_and(|s| s.to_lowercase().contains(&text_value))
                    )
                    .copied()
                    .collect(),
                TextKey::Type => card_pool.iter()
                    .filter(|x| x.card.type_code.contains(&text_value))
                    .copied()
                    .collect(),
                TextKey::Name => card_pool.iter()
                    .filter(|x| x.card.stripped_title.to_lowercase().contains(&text_value))
                    .copied()
                    .collect(),
            };

            if text_filter.is_negated {
                Ok(card_pool.iter().filter(|&sp| !results.contains(sp)).copied().collect())
            } else {
                Ok(results)
            }
        },
        QueryNode::NumFilter(num_filter) => {
            let results = match num_filter.key {
                NumericKey::Advancement => card_pool.iter()
                    .filter(|x| x.card.advancement_cost.is_some_and(|v| num_filter.comparator.as_operator(v as i32, num_filter.value)))
                    .copied()
                    .collect(),
                NumericKey::Cost => card_pool.iter()
                    .filter(|x| x.card.cost.is_some_and(|v| num_filter.comparator.as_operator(v as i32, num_filter.value)))
                    .copied()
                    .collect(),
                NumericKey::EternalPoints => card_pool.iter()
                    .filter(|x| x.card.eternal_points.is_some_and(|v| num_filter.comparator.as_operator(v as i32, num_filter.value)))
                    .copied()
                    .collect(),
                NumericKey::InfluenceCost => card_pool.iter()
                    .filter(|x| x.card.influence.is_some_and(|v| num_filter.comparator.as_operator(v as i32, num_filter.value)))
                    .copied()
                    .collect(),
                NumericKey::InfulenceLimit => card_pool.iter()
                    .filter(|x| x.card.influence_limit.is_some_and(|v| num_filter.comparator.as_operator(v as i32, num_filter.value)))
                    .copied()
                    .collect(),
                NumericKey::Link => card_pool.iter()
                    .filter(|x| x.card.base_link.is_some_and(|v| num_filter.comparator.as_operator(v as i32, num_filter.value)))
                    .copied()
                    .collect(),
                NumericKey::Memory => card_pool.iter()
                    .filter(|x| x.card.memory_cost.is_some_and(|v| num_filter.comparator.as_operator(v as i32, num_filter.value)))
                    .copied()
                    .collect(),
                NumericKey::MinDeck => card_pool.iter()
                    .filter(|x| x.card.minimum_deck_size.is_some_and(|v| num_filter.comparator.as_operator(v as i32, num_filter.value)))
                    .copied()
                    .collect(),
                NumericKey::NRDB => card_pool.iter()
                    .filter(|x| x.printing.code.parse().is_ok_and(|v: i32| num_filter.comparator.as_operator(v as i32, num_filter.value)))
                    .copied()
                    .collect(),
                NumericKey::Points => card_pool.iter()
                    .filter(|x| x.card.agenda_points.is_some_and(|v| num_filter.comparator.as_operator(v as i32, num_filter.value)))
                    .copied()
                    .collect(),
                NumericKey::Strength => card_pool.iter()
                    .filter(|x| x.card.strength.is_some_and(|v| num_filter.comparator.as_operator(v as i32, num_filter.value)))
                    .copied()
                    .collect(),
                NumericKey::TrashCost => card_pool.iter()
                    .filter(|x| x.card.trash_cost.is_some_and(|v| num_filter.comparator.as_operator(v as i32, num_filter.value)))
                    .copied()
                    .collect(),
            };
            Ok(results)
        },
        QueryNode::IsFilter(is_filter) => {
            let results = match is_filter.filter_type {
                IsFilterType::Advanceable => inner_search("o:\"you can advance this\" or o:\"can be advanced\" -o:\"that can be advanced\"", backend, card_pool, depth+1)?,
                IsFilterType::Corp => inner_search("f:neutral-corp or f:n or f:j or f:h or f:w", backend, card_pool, depth+1)?,
                IsFilterType::DoubleFaced => inner_search("o:flip", backend, card_pool, depth+1)?,
                IsFilterType::FFG => inner_search("nrdb<24002", backend, card_pool, depth+1)?,
                IsFilterType::Guest => inner_search("ft:\"Designed by\" -pavilion", backend, card_pool, depth+1)?,
                IsFilterType::NSG => inner_search("nrdb>26000 -cy:mor -cy:sm", backend, card_pool, depth+1)?,
                IsFilterType::Nearprinted => card_pool.iter().filter(|x| x.card.nearprint.is_some()).copied().collect(),
                IsFilterType::Reprint => card_pool.iter().filter(|x| x.card.printings.len()>1).copied().collect(),
                IsFilterType::Runner => inner_search("f:anarch or f:shaper or f:criminal or f:adam or f:sunny-lebeau or f:apex or f:neutral-runner", backend, card_pool, depth+1)?,
                IsFilterType::Space => inner_search("o:\"rez cost is lowered\"", backend, card_pool, depth+1)?,
                IsFilterType::Trap => inner_search("o:\"when the runner accesses this\"", backend, card_pool, depth+1)?,
                IsFilterType::Unique => card_pool.iter().filter(|x| x.card.uniqueness).copied().collect(),
            };

            if is_filter.is_negated {
                Ok(card_pool.iter().filter(|&sp| !results.contains(sp)).copied().collect())
            } else {
                Ok(results)
            }
        },
    };

    match &results {
        Ok(set) => {println!("{} {} results found", ">".repeat(depth), set.len())},
        Err(e) => println!("{} Search error: {e}", ">".repeat(depth))
    }

    results
}

fn inner_search<'a>(query: &str, backend: &Backend, card_pool: &HashSet<SearchPrinting<'a>>, depth: usize) -> Result<HashSet<SearchPrinting<'a>>, SearchError> {
    match parse_query(query) {
        Ok((node, _)) => search_impl(node, backend, card_pool, depth),
        Err(err) => Err(SearchError::InternalError(format!("failed to parse internally-generated query '{query}': {err}")))
    }
}
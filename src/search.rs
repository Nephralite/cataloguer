use std::fmt::Debug;
use serde_json::{json, Map, Value};
use rand::thread_rng;
use rand::seq::SliceRandom;
use regex::Regex;
use crate::structs::{Card, Backend, Set, Printing};

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

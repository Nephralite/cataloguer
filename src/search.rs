use crate::parse::{
    parse_query, IsFilterType, NumericKey, ParseError, PrintingPreference, QueryNode,
    SearchDirection, SearchOrder, SearchSettings, TextKey, TextValue,
};
use crate::structs::{Backend, Card, Printing, Set};
use rand::seq::SliceRandom;
use rand::thread_rng;
use regex::Regex;
use serde_json::{json, Map, Value};
use std::borrow::Cow;
use std::collections::HashSet;
use std::fmt::Debug;
use thiserror::Error;
use tracing::debug;

pub fn faction_order(faction: &str) -> usize {
    let order = [
        "anarch",
        "criminal",
        "shaper",
        "neutral-runner",
        "apex",
        "adam",
        "sunny-lebeau",
        "haas-bioroid",
        "jinteki",
        "nbn",
        "weyland-consortium",
        "neutral-corp",
    ];
    let answer = order.iter().position(|&r| r == faction);
    if answer.is_some() {
        answer.unwrap()
    } else {
        255
    }
}

// Struct used only inside the search.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct SearchPrinting<'a> {
    printing: &'a Printing,
    card: &'a Card,
}

#[derive(Error, Debug)]
pub enum SearchError {
    #[error("invalid search query: {0}")]
    QueryError(String),
    #[error("internal error while searching: {0}")]
    InternalError(String),
    #[error("feature not yet implemented: {0}")]
    NotYetImplemented(String),
}
impl From<ParseError> for SearchError {
    fn from(value: ParseError) -> Self {
        match value {
            ParseError::DefinedTwice(_)
            | ParseError::InvalidFilter(_)
            | ParseError::InvalidRegex(_)
            | ParseError::Malformed => Self::QueryError(value.to_string()),
            ParseError::Unreachable(_) => Self::InternalError(value.to_string()),
        }
    }
}

pub(crate) fn do_search(
    query: &str,
    backend: &Backend,
    form_settings: SearchSettings,
) -> Result<Vec<Printing>, SearchError> {
    debug!("Begin query: '{query}'");
    let (node, settings) = parse_query(query)?;
    let card_pool: HashSet<SearchPrinting> = backend
        .cards
        .iter()
        .flat_map(|card| {
            card.printings
                .iter()
                .map(|printing| SearchPrinting { printing, card })
        })
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

    let search_order = settings
        .sort
        .or(form_settings.sort)
        .unwrap_or(SearchOrder::Released);
    match search_order {
        SearchOrder::Name => results.sort_by_cached_key(|card| &card.stripped_title),
        SearchOrder::Artist => results.sort_by_key(|card| &card.printings.last().unwrap().artist),
        SearchOrder::Cost => results.sort_by_key(|card| card.cost),
        SearchOrder::Type => results.sort_by_key(|card| &card.type_code),
        SearchOrder::Faction => results.sort_by_key(|card| faction_order(&card.faction)),
        SearchOrder::Influence => results.sort_by_key(|card| card.influence),
        SearchOrder::Released => results.sort_by_key(|card| &card.printings.first().unwrap().code),
        SearchOrder::Random => results.shuffle(&mut thread_rng()),
        SearchOrder::Strength => results.sort_by_key(|card| card.strength),
        SearchOrder::Set => return Err(SearchError::NotYetImplemented("sort by set".to_string())),
        SearchOrder::TrashOrBusto => {
            let ranks: Map<String, Value> = serde_json::from_str::<Map<String, Value>>(
                &std::fs::read_to_string("assets/trashobusto.json").unwrap(),
            )
            .unwrap();
            results = results
                .into_iter()
                .filter(|c| ranks.contains_key(&c.title))
                .collect::<Vec<&Card>>();
            results.sort_by_key(|card| ranks.get(&card.title).unwrap().as_u64())
        }
    };

    let search_direction = settings
        .direction
        .or(form_settings.direction)
        .unwrap_or(SearchDirection::Ascending);
    if matches!(search_direction, SearchDirection::Descending) {
        results.reverse();
    }

    let printing_preference = settings
        .prefer
        .or(form_settings.prefer)
        .unwrap_or(PrintingPreference::Newest);
    match printing_preference {
        PrintingPreference::Oldest => Ok(results
            .into_iter()
            .map(|c| c.printings.first().unwrap().clone())
            .collect()),
        PrintingPreference::Newest => Ok(results
            .into_iter()
            .map(|c| c.printings.last().unwrap().clone())
            .collect()),
    }
}

/// Check whether a card matches a given query.
pub(crate) fn card_matches(
    query: &str,
    backend: &Backend,
    card: &Card,
) -> Result<bool, SearchError> {
    debug!("Begin query: '{query}'");
    let (node, _) = parse_query(query)?;
    let card_pool: HashSet<SearchPrinting> = card
        .printings
        .iter()
        .map(|printing| SearchPrinting { printing, card })
        .collect();

    Ok(!search_impl(node, backend, &card_pool, 0)?.is_empty())
}

fn search_impl<'a>(
    node: QueryNode,
    backend: &Backend,
    card_pool: &HashSet<SearchPrinting<'a>>,
    depth: usize,
) -> Result<HashSet<SearchPrinting<'a>>, SearchError> {
    debug!("{} {:?}", ">".repeat(depth), &node);
    let results = match node {
        QueryNode::OrGroup(inner_nodes) => {
            // doing this with iterators is a huge pain due to the Result handling, unfortunately
            let mut hs = HashSet::new();
            for inner in inner_nodes {
                let results = search_impl(*inner, backend, card_pool, depth + 1)?;
                hs.extend(results);
            }
            Ok(hs)
        }
        QueryNode::AndGroup(inner_nodes) => {
            let mut pool = Cow::Borrowed(card_pool);
            for inner in inner_nodes {
                pool = Cow::Owned(search_impl(*inner, backend, &pool, depth + 1)?);
            }
            Ok(pool.into_owned())
        }
        QueryNode::TextFilter(text_filter) => {
            if matches!(text_filter.value, TextValue::Exact(_)) {
                return Err(SearchError::NotYetImplemented(
                    "exact-string searching".to_string(),
                ));
            }
            let results: HashSet<SearchPrinting> = match text_filter.key {
                TextKey::Artist => card_pool
                    .iter()
                    .filter(|x| {
                        x.printing
                            .artist
                            .as_ref()
                            .is_some_and(|s| text_filter.value.matches(s))
                    })
                    .copied()
                    .collect(),
                TextKey::Agenda => {
                    let text_value = match text_filter.value {
                        TextValue::Plain(s) => s,
                        TextValue::Exact(_) => unreachable!(),
                        TextValue::Regex(_) => {
                            return Err(SearchError::QueryError(format!(
                                "can't use regex with '{}:' filter",
                                text_filter.original_key
                            )))
                        }
                    };
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
                    inner_search(
                        &format!("advancement:{adv_req} points:{points}"),
                        backend,
                        card_pool,
                        depth + 1,
                    )?
                }
                TextKey::Banned => {
                    let text_value = match text_filter.value {
                        TextValue::Plain(s) => s,
                        TextValue::Exact(_) => unreachable!(),
                        TextValue::Regex(_) => {
                            return Err(SearchError::QueryError(format!(
                                "can't use regex with '{}:' filter",
                                text_filter.original_key
                            )))
                        }
                    };
                    let Some(banlist_arr) =
                        backend.banlist.get(&text_value).and_then(|a| a.as_array())
                    else {
                        return Err(SearchError::QueryError(format!(
                            "not a known banlist: '{text_value}'"
                        )));
                    };
                    card_pool
                        .iter()
                        .filter(|x| banlist_arr.contains(&json!(x.card.stripped_title)))
                        .copied()
                        .collect()
                }
                TextKey::Date => {
                    let text_value = match text_filter.value {
                        TextValue::Plain(s) => s,
                        TextValue::Exact(_) => unreachable!(),
                        TextValue::Regex(_) => {
                            return Err(SearchError::QueryError(format!(
                                "can't use regex with '{}:' filter",
                                text_filter.original_key
                            )))
                        }
                    };
                    let sets: Vec<&Set> = match &text_value {
                        x if Regex::new(r"^\d{2}/\d{2}/\d{4}$").unwrap().is_match(x) => {
                            return Err(SearchError::NotYetImplemented(
                                "dates of the form xx/xx/xxxx".to_string(),
                            ))
                        }
                        x if Regex::new(r"^\d{2}/\d{2}/\d{2}$").unwrap().is_match(x) => {
                            return Err(SearchError::NotYetImplemented(
                                "dates of the form xx/xx/xx".to_string(),
                            ))
                        }
                        x if Regex::new(r"^\d{4}$").unwrap().is_match(x) => backend
                            .sets
                            .iter()
                            .filter(|x| !x.date.is_empty() && x.date[6..8] == text_value[2..4])
                            .collect(),
                        x if Regex::new(r"^\w{2,4}$").unwrap().is_match(x) => backend
                            .sets
                            .iter()
                            .filter(|x| x.code == text_value)
                            .collect(), //works
                        _ => {
                            return Err(SearchError::QueryError(format!(
                                "invalid date: '{text_value}'"
                            )))
                        }
                    };
                    let set_codes: Vec<&str> =
                        sets.into_iter().map(|set| set.code.as_str()).collect();

                    card_pool
                        .iter()
                        .filter(|x| set_codes.contains(&x.printing.set.as_str()))
                        .copied()
                        .collect()
                }
                TextKey::Faction => {
                    let text_value = match text_filter.value {
                        TextValue::Plain(s) => s,
                        TextValue::Exact(_) => unreachable!(),
                        TextValue::Regex(_) => {
                            return Err(SearchError::QueryError(format!(
                                "can't use regex with '{}:' filter",
                                text_filter.original_key
                            )))
                        }
                    };
                    let faction = match text_value.as_str() {
                        "a" => "anarch",
                        "c" | "crim" => "criminal",
                        "s" => "shaper",
                        "h" | "hb" => "haas-bioroid",
                        "j" | "jin" => "jinteki",
                        "n" => "nbn",
                        "w" | "wey" | "weyland" => "weyland-consortium",
                        other => other,
                    }; //probably could be smarter but this is simple
                    if faction == "neutral" {
                        card_pool
                            .iter()
                            .filter(|x| {
                                x.card.faction == "neutral-runner"
                                    || x.card.faction == "neutral-corp"
                            })
                            .copied()
                            .collect()
                    } else {
                        card_pool
                            .iter()
                            .filter(|x| x.card.faction == faction)
                            .copied()
                            .collect()
                    }
                }
                TextKey::Format => {
                    let text_value = match text_filter.value {
                        TextValue::Plain(s) => s,
                        TextValue::Exact(_) => unreachable!(),
                        TextValue::Regex(_) => {
                            return Err(SearchError::QueryError(format!(
                                "can't use regex with '{}:' filter",
                                text_filter.original_key
                            )))
                        }
                    };
                    let query_str = match text_value.as_str() {
                        "startup" | "sup" => "(cy:lib or cy:sg or cy:ele) -banned:startup -o:\"starter game only\"",
                        "neo" => "is:nsg -set:su21 -banned:neo -o:\"starter game only\"",
                        // "rig" | "postgateway" | "librealis" | "twocycle" => "date>=sg -banned:rig -o:\"starter game only\"",
                        "standard" | "current" | "std" | "25.12" => "is:nsg -set:vp -set:su21 -banned:standard -o:\"starter game only\"",
                        "24.12" => "cy:kit or cy:rs or (nrdb>26000 -cy:sm -cy:ele) -banned:24.12 -o:\"starter game only\"",
                        "sunset" => "-banned:sunset -o:\"starter game only\" cy:kit or cy:rs or (nrdb>26000 -cy:sm -cy:ele) or cy:mor",
                        "eternal" => "-banned:eternal -o:\"starter game only\" -set:vp -set:tdc -cy:draft -cy:napd",
                        "pawnshop" => "-o:\"starter game only\" -set:tdc -cy:draft -cy:napd (is:corp tob>532) or (is:runner tob>442)",
                        "pawnshopprev" => "-o:\"starter game only\" -set:tdc -cy:draft -cy:napd (is:corp tobold>532) or (is:runner tobold>442)",
                        "throwback" => "-banned:throwback -o:\"starter game only\" -set:tdc -cy:draft -cy:napd",
                        _ => return Err(SearchError::QueryError(format!("not a known format: '{text_value}'"))),
                    };
                    let Ok((node, _)) = parse_query(query_str) else {
                        return Err(SearchError::InternalError(
                            "failed to parse internally-generated query".to_string(),
                        ));
                    };
                    search_impl(node, backend, card_pool, depth + 1)?
                }
                TextKey::FlavourText => card_pool
                    .iter()
                    .filter(|x| {
                        x.printing
                            .flavour
                            .as_ref()
                            .is_some_and(|s| text_filter.value.matches(s))
                    })
                    .copied()
                    .collect(),
                TextKey::OracleText => card_pool
                    .iter()
                    .filter(|x| {
                        x.card
                            .stripped_text
                            .as_ref()
                            .is_some_and(|s| text_filter.value.matches(s))
                    })
                    .copied()
                    .collect(),
                TextKey::Symbol => card_pool
                    .iter()
                    .filter(|x| {
                        x.card
                            .text
                            .as_ref()
                            .is_some_and(|s| s.to_lowercase().contains(&format!("[{}]", text_value)))
                    })
                    .copied()
                    .collect(),
                TextKey::Pronouns => card_pool
                    .iter()
                    .filter(|x| {
                        x.card
                            .pronouns
                            .as_ref()
                            .is_some_and(|s| text_filter.value.matches(s))
                    })
                    .copied()
                    .collect(),
                TextKey::Set => {
                    let text_value = match text_filter.value {
                        TextValue::Plain(s) => s,
                        TextValue::Exact(_) => unreachable!(),
                        TextValue::Regex(_) => {
                            return Err(SearchError::QueryError(format!(
                                "can't use regex with '{}:' filter",
                                text_filter.original_key
                            )))
                        }
                    };
                    let set = backend
                        .sets
                        .iter()
                        .find(|x| x.code == text_value)
                        .ok_or_else(|| {
                            SearchError::QueryError(format!("not a known set: '{text_value}'"))
                        })?;
                    let start = &set.start_num;
                    let end = &set.end_num;
                    let Ok((node, _)) = parse_query(&format!("nrdb<={end} nrdb>={start}")) else {
                        return Err(SearchError::InternalError(
                            "failed to parse internally-generated query".to_string(),
                        ));
                    };
                    search_impl(node, backend, card_pool, depth + 1)?
                }
                TextKey::Subtype => card_pool
                    .iter()
                    .filter(|x| {
                        x.card
                            .subtypes
                            .as_ref()
                            .is_some_and(|s| text_filter.value.matches(s))
                    })
                    .copied()
                    .collect(),
                TextKey::Type => card_pool
                    .iter()
                    .filter(|x| text_filter.value.matches(&x.card.type_code))
                    .copied()
                    .collect(),
                TextKey::Name => card_pool
                    .iter()
                    .filter(|x| text_filter.value.matches(&x.card.stripped_title))
                    .copied()
                    .collect(),
            };

            if text_filter.is_negated {
                Ok(card_pool
                    .iter()
                    .filter(|&sp| !results.contains(sp))
                    .copied()
                    .collect())
            } else {
                Ok(results)
            }
        }
        QueryNode::NumFilter(num_filter) => {
            let results = match num_filter.key {
                NumericKey::Advancement => card_pool
                    .iter()
                    .filter(|x| {
                        x.card.advancement_cost.is_some_and(|v| {
                            num_filter
                                .comparator
                                .as_operator(v as i32, num_filter.value)
                        })
                    })
                    .copied()
                    .collect(),
                NumericKey::Cost => card_pool
                    .iter()
                    .filter(|x| {
                        x.card.cost.is_some_and(|v| {
                            num_filter
                                .comparator
                                .as_operator(v as i32, num_filter.value)
                        })
                    })
                    .copied()
                    .collect(),
                NumericKey::EternalPoints => card_pool
                    .iter()
                    .filter(|x| {
                        x.card.eternal_points.is_some_and(|v| {
                            num_filter
                                .comparator
                                .as_operator(v as i32, num_filter.value)
                        })
                    })
                    .copied()
                    .collect(),
                NumericKey::InfluenceCost => card_pool
                    .iter()
                    .filter(|x| {
                        x.card.influence.is_some_and(|v| {
                            num_filter
                                .comparator
                                .as_operator(v as i32, num_filter.value)
                        })
                    })
                    .copied()
                    .collect(),
                NumericKey::InfluenceLimit => card_pool
                    .iter()
                    .filter(|x| {
                        x.card.influence_limit.is_some_and(|v| {
                            num_filter
                                .comparator
                                .as_operator(v as i32, num_filter.value)
                        })
                    })
                    .copied()
                    .collect(),
                NumericKey::Link => card_pool
                    .iter()
                    .filter(|x| {
                        x.card.base_link.is_some_and(|v| {
                            num_filter
                                .comparator
                                .as_operator(v as i32, num_filter.value)
                        })
                    })
                    .copied()
                    .collect(),
                NumericKey::Memory => card_pool
                    .iter()
                    .filter(|x| {
                        x.card.memory_cost.is_some_and(|v| {
                            num_filter
                                .comparator
                                .as_operator(v as i32, num_filter.value)
                        })
                    })
                    .copied()
                    .collect(),
                NumericKey::MinDeck => card_pool
                    .iter()
                    .filter(|x| {
                        x.card.minimum_deck_size.is_some_and(|v| {
                            num_filter
                                .comparator
                                .as_operator(v as i32, num_filter.value)
                        })
                    })
                    .copied()
                    .collect(),
                NumericKey::NRDB => card_pool
                    .iter()
                    .filter(|x| {
                        x.printing.code.parse().is_ok_and(|v: i32| {
                            num_filter.comparator.as_operator(v, num_filter.value)
                        })
                    })
                    .copied()
                    .collect(),
                NumericKey::Points => card_pool
                    .iter()
                    .filter(|x| {
                        x.card.agenda_points.is_some_and(|v| {
                            num_filter
                                .comparator
                                .as_operator(v as i32, num_filter.value)
                        })
                    })
                    .copied()
                    .collect(),
                NumericKey::Strength => card_pool
                    .iter()
                    .filter(|x| {
                        x.card.strength.is_some_and(|v| {
                            num_filter
                                .comparator
                                .as_operator(v as i32, num_filter.value)
                        })
                    })
                    .copied()
                    .collect(),
                NumericKey::TrashCost => card_pool
                    .iter()
                    .filter(|x| {
                        x.card.trash_cost.is_some_and(|v| {
                            num_filter
                                .comparator
                                .as_operator(v as i32, num_filter.value)
                        })
                    })
                    .copied()
                    .collect(),
            };
            Ok(results)
        }
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
                Ok(card_pool
                    .iter()
                    .filter(|&sp| !results.contains(sp))
                    .copied()
                    .collect())
            } else {
                Ok(results)
            }
        }
    };

    match &results {
        Ok(set) => {
            debug!("{} {} results found", ">".repeat(depth), set.len())
        }
        Err(e) => debug!("{} Search error: {e}", ">".repeat(depth)),
    }

    results
}

fn inner_search<'a>(
    query: &str,
    backend: &Backend,
    card_pool: &HashSet<SearchPrinting<'a>>,
    depth: usize,
) -> Result<HashSet<SearchPrinting<'a>>, SearchError> {
    match parse_query(query) {
        Ok((node, _)) => search_impl(node, backend, card_pool, depth),
        Err(err) => Err(SearchError::InternalError(format!(
            "failed to parse internally-generated query '{query}': {err}"
        ))),
    }
}

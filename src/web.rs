use std::collections::HashMap;
use askama::Template;
use askama_axum::{IntoResponse, Response};
use axum::extract::{Path, Query, State};
use axum::http::StatusCode;
use axum::Json;
use axum::response::Html;
use rand::prelude::SliceRandom;
use rand::thread_rng;
use serde_json::{Map, Value};
use crate::structs::{Backend, Card, Legality, Set, SimpleAPIout};
use crate::web::search::{faction_order, search_cards};

#[path="search.rs"]
mod search;

pub(crate) async fn setspage(State(backend): State<Backend>) -> impl IntoResponse {
    Templates::SetsPageTemplate(SetsPageTemplate {query:"".to_owned(), order:"".to_owned(), dir:"".to_owned(), sets:backend.sets})
}

#[derive(Template)]
#[template(path = "cardpage.html")]
struct CardPageTemplate {
    query: String,
    order: String,
    dir: String,
    card: Card,
    x: usize,
    legality: Legality,
}

#[derive(serde::Deserialize, Debug)]
pub struct SearchForm {
    search: Option<String>,
    order: Option<String>,
    dir: Option<String>
}

#[derive(Template)]
#[template(path = "index.html")]
struct IndexTemplate {query: String, order: String, dir: String}

#[derive(Template)]
#[template(path = "syntax.html")]
struct SyntaxTemplate {query: String, order: String, dir: String}

#[derive(Template)]
#[template(path = "cards.html")]
struct CardsList {
    query: String,
    cards: Vec<[String; 2]>,
    order: String,
    dir: String
}

#[derive(Template)]
#[template(path = "sets.html")]
struct SetsPageTemplate {
    query: String,
    order: String,
    dir: String,
    sets: Vec<Set>
}

enum Templates {
    CardsList(CardsList),
    SyntaxTemplate(SyntaxTemplate),
    IndexTemplate(IndexTemplate),
    CardPageTemplate(CardPageTemplate),
    SetsPageTemplate(SetsPageTemplate)
}

impl IntoResponse for Templates {
    fn into_response(self) -> Response {
        // Attempt to render the template with askama
        let inner = match self {
            Templates::CardsList(c) => c.render(),
            Templates::IndexTemplate(c) => c.render(),
            Templates::SyntaxTemplate(c) => c.render(),
            Templates::CardPageTemplate(c) => c.render(),
            Templates::SetsPageTemplate(c) => c.render(),
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

pub(crate) async fn syntax() -> impl IntoResponse {
    Templates::SyntaxTemplate(SyntaxTemplate { query: "".to_owned(), order: "".to_owned(), dir: "".to_owned(), })
}

pub(crate) async fn cardpage(Path(params): Path<HashMap<String, String>>, State(backend): State<Backend>) -> impl IntoResponse {
    let printing = params.get("printing").unwrap().parse().unwrap();
    let id = params.get("id").unwrap();
    let card = backend.cards.iter().find(|x| x.stripped_title.to_lowercase() == *id).unwrap();

    let startup = if search::search_cards("z:startup", &backend, vec!(card.clone())) == Some(vec!(card.clone())) {"legal"} else if search_cards("banned:startup", &backend, vec!(card.clone())) == Some(vec!(card.clone())) {"banned"} else {"not legal"};
    let standard= if search::search_cards("z:standard", &backend, vec!(card.clone())) == Some(vec!(card.clone())) {"legal"} else if search_cards("banned:standard", &backend, vec!(card.clone())) == Some(vec!(card.clone())) {"banned"} else {"not legal"};
    let eternal = if search::search_cards("ep>0", &backend, vec!(card.clone())) == Some(vec!(card.clone())) {format!("{} Points", card.eternal_points.unwrap())} else if search_cards("z:eternal", &backend, vec!(card.clone())) == Some(vec!(card.clone())) {"legal".to_owned()} else if search_cards("banned:eternal", &backend, vec!(card.clone())) == Some(vec!(card.clone())) {"banned".to_owned()} else {"not legal".to_owned()};

    Templates::CardPageTemplate(CardPageTemplate{ query:"".to_owned(), order:"".to_owned(), dir:"".to_owned(), card: card.clone(), x: printing, legality: Legality {startup, standard, eternal}})
}

pub(crate) async fn simple_api(
    State(backend): State<Backend>,
    Query(params): Query<SearchForm>
) -> impl IntoResponse {
    if let Some(query) = params.search {
        let mut temp: Vec<String> = vec![];
        let results: Option<Vec<Card>> = search_cards(&query, &backend, backend.cards.clone());
        if results.is_some() {
            for card in results.unwrap() {
                temp.push(card.printings.last().unwrap().code.clone());
            }
            Json(SimpleAPIout{len: temp.len(), data: temp}).into_response()
        } else {
            Json(SimpleAPIout{len: 0, data: vec!()}).into_response()
        }
    } else {
        Json(SimpleAPIout{len: 0, data: vec!()}).into_response()
    }
}
pub(crate) async fn search(
    State(backend): State<Backend>,
    Query(params): Query<SearchForm>,
) -> impl IntoResponse {
    if let Some(query) = params.search {
        let mut temp: Vec<[String; 2]> = vec![];
        let results: Option<Vec<Card>> = search_cards(&query, &backend, backend.cards.clone());
        if results.is_some() {
            let  mut results = results.unwrap();
            match params.order.clone().unwrap_or("".to_owned()).as_str() {
                "artist" => results.sort_by_key(|card| card.printings.last()?.artist.clone()),
                "cost" => results.sort_by_key(|card| card.cost),
                "faction" => results.sort_by_key(|card| faction_order(&card.faction)),
                "inf" => results.sort_by_key(|card| card.influence),
                "released" => results.sort_by_key(|card| card.printings.first().unwrap().code.clone()),
                "random" => results.shuffle(&mut thread_rng()),
                "alphabetical" => results.sort_by_key(|card| card.stripped_title.clone()),
                "tob" => {
                    let ranks: Map<String, Value> = serde_json::from_str::<Map<String, Value>>(&std::fs::read_to_string("assets/trashobusto.json").unwrap()).unwrap();
                    results = results.into_iter().filter(|c| ranks.contains_key(&c.title)).collect::<Vec<Card>>();
                    results.sort_by_key(|card| ranks.get(&card.title).unwrap().as_u64())
                },
                _ => (),
            };
            //match params.dir {
            //}
            if params.dir == Some("desc".to_owned()) {results.reverse()}
            if query.contains("prefer:oldest") {
                for card in results {
                    temp.push([card.printings.first().unwrap().code.clone(), card.printings.first().unwrap().img_type.clone()]);
                }
            } else { //prefer:newest is just the default
                for card in results {
                    temp.push([card.printings.last().unwrap().code.clone(), card.printings.last().unwrap().img_type.clone()]);

                }
            }
        }
        Templates::CardsList(CardsList { query, cards: temp, order: params.order.unwrap_or("".to_owned()), dir: params.dir.unwrap_or("".to_owned()) })
    } else {
        Templates::IndexTemplate(IndexTemplate {query:"".to_owned(), order:"".to_owned(), dir:"".to_owned()})
    }
}
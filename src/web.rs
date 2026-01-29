use std::collections::HashMap;
use askama::Template;
use askama_axum::{IntoResponse, Response};
use axum::extract::{Path, Query, State};
use axum::http::StatusCode;
use axum::Json;
use axum::response::Html;
use crate::parse::{SearchDirection, SearchSettings};
use crate::structs::{Backend, Card, Legality, Set, SimpleAPIout};
use crate::search::{card_matches, do_search};

pub async fn setspage(State(backend): State<Backend>) -> impl IntoResponse {
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

#[derive(Template)]
#[template(path = "search_error.html")]
struct SearchErrorTemplate {
    query: String,
    order: String,
    dir: String,
    error: String,
}

enum Templates {
    CardsList(CardsList),
    SyntaxTemplate(SyntaxTemplate),
    IndexTemplate(IndexTemplate),
    CardPageTemplate(CardPageTemplate),
    SetsPageTemplate(SetsPageTemplate),
    SearchErrorTemplate(SearchErrorTemplate),
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
            Templates::SearchErrorTemplate(c) => c.render(),
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

pub async fn syntax() -> impl IntoResponse {
    Templates::SyntaxTemplate(SyntaxTemplate { query: "".to_owned(), order: "".to_owned(), dir: "".to_owned(), })
}

pub async fn cardpage(Path(params): Path<HashMap<String, String>>, State(backend): State<Backend>) -> impl IntoResponse {
    let printing = params.get("printing").unwrap().parse().unwrap();
    let id = params.get("id").unwrap();
    let card = backend.cards.iter().find(|x| x.stripped_title.to_lowercase() == *id).unwrap();

    // SAFETY: The unwrap() calls here should all be safe as these are static string searches.
    let startup = if card_matches("z:startup", &backend, card).unwrap() {"legal"} else if card_matches("banned:startup", &backend, card).unwrap() {"banned"} else {"not legal"};
    let standard= if card_matches("z:standard", &backend, card).unwrap() {"legal"} else if card_matches("banned:standard", &backend, card).unwrap() {"banned"} else {"not legal"};
    let eternal = if card_matches("ep>0", &backend, card).unwrap() {format!("{} Points", card.eternal_points.unwrap())} else if card_matches("z:eternal", &backend, card).unwrap() {"legal".to_owned()} else if card_matches("banned:eternal", &backend, card).unwrap() {"banned".to_owned()} else {"not legal".to_owned()};

    Templates::CardPageTemplate(CardPageTemplate{ query:"".to_owned(), order:"".to_owned(), dir:"".to_owned(), card: card.clone(), x: printing, legality: Legality {startup, standard, eternal}})
}

pub async fn simple_api(
    State(backend): State<Backend>,
    Query(params): Query<SearchForm>
) -> impl IntoResponse {
    if let Some(query) = params.search {
        match do_search(&query, &backend, SearchSettings::default()) {
            Ok(printings) => Json(SimpleAPIout{len: printings.len(), data: printings.iter().map(|printing| printing.code.clone()).collect(), error: None}).into_response(),
            Err(e) => Json(SimpleAPIout{len: 0, data: vec!(), error: Some(e.to_string())}).into_response(),
        }
    } else {
        Json(SimpleAPIout{len: 0, data: vec!(), error: Some("no query provided".to_string())}).into_response()
    }
}

pub async fn search(
    State(backend): State<Backend>,
    Query(params): Query<SearchForm>,
) -> impl IntoResponse {
    if let Some(query) = params.search {
        let form_search_settings = SearchSettings {
            direction: match params.dir.as_deref() {
                Some("asc") => Some(SearchDirection::Ascending),
                Some("desc") => Some(SearchDirection::Descending),
                _ => None,
            },
            sort: params.order.clone().and_then(|s| s.as_str().try_into().ok()),
            prefer: None,
            unique_by: None,
            display: None,
        };

        // TODO: on Error, redirect to an error page or something rather than just returning nothing
        let results = match do_search(&query, &backend, form_search_settings) {
            Ok(results) => results,
            Err(e) => return Templates::SearchErrorTemplate(
                SearchErrorTemplate {
                    query,
                    order: params.order.unwrap_or("".to_owned()),
                    dir: params.dir.unwrap_or("".to_owned()),
                    error: e.to_string()
                }
            )
        };

        Templates::CardsList(
            CardsList {
                query,
                cards: results.iter().map(|p| [p.code.clone(), p.img_type.clone()]).collect(),
                order: params.order.unwrap_or("".to_owned()), dir: params.dir.unwrap_or("".to_owned())
            }
        )
    } else {
        Templates::IndexTemplate(IndexTemplate {query:"".to_owned(), order:"".to_owned(), dir:"".to_owned()})
    }
}

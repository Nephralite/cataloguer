use anyhow::{Context, bail};
use axum::{
    routing::get,
    Router
};
use serde_json::{Map, Value};
use std::env;
use tokio::net::TcpListener;
use tower_http::services::ServeDir;
use tracing::info;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};
use cataloguer::*;

//initialize a backend from our jsons
fn init_backend() -> anyhow::Result<structs::Backend> {
    let backend = structs::Backend {
        cards: serde_json::from_str::<Vec<structs::Card>>(&std::fs::read_to_string("assets/cards.json")?)?,
        banlist: serde_json::from_str::<Map<String, Value>>(&std::fs::read_to_string("assets/banned.json")?)?,
        sets: serde_json::from_str::<Vec<structs::Set>>(&std::fs::read_to_string("assets/sets.json")?)?,
    };

    // Safety check - ensure that all cards have at least one printing
    for card in &backend.cards {
        if card.printings.is_empty() {
            bail!("Card '{}' has no printings!", card.title)
        }
        // if card.stripped_text.is_none() {
        //     bail!("Card '{}' has no stripped_text", card.title)
        // }
    }
    
    Ok(backend)
}

//creates a router and hosts several pages on 0.0.0.0:PORT
#[tokio::main]
async fn main() -> anyhow::Result<()> {
    //add tracing for some console logs from doing all this
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
        .route("/", get(web::search))
        .route("/cards/:id/:printing", get(web::cardpage))
        .route("/syntax", get(web::syntax))
        .route("/sets", get(web::setspage))
        .route("/simple_api/", get(web::simple_api))
        .with_state(backend)
        .nest_service(
            "/assets",
            ServeDir::new(format!("{}/assets", env::current_dir()?.to_str().unwrap())),
        );
    let port = env::var("PORT").unwrap_or_else(|_| "8080".to_owned());
    let listener = TcpListener::bind(format!("0.0.0.0:{port}")).await?;

    info!("router initialized, listening on localhost:{port}");

    axum::serve(listener, router)
        .await
        .context("error while starting server")?;

    Ok(())
}

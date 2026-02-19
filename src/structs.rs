use serde_json::{Map, Value};

//the backend is a static pile of jsons because cards.json was our existing input
//realistically a database would probably be better, but I'd rather hit release first
#[derive(Clone)]
pub struct Backend {
    pub cards: Vec<Card>,
    pub banlist: Map<String, Value>,
    pub sets: Vec<Set>,
}

//had to restructure out of Vec<Value> due to askama referencing issues
#[derive(serde::Deserialize, Debug, Clone)]
pub struct Set {
    pub code: String,
    pub name: String,
    pub cycle: bool,
    pub date: String,
    pub cards: u32,
    pub start_num: String,
    pub end_num: String,
}

pub struct Legality {
    pub startup: &'static str,
    pub standard: &'static str,
    pub eternal: String,
}

#[derive(serde::Serialize)]
pub struct SimpleAPIout {
    pub data: Vec<String>,
    pub error: Option<String>,
    pub len: usize,
}

#[derive(serde::Serialize)]
pub struct FullAPIout {
    pub data: Vec<Card>,
    pub error: Option<String>,
    pub len: usize,
}

#[derive(serde::Serialize, serde::Deserialize, Clone, Eq)]
pub struct Printing {
    pub artist: Option<String>,
    pub flavour: Option<String>,
    pub code: String,
    pub img_type: String,
    pub set: String,
}

impl PartialEq for Printing {
    fn eq(&self, other: &Printing) -> bool {
        self.code == other.code
    }
}

impl std::hash::Hash for Printing {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.code.hash(state);
    }
}


#[derive(serde::Serialize, serde::Deserialize, Clone, Eq)]
pub struct Card {
    pub printings: Vec<Printing>,
    pub faction: String,
    pub type_code: String,
    pub subtypes: Option<String>,
    pub title: String,
    pub stripped_title: String,
    pub text: Option<String>,
    pub stripped_text: Option<String>,
    pub uniqueness: bool,
    pub influence: Option<u8>,
    pub influence_limit: Option<u8>,
    pub minimum_deck_size: Option<u8>,
    pub strength: Option<u8>,
    pub base_link: Option<u8>,
    pub cost: Option<u8>,
    pub trash_cost: Option<u8>,
    pub memory_cost: Option<u8>,
    pub advancement_cost: Option<u8>,
    pub agenda_points: Option<u8>,
    pub eternal_points: Option<u8>,
    pub nearprint: Option<String>,
    pub pronouns: Option<String>
}

impl PartialEq for Card {
    fn eq(&self, other: &Card) -> bool {
        self.title == other.title
    }
}
impl std::hash::Hash for Card {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.title.hash(state);
    }
}

use std::borrow::Cow;

use anyhow::anyhow;
use pest::{
    iterators::{Pair, Pairs},
    Parser,
};
use pest_derive::Parser;
use thiserror::Error;

#[derive(Parser)]
#[grammar = "query.pest"] // relative to src
struct QueryParser;

#[derive(Debug)]
pub enum QueryNode {
    OrGroup(Vec<Box<QueryNode>>),
    AndGroup(Vec<Box<QueryNode>>),
    TextFilter(TextFilter),
    NumFilter(NumFilter),
    IsFilter(IsFilter),
}

#[derive(Debug)]
pub struct TextFilter {
    pub key: TextKey,
    pub value: TextValue,
    pub is_negated: bool,
}

#[derive(Debug)]
pub enum TextValue {
    Plain(String),
    Exact(String),
    Regex(String),
}
impl TextValue {
    pub fn value(&self) -> &str {
        match self {
            TextValue::Plain(s) => s.as_str(),
            TextValue::Exact(s) => s.as_str(),
            TextValue::Regex(s) => s.as_str(),
        }
    }
}
impl TryFrom<&Pair<'_, Rule>> for TextValue {
    type Error = ParseError;

    fn try_from(value: &Pair<'_, Rule>) -> Result<Self, Self::Error> {
        let vs = value.as_str();
        match value.as_rule() {
            // TODO: string escaping or something lol
            Rule::exact_quoted_value => Ok(TextValue::Exact((vs[2..vs.len() - 1]).to_string())),
            Rule::exact_unquoted_value => Ok(TextValue::Exact(vs[1..vs.len()].to_string())),
            Rule::quoted_value => Ok(TextValue::Plain(vs[1..vs.len() - 1].to_lowercase().to_string())),
            Rule::unquoted_value => Ok(TextValue::Plain(vs.to_lowercase().to_string())),
            Rule::regex_value => Ok(TextValue::Regex(vs[1..vs.len() - 1].to_string())),

            _ => Err(ParseError::Unreachable(format!(
                "invalid inner for TextValue: {:?}",
                value.as_rule()
            ))),
        }
    }
}

#[derive(Debug)]
pub enum TextKey {
    Artist,
    Banned,
    Date, // not really text but close enough
    Faction,
    Format,
    FlavourText,
    OracleText,
    Pronouns,
    Set,
    Subtype,
    Type,
    Name,
}

#[derive(Debug)]
pub struct NumFilter {
    pub key: NumericKey,
    pub value: i32,
    pub comparator: NumericComparator,
}

#[derive(Debug)]
pub enum NumericKey {
    Advancement,
    Cost,
    EternalPoints,
    InfluenceCost,
    InfulenceLimit,
    Link,
    Memory,
    MinDeck,
    NRDB,
    Points,
    Strength,
    TrashCost,
}

#[derive(Debug)]
pub enum NumericComparator {
    Eq,
    Neq,
    Ge,
    Geq,
    Le,
    Leq,
}
impl NumericComparator {
    pub fn as_operator<T: PartialEq + PartialOrd>(&self, x: T, y: T) -> bool {
        match self {
            NumericComparator::Eq => x == y,
            NumericComparator::Neq => x != y,
            NumericComparator::Ge => x > y,
            NumericComparator::Geq => x >= y,
            NumericComparator::Le => x < y,
            NumericComparator::Leq => x <= y,
        }
    }
}

#[derive(Debug)]
pub struct IsFilter {
    pub filter_type: IsFilterType,
    pub is_negated: bool,
}

#[derive(Debug)]
pub enum IsFilterType {
    Advanceable,
    Corp,
    DoubleFaced,
    FFG,
    Guest,
    NSG,
    Nearprinted,
    Reprint,
    Runner,
    Space,
    Trap,
    Unique,
}

#[derive(Debug, Default)]
pub(crate) struct SearchSettings {
    pub direction: Option<SearchDirection>,
    pub sort: Option<SearchOrder>,
    pub prefer: Option<PrintingPreference>,
    pub unique_by: Option<UniqueBy>,
    pub display: Option<ResultDisplay>,
}

#[derive(Debug)]
pub(crate) enum SearchOrder {
    Artist,
    Cost,
    Faction,
    Influence,
    Name,
    Random,
    Released,
    Strength,
    Set,
    TrashOrBusto,
    Type,
}
impl TryFrom<&str> for SearchOrder {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "artist" => Ok(Self::Artist),
            "cost" => Ok(Self::Cost),
            "faction" => Ok(Self::Faction),
            "inf" => Ok(Self::Influence),
            "alphabetical" | "name" => Ok(Self::Name),
            "random" => Ok(Self::Random),
            "released" => Ok(Self::Released),
            "strength" | "str" => Ok(Self::Strength),
            "set" => Ok(Self::Set),
            "tob" => Ok(Self::TrashOrBusto),
            "type" => Ok(Self::Type),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub(crate) enum SearchDirection {
    Ascending,
    Descending,
}

#[derive(Debug)]
pub(crate) enum PrintingPreference {
    Oldest,
    Newest,
}

#[derive(Debug)]
pub(crate) enum UniqueBy {
    Cards,
    Prints,
    Art,
}

#[derive(Debug)]
pub(crate) enum ResultDisplay {
    Grid,
    Checklist,
    Full,
    Text,
}

pub fn parse_query(query: &str) -> Result<(QueryNode, SearchSettings), ParseError> {
    let mut parsed = QueryParser::parse(Rule::input, query).or(Err(ParseError::Malformed))?;

    let pair = parsed.next().ok_or(ParseError::Unreachable(
        "initial parse succeeded".to_string(),
    ))?;

    if !matches!(pair.as_rule(), Rule::input) {
        return Err(ParseError::Unreachable(format!(
            "expected rule `input`, got rule `{:?}`",
            pair.as_rule()
        )));
    }

    let query_group = pair.into_inner().next().ok_or(ParseError::Unreachable(
        "`input` always contains exactly 1 query_group".to_string(),
    ))?;

    let mut search_settings = SearchSettings::default();
    let node = parse_query_group(query_group, &mut search_settings)?;

    Ok((node, search_settings))
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("search setting '{0}' defined twice")]
    DefinedTwice(String),
    #[error("invalid filter: {0}")]
    InvalidFilter(String),
    #[error("invalid regex: '{0}'")]
    InvalidRegex(String),
    #[error("malformed query, failed to parse")]
    Malformed,
    #[error("internal error - hit code path that should have been unreachable: {0}")]
    Unreachable(String), // use instead of unreachable, expect, unwrap, etc to avoid panics
}

fn parse_query_group(
    pair: Pair<'_, Rule>,
    settings: &mut SearchSettings,
) -> Result<QueryNode, ParseError> {
    match pair.as_rule() {
        Rule::query_group => {
            let inner_group = pair
                .into_inner()
                .next()
                .ok_or(ParseError::Unreachable("query group has exactly one inner".to_string()))?;
            parse_query_group(inner_group, settings)
        }
        Rule::bracketed_query => {
            let inner_bracketed = pair.into_inner().next().ok_or(ParseError::Unreachable("bracketed has exactly one inner".to_string()))?;
            parse_query_group(inner_bracketed, settings)
        }
        Rule::or_query => Ok(QueryNode::OrGroup(
            pair
                .into_inner()
                .map(|inner| parse_query_group(inner, settings).map(Box::new))
                .collect::<Result<Vec<Box<QueryNode>>, ParseError>>()?,
        )),
        Rule::and_query => Ok(QueryNode::AndGroup(
            pair
                .into_inner()
                .map(|inner: Pair<'_, Rule>| {
                    parse_query_group(inner, settings)
                    .map(Box::new)
                })
                .collect::<Result<Vec<Box<QueryNode>>, ParseError>>()?,
        )),
        Rule::filter => parse_filter(pair, settings),
        other_rule => Err(ParseError::Unreachable(format!("expected one of `query_group | and_query | bracketed_query | or_query | filter`, got rule `{:?}`", other_rule))),
    }
}

fn parse_filter(
    filter: Pair<'_, Rule>,
    settings: &mut SearchSettings,
) -> Result<QueryNode, ParseError> {
    if !matches!(filter.as_rule(), Rule::filter) {
        return Err(ParseError::Unreachable(format!(
            "expected rule `filter`, got rule `{:?}`",
            filter.as_rule()
        )));
    }
    let the_filter = filter
        .into_inner()
        .next()
        .expect("`filter` always contains exactly 1 group type");
    match the_filter.as_rule() {
        Rule::numeric_filter => {
            let mut iter = the_filter.into_inner();
            let key_str = iter.next().unwrap().as_str();
            let comparator = iter.next().unwrap();
            let value = iter.next().unwrap().as_str().parse().unwrap();
            Ok(QueryNode::NumFilter(NumFilter {
                key: match key_str {
                    "adv" | "g" | "advancement" => NumericKey::Advancement,
                    "c" | "cost" | "rez" => NumericKey::Cost,
                    "ep" => NumericKey::EternalPoints,
                    "i" | "inf" | "influence" => NumericKey::InfluenceCost,
                    "inf_lim" | "il" => NumericKey::InfulenceLimit,
                    "l" | "link" => NumericKey::Link,
                    "m" | "mem" | "memory" => NumericKey::Memory,
                    "md" | "min_deck" => NumericKey::MinDeck,
                    "nrdb" => NumericKey::NRDB,
                    "p" | "v" | "points" => NumericKey::Points,
                    "str" | "strength" => NumericKey::Strength,
                    "trash" | "bin" | "h" => NumericKey::Strength,
                    _ => {
                        return Err(ParseError::InvalidFilter(format!(
                            "not a numeric filter: '{}'",
                            key_str
                        )))
                    }
                },
                value,
                comparator: match comparator.as_str() {
                    "<=" => NumericComparator::Leq,
                    ">=" => NumericComparator::Geq,
                    "!=" => NumericComparator::Neq,
                    "=" => NumericComparator::Eq,
                    ">" => NumericComparator::Ge,
                    "<" => NumericComparator::Le,
                    _ => {
                        return Err(ParseError::Unreachable(format!(
                            "invalid comparator: {}",
                            comparator.as_str()
                        )))
                    }
                },
            }))
        }
        Rule::negative_filter | Rule::positive_filter => {
            let is_negated = matches!(the_filter.as_rule(), Rule::negative_filter);
            let mut iter = the_filter.into_inner();
            let key_str = iter.next().unwrap().as_str();
            let value_pair = iter.next().unwrap().into_inner().next().unwrap();
            let text_value = TextValue::try_from(&value_pair)?;

            // Syntactic sugar: "agenda" should parse out to an AndGroup of a points and value search
            // TODO: move this out to the search layer, it doesn't need to live in the parse layer lol
            if key_str == "agenda" {
                let text_str = text_value.value();
                let parts: Vec<&str> = text_str.split('/').collect();
                // reusable error message
                let bad_agenda = || {
                    ParseError::InvalidFilter(format!(
                        "not a valid 'agenda' filter: '{}'",
                        text_value.value()
                    ))
                };
                dbg!(text_value.value());
                let [adv_str, points_str] = &parts[..] else {
                    return Err(bad_agenda());
                };
                let adv_req: i32 = adv_str.parse().map_err(|_| bad_agenda())?;
                let points: i32 = points_str.parse().map_err(|_| bad_agenda())?;
                return Ok(QueryNode::AndGroup(vec![
                    Box::new(QueryNode::NumFilter(NumFilter {
                        key: NumericKey::Advancement,
                        value: adv_req,
                        comparator: NumericComparator::Eq,
                    })),
                    Box::new(QueryNode::NumFilter(NumFilter {
                        key: NumericKey::Points,
                        value: points,
                        comparator: NumericComparator::Eq,
                    })),
                ]));
            }

            // Special case for "is:" and "not:" filters.
            // In the grammar, they're functionally text filters, but we parse them into
            // `IsFilter`s instead.
            if key_str == "is" || key_str == "not" {
                // we maybe shouldn't allow -not:xyz? but whatevs
                let negated = is_negated ^ (key_str == "not");
                if !matches!(
                    value_pair.as_rule(),
                    Rule::unquoted_value | Rule::quoted_value
                ) {
                    return Err(ParseError::InvalidFilter(format!(
                        "can't use regex/exact values in a '{key_str}:' filter."
                    )));
                }
                return Ok(QueryNode::IsFilter(IsFilter {
                    filter_type: match text_value.value() {
                        "advanceable" => IsFilterType::Advanceable,
                        "corp" | "c" => IsFilterType::Corp,
                        "dfc" => IsFilterType::DoubleFaced,
                        "ffg" => IsFilterType::FFG,
                        "guest" => IsFilterType::Guest,
                        "nsg" => IsFilterType::NSG,
                        "nearprinted" => IsFilterType::Nearprinted,
                        "reprint" => IsFilterType::Reprint,
                        "runner" | "r" => IsFilterType::Runner,
                        "space" => IsFilterType::Space,
                        "trap" => IsFilterType::Trap,
                        "unique" => IsFilterType::Unique,
                        _ => {
                            return Err(ParseError::InvalidFilter(format!(
                                "not a valid '{key_str}:' value: {}",
                                text_value.value()
                            )))
                        }
                    },
                    is_negated: negated,
                }));
            }

            // Special case for setting search settings from the query.

            // Parse actual `TextFilter`s.
            Ok(QueryNode::TextFilter(TextFilter {
                key: match key_str {
                    "a" | "artist" => TextKey::Artist,
                    "b" | "banned" => TextKey::Banned,
                    "d" | "date" | "year" => TextKey::Date,
                    "f" | "faction" => TextKey::Faction,
                    "fmt" | "format" | "z" | "legal" => TextKey::Format,
                    "ft" | "flavor" | "flavour" => TextKey::FlavourText,
                    "o" | "x" | "text" | "oracle" => TextKey::OracleText,
                    "pronouns" => TextKey::Pronouns,
                    "s" | "sub" | "subtype" => TextKey::Subtype,
                    "set" | "e" | "edition" | "cycle" | "cyc" | "cy" => TextKey::Set,
                    "t" | "type" => TextKey::Type,
                    "_" | "name" => TextKey::Name,
                    _ => {
                        return Err(ParseError::InvalidFilter(format!(
                            "not a text filter: '{key_str}'"
                        )))
                    }
                },
                value: text_value,
                is_negated,
            }))
        }
        Rule::negative_value => {
            let Some(value) = the_filter.into_inner().next() else {
                return Err(ParseError::Unreachable(
                    "negative_value should always have inner".to_string(),
                ));
            };
            if !matches!(value.as_rule(), Rule::value) {
                return Err(ParseError::Unreachable(
                    "inner of negative_value should always be value".to_string(),
                )); 
            }
            let Some(inner) = value.into_inner().next() else {
                return Err(ParseError::Unreachable(
                    "value should always have inner".to_string(),
                ));
            };

            Ok(QueryNode::TextFilter(TextFilter {
                key: TextKey::Name,
                value: (&inner).try_into()?,
                is_negated: true,
            }))
        }
        Rule::value => {
            let Some(inner) = the_filter.into_inner().next() else {
                return Err(ParseError::Unreachable(
                    "value should always have inner".to_string(),
                ));
            };

            Ok(QueryNode::TextFilter(TextFilter {
                key: TextKey::Name,
                value: (&inner).try_into()?,
                is_negated: false,
            }))
        }
        _ => Err(ParseError::Unreachable(
            "parse_filter fallthrough".to_string(),
        )),
    }
}

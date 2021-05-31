use std::fmt::Formatter;

use proj_core::method::LABEL_LEAD_END;
use serde::de::Error;
use serde::de::SeqAccess;
use serde::de::Visitor;
use serde::Deserialize;
use serde::Deserializer;

/// The values of the `base_calls` attribute
#[derive(Debug, Clone)]
pub enum BaseCalls {
    Near,
    Far,
}

impl<'de> Deserialize<'de> for BaseCalls {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let lower_str = s.to_lowercase();
        Ok(match lower_str.as_str() {
            "near" => BaseCalls::Near,
            "far" => BaseCalls::Far,
            _ => return Err(Error::custom(format!("unknown call type '{}'", s))),
        })
    }
}

/// The values for the `calls` attribute
#[derive(Debug, Clone)]
pub enum Calls {
    Base(BaseCalls),
    Specific(Vec<CallSpec>),
}

struct CallsVisitor;

impl<'de> Visitor<'de> for CallsVisitor {
    type Value = Calls;

    fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
        formatter.write_str("`near` or `far`")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: Error,
    {
        let lower_str = v.to_lowercase();
        Ok(match lower_str.as_str() {
            "near" => Calls::Base(BaseCalls::Near),
            "far" => Calls::Base(BaseCalls::Far),
            _ => return Err(E::custom(format!("unknown call type '{}'", v))),
        })
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        let mut calls = Vec::with_capacity(seq.size_hint().unwrap_or(10));
        while let Some(e) = seq.next_element::<CallSpec>()? {
            calls.push(e);
        }
        Ok(Calls::Specific(calls))
    }
}

impl<'de> Deserialize<'de> for Calls {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(CallsVisitor)
    }
}

/// The specification of a single call type used in a composition.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct CallSpec {
    place_notation: String,
    symbol: String,
    #[serde(default = "lead_end")]
    lead_location: String,
    calling_positions: Option<String>,
}

#[inline(always)]
fn lead_end() -> String {
    LABEL_LEAD_END.to_owned()
}

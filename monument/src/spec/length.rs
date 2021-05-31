use std::{
    fmt,
    ops::{Range, RangeInclusive},
};

use serde::{
    de::{Error, MapAccess, Visitor},
    Deserialize, Deserializer,
};

/* Constants for commonly used length ranges */

pub const QP: RangeInclusive<usize> = 1250..=1350;
pub const HALF_PEAL: RangeInclusive<usize> = 2500..=2600;
pub const PEAL: RangeInclusive<usize> = 5000..=5200;

/// A new-typed range with human-friendly deserialisation
#[derive(Debug, Clone)]
#[repr(transparent)]
pub(super) struct Length {
    range: Range<usize>,
}

// Convert Length to a ranges
impl From<Length> for Range<usize> {
    #[inline(always)]
    fn from(l: Length) -> Range<usize> {
        l.range
    }
}

impl From<usize> for Length {
    #[inline(always)]
    fn from(v: usize) -> Length {
        Length { range: v..v + 1 }
    }
}

impl From<Range<usize>> for Length {
    #[inline(always)]
    fn from(r: Range<usize>) -> Length {
        Length { range: r }
    }
}

impl From<RangeInclusive<usize>> for Length {
    #[inline(always)]
    fn from(r: RangeInclusive<usize>) -> Length {
        Length {
            range: *r.start()..r.end() + 1,
        }
    }
}

struct LengthVisitor;

impl<'de> Visitor<'de> for LengthVisitor {
    type Value = Length;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a positive integer or a name like 'peal' or a 'min/max' range")
    }

    fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
    where
        E: Error,
    {
        de_signed_num(v)
    }

    fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Ok(Length::from(v as usize))
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "lowercase", deny_unknown_fields)]
        enum Fields {
            Min,
            Max,
        }

        // If we are deserialising a map, we expect the form `{ min: _, max: _ }`
        let mut min: Option<usize> = None;
        let mut max: Option<usize> = None;
        // I don't care about allocating strings here - we'll only serialise one length per run
        while let Some(key) = map.next_key()? {
            match key {
                Fields::Min => {
                    if min.is_some() {
                        return Err(Error::duplicate_field("min"));
                    }
                    min = Some(map.next_value()?);
                }
                Fields::Max => {
                    if max.is_some() {
                        return Err(Error::duplicate_field("max"));
                    }
                    max = Some(map.next_value()?);
                }
            }
        }
        let min = min.ok_or_else(|| Error::missing_field("min"))?;
        let max = max.ok_or_else(|| Error::missing_field("max"))?;
        Ok(Length::from(min..=max))
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: Error,
    {
        let lower_str = v.to_lowercase();
        Ok(Length::from(match lower_str.as_str() {
            "qp" => QP,
            "quarter peal" => QP,
            "peal" => PEAL,
            "half peal" => HALF_PEAL,
            _ => return Err(E::custom(format!("unknown length name '{}'", v))),
        }))
    }
}

/// Helper function to generate an error message on negative lengths
#[inline(always)]
fn de_signed_num<E: Error>(v: i64) -> Result<Length, E> {
    if v >= 0 {
        Ok(Length::from(v as usize))
    } else {
        Err(E::custom(format!("negative length: {}", v)))
    }
}

impl<'de> Deserialize<'de> for Length {
    fn deserialize<D>(deserializer: D) -> Result<Length, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(LengthVisitor)
    }
}

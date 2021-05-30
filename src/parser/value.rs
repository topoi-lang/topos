//! From the result of the Tokeniser (Token, Option<SmolStr>),
//! pattern match the Token and parse to Value.

use std::fmt;
use super::tokeniser::Token;

pub enum Value {
    Float(f64),
    Integer(i64),
    String(String),
}

impl From<i64> for Value {
    fn from(val: i64) -> Value {
        Value::Integer(val)
    }
}

impl From<f64> for Value {
    fn from(val: f64) -> Value {
        Value::Float(val)
    }
}

#[derive(Debug)]
pub enum ValueError {
    Float(std::num::ParseFloatError),
    Integer(std::num::ParseIntError),
    Unknown,
}

impl From<std::num::ParseFloatError> for ValueError {
    fn from(err: std::num::ParseFloatError) -> Self {
        ValueError::Float(err)
    }
}
impl From<std::num::ParseIntError> for ValueError {
    fn from(err: std::num::ParseIntError) -> Self {
        ValueError::Integer(err)
    }
}

impl fmt::Display for ValueError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueError::Float(err) => write!(f, "falied to parse float: {}", err),
            ValueError::Integer(err) =>write!(f, "failed to parse integer: {}", err),
            ValueError::Unknown => write!(f, "unknown value"),
        }
    }
}

impl std::error::Error for ValueError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ValueError::Float(err) => Some(err),
            ValueError::Integer(err) => Some(err),
            ValueError::Unknown => None,
        }
    }
}

impl Value {
    // ~~Maybe I can use SIMD parsing here? https://kholdstare.github.io/technical/2020/05/26/faster-integer-parsing.html ~~
    // Now the results are similar as the blog stated. So we can use parse at ease.
    pub fn from_token(token: Token, s: &str) -> Result<Self, ValueError> {
        let value = match token {
            Token::Float => Value::Float(s.parse()?),
            Token::Ident => Value::Integer(s.parse()?),
            Token::String => Value::String(String::from(s)),
            _ => return Err(ValueError::Unknown),
        };
        Ok(value)
    }
}
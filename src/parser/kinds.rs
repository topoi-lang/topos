//! Rowan's syntax kind representation

#[repr(u16)]
#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, PartialOrd, Hash)]
pub enum SyntaxKind {
    TOKEN_ERROR,
    TOKEN_COMMENT,
    TOKEN_WHITESPACE,

    TOKEN_INTEGER,
    TOKEN_FLOAT,
    TOKEN_STRING,

    TOKEN_LET,
    TOKEN_IN,


    // Composite nodes,
    NODE_LET_IN,

    #[doc(hidden)]
    __LAST,
}

use SyntaxKind::*;

impl SyntaxKind {
    /// Returns true if the token is a literal, can be parsed as a value.
    pub fn is_literal(self) -> bool {
        match self {
            TOKEN_INTEGER => true,
            TOKEN_FLOAT => true,
            TOKEN_STRING => true,
            _ => false,
        }
    }
}

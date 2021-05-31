//! The body of the parser: turn the token streams into an AST

use rowan::{Checkpoint, GreenNode, GreenNodeBuilder, Language, TextRange, TextSize};
use smol_str::SmolStr;

use super::{
    // types::{Root},
    SyntaxNode, SyntaxToken, SyntaxElement
};

macro_rules! structured {
    ($($kind:expr => $name:ident$(: $trait:ident)*$(: { $($block:tt)* })*),*) => {
        $(
            #[derive(Clone, Debug)]
            pub struct $name(SyntaxNode);

            impl TypedNode for $name {
                fn cast(from: SyntaxNode) -> Option<Self> {
                    if from.kind() == $kind {
                        Some(Self(from))
                    } else {
                        None
                    }
                }
                fn node(&self) -> &SyntaxNode {
                    &self.0
                }
            }
            $(impl $trait for $name {})*
            $(impl $name { $($block)* })*
        )*
    }
}

pub enum ParseError {
    Unexpected(TextRange),
    UnexpectedEOF,
}

// impl fmt::Display for ParseError { }
// impl std::error::Error for ParseError {}

pub struct AST {
    /// It is equivalent to Roslyn's Red-Green Syntax node,
    /// this is an immutable tree, which is cheap to change
    /// but doesn't contain offsets and parent pointers, we store parse results here
    node: GreenNode,
    errors: Vec<ParseError>,
}

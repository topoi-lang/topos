//! The body of the parser: turn the token streams into an AST

use rowan::{Checkpoint, GreenNode, GreenNodeBuilder, Language, TextRange, TextSize};
use smol_str::SmolStr;

use super::{
    // types::{Root},
    SyntaxNode, SyntaxToken, SyntaxElement
};

pub enum ParseError {
    Unexpected(TextRange),
    UnexpectedEOF,
}

// impl fmt::Display for ParseError { }
// impl std::error::Error for ParseError {}

pub struct AST {
    node: GreenNode,
    errors: Vec<ParseError>,
}

impl AST {
    /// Return the root node
    pub fn node(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.node.clone())
    }

    // Return a borrowed typed root node
    //pub fn root(&self) -> Root {
    //    Root::cast(self.node()).unwrap()
    //}

}
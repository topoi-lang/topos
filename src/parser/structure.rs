//! Provide a structure for the AST
//! 

use std::convert::TryFrom;
use std::fmt;

use super::{
    SyntaxNode, SyntaxToken, SyntaxElement,
};

/// A struct that prints out the textual representation of a node in a
/// stable format. See StructedNode::dump
pub struct TextDump(SyntaxNode);

// impl fmt::Display for TextDump {}

pub trait StructedNode: Clone {
    /// Cast a node into this structed node. Returns None if the type
    /// was not correct
    fn cast(from: SyntaxNode) -> Option<Self>;

    /// Return a reference to the inner node
    fn node(&self) -> &SyntaxNode;

    // Return all errors of all children, recursively
    //fn errors(&self) -> Vec<SyntaxElement> {
    //    self.node()
    //        .descendants_with_tokens()
    //}
}
//! Provide a structure for the AST
//! By design, syntax tree can be built even for completely invalid source code.

use std::convert::TryFrom;
use std::fmt;

use super::{
    tokeniser::SyntaxKind,
    SyntaxNode, SyntaxToken, SyntaxElement,
};

/// A struct that prints out the textual representation of a node in a
/// stable format. See StructuredNode::dump
pub struct TextDump(SyntaxNode);

// impl fmt::Display for TextDump {}

pub trait StructuredNode: Clone {
    /// Cast a node into this structured node. Returns None if the type
    /// was not correct
    fn cast(from: SyntaxNode) -> Option<Self>;

    /// Return a reference to the inner node
    fn node(&self) -> &SyntaxNode;

    fn errors(&self) -> Vec<SyntaxElement> {
        self.node()
            .descendants_with_tokens()
            .filter(|node| !node.text_range().is_empty())
            .filter(|node| node.kind() == SyntaxKind::Error)
            .collect()
    }

    fn dump(&self) -> TextDump {
        TextDump(self.node().clone())
    }
}

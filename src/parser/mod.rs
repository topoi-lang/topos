
use rowan;

mod kinds;
pub use self::{
    kinds::SyntaxKind
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ToposLang {}

pub type SyntaxNode = rowan::SyntaxNode<ToposLang>;
pub type SyntaxToken = rowan::SyntaxToken<ToposLang>;
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

impl rowan::Language for ToposLang {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        let discriminant: u16 = raw.0;
        assert!(discriminant <= (SyntaxKind::__LAST as u16));
        unsafe { std::mem::transmute::<u16, SyntaxKind>(discriminant) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind as u16)
    }
}

use rowan:: {
    GreenNode,
};

pub enum ParseError {

}

pub struct AST {
    node: GreenNode,
    errors: Vec<ParseError>
}

// pub fn parse(input: &str) -> AST { }
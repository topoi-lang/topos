
pub mod tokeniser;
mod syntax;
mod structure;

use tokeniser::Token;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ToposLang {}

pub type SyntaxNode = rowan::SyntaxNode<ToposLang>;
pub type SyntaxToken = rowan::SyntaxToken<ToposLang>;
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

// maps SyntaxKind to Token
impl rowan::Language for ToposLang {
    type Kind = tokeniser::Token;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        let discriminant: u16 = raw.0;
        assert!(discriminant <= (Token::__LAST as u16));
        unsafe { std::mem::transmute::<u16, Token>(discriminant) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind as u16)
    }
}

// pub fn parse(input: &str) -> AST { }
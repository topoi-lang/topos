//! The tokeniser turns a string into tokens
//! 

use logos::{ Logos, Lexer };
use smol_str::SmolStr;

#[repr(u16)] // size_of::<Token>() == 2
#[derive(Logos, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token {
    #[token("cond")] Cond,
    #[token("fn")] Func,

    // These tokens need to use slice() to get the information
    #[regex("[a-zA-Z_$][a-zA-Z0-9_$]*")] Ident,
    #[regex("-?[0-9]+")]                 Integer,
    #[regex("-?[0-9]+\\.[0-9]+")]        Float,
    #[regex("\"([^\"\\\\]|\\\\.)*\"")]
    #[regex("'([^'\\\\]|\\\\.)*'")]
    String,

    // Group multiple whitespaces into one token
    #[regex(r"[ ]+")]  Whitespace,
    #[regex(r"[\t]+")] WhitespaceTab,

    // We need to ungroup newline to keep track of decorators
    #[regex(r"\n")]
    #[regex(r"\r\f")]
    Newline,

    // Logos requires one token variant to handle errors,
    // it can be named anything you wish.
    #[error] Error,

    #[doc(hidden)]
    __LAST, // last value of the enum, requires Ord trait
}

pub struct Tokeniser<'source> {
    lexer: Lexer<'source, Token>,
}

impl<'source> Tokeniser<'source> {
    pub fn new(input: &'source str) -> Self {
        Self { lexer: Token::lexer(input) }
    }

    /// Source from which this Lexer is reading tokens, such as input string
    pub fn source(&self) -> &'source str {
        self.lexer.source()
    }

    fn need_capture(token: &Token) -> bool {
        match token {
            Token::Ident => true,
            Token::Integer => true,
            Token::Float => true,
            Token::String => true,
            _ => false,
        }
    }
}

impl Iterator for Tokeniser<'_> {
    type Item = (Token, Option<SmolStr>);

    // Allocate the lexer text slice into stack allocate string only when it
    // needs to be captured.
    fn next(&mut self) -> Option<Self::Item> {
        let lex = self.lexer.next();
        match lex {
            None => None,
            Some(tok) => {
                if Tokeniser::need_capture(&tok) {
                    Some((tok, Some(self.lexer.slice().into())))
                } else {
                    Some((tok, None))
                }
            }
        }
    }
}

#[test]
 pub fn tokenizer_test() {
     use Token::*;
     let lex = Token::lexer("cond fn closure wow");

     let a : Vec<_> = lex.spanned().collect();
     assert_eq!(a, vec![(Cond, 0..4), (Whitespace, 4..5), (Func, 5..7), (Whitespace, 7..8), (Ident, 8..15), (Whitespace, 15..16), (Ident, 16..19)])
 }

#[test]
pub fn tokenizer_failparse() {
    use Token::*;
    let mut lex = Token::lexer("1.2.3");

    lex.next();
    assert_eq!(lex.slice(), "1.2");
}

#[test]
pub fn tokenizer_whitespace_groupping() {
    use Token::*;
    let mut lex = Token::lexer("    ");

    let a : Vec<_> = lex.spanned().collect();
    assert_eq!(a, vec![(Whitespace, 0..4)]);
}

#[test]
pub fn tokeniser_iterator_smolstring() {
    use Token::*;
    use self::*;
    let a : Vec<(Token, Option<SmolStr>)> = Tokeniser::new("cond  fn closure wow").collect();

    assert_eq!(a, vec![
        (Cond, None),
        (Whitespace, None),
        (Func, None),
        (Whitespace, None),
        (Ident, Some("closure".into())),
        (Whitespace, None),
        (Ident, Some("wow".into())),
    ])
}
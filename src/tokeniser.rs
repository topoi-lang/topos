
use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
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

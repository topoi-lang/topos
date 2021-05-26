
use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    #[token("cond")] Cond,
    #[token("fn")] Func,
    #[regex("[a-zA-Z]+")] Ident,

    #[regex("-?[0-9]+", |t| t.slice().parse())]
    Integer(i64),

    #[regex("-?[0-9]+\\.[0-9]+", |t| t.slice().parse())]
    Float(f64),

    // Logos requires one token variant to handle errors,
    // it can be named anything you wish.
    #[error]
    // We can also use this variant to define whitespace,
    // or any other matches we wish to skip.
    #[regex(r"[ \t\n\r\f]+", logos::skip)]
    Error,
}

#[test]
fn tokenizer_test() {
    use Token::*;
    let lex = Token::lexer("cond fn closure wow");

    let a : Vec<_> = lex.spanned().collect();

    assert_eq!(a, vec![(Cond, 0..4), (Func, 5..7), (Ident, 8..15), (Ident, 16..19)])
}
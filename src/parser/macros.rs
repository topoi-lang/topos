#[macro_export]
#[rustfmt::skip]
macro_rules! P {
    (let) => ($super::kinds::SyntaxKind::TOKEN_LET);
    (in)  => ($super::kinds::SyntaxKind::TOKEN_IN);
}
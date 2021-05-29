
#[repr(u16)] // size_of::<SyntaxKind>() == 2
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum SyntaxKind {
    TokenLet,

    #[doc(hidden)]
    __LAST, // enum last value, need Ord
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // --- Keywords ---
    Fn, If, Elif, Else, For, While, Return, Struct, Mut, Pub, Use, Match, Mod, True, False, In, Or, And, Not,
    Try, Catch, Throw, Break, Continue,

    // --- Literals ---
    Int(i64), Float(f64), Str(String),

    // --- Identifier ---
    Ident(String),

    // --- Operators ---
    Plus, Minus, Star, Slash, Percent,
    Eq, EqEq, NotEq, Lt, Gt, LtEq, GtEq,
    ColonEq, ColonColon, Arrow, Question, Dot, DotDot,
    Pipe,
    PipeArrow, // |>

    // --- Delimiters ---
    LParen, RParen, LBracket, RBracket, LBrace, RBrace, Colon, Comma,

    // --- F-String ---
    FStringStart,
    FStringText(String),
    FStringExprStart,
    FStringExprEnd,
    FStringEnd,

    // --- Structure ---
    Newline, Indent, Dedent, EOF,
}

#[derive(Debug, Clone)]
pub struct Spanned {
    pub token: Token,
    pub line: usize,
    pub col: usize,
}
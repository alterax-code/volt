#[derive(Debug, Clone, PartialEq)]
pub enum BinOp { Add, Sub, Mul, Div, Mod, Eq, NotEq, Lt, Gt, LtEq, GtEq, And, Or, Range, Pipe }

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp { Neg, Not }

#[derive(Debug, Clone, PartialEq)]
pub enum Type { Named(String), Optional(Box<Type>), Array(Box<Type>), Generic(String, Vec<Type>) }

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Param { pub name: String, pub typ: Option<Type>, pub default: Option<Expr> }

#[derive(Debug, Clone)]
pub struct Field { pub name: String, pub typ: Type }

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64), Float(f64), Str(String), Bool(bool), Ident(String),
    BinOp   { left: Box<Expr>, op: BinOp, right: Box<Expr> },
    UnaryOp { op: UnaryOp, expr: Box<Expr> },
    Call    { func: Box<Expr>, args: Vec<Expr> },
    Field   { object: Box<Expr>, name: String },
    Index   { object: Box<Expr>, index: Box<Expr> },
    Array   (Vec<Expr>),
    Map     (Vec<(String, Expr)>),
    FString (Vec<Expr>),
    Lambda  { params: Vec<String>, body: Block },
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Wildcard,
    Binding(String),
    Or(Vec<Pattern>),
}

#[derive(Debug, Clone)]
pub struct MatchArm { pub pattern: Pattern, pub body: Block }

#[derive(Debug, Clone)]
pub enum Stmt {
    Let      { name: String, mutable: bool, typ: Option<Type>, value: Expr },
    Assign   { target: Expr, value: Expr },
    FnDef    { name: String, params: Vec<Param>, ret_type: Option<Type>, body: Block, is_pub: bool },
    Struct   { name: String, fields: Vec<Field>, methods: Vec<Stmt> },
    If       { cond: Expr, then_b: Block, elifs: Vec<(Expr, Block)>, else_b: Option<Block> },
    For      { var: String, iter: Expr, body: Block },
    While    { cond: Expr, body: Block },
    Return   (Option<Expr>),
    ExprStmt (Expr),
    TryCatch { try_body: Block, catch_var: String, catch_body: Block },
    Throw    (Expr),
    Break,
    Continue,
    Match    { expr: Expr, arms: Vec<MatchArm> },
    Use      { path: String },
}

pub type Block = Vec<Stmt>;
pub type Program = Vec<Stmt>;
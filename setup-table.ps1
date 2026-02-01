## VOLT - Data Phase B: Table + CSV + Query Methods
Write-Host "`n  VOLT - Installing Table + CSV...`n" -ForegroundColor Cyan

$f0 = @'
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
'@
[System.IO.File]::WriteAllText("$PWD/src/lexer/token.rs", $f0)
Write-Host "  [OK] src/lexer/token.rs" -ForegroundColor Green

$f1 = @'
pub mod token;

use self::token::{Token, Spanned};

pub struct Lexer {
    chars: Vec<char>,
    pos: usize,
    line: usize,
    col: usize,
    indent_stack: Vec<usize>,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self { chars: source.chars().collect(), pos: 0, line: 1, col: 1, indent_stack: vec![0] }
    }

    fn at_end(&self) -> bool { self.pos >= self.chars.len() }
    fn current(&self) -> char { self.chars[self.pos] }
    fn peek(&self) -> Option<char> { self.chars.get(self.pos + 1).copied() }
    fn advance(&mut self) -> char {
        let ch = self.chars[self.pos]; self.pos += 1; self.col += 1; ch
    }
    fn make(&self, token: Token, line: usize, col: usize) -> Spanned { Spanned { token, line, col } }
    fn skip_inline_whitespace(&mut self) { while !self.at_end() && self.current() == ' ' { self.advance(); } }
    fn skip_comment(&mut self) { while !self.at_end() && self.current() != '\n' { self.pos += 1; } }

    fn read_string(&mut self) -> Spanned {
        let (line, col) = (self.line, self.col);
        self.advance();
        let mut s = String::new();
        while !self.at_end() && self.current() != '"' && self.current() != '\n' {
            if self.current() == '\\' {
                self.advance();
                if !self.at_end() {
                    match self.current() {
                        'n' => s.push('\n'), 't' => s.push('\t'),
                        '\\' => s.push('\\'), '"' => s.push('"'),
                        other => { s.push('\\'); s.push(other); }
                    }
                    self.advance();
                }
            } else { s.push(self.advance()); }
        }
        if !self.at_end() && self.current() == '"' { self.advance(); }
        self.make(Token::Str(s), line, col)
    }

    fn read_number(&mut self) -> Spanned {
        let (line, col) = (self.line, self.col);
        let mut num = String::new();
        let mut is_float = false;
        while !self.at_end() && (self.current().is_ascii_digit() || self.current() == '.') {
            if self.current() == '.' {
                if is_float { break; }
                match self.peek() { Some(c) if c.is_ascii_digit() => is_float = true, _ => break }
            }
            num.push(self.advance());
        }
        let token = if is_float { Token::Float(num.parse().unwrap_or(0.0)) }
                    else { Token::Int(num.parse().unwrap_or(0)) };
        self.make(token, line, col)
    }

    fn read_ident(&mut self) -> Spanned {
        let (line, col) = (self.line, self.col);
        let mut name = String::new();
        while !self.at_end() && (self.current().is_alphanumeric() || self.current() == '_') {
            name.push(self.advance());
        }
        let token = match name.as_str() {
            "fn" => Token::Fn, "if" => Token::If, "elif" => Token::Elif,
            "else" => Token::Else, "for" => Token::For, "while" => Token::While,
            "return" => Token::Return, "struct" => Token::Struct, "mut" => Token::Mut,
            "pub" => Token::Pub, "use" => Token::Use, "match" => Token::Match,
            "mod" => Token::Mod, "true" => Token::True, "false" => Token::False,
            "in" => Token::In, "or" => Token::Or, "and" => Token::And, "not" => Token::Not,
            "try" => Token::Try, "catch" => Token::Catch, "throw" => Token::Throw,
            "break" => Token::Break, "continue" => Token::Continue,
            _ => Token::Ident(name),
        };
        self.make(token, line, col)
    }

    fn read_fstring(&mut self) -> Vec<Spanned> {
        let (line, col) = (self.line, self.col);
        self.advance(); self.advance();
        let mut result = vec![self.make(Token::FStringStart, line, col)];
        loop {
            if self.at_end() { break; }
            if self.current() == '"' {
                let (el, ec) = (self.line, self.col); self.advance();
                result.push(self.make(Token::FStringEnd, el, ec)); break;
            }
            if self.current() == '{' {
                let (el, ec) = (self.line, self.col); self.advance();
                result.push(self.make(Token::FStringExprStart, el, ec));
                let mut expr_str = String::new();
                let mut depth = 1;
                while !self.at_end() && depth > 0 {
                    let c = self.current();
                    if c == '{' { depth += 1; }
                    if c == '}' { depth -= 1; if depth == 0 { break; } }
                    expr_str.push(self.advance());
                }
                if !expr_str.is_empty() {
                    let mut sub = Lexer::new(&expr_str);
                    for t in sub.tokenize() {
                        match t.token {
                            Token::EOF | Token::Newline | Token::Indent | Token::Dedent => {}
                            _ => result.push(Spanned { token: t.token, line: el, col: ec }),
                        }
                    }
                }
                if !self.at_end() && self.current() == '}' {
                    let (el2, ec2) = (self.line, self.col); self.advance();
                    result.push(self.make(Token::FStringExprEnd, el2, ec2));
                }
                continue;
            }
            let (tl, tc) = (self.line, self.col);
            let mut text = String::new();
            while !self.at_end() && self.current() != '{' && self.current() != '"' && self.current() != '\n' {
                if self.current() == '\\' {
                    self.advance();
                    if !self.at_end() {
                        match self.current() {
                            'n' => text.push('\n'), 't' => text.push('\t'),
                            '\\' => text.push('\\'), '"' => text.push('"'),
                            '{' => text.push('{'), '}' => text.push('}'),
                            other => { text.push('\\'); text.push(other); }
                        }
                        self.advance();
                    }
                } else { text.push(self.advance()); }
            }
            if !text.is_empty() {
                result.push(self.make(Token::FStringText(text), tl, tc));
            }
        }
        result
    }

    pub fn tokenize(&mut self) -> Vec<Spanned> {
        let mut tokens: Vec<Spanned> = Vec::new();
        let mut at_line_start = true;

        while !self.at_end() {
            if at_line_start {
                let mut spaces: usize = 0;
                while !self.at_end() && (self.current() == ' ' || self.current() == '\t') {
                    if self.current() == '\t' { spaces += 4; } else { spaces += 1; }
                    self.pos += 1; self.col += 1;
                }
                if !self.at_end() && self.current() == '\r' { self.pos += 1; }
                if !self.at_end() && self.current() == '\n' {
                    self.pos += 1; self.line += 1; self.col = 1; continue;
                }
                if !self.at_end() && self.current() == '/' {
                    if self.peek() == Some('/') { self.skip_comment(); continue; }
                }
                if self.at_end() { break; }

                let current_indent = *self.indent_stack.last().unwrap();
                if spaces > current_indent {
                    self.indent_stack.push(spaces);
                    tokens.push(self.make(Token::Indent, self.line, 1));
                } else {
                    while spaces < *self.indent_stack.last().unwrap() {
                        self.indent_stack.pop();
                        tokens.push(self.make(Token::Dedent, self.line, 1));
                    }
                }
                at_line_start = false;
            }

            self.skip_inline_whitespace();
            if self.at_end() { break; }

            let (line, col) = (self.line, self.col);
            let ch = self.current();

            match ch {
                '\n' => { tokens.push(self.make(Token::Newline, line, col)); self.pos += 1; self.line += 1; self.col = 1; at_line_start = true; }
                '\r' => { self.pos += 1; }
                '/' if self.peek() == Some('/') => { self.skip_comment(); }
                '"' => tokens.push(self.read_string()),
                '0'..='9' => tokens.push(self.read_number()),

                'a'..='z' | 'A'..='Z' | '_' => {
                    if ch == 'f' && self.peek() == Some('"') {
                        tokens.extend(self.read_fstring());
                    } else {
                        tokens.push(self.read_ident());
                    }
                }

                '-' if self.peek() == Some('>') => { self.advance(); self.advance(); tokens.push(self.make(Token::Arrow, line, col)); }
                ':' if self.peek() == Some('=') => { self.advance(); self.advance(); tokens.push(self.make(Token::ColonEq, line, col)); }
                ':' if self.peek() == Some(':') => { self.advance(); self.advance(); tokens.push(self.make(Token::ColonColon, line, col)); }
                '=' if self.peek() == Some('=') => { self.advance(); self.advance(); tokens.push(self.make(Token::EqEq, line, col)); }
                '!' if self.peek() == Some('=') => { self.advance(); self.advance(); tokens.push(self.make(Token::NotEq, line, col)); }
                '<' if self.peek() == Some('=') => { self.advance(); self.advance(); tokens.push(self.make(Token::LtEq, line, col)); }
                '>' if self.peek() == Some('=') => { self.advance(); self.advance(); tokens.push(self.make(Token::GtEq, line, col)); }
                '.' if self.peek() == Some('.') => { self.advance(); self.advance(); tokens.push(self.make(Token::DotDot, line, col)); }

                '+' => { self.advance(); tokens.push(self.make(Token::Plus, line, col)); }
                '-' => { self.advance(); tokens.push(self.make(Token::Minus, line, col)); }
                '*' => { self.advance(); tokens.push(self.make(Token::Star, line, col)); }
                '/' => { self.advance(); tokens.push(self.make(Token::Slash, line, col)); }
                '%' => { self.advance(); tokens.push(self.make(Token::Percent, line, col)); }
                '=' => { self.advance(); tokens.push(self.make(Token::Eq, line, col)); }
                '<' => { self.advance(); tokens.push(self.make(Token::Lt, line, col)); }
                '>' => { self.advance(); tokens.push(self.make(Token::Gt, line, col)); }
                '.' => { self.advance(); tokens.push(self.make(Token::Dot, line, col)); }
                ':' => { self.advance(); tokens.push(self.make(Token::Colon, line, col)); }
                '?' => { self.advance(); tokens.push(self.make(Token::Question, line, col)); }
                '(' => { self.advance(); tokens.push(self.make(Token::LParen, line, col)); }
                ')' => { self.advance(); tokens.push(self.make(Token::RParen, line, col)); }
                '[' => { self.advance(); tokens.push(self.make(Token::LBracket, line, col)); }
                ']' => { self.advance(); tokens.push(self.make(Token::RBracket, line, col)); }
                '{' => { self.advance(); tokens.push(self.make(Token::LBrace, line, col)); }
                '}' => { self.advance(); tokens.push(self.make(Token::RBrace, line, col)); }
                ',' => { self.advance(); tokens.push(self.make(Token::Comma, line, col)); }
                '|' => {
                    self.advance();
                    if !self.at_end() && self.current() == '>' { self.advance(); tokens.push(self.make(Token::PipeArrow, line, col)); }
                    else { tokens.push(self.make(Token::Pipe, line, col)); }
                }
                _ => { self.advance(); }
            }
        }

        while self.indent_stack.len() > 1 {
            self.indent_stack.pop();
            tokens.push(self.make(Token::Dedent, self.line, 1));
        }
        tokens.push(self.make(Token::EOF, self.line, self.col));
        tokens
    }
}
'@
[System.IO.File]::WriteAllText("$PWD/src/lexer/mod.rs", $f1)
Write-Host "  [OK] src/lexer/mod.rs" -ForegroundColor Green

$f2 = @'
#[derive(Debug, Clone, PartialEq)]
pub enum BinOp { Add, Sub, Mul, Div, Mod, Eq, NotEq, Lt, Gt, LtEq, GtEq, And, Or, Range, Pipe }

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp { Neg, Not }

#[derive(Debug, Clone, PartialEq)]
pub enum Type { Named(String), Optional(Box<Type>), Array(Box<Type>), Generic(String, Vec<Type>) }

#[derive(Debug, Clone)]
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
'@
[System.IO.File]::WriteAllText("$PWD/src/parser/ast.rs", $f2)
Write-Host "  [OK] src/parser/ast.rs" -ForegroundColor Green

$f3 = @'
pub mod ast;

use crate::lexer::token::{Token, Spanned};
use self::ast::*;

pub struct Parser { tokens: Vec<Spanned>, pos: usize }

impl Parser {
    pub fn new(tokens: Vec<Spanned>) -> Self { Self { tokens, pos: 0 } }

    fn current(&self) -> &Token { &self.tokens[self.pos].token }
    fn peek(&self) -> &Token { let i = (self.pos + 1).min(self.tokens.len() - 1); &self.tokens[i].token }
    fn advance(&mut self) { if self.pos < self.tokens.len() - 1 { self.pos += 1; } }
    fn expect(&mut self, expected: Token) -> Result<(), String> {
        let c = self.current().clone();
        if c == expected { self.advance(); Ok(()) } else { Err(format!("{}:{}: expected {:?}, found {:?}", self.tokens[self.pos].line, self.tokens[self.pos].col, expected, c)) }
    }
    fn expect_ident(&mut self, what: &str) -> Result<String, String> {
        match self.current().clone() { Token::Ident(n) => { self.advance(); Ok(n) } _ => Err(self.err(&format!("expected {}", what))) }
    }
    fn skip_newlines(&mut self) { while matches!(self.current(), Token::Newline) { self.advance(); } }
    fn at_end(&self) -> bool { matches!(self.current(), Token::EOF) }
    fn loc(&self) -> (usize, usize) { (self.tokens[self.pos].line, self.tokens[self.pos].col) }
    fn err(&self, msg: &str) -> String { let (l, c) = self.loc(); format!("{}:{}: {}", l, c, msg) }

    pub fn parse(&mut self) -> Result<Program, String> {
        let mut stmts = Vec::new();
        self.skip_newlines();
        while !self.at_end() { stmts.push(self.parse_stmt()?); self.skip_newlines(); }
        Ok(stmts)
    }

    fn parse_block(&mut self) -> Result<Block, String> {
        self.expect(Token::Indent)?;
        let mut stmts = Vec::new();
        loop {
            self.skip_newlines();
            if matches!(self.current(), Token::Dedent | Token::EOF) { break; }
            stmts.push(self.parse_stmt()?);
        }
        if matches!(self.current(), Token::Dedent) { self.advance(); }
        Ok(stmts)
    }

    fn parse_stmt(&mut self) -> Result<Stmt, String> {
        match self.current().clone() {
            Token::Fn     => self.parse_fn(false),
            Token::Pub    => { self.advance(); if matches!(self.current(), Token::Fn) { self.parse_fn(true) } else { Err(self.err("expected 'fn' after 'pub'")) } }
            Token::Struct => self.parse_struct(),
            Token::If     => self.parse_if(),
            Token::For    => self.parse_for(),
            Token::While  => self.parse_while(),
            Token::Return => self.parse_return(),
            Token::Mut    => self.parse_mut_let(),
            Token::Try    => self.parse_try_catch(),
            Token::Throw  => self.parse_throw(),
            Token::Break  => { self.advance(); Ok(Stmt::Break) }
            Token::Continue => { self.advance(); Ok(Stmt::Continue) }
            Token::Match  => self.parse_match(),
            Token::Use    => self.parse_use(),
            _             => self.parse_expr_stmt(),
        }
    }

    fn parse_fn(&mut self, is_pub: bool) -> Result<Stmt, String> {
        self.advance();
        let name = self.expect_ident("function name")?;
        self.expect(Token::LParen)?;
        let params = self.parse_params()?;
        self.expect(Token::RParen)?;
        let ret_type = if matches!(self.current(), Token::Arrow) { self.advance(); Some(self.parse_type()?) } else { None };
        self.skip_newlines();
        let body = self.parse_block()?;
        Ok(Stmt::FnDef { name, params, ret_type, body, is_pub })
    }

    fn parse_params(&mut self) -> Result<Vec<Param>, String> {
        let mut params = Vec::new();
        if matches!(self.current(), Token::RParen) { return Ok(params); }
        loop {
            let name = if matches!(self.current(), Token::Mut) { self.advance(); format!("mut {}", self.expect_ident("param name")?) } else { self.expect_ident("param name")? };
            let typ = if matches!(self.current(), Token::Colon) { self.advance(); Some(self.parse_type()?) } else { None };
            let default = if matches!(self.current(), Token::Eq) { self.advance(); Some(self.parse_expr(0)?) } else { None };
            params.push(Param { name, typ, default });
            if matches!(self.current(), Token::Comma) { self.advance(); } else { break; }
        }
        Ok(params)
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        if matches!(self.current(), Token::Question) { self.advance(); return Ok(Type::Optional(Box::new(self.parse_type()?))); }
        if matches!(self.current(), Token::LBracket) { self.advance(); self.expect(Token::RBracket)?; return Ok(Type::Array(Box::new(self.parse_type()?))); }
        let name = self.expect_ident("type name")?;
        if matches!(self.current(), Token::LBracket) {
            self.advance();
            let mut args = Vec::new();
            loop { args.push(self.parse_type()?); if matches!(self.current(), Token::Comma) { self.advance(); } else { break; } }
            self.expect(Token::RBracket)?;
            Ok(Type::Generic(name, args))
        } else { Ok(Type::Named(name)) }
    }

    fn parse_struct(&mut self) -> Result<Stmt, String> {
        self.advance();
        let name = self.expect_ident("struct name")?;
        self.skip_newlines();
        self.expect(Token::Indent)?;
        let mut fields = Vec::new();
        let mut methods = Vec::new();
        loop {
            self.skip_newlines();
            if matches!(self.current(), Token::Dedent | Token::EOF) { break; }
            match self.current().clone() {
                Token::Fn => methods.push(self.parse_fn(false)?),
                Token::Pub => { self.advance(); if matches!(self.current(), Token::Fn) { methods.push(self.parse_fn(true)?); } else { return Err(self.err("expected 'fn' after 'pub'")); } }
                Token::Ident(field_name) => { self.advance(); self.expect(Token::Colon)?; let typ = self.parse_type()?; fields.push(Field { name: field_name, typ }); }
                _ => return Err(self.err("expected field or method in struct")),
            }
        }
        if matches!(self.current(), Token::Dedent) { self.advance(); }
        Ok(Stmt::Struct { name, fields, methods })
    }

    fn parse_if(&mut self) -> Result<Stmt, String> {
        self.advance();
        let cond = self.parse_expr(0)?;
        self.skip_newlines();
        let then_b = self.parse_block()?;
        let mut elifs = Vec::new();
        let mut else_b = None;
        loop {
            self.skip_newlines();
            match self.current().clone() {
                Token::Elif => { self.advance(); let c = self.parse_expr(0)?; self.skip_newlines(); elifs.push((c, self.parse_block()?)); }
                Token::Else => { self.advance(); self.skip_newlines(); else_b = Some(self.parse_block()?); break; }
                _ => break,
            }
        }
        Ok(Stmt::If { cond, then_b, elifs, else_b })
    }

    fn parse_for(&mut self) -> Result<Stmt, String> {
        self.advance(); let var = self.expect_ident("variable")?; self.expect(Token::In)?;
        let iter = self.parse_expr(0)?; self.skip_newlines(); let body = self.parse_block()?;
        Ok(Stmt::For { var, iter, body })
    }

    fn parse_while(&mut self) -> Result<Stmt, String> {
        self.advance(); let cond = self.parse_expr(0)?; self.skip_newlines(); let body = self.parse_block()?;
        Ok(Stmt::While { cond, body })
    }

    fn parse_return(&mut self) -> Result<Stmt, String> {
        self.advance();
        if matches!(self.current(), Token::Newline | Token::Dedent | Token::EOF) { Ok(Stmt::Return(None)) }
        else { Ok(Stmt::Return(Some(self.parse_expr(0)?))) }
    }

    fn parse_mut_let(&mut self) -> Result<Stmt, String> {
        self.advance(); let name = self.expect_ident("variable")?;
        if matches!(self.current(), Token::Colon) {
            self.advance();
            let typ = self.parse_type()?;
            self.expect(Token::ColonEq)?; let value = self.parse_expr(0)?;
            Ok(Stmt::Let { name, mutable: true, typ: Some(typ), value })
        } else {
            self.expect(Token::ColonEq)?; let value = self.parse_expr(0)?;
            Ok(Stmt::Let { name, mutable: true, typ: None, value })
        }
    }

    fn parse_use(&mut self) -> Result<Stmt, String> {
        self.advance();
        match self.current().clone() {
            Token::Str(path) => { self.advance(); Ok(Stmt::Use { path }) }
            _ => Err(self.err("expected string after 'use'"))
        }
    }

    fn parse_match(&mut self) -> Result<Stmt, String> {
        self.advance(); // skip 'match'
        let expr = self.parse_expr(0)?;
        self.skip_newlines();
        self.expect(Token::Indent)?;
        let mut arms = Vec::new();
        loop {
            self.skip_newlines();
            if matches!(self.current(), Token::Dedent | Token::EOF) { break; }
            arms.push(self.parse_match_arm()?);
        }
        if matches!(self.current(), Token::Dedent) { self.advance(); }
        Ok(Stmt::Match { expr, arms })
    }

    fn parse_match_arm(&mut self) -> Result<MatchArm, String> {
        let pattern = self.parse_pattern()?;
        let body = if matches!(self.current(), Token::Arrow) {
            self.advance();
            let expr = self.parse_expr(0)?;
            vec![Stmt::ExprStmt(expr)]
        } else {
            self.skip_newlines();
            self.parse_block()?
        };
        Ok(MatchArm { pattern, body })
    }

    fn parse_pattern(&mut self) -> Result<Pattern, String> {
        let mut pat = self.parse_single_pattern()?;
        if matches!(self.current(), Token::Pipe) {
            let mut pats = vec![pat];
            while matches!(self.current(), Token::Pipe) {
                self.advance();
                pats.push(self.parse_single_pattern()?);
            }
            pat = Pattern::Or(pats);
        }
        Ok(pat)
    }

    fn parse_single_pattern(&mut self) -> Result<Pattern, String> {
        match self.current().clone() {
            Token::Int(n) => { self.advance(); Ok(Pattern::Int(n)) }
            Token::Float(n) => { self.advance(); Ok(Pattern::Float(n)) }
            Token::Str(s) => { self.advance(); Ok(Pattern::Str(s)) }
            Token::True => { self.advance(); Ok(Pattern::Bool(true)) }
            Token::False => { self.advance(); Ok(Pattern::Bool(false)) }
            Token::Minus => {
                self.advance();
                match self.current().clone() {
                    Token::Int(n) => { self.advance(); Ok(Pattern::Int(-n)) }
                    Token::Float(n) => { self.advance(); Ok(Pattern::Float(-n)) }
                    _ => Err(self.err("expected number after '-' in pattern"))
                }
            }
            Token::Ident(name) if name == "_" => { self.advance(); Ok(Pattern::Wildcard) }
            Token::Ident(name) => { self.advance(); Ok(Pattern::Binding(name)) }
            _ => Err(self.err("expected pattern"))
        }
    }

    fn parse_try_catch(&mut self) -> Result<Stmt, String> {
        self.advance(); // skip 'try'
        self.skip_newlines();
        let try_body = self.parse_block()?;
        self.skip_newlines();
        if !matches!(self.current(), Token::Catch) { return Err(self.err("expected 'catch' after try block")); }
        self.advance(); // skip 'catch'
        let catch_var = self.expect_ident("catch variable")?;
        self.skip_newlines();
        let catch_body = self.parse_block()?;
        Ok(Stmt::TryCatch { try_body, catch_var, catch_body })
    }

    fn parse_throw(&mut self) -> Result<Stmt, String> {
        self.advance(); // skip 'throw'
        let expr = self.parse_expr(0)?;
        Ok(Stmt::Throw(expr))
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, String> {
        if let Token::Ident(name) = self.current().clone() {
            if matches!(self.peek(), Token::ColonEq) {
                self.advance(); self.advance();
                let value = self.parse_expr(0)?;
                return Ok(Stmt::Let { name, mutable: false, typ: None, value });
            }
            if matches!(self.peek(), Token::Colon) {
                // typed let: name: type := expr
                self.advance(); self.advance();
                let typ = self.parse_type()?;
                self.expect(Token::ColonEq)?;
                let value = self.parse_expr(0)?;
                return Ok(Stmt::Let { name, mutable: false, typ: Some(typ), value });
            }
        }
        let expr = self.parse_expr(0)?;
        if matches!(self.current(), Token::Eq) { self.advance(); let value = self.parse_expr(0)?; return Ok(Stmt::Assign { target: expr, value }); }
        Ok(Stmt::ExprStmt(expr))
    }

    fn parse_expr(&mut self, min_bp: u8) -> Result<Expr, String> {
        let mut left = self.parse_atom()?;
        loop {
            if matches!(self.current(), Token::LParen) && min_bp <= 20 { left = self.parse_call(left)?; continue; }
            if matches!(self.current(), Token::Dot) && min_bp <= 20 {
                self.advance(); let name = self.expect_ident("field name")?;
                if matches!(self.current(), Token::LParen) { left = self.parse_call(Expr::Field { object: Box::new(left), name })?; }
                else { left = Expr::Field { object: Box::new(left), name }; }
                continue;
            }
            if matches!(self.current(), Token::LBracket) && min_bp <= 20 {
                self.advance(); let index = self.parse_expr(0)?; self.expect(Token::RBracket)?;
                left = Expr::Index { object: Box::new(left), index: Box::new(index) };
                continue;
            }
            let (op, l_bp, r_bp) = match self.current() {
                Token::PipeArrow => (BinOp::Pipe,  1,  2),
                Token::Or      => (BinOp::Or,    3,  4),  Token::And     => (BinOp::And,   5,  6),
                Token::EqEq    => (BinOp::Eq,    7,  8),  Token::NotEq   => (BinOp::NotEq, 7,  8),
                Token::Lt      => (BinOp::Lt,    9, 10),  Token::Gt      => (BinOp::Gt,    9, 10),
                Token::LtEq    => (BinOp::LtEq,  9, 10),  Token::GtEq    => (BinOp::GtEq,  9, 10),
                Token::DotDot  => (BinOp::Range, 11, 12),
                Token::Plus    => (BinOp::Add,   13, 14),  Token::Minus   => (BinOp::Sub,   13, 14),
                Token::Star    => (BinOp::Mul,   15, 16),  Token::Slash   => (BinOp::Div,   15, 16),
                Token::Percent => (BinOp::Mod,   15, 16),
                _ => break,
            };
            if l_bp < min_bp { break; }
            self.advance();
            // Skip newlines after |> so pipelines can span multiple lines
            if op == BinOp::Pipe { self.skip_newlines(); }
            let right = self.parse_expr(r_bp)?;
            // Desugar: x |> f(a, b) → f(x, a, b)
            if op == BinOp::Pipe {
                left = match right {
                    Expr::Call { func, mut args } => {
                        args.insert(0, left);
                        Expr::Call { func, args }
                    }
                    Expr::Ident(_) => {
                        Expr::Call { func: Box::new(right), args: vec![left] }
                    }
                    _ => Expr::BinOp { left: Box::new(left), op, right: Box::new(right) },
                };
                continue;
            }
            left = Expr::BinOp { left: Box::new(left), op, right: Box::new(right) };
        }
        Ok(left)
    }

    fn parse_atom(&mut self) -> Result<Expr, String> {
        let tok = self.current().clone();
        match tok {
            Token::Int(n)   => { self.advance(); Ok(Expr::Int(n)) }
            Token::Float(n) => { self.advance(); Ok(Expr::Float(n)) }
            Token::Str(s)   => { self.advance(); Ok(Expr::Str(s)) }
            Token::True     => { self.advance(); Ok(Expr::Bool(true)) }
            Token::False    => { self.advance(); Ok(Expr::Bool(false)) }
            Token::Ident(n) => { self.advance(); Ok(Expr::Ident(n)) }
            Token::Minus    => { self.advance(); let e = self.parse_expr(16)?; Ok(Expr::UnaryOp { op: UnaryOp::Neg, expr: Box::new(e) }) }
            Token::Not      => { self.advance(); let e = self.parse_expr(16)?; Ok(Expr::UnaryOp { op: UnaryOp::Not, expr: Box::new(e) }) }
            Token::LParen   => { self.advance(); let e = self.parse_expr(0)?; self.expect(Token::RParen)?; Ok(e) }
            Token::LBracket => {
                self.advance();
                let mut items = Vec::new();
                if !matches!(self.current(), Token::RBracket) {
                    loop { items.push(self.parse_expr(0)?); if matches!(self.current(), Token::Comma) { self.advance(); } else { break; } }
                }
                self.expect(Token::RBracket)?;
                Ok(Expr::Array(items))
            }

            // Map literal: {key: val, ...}
            Token::LBrace => {
                self.advance();
                let mut entries = Vec::new();
                self.skip_newlines();
                if !matches!(self.current(), Token::RBrace) {
                    loop {
                        self.skip_newlines();
                        let key = self.expect_ident("map key")?;
                        self.expect(Token::Colon)?;
                        let val = self.parse_expr(0)?;
                        entries.push((key, val));
                        self.skip_newlines();
                        if matches!(self.current(), Token::Comma) { self.advance(); } else { break; }
                    }
                }
                self.skip_newlines();
                self.expect(Token::RBrace)?;
                Ok(Expr::Map(entries))
            }

            // F-String
            Token::FStringStart => {
                self.advance();
                let mut parts = Vec::new();
                loop {
                    match self.current().clone() {
                        Token::FStringEnd => { self.advance(); break; }
                        Token::FStringText(s) => { self.advance(); parts.push(Expr::Str(s)); }
                        Token::FStringExprStart => {
                            self.advance();
                            parts.push(self.parse_expr(0)?);
                            if matches!(self.current(), Token::FStringExprEnd) { self.advance(); }
                        }
                        Token::EOF => break,
                        _ => break,
                    }
                }
                Ok(Expr::FString(parts))
            }

            // Lambda: |params| expr  OR  |params|\n block
            Token::Pipe => {
                self.advance();
                let mut params = Vec::new();
                if !matches!(self.current(), Token::Pipe) {
                    loop {
                        params.push(self.expect_ident("parameter")?);
                        if matches!(self.current(), Token::Comma) { self.advance(); } else { break; }
                    }
                }
                self.expect(Token::Pipe)?;
                if matches!(self.current(), Token::Newline) {
                    self.skip_newlines();
                    let body = self.parse_block()?;
                    Ok(Expr::Lambda { params, body })
                } else {
                    let expr = self.parse_expr(0)?;
                    Ok(Expr::Lambda { params, body: vec![Stmt::Return(Some(expr))] })
                }
            }

            other => Err(self.err(&format!("unexpected {:?}", other))),
        }
    }

    fn parse_call(&mut self, func: Expr) -> Result<Expr, String> {
        self.advance();
        let mut args = Vec::new();
        if !matches!(self.current(), Token::RParen) {
            loop { args.push(self.parse_expr(0)?); if matches!(self.current(), Token::Comma) { self.advance(); } else { break; } }
        }
        self.expect(Token::RParen)?;
        Ok(Expr::Call { func: Box::new(func), args })
    }
}
'@
[System.IO.File]::WriteAllText("$PWD/src/parser/mod.rs", $f3)
Write-Host "  [OK] src/parser/mod.rs" -ForegroundColor Green

$f4 = @'
#![allow(dead_code)]
use std::collections::HashMap;
use crate::parser::ast::*;
use super::value::*;

// ── Opcodes ─────────────────────────────────
pub const OP_CONSTANT: u8 = 0;
pub const OP_NIL: u8 = 1;
pub const OP_TRUE: u8 = 2;
pub const OP_FALSE: u8 = 3;
pub const OP_POP: u8 = 4;
pub const OP_GET_LOCAL: u8 = 5;
pub const OP_SET_LOCAL: u8 = 6;
pub const OP_ADD: u8 = 10;
pub const OP_SUB: u8 = 11;
pub const OP_MUL: u8 = 12;
pub const OP_DIV: u8 = 13;
pub const OP_MOD: u8 = 14;
pub const OP_NEGATE: u8 = 15;
pub const OP_NOT: u8 = 16;
pub const OP_EQ: u8 = 20;
pub const OP_NEQ: u8 = 21;
pub const OP_LT: u8 = 22;
pub const OP_GT: u8 = 23;
pub const OP_LTE: u8 = 24;
pub const OP_GTE: u8 = 25;
pub const OP_JUMP: u8 = 30;
pub const OP_LOOP: u8 = 31;
pub const OP_JUMP_IF_FALSE: u8 = 32;
pub const OP_CALL: u8 = 40;
pub const OP_RETURN: u8 = 41;
pub const OP_PRINT: u8 = 50;
pub const OP_SQRT: u8 = 51;
pub const OP_ABS: u8 = 52;
pub const OP_LEN: u8 = 53;
pub const OP_MIN: u8 = 54;
pub const OP_MAX: u8 = 55;
pub const OP_TO_STR: u8 = 56;
pub const OP_BUILD_STR: u8 = 57;
pub const OP_NEW_ARRAY: u8 = 60;
pub const OP_GET_INDEX: u8 = 61;
pub const OP_NEW_STRUCT: u8 = 62;
pub const OP_GET_FIELD: u8 = 63;
pub const OP_INVOKE: u8 = 64;
pub const OP_SET_INDEX: u8 = 65;
pub const OP_CLOSURE: u8 = 70;
pub const OP_CALL_VALUE: u8 = 71;
pub const OP_GET_UPVALUE: u8 = 72;
pub const OP_TRY: u8 = 80;
pub const OP_END_TRY: u8 = 81;
pub const OP_THROW: u8 = 82;
pub const OP_INT: u8 = 83;
pub const OP_FLOAT: u8 = 84;
pub const OP_TYPE: u8 = 85;
pub const OP_INPUT: u8 = 86;
pub const OP_CHECK_TYPE: u8 = 87;
pub const OP_NEW_MAP: u8 = 88;
pub const OP_READ_FILE: u8 = 89;
pub const OP_WRITE_FILE: u8 = 90;
pub const OP_APPEND_FILE: u8 = 91;
pub const OP_LOAD_CSV: u8 = 92;
pub const OP_SAVE_CSV: u8 = 93;

pub struct Function { pub name: String, pub arity: u8, pub code: Vec<u8>, pub constants: Vec<Value> }
pub struct StructDef { pub name: String, pub field_names: Vec<String>, pub methods: HashMap<String, usize> }

#[derive(Clone)]
struct Local { name: String, depth: usize }
struct Upvalue { name: String, outer_slot: u8 }

pub struct Compiler {
    pub functions: Vec<Function>,
    pub struct_defs: Vec<StructDef>,
    pub heap: Heap,
    fn_map: HashMap<String, usize>,
    struct_map: HashMap<String, usize>,
    current_fn: usize,
    locals: Vec<Local>,
    scope_depth: usize,
    upvalues: Vec<Upvalue>,
    outer_locals: Option<Vec<Local>>,
    loop_breaks: Vec<Vec<usize>>,
    loop_continues: Vec<Vec<usize>>,
}

impl Compiler {
    fn emit(&mut self, b: u8) { self.functions[self.current_fn].code.push(b); }
    fn emit2(&mut self, a: u8, b: u8) { self.emit(a); self.emit(b); }

    fn add_constant(&mut self, val: Value) -> u8 {
        let cs = &mut self.functions[self.current_fn].constants;
        for (i, c) in cs.iter().enumerate() { if c.0 == val.0 { return i as u8; } }
        let i = cs.len(); cs.push(val); i as u8
    }
    fn emit_constant(&mut self, val: Value) { let i = self.add_constant(val); self.emit2(OP_CONSTANT, i); }
    fn add_string_constant(&mut self, s: &str) -> u8 {
        let oi = self.heap.alloc(Obj::Str(s.to_string()));
        self.add_constant(Value::obj(oi))
    }
    fn emit_jump(&mut self, op: u8) -> usize {
        self.emit(op); self.emit(0xFF); self.emit(0xFF);
        self.functions[self.current_fn].code.len() - 2
    }
    fn patch_jump(&mut self, offset: usize) {
        let code = &mut self.functions[self.current_fn].code;
        let jump = code.len() - offset - 2;
        code[offset] = ((jump >> 8) & 0xFF) as u8;
        code[offset + 1] = (jump & 0xFF) as u8;
    }
    fn emit_loop(&mut self, start: usize) {
        self.emit(OP_LOOP);
        let len = self.functions[self.current_fn].code.len();
        let offset = len + 2 - start;
        self.emit(((offset >> 8) & 0xFF) as u8); self.emit((offset & 0xFF) as u8);
    }
    fn current_offset(&self) -> usize { self.functions[self.current_fn].code.len() }
    fn begin_scope(&mut self) { self.scope_depth += 1; }
    fn end_scope(&mut self) {
        self.scope_depth -= 1;
        while let Some(local) = self.locals.last() {
            if local.depth <= self.scope_depth { break; }
            self.emit(OP_POP); self.locals.pop();
        }
    }
    fn add_local(&mut self, name: &str) { self.locals.push(Local { name: name.to_string(), depth: self.scope_depth }); }
    fn resolve_local(&self, name: &str) -> Option<u8> {
        for (i, l) in self.locals.iter().enumerate().rev() { if l.name == name { return Some(i as u8); } }
        None
    }
    fn resolve_upvalue(&mut self, name: &str) -> Option<u8> {
        if let Some(ref outers) = self.outer_locals {
            for (i, l) in outers.iter().enumerate().rev() {
                if l.name == name {
                    for (ui, uv) in self.upvalues.iter().enumerate() {
                        if uv.name == name { return Some(ui as u8); }
                    }
                    let ui = self.upvalues.len();
                    self.upvalues.push(Upvalue { name: name.to_string(), outer_slot: i as u8 });
                    return Some(ui as u8);
                }
            }
        }
        None
    }

    pub fn compile(program: &Program) -> Result<Self, String> {
        let mut c = Self {
            functions: vec![Function { name: "__entry".into(), arity: 0, code: vec![], constants: vec![] }],
            struct_defs: vec![], heap: Heap::new(),
            fn_map: HashMap::new(), struct_map: HashMap::new(),
            current_fn: 0, locals: vec![], scope_depth: 0,
            upvalues: Vec::new(), outer_locals: None,
            loop_breaks: Vec::new(), loop_continues: Vec::new(),
        };
        // Pass 1: register
        for stmt in program {
            match stmt {
                Stmt::FnDef { name, params, .. } => {
                    let idx = c.functions.len();
                    c.functions.push(Function { name: name.clone(), arity: params.len() as u8, code: vec![], constants: vec![] });
                    c.fn_map.insert(name.clone(), idx);
                }
                Stmt::Struct { name, fields, methods } => {
                    let sidx = c.struct_defs.len();
                    let fnames: Vec<String> = fields.iter().map(|f| f.name.clone()).collect();
                    let mut mmap = HashMap::new();
                    for m in methods {
                        if let Stmt::FnDef { name: mn, params, .. } = m {
                            let fidx = c.functions.len();
                            c.functions.push(Function { name: format!("{}::{}", name, mn), arity: params.len() as u8, code: vec![], constants: vec![] });
                            mmap.insert(mn.clone(), fidx);
                        }
                    }
                    c.struct_defs.push(StructDef { name: name.clone(), field_names: fnames, methods: mmap });
                    c.struct_map.insert(name.clone(), sidx);
                }
                _ => {}
            }
        }
        // Pass 2: compile functions
        for stmt in program {
            match stmt {
                Stmt::FnDef { name, params, body, .. } => { let idx = c.fn_map[name]; c.compile_fn(idx, params, body)?; }
                Stmt::Struct { name, methods, .. } => {
                    let sidx = c.struct_map[name];
                    for m in methods {
                        if let Stmt::FnDef { name: mn, params, body, .. } = m {
                            let fidx = c.struct_defs[sidx].methods[mn];
                            c.compile_fn(fidx, params, body)?;
                        }
                    }
                }
                _ => {}
            }
        }
        // Pass 3: entry
        c.current_fn = 0; c.locals = vec![]; c.scope_depth = 0;
        if let Some(&main_idx) = c.fn_map.get("main") {
            c.emit(OP_CALL); c.emit(main_idx as u8); c.emit(0); c.emit(OP_POP);
        } else {
            for stmt in program { match stmt { Stmt::FnDef { .. } | Stmt::Struct { .. } => {} _ => c.compile_stmt(stmt)? } }
        }
        c.emit(OP_NIL); c.emit(OP_RETURN);
        Ok(c)
    }

    fn compile_fn(&mut self, fn_idx: usize, params: &[Param], body: &Block) -> Result<(), String> {
        let prev = (self.current_fn, std::mem::take(&mut self.locals), self.scope_depth);
        self.current_fn = fn_idx; self.scope_depth = 0; self.locals = Vec::new();
        for p in params { self.locals.push(Local { name: p.name.clone(), depth: 0 }); }
        // Emit type checks for typed params
        for (i, p) in params.iter().enumerate() {
            if let Some(typ) = &p.typ {
                let tn = self.type_to_string(typ);
                let ci = self.add_string_constant(&tn);
                self.emit(OP_CHECK_TYPE); self.emit(i as u8); self.emit(ci);
            }
        }
        let len = body.len();
        for (i, stmt) in body.iter().enumerate() {
            if i == len - 1 {
                if let Stmt::ExprStmt(e) = stmt {
                    self.compile_expr(e)?; self.emit(OP_RETURN);
                    self.current_fn = prev.0; self.locals = prev.1; self.scope_depth = prev.2;
                    return Ok(());
                }
            }
            self.compile_stmt(stmt)?;
        }
        let code = &self.functions[self.current_fn].code;
        if code.is_empty() || *code.last().unwrap() != OP_RETURN { self.emit(OP_NIL); self.emit(OP_RETURN); }
        self.current_fn = prev.0; self.locals = prev.1; self.scope_depth = prev.2;
        Ok(())
    }

    // ── Lambda / Closure compilation ────────────
    fn compile_lambda(&mut self, params: &[String], body: &Block) -> Result<(), String> {
        let fn_idx = self.functions.len();
        self.functions.push(Function {
            name: format!("__lambda_{}", fn_idx), arity: params.len() as u8,
            code: vec![], constants: vec![],
        });
        // Save state
        let prev_fn = self.current_fn;
        let prev_locals = std::mem::take(&mut self.locals);
        let prev_depth = self.scope_depth;
        let prev_upvalues = std::mem::take(&mut self.upvalues);
        let prev_outer = self.outer_locals.take();

        self.current_fn = fn_idx;
        self.scope_depth = 0;
        self.locals = Vec::new();
        self.upvalues = Vec::new();
        self.outer_locals = Some(prev_locals.clone());

        for p in params { self.locals.push(Local { name: p.clone(), depth: 0 }); }

        // Compile body (with implicit return for last expression)
        let len = body.len();
        for (i, stmt) in body.iter().enumerate() {
            if i == len - 1 {
                if let Stmt::ExprStmt(e) = stmt {
                    self.compile_expr(e)?; self.emit(OP_RETURN);
                    let upvalues = std::mem::take(&mut self.upvalues);
                    self.current_fn = prev_fn; self.locals = prev_locals;
                    self.scope_depth = prev_depth; self.upvalues = prev_upvalues;
                    self.outer_locals = prev_outer;
                    return self.emit_closure(fn_idx, &upvalues);
                }
            }
            self.compile_stmt(stmt)?;
        }
        let needs_ret = {
            let code = &self.functions[fn_idx].code;
            code.is_empty() || *code.last().unwrap() != OP_RETURN
        };
        if needs_ret { self.emit(OP_NIL); self.emit(OP_RETURN); }

        let upvalues = std::mem::take(&mut self.upvalues);
        self.current_fn = prev_fn; self.locals = prev_locals;
        self.scope_depth = prev_depth; self.upvalues = prev_upvalues;
        self.outer_locals = prev_outer;
        self.emit_closure(fn_idx, &upvalues)
    }

    fn emit_closure(&mut self, fn_idx: usize, upvalues: &[Upvalue]) -> Result<(), String> {
        let ci = self.add_constant(Value::number(fn_idx as f64));
        self.emit(OP_CLOSURE); self.emit(ci); self.emit(upvalues.len() as u8);
        for uv in upvalues { self.emit(uv.outer_slot); }
        Ok(())
    }

    // ── Statements ──────────────────────────────
    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Let { name, value, typ, .. } => {
                self.compile_expr(value)?;
                self.add_local(name);
                if let Some(t) = typ {
                    let slot = self.resolve_local(name).unwrap();
                    let tn = self.type_to_string(t);
                    let ci = self.add_string_constant(&tn);
                    self.emit(OP_CHECK_TYPE); self.emit(slot); self.emit(ci);
                }
                Ok(())
            }
            Stmt::Assign { target, value } => {
                match target {
                    Expr::Ident(name) => {
                        self.compile_expr(value)?;
                        let idx = self.resolve_local(name).ok_or_else(|| format!("undefined '{}'", name))?;
                        self.emit2(OP_SET_LOCAL, idx);
                    }
                    Expr::Index { object, index } => {
                        self.compile_expr(object)?;
                        self.compile_expr(index)?;
                        self.compile_expr(value)?;
                        self.emit(OP_SET_INDEX);
                    }
                    _ => return Err("invalid assignment target".into()),
                }
                Ok(())
            }
            Stmt::If { cond, then_b, elifs, else_b } => {
                self.compile_expr(cond)?;
                let then_jmp = self.emit_jump(OP_JUMP_IF_FALSE);
                self.emit(OP_POP);
                self.begin_scope(); for s in then_b { self.compile_stmt(s)?; } self.end_scope();
                let else_jmp = self.emit_jump(OP_JUMP);
                self.patch_jump(then_jmp); self.emit(OP_POP);
                let mut end_jumps = vec![else_jmp];
                for (ec, eb) in elifs {
                    self.compile_expr(ec)?;
                    let ej = self.emit_jump(OP_JUMP_IF_FALSE); self.emit(OP_POP);
                    self.begin_scope(); for s in eb { self.compile_stmt(s)?; } self.end_scope();
                    end_jumps.push(self.emit_jump(OP_JUMP));
                    self.patch_jump(ej); self.emit(OP_POP);
                }
                if let Some(eb) = else_b { self.begin_scope(); for s in eb { self.compile_stmt(s)?; } self.end_scope(); }
                for ej in end_jumps { self.patch_jump(ej); }
                Ok(())
            }
            Stmt::For { var, iter, body } => {
                if let Expr::BinOp { left, op, right } = iter {
                    if *op == BinOp::Range { return self.compile_for_range(var, left, right, body); }
                }
                self.compile_for_array(var, iter, body)
            }
            Stmt::While { cond, body } => {
                let loop_start = self.current_offset();
                self.loop_breaks.push(Vec::new());
                self.loop_continues.push(Vec::new());
                self.compile_expr(cond)?;
                let exit = self.emit_jump(OP_JUMP_IF_FALSE); self.emit(OP_POP);
                self.begin_scope(); for s in body { self.compile_stmt(s)?; } self.end_scope();
                // continue jumps here (before loop back)
                let continues = self.loop_continues.pop().unwrap();
                for c in continues { self.patch_jump(c); }
                self.emit_loop(loop_start); self.patch_jump(exit); self.emit(OP_POP);
                let breaks = self.loop_breaks.pop().unwrap();
                for b in breaks { self.patch_jump(b); }
                Ok(())
            }
            Stmt::Return(expr) => { match expr { Some(e) => self.compile_expr(e)?, None => self.emit(OP_NIL) } self.emit(OP_RETURN); Ok(()) }
            Stmt::ExprStmt(e) => { self.compile_expr(e)?; self.emit(OP_POP); Ok(()) }
            Stmt::TryCatch { try_body, catch_var, catch_body } => {
                // OP_TRY <catch_offset_hi> <catch_offset_lo>
                let try_jmp = self.emit_jump(OP_TRY);
                self.begin_scope();
                for s in try_body { self.compile_stmt(s)?; }
                self.end_scope();
                self.emit(OP_END_TRY);
                let end_jmp = self.emit_jump(OP_JUMP);
                // Catch block: error string is on stack
                self.patch_jump(try_jmp);
                self.begin_scope();
                self.add_local(catch_var); // error value already on stack
                for s in catch_body { self.compile_stmt(s)?; }
                self.end_scope();
                self.patch_jump(end_jmp);
                Ok(())
            }
            Stmt::Throw(expr) => {
                self.compile_expr(expr)?;
                self.emit(OP_THROW);
                Ok(())
            }
            Stmt::Break => {
                if self.loop_breaks.is_empty() { return Err("break outside loop".into()); }
                let jmp = self.emit_jump(OP_JUMP);
                self.loop_breaks.last_mut().unwrap().push(jmp);
                Ok(())
            }
            Stmt::Continue => {
                if self.loop_continues.is_empty() { return Err("continue outside loop".into()); }
                let jmp = self.emit_jump(OP_JUMP);
                self.loop_continues.last_mut().unwrap().push(jmp);
                Ok(())
            }
            Stmt::FnDef { name, params, body, .. } => {
                if self.fn_map.contains_key(name) {
                    return Ok(()); // top-level, already compiled in pass 2
                }
                // Nested function → compile as local closure
                let param_names: Vec<String> = params.iter().map(|p| p.name.clone()).collect();
                self.compile_lambda(&param_names, body)?;
                self.add_local(name);
                Ok(())
            }
            Stmt::Struct { .. } => Ok(()),
            Stmt::Match { expr, arms } => self.compile_match(expr, arms),
            Stmt::Use { .. } => Ok(()), // resolved before compilation
        }
    }

    fn compile_match(&mut self, expr: &Expr, arms: &[MatchArm]) -> Result<(), String> {
        self.begin_scope();
        self.compile_expr(expr)?;
        self.add_local("__match");
        let match_slot = self.resolve_local("__match").unwrap();
        let mut end_jumps = Vec::new();

        for arm in arms {
            match &arm.pattern {
                Pattern::Wildcard => {
                    self.begin_scope();
                    for s in &arm.body { self.compile_stmt(s)?; }
                    self.end_scope();
                    end_jumps.push(self.emit_jump(OP_JUMP));
                }
                Pattern::Binding(name) => {
                    self.begin_scope();
                    self.emit2(OP_GET_LOCAL, match_slot);
                    self.add_local(name);
                    for s in &arm.body { self.compile_stmt(s)?; }
                    self.end_scope();
                    end_jumps.push(self.emit_jump(OP_JUMP));
                }
                Pattern::Int(_) | Pattern::Float(_) | Pattern::Str(_) | Pattern::Bool(_) => {
                    self.emit2(OP_GET_LOCAL, match_slot);
                    self.emit_pattern_literal(&arm.pattern)?;
                    self.emit(OP_EQ);
                    let skip = self.emit_jump(OP_JUMP_IF_FALSE);
                    self.emit(OP_POP);
                    self.begin_scope();
                    for s in &arm.body { self.compile_stmt(s)?; }
                    self.end_scope();
                    end_jumps.push(self.emit_jump(OP_JUMP));
                    self.patch_jump(skip);
                    self.emit(OP_POP);
                }
                Pattern::Or(patterns) => {
                    let mut body_jumps = Vec::new();
                    for pat in patterns {
                        self.emit2(OP_GET_LOCAL, match_slot);
                        self.emit_pattern_literal(pat)?;
                        self.emit(OP_EQ);
                        let no = self.emit_jump(OP_JUMP_IF_FALSE);
                        self.emit(OP_POP);
                        body_jumps.push(self.emit_jump(OP_JUMP));
                        self.patch_jump(no);
                        self.emit(OP_POP);
                    }
                    let skip = self.emit_jump(OP_JUMP);
                    for bj in body_jumps { self.patch_jump(bj); }
                    self.begin_scope();
                    for s in &arm.body { self.compile_stmt(s)?; }
                    self.end_scope();
                    end_jumps.push(self.emit_jump(OP_JUMP));
                    self.patch_jump(skip);
                }
            }
        }
        for ej in end_jumps { self.patch_jump(ej); }
        self.end_scope();
        Ok(())
    }

    fn emit_pattern_literal(&mut self, pat: &Pattern) -> Result<(), String> {
        match pat {
            Pattern::Int(n) => self.emit_constant(Value::number(*n as f64)),
            Pattern::Float(n) => self.emit_constant(Value::number(*n)),
            Pattern::Str(s) => { let oi = self.heap.alloc(Obj::Str(s.clone())); self.emit_constant(Value::obj(oi)); }
            Pattern::Bool(true) => self.emit(OP_TRUE),
            Pattern::Bool(false) => self.emit(OP_FALSE),
            _ => return Err("expected literal pattern".into()),
        }
        Ok(())
    }

    fn type_to_string(&self, t: &Type) -> String {
        match t {
            Type::Named(n) => n.clone(),
            Type::Optional(inner) => format!("?{}", self.type_to_string(inner)),
            Type::Array(inner) => format!("[]{}", self.type_to_string(inner)),
            Type::Generic(n, _) => n.clone(),
        }
    }

    fn compile_for_range(&mut self, var: &str, start: &Expr, end: &Expr, body: &Block) -> Result<(), String> {
        self.begin_scope();
        self.compile_expr(start)?; self.add_local(var);
        self.compile_expr(end)?; self.add_local("__end");
        let loop_start = self.current_offset();
        let i_idx = self.resolve_local(var).unwrap();
        let end_idx = self.resolve_local("__end").unwrap();
        self.emit2(OP_GET_LOCAL, i_idx); self.emit2(OP_GET_LOCAL, end_idx); self.emit(OP_LT);
        let exit = self.emit_jump(OP_JUMP_IF_FALSE); self.emit(OP_POP);
        self.loop_breaks.push(Vec::new());
        self.loop_continues.push(Vec::new());
        self.begin_scope(); for s in body { self.compile_stmt(s)?; } self.end_scope();
        // continue jumps here (to increment)
        let continues = self.loop_continues.pop().unwrap();
        for c in continues { self.patch_jump(c); }
        self.emit2(OP_GET_LOCAL, i_idx); self.emit_constant(Value::number(1.0)); self.emit(OP_ADD);
        self.emit2(OP_SET_LOCAL, i_idx);
        self.emit_loop(loop_start); self.patch_jump(exit); self.emit(OP_POP);
        let breaks = self.loop_breaks.pop().unwrap();
        for b in breaks { self.patch_jump(b); }
        self.end_scope(); Ok(())
    }

    fn compile_for_array(&mut self, var: &str, iter: &Expr, body: &Block) -> Result<(), String> {
        self.begin_scope();
        self.compile_expr(iter)?; self.add_local("__arr");
        self.emit_constant(Value::number(0.0)); self.add_local("__idx");
        self.emit(OP_NIL); self.add_local(var);
        let loop_start = self.current_offset();
        let arr_i = self.resolve_local("__arr").unwrap();
        let idx_i = self.resolve_local("__idx").unwrap();
        let var_i = self.resolve_local(var).unwrap();
        self.emit2(OP_GET_LOCAL, idx_i); self.emit2(OP_GET_LOCAL, arr_i); self.emit(OP_LEN); self.emit(OP_LT);
        let exit = self.emit_jump(OP_JUMP_IF_FALSE); self.emit(OP_POP);
        self.emit2(OP_GET_LOCAL, arr_i); self.emit2(OP_GET_LOCAL, idx_i); self.emit(OP_GET_INDEX);
        self.emit2(OP_SET_LOCAL, var_i);
        self.loop_breaks.push(Vec::new());
        self.loop_continues.push(Vec::new());
        self.begin_scope(); for s in body { self.compile_stmt(s)?; } self.end_scope();
        // continue jumps here (to increment)
        let continues = self.loop_continues.pop().unwrap();
        for c in continues { self.patch_jump(c); }
        self.emit2(OP_GET_LOCAL, idx_i); self.emit_constant(Value::number(1.0)); self.emit(OP_ADD);
        self.emit2(OP_SET_LOCAL, idx_i);
        self.emit_loop(loop_start); self.patch_jump(exit); self.emit(OP_POP);
        let breaks = self.loop_breaks.pop().unwrap();
        for b in breaks { self.patch_jump(b); }
        self.end_scope(); Ok(())
    }

    // ── Expressions ─────────────────────────────
    fn compile_expr(&mut self, expr: &Expr) -> Result<(), String> {
        match expr {
            Expr::Int(n) => { self.emit_constant(Value::number(*n as f64)); Ok(()) }
            Expr::Float(n) => { self.emit_constant(Value::number(*n)); Ok(()) }
            Expr::Bool(true) => { self.emit(OP_TRUE); Ok(()) }
            Expr::Bool(false) => { self.emit(OP_FALSE); Ok(()) }
            Expr::Str(s) => { let oi = self.heap.alloc(Obj::Str(s.clone())); self.emit_constant(Value::obj(oi)); Ok(()) }
            Expr::Ident(name) => {
                if let Some(idx) = self.resolve_local(name) {
                    self.emit2(OP_GET_LOCAL, idx);
                } else if let Some(idx) = self.resolve_upvalue(name) {
                    self.emit2(OP_GET_UPVALUE, idx);
                } else if let Some(&fidx) = self.fn_map.get(name) {
                    // Named function as value → wrap in zero-capture closure
                    let ci = self.add_constant(Value::number(fidx as f64));
                    self.emit(OP_CLOSURE); self.emit(ci); self.emit(0);
                } else {
                    return Err(format!("undefined '{}'", name));
                }
                Ok(())
            }
            Expr::BinOp { left, op, right } => {
                match op {
                    BinOp::And => {
                        self.compile_expr(left)?;
                        let end = self.emit_jump(OP_JUMP_IF_FALSE); self.emit(OP_POP);
                        self.compile_expr(right)?; self.patch_jump(end); Ok(())
                    }
                    BinOp::Or => {
                        self.compile_expr(left)?;
                        let false_jmp = self.emit_jump(OP_JUMP_IF_FALSE);
                        let end_jmp = self.emit_jump(OP_JUMP);
                        self.patch_jump(false_jmp); self.emit(OP_POP);
                        self.compile_expr(right)?; self.patch_jump(end_jmp); Ok(())
                    }
                    _ => {
                        self.compile_expr(left)?; self.compile_expr(right)?;
                        match op {
                            BinOp::Add => self.emit(OP_ADD), BinOp::Sub => self.emit(OP_SUB),
                            BinOp::Mul => self.emit(OP_MUL), BinOp::Div => self.emit(OP_DIV),
                            BinOp::Mod => self.emit(OP_MOD), BinOp::Eq => self.emit(OP_EQ),
                            BinOp::NotEq => self.emit(OP_NEQ), BinOp::Lt => self.emit(OP_LT),
                            BinOp::Gt => self.emit(OP_GT), BinOp::LtEq => self.emit(OP_LTE),
                            BinOp::GtEq => self.emit(OP_GTE), _ => {}
                        }
                        Ok(())
                    }
                }
            }
            Expr::UnaryOp { op, expr } => {
                self.compile_expr(expr)?;
                match op { UnaryOp::Neg => self.emit(OP_NEGATE), UnaryOp::Not => self.emit(OP_NOT) }
                Ok(())
            }
            Expr::Call { func, args } => self.compile_call(func, args),
            Expr::Field { object, name } => {
                self.compile_expr(object)?;
                let idx = self.add_string_constant(name);
                self.emit2(OP_GET_FIELD, idx); Ok(())
            }
            Expr::Index { object, index } => {
                self.compile_expr(object)?; self.compile_expr(index)?;
                self.emit(OP_GET_INDEX); Ok(())
            }
            Expr::Array(items) => {
                for item in items { self.compile_expr(item)?; }
                self.emit2(OP_NEW_ARRAY, items.len() as u8); Ok(())
            }
            Expr::FString(parts) => {
                if parts.is_empty() {
                    let oi = self.heap.alloc(Obj::Str(String::new()));
                    self.emit_constant(Value::obj(oi));
                } else {
                    for part in parts { self.compile_expr(part)?; }
                    self.emit2(OP_BUILD_STR, parts.len() as u8);
                }
                Ok(())
            }
            Expr::Lambda { params, body } => self.compile_lambda(params, body),
            Expr::Map(entries) => {
                for (key, val) in entries {
                    let ki = self.heap.alloc(Obj::Str(key.clone()));
                    self.emit_constant(Value::obj(ki));
                    self.compile_expr(val)?;
                }
                self.emit2(OP_NEW_MAP, entries.len() as u8); Ok(())
            }
        }
    }

    fn compile_call(&mut self, func: &Expr, args: &[Expr]) -> Result<(), String> {
        match func {
            Expr::Ident(name) => match name.as_str() {
                "print" => { for a in args { self.compile_expr(a)?; } self.emit2(OP_PRINT, args.len() as u8); self.emit(OP_NIL); Ok(()) }
                "sqrt"  => { self.compile_expr(&args[0])?; self.emit(OP_SQRT); Ok(()) }
                "abs"   => { self.compile_expr(&args[0])?; self.emit(OP_ABS); Ok(()) }
                "len"   => { self.compile_expr(&args[0])?; self.emit(OP_LEN); Ok(()) }
                "min"   => { self.compile_expr(&args[0])?; self.compile_expr(&args[1])?; self.emit(OP_MIN); Ok(()) }
                "max"   => { self.compile_expr(&args[0])?; self.compile_expr(&args[1])?; self.emit(OP_MAX); Ok(()) }
                "str"   => { self.compile_expr(&args[0])?; self.emit(OP_TO_STR); Ok(()) }
                "int"   => { self.compile_expr(&args[0])?; self.emit(OP_INT); Ok(()) }
                "float" => { self.compile_expr(&args[0])?; self.emit(OP_FLOAT); Ok(()) }
                "type"  => { self.compile_expr(&args[0])?; self.emit(OP_TYPE); Ok(()) }
                "input" => { if args.is_empty() { self.emit(OP_NIL); } else { self.compile_expr(&args[0])?; } self.emit(OP_INPUT); Ok(()) }
                "read_file" => { self.compile_expr(&args[0])?; self.emit(OP_READ_FILE); Ok(()) }
                "write_file" => { self.compile_expr(&args[0])?; self.compile_expr(&args[1])?; self.emit(OP_WRITE_FILE); Ok(()) }
                "append_file" => { self.compile_expr(&args[0])?; self.compile_expr(&args[1])?; self.emit(OP_APPEND_FILE); Ok(()) }
                "load_csv" => { self.compile_expr(&args[0])?; self.emit(OP_LOAD_CSV); Ok(()) }
                "save_csv" => { self.compile_expr(&args[0])?; self.compile_expr(&args[1])?; self.emit(OP_SAVE_CSV); Ok(()) }
                _ => {
                    // Struct constructor
                    if let Some(&sidx) = self.struct_map.get(name) {
                        for a in args { self.compile_expr(a)?; }
                        self.emit(OP_NEW_STRUCT); self.emit(sidx as u8); self.emit(args.len() as u8);
                        return Ok(());
                    }
                    // Known function (direct call)
                    if let Some(&fidx) = self.fn_map.get(name) {
                        for a in args { self.compile_expr(a)?; }
                        self.emit(OP_CALL); self.emit(fidx as u8); self.emit(args.len() as u8);
                        return Ok(());
                    }
                    // Local variable → closure call
                    if let Some(local_idx) = self.resolve_local(name) {
                        self.emit2(OP_GET_LOCAL, local_idx);
                        for a in args { self.compile_expr(a)?; }
                        self.emit2(OP_CALL_VALUE, args.len() as u8);
                        return Ok(());
                    }
                    // Upvalue → closure call
                    if let Some(uv_idx) = self.resolve_upvalue(name) {
                        self.emit2(OP_GET_UPVALUE, uv_idx);
                        for a in args { self.compile_expr(a)?; }
                        self.emit2(OP_CALL_VALUE, args.len() as u8);
                        return Ok(());
                    }
                    Err(format!("undefined function '{}'", name))
                }
            }
            Expr::Field { object, name } => {
                self.compile_expr(object)?;
                for a in args { self.compile_expr(a)?; }
                let idx = self.add_string_constant(name);
                self.emit(OP_INVOKE); self.emit(idx); self.emit(args.len() as u8);
                Ok(())
            }
            // General expression call (e.g. lambda IIFE, indexed access)
            _ => {
                self.compile_expr(func)?;
                for a in args { self.compile_expr(a)?; }
                self.emit2(OP_CALL_VALUE, args.len() as u8);
                Ok(())
            }
        }
    }
}
'@
[System.IO.File]::WriteAllText("$PWD/src/vm/compiler.rs", $f4)
Write-Host "  [OK] src/vm/compiler.rs" -ForegroundColor Green

$f5 = @'
#![allow(dead_code)]

const QNAN: u64 = 0x7ffc_0000_0000_0000;
const SIGN: u64 = 0x8000_0000_0000_0000;

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Value(pub u64);

impl Value {
    pub const NIL: Value = Value(QNAN | 1);
    pub const FALSE: Value = Value(QNAN | 2);
    pub const TRUE: Value = Value(QNAN | 3);

    #[inline(always)] pub fn number(n: f64) -> Self { Value(n.to_bits()) }
    #[inline(always)] pub fn boolean(b: bool) -> Self { if b { Self::TRUE } else { Self::FALSE } }
    #[inline(always)] pub fn obj(idx: u32) -> Self { Value(SIGN | QNAN | idx as u64) }
    #[inline(always)] pub fn is_number(self) -> bool { (self.0 & QNAN) != QNAN }
    #[inline(always)] pub fn as_number(self) -> f64 { f64::from_bits(self.0) }
    #[inline(always)] pub fn is_obj(self) -> bool { (self.0 & (QNAN | SIGN)) == (QNAN | SIGN) }
    #[inline(always)] pub fn as_obj(self) -> u32 { self.0 as u32 }
    #[inline(always)] pub fn is_nil(self) -> bool { self.0 == Self::NIL.0 }
    #[inline(always)] pub fn is_bool(self) -> bool { self.0 == Self::TRUE.0 || self.0 == Self::FALSE.0 }

    #[inline(always)]
    pub fn is_truthy(self) -> bool {
        if self.is_number() { return self.as_number() != 0.0; }
        self.0 != Self::FALSE.0 && self.0 != Self::NIL.0
    }

    pub fn display(self, heap: &Heap) -> String {
        if self.is_number() {
            let n = self.as_number();
            if n == (n as i64) as f64 && n.abs() < 1e15 { format!("{}", n as i64) }
            else { format!("{}", n) }
        } else if self.0 == Self::TRUE.0 { "true".into() }
        else if self.0 == Self::FALSE.0 { "false".into() }
        else if self.0 == Self::NIL.0 { "none".into() }
        else if self.is_obj() {
            match heap.get(self.as_obj()) {
                Obj::Str(s) => s.clone(),
                Obj::Array(items) => {
                    let p: Vec<String> = items.iter().map(|v| v.display(heap)).collect();
                    format!("[{}]", p.join(", "))
                }
                Obj::Struct { .. } => "<struct>".into(),
                Obj::Closure { fn_idx, .. } => format!("<closure@{}>", fn_idx),
                Obj::Map(entries) => {
                    let p: Vec<String> = entries.iter().map(|(k, v)| format!("{}: {}", k, v.display(heap))).collect();
                    format!("{{{}}}", p.join(", "))
                }
                Obj::Table { columns, rows } => {
                    if rows.is_empty() { return format!("Table({}) [0 rows]", columns.join(", ")); }
                    // Column widths
                    let mut widths: Vec<usize> = columns.iter().map(|c| c.len()).collect();
                    let show = if rows.len() > 20 { 20 } else { rows.len() };
                    for row in rows.iter().take(show) {
                        for (j, v) in row.iter().enumerate() {
                            if j < widths.len() { widths[j] = widths[j].max(v.display(heap).len()); }
                        }
                    }
                    let mut out = String::new();
                    // Header
                    for (j, col) in columns.iter().enumerate() {
                        if j > 0 { out.push_str(" | "); }
                        out.push_str(&format!("{:width$}", col, width = widths[j]));
                    }
                    out.push('\n');
                    for (j, _) in columns.iter().enumerate() {
                        if j > 0 { out.push_str("-+-"); }
                        out.push_str(&"-".repeat(widths[j]));
                    }
                    out.push('\n');
                    // Rows
                    for row in rows.iter().take(show) {
                        for (j, v) in row.iter().enumerate() {
                            if j > 0 { out.push_str(" | "); }
                            let s = v.display(heap);
                            if j < widths.len() { out.push_str(&format!("{:width$}", s, width = widths[j])); }
                            else { out.push_str(&s); }
                        }
                        out.push('\n');
                    }
                    if rows.len() > show {
                        out.push_str(&format!("... ({} more rows)\n", rows.len() - show));
                    }
                    out.push_str(&format!("[{} rows x {} cols]", rows.len(), columns.len()));
                    out
                }
            }
        } else { "?".into() }
    }

    pub fn is_str(self, heap: &Heap) -> bool {
        self.is_obj() && matches!(heap.get(self.as_obj()), Obj::Str(_))
    }
}

pub enum Obj {
    Str(String),
    Array(Vec<Value>),
    Struct { type_idx: u16, fields: Vec<Value> },
    Closure { fn_idx: u32, captures: Vec<Value> },
    Map(Vec<(String, Value)>),
    Table { columns: Vec<String>, rows: Vec<Vec<Value>> },
}

pub struct Heap { objects: Vec<Obj> }
impl Heap {
    pub fn new() -> Self { Self { objects: Vec::new() } }
    pub fn alloc(&mut self, obj: Obj) -> u32 { let i = self.objects.len() as u32; self.objects.push(obj); i }
    pub fn get(&self, i: u32) -> &Obj { &self.objects[i as usize] }
    pub fn get_mut(&mut self, i: u32) -> &mut Obj { &mut self.objects[i as usize] }
}
'@
[System.IO.File]::WriteAllText("$PWD/src/vm/value.rs", $f5)
Write-Host "  [OK] src/vm/value.rs" -ForegroundColor Green

$f6 = @'
#![allow(unsafe_op_in_unsafe_fn)]
pub mod value;
pub mod compiler;

use self::value::*;
use self::compiler::*;

const STACK_MAX: usize = 512;
const FRAME_MAX: usize = 128;

#[derive(Clone, Copy)]
struct CallFrame { fn_idx: usize, ip: usize, slot: usize, closure: u32 }

pub struct VM {
    stack: [Value; STACK_MAX],
    sp: usize,
    frames: [CallFrame; FRAME_MAX],
    fc: usize,
    functions: Vec<Function>,
    struct_defs: Vec<StructDef>,
    heap: Heap,
    handlers: Vec<ErrorHandler>,
}

#[derive(Clone, Copy)]
struct ErrorHandler {
    catch_ip: usize,   // IP to jump to in catch block
    fn_idx: usize,     // function index where handler was set
    fc: usize,         // frame count when handler was set
    sp: usize,         // stack pointer when handler was set
    slot: usize,       // slot of the frame when handler was set
}

impl VM {
    pub fn new(c: Compiler) -> Self {
        VM {
            stack: [Value::NIL; STACK_MAX], sp: 0,
            frames: [CallFrame { fn_idx: 0, ip: 0, slot: 0, closure: u32::MAX }; FRAME_MAX], fc: 1,
            functions: c.functions, struct_defs: c.struct_defs, heap: c.heap,
            handlers: Vec::new(),
        }
    }

    fn get_field_val(&self, obj: Value, name_val: Value) -> Value {
        if !obj.is_obj() { return Value::NIL; }
        match self.heap.get(obj.as_obj()) {
            Obj::Struct { type_idx, fields } => {
                if let Obj::Str(s) = self.heap.get(name_val.as_obj()) {
                    let sdef = &self.struct_defs[*type_idx as usize];
                    if let Some(pos) = sdef.field_names.iter().position(|f| f == s) { return fields[pos]; }
                }
                Value::NIL
            }
            Obj::Map(entries) => {
                if let Obj::Str(key) = self.heap.get(name_val.as_obj()) {
                    for (k, v) in entries { if k == key { return *v; } }
                }
                Value::NIL
            }
            _ => Value::NIL,
        }
    }

    fn find_method(&self, obj: Value, name: &str) -> Option<usize> {
        if !obj.is_obj() { return None; }
        if let Obj::Struct { type_idx, .. } = self.heap.get(obj.as_obj()) {
            return self.struct_defs[*type_idx as usize].methods.get(name).copied();
        }
        None
    }

    // ── Closure call helpers (for array methods) ──────────
    fn call_closure_1(&mut self, closure_val: Value, arg: Value) -> Result<Value, String> {
        if !closure_val.is_obj() { return Err("not callable".into()); }
        let (fn_idx, closure_idx) = match self.heap.get(closure_val.as_obj()) {
            Obj::Closure { fn_idx, .. } => (*fn_idx as usize, closure_val.as_obj()),
            _ => return Err("not callable".into()),
        };
        unsafe {
            let base = self.sp;
            *self.stack.get_unchecked_mut(self.sp) = arg;
            self.sp += 1;
            let saved_fc = self.fc;
            let f = self.frames.get_unchecked_mut(self.fc);
            f.fn_idx = fn_idx; f.ip = 0; f.slot = base; f.closure = closure_idx;
            self.fc += 1;
            self.dispatch(saved_fc)?;
            self.sp -= 1;
            Ok(*self.stack.get_unchecked(self.sp))
        }
    }

    fn call_closure_2(&mut self, closure_val: Value, arg1: Value, arg2: Value) -> Result<Value, String> {
        if !closure_val.is_obj() { return Err("not callable".into()); }
        let (fn_idx, closure_idx) = match self.heap.get(closure_val.as_obj()) {
            Obj::Closure { fn_idx, .. } => (*fn_idx as usize, closure_val.as_obj()),
            _ => return Err("not callable".into()),
        };
        unsafe {
            let base = self.sp;
            *self.stack.get_unchecked_mut(self.sp) = arg1;
            *self.stack.get_unchecked_mut(self.sp + 1) = arg2;
            self.sp += 2;
            let saved_fc = self.fc;
            let f = self.frames.get_unchecked_mut(self.fc);
            f.fn_idx = fn_idx; f.ip = 0; f.slot = base; f.closure = closure_idx;
            self.fc += 1;
            self.dispatch(saved_fc)?;
            self.sp -= 1;
            Ok(*self.stack.get_unchecked(self.sp))
        }
    }

    pub fn run(&mut self) -> Result<(), String> { unsafe { self.dispatch(0) } }

    fn handle_error(&mut self, msg: String, base_fc: usize) -> Result<(), String> {
        if let Some(handler) = self.handlers.pop() {
            // Unwind to handler's frame
            self.fc = handler.fc;
            self.sp = handler.sp;
            unsafe {
                let f = self.frames.get_unchecked_mut(handler.fc - 1);
                f.ip = handler.catch_ip;
                f.fn_idx = handler.fn_idx;
                // Push error string to stack as the catch variable
                let err_idx = self.heap.alloc(Obj::Str(msg));
                *self.stack.get_unchecked_mut(self.sp) = Value::obj(err_idx);
                self.sp += 1;
            }
            Ok(())
        } else {
            Err(format!("Unhandled error: {}", msg))
        }
    }

    unsafe fn runtime_error(&mut self, msg: String, base_fc: usize) -> Result<bool, String> {
        // Returns Ok(true) if error was caught (continue dispatch), Err if uncaught
        match self.handle_error(msg, base_fc) {
            Ok(()) => Ok(true),
            Err(e) => Err(e),
        }
    }

    #[inline(never)]
    unsafe fn dispatch(&mut self, base_fc: usize) -> Result<(), String> {
        loop {
            let fc = self.fc;
            let frame = self.frames.get_unchecked(fc - 1);
            let fi = frame.fn_idx;
            let ip = frame.ip;
            let slot = frame.slot;
            let func = self.functions.get_unchecked(fi);
            let code = func.code.as_ptr();
            let op = *code.add(ip);

            match op {
                OP_CONSTANT => {
                    let idx = *code.add(ip + 1) as usize;
                    *self.stack.get_unchecked_mut(self.sp) = *func.constants.get_unchecked(idx);
                    self.sp += 1;
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 2;
                }
                OP_NIL   => { *self.stack.get_unchecked_mut(self.sp) = Value::NIL;  self.sp += 1; self.frames.get_unchecked_mut(fc - 1).ip = ip + 1; }
                OP_TRUE  => { *self.stack.get_unchecked_mut(self.sp) = Value::TRUE;  self.sp += 1; self.frames.get_unchecked_mut(fc - 1).ip = ip + 1; }
                OP_FALSE => { *self.stack.get_unchecked_mut(self.sp) = Value::FALSE; self.sp += 1; self.frames.get_unchecked_mut(fc - 1).ip = ip + 1; }
                OP_POP   => { self.sp -= 1; self.frames.get_unchecked_mut(fc - 1).ip = ip + 1; }

                OP_GET_LOCAL => {
                    let idx = *code.add(ip + 1) as usize;
                    *self.stack.get_unchecked_mut(self.sp) = *self.stack.get_unchecked(slot + idx);
                    self.sp += 1;
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 2;
                }
                OP_SET_LOCAL => {
                    let idx = *code.add(ip + 1) as usize;
                    self.sp -= 1;
                    *self.stack.get_unchecked_mut(slot + idx) = *self.stack.get_unchecked(self.sp);
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 2;
                }

                // ── Upvalue access (closures) ──
                OP_GET_UPVALUE => {
                    let idx = *code.add(ip + 1) as usize;
                    let closure_idx = self.frames.get_unchecked(fc - 1).closure;
                    let val = match self.heap.get(closure_idx) {
                        Obj::Closure { captures, .. } => *captures.get_unchecked(idx),
                        _ => Value::NIL,
                    };
                    *self.stack.get_unchecked_mut(self.sp) = val;
                    self.sp += 1;
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 2;
                }

                // ── Arithmetic (polymorphic ADD for strings) ──
                OP_ADD => {
                    let b = *self.stack.get_unchecked(self.sp - 1);
                    let a = *self.stack.get_unchecked(self.sp - 2);
                    let result = if a.is_number() && b.is_number() {
                        Value::number(a.as_number() + b.as_number())
                    } else {
                        let sa = a.display(&self.heap);
                        let sb = b.display(&self.heap);
                        let idx = self.heap.alloc(Obj::Str(format!("{}{}", sa, sb)));
                        Value::obj(idx)
                    };
                    *self.stack.get_unchecked_mut(self.sp - 2) = result;
                    self.sp -= 1;
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }
                OP_SUB => {
                    let b = (*self.stack.get_unchecked(self.sp - 1)).as_number();
                    let a = (*self.stack.get_unchecked(self.sp - 2)).as_number();
                    *self.stack.get_unchecked_mut(self.sp - 2) = Value::number(a - b);
                    self.sp -= 1; self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }
                OP_MUL => {
                    let b = (*self.stack.get_unchecked(self.sp - 1)).as_number();
                    let a = (*self.stack.get_unchecked(self.sp - 2)).as_number();
                    *self.stack.get_unchecked_mut(self.sp - 2) = Value::number(a * b);
                    self.sp -= 1; self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }
                OP_DIV => {
                    let b = (*self.stack.get_unchecked(self.sp - 1)).as_number();
                    let a = (*self.stack.get_unchecked(self.sp - 2)).as_number();
                    if b == 0.0 {
                        self.sp -= 2;
                        self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                        if self.runtime_error("division by zero".into(), base_fc)? { continue; }
                    }
                    *self.stack.get_unchecked_mut(self.sp - 2) = Value::number(a / b);
                    self.sp -= 1; self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }
                OP_MOD => {
                    let b = (*self.stack.get_unchecked(self.sp - 1)).as_number();
                    let a = (*self.stack.get_unchecked(self.sp - 2)).as_number();
                    *self.stack.get_unchecked_mut(self.sp - 2) = Value::number(a % b);
                    self.sp -= 1; self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }
                OP_NEGATE => {
                    let v = (*self.stack.get_unchecked(self.sp - 1)).as_number();
                    *self.stack.get_unchecked_mut(self.sp - 1) = Value::number(-v);
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }
                OP_NOT => {
                    let v = *self.stack.get_unchecked(self.sp - 1);
                    *self.stack.get_unchecked_mut(self.sp - 1) = Value::boolean(!v.is_truthy());
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }

                // ── Comparison (with string value equality) ──
                OP_EQ => {
                    let b = *self.stack.get_unchecked(self.sp - 1);
                    let a = *self.stack.get_unchecked(self.sp - 2);
                    let eq = if a.0 == b.0 { true }
                    else if a.is_obj() && b.is_obj() {
                        match (self.heap.get(a.as_obj()), self.heap.get(b.as_obj())) {
                            (Obj::Str(sa), Obj::Str(sb)) => sa == sb,
                            _ => false,
                        }
                    } else { false };
                    *self.stack.get_unchecked_mut(self.sp - 2) = Value::boolean(eq);
                    self.sp -= 1; self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }
                OP_NEQ => {
                    let b = *self.stack.get_unchecked(self.sp - 1);
                    let a = *self.stack.get_unchecked(self.sp - 2);
                    let eq = if a.0 == b.0 { true }
                    else if a.is_obj() && b.is_obj() {
                        match (self.heap.get(a.as_obj()), self.heap.get(b.as_obj())) {
                            (Obj::Str(sa), Obj::Str(sb)) => sa == sb,
                            _ => false,
                        }
                    } else { false };
                    *self.stack.get_unchecked_mut(self.sp - 2) = Value::boolean(!eq);
                    self.sp -= 1; self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }
                OP_LT  => { let b = (*self.stack.get_unchecked(self.sp - 1)).as_number(); let a = (*self.stack.get_unchecked(self.sp - 2)).as_number(); *self.stack.get_unchecked_mut(self.sp - 2) = Value::boolean(a < b);  self.sp -= 1; self.frames.get_unchecked_mut(fc - 1).ip = ip + 1; }
                OP_GT  => { let b = (*self.stack.get_unchecked(self.sp - 1)).as_number(); let a = (*self.stack.get_unchecked(self.sp - 2)).as_number(); *self.stack.get_unchecked_mut(self.sp - 2) = Value::boolean(a > b);  self.sp -= 1; self.frames.get_unchecked_mut(fc - 1).ip = ip + 1; }
                OP_LTE => { let b = (*self.stack.get_unchecked(self.sp - 1)).as_number(); let a = (*self.stack.get_unchecked(self.sp - 2)).as_number(); *self.stack.get_unchecked_mut(self.sp - 2) = Value::boolean(a <= b); self.sp -= 1; self.frames.get_unchecked_mut(fc - 1).ip = ip + 1; }
                OP_GTE => { let b = (*self.stack.get_unchecked(self.sp - 1)).as_number(); let a = (*self.stack.get_unchecked(self.sp - 2)).as_number(); *self.stack.get_unchecked_mut(self.sp - 2) = Value::boolean(a >= b); self.sp -= 1; self.frames.get_unchecked_mut(fc - 1).ip = ip + 1; }

                // ── Control flow ──
                OP_JUMP => {
                    let hi = *code.add(ip + 1) as usize; let lo = *code.add(ip + 2) as usize;
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 3 + ((hi << 8) | lo);
                }
                OP_LOOP => {
                    let hi = *code.add(ip + 1) as usize; let lo = *code.add(ip + 2) as usize;
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 3 - ((hi << 8) | lo);
                }
                OP_JUMP_IF_FALSE => {
                    let hi = *code.add(ip + 1) as usize; let lo = *code.add(ip + 2) as usize;
                    let offset = (hi << 8) | lo;
                    let val = *self.stack.get_unchecked(self.sp - 1);
                    let falsy = val.0 == Value::FALSE.0 || val.0 == Value::NIL.0
                        || (val.is_number() && val.as_number() == 0.0);
                    self.frames.get_unchecked_mut(fc - 1).ip = if falsy { ip + 3 + offset } else { ip + 3 };
                }

                // ── Functions ──
                OP_CALL => {
                    let callee = *code.add(ip + 1) as usize;
                    let argc = *code.add(ip + 2) as usize;
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 3;
                    let f = self.frames.get_unchecked_mut(fc);
                    f.fn_idx = callee; f.ip = 0; f.slot = self.sp - argc; f.closure = u32::MAX;
                    self.fc += 1; continue;
                }
                OP_RETURN => {
                    self.sp -= 1;
                    let result = *self.stack.get_unchecked(self.sp);
                    self.sp = slot; self.fc -= 1;
                    *self.stack.get_unchecked_mut(self.sp) = result;
                    self.sp += 1;
                    if self.fc <= base_fc { return Ok(()); }
                    continue;
                }

                // ── Closure creation ──
                OP_CLOSURE => {
                    let ci = *code.add(ip + 1) as usize;
                    let fn_idx = (*func.constants.get_unchecked(ci)).as_number() as u32;
                    let n = *code.add(ip + 2) as usize;
                    let mut captures = Vec::with_capacity(n);
                    for i in 0..n {
                        let local_slot = *code.add(ip + 3 + i) as usize;
                        captures.push(*self.stack.get_unchecked(slot + local_slot));
                    }
                    let idx = self.heap.alloc(Obj::Closure { fn_idx, captures });
                    *self.stack.get_unchecked_mut(self.sp) = Value::obj(idx);
                    self.sp += 1;
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 3 + n;
                }

                // ── Closure call ──
                OP_CALL_VALUE => {
                    let argc = *code.add(ip + 1) as usize;
                    let closure_val = *self.stack.get_unchecked(self.sp - argc - 1);
                    if !closure_val.is_obj() { return Err("not callable".into()); }
                    let (call_fn, closure_idx) = match self.heap.get(closure_val.as_obj()) {
                        Obj::Closure { fn_idx, .. } => (*fn_idx as usize, closure_val.as_obj()),
                        _ => return Err("not callable".into()),
                    };
                    let base = self.sp - argc - 1;
                    for i in 0..argc {
                        *self.stack.get_unchecked_mut(base + i) = *self.stack.get_unchecked(base + 1 + i);
                    }
                    self.sp -= 1;
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 2;
                    let f = self.frames.get_unchecked_mut(fc);
                    f.fn_idx = call_fn; f.ip = 0; f.slot = self.sp - argc; f.closure = closure_idx;
                    self.fc += 1; continue;
                }

                // ── Builtins ──
                OP_PRINT => {
                    let argc = *code.add(ip + 1) as usize;
                    let base = self.sp - argc;
                    let mut parts = Vec::with_capacity(argc);
                    for i in 0..argc { parts.push((*self.stack.get_unchecked(base + i)).display(&self.heap)); }
                    self.sp = base;
                    println!("{}", parts.join(" "));
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 2;
                }
                OP_SQRT => { let v = (*self.stack.get_unchecked(self.sp - 1)).as_number(); *self.stack.get_unchecked_mut(self.sp - 1) = Value::number(v.sqrt()); self.frames.get_unchecked_mut(fc - 1).ip = ip + 1; }
                OP_ABS  => { let v = (*self.stack.get_unchecked(self.sp - 1)).as_number(); *self.stack.get_unchecked_mut(self.sp - 1) = Value::number(v.abs()); self.frames.get_unchecked_mut(fc - 1).ip = ip + 1; }
                OP_LEN => {
                    let v = *self.stack.get_unchecked(self.sp - 1);
                    let len = if v.is_obj() { match self.heap.get(v.as_obj()) { Obj::Array(items) => items.len(), Obj::Str(s) => s.len(), _ => 0 } } else { 0 };
                    *self.stack.get_unchecked_mut(self.sp - 1) = Value::number(len as f64);
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }
                OP_MIN => { let b = (*self.stack.get_unchecked(self.sp - 1)).as_number(); let a = (*self.stack.get_unchecked(self.sp - 2)).as_number(); *self.stack.get_unchecked_mut(self.sp - 2) = Value::number(a.min(b)); self.sp -= 1; self.frames.get_unchecked_mut(fc - 1).ip = ip + 1; }
                OP_MAX => { let b = (*self.stack.get_unchecked(self.sp - 1)).as_number(); let a = (*self.stack.get_unchecked(self.sp - 2)).as_number(); *self.stack.get_unchecked_mut(self.sp - 2) = Value::number(a.max(b)); self.sp -= 1; self.frames.get_unchecked_mut(fc - 1).ip = ip + 1; }

                // ── String builtins ──
                OP_TO_STR => {
                    let v = *self.stack.get_unchecked(self.sp - 1);
                    if v.is_obj() { if let Obj::Str(_) = self.heap.get(v.as_obj()) { self.frames.get_unchecked_mut(fc - 1).ip = ip + 1; continue; } }
                    let s = v.display(&self.heap);
                    let idx = self.heap.alloc(Obj::Str(s));
                    *self.stack.get_unchecked_mut(self.sp - 1) = Value::obj(idx);
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }
                OP_BUILD_STR => {
                    let count = *code.add(ip + 1) as usize;
                    let base = self.sp - count;
                    let mut result = String::new();
                    for i in 0..count { result.push_str(&(*self.stack.get_unchecked(base + i)).display(&self.heap)); }
                    self.sp = base;
                    let idx = self.heap.alloc(Obj::Str(result));
                    *self.stack.get_unchecked_mut(self.sp) = Value::obj(idx);
                    self.sp += 1;
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 2;
                }
                OP_INT => {
                    let v = *self.stack.get_unchecked(self.sp - 1);
                    if v.is_obj() {
                        if let Obj::Str(s) = self.heap.get(v.as_obj()) {
                            let n = s.parse::<f64>().unwrap_or(0.0) as i64;
                            *self.stack.get_unchecked_mut(self.sp - 1) = Value::number(n as f64);
                        }
                    } else {
                        let n = v.as_number() as i64;
                        *self.stack.get_unchecked_mut(self.sp - 1) = Value::number(n as f64);
                    }
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }
                OP_FLOAT => {
                    let v = *self.stack.get_unchecked(self.sp - 1);
                    if v.is_obj() {
                        if let Obj::Str(s) = self.heap.get(v.as_obj()) {
                            let n = s.parse::<f64>().unwrap_or(0.0);
                            *self.stack.get_unchecked_mut(self.sp - 1) = Value::number(n);
                        }
                    }
                    // numbers stay as-is
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }
                OP_TYPE => {
                    let v = *self.stack.get_unchecked(self.sp - 1);
                    let tn = if v.is_nil() { "nil" }
                        else if v.is_bool() { "bool" }
                        else if v.is_number() {
                            let n = v.as_number();
                            if n == (n as i64) as f64 { "int" } else { "float" }
                        }
                        else if v.is_obj() {
                            match self.heap.get(v.as_obj()) {
                                Obj::Str(_) => "str",
                                Obj::Array(_) => "array",
                                Obj::Map(_) => "map",
                                Obj::Table { .. } => "table",
                                Obj::Struct { .. } => "struct",
                                Obj::Closure { .. } => "fn",
                            }
                        } else { "unknown" };
                    let oi = self.heap.alloc(Obj::Str(tn.to_string()));
                    *self.stack.get_unchecked_mut(self.sp - 1) = Value::obj(oi);
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }
                OP_INPUT => {
                    use std::io::{self, Write, BufRead};
                    let v = *self.stack.get_unchecked(self.sp - 1);
                    if !v.is_nil() {
                        print!("{}", v.display(&self.heap));
                        io::stdout().flush().ok();
                    }
                    self.sp -= 1;
                    let mut line = String::new();
                    io::stdin().lock().read_line(&mut line).ok();
                    let line = line.trim_end().to_string();
                    let oi = self.heap.alloc(Obj::Str(line));
                    *self.stack.get_unchecked_mut(self.sp) = Value::obj(oi);
                    self.sp += 1;
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }

                // ── Data structures ──
                OP_NEW_ARRAY => {
                    let count = *code.add(ip + 1) as usize;
                    let base = self.sp - count;
                    let mut items = Vec::with_capacity(count);
                    for i in 0..count { items.push(*self.stack.get_unchecked(base + i)); }
                    self.sp = base;
                    let idx = self.heap.alloc(Obj::Array(items));
                    *self.stack.get_unchecked_mut(self.sp) = Value::obj(idx); self.sp += 1;
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 2;
                }
                OP_GET_INDEX => {
                    let idx = (*self.stack.get_unchecked(self.sp - 1)).as_number() as usize;
                    let arr = *self.stack.get_unchecked(self.sp - 2);
                    self.sp -= 1;
                    let result = if arr.is_obj() { if let Obj::Array(items) = self.heap.get(arr.as_obj()) { if idx < items.len() { items[idx] } else { Value::NIL } } else { Value::NIL } } else { Value::NIL };
                    *self.stack.get_unchecked_mut(self.sp - 1) = result;
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }
                OP_SET_INDEX => {
                    let val = *self.stack.get_unchecked(self.sp - 1);
                    let idx = (*self.stack.get_unchecked(self.sp - 2)).as_number() as usize;
                    let arr = *self.stack.get_unchecked(self.sp - 3);
                    self.sp -= 3;
                    if arr.is_obj() {
                        if let Obj::Array(items) = self.heap.get_mut(arr.as_obj()) {
                            if idx < items.len() { items[idx] = val; }
                        }
                    }
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }
                OP_NEW_STRUCT => {
                    let type_idx = *code.add(ip + 1);
                    let argc = *code.add(ip + 2) as usize;
                    let base = self.sp - argc;
                    let mut fields = Vec::with_capacity(argc);
                    for i in 0..argc { fields.push(*self.stack.get_unchecked(base + i)); }
                    self.sp = base;
                    let idx = self.heap.alloc(Obj::Struct { type_idx: type_idx as u16, fields });
                    *self.stack.get_unchecked_mut(self.sp) = Value::obj(idx); self.sp += 1;
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 3;
                }
                OP_GET_FIELD => {
                    let ci = *code.add(ip + 1) as usize;
                    let name_val = *func.constants.get_unchecked(ci);
                    let obj = *self.stack.get_unchecked(self.sp - 1);
                    *self.stack.get_unchecked_mut(self.sp - 1) = self.get_field_val(obj, name_val);
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 2;
                }

                // ── Method invocation (structs + string + array methods) ──
                OP_INVOKE => {
                    let ci = *code.add(ip + 1) as usize;
                    let argc = *code.add(ip + 2) as usize;
                    let name_val = *func.constants.get_unchecked(ci);
                    let method_name = match self.heap.get(name_val.as_obj()) {
                        Obj::Str(s) => s.clone(), _ => return Err("invalid method".into()),
                    };
                    let obj = *self.stack.get_unchecked(self.sp - argc - 1);

                    // ── Builtin string methods ──
                    if obj.is_obj() {
                        let is_str = matches!(self.heap.get(obj.as_obj()), Obj::Str(_));
                        if is_str {
                            let s = match self.heap.get(obj.as_obj()) { Obj::Str(s) => s.clone(), _ => unreachable!() };
                            let result: Option<Value> = match method_name.as_str() {
                                "upper" => { let i = self.heap.alloc(Obj::Str(s.to_uppercase())); Some(Value::obj(i)) }
                                "lower" => { let i = self.heap.alloc(Obj::Str(s.to_lowercase())); Some(Value::obj(i)) }
                                "trim"  => { let i = self.heap.alloc(Obj::Str(s.trim().to_string())); Some(Value::obj(i)) }
                                "len"   => Some(Value::number(s.len() as f64)),
                                "contains" if argc >= 1 => {
                                    let a = *self.stack.get_unchecked(self.sp - 1);
                                    if a.is_obj() { if let Obj::Str(sub) = self.heap.get(a.as_obj()) { Some(Value::boolean(s.contains(sub.as_str()))) } else { Some(Value::FALSE) } }
                                    else { Some(Value::FALSE) }
                                }
                                "starts_with" if argc >= 1 => {
                                    let a = *self.stack.get_unchecked(self.sp - 1);
                                    if a.is_obj() { if let Obj::Str(sub) = self.heap.get(a.as_obj()) { Some(Value::boolean(s.starts_with(sub.as_str()))) } else { Some(Value::FALSE) } }
                                    else { Some(Value::FALSE) }
                                }
                                "ends_with" if argc >= 1 => {
                                    let a = *self.stack.get_unchecked(self.sp - 1);
                                    if a.is_obj() { if let Obj::Str(sub) = self.heap.get(a.as_obj()) { Some(Value::boolean(s.ends_with(sub.as_str()))) } else { Some(Value::FALSE) } }
                                    else { Some(Value::FALSE) }
                                }
                                "replace" if argc >= 2 => {
                                    let fv = *self.stack.get_unchecked(self.sp - 2);
                                    let tv = *self.stack.get_unchecked(self.sp - 1);
                                    if fv.is_obj() && tv.is_obj() {
                                        let from = match self.heap.get(fv.as_obj()) { Obj::Str(s) => s.clone(), _ => return Err("replace: expected string".into()) };
                                        let to = match self.heap.get(tv.as_obj()) { Obj::Str(s) => s.clone(), _ => return Err("replace: expected string".into()) };
                                        let i = self.heap.alloc(Obj::Str(s.replace(&from, &to)));
                                        Some(Value::obj(i))
                                    } else { None }
                                }
                                "split" if argc >= 1 => {
                                    let dv = *self.stack.get_unchecked(self.sp - 1);
                                    if dv.is_obj() {
                                        let delim = match self.heap.get(dv.as_obj()) { Obj::Str(s) => s.clone(), _ => return Err("split: expected string".into()) };
                                        let parts: Vec<Value> = s.split(&delim).map(|p| { let i = self.heap.alloc(Obj::Str(p.to_string())); Value::obj(i) }).collect();
                                        let i = self.heap.alloc(Obj::Array(parts));
                                        Some(Value::obj(i))
                                    } else { None }
                                }
                                "chars" => {
                                    let items: Vec<Value> = s.chars().map(|c| { let i = self.heap.alloc(Obj::Str(c.to_string())); Value::obj(i) }).collect();
                                    let i = self.heap.alloc(Obj::Array(items));
                                    Some(Value::obj(i))
                                }
                                _ => None,
                            };
                            if let Some(val) = result {
                                self.sp = self.sp - argc - 1;
                                *self.stack.get_unchecked_mut(self.sp) = val;
                                self.sp += 1;
                                self.frames.get_unchecked_mut(fc - 1).ip = ip + 3;
                                continue;
                            }
                        }
                    }

                    // ── Builtin array methods ──
                    if obj.is_obj() && matches!(self.heap.get(obj.as_obj()), Obj::Array(_)) {
                        // Simple methods (no closure calls)
                        let simple_result: Option<Value> = match method_name.as_str() {
                            "len" => {
                                let len = match self.heap.get(obj.as_obj()) { Obj::Array(items) => items.len(), _ => 0 };
                                Some(Value::number(len as f64))
                            }
                            "push" if argc >= 1 => {
                                let val = *self.stack.get_unchecked(self.sp - 1);
                                if let Obj::Array(items) = self.heap.get_mut(obj.as_obj()) { items.push(val); }
                                Some(Value::NIL)
                            }
                            "pop" => {
                                let result = if let Obj::Array(items) = self.heap.get_mut(obj.as_obj()) {
                                    items.pop().unwrap_or(Value::NIL)
                                } else { Value::NIL };
                                Some(result)
                            }
                            "reverse" => {
                                let items = match self.heap.get(obj.as_obj()) { Obj::Array(items) => items.clone(), _ => vec![] };
                                let mut rev = items; rev.reverse();
                                let i = self.heap.alloc(Obj::Array(rev));
                                Some(Value::obj(i))
                            }
                            "sort" => {
                                let items = match self.heap.get(obj.as_obj()) { Obj::Array(items) => items.clone(), _ => vec![] };
                                let mut sorted = items;
                                sorted.sort_by(|a, b| {
                                    if a.is_number() && b.is_number() {
                                        a.as_number().partial_cmp(&b.as_number()).unwrap_or(std::cmp::Ordering::Equal)
                                    } else { a.display(&self.heap).cmp(&b.display(&self.heap)) }
                                });
                                let i = self.heap.alloc(Obj::Array(sorted));
                                Some(Value::obj(i))
                            }
                            "join" if argc >= 1 => {
                                let sep_val = *self.stack.get_unchecked(self.sp - 1);
                                let sep = if sep_val.is_obj() { match self.heap.get(sep_val.as_obj()) { Obj::Str(s) => s.clone(), _ => String::new() } } else { String::new() };
                                let items = match self.heap.get(obj.as_obj()) { Obj::Array(items) => items.clone(), _ => vec![] };
                                let parts: Vec<String> = items.iter().map(|v| v.display(&self.heap)).collect();
                                let i = self.heap.alloc(Obj::Str(parts.join(&sep)));
                                Some(Value::obj(i))
                            }
                            _ => None,
                        };

                        if let Some(val) = simple_result {
                            self.sp = self.sp - argc - 1;
                            *self.stack.get_unchecked_mut(self.sp) = val;
                            self.sp += 1;
                            self.frames.get_unchecked_mut(fc - 1).ip = ip + 3;
                            continue;
                        }

                        // Closure-calling methods (map, filter, reduce, forEach, find)
                        match method_name.as_str() {
                            "map" if argc >= 1 => {
                                let closure_val = *self.stack.get_unchecked(self.sp - 1);
                                let items = match self.heap.get(obj.as_obj()) { Obj::Array(items) => items.clone(), _ => vec![] };
                                self.sp = self.sp - argc - 1;
                                self.frames.get_unchecked_mut(fc - 1).ip = ip + 3;
                                let mut results = Vec::with_capacity(items.len());
                                for item in &items { results.push(self.call_closure_1(closure_val, *item)?); }
                                let i = self.heap.alloc(Obj::Array(results));
                                *self.stack.get_unchecked_mut(self.sp) = Value::obj(i);
                                self.sp += 1;
                                continue;
                            }
                            "filter" if argc >= 1 => {
                                let closure_val = *self.stack.get_unchecked(self.sp - 1);
                                let items = match self.heap.get(obj.as_obj()) { Obj::Array(items) => items.clone(), _ => vec![] };
                                self.sp = self.sp - argc - 1;
                                self.frames.get_unchecked_mut(fc - 1).ip = ip + 3;
                                let mut results = Vec::new();
                                for item in &items {
                                    let r = self.call_closure_1(closure_val, *item)?;
                                    if r.is_truthy() { results.push(*item); }
                                }
                                let i = self.heap.alloc(Obj::Array(results));
                                *self.stack.get_unchecked_mut(self.sp) = Value::obj(i);
                                self.sp += 1;
                                continue;
                            }
                            "reduce" if argc >= 2 => {
                                let closure_val = *self.stack.get_unchecked(self.sp - 2);
                                let init = *self.stack.get_unchecked(self.sp - 1);
                                let items = match self.heap.get(obj.as_obj()) { Obj::Array(items) => items.clone(), _ => vec![] };
                                self.sp = self.sp - argc - 1;
                                self.frames.get_unchecked_mut(fc - 1).ip = ip + 3;
                                let mut acc = init;
                                for item in &items { acc = self.call_closure_2(closure_val, acc, *item)?; }
                                *self.stack.get_unchecked_mut(self.sp) = acc;
                                self.sp += 1;
                                continue;
                            }
                            "for_each" | "forEach" if argc >= 1 => {
                                let closure_val = *self.stack.get_unchecked(self.sp - 1);
                                let items = match self.heap.get(obj.as_obj()) { Obj::Array(items) => items.clone(), _ => vec![] };
                                self.sp = self.sp - argc - 1;
                                self.frames.get_unchecked_mut(fc - 1).ip = ip + 3;
                                for item in &items { self.call_closure_1(closure_val, *item)?; }
                                *self.stack.get_unchecked_mut(self.sp) = Value::NIL;
                                self.sp += 1;
                                continue;
                            }
                            "find" if argc >= 1 => {
                                let closure_val = *self.stack.get_unchecked(self.sp - 1);
                                let items = match self.heap.get(obj.as_obj()) { Obj::Array(items) => items.clone(), _ => vec![] };
                                self.sp = self.sp - argc - 1;
                                self.frames.get_unchecked_mut(fc - 1).ip = ip + 3;
                                let mut found = Value::NIL;
                                for item in &items {
                                    let r = self.call_closure_1(closure_val, *item)?;
                                    if r.is_truthy() { found = *item; break; }
                                }
                                *self.stack.get_unchecked_mut(self.sp) = found;
                                self.sp += 1;
                                continue;
                            }
                            _ => {} // fall through to struct methods
                        }
                    }

                    // ── Builtin table methods ──
                    if obj.is_obj() && matches!(self.heap.get(obj.as_obj()), Obj::Table { .. }) {
                        let result: Option<Value> = match method_name.as_str() {
                            "len" | "count" => {
                                let n = match self.heap.get(obj.as_obj()) { Obj::Table { rows, .. } => rows.len(), _ => 0 };
                                Some(Value::number(n as f64))
                            }
                            "columns" => {
                                let cols = match self.heap.get(obj.as_obj()) { Obj::Table { columns, .. } => columns.clone(), _ => vec![] };
                                let arr: Vec<Value> = cols.iter().map(|c| { let i = self.heap.alloc(Obj::Str(c.clone())); Value::obj(i) }).collect();
                                let i = self.heap.alloc(Obj::Array(arr));
                                Some(Value::obj(i))
                            }
                            "head" if argc >= 1 => {
                                let n = (*self.stack.get_unchecked(self.sp - 1)).as_number() as usize;
                                let (cols, taken) = match self.heap.get(obj.as_obj()) {
                                    Obj::Table { columns, rows } => (columns.clone(), rows.iter().take(n).cloned().collect::<Vec<_>>()),
                                    _ => (vec![], vec![]),
                                };
                                let i = self.heap.alloc(Obj::Table { columns: cols, rows: taken });
                                Some(Value::obj(i))
                            }
                            "head" => {
                                let (cols, taken) = match self.heap.get(obj.as_obj()) {
                                    Obj::Table { columns, rows } => (columns.clone(), rows.iter().take(5).cloned().collect::<Vec<_>>()),
                                    _ => (vec![], vec![]),
                                };
                                let i = self.heap.alloc(Obj::Table { columns: cols, rows: taken });
                                Some(Value::obj(i))
                            }
                            "limit" if argc >= 1 => {
                                let n = (*self.stack.get_unchecked(self.sp - 1)).as_number() as usize;
                                let (cols, taken) = match self.heap.get(obj.as_obj()) {
                                    Obj::Table { columns, rows } => (columns.clone(), rows.iter().take(n).cloned().collect::<Vec<_>>()),
                                    _ => (vec![], vec![]),
                                };
                                let i = self.heap.alloc(Obj::Table { columns: cols, rows: taken });
                                Some(Value::obj(i))
                            }
                            "select" => {
                                let mut sel_cols = Vec::new();
                                for a in 0..argc {
                                    let v = *self.stack.get_unchecked(self.sp - argc + a);
                                    sel_cols.push(v.display(&self.heap));
                                }
                                let (cols, rows) = match self.heap.get(obj.as_obj()) {
                                    Obj::Table { columns, rows } => (columns.clone(), rows.clone()),
                                    _ => (vec![], vec![]),
                                };
                                let indices: Vec<usize> = sel_cols.iter()
                                    .filter_map(|c| cols.iter().position(|h| h == c))
                                    .collect();
                                let new_cols: Vec<String> = indices.iter().map(|&i| cols[i].clone()).collect();
                                let new_rows: Vec<Vec<Value>> = rows.iter().map(|row| {
                                    indices.iter().map(|&i| if i < row.len() { row[i] } else { Value::NIL }).collect()
                                }).collect();
                                let i = self.heap.alloc(Obj::Table { columns: new_cols, rows: new_rows });
                                Some(Value::obj(i))
                            }
                            "sort_by" if argc >= 1 => {
                                let col_name = (*self.stack.get_unchecked(self.sp - argc)).display(&self.heap);
                                let desc = if argc >= 2 {
                                    let d = (*self.stack.get_unchecked(self.sp - argc + 1)).display(&self.heap);
                                    d == "desc"
                                } else { false };
                                let (cols, rows) = match self.heap.get(obj.as_obj()) {
                                    Obj::Table { columns, rows } => (columns.clone(), rows.clone()),
                                    _ => (vec![], vec![]),
                                };
                                let col_idx = cols.iter().position(|c| *c == col_name);
                                if let Some(ci) = col_idx {
                                    let mut sorted = rows;
                                    sorted.sort_by(|a, b| {
                                        let va = if ci < a.len() { a[ci] } else { Value::NIL };
                                        let vb = if ci < b.len() { b[ci] } else { Value::NIL };
                                        let cmp = if va.is_number() && vb.is_number() {
                                            va.as_number().partial_cmp(&vb.as_number()).unwrap_or(std::cmp::Ordering::Equal)
                                        } else {
                                            va.display(&self.heap).cmp(&vb.display(&self.heap))
                                        };
                                        if desc { cmp.reverse() } else { cmp }
                                    });
                                    let i = self.heap.alloc(Obj::Table { columns: cols, rows: sorted });
                                    Some(Value::obj(i))
                                } else { Some(obj) }
                            }
                            "col" if argc >= 1 => {
                                let col_name = (*self.stack.get_unchecked(self.sp - 1)).display(&self.heap);
                                let (cols, rows) = match self.heap.get(obj.as_obj()) {
                                    Obj::Table { columns, rows } => (columns.clone(), rows.clone()),
                                    _ => (vec![], vec![]),
                                };
                                if let Some(ci) = cols.iter().position(|c| *c == col_name) {
                                    let arr: Vec<Value> = rows.iter().map(|row| if ci < row.len() { row[ci] } else { Value::NIL }).collect();
                                    let i = self.heap.alloc(Obj::Array(arr));
                                    Some(Value::obj(i))
                                } else { let i = self.heap.alloc(Obj::Array(vec![])); Some(Value::obj(i)) }
                            }
                            "sum" if argc >= 1 => {
                                let col_name = (*self.stack.get_unchecked(self.sp - 1)).display(&self.heap);
                                let (cols, rows) = match self.heap.get(obj.as_obj()) {
                                    Obj::Table { columns, rows } => (columns.clone(), rows.clone()),
                                    _ => (vec![], vec![]),
                                };
                                if let Some(ci) = cols.iter().position(|c| *c == col_name) {
                                    let total: f64 = rows.iter().map(|row| if ci < row.len() && row[ci].is_number() { row[ci].as_number() } else { 0.0 }).sum();
                                    Some(Value::number(total))
                                } else { Some(Value::number(0.0)) }
                            }
                            "avg" if argc >= 1 => {
                                let col_name = (*self.stack.get_unchecked(self.sp - 1)).display(&self.heap);
                                let (cols, rows) = match self.heap.get(obj.as_obj()) {
                                    Obj::Table { columns, rows } => (columns.clone(), rows.clone()),
                                    _ => (vec![], vec![]),
                                };
                                if let Some(ci) = cols.iter().position(|c| *c == col_name) {
                                    let total: f64 = rows.iter().map(|row| if ci < row.len() && row[ci].is_number() { row[ci].as_number() } else { 0.0 }).sum();
                                    let avg = if rows.is_empty() { 0.0 } else { total / rows.len() as f64 };
                                    Some(Value::number(avg))
                                } else { Some(Value::number(0.0)) }
                            }
                            "min" if argc >= 1 => {
                                let col_name = (*self.stack.get_unchecked(self.sp - 1)).display(&self.heap);
                                let (cols, rows) = match self.heap.get(obj.as_obj()) {
                                    Obj::Table { columns, rows } => (columns.clone(), rows.clone()),
                                    _ => (vec![], vec![]),
                                };
                                if let Some(ci) = cols.iter().position(|c| *c == col_name) {
                                    let mn = rows.iter().filter_map(|row| if ci < row.len() && row[ci].is_number() { Some(row[ci].as_number()) } else { None }).fold(f64::INFINITY, f64::min);
                                    Some(Value::number(if mn.is_infinite() { 0.0 } else { mn }))
                                } else { Some(Value::number(0.0)) }
                            }
                            "max" if argc >= 1 => {
                                let col_name = (*self.stack.get_unchecked(self.sp - 1)).display(&self.heap);
                                let (cols, rows) = match self.heap.get(obj.as_obj()) {
                                    Obj::Table { columns, rows } => (columns.clone(), rows.clone()),
                                    _ => (vec![], vec![]),
                                };
                                if let Some(ci) = cols.iter().position(|c| *c == col_name) {
                                    let mx = rows.iter().filter_map(|row| if ci < row.len() && row[ci].is_number() { Some(row[ci].as_number()) } else { None }).fold(f64::NEG_INFINITY, f64::max);
                                    Some(Value::number(if mx.is_infinite() { 0.0 } else { mx }))
                                } else { Some(Value::number(0.0)) }
                            }
                            _ => None,
                        };
                        if let Some(val) = result {
                            self.sp = self.sp - argc - 1;
                            *self.stack.get_unchecked_mut(self.sp) = val;
                            self.sp += 1;
                            self.frames.get_unchecked_mut(fc - 1).ip = ip + 3;
                            continue;
                        }
                        // Table.where - uses closure
                        if method_name == "where" && argc >= 1 {
                            let closure_val = *self.stack.get_unchecked(self.sp - 1);
                            let (cols, rows) = match self.heap.get(obj.as_obj()) {
                                Obj::Table { columns, rows } => (columns.clone(), rows.clone()),
                                _ => (vec![], vec![]),
                            };
                            self.sp = self.sp - argc - 1;
                            self.frames.get_unchecked_mut(fc - 1).ip = ip + 3;
                            // For each row, create a map and call closure
                            let mut filtered = Vec::new();
                            for row in &rows {
                                let mut entries = Vec::new();
                                for (j, col) in cols.iter().enumerate() {
                                    entries.push((col.clone(), if j < row.len() { row[j] } else { Value::NIL }));
                                }
                                let map_idx = self.heap.alloc(Obj::Map(entries));
                                let result = self.call_closure_1(closure_val, Value::obj(map_idx))?;
                                if result.is_truthy() { filtered.push(row.clone()); }
                            }
                            let i = self.heap.alloc(Obj::Table { columns: cols, rows: filtered });
                            *self.stack.get_unchecked_mut(self.sp) = Value::obj(i);
                            self.sp += 1;
                            continue;
                        }
                        // Table.group_by
                        if method_name == "group_by" && argc >= 1 {
                            let col_name = (*self.stack.get_unchecked(self.sp - 1)).display(&self.heap);
                            let (cols, rows) = match self.heap.get(obj.as_obj()) {
                                Obj::Table { columns, rows } => (columns.clone(), rows.clone()),
                                _ => (vec![], vec![]),
                            };
                            self.sp = self.sp - argc - 1;
                            self.frames.get_unchecked_mut(fc - 1).ip = ip + 3;
                            if let Some(ci) = cols.iter().position(|c| *c == col_name) {
                                let mut groups: Vec<(String, Vec<Vec<Value>>)> = Vec::new();
                                for row in &rows {
                                    let key = if ci < row.len() { row[ci].display(&self.heap) } else { "".into() };
                                    if let Some(g) = groups.iter_mut().find(|(k, _)| *k == key) {
                                        g.1.push(row.clone());
                                    } else {
                                        groups.push((key, vec![row.clone()]));
                                    }
                                }
                                // Build result table: group_col, count
                                let new_cols = vec![col_name.clone(), "count".to_string()];
                                let new_rows: Vec<Vec<Value>> = groups.iter().map(|(key, rows)| {
                                    let k = self.heap.alloc(Obj::Str(key.clone()));
                                    vec![Value::obj(k), Value::number(rows.len() as f64)]
                                }).collect();
                                let i = self.heap.alloc(Obj::Table { columns: new_cols, rows: new_rows });
                                *self.stack.get_unchecked_mut(self.sp) = Value::obj(i);
                                self.sp += 1;
                            } else {
                                *self.stack.get_unchecked_mut(self.sp) = obj;
                                self.sp += 1;
                            }
                            continue;
                        }
                    }

                    // ── Struct method fallthrough ──
                    match self.find_method(obj, &method_name) {
                        Some(method_fi) => {
                            self.frames.get_unchecked_mut(fc - 1).ip = ip + 3;
                            let f = self.frames.get_unchecked_mut(fc);
                            f.fn_idx = method_fi; f.ip = 0; f.slot = self.sp - argc - 1; f.closure = u32::MAX;
                            self.fc += 1; continue;
                        }
                        None => return Err(format!("method '{}' not found", method_name)),
                    }
                }
                // ── Error handling ──
                OP_TRY => {
                    let hi = *code.add(ip + 1) as usize;
                    let lo = *code.add(ip + 2) as usize;
                    let catch_ip = ip + 3 + ((hi << 8) | lo);
                    self.handlers.push(ErrorHandler {
                        catch_ip, fn_idx: fi, fc, sp: self.sp, slot,
                    });
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 3;
                }
                OP_END_TRY => {
                    self.handlers.pop();
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }
                OP_THROW => {
                    let val = *self.stack.get_unchecked(self.sp - 1);
                    self.sp -= 1;
                    let msg = val.display(&self.heap);
                    match self.handle_error(msg, base_fc) {
                        Ok(()) => continue,
                        Err(e) => return Err(e),
                    }
                }
                // ── Type checking ──
                OP_CHECK_TYPE => {
                    let local_slot = *code.add(ip + 1) as usize;
                    let type_ci = *code.add(ip + 2) as usize;
                    let val = *self.stack.get_unchecked(slot + local_slot);
                    let type_val = *func.constants.get_unchecked(type_ci);
                    let type_name = if let Obj::Str(s) = self.heap.get(type_val.as_obj()) { s.as_str() } else { "" };
                    let ok = match type_name {
                        "int" => val.is_number() && { let n = val.as_number(); n == (n as i64) as f64 },
                        "float" => val.is_number(),
                        "str" => val.is_obj() && matches!(self.heap.get(val.as_obj()), Obj::Str(_)),
                        "bool" => val.is_bool(),
                        "array" => val.is_obj() && matches!(self.heap.get(val.as_obj()), Obj::Array(_)),
                        "map" => val.is_obj() && matches!(self.heap.get(val.as_obj()), Obj::Map(_)),
                        "table" => val.is_obj() && matches!(self.heap.get(val.as_obj()), Obj::Table { .. }),
                        "fn" => val.is_obj() && matches!(self.heap.get(val.as_obj()), Obj::Closure { .. }),
                        _ => {
                            if val.is_obj() {
                                if let Obj::Struct { type_idx, .. } = self.heap.get(val.as_obj()) { self.struct_defs[*type_idx as usize].name == type_name } else { false }
                            } else { false }
                        }
                    };
                    if !ok {
                        let actual = if val.is_nil() { "nil" }
                            else if val.is_bool() { "bool" }
                            else if val.is_number() { let n = val.as_number(); if n == (n as i64) as f64 { "int" } else { "float" } }
                            else if val.is_obj() { match self.heap.get(val.as_obj()) { Obj::Str(_) => "str", Obj::Array(_) => "array", Obj::Map(_) => "map", Obj::Table { .. } => "table", Obj::Struct { .. } => "struct", Obj::Closure { .. } => "fn" } }
                            else { "unknown" };
                        let msg = format!("type error: expected {}, got {}", type_name, actual);
                        self.frames.get_unchecked_mut(fc - 1).ip = ip + 3;
                        if self.runtime_error(msg, base_fc)? { continue; }
                    }
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 3;
                }
                // ── Map ──
                OP_NEW_MAP => {
                    let count = *code.add(ip + 1) as usize;
                    let base_sp = self.sp - count * 2;
                    let mut entries = Vec::with_capacity(count);
                    for i in 0..count {
                        let key_val = *self.stack.get_unchecked(base_sp + i * 2);
                        let val = *self.stack.get_unchecked(base_sp + i * 2 + 1);
                        let key = if let Obj::Str(s) = self.heap.get(key_val.as_obj()) { s.clone() } else { String::new() };
                        entries.push((key, val));
                    }
                    self.sp = base_sp;
                    let idx = self.heap.alloc(Obj::Map(entries));
                    *self.stack.get_unchecked_mut(self.sp) = Value::obj(idx); self.sp += 1;
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 2;
                }
                // ── File I/O ──
                OP_READ_FILE => {
                    let path_val = *self.stack.get_unchecked(self.sp - 1);
                    let path = path_val.display(&self.heap);
                    self.sp -= 1;
                    match std::fs::read_to_string(&path) {
                        Ok(content) => {
                            let oi = self.heap.alloc(Obj::Str(content));
                            *self.stack.get_unchecked_mut(self.sp) = Value::obj(oi);
                            self.sp += 1;
                        }
                        Err(e) => {
                            let msg = format!("read_file error: {}", e);
                            self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                            if self.runtime_error(msg, base_fc)? { continue; }
                        }
                    }
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }
                OP_WRITE_FILE => {
                    let content_val = *self.stack.get_unchecked(self.sp - 1);
                    let path_val = *self.stack.get_unchecked(self.sp - 2);
                    let path = path_val.display(&self.heap);
                    let content = content_val.display(&self.heap);
                    self.sp -= 2;
                    match std::fs::write(&path, &content) {
                        Ok(()) => {
                            *self.stack.get_unchecked_mut(self.sp) = Value::TRUE;
                            self.sp += 1;
                        }
                        Err(e) => {
                            let msg = format!("write_file error: {}", e);
                            self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                            if self.runtime_error(msg, base_fc)? { continue; }
                        }
                    }
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }
                OP_APPEND_FILE => {
                    use std::io::Write;
                    let content_val = *self.stack.get_unchecked(self.sp - 1);
                    let path_val = *self.stack.get_unchecked(self.sp - 2);
                    let path = path_val.display(&self.heap);
                    let content = content_val.display(&self.heap);
                    self.sp -= 2;
                    match std::fs::OpenOptions::new().append(true).create(true).open(&path) {
                        Ok(mut f) => {
                            f.write_all(content.as_bytes()).ok();
                            *self.stack.get_unchecked_mut(self.sp) = Value::TRUE;
                            self.sp += 1;
                        }
                        Err(e) => {
                            let msg = format!("append_file error: {}", e);
                            self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                            if self.runtime_error(msg, base_fc)? { continue; }
                        }
                    }
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }
                // ── CSV / Table ──
                OP_LOAD_CSV => {
                    let path_val = *self.stack.get_unchecked(self.sp - 1);
                    let path = path_val.display(&self.heap);
                    self.sp -= 1;
                    match std::fs::read_to_string(&path) {
                        Ok(content) => {
                            let mut lines = content.lines();
                            let columns: Vec<String> = if let Some(header) = lines.next() {
                                header.split(',').map(|s| s.trim().trim_matches('"').to_string()).collect()
                            } else { vec![] };
                            let mut rows = Vec::new();
                            for line in lines {
                                if line.trim().is_empty() { continue; }
                                let row: Vec<Value> = line.split(',').map(|cell| {
                                    let cell = cell.trim().trim_matches('"');
                                    if let Ok(n) = cell.parse::<i64>() { Value::number(n as f64) }
                                    else if let Ok(n) = cell.parse::<f64>() { Value::number(n) }
                                    else if cell == "true" { Value::TRUE }
                                    else if cell == "false" { Value::FALSE }
                                    else { let i = self.heap.alloc(Obj::Str(cell.to_string())); Value::obj(i) }
                                }).collect();
                                rows.push(row);
                            }
                            let idx = self.heap.alloc(Obj::Table { columns, rows });
                            *self.stack.get_unchecked_mut(self.sp) = Value::obj(idx);
                            self.sp += 1;
                        }
                        Err(e) => {
                            let msg = format!("load_csv error: {}", e);
                            self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                            if self.runtime_error(msg, base_fc)? { continue; }
                        }
                    }
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }
                OP_SAVE_CSV => {
                    let path_val = *self.stack.get_unchecked(self.sp - 2);
                    let table_val = *self.stack.get_unchecked(self.sp - 1);
                    let path = path_val.display(&self.heap);
                    self.sp -= 2;
                    if table_val.is_obj() {
                        if let Obj::Table { columns, rows } = self.heap.get(table_val.as_obj()) {
                            let mut out = columns.join(",");
                            out.push('\n');
                            for row in rows {
                                let cells: Vec<String> = row.iter().map(|v| {
                                    let s = v.display(&self.heap);
                                    if s.contains(',') || s.contains('"') { format!("\"{}\"", s.replace('"', "\"\"")) }
                                    else { s }
                                }).collect();
                                out.push_str(&cells.join(","));
                                out.push('\n');
                            }
                            match std::fs::write(&path, &out) {
                                Ok(()) => {
                                    *self.stack.get_unchecked_mut(self.sp) = Value::TRUE;
                                    self.sp += 1;
                                }
                                Err(e) => {
                                    let msg = format!("save_csv error: {}", e);
                                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                                    if self.runtime_error(msg, base_fc)? { continue; }
                                }
                            }
                        }
                    }
                    self.frames.get_unchecked_mut(fc - 1).ip = ip + 1;
                }
                _ => return Err(format!("unknown opcode {}", op)),
            }
        }
    }
}
'@
[System.IO.File]::WriteAllText("$PWD/src/vm/mod.rs", $f6)
Write-Host "  [OK] src/vm/mod.rs" -ForegroundColor Green

$f7 = @'
use std::fmt;
use std::collections::HashMap;

#[derive(Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Array(Vec<Value>),
    Struct { name: String, fields: HashMap<String, Value> },
    None,
}

impl Value {
    #[inline]
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Int(n)  => *n != 0,
            Value::None    => false,
            _ => true,
        }
    }

    #[inline]
    pub fn as_int(&self) -> i64 {
        match self {
            Value::Int(n)   => *n,
            Value::Float(n) => *n as i64,
            Value::Bool(b)  => if *b { 1 } else { 0 },
            _ => 0,
        }
    }

    #[inline]
    pub fn as_float(&self) -> f64 {
        match self {
            Value::Float(n) => *n,
            Value::Int(n)   => *n as f64,
            _ => 0.0,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(n) => {
                if *n == (*n as i64) as f64 && n.abs() < 1e15 {
                    write!(f, "{}", *n as i64)
                } else {
                    write!(f, "{}", n)
                }
            }
            Value::Bool(b) => write!(f, "{}", b),
            Value::Str(s)  => write!(f, "{}", s),
            Value::Array(items) => {
                write!(f, "[")?;
                for (i, v) in items.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            Value::Struct { name, fields } => {
                write!(f, "{}(", name)?;
                let mut first = true;
                for (k, v) in fields {
                    if !first { write!(f, ", ")?; }
                    write!(f, "{}: {}", k, v)?;
                    first = false;
                }
                write!(f, ")")
            }
            Value::None => write!(f, "none"),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}
'@
[System.IO.File]::WriteAllText("$PWD/src/interpreter/value.rs", $f7)
Write-Host "  [OK] src/interpreter/value.rs" -ForegroundColor Green

$f8 = @'
pub mod value;

use std::collections::HashMap;
use crate::parser::ast::*;
use self::value::Value;

// ── Control flow signal ─────────────────────────

enum Signal {
    Return(Value),
    Error(String),
}

type Res = Result<Value, Signal>;
type StmtRes = Result<(), Signal>;

#[inline]
fn err(msg: impl Into<String>) -> Signal { Signal::Error(msg.into()) }

// ── Stored definitions ──────────────────────────

struct FnInfo {
    params: Vec<String>,
    body: Block,
}

struct StructInfo {
    field_names: Vec<String>,
    methods: HashMap<String, FnInfo>,
}

// ── Interpreter ─────────────────────────────────

pub struct Interpreter {
    scopes: Vec<HashMap<String, Value>>,
    functions: HashMap<String, FnInfo>,
    structs: HashMap<String, StructInfo>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::with_capacity(16)],
            functions: HashMap::new(),
            structs: HashMap::new(),
        }
    }

    // ── Scope management ────────────────────────

    #[inline]
    fn push_scope(&mut self) { self.scopes.push(HashMap::with_capacity(8)); }
    #[inline]
    fn pop_scope(&mut self) { self.scopes.pop(); }

    fn define(&mut self, name: &str, val: Value) {
        self.scopes.last_mut().unwrap().insert(name.to_string(), val);
    }

    fn get(&self, name: &str) -> Res {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.get(name) { return Ok(v.clone()); }
        }
        Err(err(format!("undefined variable '{}'", name)))
    }

    fn set(&mut self, name: &str, val: Value) -> StmtRes {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), val);
                return Ok(());
            }
        }
        Err(err(format!("undefined variable '{}'", name)))
    }

    // ── Implicit return ─────────────────────────
    // Last expression in a function body = return value

    fn implicit_return(body: &mut Block) {
        if matches!(body.last(), Some(Stmt::ExprStmt(_))) {
            if let Some(Stmt::ExprStmt(expr)) = body.pop() {
                body.push(Stmt::Return(Some(expr)));
            }
        }
    }

    // ── Entry point ─────────────────────────────

    pub fn run(&mut self, program: &Program) -> Result<(), String> {
        // Pass 1: register all functions and structs
        for stmt in program {
            match stmt {
                Stmt::FnDef { name, params, body, .. } => {
                    let mut body = body.clone();
                    Self::implicit_return(&mut body);
                    let ps = params.iter().map(|p| p.name.clone()).collect();
                    self.functions.insert(name.clone(), FnInfo { params: ps, body });
                }
                Stmt::Struct { name, fields, methods } => {
                    let fnames = fields.iter().map(|f| f.name.clone()).collect();
                    let mut meths = HashMap::new();
                    for m in methods {
                        if let Stmt::FnDef { name: mn, params, body, .. } = m {
                            let mut body = body.clone();
                            Self::implicit_return(&mut body);
                            let ps = params.iter().map(|p| p.name.clone()).collect();
                            meths.insert(mn.clone(), FnInfo { params: ps, body });
                        }
                    }
                    self.structs.insert(name.clone(), StructInfo { field_names: fnames, methods: meths });
                }
                _ => {}
            }
        }

        // Pass 2: execute
        let map_err = |s: Signal| -> String {
            match s { Signal::Error(e) => e, Signal::Return(_) => String::new() }
        };

        if self.functions.contains_key("main") {
            self.call_fn("main", vec![]).map(|_| ()).map_err(map_err)
        } else {
            for stmt in program {
                if matches!(stmt, Stmt::FnDef { .. } | Stmt::Struct { .. }) { continue; }
                self.exec_stmt(stmt).map_err(map_err)?;
            }
            Ok(())
        }
    }

    // ── Function call ───────────────────────────

    fn call_fn(&mut self, name: &str, args: Vec<Value>) -> Res {
        let info = self.functions.get(name)
            .ok_or_else(|| err(format!("undefined function '{}'", name)))?;
        let params = info.params.clone();
        let body = info.body.clone();

        self.push_scope();
        for (p, a) in params.iter().zip(args) { self.define(p, a); }
        let result = self.exec_block(&body);
        self.pop_scope();

        match result {
            Ok(()) => Ok(Value::None),
            Err(Signal::Return(v)) => Ok(v),
            Err(e) => Err(e),
        }
    }

    // ── Method call ─────────────────────────────

    fn call_method(&mut self, obj: &Value, method: &str, args: Vec<Value>) -> Res {
        // String methods
        if let Value::Str(s) = obj {
            return match method {
                "upper" => Ok(Value::Str(s.to_uppercase())),
                "lower" => Ok(Value::Str(s.to_lowercase())),
                "trim" => Ok(Value::Str(s.trim().to_string())),
                "len" => Ok(Value::Int(s.len() as i64)),
                "contains" => {
                    if let Some(Value::Str(sub)) = args.first() { Ok(Value::Bool(s.contains(sub.as_str()))) }
                    else { Err(err("contains: expected string")) }
                }
                "starts_with" => {
                    if let Some(Value::Str(sub)) = args.first() { Ok(Value::Bool(s.starts_with(sub.as_str()))) }
                    else { Err(err("starts_with: expected string")) }
                }
                "ends_with" => {
                    if let Some(Value::Str(sub)) = args.first() { Ok(Value::Bool(s.ends_with(sub.as_str()))) }
                    else { Err(err("ends_with: expected string")) }
                }
                "replace" => {
                    if let (Some(Value::Str(from)), Some(Value::Str(to))) = (args.get(0), args.get(1)) {
                        Ok(Value::Str(s.replace(from.as_str(), to.as_str())))
                    } else { Err(err("replace: expected 2 strings")) }
                }
                "split" => {
                    if let Some(Value::Str(delim)) = args.first() {
                        let parts: Vec<Value> = s.split(delim.as_str()).map(|p| Value::Str(p.to_string())).collect();
                        Ok(Value::Array(parts))
                    } else { Err(err("split: expected string")) }
                }
                "chars" => {
                    let items: Vec<Value> = s.chars().map(|c| Value::Str(c.to_string())).collect();
                    Ok(Value::Array(items))
                }
                _ => Err(err(format!("no method '{}' on string", method))),
            };
        }

        let sname = match obj {
            Value::Struct { name, .. } => name.clone(),
            _ => return Err(err("method call on non-struct")),
        };
        let sinfo = self.structs.get(&sname)
            .ok_or_else(|| err(format!("undefined struct '{}'", sname)))?;
        let minfo = sinfo.methods.get(method)
            .ok_or_else(|| err(format!("no method '{}' on '{}'", method, sname)))?;
        let params = minfo.params.clone();
        let body = minfo.body.clone();

        self.push_scope();
        if let Some(p) = params.first() { self.define(p, obj.clone()); }
        for (p, a) in params.iter().skip(1).zip(args) { self.define(p, a); }
        let result = self.exec_block(&body);
        self.pop_scope();

        match result {
            Ok(()) => Ok(Value::None),
            Err(Signal::Return(v)) => Ok(v),
            Err(e) => Err(e),
        }
    }

    // ── Block execution ─────────────────────────

    fn exec_block(&mut self, block: &Block) -> StmtRes {
        for stmt in block { self.exec_stmt(stmt)?; }
        Ok(())
    }

    // ── Statement execution ─────────────────────

    fn exec_stmt(&mut self, stmt: &Stmt) -> StmtRes {
        match stmt {
            Stmt::Let { name, value, .. } => {
                let val = self.eval(value)?;
                self.define(name, val);
                Ok(())
            }

            Stmt::Assign { target, value } => {
                let val = self.eval(value)?;
                if let Expr::Ident(name) = target { self.set(name, val) }
                else { Err(err("invalid assignment target")) }
            }

            Stmt::If { cond, then_b, elifs, else_b } => {
                if self.eval(cond)?.is_truthy() {
                    self.push_scope();
                    let r = self.exec_block(then_b);
                    self.pop_scope();
                    return r;
                }
                for (ec, eb) in elifs {
                    if self.eval(ec)?.is_truthy() {
                        self.push_scope();
                        let r = self.exec_block(eb);
                        self.pop_scope();
                        return r;
                    }
                }
                if let Some(eb) = else_b {
                    self.push_scope();
                    let r = self.exec_block(eb);
                    self.pop_scope();
                    return r;
                }
                Ok(())
            }

            Stmt::For { var, iter, body } => {
                // Range: for i in 0..10
                if let Expr::BinOp { left, op, right } = iter {
                    if *op == BinOp::Range {
                        let s = self.eval(left)?.as_int();
                        let e = self.eval(right)?.as_int();
                        for i in s..e {
                            self.push_scope();
                            self.define(var, Value::Int(i));
                            self.exec_block(body)?;
                            self.pop_scope();
                        }
                        return Ok(());
                    }
                }
                // Array: for item in items
                let val = self.eval(iter)?;
                if let Value::Array(items) = val {
                    for item in items {
                        self.push_scope();
                        self.define(var, item);
                        self.exec_block(body)?;
                        self.pop_scope();
                    }
                    Ok(())
                } else {
                    Err(err("for requires range or array"))
                }
            }

            Stmt::While { cond, body } => {
                loop {
                    if !self.eval(cond)?.is_truthy() { break; }
                    self.push_scope();
                    self.exec_block(body)?;
                    self.pop_scope();
                }
                Ok(())
            }

            Stmt::Return(expr) => {
                let val = match expr {
                    Some(e) => self.eval(e)?,
                    None => Value::None,
                };
                Err(Signal::Return(val))
            }

            Stmt::ExprStmt(expr) => {
                self.eval(expr)?;
                Ok(())
            }

            Stmt::FnDef { .. } | Stmt::Struct { .. } => Ok(()),
            Stmt::TryCatch { .. } => Err(err("try/catch requires VM mode (use: volt run)")),
            Stmt::Throw(_) => Err(err("throw requires VM mode (use: volt run)")),
            Stmt::Break => Err(err("break requires VM mode (use: volt run)")),
            Stmt::Continue => Err(err("continue requires VM mode (use: volt run)")),
            Stmt::Match { .. } => Err(err("match requires VM mode (use: volt run)")),
            Stmt::Use { .. } => Ok(()),
        }
    }

    // ── Expression evaluation ───────────────────

    fn eval(&mut self, expr: &Expr) -> Res {
        match expr {
            Expr::Int(n)   => Ok(Value::Int(*n)),
            Expr::Float(n) => Ok(Value::Float(*n)),
            Expr::Str(s)   => Ok(Value::Str(s.clone())),
            Expr::Bool(b)  => Ok(Value::Bool(*b)),
            Expr::Ident(n) => self.get(n),

            Expr::BinOp { left, op, right } => {
                let l = self.eval(left)?;
                // Short-circuit for and/or
                if *op == BinOp::And {
                    return if !l.is_truthy() { Ok(Value::Bool(false)) }
                    else { Ok(Value::Bool(self.eval(right)?.is_truthy())) };
                }
                if *op == BinOp::Or {
                    return if l.is_truthy() { Ok(Value::Bool(true)) }
                    else { Ok(Value::Bool(self.eval(right)?.is_truthy())) };
                }
                let r = self.eval(right)?;
                self.binop(&l, op, &r)
            }

            Expr::UnaryOp { op, expr } => {
                let v = self.eval(expr)?;
                match op {
                    UnaryOp::Neg => match v {
                        Value::Int(n)   => Ok(Value::Int(-n)),
                        Value::Float(n) => Ok(Value::Float(-n)),
                        _ => Err(err("cannot negate")),
                    },
                    UnaryOp::Not => Ok(Value::Bool(!v.is_truthy())),
                }
            }

            Expr::Call { func, args } => self.eval_call(func, args),

            Expr::Field { object, name } => {
                let obj = self.eval(object)?;
                match obj {
                    Value::Struct { fields, .. } => {
                        fields.get(name).cloned()
                            .ok_or_else(|| err(format!("no field '{}'", name)))
                    }
                    _ => Err(err("field access on non-struct")),
                }
            }

            Expr::Index { object, index } => {
                let obj = self.eval(object)?;
                let idx = self.eval(index)?;
                match (obj, idx) {
                    (Value::Array(items), Value::Int(i)) => {
                        items.get(i as usize).cloned()
                            .ok_or_else(|| err("index out of bounds"))
                    }
                    _ => Err(err("invalid index")),
                }
            }

            Expr::Array(items) => {
                let mut vals = Vec::with_capacity(items.len());
                for item in items { vals.push(self.eval(item)?); }
                Ok(Value::Array(vals))
            }

            Expr::FString(parts) => {
                let mut result = String::new();
                for part in parts {
                    let v = self.eval(part)?;
                    result.push_str(&format!("{}", v));
                }
                Ok(Value::Str(result))
            }

            Expr::Lambda { .. } => Err(err("lambdas require VM mode (use: volt run)")),
            Expr::Map { .. } => Err(err("maps require VM mode (use: volt run)")),
        }
    }

    // ── Function / method / constructor call ────

    fn eval_call(&mut self, func: &Expr, args: &[Expr]) -> Res {
        match func {
            // Built-in: print(...)
            Expr::Ident(n) if n == "print" => {
                let vals: Result<Vec<String>, Signal> = args.iter()
                    .map(|a| self.eval(a).map(|v| v.to_string()))
                    .collect();
                println!("{}", vals?.join(" "));
                Ok(Value::None)
            }
            // Built-in: sqrt(x)
            Expr::Ident(n) if n == "sqrt" => {
                let v = self.eval(&args[0])?;
                Ok(Value::Float(v.as_float().sqrt()))
            }
            // Built-in: abs(x)
            Expr::Ident(n) if n == "abs" => {
                match self.eval(&args[0])? {
                    Value::Int(v)   => Ok(Value::Int(v.abs())),
                    Value::Float(v) => Ok(Value::Float(v.abs())),
                    _ => Err(err("abs requires number")),
                }
            }
            // Built-in: len(x)
            Expr::Ident(n) if n == "len" => {
                match self.eval(&args[0])? {
                    Value::Array(a) => Ok(Value::Int(a.len() as i64)),
                    Value::Str(s)   => Ok(Value::Int(s.len() as i64)),
                    _ => Err(err("len requires array or string")),
                }
            }
            // Built-in: max(a, b)
            Expr::Ident(n) if n == "max" => {
                let a = self.eval(&args[0])?.as_int();
                let b = self.eval(&args[1])?.as_int();
                Ok(Value::Int(a.max(b)))
            }
            // Built-in: min(a, b)
            Expr::Ident(n) if n == "min" => {
                let a = self.eval(&args[0])?.as_int();
                let b = self.eval(&args[1])?.as_int();
                Ok(Value::Int(a.min(b)))
            }
            // Built-in: str(x)
            Expr::Ident(n) if n == "str" => {
                let v = self.eval(&args[0])?;
                Ok(Value::Str(format!("{}", v)))
            }

            // Struct constructor or user function
            Expr::Ident(name) => {
                let mut vals = Vec::with_capacity(args.len());
                for a in args { vals.push(self.eval(a)?); }

                // Struct constructor?
                if let Some(si) = self.structs.get(name) {
                    let fnames = si.field_names.clone();
                    if vals.len() != fnames.len() {
                        return Err(err(format!(
                            "'{}' expects {} args, got {}", name, fnames.len(), vals.len()
                        )));
                    }
                    let mut fields = HashMap::new();
                    for (fname, fval) in fnames.into_iter().zip(vals) {
                        fields.insert(fname, fval);
                    }
                    Ok(Value::Struct { name: name.clone(), fields })
                } else {
                    self.call_fn(name, vals)
                }
            }

            // Method call: obj.method(args)
            Expr::Field { object, name: method } => {
                let obj = self.eval(object)?;
                let mut vals = Vec::with_capacity(args.len());
                for a in args { vals.push(self.eval(a)?); }
                self.call_method(&obj, method, vals)
            }

            _ => Err(err("invalid call target")),
        }
    }

    // ── Binary operations ───────────────────────

    #[inline]
    fn binop(&self, l: &Value, op: &BinOp, r: &Value) -> Res {
        // String concatenation (auto-convert if one side is string)
        if *op == BinOp::Add {
            if let (Value::Str(a), Value::Str(b)) = (l, r) {
                return Ok(Value::Str(format!("{}{}", a, b)));
            }
            if matches!(l, Value::Str(_)) || matches!(r, Value::Str(_)) {
                return Ok(Value::Str(format!("{}{}", l, r)));
            }
        }

        // Auto-promote to float if either operand is float
        if matches!(l, Value::Float(_)) || matches!(r, Value::Float(_)) {
            let a = l.as_float();
            let b = r.as_float();
            return match op {
                BinOp::Add   => Ok(Value::Float(a + b)),
                BinOp::Sub   => Ok(Value::Float(a - b)),
                BinOp::Mul   => Ok(Value::Float(a * b)),
                BinOp::Div   => if b == 0.0 { Err(err("division by zero")) }
                                else { Ok(Value::Float(a / b)) },
                BinOp::Mod   => Ok(Value::Float(a % b)),
                BinOp::Eq    => Ok(Value::Bool(a == b)),
                BinOp::NotEq => Ok(Value::Bool(a != b)),
                BinOp::Lt    => Ok(Value::Bool(a < b)),
                BinOp::Gt    => Ok(Value::Bool(a > b)),
                BinOp::LtEq  => Ok(Value::Bool(a <= b)),
                BinOp::GtEq  => Ok(Value::Bool(a >= b)),
                _ => Err(err("invalid float op")),
            };
        }

        // Integer operations
        let a = l.as_int();
        let b = r.as_int();
        match op {
            BinOp::Add   => Ok(Value::Int(a + b)),
            BinOp::Sub   => Ok(Value::Int(a - b)),
            BinOp::Mul   => Ok(Value::Int(a * b)),
            BinOp::Div   => if b == 0 { Err(err("division by zero")) }
                            else { Ok(Value::Int(a / b)) },
            BinOp::Mod   => Ok(Value::Int(a % b)),
            BinOp::Eq    => Ok(Value::Bool(a == b)),
            BinOp::NotEq => Ok(Value::Bool(a != b)),
            BinOp::Lt    => Ok(Value::Bool(a < b)),
            BinOp::Gt    => Ok(Value::Bool(a > b)),
            BinOp::LtEq  => Ok(Value::Bool(a <= b)),
            BinOp::GtEq  => Ok(Value::Bool(a >= b)),
            _ => Err(err("invalid int op")),
        }
    }
}
'@
[System.IO.File]::WriteAllText("$PWD/src/interpreter/mod.rs", $f8)
Write-Host "  [OK] src/interpreter/mod.rs" -ForegroundColor Green

$f9 = @'
mod lexer;
mod parser;
mod interpreter;
mod vm;

use std::env;
use std::fs;
use std::time::Instant;
use std::path::Path;
use std::collections::HashSet;

use lexer::Lexer;
use parser::Parser;
use parser::ast::*;

fn resolve_imports(stmts: &[Stmt], base_dir: &Path, imported: &mut HashSet<String>) -> Result<Vec<Stmt>, String> {
    let mut result = Vec::new();
    for stmt in stmts {
        if let Stmt::Use { path } = stmt {
            let file_path = base_dir.join(format!("{}.volt", path));
            let key = file_path.display().to_string();
            if imported.contains(&key) { continue; }
            let source = fs::read_to_string(&file_path)
                .map_err(|_| format!("module not found: '{}'", path))?;
            imported.insert(key);
            let mut lexer = Lexer::new(&source);
            let tokens = lexer.tokenize();
            let mut parser = Parser::new(tokens);
            let sub = parser.parse().map_err(|e| format!("in {}: {}", path, e))?;
            let sub_dir = file_path.parent().unwrap_or(base_dir);
            let resolved = resolve_imports(&sub, sub_dir, imported)?;
            for s in resolved {
                match &s {
                    Stmt::FnDef { .. } | Stmt::Struct { .. } => result.push(s),
                    _ => {}
                }
            }
        } else {
            result.push(stmt.clone());
        }
    }
    Ok(result)
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("  VOLT 1.0\n");
        println!("  Usage:");
        println!("    volt run <file.volt>     Run program");
        println!("    volt repl                Interactive REPL");
        println!("    volt <file.volt>         Show AST");
        return;
    }

    if args[1] == "repl" {
        run_repl();
        return;
    }

    let (is_run, filename) = if args[1] == "run" {
        if args.len() < 3 { eprintln!("Error: missing filename"); return; }
        (true, args[2].as_str())
    } else {
        (false, args[1].as_str())
    };

    let source = match fs::read_to_string(filename) {
        Ok(s) => s,
        Err(e) => { eprintln!("Error: {}", e); return; }
    };

    let lex_start = Instant::now();
    let mut lexer = Lexer::new(&source);
    let tokens = lexer.tokenize();
    let lex_time = lex_start.elapsed();
    let token_count = tokens.len();

    let parse_start = Instant::now();
    let mut parser = Parser::new(tokens);
    let program = match parser.parse() {
        Ok(p) => p,
        Err(e) => { eprintln!("Parse error: {}", e); return; }
    };
    let parse_time = parse_start.elapsed();

    // Resolve imports
    let base_dir = Path::new(filename).parent().unwrap_or(Path::new("."));
    let mut imported = HashSet::new();
    let program = match resolve_imports(&program, base_dir, &mut imported) {
        Ok(p) => p,
        Err(e) => { eprintln!("Import error: {}", e); return; }
    };

    if is_run {
        let compile_start = Instant::now();
        let compiler = match vm::compiler::Compiler::compile(&program) {
            Ok(c) => c,
            Err(e) => { eprintln!("Compile error: {}", e); return; }
        };
        let compile_time = compile_start.elapsed();

        let exec_start = Instant::now();
        let mut machine = vm::VM::new(compiler);
        match machine.run() {
            Ok(()) => {}
            Err(e) => { eprintln!("Runtime error: {}", e); return; }
        }
        let exec_time = exec_start.elapsed();
        let total = lex_time + parse_time + compile_time + exec_time;

        eprintln!();
        eprintln!("  VOLT | {}", filename);
        eprintln!("  {}", "-".repeat(46));
        eprintln!("  Lexer:    {:>6} tokens  ({:.2}ms)", token_count, lex_time.as_secs_f64() * 1000.0);
        eprintln!("  Parser:   {:>6} stmts   ({:.2}ms)", program.len(), parse_time.as_secs_f64() * 1000.0);
        eprintln!("  Compiler:                ({:.2}ms)", compile_time.as_secs_f64() * 1000.0);
        eprintln!("  VM:                      ({:.2}ms)", exec_time.as_secs_f64() * 1000.0);
        eprintln!("  {}", "-".repeat(46));
        eprintln!("  Total: {:.2}ms", total.as_secs_f64() * 1000.0);
    } else {
        println!();
        println!("  VOLT | {}", filename);
        println!("  {}", "-".repeat(46));
        println!("  Lexer:  {} tokens  ({:.2}ms)", token_count, lex_time.as_secs_f64() * 1000.0);
        println!("  Parser: {} stmts   ({:.2}ms)", program.len(), parse_time.as_secs_f64() * 1000.0);
        println!("  {}", "-".repeat(46));
        println!();
        print_ast(&program, 1);
        println!();
    }
}

fn run_repl() {
    use std::io::{self, Write, BufRead, IsTerminal};

    let interactive = io::stdin().is_terminal();
    if interactive {
        println!("\n  VOLT 1.0 REPL");
        println!("  :help for commands, :quit to exit\n");
    }

    let mut defs = String::new();
    let mut vars = String::new();

    loop {
        if interactive { print!(">> "); io::stdout().flush().ok(); }

        let mut line = String::new();
        match io::stdin().lock().read_line(&mut line) {
            Ok(0) | Err(_) => break,
            _ => {}
        }
        let trimmed = line.trim();
        if trimmed.is_empty() { continue; }

        // Commands
        if trimmed.starts_with(':') {
            match trimmed {
                ":quit" | ":q" => break,
                ":help" | ":h" => {
                    println!("  :quit    Exit REPL");
                    println!("  :clear   Reset all state");
                    println!("  :defs    Show definitions");
                    println!("  :vars    Show variables");
                    println!("  :help    Show this help");
                    continue;
                }
                ":clear" | ":c" => { defs.clear(); vars.clear(); println!("  State cleared."); continue; }
                ":defs" | ":d" => { if defs.is_empty() { println!("  (none)"); } else { print!("{}", defs); } continue; }
                ":vars" | ":v" => { if vars.is_empty() { println!("  (none)"); } else { print!("{}", vars); } continue; }
                _ => { println!("  Unknown command. Type :help"); continue; }
            }
        }

        // Multi-line input
        let mut input = trimmed.to_string();
        let block_kw = ["fn ", "pub fn ", "if ", "for ", "while ", "try", "match ", "struct "];
        if block_kw.iter().any(|k| input.starts_with(k)) {
            loop {
                if interactive { print!(".. "); io::stdout().flush().ok(); }
                let mut cont = String::new();
                match io::stdin().lock().read_line(&mut cont) {
                    Ok(0) | Err(_) => break,
                    _ => {}
                }
                if cont.trim().is_empty() { break; }
                input.push('\n');
                input.push_str(cont.trim_end_matches(|c: char| c == '\n' || c == '\r'));
            }
        }

        // Auto-print bare expressions
        let exec_input = if is_repl_expr(&input) {
            format!("print({})", input)
        } else {
            input.clone()
        };

        // Build full source: defs + vars + new input
        let source = format!("{}\n{}\n{}", defs, vars, exec_input);

        let mut lexer = Lexer::new(&source);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);

        let result = parser.parse()
            .and_then(|prog| {
                let base = Path::new(".");
                let mut imp = HashSet::new();
                resolve_imports(&prog, &base, &mut imp)
            })
            .and_then(|prog| vm::compiler::Compiler::compile(&prog));

        match result {
            Ok(compiler) => {
                let mut machine = vm::VM::new(compiler);
                match machine.run() {
                    Ok(()) => {
                        let t = input.trim();
                        if t.starts_with("fn ") || t.starts_with("pub fn ") || t.starts_with("struct ") || t.starts_with("use ") {
                            defs.push_str(&input); defs.push('\n');
                        } else if t.contains(":=") {
                            vars.push_str(&input); vars.push('\n');
                        }
                    }
                    Err(e) => eprintln!("  Error: {}", e),
                }
            }
            Err(e) => eprintln!("  Error: {}", e),
        }
    }
    if interactive { println!("\n  Goodbye!"); }
}

fn is_repl_expr(input: &str) -> bool {
    let t = input.trim();
    if t.is_empty() || t.contains('\n') { return false; }
    let kw = ["fn ", "struct ", "if ", "for ", "while ", "try", "match ",
              "mut ", "return", "use ", "throw ", "break", "continue", "print(", "pub "];
    if kw.iter().any(|k| t.starts_with(k)) { return false; }
    if t.contains(":=") { return false; }
    let no_ops = t.replace("==", "XX").replace("!=", "XX").replace("<=", "XX").replace(">=", "XX");
    if no_ops.contains(" = ") { return false; }
    true
}

fn print_ast(stmts: &[Stmt], depth: usize) {
    let pad = "  ".repeat(depth);
    for (i, stmt) in stmts.iter().enumerate() {
        if depth == 1 && i > 0 { println!(); }
        match stmt {
            Stmt::FnDef { name, params, ret_type, body, is_pub } => {
                let vis = if *is_pub { "pub " } else { "" };
                let ps: Vec<String> = params.iter().map(|p| {
                    match &p.typ { Some(t) => format!("{}: {}", p.name, fmt_type(t)), None => p.name.clone() }
                }).collect();
                let ret = match ret_type { Some(t) => format!(" -> {}", fmt_type(t)), None => String::new() };
                println!("{}{}fn {}({}){}", pad, vis, name, ps.join(", "), ret);
                print_ast(body, depth + 1);
            }
            Stmt::Struct { name, fields, methods } => {
                println!("{}struct {}", pad, name);
                for f in fields { println!("{}  {}: {}", pad, f.name, fmt_type(&f.typ)); }
                if !methods.is_empty() { print_ast(methods, depth + 1); }
            }
            Stmt::Let { name, mutable, typ, value } => {
                let m = if *mutable { "mut " } else { "" };
                let t = match typ { Some(t) => format!(": {}", fmt_type(t)), None => String::new() };
                println!("{}let {}{}{} := {}", pad, m, name, t, fmt_expr(value));
            }
            Stmt::Assign { target, value } => println!("{}{} = {}", pad, fmt_expr(target), fmt_expr(value)),
            Stmt::If { cond, then_b, elifs, else_b } => {
                println!("{}if {}", pad, fmt_expr(cond));
                print_ast(then_b, depth + 1);
                for (c, b) in elifs { println!("{}elif {}", pad, fmt_expr(c)); print_ast(b, depth + 1); }
                if let Some(eb) = else_b { println!("{}else", pad); print_ast(eb, depth + 1); }
            }
            Stmt::For { var, iter, body } => { println!("{}for {} in {}", pad, var, fmt_expr(iter)); print_ast(body, depth + 1); }
            Stmt::While { cond, body } => { println!("{}while {}", pad, fmt_expr(cond)); print_ast(body, depth + 1); }
            Stmt::Return(Some(e)) => println!("{}return {}", pad, fmt_expr(e)),
            Stmt::Return(None) => println!("{}return", pad),
            Stmt::ExprStmt(e) => println!("{}{}", pad, fmt_expr(e)),
            Stmt::TryCatch { try_body, catch_var, catch_body } => {
                println!("{}try", pad); print_ast(try_body, depth + 1);
                println!("{}catch {}", pad, catch_var); print_ast(catch_body, depth + 1);
            }
            Stmt::Throw(e) => println!("{}throw {}", pad, fmt_expr(e)),
            Stmt::Break => println!("{}break", pad),
            Stmt::Continue => println!("{}continue", pad),
            Stmt::Match { expr, arms } => {
                println!("{}match {}", pad, fmt_expr(expr));
                for arm in arms {
                    println!("{}  {} ->", pad, fmt_pattern(&arm.pattern));
                    print_ast(&arm.body, depth + 2);
                }
            }
            Stmt::Use { path } => println!("{}use \"{}\"", pad, path),
        }
    }
}

fn fmt_expr(e: &Expr) -> String {
    match e {
        Expr::Int(n) => n.to_string(), Expr::Float(n) => n.to_string(),
        Expr::Str(s) => format!("\"{}\"", s), Expr::Bool(b) => b.to_string(),
        Expr::Ident(n) => n.clone(),
        Expr::BinOp { left, op, right } => format!("({} {} {})", fmt_expr(left), fmt_binop(op), fmt_expr(right)),
        Expr::UnaryOp { op, expr } => format!("({}{})", fmt_unary(op), fmt_expr(expr)),
        Expr::Call { func, args } => {
            let a: Vec<String> = args.iter().map(|x| fmt_expr(x)).collect();
            format!("{}({})", fmt_expr(func), a.join(", "))
        }
        Expr::Field { object, name } => format!("{}.{}", fmt_expr(object), name),
        Expr::Index { object, index } => format!("{}[{}]", fmt_expr(object), fmt_expr(index)),
        Expr::Array(items) => { let a: Vec<String> = items.iter().map(|x| fmt_expr(x)).collect(); format!("[{}]", a.join(", ")) }
        Expr::Map(entries) => { let e: Vec<String> = entries.iter().map(|(k, v)| format!("{}: {}", k, fmt_expr(v))).collect(); format!("{{{}}}", e.join(", ")) }
        Expr::FString(parts) => {
            let mut s = String::from("f\"");
            for p in parts {
                match p { Expr::Str(t) => s.push_str(t), _ => { s.push('{'); s.push_str(&fmt_expr(p)); s.push('}'); } }
            }
            s.push('"'); s
        }
        Expr::Lambda { params, body } => {
            let ps = params.join(", ");
            if body.len() == 1 {
                if let Stmt::Return(Some(e)) = &body[0] { return format!("|{}| {}", ps, fmt_expr(e)); }
            }
            format!("|{}| <block>", ps)
        }
    }
}

fn fmt_binop(op: &BinOp) -> &str {
    match op {
        BinOp::Add => "+", BinOp::Sub => "-", BinOp::Mul => "*", BinOp::Div => "/", BinOp::Mod => "%",
        BinOp::Eq => "==", BinOp::NotEq => "!=", BinOp::Lt => "<", BinOp::Gt => ">",
        BinOp::LtEq => "<=", BinOp::GtEq => ">=", BinOp::And => "and", BinOp::Or => "or", BinOp::Range => "..", BinOp::Pipe => "|>",
    }
}
fn fmt_unary(op: &UnaryOp) -> &str { match op { UnaryOp::Neg => "-", UnaryOp::Not => "not " } }
fn fmt_pattern(p: &Pattern) -> String {
    match p {
        Pattern::Int(n) => n.to_string(), Pattern::Float(n) => n.to_string(),
        Pattern::Str(s) => format!("\"{}\"", s), Pattern::Bool(b) => b.to_string(),
        Pattern::Wildcard => "_".to_string(), Pattern::Binding(n) => n.clone(),
        Pattern::Or(pats) => pats.iter().map(|p| fmt_pattern(p)).collect::<Vec<_>>().join(" | "),
    }
}
fn fmt_type(t: &Type) -> String {
    match t {
        Type::Named(n) => n.clone(), Type::Optional(i) => format!("?{}", fmt_type(i)),
        Type::Array(i) => format!("[]{}", fmt_type(i)),
        Type::Generic(n, p) => { let ps: Vec<String> = p.iter().map(|x| fmt_type(x)).collect(); format!("{}[{}]", n, ps.join(", ")) }
    }
}
'@
[System.IO.File]::WriteAllText("$PWD/src/main.rs", $f9)
Write-Host "  [OK] src/main.rs" -ForegroundColor Green

$f10 = @'
// ── VOLT Data A: File I/O + Pipe Operator ──

// 1. Write file
write_file("test_output.txt", "Hello from VOLT!")
print("written")

// 2. Read file
content := read_file("test_output.txt")
print(content)

// 3. Append file
append_file("test_output.txt", "\nSecond line")
content2 := read_file("test_output.txt")
print(content2)

// 4. Read error caught
try
    bad := read_file("nonexistent.txt")
catch e
    print("caught file error")

// 5. Basic pipe
fn double(x)
    return x * 2

result := 5 |> double()
print(result)

// 6. Pipe chain
fn triple(x)
    return x * 3

result2 := 2 |> double() |> triple()
print(result2)

// 7. Pipe with extra args
fn add(a, b)
    return a + b

result3 := 10 |> add(5)
print(result3)

// 8. Pipe with lambdas
fn apply(val, f)
    return f(val)

result4 := 7 |> apply(|x| x * x)
print(result4)

// 9. Pipe into print
"VOLT pipes work!" |> print()

// 10. Write and read back data
data := [10, 20, 30]
write_file("data.txt", str(data))
loaded := read_file("data.txt")
print(f"loaded: {loaded}")

// 11. String pipe chain
fn upper(s)
    return s.upper()

"hello volt" |> upper() |> print()

// 12. Pipe with array methods (via wrapper functions)
fn do_filter(arr, f)
    return arr.filter(f)

fn do_map(arr, f)
    return arr.map(f)

fn do_reduce(arr, f, init)
    return arr.reduce(f, init)

nums := [1, 2, 3, 4, 5]
total := nums |> do_filter(|x| x > 2) |> do_map(|x| x * 10) |> do_reduce(|a, b| a + b, 0)
print(total)

// Cleanup
write_file("test_output.txt", "")
write_file("data.txt", "")
print("done")
'@
[System.IO.File]::WriteAllText("$PWD/tests/programs/pipe_test.volt", $f10)
Write-Host "  [OK] tests/programs/pipe_test.volt" -ForegroundColor Green

$f11 = @'
name,age,city,salary
Alice,30,Paris,55000
Bob,25,Lyon,42000
Charlie,35,Paris,68000
Diana,28,Marseille,47000
Eve,32,Lyon,51000
Frank,40,Paris,72000
Grace,27,Marseille,44000
Hugo,33,Lyon,59000
'@
[System.IO.File]::WriteAllText("$PWD/tests/programs/users.csv", $f11)
Write-Host "  [OK] tests/programs/users.csv" -ForegroundColor Green

$f12 = @'
// ── VOLT Data B: Table + CSV ──

// 1. Load CSV
t := load_csv("tests/programs/users.csv")
print(type(t))
// → table

// 2. Row count
print(t.count())
// → 8

// 3. Columns
print(t.columns())
// → [name, age, city, salary]

// 4. Head
print(t.head(3))

// 5. Select columns
t2 := t.select("name", "salary")
print(t2.head(2))

// 6. Sort by age
t3 := t.sort_by("age")
print(t3.head(3))

// 7. Sort descending
t4 := t.sort_by("salary", "desc")
print(t4.head(3))

// 8. Where (filter)
paris := t.where(|r| r.city == "Paris")
print(paris.count())
// → 3

// 9. Aggregations
print(t.sum("salary"))
print(t.avg("salary"))
print(t.min("age"))
print(t.max("age"))

// 10. Column extraction
ages := t.col("age")
print(ages)

// 11. Group by
groups := t.group_by("city")
print(groups)

// 12. Chained pipeline
result := t.where(|r| r.age > 28).select("name", "salary").sort_by("salary", "desc").limit(3)
print(result)

// 13. Save CSV
save_csv("tests/programs/output.csv", result)
check := load_csv("tests/programs/output.csv")
print(check.count())

// 14. Pipe with table
fn show_count(tbl)
    return tbl.count()

c := t |> show_count()
print(f"piped count: {c}")

print("done")
'@
[System.IO.File]::WriteAllText("$PWD/tests/programs/table_test.volt", $f12)
Write-Host "  [OK] tests/programs/table_test.volt" -ForegroundColor Green


Write-Host "`n  Table + CSV installed!" -ForegroundColor Cyan
Write-Host "  Test: cargo run -- run tests/programs/table_test.volt"
Write-Host "  Pipe: cargo run -- run tests/programs/pipe_test.volt"

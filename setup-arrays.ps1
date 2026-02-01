## VOLT - Etape 8: Array Methods
Write-Host "`n  VOLT - Installing array methods...`n" -ForegroundColor Cyan

$f0 = @'
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // --- Keywords ---
    Fn, If, Elif, Else, For, While, Return, Struct, Mut, Pub, Use, Match, Mod, True, False, In, Or, And, Not,

    // --- Literals ---
    Int(i64), Float(f64), Str(String),

    // --- Identifier ---
    Ident(String),

    // --- Operators ---
    Plus, Minus, Star, Slash, Percent,
    Eq, EqEq, NotEq, Lt, Gt, LtEq, GtEq,
    ColonEq, ColonColon, Arrow, Question, Dot, DotDot,
    Pipe,

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
                '|' => { self.advance(); tokens.push(self.make(Token::Pipe, line, col)); }
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
pub enum BinOp { Add, Sub, Mul, Div, Mod, Eq, NotEq, Lt, Gt, LtEq, GtEq, And, Or, Range }

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
    FString (Vec<Expr>),
    Lambda  { params: Vec<String>, body: Block },
}

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
        self.expect(Token::ColonEq)?; let value = self.parse_expr(0)?;
        Ok(Stmt::Let { name, mutable: true, typ: None, value })
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, String> {
        if let Token::Ident(name) = self.current().clone() {
            if matches!(self.peek(), Token::ColonEq) {
                self.advance(); self.advance();
                let value = self.parse_expr(0)?;
                return Ok(Stmt::Let { name, mutable: false, typ: None, value });
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
                Token::Or      => (BinOp::Or,    2,  3),  Token::And     => (BinOp::And,   4,  5),
                Token::EqEq    => (BinOp::Eq,    6,  7),  Token::NotEq   => (BinOp::NotEq, 6,  7),
                Token::Lt      => (BinOp::Lt,    8,  9),  Token::Gt      => (BinOp::Gt,    8,  9),
                Token::LtEq    => (BinOp::LtEq,  8,  9),  Token::GtEq    => (BinOp::GtEq,  8,  9),
                Token::DotDot  => (BinOp::Range, 10, 11),
                Token::Plus    => (BinOp::Add,   12, 13),  Token::Minus   => (BinOp::Sub,   12, 13),
                Token::Star    => (BinOp::Mul,   14, 15),  Token::Slash   => (BinOp::Div,   14, 15),
                Token::Percent => (BinOp::Mod,   14, 15),
                _ => break,
            };
            if l_bp < min_bp { break; }
            self.advance();
            let right = self.parse_expr(r_bp)?;
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
            Stmt::Let { name, value, .. } => { self.compile_expr(value)?; self.add_local(name); Ok(()) }
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
                self.compile_expr(cond)?;
                let exit = self.emit_jump(OP_JUMP_IF_FALSE); self.emit(OP_POP);
                self.begin_scope(); for s in body { self.compile_stmt(s)?; } self.end_scope();
                self.emit_loop(loop_start); self.patch_jump(exit); self.emit(OP_POP);
                Ok(())
            }
            Stmt::Return(expr) => { match expr { Some(e) => self.compile_expr(e)?, None => self.emit(OP_NIL) } self.emit(OP_RETURN); Ok(()) }
            Stmt::ExprStmt(e) => { self.compile_expr(e)?; self.emit(OP_POP); Ok(()) }
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
        self.begin_scope(); for s in body { self.compile_stmt(s)?; } self.end_scope();
        self.emit2(OP_GET_LOCAL, i_idx); self.emit_constant(Value::number(1.0)); self.emit(OP_ADD);
        self.emit2(OP_SET_LOCAL, i_idx);
        self.emit_loop(loop_start); self.patch_jump(exit); self.emit(OP_POP);
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
        self.begin_scope(); for s in body { self.compile_stmt(s)?; } self.end_scope();
        self.emit2(OP_GET_LOCAL, idx_i); self.emit_constant(Value::number(1.0)); self.emit(OP_ADD);
        self.emit2(OP_SET_LOCAL, idx_i);
        self.emit_loop(loop_start); self.patch_jump(exit); self.emit(OP_POP);
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
}

impl VM {
    pub fn new(c: Compiler) -> Self {
        VM {
            stack: [Value::NIL; STACK_MAX], sp: 0,
            frames: [CallFrame { fn_idx: 0, ip: 0, slot: 0, closure: u32::MAX }; FRAME_MAX], fc: 1,
            functions: c.functions, struct_defs: c.struct_defs, heap: c.heap,
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

use lexer::Lexer;
use parser::Parser;
use parser::ast::*;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("  VOLT 0.3.0\n");
        println!("  Usage:");
        println!("    volt <file.volt>         Show AST");
        println!("    volt run <file.volt>     Run (bytecode VM)");
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
        BinOp::LtEq => "<=", BinOp::GtEq => ">=", BinOp::And => "and", BinOp::Or => "or", BinOp::Range => "..",
    }
}
fn fmt_unary(op: &UnaryOp) -> &str { match op { UnaryOp::Neg => "-", UnaryOp::Not => "not " } }
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
// ── VOLT Etape 7: Closures & Lambdas ──

// 1. Basic lambda (inline expression)
add := |a, b| a + b
print(add(3, 4))
// → 7

// 2. Lambda stored in variable
double := |x| x * 2
print(double(5))
// → 10

// 3. Higher-order function: pass function as argument
fn apply(f, x)
    return f(x)

print(apply(double, 21))
// → 42

// 4. Closure captures outer variable
multiplier := 10
scale := |x| x * multiplier
print(scale(7))
// → 70

// 5. Function returning a closure (factory)
fn make_adder(n)
    return |x| x + n

add5 := make_adder(5)
print(add5(10))
// → 15

// 6. Named function passed as value
fn square(x)
    return x * x

print(apply(square, 6))
// → 36

// 7. Closure with multiple captures
base := 100
bonus := 50
calc := |x| x + base + bonus
print(calc(5))
// → 155

// 8. Lambda in for loop (build array of results)
nums := [1, 2, 3, 4, 5]
transform := |x| x * x + 1
mut results := ""
for n in nums
    mut r := transform(n)
    if results == ""
        results = str(r)
    else
        results = results + " " + str(r)
print(results)
// → 2 5 10 17 26

// 9. Zero-param lambda
greet := || "hello"
print(greet())
// → hello

// 10. Nested closure
fn outer(x)
    fn inner(y)
        return x + y
    return inner(10)

print(outer(5))
// → 15
'@
[System.IO.File]::WriteAllText("$PWD/tests/programs/closure_test.volt", $f10)
Write-Host "  [OK] tests/programs/closure_test.volt" -ForegroundColor Green

$f11 = @'
// ── VOLT Etape 8: Array Methods ──

// 1. push (mutates)
mut arr := [1, 2, 3]
arr.push(4)
print(arr)
// → [1, 2, 3, 4]

// 2. pop (mutates, returns removed)
val := arr.pop()
print(val, arr)
// → 4 [1, 2, 3]

// 3. len (as method)
print(arr.len())
// → 3

// 4. map
doubled := [1, 2, 3, 4].map(|x| x * 2)
print(doubled)
// → [2, 4, 6, 8]

// 5. filter
evens := [1, 2, 3, 4, 5, 6].filter(|x| x % 2 == 0)
print(evens)
// → [2, 4, 6]

// 6. reduce
sum := [1, 2, 3, 4, 5].reduce(|acc, x| acc + x, 0)
print(sum)
// → 15

// 7. forEach
[10, 20, 30].for_each(|x| print(x))
// → 10
// → 20
// → 30

// 8. find
found := [1, 2, 3, 4, 5].find(|x| x > 3)
print(found)
// → 4

// 9. sort
sorted := [3, 1, 4, 1, 5, 9].sort()
print(sorted)
// → [1, 1, 3, 4, 5, 9]

// 10. reverse
rev := [1, 2, 3].reverse()
print(rev)
// → [3, 2, 1]

// 11. join
str_val := ["hello", "world"].join(" ")
print(str_val)
// → hello world

// 12. chaining: filter + map + join
result := [1, 2, 3, 4, 5].filter(|x| x > 2).map(|x| x * 10).join(", ")
print(result)
// → 30, 40, 50

// 13. Index assignment
mut data := [10, 20, 30]
data[1] = 99
print(data)
// → [10, 99, 30]

// 14. map with named function
fn square(x)
    return x * x

print([1, 2, 3].map(square))
// → [1, 4, 9]
'@
[System.IO.File]::WriteAllText("$PWD/tests/programs/array_test.volt", $f11)
Write-Host "  [OK] tests/programs/array_test.volt" -ForegroundColor Green


Write-Host "`n  Array methods installed!" -ForegroundColor Cyan
Write-Host ""
Write-Host "  Test:"
Write-Host "    cargo run -- run tests/programs/array_test.volt"
Write-Host "    cargo run -- run tests/programs/closure_test.volt"
Write-Host "    cargo run -- run tests/programs/string_test.volt"
Write-Host "    cargo run -- run tests/programs/fibonacci.volt"
Write-Host ""

## VOLT Parser - Setup Script
## Run from C:\Users\lucas\volt

Write-Host "`n  VOLT - Installing Parser (Etape 3)...`n" -ForegroundColor Cyan

# ── src/parser/ast.rs ──
$ast = @'
/// Binary operators.
#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add, Sub, Mul, Div, Mod,
    Eq, NotEq, Lt, Gt, LtEq, GtEq,
    And, Or,
    Range,
}

/// Unary operators.
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
}

/// Type annotations.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Named(String),
    Optional(Box<Type>),
    Array(Box<Type>),
    Generic(String, Vec<Type>),
}

/// Function parameter.
#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub typ: Option<Type>,
    pub default: Option<Expr>,
}

/// Struct field.
#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub typ: Type,
}

/// Expressions produce a value.
#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Ident(String),
    BinOp   { left: Box<Expr>, op: BinOp, right: Box<Expr> },
    UnaryOp { op: UnaryOp, expr: Box<Expr> },
    Call    { func: Box<Expr>, args: Vec<Expr> },
    Field   { object: Box<Expr>, name: String },
    Index   { object: Box<Expr>, index: Box<Expr> },
    Array   (Vec<Expr>),
}

/// Statements perform an action.
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

# ── src/parser/mod.rs ──
$parser = @'
pub mod ast;

use crate::lexer::token::{Token, Spanned};
use self::ast::*;

pub struct Parser {
    tokens: Vec<Spanned>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Spanned>) -> Self {
        Self { tokens, pos: 0 }
    }

    // -- Navigation --

    fn current(&self) -> &Token { &self.tokens[self.pos].token }

    fn peek(&self) -> &Token {
        let i = (self.pos + 1).min(self.tokens.len() - 1);
        &self.tokens[i].token
    }

    fn advance(&mut self) {
        if self.pos < self.tokens.len() - 1 { self.pos += 1; }
    }

    fn expect(&mut self, expected: Token) -> Result<(), String> {
        let current = self.current().clone();
        if current == expected { self.advance(); Ok(()) }
        else { Err(self.err(&format!("expected {:?}, found {:?}", expected, current))) }
    }

    fn expect_ident(&mut self, what: &str) -> Result<String, String> {
        match self.current().clone() {
            Token::Ident(n) => { self.advance(); Ok(n) }
            _ => Err(self.err(&format!("expected {}", what)))
        }
    }

    fn skip_newlines(&mut self) {
        while matches!(self.current(), Token::Newline) { self.advance(); }
    }

    fn at_end(&self) -> bool { matches!(self.current(), Token::EOF) }

    fn loc(&self) -> (usize, usize) {
        (self.tokens[self.pos].line, self.tokens[self.pos].col)
    }

    fn err(&self, msg: &str) -> String {
        let (l, c) = self.loc();
        format!("{}:{}: {}", l, c, msg)
    }

    // -- Entry point --

    pub fn parse(&mut self) -> Result<Program, String> {
        let mut stmts = Vec::new();
        self.skip_newlines();
        while !self.at_end() {
            stmts.push(self.parse_stmt()?);
            self.skip_newlines();
        }
        Ok(stmts)
    }

    // -- Block (indented) --

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

    // -- Statements --

    fn parse_stmt(&mut self) -> Result<Stmt, String> {
        match self.current().clone() {
            Token::Fn     => self.parse_fn(false),
            Token::Pub    => {
                self.advance();
                if matches!(self.current(), Token::Fn) { self.parse_fn(true) }
                else { Err(self.err("expected 'fn' after 'pub'")) }
            }
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
        let ret_type = if matches!(self.current(), Token::Arrow) {
            self.advance(); Some(self.parse_type()?)
        } else { None };
        self.skip_newlines();
        let body = self.parse_block()?;
        Ok(Stmt::FnDef { name, params, ret_type, body, is_pub })
    }

    fn parse_params(&mut self) -> Result<Vec<Param>, String> {
        let mut params = Vec::new();
        if matches!(self.current(), Token::RParen) { return Ok(params); }
        loop {
            let name = if matches!(self.current(), Token::Mut) {
                self.advance();
                format!("mut {}", self.expect_ident("param name")?)
            } else {
                self.expect_ident("param name")?
            };
            let typ = if matches!(self.current(), Token::Colon) {
                self.advance(); Some(self.parse_type()?)
            } else { None };
            let default = if matches!(self.current(), Token::Eq) {
                self.advance(); Some(self.parse_expr(0)?)
            } else { None };
            params.push(Param { name, typ, default });
            if matches!(self.current(), Token::Comma) { self.advance(); }
            else { break; }
        }
        Ok(params)
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        if matches!(self.current(), Token::Question) {
            self.advance();
            return Ok(Type::Optional(Box::new(self.parse_type()?)));
        }
        if matches!(self.current(), Token::LBracket) {
            self.advance();
            self.expect(Token::RBracket)?;
            return Ok(Type::Array(Box::new(self.parse_type()?)));
        }
        let name = self.expect_ident("type name")?;
        if matches!(self.current(), Token::LBracket) {
            self.advance();
            let mut args = Vec::new();
            loop {
                args.push(self.parse_type()?);
                if matches!(self.current(), Token::Comma) { self.advance(); }
                else { break; }
            }
            self.expect(Token::RBracket)?;
            Ok(Type::Generic(name, args))
        } else {
            Ok(Type::Named(name))
        }
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
                Token::Pub => {
                    self.advance();
                    if matches!(self.current(), Token::Fn) { methods.push(self.parse_fn(true)?); }
                    else { return Err(self.err("expected 'fn' after 'pub'")); }
                }
                Token::Ident(field_name) => {
                    self.advance();
                    self.expect(Token::Colon)?;
                    let typ = self.parse_type()?;
                    fields.push(Field { name: field_name, typ });
                }
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
                Token::Elif => {
                    self.advance();
                    let c = self.parse_expr(0)?;
                    self.skip_newlines();
                    elifs.push((c, self.parse_block()?));
                }
                Token::Else => {
                    self.advance();
                    self.skip_newlines();
                    else_b = Some(self.parse_block()?);
                    break;
                }
                _ => break,
            }
        }
        Ok(Stmt::If { cond, then_b, elifs, else_b })
    }

    fn parse_for(&mut self) -> Result<Stmt, String> {
        self.advance();
        let var = self.expect_ident("variable")?;
        self.expect(Token::In)?;
        let iter = self.parse_expr(0)?;
        self.skip_newlines();
        let body = self.parse_block()?;
        Ok(Stmt::For { var, iter, body })
    }

    fn parse_while(&mut self) -> Result<Stmt, String> {
        self.advance();
        let cond = self.parse_expr(0)?;
        self.skip_newlines();
        let body = self.parse_block()?;
        Ok(Stmt::While { cond, body })
    }

    fn parse_return(&mut self) -> Result<Stmt, String> {
        self.advance();
        if matches!(self.current(), Token::Newline | Token::Dedent | Token::EOF) {
            Ok(Stmt::Return(None))
        } else {
            Ok(Stmt::Return(Some(self.parse_expr(0)?)))
        }
    }

    fn parse_mut_let(&mut self) -> Result<Stmt, String> {
        self.advance();
        let name = self.expect_ident("variable")?;
        self.expect(Token::ColonEq)?;
        let value = self.parse_expr(0)?;
        Ok(Stmt::Let { name, mutable: true, typ: None, value })
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, String> {
        if let Token::Ident(name) = self.current().clone() {
            if matches!(self.peek(), Token::ColonEq) {
                self.advance();
                self.advance();
                let value = self.parse_expr(0)?;
                return Ok(Stmt::Let { name, mutable: false, typ: None, value });
            }
        }
        let expr = self.parse_expr(0)?;
        if matches!(self.current(), Token::Eq) {
            self.advance();
            let value = self.parse_expr(0)?;
            return Ok(Stmt::Assign { target: expr, value });
        }
        Ok(Stmt::ExprStmt(expr))
    }

    // -- Expressions (Pratt parser) --
    //
    // Binding power (higher = tighter):
    //   or          2, 3
    //   and         4, 5
    //   == !=       6, 7
    //   < > <= >=   8, 9
    //   ..         10, 11
    //   + -        12, 13
    //   * / %      14, 15
    //   unary        16
    //   call/field   20

    fn parse_expr(&mut self, min_bp: u8) -> Result<Expr, String> {
        let mut left = self.parse_atom()?;

        loop {
            // Postfix: call, field, index
            if matches!(self.current(), Token::LParen) && min_bp <= 20 {
                left = self.parse_call(left)?;
                continue;
            }
            if matches!(self.current(), Token::Dot) && min_bp <= 20 {
                self.advance();
                let name = self.expect_ident("field name")?;
                if matches!(self.current(), Token::LParen) {
                    left = self.parse_call(Expr::Field {
                        object: Box::new(left), name,
                    })?;
                } else {
                    left = Expr::Field { object: Box::new(left), name };
                }
                continue;
            }
            if matches!(self.current(), Token::LBracket) && min_bp <= 20 {
                self.advance();
                let index = self.parse_expr(0)?;
                self.expect(Token::RBracket)?;
                left = Expr::Index {
                    object: Box::new(left), index: Box::new(index),
                };
                continue;
            }

            // Infix binary operators
            let (op, l_bp, r_bp) = match self.current() {
                Token::Or      => (BinOp::Or,    2,  3),
                Token::And     => (BinOp::And,   4,  5),
                Token::EqEq    => (BinOp::Eq,    6,  7),
                Token::NotEq   => (BinOp::NotEq, 6,  7),
                Token::Lt      => (BinOp::Lt,    8,  9),
                Token::Gt      => (BinOp::Gt,    8,  9),
                Token::LtEq    => (BinOp::LtEq,  8,  9),
                Token::GtEq    => (BinOp::GtEq,  8,  9),
                Token::DotDot  => (BinOp::Range, 10, 11),
                Token::Plus    => (BinOp::Add,   12, 13),
                Token::Minus   => (BinOp::Sub,   12, 13),
                Token::Star    => (BinOp::Mul,   14, 15),
                Token::Slash   => (BinOp::Div,   14, 15),
                Token::Percent => (BinOp::Mod,   14, 15),
                _ => break,
            };

            if l_bp < min_bp { break; }
            self.advance();
            let right = self.parse_expr(r_bp)?;
            left = Expr::BinOp {
                left: Box::new(left), op, right: Box::new(right),
            };
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
            Token::Minus => {
                self.advance();
                let e = self.parse_expr(16)?;
                Ok(Expr::UnaryOp { op: UnaryOp::Neg, expr: Box::new(e) })
            }
            Token::Not => {
                self.advance();
                let e = self.parse_expr(16)?;
                Ok(Expr::UnaryOp { op: UnaryOp::Not, expr: Box::new(e) })
            }
            Token::LParen => {
                self.advance();
                let e = self.parse_expr(0)?;
                self.expect(Token::RParen)?;
                Ok(e)
            }
            Token::LBracket => {
                self.advance();
                let mut items = Vec::new();
                if !matches!(self.current(), Token::RBracket) {
                    loop {
                        items.push(self.parse_expr(0)?);
                        if matches!(self.current(), Token::Comma) { self.advance(); }
                        else { break; }
                    }
                }
                self.expect(Token::RBracket)?;
                Ok(Expr::Array(items))
            }
            other => Err(self.err(&format!("unexpected {:?}", other))),
        }
    }

    fn parse_call(&mut self, func: Expr) -> Result<Expr, String> {
        self.advance();
        let mut args = Vec::new();
        if !matches!(self.current(), Token::RParen) {
            loop {
                args.push(self.parse_expr(0)?);
                if matches!(self.current(), Token::Comma) { self.advance(); }
                else { break; }
            }
        }
        self.expect(Token::RParen)?;
        Ok(Expr::Call { func: Box::new(func), args })
    }
}
'@

# ── src/main.rs ──
$main = @'
mod lexer;
mod parser;

use std::env;
use std::fs;

use lexer::Lexer;
use parser::Parser;
use parser::ast::*;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("  VOLT 0.1.0\n");
        println!("  Usage:");
        println!("    volt <file.volt>         Parse and display AST");
        println!("    volt run <file.volt>     Run a file (coming soon)");
        println!("    volt build <file.volt>   Compile to native (coming soon)");
        return;
    }

    let filename = if args[1] == "run" || args[1] == "build" {
        if args.len() < 3 {
            eprintln!("Error: missing filename after '{}'", args[1]);
            return;
        }
        &args[2]
    } else {
        &args[1]
    };

    let source = match fs::read_to_string(filename) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error: cannot read '{}': {}", filename, e);
            return;
        }
    };

    let mut lexer = Lexer::new(&source);
    let tokens = lexer.tokenize();
    let token_count = tokens.len();

    let mut parser = Parser::new(tokens);
    match parser.parse() {
        Ok(program) => {
            println!();
            println!("  VOLT | {}", filename);
            println!("  {}", "-".repeat(46));
            println!("  Lexer:  {} tokens", token_count);
            println!("  Parser: {} top-level statements", program.len());
            println!("  {}", "-".repeat(46));
            println!();
            print_ast(&program, 1);
            println!();
        }
        Err(e) => {
            eprintln!();
            eprintln!("  VOLT | Parse Error");
            eprintln!("  {}", "-".repeat(46));
            eprintln!("  {}", e);
            eprintln!();
        }
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
                    match &p.typ {
                        Some(t) => format!("{}: {}", p.name, fmt_type(t)),
                        None => p.name.clone(),
                    }
                }).collect();
                let ret = match ret_type {
                    Some(t) => format!(" -> {}", fmt_type(t)),
                    None => String::new(),
                };
                println!("{}{}fn {}({}){}", pad, vis, name, ps.join(", "), ret);
                print_ast(body, depth + 1);
            }
            Stmt::Struct { name, fields, methods } => {
                println!("{}struct {}", pad, name);
                for f in fields {
                    println!("{}  {}: {}", pad, f.name, fmt_type(&f.typ));
                }
                if !methods.is_empty() { print_ast(methods, depth + 1); }
            }
            Stmt::Let { name, mutable, typ, value } => {
                let m = if *mutable { "mut " } else { "" };
                let t = match typ {
                    Some(t) => format!(": {}", fmt_type(t)),
                    None => String::new(),
                };
                println!("{}let {}{}{} := {}", pad, m, name, t, fmt_expr(value));
            }
            Stmt::Assign { target, value } => {
                println!("{}{} = {}", pad, fmt_expr(target), fmt_expr(value));
            }
            Stmt::If { cond, then_b, elifs, else_b } => {
                println!("{}if {}", pad, fmt_expr(cond));
                print_ast(then_b, depth + 1);
                for (c, b) in elifs {
                    println!("{}elif {}", pad, fmt_expr(c));
                    print_ast(b, depth + 1);
                }
                if let Some(eb) = else_b {
                    println!("{}else", pad);
                    print_ast(eb, depth + 1);
                }
            }
            Stmt::For { var, iter, body } => {
                println!("{}for {} in {}", pad, var, fmt_expr(iter));
                print_ast(body, depth + 1);
            }
            Stmt::While { cond, body } => {
                println!("{}while {}", pad, fmt_expr(cond));
                print_ast(body, depth + 1);
            }
            Stmt::Return(Some(e)) => println!("{}return {}", pad, fmt_expr(e)),
            Stmt::Return(None)    => println!("{}return", pad),
            Stmt::ExprStmt(e)     => println!("{}{}", pad, fmt_expr(e)),
        }
    }
}

fn fmt_expr(e: &Expr) -> String {
    match e {
        Expr::Int(n)    => n.to_string(),
        Expr::Float(n)  => n.to_string(),
        Expr::Str(s)    => format!("\"{}\"", s),
        Expr::Bool(b)   => b.to_string(),
        Expr::Ident(n)  => n.clone(),
        Expr::BinOp { left, op, right } =>
            format!("({} {} {})", fmt_expr(left), fmt_op(op), fmt_expr(right)),
        Expr::UnaryOp { op, expr } =>
            format!("({}{})", fmt_unary(op), fmt_expr(expr)),
        Expr::Call { func, args } => {
            let a: Vec<String> = args.iter().map(|x| fmt_expr(x)).collect();
            format!("{}({})", fmt_expr(func), a.join(", "))
        }
        Expr::Field { object, name } =>
            format!("{}.{}", fmt_expr(object), name),
        Expr::Index { object, index } =>
            format!("{}[{}]", fmt_expr(object), fmt_expr(index)),
        Expr::Array(items) => {
            let a: Vec<String> = items.iter().map(|x| fmt_expr(x)).collect();
            format!("[{}]", a.join(", "))
        }
    }
}

fn fmt_op(op: &BinOp) -> &str {
    match op {
        BinOp::Add => "+", BinOp::Sub => "-",
        BinOp::Mul => "*", BinOp::Div => "/", BinOp::Mod => "%",
        BinOp::Eq => "==", BinOp::NotEq => "!=",
        BinOp::Lt => "<", BinOp::Gt => ">",
        BinOp::LtEq => "<=", BinOp::GtEq => ">=",
        BinOp::And => "and", BinOp::Or => "or",
        BinOp::Range => "..",
    }
}

fn fmt_unary(op: &UnaryOp) -> &str {
    match op { UnaryOp::Neg => "-", UnaryOp::Not => "not " }
}

fn fmt_type(t: &Type) -> String {
    match t {
        Type::Named(n) => n.clone(),
        Type::Optional(inner) => format!("?{}", fmt_type(inner)),
        Type::Array(inner) => format!("[]{}", fmt_type(inner)),
        Type::Generic(name, params) => {
            let p: Vec<String> = params.iter().map(|x| fmt_type(x)).collect();
            format!("{}[{}]", name, p.join(", "))
        }
    }
}
'@

# ── tests/programs/full_test.volt ──
$test = @'
// VOLT Full Test Program

struct Point
    x: f64
    y: f64

    fn distance(self, other: Point) -> f64
        dx := self.x - other.x
        dy := self.y - other.y
        sqrt((dx * dx) + (dy * dy))

fn fibonacci(n: int) -> int
    if n <= 1
        return n
    fibonacci(n - 1) + fibonacci(n - 2)

fn main()
    x := 42
    name := "Lucas"
    mut counter := 0
    items := [1, 2, 3, 4, 5]
    pi := 3.14

    result := fibonacci(10)

    if result > 40 and x == 42
        print("Big!")
    elif result > 20
        print("Medium")
    else
        print("Small")

    for i in 0..10
        counter = counter + 1
        print(i)

    a := Point(0.0, 0.0)
    b := Point(3.0, 4.0)
    dist := a.distance(b)
    print(dist)

    while counter > 0
        counter = counter - 1
'@

# ── Write files ──
[System.IO.File]::WriteAllText("$PWD\src\parser\ast.rs", $ast)
Write-Host "  [OK] src\parser\ast.rs" -ForegroundColor Green

[System.IO.File]::WriteAllText("$PWD\src\parser\mod.rs", $parser)
Write-Host "  [OK] src\parser\mod.rs" -ForegroundColor Green

[System.IO.File]::WriteAllText("$PWD\src\main.rs", $main)
Write-Host "  [OK] src\main.rs" -ForegroundColor Green

[System.IO.File]::WriteAllText("$PWD\tests\programs\full_test.volt", $test)
Write-Host "  [OK] tests\programs\full_test.volt" -ForegroundColor Green

Write-Host "`n  Parser installed! Run:" -ForegroundColor Cyan
Write-Host "    cargo run -- tests\programs\hello.volt" -ForegroundColor Yellow
Write-Host "    cargo run -- tests\programs\full_test.volt" -ForegroundColor Yellow
Write-Host ""

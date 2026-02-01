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
            // Desugar: x |> f(a, b) â†’ f(x, a, b)
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
## VOLT Interpreter - Setup Script (Etape 4)
## Run from C:\Users\lucas\volt

Write-Host "`n  VOLT - Installing Interpreter (Etape 4)...`n" -ForegroundColor Cyan

# Create directory
New-Item -ItemType Directory -Force -Path "$PWD\src\interpreter" | Out-Null

# ══════════════════════════════════════════════════
# src/interpreter/value.rs — Runtime values
# ══════════════════════════════════════════════════
$value = @'
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

# ══════════════════════════════════════════════════
# src/interpreter/mod.rs — Tree-walking interpreter
# ══════════════════════════════════════════════════
$interp = @'
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
        // String concatenation
        if *op == BinOp::Add {
            if let (Value::Str(a), Value::Str(b)) = (l, r) {
                return Ok(Value::Str(format!("{}{}", a, b)));
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

# ══════════════════════════════════════════════════
# src/main.rs — Updated with volt run support
# ══════════════════════════════════════════════════
$main = @'
mod lexer;
mod parser;
mod interpreter;

use std::env;
use std::fs;
use std::time::Instant;

use lexer::Lexer;
use parser::Parser;
use parser::ast::*;
use interpreter::Interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("  VOLT 0.1.0\n");
        println!("  Usage:");
        println!("    volt <file.volt>         Parse and display AST");
        println!("    volt run <file.volt>     Run a VOLT program");
        println!("    volt build <file.volt>   Compile to native (coming soon)");
        return;
    }

    // Parse command
    let (is_run, filename) = if args[1] == "run" {
        if args.len() < 3 {
            eprintln!("Error: missing filename after 'run'");
            return;
        }
        (true, args[2].as_str())
    } else {
        (false, args[1].as_str())
    };

    // Read source
    let source = match fs::read_to_string(filename) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error: cannot read '{}': {}", filename, e);
            return;
        }
    };

    // Lex
    let lex_start = Instant::now();
    let mut lexer = Lexer::new(&source);
    let tokens = lexer.tokenize();
    let lex_time = lex_start.elapsed();
    let token_count = tokens.len();

    // Parse
    let parse_start = Instant::now();
    let mut parser = Parser::new(tokens);
    let program = match parser.parse() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("\n  VOLT | Parse Error");
            eprintln!("  {}", "-".repeat(46));
            eprintln!("  {}", e);
            return;
        }
    };
    let parse_time = parse_start.elapsed();

    if is_run {
        // ── Run mode ──
        let exec_start = Instant::now();
        let mut interp = Interpreter::new();
        match interp.run(&program) {
            Ok(()) => {}
            Err(e) => {
                eprintln!("\n  Runtime Error: {}", e);
                return;
            }
        }
        let exec_time = exec_start.elapsed();

        eprintln!();
        eprintln!("  VOLT | {}", filename);
        eprintln!("  {}", "-".repeat(46));
        eprintln!("  Lexer:    {:>6} tokens  ({:.2}ms)",
            token_count, lex_time.as_secs_f64() * 1000.0);
        eprintln!("  Parser:   {:>6} stmts   ({:.2}ms)",
            program.len(), parse_time.as_secs_f64() * 1000.0);
        eprintln!("  Executed: {:>14}  ({:.2}ms)",
            "", exec_time.as_secs_f64() * 1000.0);
        eprintln!("  {}", "-".repeat(46));
        eprintln!("  Total: {:.2}ms",
            (lex_time + parse_time + exec_time).as_secs_f64() * 1000.0);
    } else {
        // ── Parse mode (show AST) ──
        println!();
        println!("  VOLT | {}", filename);
        println!("  {}", "-".repeat(46));
        println!("  Lexer:  {} tokens  ({:.2}ms)",
            token_count, lex_time.as_secs_f64() * 1000.0);
        println!("  Parser: {} top-level statements  ({:.2}ms)",
            program.len(), parse_time.as_secs_f64() * 1000.0);
        println!("  {}", "-".repeat(46));
        println!();
        print_ast(&program, 1);
        println!();
    }
}

// ── Pretty printer ──────────────────────────────

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
            Stmt::Assign { target, value } =>
                println!("{}{} = {}", pad, fmt_expr(target), fmt_expr(value)),
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

# ══════════════════════════════════════════════════
# Test programs
# ══════════════════════════════════════════════════
$full_test = @'
// VOLT Full Test

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

$fibonacci = @'
// Fibonacci benchmark
fn fibonacci(n: int) -> int
    if n <= 1
        return n
    fibonacci(n - 1) + fibonacci(n - 2)

fn main()
    result := fibonacci(30)
    print(result)
'@

$simple = @'
// Simple test (no main function)
x := 10
y := 20
print(x + y)
print(x * y)
print("Hello VOLT!")

items := [1, 2, 3, 4, 5]
print(items)
print(len(items))
'@

# ── Write all files ──────────────────────────────

[System.IO.File]::WriteAllText("$PWD\src\interpreter\value.rs", $value)
Write-Host "  [OK] src\interpreter\value.rs" -ForegroundColor Green

[System.IO.File]::WriteAllText("$PWD\src\interpreter\mod.rs", $interp)
Write-Host "  [OK] src\interpreter\mod.rs" -ForegroundColor Green

[System.IO.File]::WriteAllText("$PWD\src\main.rs", $main)
Write-Host "  [OK] src\main.rs" -ForegroundColor Green

[System.IO.File]::WriteAllText("$PWD\tests\programs\full_test.volt", $full_test)
Write-Host "  [OK] tests\programs\full_test.volt" -ForegroundColor Green

[System.IO.File]::WriteAllText("$PWD\tests\programs\fibonacci.volt", $fibonacci)
Write-Host "  [OK] tests\programs\fibonacci.volt" -ForegroundColor Green

[System.IO.File]::WriteAllText("$PWD\tests\programs\simple.volt", $simple)
Write-Host "  [OK] tests\programs\simple.volt" -ForegroundColor Green

Write-Host "`n  Interpreter installed! Test with:" -ForegroundColor Cyan
Write-Host "    cargo run -- run tests\programs\simple.volt" -ForegroundColor Yellow
Write-Host "    cargo run -- run tests\programs\full_test.volt" -ForegroundColor Yellow
Write-Host "    cargo run -- run tests\programs\fibonacci.volt" -ForegroundColor Yellow
Write-Host ""

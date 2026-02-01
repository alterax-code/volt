## VOLT - Etape 6: String support (fix interpreter)
Write-Host "`n  Patching interpreter...`n" -ForegroundColor Cyan

$interp_rs = @'
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
[System.IO.File]::WriteAllText("$PWD/src/interpreter/mod.rs", $interp_rs)
Write-Host "  [OK] src/interpreter/mod.rs" -ForegroundColor Green
Write-Host "  Done! Run: cargo run -- run tests/programs/string_test.volt" -ForegroundColor Cyan

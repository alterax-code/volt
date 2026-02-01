## VOLT Bytecode VM - Setup Script (Etape 4b)
## Run from C:\Users\lucas\volt

Write-Host "`n  VOLT - Installing Bytecode VM...`n" -ForegroundColor Cyan

# Create VM directory
New-Item -ItemType Directory -Force -Path "$PWD\src\vm" | Out-Null

# ═══ src/vm/value.rs ═══
$value_rs = @'
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
            if n == (n as i64) as f64 && n.abs() < 1e15 {
                format!("{}", n as i64)
            } else { format!("{}", n) }
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
            }
        } else { "?".into() }
    }
}

pub enum Obj {
    Str(String),
    Array(Vec<Value>),
    Struct { type_idx: u16, fields: Vec<Value> },
}

pub struct Heap { objects: Vec<Obj> }
impl Heap {
    pub fn new() -> Self { Self { objects: Vec::new() } }
    pub fn alloc(&mut self, obj: Obj) -> u32 {
        let i = self.objects.len() as u32; self.objects.push(obj); i
    }
    pub fn get(&self, i: u32) -> &Obj { &self.objects[i as usize] }
}

'@

# ═══ src/vm/compiler.rs ═══
$compiler_rs = @'
#![allow(dead_code)]
use std::collections::HashMap;
use crate::parser::ast::*;
use super::value::*;

// ── Opcodes ─────────────────────────────────────
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
pub const OP_NEW_ARRAY: u8 = 60;
pub const OP_GET_INDEX: u8 = 61;
pub const OP_NEW_STRUCT: u8 = 62;
pub const OP_GET_FIELD: u8 = 63;
pub const OP_INVOKE: u8 = 64;

// ── Compiled function ───────────────────────────
pub struct Function {
    pub name: String,
    pub arity: u8,
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
}

pub struct StructDef {
    pub name: String,
    pub field_names: Vec<String>,
    pub methods: HashMap<String, usize>,
}

struct Local { name: String, depth: usize }

// ── Compiler ────────────────────────────────────
pub struct Compiler {
    pub functions: Vec<Function>,
    pub struct_defs: Vec<StructDef>,
    pub heap: Heap,
    fn_map: HashMap<String, usize>,
    struct_map: HashMap<String, usize>,
    current_fn: usize,
    locals: Vec<Local>,
    scope_depth: usize,
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
        self.emit(((offset >> 8) & 0xFF) as u8);
        self.emit((offset & 0xFF) as u8);
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

    fn add_local(&mut self, name: &str) {
        self.locals.push(Local { name: name.to_string(), depth: self.scope_depth });
    }

    fn resolve_local(&self, name: &str) -> Option<u8> {
        for (i, l) in self.locals.iter().enumerate().rev() {
            if l.name == name { return Some(i as u8); }
        }
        None
    }

    // ── Entry point ─────────────────────────────
    pub fn compile(program: &Program) -> Result<Self, String> {
        let mut c = Self {
            functions: vec![Function { name: "__entry".into(), arity: 0, code: vec![], constants: vec![] }],
            struct_defs: vec![], heap: Heap::new(),
            fn_map: HashMap::new(), struct_map: HashMap::new(),
            current_fn: 0, locals: vec![], scope_depth: 0,
        };

        // Pass 1: register functions and structs
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

        // Pass 2: compile function bodies
        for stmt in program {
            match stmt {
                Stmt::FnDef { name, params, body, .. } => {
                    let idx = c.fn_map[name];
                    c.compile_fn(idx, params, body)?;
                }
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

        // Pass 3: compile entry
        c.current_fn = 0; c.locals = vec![]; c.scope_depth = 0;
        if let Some(&main_idx) = c.fn_map.get("main") {
            c.emit(OP_CALL); c.emit(main_idx as u8); c.emit(0);
            c.emit(OP_POP);
        } else {
            for stmt in program {
                match stmt {
                    Stmt::FnDef { .. } | Stmt::Struct { .. } => {}
                    _ => c.compile_stmt(stmt)?,
                }
            }
        }
        c.emit(OP_NIL); c.emit(OP_RETURN);
        Ok(c)
    }

    // ── Function compilation ────────────────────
    fn compile_fn(&mut self, fn_idx: usize, params: &[Param], body: &Block) -> Result<(), String> {
        let prev = (self.current_fn, std::mem::take(&mut self.locals), self.scope_depth);
        self.current_fn = fn_idx;
        self.scope_depth = 0;
        self.locals = Vec::new();
        for p in params { self.locals.push(Local { name: p.name.clone(), depth: 0 }); }

        // Implicit return: last ExprStmt → Return
        let len = body.len();
        for (i, stmt) in body.iter().enumerate() {
            if i == len - 1 {
                if let Stmt::ExprStmt(e) = stmt {
                    self.compile_expr(e)?;
                    self.emit(OP_RETURN);
                    self.current_fn = prev.0; self.locals = prev.1; self.scope_depth = prev.2;
                    return Ok(());
                }
            }
            self.compile_stmt(stmt)?;
        }

        let code = &self.functions[self.current_fn].code;
        if code.is_empty() || *code.last().unwrap() != OP_RETURN {
            self.emit(OP_NIL); self.emit(OP_RETURN);
        }
        self.current_fn = prev.0; self.locals = prev.1; self.scope_depth = prev.2;
        Ok(())
    }

    // ── Statement compilation ───────────────────
    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Let { name, value, .. } => {
                self.compile_expr(value)?;
                self.add_local(name);
                Ok(())
            }
            Stmt::Assign { target, value } => {
                self.compile_expr(value)?;
                if let Expr::Ident(name) = target {
                    let idx = self.resolve_local(name).ok_or_else(|| format!("undefined '{}'", name))?;
                    self.emit2(OP_SET_LOCAL, idx);
                } else { return Err("invalid assignment target".into()); }
                Ok(())
            }
            Stmt::If { cond, then_b, elifs, else_b } => {
                self.compile_expr(cond)?;
                let then_jmp = self.emit_jump(OP_JUMP_IF_FALSE);
                self.emit(OP_POP);
                self.begin_scope();
                for s in then_b { self.compile_stmt(s)?; }
                self.end_scope();
                let else_jmp = self.emit_jump(OP_JUMP);
                self.patch_jump(then_jmp);
                self.emit(OP_POP);

                let mut end_jumps = vec![else_jmp];
                for (ec, eb) in elifs {
                    self.compile_expr(ec)?;
                    let ej = self.emit_jump(OP_JUMP_IF_FALSE);
                    self.emit(OP_POP);
                    self.begin_scope();
                    for s in eb { self.compile_stmt(s)?; }
                    self.end_scope();
                    end_jumps.push(self.emit_jump(OP_JUMP));
                    self.patch_jump(ej);
                    self.emit(OP_POP);
                }
                if let Some(eb) = else_b {
                    self.begin_scope();
                    for s in eb { self.compile_stmt(s)?; }
                    self.end_scope();
                }
                for ej in end_jumps { self.patch_jump(ej); }
                Ok(())
            }
            Stmt::For { var, iter, body } => {
                if let Expr::BinOp { left, op, right } = iter {
                    if *op == BinOp::Range {
                        return self.compile_for_range(var, left, right, body);
                    }
                }
                self.compile_for_array(var, iter, body)
            }
            Stmt::While { cond, body } => {
                let loop_start = self.current_offset();
                self.compile_expr(cond)?;
                let exit = self.emit_jump(OP_JUMP_IF_FALSE);
                self.emit(OP_POP);
                self.begin_scope();
                for s in body { self.compile_stmt(s)?; }
                self.end_scope();
                self.emit_loop(loop_start);
                self.patch_jump(exit);
                self.emit(OP_POP);
                Ok(())
            }
            Stmt::Return(expr) => {
                match expr { Some(e) => self.compile_expr(e)?, None => self.emit(OP_NIL) }
                self.emit(OP_RETURN);
                Ok(())
            }
            Stmt::ExprStmt(e) => { self.compile_expr(e)?; self.emit(OP_POP); Ok(()) }
            Stmt::FnDef { .. } | Stmt::Struct { .. } => Ok(()),
        }
    }

    fn compile_for_range(&mut self, var: &str, start: &Expr, end: &Expr, body: &Block) -> Result<(), String> {
        self.begin_scope();
        self.compile_expr(start)?; self.add_local(var);
        self.compile_expr(end)?; self.add_local("__end");
        let loop_start = self.current_offset();
        let i_idx = self.resolve_local(var).unwrap();
        let end_idx = self.resolve_local("__end").unwrap();
        self.emit2(OP_GET_LOCAL, i_idx);
        self.emit2(OP_GET_LOCAL, end_idx);
        self.emit(OP_LT);
        let exit = self.emit_jump(OP_JUMP_IF_FALSE);
        self.emit(OP_POP);
        self.begin_scope();
        for s in body { self.compile_stmt(s)?; }
        self.end_scope();
        self.emit2(OP_GET_LOCAL, i_idx);
        self.emit_constant(Value::number(1.0));
        self.emit(OP_ADD);
        self.emit2(OP_SET_LOCAL, i_idx);
        self.emit_loop(loop_start);
        self.patch_jump(exit);
        self.emit(OP_POP);
        self.end_scope();
        Ok(())
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
        self.emit2(OP_GET_LOCAL, idx_i);
        self.emit2(OP_GET_LOCAL, arr_i);
        self.emit(OP_LEN);
        self.emit(OP_LT);
        let exit = self.emit_jump(OP_JUMP_IF_FALSE);
        self.emit(OP_POP);
        self.emit2(OP_GET_LOCAL, arr_i);
        self.emit2(OP_GET_LOCAL, idx_i);
        self.emit(OP_GET_INDEX);
        self.emit2(OP_SET_LOCAL, var_i);
        self.begin_scope();
        for s in body { self.compile_stmt(s)?; }
        self.end_scope();
        self.emit2(OP_GET_LOCAL, idx_i);
        self.emit_constant(Value::number(1.0));
        self.emit(OP_ADD);
        self.emit2(OP_SET_LOCAL, idx_i);
        self.emit_loop(loop_start);
        self.patch_jump(exit);
        self.emit(OP_POP);
        self.end_scope();
        Ok(())
    }

    // ── Expression compilation ──────────────────
    fn compile_expr(&mut self, expr: &Expr) -> Result<(), String> {
        match expr {
            Expr::Int(n) => { self.emit_constant(Value::number(*n as f64)); Ok(()) }
            Expr::Float(n) => { self.emit_constant(Value::number(*n)); Ok(()) }
            Expr::Bool(true) => { self.emit(OP_TRUE); Ok(()) }
            Expr::Bool(false) => { self.emit(OP_FALSE); Ok(()) }
            Expr::Str(s) => {
                let oi = self.heap.alloc(Obj::Str(s.clone()));
                self.emit_constant(Value::obj(oi)); Ok(())
            }
            Expr::Ident(name) => {
                let idx = self.resolve_local(name).ok_or_else(|| format!("undefined '{}'", name))?;
                self.emit2(OP_GET_LOCAL, idx); Ok(())
            }
            Expr::BinOp { left, op, right } => {
                match op {
                    BinOp::And => {
                        self.compile_expr(left)?;
                        let end = self.emit_jump(OP_JUMP_IF_FALSE);
                        self.emit(OP_POP);
                        self.compile_expr(right)?;
                        self.patch_jump(end);
                        Ok(())
                    }
                    BinOp::Or => {
                        self.compile_expr(left)?;
                        let false_jmp = self.emit_jump(OP_JUMP_IF_FALSE);
                        let end_jmp = self.emit_jump(OP_JUMP);
                        self.patch_jump(false_jmp);
                        self.emit(OP_POP);
                        self.compile_expr(right)?;
                        self.patch_jump(end_jmp);
                        Ok(())
                    }
                    _ => {
                        self.compile_expr(left)?;
                        self.compile_expr(right)?;
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
                self.compile_expr(object)?;
                self.compile_expr(index)?;
                self.emit(OP_GET_INDEX); Ok(())
            }
            Expr::Array(items) => {
                for item in items { self.compile_expr(item)?; }
                self.emit2(OP_NEW_ARRAY, items.len() as u8); Ok(())
            }
        }
    }

    fn compile_call(&mut self, func: &Expr, args: &[Expr]) -> Result<(), String> {
        match func {
            Expr::Ident(name) => match name.as_str() {
                "print" => {
                    for a in args { self.compile_expr(a)?; }
                    self.emit2(OP_PRINT, args.len() as u8);
                    self.emit(OP_NIL); Ok(())
                }
                "sqrt" => { self.compile_expr(&args[0])?; self.emit(OP_SQRT); Ok(()) }
                "abs"  => { self.compile_expr(&args[0])?; self.emit(OP_ABS); Ok(()) }
                "len"  => { self.compile_expr(&args[0])?; self.emit(OP_LEN); Ok(()) }
                "min"  => { self.compile_expr(&args[0])?; self.compile_expr(&args[1])?; self.emit(OP_MIN); Ok(()) }
                "max"  => { self.compile_expr(&args[0])?; self.compile_expr(&args[1])?; self.emit(OP_MAX); Ok(()) }
                _ => {
                    if let Some(&sidx) = self.struct_map.get(name) {
                        for a in args { self.compile_expr(a)?; }
                        self.emit(OP_NEW_STRUCT); self.emit(sidx as u8); self.emit(args.len() as u8);
                        return Ok(());
                    }
                    if let Some(&fidx) = self.fn_map.get(name) {
                        for a in args { self.compile_expr(a)?; }
                        self.emit(OP_CALL); self.emit(fidx as u8); self.emit(args.len() as u8);
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
            _ => Err("invalid call target".into()),
        }
    }
}

'@

# ═══ src/vm/mod.rs ═══
$mod_rs = @'
pub mod value;
pub mod compiler;

use self::value::*;
use self::compiler::*;

const STACK_MAX: usize = 256;
const FRAME_MAX: usize = 64;

#[derive(Clone, Copy)]
struct CallFrame { fn_idx: usize, ip: usize, slot: usize }

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
            frames: [CallFrame { fn_idx: 0, ip: 0, slot: 0 }; FRAME_MAX], fc: 1,
            functions: c.functions, struct_defs: c.struct_defs, heap: c.heap,
        }
    }

    #[inline(always)] fn push(&mut self, v: Value) { self.stack[self.sp] = v; self.sp += 1; }
    #[inline(always)] fn pop(&mut self) -> Value { self.sp -= 1; self.stack[self.sp] }
    #[inline(always)] fn peek(&self) -> Value { self.stack[self.sp - 1] }

    fn get_field_val(&self, obj: Value, name_val: Value) -> Value {
        if !obj.is_obj() { return Value::NIL; }
        match self.heap.get(obj.as_obj()) {
            Obj::Struct { type_idx, fields } => {
                if let Obj::Str(s) = self.heap.get(name_val.as_obj()) {
                    let sdef = &self.struct_defs[*type_idx as usize];
                    if let Some(pos) = sdef.field_names.iter().position(|f| f == s) {
                        return fields[pos];
                    }
                }
                Value::NIL
            }
            _ => Value::NIL,
        }
    }

    fn find_method(&self, obj: Value, name: &str) -> Option<usize> {
        if !obj.is_obj() { return None; }
        if let Obj::Struct { type_idx, .. } = self.heap.get(obj.as_obj()) {
            let sdef = &self.struct_defs[*type_idx as usize];
            return sdef.methods.get(name).copied();
        }
        None
    }

    pub fn run(&mut self) -> Result<(), String> {
        loop {
            let fc = self.fc;
            let fi = self.frames[fc - 1].fn_idx;
            let ip = self.frames[fc - 1].ip;
            let slot = self.frames[fc - 1].slot;
            let op = self.functions[fi].code[ip];

            match op {
                OP_CONSTANT => {
                    let idx = self.functions[fi].code[ip + 1] as usize;
                    let val = self.functions[fi].constants[idx];
                    self.push(val);
                    self.frames[fc - 1].ip = ip + 2;
                }
                OP_NIL => { self.push(Value::NIL); self.frames[fc - 1].ip = ip + 1; }
                OP_TRUE => { self.push(Value::TRUE); self.frames[fc - 1].ip = ip + 1; }
                OP_FALSE => { self.push(Value::FALSE); self.frames[fc - 1].ip = ip + 1; }
                OP_POP => { self.sp -= 1; self.frames[fc - 1].ip = ip + 1; }

                OP_GET_LOCAL => {
                    let idx = self.functions[fi].code[ip + 1] as usize;
                    let val = self.stack[slot + idx];
                    self.push(val);
                    self.frames[fc - 1].ip = ip + 2;
                }
                OP_SET_LOCAL => {
                    let idx = self.functions[fi].code[ip + 1] as usize;
                    let val = self.pop();
                    self.stack[slot + idx] = val;
                    self.frames[fc - 1].ip = ip + 2;
                }

                OP_ADD => {
                    let b = self.pop(); let a = self.pop();
                    self.push(Value::number(a.as_number() + b.as_number()));
                    self.frames[fc - 1].ip = ip + 1;
                }
                OP_SUB => {
                    let b = self.pop(); let a = self.pop();
                    self.push(Value::number(a.as_number() - b.as_number()));
                    self.frames[fc - 1].ip = ip + 1;
                }
                OP_MUL => {
                    let b = self.pop(); let a = self.pop();
                    self.push(Value::number(a.as_number() * b.as_number()));
                    self.frames[fc - 1].ip = ip + 1;
                }
                OP_DIV => {
                    let b = self.pop(); let a = self.pop();
                    self.push(Value::number(a.as_number() / b.as_number()));
                    self.frames[fc - 1].ip = ip + 1;
                }
                OP_MOD => {
                    let b = self.pop(); let a = self.pop();
                    self.push(Value::number(a.as_number() % b.as_number()));
                    self.frames[fc - 1].ip = ip + 1;
                }
                OP_NEGATE => {
                    let v = self.pop();
                    self.push(Value::number(-v.as_number()));
                    self.frames[fc - 1].ip = ip + 1;
                }
                OP_NOT => {
                    let v = self.pop();
                    self.push(Value::boolean(!v.is_truthy()));
                    self.frames[fc - 1].ip = ip + 1;
                }

                OP_EQ => {
                    let b = self.pop(); let a = self.pop();
                    self.push(Value::boolean(a.0 == b.0));
                    self.frames[fc - 1].ip = ip + 1;
                }
                OP_NEQ => {
                    let b = self.pop(); let a = self.pop();
                    self.push(Value::boolean(a.0 != b.0));
                    self.frames[fc - 1].ip = ip + 1;
                }
                OP_LT => {
                    let b = self.pop(); let a = self.pop();
                    self.push(Value::boolean(a.as_number() < b.as_number()));
                    self.frames[fc - 1].ip = ip + 1;
                }
                OP_GT => {
                    let b = self.pop(); let a = self.pop();
                    self.push(Value::boolean(a.as_number() > b.as_number()));
                    self.frames[fc - 1].ip = ip + 1;
                }
                OP_LTE => {
                    let b = self.pop(); let a = self.pop();
                    self.push(Value::boolean(a.as_number() <= b.as_number()));
                    self.frames[fc - 1].ip = ip + 1;
                }
                OP_GTE => {
                    let b = self.pop(); let a = self.pop();
                    self.push(Value::boolean(a.as_number() >= b.as_number()));
                    self.frames[fc - 1].ip = ip + 1;
                }

                OP_JUMP => {
                    let hi = self.functions[fi].code[ip + 1] as usize;
                    let lo = self.functions[fi].code[ip + 2] as usize;
                    self.frames[fc - 1].ip = ip + 3 + ((hi << 8) | lo);
                }
                OP_LOOP => {
                    let hi = self.functions[fi].code[ip + 1] as usize;
                    let lo = self.functions[fi].code[ip + 2] as usize;
                    self.frames[fc - 1].ip = ip + 3 - ((hi << 8) | lo);
                }
                OP_JUMP_IF_FALSE => {
                    let hi = self.functions[fi].code[ip + 1] as usize;
                    let lo = self.functions[fi].code[ip + 2] as usize;
                    let offset = (hi << 8) | lo;
                    if !self.peek().is_truthy() {
                        self.frames[fc - 1].ip = ip + 3 + offset;
                    } else {
                        self.frames[fc - 1].ip = ip + 3;
                    }
                }

                OP_CALL => {
                    let callee = self.functions[fi].code[ip + 1] as usize;
                    let argc = self.functions[fi].code[ip + 2] as usize;
                    self.frames[fc - 1].ip = ip + 3;
                    self.frames[fc] = CallFrame { fn_idx: callee, ip: 0, slot: self.sp - argc };
                    self.fc += 1;
                    continue;
                }
                OP_RETURN => {
                    let result = self.pop();
                    self.sp = slot;
                    self.fc -= 1;
                    if self.fc == 0 { return Ok(()); }
                    self.push(result);
                    continue;
                }

                OP_PRINT => {
                    let argc = self.functions[fi].code[ip + 1] as usize;
                    let mut parts = Vec::with_capacity(argc);
                    for _ in 0..argc {
                        let v = self.pop();
                        parts.push(v.display(&self.heap));
                    }
                    parts.reverse();
                    println!("{}", parts.join(" "));
                    self.frames[fc - 1].ip = ip + 2;
                }
                OP_SQRT => {
                    let v = self.pop().as_number();
                    self.push(Value::number(v.sqrt()));
                    self.frames[fc - 1].ip = ip + 1;
                }
                OP_ABS => {
                    let v = self.pop().as_number();
                    self.push(Value::number(v.abs()));
                    self.frames[fc - 1].ip = ip + 1;
                }
                OP_LEN => {
                    let v = self.pop();
                    let len = if v.is_obj() {
                        match self.heap.get(v.as_obj()) {
                            Obj::Array(items) => items.len(),
                            Obj::Str(s) => s.len(),
                            _ => 0,
                        }
                    } else { 0 };
                    self.push(Value::number(len as f64));
                    self.frames[fc - 1].ip = ip + 1;
                }
                OP_MIN => {
                    let b = self.pop().as_number(); let a = self.pop().as_number();
                    self.push(Value::number(a.min(b)));
                    self.frames[fc - 1].ip = ip + 1;
                }
                OP_MAX => {
                    let b = self.pop().as_number(); let a = self.pop().as_number();
                    self.push(Value::number(a.max(b)));
                    self.frames[fc - 1].ip = ip + 1;
                }

                OP_NEW_ARRAY => {
                    let count = self.functions[fi].code[ip + 1] as usize;
                    let mut items = Vec::with_capacity(count);
                    for _ in 0..count { items.push(self.pop()); }
                    items.reverse();
                    let idx = self.heap.alloc(Obj::Array(items));
                    self.push(Value::obj(idx));
                    self.frames[fc - 1].ip = ip + 2;
                }
                OP_GET_INDEX => {
                    let idx = self.pop().as_number() as usize;
                    let arr = self.pop();
                    if arr.is_obj() {
                        if let Obj::Array(items) = self.heap.get(arr.as_obj()) {
                            if idx < items.len() { self.push(items[idx]); }
                            else { self.push(Value::NIL); }
                        } else { self.push(Value::NIL); }
                    } else { self.push(Value::NIL); }
                    self.frames[fc - 1].ip = ip + 1;
                }
                OP_NEW_STRUCT => {
                    let type_idx = self.functions[fi].code[ip + 1];
                    let argc = self.functions[fi].code[ip + 2] as usize;
                    let mut fields = Vec::with_capacity(argc);
                    for _ in 0..argc { fields.push(self.pop()); }
                    fields.reverse();
                    let idx = self.heap.alloc(Obj::Struct { type_idx: type_idx as u16, fields });
                    self.push(Value::obj(idx));
                    self.frames[fc - 1].ip = ip + 3;
                }
                OP_GET_FIELD => {
                    let ci = self.functions[fi].code[ip + 1] as usize;
                    let name_val = self.functions[fi].constants[ci];
                    let obj = self.pop();
                    let result = self.get_field_val(obj, name_val);
                    self.push(result);
                    self.frames[fc - 1].ip = ip + 2;
                }
                OP_INVOKE => {
                    let ci = self.functions[fi].code[ip + 1] as usize;
                    let argc = self.functions[fi].code[ip + 2] as usize;
                    let name_val = self.functions[fi].constants[ci];
                    let method_name = match self.heap.get(name_val.as_obj()) {
                        Obj::Str(s) => s.clone(),
                        _ => return Err("invalid method".into()),
                    };
                    let obj = self.stack[self.sp - argc - 1];
                    match self.find_method(obj, &method_name) {
                        Some(method_fi) => {
                            self.frames[fc - 1].ip = ip + 3;
                            self.frames[fc] = CallFrame { fn_idx: method_fi, ip: 0, slot: self.sp - argc - 1 };
                            self.fc += 1;
                            continue;
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

# ═══ src/main.rs ═══
$main_rs = @'
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
        println!("  VOLT 0.2.0\n");
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

# ═══ Write all files ═══
[System.IO.File]::WriteAllText("$PWD\src\vm\value.rs", $value_rs)
Write-Host "  [OK] src\vm\value.rs" -ForegroundColor Green

[System.IO.File]::WriteAllText("$PWD\src\vm\compiler.rs", $compiler_rs)
Write-Host "  [OK] src\vm\compiler.rs" -ForegroundColor Green

[System.IO.File]::WriteAllText("$PWD\src\vm\mod.rs", $mod_rs)
Write-Host "  [OK] src\vm\mod.rs" -ForegroundColor Green

[System.IO.File]::WriteAllText("$PWD\src\main.rs", $main_rs)
Write-Host "  [OK] src\main.rs" -ForegroundColor Green

# ═══ Update Cargo.toml with release profile ═══
$cargo = Get-Content "$PWD\Cargo.toml" -Raw
if ($cargo -notmatch "profile.release") {
    $release = @"

[profile.release]
opt-level = 3
lto = "fat"
codegen-units = 1
panic = "abort"
"@
    Add-Content -Path "$PWD\Cargo.toml" -Value $release
    Write-Host "  [OK] Cargo.toml (release profile added)" -ForegroundColor Green
} else {
    Write-Host "  [OK] Cargo.toml (release profile exists)" -ForegroundColor Yellow
}

Write-Host "`n  Bytecode VM installed!" -ForegroundColor Cyan
Write-Host ""
Write-Host "  Test (debug mode):" -ForegroundColor White
Write-Host "    cargo run -- run tests\programs\simple.volt" -ForegroundColor Yellow
Write-Host "    cargo run -- run tests\programs\fibonacci.volt" -ForegroundColor Yellow
Write-Host "    cargo run -- run tests\programs\full_test.volt" -ForegroundColor Yellow
Write-Host ""
Write-Host "  Benchmark (release mode - FAST):" -ForegroundColor White
Write-Host "    cargo build --release" -ForegroundColor Yellow
Write-Host "    .\target\release\volt.exe run tests\programs\fibonacci.volt" -ForegroundColor Yellow
Write-Host ""

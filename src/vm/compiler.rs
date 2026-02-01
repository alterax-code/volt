#![allow(dead_code)]
use std::collections::HashMap;
use crate::parser::ast::*;
use super::value::*;

// â”€â”€ Opcodes â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
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

    // â”€â”€ Lambda / Closure compilation â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
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

    // â”€â”€ Statements â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
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
                // Nested function â†’ compile as local closure
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

    // â”€â”€ Expressions â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
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
                    // Named function as value â†’ wrap in zero-capture closure
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
                    // Local variable â†’ closure call
                    if let Some(local_idx) = self.resolve_local(name) {
                        self.emit2(OP_GET_LOCAL, local_idx);
                        for a in args { self.compile_expr(a)?; }
                        self.emit2(OP_CALL_VALUE, args.len() as u8);
                        return Ok(());
                    }
                    // Upvalue â†’ closure call
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
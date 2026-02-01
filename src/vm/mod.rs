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
#[allow(dead_code)]
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

    // â”€â”€ Closure call helpers (for array methods) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
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

    fn handle_error(&mut self, msg: String, _base_fc: usize) -> Result<(), String> {
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

                // â”€â”€ Upvalue access (closures) â”€â”€
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

                // â”€â”€ Arithmetic (polymorphic ADD for strings) â”€â”€
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

                // â”€â”€ Comparison (with string value equality) â”€â”€
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

                // â”€â”€ Control flow â”€â”€
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

                // â”€â”€ Functions â”€â”€
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

                // â”€â”€ Closure creation â”€â”€
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

                // â”€â”€ Closure call â”€â”€
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

                // â”€â”€ Builtins â”€â”€
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

                // â”€â”€ String builtins â”€â”€
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

                // â”€â”€ Data structures â”€â”€
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

                // â”€â”€ Method invocation (structs + string + array methods) â”€â”€
                OP_INVOKE => {
                    let ci = *code.add(ip + 1) as usize;
                    let argc = *code.add(ip + 2) as usize;
                    let name_val = *func.constants.get_unchecked(ci);
                    let method_name = match self.heap.get(name_val.as_obj()) {
                        Obj::Str(s) => s.clone(), _ => return Err("invalid method".into()),
                    };
                    let obj = *self.stack.get_unchecked(self.sp - argc - 1);

                    // â”€â”€ Builtin string methods â”€â”€
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

                    // â”€â”€ Builtin array methods â”€â”€
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

                    // â”€â”€ Builtin table methods â”€â”€
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

                    // â”€â”€ Struct method fallthrough â”€â”€
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
                // â”€â”€ Error handling â”€â”€
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
                // â”€â”€ Type checking â”€â”€
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
                // â”€â”€ Map â”€â”€
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
                // â”€â”€ File I/O â”€â”€
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
                // â”€â”€ CSV / Table â”€â”€
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
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

pub struct Heap {
    objects: Vec<Obj>,
    strings: std::collections::HashMap<String, u32>,
    free_list: Vec<u32>,
}
impl Heap {
    pub fn new() -> Self { Self { objects: Vec::new(), strings: std::collections::HashMap::new(), free_list: Vec::new() } }
    pub fn alloc(&mut self, obj: Obj) -> u32 {
        // String interning
        if let Obj::Str(ref s) = obj {
            if let Some(&idx) = self.strings.get(s) { return idx; }
            let idx = self.alloc_raw(obj);
            if let Obj::Str(ref s) = self.objects[idx as usize] {
                self.strings.insert(s.clone(), idx);
            }
            return idx;
        }
        self.alloc_raw(obj)
    }
    fn alloc_raw(&mut self, obj: Obj) -> u32 {
        if let Some(idx) = self.free_list.pop() {
            self.objects[idx as usize] = obj;
            idx
        } else {
            let i = self.objects.len() as u32;
            self.objects.push(obj);
            i
        }
    }
    pub fn get(&self, i: u32) -> &Obj { &self.objects[i as usize] }
    pub fn get_mut(&mut self, i: u32) -> &mut Obj { &mut self.objects[i as usize] }
    #[allow(dead_code)]
    pub fn len(&self) -> usize { self.objects.len() }
}
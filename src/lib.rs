//! # VOLT â€” A fast, expressive scripting language
//!
//! VOLT is a bytecode-compiled scripting language with:
//! - Indentation-based syntax
//! - NaN-boxed VM for high performance
//! - First-class functions and closures
//! - Pattern matching, f-strings, error handling
//! - Table type with SQL-like query methods
//! - File I/O and pipe operator
//!
//! ## Quick Start
//!
//! ```rust,no_run
//! use volt::run;
//! run("print(\"Hello from VOLT!\")").unwrap();
//! ```
//!
//! ## Compile and Run Separately
//!
//! ```rust,no_run
//! use volt::{compile, VM};
//! let compiled = compile("x := 42\nprint(x)").unwrap();
//! let mut vm = VM::new(compiled);
//! vm.run().unwrap();
//! ```

pub mod lexer;
pub mod parser;
pub mod vm;

use std::path::Path;
use std::collections::HashSet;
use std::fs;

pub use lexer::Lexer;
pub use parser::Parser;
pub use parser::ast::{Stmt, Expr, BinOp, UnaryOp, Pattern, Type, Param, MatchArm, Field};
pub use vm::compiler::Compiler;
pub use vm::VM;

/// Resolve `use` imports recursively from a program's AST.
pub fn resolve_imports(stmts: &[Stmt], base_dir: &Path, imported: &mut HashSet<String>) -> Result<Vec<Stmt>, String> {
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

/// Compile VOLT source code into bytecode.
///
/// Returns a `Compiler` containing the compiled bytecode, constants, and heap.
/// Pass this to `VM::new()` to execute.
pub fn compile(source: &str) -> Result<Compiler, String> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;
    let program = resolve_imports(&program, Path::new("."), &mut HashSet::new())?;
    Compiler::compile(&program)
}

/// Compile and run VOLT source code in one step.
///
/// Equivalent to calling `compile()` then `VM::new(compiled).run()`.
pub fn run(source: &str) -> Result<(), String> {
    let compiled = compile(source)?;
    let mut machine = VM::new(compiled);
    machine.run()
}

/// Compile and run a VOLT file, resolving imports relative to the file's directory.
pub fn run_file(path: &str) -> Result<(), String> {
    let source = fs::read_to_string(path)
        .map_err(|e| format!("cannot read '{}': {}", path, e))?;
    let mut lexer = Lexer::new(&source);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;
    let base_dir = Path::new(path).parent().unwrap_or(Path::new("."));
    let mut imported = HashSet::new();
    let program = resolve_imports(&program, base_dir, &mut imported)?;
    let compiled = Compiler::compile(&program)?;
    let mut machine = VM::new(compiled);
    machine.run()
}
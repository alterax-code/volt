use std::env;
use std::fs;
use std::time::Instant;
use std::path::Path;
use std::collections::HashSet;

use volt::Lexer;
use volt::Parser;
use volt::parser::ast::*;
use volt::vm;
use volt::resolve_imports;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("  VOLT 1.0\n");
        println!("  Usage:");
        println!("    volt run <file.volt>     Run program");
        println!("    volt repl                Interactive REPL");
        println!("    volt <file.volt>         Show AST");
        return;
    }

    if args[1] == "repl" {
        run_repl();
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

    let base_dir = Path::new(filename).parent().unwrap_or(Path::new("."));
    let mut imported = HashSet::new();
    let program = match resolve_imports(&program, base_dir, &mut imported) {
        Ok(p) => p,
        Err(e) => { eprintln!("Import error: {}", e); return; }
    };

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

fn run_repl() {
    use std::io::{self, Write, BufRead, IsTerminal};

    let interactive = io::stdin().is_terminal();
    if interactive {
        println!("\n  VOLT 1.0 REPL");
        println!("  :help for commands, :quit to exit\n");
    }

    let mut defs = String::new();
    let mut vars = String::new();

    loop {
        if interactive { print!(">> "); io::stdout().flush().ok(); }

        let mut line = String::new();
        match io::stdin().lock().read_line(&mut line) {
            Ok(0) | Err(_) => break,
            _ => {}
        }
        let trimmed = line.trim();
        if trimmed.is_empty() { continue; }

        if trimmed.starts_with(':') {
            match trimmed {
                ":quit" | ":q" => break,
                ":help" | ":h" => {
                    println!("  :quit    Exit REPL");
                    println!("  :clear   Reset all state");
                    println!("  :defs    Show definitions");
                    println!("  :vars    Show variables");
                    println!("  :help    Show this help");
                    continue;
                }
                ":clear" | ":c" => { defs.clear(); vars.clear(); println!("  State cleared."); continue; }
                ":defs" | ":d" => { if defs.is_empty() { println!("  (none)"); } else { print!("{}", defs); } continue; }
                ":vars" | ":v" => { if vars.is_empty() { println!("  (none)"); } else { print!("{}", vars); } continue; }
                _ => { println!("  Unknown command. Type :help"); continue; }
            }
        }

        let mut input = trimmed.to_string();
        let block_kw = ["fn ", "pub fn ", "if ", "for ", "while ", "try", "match ", "struct "];
        if block_kw.iter().any(|k| input.starts_with(k)) {
            loop {
                if interactive { print!(".. "); io::stdout().flush().ok(); }
                let mut cont = String::new();
                match io::stdin().lock().read_line(&mut cont) {
                    Ok(0) | Err(_) => break,
                    _ => {}
                }
                if cont.trim().is_empty() { break; }
                input.push('\n');
                input.push_str(cont.trim_end_matches(|c: char| c == '\n' || c == '\r'));
            }
        }

        let exec_input = if is_repl_expr(&input) {
            format!("print({})", input)
        } else {
            input.clone()
        };

        let source = format!("{}\n{}\n{}", defs, vars, exec_input);

        let mut lexer = Lexer::new(&source);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);

        let result = parser.parse()
            .and_then(|prog| {
                let base = Path::new(".");
                let mut imp = HashSet::new();
                resolve_imports(&prog, base, &mut imp)
            })
            .and_then(|prog| vm::compiler::Compiler::compile(&prog));

        match result {
            Ok(compiler) => {
                let mut machine = vm::VM::new(compiler);
                match machine.run() {
                    Ok(()) => {
                        let t = input.trim();
                        if t.starts_with("fn ") || t.starts_with("pub fn ") || t.starts_with("struct ") || t.starts_with("use ") {
                            defs.push_str(&input); defs.push('\n');
                        } else if t.contains(":=") {
                            vars.push_str(&input); vars.push('\n');
                        }
                    }
                    Err(e) => eprintln!("  Error: {}", e),
                }
            }
            Err(e) => eprintln!("  Error: {}", e),
        }
    }
    if interactive { println!("\n  Goodbye!"); }
}

fn is_repl_expr(input: &str) -> bool {
    let t = input.trim();
    if t.is_empty() || t.contains('\n') { return false; }
    let kw = ["fn ", "struct ", "if ", "for ", "while ", "try", "match ",
              "mut ", "return", "use ", "throw ", "break", "continue", "print(", "pub "];
    if kw.iter().any(|k| t.starts_with(k)) { return false; }
    if t.contains(":=") { return false; }
    let no_ops = t.replace("==", "XX").replace("!=", "XX").replace("<=", "XX").replace(">=", "XX");
    if no_ops.contains(" = ") { return false; }
    true
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
            Stmt::TryCatch { try_body, catch_var, catch_body } => {
                println!("{}try", pad); print_ast(try_body, depth + 1);
                println!("{}catch {}", pad, catch_var); print_ast(catch_body, depth + 1);
            }
            Stmt::Throw(e) => println!("{}throw {}", pad, fmt_expr(e)),
            Stmt::Break => println!("{}break", pad),
            Stmt::Continue => println!("{}continue", pad),
            Stmt::Match { expr, arms } => {
                println!("{}match {}", pad, fmt_expr(expr));
                for arm in arms {
                    println!("{}  {} ->", pad, fmt_pattern(&arm.pattern));
                    print_ast(&arm.body, depth + 2);
                }
            }
            Stmt::Use { path } => println!("{}use \"{}\"", pad, path),
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
        Expr::Map(entries) => { let e: Vec<String> = entries.iter().map(|(k, v)| format!("{}: {}", k, fmt_expr(v))).collect(); format!("{{{}}}", e.join(", ")) }
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
        BinOp::LtEq => "<=", BinOp::GtEq => ">=", BinOp::And => "and", BinOp::Or => "or", BinOp::Range => "..", BinOp::Pipe => "|>",
    }
}
fn fmt_unary(op: &UnaryOp) -> &str { match op { UnaryOp::Neg => "-", UnaryOp::Not => "not " } }
fn fmt_pattern(p: &Pattern) -> String {
    match p {
        Pattern::Int(n) => n.to_string(), Pattern::Float(n) => n.to_string(),
        Pattern::Str(s) => format!("\"{}\"", s), Pattern::Bool(b) => b.to_string(),
        Pattern::Wildcard => "_".to_string(), Pattern::Binding(n) => n.clone(),
        Pattern::Or(pats) => pats.iter().map(|p| fmt_pattern(p)).collect::<Vec<_>>().join(" | "),
    }
}
fn fmt_type(t: &Type) -> String {
    match t {
        Type::Named(n) => n.clone(), Type::Optional(i) => format!("?{}", fmt_type(i)),
        Type::Array(i) => format!("[]{}", fmt_type(i)),
        Type::Generic(n, p) => { let ps: Vec<String> = p.iter().map(|x| fmt_type(x)).collect(); format!("{}[{}]", n, ps.join(", ")) }
    }
}
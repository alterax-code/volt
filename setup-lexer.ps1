## VOLT Lexer - Setup Script
## Run this from C:\Users\lucas\volt

Write-Host "`n  ⚡ VOLT - Installing Lexer files...`n" -ForegroundColor Cyan

# --- src/lexer/token.rs ---
$token = @'
/// All token types in the VOLT language.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // --- Keywords ---
    Fn,
    If,
    Elif,
    Else,
    For,
    While,
    Return,
    Struct,
    Mut,
    Pub,
    Use,
    Match,
    Mod,
    True,
    False,
    In,
    Or,
    And,
    Not,

    // --- Literals ---
    Int(i64),
    Float(f64),
    Str(String),

    // --- Identifier ---
    Ident(String),

    // --- Operators ---
    Plus,       // +
    Minus,      // -
    Star,       // *
    Slash,      // /
    Percent,    // %
    Eq,         // =
    EqEq,       // ==
    NotEq,      // !=
    Lt,         // <
    Gt,         // >
    LtEq,       // <=
    GtEq,       // >=
    ColonEq,    // :=
    ColonColon, // ::
    Arrow,      // ->
    Question,   // ?
    Dot,        // .
    DotDot,     // ..

    // --- Delimiters ---
    LParen,   // (
    RParen,   // )
    LBracket, // [
    RBracket, // ]
    LBrace,   // {
    RBrace,   // }
    Colon,    // :
    Comma,    // ,

    // --- Structure ---
    Newline,
    Indent,
    Dedent,
    EOF,
}

/// A token with its position in the source code.
#[derive(Debug, Clone)]
pub struct Spanned {
    pub token: Token,
    pub line: usize,
    pub col: usize,
}
'@

# --- src/lexer/mod.rs ---
$lexer = @'
pub mod token;

use self::token::{Token, Spanned};

/// The VOLT lexer. Transforms source code into a stream of tokens.
/// Handles indentation-based blocks (like Python).
pub struct Lexer {
    chars: Vec<char>,
    pos: usize,
    line: usize,
    col: usize,
    indent_stack: Vec<usize>,
}

impl Lexer {
    /// Create a new lexer from source code.
    pub fn new(source: &str) -> Self {
        Self {
            chars: source.chars().collect(),
            pos: 0,
            line: 1,
            col: 1,
            indent_stack: vec![0],
        }
    }

    fn at_end(&self) -> bool {
        self.pos >= self.chars.len()
    }

    fn current(&self) -> char {
        self.chars[self.pos]
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos + 1).copied()
    }

    fn advance(&mut self) -> char {
        let ch = self.chars[self.pos];
        self.pos += 1;
        self.col += 1;
        ch
    }

    fn make(&self, token: Token, line: usize, col: usize) -> Spanned {
        Spanned { token, line, col }
    }

    fn skip_inline_whitespace(&mut self) {
        while !self.at_end() && self.current() == ' ' {
            self.advance();
        }
    }

    fn skip_comment(&mut self) {
        while !self.at_end() && self.current() != '\n' {
            self.pos += 1;
        }
    }

    /// Read a string literal: "hello world"
    fn read_string(&mut self) -> Spanned {
        let (line, col) = (self.line, self.col);
        self.advance(); // skip opening "

        let mut s = String::new();
        while !self.at_end() && self.current() != '"' && self.current() != '\n' {
            if self.current() == '\\' {
                self.advance();
                if !self.at_end() {
                    match self.current() {
                        'n' => s.push('\n'),
                        't' => s.push('\t'),
                        '\\' => s.push('\\'),
                        '"' => s.push('"'),
                        other => {
                            s.push('\\');
                            s.push(other);
                        }
                    }
                    self.advance();
                }
            } else {
                s.push(self.advance());
            }
        }

        if !self.at_end() && self.current() == '"' {
            self.advance(); // skip closing "
        }

        self.make(Token::Str(s), line, col)
    }

    /// Read a number: 42 or 3.14
    fn read_number(&mut self) -> Spanned {
        let (line, col) = (self.line, self.col);
        let mut num = String::new();
        let mut is_float = false;

        while !self.at_end() && (self.current().is_ascii_digit() || self.current() == '.') {
            if self.current() == '.' {
                if is_float {
                    break;
                }
                match self.peek() {
                    Some(c) if c.is_ascii_digit() => is_float = true,
                    _ => break,
                }
            }
            num.push(self.advance());
        }

        let token = if is_float {
            Token::Float(num.parse().unwrap_or(0.0))
        } else {
            Token::Int(num.parse().unwrap_or(0))
        };

        self.make(token, line, col)
    }

    /// Read an identifier or keyword.
    fn read_ident(&mut self) -> Spanned {
        let (line, col) = (self.line, self.col);
        let mut name = String::new();

        while !self.at_end() && (self.current().is_alphanumeric() || self.current() == '_') {
            name.push(self.advance());
        }

        let token = match name.as_str() {
            "fn" => Token::Fn,
            "if" => Token::If,
            "elif" => Token::Elif,
            "else" => Token::Else,
            "for" => Token::For,
            "while" => Token::While,
            "return" => Token::Return,
            "struct" => Token::Struct,
            "mut" => Token::Mut,
            "pub" => Token::Pub,
            "use" => Token::Use,
            "match" => Token::Match,
            "mod" => Token::Mod,
            "true" => Token::True,
            "false" => Token::False,
            "in" => Token::In,
            "or" => Token::Or,
            "and" => Token::And,
            "not" => Token::Not,
            _ => Token::Ident(name),
        };

        self.make(token, line, col)
    }

    /// Tokenize the entire source code.
    pub fn tokenize(&mut self) -> Vec<Spanned> {
        let mut tokens: Vec<Spanned> = Vec::new();
        let mut at_line_start = true;

        while !self.at_end() {
            // ── Handle indentation at the start of each line ──
            if at_line_start {
                let mut spaces: usize = 0;
                while !self.at_end() && (self.current() == ' ' || self.current() == '\t') {
                    if self.current() == '\t' {
                        spaces += 4;
                    } else {
                        spaces += 1;
                    }
                    self.pos += 1;
                    self.col += 1;
                }

                // Skip carriage return (Windows)
                if !self.at_end() && self.current() == '\r' {
                    self.pos += 1;
                }

                // Skip blank lines
                if !self.at_end() && self.current() == '\n' {
                    self.pos += 1;
                    self.line += 1;
                    self.col = 1;
                    continue;
                }

                // Skip comment-only lines
                if !self.at_end() && self.current() == '/' {
                    if self.peek() == Some('/') {
                        self.skip_comment();
                        continue;
                    }
                }

                if self.at_end() {
                    break;
                }

                // Emit Indent / Dedent tokens
                let current_indent = *self.indent_stack.last().unwrap();

                if spaces > current_indent {
                    self.indent_stack.push(spaces);
                    tokens.push(self.make(Token::Indent, self.line, 1));
                } else {
                    while spaces < *self.indent_stack.last().unwrap() {
                        self.indent_stack.pop();
                        tokens.push(self.make(Token::Dedent, self.line, 1));
                    }
                }

                at_line_start = false;
            }

            // ── Skip inline whitespace ──
            self.skip_inline_whitespace();
            if self.at_end() {
                break;
            }

            let (line, col) = (self.line, self.col);
            let ch = self.current();

            match ch {
                // ── Newline ──
                '\n' => {
                    tokens.push(self.make(Token::Newline, line, col));
                    self.pos += 1;
                    self.line += 1;
                    self.col = 1;
                    at_line_start = true;
                }

                // ── Carriage return (Windows) ──
                '\r' => {
                    self.pos += 1;
                }

                // ── Comments ──
                '/' if self.peek() == Some('/') => {
                    self.skip_comment();
                }

                // ── Literals ──
                '"' => tokens.push(self.read_string()),
                '0'..='9' => tokens.push(self.read_number()),
                'a'..='z' | 'A'..='Z' | '_' => tokens.push(self.read_ident()),

                // ── Two-character operators (matched FIRST) ──
                '-' if self.peek() == Some('>') => {
                    self.advance(); self.advance();
                    tokens.push(self.make(Token::Arrow, line, col));
                }
                ':' if self.peek() == Some('=') => {
                    self.advance(); self.advance();
                    tokens.push(self.make(Token::ColonEq, line, col));
                }
                ':' if self.peek() == Some(':') => {
                    self.advance(); self.advance();
                    tokens.push(self.make(Token::ColonColon, line, col));
                }
                '=' if self.peek() == Some('=') => {
                    self.advance(); self.advance();
                    tokens.push(self.make(Token::EqEq, line, col));
                }
                '!' if self.peek() == Some('=') => {
                    self.advance(); self.advance();
                    tokens.push(self.make(Token::NotEq, line, col));
                }
                '<' if self.peek() == Some('=') => {
                    self.advance(); self.advance();
                    tokens.push(self.make(Token::LtEq, line, col));
                }
                '>' if self.peek() == Some('=') => {
                    self.advance(); self.advance();
                    tokens.push(self.make(Token::GtEq, line, col));
                }
                '.' if self.peek() == Some('.') => {
                    self.advance(); self.advance();
                    tokens.push(self.make(Token::DotDot, line, col));
                }

                // ── Single-character operators ──
                '+' => { self.advance(); tokens.push(self.make(Token::Plus, line, col)); }
                '-' => { self.advance(); tokens.push(self.make(Token::Minus, line, col)); }
                '*' => { self.advance(); tokens.push(self.make(Token::Star, line, col)); }
                '/' => { self.advance(); tokens.push(self.make(Token::Slash, line, col)); }
                '%' => { self.advance(); tokens.push(self.make(Token::Percent, line, col)); }
                '=' => { self.advance(); tokens.push(self.make(Token::Eq, line, col)); }
                '<' => { self.advance(); tokens.push(self.make(Token::Lt, line, col)); }
                '>' => { self.advance(); tokens.push(self.make(Token::Gt, line, col)); }
                '.' => { self.advance(); tokens.push(self.make(Token::Dot, line, col)); }
                ':' => { self.advance(); tokens.push(self.make(Token::Colon, line, col)); }
                '?' => { self.advance(); tokens.push(self.make(Token::Question, line, col)); }

                // ── Delimiters ──
                '(' => { self.advance(); tokens.push(self.make(Token::LParen, line, col)); }
                ')' => { self.advance(); tokens.push(self.make(Token::RParen, line, col)); }
                '[' => { self.advance(); tokens.push(self.make(Token::LBracket, line, col)); }
                ']' => { self.advance(); tokens.push(self.make(Token::RBracket, line, col)); }
                '{' => { self.advance(); tokens.push(self.make(Token::LBrace, line, col)); }
                '}' => { self.advance(); tokens.push(self.make(Token::RBrace, line, col)); }
                ',' => { self.advance(); tokens.push(self.make(Token::Comma, line, col)); }

                // ── Unknown → skip ──
                _ => { self.advance(); }
            }
        }

        // ── Close all remaining indent blocks ──
        while self.indent_stack.len() > 1 {
            self.indent_stack.pop();
            tokens.push(self.make(Token::Dedent, self.line, 1));
        }

        tokens.push(self.make(Token::EOF, self.line, self.col));
        tokens
    }
}
'@

# --- src/main.rs ---
$main = @'
mod lexer;

use std::env;
use std::fs;

use lexer::Lexer;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("  ⚡ VOLT 0.1.0");
        println!();
        println!("  Usage:");
        println!("    volt <file.volt>         Tokenize a file (debug)");
        println!("    volt run <file.volt>     Run a file (coming soon)");
        println!("    volt build <file.volt>   Compile a file (coming soon)");
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

    println!();
    println!("  VOLT Lexer | {} tokens from '{}'", tokens.len(), filename);
    println!("  {}", "-".repeat(46));

    for tok in &tokens {
        let marker = match &tok.token {
            lexer::token::Token::Indent  => "  >>>",
            lexer::token::Token::Dedent  => "  <<<",
            lexer::token::Token::Newline => "",
            lexer::token::Token::EOF     => "",
            _ => "",
        };
        println!(
            "    {:>3}:{:<3}  {:?}{}",
            tok.line, tok.col, tok.token, marker
        );
    }

    println!("  {}", "-".repeat(46));
    println!("  Done.");
    println!();
}
'@

# --- tests/programs/hello.volt ---
$hello = @'
// Hello World in VOLT
fn add(a: int, b: int) -> int
    a + b

fn main()
    x := 42
    name := "Lucas"
    result := add(x, 8)

    if result > 40
        print("Big number!")
    else
        print("Small number")

    for i in 0..10
        print(i)
'@

# ── Write all files ──
[System.IO.File]::WriteAllText("$PWD\src\lexer\token.rs", $token)
Write-Host "  [OK] src\lexer\token.rs" -ForegroundColor Green

[System.IO.File]::WriteAllText("$PWD\src\lexer\mod.rs", $lexer)
Write-Host "  [OK] src\lexer\mod.rs" -ForegroundColor Green

[System.IO.File]::WriteAllText("$PWD\src\main.rs", $main)
Write-Host "  [OK] src\main.rs" -ForegroundColor Green

[System.IO.File]::WriteAllText("$PWD\tests\programs\hello.volt", $hello)
Write-Host "  [OK] tests\programs\hello.volt" -ForegroundColor Green

Write-Host "`n  All files created! Now run:" -ForegroundColor Cyan
Write-Host "    cargo run -- tests\programs\hello.volt" -ForegroundColor Yellow
Write-Host ""

pub mod token;

use self::token::{Token, Spanned};

pub struct Lexer {
    chars: Vec<char>,
    pos: usize,
    line: usize,
    col: usize,
    indent_stack: Vec<usize>,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self { chars: source.chars().collect(), pos: 0, line: 1, col: 1, indent_stack: vec![0] }
    }

    fn at_end(&self) -> bool { self.pos >= self.chars.len() }
    fn current(&self) -> char { self.chars[self.pos] }
    fn peek(&self) -> Option<char> { self.chars.get(self.pos + 1).copied() }
    fn advance(&mut self) -> char {
        let ch = self.chars[self.pos]; self.pos += 1; self.col += 1; ch
    }
    fn make(&self, token: Token, line: usize, col: usize) -> Spanned { Spanned { token, line, col } }
    fn skip_inline_whitespace(&mut self) { while !self.at_end() && self.current() == ' ' { self.advance(); } }
    fn skip_comment(&mut self) { while !self.at_end() && self.current() != '\n' { self.pos += 1; } }

    fn read_string(&mut self) -> Spanned {
        let (line, col) = (self.line, self.col);
        self.advance();
        let mut s = String::new();
        while !self.at_end() && self.current() != '"' && self.current() != '\n' {
            if self.current() == '\\' {
                self.advance();
                if !self.at_end() {
                    match self.current() {
                        'n' => s.push('\n'), 't' => s.push('\t'),
                        '\\' => s.push('\\'), '"' => s.push('"'),
                        other => { s.push('\\'); s.push(other); }
                    }
                    self.advance();
                }
            } else { s.push(self.advance()); }
        }
        if !self.at_end() && self.current() == '"' { self.advance(); }
        self.make(Token::Str(s), line, col)
    }

    fn read_number(&mut self) -> Spanned {
        let (line, col) = (self.line, self.col);
        let mut num = String::new();
        let mut is_float = false;
        while !self.at_end() && (self.current().is_ascii_digit() || self.current() == '.') {
            if self.current() == '.' {
                if is_float { break; }
                match self.peek() { Some(c) if c.is_ascii_digit() => is_float = true, _ => break }
            }
            num.push(self.advance());
        }
        let token = if is_float { Token::Float(num.parse().unwrap_or(0.0)) }
                    else { Token::Int(num.parse().unwrap_or(0)) };
        self.make(token, line, col)
    }

    fn read_ident(&mut self) -> Spanned {
        let (line, col) = (self.line, self.col);
        let mut name = String::new();
        while !self.at_end() && (self.current().is_alphanumeric() || self.current() == '_') {
            name.push(self.advance());
        }
        let token = match name.as_str() {
            "fn" => Token::Fn, "if" => Token::If, "elif" => Token::Elif,
            "else" => Token::Else, "for" => Token::For, "while" => Token::While,
            "return" => Token::Return, "struct" => Token::Struct, "mut" => Token::Mut,
            "pub" => Token::Pub, "use" => Token::Use, "match" => Token::Match,
            "mod" => Token::Mod, "true" => Token::True, "false" => Token::False,
            "in" => Token::In, "or" => Token::Or, "and" => Token::And, "not" => Token::Not,
            "try" => Token::Try, "catch" => Token::Catch, "throw" => Token::Throw,
            "break" => Token::Break, "continue" => Token::Continue,
            _ => Token::Ident(name),
        };
        self.make(token, line, col)
    }

    fn read_fstring(&mut self) -> Vec<Spanned> {
        let (line, col) = (self.line, self.col);
        self.advance(); self.advance();
        let mut result = vec![self.make(Token::FStringStart, line, col)];
        loop {
            if self.at_end() { break; }
            if self.current() == '"' {
                let (el, ec) = (self.line, self.col); self.advance();
                result.push(self.make(Token::FStringEnd, el, ec)); break;
            }
            if self.current() == '{' {
                let (el, ec) = (self.line, self.col); self.advance();
                result.push(self.make(Token::FStringExprStart, el, ec));
                let mut expr_str = String::new();
                let mut depth = 1;
                while !self.at_end() && depth > 0 {
                    let c = self.current();
                    if c == '{' { depth += 1; }
                    if c == '}' { depth -= 1; if depth == 0 { break; } }
                    expr_str.push(self.advance());
                }
                if !expr_str.is_empty() {
                    let mut sub = Lexer::new(&expr_str);
                    for t in sub.tokenize() {
                        match t.token {
                            Token::EOF | Token::Newline | Token::Indent | Token::Dedent => {}
                            _ => result.push(Spanned { token: t.token, line: el, col: ec }),
                        }
                    }
                }
                if !self.at_end() && self.current() == '}' {
                    let (el2, ec2) = (self.line, self.col); self.advance();
                    result.push(self.make(Token::FStringExprEnd, el2, ec2));
                }
                continue;
            }
            let (tl, tc) = (self.line, self.col);
            let mut text = String::new();
            while !self.at_end() && self.current() != '{' && self.current() != '"' && self.current() != '\n' {
                if self.current() == '\\' {
                    self.advance();
                    if !self.at_end() {
                        match self.current() {
                            'n' => text.push('\n'), 't' => text.push('\t'),
                            '\\' => text.push('\\'), '"' => text.push('"'),
                            '{' => text.push('{'), '}' => text.push('}'),
                            other => { text.push('\\'); text.push(other); }
                        }
                        self.advance();
                    }
                } else { text.push(self.advance()); }
            }
            if !text.is_empty() {
                result.push(self.make(Token::FStringText(text), tl, tc));
            }
        }
        result
    }

    pub fn tokenize(&mut self) -> Vec<Spanned> {
        let mut tokens: Vec<Spanned> = Vec::new();
        let mut at_line_start = true;

        while !self.at_end() {
            if at_line_start {
                let mut spaces: usize = 0;
                while !self.at_end() && (self.current() == ' ' || self.current() == '\t') {
                    if self.current() == '\t' { spaces += 4; } else { spaces += 1; }
                    self.pos += 1; self.col += 1;
                }
                if !self.at_end() && self.current() == '\r' { self.pos += 1; }
                if !self.at_end() && self.current() == '\n' {
                    self.pos += 1; self.line += 1; self.col = 1; continue;
                }
                if !self.at_end() && self.current() == '/' {
                    if self.peek() == Some('/') { self.skip_comment(); continue; }
                }
                if self.at_end() { break; }

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

            self.skip_inline_whitespace();
            if self.at_end() { break; }

            let (line, col) = (self.line, self.col);
            let ch = self.current();

            match ch {
                '\n' => { tokens.push(self.make(Token::Newline, line, col)); self.pos += 1; self.line += 1; self.col = 1; at_line_start = true; }
                '\r' => { self.pos += 1; }
                '/' if self.peek() == Some('/') => { self.skip_comment(); }
                '"' => tokens.push(self.read_string()),
                '0'..='9' => tokens.push(self.read_number()),

                'a'..='z' | 'A'..='Z' | '_' => {
                    if ch == 'f' && self.peek() == Some('"') {
                        tokens.extend(self.read_fstring());
                    } else {
                        tokens.push(self.read_ident());
                    }
                }

                '-' if self.peek() == Some('>') => { self.advance(); self.advance(); tokens.push(self.make(Token::Arrow, line, col)); }
                ':' if self.peek() == Some('=') => { self.advance(); self.advance(); tokens.push(self.make(Token::ColonEq, line, col)); }
                ':' if self.peek() == Some(':') => { self.advance(); self.advance(); tokens.push(self.make(Token::ColonColon, line, col)); }
                '=' if self.peek() == Some('=') => { self.advance(); self.advance(); tokens.push(self.make(Token::EqEq, line, col)); }
                '!' if self.peek() == Some('=') => { self.advance(); self.advance(); tokens.push(self.make(Token::NotEq, line, col)); }
                '<' if self.peek() == Some('=') => { self.advance(); self.advance(); tokens.push(self.make(Token::LtEq, line, col)); }
                '>' if self.peek() == Some('=') => { self.advance(); self.advance(); tokens.push(self.make(Token::GtEq, line, col)); }
                '.' if self.peek() == Some('.') => { self.advance(); self.advance(); tokens.push(self.make(Token::DotDot, line, col)); }

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
                '(' => { self.advance(); tokens.push(self.make(Token::LParen, line, col)); }
                ')' => { self.advance(); tokens.push(self.make(Token::RParen, line, col)); }
                '[' => { self.advance(); tokens.push(self.make(Token::LBracket, line, col)); }
                ']' => { self.advance(); tokens.push(self.make(Token::RBracket, line, col)); }
                '{' => { self.advance(); tokens.push(self.make(Token::LBrace, line, col)); }
                '}' => { self.advance(); tokens.push(self.make(Token::RBrace, line, col)); }
                ',' => { self.advance(); tokens.push(self.make(Token::Comma, line, col)); }
                '|' => {
                    self.advance();
                    if !self.at_end() && self.current() == '>' { self.advance(); tokens.push(self.make(Token::PipeArrow, line, col)); }
                    else { tokens.push(self.make(Token::Pipe, line, col)); }
                }
                _ => { self.advance(); }
            }
        }

        while self.indent_stack.len() > 1 {
            self.indent_stack.pop();
            tokens.push(self.make(Token::Dedent, self.line, 1));
        }
        tokens.push(self.make(Token::EOF, self.line, self.col));
        tokens
    }
}
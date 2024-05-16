extern crate utils;

use utils::RazeResult;

enum TokenKind {
    // Single character
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Slash,
    Star,

    // One or two characters
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    Struct,
    Fn,
    SelfKw,
    Var,
    Return,
    If,
    Else,
    And,
    Or,
    Null,
    Print,
    For,
    While,
    True,
    False,
    EOF,
}

struct Loc {
    start: usize,
    end: usize
}

pub struct Token {
    kind: TokenKind,
    value: String,
    line: usize,
    loc: Loc
}

struct Lexer {
    code: Vec<char>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl Lexer {
    pub fn new(code: String) -> Self {
        Self {
            code: code.chars().collect(),
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1
        }
    }

    pub fn tokenize(&mut self) -> Result<&Vec<Token>, Vec<RazeResult>> {
        let mut errors: Vec<RazeResult> = vec![];

        while !self.eof() {
            let c = self.eat();

            match c {
                // Skipable char
                '\r' | '\t' => {},
                '\n' => self.line += 1,
                // Single char tokens
                '(' => self.add_token(TokenKind::OpenParen),
                ')' => self.add_token(TokenKind::CloseParen),
                '{' => self.add_token(TokenKind::OpenBrace),
                '}' => self.add_token(TokenKind::CloseBrace),
                ',' => self.add_token(TokenKind::Comma),
                '.' => self.add_token(TokenKind::Dot),
                '-' => self.add_token(TokenKind::Minus),
                '+' => self.add_token(TokenKind::Plus),
                '*' => self.add_token(TokenKind::Star),

                // One or two char tokens
                '!' => {
                    let tk = if self.is_at('=') {
                        TokenKind::BangEqual
                    } else {
                        TokenKind::Bang
                    };

                    self.add_token(tk);
                },
                '=' => {
                    let tk = if self.is_at('=') {
                        TokenKind::EqualEqual
                    } else {
                        TokenKind::Equal
                    };

                    self.add_token(tk);
                },
                '<' => {
                    let tk = if self.is_at('=') {
                        TokenKind::LessEqual
                    } else {
                        TokenKind::Less
                    };

                    self.add_token(tk);
                },
                '>' => {
                    let tk = if self.is_at('=') {
                        TokenKind::GreaterEqual
                    } else {
                        TokenKind::Greater
                    };

                    self.add_token(tk);
                },

                // Longer tokens
                '/' => {
                    if self.is_at('/') {
                        self.lex_comment()
                    } else {
                        self.add_token(TokenKind::Slash)
                    }
                },
                '\"' => match self.lex_string() {
                    Ok(_) => {},
                    Err(e) => errors.push(e)
                },

                _ => errors.push(RazeResult::lexer_error(
                        format!("Unexpected token found: {}", c),
                        self.line,
                        self.start,
                        self.current
                    ))
            }
        }

        match errors.is_empty() {
            true => Ok(&self.tokens),
            false => Err(errors)
        }
    }

    fn lex_comment(&mut self) {
        while !self.eof() && *self.at() != '\n' {
            self.eat();
        }
    }

    fn lex_string(&mut self) -> Result<(), RazeResult> {
        while !self.eof() && *self.at() != '\"' {
            if *self.at() == '\n' { self.line += 1 }

            self.eat();
        }

        if self.eof() {
            return Err(RazeResult::lexer_error(
                "String literal never closed with '\"'".into(),
                self.line,
                self.start,
                self.current
            ))
        }

        // We eat the "
        self.eat();

        // We create the token without the surronding quotes
        let value: String = self.code.get(self.start + 1..self.current - 1).unwrap().iter().collect();
        self.add_value_token(TokenKind::String, value);

        Ok(())
    }

    fn eof(&self) -> bool {
        self.current >= self.code.len()
    }

    // Unwrap is ok because only called when !eof()
    fn at(&self) -> &char {
        self.code.get(self.current).unwrap()
    }

    fn prev(&self) -> &char {
        self.code.get(self.current - 1).unwrap()
    }

    fn eat(&mut self) -> &char {
        self.current += 1;
        self.prev()
    }

    fn is_at(&mut self, expected: char) -> bool {
        if self.eof() { return false }
        if self.at() != &expected { return false }

        self.current += 1;
        return true
    }

    fn add_token(&mut self, kind: TokenKind) {
        self.tokens.push(Token {
            kind,
            value: self.code[self.start..self.current].iter().collect(),
            line: self.line,
            loc: Loc { start: self.start, end: self.current }
        });
    }

    fn add_value_token(&mut self, kind: TokenKind, value: String) {
        self.tokens.push(Token {
            kind,
            value,
            line: self.line,
            loc: Loc { start: self.start, end: self.current }
        });
    }
}
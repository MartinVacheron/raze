use std::collections::HashMap;

use super::results::ArcResult;

#[derive(Clone, Debug, PartialEq)]
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
    Const,
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

    NewLine,
    EOF,
}

#[derive(Debug, PartialEq)]
pub struct Loc {
    pub start: usize,
    pub end: usize
}

impl Loc {
    fn new(line: usize, start: usize, end: usize) -> Self {
        Self { start, end }
    }
     pub fn get_len(&self) -> usize {
        self.end - self.start
    }
}

#[derive(Debug, PartialEq)]
pub struct Token {
    kind: TokenKind,
    value: String,
    loc: Loc
}

pub struct Lexer {
    code: Vec<char>,
    tokens: Vec<Token>,
    keywords: HashMap<String, TokenKind>,
    start: usize,
    current: usize,
    line: usize,
}

impl Lexer {
    pub fn new(code: &String) -> Self {
        let mut lex = Lexer {
            code: code.chars().collect(),
            tokens: vec![],
            keywords: HashMap::new(),
            start: 0,
            current: 0,
            line: 1
        };

        lex.generate_keywords();
        lex
    }

    fn generate_keywords(&mut self) {
        let mut map: HashMap<String, TokenKind> = HashMap::new();

        map.insert("var".into(), TokenKind::Var);
        map.insert("const".into(), TokenKind::Const);
        map.insert("true".into(), TokenKind::True);
        map.insert("false".into(), TokenKind::False);
        map.insert("struct".into(), TokenKind::Struct);
        map.insert("self".into(), TokenKind::SelfKw);
        map.insert("fn".into(), TokenKind::Fn);
        map.insert("return".into(), TokenKind::Return);
        map.insert("if".into(), TokenKind::If);
        map.insert("else".into(), TokenKind::Else);
        map.insert("and".into(), TokenKind::And);
        map.insert("or".into(), TokenKind::Or);
        map.insert("for".into(), TokenKind::For);
        map.insert("while".into(), TokenKind::While);
        map.insert("print".into(), TokenKind::Print);

        self.keywords = map;
    }

    pub fn tokenize(&mut self) -> Result<&Vec<Token>, Vec<ArcResult>> {
        let mut errors: Vec<ArcResult> = vec![];
        
        while !self.eof() {
            self.start = self.current;

            let c = self.eat();

            match c {
                // Skipable char
                '\r' | '\t' | ' ' => {},
                '\n' => self.lex_new_line(),
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

                _ => {
                    if c.is_numeric() {
                        match self.lex_number() {
                            Ok(_) => {},
                            Err(e) => errors.push(e)
                        }
                    } else if c.is_alphabetic() {
                        match self.lex_identifier() {
                            Ok(_) => {},
                            Err(e) => errors.push(e)
                        }
                    } else {
                        errors.push(ArcResult::lexer_error(
                            format!("Unexpected token found: {}", c),
                            self.get_loc() 
                        ))
                    }
                }
            }
        }

        match errors.is_empty() {
            true => Ok(&self.tokens),
            false => Err(errors)
        }
    }

    fn lex_new_line(&mut self) {
        self.line += 1;
        self.add_token(TokenKind::NewLine);
    }

    fn lex_comment(&mut self) {
        while !self.eof() && self.at() != '\n' {
            self.eat();
        }
    }

    fn lex_string(&mut self) -> Result<(), ArcResult> {
        while !self.eof() && self.at() != '\"' {
            if self.at() == '\n' {
                self.eat();
                self.lex_new_line();
            } else {
                self.eat();
            }
        }

        if self.eof() {
            return self.trigger_error("String literal never closed with '\"'".into())
        }

        // We create the token without the surronding quotes
        let value: String = self.code.get(self.start + 1..self.current).unwrap().iter().collect();
        self.add_value_token(TokenKind::String, value);

        // We eat the "
        self.eat();

        Ok(())
    }

    fn lex_number(&mut self) -> Result<(), ArcResult> {
        while self.at().is_numeric() {
            self.eat();
        }
        
        if self.at() == '.' {
            self.eat();

            if self.eof() || self.is_skippable() || self.at() == '\n' {
                
            } else if !self.at().is_numeric() {
                return self.trigger_error(
                    format!(
                        "Expected numbers or nothing after '.' in number literal, found: '{}'",
                        self.at()
                    )
                )
            } else {
                while self.at().is_numeric() {
                    self.eat();
                }

                // After all the numbers, we expect a white space
                if !self.eof() && !self.is_skippable() && self.at() != '\n' {
                    return self.trigger_error(
                        format!(
                            "Expected nothing after number literal, found: '{}'",
                            self.at()
                        )
                    )
                }
            }
        }

        self.add_token(TokenKind::Number);
        Ok(())
    }

    fn lex_identifier(&mut self) -> Result<(), ArcResult> {
        while self.at().is_alphanumeric() || self.at() == '_' {
            self.eat();
        }

        let ident: String = self.code.get(self.start..self.current).unwrap().iter().collect();
        
        match self.keywords.get(&ident) {
            Some(tk) => self.add_token(tk.clone()),
            None => self.add_value_token(TokenKind::Identifier, ident)
        }

        Ok(())
    }

    fn eof(&self) -> bool {
        self.current >= self.code.len()
    }

    // Unwrap is ok because only called when !eof()
    fn at(&self) -> char {
        if !self.eof() {
            *self.code.get(self.current).unwrap()
        } else {
            '\0'
        }
    }

    fn prev(&self) -> char {
        *self.code.get(self.current - 1).unwrap()
    }

    fn is_skippable(&self) -> bool {
        match self.at() {
            ' ' | '\t' | '\r' => true,
            _ => false
        }
    }

    fn eat(&mut self) -> char {
        self.current += 1;
        self.prev()
    }

    fn is_at(&mut self, expected: char) -> bool {
        if self.eof() { return false }
        if self.at() != expected { return false }

        self.current += 1;
        return true
    }

    fn trigger_error(&mut self, msg: String) -> Result<(), ArcResult> {
        self.synchronize();
        let err = ArcResult::lexer_error(msg, self.get_loc());

        Err(err)
    }

    // Function used when an error is encountered. We skip until next
    // part to lex aka white space, to collect potentially more errors
    fn synchronize(&mut self) {
        // Only for error reporting purpose, we move the cursor at the
        // beggining of the token that caused the error
        self.start += 1;
        // We rewind
        self.current = self.start;
        // Until white space, we skip
        while (self.at() != ' ' && self.at() != '\n') && !self.eof() {
            self.current += 1;
        }
    }

    fn add_token(&mut self, kind: TokenKind) {
        self.tokens.push(Token {
            kind,
            value: self.code[self.start..self.current].iter().collect(),
            loc: self.get_loc()
        });
    }

    // Add a toekn with a specific value
    fn add_value_token(&mut self, kind: TokenKind, value: String) {
        self.tokens.push(Token {
            kind,
            value,
            loc: self.get_loc()
        });
    }

    fn get_loc(&self) -> Loc {
        Loc::new(self.line, self.start, self.current)
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{Token, TokenKind, Loc};

    use super::Lexer;

    #[test]
    fn tokenize_single_char() {
        let code: String = "(){},.-+/*=!<>".into();
        let mut lexer = Lexer::new(&code); 
        let tokens = lexer.tokenize().unwrap();

        let tk_kind: Vec<TokenKind> = tokens.iter().map(|tk| tk.kind.clone()).collect();

        assert_eq!(
            tk_kind,
            vec![
                TokenKind::OpenParen,
                TokenKind::CloseParen,
                TokenKind::OpenBrace,
                TokenKind::CloseBrace,
                TokenKind::Comma,
                TokenKind::Dot,
                TokenKind::Minus,
                TokenKind::Plus,
                TokenKind::Slash,
                TokenKind::Star,
                TokenKind::Equal,
                TokenKind::Bang,
                TokenKind::Less,
                TokenKind::Greater,
            ]
        );
    }

    #[test]
    fn tokenize_double_char() {
        let code: String = "!= <= >= ==".into();
        let mut lexer = Lexer::new(&code); 
        let tokens = lexer.tokenize().unwrap();

        let tk_kind: Vec<TokenKind> = tokens.iter().map(|tk| tk.kind.clone()).collect();

        assert_eq!(
            tk_kind,
            vec![
                TokenKind::BangEqual,
                TokenKind::LessEqual,
                TokenKind::GreaterEqual,
                TokenKind::EqualEqual,
            ]
        );
    }

    #[test]
    fn tokenize_string() {
        let code: String = "\"hello world!\"".into();
        let mut lexer = Lexer::new(&code); 
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            &vec![
                Token {
                    kind: TokenKind::String,
                    value: "hello world!".into(),
                    loc: Loc { start: 0, end: 13 }
                }
            ]
        );
    }

    #[test]
    fn tokenize_number() {
        let code: String = "12 25. 26.345".into();
        let mut lexer = Lexer::new(&code); 
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            &vec![
                Token {
                    kind: TokenKind::Number,
                    value: "12".into(),
                    loc: Loc { line: 1, start: 0, end: 2 }
                },
                Token {
                    kind: TokenKind::Number,
                    value: "25.".into(),
                    loc: Loc { line: 1, start: 3, end: 6 }
                },
                Token {
                    kind: TokenKind::Number,
                    value: "26.345".into(),
                    loc: Loc { line: 1, start: 7, end: 13 }
                }
            ]
        );
    }
}

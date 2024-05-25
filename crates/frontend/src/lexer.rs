use std::{collections::HashMap, fmt::Display};
use ecow::EcoString;

use super::results::ArcResult;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
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
    Int,
    Real,

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
    Eof,
}

#[derive(Debug, PartialEq, Default, Clone)]
pub struct Loc {
    pub start: usize,
    pub end: usize
}

impl Loc {
    fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
     pub fn get_len(&self) -> usize {
        self.end - self.start
    }
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: EcoString,
    pub loc: Loc
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
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
    pub fn new(code: &str) -> Self {
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
        map.insert("null".into(), TokenKind::Null);
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
        
        // We do it like this because if last token was an error, we synchronized
        // att eof already so we are at out of bounds. We manually add a slot
        // past end of file to represent the token location
        self.tokens.push(
            Token {
                kind: TokenKind::Eof,
                value: "eof".into(),
                loc: Loc { start: self.code.len(), end: self.code.len() + 1 }
            }
        );

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
        let value: String = self.code.get(self.start + 2..self.current).unwrap().iter().collect();
        // We eat the "
        self.eat();

        self.add_value_token(TokenKind::String, value.into());
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
            self.add_token(TokenKind::Real);

        } else {
            self.add_token(TokenKind::Int);
        }

        Ok(())
    }

    fn lex_identifier(&mut self) -> Result<(), ArcResult> {
        while self.at().is_alphanumeric() || self.at() == '_' {
            self.eat();
        }

        let ident: String = self.code.get(self.start..self.current).unwrap().iter().collect();
        
        match self.keywords.get(&ident) {
            Some(tk) => self.add_token(tk.clone()),
            None => self.add_value_token(TokenKind::Identifier, ident.into())
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
        matches!(self.at(), ' ' | '\t' | '\r')
    }

    fn eat(&mut self) -> char {
        self.current += 1;
        self.prev()
    }

    fn is_at(&mut self, expected: char) -> bool {
        if self.eof() { return false }
        if self.at() != expected { return false }

        self.current += 1;
        true
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
        while !self.is_skippable() && self.at() != '\n' && !self.eof() {
            self.current += 1;
        }
        // We add the last one
        self.current += 1;
    }

    fn add_token(&mut self, kind: TokenKind) {
        let code: String = self.code[self.start..self.current].iter().collect();

        self.tokens.push(Token {
            kind,
            value: code.into(),
            loc: self.get_loc()
        });
    }

    // Add a token with a specific value
    fn add_value_token(&mut self, kind: TokenKind, value: EcoString) {
        self.tokens.push(Token {
            kind,
            value,
            loc: self.get_loc()
        });
    }

    fn get_loc(&self) -> Loc {
        Loc::new(self.start, self.current)
    }
}

#[cfg(test)]
mod tests {
    use ecow::EcoString;

    use crate::lexer::{ TokenKind, Loc };

    use super::Lexer;

    #[test]
    fn tokenize_single_char() {
        let code: String = "(){},.-+/*=!<>\n".into();
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
                TokenKind::NewLine,
                TokenKind::Eof,
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
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn tokenize_string() {
        let code: String = "\"hello world!\"".into();
        let mut lexer = Lexer::new(&code); 
        let tokens = lexer.tokenize().unwrap();

        let tk_kind: Vec<TokenKind> = tokens.iter().map(|tk| tk.kind.clone()).collect();

        assert_eq!(tk_kind, vec![TokenKind::String, TokenKind::Eof]);
    }

    #[test]
    fn tokenize_number() {
        let code: String = "12 25. 26.345".into();
        let mut lexer = Lexer::new(&code); 
        let tokens = lexer.tokenize().unwrap();

        let tk_type: Vec<TokenKind> = tokens.iter().map(|tk| tk.kind.clone()).collect();
        let tk_value: Vec<EcoString> = tokens.iter().map(|tk| tk.value.clone()).collect();

        assert_eq!(
            tk_type,
            vec![TokenKind::Int, TokenKind::Real, TokenKind::Real, TokenKind::Eof]
        );

        assert_eq!(
            tk_value,
            vec!["12".to_string(), "25.".to_string(), "26.345".to_string(), "eof".to_string()]
        );
    }

    #[test]
    fn number_errors() {
        let code: String = "12.5.".into();
        let mut lexer = Lexer::new(&code); 
        let tokens = lexer.tokenize();

        assert!(tokens.is_err());

        let code: String = "12.534.45".into();
        let mut lexer = Lexer::new(&code); 
        let tokens = lexer.tokenize();

        assert!(tokens.is_err());

        let code: String = "12.a".into();
        let mut lexer = Lexer::new(&code); 
        let tokens = lexer.tokenize();

        assert!(tokens.is_err());
    }

    #[test]
    fn string_errors() {
        let code: String = "\"foo".into();
        let mut lexer = Lexer::new(&code); 
        let tokens = lexer.tokenize();

        assert!(tokens.is_err());
    }

    #[test]
    fn location() {
        let code: String = "
12345.43
\"foo bar\"
for while

break 45+7".into();
        let mut lexer = Lexer::new(&code); 
        let tokens = lexer.tokenize().unwrap();

        let tk_loc: Vec<&Loc> = tokens.iter().map(|tk| &tk.loc).collect();

        assert_eq!(
            tk_loc,
            vec![
                &Loc::new(0, 1),
                &Loc::new(1, 9),
                &Loc::new(9, 10),
                &Loc::new(10, 19),
                &Loc::new(19, 20),
                &Loc::new(20, 23),
                &Loc::new(24, 29),
                &Loc::new(29, 30),
                &Loc::new(30, 31),
                &Loc::new(31, 36),
                &Loc::new(37, 39),
                &Loc::new(39, 40),
                &Loc::new(40, 41),
                &Loc::new(41, 42),
            ]
        );
    }
}

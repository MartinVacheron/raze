use std::{collections::HashMap, fmt::Display};
use ecow::EcoString;
use thiserror::Error;
use colored::*;

use tools::results::{RevReport, RevResult, Loc};


// ----------------
// Error managment
// ----------------
#[derive(Error, Debug)]
pub enum LexerErr {
    //Tokens
    #[error("unexpected token found: '{0}'")]
    UnexpectedToken(char),

    // Numbers
    #[error("decimal part is not a number")]
    NonNumberDecimal,

    #[error("numbers can't have two decimal parts")]
    TwoDecimalParts,

    // Strings
    #[error("string literal never closed with '\"'")]
    StringNeverClosed,
}

impl RevReport for LexerErr {
    fn get_err_msg(&self) -> String {
        format!("{} {}", "Lexer error:".red(), self)
    }
}

type RevResLex = RevResult<LexerErr>;


// --------
//  Lexing
// --------
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
    Modulo,

    // One or two characters
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    DotDot,

    // Literals
    Identifier,
    String,
    Int,
    Float,

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
    In,
    True,
    False,

    NewLine,
    Eof,
}


#[derive(Debug, PartialEq, Clone)]
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

#[derive(Default)]
pub struct Lexer {
    code: Vec<char>,
    tokens: Vec<Token>,
    keywords: HashMap<String, TokenKind>,
    start: usize,
    current: usize,
}

impl Lexer {
    pub fn new() -> Self {
        let mut lex = Lexer::default();

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
        map.insert("in".into(), TokenKind::In);
        map.insert("null".into(), TokenKind::Null);
        map.insert("print".into(), TokenKind::Print);

        self.keywords = map;
    }

    pub fn tokenize(&mut self, code: &str) -> Result<&Vec<Token>, Vec<RevResLex>> {
        self.code = code.chars().collect();

        let mut errors: Vec<RevResLex> = vec![];
        
        while !self.eof() {
            self.start = self.current;

            let c = self.eat();

            match c {
                // Skipable char
                '\r' | '\t' | ' ' => {},
                '\n' => self.add_token(TokenKind::NewLine),
                // Single char tokens
                '(' => self.add_token(TokenKind::OpenParen),
                ')' => self.add_token(TokenKind::CloseParen),
                '{' => self.add_token(TokenKind::OpenBrace),
                '}' => self.add_token(TokenKind::CloseBrace),
                ',' => self.add_token(TokenKind::Comma),
                '.' => {
                    if self.at().is_numeric() {
                        match self.lex_number(true) {
                            Ok(_) => {},
                            Err(e) => errors.push(e)
                        }
                    }
                    else if self.is_at('.') {
                        self.add_token(TokenKind::DotDot);
                    } else {
                        self.add_token(TokenKind::Dot);
                    }
                },
                '-' => self.add_token(TokenKind::Minus),
                '+' => self.add_token(TokenKind::Plus),
                '*' => self.add_token(TokenKind::Star),
                '%' => self.add_token(TokenKind::Modulo),

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
                        match self.lex_number(false) {
                            Ok(_) => {},
                            Err(e) => errors.push(e)
                        }
                    } else if c.is_alphabetic() {
                        match self.lex_identifier() {
                            Ok(_) => {},
                            Err(e) => errors.push(e)
                        }
                    } else {
                        errors.push(self.trigger_error(LexerErr::UnexpectedToken(c)))
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

    fn lex_comment(&mut self) {
        while !self.eof() && self.at() != '\n' {
            self.eat();
        }
    }

    fn lex_string(&mut self) -> Result<(), RevResLex> {
        while !self.eof() && self.at() != '\"' {
            self.eat();
        }

        if self.eof() {
            return Err(self.trigger_error(LexerErr::StringNeverClosed))
        }

        // We create the token without the surronding quotes
        let value: String = self.code.get(self.start + 1..self.current).unwrap().iter().collect();
        // We eat the "
        self.eat();

        self.add_value_token(TokenKind::String, value.into());
        Ok(())
    }

    // point_float is when we are in the case ".456" and we have already parsed
    // the '.' 
    fn lex_number(&mut self, point_float: bool) -> Result<(), RevResLex> {
        while self.at().is_numeric() {
            self.eat();
        }

        if point_float {
            if self.at() == '.' {
                return Err(self.trigger_error(LexerErr::TwoDecimalParts))
            }

            if !self.is_skippable()
                && self.at() != '\n'
                && !self.is_math_op()
                && !self.eof()
            {
                return Err(self.trigger_error(LexerErr::NonNumberDecimal))
            }

            self.add_token(TokenKind::Float);

            return Ok(())
        }
        
        if self.at() == '.' {
            // Range
            if self.next() == '.' {
                return self.lex_range()
            }

            self.eat();

            if !self.at().is_numeric()
                && !self.is_skippable()
                && self.at() != '\n'
                && !self.is_math_op()
                && !self.eof()
            {
                return Err(self.trigger_error(LexerErr::NonNumberDecimal))
            }

            while self.at().is_numeric() {
                self.eat();
            }

            if self.at() == '.' {
                return Err(self.trigger_error(LexerErr::TwoDecimalParts))
            }
            
            self.add_token(TokenKind::Float);

        } else {
            self.add_token(TokenKind::Int);
        }

        Ok(())
    }

    fn lex_range(&mut self) -> Result<(), RevResLex> {
        self.add_token(TokenKind::Int);

        self.start = self.current;
        self.eat();
        self.eat();
        self.add_token(TokenKind::DotDot);

        self.start = self.current;
        while self.at().is_numeric() {
            self.eat();
        }

        if self.start != self.current {
            self.add_token(TokenKind::Int);
        }

        Ok(())
    }

    fn lex_identifier(&mut self) -> Result<(), RevResLex> {
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

    fn next(&self) -> char {
        if self.current < self.code.len() - 1 {
            *self.code.get(self.current + 1).unwrap()
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

    fn is_math_op(&self) -> bool {
        match self.at() {
            '+' | '-' | '*' | '/' => true,
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
        true
    }

    fn trigger_error(&mut self, err: LexerErr) -> RevResLex {
        self.synchronize();
        RevResult::new(err, Some(self.get_loc()))
    }

    // Function used when an error is encountered. We skip until next
    // part to lex aka white space, to collect potentially more errors
    fn synchronize(&mut self) {
        // We rewind
        self.current = self.start;
        // Until white space, we skip
        while !self.is_skippable() && self.at() != '\n' && !self.eof() {
            self.current += 1;
        }
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
        println!("Loc creation: start: {}, end: {}", self.start, self.current - 1);
        Loc::new(self.start, self.current - 1)
    }
}

#[cfg(test)]
mod tests {
    use ecow::EcoString;

    use crate::lexer::{ LexerErr, Loc, TokenKind };

    use super::Lexer;

    #[test]
    fn tokenize_single_char() {
        let code: String = "(){},.-+%/*=!<>\n".into();
        let mut lexer = Lexer::new(); 
        let tokens = lexer.tokenize(&code).unwrap();

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
                TokenKind::Modulo,
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
        let code: String = "!= <= >= == ..".into();
        let mut lexer = Lexer::new(); 
        let tokens = lexer.tokenize(&code).unwrap();

        let tk_kind: Vec<TokenKind> = tokens.iter().map(|tk| tk.kind.clone()).collect();

        assert_eq!(
            tk_kind,
            vec![
                TokenKind::BangEqual,
                TokenKind::LessEqual,
                TokenKind::GreaterEqual,
                TokenKind::EqualEqual,
                TokenKind::DotDot,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn tokenize_string() {
        let code: String = "\"hello world!\"".into();
        let mut lexer = Lexer::new(); 
        let tokens = lexer.tokenize(&code).unwrap();

        let tk_kind: Vec<TokenKind> = tokens.iter().map(|tk| tk.kind.clone()).collect();

        assert_eq!(tk_kind, vec![TokenKind::String, TokenKind::Eof]);
    }

    #[test]
    fn tokenize_number() {
        let code: String = "12 25. 26.345".into();
        let mut lexer = Lexer::new(); 
        let tokens = lexer.tokenize(&code).unwrap();

        let tk_type: Vec<TokenKind> = tokens.iter().map(|tk| tk.kind.clone()).collect();
        let tk_value: Vec<EcoString> = tokens.iter().map(|tk| tk.value.clone()).collect();

        assert_eq!(
            tk_type,
            vec![TokenKind::Int, TokenKind::Float, TokenKind::Float, TokenKind::Eof]
        );

        assert_eq!(
            tk_value,
            vec!["12".to_string(), "25.".to_string(), "26.345".to_string(), "eof".to_string()]
        );
    }

    #[test]
    fn tokenize_range() {
        let code: String = "2..5".into();
        let mut lexer = Lexer::new(); 
        let tokens = lexer.tokenize(&code).unwrap();

        let tk_type: Vec<TokenKind> = tokens.iter().map(|tk| tk.kind.clone()).collect();

        assert_eq!(
            tk_type,
            vec![TokenKind::Int, TokenKind::DotDot, TokenKind::Int, TokenKind::Eof]
        );
    }

    #[test]
    fn string_errors() {
        let code: String = "\"foo".into();
        let mut lexer = Lexer::new(); 
        let tokens = lexer.tokenize(&code);

        assert!(matches!(
            tokens.err().unwrap()[0].err,
            LexerErr::StringNeverClosed
        ));
    }

    #[test]
    fn location() {
        let code: String = "
12345.43
\"foo bar\"
for while

break 45+7".into();
        let mut lexer = Lexer::new(); 
        let tokens = lexer.tokenize(&code).unwrap();

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

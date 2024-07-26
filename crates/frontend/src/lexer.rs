use colored::*;
use ecow::EcoString;
use std::{collections::HashMap, fmt::Display};
use thiserror::Error;

use tools::results::{Loc, RevReport, RevResult};

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
    Colon,
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
    StringLit,
    IntLit,
    FloatLit,

    // Types
    IntType,
    FloatType,
    StringType,
    BoolType,

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
    Is,

    // Temporary
    Range(Box<(Token, Token, Option<Token>)>),

    NewLine,
    Eof,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub value: EcoString,
    pub loc: Loc,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Default)]
pub struct Lexer {
    code: Vec<char>,
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
        map.insert("int".into(), TokenKind::IntType);
        map.insert("float".into(), TokenKind::FloatType);
        map.insert("str".into(), TokenKind::StringType);
        map.insert("bool".into(), TokenKind::BoolType);
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
        map.insert("is".into(), TokenKind::Is);

        self.keywords = map;
    }

    pub fn tokenize(&mut self, code: &str) -> Result<Vec<Token>, Vec<RevResLex>> {
        self.code = code.chars().collect();

        let mut errors: Vec<RevResLex> = vec![];
        let mut tokens: Vec<Token> = vec![];

        while !self.eof() {
            self.start = self.current;

            let c = self.eat();

            // Skipable char
            if matches!(c, '\r' | '\t' | ' ') {
                continue;
            }

            if c == '/' && self.at() == '/' {
                self.lex_comment();
                continue;
            }

            let res = match c {
                '\n' => self.add_token(TokenKind::NewLine),
                // Single char tokens
                '(' => self.add_token(TokenKind::OpenParen),
                ')' => self.add_token(TokenKind::CloseParen),
                '{' => self.add_token(TokenKind::OpenBrace),
                '}' => self.add_token(TokenKind::CloseBrace),
                ',' => self.add_token(TokenKind::Comma),
                '.' => {
                    if self.at().is_numeric() {
                        self.lex_number(true)
                    } else if self.is_at_and_advance('.') {
                        self.add_token(TokenKind::DotDot)
                    } else {
                        self.add_token(TokenKind::Dot)
                    }
                }
                ':' => self.add_token(TokenKind::Colon),
                '-' => self.add_token(TokenKind::Minus),
                '+' => self.add_token(TokenKind::Plus),
                '*' => self.add_token(TokenKind::Star),
                '%' => self.add_token(TokenKind::Modulo),

                // One or two char tokens
                '!' => {
                    let tk = if self.is_at_and_advance('=') {
                        TokenKind::BangEqual
                    } else {
                        TokenKind::Bang
                    };

                    self.add_token(tk)
                }
                '=' => {
                    let tk = if self.is_at_and_advance('=') {
                        TokenKind::EqualEqual
                    } else {
                        TokenKind::Equal
                    };

                    self.add_token(tk)
                }
                '<' => {
                    let tk = if self.is_at_and_advance('=') {
                        TokenKind::LessEqual
                    } else {
                        TokenKind::Less
                    };

                    self.add_token(tk)
                }
                '>' => {
                    let tk = if self.is_at_and_advance('=') {
                        TokenKind::GreaterEqual
                    } else {
                        TokenKind::Greater
                    };

                    self.add_token(tk)
                }
                // Longer tokens
                '/' => self.add_token(TokenKind::Slash),
                '\"' => self.lex_string(),

                _ => {
                    if c.is_numeric() {
                        self.lex_number(false)
                    } else if c.is_alphabetic() {
                        self.lex_identifier()
                    } else {
                        Err(self.trigger_error(LexerErr::UnexpectedToken(c)))
                    }
                }
            };

            match res {
                Ok(tk) => {
                    if let TokenKind::Range(range) = tk.kind {
                        tokens.push(range.0);
                        tokens.push(range.1);

                        if let Some(t) = range.2 {
                            tokens.push(t);
                        }
                    } else {
                        tokens.push(tk);
                    }
                }
                Err(e) => errors.push(e),
            }
        }

        // We do it like this because if last token was an error, we synchronized
        // att eof already so we are at out of bounds. We manually add a slot
        // past end of file to represent the token location
        tokens.push(Token {
            kind: TokenKind::Eof,
            value: "eof".into(),
            loc: Loc {
                start: self.code.len(),
                end: self.code.len() + 1,
            },
        });

        match errors.is_empty() {
            true => Ok(tokens),
            false => Err(errors),
        }
    }

    fn lex_comment(&mut self) {
        while !self.eof() && self.at() != '\n' {
            self.eat();
        }
    }

    fn lex_string(&mut self) -> Result<Token, RevResLex> {
        let open_quote = self.current - 1;

        while !self.eof() && self.at() != '\"' {
            self.eat();
        }

        if self.eof() {
            return Err(self.trigger_error_with_loc(
                LexerErr::StringNeverClosed,
                Loc::new(open_quote, open_quote),
            ));
        }

        // We create the token without the surronding quotes
        let value: String = self
            .code
            .get(self.start + 1..self.current)
            .unwrap()
            .iter()
            .collect();
        // We eat the "
        self.eat();

        self.add_value_token(TokenKind::StringLit, value.into())
    }

    // point_float is when we are in the case ".456" and we have already parsed
    // the '.'
    fn lex_number(&mut self, point_float: bool) -> Result<Token, RevResLex> {
        while self.at().is_numeric() {
            self.eat();
        }

        if point_float {
            if self.at() == '.' {
                return Err(self.trigger_error(LexerErr::TwoDecimalParts));
            }

            if !self.is_skippable() && self.at() != '\n' && !self.is_math_op() && !self.eof() {
                return Err(self.trigger_error(LexerErr::NonNumberDecimal));
            }

            return self.add_token(TokenKind::FloatLit);
        }

        if self.at() == '.' {
            // Range
            if self.next() == '.' {
                return self.lex_range();
            }

            self.eat();

            if !self.at().is_numeric()
                && !self.is_skippable()
                && self.at() != '\n'
                && !self.is_math_op()
                && !self.eof()
            {
                return Err(self.trigger_error(LexerErr::NonNumberDecimal));
            }

            while self.at().is_numeric() {
                self.eat();
            }

            if self.at() == '.' {
                return Err(self.trigger_error(LexerErr::TwoDecimalParts));
            }

            self.add_token(TokenKind::FloatLit)
        } else {
            self.add_token(TokenKind::IntLit)
        }
    }

    fn lex_range(&mut self) -> Result<Token, RevResLex> {
        let start = self.add_token(TokenKind::IntLit)?;

        self.start = self.current;
        self.eat();
        self.eat();
        let dotdot = self.add_token(TokenKind::DotDot)?;

        self.start = self.current;
        while self.at().is_numeric() {
            self.eat();
        }

        let mut end: Option<Token> = None;
        if self.start != self.current {
            end = Some(self.add_token(TokenKind::IntLit)?);
        }

        Ok(Token {
            kind: TokenKind::Range(Box::new((start, dotdot, end))),
            value: "".into(),
            loc: Loc { start: 0, end: 0 },
        })
    }

    fn lex_identifier(&mut self) -> Result<Token, RevResLex> {
        while self.at().is_alphanumeric() || self.at() == '_' {
            self.eat();
        }

        let ident: String = self
            .code
            .get(self.start..self.current)
            .unwrap()
            .iter()
            .collect();

        match self.keywords.get(&ident) {
            Some(tk) => self.add_token(tk.clone()),
            None => self.add_value_token(TokenKind::Identifier, ident.into()),
        }
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
        matches!(self.at(), '+' | '-' | '*' | '/')
    }

    fn eat(&mut self) -> char {
        self.current += 1;
        self.prev()
    }

    fn is_at_and_advance(&mut self, expected: char) -> bool {
        if self.eof() {
            return false;
        }
        if self.at() != expected {
            return false;
        }

        self.current += 1;
        true
    }

    fn trigger_error(&mut self, err: LexerErr) -> RevResLex {
        let loc = self.get_loc();
        self.trigger_error_with_loc(err, loc)
    }

    fn trigger_error_with_loc(&mut self, err: LexerErr, loc: Loc) -> RevResLex {
        self.synchronize();

        RevResult::new(err, Some(loc))
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

    fn add_token(&mut self, kind: TokenKind) -> Result<Token, RevResLex> {
        let code: String = self.code[self.start..self.current].iter().collect();

        Ok(Token {
            kind,
            value: code.into(),
            loc: self.get_loc(),
        })
    }

    // Add a token with a specific value
    fn add_value_token(&mut self, kind: TokenKind, value: EcoString) -> Result<Token, RevResLex> {
        Ok(Token {
            kind,
            value,
            loc: self.get_loc(),
        })
    }

    fn get_loc(&self) -> Loc {
        Loc::new(self.start, self.current - 1)
    }
}

#[cfg(test)]
mod tests {
    use ecow::EcoString;

    use crate::lexer::{LexerErr, Loc, TokenKind};

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

        assert_eq!(tk_kind, vec![TokenKind::StringLit, TokenKind::Eof]);
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
            vec![
                TokenKind::IntLit,
                TokenKind::FloatLit,
                TokenKind::FloatLit,
                TokenKind::Eof
            ]
        );

        assert_eq!(
            tk_value,
            vec![
                "12".to_string(),
                "25.".to_string(),
                "26.345".to_string(),
                "eof".to_string()
            ]
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
            vec![
                TokenKind::IntLit,
                TokenKind::DotDot,
                TokenKind::IntLit,
                TokenKind::Eof
            ]
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

break 45+7"
            .into();
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(&code).unwrap();

        let tk_loc: Vec<&Loc> = tokens.iter().map(|tk| &tk.loc).collect();

        assert_eq!(
            tk_loc,
            vec![
                &Loc::new(0, 0),
                &Loc::new(1, 8),
                &Loc::new(9, 9),
                &Loc::new(10, 18),
                &Loc::new(19, 19),
                &Loc::new(20, 22),
                &Loc::new(24, 28),
                &Loc::new(29, 29),
                &Loc::new(30, 30),
                &Loc::new(31, 35),
                &Loc::new(37, 38),
                &Loc::new(39, 39),
                &Loc::new(40, 40),
                &Loc::new(41, 42),
            ]
        );
    }
}

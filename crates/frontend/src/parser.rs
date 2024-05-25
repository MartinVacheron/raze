use crate::expr::{BinaryExpr, Expr, GroupingExpr, IdentifierExpr, IntLiteralExpr, RealLiteralExpr, UnaryExpr};
use crate::results::ArcResult;
use crate::lexer::{Token, TokenKind};

pub struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
    nodes: Vec<Expr>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, current: 0, nodes: vec![] }
    }

    pub fn parse(&mut self) -> Result<&Vec<Expr>, Vec<ArcResult>> {
        let mut errors: Vec<ArcResult> = vec![];

        while !self.eof() {
            // If we have a new line to begin a statement/expr parsing,
            // we skip it. There are important only in parsing steps
            if self.at().kind == TokenKind::NewLine {
                self.current += 1;
                continue
            }

            match self.parse_expr() {
                Ok(expr) => self.nodes.push(expr),
                Err(e) => {
                    self.synchronize();

                    errors.push(e)
                }
            }
        }
        
        if !errors.is_empty() {
            return Err(errors)
        }

        Ok(&self.nodes)
    }

    fn parse_expr(&mut self) -> Result<Expr, ArcResult> {
        self.parse_equality()
    }

    fn parse_equality(&mut self) -> Result<Expr, ArcResult> {
        let mut expr = self.parse_comparison()?;

        while self.is_at(TokenKind::EqualEqual) || self.is_at(TokenKind::BangEqual) {
            let operator = self.eat()?.value.clone();
            let right = self.parse_comparison()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right)
            });
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, ArcResult> {
        let mut expr = self.parse_term()?;

        while self.is_at(TokenKind::Less)
                || self.is_at(TokenKind::LessEqual)
                || self.is_at(TokenKind::Greater)
                || self.is_at(TokenKind::GreaterEqual)
        {
            let operator = self.eat()?.value.clone();
            let right = self.parse_term()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right)
            });
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expr, ArcResult> {
        let mut expr = self.parse_factor()?;

        while self.is_at(TokenKind::Minus) || self.is_at(TokenKind::Plus) {
            let operator = self.eat()?.value.clone();
            let right = self.parse_factor()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right)
            });
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expr, ArcResult> {
        let mut expr = self.parse_unary()?;

        while self.is_at(TokenKind::Star) || self.is_at(TokenKind::Slash) {
            let operator = self.eat()?.value.clone();
            let right = self.parse_unary()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right)
            });
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, ArcResult> {
        if self.is_at(TokenKind::Bang) || self.is_at(TokenKind::Minus) {
            let operator = self.eat()?.value.clone();
            let right = self.parse_primary()?;

            return Ok(Expr::Unary(UnaryExpr {
                operator,
                right: Box::new(right)
            }))
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr, ArcResult> {
        match self.eat()?.kind {
            TokenKind::Identifier => Ok(Expr::Identifier(IdentifierExpr { name: self.prev().value.clone() })),
            TokenKind::Int => self.parse_int_literal(),
            TokenKind::Real => self.parse_real_literal(),
            TokenKind::OpenParen => self.parse_grouping(),
            TokenKind::True => Ok(Expr::Identifier(IdentifierExpr { name: self.at().value.clone() })),
            TokenKind::False => Ok(Expr::Identifier(IdentifierExpr { name: self.at().value.clone() })),
            TokenKind::Null => Ok(Expr::Identifier(IdentifierExpr { name: self.at().value.clone() })),
            _ => Err(ArcResult::parser_error(format!("Unknown token to parse: '{}'", self.prev()), self.prev().loc.clone()))
        }
    }

    fn parse_int_literal(&self) -> Result<Expr, ArcResult> {
        let tk = self.prev();
        let value = tk.value.parse::<i64>().map_err(|_| ArcResult::internal_error("Error parsing int from string".into()))?;

        Ok(Expr::IntLiteral(IntLiteralExpr { value }))
    }

    fn parse_real_literal(&self) -> Result<Expr, ArcResult> {
        let tk = self.prev();
        let value = tk.value.parse::<f64>().map_err(|_| ArcResult::internal_error("Error parsing real from string".into()))?;

        Ok(Expr::RealLiteral(RealLiteralExpr { value }))
    }

    fn parse_grouping(&mut self) -> Result<Expr, ArcResult> {
        let expr = self.parse_expr()?;
        self.expect(TokenKind::CloseParen)?;

        Ok(Expr::Grouping(GroupingExpr { expr: Box::new(expr) }))
    }

    fn at(&self) -> &Token {
        self.tokens.get(self.current).unwrap()
    }

    fn eat(&mut self) -> Result<&Token, ArcResult> {
        if self.eof() {
            return Err(ArcResult::internal_error("Token access out of bound".into()))
        }

        self.current += 1;
        Ok(self.prev())
    }
    
    fn expect(&mut self, kind: TokenKind) -> Result<(), ArcResult> {
        let tk = self.eat()?;

        match tk.kind == kind {
            true => Ok(()),
            false => Err(ArcResult::parser_error(
                format!("Expected token type '{:?}', found: {:?}", kind, tk.kind),
                tk.loc.clone()
            ))
        }
    }

    fn is_at(&self, kind: TokenKind) -> bool {
        self.at().kind == kind
    }

    fn prev(&self) -> &Token {
        self.tokens.get(self.current - 1).unwrap()
    }
    
    fn eof(&self) -> bool {
        self.is_at(TokenKind::Eof)
    }

    // TODO: For now, we are only looking for new line token as we
    // don't have ';' to clearly know where the current statement stops.
    // It would be great to have an argument to this function that let
    // us know where we were when we got the error to know which corresponding
    // token to look for.

    // We are here in panic mode
    fn synchronize(&mut self) {

        // We parse potential other errors in statements
        while !self.eof() {
            match self.at().kind {
                TokenKind::NewLine
                | TokenKind::Struct
                | TokenKind::Fn
                | TokenKind::Var
                | TokenKind::Const
                | TokenKind::For
                | TokenKind::If
                | TokenKind::While
                | TokenKind::Print
                | TokenKind::Return => return,
                _ => { let _ = self.eat(); }
            }
        }
    }
}

use crate::expr::{
    BinaryExpr, Expr, GroupingExpr, IdentifierExpr, IntLiteralExpr, RealLiteralExpr, StrLiteralExpr, UnaryExpr
};
use crate::lexer::{Loc, Token, TokenKind};
use crate::results::PhyResult;


#[derive(Default)]
pub struct Parser<'a> {
    tokens: &'a [Token],
    start_loc: usize,
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn parse(&mut self, tokens: &'a [Token]) -> Result<Vec<Expr>, Vec<PhyResult>> {
        self.tokens = tokens;

        let mut nodes: Vec<Expr> = vec![];
        let mut errors: Vec<PhyResult> = vec![];

        while !self.eof() {
            // If we have a new line to begin a statement/expr parsing,
            // we skip it. There are important only in parsing steps
            while !self.eof() && self.is_at(TokenKind::NewLine) {
                self.current += 1;
            }

            // We could have reached EOF while skipping new lines
            if self.eof() {
                break;
            }

            self.start_loc = self.at().loc.start;

            match self.parse_expr() {
                Ok(expr) => nodes.push(expr),
                Err(e) => errors.push(e),
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(nodes)
    }

    fn parse_expr(&mut self) -> Result<Expr, PhyResult> {
        self.parse_equality()
    }

    fn parse_equality(&mut self) -> Result<Expr, PhyResult> {
        let mut expr = self.parse_comparison()?;

        while self.is_at(TokenKind::EqualEqual) || self.is_at(TokenKind::BangEqual) {
            let operator = self.eat()?.value.clone();
            let right = self.parse_comparison()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                loc: self.get_loc(),
            });
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, PhyResult> {
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
                right: Box::new(right),
                loc: self.get_loc(),
            });
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expr, PhyResult> {
        let mut expr = self.parse_factor()?;

        while self.is_at(TokenKind::Minus) || self.is_at(TokenKind::Plus) {
            let operator = self.eat()?.value.clone();
            let right = self.parse_factor()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                loc: self.get_loc(),
            });
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expr, PhyResult> {
        let mut expr = self.parse_unary()?;

        while self.is_at(TokenKind::Star) || self.is_at(TokenKind::Slash) {
            let operator = self.eat()?.value.clone();
            let right = self.parse_unary()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                loc: self.get_loc(),
            });
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, PhyResult> {
        if self.is_at(TokenKind::Bang) || self.is_at(TokenKind::Minus) {
            let operator = self.eat()?.value.clone();
            let right = self.parse_primary()?;

            return Ok(Expr::Unary(UnaryExpr {
                operator,
                right: Box::new(right),
                loc: self.get_loc(),
            }));
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr, PhyResult> {
        match &self.eat()?.kind {
            TokenKind::Identifier | TokenKind::True | TokenKind::False | TokenKind::Null => {
                Ok(Expr::Identifier(IdentifierExpr {
                    name: self.prev().value.clone(),
                    loc: self.get_loc(),
                }))
            }
            TokenKind::Int => self.parse_int_literal(),
            TokenKind::Real => self.parse_real_literal(),
            TokenKind::String => self.parse_str_literal(),
            TokenKind::OpenParen => self.parse_grouping(),
            TokenKind::NewLine => Err(self.trigger_error("Unexpected end of line".into(), false)),
            tk => match tk {
                TokenKind::Star | TokenKind::Plus | TokenKind::Slash | TokenKind::Modulo => {
                    Err(self
                        .trigger_error("Missing left hand side of binary expression".into(), true))
                }
                _ => {
                    Err(self
                        .trigger_error(format!("Unknown token to parse: '{}'", self.prev()), true))
                }
            },
        }
    }

    fn parse_int_literal(&self) -> Result<Expr, PhyResult> {
        let tk = self.prev();
        let value = tk
            .value
            .parse::<i64>()
            .map_err(|_| PhyResult::internal_error("Error parsing int from string".into()))?;

        Ok(Expr::IntLiteral(IntLiteralExpr {
            value,
            loc: self.get_loc(),
        }))
    }

    fn parse_real_literal(&self) -> Result<Expr, PhyResult> {
        let tk = self.prev();
        let value = tk
            .value
            .parse::<f64>()
            .map_err(|_| PhyResult::internal_error("Error parsing real from string".into()))?;

        Ok(Expr::RealLiteral(RealLiteralExpr {
            value,
            loc: self.get_loc(),
        }))
    }
    
    fn parse_str_literal(&self) -> Result<Expr, PhyResult> {
        let tk = self.prev();

        Ok(Expr::StrLiteral(StrLiteralExpr {
            value: tk.value.clone(),
            loc: self.get_loc()
        }))
    }

    fn parse_grouping(&mut self) -> Result<Expr, PhyResult> {
        let expr = self.parse_expr()?;
        self.expect(TokenKind::CloseParen).map_err(|_| {
            PhyResult::parser_error("Parenthesis group is never closed".into(), self.get_loc())
        })?;

        Ok(Expr::Grouping(GroupingExpr {
            expr: Box::new(expr),
            loc: self.get_loc(),
        }))
    }

    fn at(&self) -> &Token {
        self.tokens.get(self.current).unwrap()
    }

    fn eat(&mut self) -> Result<&Token, PhyResult> {
        if self.eof() {
            return Err(PhyResult::parser_error("Unexpected end of file".into(), self.get_loc()))
        }

        self.current += 1;
        Ok(self.prev())
    }

    fn expect(&mut self, kind: TokenKind) -> Result<(), PhyResult> {
        let tk = self.eat()?;

        match tk.kind == kind {
            true => Ok(()),
            false => {
                let msg = format!("Expected token type '{:?}', found: {:?}", kind, tk.kind);
                Err(self.trigger_error(msg, true))
            }
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

    // We dont have to activate the synchro each time, if the error occured
    // because we ate a '\n' that wasn't supposed to be here, we are already
    // past the error, we are on the new line. No need to synchronize
    fn trigger_error(&mut self, msg: String, synchro: bool) -> PhyResult {
        if synchro {
            self.synchronize();
        }

        PhyResult::parser_error(msg, self.get_loc())
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
                _ => {
                    let _ = self.eat();
                }
            }
        }
    }

    fn get_loc(&self) -> Loc {
        Loc::new(self.start_loc, self.at().loc.start)
    }
}

#[cfg(test)]
mod tests {
    use ecow::EcoString;

    use super::Parser;
    use crate::{
        lexer::{Lexer, Loc},
        results::PhyResultKind,
        test_parser::TestParser,
    };

    #[test]
    fn parse_primary() {
        let code: String = "12
24.
54.678
\"foo bar! 5-{6}\"
(true)
( (null ))"
            .into();
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(code.as_str()).unwrap();
        let mut parser = Parser::default();
        let nodes = parser.parse(tokens).unwrap();

        let mut test_parser = TestParser::default();
        let infos = test_parser.get_all_infos(&nodes).unwrap();

        assert_eq!(infos.get_int_values(), vec![&12]);
        assert_eq!(infos.get_real_values(), vec![&24., &54.678]);
        assert_eq!(infos.get_str_values(), vec![EcoString::from("foo bar! 5-{6}")]);

        assert_eq!(
            infos.get_grp_values()[0].get_ident_values(),
            vec![EcoString::from("true")]
        );
        assert_eq!(
            infos.get_grp_values()[1].get_grp_values()[0].get_ident_values(),
            vec![EcoString::from("null")]
        );
    }

    #[test]
    fn parse_binop() {
        let code: String = "14. + -67
25. + 3 * 4
25. / 3 + 4
            "
        .into();
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(code.as_str()).unwrap();
        let mut parser = Parser::default();
        let nodes = parser.parse(tokens).unwrap();
        let mut test_parser = TestParser::default();
        let infos = test_parser.get_all_infos(&nodes).unwrap();

        let left = infos.get_binop_values()[0].0.get_real_values()[0];
        let right = infos.get_binop_values()[0].2.unary[0].expr.get_int_values()[0];
        assert_eq!(left, &14f64);
        assert_eq!(infos.get_binop_values()[0].1, EcoString::from("+"));
        assert_eq!(right, &67);

        let left = infos.get_binop_values()[1].0.get_real_values()[0];
        let op = infos.get_binop_values()[1].1.clone();
        let right_binop = &infos.get_binop_values()[1].2.get_binop_values()[0];
        let left_bis = right_binop.0.get_int_values()[0];
        let op_bis = right_binop.1.clone();
        let right_bis = right_binop.2.get_int_values()[0];
        assert_eq!(left, &25f64);
        assert_eq!(op, EcoString::from("+"));
        assert_eq!(left_bis, &3i64);
        assert_eq!(op_bis, EcoString::from("*"));
        assert_eq!(right_bis, &4i64);

        let left_binop = &infos.get_binop_values()[2].0.get_binop_values()[0];
        let left_bis = left_binop.0.get_real_values()[0];
        let op_bis = left_binop.1.clone();
        let right_bis = left_binop.2.get_int_values()[0];
        let op = infos.get_binop_values()[2].1.clone();
        let right = infos.get_binop_values()[2].2.get_int_values()[0];
        assert_eq!(left_bis, &25f64);
        assert_eq!(op_bis, EcoString::from("/"));
        assert_eq!(right_bis, &3i64);
        assert_eq!(op, EcoString::from("+"));
        assert_eq!(right, &4i64);
    }

    #[test]
    fn parse_unary() {
        let code: String = "-12
-foo
-54.67
!true"
            .into();
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(code.as_str()).unwrap();
        let mut parser = Parser::default();
        let nodes = parser.parse(tokens).unwrap();

        let mut test_parser = TestParser::default();
        let infos = test_parser.get_all_infos(&nodes).unwrap();

        assert_eq!(infos.unary[0].expr.get_int_values(), vec![&12]);
        assert_eq!(infos.unary[0].op, EcoString::from("-"));

        assert_eq!(
            infos.unary[1].expr.get_ident_values(),
            vec![EcoString::from("foo")]
        );
        assert_eq!(infos.unary[1].op, EcoString::from("-"));

        assert_eq!(infos.unary[2].expr.get_real_values(), vec![&54.67]);
        assert_eq!(infos.unary[2].op, EcoString::from("-"));

        assert_eq!(
            infos.unary[3].expr.get_ident_values(),
            vec![EcoString::from("true")]
        );
        assert_eq!(infos.unary[3].op, EcoString::from("!"));
    }

    #[test]
    fn location() {
        let code: String = "-12
    98
  -24. + 6
(a + foo)"
            .into();
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(code.as_str()).unwrap();
        let mut parser = Parser::default();
        let nodes = parser.parse(tokens).unwrap();

        let mut test_parser = TestParser::default();
        let infos = test_parser.get_all_infos(&nodes).unwrap();

        assert_eq!(
            infos.get_locations(),
            vec![
                &Loc::new(8, 10),
                &Loc::new(13, 21),
                &Loc::new(0, 3),
                &Loc::new(22, 31),
            ]
        );
    }

    #[test]
    fn errors() {
        let code: String = "+5
*6
/7
%8
5 +
(art + "
            .into();
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(code.as_str()).unwrap();
        let mut parser = Parser::default();
        let errs = parser.parse(tokens).err().unwrap();
        assert_eq!(errs.len(), 6usize);
        // It must not be internal errors
        assert!(errs.iter().all(|e| e.kind != PhyResultKind::InternalErr));
    }
}

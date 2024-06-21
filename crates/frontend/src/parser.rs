use colored::*;
use ecow::EcoString;
use thiserror::Error;

use crate::expr::{
    AssignExpr, BinaryExpr, Expr, GroupingExpr, IdentifierExpr, IntLiteralExpr, LogicalExpr,
    RealLiteralExpr, StrLiteralExpr, UnaryExpr,
};
use crate::lexer::{Loc, Token, TokenKind};
use crate::results::{PhyReport, PhyResult};
use crate::stmt::{
    BlockStmt, ExprStmt, ForRange, ForStmt, IfStmt, PrintStmt, Stmt, VarDeclStmt, WhileStmt,
};

// ----------------
// Error managment
// ----------------
#[derive(Debug, Error, PartialEq)]
pub enum ParserErr {
    // Primary
    #[error("unexpected end of line")]
    UnexpectedEol,

    #[error("missing left hand side of binary expression")]
    MissingLhsInBinop,

    #[error("unexpected token to parse: '{0}'")]
    UnexpectedToken(String),

    #[error("error parsing int")]
    ParsingInt,

    #[error("error parsing real")]
    ParsingReal,

    #[error("parenthesis group is never closed")]
    ParenNeverClosed,

    // Variables
    #[error("missing variable name after 'var' keyword in declaration")]
    VarDeclNoName,

    #[error("value assigned during declaration is incorrect: {0}")]
    IncorrectVarDeclVal(String),

    #[error("expected an assignment or nothing in variable declaration")]
    WrongRhsVarDecl,

    #[error("expected expression for variable assignment")]
    NoExprAssign,

    // Assignment
    #[error("invalid assignment target")]
    InvalidAssignTarget,

    // Assignment
    #[error("expected '}}' after block statement")]
    UnclosedBlock,

    // If
    #[error("missing block start '{{' after 'if' condition")]
    MissingIfOpenBrace,

    #[error("missing block end '}}' in 'if' branch")]
    MissingIfCloseBrace,

    #[error("missing block start '{{' after 'else'")]
    MissingElseOpenBrace,

    #[error("missing block end '}}' in 'else' branch")]
    MissingElseCloseBrace,

    #[error("'if' statement with no condition")]
    IfWithNoCond,

    #[error("missing right expression in 'or' statement")]
    OrWithNoCond,

    #[error("missing right expression in 'and' statement")]
    AndWithNoCond,

    #[error("variable declaration inside 'if' block is not allowed")]
    VarDeclInIf,

    #[error("'else' branch can't have a condition")]
    ElseWithCond,

    // While
    #[error("'while' statement with no condition")]
    WhileWithNoCond,

    #[error("missing block start '{{' after 'while' condition")]
    MissingWhileOpenBrace,

    #[error("missing block end '}}' after 'while' body")]
    MissingWhileCloseBrace,

    #[error("'while' can't be empty, it requires at least one statement to break infinite loop")]
    NoBodyWhile,

    // For
    #[error("missing variable name in 'for' loop")]
    MissingVarNameFor,

    #[error("missing 'in' after variable name in 'for' loop")]
    MissingInFor,

    #[error("missing range in 'for' loop")]
    MissingForRange,

    #[error("only 'ints' are supported as 'for' ranges")]
    NonIntForRange,

    #[error("range can only be positive")]
    NegativeForRange,

    #[error("end of range smaller than start")]
    LesserEndForRange,

    #[error("missing start of range before '..'")]
    MissingStartForRange,

    #[error("missing end of range after '..'")]
    MissingEndForRange,

    #[error("missing block start '{{' after 'for' condition")]
    MissingForOpenBrace,

    #[error("missing block end '}}' after 'for' body")]
    MissingForCloseBrace,

    // Others
    #[error("unexpected end of file")]
    UnexpectedEof,

    #[error("expected token type '{0:?}', found: {1:?}")]
    ExpectedToken(String, String),
}

impl PhyReport for ParserErr {
    fn get_err_msg(&self) -> String {
        format!("{} {}", "Parser error:".red(), self)
    }
}

pub(crate) type PhyResParser = PhyResult<ParserErr>;
pub(crate) type ParserStmtRes = Result<Stmt, PhyResParser>;
pub(crate) type ParserExprRes = Result<Expr, PhyResParser>;
// ---------
//  Parsing
// ---------
#[derive(Default)]
pub struct Parser<'a> {
    tokens: &'a [Token],
    start_loc: usize,
    current: usize,
}

// TODO: Faire des localisation plus specifique. PAr exemple, si on parse :
//          print a + 2
//  la localisation de la variable "a" est en réalité celle de "print a".
//  Il faudrait faire une stack d'appel avec des localisations locales et
//  remonter.
impl<'a> Parser<'a> {
    pub fn parse(&mut self, tokens: &'a [Token]) -> Result<Vec<Stmt>, Vec<PhyResParser>> {
        self.tokens = tokens;

        let mut stmts: Vec<Stmt> = vec![];
        let mut errors: Vec<PhyResParser> = vec![];

        while !self.eof() {
            self.skip_new_lines();

            // We could have reached EOF while skipping new lines
            if self.eof() {
                break;
            }

            match self.parse_declarations() {
                Ok(stmt) => stmts.push(stmt),
                Err(e) => errors.push(e),
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(stmts)
    }

    fn parse_declarations(&mut self) -> ParserStmtRes {
        match self.at().kind {
            TokenKind::Var => self.parse_var_declaration(),
            _ => self.parse_stmt(),
        }
    }

    fn parse_var_declaration(&mut self) -> ParserStmtRes {
        self.expect(TokenKind::Var)?;
        let name = self
            .expect(TokenKind::Identifier)
            .map_err(|_| self.trigger_error(ParserErr::VarDeclNoName, true))?
            .value
            .clone();

        let mut value: Option<Expr> = None;

        match self.at().kind {
            TokenKind::Equal => {
                self.eat()?;
                let v = self.parse_expr();

                match v {
                    Ok(e) => value = Some(e),
                    Err(e) => match e.err {
                        ParserErr::UnexpectedEol | ParserErr::UnexpectedEof => {
                            return Err(self.trigger_error(ParserErr::NoExprAssign, true))
                        }
                        e => {
                            return Err(self.trigger_error(
                                ParserErr::IncorrectVarDeclVal(e.to_string()),
                                true,
                            ))
                        }
                    },
                }
            }
            TokenKind::NewLine | TokenKind::Eof => {}
            _ => return Err(self.trigger_error(ParserErr::WrongRhsVarDecl, true)),
        }

        self.skip_new_lines();

        Ok(Stmt::VarDecl(VarDeclStmt {
            name,
            value,
            loc: self.get_loc(),
        }))
    }

    fn parse_stmt(&mut self) -> ParserStmtRes {
        let stmt = match self.at().kind {
            TokenKind::Print => self.parse_print_stmt(),
            TokenKind::OpenBrace => self.parse_block_stmt(),
            TokenKind::If => self.parse_if_stmt(),
            TokenKind::While => self.parse_while_stmt(),
            TokenKind::For => self.parse_for_stmt(),
            _ => self.parse_expr_stmt(),
        };

        self.skip_new_lines();

        stmt
    }

    fn parse_print_stmt(&mut self) -> ParserStmtRes {
        self.expect(TokenKind::Print)?;

        let expr = self.parse_expr()?;

        Ok(Stmt::Print(PrintStmt {
            expr,
            loc: self.get_loc(),
        }))
    }

    fn parse_block_stmt(&mut self) -> ParserStmtRes {
        self.expect_and_skip(TokenKind::OpenBrace)?;

        let mut stmts: Vec<Stmt> = vec![];

        while !self.is_at(TokenKind::CloseBrace) && !self.eof() {
            stmts.push(self.parse_declarations()?);
        }

        self.expect_and_skip(TokenKind::CloseBrace)
            .map_err(|_| self.trigger_error(ParserErr::UnclosedBlock, true))?;

        Ok(Stmt::Block(BlockStmt {
            stmts,
            loc: self.get_loc(),
        }))
    }

    fn parse_if_stmt(&mut self) -> ParserStmtRes {
        self.eat()?;
        self.is_at_brace_or_end_of(ParserErr::IfWithNoCond)?;

        let condition = self.parse_expr()?;

        self.skip_expect_and_skip(TokenKind::OpenBrace)
            .map_err(|_| self.trigger_error(ParserErr::MissingIfOpenBrace, true))?;

        let mut then_branch = None;

        if !self.is_at(TokenKind::CloseBrace) {
            if self.is_at(TokenKind::Var) {
                return Err(self.trigger_error(ParserErr::VarDeclInIf, true));
            }

            then_branch = Some(Box::new(self.parse_stmt()?));
        }

        self.expect_and_skip(TokenKind::CloseBrace)
            .map_err(|_| self.trigger_error(ParserErr::MissingIfCloseBrace, true))?;

        let mut else_branch: Option<Box<Stmt>> = None;

        if self.is_at(TokenKind::Else) {
            self.eat()?;
            self.skip_new_lines();
            self.is_not_at_brace_or_end_of(ParserErr::ElseWithCond)?;

            self.expect_and_skip(TokenKind::OpenBrace)
                .map_err(|_| self.trigger_error(ParserErr::MissingElseOpenBrace, true))?;

            match self.at().kind {
                TokenKind::CloseBrace => {
                    self.eat()?;
                }
                _ => {
                    else_branch = Some(Box::new(self.parse_stmt()?));

                    self.expect_and_skip(TokenKind::CloseBrace)
                        .map_err(|_| self.trigger_error(ParserErr::MissingElseCloseBrace, true))?;
                }
            }
        }

        Ok(Stmt::If(IfStmt {
            condition,
            then_branch,
            else_branch,
            loc: self.get_loc(),
        }))
    }

    fn parse_while_stmt(&mut self) -> ParserStmtRes {
        self.eat()?;
        self.is_at_brace_or_end_of(ParserErr::WhileWithNoCond)?;

        let condition = self.parse_expr()?;

        self.skip_expect_and_skip(TokenKind::OpenBrace)
            .map_err(|_| self.trigger_error(ParserErr::MissingWhileOpenBrace, true))?;

        if self.is_at(TokenKind::CloseBrace) {
            return Err(self.trigger_error(ParserErr::NoBodyWhile, true));
        }

        let body = Box::new(self.parse_stmt()?);

        self.expect_and_skip(TokenKind::CloseBrace)
            .map_err(|_| self.trigger_error(ParserErr::MissingWhileCloseBrace, true))?;

        Ok(Stmt::While(WhileStmt {
            condition,
            body,
            loc: self.get_loc(),
        }))
    }

    fn parse_for_stmt(&mut self) -> ParserStmtRes {
        self.eat()?;

        let placeholder = self
            .expect(TokenKind::Identifier)
            .map_err(|_| self.trigger_error(ParserErr::MissingVarNameFor, true))?
            .value;

        self.expect(TokenKind::In)
            .map_err(|_| self.trigger_error(ParserErr::MissingInFor, true))?;

        self.is_at_brace_or_end_of(ParserErr::MissingForRange)?;

        if self.is_at(TokenKind::DotDot) {
            return Err(self.trigger_error(ParserErr::MissingStartForRange, true))
        }

        if self.is_at(TokenKind::Minus) {
            return Err(self.trigger_error(ParserErr::NegativeForRange, true))
        }

        let start = self
            .expect(TokenKind::Int)
            .map_err(|_| self.trigger_error(ParserErr::NonIntForRange, true))?
            .value
            .parse::<i64>()
            .map_err(|_| self.trigger_error(ParserErr::ParsingInt, true))?;

        let mut end = None;
        if self.is_at(TokenKind::DotDot) {
            self.eat()?;

            self.is_at_brace_or_end_of(ParserErr::MissingEndForRange)?;

        println!("Before end: {}", self.at());
            end = Some(
                self.expect(TokenKind::Int)
                    .map_err(|_| self.trigger_error(ParserErr::NonIntForRange, true))?
                    .value
                    .parse::<i64>()
                    .map_err(|_| self.trigger_error(ParserErr::ParsingInt, true))?
            );

        println!("After end: {}", self.at());

            if Some(start) > end {
                return Err(self.trigger_error(ParserErr::LesserEndForRange, true))
            }
        }

        self.skip_expect_and_skip(TokenKind::OpenBrace)
            .map_err(|_| self.trigger_error(ParserErr::MissingForOpenBrace, true))?;

        let mut body = None;
        if !self.is_at(TokenKind::CloseBrace) {
            body = Some(Box::new(self.parse_stmt()?));
        }

        self.skip_expect_and_skip(TokenKind::CloseBrace)
            .map_err(|_| self.trigger_error(ParserErr::MissingForCloseBrace, true))?;

        Ok(Stmt::For(ForStmt {
            placeholder,
            range: ForRange { start, end },
            body,
            loc: self.get_loc(),
        }))
    }

    fn parse_expr_stmt(&mut self) -> ParserStmtRes {
        let expr = self.parse_expr()?;

        Ok(Stmt::Expr(ExprStmt {
            expr,
            loc: self.get_loc(),
        }))
    }

    fn parse_expr(&mut self) -> ParserExprRes {
        self.parse_assign()
    }

    fn parse_assign(&mut self) -> ParserExprRes {
        let assigne = self.parse_or()?;

        if self.is_at(TokenKind::Equal) {
            self.eat()?;
            let value = self.parse_assign()?;

            if let Expr::Identifier(e) = assigne {
                return Ok(Expr::Assign(AssignExpr {
                    name: e.name.clone(),
                    value: Box::new(value),
                    loc: self.get_loc(),
                }));
            } else {
                return Err(self.trigger_error(ParserErr::InvalidAssignTarget, true));
            }
        }

        Ok(assigne)
    }

    fn parse_or(&mut self) -> ParserExprRes {
        let left = self.parse_and()?;

        if self.is_at(TokenKind::Or) {
            self.eat()?;

            if self.is_at(TokenKind::OpenBrace)
                || self.is_at(TokenKind::Eof)
                || self.is_at(TokenKind::NewLine)
            {
                return Err(self.trigger_error(ParserErr::OrWithNoCond, true));
            }

            let right = self.parse_and()?;

            return Ok(Expr::Logical(LogicalExpr {
                left: Box::new(left),
                operator: EcoString::from("or"),
                right: Box::new(right),
                loc: self.get_loc(),
            }));
        }

        Ok(left)
    }

    fn parse_and(&mut self) -> ParserExprRes {
        let left = self.parse_equality()?;

        if self.is_at(TokenKind::And) {
            self.eat()?;

            if self.is_at(TokenKind::OpenBrace)
                || self.is_at(TokenKind::Eof)
                || self.is_at(TokenKind::NewLine)
            {
                return Err(self.trigger_error(ParserErr::AndWithNoCond, true));
            }

            let right = self.parse_equality()?;

            return Ok(Expr::Logical(LogicalExpr {
                left: Box::new(left),
                operator: EcoString::from("and"),
                right: Box::new(right),
                loc: self.get_loc(),
            }));
        }

        Ok(left)
    }

    fn parse_equality(&mut self) -> ParserExprRes {
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

    fn parse_comparison(&mut self) -> ParserExprRes {
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

    fn parse_term(&mut self) -> ParserExprRes {
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

    fn parse_factor(&mut self) -> ParserExprRes {
        let mut expr = self.parse_unary()?;

        while self.is_at(TokenKind::Star)
            || self.is_at(TokenKind::Slash)
            || self.is_at(TokenKind::Modulo)
        {
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

    fn parse_unary(&mut self) -> ParserExprRes {
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

    fn parse_primary(&mut self) -> ParserExprRes {
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
            TokenKind::NewLine => Err(self.trigger_error(ParserErr::UnexpectedEol, false)),
            tk => {
                match tk {
                    TokenKind::Star | TokenKind::Plus | TokenKind::Slash | TokenKind::Modulo => {
                        Err(self.trigger_error(ParserErr::MissingLhsInBinop, true))
                    }
                    _ => Err(self
                        .trigger_error(ParserErr::UnexpectedToken(self.prev().to_string()), true)),
                }
            }
        }
    }

    fn parse_int_literal(&mut self) -> ParserExprRes {
        let tk = self.prev();
        let value = tk
            .value
            .parse::<i64>()
            .map_err(|_| self.trigger_error(ParserErr::ParsingInt, true))?;

        Ok(Expr::IntLiteral(IntLiteralExpr {
            value,
            loc: self.get_loc(),
        }))
    }

    fn parse_real_literal(&mut self) -> ParserExprRes {
        let tk = self.prev();
        let value = tk
            .value
            .parse::<f64>()
            .map_err(|_| self.trigger_error(ParserErr::ParsingReal, true))?;

        Ok(Expr::RealLiteral(RealLiteralExpr {
            value,
            loc: self.get_loc(),
        }))
    }

    fn parse_str_literal(&self) -> ParserExprRes {
        let tk = self.prev();

        Ok(Expr::StrLiteral(StrLiteralExpr {
            value: tk.value.clone(),
            loc: self.get_loc(),
        }))
    }

    fn parse_grouping(&mut self) -> ParserExprRes {
        let expr = match self.parse_expr() {
            Ok(expr) => expr,
            Err(e) => match e.err {
                ParserErr::UnexpectedEof | ParserErr::UnexpectedEol => {
                    return Err(PhyResult::new(
                        ParserErr::ParenNeverClosed,
                        Some(self.get_loc()),
                    ))
                }
                _ => return Err(e),
            },
        };

        self.expect(TokenKind::CloseParen)
            .map_err(|_| PhyResult::new(ParserErr::ParenNeverClosed, Some(self.get_loc())))?;

        Ok(Expr::Grouping(GroupingExpr {
            expr: Box::new(expr),
            loc: self.get_loc(),
        }))
    }

    fn at(&self) -> &Token {
        self.tokens.get(self.current).unwrap()
    }

    fn eat(&mut self) -> Result<&Token, PhyResParser> {
        if self.eof() {
            return Err(PhyResult::new(
                ParserErr::UnexpectedEof,
                Some(self.get_loc()),
            ));
        }

        self.current += 1;
        Ok(self.prev())
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token, PhyResParser> {
        let tk = self.eat()?;

        match tk.kind == kind {
            true => Ok(self.prev().clone()),
            false => Err(PhyResult::new(
                ParserErr::ExpectedToken(format!("{:?}", kind), format!("{:?}", tk.kind)),
                Some(self.get_loc()),
            )),
        }
    }

    fn expect_and_skip(&mut self, kind: TokenKind) -> Result<(), PhyResParser> {
        self.expect(kind)?;
        self.skip_new_lines();

        Ok(())
    }

    fn skip_expect_and_skip(&mut self, kind: TokenKind) -> Result<(), PhyResParser> {
        self.skip_new_lines();
        self.expect(kind)?;
        self.skip_new_lines();

        Ok(())
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

    fn skip_new_lines(&mut self) {
        // If we have a new line to begin a statement/expr parsing,
        // we skip it. There are important only in parsing steps
        while !self.eof() && self.is_at(TokenKind::NewLine) {
            self.current += 1;
        }

        self.start_loc = self.at().loc.start;
    }

    // We dont have to activate the synchro each time, if the error occured
    // because we ate a '\n' that wasn't supposed to be here, we are already
    // past the error, we are on the new line. No need to synchronize
    fn trigger_error(&mut self, err: ParserErr, synchro: bool) -> PhyResParser {
        if synchro {
            self.synchronize();
        }

        PhyResult::new(err, Some(self.get_loc()))
    }

    fn is_at_brace_or_end_of(&mut self, err: ParserErr) -> Result<(), PhyResParser> {
        if self.is_at(TokenKind::OpenBrace)
            || self.is_at(TokenKind::Eof)
            || self.is_at(TokenKind::NewLine)
        {
            return Err(self.trigger_error(err, true));
        }

        Ok(())
    }

    fn is_not_at_brace_or_end_of(&mut self, err: ParserErr) -> Result<(), PhyResParser> {
        if !self.is_at(TokenKind::OpenBrace)
            || self.is_at(TokenKind::Eof)
            || self.is_at(TokenKind::NewLine)
        {
            return Err(self.trigger_error(err, true));
        }

        Ok(())
    }

    // TODO: For now, we are only looking for new line token as we
    // don't have ';' to clearly know where the current statement stops.
    // It would be great to have an argument to this function that let
    // us know where we were when we got the error to know which corresponding
    // token to look for. In a struct def, we go for a closing '}', ...

    // We are here in panic mode
    fn synchronize(&mut self) {
        // If the error occured because unexpected Eol, we are synchro
        if self.prev().kind == TokenKind::NewLine {
            return;
        }

        // We parse potential other errors in statements
        while !self.eof() {
            match self.at().kind {
                TokenKind::NewLine => return,
                //| TokenKind::Struct
                //| TokenKind::Fn
                //| TokenKind::Var
                //| TokenKind::Const
                //| TokenKind::For
                //| TokenKind::If
                //| TokenKind::While
                //| TokenKind::Print
                //| TokenKind::Return => return,
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
    use crate::lexer::Loc;
    use crate::parser::ParserErr;
    use crate::utils::*;
    use ecow::EcoString;

    #[test]
    fn parse_primary() {
        let code = "12
24.
54.678
\"foo bar! 5-{6}\"
(true)
( (null ))";

        let infos = get_expr_nodes_infos(code);
        assert_eq!(infos.get_int_values(), vec![&12]);
        assert_eq!(infos.get_real_values(), vec![&24., &54.678]);
        assert_eq!(
            infos.get_str_values(),
            vec![EcoString::from("foo bar! 5-{6}")]
        );

        assert_eq!(
            infos.get_grp_values()[0].get_ident_values(),
            vec![EcoString::from("true")]
        );
        assert_eq!(
            infos.get_grp_values()[1].get_grp_values()[0].get_ident_values(),
            vec![EcoString::from("null")]
        );

        // Errors
        let code = "(art + 
";

        let errs = lex_and_parse(code).err().unwrap();
        let e = errs.iter().map(|e| &e.err).collect::<Vec<&ParserErr>>();

        assert_eq!(e, vec![&ParserErr::ParenNeverClosed]);
    }

    #[test]
    fn parse_binop() {
        let code = "14. + -67
25. + 3 * 4
25. / 3 + 4";

        let infos = get_expr_nodes_infos(code);
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

        // Errors
        let code = "5 +
";

        let errs = lex_and_parse(code).err().unwrap();
        let e = errs.iter().map(|e| &e.err).collect::<Vec<&ParserErr>>();

        assert_eq!(e, vec![&ParserErr::UnexpectedEol]);
    }

    #[test]
    fn parse_unary() {
        let code = "-12
-foo
-54.67
!true";

        let infos = get_expr_nodes_infos(code);
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

        // Errors
        let code = "+5
*6
/7
%8";

        let errs = lex_and_parse(code).err().unwrap();
        let e = errs.iter().map(|e| &e.err).collect::<Vec<&ParserErr>>();

        assert_eq!(
            e,
            vec![
                &ParserErr::MissingLhsInBinop,
                &ParserErr::MissingLhsInBinop,
                &ParserErr::MissingLhsInBinop,
                &ParserErr::MissingLhsInBinop,
            ]
        );
    }

    #[test]
    fn var_declaration() {
        let code = "var a
var b_cc = 4.
var c34_U = 2 + 6 ";

        let infos = get_nodes_infos(code);
        assert_eq!(infos.var_decl[0], ("a".into(), None));
        assert_eq!(infos.var_decl[1].0, EcoString::from("b_cc"));
        assert_eq!(
            infos.var_decl[1].1.as_ref().unwrap().get_real_values(),
            vec![&4f64]
        );
        assert_eq!(infos.var_decl[2].0, EcoString::from("c34_U"));
        assert_eq!(
            infos.var_decl[2].1.as_ref().unwrap().get_binop_values()[0]
                .0
                .get_int_values(),
            vec![&2]
        );
        assert_eq!(
            infos.var_decl[2].1.as_ref().unwrap().get_binop_values()[0].1,
            EcoString::from("+")
        );
        assert_eq!(
            infos.var_decl[2].1.as_ref().unwrap().get_binop_values()[0]
                .2
                .get_int_values(),
            vec![&6]
        );

        // Errors
        let code = "var 
var b if
var b =
var c = var";
        let errs = lex_and_parse(code).err().unwrap();
        let e = errs.iter().map(|e| &e.err).collect::<Vec<&ParserErr>>();
        assert!(e[0] == &ParserErr::VarDeclNoName);
        assert!(e[1] == &ParserErr::WrongRhsVarDecl);
        assert!(e[2] == &ParserErr::NoExprAssign, "it was: {}", e[2]);
        assert!(matches!(e[3], &ParserErr::IncorrectVarDeclVal { .. }));
    }

    #[test]
    fn location() {
        let code = "-12
    98
  -24. + 6
(a + foo)";

        let infos = get_expr_nodes_infos(code);
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
    fn assignment() {
        let code = "var a
a = 6

var foo_b4r = 8
foo_b4r = 65 % 6.";

        let infos = get_expr_nodes_infos(code);
        let assign_infos = infos.get_assign_values();
        assert_eq!(assign_infos[0].0, EcoString::from("a"));
        assert_eq!(assign_infos[0].1.get_int_values()[0], &6i64);

        let assign2_binop = assign_infos[1].1.get_binop_values();
        assert_eq!(assign_infos[1].0, EcoString::from("foo_b4r"));
        assert_eq!(assign2_binop[0].0.get_int_values()[0], &65i64);
        assert_eq!(assign2_binop[0].1, EcoString::from("%"));
        assert_eq!(assign2_binop[0].2.get_real_values()[0], &6f64);

        let code = "var a
7 = 6";

        let errs = lex_and_parse(code).err().unwrap();
        let e = errs.iter().map(|e| &e.err).collect::<Vec<&ParserErr>>();
        assert!(e[0] == &ParserErr::InvalidAssignTarget);
    }

    #[test]
    fn block() {
        let code = "
{
    var a = 3
    print 8
}
";
        let infos = get_stmt_nodes_infos(code);
        let block = &infos.block[0];
        assert_eq!(
            block.var_decl[0].0,
            EcoString::from("a"),
            "block: {:?}",
            block
        );
        assert_eq!(
            block.var_decl[0].1.as_ref().unwrap().get_int_values()[0],
            &3
        );
        assert_eq!(&block.print[0], &String::from("8"));

        let code = "
{
    var a = 3
";

        let errs = lex_and_parse(code).err().unwrap();
        let e = errs.iter().map(|e| &e.err).collect::<Vec<&ParserErr>>();
        assert!(e[0] == &ParserErr::UnclosedBlock);
    }

    #[test]
    fn if_stmt() {
        let code = "
if c {
   a = 1
}

if b == true {a = 1}
else {
    a = 0}

if a {} else {}
";
        // 0
        let infos = get_stmt_nodes_infos(code);
        let if_stmt = &infos.if_stmt[0];
        assert_eq!(
            if_stmt.condition.get_ident_values()[0],
            EcoString::from("c")
        );

        let then_branch = &if_stmt.then_branch.as_ref().unwrap().expr.assign[0];
        assert_eq!(then_branch.name, EcoString::from("a"));
        assert_eq!(then_branch.expr.get_int_values()[0], &1);

        // 1
        let if_stmt = &infos.if_stmt[1];
        let cond_binop = &if_stmt.condition.binop[0];
        assert_eq!(cond_binop.left.get_ident_values()[0], EcoString::from("b"));
        assert_eq!(cond_binop.op, EcoString::from("=="));
        assert_eq!(
            cond_binop.right.get_ident_values()[0],
            EcoString::from("true")
        );

        let then_branch = &if_stmt.then_branch.as_ref().unwrap().expr.assign[0];
        assert_eq!(then_branch.name, EcoString::from("a"));
        assert_eq!(then_branch.expr.get_int_values()[0], &1);

        let else_branch = &if_stmt.else_branch.as_ref().unwrap().expr.assign[0];
        assert_eq!(else_branch.name, EcoString::from("a"));
        assert_eq!(else_branch.expr.get_int_values()[0], &0);

        // 2
        let if_stmt = &infos.if_stmt[2];
        assert!(if_stmt.then_branch.is_none());
        assert!(if_stmt.else_branch.is_none());

        // Errors
        let code = "
if
if {}
if a { var a = 1 }
if a {} else a {}
";
        // 0
        let errs = lex_and_parse(code).err().unwrap();
        let e = errs.iter().map(|e| &e.err).collect::<Vec<&ParserErr>>();
        assert!(e[0] == &ParserErr::IfWithNoCond);
        assert!(e[1] == &ParserErr::IfWithNoCond);
        assert!(e[2] == &ParserErr::VarDeclInIf);
        assert!(e[3] == &ParserErr::ElseWithCond);
    }

    #[test]
    fn logical() {
        let code = "
if a or b {} else {}
if a and b {} else {}
if a and b or c {} else {}
";
        // 0
        let infos = get_stmt_nodes_infos(code);
        let logical = &infos.if_stmt[0].condition.logical[0];
        assert_eq!(logical.left.get_ident_values()[0], EcoString::from("a"));
        assert_eq!(logical.op, EcoString::from("or"));
        assert_eq!(logical.right.get_ident_values()[0], EcoString::from("b"));

        // 1
        let logical = &infos.if_stmt[1].condition.logical[0];
        assert_eq!(logical.left.get_ident_values()[0], EcoString::from("a"));
        assert_eq!(logical.op, EcoString::from("and"));
        assert_eq!(logical.right.get_ident_values()[0], EcoString::from("b"));

        // 2.2
        let logical = &infos.if_stmt[2].condition.logical[0];
        let prev_logical = &logical.left.logical[0];
        assert_eq!(
            prev_logical.left.get_ident_values()[0],
            EcoString::from("a")
        );
        assert_eq!(prev_logical.op, EcoString::from("and"));
        assert_eq!(
            prev_logical.right.get_ident_values()[0],
            EcoString::from("b")
        );
        assert_eq!(logical.op, EcoString::from("or"));
        assert_eq!(logical.right.get_ident_values()[0], EcoString::from("c"));

        // Errors
        let code = "
if a or {}
if a and {}
";
        // 0
        let errs = lex_and_parse(code).err().unwrap();
        let e = errs.iter().map(|e| &e.err).collect::<Vec<&ParserErr>>();
        assert!(e[0] == &ParserErr::OrWithNoCond);
        assert!(e[1] == &ParserErr::AndWithNoCond);
    }

    #[test]
    fn while_stmt() {
        let code = "
while a or true

{
    a = 1
}
";
        // 0
        let infos = get_stmt_nodes_infos(code);
        let while_stmt = &infos.while_stmt[0];
        let while_cond = &while_stmt.condition.logical[0];
        assert_eq!(while_cond.left.get_ident_values()[0], EcoString::from("a"));
        assert_eq!(while_cond.op, EcoString::from("or"));
        assert_eq!(
            while_cond.right.get_ident_values()[0],
            EcoString::from("true")
        );

        let body = &while_stmt.body.expr.assign[0];
        assert_eq!(body.name, EcoString::from("a"));
        assert_eq!(body.expr.get_int_values()[0], &1);

        // Errors
        let code = "
while {}
while a {}
while a
}
";
        // 0
        let errs = lex_and_parse(code).err().unwrap();
        let e = errs.iter().map(|e| &e.err).collect::<Vec<&ParserErr>>();
        assert!(e[0] == &ParserErr::WhileWithNoCond);
        assert!(e[1] == &ParserErr::NoBodyWhile);
        assert!(e[2] == &ParserErr::MissingWhileOpenBrace);
    }

    #[test]
    fn for_stmt() {
        let code = "
for a in 5 {}
for foo_b4r in 5..10

{
    print a

}
";
        // 0
        let infos = get_stmt_nodes_infos(code);
        let for_stmt = &infos.for_stmt[0];
        assert_eq!(for_stmt.placeholder, EcoString::from("a"));
        assert_eq!(
            for_stmt.range,
            (5, None)
        );

        let for_stmt = &infos.for_stmt[1];
        assert_eq!(for_stmt.placeholder, EcoString::from("foo_b4r"));
        assert_eq!(
            for_stmt.range,
            (5, Some(10))
        );

        // Errors
        let code = "
for in 5 {}
for a 5 {}
for a in {}
for a in 5 print a
for a in ..5 {}
for a in 5.. {}
for a in -5 {}
for a in 3.14 {}
for a in 5..0 {}
";
        // 0
        let errs = lex_and_parse(code).err().unwrap();
        let e = errs.iter().map(|e| &e.err).collect::<Vec<&ParserErr>>();
        assert!(e[0] == &ParserErr::MissingVarNameFor);
        assert!(e[1] == &ParserErr::MissingInFor);
        assert!(e[2] == &ParserErr::MissingForRange);
        assert!(e[3] == &ParserErr::MissingForOpenBrace);
        assert!(e[4] == &ParserErr::MissingStartForRange);
        assert!(e[5] == &ParserErr::MissingEndForRange);
        assert!(e[6] == &ParserErr::NegativeForRange, "{}", e[6]);
        assert!(e[7] == &ParserErr::NonIntForRange);
        assert!(e[8] == &ParserErr::LesserEndForRange);
    }
}

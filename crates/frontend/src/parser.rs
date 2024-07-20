use std::rc::Rc;

use colored::*;
use ecow::EcoString;
use thiserror::Error;

use crate::ast::expr::{
    AssignExpr, BinaryExpr, CallExpr, Expr, GetExpr, GroupingExpr, IdentifierExpr, IntLiteralExpr,
    LogicalExpr, FloatLiteralExpr, SelfExpr, SetExpr, StrLiteralExpr, UnaryExpr,
};
use crate::ast::stmt::{
    BlockStmt, ExprStmt, FnDeclStmt, ForRange, ForStmt, IfStmt, PrintStmt, ReturnStmt, Stmt,
    StructMember, StructStmt, VarDeclStmt, WhileStmt,
};
use crate::lexer::{Token, TokenKind};
use tools::results::{Loc, RevReport, RevResult};

// Children mods
pub mod test_parser;
pub mod utils;

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

    #[error("error parsing float")]
    ParsingFloat,

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

    // Call
    #[error("missing close parenthesis after arguments list")]
    MissingCallCloseParen,

    #[error("can't have more than 255 arguments")]
    TooManyCallArgs,

    #[error("missing comma to seperate arguments")]
    MissingArgsComma,

    // Function declaration
    #[error("missing function name after 'fn' keyword")]
    MissingFnName,

    #[error("missing '(' after function name")]
    NoOpenParenAfterFnName,

    #[error("can't have more than 255 parameters")]
    MaxFnArgs,

    #[error("function paramters must be identifiers")]
    WrongFnArgType,

    #[error("missing '{{' before function body")]
    MissingFnOpenBrace,

    // Structure declaration
    #[error("missing structure name after 'struct' keyword")]
    MissingStructName,

    #[error("missing '{{' before structure body")]
    MissingStructOpenBrace,

    #[error("missing '}}' after structure body")]
    MissingStructCloseBrace,

    #[error("fields must be declared before methods")]
    FieldDeclAfterFn,

    #[error("methods must be declared with 'fn' keyword")]
    MissingFnKwForMethod,

    // Property
    #[error("missing property name after '.'")]
    MissingPropName,

    // Others
    #[error("unexpected end of file")]
    UnexpectedEof,

    #[error("expected token type '{0:?}', found: {1:?}")]
    ExpectedToken(String, String),
}

impl RevReport for ParserErr {
    fn get_err_msg(&self) -> String {
        format!("{} {}", "Parser error:".red(), self)
    }
}

pub(crate) type RevResParser = RevResult<ParserErr>;
pub(crate) type ParserStmtRes = Result<Stmt, RevResParser>;
pub(crate) type ParserExprRes = Result<Expr, RevResParser>;


// To keep track were we are to know how to synchronize
#[derive(Default, PartialEq, Debug)]
enum CodeBlock {
    #[default]
    Global,
    FnDecl,
    FnDeclBeforeBody,
    FnCall,
    FnCallBeforeParen,
    Struct,
    Block,
}

// ---------
//  Parsing
// ---------
#[derive(Default)]
pub struct Parser<'a> {
    tokens: &'a [Token],
    start_loc: usize,
    current: usize,
    code_blocks: Vec<CodeBlock>,
}

impl<'a> Parser<'a> {
    pub fn parse(&mut self, tokens: &'a [Token]) -> Result<Vec<Stmt>, Vec<RevResParser>> {
        self.tokens = tokens;

        let mut stmts: Vec<Stmt> = vec![];
        let mut errors: Vec<RevResParser> = vec![];

        self.code_blocks.push(CodeBlock::Global);

        while !self.eof() {
            self.skip_new_lines();

            // We could have reached EOF while skipping new lines
            if self.eof() {
                break
            }

            match self.parse_declarations() {
                Ok(stmt) => stmts.push(stmt),
                Err(e) => {
                    // Case were the error was found on last method of struct
                    // we synchronized bit we are left with the last } of the 
                    // declaration, block or fn declaration
                    if self.at().kind == TokenKind::CloseBrace
                        && (
                            self.code_blocks.last() == Some(&CodeBlock::Struct)
                            || self.code_blocks.last() == Some(&CodeBlock::FnDecl)
                            || self.code_blocks.last() == Some(&CodeBlock::Block)
                        )
                    {
                        let _ = self.eat();
                        self.exit_code_block();
                    }

                    errors.push(e)
                },
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
            .map_err(|_| self.trigger_error(ParserErr::VarDeclNoName))?
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
                            return Err(self.trigger_error(ParserErr::NoExprAssign))
                        }
                        e => {
                            return Err(self.trigger_error(
                                ParserErr::IncorrectVarDeclVal(e.to_string())
                            ))
                        }
                    },
                }
            }
            TokenKind::NewLine | TokenKind::Eof | TokenKind::CloseBrace => {}
            _ => return Err(self.trigger_error(ParserErr::WrongRhsVarDecl)),
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
            TokenKind::Fn => self.parse_fn_decl_stmt(),
            TokenKind::Return => self.parse_return_stmt(),
            TokenKind::Struct => self.parse_struct_stmt(),
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
        self.enter_code_block(CodeBlock::Block);

        self.expect_and_skip(TokenKind::OpenBrace)?;

        let stmts = self.parse_block()?;

        self.exit_code_block();

        Ok(Stmt::Block(BlockStmt {
            stmts,
            loc: self.get_loc(),
        }))
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>, RevResParser> {
        let mut stmts: Vec<Stmt> = vec![];

        while !self.is_at(TokenKind::CloseBrace) && !self.eof() {
            stmts.push(self.parse_declarations()?);
            self.skip_new_lines();
        }

        self.expect_and_skip(TokenKind::CloseBrace)
            .map_err(|_| self.trigger_error(ParserErr::UnclosedBlock))?;

        Ok(stmts)
    }

    fn parse_if_stmt(&mut self) -> ParserStmtRes {
        self.eat()?;
        self.is_at_brace_or_end_of(ParserErr::IfWithNoCond)?;

        let condition = self.parse_expr()?;

        self.skip_expect_and_skip(TokenKind::OpenBrace)
            .map_err(|_| self.trigger_error(ParserErr::MissingIfOpenBrace))?;

        let mut then_branch = None;

        if !self.is_at(TokenKind::CloseBrace) {
            if self.is_at(TokenKind::Var) {
                return Err(self.trigger_error(ParserErr::VarDeclInIf));
            }

            then_branch = Some(Box::new(self.parse_stmt()?));
        }

        self.expect_and_skip(TokenKind::CloseBrace)
            .map_err(|_| self.trigger_error(ParserErr::MissingIfCloseBrace))?;

        let mut else_branch: Option<Box<Stmt>> = None;

        if self.is_at(TokenKind::Else) {
            self.eat()?;
            self.skip_new_lines();
            self.is_not_at_brace_or_end_of(ParserErr::ElseWithCond)?;

            self.expect_and_skip(TokenKind::OpenBrace)
                .map_err(|_| self.trigger_error(ParserErr::MissingElseOpenBrace))?;

            match self.at().kind {
                TokenKind::CloseBrace => {
                    self.eat()?;
                }
                _ => {
                    else_branch = Some(Box::new(self.parse_stmt()?));

                    self.expect_and_skip(TokenKind::CloseBrace)
                        .map_err(|_| self.trigger_error(ParserErr::MissingElseCloseBrace))?;
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
        self.skip_new_lines();

        if !self.is_at(TokenKind::OpenBrace) {
            return Err(self.trigger_error(ParserErr::MissingWhileOpenBrace));
        }

        let body = Box::new(self.parse_stmt()?);

        Ok(Stmt::While(WhileStmt {
            condition,
            body,
            loc: self.get_loc(),
        }))
    }

    fn parse_for_stmt(&mut self) -> ParserStmtRes {
        self.eat()?;

        let var_name = self
            .expect(TokenKind::Identifier)
            .map_err(|_| self.trigger_error(ParserErr::MissingVarNameFor))?
            .value;

        let placeholder = VarDeclStmt {
            name: var_name,
            value: None,
            loc: self.get_loc(),
        };

        self.expect(TokenKind::In)
            .map_err(|_| self.trigger_error(ParserErr::MissingInFor))?;

        self.is_at_brace_or_end_of(ParserErr::MissingForRange)?;

        if self.is_at(TokenKind::DotDot) {
            return Err(self.trigger_error(ParserErr::MissingStartForRange));
        } else if self.is_at(TokenKind::Minus) {
            return Err(self.trigger_error(ParserErr::NegativeForRange));
        }

        let start = self
            .expect(TokenKind::Int)
            .map_err(|_| self.trigger_error(ParserErr::NonIntForRange))?
            .value
            .parse::<i64>()
            .map_err(|_| self.trigger_error(ParserErr::ParsingInt))?;

        let mut end = None;
        if self.is_at(TokenKind::DotDot) {
            self.eat()?;

            self.is_at_brace_or_end_of(ParserErr::MissingEndForRange)?;

            end = Some(
                self.expect(TokenKind::Int)
                    .map_err(|_| self.trigger_error(ParserErr::NonIntForRange))?
                    .value
                    .parse::<i64>()
                    .map_err(|_| self.trigger_error(ParserErr::ParsingInt))?,
            );

            if Some(start) > end {
                return Err(self.trigger_error(ParserErr::LesserEndForRange));
            }
        }

        self.skip_new_lines();
        if !self.is_at(TokenKind::OpenBrace) {
            return Err(self.trigger_error(ParserErr::MissingForOpenBrace));
        }

        let body = Box::new(self.parse_stmt()?);

        Ok(Stmt::For(ForStmt {
            placeholder,
            range: ForRange { start, end },
            body,
            loc: self.get_loc(),
        }))
    }

    fn parse_fn_decl_stmt(&mut self) -> ParserStmtRes {
        self.eat()?;

        Ok(Stmt::FnDecl(self.parse_fn_decl()?))
    }

    fn parse_fn_decl(&mut self) -> Result<FnDeclStmt, RevResParser> {
        self.enter_code_block(CodeBlock::FnDeclBeforeBody);

        let name = self
            .expect(TokenKind::Identifier)
            .map_err(|_| self.trigger_error(ParserErr::MissingFnName))?
            .value;

        self.expect(TokenKind::OpenParen)
            .map_err(|_| self.trigger_error(ParserErr::NoOpenParenAfterFnName))?;

        let mut params: Vec<EcoString> = vec![];
        if !self.is_at(TokenKind::CloseParen) {
            loop {
                if params.len() >= 255 {
                    return Err(self.trigger_error(ParserErr::MaxFnArgs));
                }

                self.skip_new_lines();

                params.push(
                    self.expect(TokenKind::Identifier)
                        .map_err(|_| self.trigger_error(ParserErr::WrongFnArgType))?
                        .value,
                );

                if self.is_at(TokenKind::Comma) {
                    let _ = self.eat();
                    self.skip_new_lines();

                    if self.is_at(TokenKind::CloseParen) {
                        break;
                    }
                } else if !self.is_at(TokenKind::CloseParen) {
                    return Err(self.trigger_error(ParserErr::MissingArgsComma));
                } else {
                    break;
                }
            }
        }

        self.eat()?;
        self.skip_new_lines();

        if !self.is_at(TokenKind::OpenBrace) {
            return Err(self.trigger_error(ParserErr::MissingFnOpenBrace));
        }

        self.eat()?;
        self.skip_new_lines();

        self.exit_code_block();
        self.enter_code_block(CodeBlock::FnDecl);

        let body = Rc::new(self.parse_block()?);

        self.exit_code_block();

        Ok(FnDeclStmt {
            name,
            params: Rc::new(params),
            body,
            loc: self.get_loc(),
        })
    }

    fn parse_return_stmt(&mut self) -> ParserStmtRes {
        self.eat()?;

        let mut value = None;
        if !self.is_at(TokenKind::NewLine) {
            value = Some(self.parse_expr()?);
        }

        Ok(Stmt::Return(ReturnStmt {
            value,
            loc: self.get_loc(),
        }))
    }

    fn parse_struct_stmt(&mut self) -> ParserStmtRes {
        self.eat()?;

        self.enter_code_block(CodeBlock::Struct);

        let name = self
            .expect(TokenKind::Identifier)
            .map_err(|_| self.trigger_error(ParserErr::MissingStructName))?
            .value;

        self.expect(TokenKind::OpenBrace)
            .map_err(|_| self.trigger_error(ParserErr::MissingStructOpenBrace))?;

        self.skip_new_lines();

        // Fields parsing
        let mut fields: Vec<StructMember> = vec![];
        while !self.is_at(TokenKind::CloseBrace)&& !self.eof() && self.is_at(TokenKind::Identifier)
        {
            let name = self.eat()?.value.clone();

            if self.is_at(TokenKind::OpenParen) {
                return Err(self.trigger_error(ParserErr::MissingFnKwForMethod))
            }

            let mut value: Option<Expr> = None;
            if self.is_at(TokenKind::Equal) {
                self.eat()?;
                value = Some(self.parse_primary()?);
            }

            fields.push(StructMember::new(name, false, value));

            self.skip_new_lines();
        }

        let mut methods: Vec<FnDeclStmt> = vec![];
        while !self.is_at(TokenKind::CloseBrace) && !self.eof() && self.is_at(TokenKind::Fn) {
            self.eat()?;
            methods.push(self.parse_fn_decl()?);

            self.skip_new_lines();
        }

        if self.is_at(TokenKind::Identifier) {
            return Err(self.trigger_error(ParserErr::FieldDeclAfterFn))
        }

        self.expect(TokenKind::CloseBrace)
            .map_err(|_| self.trigger_error(ParserErr::MissingStructCloseBrace))?;

        self.exit_code_block();

        Ok(Stmt::Struct(StructStmt {
            name,
            fields,
            methods,
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

        match self.at().kind {
            TokenKind::Equal => {
                self.eat()?;
                let value = Box::new(self.parse_assign()?);

                match assigne {
                    Expr::Identifier(e) => Ok(Expr::Assign(AssignExpr {
                        name: e.name.clone(),
                        value,
                        loc: self.get_loc(),
                    })),
                    Expr::Get(e) => Ok(Expr::Set(SetExpr {
                        object: e.object,
                        name: e.name,
                        value,
                        loc: self.get_loc(),
                    })),
                    _ => Err(self.trigger_error(ParserErr::InvalidAssignTarget)),
                }
            }
            _ => Ok(assigne),
        }
    }

    fn parse_or(&mut self) -> ParserExprRes {
        let mut left = self.parse_and()?;

        while self.is_at(TokenKind::Or) {
            self.eat()?;

            if self.is_at(TokenKind::OpenBrace)
                || self.is_at(TokenKind::Eof)
                || self.is_at(TokenKind::NewLine)
            {
                return Err(self.trigger_error(ParserErr::OrWithNoCond));
            }

            let right = self.parse_and()?;

            left = Expr::Logical(LogicalExpr {
                left: Box::new(left),
                operator: EcoString::from("or"),
                right: Box::new(right),
                loc: self.get_loc(),
            });
        }

        Ok(left)
    }

    fn parse_and(&mut self) -> ParserExprRes {
        let mut left = self.parse_equality()?;

        while self.is_at(TokenKind::And) {
            self.eat()?;

            if self.is_at(TokenKind::OpenBrace)
                || self.is_at(TokenKind::Eof)
                || self.is_at(TokenKind::NewLine)
            {
                return Err(self.trigger_error(ParserErr::AndWithNoCond));
            }

            let right = self.parse_equality()?;

            left = Expr::Logical(LogicalExpr {
                left: Box::new(left),
                operator: EcoString::from("and"),
                right: Box::new(right),
                loc: self.get_loc(),
            });
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
            let right = self.parse_unary()?;

            return Ok(Expr::Unary(UnaryExpr {
                operator,
                right: Box::new(right),
                loc: self.get_loc(),
            }));
        }

        self.parse_call()
    }

    fn parse_call(&mut self) -> ParserExprRes {
        let mut expr = self.parse_primary()?;

        self.enter_code_block(CodeBlock::FnCallBeforeParen);

        loop {
            match self.at().kind {
                TokenKind::OpenParen => {
                    self.eat()?;

                    expr = self.finish_call(expr)?;
                }
                TokenKind::Dot => {
                    self.eat()?;
                    let prop_name = self
                        .expect(TokenKind::Identifier)
                        .map_err(|_| self.trigger_error(ParserErr::MissingPropName))?
                        .value;

                    expr = Expr::Get(GetExpr {
                        object: Box::new(expr),
                        name: prop_name,
                        loc: self.get_loc(),
                    })
                }
                _ => break,
            }
        }

        self.exit_code_block();

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> ParserExprRes {
        self.enter_code_block(CodeBlock::FnCall);

        let mut args: Vec<Expr> = vec![];

        if !self.is_at(TokenKind::CloseParen) {
            loop {
                if args.len() >= 255 {
                    return Err(self.trigger_error(ParserErr::TooManyCallArgs));
                }

                self.skip_new_lines();
                args.push(self.parse_expr()?);

                if self.is_at(TokenKind::Comma) {
                    let _ = self.eat();
                    self.skip_new_lines();

                    if self.is_at(TokenKind::CloseParen) {
                        break;
                    }
                } else if !self.is_at(TokenKind::CloseParen) {
                    return Err(self.trigger_error(ParserErr::MissingArgsComma));
                } else {
                    break;
                }
            }
        }

        self.expect(TokenKind::CloseParen)
            .map_err(|_| self.trigger_error(ParserErr::MissingCallCloseParen))?;
        
        self.exit_code_block();

        Ok(Expr::Call(CallExpr {
            callee: Box::new(callee),
            args,
            loc: self.get_loc(),
        }))
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
            TokenKind::Float => self.parse_float_literal(),
            TokenKind::String => self.parse_str_literal(),
            TokenKind::OpenParen => self.parse_grouping(),
            TokenKind::SelfKw => Ok(Expr::Selff(SelfExpr {
                name: "self".into(),
                loc: self.get_loc(),
            })),
            TokenKind::NewLine => Err(self.trigger_error(ParserErr::UnexpectedEol)),
            tk => {
                match tk {
                    TokenKind::Star | TokenKind::Plus | TokenKind::Slash | TokenKind::Modulo => {
                        Err(self.trigger_error(ParserErr::MissingLhsInBinop))
                    }
                    _ => Err(self
                        .trigger_error(ParserErr::UnexpectedToken(self.prev().to_string()))),
                }
            }
        }
    }

    fn parse_int_literal(&mut self) -> ParserExprRes {
        let tk = self.prev();
        let value = tk
            .value
            .parse::<i64>()
            .map_err(|_| self.trigger_error(ParserErr::ParsingInt))?;

        Ok(Expr::IntLiteral(IntLiteralExpr {
            value,
            loc: self.get_loc(),
        }))
    }

    fn parse_float_literal(&mut self) -> ParserExprRes {
        let tk = self.prev();
        let value = tk
            .value
            .parse::<f64>()
            .map_err(|_| self.trigger_error(ParserErr::ParsingFloat))?;

        Ok(Expr::FloatLiteral(FloatLiteralExpr {
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
                    return Err(RevResult::new(
                        ParserErr::ParenNeverClosed,
                        Some(self.get_loc()),
                    ))
                }
                _ => return Err(e),
            },
        };

        self.expect(TokenKind::CloseParen)
            .map_err(|_| RevResult::new(ParserErr::ParenNeverClosed, Some(self.get_loc())))?;

        Ok(Expr::Grouping(GroupingExpr {
            expr: Box::new(expr),
            loc: self.get_loc(),
        }))
    }

    fn at(&self) -> &Token {
        self.tokens.get(self.current).unwrap()
    }

    fn eat(&mut self) -> Result<&Token, RevResParser> {
        if self.eof() {
            return Err(RevResult::new(
                ParserErr::UnexpectedEof,
                Some(self.get_loc()),
            ));
        }

        self.current += 1;
        Ok(self.prev())
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token, RevResParser> {
        let tk = self.eat()?;

        match tk.kind == kind {
            true => Ok(self.prev().clone()),
            false => Err(RevResult::new(
                ParserErr::ExpectedToken(format!("{:?}", kind), format!("{:?}", tk.kind)),
                Some(self.get_loc()),
            )),
        }
    }

    fn expect_and_skip(&mut self, kind: TokenKind) -> Result<(), RevResParser> {
        self.expect(kind)?;
        self.skip_new_lines();

        Ok(())
    }

    fn skip_expect_and_skip(&mut self, kind: TokenKind) -> Result<(), RevResParser> {
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

    fn is_at_brace_or_end_of(&mut self, err: ParserErr) -> Result<(), RevResParser> {
        if self.is_at(TokenKind::OpenBrace)
            || self.is_at(TokenKind::Eof)
            || self.is_at(TokenKind::NewLine)
        {
            return Err(self.trigger_error(err));
        }

        Ok(())
    }

    fn is_not_at_brace_or_end_of(&mut self, err: ParserErr) -> Result<(), RevResParser> {
        if !self.is_at(TokenKind::OpenBrace)
            || self.is_at(TokenKind::Eof)
            || self.is_at(TokenKind::NewLine)
        {
            return Err(self.trigger_error(err));
        }

        Ok(())
    }

    // We dont have to activate the synchro each time, if the error occured
    // because we ate a '\n' that wasn't supposed to be here, we are already
    // past the error, we are on the new line. No need to synchronize
    fn trigger_error(&mut self, err: ParserErr) -> RevResParser {
        let err_loc = self.get_loc();

        self.synchronize();

        RevResult::new(err, Some(err_loc))
    }

    // We are here in panic mode
    fn synchronize(&mut self) {
        // If the error occured because unexpected Eol, we are synchro
        // Ex: "var " -> expect on identifier but "NewLine" was eaten already
        // Only true outside of function call: "fn(a
        // #)".  Here, # is not valid but the tk before was a new line
        if self.prev().kind == TokenKind::NewLine
            && self.code_blocks.last() != Some(&CodeBlock::FnCall)
            && self.code_blocks.last() != Some(&CodeBlock::Struct)
        {
            return
        }

        // If we are in a block of code that is not the global one
        if self.code_blocks.len() > 1 {
            let (stop_token, inverse_token) = match self.code_blocks.last().unwrap() {
                CodeBlock::FnCall
                | CodeBlock::FnCallBeforeParen => (TokenKind::CloseParen, TokenKind::OpenParen),
                _ => (TokenKind::CloseBrace, TokenKind::OpenBrace)
            };

            let mut nested_blocks = 0;

            // In case we are beofre the fn body declaration, we didn't
            // went through the open brace, so we simulate it
            // When we will encounter the opening one, we'll be at 0 looking
            // for the real closing one
            if self.code_blocks.last() == Some(&CodeBlock::FnDeclBeforeBody)
                || self.code_blocks.last() == Some(&CodeBlock::FnCallBeforeParen) {
                nested_blocks -= 1;
            }

            // Temporary values needed due to how pattern matching works
            // https://stackoverflow.com/questions/28225958/why-is-this-match-pattern-unreachable-when-using-non-literal-patterns
            while !self.eof() {
                match &self.at().kind {
                    tk if tk == &inverse_token => {
                        nested_blocks += 1;
                    },
                    tk if tk == &stop_token => {
                        if nested_blocks == 0 {
                            let _ = self.eat();
                            self.exit_code_block();

                            // For function call, we have to layers of code block
                            if self.code_blocks.last() == Some(&CodeBlock::FnCallBeforeParen) {
                                self.exit_code_block();
                            }

                            return
                        } else {
                            nested_blocks -= 1;
                        }
                    },
                    _ => {}
                }

                let _ = self.eat();
            }
        } else {
            while !self.eof() {
                match self.at().kind {
                    TokenKind::NewLine => {
                        let _ = self.eat();
                        return
                    }
                    _ => {
                        let _ = self.eat();
                    }
                }
            }
        }
    }

    fn enter_code_block(&mut self, block_kind: CodeBlock) {
        self.code_blocks.push(block_kind);
    }

    fn exit_code_block(&mut self) {
        self.code_blocks.pop();
    }

    fn get_loc(&self) -> Loc {
        Loc::new(self.start_loc, self.at().loc.start)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{utils::*, ParserErr};
    use ecow::EcoString;
    use tools::results::Loc;

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
        assert_eq!(infos.get_float_values(), vec![&24., &54.678]);
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
        let left = infos.get_binop_values()[0].0.get_float_values()[0];
        let right = infos.get_binop_values()[0].2.unary[0].expr.get_int_values()[0];
        assert_eq!(left, &14f64);
        assert_eq!(infos.get_binop_values()[0].1, EcoString::from("+"));
        assert_eq!(right, &67);

        let left = infos.get_binop_values()[1].0.get_float_values()[0];
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
        let left_bis = left_binop.0.get_float_values()[0];
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

        assert_eq!(infos.unary[2].expr.get_float_values(), vec![&54.67]);
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
            infos.var_decl[1].1.as_ref().unwrap().get_float_values(),
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
        assert!(e[1] == &ParserErr::WrongRhsVarDecl, "{}", e[1]);
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
        assert_eq!(assign2_binop[0].2.get_float_values()[0], &6f64);

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
    print a
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

        let body = &while_stmt.body.block[0].expr.assign[0];
        assert_eq!(body.name, EcoString::from("a"));
        assert_eq!(body.expr.get_int_values()[0], &1);

        let print = &while_stmt.body.block[0].print[0];
        assert_eq!(print, &String::from("a"));

        // Errors
        let code = "
while {}
while a
}
";
        // 0
        let errs = lex_and_parse(code).err().unwrap();
        let e = errs.iter().map(|e| &e.err).collect::<Vec<&ParserErr>>();
        assert!(e[0] == &ParserErr::WhileWithNoCond);
        assert!(e[1] == &ParserErr::MissingWhileOpenBrace);
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
        assert_eq!(for_stmt.range, (5, None));

        let for_stmt = &infos.for_stmt[1];
        assert_eq!(for_stmt.placeholder, EcoString::from("foo_b4r"));
        assert_eq!(for_stmt.range, (5, Some(10)));

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

    #[test]
    fn call() {
        let code = "
foo()
foo(2+(4-1))
foo(a, b,
c,
)
";
        // 0
        let infos = get_stmt_nodes_infos(code);
        let call = &infos.expr.call[0];
        assert_eq!(call.callee.get_ident_values()[0], EcoString::from("foo"));

        // 1
        let call = &infos.expr.call[1];
        assert_eq!(call.callee.get_ident_values()[0], EcoString::from("foo"));
        let arg = &call.args[0].get_binop_values()[0];
        assert_eq!(arg.0.get_int_values()[0], &2);
        assert_eq!(arg.1, EcoString::from("+"));
        let arg1 = &arg.2.grouping[0].expr.get_binop_values()[0];
        assert_eq!(arg1.0.get_int_values()[0], &4);
        assert_eq!(arg1.1, EcoString::from("-"));
        assert_eq!(arg1.2.get_int_values()[0], &1);

        // 2
        let call = &infos.expr.call[2];
        assert_eq!(call.callee.get_ident_values()[0], EcoString::from("foo"));
        assert_eq!(call.args[0].get_ident_values()[0], EcoString::from("a"));
        assert_eq!(call.args[1].get_ident_values()[0], EcoString::from("b"));
        assert_eq!(call.args[2].get_ident_values()[0], EcoString::from("c"));

        // Errors
        let code = "
foo(a b)
";
        let errs = lex_and_parse(code).err().unwrap();
        let e = errs.iter().map(|e| &e.err).collect::<Vec<&ParserErr>>();
        assert!(e[0] == &ParserErr::MissingArgsComma);
    }

    #[test]
    fn fn_decl() {
        let code = "
fn add() {}
fn add(a, b,)
{
    print a
}
";
        // 0
        let infos = get_stmt_nodes_infos(code);
        let decl = &infos.fn_decl[0];
        assert_eq!(decl.name, EcoString::from("add"));
        assert!(decl.params.is_empty());
        assert!(decl.body.is_empty());

        // 1
        let decl = &infos.fn_decl[1];
        assert_eq!(decl.name, EcoString::from("add"));
        assert_eq!(
            decl.params,
            vec![EcoString::from("a"), EcoString::from("b")]
        );

        assert_eq!(decl.body[0].print[0], String::from("a"));

        // Errors
        let code = "
fn (a, b) {}
fn add a, b {}
fn add(a b) {}
fn add(1, a, b) { print a }
fn add(a, b) print a
";
        let errs = lex_and_parse(code).err().unwrap();
        let e = errs.iter().map(|e| &e.err).collect::<Vec<&ParserErr>>();
        assert!(e[0] == &ParserErr::MissingFnName);
        assert!(e[1] == &ParserErr::NoOpenParenAfterFnName);
        assert!(e[2] == &ParserErr::MissingArgsComma);
        assert!(e[3] == &ParserErr::WrongFnArgType);
        assert!(e[4] == &ParserErr::MissingFnOpenBrace);
    }

    #[test]
    fn return_stmt() {
        let code = "
return
return 4
";
        // 0
        let infos = get_stmt_nodes_infos(code);
        assert_eq!(&infos.return_stmt[0], &None);
        assert_eq!(
            &infos.return_stmt[1].as_ref().unwrap().get_int_values()[0],
            &&4
        );
    }
}

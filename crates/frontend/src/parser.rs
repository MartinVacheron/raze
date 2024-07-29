use std::rc::Rc;

use colored::*;
use ecow::EcoString;
use thiserror::Error;

use crate::ast::expr::{
    AssignExpr, BinaryExpr, CallExpr, Expr, FloatLiteralExpr, GetExpr, GroupingExpr,
    IdentifierExpr, IntLiteralExpr, IsExpr, LogicalExpr, SelfExpr, SetExpr, StrLiteralExpr,
    UnaryExpr,
};
use crate::ast::stmt::{
    BlockStmt, ExprStmt, FnDeclStmt, FnParam, ForRange, ForStmt, IfStmt, PrintStmt, ReturnStmt, Stmt, StructStmt, VarDeclStmt, WhileStmt
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

    #[error("parenthesis is never closed")]
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

    #[error("missing structure field's type")]
    StructFieldNoType,

    // Property
    #[error("missing property name after '.'")]
    MissingPropName,

    // Types
    #[error("type name expected after '{0}'")]
    ExpectedTypeName(String),

    #[error("missing '{0}' to declare type")]
    MissingTokenTypeDecl(String),

    #[error("right hand side of 'is' must be a type identifier")]
    NonIdentTypeInIs,

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
    FnDeclArgs,
    FnDeclBody,
    FnCallArgs,
    Struct,
    Block,
}

// ---------
//  Parsing
// ---------
#[derive(Default)]
pub struct Parser {
    tokens: Vec<Token>,
    start_loc: usize,
    current: usize,
    code_blocks: Vec<CodeBlock>,
}

impl Parser {
    pub fn parse(&mut self, tokens: Vec<Token>) -> Result<Vec<Stmt>, Vec<RevResParser>> {
        self.tokens = tokens;

        let mut stmts: Vec<Stmt> = vec![];
        let mut errors: Vec<RevResParser> = vec![];

        self.code_blocks.push(CodeBlock::Global);

        while !self.eof() {
            self.skip_new_lines();

            // We could have reached EOF while skipping new lines
            if self.eof() {
                break;
            }

            match self.parse_declarations() {
                Ok(stmt) => stmts.push(stmt),
                Err(e) => errors.push(e)
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(stmts)
    }

    fn parse_declarations(&mut self) -> ParserStmtRes {
        match self.at().kind {
            TokenKind::Var => self.parse_var_declaration_stmt(),
            _ => self.parse_stmt(),
        }
    }

    fn parse_var_declaration_stmt(&mut self) -> ParserStmtRes {
        self.eat()?;

        Ok(Stmt::VarDecl(self.parse_var_declaration()?))
    }

    fn parse_var_declaration(&mut self) -> Result<VarDeclStmt, RevResParser> {
        let name = self
            .expect_no_eat(TokenKind::Identifier)
            .map_err(|_| self.trigger_error_before_cur_len_one(ParserErr::VarDeclNoName))?;

        let typ = self.parse_type(TokenKind::Colon, ":")?;

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
                            return Err(
                                self.trigger_error(ParserErr::IncorrectVarDeclVal(e.to_string()))
                            )
                        }
                    },
                }
            }
            TokenKind::NewLine | TokenKind::Eof | TokenKind::CloseBrace => {}
            _ => return Err(self.trigger_error(ParserErr::WrongRhsVarDecl)),
        }

        // self.skip_new_lines();

        Ok(VarDeclStmt {
            name,
            value,
            typ,
            loc: self.get_loc(),
        })
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

        let open_brace = self.expect_and_skip(TokenKind::OpenBrace)?;

        let stmts = self.parse_block(open_brace)?;

        self.exit_code_block();

        Ok(Stmt::Block(BlockStmt {
            stmts,
            loc: self.get_loc(),
        }))
    }

    fn parse_block(&mut self, open_brace: Token) -> Result<Vec<Stmt>, RevResParser> {
        let mut stmts: Vec<Stmt> = vec![];

        while !self.is_at(TokenKind::CloseBrace) && !self.eof() {
            stmts.push(self.parse_declarations()?);
            self.skip_new_lines();
        }

        self.expect_and_skip(TokenKind::CloseBrace)
            .map_err(|_| self.trigger_error_with_loc(ParserErr::UnclosedBlock, open_brace.loc))?;

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

        let name = self
            .expect(TokenKind::Identifier)
            .map_err(|_| self.trigger_error(ParserErr::MissingVarNameFor))?;

        let placeholder = VarDeclStmt {
            name,
            value: None,
            typ: None,
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
            .expect(TokenKind::IntLit)
            .map_err(|_| self.trigger_error(ParserErr::NonIntForRange))?
            .value
            .parse::<i64>()
            .map_err(|_| self.trigger_error(ParserErr::ParsingInt))?;

        let mut end = None;
        if self.is_at(TokenKind::DotDot) {
            self.eat()?;

            self.is_at_brace_or_end_of(ParserErr::MissingEndForRange)?;

            end = Some(
                self.expect(TokenKind::IntLit)
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
        self.enter_code_block(CodeBlock::FnDecl);

        let name = self
            .expect_no_eat(TokenKind::Identifier)
            .map_err(|_| self.trigger_error_before_cur_len_one(ParserErr::MissingFnName))?;

        let open_paren = self
            .expect_no_eat_and_skip(TokenKind::OpenParen)
            .map_err(|_| self.trigger_error_before_cur_len_one(ParserErr::NoOpenParenAfterFnName))?;

        self.enter_code_block(CodeBlock::FnDeclArgs);

        let mut params: Vec<FnParam> = vec![];
        if !self.is_at(TokenKind::CloseParen) && !self.eof() {
            loop {
                if params.len() >= 255 {
                    return Err(self.trigger_error(ParserErr::MaxFnArgs));
                }

                let param_name = self.expect_no_eat_and_skip(TokenKind::Identifier)
                                    .map_err(|_| self.trigger_error(ParserErr::WrongFnArgType))?;

                let param_type = self.parse_type(TokenKind::Colon, ":")?;

                params.push(FnParam { name: param_name, typ: param_type });

                if self.is_at(TokenKind::Comma) {
                    let _ = self.eat();
                    self.skip_new_lines();

                    if self.is_at(TokenKind::CloseParen) {
                        break;
                    }
                } else if !self.is_at(TokenKind::CloseParen) {
                    if self.is_at(TokenKind::Identifier) {
                        return Err(self.trigger_error(ParserErr::MissingArgsComma));
                    }

                    return Err(
                        self.trigger_error_with_loc(ParserErr::ParenNeverClosed, open_paren.loc)
                    );
                } else {
                    break;
                }
            }
        }

        let close_paren = self.expect_and_skip(TokenKind::CloseParen).map_err(|_| {
            self.trigger_error_with_loc(ParserErr::ParenNeverClosed, open_paren.loc)
        })?;

        self.exit_code_block();

        let return_type = self.parse_type(TokenKind::SmallArrow, "->")?;

        let open_brace = self.expect_no_eat_and_skip(TokenKind::OpenBrace).map_err(|_| {
            self.trigger_error_with_loc(ParserErr::MissingFnOpenBrace, close_paren.loc)
        })?;

        self.enter_code_block(CodeBlock::FnDeclBody);

        let body = Rc::new(self.parse_block(open_brace)?);

        self.exit_code_block();  // Body
        self.exit_code_block();  // Fn decl

        Ok(FnDeclStmt {
            name,
            params: Rc::new(params),
            body,
            return_type,
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
            loc: self.get_loc_from_prev(),
        }))
    }

    fn parse_struct_stmt(&mut self) -> ParserStmtRes {
        self.eat()?;

        self.enter_code_block(CodeBlock::Struct);

        let name = self
            .expect(TokenKind::Identifier)
            .map_err(|_| self.trigger_error(ParserErr::MissingStructName))?;

        self.expect(TokenKind::OpenBrace)
            .map_err(|_| self.trigger_error(ParserErr::MissingStructOpenBrace))?;

        self.skip_new_lines();

        // Fields parsing
        let mut fields: Vec<VarDeclStmt> = vec![];
        while !self.is_at(TokenKind::CloseBrace) && !self.eof() && self.is_at(TokenKind::Identifier)
        {
            if self.next_is(TokenKind::OpenParen) {
                return Err(self.trigger_error(ParserErr::MissingFnKwForMethod));
            }

            let field = self.parse_var_declaration()?;

            if field.typ.is_none() {
                return Err(self
                    .trigger_error_with_loc(ParserErr::StructFieldNoType, field.name.loc.clone()));
            }

            fields.push(field);

            self.skip_new_lines();
        }

        let mut methods: Vec<FnDeclStmt> = vec![];
        while !self.is_at(TokenKind::CloseBrace) && !self.eof() && self.is_at(TokenKind::Fn) {
            self.eat()?;
            methods.push(self.parse_fn_decl()?);

            self.skip_new_lines();
        }

        if self.is_at(TokenKind::Identifier) {
            return Err(self.trigger_error(ParserErr::FieldDeclAfterFn));
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

    fn parse_type(&mut self, start_token: TokenKind, tk_str: &str) -> Result<Option<Token>, RevResParser> {
        let mut typ: Option<Token> = None;
        
        if self.is_at(start_token) {
            self.eat()?;

            if self.is_at_type() {
                typ = Some(self.eat()?.clone());
            } else {
                let err_loc = Loc::new_len_one_from_start(self.prev().loc.clone());

                return Err(self.trigger_error_with_loc(ParserErr::ExpectedTypeName(tk_str.into()), err_loc));
            }
        } else if self.is_at_type() {
            let err_loc = Loc::new_len_one_from_start(self.at().loc.clone());

            return Err(self.trigger_error_with_loc(ParserErr::MissingTokenTypeDecl(tk_str.into()), err_loc));
        }

        Ok(typ)
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

                match assigne {
                    Expr::Identifier(e) => Ok(Expr::Assign(AssignExpr {
                        name: e.name.clone(),
                        value: Box::new(self.parse_assign()?),
                        loc: self.get_loc_from_prev(),
                    })),
                    Expr::Get(e) => Ok(Expr::Set(SetExpr {
                        object: e.object,
                        name: e.name,
                        value: Box::new(self.parse_assign()?),
                        loc: self.get_loc_from_prev(),
                    })),
                    _ => Err(self
                        .trigger_error_with_loc(ParserErr::InvalidAssignTarget, assigne.get_loc())),
                }
            }
            _ => Ok(assigne),
        }
    }

    fn parse_or(&mut self) -> ParserExprRes {
        let mut left = self.parse_and()?;

        while self.is_at(TokenKind::Or) {
            let op = self.eat()?.clone();

            if self.is_at(TokenKind::OpenBrace)
                || self.is_at(TokenKind::Eof)
                || self.is_at(TokenKind::NewLine)
            {
                return Err(self.trigger_error(ParserErr::OrWithNoCond));
            }

            let right = self.parse_and()?;

            left = Expr::Logical(LogicalExpr {
                left: Box::new(left),
                operator: op,
                right: Box::new(right),
                loc: self.get_loc(),
            });
        }

        Ok(left)
    }

    fn parse_and(&mut self) -> ParserExprRes {
        let mut left = self.parse_equality()?;

        while self.is_at(TokenKind::And) {
            let op = self.eat()?.clone();

            if self.is_at(TokenKind::OpenBrace)
                || self.is_at(TokenKind::Eof)
                || self.is_at(TokenKind::NewLine)
            {
                return Err(self.trigger_error(ParserErr::AndWithNoCond));
            }

            let right = self.parse_equality()?;

            left = Expr::Logical(LogicalExpr {
                left: Box::new(left),
                operator: op,
                right: Box::new(right),
                loc: self.get_loc(),
            });
        }

        Ok(left)
    }

    fn parse_equality(&mut self) -> ParserExprRes {
        let mut expr = self.parse_comparison()?;

        while self.is_at(TokenKind::EqualEqual) || self.is_at(TokenKind::BangEqual) {
            let operator = self.eat()?.clone();
            let right = self.parse_comparison()?;

            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> ParserExprRes {
        let mut expr = self.parse_is()?;

        while self.is_at(TokenKind::Less)
            || self.is_at(TokenKind::LessEqual)
            || self.is_at(TokenKind::Greater)
            || self.is_at(TokenKind::GreaterEqual)
        {
            let operator = self.eat()?.clone();
            let right = self.parse_term()?;

            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn parse_is(&mut self) -> ParserExprRes {
        let mut expr = self.parse_term()?;

        if self.is_at(TokenKind::Is) {
            self.eat()?;

            if !self.is_at_type() {
                return Err(self.trigger_error(ParserErr::NonIdentTypeInIs));
            }

            let typ = self.eat()?.clone();

            expr = Expr::Is(IsExpr {
                left: Box::new(expr),
                typ,
                loc: self.get_loc(),
            });
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> ParserExprRes {
        let mut expr = self.parse_factor()?;

        while self.is_at(TokenKind::Minus) || self.is_at(TokenKind::Plus) {
            let operator = self.eat()?.clone();
            let right = self.parse_factor()?;

            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
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
            let operator = self.eat()?.clone();
            let right = self.parse_unary()?;

            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> ParserExprRes {
        if self.is_at(TokenKind::Bang) || self.is_at(TokenKind::Minus) {
            let operator = self.eat()?.clone();
            let right = self.parse_unary()?;

            return Ok(Expr::Unary(UnaryExpr {
                operator,
                right: Box::new(right),
            }));
        }

        self.parse_call()
    }

    fn parse_call(&mut self) -> ParserExprRes {
        let mut expr = self.parse_primary()?;

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
                        loc: self.prev().loc.clone(),
                    })
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> ParserExprRes {
        self.enter_code_block(CodeBlock::FnCallArgs);

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

        // Before eating close paren
        let loc = self.get_loc_from_prev();

        self.expect(TokenKind::CloseParen)
            .map_err(|_| self.trigger_error(ParserErr::MissingCallCloseParen))?;

        self.exit_code_block();

        Ok(Expr::Call(CallExpr {
            callee: Box::new(callee),
            args,
            loc,
        }))
    }

    fn parse_primary(&mut self) -> ParserExprRes {
        match &self.at().kind {
            TokenKind::Identifier
            | TokenKind::True
            | TokenKind::False
            | TokenKind::IntType
            | TokenKind::FloatType
            | TokenKind::StringType
            | TokenKind::BoolType
            | TokenKind::Null => Ok(Expr::Identifier(IdentifierExpr {
                name: self.eat()?.value.clone(),
                loc: self.prev().loc.clone(),
            })),
            TokenKind::IntLit => self.parse_int_literal(),
            TokenKind::FloatLit => self.parse_float_literal(),
            TokenKind::StringLit => self.parse_str_literal(),
            TokenKind::OpenParen => self.parse_grouping(),
            TokenKind::SelfKw => Ok(Expr::Selff(SelfExpr {
                name: self.eat()?.value.clone(),
                loc: self.get_loc_from_prev(),
            })),
            TokenKind::NewLine => Err(self.trigger_error(ParserErr::UnexpectedEol)),
            tk => match tk {
                TokenKind::Star | TokenKind::Plus | TokenKind::Slash | TokenKind::Modulo => {
                    Err(self.trigger_error(ParserErr::MissingLhsInBinop))
                }
                _ => Err(self.trigger_error(ParserErr::UnexpectedToken(self.prev().to_string()))),
            },
        }
    }

    fn parse_int_literal(&mut self) -> ParserExprRes {
        let tk = self.eat()?.clone();
        let value = tk
            .value
            .parse::<i64>()
            .map_err(|_| self.trigger_error(ParserErr::ParsingInt))?;

        Ok(Expr::IntLiteral(IntLiteralExpr { value, loc: tk.loc }))
    }

    fn parse_float_literal(&mut self) -> ParserExprRes {
        let tk = self.eat()?.clone();
        let value = tk
            .value
            .parse::<f64>()
            .map_err(|_| self.trigger_error(ParserErr::ParsingFloat))?;

        Ok(Expr::FloatLiteral(FloatLiteralExpr { value, loc: tk.loc }))
    }

    fn parse_str_literal(&mut self) -> ParserExprRes {
        let tk = self.eat()?;

        Ok(Expr::StrLiteral(StrLiteralExpr {
            value: tk.value.clone(),
            loc: tk.loc.clone(),
        }))
    }

    fn parse_grouping(&mut self) -> ParserExprRes {
        self.eat()?;
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

        self.expect(TokenKind::CloseParen).map_err(|_| {
            let err_loc = Loc::new_len_one_from_start(expr.get_loc());
            self.trigger_error_with_loc(ParserErr::ParenNeverClosed, err_loc)
        })?;

        Ok(Expr::Grouping(GroupingExpr {
            expr: Box::new(expr),
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

    fn expect_no_eat(&mut self, kind: TokenKind) -> Result<Token, RevResParser> {
        let tk = self.at();

        if tk.kind == kind {
            Ok(self.eat()?.clone())
        } else {
            Err(RevResult::new(
                ParserErr::ExpectedToken(format!("{:?}", kind), format!("{:?}", tk.kind)),
                Some(self.get_loc()),
            ))
        }
    }

    fn expect_and_skip(&mut self, kind: TokenKind) -> Result<Token, RevResParser> {
        let res = self.expect(kind)?;
        self.skip_new_lines();

        Ok(res)
    }

    fn expect_no_eat_and_skip(&mut self, kind: TokenKind) -> Result<Token, RevResParser> {
        let res = self.expect_no_eat(kind)?;
        self.skip_new_lines();

        Ok(res)
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

    fn is_at_type(&self) -> bool {
        matches!(
            self.at().kind,
            TokenKind::Identifier
            | TokenKind::IntType
            | TokenKind::FloatType
            | TokenKind::StringType
            | TokenKind::BoolType
            | TokenKind::Null
            | TokenKind::AnyType
            | TokenKind::VoidType
        )
    }

    fn prev(&self) -> &Token {
        self.tokens.get(self.current - 1).unwrap()
    }

    fn next_is(&self, kind: TokenKind) -> bool {
        let next = self.tokens.get(self.current + 1);

        match next {
            Some(tk) => tk.kind == kind,
            None => false,
        }
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

        self.trigger_error_with_loc(err, err_loc)
    }

    fn trigger_error_with_loc(&mut self, err: ParserErr, loc: Loc) -> RevResParser {
        self.synchronize();

        RevResult::new(err, Some(loc))
    }

    // Triggers before the beginning of prev token, ex:
    // var = 8
    //    ^
    fn trigger_error_before_cur_len_one(&mut self, err: ParserErr) -> RevResParser {
        let prev_start = self.at().loc.start;
        let prev_loc = Loc::new(prev_start - 1, prev_start - 1);

        self.trigger_error_with_loc(err, prev_loc)
    }

    // We are here in panic mode
    fn synchronize(&mut self) {
        if self.code_blocks.last() == Some(&CodeBlock::Global) {
            while self.at().kind != TokenKind::NewLine && !self.eof() {
                let _ = self.eat();
            }

            return
        }

        while self.code_blocks.last() != Some(&CodeBlock::Global) {
            match self.code_blocks.last() {
                Some(&CodeBlock::FnDecl) => {
                    while !self.eof() {
                        match self.at().kind {
                            TokenKind::NewLine => {
                                self.skip_new_lines();
                            }
                            TokenKind::OpenParen
                            | TokenKind::OpenBrace => {
                                let _ = self.eat();
                            }
                            TokenKind::CloseParen => {
                                let _ = self.eat();
                                self.skip_new_lines();

                                if self.at().kind == TokenKind::OpenBrace {
                                    let _ = self.eat();
                                } else {
                                    break
                                }
                            }
                            TokenKind::CloseBrace => break,
                            _ => { let _ = self.eat(); }
                        }
                    }

                    self.exit_code_block();
                }
                Some(&CodeBlock::FnDeclArgs) => {
                    while !self.eof() {
                        match self.at().kind {
                            TokenKind::NewLine => {
                                self.skip_new_lines();
                            }
                            TokenKind::CloseParen => {
                                let _ = self.eat();
                                
                                self.skip_new_lines();
    
                                if self.at().kind == TokenKind::OpenBrace {
                                    let _ = self.eat();
                                } else {
                                    break
                                }
                            },
                            TokenKind::CloseBrace => break,
                            _ => { let _ = self.eat(); }
                                
                        }
                    }

                    self.exit_code_block();
                }
                Some(&CodeBlock::FnDeclBody) => {
                    let mut nb_blocks = 0;

                    while !self.eof() {
                        match self.at().kind {
                            TokenKind::CloseBrace => {
                                let _ = self.eat();

                                if nb_blocks == 0 {
                                    self.skip_new_lines();

                                    break
                                } else {
                                    nb_blocks -= 1;
                                }
                            },
                            TokenKind::OpenBrace => { 
                                let _ = self.eat();
                                nb_blocks += 1;
                            }
                            _ => { let _ = self.eat(); }
                        }
                    }

                    self.exit_code_block();
                }
                Some(&CodeBlock::FnCallArgs) => {
                    let mut nb_parens = 0;

                    while !self.eof() {
                        let _ = self.eat();
    
                        match self.prev().kind {
                            TokenKind::CloseParen => {
                                if nb_parens == 0 {
                                    self.skip_new_lines();
        
                                    break
                                } else {
                                    nb_parens -= 1;
                                }
                            },
                            TokenKind::OpenParen => { nb_parens += 1; }
                            _ => {}
                        }
                    }
                    
                    self.exit_code_block();
                }
                Some(&CodeBlock::Struct) => {
                    let mut nb_blocks = 0;

                    while !self.eof() {
                        let _ = self.eat();
                        
                        match self.prev().kind {
                            TokenKind::CloseBrace => {
                                if nb_blocks == 0 {
                                    self.skip_new_lines();
        
                                    break
                                } else {
                                    nb_blocks -= 1;
                                }
                            },
                            TokenKind::OpenBrace => { nb_blocks += 1; }
                            _ => {}
                        }
                    }

                    self.exit_code_block();
                }
                Some(&CodeBlock::Block) => {
                    let mut nb_blocks = 0;

                    while !self.eof() {
                        let _ = self.eat();
                        
                        match self.prev().kind {
                            TokenKind::CloseBrace => {
                                if nb_blocks == 0 {
                                    self.exit_code_block();
                                    self.skip_new_lines();
        
                                    break
                                } else {
                                    nb_blocks -= 1;
                                }
                            },
                            TokenKind::OpenBrace => { nb_blocks += 1; }
                            _ => {}
                        }
                    }
                }
                _ => {}
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

    fn get_loc_from_prev(&self) -> Loc {
        Loc::new(self.start_loc, self.prev().loc.end)
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

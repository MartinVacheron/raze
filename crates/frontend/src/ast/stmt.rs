use std::rc::Rc;

use ecow::EcoString;

use crate::lexer::Token;

use super::expr::Expr;
use tools::results::{Loc, RevReport, RevResult};

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Expr(ExprStmt),
    Print(PrintStmt),
    VarDecl(VarDeclStmt),
    Block(BlockStmt),
    If(IfStmt),
    While(WhileStmt),
    For(ForStmt),
    FnDecl(FnDeclStmt),
    Return(ReturnStmt),
    Struct(StructStmt),
}

#[derive(Debug, PartialEq)]
pub struct ExprStmt {
    pub expr: Expr,
    pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct PrintStmt {
    pub expr: Expr,
    pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
    pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct VarDeclStmt {
    pub name: Token,
    pub value: Option<Expr>,
    pub typ: Option<Token>,
    pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Option<Box<Stmt>>,
    pub else_branch: Option<Box<Stmt>>,
    pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Box<Stmt>,
    pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct ForStmt {
    pub placeholder: VarDeclStmt,
    pub range: ForRange,
    pub body: Box<Stmt>,
    pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct ForRange {
    pub start: i64,
    pub end: Option<i64>,
}

#[derive(Debug, PartialEq)]
pub struct FnDeclStmt {
    pub name: Token,
    pub params: Rc<Vec<EcoString>>,
    pub body: Rc<Vec<Stmt>>,
    pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
    pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct StructStmt {
    pub name: Token,
    pub fields: Vec<VarDeclStmt>,
    pub methods: Vec<FnDeclStmt>,
    pub loc: Loc,
}


impl Stmt {
    pub fn accept<T, U: RevReport>(
        &self,
        visitor: &mut impl VisitStmt<T, U>,
    ) -> Result<T, RevResult<U>> {
        match self {
            Stmt::Expr(stmt) => visitor.visit_expr_stmt(stmt),
            Stmt::Print(stmt) => visitor.visit_print_stmt(stmt),
            Stmt::VarDecl(stmt) => visitor.visit_var_decl_stmt(stmt),
            Stmt::Block(stmt) => visitor.visit_block_stmt(stmt),
            Stmt::If(stmt) => visitor.visit_if_stmt(stmt),
            Stmt::While(stmt) => visitor.visit_while_stmt(stmt),
            Stmt::For(stmt) => visitor.visit_for_stmt(stmt),
            Stmt::FnDecl(stmt) => visitor.visit_fn_decl_stmt(stmt),
            Stmt::Return(stmt) => visitor.visit_return_stmt(stmt),
            Stmt::Struct(stmt) => visitor.visit_struct_stmt(stmt),
        }
    }
}

pub trait VisitStmt<T, U: RevReport> {
    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> Result<T, RevResult<U>>;
    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> Result<T, RevResult<U>>;
    fn visit_var_decl_stmt(&mut self, stmt: &VarDeclStmt) -> Result<T, RevResult<U>>;
    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> Result<T, RevResult<U>>;
    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> Result<T, RevResult<U>>;
    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> Result<T, RevResult<U>>;
    fn visit_for_stmt(&mut self, stmt: &ForStmt) -> Result<T, RevResult<U>>;
    fn visit_fn_decl_stmt(&mut self, stmt: &FnDeclStmt) -> Result<T, RevResult<U>>;
    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> Result<T, RevResult<U>>;
    fn visit_struct_stmt(&mut self, stmt: &StructStmt) -> Result<T, RevResult<U>>;
}

// Into
impl From<&VarDeclStmt> for Stmt {
    fn from(value: &VarDeclStmt) -> Self {
        Self::VarDecl(VarDeclStmt {
            name: value.name.clone(),
            value: value.value.clone(),
            typ: value.typ.clone(),
            loc: value.loc.clone(),
        })
    }
}

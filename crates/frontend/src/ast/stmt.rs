use std::sync::Arc;

use crate::lexer::Token;

use super::expr::Expr;
use tools::results::{Loc, RevReport, RevResult};


#[derive(Debug, PartialEq, Clone)]
pub enum VarTypeDecl {
    Identifier(Token),
    Fn {
        fn_tk: Token,  // For error reporting
        param_types: Vec<Token>,
        return_type: Option<Box<VarTypeDecl>>
    }
}

impl VarTypeDecl {
    pub fn get_loc(&self) -> Loc {
        match self {
            VarTypeDecl::Identifier(tk) => tk.loc.clone(),
            VarTypeDecl::Fn { fn_tk, param_types, return_type } => {
                let end = return_type
                    .as_ref()
                    .map(|r| r.get_loc().end)
                    .unwrap_or_else(|| param_types
                        .last()
                        .map_or(fn_tk.loc.end, |p| p.loc.end));

                Loc::new(fn_tk.loc.start, end)
            }
        }
    }
}


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
}

#[derive(Debug, PartialEq)]
pub struct VarDeclStmt {
    pub name: Token,
    pub value: Option<Expr>,
    pub typ: Option<VarTypeDecl>,
    pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Option<BlockStmt>,
    pub else_branch: Option<BlockStmt>,
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
    pub params: Arc<Vec<FnParam>>,
    pub body: Arc<BlockStmt>,
    pub return_type: Option<VarTypeDecl>,
    pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct FnParam {
    pub name: Token,
    pub typ: VarTypeDecl,
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
            Stmt::Block(stmt) => stmt.accept(visitor),
            Stmt::If(stmt) => visitor.visit_if_stmt(stmt),
            Stmt::While(stmt) => visitor.visit_while_stmt(stmt),
            Stmt::For(stmt) => visitor.visit_for_stmt(stmt),
            Stmt::FnDecl(stmt) => visitor.visit_fn_decl_stmt(stmt),
            Stmt::Return(stmt) => visitor.visit_return_stmt(stmt),
            Stmt::Struct(stmt) => visitor.visit_struct_stmt(stmt),
        }
    }
}

impl BlockStmt {
    pub fn accept<T, U: RevReport>(
        &self,
        visitor: &mut impl VisitStmt<T, U>,
    ) -> Result<T, RevResult<U>> {
        visitor.visit_block_stmt(self)
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
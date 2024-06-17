use std::{cell::RefCell, rc::Rc};

use ecow::EcoString;

use crate::{
    environment::Env, expr::Expr, lexer::Loc, results::{PhyReport, PhyResult}
};

#[derive(Debug)]
pub enum Stmt {
    Expr(ExprStmt),
    Print(PrintStmt),
    VarDecl(VarDeclStmt),
    Block(BlockStmt),
}

#[derive(Debug)]
pub struct ExprStmt {
    pub expr: Expr,
    pub loc: Loc,
}

#[derive(Debug)]
pub struct PrintStmt {
    pub expr: Expr,
    pub loc: Loc,
}

#[derive(Debug)]
pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
    pub loc: Loc,
}

#[derive(Debug)]
pub struct VarDeclStmt {
    pub name: EcoString,
    pub value: Option<Expr>,
    pub loc: Loc,
}

impl Stmt {
	pub fn accept<T, U: PhyReport>(& self, visitor: &dyn VisitStmt<T, U>, env: Rc<RefCell<Env>>) -> Result<T, PhyResult<U>> {
        match self {
            Stmt::Expr(stmt) => visitor.visit_expr_stmt(stmt, env),
            Stmt::Print(stmt) => visitor.visit_print_stmt(stmt, env),
            Stmt::VarDecl(stmt) => visitor.visit_var_decl_stmt(stmt, env),
            Stmt::Block(stmt) => visitor.visit_block_stmt(stmt, env),
		}
	}
}

pub trait VisitStmt<T, U: PhyReport> {
    fn visit_expr_stmt(&self, stmt: &ExprStmt, env: Rc<RefCell<Env>>) -> Result<T, PhyResult<U>>;
    fn visit_print_stmt(&self, stmt: &PrintStmt, env: Rc<RefCell<Env>>) -> Result<T, PhyResult<U>>;
    fn visit_var_decl_stmt(&self, stmt: &VarDeclStmt, env: Rc<RefCell<Env>>) -> Result<T, PhyResult<U>>;
    fn visit_block_stmt(&self, stmt: &BlockStmt, env: Rc<RefCell<Env>>) -> Result<T, PhyResult<U>>;
}

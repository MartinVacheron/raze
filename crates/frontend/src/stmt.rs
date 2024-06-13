use ecow::EcoString;

use crate::{
    expr::Expr, lexer::Loc, results::{PhyReport, PhyResult}
};

pub enum Stmt {
    Expr(ExprStmt),
    Print(PrintStmt),
    VarDecl(VarDeclStmt),
}

pub struct ExprStmt {
    pub expr: Expr,
    pub loc: Loc,
}

pub struct PrintStmt {
    pub expr: Expr,
    pub loc: Loc,
}

pub struct VarDeclStmt {
    pub name: EcoString,
    pub value: Option<Expr>,
    pub loc: Loc,
}

impl Stmt {
	pub fn accept<T, U: PhyReport>(&self, visitor: &dyn VisitStmt<T, U>) -> Result<T, PhyResult<U>> {
		match self {
            Stmt::Expr(stmt) => visitor.visit_expr_stmt(stmt),
            Stmt::Print(stmt) => visitor.visit_print_stmt(stmt),
            Stmt::VarDecl(stmt) => visitor.visit_var_decl_stmt(stmt),
		}
	}
}

pub trait VisitStmt<T, U: PhyReport> {
    fn visit_expr_stmt(&self, stmt: &ExprStmt) -> Result<T, PhyResult<U>>;
    fn visit_print_stmt(&self, stmt: &PrintStmt) -> Result<T, PhyResult<U>>;
    fn visit_var_decl_stmt(&self, stmt: &VarDeclStmt) -> Result<T, PhyResult<U>>;
}

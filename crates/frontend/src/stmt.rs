use ecow::EcoString;

use crate::{
    expr::Expr, lexer::Loc, results::{PhyReport, PhyResult}
};

#[derive(Debug)]
pub enum Stmt {
    Expr(ExprStmt),
    Print(PrintStmt),
    VarDecl(VarDeclStmt),
    Block(BlockStmt),
    If(IfStmt),
    While(WhileStmt),
    For(ForStmt),
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

#[derive(Debug)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Option<Box<Stmt>>,
    pub else_branch: Option<Box<Stmt>>,
    pub loc: Loc,
}

#[derive(Debug)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Box<Stmt>,
    pub loc: Loc,
}

#[derive(Debug)]
pub struct ForStmt {
    pub placeholder: EcoString,
    pub range: ForRange,
    pub body: Option<Box<Stmt>>,
    pub loc: Loc,
}

#[derive(Debug)]
pub struct ForRange {
    pub start: i64,
    pub end: Option<i64>,
}

impl Stmt {
	pub fn accept<T, U: PhyReport>(&self, visitor: &dyn VisitStmt<T, U>) -> Result<T, PhyResult<U>> {
        match self {
            Stmt::Expr(stmt) => visitor.visit_expr_stmt(stmt),
            Stmt::Print(stmt) => visitor.visit_print_stmt(stmt),
            Stmt::VarDecl(stmt) => visitor.visit_var_decl_stmt(stmt),
            Stmt::Block(stmt) => visitor.visit_block_stmt(stmt),
            Stmt::If(stmt) => visitor.visit_if_stmt(stmt),
            Stmt::While(stmt) => visitor.visit_while_stmt(stmt),
            Stmt::For(stmt) => visitor.visit_for_stmt(stmt),
		}
	}
}

pub trait VisitStmt<T, U: PhyReport> {
    fn visit_expr_stmt(&self, stmt: &ExprStmt) -> Result<T, PhyResult<U>>;
    fn visit_print_stmt(&self, stmt: &PrintStmt) -> Result<T, PhyResult<U>>;
    fn visit_var_decl_stmt(&self, stmt: &VarDeclStmt) -> Result<T, PhyResult<U>>;
    fn visit_block_stmt(&self, stmt: &BlockStmt) -> Result<T, PhyResult<U>>;
    fn visit_if_stmt(&self, stmt: &IfStmt) -> Result<T, PhyResult<U>>;
    fn visit_while_stmt(&self, stmt: &WhileStmt) -> Result<T, PhyResult<U>>;
    fn visit_for_stmt(&self, stmt: &ForStmt) -> Result<T, PhyResult<U>>;
}

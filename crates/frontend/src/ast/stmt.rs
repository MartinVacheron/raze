use std::rc::Rc;

use ecow::EcoString;

use super::expr::Expr;
use tools::results::{Loc, PhyReport, PhyResult};

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
    pub name: EcoString,
    pub value: Option<Expr>,
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
    pub name: EcoString,
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
    pub name: EcoString,
    pub fields: Vec<StructMember>,
    pub methods: Vec<FnDeclStmt>,
    pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct StructMember {
    pub name: EcoString,
    pub is_const: bool,
    pub value: Option<Expr>,
}

impl StructMember {
    pub fn new(name: EcoString, is_const: bool, value: Option<Expr>) -> Self {
        Self { name, is_const, value }
    }
}


impl Stmt {
    pub fn accept<T, U: PhyReport>(
        &self,
        visitor: &mut impl VisitStmt<T, U>,
    ) -> Result<T, PhyResult<U>> {
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

pub trait VisitStmt<T, U: PhyReport> {
    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> Result<T, PhyResult<U>>;
    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> Result<T, PhyResult<U>>;
    fn visit_var_decl_stmt(&mut self, stmt: &VarDeclStmt) -> Result<T, PhyResult<U>>;
    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> Result<T, PhyResult<U>>;
    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> Result<T, PhyResult<U>>;
    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> Result<T, PhyResult<U>>;
    fn visit_for_stmt(&mut self, stmt: &ForStmt) -> Result<T, PhyResult<U>>;
    fn visit_fn_decl_stmt(&mut self, stmt: &FnDeclStmt) -> Result<T, PhyResult<U>>;
    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> Result<T, PhyResult<U>>;
    fn visit_struct_stmt(&mut self, stmt: &StructStmt) -> Result<T, PhyResult<U>>;
}

// Into
impl From<&VarDeclStmt> for Stmt {
    fn from(value: &VarDeclStmt) -> Self {
        Self::VarDecl(VarDeclStmt {
            name: value.name.clone(),
            value: value.value.clone(),
            loc: value.loc.clone(),
        })
    }
}

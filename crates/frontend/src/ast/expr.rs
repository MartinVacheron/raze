use std::fmt::Display;

use ecow::EcoString;
use tools::results::{Loc, PhyReport, PhyResult};

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Binary(BinaryExpr),
    Grouping(GroupingExpr),
    IntLiteral(IntLiteralExpr),
    RealLiteral(RealLiteralExpr),
    StrLiteral(StrLiteralExpr),
    Identifier(IdentifierExpr),
    Unary(UnaryExpr),
    Assign(AssignExpr),
    Logical(LogicalExpr),
    Call(CallExpr),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary(e) => write!(f, "{} {} {}", e.left, e.operator, e.right),
            Expr::Grouping(e) => write!(f, "{}", e.expr),
            Expr::IntLiteral(e) => write!(f, "{}", e.value),
            Expr::RealLiteral(e) => write!(f, "{}", e.value),
            Expr::StrLiteral(e) => write!(f, "{}", e.value),
            Expr::Identifier(e) => write!(f, "{}", e.name),
            Expr::Unary(e) => write!(f, "{} {}", e.operator, e.right),
            Expr::Assign(e) => write!(f, "{} {}", e.name, e.value),
            Expr::Logical(e) => write!(f, "{} {} {}", e.left, e.operator, e.right),
            Expr::Call(e) => write!(f, "{}: {:?}", e.callee, e.args),
        }
    }
}

impl Expr {
    pub fn get_loc(&self) -> Loc {
        match self {
            Self::Binary(b) => b.loc.clone(),
            Self::Grouping(g) => g.loc.clone(),
            Self::IntLiteral(i) => i.loc.clone(),
            Self::RealLiteral(r) => r.loc.clone(),
            Self::StrLiteral(s) => s.loc.clone(),
            Self::Identifier(i) => i.loc.clone(),
            Self::Unary(u) => u.loc.clone(),
            Self::Assign(a) => a.loc.clone(),
            Self::Logical(l) => l.loc.clone(),
            Self::Call(c) => c.loc.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: EcoString,
    pub right: Box<Expr>,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct GroupingExpr {
    pub expr: Box<Expr>,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntLiteralExpr {
    pub value: i64,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct RealLiteralExpr {
    pub value: f64,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StrLiteralExpr {
    pub value: EcoString,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IdentifierExpr {
    pub name: EcoString,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryExpr {
    pub operator: EcoString,
    pub right: Box<Expr>,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AssignExpr {
    pub name: EcoString,
    pub value: Box<Expr>,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LogicalExpr {
    pub left: Box<Expr>,
    pub operator: EcoString,
    pub right: Box<Expr>,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
    pub loc: Loc,
}

impl Expr {
    pub fn accept<T, U: PhyReport>(
        &self,
        visitor: &mut impl VisitExpr<T, U>,
    ) -> Result<T, PhyResult<U>> {
        match self {
            Expr::Binary(e) => visitor.visit_binary_expr(e),
            Expr::Grouping(e) => visitor.visit_grouping_expr(e),
            Expr::IntLiteral(e) => visitor.visit_int_literal_expr(e),
            Expr::RealLiteral(e) => visitor.visit_real_literal_expr(e),
            Expr::StrLiteral(e) => visitor.visit_str_literal_expr(e),
            Expr::Identifier(e) => visitor.visit_identifier_expr(e),
            Expr::Unary(e) => visitor.visit_unary_expr(e),
            Expr::Assign(e) => visitor.visit_assign_expr(e),
            Expr::Logical(l) => visitor.visit_logical_expr(l),
            Expr::Call(c) => visitor.visit_call_expr(c),
        }
    }
}

pub trait VisitExpr<T, U: PhyReport> {
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Result<T, PhyResult<U>>;
    fn visit_grouping_expr(&mut self, expr: &GroupingExpr) -> Result<T, PhyResult<U>>;
    fn visit_int_literal_expr(&mut self, expr: &IntLiteralExpr) -> Result<T, PhyResult<U>>;
    fn visit_real_literal_expr(&mut self, expr: &RealLiteralExpr) -> Result<T, PhyResult<U>>;
    fn visit_str_literal_expr(&mut self, expr: &StrLiteralExpr) -> Result<T, PhyResult<U>>;
    fn visit_identifier_expr(&mut self, expr: &IdentifierExpr) -> Result<T, PhyResult<U>>;
    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Result<T, PhyResult<U>>;
    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Result<T, PhyResult<U>>;
    fn visit_logical_expr(&mut self, expr: &LogicalExpr) -> Result<T, PhyResult<U>>;
    fn visit_call_expr(&mut self, expr: &CallExpr) -> Result<T, PhyResult<U>>;
}

// Into
impl From<&IdentifierExpr> for Expr {
    fn from(value: &IdentifierExpr) -> Self {
        Self::Identifier(IdentifierExpr {
            name: value.name.clone(),
            loc: value.loc.clone(),
        })
    }
}

impl From<&AssignExpr> for Expr {
    fn from(value: &AssignExpr) -> Self {
        Self::Assign(AssignExpr {
            name: value.name.clone(),
            value: value.value.clone(),
            loc: value.loc.clone(),
        })
    }
}

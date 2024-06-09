use std::{borrow::Borrow, fmt::Display};
use ecow::EcoString;
use crate::{lexer::Loc, results::PhyResult};


#[derive(Debug, PartialEq)]
pub enum Expr {
    Binary(BinaryExpr),
    Grouping(GroupingExpr),
    IntLiteral(IntLiteralExpr),
    RealLiteral(RealLiteralExpr),
    StrLiteral(StrLiteralExpr),
    Identifier(IdentifierExpr),
    Unary(UnaryExpr),
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
        }
    }
}

impl Expr {
    pub fn get_loc(&self) -> Loc {
        match self {
            Self::Binary(b) => b.borrow().loc.clone(),
            Self::Grouping(g) => g.borrow().loc.clone(),
            Self::IntLiteral(i) => i.borrow().loc.clone(),
            Self::RealLiteral(r) => r.borrow().loc.clone(),
            Self::StrLiteral(s) => s.borrow().loc.clone(),
            Self::Identifier(i) => i.borrow().loc.clone(),
            Self::Unary(u) => u.borrow().loc.clone(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: EcoString,
    pub right: Box<Expr>,
    pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct GroupingExpr {
    pub expr: Box<Expr>,
    pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct IntLiteralExpr {
    pub value: i64,
    pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct RealLiteralExpr {
    pub value: f64,
    pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct StrLiteralExpr {
    pub value: EcoString,
    pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct IdentifierExpr {
    pub name: EcoString,
    pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct UnaryExpr {
    pub operator: EcoString,
    pub right: Box<Expr>,
    pub loc: Loc,
}

impl Expr {
	pub fn accept<T>(&self, visitor: &dyn VisitExpr<T>) -> Result<T, PhyResult> {
		match self {
			Expr::Binary(e) => visitor.visit_binary_expr(e),
			Expr::Grouping(e) => visitor.visit_grouping_expr(e),
			Expr::IntLiteral(e) => visitor.visit_int_literal_expr(e),
			Expr::RealLiteral(e) => visitor.visit_real_literal_expr(e),
			Expr::StrLiteral(e) => visitor.visit_str_literal_expr(e),
			Expr::Identifier(e) => visitor.visit_identifier_expr(e),
			Expr::Unary(e) => visitor.visit_unary_expr(e),
		}
	}
}


pub trait VisitExpr<T> {
	fn visit_binary_expr(&self, expr: &BinaryExpr) -> Result<T, PhyResult>;
	fn visit_grouping_expr(&self, expr: &GroupingExpr) -> Result<T, PhyResult>;
	fn visit_int_literal_expr(&self, expr: &IntLiteralExpr) -> Result<T, PhyResult>;
	fn visit_real_literal_expr(&self, expr: &RealLiteralExpr) -> Result<T, PhyResult>;
	fn visit_str_literal_expr(&self, expr: &StrLiteralExpr) -> Result<T, PhyResult>;
	fn visit_identifier_expr(&self, expr: &IdentifierExpr) -> Result<T, PhyResult>;
	fn visit_unary_expr(&self, expr: &UnaryExpr) -> Result<T, PhyResult>;
}

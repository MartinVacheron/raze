use std::fmt::Display;
use ecow::EcoString;
use crate::lexer::Token;
use crate::results::ArcResult;


pub enum Expr {
    Binary(BinaryExpr),
    Grouping(GroupingExpr),
    Literal(LiteralExpr),
    Unary(UnaryExpr),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary(e) => write!(f, "{} {} {}", e.left, e.operator, e.right),
            Expr::Grouping(e) => write!(f, "{}", e.expr),
            Expr::Literal(e) => write!(f, "{}", e.value),
            Expr::Unary(e) => write!(f, "{} {}", e.operator, e.right),
        }
    }
}

pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

pub struct GroupingExpr {
    pub expr: Box<Expr>,
}

pub struct LiteralExpr {
    pub value: EcoString,
}

pub struct UnaryExpr {
    pub operator: Token,
    pub right: Box<Expr>,
}

impl Expr {
	pub fn accept<T>(&self, visitor: &dyn VisitExpr<T>) -> Result<T, ArcResult> {
		match self {
			Expr::Binary(e) => visitor.visit_binary_expr(e),
			Expr::Grouping(e) => visitor.visit_grouping_expr(e),
			Expr::Literal(e) => visitor.visit_literal_expr(e),
			Expr::Unary(e) => visitor.visit_unary_expr(e),
		}
	}
}


pub trait VisitExpr<T> {
	fn visit_binary_expr(&self, expr: &BinaryExpr) -> Result<T, ArcResult>;
	fn visit_grouping_expr(&self, expr: &GroupingExpr) -> Result<T, ArcResult>;
	fn visit_literal_expr(&self, expr: &LiteralExpr) -> Result<T, ArcResult>;
	fn visit_unary_expr(&self, expr: &UnaryExpr) -> Result<T, ArcResult>;
}

use std::rc::Rc;
use crate::lexer::Token;
use crate::results::ArcResult;



pub enum Expr {
	Binary(Rc<BinaryExpr>),
	Grouping(Rc<GroupingExpr>),
	Literal(Rc<LiteralExpr>),
	Unary(Rc<UnaryExpr>),
}

impl Expr {
	pub fn accept<T>(&self, visitor: &dyn VisitExpr<T>) -> Result<T, ArcResult> {
		match self {
			Expr::Binary(e) => visitor.visit_binary_expr(e.clone()),
			Expr::Grouping(e) => visitor.visit_grouping_expr(e.clone()),
			Expr::Literal(e) => visitor.visit_literal_expr(e.clone()),
			Expr::Unary(e) => visitor.visit_unary_expr(e.clone()),
		}
	}
}


pub struct BinaryExpr {
	left: Expr,
	operator: Token,
	right: Expr,
}

pub struct GroupingExpr {
	expression: Expr,
}

pub struct LiteralExpr {
	value: String,
}

pub struct UnaryExpr {
	operator: Token,
	right: Expr,
}


pub trait VisitExpr<T> {
	fn visit_binary_expr(&self, expr: Rc<BinaryExpr>) -> Result<T, ArcResult>;
	fn visit_grouping_expr(&self, expr: Rc<GroupingExpr>) -> Result<T, ArcResult>;
	fn visit_literal_expr(&self, expr: Rc<LiteralExpr>) -> Result<T, ArcResult>;
	fn visit_unary_expr(&self, expr: Rc<UnaryExpr>) -> Result<T, ArcResult>;
}

use std::rc::Rc;

use crate::expr::{
    BinaryExpr, GroupingExpr, IdentifierExpr, IntLiteralExpr, RealLiteralExpr, StrLiteralExpr, UnaryExpr, VisitExpr
};
use crate::values::Values;
use crate::results::RazeResult;

struct Interpreter {}

impl VisitExpr<Values> for Interpreter {
    fn visit_binary_expr(&self, expr: &BinaryExpr) -> Result<Values, RazeResult> {
        todo!()
    }

    fn visit_grouping_expr(&self, expr: &GroupingExpr) -> Result<Values, RazeResult> {
        todo!()
    }

    fn visit_int_literal_expr(&self, expr: &IntLiteralExpr) -> Result<Values, RazeResult> {
        Ok(expr.value.into())
    }

    fn visit_real_literal_expr(&self, expr: &RealLiteralExpr) -> Result<Values, RazeResult> {
        Ok(expr.value.into())
    }

    fn visit_str_literal_expr(&self, expr: &StrLiteralExpr) -> Result<Values, RazeResult> {
        Ok((&expr.value).into())
    }

    fn visit_identifier_expr(&self, expr: &IdentifierExpr) -> Result<Values, RazeResult> {
        todo!()
    }

    fn visit_unary_expr(&self, expr: &UnaryExpr) -> Result<Values, RazeResult> {
        todo!()
    }
}

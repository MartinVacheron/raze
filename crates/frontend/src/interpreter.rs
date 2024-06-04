use crate::expr::{
    BinaryExpr, GroupingExpr, IdentifierExpr, IntLiteralExpr, RealLiteralExpr, StrLiteralExpr, UnaryExpr, VisitExpr
};
use crate::values::Values;
use crate::results::PhyResult;

struct Interpreter {}

impl VisitExpr<Values> for Interpreter {
    fn visit_binary_expr(&self, expr: &BinaryExpr) -> Result<Values, PhyResult> {
        let lhs = expr.left.accept(self)?;
        let rhs = expr.right.accept(self)?;

        match lhs.operate(&rhs, &expr.operator) {
            Ok(res) => Ok(res),
            Err(e) => Err(PhyResult::interpreter_error(e.msg, expr.loc.clone()))
        }
    }

    fn visit_grouping_expr(&self, expr: &GroupingExpr) -> Result<Values, PhyResult> {
        expr.expr.accept(self)
    }

    fn visit_int_literal_expr(&self, expr: &IntLiteralExpr) -> Result<Values, PhyResult> {
        Ok(expr.value.into())
    }

    fn visit_real_literal_expr(&self, expr: &RealLiteralExpr) -> Result<Values, PhyResult> {
        Ok(expr.value.into())
    }

    fn visit_str_literal_expr(&self, expr: &StrLiteralExpr) -> Result<Values, PhyResult> {
        Ok(expr.value.clone().into())
    }

    fn visit_identifier_expr(&self, expr: &IdentifierExpr) -> Result<Values, PhyResult> {
        todo!()
    }

    fn visit_unary_expr(&self, expr: &UnaryExpr) -> Result<Values, PhyResult> {
        let value = expr.right.accept(self)?;

        match (&value, expr.operator.as_str()) {
            (Values::IntVal(..) | Values::RealVal(..), "!") => return Err(PhyResult::interpreter_error(
                "Can't use '!' token on anything other than a bool value".into(),
                expr.loc.clone(),
            )),
            (Values::BoolVal(..), "-") => return Err(PhyResult::interpreter_error(
                "Can't use '-' token on anything other than an int or a real value".into(),
                expr.loc.clone(),
            )),
            _ => {}
        }

        value.negate()?;

        Ok(value)
    }
}


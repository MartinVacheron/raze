use crate::expr::{
    BinaryExpr, Expr, GroupingExpr, IdentifierExpr, IntLiteralExpr, RealLiteralExpr, StrLiteralExpr, UnaryExpr, VisitExpr
};
use crate::values::RuntimeVal;
use crate::results::PhyResult;

pub struct Interpreter {}

impl Interpreter {
    pub fn interpret(&self, nodes: &Vec<Expr>) -> Result<(), PhyResult> {
        for node in nodes {
           match node.accept(self) {
                Ok(r) => println!("{}", r),
                Err(e) => return Err(PhyResult::runtime_error(e.msg, node.get_loc()))
            }
        }

        Ok(())
    }
}

impl VisitExpr<RuntimeVal> for Interpreter {
    fn visit_binary_expr(&self, expr: &BinaryExpr) -> Result<RuntimeVal, PhyResult> {
        let lhs = expr.left.accept(self)?;
        let rhs = expr.right.accept(self)?;

        match lhs.operate(&rhs, &expr.operator) {
            Ok(res) => Ok(res),
            Err(e) => Err(PhyResult::interpreter_error(e.msg, expr.loc.clone()))
        }
    }

    fn visit_grouping_expr(&self, expr: &GroupingExpr) -> Result<RuntimeVal, PhyResult> {
        expr.expr.accept(self)
    }

    fn visit_int_literal_expr(&self, expr: &IntLiteralExpr) -> Result<RuntimeVal, PhyResult> {
        Ok(expr.value.into())
    }

    fn visit_real_literal_expr(&self, expr: &RealLiteralExpr) -> Result<RuntimeVal, PhyResult> {
        Ok(expr.value.into())
    }

    fn visit_str_literal_expr(&self, expr: &StrLiteralExpr) -> Result<RuntimeVal, PhyResult> {
        Ok(expr.value.clone().into())
    }

    fn visit_identifier_expr(&self, expr: &IdentifierExpr) -> Result<RuntimeVal, PhyResult> {
        match expr.name.as_str() {
            "true" => Ok(true.into()),
            "false" => Ok(false.into()),
            "null" => Ok(RuntimeVal::new_null()),
            _ => todo!()
        }
    }

    fn visit_unary_expr(&self, expr: &UnaryExpr) -> Result<RuntimeVal, PhyResult> {
        let value = expr.right.accept(self)?;

        match (&value, expr.operator.as_str()) {
            (RuntimeVal::IntVal(..) | RuntimeVal::RealVal(..), "!") => return Err(PhyResult::interpreter_error(
                "Can't use '!' token on anything other than a bool value".into(),
                expr.loc.clone(),
            )),
            (RuntimeVal::BoolVal(..), "-") => return Err(PhyResult::interpreter_error(
                "Can't use '-' token on anything other than an int or a real value".into(),
                expr.loc.clone(),
            )),
            _ => {}
        }

        value.negate()?;

        Ok(value)
    }
}


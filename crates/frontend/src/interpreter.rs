use colored::Colorize;
use thiserror::Error;

use crate::expr::{
    BinaryExpr, Expr, GroupingExpr, IdentifierExpr, IntLiteralExpr, RealLiteralExpr,
    StrLiteralExpr, UnaryExpr, VisitExpr,
};
use crate::results::{PhyReport, PhyResult};
use crate::stmt::{ExprStmt, PrintStmt, Stmt, VarDeclStmt, VisitStmt};
use crate::values::RuntimeVal;

// ----------------
// Error managment
// ----------------
#[derive(Debug, Error, PartialEq)]
pub enum InterpErr {
    // Binop
    #[error("{0}")]
    OperationEvaluation(String),

    // Negate
    #[error("can't use '!' token on anything other than a bool value")]
    BangOpOnNonBool,

    #[error("can't use '-' token on anything other than an int or a real value")]
    NegateNonNumeric,

    #[error("{0}")]
    Negation(String),
}

impl PhyReport for InterpErr {
    fn get_err_msg(&self) -> String {
        format!("{} {}", "Interpreter error:".red(), self)
    }
}

pub(crate) type PhyResInterp = PhyResult<InterpErr>;

// --------------
//  Interpreting
// --------------
pub struct Interpreter {}

impl Interpreter {
    pub fn interpret(&self, nodes: &Vec<Stmt>) -> Result<RuntimeVal, PhyResInterp> {
        let mut res: RuntimeVal = RuntimeVal::Null;

        for node in nodes {
            match node.accept(self) {
                Ok(r) => res = r,
                Err(e) => return Err(e),
            }
        }

        Ok(res)
    }
}

impl VisitStmt<RuntimeVal, InterpErr> for Interpreter {
    fn visit_expr_stmt(&self, stmt: &ExprStmt) -> Result<RuntimeVal, PhyResult<InterpErr>> {
        Ok(stmt.expr.accept(self)?)
    }

    fn visit_print_stmt(&self, stmt: &PrintStmt) -> Result<RuntimeVal, PhyResult<InterpErr>> {
        Ok(stmt.expr.accept(self)?.into())
    }
     fn visit_var_decl_stmt(&self, stmt: &VarDeclStmt) -> Result<RuntimeVal, PhyResult<InterpErr>> {
         todo!()
     }
}

impl VisitExpr<RuntimeVal, InterpErr> for Interpreter {
    fn visit_binary_expr(&self, expr: &BinaryExpr) -> Result<RuntimeVal, PhyResInterp> {
        let lhs = expr.left.accept(self)?;
        let rhs = expr.right.accept(self)?;

        match lhs.operate(&rhs, &expr.operator) {
            Ok(res) => Ok(res),
            Err(e) => Err(PhyResult::new(
                InterpErr::OperationEvaluation(e.err.to_string()),
                Some(expr.loc.clone()),
            )),
        }
    }

    fn visit_grouping_expr(&self, expr: &GroupingExpr) -> Result<RuntimeVal, PhyResInterp> {
        expr.expr.accept(self)
    }

    fn visit_int_literal_expr(&self, expr: &IntLiteralExpr) -> Result<RuntimeVal, PhyResInterp> {
        Ok(expr.value.into())
    }

    fn visit_real_literal_expr(&self, expr: &RealLiteralExpr) -> Result<RuntimeVal, PhyResInterp> {
        Ok(expr.value.into())
    }

    fn visit_str_literal_expr(&self, expr: &StrLiteralExpr) -> Result<RuntimeVal, PhyResInterp> {
        Ok(expr.value.clone().into())
    }

    fn visit_identifier_expr(&self, expr: &IdentifierExpr) -> Result<RuntimeVal, PhyResInterp> {
        match expr.name.as_str() {
            "true" => Ok(true.into()),
            "false" => Ok(false.into()),
            "null" => Ok(RuntimeVal::new_null()),
            _ => todo!(),
        }
    }

    fn visit_unary_expr(&self, expr: &UnaryExpr) -> Result<RuntimeVal, PhyResInterp> {
        let value = expr.right.accept(self)?;

        match (&value, expr.operator.as_str()) {
            (RuntimeVal::IntVal(..) | RuntimeVal::RealVal(..), "!") => {
                return Err(PhyResult::new(
                    InterpErr::BangOpOnNonBool,
                    Some(expr.loc.clone()),
                ))
            }
            (
                RuntimeVal::BoolVal(..)
                | RuntimeVal::StrVal(..)
                | RuntimeVal::Null, "-") => {
                return Err(PhyResult::new(
                    InterpErr::NegateNonNumeric,
                    Some(expr.loc.clone()),
                ))
            }
            _ => {}
        }

        value.negate().map_err(|e| {
            PhyResult::new(
                InterpErr::Negation(e.err.to_string()),
                Some(expr.loc.clone()),
            )
        })?;

        Ok(value)
    }
}

#[cfg(test)]
mod tests {
    use ecow::EcoString;

    use crate::{interpreter::InterpErr, utils::lex_parse_interp};

    #[test]
    fn interp_literals() {
        let code = "1";
        assert_eq!(lex_parse_interp(code).unwrap(), 1.into());

        let code = "-45.";
        assert_eq!(lex_parse_interp(code).unwrap(), (-45f64).into());

        let code = "\"hello world!\"";
        assert_eq!(
            lex_parse_interp(code).unwrap(),
            EcoString::from("hello world!").into()
        );
    }

    #[test]
    fn interp_binop() {
        let code = "1 +2";
        assert_eq!(lex_parse_interp(code).unwrap(), 3.into());

        let code = "1. + -2 *24";
        assert_eq!(lex_parse_interp(code).unwrap(), (-47f64).into());

        let code = "5 + (6 * (2+3)) - (((6)))";
        assert_eq!(lex_parse_interp(code).unwrap(), 29.into());
    }

    #[test]
    fn interp_str_op() {
        let code = "\"foo\" * 4";
        assert_eq!(
            lex_parse_interp(code).unwrap(),
            EcoString::from("foofoofoofoo").into()
        );

        let code = "4 * \"foo\"";
        assert_eq!(
            lex_parse_interp(code).unwrap(),
            EcoString::from("foofoofoofoo").into()
        );

        let code = "\"foo\" + \" \" + \"bar\"";
        assert_eq!(
            lex_parse_interp(code).unwrap(),
            EcoString::from("foo bar").into()
        );

        // Errors
        let code = "\"foo\" * 3.5";
        matches!(
            lex_parse_interp(code).err().unwrap().err,
            InterpErr::OperationEvaluation {..}
        );

        let code = "\"foo\" + 56";
        matches!(
            lex_parse_interp(code).err().unwrap().err,
            InterpErr::OperationEvaluation {..}
        );
    }

    #[test]
    fn interp_negation() {
        let code = "-3";
        assert_eq!(lex_parse_interp(code).unwrap(), (-3).into());

        let code = "-3.";
        assert_eq!(lex_parse_interp(code).unwrap(), (-3f64).into());

        let code = "!true";
        assert_eq!(lex_parse_interp(code).unwrap(), false.into());

        let code = "!false";
        assert_eq!(lex_parse_interp(code).unwrap(), true.into());

        // Errors
        let code = "- \"foo\"";
        assert_eq!(
            lex_parse_interp(code).err().unwrap().err,
            InterpErr::NegateNonNumeric
        );

        let code = "!8";
        assert_eq!(
            lex_parse_interp(code).err().unwrap().err,
            InterpErr::BangOpOnNonBool
        );
    }
}

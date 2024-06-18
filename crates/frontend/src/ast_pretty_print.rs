use std::cell::RefCell;
use std::rc::Rc;

use crate::environment::{Env, EnvWrapper};
use crate::expr::{
    AssignExpr, BinaryExpr, GroupingExpr, IdentifierExpr, IntLiteralExpr, RealLiteralExpr, StrLiteralExpr, UnaryExpr, VisitExpr
};
use crate::results::PhyReport;
use crate::stmt::{BlockStmt, ExprStmt, IfStmt, PrintStmt, Stmt, VarDeclStmt, VisitStmt};
use crate::{expr::Expr, results::PhyResult};

#[derive(Debug)]
pub enum AstPrinterErr {}

type PhyResAstPrint = PhyResult<AstPrinterErr>;

impl PhyReport for AstPrinterErr {
    fn get_err_msg(&self) -> String {
        String::from("")
    }
}

pub struct AstPrinter {}

impl AstPrinter {
    pub fn print(&self, expr: &Stmt) -> Result<String, PhyResAstPrint> {
        expr.accept(self, Rc::new(RefCell::new(Env::new(None))))
    }

    fn parenthesize(&self, name: &str, exprs: &[&Expr]) -> Result<String, PhyResAstPrint> {
        let mut final_str: String = format!("({}", name);

        for expr in exprs {
            final_str.push(' ');
            final_str.push_str(expr.accept(self, Rc::new(RefCell::new(Env::default())))?.as_str());
        }

        final_str.push(')');

        Ok(final_str)
    }
}

impl VisitStmt<String, AstPrinterErr> for AstPrinter {
    fn visit_expr_stmt(&self, stmt: &ExprStmt, env: Rc<RefCell<Env>>) -> Result<String, PhyResult<AstPrinterErr>> {
        stmt.expr.accept(self, env)
    }

    fn visit_print_stmt(&self, stmt: &PrintStmt, _: Rc<RefCell<Env>>) -> Result<String, PhyResult<AstPrinterErr>> {
        self.parenthesize("print", &[&stmt.expr])
    }

    fn visit_var_decl_stmt(&self, stmt: &VarDeclStmt, _: Rc<RefCell<Env>>) -> Result<String, PhyResult<AstPrinterErr>> {
        let val_str = if let Some(v) = &stmt.value { format!("{}", v) } else { "None".to_string() };
        let decl_str = format!("decl {} = {}", stmt.name, val_str);
        self.parenthesize(&decl_str, &[])
    }

    fn visit_block_stmt(&self, _stmt: &BlockStmt, _: Rc<RefCell<Env>>) -> Result<String, PhyResult<AstPrinterErr>> {
        self.parenthesize("block", &[])
    }

    fn visit_if_stmt(&self, _stmt: &IfStmt, _env: Rc<RefCell<Env>>) -> Result<String, PhyResult<AstPrinterErr>> {
        todo!()
    }
}

impl VisitExpr<String, AstPrinterErr> for AstPrinter {
    fn visit_binary_expr(&self, expr: &BinaryExpr, _: EnvWrapper) -> Result<String, PhyResAstPrint> {
        self.parenthesize(expr.operator.as_str(), &[&expr.left, &expr.right])
    }

    fn visit_grouping_expr(&self, expr: &GroupingExpr, _: EnvWrapper) -> Result<String, PhyResAstPrint> {
        self.parenthesize("group", &[&expr.expr])
    }

    fn visit_int_literal_expr(&self, expr: &IntLiteralExpr, _: EnvWrapper) -> Result<String, PhyResAstPrint> {
        Ok(format!("{}", expr.value))
    }

    fn visit_real_literal_expr(&self, expr: &RealLiteralExpr, _: EnvWrapper) -> Result<String, PhyResAstPrint> {
        Ok(format!("{}", expr.value))
    }

    fn visit_str_literal_expr(&self, expr: &StrLiteralExpr, _: EnvWrapper) -> Result<String, PhyResAstPrint> {
        Ok(format!("\"{}\"", expr.value))
    }

    fn visit_identifier_expr(&self, expr: &IdentifierExpr, _: EnvWrapper) -> Result<String, PhyResAstPrint> {
        Ok(expr.name.to_string())
    }

    fn visit_unary_expr(&self, expr: &UnaryExpr, _: EnvWrapper) -> Result<String, PhyResAstPrint> {
        self.parenthesize(expr.operator.as_str(), &[&expr.right])
    }

    fn visit_assign_expr(&self, expr: &AssignExpr, env: EnvWrapper) -> Result<String, PhyResult<AstPrinterErr>> {
        let assign_str = format!("assign {} to {}", expr.value.accept(self, env)?, expr.name);
        self.parenthesize(assign_str.as_str(), &[])
    }
}

use tools::results::{RevReport, RevResult};

use super::expr::{
    AssignExpr, BinaryExpr, CallExpr, Expr, GroupingExpr, IdentifierExpr, IntLiteralExpr, LogicalExpr, FloatLiteralExpr, StrLiteralExpr, UnaryExpr, VisitExpr
};

use super::stmt::{BlockStmt, ExprStmt, FnDeclStmt, ForStmt, IfStmt, PrintStmt, ReturnStmt, Stmt, VarDeclStmt, VisitStmt, WhileStmt};

#[derive(Debug)]
pub enum AstPrinterErr {}

type RevResAstPrint = RevResult<AstPrinterErr>;

impl RevReport for AstPrinterErr {
    fn get_err_msg(&self) -> String {
        String::from("")
    }
}

pub struct AstPrinter {}

impl AstPrinter {
    pub fn print(&mut self, expr: &Stmt) -> Result<String, RevResAstPrint> {
        expr.accept(self)
    }

    fn parenthesize(&mut self, name: &str, exprs: &[&Expr]) -> Result<String, RevResAstPrint> {
        let mut final_str: String = format!("({}", name);

        for expr in exprs {
            final_str.push(' ');
            final_str.push_str(expr.accept(self)?.as_str());
        }

        final_str.push(')');

        Ok(final_str)
    }
}

impl VisitStmt<String, AstPrinterErr> for AstPrinter {
    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> Result<String, RevResult<AstPrinterErr>> {
        stmt.expr.accept(self)
    }

    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> Result<String, RevResult<AstPrinterErr>> {
        self.parenthesize("print", &[&stmt.expr])
    }

    fn visit_var_decl_stmt(&mut self, stmt: &VarDeclStmt) -> Result<String, RevResult<AstPrinterErr>> {
        let val_str = if let Some(v) = &stmt.value { format!("{}", v) } else { "None".to_string() };
        let decl_str = format!("decl {} = {}", stmt.name, val_str);
        self.parenthesize(&decl_str, &[])
    }

    fn visit_block_stmt(&mut self, _stmt: &BlockStmt) -> Result<String, RevResult<AstPrinterErr>> {
        self.parenthesize("block", &[])
    }

    fn visit_if_stmt(&mut self, _stmt: &IfStmt) -> Result<String, RevResult<AstPrinterErr>> {
        todo!()
    }

    fn visit_while_stmt(&mut self, _stmt: &WhileStmt) -> Result<String, RevResult<AstPrinterErr>> {
        todo!()
    }

    fn visit_for_stmt(&mut self, _stmt: &ForStmt) -> Result<String, RevResult<AstPrinterErr>> {
        todo!()
    }

    fn visit_fn_decl_stmt(&mut self, _stmt: &FnDeclStmt) -> Result<String, RevResult<AstPrinterErr>> {
        todo!()
    }

    fn visit_return_stmt(&mut self, _stmt: &ReturnStmt) -> Result<String, RevResult<AstPrinterErr>> {
        todo!()
    }
    
    fn visit_struct_stmt(&mut self, _stmt: &super::stmt::StructStmt) -> Result<String, RevResult<AstPrinterErr>> {
        todo!()
    }
}

impl VisitExpr<String, AstPrinterErr> for AstPrinter {
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Result<String, RevResAstPrint> {
        self.parenthesize(expr.operator.as_str(), &[&expr.left, &expr.right])
    }

    fn visit_grouping_expr(&mut self, expr: &GroupingExpr) -> Result<String, RevResAstPrint> {
        self.parenthesize("group", &[&expr.expr])
    }

    fn visit_int_literal_expr(&mut self, expr: &IntLiteralExpr) -> Result<String, RevResAstPrint> {
        Ok(format!("{}", expr.value))
    }

    fn visit_float_literal_expr(&mut self, expr: &FloatLiteralExpr) -> Result<String, RevResAstPrint> {
        Ok(format!("{}", expr.value))
    }

    fn visit_str_literal_expr(&mut self, expr: &StrLiteralExpr) -> Result<String, RevResAstPrint> {
        Ok(format!("\"{}\"", expr.value))
    }

    fn visit_identifier_expr(&mut self, expr: &IdentifierExpr) -> Result<String, RevResAstPrint> {
        Ok(expr.name.to_string())
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Result<String, RevResAstPrint> {
        self.parenthesize(expr.operator.as_str(), &[&expr.right])
    }

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Result<String, RevResult<AstPrinterErr>> {
        let assign_str = format!("assign {} to {}", expr.value.accept(self)?, expr.name);
        self.parenthesize(assign_str.as_str(), &[])
    }

    fn visit_logical_expr(&mut self, expr: &LogicalExpr) -> Result<String, RevResult<AstPrinterErr>> {
        self.parenthesize(expr.operator.as_str(), &[&expr.left, &expr.right])
    }

    fn visit_call_expr(&mut self, _: &CallExpr) -> Result<String, RevResult<AstPrinterErr>> {
        todo!()
    }
    
    fn visit_get_expr(&mut self, _: &super::expr::GetExpr) -> Result<String, RevResult<AstPrinterErr>> {
        todo!()
    }
    
    fn visit_set_expr(&mut self, _: &super::expr::SetExpr) -> Result<String, RevResult<AstPrinterErr>> {
        todo!()
    }
    
    fn visit_self_expr(&mut self, _: &super::expr::SelfExpr) -> Result<String, RevResult<AstPrinterErr>> {
        todo!()
    }
}

use std::{cell::RefCell, collections::HashMap};

use colored::Colorize;
use ecow::EcoString;
use thiserror::Error;
use tools::results::{PhyResult, PhyReport};


use crate::ast::{expr::{Expr, VisitExpr}, stmt::{BlockStmt, ExprStmt, FnDeclStmt, ForStmt, IfStmt, PrintStmt, ReturnStmt, Stmt, VarDeclStmt, VisitStmt, WhileStmt}};


#[derive(Error, Debug)]
enum ResolverErr {

}

impl PhyReport for ResolverErr {
    fn get_err_msg(&self) -> String {
        format!("{}: ", "Resolver error".red())
    }
}

type ResolverRes = Result<(), PhyResult<ResolverErr>>;

struct Resolver {
    scopes: RefCell<Vec<HashMap<EcoString, bool>>>,
}


// If we can’t find it in the stack of local scopes, we assume it must be global
impl Resolver {
    fn resolve(&self, stmts: &Vec<Stmt>) -> ResolverRes {
        for s in stmts {
            self.resolve_stmt(s)?
        }

        Ok(())
    }

    fn resolve_stmt(&self, stmt: &Stmt) -> ResolverRes {
        stmt.accept(self)
    }

    fn resolve_expr(&self, expr: &Expr) -> ResolverRes {
        expr.accept(self)
    }

    fn declare(&self, name: EcoString) {
        if self.scopes.borrow().is_empty() { return }
    }

    fn define(&self, name: EcoString) {

    }

    fn begin_scope(&self) {
        self.scopes.borrow_mut().push(HashMap::new());
    }

    fn end_scope(&self) {
        self.scopes.borrow_mut().pop();
    }
}


impl VisitStmt<(), ResolverErr> for Resolver {
    fn visit_expr_stmt(&self, stmt: &ExprStmt) -> ResolverRes {
        todo!()
    }

    fn visit_print_stmt(&self, stmt: &PrintStmt) -> ResolverRes {
        todo!()
    }

    fn visit_var_decl_stmt(&self, stmt: &VarDeclStmt) -> ResolverRes {
        self.declare(stmt.name.clone());

        if let Some(v) = &stmt.value {
            self.resolve_expr(v)?;
        }

        self.define(stmt.name.clone());

        Ok(())
    }

    fn visit_block_stmt(&self, stmt: &BlockStmt) -> ResolverRes {
        self.begin_scope();
        self.resolve(&stmt.stmts)
    }

    fn visit_if_stmt(&self, stmt: &IfStmt) -> ResolverRes {
        todo!()
    }

    fn visit_while_stmt(&self, stmt: &WhileStmt) -> ResolverRes {
        todo!()
    }

    fn visit_for_stmt(&self, stmt: &ForStmt) -> ResolverRes {
        todo!()
    }

    fn visit_fn_decl_stmt(&self, stmt: &FnDeclStmt) -> ResolverRes {
        todo!()
    }

    fn visit_return_stmt(&self, stmt: &ReturnStmt) -> ResolverRes {
        todo!()
    }
}

impl VisitExpr<(), ResolverErr> for Resolver {
    fn visit_binary_expr(&self, expr: &crate::ast::expr::BinaryExpr) -> ResolverRes {
        todo!()
    }

    fn visit_grouping_expr(&self, expr: &crate::ast::expr::GroupingExpr) -> ResolverRes {
        todo!()
    }

    fn visit_int_literal_expr(&self, expr: &crate::ast::expr::IntLiteralExpr) -> ResolverRes {
        todo!()
    }

    fn visit_real_literal_expr(&self, expr: &crate::ast::expr::RealLiteralExpr) -> ResolverRes {
        todo!()
    }

    fn visit_str_literal_expr(&self, expr: &crate::ast::expr::StrLiteralExpr) -> ResolverRes {
        todo!()
    }

    fn visit_identifier_expr(&self, expr: &crate::ast::expr::IdentifierExpr) -> ResolverRes {
        todo!()
    }

    fn visit_unary_expr(&self, expr: &crate::ast::expr::UnaryExpr) -> ResolverRes {
        todo!()
    }

    fn visit_assign_expr(&self, expr: &crate::ast::expr::AssignExpr) -> ResolverRes {
        todo!()
    }

    fn visit_logical_expr(&self, expr: &crate::ast::expr::LogicalExpr) -> ResolverRes {
        todo!()
    }

    fn visit_call_expr(&self, expr: &crate::ast::expr::CallExpr) -> ResolverRes {
        todo!()
    }
}
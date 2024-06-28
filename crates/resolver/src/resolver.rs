use std::collections::HashMap;

use colored::Colorize;
use ecow::EcoString;
use thiserror::Error;
use tools::results::{PhyReport, PhyResult};

use frontend::ast::{
    expr::{
        AssignExpr, BinaryExpr, CallExpr, Expr, GroupingExpr, IdentifierExpr, IntLiteralExpr,
        LogicalExpr, RealLiteralExpr, StrLiteralExpr, UnaryExpr, VisitExpr,
    },
    stmt::{
        BlockStmt, ExprStmt, FnDeclStmt, ForStmt, IfStmt, PrintStmt, ReturnStmt, Stmt, VarDeclStmt,
        VisitStmt, WhileStmt,
    },
};

#[derive(Error, Debug, PartialEq)]
pub enum ResolverErr {
    #[error("local variable initializer is shadoweding global variable")]
    LocalVarInOwnInit,
}

impl PhyReport for ResolverErr {
    fn get_err_msg(&self) -> String {
        format!("{}: ", "Resolver error".red())
    }
}

pub type ResolverRes = Result<(), PhyResult<ResolverErr>>;

// Bool is for tracking if the variable is initialized, avoiding weird cases
// where we initialize the variable with its shadowing global one
// We track if it is initialized or not.
// You can't initialize a variable with a shadowed one, avoiding user errors
// var a = "outer"
// { var a = a }
#[derive(Default)]
pub struct Resolver {
    scopes: Vec<HashMap<EcoString, bool>>,
    locals: HashMap<EcoString, usize>,
}

// If we can’t find it in the stack of local scopes, we assume it must be global
impl Resolver {
    pub fn resolve(&mut self, stmts: &[Stmt]) -> ResolverRes {
        stmts.iter().try_for_each(|s| self.resolve_stmt(s))?;

        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> ResolverRes {
        stmt.accept(self)
    }

    fn resolve_expr(&mut self, expr: &Expr) -> ResolverRes {
        expr.accept(self)
    }

    fn resolve_local(&mut self, name: &EcoString) {
        for (idx, scope) in self.scopes.iter().rev().enumerate() {
            match scope.get(name) {
                Some(v) => match v {
                    true => { let _ = self.locals.insert(name.clone(), idx); },
                    false => continue,
                },
                None => continue,
            }
        }
    }

    fn declare(&mut self, name: EcoString) {
        if self.scopes.is_empty() {
            return;
        }

        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.clone(), false);
    }

    fn define(&mut self, name: EcoString) {
        if self.scopes.is_empty() {
            return;
        }

        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.clone(), true);
    }

    fn resolve_fn(&mut self, stmt: &FnDeclStmt) -> ResolverRes {
        self.begin_scope();

        stmt.params.iter().for_each(|p| {
            self.declare(p.clone());
            self.define(p.clone());
        });

        self.resolve(&stmt.body.clone())?;

        self.end_scope();

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }
}

impl VisitStmt<(), ResolverErr> for Resolver {
    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> ResolverRes {
        self.resolve_expr(&stmt.expr)?;

        Ok(())
    }

    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> ResolverRes {
        self.resolve_expr(&stmt.expr)
    }

    fn visit_var_decl_stmt(&mut self, stmt: &VarDeclStmt) -> ResolverRes {
        self.declare(stmt.name.clone());

        if let Some(v) = &stmt.value {
            self.resolve_expr(v)?;
        }

        self.define(stmt.name.clone());

        Ok(())
    }

    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> ResolverRes {
        self.begin_scope();
        self.resolve(&stmt.stmts)?;
        self.end_scope();

        Ok(())
    }

    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> ResolverRes {
        self.resolve_expr(&stmt.condition)?;

        if let Some(t) = &stmt.then_branch {
            self.resolve_stmt(t)?;
        }
        if let Some(e) = &stmt.else_branch {
            self.resolve_stmt(e)?;
        }

        Ok(())
    }

    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> ResolverRes {
        self.resolve_expr(&stmt.condition)?;
        self.resolve_stmt(&stmt.body)
    }

    fn visit_for_stmt(&mut self, stmt: &ForStmt) -> ResolverRes {
        self.resolve_stmt(&(&stmt.placeholder).into())?;
        self.resolve_stmt(&stmt.body)
    }

    fn visit_fn_decl_stmt(&mut self, stmt: &FnDeclStmt) -> ResolverRes {
        self.declare(stmt.name.clone());
        self.define(stmt.name.clone());

        self.resolve_fn(stmt)
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> ResolverRes {
        if let Some(v) = &stmt.value {
            self.resolve_expr(v)?;
        }

        Ok(())
    }
}

impl VisitExpr<(), ResolverErr> for Resolver {
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> ResolverRes {
        self.resolve_expr(&expr.left)?;
        self.resolve_expr(&expr.right)
    }

    fn visit_grouping_expr(&mut self, expr: &GroupingExpr) -> ResolverRes {
        self.resolve_expr(&expr.expr)?;

        Ok(())
    }

    fn visit_int_literal_expr(&mut self, _: &IntLiteralExpr) -> ResolverRes {
        Ok(())
    }

    fn visit_real_literal_expr(&mut self, _: &RealLiteralExpr) -> ResolverRes {
        Ok(())
    }

    fn visit_str_literal_expr(&mut self, _: &StrLiteralExpr) -> ResolverRes {
        Ok(())
    }

    fn visit_identifier_expr(&mut self, expr: &IdentifierExpr) -> ResolverRes {
        if !self.scopes.is_empty()
            && self.scopes.last().unwrap().get(&expr.name) == Some(&false)
        {
            return Err(PhyResult::new(
                ResolverErr::LocalVarInOwnInit,
                Some(expr.loc.clone()),
            ));
        }

        self.resolve_local(&expr.name);

        Ok(())
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> ResolverRes {
        self.resolve_expr(&expr.right)
    }

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> ResolverRes {
        self.resolve_expr(&expr.into())?;
        self.resolve_local(&expr.name);

        Ok(())
    }

    fn visit_logical_expr(&mut self, expr: &LogicalExpr) -> ResolverRes {
        self.resolve_expr(&expr.right)?;
        self.resolve_expr(&expr.left)
    }

    fn visit_call_expr(&mut self, expr: &CallExpr) -> ResolverRes {
        self.resolve_expr(&expr.callee)?;

        for arg in &expr.args {
            self.resolve_expr(arg)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{resolver::ResolverErr, utils::lex_parse_resolve};

    #[test]
    fn depth() {
        let code = "
var a
{
    var b = a
    var c
    {
        var d = c + a
        {
            var e
            e = e + d + b + a
        }
    }
}
";
        let resolver = lex_parse_resolve(code);
    }

    #[test]
    fn local_var_in_its_init() {
        let code = "
var a
{
    var a = a
}
";
        let resolver = lex_parse_resolve(code);
        let err = resolver.err().unwrap().err;
        assert_eq!(err, ResolverErr::LocalVarInOwnInit);
    }
}

use std::{cell::RefCell, collections::HashMap};

use colored::Colorize;
use ecow::EcoString;
use runtime::interpreter::Interpreter;
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

#[derive(Error, Debug)]
enum ResolverErr {
    #[error("local variable initializer is shadoweding global variable")]
    LocalVarInOwnInit,
}

impl PhyReport for ResolverErr {
    fn get_err_msg(&self) -> String {
        format!("{}: ", "Resolver error".red())
    }
}

type ResolverRes = Result<(), PhyResult<ResolverErr>>;

// Bool is for tracking if the variable is initialized, avoiding weird cases
// where we initialize the variable with its shadowing global one
// We track if it is initialized or not.
// You can't initialize a variable with a shadowed one, avoiding user errors
// var a = "outer"
// { var a = a }
struct Resolver<'a> {
    scopes: RefCell<Vec<HashMap<EcoString, bool>>>,
    interp: &'a mut Interpreter,
}

// If we can’t find it in the stack of local scopes, we assume it must be global
impl<'a> Resolver<'a> {
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

    fn resolve_local(&self, expr: &Expr, name: &EcoString) {
        for scope in self.scopes.borrow().iter().rev() {
            match scope.get(name) {
                Some(v) => match v {
                    true => todo!(),
                    false => continue,
                },
                None => continue,
            }
        }
    }

    fn declare(&self, name: EcoString) {
        if self.scopes.borrow().is_empty() {
            return;
        }

        self.scopes
            .borrow_mut()
            .last_mut()
            .unwrap()
            .insert(name.clone(), false);
    }

    fn define(&self, name: EcoString) {
        if self.scopes.borrow().is_empty() {
            return;
        }

        self.scopes
            .borrow_mut()
            .last_mut()
            .unwrap()
            .insert(name.clone(), true);
    }

    fn resolve_fn(&self, stmt: &FnDeclStmt) -> ResolverRes {
        self.begin_scope();

        stmt.params.iter().for_each(|p| {
            self.declare(p.clone());
            self.define(p.clone());
        });

        self.resolve(&stmt.body.clone())?;

        self.end_scope();

        Ok(())
    }

    fn begin_scope(&self) {
        self.scopes.borrow_mut().push(HashMap::new());
    }

    fn end_scope(&self) {
        self.scopes.borrow_mut().pop();
    }
}

impl<'a> VisitStmt<(), ResolverErr> for Resolver<'a> {
    fn visit_expr_stmt(&self, stmt: &ExprStmt) -> ResolverRes {
        self.resolve_expr(&stmt.expr)?;

        Ok(())
    }

    fn visit_print_stmt(&self, stmt: &PrintStmt) -> ResolverRes {
        self.resolve_expr(&stmt.expr)
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
        self.resolve_expr(&stmt.condition)?;

        if let Some(t) = &stmt.then_branch {
            self.resolve_stmt(t)?;
        }
        if let Some(e) = &stmt.else_branch {
            self.resolve_stmt(e)?;
        }

        Ok(())
    }

    fn visit_while_stmt(&self, stmt: &WhileStmt) -> ResolverRes {
        self.resolve_expr(&stmt.condition)?;
        self.resolve_stmt(&stmt.body)
    }

    fn visit_for_stmt(&self, stmt: &ForStmt) -> ResolverRes {
        self.resolve_stmt(&(&stmt.placeholder).into())?;
        self.resolve_stmt(&stmt.body)
    }

    fn visit_fn_decl_stmt(&self, stmt: &FnDeclStmt) -> ResolverRes {
        self.declare(stmt.name.clone());
        self.define(stmt.name.clone());

        self.resolve_fn(stmt)
    }

    fn visit_return_stmt(&self, stmt: &ReturnStmt) -> ResolverRes {
        if let Some(v) = &stmt.value {
            self.resolve_expr(v)?;
        }

        Ok(())
    }
}

impl<'a> VisitExpr<(), ResolverErr> for Resolver<'a> {
    fn visit_binary_expr(&self, expr: &BinaryExpr) -> ResolverRes {
        self.resolve_expr(&expr.left)?;
        self.resolve_expr(&expr.right)
    }

    fn visit_grouping_expr(&self, expr: &GroupingExpr) -> ResolverRes {
        self.resolve_expr(&expr.expr)?;

        Ok(())
    }

    fn visit_int_literal_expr(&self, _: &IntLiteralExpr) -> ResolverRes {
        Ok(())
    }

    fn visit_real_literal_expr(&self, _: &RealLiteralExpr) -> ResolverRes {
        Ok(())
    }

    fn visit_str_literal_expr(&self, _: &StrLiteralExpr) -> ResolverRes {
        Ok(())
    }

    fn visit_identifier_expr(&self, expr: &IdentifierExpr) -> ResolverRes {
        if !self.scopes.borrow().is_empty()
            && self.scopes.borrow().last().unwrap().get(&expr.name) == Some(&false)
        {
            return Err(PhyResult::new(
                ResolverErr::LocalVarInOwnInit,
                Some(expr.loc.clone()),
            ));
        }

        self.resolve_local(&expr.into(), &expr.name);

        Ok(())
    }

    fn visit_unary_expr(&self, expr: &UnaryExpr) -> ResolverRes {
        self.resolve_expr(&expr.right)
    }

    fn visit_assign_expr(&self, expr: &AssignExpr) -> ResolverRes {
        self.resolve_expr(&expr.into())?;
        self.resolve_local(&expr.into(), &expr.name);

        Ok(())
    }

    fn visit_logical_expr(&self, expr: &LogicalExpr) -> ResolverRes {
        self.resolve_expr(&expr.right)?;
        self.resolve_expr(&expr.left)
    }

    fn visit_call_expr(&self, expr: &CallExpr) -> ResolverRes {
        self.resolve_expr(&expr.callee)?;

        for arg in &expr.args {
            self.resolve_expr(arg)?;
        }

        Ok(())
    }
}


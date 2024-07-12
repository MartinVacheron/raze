use std::collections::HashMap;

use colored::Colorize;
use ecow::EcoString;
use thiserror::Error;
use tools::{results::{Loc, PhyReport, PhyResult}, ToUuid};

use frontend::ast::{
    expr::{
        AssignExpr, BinaryExpr, CallExpr, Expr, GetExpr, SetExpr, GroupingExpr, IdentifierExpr, IntLiteralExpr, LogicalExpr, RealLiteralExpr, StrLiteralExpr, UnaryExpr, VisitExpr
    },
    stmt::{
        BlockStmt, ExprStmt, FnDeclStmt, ForStmt, IfStmt, PrintStmt, ReturnStmt, Stmt, StructStmt, VarDeclStmt, VisitStmt, WhileStmt
    },
};


#[derive(Error, Debug, PartialEq)]
pub enum ResolverErr {
    #[error("local variable initializer is shadoweding global variable")]
    LocalVarInOwnInit,

    #[error("A variable with the same name as already been declared in this local scope")]
    AlreadyDeclInLocal,

    #[error("Can't return from top level code")]
    TopLevelReturn,
}

impl PhyReport for ResolverErr {
    fn get_err_msg(&self) -> String {
        format!("{}: ", "Resolver error".red())
    }
}

pub type PhyResResolv = PhyResult<ResolverErr>;
pub type ResolverRes = Result<(), PhyResResolv>;


#[derive(Default, Clone, Copy, PartialEq)]
enum FnType {
    #[default]
    None,
    Function,
    Method,
}


// Bool is for tracking if the variable is initialized, avoiding weird cases
// where we initialize the variable with its shadowing global one
// We track if it is initialized or not.
// You can't initialize a variable with a shadowed one, avoiding user errors
// var a = "outer"
// { var a = a }
#[derive(Default)]
pub struct Resolver {
    scopes: Vec<HashMap<EcoString, bool>>,
    locals: HashMap<String, usize>,
    current_fn: FnType,
}

// If we can’t find it in the stack of local scopes, we assume it must be global
impl Resolver {
    pub fn resolve(&mut self, stmts: &[Stmt]) -> Result<HashMap<String, usize>, Vec<PhyResResolv>> {
        let mut errors: Vec<PhyResResolv> = vec![];

        for s in stmts {
            match self.resolve_stmt(s) {
                Ok(_) => continue,
                Err(e) => errors.push(e)
            }
        }
        
        if !errors.is_empty() {
            return Err(errors)
        }

        Ok(self.locals.clone())
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> ResolverRes {
        stmt.accept(self)
    }

    fn resolve_expr(&mut self, expr: &Expr) -> ResolverRes {
        expr.accept(self)
    }

    fn resolve_local<T: ToUuid>(&mut self, expr: &T, name: &EcoString) {
        for (idx, scope) in self.scopes.iter().rev().enumerate() {
            match scope.get(name) {
                Some(v) => match v {
                    true => {
                        let _ = self.locals.insert(expr.to_uuid(), idx);
                        break
                    },
                    false => continue,
                },
                None => continue,
            }
        }
    }

    fn declare(&mut self, name: EcoString, loc: &Loc) -> ResolverRes {
        if self.scopes.is_empty() {
            return Ok(())
        }

        if self.scopes.last().unwrap().contains_key(&name) {
            return Err(PhyResult::new(
                ResolverErr::AlreadyDeclInLocal,
                Some(loc.clone()),
            ))
        }

        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.clone(), false);

        Ok(())
    }

    fn define(&mut self, name: EcoString) {
        if self.scopes.is_empty() {
            return
        }

        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.clone(), true);
    }

    fn resolve_fn(&mut self, stmt: &FnDeclStmt, typ: FnType) -> ResolverRes {
        let prev_fn_type = self.current_fn;
        self.current_fn = typ;

        self.begin_scope();

        for p in stmt.params.iter() {
            self.declare(p.clone(), &stmt.loc)?;
            self.define(p.clone());
        }

        stmt.body.iter().try_for_each(|s| self.resolve_stmt(s))?;

        self.end_scope();

        self.current_fn = prev_fn_type;

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
        self.declare(stmt.name.clone(), &stmt.loc)?;

        if let Some(v) = &stmt.value {
            self.resolve_expr(v)?;
        }

        self.define(stmt.name.clone());

        Ok(())
    }

    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> ResolverRes {
        self.begin_scope();
        stmt.stmts.iter().try_for_each(|s| self.resolve_stmt(s))?;
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
        self.declare(stmt.name.clone(), &stmt.loc)?;
        self.define(stmt.name.clone());

        self.resolve_fn(stmt, FnType::Function)
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> ResolverRes {
        if self.current_fn == FnType::None {
            return Err(PhyResult::new(
                ResolverErr::TopLevelReturn,
                Some(stmt.loc.clone()),
            ))
        }

        if let Some(v) = &stmt.value {
            self.resolve_expr(v)?;
        }

        Ok(())
    }

    fn visit_struct_stmt(&mut self, stmt: &StructStmt) -> ResolverRes {
        self.declare(stmt.name.clone(), &stmt.loc)?;
        self.define(stmt.name.clone());

        stmt.methods.iter().try_for_each(|m| self.resolve_fn(m, FnType::Method))?;


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
            ))
        }

        self.resolve_local(expr, &expr.name);

        Ok(())
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> ResolverRes {
        self.resolve_expr(&expr.right)
    }

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> ResolverRes {
        self.resolve_expr(&expr.value)?;
        self.resolve_local(expr, &expr.name);

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
    
    fn visit_get_expr(&mut self, expr: &GetExpr) -> Result<(), PhyResult<ResolverErr>> {
        self.resolve_expr(&expr.object)?;

        Ok(())
    }
    
    fn visit_set_expr(&mut self, expr: &SetExpr) -> Result<(), PhyResult<ResolverErr>> {
        self.resolve_expr(&expr.object)?;
        self.resolve_expr(&expr.value)?;

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
    var c = b
    {
        var d = c
        d = a + 6
        {
            var e
            e = e + 1
            e = d - 5
            e = b
        }
    }
}
";
        let resolver = lex_parse_resolve(code).unwrap();

        assert_eq!(resolver.locals.len(), 9);

        // We have to sort because (key, value) aren't inserted in the hashmap
        // in the same order as insertion
        let mut depths = resolver.locals.into_values().collect::<Vec<usize>>();
        depths.sort();

        assert_eq!(
            depths,
            vec![0usize, 0usize, 0usize, 0usize, 0usize, 0usize, 1usize, 1usize, 2usize]
        );
    }

    #[test]
    fn depth_fn() {
        let code = "
{
    var a
    var b
    {
        fn foo(a) {
            var c = a
            var d = b
        }
    }
}
";
        let resolver = lex_parse_resolve(code).unwrap();

        assert_eq!(resolver.locals.len(), 2);

        // We have to sort because (key, value) aren't inserted in the hashmap
        // in the same order as insertion
        let mut depths = resolver.locals.into_values().collect::<Vec<usize>>();
        depths.sort();

        assert_eq!(
            depths,
            vec![0usize, 2usize]
        );
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
        let errs = resolver.err().unwrap();
        assert_eq!(errs[0].err, ResolverErr::LocalVarInOwnInit);
    }

    #[test]
    fn already_decl() {
        let code = "
{
    var a
    var a
}
";
        let resolver = lex_parse_resolve(code);
        let errs = resolver.err().unwrap();
        assert_eq!(errs[0].err, ResolverErr::AlreadyDeclInLocal);
    }

    #[test]
    fn toplev_return() {
        let code = "
return
";
        let resolver = lex_parse_resolve(code);
        let errs = resolver.err().unwrap();
        assert_eq!(errs[0].err, ResolverErr::TopLevelReturn);
    }
}

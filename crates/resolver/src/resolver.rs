use std::collections::HashMap;

use colored::Colorize;
use ecow::EcoString;
use thiserror::Error;
use tools::{results::{Loc, RevReport, RevResult}, ToUuid};

use frontend::ast::{
    expr::{
        AssignExpr, BinaryExpr, CallExpr, Expr, GetExpr, GroupingExpr, IdentifierExpr, IntLiteralExpr, LogicalExpr, FloatLiteralExpr, SelfExpr, SetExpr, StrLiteralExpr, UnaryExpr, VisitExpr
    },
    stmt::{
        BlockStmt, ExprStmt, FnDeclStmt, ForStmt, IfStmt, PrintStmt, ReturnStmt, Stmt, StructStmt, VarDeclStmt, VisitStmt, WhileStmt
    },
};


#[derive(Error, Debug, PartialEq)]
pub enum ResolverErr {
    #[error("local variable initializer is shadoweding global variable")]
    LocalVarInOwnInit,

    #[error("a variable with the same name as already been declared in this local scope")]
    AlreadyDeclInLocal,

    #[error("can't return from top level code")]
    TopLevelReturn,

    #[error("use of self outside of a structure")]
    SelfOutsideStruct,

    #[error("can't return a value from the constructor")]
    ReturnFromInit,

    #[error("can't call the constructor directly")]
    DirectConstructorCall,
}

impl RevReport for ResolverErr {
    fn get_err_msg(&self) -> String {
        format!("{} {}", "Resolver error:".red(), self)
    }
}

pub type RevResResolv = RevResult<ResolverErr>;
pub type ResolverRes = Result<(), RevResResolv>;


#[derive(Default, Clone, Copy, PartialEq)]
enum FnType {
    #[default]
    None,
    Function,
    Init,
    Method,
}


// TEST:
//   can't have same name for field and method because other wise 
//     we don't know which we get when: foo.name if there is a method "name" too
//     or transforme it into a getter


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
    fn_type: FnType,
    in_struct: bool,
}

// If we can’t find it in the stack of local scopes, we assume it must be global
impl Resolver {
    pub fn resolve(&mut self, stmts: &[Stmt]) -> Result<HashMap<String, usize>, Vec<RevResResolv>> {
        let mut errors: Vec<RevResResolv> = vec![];

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
            return Err(RevResult::new(
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
        let prev_fn_type = std::mem::replace(&mut self.fn_type, typ);

        self.begin_scope();

        for p in stmt.params.iter() {
            self.declare(p.clone(), &stmt.loc)?;
            self.define(p.clone());
        }

        stmt.body.iter().try_for_each(|s| self.resolve_stmt(s))?;

        self.end_scope();

        self.fn_type = prev_fn_type;

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
        // Different from book because don't handle for loops the same.
        // I need a scope to declare the placeholder
        self.begin_scope();
        self.resolve_stmt(&(&stmt.placeholder).into())?;
        self.resolve_stmt(&stmt.body)?;
        self.end_scope();

        Ok(())
    }

    fn visit_fn_decl_stmt(&mut self, stmt: &FnDeclStmt) -> ResolverRes {
        self.declare(stmt.name.clone(), &stmt.loc)?;
        self.define(stmt.name.clone());

        self.resolve_fn(stmt, FnType::Function)
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> ResolverRes {
        match self.fn_type {
            FnType::None => return Err(RevResult::new(
                ResolverErr::TopLevelReturn,
                Some(stmt.loc.clone()),
            )),
            FnType::Init => return Err(RevResult::new(
                ResolverErr::ReturnFromInit, Some(stmt.loc.clone())
            )),
            _ => {
                if let Some(v) = &stmt.value {
                    self.resolve_expr(v)?;
                }
            }
        }

        Ok(())
    }

    fn visit_struct_stmt(&mut self, stmt: &StructStmt) -> ResolverRes {
        self.in_struct = true;

        self.declare(stmt.name.clone(), &stmt.loc)?;
        self.define(stmt.name.clone());

        self.begin_scope();
        self.scopes.last_mut().unwrap().insert("self".into(), true);

        stmt.methods.iter().try_for_each(|m| {
            let typ = if m.name == EcoString::from("init") {
                FnType::Init
            } else {
                FnType::Method
            };

            self.resolve_fn(m, typ)
        })?;

        self.end_scope();

        self.in_struct = false;

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

    fn visit_float_literal_expr(&mut self, _: &FloatLiteralExpr) -> ResolverRes {
        Ok(())
    }

    fn visit_str_literal_expr(&mut self, _: &StrLiteralExpr) -> ResolverRes {
        Ok(())
    }

    fn visit_identifier_expr(&mut self, expr: &IdentifierExpr) -> ResolverRes {
        if !self.scopes.is_empty()
            && self.scopes.last().unwrap().get(&expr.name) == Some(&false)
        {
            return Err(RevResult::new(
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
        expr.args.iter().try_for_each(|arg| self.resolve_expr(arg))?;

        Ok(())
    }
    
    fn visit_get_expr(&mut self, expr: &GetExpr) -> ResolverRes {
        // Can't call constructor like: Foo().init()
        if expr.name.as_str() == "init" {
            return Err(RevResult::new(
                ResolverErr::DirectConstructorCall,
                Some(expr.loc.clone()),
            ))
        }

        self.resolve_expr(&expr.object)?;

        Ok(())
    }
    
    fn visit_set_expr(&mut self, expr: &SetExpr) -> ResolverRes {
        self.resolve_expr(&expr.object)?;
        self.resolve_expr(&expr.value)?;

        Ok(())
    }
    
    fn visit_self_expr(&mut self, expr: &SelfExpr) -> ResolverRes {
        if !self.in_struct {
            return Err(RevResult::new(ResolverErr::SelfOutsideStruct, Some(expr.loc.clone())))
        }

        self.resolve_local(expr, &expr.name);

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
        let locals = lex_parse_resolve(code).unwrap();

        assert_eq!(locals.len(), 9);

        // We have to sort because (key, value) aren't inserted in the hashmap
        // in the same order as insertion
        let mut depths = locals.into_values().collect::<Vec<usize>>();
        depths.sort();

        assert_eq!(
            depths,
            vec![0, 0, 0, 0, 0, 0, 1, 1, 2]
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
        let locals = lex_parse_resolve(code).unwrap();

        assert_eq!(locals.len(), 2);

        // We have to sort because (key, value) aren't inserted in the hashmap
        // in the same order as insertion
        let mut depths = locals.into_values().collect::<Vec<usize>>();
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

    #[test]
    fn self_no_struct() {
        let code = "
self.foo
";
        let resolver = lex_parse_resolve(code);
        let errs = resolver.err().unwrap();
        assert_eq!(errs[0].err, ResolverErr::SelfOutsideStruct);
    }

    #[test]
    fn return_from_init() {
        let code = "
struct Foo {
    fn init() { return 1 }
}
";
        let resolver = lex_parse_resolve(code);
        let errs = resolver.err().unwrap();
        assert_eq!(errs[0].err, ResolverErr::ReturnFromInit);
    }
}

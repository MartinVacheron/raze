use std::{collections::HashMap, fmt::Display};

use colored::Colorize;
use ecow::EcoString;
use thiserror::Error;
use rizon_tools::results::{Loc, RizonReport, RizonResult};

use rizon_frontend::{
    ast::{
        expr::{
            AssignExpr, BinaryExpr, CallExpr, Expr, FloatLiteralExpr, GetExpr, GroupingExpr,
            IdentifierExpr, IntLiteralExpr, IsExpr, LogicalExpr, SelfExpr, SetExpr, StrLiteralExpr,
            UnaryExpr, VisitExpr,
        },
        stmt::{
            BlockStmt, ExprStmt, FnDeclStmt, ForStmt, IfStmt, PrintStmt, ReturnStmt, Stmt,
            StructStmt, VarDeclStmt, VarTypeDecl, VisitStmt, WhileStmt,
        },
    },
    lexer::{Token, TokenKind},
};

#[derive(Error, Debug, PartialEq)]
pub enum StaticAnalyzerErr {
    #[error("local variable initializer is shadoweding global variable")]
    LocalVarInOwnInit,

    #[error("can't return from top level code")]
    TopLevelReturn,

    #[error("use of self outside of a structure")]
    SelfOutsideStruct,

    #[error("can't return a value from the constructor")]
    ReturnFromInit,

    #[error("can't call the constructor directly")]
    DirectConstructorCall,

    #[error("undeclared variable '{0}'")]
    UndeclaredVar(String),

    // Structures
    #[error("structure '{0}' has no field '{1}'")]
    InexistantField(String, String),

    #[error("structure '{0}' has no method constructor")]
    InexistantConstructor(String),

    #[error("only structure instances have fields")]
    NonStructFieldAccess,

    #[error("constructor can't return anything")]
    ConstructorReturnType,

    // Types
    #[error("unknown type '{0}'")]
    UnknownType(String),

    #[error("a {0} with the same name as already been declared in this scope")]
    AlreadyDecl(String),

    #[error("operation '{0}' is not allowed between types '{1}' and '{2}'")]
    InvalidOp(String, String, String),

    #[error("unary operator '!' can only be used on 'bool' type")]
    NonBoolBangUnary,

    #[error("unary operator '-' can only be used on numeric types")]
    NonNumMinusUnary,

    #[error("unknown operator")]
    UnknownOp,

    #[error("variable has no type")]
    VarNonType,

    #[error("trying to assign value of type '{0}' to variable of type '{1}'")]
    WrongTypeAssign(String, String),

    #[error("logical operators must have same type on each side, found '{0}' and '{1}'")]
    WrongTypeLogical(String, String),

    #[error("variable is not of type '{0}'")]
    WrongVarType(String),

    // Functions
    #[error("not all code paths return a value of type '{0}' in function '{1}'")]
    NotAllPathReturn(String, String),

    #[error("wrong type returned, expected '{0}' but found '{1}'")]
    WrongReturnType(String, String),

    #[error("wrong arguments number, expected {0} but found {1}")]
    WrongArgsNb(usize, usize),

    #[error("wrong arguments type, expected '{0}' but found '{1}'")]
    WrongArgsType(String, String),

    // Call
    #[error("only functions and structures are callable")]
    NonFnCall,

    // Warings
    #[error("{0}")]
    Warning(#[from] Warning),
}

impl RizonReport for StaticAnalyzerErr {
    fn get_err_msg(&self) -> String {
        match self {
            StaticAnalyzerErr::Warning(_) => format!("{} {}", "Static analyser warning:".red(), self),
            _ => format!("{} {}", "Static analyser error:".red(), self)
        }
    }
}

#[derive(Debug, Error, PartialEq, Clone)]
pub enum StaticAnalyzerWarning {
    #[error("comparison between int and float can lead to misleading result")]
    CompIntFloat,

    #[error("unreachable code after 'return'")]
    UnreachAfterReturn,
}

#[derive(Debug, Error, PartialEq, Clone)]
pub struct Warning {
    kind: StaticAnalyzerWarning,
    loc: Loc,
}

impl Warning {
    pub fn new(kind: StaticAnalyzerWarning, loc: Loc) -> Self {
        Self { kind, loc }
    }
}

impl Display for Warning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

pub type RizonResResolv = RizonResult<StaticAnalyzerErr>;
pub type StaticAnalyzerRes = Result<bool, RizonResResolv>;
pub type StaticAnalyzerExprRes = Result<VarType, RizonResResolv>;

#[derive(Clone, Copy, PartialEq)]
enum FnKind {
    None,
    Function,
    Init,
    Method,
}

struct FnCtx {
    kind: FnKind,
    return_type: VarType,
}

impl Default for FnCtx {
    fn default() -> Self {
        Self {
            kind: FnKind::None,
            return_type: VarType::Void,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum VarType {
    Any,
    Int,
    Float,
    Str,
    Bool,
    Null,
    Void,
    Struct(EcoString),
    Fn(Box<FnType>),
}

impl VarType {
    fn into_fn_return_type(self) -> Self {
        match self {
            VarType::Fn(f) => f.return_type,
            _ => self,
        }
    }

    pub fn new_fn(args_type: Vec<VarType>, return_type: VarType) -> Self {
        VarType::Fn(Box::new(FnType {
            args_type,
            return_type,
        }))
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct FnType {
    args_type: Vec<VarType>,
    return_type: VarType,
}

impl Default for FnType {
    fn default() -> Self {
        FnType {
            args_type: vec![],
            return_type: VarType::Void,
        }
    }
}

#[derive(Default, PartialEq)]
struct StructType {
    name: EcoString,
    fields: HashMap<EcoString, VarType>,
    methods: HashMap<EcoString, VarType>,
}

impl StructType {
    fn get_member_type(&self, member_name: &Token) -> StaticAnalyzerExprRes {
        if let Some(t) = self
            .fields
            .get(&member_name.value)
            .or_else(|| self.methods.get(&member_name.value))
        {
            return Ok(t.clone());
        }

        Err(RizonResult::new(
            StaticAnalyzerErr::InexistantField(
                self.name.clone().into(),
                member_name.value.clone().into(),
            ),
            Some(member_name.loc.clone()),
        ))
    }
}

impl Display for VarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarType::Any => write!(f, "any"),
            VarType::Int => write!(f, "int"),
            VarType::Float => write!(f, "float"),
            VarType::Str => write!(f, "str"),
            VarType::Bool => write!(f, "bool"),
            VarType::Null => write!(f, "null"),
            VarType::Void => write!(f, "void"),
            VarType::Struct(t) => write!(f, "{}", t),
            VarType::Fn(t) => {
                write!(f, "fn(")?;

                for (i, arg) in t.args_type.iter().enumerate() {
                    write!(f, "{}", arg)?;

                    if i < t.args_type.len() - 1 {
                        write!(f, ", ",)?;
                    }
                }

                write!(f, ") -> {}", t.return_type)
            }
        }
    }
}

impl From<&Token> for VarType {
    fn from(value: &Token) -> Self {
        match value.value.as_str() {
            "any" => VarType::Any,
            "int" => VarType::Int,
            "float" => VarType::Float,
            "str" => VarType::Str,
            "bool" => VarType::Bool,
            "null" => VarType::Null,
            "void" => VarType::Void,
            other => VarType::Struct(other.into()),
        }
    }
}

impl From<&Option<Token>> for VarType {
    fn from(value: &Option<Token>) -> Self {
        match value {
            Some(v) => v.into(),
            None => VarType::Void,
        }
    }
}

impl From<&Option<Box<VarTypeDecl>>> for VarType {
    fn from(value: &Option<Box<VarTypeDecl>>) -> Self {
        match value {
            Some(v) => (&**v).into(),
            None => VarType::Void,
        }
    }
}

impl From<&VarTypeDecl> for VarType {
    fn from(value: &VarTypeDecl) -> Self {
        match value {
            VarTypeDecl::Identifier(tk) => tk.into(),
            VarTypeDecl::Fn {
                param_types,
                return_type,
                ..
            } => {
                let args_type = param_types.iter().map(VarType::from).collect();
                let return_type = return_type.into();

                VarType::new_fn(args_type, return_type)
            }
        }
    }
}

impl From<&Option<VarTypeDecl>> for VarType {
    fn from(value: &Option<VarTypeDecl>) -> Self {
        match value {
            Some(v) => v.into(),
            None => VarType::Void,
        }
    }
}

impl From<&EcoString> for VarType {
    fn from(value: &EcoString) -> Self {
        VarType::Struct(value.clone())
    }
}

#[derive(Default)]
struct Scope {
    variables: HashMap<EcoString, bool>,
    var_types: HashMap<EcoString, VarType>,
    types_def: HashMap<EcoString, StructType>,
}

#[derive(Default)]
pub struct StaticAnalyzer {
    globals: Scope,
    scopes: Vec<Scope>,
    locals: HashMap<Loc, usize>,
    fn_ctx: FnCtx,
    current_struct: Option<EcoString>,
    warnings: Vec<Warning>,
}

// If we can’t find it in the stack of local scopes, we assume it must be global
impl StaticAnalyzer {
    pub fn resolve(&mut self, stmts: &[Stmt]) -> Result<HashMap<Loc, usize>, Vec<RizonResResolv>> {
        self.set_globals();

        let mut errors: Vec<RizonResResolv> = vec![];

        for s in stmts {
            match self.resolve_stmt(s) {
                Ok(_) => continue,
                Err(e) => errors.push(e),
            }
        }

        if !errors.is_empty() || !self.warnings.is_empty() {
            self.warnings.iter().for_each(|w| errors.push(RizonResult::new(
                StaticAnalyzerErr::Warning((*w).clone()),
                Some(w.loc.clone()),
            )));

            return Err(errors);
        }

        Ok(self.locals.clone())
    }

    fn set_globals(&mut self) {
        self.globals.variables.insert("true".into(), true);
        self.globals.variables.insert("false".into(), true);
        self.globals.variables.insert("null".into(), true);
        self.globals.variables.insert("clock".into(), true);
        self.globals.variables.insert("input".into(), true);

        self.globals.var_types.insert("true".into(), VarType::Bool);
        self.globals.var_types.insert("false".into(), VarType::Bool);
        self.globals.var_types.insert("null".into(), VarType::Null);
        self.globals
            .var_types
            .insert("clock".into(), VarType::new_fn(vec![], VarType::Float));
        self.globals
            .var_types
            .insert("input".into(), VarType::new_fn(vec![VarType::Str], VarType::Str));

        for t in ["any", "int", "float", "str", "bool", "void"] {
            self.globals
                .types_def
                .insert(t.into(), StructType::default());
        }
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> StaticAnalyzerRes {
        stmt.accept(self)
    }

    fn resolve_expr(&mut self, expr: &Expr) -> StaticAnalyzerExprRes {
        expr.accept(self)
    }

    fn resolve_local(&mut self, loc: &Loc, name: &EcoString) -> Result<(), RizonResResolv> {
        for (idx, scope) in self.scopes.iter().rev().enumerate() {
            if let Some(&v) = scope.variables.get(name) {
                if v {
                    self.locals.insert(loc.clone(), idx);
                    return Ok(());
                }
            }
        }

        if !self.globals.variables.contains_key(name) {
            return Err(RizonResult::new(
                StaticAnalyzerErr::UndeclaredVar(name.into()),
                Some(loc.clone()),
            ));
        }

        Ok(())
    }

    fn declare_name(&mut self, name: &EcoString, loc: &Loc, decl_type: &str) -> Result<(), RizonResResolv> {
        if self.scopes.is_empty() {
            if self.globals.variables.contains_key(name) {
                return Err(RizonResult::new(
                    StaticAnalyzerErr::AlreadyDecl(decl_type.into()),
                    Some(loc.clone()),
                ));
            }

            self.globals.variables.insert(name.clone(), false);

            return Ok(());
        }

        if self.scopes.last().unwrap().variables.contains_key(name) {
            return Err(RizonResult::new(
                StaticAnalyzerErr::AlreadyDecl(decl_type.into()),
                Some(loc.clone()),
            ));
        }

        self.scopes
            .last_mut()
            .unwrap()
            .variables
            .insert(name.clone(), false);

        Ok(())
    }

    fn define_name(&mut self, name: &EcoString) {
        if self.scopes.is_empty() {
            self.globals.variables.insert(name.clone(), true);

            return;
        }

        self.scopes
            .last_mut()
            .unwrap()
            .variables
            .insert(name.clone(), true);
    }

    fn resolve_fn(&mut self, stmt: &FnDeclStmt, fn_ctx: FnCtx) -> Result<(), RizonResResolv> {
        if fn_ctx.kind == FnKind::Init && stmt.return_type.is_some() {
            return Err(RizonResult::new(
                StaticAnalyzerErr::ConstructorReturnType,
                Some(stmt.return_type.as_ref().unwrap().get_loc()),
            ));
        }

        let prev_fn_ctx = std::mem::replace(&mut self.fn_ctx, fn_ctx);

        self.begin_scope();

        for p in stmt.params.iter() {
            self.declare_name(&p.name.value, &stmt.name.loc, "variable")?;
            self.define_name(&p.name.value);
            self.init_var_type(&p.name.value, (&p.typ).into());
        }

        let mut return_type = None;
        let mut end_reached = false;

        for stmt in &stmt.body.stmts {
            if return_type.is_some() || end_reached {
                self.warnings.push(Warning::new(StaticAnalyzerWarning::UnreachAfterReturn, stmt.get_loc()));
            }

            if let Stmt::Return(r) = stmt {
                if let Some(v) = &r.value {
                    return_type = Some(v.accept(self)?);
                }
            }

            end_reached = end_reached || stmt.accept(self)?;
        }

        match (return_type, &stmt.return_type) {
            (None, Some(r)) if !end_reached => {
                if Into::<VarType>::into(r) != VarType::Void {
                    return Err(RizonResult::new(
                        StaticAnalyzerErr::NotAllPathReturn(
                            Into::<VarType>::into(r).to_string(),
                            stmt.name.value.to_string()
                        ),
                        Some(r.get_loc()),
                    ));
                }
            }
            _ => {}
        }

        self.end_scope();

        self.fn_ctx = prev_fn_ctx;

        Ok(())
    }

    fn resolve_fn_type(stmt: &FnDeclStmt) -> VarType {
        let args_type: Vec<VarType> = stmt.params.iter().map(|p| (&p.typ).into()).collect();
        let return_type: VarType = (&stmt.return_type).into();

        VarType::Fn(Box::new(FnType {
            args_type,
            return_type,
        }))
    }

    fn declare_type(&mut self, var_type: StructType, loc: &Loc) -> Result<(), RizonResResolv> {
        if self.scopes.is_empty() {
            if self.globals.types_def.contains_key(&var_type.name) {
                return Err(RizonResult::new(
                    StaticAnalyzerErr::AlreadyDecl("type".into()),
                    Some(loc.clone()),
                ));
            }

            self.globals
                .types_def
                .insert(var_type.name.clone(), var_type);

            return Ok(());
        }

        if self
            .scopes
            .last()
            .unwrap()
            .types_def
            .contains_key(&var_type.name)
        {
            return Err(RizonResult::new(
                StaticAnalyzerErr::AlreadyDecl("type".into()),
                Some(loc.clone()),
            ));
        }

        self.scopes
            .last_mut()
            .unwrap()
            .types_def
            .insert(var_type.name.clone(), var_type);

        Ok(())
    }

    fn init_var_type(&mut self, var_name: &EcoString, var_type: VarType) {
        let target = if self.scopes.is_empty() {
            &mut self.globals.var_types
        } else {
            &mut self.scopes.last_mut().unwrap().var_types
        };

        target.insert(var_name.clone(), var_type);
    }

    fn update_var_type(&mut self, var_name: &EcoString, var_type: VarType, loc: &Loc) {
        if let Some(depth) = self.locals.get(loc) {
            if let Some(scope) = self.scopes.iter_mut().rev().nth(*depth) {
                scope.var_types.insert(var_name.clone(), var_type);
                return;
            }
        }

        self.globals.var_types.insert(var_name.clone(), var_type);
    }

    fn check_type_exists(&self, type_name: &EcoString, loc: &Loc) -> Result<(), RizonResResolv> {
        if self.globals.types_def.contains_key(type_name) {
            return Ok(());
        }

        if self.scopes.is_empty() {
            return Err(RizonResult::new(
                StaticAnalyzerErr::UnknownType(type_name.to_string()),
                Some(loc.clone()),
            ));
        }

        for scope in self.scopes.iter().rev() {
            if scope.types_def.contains_key(type_name) {
                return Ok(());
            }
        }

        Err(RizonResult::new(
            StaticAnalyzerErr::UnknownType(type_name.to_string()),
            Some(loc.clone()),
        ))
    }

    fn get_var_type(&self, var_name: &EcoString, loc: &Loc) -> StaticAnalyzerExprRes {
        for scope in self.scopes.iter().rev() {
            if let Some(t) = scope.var_types.get(var_name) {
                return Ok(t.clone());
            }

            // Case where we call the type directly like: var f = Foo()
            if let Some(t) = scope.types_def.get(var_name) {
                return Ok((&t.name).into());
            }
        }

        if let Some(t) = self.globals.var_types.get(var_name) {
            return Ok(t.clone());
        }

        if let Some(t) = self.globals.types_def.get(var_name) {
            return Ok((&t.name).into());
        }

        Err(RizonResult::new(StaticAnalyzerErr::VarNonType, Some(loc.clone())))
    }

    fn get_type_def(&self, type_name: &EcoString, loc: &Loc) -> Result<&StructType, RizonResResolv> {
        for scope in self.scopes.iter().rev() {
            if let Some(t) = scope.types_def.get(type_name) {
                return Ok(t);
            }
        }

        if let Some(t) = self.globals.types_def.get(type_name) {
            return Ok(t);
        }

        Err(RizonResult::new(
            StaticAnalyzerErr::UnknownType(type_name.into()),
            Some(loc.clone()),
        ))
    }

    fn struct_members_types(
        fields: &[VarDeclStmt],
        methods: &[FnDeclStmt],
    ) -> Result<(HashMap<EcoString, VarType>, HashMap<EcoString, VarType>), RizonResResolv> {
        let mut fields_types: HashMap<EcoString, VarType> = HashMap::new();
        let mut methods_types: HashMap<EcoString, VarType> = HashMap::new();
        let mut has_init = false;

        for field in fields {
            if fields_types.contains_key(&field.name.value) {
                return Err(RizonResult::new(
                    StaticAnalyzerErr::AlreadyDecl("field".into()),
                    Some(field.name.loc.clone()),
                ));
            }

            let field_type = field.typ.as_ref().map_or(VarType::Any, |t| t.into());

            fields_types.insert(field.name.value.clone(), field_type);
        }

        for method in methods {
            if methods_types.contains_key(&method.name.value) {
                return Err(RizonResult::new(
                    StaticAnalyzerErr::AlreadyDecl("method".into()),
                    Some(method.name.loc.clone()),
                ));
            }

            if method.name.value.as_str() == "init" {
                has_init = true;
            }

            let fn_ctx = StaticAnalyzer::resolve_fn_type(method);
            methods_types.insert(method.name.value.clone(), fn_ctx);
        }

        if !has_init {
            methods_types.insert(
                EcoString::from("init"),
                VarType::Fn(Box::new(FnType::default())),
            );
        }

        Ok((fields_types, methods_types))
    }

    fn is_castable(current_type: &VarType, cast_to: &VarType) -> bool {
        match (current_type, cast_to) {
            (VarType::Int, VarType::Float) => true,
            _ => false,
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }
}

impl VisitStmt<bool, StaticAnalyzerErr> for StaticAnalyzer {
    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> StaticAnalyzerRes {
        self.resolve_expr(&stmt.expr)?;

        Ok(false)
    }

    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> StaticAnalyzerRes {
        match self.resolve_expr(&stmt.expr) {
            Ok(_) => Ok(false),
            Err(e) => Err(e),
        }
    }

    fn visit_var_decl_stmt(&mut self, stmt: &VarDeclStmt) -> StaticAnalyzerRes {
        self.declare_name(&stmt.name.value, &stmt.name.loc, "variable")?;

        let mut final_type = match &stmt.typ {
            Some(VarTypeDecl::Identifier(i)) => {
                self.check_type_exists(&i.value, &i.loc)?;
                i.into()
            }
            t @ Some(VarTypeDecl::Fn {
                param_types,
                return_type,
                ..
            }) => {
                if let Some(r) = return_type {
                    if let VarTypeDecl::Identifier(i) = &**r {
                        self.check_type_exists(&i.value, &i.loc)?;
                    }
                }

                param_types
                    .iter()
                    .try_for_each(|p| self.check_type_exists(&p.value, &p.loc))?;

                t.into()
            }
            None => VarType::Any,
        };

        if let Some(v) = &stmt.value {
            let value_type = self.resolve_expr(v)?;

            if final_type != VarType::Any && final_type != value_type {
                // We allow passing int values to float types
                if !StaticAnalyzer::is_castable(&value_type, &final_type) {
                    return Err(RizonResult::new(
                        StaticAnalyzerErr::WrongTypeAssign(
                            value_type.to_string(),
                            final_type.to_string(),
                        ),
                        Some(v.get_loc()),
                    ));
                }
            } else {
                final_type = value_type;
            }
        }

        self.define_name(&stmt.name.value);
        self.init_var_type(&stmt.name.value, final_type);

        Ok(false)
    }

    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> StaticAnalyzerRes {
        self.begin_scope();

        let mut end_reached = false;
        for stmt in &stmt.stmts {
            if end_reached {
                self.warnings.push(Warning::new(StaticAnalyzerWarning::UnreachAfterReturn, stmt.get_loc()));
            }

            end_reached = end_reached || stmt.accept(self)?;
        }

        self.end_scope();

        Ok(end_reached)
    }

    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> StaticAnalyzerRes {
        stmt.condition.accept(self)?;

        let complete_then = if let Some(t) = &stmt.then_branch {
            t.accept(self)?
        } else {
            false
        };

        let complete_else = if let Some(e) = &stmt.else_branch {
            e.accept(self)?
        } else {
            false
        };

        Ok(complete_then && complete_else)
    }

    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> StaticAnalyzerRes {
        stmt.condition.accept(self)?;
        stmt.body.accept(self)?;

        Ok(false)
    }

    fn visit_for_stmt(&mut self, stmt: &ForStmt) -> StaticAnalyzerRes {
        self.begin_scope();
        self.visit_var_decl_stmt(&stmt.placeholder)?;
        self.init_var_type(&stmt.placeholder.name.value, VarType::Int);
        stmt.body.accept(self)?;
        self.end_scope();

        Ok(false)
    }

    fn visit_fn_decl_stmt(&mut self, stmt: &FnDeclStmt) -> StaticAnalyzerRes {
        self.declare_name(&stmt.name.value, &stmt.name.loc, "function")?;
        self.define_name(&stmt.name.value);

        let return_type = StaticAnalyzer::resolve_fn_type(stmt);
        self.init_var_type(&stmt.name.value, return_type.clone());

        self.resolve_fn(
            stmt,
            FnCtx {
                kind: FnKind::Function,
                return_type: return_type.into_fn_return_type(),
            },
        )?;

        Ok(false)
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> StaticAnalyzerRes {
        match self.fn_ctx.kind {
            FnKind::None => {
                return Err(RizonResult::new(
                    StaticAnalyzerErr::TopLevelReturn,
                    Some(stmt.loc.clone()),
                ))
            }
            FnKind::Init => {
                return Err(RizonResult::new(
                    StaticAnalyzerErr::ReturnFromInit,
                    Some(stmt.loc.clone()),
                ))
            }
            _ => {
                if let Some(v) = &stmt.value {
                    let return_type = self.resolve_expr(v)?;

                    if return_type != self.fn_ctx.return_type {
                        return Err(RizonResult::new(
                            StaticAnalyzerErr::WrongReturnType(
                                self.fn_ctx.return_type.to_string(),
                                return_type.to_string(),
                            ),
                            Some(stmt.loc.clone()),
                        ));
                    }
                }
            }
        }

        Ok(true)
    }

    fn visit_struct_stmt(&mut self, stmt: &StructStmt) -> StaticAnalyzerRes {
        self.current_struct = Some(stmt.name.value.clone());

        self.declare_name(&stmt.name.value, &stmt.name.loc, "structure")?;
        self.define_name(&stmt.name.value);

        let (fields, methods) = StaticAnalyzer::struct_members_types(&stmt.fields, &stmt.methods)?;

        let struct_type = StructType {
            name: stmt.name.value.clone(),
            fields,
            methods,
        };

        self.declare_type(struct_type, &stmt.name.loc)?;

        self.begin_scope();
        self.scopes
            .last_mut()
            .unwrap()
            .variables
            .insert("self".into(), true);

        stmt.methods.iter().try_for_each(|m| {
            let kind = if m.name.value == "init" {
                FnKind::Init
            } else {
                FnKind::Method
            };

            self.resolve_fn(
                m,
                FnCtx {
                    kind,
                    return_type: (&m.return_type).into(),
                },
            )
        })?;

        self.end_scope();
        self.current_struct = None;

        Ok(false)
    }
}

impl VisitExpr<VarType, StaticAnalyzerErr> for StaticAnalyzer {
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> StaticAnalyzerExprRes {
        let lhs_type = self.resolve_expr(&expr.left)?.into_fn_return_type();
        let rhs_type = self.resolve_expr(&expr.right)?.into_fn_return_type();

        let invalid_op_error = |op: &str| {
            RizonResult::new(
                StaticAnalyzerErr::InvalidOp(op.into(), lhs_type.to_string(), rhs_type.to_string()),
                Some(expr.get_loc()),
            )
        };

        match &expr.operator.kind {
            TokenKind::Plus => match (&lhs_type, &rhs_type) {
                (VarType::Int, VarType::Int) => Ok(VarType::Int),
                (VarType::Int, VarType::Float)
                | (VarType::Float, VarType::Int | VarType::Float) => Ok(VarType::Float),
                (VarType::Str, VarType::Str) => Ok(VarType::Str),
                _ => Err(invalid_op_error("+")),
            },
            TokenKind::Minus | TokenKind::Slash | TokenKind::Modulo => {
                match (&lhs_type, &rhs_type) {
                    (VarType::Int, VarType::Int) => Ok(VarType::Int),
                    (VarType::Int, VarType::Float)
                    | (VarType::Float, VarType::Int | VarType::Float) => Ok(VarType::Float),
                    _ => Err(invalid_op_error(expr.operator.value.as_str())),
                }
            }
            TokenKind::Star => match (&lhs_type, &rhs_type) {
                (VarType::Int, VarType::Int) => Ok(VarType::Int),
                (VarType::Int, VarType::Float)
                | (VarType::Float, VarType::Int | VarType::Float) => Ok(VarType::Float),
                (VarType::Int, VarType::Str) | (VarType::Str, VarType::Int) => Ok(VarType::Str),
                _ => Err(invalid_op_error("*")),
            },
            TokenKind::Less
            | TokenKind::Greater
            | TokenKind::LessEqual
            | TokenKind::GreaterEqual => match (&lhs_type, &rhs_type) {
                (VarType::Int, VarType::Int) | (VarType::Float, VarType::Float) => {
                    Ok(VarType::Bool)
                }
                (VarType::Int, VarType::Float) | (VarType::Float, VarType::Int) => {
                    self.warnings.push(Warning::new(StaticAnalyzerWarning::CompIntFloat, expr.get_loc()));

                    Ok(VarType::Bool)
                }
                _ => Err(invalid_op_error(expr.operator.value.as_str())),
            },
            TokenKind::EqualEqual | TokenKind::BangEqual => match (&lhs_type, &rhs_type) {
                (VarType::Int, VarType::Int)
                | (VarType::Float, VarType::Float)
                | (VarType::Str, VarType::Str)
                | (VarType::Bool, VarType::Bool) => Ok(VarType::Bool),
                (VarType::Int, VarType::Float) | (VarType::Float, VarType::Int) => {
                    self.warnings.push(Warning::new(StaticAnalyzerWarning::CompIntFloat, expr.get_loc()));

                    Ok(VarType::Bool)
                }
                (VarType::Struct(_), VarType::Struct(_)) => Ok(VarType::Bool),
                _ => Err(invalid_op_error(expr.operator.value.as_str())),
            },
            _ => Err(RizonResult::new(
                StaticAnalyzerErr::UnknownOp,
                Some(expr.operator.loc.clone()),
            )),
        }
    }

    fn visit_grouping_expr(&mut self, expr: &GroupingExpr) -> StaticAnalyzerExprRes {
        self.resolve_expr(&expr.expr)
    }

    fn visit_int_literal_expr(&mut self, _: &IntLiteralExpr) -> StaticAnalyzerExprRes {
        Ok(VarType::Int)
    }

    fn visit_float_literal_expr(&mut self, _: &FloatLiteralExpr) -> StaticAnalyzerExprRes {
        Ok(VarType::Float)
    }

    fn visit_str_literal_expr(&mut self, _: &StrLiteralExpr) -> StaticAnalyzerExprRes {
        Ok(VarType::Str)
    }

    fn visit_identifier_expr(&mut self, expr: &IdentifierExpr) -> StaticAnalyzerExprRes {
        if !self.scopes.is_empty()
            && self.scopes.last().unwrap().variables.get(&expr.name) == Some(&false)
        {
            return Err(RizonResult::new(
                StaticAnalyzerErr::LocalVarInOwnInit,
                Some(expr.loc.clone()),
            ));
        }

        self.resolve_local(&expr.loc, &expr.name)?;
        self.get_var_type(&expr.name, &expr.loc)
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> StaticAnalyzerExprRes {
        let val_type = self.resolve_expr(&expr.right)?;

        match expr.operator.kind {
            TokenKind::Minus => {
                if val_type != VarType::Int && val_type != VarType::Float {
                    return Err(RizonResult::new(
                        StaticAnalyzerErr::NonNumMinusUnary,
                        Some(expr.right.get_loc().clone()),
                    ));
                }
            }
            TokenKind::Bang => {
                if val_type != VarType::Bool {
                    return Err(RizonResult::new(
                        StaticAnalyzerErr::NonBoolBangUnary,
                        Some(expr.right.get_loc().clone()),
                    ));
                }
            }
            _ => {}
        }

        Ok(val_type)
    }

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> StaticAnalyzerExprRes {
        self.resolve_local(&expr.loc, &expr.name)?;

        self.resolve_expr(&expr.value)?;
        let lhs_type = self.get_var_type(&expr.name, &expr.loc)?;
        let value_type = expr.value.accept(self)?;

        if lhs_type != value_type {
            if lhs_type == VarType::Any {
                self.update_var_type(&expr.name, value_type, &expr.loc);
            } else if !StaticAnalyzer::is_castable(&value_type, &lhs_type) {
                return Err(RizonResult::new(
                    StaticAnalyzerErr::WrongTypeAssign(value_type.to_string(), lhs_type.to_string()),
                    Some(expr.value.get_loc()),
                ));
            }
        }

        Ok(lhs_type)
    }

    fn visit_logical_expr(&mut self, expr: &LogicalExpr) -> StaticAnalyzerExprRes {
        let rhs_type = self.resolve_expr(&expr.right)?;
        let lhs_type = self.resolve_expr(&expr.left)?;

        if lhs_type != rhs_type {
            return Err(RizonResult::new(
                StaticAnalyzerErr::WrongTypeLogical(lhs_type.to_string(), lhs_type.to_string()),
                Some(expr.loc.clone()),
            ));
        }

        Ok(rhs_type)
    }

    fn visit_call_expr(&mut self, expr: &CallExpr) -> StaticAnalyzerExprRes {
        let callee_type = self.resolve_expr(&expr.callee)?;

        let call_args: Vec<VarType> = expr
            .args
            .iter()
            .map(|a| a.accept(self))
            .collect::<Result<_, _>>()?;

        let fn_ctx = match &callee_type {
            VarType::Struct(s) => {
                let typedef = self.get_type_def(s, &expr.loc)?;
                let constructor =
                    typedef
                        .methods
                        .get(&EcoString::from("init"))
                        .ok_or_else(|| {
                            RizonResult::new(
                                StaticAnalyzerErr::InexistantConstructor(s.to_string()),
                                Some(expr.loc.clone()),
                            )
                        })?;

                if let VarType::Fn(f) = constructor {
                    f
                } else {
                    return Err(RizonResult::new(
                        StaticAnalyzerErr::InexistantConstructor(s.to_string()),
                        Some(expr.loc.clone()),
                    ));
                }
            }
            VarType::Fn(f) => f,
            _ => {
                return Err(RizonResult::new(
                    StaticAnalyzerErr::NonFnCall,
                    Some(expr.callee.get_loc()),
                ))
            }
        };

        if fn_ctx.args_type.len() != expr.args.len() {
            return Err(RizonResult::new(
                StaticAnalyzerErr::WrongArgsNb(fn_ctx.args_type.len(), expr.args.len()),
                Some(expr.loc.clone()),
            ));
        }

        for (mut call_arg, arg_decl) in call_args.into_iter().zip(&fn_ctx.args_type) {
            // If we dont wait for a function as arg, we collapse it to the return value
            if !matches!(arg_decl, VarType::Fn(_)) {
                call_arg = call_arg.into_fn_return_type();
            }

            if &call_arg != arg_decl && !StaticAnalyzer::is_castable(&call_arg, arg_decl) {
                return Err(RizonResult::new(
                    StaticAnalyzerErr::WrongArgsType(arg_decl.to_string(), call_arg.to_string()),
                    Some(expr.loc.clone()),
                ));
            }
        }

        expr.args.iter().try_for_each(|arg| {
            self.resolve_expr(arg)?;
            Ok(())
        })?;

        Ok(callee_type.into_fn_return_type())
    }

    fn visit_get_expr(&mut self, expr: &GetExpr) -> StaticAnalyzerExprRes {
        // Can't call constructor like: Foo().init()
        if expr.name.value.as_str() == "init" {
            return Err(RizonResult::new(
                StaticAnalyzerErr::DirectConstructorCall,
                Some(expr.loc.clone()),
            ));
        }

        let obj_type = self.resolve_expr(&expr.object)?;

        if let VarType::Struct(s) = &obj_type {
            let type_info = self.get_type_def(s, &expr.loc)?;

            return type_info.get_member_type(&expr.name);
        }

        Err(RizonResult::new(
            StaticAnalyzerErr::NonStructFieldAccess,
            Some(expr.loc.clone()),
        ))
    }

    fn visit_set_expr(&mut self, expr: &SetExpr) -> StaticAnalyzerExprRes {
        let obj_type = self.resolve_expr(&expr.object)?;
        let value_type = self.resolve_expr(&expr.value)?;

        if let VarType::Struct(struct_name) = &obj_type {
            let struct_type = self.get_type_def(&struct_name, &expr.loc)?;

            let member_type = struct_type.get_member_type(&expr.name)?;

            if member_type != value_type && !StaticAnalyzer::is_castable(&value_type, &member_type) {
                return Err(RizonResult::new(
                    StaticAnalyzerErr::WrongTypeAssign(value_type.to_string(), member_type.to_string()),
                    Some(expr.value.get_loc()),
                ));
            }
        } else {
            return Err(RizonResult::new(
                StaticAnalyzerErr::NonStructFieldAccess,
                Some(expr.loc.clone()),
            ));
        }

        Ok(obj_type)
    }

    fn visit_self_expr(&mut self, expr: &SelfExpr) -> StaticAnalyzerExprRes {
        if self.current_struct.is_none() {
            return Err(RizonResult::new(
                StaticAnalyzerErr::SelfOutsideStruct,
                Some(expr.loc.clone()),
            ));
        }

        self.resolve_local(&expr.loc, &expr.name)?;

        Ok(VarType::Struct(
            self.current_struct.as_ref().unwrap().clone(),
        ))
    }

    fn visit_is_expr(&mut self, expr: &IsExpr) -> StaticAnalyzerExprRes {
        let left_type = self.resolve_expr(&expr.left)?;
        self.check_type_exists(&expr.typ.value, &expr.loc)?;

        let right_type: VarType = (&expr.typ).into();

        if left_type != right_type {
            return Err(RizonResult::new(
                StaticAnalyzerErr::WrongVarType(right_type.to_string()),
                Some(expr.left.get_loc()),
            ));
        }

        Ok(VarType::Bool)
    }
}

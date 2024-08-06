use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};
use std::io::{self, Write};

use colored::Colorize;
use ecow::EcoString;
use rizon_frontend::ast::expr::{GetExpr, SelfExpr, SetExpr};
use thiserror::Error;
use rizon_tools::results::{RizonReport, RizonResult, Loc};

use crate::callable::Callable;
use crate::environment::Env;
use crate::native_functions::RizonNativeFn;
use crate::values::{Function, RtVal, Negate};
use crate::native_functions::NativeFnErr;
use rizon_frontend::ast::{expr::{
    AssignExpr, BinaryExpr, CallExpr, GroupingExpr, IdentifierExpr, IntLiteralExpr, LogicalExpr,
    FloatLiteralExpr, StrLiteralExpr, UnaryExpr, VisitExpr, IsExpr,
}, stmt::StructStmt};
use rizon_frontend::ast::stmt::{
    BlockStmt, ExprStmt, FnDeclStmt, ForStmt, IfStmt, PrintStmt, ReturnStmt, Stmt, VarDeclStmt, VarTypeDecl, VisitStmt, WhileStmt
};
use rizon_frontend::lexer::TokenKind;


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

    #[error("can't use '-' token on anything other than an int or a float value")]
    NegateNonNumeric,

    #[error("unknow unary operator '{0}'")]
    UnknownUnaryOp(String),

    // Variables
    #[error("{0}")]
    VarDeclEnv(String),

    #[error("{0}")]
    GetVarEnv(String),

    #[error("{0}")]
    AssignEnv(String),

    #[error("uninitialized variable")]
    UninitializedValue,

    // If
    #[error("'if' condition is not a boolean")]
    NonBoolIfCond,

    // While
    #[error("'while' condition is not a boolean")]
    NonBoolWhileCond,

    // For
    #[error("{0}")]
    ForLoop(String),

    // Call
    // TODO: Remove, already done in static analysis
    #[error("only functions and structures are callable")]
    NonFnCall,

    #[error("wrong arguments number, expected {0} but got {1}")]
    WrongArgsNb(usize, usize),

    #[error("{0}")]
    FnCall(String),

    // Property access
    // TODO: Remove, already done in static analysis
    #[error("only structure instances have properties")]
    NonInstPropAccess,

    // TODO: Remove, already done in static analysis
    #[error("structure has no field '{0}'")]
    InexistantField(EcoString),

    #[error("{0}")]
    InexistantFieldBis(String),

    // Results
    #[error("return")]
    Return(Rc<RefCell<RtVal>>),
}

impl RizonReport for InterpErr {
    fn get_err_msg(&self) -> String {
        format!("{} {}", "Interpreter error:".red(), self)
    }
}

pub(crate) type RizonResInterp = RizonResult<InterpErr>;
pub(crate) type InterpRes = Result<Rc<RefCell<RtVal>>, RizonResInterp>;

// --------------
//  Interpreting
// --------------
pub struct Interpreter {
    pub globals: Rc<RefCell<Env>>,
    pub env: Rc<RefCell<Env>>,
    pub locals: HashMap<Loc, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(Env::new(None)));


        let _ = globals.borrow_mut().declare_var(
            EcoString::from("clock"),
            Rc::new(RefCell::new(RtVal::NativeFnVal(RizonNativeFn {
                name: EcoString::from("clock"),
                arity: 0,
                func: |_, _| {
                    match SystemTime::now().duration_since(UNIX_EPOCH) {
                        Ok(t) => Ok(RtVal::new_float(t.as_millis() as f64 / 1000.).into()),
                        Err(_) => Err(RizonResult::new(NativeFnErr::GetTime.into(), None))
                    }
                },
            }))),
        );

        let _ = globals.borrow_mut().declare_var(
            EcoString::from("input"),
            Rc::new(RefCell::new(RtVal::NativeFnVal(RizonNativeFn {
                name: EcoString::from("input"),
                arity: 1,
                func: |_, args| {
                    println!("{}", args[0].borrow());
                    std::io::stdout().flush().unwrap();

                    let mut input = String::new();
                    io::stdin().read_line(&mut input).expect("Failed to read line");

                    let input = input.trim();

                    Ok(RtVal::new_str(input.into()).into())
                },
            }))),
        );

        let env = globals.clone();

        Self {
            globals,
            env,
            locals: HashMap::new(),
        }
    }
}

impl Interpreter {
    pub fn interpret(&mut self, nodes: &Vec<Stmt>, locals: HashMap<Loc, usize>) -> InterpRes {
        self.locals = locals;

        let mut res = RtVal::new_null();

        for node in nodes {
            match node.accept(self) {
                Ok(r) => res = r,
                Err(e) => return Err(e),
            }
        }

        Ok(res)
    }
}

impl VisitStmt<Rc<RefCell<RtVal>>, InterpErr> for Interpreter {
    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> InterpRes {
        stmt.expr.accept(self)
    }

    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> InterpRes {
        let value = stmt.expr.accept(self)?;
        println!("{}", value.borrow());

        Ok(RtVal::new_null())
    }
    fn visit_var_decl_stmt(&mut self, stmt: &VarDeclStmt) -> InterpRes {
        let mut value = match &stmt.value {
            Some(v) => v.accept(self)?,
            None => RtVal::new_null(),
        };

        // We check for the case where we init a float with a real to be sure
        // to keep the 'float' information: var a: float = 1 + 2
        if let Some(VarTypeDecl::Identifier(i)) = &stmt.typ {
            if i.kind == TokenKind::FloatType {
                let mut val = None;

                if let RtVal::IntVal(v) = &*value.borrow() {
                    val = Some(v.value);
                }

                if let Some(i) = val {
                    value = RtVal::new_float(i as f64).into();
                }
            }
            
        }

        self.env
            .borrow_mut()
            .declare_var(stmt.name.value.clone(), value)
            .map_err(|e| {
                RizonResult::new(InterpErr::VarDeclEnv(e.to_string()), Some(stmt.loc.clone()))
            })?;

        Ok(RtVal::new_null())
    }

    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> InterpRes {
        let new_env = Env::new(Some(self.env.clone()));
        self.execute_block_stmt(&stmt.stmts, new_env)?;

        Ok(RtVal::new_null())
    }

    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> InterpRes {
        let cond = stmt.condition.accept(self)?;
        let tmp = &*cond.borrow();

        match tmp {
            RtVal::BoolVal(b) => match b.value {
                true => {
                    if let Some(t) = &stmt.then_branch {
                        t.accept(self)
                    } else {
                        Ok(RtVal::new_null())
                    }
                }
                false => {
                    if let Some(e) = &stmt.else_branch {
                        e.accept(self)
                    } else {
                        Ok(RtVal::new_null())
                    }
                }
            },
            _ => Err(RizonResult::new(
                InterpErr::NonBoolIfCond,
                Some(stmt.loc.clone()),
            )),
        }
    }

    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> InterpRes {
        loop {
            let cond = stmt.condition.accept(self)?;
            let tmp = &*cond.borrow();

            match tmp {
                RtVal::BoolVal(b) => match b.value {
                    true => {
                        stmt.body.accept(self)?;
                    }
                    false => break,
                },
                _ => {
                    return Err(RizonResult::new(
                        InterpErr::NonBoolWhileCond,
                        Some(stmt.loc.clone()),
                    ))
                }
            }
        }

        Ok(RtVal::new_null())
    }

    fn visit_for_stmt(&mut self, stmt: &ForStmt) -> InterpRes {
        let new_env = Env::new(Some(self.env.clone()));
        let prev_env = std::mem::replace(&mut self.env, Rc::new(RefCell::new(new_env)));

        self.visit_var_decl_stmt(&stmt.placeholder)?;
        let mut range = 0..stmt.range.start;

        if let Some(i) = stmt.range.end {
            range = stmt.range.start..i;
        }

        for i in range {
            self.env
                .borrow_mut()
                .assign(stmt.placeholder.name.value.clone(), RtVal::new_int(i).into())
                .map_err(|e| {
                    RizonResult::new(InterpErr::ForLoop(e.to_string()), Some(stmt.loc.clone()))
                })?;

            stmt.body.accept(self)?;
        }

        let _ = std::mem::replace(&mut self.env, prev_env);

        Ok(RtVal::new_null())
    }

    fn visit_fn_decl_stmt(&mut self, stmt: &FnDeclStmt) -> InterpRes {
        let func = RtVal::new_fn(stmt, self.env.clone());

        self.env
            .borrow_mut()
            .declare_var(stmt.name.value.clone(), func.into())
            .map_err(|_| {
                RizonResult::new(
                    InterpErr::VarDeclEnv(stmt.name.to_string()),
                    Some(stmt.loc.clone()),
                )
            })?;

        Ok(RtVal::new_null())
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> InterpRes {
        let mut value = RtVal::new_null();

        if let Some(v) = &stmt.value {
            value = v.accept(self)?;
        }

        Err(RizonResult::new(InterpErr::Return(value), None))
    }
    
    fn visit_struct_stmt(&mut self, stmt: &StructStmt) -> InterpRes {
        let mut methods: HashMap<EcoString, Function> = HashMap::new();
        stmt.methods.iter().for_each(|m| {
            methods.insert(m.name.value.clone(), Function::new(m, self.env.clone()));
        });

        // Two steps: declaring and assigning. Allow the struct to reference itself
        // in its methods
        self.env
            .borrow_mut()
            .declare_var(stmt.name.value.clone(), RtVal::new_null())
            .map_err(|_| {
                RizonResult::new(
                    InterpErr::VarDeclEnv(stmt.name.to_string()),
                    Some(stmt.loc.clone()),
                )
            })?;

        let mut fields: HashMap<EcoString, Rc<RefCell<RtVal>>> = HashMap::new();
        for f in &stmt.fields {
            let value = match &f.value {
                Some(v) => v.accept(self)?,
                None => RtVal::new_null(),
            };

            fields.insert(f.name.value.clone(), value);
        }

        let struct_val = RtVal::new_struct(stmt, fields, methods);

        self.env
            .borrow_mut()
            .assign(stmt.name.value.clone(), Rc::new(RefCell::new(struct_val.clone())))
            .map_err(|_| {
                RizonResult::new(
                    InterpErr::VarDeclEnv(stmt.name.to_string()),
                    Some(stmt.loc.clone()),
                )
            })?;

        Ok(RtVal::new_null())
    }
}

impl Interpreter {
    pub fn execute_block_stmt(&mut self, stmts: &Vec<Stmt>, env: Env) -> InterpRes {
        let prev_env = std::mem::replace(&mut self.env, Rc::new(RefCell::new(env)));

        let mut res = Ok(RtVal::new_null());
        for s in stmts {
            res = s.accept(self);

            if res.is_err() {
                break;
            }
        }

        let _ = std::mem::replace(&mut self.env, prev_env);

        res
    }
}

impl VisitExpr<Rc<RefCell<RtVal>>, InterpErr> for Interpreter {
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> InterpRes {
        let lhs = expr.left.accept(self)?;
        if lhs == RtVal::new_null() {
            return Err(RizonResult::new(
                InterpErr::UninitializedValue,
                Some(expr.left.get_loc()),
            ));
        }

        let rhs = expr.right.accept(self)?;
        if rhs == RtVal::new_null() {
            return Err(RizonResult::new(
                InterpErr::UninitializedValue,
                Some(expr.right.get_loc()),
            ));
        }

        let tmp = rhs.borrow();
        let tmp2 = lhs.borrow();

        match tmp2.operate(&tmp, &expr.operator.value) {
            Ok(res) => Ok(res.into()),
            Err(e) => Err(RizonResult::new(
                InterpErr::OperationEvaluation(e.to_string()),
                Some(expr.right.get_loc()),
            )),
        }
    }

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> InterpRes {
        let value = expr.value.accept(self)?;

        match self.locals.get(&expr.loc) {
            Some(i) => self
                .env
                .borrow_mut()
                .assign_at(expr.name.clone(), value.clone(), i)
                .map_err(|e| {
                    RizonResult::new(InterpErr::AssignEnv(e.to_string()), Some(expr.loc.clone()))
                })?,
            None => self.globals.borrow_mut().assign(expr.name.clone(), value.clone()).map_err(|e| {
                RizonResult::new(InterpErr::AssignEnv(e.to_string()), Some(expr.loc.clone()))
            })?,
        }

        Ok(value)
    }

    fn visit_grouping_expr(&mut self, expr: &GroupingExpr) -> InterpRes {
        expr.expr.accept(self)
    }

    fn visit_int_literal_expr(&mut self, expr: &IntLiteralExpr) -> InterpRes {
        Ok(RtVal::new_int(expr.value).into())
    }

    fn visit_float_literal_expr(&mut self, expr: &FloatLiteralExpr) -> InterpRes {
        Ok(RtVal::new_float(expr.value).into())
    }

    fn visit_str_literal_expr(&mut self, expr: &StrLiteralExpr) -> InterpRes {
        Ok(RtVal::new_str(expr.value.clone()).into())
    }

    fn visit_identifier_expr(&mut self, expr: &IdentifierExpr) -> InterpRes {
        match expr.name.as_str() {
            "true" => Ok(RtVal::new_bool(true).into()),
            "false" => Ok(RtVal::new_bool(false).into()),
            "null" => Ok(RtVal::new_null()),
            _ => match self.locals.get(&expr.loc) {
                Some(i) => self
                    .env
                    .borrow()
                    .get_var_at(expr.name.clone(), i)
                    .map_err(|e| {
                        RizonResult::new(InterpErr::GetVarEnv(e.to_string()), Some(expr.loc.clone()))
                    }),
                None => self.globals.borrow().get_var(expr.name.clone()).map_err(|e| {
                    RizonResult::new(InterpErr::GetVarEnv(e.to_string()), Some(expr.loc.clone()))
                }),
            },
        }
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> InterpRes {
        let value = expr.right.accept(self)?;

        match expr.operator.kind {
            TokenKind::Bang => match &mut *value.borrow_mut() {
                RtVal::BoolVal(v) => v.negate(),
                _ => return Err(RizonResult::new(
                    InterpErr::BangOpOnNonBool,
                    Some(expr.right.get_loc()),
                ))
            }
            TokenKind::Minus => match &mut *value.borrow_mut() {
                RtVal::IntVal(v) => v.negate(),
                RtVal::FloatVal(v) => v.negate(),
                _ => return Err(RizonResult::new(
                    InterpErr::NegateNonNumeric,
                    Some(expr.right.get_loc()),
                ))
            },
            _ => return Err(RizonResult::new(
                    InterpErr::UnknownUnaryOp(expr.operator.value.clone().into()),
                    Some(expr.right.get_loc()),
                ))
        }

        Ok(value)
    }

    fn visit_logical_expr(&mut self, expr: &LogicalExpr) -> InterpRes {
        let left = expr.left.accept(self)?;

        if expr.operator.kind == TokenKind::Or {
            match &*left.clone().borrow() {
                RtVal::BoolVal(b) => {
                    if b.value {
                        return Ok(left)
                    }
                }
                _ => {
                    return Err(RizonResult::new(
                        InterpErr::NonBoolIfCond,
                        Some(expr.loc.clone()),
                    ))
                }
            }
        } else if expr.operator.kind == TokenKind::And {
            match &*left.clone().borrow() {
                RtVal::BoolVal(b) => {
                    if !b.value {
                        return Ok(left)
                    }
                }
                _ => {
                    return Err(RizonResult::new(
                        InterpErr::NonBoolIfCond,
                        Some(expr.loc.clone()),
                    ))
                }
            }
        }

        expr.right.accept(self)
    }

    fn visit_call_expr(&mut self, expr: &CallExpr) -> InterpRes {
        let callee = expr.callee.accept(self)?;

        let mut args: Vec<Rc<RefCell<RtVal>>> = vec![];
        for a in &expr.args {
            args.push(a.accept(self)?);
        }

        let tmp = &*callee.borrow();

        let callable: Box<&dyn Callable> = match tmp {
            RtVal::FuncVal(f) => Box::new(f),
            RtVal::NativeFnVal(f) => Box::new(f),
            RtVal::StructVal(s) => Box::new(s),
            _ => return Err(RizonResult::new(InterpErr::NonFnCall, Some(expr.callee.get_loc())))
        };

        // TODO: already done by static analysis
        if callable.arity() != args.len() {
            return Err(RizonResult::new(
                InterpErr::WrongArgsNb(callable.arity(), args.len()),
                Some(expr.loc.clone()),
            ));
        }

        callable.call(self, args).map_err(|e| {
            RizonResult::new(InterpErr::FnCall(e.err.to_string()), Some(expr.loc.clone()))
        })
    }
    
    fn visit_get_expr(&mut self, expr: &GetExpr) -> InterpRes {
        let obj = expr.object.accept(self)?;
        let tmp = &*obj.borrow();

        if let RtVal::InstanceVal(inst) = tmp {
            // Field
            if let Some(v) = inst.fields.get(&expr.name.value) {
                Ok(v.clone())
            // Methods
            } else if let Some(m) = inst.strukt.borrow().methods.get(&expr.name.value) {
                Ok(m.wrap_bind(obj.clone()))
            } else {
                Err(RizonResult::new(InterpErr::InexistantField(expr.name.value.clone()), Some(expr.loc.clone())))
            }
        } else {
            Err(RizonResult::new(InterpErr::NonInstPropAccess, Some(expr.loc.clone())))
        }
    }
    
    fn visit_set_expr(&mut self, expr: &SetExpr) -> InterpRes {
        let obj = expr.object.accept(self)?;
        let mut tmp = obj.borrow_mut();

        match &mut *tmp {
            RtVal::InstanceVal(inst) => {
                let val = expr.value.accept(self)?;

                inst.set(expr.name.value.clone(), val.clone())
                    .map_err(|e| RizonResult::new(InterpErr::InexistantFieldBis(e.to_string()), Some(expr.loc.clone())))?;

                Ok(val)
            },
            _ => Err(RizonResult::new(InterpErr::NonInstPropAccess, Some(expr.loc.clone())))
        }
    }
    
    fn visit_self_expr(&mut self, expr: &SelfExpr) -> InterpRes {
        self.env.borrow()
            .get_var(expr.name.clone())
            .map_err(|e| {
                    RizonResult::new(InterpErr::GetVarEnv(e.to_string()), Some(expr.loc.clone()))
            })
    }
    
    fn visit_is_expr(&mut self, expr: &IsExpr) -> InterpRes {
        let value = expr.left.accept(self)?;
        let tmp = value.borrow();

        Ok(tmp.is_of_type(&expr.typ.value).into())
    }
}

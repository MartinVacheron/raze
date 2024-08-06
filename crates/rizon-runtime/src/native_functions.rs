use colored::*;
use ecow::EcoString;
use std::{cell::RefCell, fmt, rc::Rc};
use thiserror::Error;

use crate::{
    callable::{CallRes, Callable},
    interpreter::Interpreter,
    values::RtVal,
};
use rizon_tools::results::RizonReport;


#[derive(Debug, Error)]
pub enum NativeFnErr {
    #[error("time access failed")]
    GetTime,
}

impl RizonReport for NativeFnErr {
    fn get_err_msg(&self) -> String {
        format!("{} {}", "Function error:".red(), self)
    }
}


#[derive(Clone)]
pub struct RizonNativeFn {
    pub name: EcoString,
    pub arity: usize,
    pub func: fn(&mut Interpreter, Vec<Rc<RefCell<RtVal>>>) -> CallRes,
}

impl Callable for RizonNativeFn {
    fn arity(&self) -> usize {
        self.arity
    }

    fn call(&self, interp: &mut Interpreter, args: Vec<Rc<RefCell<RtVal>>>) -> CallRes {
        (self.func)(interp, args)
    }
}


impl fmt::Debug for RizonNativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn>")
    }
}

impl PartialEq for RizonNativeFn {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name 
    }
}

impl fmt::Display for RizonNativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn>")
    }
}

use colored::*;
use ecow::EcoString;
use std::{cell::RefCell, fmt, rc::Rc};
use thiserror::Error;

use crate::{
    callable::{CallRes, Callable},
    interpreter::Interpreter,
    values::RtVal,
};
use tools::results::RevReport;


#[derive(Debug, Error)]
pub enum NativeFnErr {
    #[error("time access failed")]
    GetTime,
}

impl RevReport for NativeFnErr {
    fn get_err_msg(&self) -> String {
        format!("{} {}", "Function error:".red(), self)
    }
}


#[derive(Clone)]
pub struct RevNativeFn {
    pub name: EcoString,
    pub arity: usize,
    pub func: fn(&mut Interpreter, Vec<Rc<RefCell<RtVal>>>) -> CallRes,
}

impl Callable for RevNativeFn {
    fn arity(&self) -> usize {
        self.arity
    }

    fn call(&self, interp: &mut Interpreter, args: Vec<Rc<RefCell<RtVal>>>) -> CallRes {
        (self.func)(interp, args)
    }
}


impl fmt::Debug for RevNativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn>")
    }
}

impl PartialEq for RevNativeFn {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name 
    }
}

impl fmt::Display for RevNativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn>")
    }
}

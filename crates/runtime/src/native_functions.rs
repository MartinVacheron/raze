use colored::*;
use ecow::EcoString;
use std::fmt;
use thiserror::Error;

use crate::{
    callable::{ CallRes, Callable},
    interpreter::Interpreter,
    values::RtVal,
};
use tools::results::PhyReport;


#[derive(Debug, Error)]
pub enum NativeFnErr {
    #[error("time access failed")]
    GetTime,
}

impl PhyReport for NativeFnErr {
    fn get_err_msg(&self) -> String {
        format!("{} {}", "Function error:".red(), self)
    }
}


#[derive(Clone)]
pub struct PhyNativeFn {
    pub name: EcoString,
    pub arity: usize,
    pub func: fn(&mut Interpreter, Vec<RtVal>) -> CallRes,
}

impl Callable for PhyNativeFn {
    fn arity(&self) -> usize {
        self.arity
    }

    fn call(&self, interp: &mut Interpreter, args: Vec<RtVal>) -> CallRes {
        (self.func)(interp, args)
    }
}


impl fmt::Debug for PhyNativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn>")
    }
}

impl PartialEq for PhyNativeFn {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name 
    }
}

impl fmt::Display for PhyNativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn>")
    }
}

use std::collections::{
    HashMap,
    hash_map::Entry::Vacant,
};
use ecow::EcoString;
use thiserror::Error;
use colored::*;

use crate::{results::{PhyReport, PhyResult}, values::RtVal};


// -----------------
//  Error managment
// -----------------
#[derive(Error, Debug)]
pub enum EnvErr {
    #[error("variable '{0}' is already declared")]
    AlreadyDeclaredVar(String),

    #[error("undeclared variable '{0}'")]
    UndeclaredVar(String),
}

impl PhyReport for EnvErr {
    fn get_err_msg(&self) -> String {
        format!("{} {}", "Environment error:".red(), self)
    }
}

pub type PhyResEnv = PhyResult<EnvErr>;


// -------------
//  Environment
// -------------
#[derive(Default)]
pub struct Env {
    pub vars: HashMap<EcoString, RtVal>,
}

impl Env {
    pub fn declare_var(&mut self, var_name: EcoString, value: RtVal) -> Result<(), PhyResEnv> {
        if let Vacant(v) = self.vars.entry(var_name.clone()) {
            v.insert(value);
        } else {
            return Err(PhyResult::new(EnvErr::AlreadyDeclaredVar(var_name.into()), None))
        }

        Ok(())
    }

    pub fn get_var(&self, var_name: EcoString) -> Result<RtVal, PhyResEnv> {
        match self.vars.get(&var_name) {
            Some(v) => Ok(v.clone()),
            None => Err(PhyResult::new(EnvErr::UndeclaredVar(var_name.into()), None))
        }
    }
}


#[cfg(test)]
mod tests {
    use ecow::EcoString;
    use crate::{environment::EnvErr, values::RtVal};
    use super::Env;

    #[test]
    fn var_declaration() {
        let mut env = Env::default();
        assert!(env.declare_var(EcoString::from("foo"), RtVal::new_null()).is_ok());
        assert!(matches!(
            env.declare_var(EcoString::from("foo"), RtVal::new_null()).err().unwrap().err,
            EnvErr::AlreadyDeclaredVar { .. }
        ));
    }

    #[test]
    fn get_var() {
        let mut env = Env::default();
        assert!(env.declare_var(EcoString::from("foo"), 3.into()).is_ok());
        assert_eq!(env.get_var(EcoString::from("foo")).unwrap(), 3.into());
        assert!(matches!(
            env.get_var(EcoString::from("bar")).err().unwrap().err,
            EnvErr::UndeclaredVar { .. }
        ));
    }
}

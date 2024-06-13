use std::collections::{
    HashMap,
    hash_map::Entry::{Vacant, Occupied},
};
use ecow::EcoString;
use thiserror::Error;

use crate::values::RtVal;


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


// -------------
//  Environment
// -------------
#[derive(Default)]
pub struct Env<'a> {
    enclosing: Option<&'a mut Env<'a>>,
    pub vars: HashMap<EcoString, RtVal>,
}

impl<'a> Env<'a> {
    pub fn declare_var(&mut self, var_name: EcoString, value: RtVal) -> Result<(), EnvErr> {
        if let Vacant(v) = self.vars.entry(var_name.clone()) {
            v.insert(value);
        } else {
            return Err(EnvErr::AlreadyDeclaredVar(var_name.into()))
        }

        Ok(())
    }

    pub fn get_var(&self, var_name: EcoString) -> Result<RtVal, EnvErr> {
        match self.vars.get(&var_name) {
            Some(v) => Ok(v.clone()),
            None => {
                if let Some(enclo) = &self.enclosing {
                    enclo.get_var(var_name)
                } else {
                    Err(EnvErr::UndeclaredVar(var_name.into()))
                }
            }
        }
    }

    pub fn assign(&mut self, var_name: EcoString, value: RtVal) -> Result<(), EnvErr> {
        if let Occupied(mut v) = self.vars.entry(var_name.clone()) {
            v.insert(value);
            Ok(())
        } else {
            if let Some(enclo) = &mut self.enclosing {
                enclo.assign(var_name, value)
            } else {
                Err(EnvErr::UndeclaredVar(var_name.into()))
            }
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
            env.declare_var(EcoString::from("foo"), RtVal::new_null()).err().unwrap(),
            EnvErr::AlreadyDeclaredVar { .. }
        ));
    }

    #[test]
    fn get_var() {
        let mut env = Env::default();
        assert!(env.declare_var(EcoString::from("foo"), 3.into()).is_ok());
        assert_eq!(env.get_var(EcoString::from("foo")).unwrap(), 3.into());
        assert!(matches!(
            env.get_var(EcoString::from("bar")).err().unwrap(),
            EnvErr::UndeclaredVar { .. }
        ));
    }
}

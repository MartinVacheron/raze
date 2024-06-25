use ecow::EcoString;
use std::{
    cell::RefCell,
    collections::{
        hash_map::Entry::{Occupied, Vacant},
        HashMap,
    },
    rc::Rc,
};
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

type EnvWrapper = Rc<RefCell<Env>>;

// -------------
//  Environment
// -------------
#[derive(Debug, Default)]
pub struct Env {
    pub enclosing: Option<Rc<RefCell<Env>>>,
    pub vars: HashMap<EcoString, RtVal>,
}

impl Env {
    pub fn new(enclosing: Option<EnvWrapper>) -> Self {
        Self {
            enclosing,
            vars: HashMap::new(),
        }
    }

    pub fn declare_var(&mut self, var_name: EcoString, value: RtVal) -> Result<(), EnvErr> {
        if let Vacant(v) = self.vars.entry(var_name.clone()) {
            v.insert(value);
        } else {
            return Err(EnvErr::AlreadyDeclaredVar(var_name.into()));
        }

        Ok(())
    }

    pub fn get_var(&self, var_name: EcoString) -> Result<RtVal, EnvErr> {
        match self.vars.get(&var_name) {
            Some(v) => Ok(v.clone()),
            None => {
                if let Some(enclo) = &self.enclosing {
                    enclo.borrow().get_var(var_name)
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
        } else if let Some(enclo) = &self.enclosing {
            enclo.borrow_mut().assign(var_name, value)
        } else {
            Err(EnvErr::UndeclaredVar(var_name.into()))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Env;
    use crate::{environment::EnvErr, values::RtVal};
    use ecow::EcoString;

    #[test]
    fn var_declaration() {
        let mut env = Env::default();
        assert!(env
            .declare_var(EcoString::from("foo"), RtVal::new_null())
            .is_ok());
        assert!(matches!(
            env.declare_var(EcoString::from("foo"), RtVal::new_null())
                .err()
                .unwrap(),
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

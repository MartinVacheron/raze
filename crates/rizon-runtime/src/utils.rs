use std::cell::RefCell;
use std::rc::Rc;

use rizon_static_analysis::utils::lex_parse_resolve;
use rizon_frontend::parser::utils::lex_and_parse;

use crate::values::RtVal;
use crate::interpreter::{Interpreter, RizonResInterp};


pub fn lex_parse_resolve_interp(code: &str) -> Result<Rc<RefCell<RtVal>>, RizonResInterp> {
    let locals = lex_parse_resolve(code).unwrap();
    let nodes = lex_and_parse(code).unwrap();
    let mut interp = Interpreter::new();
    interp.interpret(&nodes, locals)
}

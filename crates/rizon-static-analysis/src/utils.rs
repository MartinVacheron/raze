use std::collections::HashMap;

use rizon_frontend::parser::utils::lex_and_parse;
use rizon_tools::results::Loc;

use crate::resolver::{RizonResResolv, Resolver};


pub fn lex_parse_resolve(code: &str) -> Result<HashMap<Loc, usize>, Vec<RizonResResolv>> {
    let nodes = lex_and_parse(code).unwrap();
    let mut resolver = Resolver::default();
    resolver.resolve(&nodes)
}

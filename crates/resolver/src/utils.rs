use std::collections::HashMap;

use frontend::parser::utils::lex_and_parse;
use tools::results::Loc;

use crate::resolver::{RevResResolv, Resolver};


pub fn lex_parse_resolve(code: &str) -> Result<HashMap<Loc, usize>, Vec<RevResResolv>> {
    let nodes = lex_and_parse(code).unwrap();
    let mut resolver = Resolver::default();
    resolver.resolve(&nodes)
}

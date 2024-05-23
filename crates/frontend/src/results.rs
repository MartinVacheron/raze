use std::{collections::VecDeque, fmt::Display};

use colored::*;
use super::lexer::Loc;


struct ReportContext<'a> {
    line: usize,
    snippets: VecDeque<(usize, &'a str)>,
    offset: usize,
}

pub struct ArcResult {
    kind: ArcResultKind,
    msg: String,
    loc: Loc,
}

#[derive(Debug)]
pub enum ArcResultKind {
    LexerErr,
}

impl Display for ArcResultKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArcResultKind::LexerErr => write!(f, "{}", "Lexer error".red()),
        }
    }
}

impl<'a> ArcResult {
    pub fn lexer_error(msg: String, loc: Loc) -> Self {
        ArcResult {
            kind: ArcResultKind::LexerErr,
            msg,
            loc
        }
    }
    
    pub fn report(&self, file_name: &String, code: &String) {
        let cx = self.get_context(code);
        let deco = self.get_decorators(&cx);

        println!("{}: {}", self.kind, self.msg);
        println!("  {} {} [line {}]", "-->".cyan(), file_name, cx.line);

        for (i, line) in cx.snippets {
            println!(" {} {}", format!("{} |", i).cyan(), line);
        }

        // Here, 3 is for space between line nb and '|' and space again
        let margin = cx.line.to_string().len() + 3;
        println!("{}{}\n", " ".repeat(margin), deco.red());
    }

    fn get_context(&'a self, code: &'a String) -> ReportContext {
        let mut offset: usize = 0;
        let mut lines: VecDeque<(usize, &'a str)> = VecDeque::new();

        for (i, line) in code.split('\n').enumerate() {
            lines.push_back((i + 1, line));

            if self.loc.start > offset && self.loc.start < offset + line.len() {
                return ReportContext { line: i + 1, snippets: lines, offset }
            } else {
                if lines.len() == 2 {
                    lines.pop_front();
                }

                offset += line.len() + 1;
            }
        }

        panic!("Code snippet not found while reporting error: {}", self.msg)
    }
    
    fn get_decorators(&self, cx: &ReportContext) -> String {
        let mut decorators = " ".repeat(self.loc.start - cx.offset);
        let indicators = "^".repeat(self.loc.get_len());

        decorators.push_str(indicators.as_str());
        decorators
    }
}

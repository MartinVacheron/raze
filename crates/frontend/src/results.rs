use std::collections::VecDeque;

use colored::*;
use super::lexer::Loc;


pub trait PhyReport {
    fn get_err_msg(&self) -> String;
}

struct ReportContext<'a> {
    line: usize,
    snippets: VecDeque<(usize, &'a str)>,
    offset: usize,
}

#[derive(Debug)]
pub struct PhyResult<T: PhyReport> {
    pub err: T,
    pub loc: Option<Loc>,
}

impl<'a, T: PhyReport> PhyResult<T> {
    pub fn new(err: T, loc: Option<Loc>) -> PhyResult<T> {
        PhyResult { err, loc }
    }

    pub fn report(&self, file_name: &String, code: &str) {
        // Error msg
        println!("{}", self.err.get_err_msg());

        // Additional infos on location
        if let Some(loc) = &self.loc {
            let cx = self.get_context(code, loc);
            let deco = self.get_decorators(&cx, loc);

            println!("  {} {} [line {}]", "-->".cyan(), file_name, cx.line);

            for (i, line) in cx.snippets {
                // If this line + 1 is % 10, the next one will be one digit
                // longer, so we add a space before the smallest
                let add_space = if (i + 1) % 10 == 0 { " " } else { "" };

                println!(" {} {}", format!("{}{} |", add_space, i).cyan(), line);
            }

            // Here, 4 is for space at the beginning and between line nb and '|' and space again
            let margin = cx.line.to_string().len() + 4;
            print!("{}{}\n", " ".repeat(margin), deco.red());
        }
    }

    fn get_context(&'a self, code: &'a str, loc: &Loc) -> ReportContext {
        let mut offset: usize = 0;
        let mut lines: VecDeque<(usize, &'a str)> = VecDeque::new();

        for (i, line) in code.split('\n').enumerate() {
            lines.push_back((i + 1, line));

            if loc.start >= offset && loc.start < offset + line.len() {
                return ReportContext { line: i + 1, snippets: lines, offset }
            } else {
                if lines.len() == 2 {
                    lines.pop_front();
                }

                // + 1 because we don't have '\n' anymore
                offset += line.len() + 1;
            }
        }

        panic!("Code snippet not found while reporting error: {}", self.err.get_err_msg())
    }
    
    fn get_decorators(&self, cx: &ReportContext, loc: &Loc) -> String {
        let mut decorators = " ".repeat(loc.start - cx.offset);
        let indicators = "^".repeat(loc.get_len());

        decorators.push_str(indicators.as_str());
        decorators
    }
}

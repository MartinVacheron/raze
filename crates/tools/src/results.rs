use std::collections::VecDeque;
use colored::*;


#[derive(Debug, PartialEq, Default, Clone)]
pub struct Loc {
    pub start: usize,
    pub end: usize
}

impl Loc {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn new_len_one_from_start(loc: Loc) -> Self {
        Self { start: loc.start, end: loc.start }
    }
    
    pub fn get_len(&self) -> usize {
        self.end - self.start + 1
    }
}

pub trait RevReport {
    fn get_err_msg(&self) -> String;
}

struct ReportContext<'a> {
    line: usize,
    snippets: VecDeque<(usize, &'a str)>,
    offset: usize,
}

#[derive(Debug)]
pub struct RevResult<T: RevReport> {
    pub err: T,
    pub loc: Option<Loc>,
}

impl<'a, T: RevReport> RevResult<T> {
    pub fn new(err: T, loc: Option<Loc>) -> RevResult<T> {
        RevResult { err, loc }
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
                let add_space = if (i+1).to_string().len() - i.to_string().len() == 1 {
                    " "
                } else {
                    ""
                };

                println!(" {} {}", format!("{}{} |", add_space, i).cyan(), line);
            }

            // Here, 4 is for space at the beginning and between line nb and '|' and space again
            let margin = cx.line.to_string().len() + 4;
            println!("{}{}", " ".repeat(margin), deco.red());
        }
    }

    fn get_context(&'a self, code: &'a str, loc: &Loc) -> ReportContext {
        let mut offset: usize = 0;
        let mut lines: VecDeque<(usize, &'a str)> = VecDeque::new();

        for (i, line) in code.split('\n').enumerate() {
            lines.push_back((i + 1, line));

            if loc.start >= offset && loc.start < offset + line.len() {
                return ReportContext {
                    line: i + 1,
                    snippets: lines,
                    offset,
                };
            } else {
                if lines.len() == 2 {
                    lines.pop_front();
                }

                // + 1 because we don't have '\n' anymore
                offset += line.len() + 1;
            }
        }

        panic!(
            "Code snippet not found while reporting error: {}",
            self.err.get_err_msg()
        )
    }

    fn get_decorators(&self, cx: &ReportContext, loc: &Loc) -> String {
        let mut decorators = " ".repeat(loc.start - cx.offset);
        let indicators = "^".repeat(loc.get_len());

        decorators.push_str(indicators.as_str());
        decorators
    }
}

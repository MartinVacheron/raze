use std::{collections::VecDeque, fmt::Display};

use colored::*;
use super::lexer::Loc;


struct ReportContext<'a> {
    line: usize,
    snippets: VecDeque<(usize, &'a str)>,
    offset: usize,
}

#[derive(Debug)]
pub struct RazeResult {
    pub kind: RazeResultKind,
    msg: String,
}

#[derive(Debug, PartialEq)]
pub enum RazeResultKind {
    LexerErr {
        loc: Loc
    },
    ParserErr {
        loc: Loc
    },
    InternalErr,
}

impl Display for RazeResultKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RazeResultKind::LexerErr { .. } => write!(f, "{}", "Lexer error".red()),
            RazeResultKind::ParserErr { .. } => write!(f, "{}", "Parser error".red()),
            RazeResultKind::InternalErr => write!(f, "{}", "Internal error".red()),
        }
    }
}

impl<'a> RazeResult {
    pub fn lexer_error(msg: String, loc: Loc) -> Self {
        RazeResult {
            kind: RazeResultKind::LexerErr { loc },
            msg,
        }
    }

    pub fn parser_error(msg: String, loc: Loc) -> Self {
        RazeResult {
            kind: RazeResultKind::ParserErr { loc },
            msg,
        }
    }

    pub fn internal_error(msg: String) -> Self {
        RazeResult {
            kind: RazeResultKind::InternalErr,
            msg,
        }
    }
    
    pub fn report(&self, file_name: &String, code: &str) {
        // Error msg
        println!("{}: {}", self.kind, self.msg);

        // Additional infos
        match &self.kind {
            RazeResultKind::LexerErr { loc }
            | RazeResultKind::ParserErr { loc } => {
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
                println!("{}{}\n", " ".repeat(margin), deco.red());
            }
            _ => {}
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

        panic!("Code snippet not found while reporting error: {}", self.msg)
    }
    
    fn get_decorators(&self, cx: &ReportContext, loc: &Loc) -> String {
        let mut decorators = " ".repeat(loc.start - cx.offset);
        let indicators = "^".repeat(loc.get_len());

        decorators.push_str(indicators.as_str());
        decorators
    }
}

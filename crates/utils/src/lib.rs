use colored::*;


pub enum RazeResult {
    LexerErr {
        msg: String,
        line: usize,
        start: usize,
        end: usize
    },
}

impl RazeResult {
    pub fn lexer_error(msg: String, line: usize, start: usize, end: usize) -> Self {
        let err = RazeResult::LexerErr { msg, line, start, end };
        err.report();
        err
    }
    
    pub fn report(&self) {
        match self {
            RazeResult::LexerErr { msg, line, .. } => {
                println!("[line {}] {}: {}.", line, "Error".red().bold(), msg);
            }
        }
    }
}

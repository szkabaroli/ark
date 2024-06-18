pub use self::ast::NodeId;
pub use self::error::{ParseError, ParseErrorWithLocation};
pub use self::green::{GreenElement, GreenNode, GreenToken, GreenTreeBuilder};
pub use self::lexer::lex;
pub use self::span::Span;
pub use self::token::TokenKind;

pub mod ast;
pub mod parser;
mod error;
mod green;
mod lexer;
mod span;
mod token;

pub fn compute_line_starts(content: &str) -> Vec<u32> {
    let mut pos: u32 = 0;
    let mut line_starts = vec![0];
    for ch in content.chars() {
        if ch == '\n' {
            line_starts.push(pos + 1);
        }
        pos += 1;
    }
    line_starts
}

pub fn compute_line_column(line_starts: &[u32], offset: u32) -> (u32, u32) {
    let result = line_starts.binary_search(&offset);
    match result {
        Ok(idx) => {
            let idx: u32 = idx.try_into().expect("overflow");
            (idx + 1, 1)
        }
        Err(idx) => {
            let line_start = line_starts[idx - 1];
            (idx.try_into().expect("overflow"), offset - line_start + 1)
        }
    }
}
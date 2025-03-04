use std::collections::HashMap;

use crate::source_file::SourceFileId;
use crate::{error::ParseErrorWithLocation, token::TokenKind};
use crate::{ParseError, Span, TokenKind::*};

pub struct LexerResult {
    pub tokens: Vec<TokenKind>,
    pub widths: Vec<u32>,
    pub errors: Vec<ParseErrorWithLocation>,
}

pub fn lex(source_id: SourceFileId, content: &str) -> LexerResult {
    let mut lexer = Lexer::new(source_id, content);
    let mut tokens = Vec::new();
    let mut widths = Vec::new();

    while !lexer.is_eof() {
        let start = lexer.offset();
        let token = lexer.read_token();
        assert!(token < TokenKind::EOF);
        let end = lexer.offset();
        tokens.push(token);
        widths.push(end - start);
    }

    LexerResult {
        tokens,
        widths,
        errors: lexer.errors,
    }
}

struct Lexer<'a> {
    source_id: SourceFileId,
    content: &'a str,
    offset: usize,
    keywords: HashMap<&'static str, TokenKind>,
    errors: Vec<ParseErrorWithLocation>,
    open_braces: Vec<usize>,
}

impl<'a> Lexer<'a> {
    fn new(source_id: SourceFileId, content: &str) -> Lexer {
        let keywords = keywords_in_map();

        Lexer {
            source_id,
            offset: 0,
            content,
            keywords,
            errors: Vec::new(),
            open_braces: Vec::new(),
        }
    }

    fn read_token(&mut self) -> TokenKind {
        let ch = self.curr().expect("end of file reached");
        let ch = Some(ch);

        if is_whitespace(ch) {
            self.read_white_space()
        } else if is_digit(ch) {
            self.read_number()
        } else if self.is_line_comment() {
            self.read_line_comment()
        } else if self.is_multiline_comment() {
            self.read_multiline_comment()
        } else if is_identifier_start(ch) {
            self.read_identifier()
        } else if is_quote(ch) {
            self.read_string(false)
        } else if is_char_quote(ch) {
            self.read_char_literal()
        } else if is_operator(ch) {
            self.read_operator()
        } else {
            self.read_unknown_char()
        }
    }

    fn read_unknown_char(&mut self) -> TokenKind {
        let start = self.offset();
        let ch = self.curr().expect("missing char");
        self.eat_char();
        let span = self.span_from(start);
        self.report_error_at(ParseError::UnknownChar(ch), span);
        UNKNOWN
    }

    fn read_white_space(&mut self) -> TokenKind {
        while is_whitespace(self.curr()) {
            self.eat_char();
        }

        WHITESPACE
    }

    fn read_line_comment(&mut self) -> TokenKind {
        while !self.curr().is_none() && !is_newline(self.curr()) {
            self.eat_char();
        }

        LINE_COMMENT
    }

    fn read_multiline_comment(&mut self) -> TokenKind {
        let start = self.offset();

        self.eat_char();
        self.eat_char();

        while !self.curr().is_none() && !self.is_multi_comment_end() {
            self.eat_char();
        }

        if self.curr().is_none() {
            let span = self.span_from(start);
            self.report_error_at(ParseError::UnclosedComment, span);
        }

        self.eat_char();
        self.eat_char();

        MULTILINE_COMMENT
    }

    fn read_identifier(&mut self) -> TokenKind {
        let value = self.read_identifier_as_string();

        let lookup = self.keywords.get(&value[..]).cloned();

        if let Some(tok_type) = lookup {
            tok_type
        } else if value == "_" {
            UNDERSCORE
        } else {
            IDENTIFIER
        }
    }

    fn read_identifier_as_string(&mut self) -> String {
        let mut value = String::new();

        while is_identifier(self.curr()) {
            let ch = self.curr().unwrap();
            self.eat_char();
            value.push(ch);
        }

        value
    }

    fn read_char_literal(&mut self) -> TokenKind {
        let start = self.offset();
        self.eat_char();

        while self.curr().is_some() && !is_char_quote(self.curr()) {
            self.read_escaped_char();
        }

        if is_char_quote(self.curr()) {
            self.eat_char();
        } else {
            let span = self.span_from(start);
            self.report_error_at(ParseError::UnclosedChar, span);
        }

        CHAR_LITERAL
    }

    fn read_escaped_char(&mut self) {
        if self.eat_char() == Some('\\') {
            self.eat_char();
        }
    }

    fn read_string(&mut self, continuation: bool) -> TokenKind {
        let mut start = self.offset();

        if continuation {
            // } was already consumed by read_operator().
            start -= '}'.len_utf8() as u32;
        } else {
            assert_eq!(self.curr(), Some('\"'));
            self.eat_char();
        }

        while self.curr().is_some() && !is_quote(self.curr()) {
            if self.curr() == Some('$') && self.lookahead() == Some('{') {
                self.eat_char();
                self.eat_char();

                self.open_braces.push(1);
                return TEMPLATE_LITERAL;
            }

            self.read_escaped_char();
        }

        if is_quote(self.curr()) {
            self.eat_char();
        } else {
            let span = self.span_from(start);
            self.report_error_at(ParseError::UnclosedString, span);
        }

        if continuation {
            TEMPLATE_END_LITERAL
        } else {
            STRING_LITERAL
        }
    }

    fn read_operator(&mut self) -> TokenKind {
        let ch = self.curr().unwrap();
        self.eat_char();

        let nch = self.curr().unwrap_or('x');
        let nnch = self.lookahead().unwrap_or('x');

        match ch {
            '+' => ADD,
            '-' => {
                if nch == '>' {
                    self.eat_char();
                    ARROW
                } else {
                    SUB
                }
            }
            '*' => MUL,
            '/' => DIV,
            '%' => MODULO,

            '(' => L_PAREN,
            ')' => R_PAREN,
            '[' => L_BRACKET,
            ']' => R_BRACKET,
            '{' => {
                if let Some(open_braces_top) = self.open_braces.last_mut() {
                    *open_braces_top += 1;
                }
                L_BRACE
            }
            '}' => {
                if let Some(open_braces_top) = self.open_braces.last_mut() {
                    *open_braces_top -= 1;

                    if *open_braces_top == 0 {
                        self.open_braces.pop();
                        return self.read_string(true);
                    }
                }
                R_BRACE
            }

            '|' => {
                if nch == '|' {
                    self.eat_char();
                    OR_OR
                } else {
                    OR
                }
            }

            '&' => {
                if nch == '&' {
                    self.eat_char();
                    AND_AND
                } else {
                    AND
                }
            }

            '^' => CARET,
            ',' => COMMA,
            ';' => SEMICOLON,
            ':' => {
                if nch == ':' {
                    self.eat_char();
                    COLON_COLON
                } else {
                    COLON
                }
            }
            '.' => {
                if nch == '.' && nnch == '.' {
                    self.eat_char();
                    self.eat_char();

                    DOT_DOT_DOT
                } else {
                    DOT
                }
            }
            '=' => {
                if nch == '=' {
                    self.eat_char();

                    if nnch == '=' {
                        self.eat_char();
                        EQ_EQ_EQ
                    } else {
                        EQ_EQ
                    }
                } else if nch == '>' {
                    self.eat_char();
                    DOUBLE_ARROW
                } else {
                    EQ
                }
            }

            '<' => match nch {
                '=' => {
                    self.eat_char();
                    LE
                }

                '<' => {
                    self.eat_char();
                    LT_LT
                }

                _ => LT,
            },

            '>' => match nch {
                '=' => {
                    self.eat_char();
                    GE
                }

                '>' => {
                    self.eat_char();

                    if nnch == '>' {
                        self.eat_char();
                        GT_GT_GT
                    } else {
                        GT_GT
                    }
                }

                _ => GT,
            },
            '!' => {
                if nch == '=' {
                    self.eat_char();

                    if nnch == '=' {
                        self.eat_char();
                        NOT_EQ_EQ
                    } else {
                        NOT_EQ
                    }
                } else {
                    NOT
                }
            }
            '@' => AT,

            _ => {
                unreachable!()
            }
        }
    }

    fn read_number(&mut self) -> TokenKind {
        let base = if self.curr() == Some('0') {
            let next = self.lookahead();

            match next {
                Some('x') => {
                    self.eat_char();
                    self.eat_char();

                    16
                }

                Some('b') => {
                    self.eat_char();
                    self.eat_char();

                    2
                }

                _ => 10,
            }
        } else {
            10
        };

        self.read_digits(base);

        if base == 10 && self.curr() == Some('.') && is_digit(self.lookahead()) {
            return self.read_number_as_float();
        }

        if is_identifier_start(self.curr()) {
            self.read_identifier_as_string();
        }

        INT_LITERAL
    }

    fn read_number_as_float(&mut self) -> TokenKind {
        self.eat_char();

        self.read_digits(10);

        if self.curr() == Some('e') || self.curr() == Some('E') {
            self.eat_char();

            if self.curr() == Some('+') || self.curr() == Some('-') {
                self.eat_char();
            }

            self.read_digits(10);
        }

        if is_identifier_start(self.curr()) {
            self.read_identifier_as_string();
        }

        FLOAT_LITERAL
    }

    fn span_from(&self, start: u32) -> Span {
        Span::new(self.source_id, start, self.offset() - start)
    }

    fn read_digits(&mut self, base: u32) {
        while is_digit_or_underscore(self.curr(), base) {
            self.eat_char();
        }
    }

    fn offset(&self) -> u32 {
        self.offset.try_into().expect("overflow")
    }

    fn eat_char(&mut self) -> Option<char> {
        let curr = self.curr();

        if let Some(ch) = curr {
            self.offset += ch.len_utf8();
            Some(ch)
        } else {
            None
        }
    }

    fn is_eof(&self) -> bool {
        self.offset == self.content.len()
    }

    fn curr(&self) -> Option<char> {
        if self.offset < self.content.len() {
            self.content[self.offset..].chars().next()
        } else {
            None
        }
    }

    fn lookahead(&self) -> Option<char> {
        if self.offset < self.content.len() {
            let mut it = self.content[self.offset..].chars();
            it.next();
            it.next()
        } else {
            None
        }
    }

    fn report_error_at(&mut self, msg: ParseError, span: Span) {
        self.errors.push(ParseErrorWithLocation::new(span, msg));
    }

    fn is_line_comment(&self) -> bool {
        self.curr() == Some('/') && self.lookahead() == Some('/')
    }

    fn is_multiline_comment(&self) -> bool {
        self.curr() == Some('/') && self.lookahead() == Some('*')
    }

    fn is_multi_comment_end(&self) -> bool {
        self.curr() == Some('*') && self.lookahead() == Some('/')
    }
}

fn is_digit(ch: Option<char>) -> bool {
    ch.map(|ch| ch.is_digit(10)).unwrap_or(false)
}

fn is_digit_or_underscore(ch: Option<char>, base: u32) -> bool {
    ch.map(|ch| ch.is_digit(base) || ch == '_').unwrap_or(false)
}

fn is_whitespace(ch: Option<char>) -> bool {
    ch.map(|ch| ch.is_whitespace()).unwrap_or(false)
}

fn is_newline(ch: Option<char>) -> bool {
    ch == Some('\n')
}

fn is_quote(ch: Option<char>) -> bool {
    ch == Some('\"')
}

fn is_char_quote(ch: Option<char>) -> bool {
    ch == Some('\'')
}

fn is_operator(ch: Option<char>) -> bool {
    ch.map(|ch| "^+-*/%&|,=!;:.()[]{}<>@".contains(ch))
        .unwrap_or(false)
}

fn is_identifier_start(ch: Option<char>) -> bool {
    match ch {
        Some(ch) => (ch >= 'a' && ch <= 'z') || ch == '_',
        _ => false,
    }
}

fn is_identifier(ch: Option<char>) -> bool {
    is_identifier_start(ch) || is_digit(ch)
}

fn keywords_in_map() -> HashMap<&'static str, TokenKind> {
    let mut keywords = HashMap::with_capacity(30);

    // literals
    keywords.insert("true", TRUE);
    keywords.insert("false", FALSE);

    // "big" shapes
    // keywords.insert("class", CLASS_KW);
    //keywords.insert("enum", ENUM_KW);
    //keywords.insert("trait", TRAIT_KW);
    //keywords.insert("impl", IMPL_KW);
    keywords.insert("struct", STRUCT_KW);
    keywords.insert("mod", MOD_KW);
    keywords.insert("import", IMPORT_KW);
    keywords.insert("package", PACKAGE_KW);

    // "small" shapes
    keywords.insert("fn", FN_KW);
    keywords.insert("flow", FLOW_KW);
    keywords.insert("node", NODE_KW);
    keywords.insert("let", LET_KW);
    //keywords.insert("mut", MUT_KW);
    //keywords.insert("const", CONST_KW);

    // control flow
    keywords.insert("return", RETURN_KW);
    keywords.insert("if", IF_KW);
    //keywords.insert("else", ELSE_KW);
    //keywords.insert("while", WHILE_KW);
    //keywords.insert("for", FOR_KW);
    //keywords.insert("in", IN_KW);
    //keywords.insert("break", BREAK_KW);
    //keywords.insert("continue", CONTINUE_KW);
    //keywords.insert("match", MATCH_KW);

    // qualifiers
    //keywords.insert("self", SELF_KW);
    //keywords.insert("super", SUPER_KW);
    keywords.insert("pub", PUB_KW);
    keywords.insert("static", STATIC_KW);

    // casting
    //keywords.insert("as", AS_KW);
    //keywords.insert("is", IS_KW);

    keywords.insert("type", TYPE_KW);
    //keywords.insert("where", WHERE_KW);
    //keywords.insert("Self", UPCASE_SELF_KW);

    keywords
}

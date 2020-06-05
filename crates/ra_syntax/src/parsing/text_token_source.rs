//! FIXME: write short doc here

use ra_parser::Token as PToken;
use ra_parser::TokenSource;

use crate::{parsing::lexer::Token, SyntaxKind::EOF, TextRange, TextSize};
use std::cell::Cell;

pub(crate) struct TextTokenSource<'t> {
    text: &'t str,

    /// Index position of raw_tokens. This will never index a trivia token (whitespace and comment)
    curr_token_idx: usize,
    /// Start position in the text of the current non-trivia token.
    /// ```non-rust
    ///  struct Foo;
    /// ^------^---
    /// |      |  ^-
    /// 0      7  10
    /// ```
    curr_text_offset: TextSize,
    /// non-whitespace/comment token
    curr_token: Option<&'t Token>,
    curr_ptoken: PToken,
    raw_tokens: &'t [Token],
    lookahead_1: Cell<Option<PToken>>,
    lookahead_2: Cell<Option<PToken>>,
    lookahead_3: Cell<Option<PToken>>,
}

impl<'t> TokenSource for TextTokenSource<'t> {
    #[inline]
    fn current(&self) -> PToken {
        self.curr_ptoken
    }

    #[inline]
    fn lookahead_nth(&self, n: usize) -> PToken {
        let cache = |lookahead_n: &Cell<Option<PToken>>| {
            let existing = lookahead_n.get();
            if let Some(x) = existing {
                x
            } else {
                let new = self.get_token(n).0;
                lookahead_n.set(Some(new));
                new
            }
        };
        // eprintln!("lookahead_nth {}", n);
        match n {
            0 => self.current(),
            1 => cache(&self.lookahead_1),
            2 => cache(&self.lookahead_2),
            3 => cache(&self.lookahead_3),
            _ => self.get_token(n).0,
        }
    }

    #[inline]
    fn bump(&mut self) {
        if self.curr_ptoken.kind == EOF {
            return;
        }
        let (ptoken, new_offset, token, new_idx) = self.get_token(1);
        self.curr_ptoken = ptoken;
        self.curr_token = token;
        self.curr_token_idx = new_idx;
        self.curr_text_offset = new_offset;
        // trickle down the lookahead as we just bump'ed
        self.lookahead_1.set(self.lookahead_2.replace(self.lookahead_3.replace(None)));
    }

    fn is_keyword(&self, kw: &str) -> bool {
        if let Some(t) = self.curr_token {
            let range = TextRange::at(self.curr_text_offset, t.len);
            self.text[range] == *kw
        } else {
            return false;
        }
    }
}

impl<'t> TextTokenSource<'t> {
    /// Generate input from tokens(expect comment and whitespace).
    pub fn new(text: &'t str, raw_tokens: &'t [Token]) -> TextTokenSource<'t> {
        let dummy_ptoken = PToken { kind: EOF, is_jointed_to_next: false };
        let mut ret = TextTokenSource {
            text,
            curr_ptoken: dummy_ptoken,
            raw_tokens,
            curr_token_idx: 0,
            curr_text_offset: 0.into(),
            curr_token: raw_tokens.get(0),
            lookahead_1: Cell::new(None),
            lookahead_2: Cell::new(None),
            lookahead_3: Cell::new(None),
        };
        let get_token_res = ret.get_token(0);
        ret.curr_ptoken = get_token_res.0;
        ret.curr_text_offset = get_token_res.1;
        ret.curr_token = get_token_res.2;
        ret.curr_token_idx = get_token_res.3;
        ret
    }

    /// Converts a Token to a PToken. Also returns the start position in the text of the token, and the index of the token found
    fn get_token(&self, offset_from_curr: usize) -> (PToken, TextSize, Option<&'t Token>, usize) {
        let mut text_offset_from_cur: TextSize = 0.into();
        let mut found_non_trivia = 0;
        let mut found_token = None;
        // default to 1 in the case, it is not found. This will cause the index to be out of bounds, returning EOF
        let mut found_token_idx_offset = 1;
        for (idx, token) in self.raw_tokens.iter().skip(self.curr_token_idx).enumerate() {
            if !token.kind.is_trivia() {
                found_non_trivia += 1;
                // stop when we have found the current index token plus offset_from_curr non-trivial tokens
                if found_non_trivia == offset_from_curr + 1 {
                    found_token = Some(token);
                    found_token_idx_offset = idx;
                    break;
                };
            }
            text_offset_from_cur += token.len;
        }

        // if the next token after the token found is non-trivia, then mark the two as joined
        let is_jointed_to_next = self
            .raw_tokens
            .get(self.curr_token_idx + found_token_idx_offset + 1)
            .map(|next_token| !next_token.kind.is_trivia())
            .unwrap_or(false);

        let kind = found_token.map_or(EOF, |t| t.kind);
        (
            PToken { kind, is_jointed_to_next },
            self.curr_text_offset + text_offset_from_cur,
            found_token,
            self.curr_token_idx + found_token_idx_offset,
        )
    }
}

//! This is the actual "grammar" of the Rust language.
//!
//! Each function in this module and its children corresponds
//! to a production of the formal grammar. Submodules roughly
//! correspond to different *areas* of the grammar. By convention,
//! each submodule starts with `use super::*` import and exports
//! "public" productions via `pub(super)`.
//!
//! See docs for `Parser` to learn about API, available to the grammar,
//! and see docs for `Event` to learn how this actually manages to
//! produce parse trees.
//!
//! Code in this module also contains inline tests, which start with
//! `// test name-of-the-test` comment and look like this:
//!
//! ```
//! // test function_with_zero_parameters
//! // fn foo() {}
//! ```
//!
//! After adding a new inline-test, run `cargo xtask codegen` to
//! extract it as a standalone text-fixture into
//! `crates/ra_syntax/test_data/parser/`, and run `cargo test` once to
//! create the "gold" value.
//!
//! Coding convention: rules like `where_clause` always produce either a
//! node or an error, rules like `opt_where_clause` may produce nothing.
//! Non-opt rules typically start with `assert!(p.at(FIRST_TOKEN))`, the
//! caller is responsible for branching on the first token.
mod attributes;
mod expressions;
mod items;
mod params;
mod paths;
mod patterns;
mod type_args;
mod type_params;
mod types;

use crate::{
    parser::{Marker, Parser, PrecedableMarker},
    SyntaxKind::{self, *},
    TokenSet,
};

pub(crate) fn root(p: &mut Parser) {
    p.with_sealed(SOURCE_FILE, |p| {
        p.eat(SHEBANG);
        items::mod_contents(p, false);
    });
}

/// Various pieces of syntax that can be parsed by macros by example
pub(crate) mod fragments {
    use super::*;

    pub(crate) use super::{
        expressions::block_expr, paths::type_path as path, patterns::pattern, types::type_,
    };

    pub(crate) fn expr(p: &mut Parser) {
        let _ = expressions::expr(p);
    }

    pub(crate) fn stmt(p: &mut Parser) {
        expressions::stmt(p, expressions::StmtWithSemi::No)
    }

    pub(crate) fn opt_visibility(p: &mut Parser) {
        let _ = super::opt_visibility(p);
    }

    // Parse a meta item , which excluded [], e.g : #[ MetaItem ]
    pub(crate) fn meta_item(p: &mut Parser) {
        fn is_delimiter(p: &mut Parser) -> bool {
            match p.current() {
                T!['{'] | T!['('] | T!['['] => true,
                _ => false,
            }
        }

        if is_delimiter(p) {
            items::token_tree(p);
            return;
        }

        p.with_sealed(TOKEN_TREE, |p| {
            while !p.at(EOF) {
                if is_delimiter(p) {
                    items::token_tree(p);
                    break;
                } else {
                    // https://doc.rust-lang.org/reference/attributes.html
                    // https://doc.rust-lang.org/reference/paths.html#simple-paths
                    // The start of an meta must be a simple path
                    match p.current() {
                        IDENT | T![::] | T![super] | T![self] | T![crate] => p.bump_any(),
                        T![=] => {
                            p.bump_any();
                            match p.current() {
                                c if c.is_literal() => p.bump_any(),
                                T![true] | T![false] => p.bump_any(),
                                _ => {}
                            }
                            break;
                        }
                        _ => break,
                    }
                }
            }
        });
    }

    pub(crate) fn item(p: &mut Parser) {
        items::item_or_macro(p, true, items::ItemFlavor::Mod)
    }

    pub(crate) fn macro_items(p: &mut Parser) {
        p.with_sealed(MACRO_ITEMS, |p| {
            items::mod_contents(p, false);
        });
    }

    // start
    // e1
    // e2
    //  start
    //  e3
    //  finish
    // e4
    // finish

    pub(crate) fn macro_stmts(p: &mut Parser) {
        p.with_sealed(MACRO_STMTS, |p| {
            while !p.at(EOF) {
                if p.at(T![;]) {
                    p.bump(T![;]);
                    continue;
                }

                expressions::stmt(p, expressions::StmtWithSemi::Optional);
            }
        });
    }
}

pub(crate) fn reparser(
    node: SyntaxKind,
    first_child: Option<SyntaxKind>,
    parent: Option<SyntaxKind>,
) -> Option<fn(&mut Parser)> {
    let res = match node {
        BLOCK_EXPR => expressions::block_expr,
        RECORD_FIELD_DEF_LIST => items::record_field_def_list,
        RECORD_FIELD_LIST => items::record_field_list,
        ENUM_VARIANT_LIST => items::enum_variant_list,
        MATCH_ARM_LIST => items::match_arm_list,
        USE_TREE_LIST => items::use_tree_list,
        EXTERN_ITEM_LIST => items::extern_item_list,
        TOKEN_TREE if first_child? == T!['{'] => items::token_tree,
        ITEM_LIST => match parent? {
            IMPL_DEF => items::impl_item_list,
            TRAIT_DEF => items::trait_item_list,
            MODULE => items::mod_item_list,
            _ => return None,
        },
        _ => return None,
    };
    Some(res)
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum BlockLike {
    Block,
    NotBlock,
}

impl BlockLike {
    fn is_block(self) -> bool {
        self == BlockLike::Block
    }
}

fn opt_visibility(p: &mut Parser) -> bool {
    match p.current() {
        T![pub] => {
            p.with_sealed(VISIBILITY, |p| {
                p.bump(T![pub]);
                if p.at(T!['(']) {
                    match p.nth(1) {
                        // test crate_visibility
                        // pub(crate) struct S;
                        // pub(self) struct S;
                        // pub(self) struct S;
                        // pub(self) struct S;
                        T![crate] | T![self] | T![super] => {
                            p.bump_any();
                            p.bump_any();
                            p.expect(T![')']);
                        }
                        T![in] => {
                            p.bump_any();
                            p.bump_any();
                            paths::use_path(p);
                            p.expect(T![')']);
                        }
                        _ => (),
                    }
                }
            });
        }
        // test crate_keyword_vis
        // crate fn main() { }
        // struct S { crate field: u32 }
        // struct T(crate u32);
        //
        // test crate_keyword_path
        // fn foo() { crate::foo(); }
        T![crate] if !p.nth_at(1, T![::]) => {
            p.with_sealed(VISIBILITY, |p| {
                p.bump(T![crate]);
            });
        }
        _ => return false,
    }
    true
}

fn opt_alias(p: &mut Parser) {
    if p.at(T![as]) {
        p.with_sealed(ALIAS, |p| {
            p.bump(T![as]);
            if !p.eat(T![_]) {
                name(p);
            }
        })
    }
}

fn abi(p: &mut Parser) {
    assert!(p.at(T![extern]));
    p.with_sealed(ABI, |p| {
        p.bump(T![extern]);
        match p.current() {
            STRING | RAW_STRING => p.bump_any(),
            _ => (),
        }
    })
}

fn opt_fn_ret_type(p: &mut Parser) -> bool {
    if p.at(T![->]) {
        p.with_sealed(RET_TYPE, |p| {
            p.bump(T![->]);
            types::type_no_bounds(p);
        });
        true
    } else {
        false
    }
}

fn name_r(p: &mut Parser, recovery: TokenSet) {
    if p.at(IDENT) {
        p.with_sealed(NAME, |p| {
            p.bump(IDENT);
        })
    } else {
        p.err_recover("expected a name", recovery);
    }
}

fn name(p: &mut Parser) {
    name_r(p, TokenSet::EMPTY)
}

fn name_ref(p: &mut Parser) {
    if p.at(IDENT) {
        p.with_sealed(NAME_REF, |p| {
            p.bump(IDENT);
        })
    } else if p.at(T![self]) {
        p.with_sealed(T![self], |p| {
            p.bump(T![self]);
        });
    } else {
        p.err_and_bump("expected identifier");
    }
}

fn name_ref_or_index(p: &mut Parser) {
    assert!(p.at(IDENT) || p.at(INT_NUMBER));
    p.with_sealed(NAME_REF, |p| {
        p.bump_any();
    });
}

fn error_block(p: &mut Parser, message: &str) {
    assert!(p.at(T!['{']));
    p.with_sealed(ERROR, |p| {
        p.error(message);
        p.bump(T!['{']);
        expressions::expr_block_contents(p);
        p.eat(T!['}']);
    });
}

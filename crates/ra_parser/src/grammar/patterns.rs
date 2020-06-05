//! FIXME: write short doc here

use super::*;

pub(super) const PATTERN_FIRST: TokenSet = expressions::LITERAL_FIRST
    .union(paths::PATH_FIRST)
    .union(token_set![BOX_KW, REF_KW, MUT_KW, L_PAREN, L_BRACK, AMP, UNDERSCORE, MINUS, DOT]);

pub(crate) fn pattern(p: &mut Parser) {
    pattern_r(p, PAT_RECOVERY_SET);
}

/// Parses a pattern list separated by pipes `|`
pub(super) fn pattern_top(p: &mut Parser) {
    pattern_top_r(p, PAT_RECOVERY_SET)
}

pub(crate) fn pattern_single(p: &mut Parser) {
    pattern_single_r(p, PAT_RECOVERY_SET);
}

/// Parses a pattern list separated by pipes `|`
/// using the given `recovery_set`
pub(super) fn pattern_top_r(p: &mut Parser, recovery_set: TokenSet) {
    p.eat(T![|]);
    pattern_r(p, recovery_set);
}

/// Parses a pattern list separated by pipes `|`, with no leading `|`,using the
/// given `recovery_set`
// test or_pattern
// fn main() {
//     match () {
//         (_ | _) => (),
//         &(_ | _) => (),
//         (_ | _,) => (),
//         [_ | _,] => (),
//     }
// }
fn pattern_r(p: &mut Parser, recovery_set: TokenSet) {
    let m = p.start_precedable();
    pattern_single_r(p, recovery_set);

    if !p.at(T![|]) {
        m.abandon(p);
        return;
    }
    while p.eat(T![|]) {
        pattern_single_r(p, recovery_set);
    }
    m.complete_sealed(p, OR_PAT);
}

fn pattern_single_r(p: &mut Parser, recovery_set: TokenSet) {
    if let Some(lhs) = atom_pat(p, recovery_set) {
        // test range_pat
        // fn main() {
        //     match 92 {
        //         0 ... 100 => (),
        //         101 ..= 200 => (),
        //         200 .. 301=> (),
        //     }
        // }
        for &range_op in [T![...], T![..=], T![..]].iter() {
            if p.at(range_op) {
                let m = lhs.precede(p);
                p.bump(range_op);
                atom_pat(p, recovery_set);
                m.complete_sealed(p, RANGE_PAT);
                return;
            }
        }
    }
}

const PAT_RECOVERY_SET: TokenSet =
    token_set![LET_KW, IF_KW, WHILE_KW, LOOP_KW, MATCH_KW, R_PAREN, COMMA];

fn atom_pat(p: &mut Parser, recovery_set: TokenSet) -> Option<PrecedableMarker> {
    let m = match p.nth(0) {
        T![box] => box_pat(p),
        T![ref] | T![mut] => bind_pat(p, true),
        IDENT => match p.nth(1) {
            // Checks the token after an IDENT to see if a pattern is a path (Struct { .. }) or macro
            // (T![x]).
            T!['('] | T!['{'] | T![!] => path_or_macro_pat(p),
            T![:] if p.nth_at(1, T![::]) => path_or_macro_pat(p),
            _ => bind_pat(p, true),
        },

        _ if paths::is_use_path_start(p) => path_or_macro_pat(p),
        _ if is_literal_pat_start(p) => literal_pat(p),

        T![.] if p.at(T![..]) => dot_dot_pat(p),
        T![_] => placeholder_pat(p),
        T![&] => ref_pat(p),
        T!['('] => tuple_pat(p),
        T!['['] => slice_pat(p),

        _ => {
            p.err_recover("expected pattern", recovery_set);
            return None;
        }
    };

    Some(m)
}

fn is_literal_pat_start(p: &Parser) -> bool {
    p.at(T![-]) && (p.nth(1) == INT_NUMBER || p.nth(1) == FLOAT_NUMBER)
        || p.at_ts(expressions::LITERAL_FIRST)
}

// test literal_pattern
// fn main() {
//     match () {
//         -1 => (),
//         92 => (),
//         'c' => (),
//         "hello" => (),
//     }
// }
fn literal_pat(p: &mut Parser) -> PrecedableMarker {
    assert!(is_literal_pat_start(p));
    let m = p.start_precedable();
    if p.at(T![-]) {
        p.bump(T![-]);
    }
    expressions::literal(p);
    m.complete_precedable(p, LITERAL_PAT)
}

// test path_part
// fn foo() {
//     let foo::Bar = ();
//     let ::Bar = ();
//     let Bar { .. } = ();
//     let Bar(..) = ();
// }
fn path_or_macro_pat(p: &mut Parser) -> PrecedableMarker {
    assert!(paths::is_use_path_start(p));
    p.with_precedable_marker(|p, m| {
        paths::expr_path(p);
        let kind = match p.current() {
            T!['('] => {
                tuple_pat_fields(p);
                TUPLE_STRUCT_PAT
            }
            T!['{'] => {
                record_field_pat_list(p);
                RECORD_PAT
            }
            // test marco_pat
            // fn main() {
            //     let m!(x) = 0;
            // }
            T![!] => {
                items::macro_call_after_excl(p);
                return m
                    .complete_precedable(p, MACRO_CALL)
                    .precede(p)
                    .complete_precedable(p, MACRO_PAT);
            }
            _ => PATH_PAT,
        };
        m.complete_precedable(p, kind)
    })
}

// test tuple_pat_fields
// fn foo() {
//     let S() = ();
//     let S(_) = ();
//     let S(_,) = ();
//     let S(_, .. , x) = ();
// }
fn tuple_pat_fields(p: &mut Parser) {
    assert!(p.at(T!['(']));
    p.bump(T!['(']);
    pat_list(p, T![')']);
    p.expect(T![')']);
}

// test record_field_pat_list
// fn foo() {
//     let S {} = ();
//     let S { f, ref mut g } = ();
//     let S { h: _, ..} = ();
//     let S { h: _, } = ();
// }
fn record_field_pat_list(p: &mut Parser) {
    assert!(p.at(T!['{']));
    p.with_sealed(RECORD_FIELD_PAT_LIST, |p| {
        p.bump(T!['{']);
        while !p.at(EOF) && !p.at(T!['}']) {
            match p.current() {
                // A trailing `..` is *not* treated as a DOT_DOT_PAT.
                T![.] if p.at(T![..]) => p.bump(T![..]),
                T!['{'] => error_block(p, "expected ident"),

                c => {
                    p.with_sealed(RECORD_FIELD_PAT, |p| {
                        match c {
                            // test record_field_pat
                            // fn foo() {
                            //     let S { 0: 1 } = ();
                            //     let S { x: 1 } = ();
                            // }
                            IDENT | INT_NUMBER if p.nth(1) == T![:] => {
                                name_ref_or_index(p);
                                p.bump(T![:]);
                                pattern(p);
                            }
                            T![box] => {
                                // FIXME: not all box patterns should be allowed
                                box_pat(p);
                            }
                            _ => {
                                bind_pat(p, false);
                            }
                        }
                    });
                }
            }
            if !p.at(T!['}']) {
                p.expect(T![,]);
            }
        }
        p.expect(T!['}']);
    });
}

// test placeholder_pat
// fn main() { let _ = (); }
fn placeholder_pat(p: &mut Parser) -> PrecedableMarker {
    assert!(p.at(T![_]));
    let m = p.start_precedable();
    p.bump(T![_]);
    m.complete_precedable(p, PLACEHOLDER_PAT)
}

// test dot_dot_pat
// fn main() {
//     let .. = ();
//     //
//     // Tuples
//     //
//     let (a, ..) = ();
//     let (a, ..,) = ();
//     let Tuple(a, ..) = ();
//     let Tuple(a, ..,) = ();
//     let (.., ..) = ();
//     let Tuple(.., ..) = ();
//     let (.., a, ..) = ();
//     let Tuple(.., a, ..) = ();
//     //
//     // Slices
//     //
//     let [..] = ();
//     let [head, ..] = ();
//     let [head, tail @ ..] = ();
//     let [head, .., cons] = ();
//     let [head, mid @ .., cons] = ();
//     let [head, .., .., cons] = ();
//     let [head, .., mid, tail @ ..] = ();
//     let [head, .., mid, .., cons] = ();
// }
fn dot_dot_pat(p: &mut Parser) -> PrecedableMarker {
    assert!(p.at(T![..]));
    let m = p.start_precedable();
    p.bump(T![..]);
    m.complete_precedable(p, DOT_DOT_PAT)
}

// test ref_pat
// fn main() {
//     let &a = ();
//     let &mut b = ();
// }
fn ref_pat(p: &mut Parser) -> PrecedableMarker {
    assert!(p.at(T![&]));
    let m = p.start_precedable();
    p.bump(T![&]);
    p.eat(T![mut]);
    pattern_single(p);
    m.complete_precedable(p, REF_PAT)
}

// test tuple_pat
// fn main() {
//     let (a, b, ..) = ();
//     let (a,) = ();
//     let (..) = ();
//     let () = ();
// }
fn tuple_pat(p: &mut Parser) -> PrecedableMarker {
    assert!(p.at(T!['(']));
    let m = p.start_precedable();
    p.bump(T!['(']);
    let mut has_comma = false;
    let mut has_pat = false;
    let mut has_rest = false;
    while !p.at(EOF) && !p.at(T![')']) {
        has_pat = true;
        if !p.at_ts(PATTERN_FIRST) {
            p.error("expected a pattern");
            break;
        }
        has_rest |= p.at(T![..]);

        pattern(p);
        if !p.at(T![')']) {
            has_comma = true;
            p.expect(T![,]);
        }
    }
    p.expect(T![')']);

    m.complete_precedable(p, if !has_comma && !has_rest && has_pat { PAREN_PAT } else { TUPLE_PAT })
}

// test slice_pat
// fn main() {
//     let [a, b, ..] = [];
// }
fn slice_pat(p: &mut Parser) -> PrecedableMarker {
    assert!(p.at(T!['[']));
    let m = p.start_precedable();
    p.bump(T!['[']);
    pat_list(p, T![']']);
    p.expect(T![']']);
    m.complete_precedable(p, SLICE_PAT)
}

fn pat_list(p: &mut Parser, ket: SyntaxKind) {
    while !p.at(EOF) && !p.at(ket) {
        if !p.at_ts(PATTERN_FIRST) {
            p.error("expected a pattern");
            break;
        }

        pattern(p);
        if !p.at(ket) {
            p.expect(T![,]);
        }
    }
}

// test bind_pat
// fn main() {
//     let a = ();
//     let mut b = ();
//     let ref c = ();
//     let ref mut d = ();
//     let e @ _ = ();
//     let ref mut f @ g @ _ = ();
// }
fn bind_pat(p: &mut Parser, with_at: bool) -> PrecedableMarker {
    let m = p.start_precedable();
    p.eat(T![ref]);
    p.eat(T![mut]);
    name(p);
    if with_at && p.eat(T![@]) {
        pattern_single(p);
    }
    m.complete_precedable(p, BIND_PAT)
}

// test box_pat
// fn main() {
//     let box i = ();
//     let box Outer { box i, j: box Inner(box &x) } = ();
//     let box ref mut i = ();
// }
fn box_pat(p: &mut Parser) -> PrecedableMarker {
    assert!(p.at(T![box]));
    let m = p.start_precedable();
    p.bump(T![box]);
    pattern_single(p);
    m.complete_precedable(p, BOX_PAT)
}

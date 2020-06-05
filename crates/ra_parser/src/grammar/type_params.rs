//! FIXME: write short doc here

use super::*;
use crate::parser::{Precedable, Sealed};

pub(super) fn opt_type_param_list(p: &mut Parser) {
    if !p.at(T![<]) {
        return;
    }
    type_param_list(p);
}

fn type_param_list(p: &mut Parser) {
    assert!(p.at(T![<]));
    p.with_sealed(TYPE_PARAM_LIST, |p| {
        p.bump(T![<]);

        while !p.at(EOF) && !p.at(T![>]) {
            let m = p.start_precedable();

            // test generic_lifetime_type_attribute
            // fn foo<#[derive(Lifetime)] 'a, #[derive(Type)] T>(_: &'a T) {
            // }
            attributes::outer_attributes(p);

            match p.current() {
                LIFETIME => lifetime_param(p, m.into()),
                IDENT => type_param(p, m.into()),
                CONST_KW => type_const_param(p, m.into()),
                _ => {
                    m.abandon(p);
                    p.err_and_bump("expected type parameter")
                }
            }
            if !p.at(T![>]) && !p.expect(T![,]) {
                break;
            }
        }
        p.expect(T![>]);
    });
}

fn lifetime_param(p: &mut Parser, m: Marker<Sealed>) {
    assert!(p.at(LIFETIME));
    p.bump(LIFETIME);
    if p.at(T![:]) {
        lifetime_bounds(p);
    }
    m.complete_sealed(p, LIFETIME_PARAM);
}

fn type_param(p: &mut Parser, m: Marker<Sealed>) {
    assert!(p.at(IDENT));
    name(p);
    if p.at(T![:]) {
        bounds(p);
    }
    // test type_param_default
    // struct S<T = i32>;
    if p.at(T![=]) {
        p.bump(T![=]);
        types::type_(p)
    }
    m.complete_sealed(p, TYPE_PARAM);
}

// test const_param
// struct S<const N: u32>;
fn type_const_param(p: &mut Parser, m: Marker<Sealed>) {
    assert!(p.at(CONST_KW));
    p.bump(T![const]);
    name(p);
    types::ascription(p);
    m.complete_sealed(p, CONST_PARAM);
}

// test type_param_bounds
// struct S<T: 'a + ?Sized + (Copy)>;
pub(super) fn bounds(p: &mut Parser) {
    assert!(p.at(T![:]));
    p.bump(T![:]);
    bounds_without_colon(p);
}

fn lifetime_bounds(p: &mut Parser) {
    assert!(p.at(T![:]));
    p.bump(T![:]);
    while p.at(LIFETIME) {
        p.bump(LIFETIME);
        if !p.eat(T![+]) {
            break;
        }
    }
}

pub(super) fn bounds_without_colon_m(
    p: &mut Parser,
    marker: Marker<Precedable>,
) -> PrecedableMarker {
    while type_bound(p) {
        if !p.eat(T![+]) {
            break;
        }
    }

    marker.complete_precedable(p, TYPE_BOUND_LIST)
}

pub(super) fn bounds_without_colon(p: &mut Parser) {
    let m = p.start_precedable();
    bounds_without_colon_m(p, m);
}

fn type_bound(p: &mut Parser) -> bool {
    let m = p.start_precedable();
    let has_paren = p.eat(T!['(']);
    p.eat(T![?]);
    match p.current() {
        LIFETIME => p.bump(LIFETIME),
        T![for] => types::for_type(p),
        _ if paths::is_use_path_start(p) => types::path_type_(p, false),
        _ => {
            m.abandon(p);
            return false;
        }
    }
    if has_paren {
        p.expect(T![')']);
    }
    m.complete_sealed(p, TYPE_BOUND);

    true
}

// test where_clause
// fn foo()
// where
//    'a: 'b + 'c,
//    T: Clone + Copy + 'static,
//    Iterator::Item: 'a,
//    <T as Iterator>::Item: 'a
// {}
pub(super) fn opt_where_clause(p: &mut Parser) {
    if !p.at(T![where]) {
        return;
    }
    p.with_sealed(WHERE_CLAUSE, |p| {
        p.bump(T![where]);

        while is_where_predicate(p) {
            where_predicate(p);

            let comma = p.eat(T![,]);

            if is_where_clause_end(p) {
                break;
            }

            if !comma {
                p.error("expected comma");
            }
        }
    });
}

fn is_where_predicate(p: &mut Parser) -> bool {
    match p.current() {
        LIFETIME => true,
        T![impl] => false,
        token => types::TYPE_FIRST.contains(token),
    }
}

fn is_where_clause_end(p: &mut Parser) -> bool {
    match p.current() {
        T!['{'] | T![;] | T![=] => true,
        _ => false,
    }
}

fn where_predicate(p: &mut Parser) {
    p.with_sealed(WHERE_PRED, |p| {
        match p.current() {
            LIFETIME => {
                p.bump(LIFETIME);
                if p.at(T![:]) {
                    bounds(p);
                } else {
                    p.error("expected colon");
                }
            }
            T![impl] => {
                p.error("expected lifetime or type");
            }
            _ => {
                // test where_pred_for
                // fn test<F>()
                // where
                //    for<'a> F: Fn(&'a str)
                // { }
                types::type_(p);

                if p.at(T![:]) {
                    bounds(p);
                } else {
                    p.error("expected colon");
                }
            }
        }
    });
}

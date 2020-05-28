//! FIXME: write short doc here

use super::*;

pub(super) fn static_def(p: &mut Parser, m: Marker<Sealed>) {
    const_or_static(p, m, T![static], STATIC_DEF)
}

pub(super) fn const_def(p: &mut Parser, m: Marker<Sealed>) {
    const_or_static(p, m, T![const], CONST_DEF)
}

fn const_or_static(p: &mut Parser, m: Marker<Sealed>, kw: SyntaxKind, def: SyntaxKind) {
    assert!(p.at(kw));
    p.bump(kw);
    p.eat(T![mut]); // FIXME: validator to forbid const mut

    // Allow `_` in place of an identifier in a `const`.
    let is_const_underscore = kw == T![const] && p.eat(T![_]);
    if !is_const_underscore {
        name(p);
    }

    // test_err static_underscore
    // static _: i32 = 5;

    types::ascription(p);
    if p.eat(T![=]) {
        expressions::expr(p);
    }
    p.expect(T![;]);
    m.complete_sealed(p, def);
}

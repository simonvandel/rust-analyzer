//! FIXME: write short doc here

use std::{cell::Cell, marker::PhantomData, mem};

use drop_bomb::DropBomb;

use crate::{
    event::Event,
    ParseError,
    SyntaxKind::{self, EOF, ERROR, TOMBSTONE},
    TokenSet, TokenSource, TreeSink, T,
};
use mem::transmute;

/// `Parser` struct provides the low-level API for
/// navigating through the stream of tokens and
/// constructing the parse tree. The actual parsing
/// happens in the `grammar` module.
///
/// However, the result of this `Parser` is not a real
/// tree, but rather a flat stream of events of the form
/// "start expression, consume number literal,
/// finish expression". See `Event` docs for more.
pub(crate) struct Parser<'t> {
    token_source: &'t mut dyn TokenSource,
    events: Vec<Event>,
    steps: Cell<u32>,
    f: fn(&mut Parser),
    sink: &'t mut dyn TreeSink,
    // Stack of indices where the buffering of events started. This starts with empty. When empty, events are eagerly drained. When non-empty, events are buffered
    buffering_start_index: Vec<u32>,
    forward_parents: Vec<SyntaxKind>,
}

impl<'t> Parser<'t> {
    pub(super) fn new(
        token_source: &'t mut dyn TokenSource,
        f: fn(&mut Parser),
        sink: &'t mut dyn TreeSink,
    ) -> Parser<'t> {
        Parser {
            token_source,
            events: Vec::new(),
            steps: Cell::new(0),
            f,
            sink,
            buffering_start_index: Vec::new(),
            forward_parents: Vec::new(),
        }
    }

    // TODO naming
    pub(crate) fn finish(mut self) {
        let f = self.f;
        f(&mut self);
        eprintln!("finishing with len: {} cap: {}", self.events.len(), self.events.capacity());
        self.drain_events(0);
    }

    /// Returns the kind of the current token.
    /// If parser has already reached the end of input,
    /// the special `EOF` kind is returned.
    pub(crate) fn current(&self) -> SyntaxKind {
        self.nth(0)
    }

    /// Lookahead operation: returns the kind of the next nth
    /// token.
    pub(crate) fn nth(&self, n: usize) -> SyntaxKind {
        assert!(n <= 3);

        let steps = self.steps.get();
        assert!(steps <= 10_000_000, "the parser seems stuck");
        self.steps.set(steps + 1);

        self.token_source.lookahead_nth(n).kind
    }

    /// Checks if the current token is `kind`.
    pub(crate) fn at(&self, kind: SyntaxKind) -> bool {
        self.nth_at(0, kind)
    }

    pub(crate) fn nth_at(&self, n: usize, kind: SyntaxKind) -> bool {
        match kind {
            T![-=] => self.at_composite2(n, T![-], T![=]),
            T![->] => self.at_composite2(n, T![-], T![>]),
            T![::] => self.at_composite2(n, T![:], T![:]),
            T![!=] => self.at_composite2(n, T![!], T![=]),
            T![..] => self.at_composite2(n, T![.], T![.]),
            T![*=] => self.at_composite2(n, T![*], T![=]),
            T![/=] => self.at_composite2(n, T![/], T![=]),
            T![&&] => self.at_composite2(n, T![&], T![&]),
            T![&=] => self.at_composite2(n, T![&], T![=]),
            T![%=] => self.at_composite2(n, T![%], T![=]),
            T![^=] => self.at_composite2(n, T![^], T![=]),
            T![+=] => self.at_composite2(n, T![+], T![=]),
            T![<<] => self.at_composite2(n, T![<], T![<]),
            T![<=] => self.at_composite2(n, T![<], T![=]),
            T![==] => self.at_composite2(n, T![=], T![=]),
            T![=>] => self.at_composite2(n, T![=], T![>]),
            T![>=] => self.at_composite2(n, T![>], T![=]),
            T![>>] => self.at_composite2(n, T![>], T![>]),
            T![|=] => self.at_composite2(n, T![|], T![=]),
            T![||] => self.at_composite2(n, T![|], T![|]),

            T![...] => self.at_composite3(n, T![.], T![.], T![.]),
            T![..=] => self.at_composite3(n, T![.], T![.], T![=]),
            T![<<=] => self.at_composite3(n, T![<], T![<], T![=]),
            T![>>=] => self.at_composite3(n, T![>], T![>], T![=]),

            _ => self.token_source.lookahead_nth(n).kind == kind,
        }
    }

    /// Consume the next token if `kind` matches.
    pub(crate) fn eat(&mut self, kind: SyntaxKind) -> bool {
        if !self.at(kind) {
            return false;
        }
        let n_raw_tokens = match kind {
            T![-=]
            | T![->]
            | T![::]
            | T![!=]
            | T![..]
            | T![*=]
            | T![/=]
            | T![&&]
            | T![&=]
            | T![%=]
            | T![^=]
            | T![+=]
            | T![<<]
            | T![<=]
            | T![==]
            | T![=>]
            | T![>=]
            | T![>>]
            | T![|=]
            | T![||] => 2,

            T![...] | T![..=] | T![<<=] | T![>>=] => 3,
            _ => 1,
        };
        self.do_bump(kind, n_raw_tokens);
        true
    }

    fn at_composite2(&self, n: usize, k1: SyntaxKind, k2: SyntaxKind) -> bool {
        let t1 = self.token_source.lookahead_nth(n);
        let t2 = self.token_source.lookahead_nth(n + 1);
        t1.kind == k1 && t1.is_jointed_to_next && t2.kind == k2
    }

    fn at_composite3(&self, n: usize, k1: SyntaxKind, k2: SyntaxKind, k3: SyntaxKind) -> bool {
        let t1 = self.token_source.lookahead_nth(n);
        let t2 = self.token_source.lookahead_nth(n + 1);
        let t3 = self.token_source.lookahead_nth(n + 2);
        (t1.kind == k1 && t1.is_jointed_to_next)
            && (t2.kind == k2 && t2.is_jointed_to_next)
            && t3.kind == k3
    }

    /// Checks if the current token is in `kinds`.
    pub(crate) fn at_ts(&self, kinds: TokenSet) -> bool {
        kinds.contains(self.current())
    }

    /// Checks if the current token is contextual keyword with text `t`.
    pub(crate) fn at_contextual_kw(&self, kw: &str) -> bool {
        self.token_source.is_keyword(kw)
    }

    /// Starts a new node in the syntax tree. All nodes and tokens
    /// consumed between the `start` and the corresponding `Marker::complete`
    /// belong to the same node.
    pub(crate) fn start(&mut self) -> Marker<Sealed> {
        // TODO start should be discontinued, but for now be conservative and do not be eager

        let m = self.start_internal();
        self.set_buffered(&m);
        m.into()
    }

    /// Starts a new node in the syntax tree. All nodes and tokens
    /// consumed between the `start` and the corresponding `Marker::complete`
    /// belong to the same node.
    pub(crate) fn start_precedable(&mut self) -> Marker<Precedable> {
        // TODO start should be discontinued, but for now be conservative and do not be eager

        let m = self.start_internal();
        self.set_buffered(&m);
        m
    }

    pub(crate) fn start_not_precedable(&mut self) -> Marker<Sealed> {
        eprintln!("start_not_precedable called ");
        let mut m = self.start_internal();
        // TODO can this be made with typestate?
        m.set_only_completable();
        m.into()
    }

    /// Starts a new node in the syntax tree. All nodes and tokens
    /// consumed between the `start` and the corresponding `Marker::complete`
    /// belong to the same node.
    fn start_internal(&mut self) -> Marker<Precedable> {
        eprintln!("start_internal called");

        let pos = self.events.len() as u32;

        self.push_event(Event::tombstone());
        Marker::<Precedable>::new(pos)
    }

    pub(crate) fn with_precedable_marker<F>(&mut self, f: F) -> PrecedableMarker
    where
        F: FnOnce(&mut Parser, Marker<Precedable>) -> PrecedableMarker,
    {
        eprintln!("with_precedable_marker called");

        let m = self.start_internal();
        self.set_buffered(&m);
        f(self, m)
    }

    pub(crate) fn with_sealed<F>(&mut self, kind: SyntaxKind, f: F)
    where
        F: FnOnce(&mut Parser),
    {
        eprintln!(
            "with_sealed called (len of buffering_idx: {})",
            self.buffering_start_index.len()
        );
        // TODO: Sealed behøver vel ikke pos?
        let mut m = Marker::<Sealed>::new(self.events.len() as u32);
        // set the kind since we know it - this makes it possible to eagerly drain the events

        let event = Event::Start { kind: kind, forward_parent: None };
        self.push_event(event);
        m.start_event_is_buffered = !self.buffering_start_index.is_empty();
        m.defuse();

        // with_sealed guarantees that it is not possible to precede the marker, so we don't have to handle forward parents
        // This also means that we can eagerly drain the events through the sink

        f(self);

        self.push_event(Event::Finish);
        if self.buffering_start_index.is_empty() {
            // self.drain_events(start_pos);
        }
    }

    fn drain_events(&mut self, start_pos: u32) {
        eprintln!("drain_events called ({}), ({})", start_pos, self.events.len());

        for i in start_pos as usize..self.events.len() {
            let e = mem::replace(&mut self.events[i], Event::tombstone());
            self.process_event(e, i);
        }

        self.events.drain(start_pos as usize..self.events.len());

        // TODO this can probably be removed
        self.forward_parents.clear();
    }

    fn process_event(&mut self, e: Event, i: usize) {
        match e {
            Event::Start { kind: TOMBSTONE, .. } => (),

            Event::Start { kind, forward_parent } => {
                // For events[A, B, C], B is A's forward_parent, C is B's forward_parent,
                // in the normal control flow, the parent-child relation: `A -> B -> C`,
                // while with the magic forward_parent, it writes: `C <- B <- A`.

                // append `A` into parents.
                self.forward_parents.push(kind);
                let mut idx = i;
                let mut fp = forward_parent;
                while let Some(fwd) = fp {
                    idx += fwd as usize;
                    // append `A`'s forward_parent `B`
                    fp = match mem::replace(&mut self.events[idx], Event::tombstone()) {
                        Event::Start { kind, forward_parent } => {
                            if kind != TOMBSTONE {
                                self.forward_parents.push(kind);
                            }
                            forward_parent
                        }
                        _ => unreachable!(),
                    };
                    // append `B`'s forward_parent `C` in the next stage.
                }

                for kind in self.forward_parents.drain(..).rev() {
                    eprintln!("start_node kind: {:?}", kind);
                    self.sink.start_node(kind);
                }
            }
            Event::Finish => dbg!(self.sink.finish_node()),
            Event::Token { kind, n_raw_tokens } => {
                eprintln!("token kind: {:?} n_raw_tokens: {}", kind, n_raw_tokens);
                self.sink.token(kind, n_raw_tokens);
            }
            Event::Error { msg } => {
                eprintln!("error msg: {:?}", msg.clone());
                self.sink.error(msg);
            }
        }
    }

    /// Consume the next token if `kind` matches.
    pub(crate) fn bump(&mut self, kind: SyntaxKind) {
        assert!(self.eat(kind));
    }

    /// Advances the parser by one token
    pub(crate) fn bump_any(&mut self) {
        let kind = self.nth(0);
        if kind == EOF {
            return;
        }
        self.do_bump(kind, 1)
    }

    /// Advances the parser by one token, remapping its kind.
    /// This is useful to create contextual keywords from
    /// identifiers. For example, the lexer creates an `union`
    /// *identifier* token, but the parser remaps it to the
    /// `union` keyword, and keyword is what ends up in the
    /// final tree.
    pub(crate) fn bump_remap(&mut self, kind: SyntaxKind) {
        if self.nth(0) == EOF {
            // FIXME: panic!?
            return;
        }
        self.do_bump(kind, 1);
    }

    /// Emit error with the `message`
    /// FIXME: this should be much more fancy and support
    /// structured errors with spans and notes, like rustc
    /// does.
    pub(crate) fn error<T: Into<String>>(&mut self, message: T) {
        let msg = ParseError(Box::new(message.into()));
        self.push_event(Event::Error { msg })
    }

    /// Consume the next token if it is `kind` or emit an error
    /// otherwise.
    pub(crate) fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.eat(kind) {
            return true;
        }
        self.error(format!("expected {:?}", kind));
        false
    }

    /// Create an error node and consume the next token.
    pub(crate) fn err_and_bump(&mut self, message: &str) {
        self.err_recover(message, TokenSet::EMPTY);
    }

    /// Create an error node and consume the next token.
    pub(crate) fn err_recover(&mut self, message: &str, recovery: TokenSet) {
        match self.current() {
            T!['{'] | T!['}'] => {
                self.error(message);
                return;
            }
            _ => (),
        }

        if self.at_ts(recovery) {
            self.error(message);
            return;
        }

        let m = self.start_internal();
        self.error(message);
        self.bump_any();
        m.complete_sealed(self, ERROR);
    }

    fn pop_buffering_index(&mut self) -> u32 {
        dbg!(self.buffering_start_index.pop().unwrap())
    }

    /// Mark the PrecedableMarker as sealed, so it cannot be preceded
    pub(crate) fn seal(&mut self, mut m: PrecedableMarker) {
        eprintln!("seal called");
        m.bomb.defuse();
        let idx_to_drain_from = self.pop_buffering_index();

        // the current marker is now sealed - if nothing is currently in buffereing mode, we can drain the events
        if self.buffering_start_index.is_empty() {
            debug_assert!(!self.events.is_empty());
            self.drain_events(idx_to_drain_from)
        }
    }

    fn do_bump(&mut self, kind: SyntaxKind, n_raw_tokens: u8) {
        for _ in 0..n_raw_tokens {
            self.token_source.bump();
        }

        self.push_event(Event::Token { kind, n_raw_tokens });
    }

    fn push_event(&mut self, event: Event) {
        eprintln!("push_event called {:?}", event);
        if self.buffering_start_index.is_empty() {
            self.events.push(event);
            self.drain_events(self.events.len() as u32)
        } else {
            self.events.push(event)
        }
    }

    pub(crate) fn set_buffered<T: MarkerType>(&mut self, m: &Marker<T>) {
        eprintln!("set_buffered {}", m.pos);
        self.buffering_start_index.push(m.pos);
    }
}

pub(crate) trait MarkerType {}

pub(crate) struct Precedable;
impl MarkerType for Precedable {}

pub(crate) struct Sealed;
impl MarkerType for Sealed {}

impl From<Marker<Precedable>> for Marker<Sealed> {
    fn from(x: Marker<Precedable>) -> Self {
        unsafe { transmute(x) }
    }
}

/// See `Parser::start`.
pub(crate) struct Marker<T>
where
    T: MarkerType,
{
    pos: u32,
    bomb: DropBomb,
    only_completable: bool,
    t: PhantomData<T>,
    start_event_is_buffered: bool,
}

impl<T: MarkerType> Marker<T> {
    fn new(pos: u32) -> Marker<T> {
        Marker {
            pos,
            bomb: DropBomb::new("Marker must be either completed or abandoned"),
            only_completable: true,
            t: Default::default(),
            start_event_is_buffered: true,
        }
    }

    pub(crate) fn set_only_completable(&mut self) {
        self.only_completable = false;
    }

    pub(crate) fn defuse(&mut self) {
        self.bomb.defuse();
    }

    /// Finishes the syntax tree node and assigns `kind` to it,
    /// and mark the create a `CompletedMarker` for possible future
    /// operation like `.precede()` to deal with forward_parent.
    pub(crate) fn complete_sealed(mut self, p: &mut Parser, kind: SyntaxKind) {
        eprintln!("complete_sealed called kind: {:?}", kind);
        self.bomb.defuse();
        if self.start_event_is_buffered {
            let idx = self.pos as usize;
            match p.events[idx] {
                Event::Start { kind: ref mut slot, .. } => {
                    *slot = kind;
                }
                _ => unreachable!(),
            }
        }

        p.push_event(Event::Finish);
        // since the marker is sealed, we know that precede will not be called on this marker
        // so we can drain all events from the start of this marker to the event
        if p.buffering_start_index.is_empty() {
            debug_assert!(!p.events.is_empty());
            p.drain_events(self.pos);
        }
    }

    // TODO: lige nu er precedable unbounded. Hvordan kan man markere at den er død?
    // TODO: idé: on drop, use unsafe ptr to get parser

    /// Abandons the syntax tree node. All its children
    /// are attached to its parent instead.
    pub(crate) fn abandon(mut self, p: &mut Parser) {
        if !self.only_completable {
            panic!("You cannot abondon this marker");
        }
        self.bomb.defuse();
        let idx = self.pos as usize;
        if idx == p.events.len() - 1 {
            match p.events.pop() {
                Some(Event::Start { kind: TOMBSTONE, forward_parent: None }) => (),
                _ => unreachable!(),
            }
        }
        // p.drain_events(self.pos);
    }
}

impl Marker<Precedable> {
    /// Finishes the syntax tree node and assigns `kind` to it,
    /// and mark the create a `CompletedMarker` for possible future
    /// operation like `.precede()` to deal with forward_parent.
    pub(crate) fn complete_precedable(
        mut self,
        p: &mut Parser,
        kind: SyntaxKind,
    ) -> PrecedableMarker {
        if !self.only_completable {
            panic!("You cannot make this marker precedable")
        }
        self.bomb.defuse();
        let idx = self.pos as usize;
        match p.events[idx] {
            Event::Start { kind: ref mut slot, .. } => {
                *slot = kind;
            }
            _ => unreachable!(),
        }
        let finish_pos = p.events.len() as u32;
        p.push_event(Event::Finish);
        PrecedableMarker::new(self.pos, finish_pos, kind)
    }
}

#[must_use = "parser.seal() has to be called"]
pub(crate) struct PrecedableMarker {
    start_pos: u32,
    finish_pos: u32,
    kind: SyntaxKind,
    bomb: DropBomb,
}

impl PrecedableMarker {
    fn new(start_pos: u32, finish_pos: u32, kind: SyntaxKind) -> Self {
        PrecedableMarker {
            start_pos,
            finish_pos,
            kind,
            bomb: DropBomb::new("paresr.seal() has to be called"),
        }
    }

    // TODO svs: skal løses:
    // - events må ikke være unbounded
    // - det skal være muligt at løbende sende noget til sink
    // - precede er problemet - den gør at vi ikke bare kan sende alle events med det samme til sink
    // - ide: with_sealed skal eagerly sende til sink.
    // - start, eat, start
    // - når complete_sealed kaldes, skal alt mellem start og marker end draines
    // - kan man kalde start og så start, hvor den første start dør før?
    // ide: når start() kaldes, så drain alt
    // problem:
    //  start1
    //      eat
    //      start2
    //      precede(start2)
    //
    //
    //  with_sealed kan eagerly sendes til sink
    // with_precedable tilføjes der blot til events.
    //  Denne kan først draines når der ikke længere kan kaldes precede på den.
    //  Sæt must_use på procedable_marker, så man altid skal kalde parser.seal()

    /// This method allows to create a new node which starts
    /// *before* the current one. That is, parser could start
    /// node `A`, then complete it, and then after parsing the
    /// whole `A`, decide that it should have started some node
    /// `B` before starting `A`. `precede` allows to do exactly
    /// that. See also docs about `forward_parent` in `Event::Start`.
    ///
    /// Given completed events `[START, FINISH]` and its corresponding
    /// `CompletedMarker(pos: 0, _)`.
    /// Append a new `START` events as `[START, FINISH, NEWSTART]`,
    /// then mark `NEWSTART` as `START`'s parent with saving its relative
    /// distance to `NEWSTART` into forward_parent(=2 in this case);
    pub(crate) fn precede(mut self, p: &mut Parser) -> Marker<Precedable> {
        // TODO: precede design: can it be simpler? the forward_parent should be removed in favor of prepending the new parent event in the current list of events
        self.bomb.defuse();
        let new_pos = p.start_internal();
        p.set_buffered(&new_pos);
        let idx = self.start_pos as usize;
        match p.events[idx] {
            Event::Start { ref mut forward_parent, .. } => {
                *forward_parent = Some(new_pos.pos - self.start_pos);
            }
            _ => unreachable!(),
        }
        new_pos
    }

    /// Undo this completion and turns into a `Marker`
    pub(crate) fn undo_completion(mut self, p: &mut Parser) -> Marker<Sealed> {
        self.bomb.defuse();
        let start_idx = self.start_pos as usize;
        let finish_idx = self.finish_pos as usize;
        match p.events[start_idx] {
            Event::Start { ref mut kind, forward_parent: None } => *kind = TOMBSTONE,
            _ => unreachable!(),
        }
        match p.events[finish_idx] {
            ref mut slot @ Event::Finish => *slot = Event::tombstone(),
            _ => unreachable!(),
        }
        Marker::<Sealed>::new(self.start_pos)
    }

    pub(crate) fn kind(&self) -> SyntaxKind {
        self.kind
    }
}

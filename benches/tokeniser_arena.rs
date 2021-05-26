use criterion::{criterion_group, criterion_main, Criterion};
use std::ops::Range;

use logos::Logos;

use topos::tokenising::Token;
use bumpalo::{Bump, collections};

pub fn normal_vec_collect() -> Vec<(Token, Range<usize>)> {
    let lex = Token::lexer("cond fn closure wow");
    let a : Vec<_> = lex.spanned().collect();
    a
}

pub fn bumpalo_vec_collect() {
    let lex = Token::lexer("cond fn closure wow");

    let bump = Bump::new();
    let mut v = collections::Vec::new_in(&bump);
    for x in lex.spanned().into_iter() {
        v.push(x)
    }
}

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("normal vec collect", |b| b.iter(|| normal_vec_collect()));
    c.bench_function("bumpalo vec collect", |b| b.iter(|| bumpalo_vec_collect()));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
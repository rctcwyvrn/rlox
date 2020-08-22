use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rlox::interpret;
use std::fs;

fn binary_trees(c: &mut Criterion) {
    let code = fs::read_to_string("test/benchmark_v2/binary_trees.lox").unwrap();
    c.bench_function("binary_trees", |b| {
        b.iter(|| interpret(black_box(&code), false, false))
    });
}

fn equality(c: &mut Criterion) {
    let code = fs::read_to_string("test/benchmark_v2/equality.lox").unwrap();
    c.bench_function("equality", |b| {
        b.iter(|| interpret(black_box(&code), false, false))
    });
}

fn fib(c: &mut Criterion) {
    let code = fs::read_to_string("test/benchmark_v2/fib.lox").unwrap();
    c.bench_function("fib", |b| {
        b.iter(|| interpret(black_box(&code), false, false))
    });
}

fn instantiation(c: &mut Criterion) {
    let code = fs::read_to_string("test/benchmark_v2/instantiation.lox").unwrap();
    c.bench_function("instantiation", |b| {
        b.iter(|| interpret(black_box(&code), false, false))
    });
}

fn method_call(c: &mut Criterion) {
    let code = fs::read_to_string("test/benchmark_v2/method_call.lox").unwrap();
    c.bench_function("method_call", |b| {
        b.iter(|| interpret(black_box(&code), false, false))
    });
}

fn properties(c: &mut Criterion) {
    let code = fs::read_to_string("test/benchmark_v2/properties.lox").unwrap();
    c.bench_function("properties", |b| {
        b.iter(|| interpret(black_box(&code), false, false))
    });
}

fn string_equality(c: &mut Criterion) {
    let code = fs::read_to_string("test/benchmark_v2/string_equality.lox").unwrap();
    c.bench_function("string_equality", |b| {
        b.iter(|| interpret(black_box(&code), false, false))
    });
}

fn trees(c: &mut Criterion) {
    let code = fs::read_to_string("test/benchmark_v2/trees.lox").unwrap();
    c.bench_function("trees", |b| {
        b.iter(|| interpret(black_box(&code), false, false))
    });
}

fn zoo(c: &mut Criterion) {
    let code = fs::read_to_string("test/benchmark_v2/zoo.lox").unwrap();
    c.bench_function("zoo", |b| {
        b.iter(|| interpret(black_box(&code), false, false))
    });
}

criterion_group!(
    benches,
    binary_trees,
    equality,
    fib,
    instantiation,
    method_call,
    properties,
    string_equality,
    trees,
    zoo
);
criterion_main!(benches);

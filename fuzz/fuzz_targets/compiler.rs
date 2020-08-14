#![no_main]
use libfuzzer_sys::fuzz_target;

extern crate rlox;

fuzz_target!(|data: String| {
    // fuzzed code goes here
    rlox::interpret(&data, false, true);
});

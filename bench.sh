hyperfine --warmup 3 "target/release/rlox $1" "java -jar ../jlox/jlox.jar $1"
# hyperfine "target/release/rlox $1" "java -jar ../jlox/jlox.jar $1"

# lox-bc

A bytecode VM for the lox language in Rust

## Create a release build

```
cargo build --release
```

## Create a build with disassembly

```
cargo build --release  --features 'debug_trace_execution debug_print_code'
```

## Run examples

On Linux/MacOS systems, the following script may be used.

```
./run_all.sh
```

It can be adapted to run on a windows environment too.

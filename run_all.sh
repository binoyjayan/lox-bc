#!/usr/bin/env bash

cargo build --release

for f in examples/*.lox; do
  echo "==================== $f ===================="
  ./target/release/lox "$f"
done

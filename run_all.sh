#!/usr/bin/env bash

cargo build --release --no-default-features

for f in examples/*.lox; do
  echo "==================== $f ===================="
  ./target/release/lox "$f"
done

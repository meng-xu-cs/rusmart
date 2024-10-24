#!/bin/bash

#cleaning up the previous coverage files
cargo clean

cargo tarpaulin \
    --engine llvm \
    --workspace \
    --exclude-files "smt/testing/*.rs" \
    --out Lcov \
    --out html \
    --output-dir target/tarpaulin \

open target/tarpaulin/tarpaulin-report.html
#!/bin/bash

#cleaning up the previous coverage files
cargo clean

cargo tarpaulin \
    --engine llvm \
    --out Lcov \
    --out html \
    --output-dir target/tarpaulin \

open target/tarpaulin/tarpaulin-report.html


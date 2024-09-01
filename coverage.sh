#!/bin/bash

#cleaning up the previous coverage files
cargo clean

# Create a directory for the coverage files
mkdir -p target/grcov/coverage

export CARGO_INCREMENTAL=0
export RUSTFLAGS="-Cinstrument-coverage"
export LLVM_PROFILE_FILE="$(pwd)/target/grcov/coverage/rustmart-%p-%m.profraw"
# Run the tests
cargo test

# Generate the lcov report, excluding dependencies
grcov . -s . \
    --binary-path ./target/debug/ \
    -t lcov --branch --ignore-not-existing \
    --ignore "/*" \
    --ignore "target/*" \
    --ignore "**/.cargo/**" \
    --ignore "**/tests/**" \
    -o target/grcov/coverage/lcov.info

# Generate the HTML report
genhtml -o target/grcov/coverage_output/ target/grcov/coverage/lcov.info
open target/grcov/coverage_output/index.html
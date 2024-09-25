#!/bin/bash

cargo clean

cargo watch -x \
      'tarpaulin --engine llvm --out Lcov --output-dir target/tarpaulin --ignore-tests' \
      -w src/
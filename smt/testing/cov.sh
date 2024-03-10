#!/bin/bash

cargo tarpaulin \
    --engine llvm \
    --out html

open ../../tarpaulin-report.html

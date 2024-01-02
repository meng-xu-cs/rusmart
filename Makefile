# help
define cmdline
Please choose one of the specific commands:
  - lint: lint and format the code 
  - cloc: count total number of lines of code
  - rego: semantics for language: rego
endef
export cmdline

help:
	@echo "$$cmdline"

lint:
	@cargo fmt
	@cargo clippy --all-targets --all-features

cloc:
	@cloc \
		Makefile \
		Cargo.toml \
		rust-toolchain \
		utils \
		smt \
		cli \
		lang

rego:
	@cd lang/rego && \
		cargo run

.PHONY: help lint cloc rego

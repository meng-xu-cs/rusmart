# help
define cmdline
Please choose one of the specific commands:
  - lint	: lint and format the code
  - cloc	: count total number of lines of code
  - reset	: wipe out the entire states
  - deps	: clean build the dependencies
  - docs	: build and display the documentation
  - rego	: semantics for language: rego
endef
export cmdline

help:
	@echo "$$cmdline"

lint:
	@cargo fmt
	@cargo clippy --all-targets --all-features

cloc:
	@cloc \
		utils \
		smt \
		cli \
		lang \
		Cargo.toml \
		cov.sh \
		coverage.sh \
		Makefile \
		rust-toolchain \
		doc/book/dict \
		doc/book/src \
		doc/book/book.toml \
		
reset:
	@cd cli && cargo run reset

deps:
	@cd cli && \
		cargo run deps z3 build --force
	@cd cli && \
		cargo run deps cvc5 build --force

docs:
	@cd doc/book && \
		mdbook clean && mdbook build && mdbook serve

rego:
	@cd lang/rego && \
		cargo run

.PHONY: help lint cloc reset deps docs rego

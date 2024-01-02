# help
define cmdline
Please choose one of the specific commands:
  - cloc: count total number of lines of code
  - rego: semantics for language: rego
endef
export cmdline

help:
	@echo "$$cmdline"

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

.PHONY: help cloc rego

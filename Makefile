# help
define cmdline
Please choose one of the specific commands:
  - cloc: count total number of lines of code
endef
export cmdline

help:
	@echo "$$cmdline"

cloc:
	@cloc \
		Cargo.toml \
		rust-toolchain \
		utils \
		smt \
		cli \
		lang

.PHONY: help cloc

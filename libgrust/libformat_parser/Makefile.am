LIBFORMAT_PARSER = debug/liblibformat_parser.a

all-local: $(LIBFORMAT_PARSER)

RUST_BUILD_DIR=$(PWD)

# TODO: Improve `cargo` invocation with host specific flags, possibly creating a $(CARGO) variable?
$(LIBFORMAT_PARSER): $(srcdir)/Cargo.toml $(srcdir)/src/*.rs
	cd $(srcdir) && \
	cargo build --offline --target-dir $(RUST_BUILD_DIR)

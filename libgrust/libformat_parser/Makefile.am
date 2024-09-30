LIBFORMAT_PARSER = debug/liblibformat_parser.a

all-local: $(LIBFORMAT_PARSER)

# TODO: Improve `cargo` invocation with host specific flags, possibly creating a $(CARGO) variable?
$(LIBFORMAT_PARSER): $(srcdir)/Cargo.toml $(srcdir)/src/*.rs
	cargo \
	  --config $(srcdir)/.cargo/config \
	  build \
	    --offline \
	    --target-dir . \
	    --manifest-path $(srcdir)/Cargo.toml \
	    # FIXME: Not always '--release', right?

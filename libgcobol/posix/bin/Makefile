.SUFFIXES: .scr .cbl

ROOT = $(shell git rev-parse --show-toplevel)

#
# Demonstrate how to generate a new COBOL binding from a man page. 
#

SCRAPE =  $(ROOT)/libgcobol/posix/bin/scrape.awk
UDF.GEN = $(ROOT)/libgcobol/posix/bin/udf-gen

posix-funcs:
	test "$(FUNCS)"
	for F in $(FUNCS); \
		do man 2 $$F | col -b | $(SCRAPE) > posix-$$F.scr; \
		$(MAKE) -f $(ROOT)/libgcobol/posix/bin/Makefile posix-$${F}.cbl; done

posix-$(FUNC).cbl:
	man 2 $(FUNC) | col -b | $(SCRAPE) | \
	$(UDF.GEN) -D mode_t=unsigned\ long  > $@~
	@mv $@~ $@

posix-mkdir.cbl:
	man 2 mkdir | col -b | $(SCRAPE) | \
	$(UDF.GEN) -D mode_t=unsigned\ long  > $@~
	@mv $@~ $@

# ... or 

posix-stat-many.scr:
	man 2 stat | col -b | $(SCRAPE) > $@~
	@mv $@~ $@

%.cbl : %.scr
	test -s $^
	$(UDF.GEN) $(CPPFLAGS)  $^ > $@~
	@mv $@~ $@

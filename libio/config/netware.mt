IMPDIR=	$(srcdir)/config/netware

PRELUDE=	prelude.o

iostream.def:	Makefile
	-rm -f iostream.def
	echo "description \"libiostream\"" >> iostream.def
	echo "screenname \"NONE\"" >> iostream.def
	echo "version `echo $(VERSION) | sed 's|\.|,|g'`" >> iostream.def
	echo "export @$(IMPDIR)/iostream.imp" >> iostream.def

iostream.O:	$(PRELUDE) $(LIBIOSTREAM_OBJECTS)
	$(CC) -Xlinker -Ur -o $@ $(PRELUDE) $(LIBIOSTREAM_OBJECTS)

iostream.nlm:	iostream.def iostream.O $(IMPDIR)/iostream.imp
	$(NLMCONV) -l $(LD) -T iostream.def iostream.O iostream.nlm

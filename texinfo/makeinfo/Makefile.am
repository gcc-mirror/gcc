## Makefile.am for texinfo/makeinfo.
## $Id: Makefile.am,v 1.3 1998/03/24 19:41:22 law Exp $
## Run automake in .. to produce Makefile.in from this.

noinst_PROGRAMS = makeinfo

localedir = $(datadir)/locale
INCLUDES = -I$(top_srcdir)/lib -I../intl -DLOCALEDIR=\"$(localedir)\"
LDADD = ../lib/libtxi.a @INTLLIBS@

makeinfo_SOURCES = makeinfo.c makeinfo.h multi.c

EXTRA_DIST = README

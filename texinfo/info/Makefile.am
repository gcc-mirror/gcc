## Makefile.am for texinfo/info.
## $Id: Makefile.am,v 1.1.1.2 1998/03/24 18:20:06 law Exp $
## Run automake in .. to produce Makefile.in from this.

noinst_PROGRAMS = makedoc

# Use `ginfo' for building to avoid confusion with the standard `info'
# target.  The install rule removes the `g' before applying any
# user-specified name transformations.
bin_PROGRAMS = ginfo
transform = s/ginfo/info/; @program_transform_name@

localedir = $(datadir)/locale

# -I. for funs.h.
# Automake puts -I.. and -I$(srcdir) into DEFS by default, but
# we need to override it, so include them ourselves.
INCLUDES = -I. -I$(top_srcdir)/lib -I../intl -I.. -I$(srcdir)
DEFS = -DINFODIR=\"$(infodir)\" -DLOCALEDIR=\"$(localedir)\" @DEFS@
LDADD = ../lib/libtxi.a @TERMLIBS@ @INTLLIBS@

makedoc_SOURCES = makedoc.c
ginfo_SOURCES = dir.c display.c display.h doc.c doc.h dribble.c dribble.h \
  echo-area.c echo-area.h \
  filesys.c filesys.h footnotes.c footnotes.h funs.h gc.c gc.h \
  indices.c indices.h info-utils.c info-utils.h info.c info.h infodoc.c \
  infomap.c infomap.h m-x.c man.c man.h nodemenu.c nodes.c nodes.h \
  search.c search.h session.c session.h signals.c signals.h \
  termdep.h terminal.c terminal.h tilde.c tilde.h \
  variables.c variables.h window.c window.h

EXTRA_DIST = README

# The files `doc.c' and `funs.h' are created by ./makedoc run over the source
# files which contain DECLARE_INFO_COMMAND.  `funs.h' is a header file
# listing the functions found.  `doc.c' is a structure containing pointers
# to those functions along with completable names and documentation strings.
BUILT_SOURCES = doc.c funs.h

cmd_sources = $(srcdir)/session.c $(srcdir)/echo-area.c $(srcdir)/infodoc.c \
  $(srcdir)/m-x.c $(srcdir)/indices.c $(srcdir)/nodemenu.c \
  $(srcdir)/footnotes.c $(srcdir)/variables.c

$(BUILT_SOURCES): makedoc $(cmd_sources)
	./makedoc $(cmd_sources)

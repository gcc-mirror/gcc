## Process this file with automake to create Makefile.in.

AUTOMAKE_OPTIONS = cygnus

# Multilib support variables.
MULTISRCTOP =
MULTIBUILDTOP =
MULTIDIRS =
MULTISUBDIR =
MULTIDO = true
MULTICLEAN = true

## Install a library built with a cross compiler in tooldir, not
## libdir.
if USE_LIBDIR
toolexeclibdir = $(libdir)
else
toolexecdir = $(exec_prefix)/$(target_alias)
toolexeclibdir = $(toolexecdir)/lib$(MULTISUBDIR)
endif

## We don't use `libz.la' because we don't want to conflict with a
## system library of that name.
toolexeclib_LTLIBRARIES = @target_all@
EXTRA_LTLIBRARIES = libzgcj.la

libzgcj_la_SOURCES = adler32.c compress.c crc32.c deflate.c deflate.h \
gzio.c infblock.c infblock.h infcodes.c infcodes.h inffast.c inffast.h \
inffixed.h inflate.c inftrees.c inftrees.h infutil.c infutil.h trees.c \
trees.h uncompr.c zconf.h zlib.h zutil.c zutil.h

libzgcj_la_LDFLAGS = -version-info 0:0:0 -rpath $(toolexeclibdir)

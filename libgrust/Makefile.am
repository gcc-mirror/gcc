## Process this file with automake to produce Makefile.in
LIBFFI = @LIBFFI@
LIBFFIINCS = @LIBFFIINCS@

lib_LTLIBRARIES = libgrust.la

AM_CPPFLAGS = -I$(top_srcdir)/include
AM_CFLAGS = -g -O2 -std=gnu99 -Wall -Werror -fgcse $(LIBFFIINCS) $(PTHREAD_CFLAGS)

library_includedir = $(includedir)/grust
library_include_HEADERS = \
	include/rust/rust.h \
	include/rust/lang-hooks.h

libgrust_la_CFLAGS = -DGRUST -DSHARED -g -O2 -std=gnu99 -Wall -Werror $(LIBFFIINCS) $(PTHREAD_CFLAGS)
libgrust_la_LDFLAGS = -release 0.1.0 $(LIBFFI) $(PTHREAD_CFLAGS) $(AM_LDFLAGS)
libgrust_la_LIBADD = $(PTHREAD_LIBS) $(MATH_LIBS)
libgrust_la_SOURCES = \
	src/rust-main.c \
	src/lang-hooks.c

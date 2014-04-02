# Makefile.am is used by automake 1.11 to generate Makefile.in.

ACLOCAL_AMFLAGS = -I .. -I ../config
AUTOMAKE_OPTIONS = no-dependencies

gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)
target_noncanonical := @target_noncanonical@
libexecsubdir := $(libexecdir)/gcc/$(target_noncanonical)/$(gcc_version)

AM_CPPFLAGS = -I$(top_srcdir)/../include $(DEFS)
AM_CFLAGS = @ac_lto_plugin_warn_cflags@
AM_LDFLAGS = @ac_lto_plugin_ldflags@
AM_LIBTOOLFLAGS = --tag=disable-static

libexecsub_LTLIBRARIES = liblto_plugin.la
gcc_build_dir = ../$(host_subdir)/gcc
in_gcc_libs = $(foreach lib, $(libexecsub_LTLIBRARIES), $(gcc_build_dir)/$(lib))

# Can be removed when libiberty becomes a normal convenience library
Wc=-Wc,

liblto_plugin_la_SOURCES = lto-plugin.c
liblto_plugin_la_LIBADD = \
	$(if $(wildcard ../libiberty/pic/libiberty.a),$(Wc)../libiberty/pic/libiberty.a,)
# Note that we intentionally override the bindir supplied by ACX_LT_HOST_FLAGS
liblto_plugin_la_LDFLAGS = $(AM_LDFLAGS) \
	$(lt_host_flags) -module -bindir $(libexecsubdir) \
	$(if $(wildcard ../libiberty/pic/libiberty.a),,-Wc,../libiberty/libiberty.a)
liblto_plugin_la_DEPENDENCIES = $(if $(wildcard \
	../libiberty/pic/libiberty.a),../libiberty/pic/libiberty.a,)

all-local: $(in_gcc_libs)

$(in_gcc_libs) : $(gcc_build_dir)/%: %
	@if test "X`dlname=; . ./$*; echo dlname:$$dlname`" = "Xdlname:"; then \
	  echo WARNING: $* is static, not copying to $@ >&2 ; \
	else \
	  $(mkinstalldirs) $(gcc_build_dir) && \
	  $(LIBTOOL) $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) --mode=install $(INSTALL) $(INSTALL_STRIP_FLAG) $* `pwd`/$@ ; \
	fi

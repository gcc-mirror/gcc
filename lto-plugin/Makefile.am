# Makefile.am is used by automake 1.11 to generate Makefile.in.

ACLOCAL_AMFLAGS = -I .. -I ../config
AUTOMAKE_OPTIONS = no-dependencies

gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)
target_noncanonical := @target_noncanonical@
libexecsubdir := $(libexecdir)/gcc/$(target_noncanonical)/$(gcc_version)

# How to find libelf
LIBELFLIBS = @LIBELFLIBS@
LIBELFINC = @LIBELFINC@

# Which object format to parse.
LTO_FORMAT = @LTO_FORMAT@

AM_CPPFLAGS = -I$(top_srcdir)/../include $(LIBELFINC)
AM_CFLAGS = -Wall -Werror

libexecsub_LTLIBRARIES = liblto_plugin.la

liblto_plugin_la_SOURCES = lto-plugin.c lto-plugin-$(LTO_FORMAT).c
liblto_plugin_la_LIBADD = $(LIBELFLIBS) \
	$(if $(wildcard ../libiberty/pic/libiberty.a),../libiberty/pic/libiberty.a,)
liblto_plugin_la_LDFLAGS = -no-undefined -bindir $(libexecsubdir) \
	$(if $(wildcard ../libiberty/pic/libiberty.a),,-Wc,../libiberty/libiberty.a)

all: copy_lto_plugin

copy_lto_plugin: all-am
	$(LIBTOOL) $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) --mode=install $(INSTALL) $(INSTALL_STRIP_FLAG) $(libexecsub_LTLIBRARIES) `pwd`/../gcc/


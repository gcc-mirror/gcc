# Makefile.am is used by automake 1.11 to generate Makefile.in.

ACLOCAL_AMFLAGS = -I .. -I ../config
AUTOMAKE_OPTIONS = no-dependencies

gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)
target_noncanonical := @target_noncanonical@
libexecsubdir := $(libexecdir)/gcc/$(target_noncanonical)/$(gcc_version)

AM_CPPFLAGS = -I$(top_srcdir)/../include $(DEFS)
AM_CFLAGS = -Wall -Werror
AM_LIBTOOLFLAGS = --tag=disable-static

libexecsub_LTLIBRARIES = liblto_plugin.la

liblto_plugin_la_SOURCES = lto-plugin.c
liblto_plugin_la_LIBADD = \
	$(if $(wildcard ../libiberty/pic/libiberty.a),../libiberty/pic/libiberty.a,)
# Note that we intentionally override the bindir supplied by ACX_LT_HOST_FLAGS
liblto_plugin_la_LDFLAGS = $(lt_host_flags) -bindir $(libexecsubdir) \
	$(if $(wildcard ../libiberty/pic/libiberty.a),,-Wc,../libiberty/libiberty.a)

all: copy_lto_plugin

copy_lto_plugin: all-am
	$(LIBTOOL) $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) --mode=install $(INSTALL) $(INSTALL_STRIP_FLAG) $(libexecsub_LTLIBRARIES) `pwd`/../gcc/


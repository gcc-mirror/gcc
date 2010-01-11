# Makefile.am is used by automake 1.11 to generate Makefile.in.

ACLOCAL_AMFLAGS = -I .. -I ../config
AUTOMAKE_OPTIONS = no-dependencies

gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)
target_noncanonical := @target_noncanonical@
libexecsubdir := $(libexecdir)/gcc/$(target_noncanonical)/$(gcc_version)

# How to find libelf
LIBELFLIBS = @LIBELFLIBS@
LIBELFINC = @LIBELFINC@

AM_CPPFLAGS = -I$(top_srcdir)/../include $(LIBELFINC)
AM_CFLAGS = -Wall -Werror

libexecsub_LTLIBRARIES = liblto_plugin.la

liblto_plugin_la_SOURCES = lto-plugin.c
liblto_plugin_la_LIBADD = $(LIBELFLIBS) ../libiberty/pic/libiberty.a

## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign

# May be used by various substitution variables.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

# autoconf2.13's target_alias
target_noncanonical = @target_noncanonical@

tool_include_dir = $(libdir)/gcc/$(target_noncanonical)/$(gcc_version)/include
tool_include__HEADERS = jni.h jni_md.h jawt.h jawt_md.h jvmpi.h

## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign

# May be used by various substitution variables.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

# autoconf2.13's target_alias
target_noncanonical = @target_noncanonical@

gcjdir = $(gxx_include_dir)/gcj
gcj_HEADERS = array.h cni.h field.h javaprims.h method.h

tool_include_dir := $(libdir)/gcc/$(target_noncanonical)/$(gcc_version)/include
toolgcjdir       := $(tool_include_dir)/gcj
toolgcj_HEADERS = libgcj-config.h

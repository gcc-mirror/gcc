## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS=foreign

DISTCLEANFILES=ffitarget.h
EXTRA_DIST=ffi.h.in ffi_common.h

hackdir=$(includedir)

hack_DATA= ffi.h

# Where generated headers like ffitarget.h get installed.
gcc_version   := $(shell cat $(top_srcdir)/../gcc/BASE-VER)
toollibffidir := $(libdir)/gcc/$(target_alias)/$(gcc_version)/include/libffi

toollibffi_HEADERS = ffitarget.h

## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS=foreign

DISTCLEANFILES=ffitarget.h
noinst_HEADERS=ffi_common.h ffi_cfi.h tramp.h
EXTRA_DIST=ffi.h.in

# Where generated headers like ffitarget.h get installed.
gcc_version   := $(shell @get_gcc_base_ver@ $(top_srcdir)/../gcc/BASE-VER)
toollibffidir := $(libdir)/gcc/$(target_alias)/$(gcc_version)/include

toollibffi_HEADERS = ffi.h ffitarget.h

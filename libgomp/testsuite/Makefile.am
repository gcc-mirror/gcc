## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign dejagnu

# May be used by various substitution variables.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

EXPECT = $(shell if test -f $(top_builddir)/../expect/expect; then \
	   echo $(top_builddir)/../expect/expect; else echo expect; fi)

_RUNTEST = $(shell if test -f $(top_srcdir)/../dejagnu/runtest; then \
	     echo $(top_srcdir)/../dejagnu/runtest; else echo runtest; fi)
RUNTEST = "$(_RUNTEST) $(AM_RUNTESTFLAGS)"

# Used for support non-fallback offloading.
export OFFLOAD_TARGETS = $(offload_targets)
export OFFLOAD_ADDITIONAL_OPTIONS = $(offload_additional_options)
export OFFLOAD_ADDITIONAL_LIB_PATHS = $(offload_additional_lib_paths)

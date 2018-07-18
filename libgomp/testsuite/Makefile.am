## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign dejagnu

# May be used by various substitution variables.
gcc_version := $(shell @get_gcc_base_ver@ $(top_srcdir)/../gcc/BASE-VER)

EXPECT = $(shell if test -f $(top_builddir)/../expect/expect; then \
	   echo $(top_builddir)/../expect/expect; else echo expect; fi)

_RUNTEST = $(shell if test -f $(top_srcdir)/../dejagnu/runtest; then \
	     echo $(top_srcdir)/../dejagnu/runtest; else echo runtest; fi)
RUNTEST = "$(_RUNTEST) $(AM_RUNTESTFLAGS)"


# Instead of directly in ../testsuite/libgomp-test-support.exp.in, the
# following variables have to be "routed through" this Makefile, for expansion
# of the several (Makefile) variables used therein.
libgomp-test-support.exp: libgomp-test-support.pt.exp Makefile
	cp $< $@.tmp
	echo >> $@.tmp \
	  'set offload_additional_options "$(offload_additional_options)"'
	echo >> $@.tmp \
	  'set offload_additional_lib_paths "$(offload_additional_lib_paths)"'
	mv $@.tmp $@

all-local: libgomp-test-support.exp

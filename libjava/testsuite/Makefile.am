## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign dejagnu

# May be used by various substitution variables.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

# autoconf2.13's target_alias
target_noncanonical = @target_noncanonical@

# Setup the testing framework, if you have one
EXPECT = `if [ -f $(top_builddir)/../expect/expect ] ; then \
            echo $(top_builddir)/../expect/expect ; \
          else echo expect ; fi`

RUNTEST = "`if [ -f $(top_srcdir)/../dejagnu/runtest ] ; then \
	       echo $(top_srcdir)/../dejagnu/runtest ; \
	    else echo runtest; fi` $(AM_RUNTESTFLAGS)"

## Tell dejagnu whether the interpreter is enabled.
AM_RUNTESTFLAGS = INTERPRETER=$(INTERPRETER)

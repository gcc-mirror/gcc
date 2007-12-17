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

MYGCJH = gjavah

.PHONY: compile-tests

check-am: compile-tests

compile-tests:
if JAVA_MAINTAINER_MODE
	for test in `cd $(srcdir) > /dev/null; \
		     find libjava.cni libjava.jni libjava.jvmti libjava.lang \
			  libjava.loader libjava.verify -name \*.java`; do \
	  testjar=$(srcdir)/`echo $$test | sed -e 's/\.java$$/\.jar/'`; \
	  testdep=; \
	  genheader=; \
	  case "$$test" in \
	    libjava.loader/dummy.java) \
	      testjar=$(srcdir)/libjava.loader/dummy.class;; \
	    libjava.loader/MyLoader.java) \
	      continue;; \
	    libjava.loader/*.java) \
	      testdep="libjava.loader/MyLoader.java";; \
	    libjava.jni/register2.java) \
	      ;; \
	    libjava.jni/*.java|libjava.jvmti/interp/*.java) \
	      genheader=-jni;; \
	    libjava.jvmti/*.java|libjava.cni/*.java) \
	      genheader=-cni;; \
	  esac; \
	  if test -f $$testjar; then \
	    if ! test $(srcdir)/$$test -nt $$testjar; then \
	      test -z "$$testdep" && continue; \
	      test $(srcdir)/$$testdep -nt $$testjar || continue; \
	    fi; \
	  fi; \
	  echo Compiling $$test; \
	  testtmpdir=`echo $$test | sed -e 's,/,_,g'`T; \
	  rm -rf $$testtmpdir; $(mkdir_p) $$testtmpdir; \
	  if test -n "$$testdep"; then \
	    $(GCJ) -C -g -w --encoding=UTF-8 -bootclasspath \
	      $(top_builddir)/libgcj-$(gcc_version).jar::$$testtmpdir \
	      -d $$testtmpdir $(srcdir)/$$testdep || exit; \
	  fi; \
	  $(GCJ) -C -g -w --encoding=UTF-8 -bootclasspath \
	    $(top_builddir)/libgcj-$(gcc_version).jar:$$testtmpdir \
	    -d $$testtmpdir $(srcdir)/$$test || exit; \
	  case "$$test" in \
	    libjava.loader/dummy.java) \
	      cp $$testtmpdir/dummy.class $$testjar;; \
	    *) \
	      $(JAR) cf $$testjar \
		`ls $$testtmpdir/*.class \
		 | sed -e 's,^,-C ,;s,/\([^/]*\.class\)$$, \1,'` || exit ;; \
	  esac; \
	  if test -n "$$genheader"; then \
	    $(MYGCJH) $$genheader $$testtmpdir/*.class \
	      -bootclasspath $(top_builddir)/libgcj-$(gcc_version).jar \
	      -d $$testtmpdir/ || exit; \
	    mv $$testtmpdir/*.h $(srcdir)/`dirname $$test`/ 2>/dev/null; \
	  fi; \
	  rm -rf $$testtmpdir; \
	done
endif

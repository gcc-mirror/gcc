## Makefile for the testsuite subdirectory of the GNU C++ Standard library.
##
## Copyright (C) 2001, 2002, 2003 Free Software Foundation, Inc.
##
## This file is part of the libstdc++ version 3 distribution.
## Process this file with automake to produce Makefile.in.

## This file is part of the GNU ISO C++ Library.  This library is free
## software; you can redistribute it and/or modify it under the
## terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option)
## any later version.

## This library is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License along
## with this library; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
## USA.

AUTOMAKE_OPTIONS =  cygnus dejagnu

mkinstalldirs = $(SHELL) $(toplevel_srcdir)/mkinstalldirs

DEJATOOL = libstdc++-v3

EXPECT = `if [ -f @glibcpp_builddir@/../../expect/expect ] ; then \
            echo @glibcpp_builddir@/../../expect/expect ; \
          else echo expect ; fi`

RUNTEST = `if [ -f @glibcpp_srcdir@/../dejagnu/runtest ] ; then \
	       echo @glibcpp_srcdir@/../dejagnu/runtest ; \
	    else echo runtest; fi`

AM_RUNTESTFLAGS =
RUNTESTFLAGS =

## CXX is actually a "C" compiler. These are real C++ programs.
glibcpp_srcdir=@glibcpp_srcdir@
glibcpp_builddir=@glibcpp_builddir@
testsuite_flags_script=${glibcpp_builddir}/scripts/testsuite_flags
CXX=`${testsuite_flags_script} --build-cxx`

CXXLINK = \
	LD_RUN_PATH=$${LD_RUN_PATH:+$$LD_RUN_PATH:}${glibcpp_builddir}/src/.libs\
	$(LIBTOOL) --tag=CXX --mode=link $(CXX) \
	$(AM_CXXFLAGS) $(CXXFLAGS) $(LDFLAGS) -o $@

GLIBCPP_INCLUDES = @GLIBCPP_INCLUDES@
LIBSUPCXX_INCLUDES = @LIBSUPCXX_INCLUDES@
TOPLEVEL_INCLUDES = @TOPLEVEL_INCLUDES@
INCLUDES = \
	-nostdinc++ \
	@GLIBCPP_INCLUDES@ @LIBSUPCXX_INCLUDES@ @TOPLEVEL_INCLUDES@ 

## Build support library.
noinst_LIBRARIES = libv3test.a
libv3test_a_SOURCES = testsuite_hooks.cc testsuite_allocator.cc

## Build support utilities.
if GLIBCPP_TEST_ABI
noinst_PROGRAMS = abi_check
else
noinst_PROGRAMS = 
endif
abi_check_SOURCES = abi_check.cc

all-local: stamp_wchar

# Enable wchar_t tests if capable.
if GLIBCPP_TEST_WCHAR_T
stamp_wchar:
	touch testsuite_wchar_t
else
stamp_wchar:
endif

# Override this so local rules are possible.
check-am: 
	$(MAKE) $(AM_MAKEFLAGS) check-DEJAGNU; \
	$(MAKE) $(AM_MAKEFLAGS) check-local 

check-local: check-abi

baseline_dir = @baseline_dir@
baseline_file = ${baseline_dir}/baseline_symbols.txt
extract_symvers = @glibcpp_srcdir@/config/abi/extract_symvers

current_symbols.txt: ${extract_symvers} ../src/.libs/libstdc++.so
	  -@(${extract_symvers} ../src/.libs/libstdc++.so current_symbols.txt)

baseline_symbols:
	-@(output=${baseline_file}; \
	  if test ! -f $${output}; then \
	    echo "Baseline file doesn't exist."; \
	    echo "Try 'make new-abi-baseline' to create it."; \
	    exit 1; \
	  fi; \
	touch baseline_symbols)

new-abi-baseline: 
	-@$(mkinstalldirs) ${baseline_dir}
	-@(output=${baseline_file}; \
	  if test -f $${output}; then \
	    output=$${output}.new; \
	    t=`echo $${output} | sed 's=.*config/abi/=='`; \
	    echo "Baseline file already exists, writing to $${t} instead."; \
	  fi; \
	  ${extract_symvers} ../src/.libs/libstdc++.so $${output})

if GLIBCPP_TEST_ABI
# Use 'new-abi-baseline' to create an initial symbol file.  Then run
# 'check-abi' to test for changes against that file.
check-abi: abi_check baseline_symbols current_symbols.txt
	-@(./abi_check --check ./current_symbols.txt ${baseline_file})

check-abi-verbose: abi_check baseline_symbols current_symbols.txt 
	-@(./abi_check --check-verbose ./current_symbols.txt ${baseline_file})
else
check-abi:
check-abi-verbose:
endif


# These two special 'check-script' rules use the bash script 'check'
# to do testing. This script is not as portable as the dejagnu test
# harness, and is thus off by default. It does produce interesting
# output however, including various performance analysis items like
# compile time, execution time, and binary size.
check_survey = ${glibcpp_builddir}/scripts/check_survey
check-script: ${check_survey}
	-@(chmod + ${check_survey}; \
	  ${check_survey} 0)

check-script-install: ${check_survey}
	-@(chmod + ${check_survey}; \
	  ${check_survey} 0)


# Runs the testsuite/performance tests.  
# Some of these tests create large (~75MB) files, allocate huge
# ammounts of memory, or otherwise tie up machine resources. Thus,
# running this is off by default.
check_performance=${glibcpp_srcdir}/scripts/check_performance
check-performance: ${check_performance}
	-@(chmod + ${check_performance}; \
	  ${check_performance} ${glibcpp_srcdir} ${glibcpp_builddir})


# By adding these files here, automake will remove them for 'make clean'
CLEANFILES = *.txt *.tst *.exe core* filebuf_* tmp* ostream_* *.log *.sum \
	     testsuite_* site.exp abi_check baseline_symbols

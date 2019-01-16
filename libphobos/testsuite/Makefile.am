## Makefile for the testsuite subdirectory of the D Standard library.
## Copyright (C) 2016-2019 Free Software Foundation, Inc.
##
## GCC is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3, or (at your option)
## any later version.
##
## GCC is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with GCC; see the file COPYING3.  If not see
## <http://www.gnu.org/licenses/>.

# Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign dejagnu

# Setup the testing framework, if you have one
EXPECT = $(shell if test -f $(top_builddir)/../expect/expect; then \
	   echo $(top_builddir)/../expect/expect; else echo expect; fi)

_RUNTEST = $(shell if test -f $(top_srcdir)/../dejagnu/runtest; then \
	     echo $(top_srcdir)/../dejagnu/runtest; else echo runtest; fi)
RUNTEST = $(_RUNTEST) $(AM_RUNTESTFLAGS)

AM_MAKEFLAGS = "EXEEXT=$(EXEEXT)"

CLEANFILES = *.log *.sum

## Makefile for the testsuite subdirectory of the GNU C++ Standard library.
##
## Copyright (C) 2001 Free Software Foundation, Inc.
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

AUTOMAKE_OPTIONS =  foreign dejagnu

DEJATOOL = libstdc++-v3

EXPECT = `if [ -f @glibcpp_builddir@/../../expect/expect ] ; then \
            echo @glibcpp_builddir@/../../expect/expect ; \
          else echo expect ; fi`

RUNTEST = `if [ -f @glibcpp_srcdir@/../dejagnu/runtest ] ; then \
	       echo @glibcpp_srcdir@/../dejagnu/runtest ; \
	    else echo runtest; fi`

RUNTESTFLAGS =


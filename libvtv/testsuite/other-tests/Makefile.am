## Makefile for the testsuite subdirectory of the VTV library.
##
## Copyright (C) 2013-2020 Free Software Foundation, Inc.
##
## Process this file with automake to produce Makefile.in.
##
## This file is part of the Vtable Verification (VTV) Library.  This
## library is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3, or (at your option)
## any later version.

## This library is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this library; see the file COPYING3.  If not see
## <http://www.gnu.org/licenses/>.

AUTOMAKE_OPTIONS = nostdinc

# Runs the testsuite via a script.

# Create subdirectories.
stamp-subdir:
	if test ! -d lib64; then \
	  mkdir -p lib64; \
	fi; \
	if test ! -d lib32; then \
	  mkdir -p lib32; \
	fi; \
	echo `date` > stamp-subdir;


testing_script=${libvtv_srcdir}/scripts/run-testsuite.sh
check-script: ${testing_script} stamp-subdir
	-@(chmod +x ${testing_script}; \
	${testing_script} ${libvtv_srcdir} ${libvtv_builddir})

check-am:
	$(MAKE) $(AM_MAKEFLAGS) check-script

.PHONY: check-script

# By adding these files here, automake will remove them for 'make clean'
CLEANFILES = *.out environment-fail-*  stamp-* replace-fail-*

# To remove directories.
clean-local:
	rm -rf lib*

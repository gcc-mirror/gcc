## Makefile for the doc subdirectory of the GNU C++ Standard library.
##
## Copyright (C) 2008 Free Software Foundation, Inc.
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
## Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
## USA.

include $(top_srcdir)/fragment.am

# These rules are messy, but are hella worth it.
doc-doxygen-html:
	-(srcdir=`cd ${top_srcdir}; ${PWD_COMMAND}`; \
	  builddir=`cd ..; ${PWD_COMMAND}`; \
	  ${SHELL} ${top_srcdir}/doc/doxygen/run_doxygen \
	  --host_alias=${host_alias} --mode=html $${srcdir} $${builddir})

doc-doxygen-man:
	-(srcdir=`cd ${top_srcdir}; ${PWD_COMMAND}`; \
	  builddir=`cd ..; ${PWD_COMMAND}`; \
	  ${SHELL} ${top_srcdir}/doc/doxygen/run_doxygen \
	  --host_alias=${host_alias} --mode=man $${srcdir} $${builddir})

# Generates the plots and graphs for performance testing.
doc_performance_script=${top_srcdir}/scripts/make_graphs.py
doc-performance:
	-@(chmod + ${doc_performance_script}; \
	${doc_performance_script} ${top_srcdir} \
	${glibcxx_builddir}/testsuite \
	${top_srcdir}/testsuite/data/make_graph_htmls.xml \
	${top_srcdir}/testsuite/data/make_graph_test_infos.xml local g++)

# No install-pdf, install-html support in automake yet
install-pdf:
install-html:

# Installation of distribution html documentation not yet supported
# TODO: Write custom install-html rule.
.PHONY: install-html install-pdf \
	doc-doxygen-html doc-doxygen-man doc-performance

# By adding these files here, automake will remove them for 'make clean'
CLEANFILES = 

# To remove directories.
clean-local:
	rm -rf man doxygen

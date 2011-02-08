## Makefile for the doc subdirectory of the GNU C++ Standard library.
##
## Copyright (C) 2008, 2009, 2010 Free Software Foundation, Inc.
##
## This file is part of the libstdc++ version 3 distribution.
## Process this file with automake to produce Makefile.in.

## This file is part of the GNU ISO C++ Library.  This library is free
## software; you can redistribute it and/or modify it under the
## terms of the GNU General Public License as published by the
## Free Software Foundation; either version 3, or (at your option)
## any later version.

## This library is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License along
## with this library; see the file COPYING3.  If not see
## <http://www.gnu.org/licenses/>.

include $(top_srcdir)/fragment.am

# Documentation Overview
#
# There are two main source materials for libstdc++ documentation.
# The first is the doxygen markup in libstdc++ sources, which is a
# reference to the API. And the second is the docbook markup in
# doc/xml/.
#
# A third and more obscure option deals with charting
# performance tests, and should be considered experimental.

# Default rules.
#
# Point to best sub-rule for the requested documentation target,
# create, and then copy into toplevel directory with standardized names
# and layouts.

# HTML
doc-html: doc-html-docbook doc-html-doxygen
	cp -R ${docbook_outdir}/html ./libstdc++-manual.html
	cp -R ${doxygen_outdir}/html ./libstdc++-api.html

# MAN
doc-man: doc-man-doxygen
	cp -R ${doxygen_outdir}/man ./libstdc++-api.man

# PDF
doc-pdf: doc-pdf-docbook doc-pdf-doxygen
	cp ${docbook_outdir}/pdf/libstdc++-manual.pdf .
	cp ${doxygen_outdir}/pdf/libstdc++-api.pdf .

# TEXINFO
doc-texinfo: doc-texinfo-docbook

# XML
doc-xml: doc-xml-single-docbook doc-xml-single-doxygen
	cp ${manual_xml} .
	cp ${api_xml} .


# Doxygen configuration
# Assumes doxygen, graphviz (with dot), pdflatex installed
doxygen_script=${top_srcdir}/scripts/run_doxygen
doxygen_outdir = ${glibcxx_builddir}/doc/doxygen

doc-html-doxygen:
	-(srcdir=`cd ${top_srcdir}; ${PWD_COMMAND}`; \
	  builddir=`cd ..; ${PWD_COMMAND}`; \
	  ${SHELL} ${doxygen_script} \
	  --host_alias=${host_alias} --mode=html $${srcdir} $${builddir} YES)

doc-man-doxygen:
	-(srcdir=`cd ${top_srcdir}; ${PWD_COMMAND}`; \
	  builddir=`cd ..; ${PWD_COMMAND}`; \
	  ${SHELL} ${doxygen_script} \
	  --host_alias=${host_alias} --mode=man $${srcdir} $${builddir} YES)

doc-xml-doxygen:
	-(srcdir=`cd ${top_srcdir}; ${PWD_COMMAND}`; \
	  builddir=`cd ..; ${PWD_COMMAND}`; \
	  ${SHELL} ${doxygen_script} \
	  --host_alias=${host_alias} --mode=xml $${srcdir} $${builddir} NO)

api_xml = ${doxygen_outdir}/xml/libstdc++-api-single.xml
doc-xml-single-doxygen: doc-xml-doxygen
	@echo "Generating doxygen xml single file..."
	$(XSLTPROC) ${doxygen_outdir}/xml/combine.xslt \
	${doxygen_outdir}/xml/index.xml > ${api_xml};

doc-latex-doxygen:
	-(srcdir=`cd ${top_srcdir}; ${PWD_COMMAND}`; \
	  builddir=`cd ..; ${PWD_COMMAND}`; \
	  ${SHELL} ${doxygen_script} \
	  --host_alias=${host_alias} --mode=latex $${srcdir} $${builddir} NO)

# Chance of loooooonnggg creation time on this rule.  Iff this fails,
# look at refman.log and see if TeX's memory is exhausted. Symptoms
# include asking a wizard to enlarge capacity. If this is the case,
# find texmf.cnf and add a zero for pool_size, string_vacancies,
# max_strings, and pool_free values.
doxygen_pdf = ${doxygen_outdir}/latex/refman.pdf
api_pdf = ${doxygen_outdir}/pdf/libstdc++-api.pdf

${doxygen_outdir}/pdf:
	mkdir -p ${doxygen_outdir}/pdf

doc-pdf-doxygen: stamp-latex-doxygen ${doxygen_outdir}/pdf
	-(cd ${doxygen_outdir}/latex && $(MAKE) -i pdf;)
	echo "Generating doxygen pdf file...";
	if [ -f ${doxygen_pdf} ]; then \
	  mv ${doxygen_pdf} ${api_pdf} ; \
	  echo ":: PDF file is ${api_pdf}"; \
	else \
	  echo "... error"; \
	  exit 12; \
	fi

stamp-pdf-doxygen:
	@if [ ! -f stamp-pdf-doxygen ]; then \
	  $(MAKE) doc-pdf-doxygen; \
	fi
	$(STAMP) stamp-pdf-doxygen

stamp-xml-doxygen:
	@if [ ! -f stamp-xml-doxygen ]; then \
	  $(MAKE) doc-xml-doxygen; \
	  $(MAKE) doc-xml-single-doxygen; \
	fi
	$(STAMP) stamp-xml-doxygen

stamp-latex-doxygen:
	@if [ ! -f stamp-latex-doxygen ]; then \
	  $(MAKE) doc-latex-doxygen; \
	fi
	$(STAMP) stamp-latex-doxygen

# Docbook configuration.
# Assumes
# libxslt
# dblatex
# pdflatex
# docbook-style-xsl
# emacs-nxml-mode
# xmlto passivetex
docbook_outdir = ${glibcxx_builddir}/doc/docbook
xml_dir = ${glibcxx_srcdir}/doc/xml

xml_sources_basic = \
	${xml_dir}/spine.xml \
	${xml_dir}/authors.xml \
	${xml_dir}/api.xml \
	${xml_dir}/faq.xml

xml_sources_manual = \
	${xml_dir}/manual/abi.xml \
	${xml_dir}/manual/algorithms.xml \
	${xml_dir}/manual/allocator.xml \
	${xml_dir}/manual/auto_ptr.xml \
	${xml_dir}/manual/atomics.xml \
	${xml_dir}/manual/backwards_compatibility.xml \
	${xml_dir}/manual/bitmap_allocator.xml \
	${xml_dir}/manual/build_hacking.xml \
	${xml_dir}/manual/codecvt.xml \
	${xml_dir}/manual/concurrency.xml \
	${xml_dir}/manual/concurrency_extensions.xml \
	${xml_dir}/manual/configure.xml \
	${xml_dir}/manual/containers.xml \
	${xml_dir}/manual/ctype.xml \
	${xml_dir}/manual/debug_mode.xml \
	${xml_dir}/manual/debug.xml \
	${xml_dir}/manual/diagnostics.xml \
	${xml_dir}/manual/evolution.xml \
	${xml_dir}/manual/extensions.xml \
	${xml_dir}/manual/internals.xml \
	${xml_dir}/manual/intro.xml \
	${xml_dir}/manual/io.xml \
	${xml_dir}/manual/iterators.xml \
	${xml_dir}/manual/locale.xml \
	${xml_dir}/manual/localization.xml \
	${xml_dir}/manual/messages.xml \
	${xml_dir}/manual/mt_allocator.xml \
	${xml_dir}/manual/numerics.xml \
	${xml_dir}/manual/parallel_mode.xml \
	${xml_dir}/manual/prerequisites.xml \
	${xml_dir}/manual/profile_mode.xml \
	${xml_dir}/manual/shared_ptr.xml \
	${xml_dir}/manual/spine.xml \
	${xml_dir}/manual/status_cxx1998.xml \
	${xml_dir}/manual/status_cxx200x.xml \
	${xml_dir}/manual/status_cxxtr1.xml \
	${xml_dir}/manual/status_cxxtr24733.xml \
	${xml_dir}/manual/strings.xml \
	${xml_dir}/manual/support.xml \
	${xml_dir}/manual/test.xml \
	${xml_dir}/manual/using.xml \
	${xml_dir}/manual/using_exceptions.xml \
	${xml_dir}/manual/utilities.xml \
	${xml_dir}/manual/appendix_free.xml \
	${xml_dir}/manual/appendix_contributing.xml \
	${xml_dir}/manual/appendix_porting.xml

xml_sources_extra = \
	${xml_dir}/gnu/fdl-1.2.xml \
	${xml_dir}/gnu/gpl-3.0.xml

xml_sources = \
	${xml_sources_basic} \
	${xml_sources_manual} \
	${xml_sources_extra}

xml_noinst = \
	${xml_dir}/book.txml \
	${xml_dir}/chapter.txml \
	${xml_dir}/class.txml

XSLTPROC       = xsltproc
XSLTPROC_FLAGS = --nonet --xinclude
XSL_STYLE_DIR = /usr/share/sgml/docbook/xsl-stylesheets
XSL_FO_STYLE = $(XSL_STYLE_DIR)/fo/docbook.xsl
XSL_HTML_STYLE = $(XSL_STYLE_DIR)/xhtml/chunk.xsl
#XSL_HTML_SINGLE_STYLE = $(XSL_STYLE_DIR)/xhtml/onechunk.xsl
XSL_HTML_SINGLE_STYLE = $(XSL_STYLE_DIR)/xhtml/docbook.xsl

${docbook_outdir}/fo:
	mkdir -p ${docbook_outdir}/fo

${docbook_outdir}/html:
	mkdir -p ${docbook_outdir}/html

${docbook_outdir}/pdf:
	mkdir -p ${docbook_outdir}/pdf

${docbook_outdir}/texinfo:
	mkdir -p ${docbook_outdir}/texinfo

${docbook_outdir}/xml:
	mkdir -p ${docbook_outdir}/xml

# Validate existing XML structure.
XMLLINT = xmllint
#LINT_FLAGS = --debug --nonet --xinclude --nsclean --postvalid --nowarning
#LINT_FLAGS = --noblanks --noout --xinclude --postvalid --noent
LINT_FLAGS = --postvalid --debug --xinclude --noent --noblanks  --nonet --noout
DTD_FLAGS = --dtdvalid http://www.oasis-open.org/docbook/xml/4.5/docbookx.dtd
XMLLINT_FLAGS = $(LINT_FLAGS) $(DTD_FLAGS)
doc-xml-validate-docbook: $(xml_sources)
	@echo "Generating XML validation log..."
	$(XMLLINT) $(XMLLINT_FLAGS) ${top_srcdir}/doc/xml/spine.xml

# XML, all one page
# Some info on canonicalization
# http://www.mail-archive.com/help-texinfo@gnu.org/msg00864.html
manual_xml = ${docbook_outdir}/xml/libstdc++-manual-single.xml
doc-xml-single-docbook: $(xml_sources) ${docbook_outdir}/xml
	@echo "Generating XML single..."
	$(XMLLINT) --xinclude --noent --noblanks --nocdata --nsclean --c14n \
	${top_srcdir}/doc/xml/manual/spine.xml > ${manual_xml}

# HTML, index plus chapters
doc-html-docbook: $(xml_sources) ${docbook_outdir}/html
	@echo "Generating html files..."
	$(XSLTPROC) $(XSLTPROC_FLAGS) -o ${docbook_outdir}/html/ \
	$(XSL_HTML_STYLE) ${top_srcdir}/doc/xml/spine.xml

# HTML, all one page
manual_html = ${docbook_outdir}/html/libstdc++-manual-single.html
doc-html-single-docbook: $(xml_sources) ${docbook_outdir}/html
	@echo "Generating html single file..."
	$(XSLTPROC) $(XSLTPROC_FLAGS) -o ${manual_html} \
	$(XSL_HTML_SINGLE_STYLE) ${top_srcdir}/doc/xml/spine.xml

# FO
doc-fo-docbook: $(xml_sources) ${docbook_outdir}/fo
	@echo "Generating FO files..."
	$(XSLTPROC) $(XSLTPROC_FLAGS) -o ${docbook_outdir}/fo/spine.fo \
	$(XSL_FO_STYLE) ${top_srcdir}/doc/xml/spine.xml

# PDF, via dblatex
manual_pdf = ${docbook_outdir}/pdf/libstdc++-manual.pdf
DBLATEX_FLAGS = --dump --verbose --pdf -o ${manual_pdf}
doc-pdf-docbook: doc-pdf-dblatex-docbook

doc-pdf-dblatex-docbook: $(xml_sources) ${docbook_outdir}/pdf
	@echo "Generating pdf dblatex files..."
	dblatex $(DBLATEX_FLAGS) ${top_srcdir}/doc/xml/spine.xml

# Texinfo, via docbook2X
DB2TEXI_FLAGS = \
	--encoding=utf-8//TRANSLIT \
	--string-param output-file="libstdc++-manual" \
	--string-param directory-category="GNU C++ Library" \
	--string-param explicit-node-names=true
doc-texinfo-docbook: doc-xml-single-docbook ${docbook_outdir}/texinfo
	@echo "Generating texinfo files..."
	db2x_docbook2texi $(DB2TEXI_FLAGS) ${manual_xml}


# Performance doc and graph configuration.
# Assumes pychart, beautiful soup installed.
# Generates the plots and graphs for performance testing.
doc_performance_script=${top_srcdir}/scripts/make_graphs.py
doc-html-performance:
	-@(chmod + ${doc_performance_script}; \
	${doc_performance_script} ${top_srcdir} \
	${glibcxx_builddir}/testsuite \
	${top_srcdir}/testsuite/data/make_graph_htmls.xml \
	${top_srcdir}/testsuite/data/make_graph_test_infos.xml local g++)


.PHONY: doc-doxygen-html doc-doxygen-man doc-performance

# By adding these files here, automake will remove them for 'make clean'
CLEANFILES = *.log

# To remove directories.
clean-local:
	rm -rf man html pdf fo xml doxygen docbook stamp* ./libstdc++-* db2t*

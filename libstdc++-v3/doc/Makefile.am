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


# Doxygen configuration
# Assumes doxygen, graphviz (with dot) installed
doc_doxygen_script=${top_srcdir}/scripts/run_doxygen
doc-html-doxygen:
	-(srcdir=`cd ${top_srcdir}; ${PWD_COMMAND}`; \
	  builddir=`cd ..; ${PWD_COMMAND}`; \
	  ${SHELL} ${doc_doxygen_script} \
	  --host_alias=${host_alias} --mode=html $${srcdir} $${builddir} YES)

doc-man-doxygen:
	-(srcdir=`cd ${top_srcdir}; ${PWD_COMMAND}`; \
	  builddir=`cd ..; ${PWD_COMMAND}`; \
	  ${SHELL} ${doc_doxygen_script} \
	  --host_alias=${host_alias} --mode=man $${srcdir} $${builddir} YES)

doc-xml-doxygen:
	-(srcdir=`cd ${top_srcdir}; ${PWD_COMMAND}`; \
	  builddir=`cd ..; ${PWD_COMMAND}`; \
	  ${SHELL} ${doc_doxygen_script} \
	  --host_alias=${host_alias} --mode=xml $${srcdir} $${builddir} NO)

doxygen_xmldir = ${glibcxx_builddir}/doc/doxygen/xml
doc-xml-doxygen-single: doc-xml-doxygen
	@echo "Generating doxygen xml single file..."
	$(XSLTPROC) ${doxygen_xmldir}/combine.xslt ${doxygen_xmldir}/spine.xml > ${doxygen_xmldir}/all.xml;


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


# Docbook configuration.
# Assumes
# libxslt
# docbook-style-xsl
# emacs-nxml-mode 
# xmlto passivetex
xml_srcdir = ${glibcxx_srcdir}/doc/xml
xml_sources = \
	${xml_srcdir}/spine.xml \
	${xml_srcdir}/authors.xml \
	${xml_srcdir}/manual/abi.xml \
	${xml_srcdir}/manual/algorithms.xml \
	${xml_srcdir}/manual/allocator.xml \
	${xml_srcdir}/manual/auto_ptr.xml \
	${xml_srcdir}/manual/backwards_compatibility.xml \
	${xml_srcdir}/manual/bitmap_allocator.xml \
	${xml_srcdir}/manual/build_hacking.xml \
	${xml_srcdir}/manual/codecvt.xml \
	${xml_srcdir}/manual/concurrency.xml \
	${xml_srcdir}/manual/configure.xml \
	${xml_srcdir}/manual/containers.xml \
	${xml_srcdir}/manual/ctype.xml \
	${xml_srcdir}/manual/debug_mode.xml \
	${xml_srcdir}/manual/debug.xml \
	${xml_srcdir}/manual/diagnostics.xml \
	${xml_srcdir}/manual/evolution.xml \
	${xml_srcdir}/manual/extensions.xml \
	${xml_srcdir}/manual/internals.xml \
	${xml_srcdir}/manual/intro.xml \
	${xml_srcdir}/manual/io.xml \
	${xml_srcdir}/manual/iterators.xml \
	${xml_srcdir}/manual/locale.xml \
	${xml_srcdir}/manual/localization.xml \
	${xml_srcdir}/manual/messages.xml \
	${xml_srcdir}/manual/mt_allocator.xml \
	${xml_srcdir}/manual/numerics.xml \
	${xml_srcdir}/manual/parallel_mode.xml \
	${xml_srcdir}/manual/prerequisites.xml \
	${xml_srcdir}/manual/profile_mode.xml \
	${xml_srcdir}/manual/internals.xml \
	${xml_srcdir}/manual/shared_ptr.xml \
	${xml_srcdir}/manual/spine.xml \
	${xml_srcdir}/manual/status_cxx1998.xml \
	${xml_srcdir}/manual/status_cxx200x.xml \
	${xml_srcdir}/manual/status_cxxtr1.xml \
	${xml_srcdir}/manual/status_cxxtr24733.xml \
	${xml_srcdir}/manual/strings.xml \
	${xml_srcdir}/manual/support.xml \
	${xml_srcdir}/manual/test.xml \
	${xml_srcdir}/manual/using.xml \
	${xml_srcdir}/manual/utilities.xml \
	${xml_srcdir}/manual/appendix_free.xml \
	${xml_srcdir}/manual/appendix_contributing.xml \
	${xml_srcdir}/manual/appendix_porting.xml \
	${xml_srcdir}/api.xml \
	${xml_srcdir}/faq.xml

xml_sources_extra = \
	${xml_srcdir}/gnu/fdl-1.2.xml \
	${xml_srcdir}/gnu/gpl-2.0.xml

xml_noinst = \
	${xml_srcdir}/book.txml \
	${xml_srcdir}/chapter.txml \
	${xml_srcdir}/class.txml


XSLTPROC       = xsltproc
XSLTPROC_FLAGS = --nonet --xinclude
XSL_STYLE_DIR = /usr/share/sgml/docbook/xsl-stylesheets
XSL_FO_STYLE = $(XSL_STYLE_DIR)/fo/docbook.xsl
XSL_HTML_STYLE = $(XSL_STYLE_DIR)/xhtml/chunk.xsl
#XSL_HTML_SINGLE_STYLE = $(XSL_STYLE_DIR)/xhtml/onechunk.xsl
XSL_HTML_SINGLE_STYLE = $(XSL_STYLE_DIR)/xhtml/docbook.xsl

${glibcxx_builddir}/doc/html:
	mkdir ${glibcxx_builddir}/doc/html

${glibcxx_builddir}/doc/pdf:
	mkdir ${glibcxx_builddir}/doc/pdf

${glibcxx_builddir}/doc/fo:
	mkdir ${glibcxx_builddir}/doc/fo

${glibcxx_builddir}/doc/xml:
	mkdir ${glibcxx_builddir}/doc/xml

# Validate existing XML structure.
XMLLINT = xmllint
#LINT_FLAGS = --debug --nonet --xinclude --nsclean --postvalid --nowarning 
#LINT_FLAGS = --noblanks --noout --xinclude --postvalid --noent
LINT_FLAGS = --postvalid --debug --xinclude --noent --noblanks  --nonet --noout
VALID_FLAGS = --dtdvalid http://www.oasis-open.org/docbook/xml/4.5/docbookx.dtd
XMLLINT_FLAGS = $(LINT_FLAGS) $(VALID_FLAGS)
doc-xml-validate: $(xml_sources)
	@echo "Generating XML validation log..."
	$(XMLLINT) $(XMLLINT_FLAGS) ${top_srcdir}/doc/xml/spine.xml

doc-xml-single: $(xml_sources) ${glibcxx_builddir}/doc/xml
	@echo "Generating XML single..."
	$(XMLLINT) --xinclude --noent --noblanks \
	-o ${glibcxx_builddir}/doc/xml/spine-single.xml \
	${top_srcdir}/doc/xml/spine.xml

# HTML, index plus chapters
doc-html: $(xml_sources) ${glibcxx_builddir}/doc/html
	@echo "Generating html files..."
	$(XSLTPROC) $(XSLTPROC_FLAGS) -o ${glibcxx_builddir}/doc/html/ \
	$(XSL_HTML_STYLE) ${top_srcdir}/doc/xml/spine.xml

# HTML, all one page
doc-html-single: $(xml_sources) ${glibcxx_builddir}/doc/html
	@echo "Generating html single file..."
	$(XSLTPROC) $(XSLTPROC_FLAGS) -o ${glibcxx_builddir}/doc/html/ \
	$(XSL_HTML_SINGLE_STYLE) ${top_srcdir}/doc/xml/spine.xml

# FO
doc-fo: $(xml_sources) ${glibcxx_builddir}/doc/fo
	@echo "Generating FO files..."
	$(XSLTPROC) $(XSLTPROC_FLAGS) -o ${glibcxx_builddir}/doc/fo/spine.fo \
	$(XSL_FO_STYLE) ${top_srcdir}/doc/xml/spine.xml

# PDF
# Points to current best xml to PDF generation process.
doc-pdf: doc-pdf-dblatex

# PDF 1
# fop
FOP = fop
FOP_FLAGS = -d -r
doc-pdf-fop-xml: $(xml_sources) ${glibcxx_builddir}/doc/pdf 
	@echo "Generating pdf fop files from xml..."
	$(FOP) $(FOP_FLAGS) -xml ${top_srcdir}/doc/xml/spine.xml \
	-xsl $(XSL_FO_STYLE) -pdf ${glibcxx_builddir}/doc/pdf/spine.pdf

doc-pdf-fop-fo: $(xml_sources) ${glibcxx_builddir}/doc/pdf doc-fo
	@echo "Generating pdf fop files from fo..."
	$(FOP) $(FOP_FLAGS) -fo ${glibcxx_builddir}/doc/fo/spine.fo \
	-pdf ${glibcxx_builddir}/doc/pdf/spine.pdf

# PDF 2
# xmlto
XML2PDF = xmlto
XML2PDF_FLAGS = -v pdf --skip-validation -o pdf
doc-pdf-xmlto: $(xml_sources) ${glibcxx_builddir}/doc/pdf
	@echo "Generating pdf xmlto files..."
	$(XML2PDF) $(XML2PDF_FLAGS) ${top_srcdir}/doc/xml/spine.xml

# PDF 3
# xmlroff
XMLROFF = xmlroff
XMLROFF_FLAGS = --format=pdf --backend=cairo --warn=1 --debug=1 --continue
doc-pdf-xmlroff: $(xml_sources) doc-fo
	@echo "Generating pdf xmlroff files..."
	$(XMLROFF) $(XMLROFF_FLAGS) ${glibcxx_builddir}/doc/fo/spine.fo

# PDF 4
# prince
PRINCE = prince
PRINCE_FLAGS = --log prince.log -o pdf/spine.pdf
doc-pdf-prince: $(xml_sources) ${glibcxx_builddir}/doc/pdf
	@echo "Generating pdf prince files..."
	$(PRINCE) $(PRINCE_FLAGS) ${top_srcdir}/doc/xml/spine.xml

# PDF 5
# dblatex
DBLATEX_FLAGS = --dump --verbose --pdf -o pdf/manual.pdf
doc-pdf-dblatex: $(xml_sources) ${glibcxx_builddir}/doc/pdf
	@echo "Generating pdf dblatex files..."
	dblatex $(DBLATEX_FLAGS) ${top_srcdir}/doc/xml/spine.xml


.PHONY: doc-doxygen-html doc-doxygen-man doc-performance

# By adding these files here, automake will remove them for 'make clean'
CLEANFILES = *.log

# To remove directories.
clean-local:
	rm -rf man html pdf fo doxygen xml

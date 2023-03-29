## Makefile for the doc subdirectory of the GNU C++ Standard library.
##
## Copyright (C) 2008-2023 Free Software Foundation, Inc.
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
# See ./xml/manual/documentation_hacking.xml for full details.
#
# There are two main input materials for libstdc++ documentation.
# The first is the doxygen markup in libstdc++ sources, which is a
# reference to the API. And the second is the manual, via docbook markup in
# doc/xml/.
#
# A third and more obscure option deals with charting performance
# tests, and should be considered experimental.

# Documentation conditionals for output.
if BUILD_XML
STAMP_XML = doc-xml
STAMP_INSTALL_XML = doc-install-xml
else
STAMP_XML =
STAMP_INSTALL_XML =
endif

if BUILD_HTML
STAMP_HTML = doc-html
STAMP_INSTALL_HTML = doc-install-html
else
STAMP_HTML =
STAMP_INSTALL_HTML =
endif

if BUILD_MAN
STAMP_MAN = doc-man
STAMP_INSTALL_MAN = doc-install-man
else
STAMP_MAN =
STAMP_INSTALL_MAN =
endif

if BUILD_PDF
STAMP_PDF = doc-pdf
STAMP_INSTALL_PDF = doc-install-pdf
else
STAMP_PDF =
STAMP_INSTALL_PDF =
endif

if BUILD_EPUB
STAMP_EPUB = doc-epub
STAMP_INSTALL_EPUB = doc-install-epub
else
STAMP_EPUB = doc-epub
STAMP_INSTALL_EPUB = doc-install-epub
endif

# Documentation primary rules.
#
# xml:
# html:
# pdf:
# man:
# info:
# ps:
# dvi:
# epub:
# install-xml:
# install-html:
# install-pdf:
# install-man:
# install-info:
# install-ps:
# install-dvi:
# install-epub:

xml: $(STAMP_XML)
install-xml: $(STAMP_INSTALL_XML)

html: $(STAMP_HTML)
install-html: $(STAMP_INSTALL_HTML)

man: $(STAMP_MAN)
install-man: $(STAMP_INSTALL_MAN)

pdf: $(STAMP_PDF)
install-pdf: $(STAMP_INSTALL_PDF)

epub: $(STAMP_EPUB)
install-epub: $(STAMP_INSTALL_EPUB)

info:
install-info:

ps:
install-ps:

dvi:
install-dvi:


# Default creation and installation rules.
# Point to best sub-rule for the requested documentation target and
# create, and then install toplevel directory with standardized names
# and layouts.

# XML
xmldir="$(DESTDIR)@docdir@"
stamp-xml: stamp-xml-single-docbook stamp-xml-single-doxygen
	$(STAMP) stamp-xml

doc-xml: stamp-xml

doc-install-xml: doc-xml
	test -z ${xmldir} || $(mkinstalldirs) ${xmldir}
	$(INSTALL_DATA) ${manual_xml} ${xmldir}
	$(INSTALL_DATA)	${api_xml} ${xmldir}

# HTML
htmldir="$(DESTDIR)@docdir@"
stamp-html: stamp-html-docbook-data stamp-html-doxygen
	$(STAMP) stamp-html

doc-html: stamp-html

doc-install-html: doc-html
	test -z ${htmldir} || $(mkinstalldirs) ${htmldir}
	cp -r ${docbook_outdir}/html ${htmldir}/libstdc++-manual.html;
	cp -r ${doxygen_outdir}/html ${htmldir}/libstdc++-api.html;

# PDF
pdfdir="$(DESTDIR)@docdir@"
stamp-pdf: stamp-pdf-docbook stamp-pdf-doxygen
	$(STAMP) stamp-pdf

doc-pdf: stamp-pdf

doc-install-pdf: doc-pdf
	test -z ${pdfdir} || $(mkinstalldirs) ${pdfdir}
	$(INSTALL_DATA) ${docbook_outdir}/pdf/libstdc++-manual.pdf ${pdfdir}
	$(INSTALL_DATA) ${doxygen_outdir}/pdf/libstdc++-api.pdf ${pdfdir}

# MAN
mandir="$(DESTDIR)@mandir@"
stamp-man: stamp-man-doxygen
	$(STAMP) stamp-man

doc-man: stamp-man

doc-install-man: doc-man
	test -z ${mandir} || $(mkinstalldirs) ${mandir}
	cp -r ${doxygen_outdir}/man/man3 ${mandir}

# TEXINFO,INFO
infodir="$(DESTDIR)@infodir@"
stamp-texinfo: stamp-texinfo-docbook
	$(STAMP) stamp-texinfo

stamp-info: stamp-info-docbook
	$(STAMP) stamp-info

doc-texinfo: stamp-texinfo

doc-info: stamp-info

doc-install-texinfo: doc-texinfo
	test -z ${infodir} || $(mkinstalldirs) ${infodir}
	$(INSTALL_DATA) ${manual_texi} ${infodir}

doc-install-info: doc-info
	test -z ${infodir} || $(mkinstalldirs) ${infodir}
	$(INSTALL_DATA) ${manual_info} ${infodir}

# EPUB
# Assumes ruby installed
epubdir="$(DESTDIR)@docdir@"
stamp-epub: stamp-epub-docbook
	$(STAMP) stamp-epub

doc-epub: stamp-epub

doc-install-epub: doc-epub
	test -z ${epubdir} || $(mkinstalldirs) ${epubdir}
	$(INSTALL_DATA) ${manual_epub} ${epubdir}


# Doxygen configuration
# Assumes doxygen, graphviz (with dot), pdflatex installed
doxygen_script=${top_srcdir}/scripts/run_doxygen
doxygen_outdir = ${glibcxx_builddir}/doc/doxygen
api_xml = ${doxygen_outdir}/xml/libstdc++-api.xml
doxygen_pdf = ${doxygen_outdir}/latex/refman.pdf
api_pdf = ${doxygen_outdir}/pdf/libstdc++-api.pdf

${doxygen_outdir}/xml:
	mkdir -p ${doxygen_outdir}/xml

${doxygen_outdir}/html:
	mkdir -p ${doxygen_outdir}/html

${doxygen_outdir}/latex:
	mkdir -p ${doxygen_outdir}/latex

${doxygen_outdir}/pdf:
	mkdir -p ${doxygen_outdir}/pdf

${doxygen_outdir}/man:
	mkdir -p ${doxygen_outdir}/man

stamp-xml-doxygen: ${doxygen_outdir}/xml
	-srcdir=`cd ${top_srcdir}; ${PWD_COMMAND}`; \
	  builddir=`cd ..; ${PWD_COMMAND}`; \
	  ${SHELL} ${doxygen_script} \
	  --host_alias=${host_alias} --mode=xml \
	  "$${srcdir}" "$${builddir}" NO
	$(STAMP) stamp-xml-doxygen

stamp-xml-single-doxygen: stamp-xml-doxygen
	@echo "Generating doxygen xml single file..."
	$(XSLTPROC) ${doxygen_outdir}/xml/combine.xslt \
	${doxygen_outdir}/xml/index.xml > ${api_xml};
	$(STAMP) stamp-xml-single-doxygen

stamp-html-doxygen: ${doxygen_outdir}/html
	-srcdir=`cd ${top_srcdir}; ${PWD_COMMAND}`; \
	  builddir=`cd ..; ${PWD_COMMAND}`; \
	  ${SHELL} ${doxygen_script} \
	  --host_alias=${host_alias} --mode=html \
	  "$${srcdir}" "$${builddir}" YES
	$(STAMP) stamp-html-doxygen

stamp-latex-doxygen: ${doxygen_outdir}/latex
	-srcdir=`cd ${top_srcdir}; ${PWD_COMMAND}`; \
	  builddir=`cd ..; ${PWD_COMMAND}`; \
	  ${SHELL} ${doxygen_script} \
	  --host_alias=${host_alias} --mode=latex --latex_cmd=$(LATEX_CMD) \
	  "$${srcdir}" "$${builddir}" NO
	$(STAMP) stamp-latex-doxygen

# Chance of loooooonnggg creation time on this rule.  Iff this fails,
# look at refman.log and see if TeX's memory is exhausted. Symptoms
# include asking a wizard to enlarge capacity. If this is the case,
# find texmf.cnf and add a zero for pool_size, string_vacancies,
# max_strings, and pool_free values. A much simpler workaround is to
# install lualatex and set LATEX_CMD=lualatex when running make.
# Errors like "File `foo.sty' not found" mean a TeX package is missing.
stamp-pdf-doxygen: stamp-latex-doxygen ${doxygen_outdir}/pdf
	@echo "Generating doxygen pdf file...";
	-$(MAKE) -C ${doxygen_outdir}/latex -i pdf
	@if [ -f ${doxygen_pdf} ]; then \
	  mv ${doxygen_pdf} ${api_pdf} ; \
	  echo ":: PDF file is ${api_pdf}"; \
	else \
	  echo "... error"; \
	  grep -F 'LaTeX Error' ${doxygen_outdir}/latex/refman.log; \
	  grep -F 'TeX capacity exceeded, sorry' ${doxygen_outdir}/latex/refman.log; \
	  exit 12; \
	fi
	$(STAMP) stamp-pdf-doxygen

stamp-man-doxygen: ${doxygen_outdir}/man
	-srcdir=`cd ${top_srcdir}; ${PWD_COMMAND}`; \
	  builddir=`cd ..; ${PWD_COMMAND}`; \
	  ${SHELL} ${doxygen_script} \
	  --host_alias=${host_alias} --mode=man \
	  "$${srcdir}" "$${builddir}" YES
	$(STAMP) stamp-man-doxygen

doc-xml-doxygen: stamp-xml-doxygen
doc-xml-single-doxygen: stamp-xml-single-doxygen
doc-html-doxygen: stamp-html-doxygen
doc-latex-doxygen: stamp-latex-doxygen
doc-pdf-doxygen: stamp-pdf-doxygen
doc-man-doxygen: stamp-man-doxygen


# Docbook configuration.
# Assumes
# libxslt
# dblatex
# pdflatex
# docbook-style-xsl
# emacs-nxml-mode
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
	${xml_dir}/manual/documentation_hacking.xml \
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
	${xml_dir}/manual/policy_data_structures.xml \
	${xml_dir}/manual/policy_data_structures_biblio.xml \
	${xml_dir}/manual/prerequisites.xml \
	${xml_dir}/manual/shared_ptr.xml \
	${xml_dir}/manual/spine.xml \
	${xml_dir}/manual/status_cxx1998.xml \
	${xml_dir}/manual/status_cxx2011.xml \
	${xml_dir}/manual/status_cxx2014.xml \
	${xml_dir}/manual/status_cxx2017.xml \
	${xml_dir}/manual/status_cxx2020.xml \
	${xml_dir}/manual/status_cxxtr1.xml \
	${xml_dir}/manual/status_cxxtr24733.xml \
	${xml_dir}/manual/strings.xml \
	${xml_dir}/manual/support.xml \
	${xml_dir}/manual/test.xml \
	${xml_dir}/manual/test_policy_data_structures.xml \
	${xml_dir}/manual/using.xml \
	${xml_dir}/manual/using_exceptions.xml \
	${xml_dir}/manual/utilities.xml \
	${xml_dir}/manual/appendix_free.xml \
	${xml_dir}/manual/appendix_contributing.xml \
	${xml_dir}/manual/appendix_porting.xml

xml_sources_extra = \
	${xml_dir}/gnu/fdl-1.3.xml \
	${xml_dir}/gnu/gpl-3.0.xml

xml_sources = \
	${xml_sources_basic} \
	${xml_sources_manual} \
	${xml_sources_extra}

xml_image_dir = ${xml_dir}/images
xml_image_basic = \
	${xml_image_dir}/confdeps.png \
	${xml_image_dir}/pbds_balls_and_bins.png \
	${xml_image_dir}/pbds_container_tag_hierarchy.png \
	${xml_image_dir}/pbds_different_underlying_dss_1.png \
	${xml_image_dir}/pbds_different_underlying_dss_2.png \
	${xml_image_dir}/pbds_embedded_lists_1.png \
	${xml_image_dir}/pbds_embedded_lists_2.png \
	${xml_image_dir}/pbds_embedded_lists_3.png \
	${xml_image_dir}/pbds_exception_hierarchy.png \
	${xml_image_dir}/pbds_hash_policy_cd.png \
	${xml_image_dir}/pbds_hash_ranged_hash_range_hashing_fns.png \
	${xml_image_dir}/pbds_hash_range_hashing_seq_diagram2.png \
	${xml_image_dir}/pbds_hash_range_hashing_seq_diagram.png \
	${xml_image_dir}/pbds_insert_resize_sequence_diagram1.png \
	${xml_image_dir}/pbds_insert_resize_sequence_diagram2.png \
	${xml_image_dir}/pbds_insert_resize_sequence_diagram3.png \
	${xml_image_dir}/pbds_invalidation_guarantee_erase.png \
	${xml_image_dir}/pbds_invalidation_tag_hierarchy.png \
	${xml_image_dir}/pbds_list_update.png \
	${xml_image_dir}/pbds_node_invariants.png \
	${xml_image_dir}/pbds_pat_trie.png \
	${xml_image_dir}/pbds_point_iterator_hierarchy.png \
	${xml_image_dir}/pbds_point_iterators_range_ops_1.png \
	${xml_image_dir}/pbds_point_iterators_range_ops_2.png \
	${xml_image_dir}/pbds_priority_queue_different_underlying_dss.png \
	${xml_image_dir}/pbds_priority_queue_tag_hierarchy.png \
	${xml_image_dir}/pbds_rationale_null_node_updator.png \
	${xml_image_dir}/pbds_resize_policy_cd.png \
	${xml_image_dir}/pbds_restoring_node_invariants.png \
	${xml_image_dir}/pbds_simple_list.png \
	${xml_image_dir}/pbds_tree_node_invalidations.png \
	${xml_image_dir}/pbds_tree_node_invariants.png \
	${xml_image_dir}/pbds_tree_node_updator_policy_cd.png \
	${xml_image_dir}/pbds_trie_node_updator_policy_cd.png \
	${xml_image_dir}/pbds_update_seq_diagram.png

xml_image_generated = \
	${xml_image_dir}/pbds_binary_priority_queue_int_push_pop.png \
	${xml_image_dir}/pbds_binary_priority_queue_int_push.png \
	${xml_image_dir}/pbds_ccgp_hash_int_subscript_insert.png \
	${xml_image_dir}/pbds_cc_hash_int_find.png \
	${xml_image_dir}/pbds_cc_hash_int_subscript_find.png \
	${xml_image_dir}/pbds_cc_hash_int_subscript_insert.png \
	${xml_image_dir}/pbds_gp_hash_int_find.png \
	${xml_image_dir}/pbds_gp_hash_int_subscript_find.png \
	${xml_image_dir}/pbds_gp_hash_int_subscript_insert.png \
	${xml_image_dir}/pbds_hash_int_erase_mem.png \
	${xml_image_dir}/pbds_hash_text_find.png \
	${xml_image_dir}/pbds_hash_zlob_int_find.png \
	${xml_image_dir}/pbds_multimap_text_find_large_s2p_hash.png \
	${xml_image_dir}/pbds_multimap_text_find_large_s2p_tree.png \
	${xml_image_dir}/pbds_multimap_text_find_small_s2p_hash.png \
	${xml_image_dir}/pbds_multimap_text_find_small_s2p_tree.png \
	${xml_image_dir}/pbds_multimap_text_insert_large_s2p_hash.png \
	${xml_image_dir}/pbds_multimap_text_insert_large_s2p_tree.png \
	${xml_image_dir}/pbds_multimap_text_insert_mem_large_s2p_hash.png \
	${xml_image_dir}/pbds_multimap_text_insert_mem_large_s2p_tree.png \
	${xml_image_dir}/pbds_multimap_text_insert_mem_small_s2p_hash.png \
	${xml_image_dir}/pbds_multimap_text_insert_mem_small_s2p_tree.png \
	${xml_image_dir}/pbds_multimap_text_insert_small_s2p_hash.png \
	${xml_image_dir}/pbds_multimap_text_insert_small_s2p_tree.png \
	${xml_image_dir}/pbds_pairing_priority_queue_text_modify_down_thin.png \
	${xml_image_dir}/pbds_pairing_priority_queue_text_modify_up_thin.png \
	${xml_image_dir}/pbds_pairing_priority_queue_text_push_pop.png \
	${xml_image_dir}/pbds_pairing_priority_queue_text_push.png \
	${xml_image_dir}/pbds_priority_queue_int_push_pop.png \
	${xml_image_dir}/pbds_priority_queue_int_push.png \
	${xml_image_dir}/pbds_priority_queue_text_join.png \
	${xml_image_dir}/pbds_priority_queue_text_modify_down.png \
	${xml_image_dir}/pbds_priority_queue_text_modify_up.png \
	${xml_image_dir}/pbds_priority_queue_text_pop_mem.png \
	${xml_image_dir}/pbds_priority_queue_text_push_pop.png \
	${xml_image_dir}/pbds_priority_queue_text_push.png \
	${xml_image_dir}/pbds_tree_int_find.png \
	${xml_image_dir}/pbds_tree_order_statistics.png \
	${xml_image_dir}/pbds_tree_split_join.png \
	${xml_image_dir}/pbds_tree_text_find.png \
	${xml_image_dir}/pbds_tree_text_insert_node.png \
	${xml_image_dir}/pbds_tree_text_insert_trie.png \
	${xml_image_dir}/pbds_tree_text_insert_vector.png \
	${xml_image_dir}/pbds_tree_text_lor_find.png

xml_images = ${xml_image_basic} ${xml_image_generated}

xml_noinst = \
	${xml_dir}/book.txml \
	${xml_dir}/chapter.txml \
	${xml_dir}/class.txml \
	${xml_image_dir}/confdeps.dot \
	${xml_image_dir}/confdeps.pdf

XSLTPROC       = xsltproc
XSLT_FLAGS = --nonet --xinclude
XSLT_PARAM = --param toc.section.depth 4 --param generate.consistent.ids 1
#XSL_STYLE_DIR = /usr/share/xml/docbook/stylesheet/docbook-xsl-ns
#XSL_STYLE_DIR = /usr/share/sgml/docbook/xsl-ns-stylesheets
XSL_FO_STYLE = $(XSL_STYLE_DIR)/fo/docbook.xsl
XSL_HTML_STYLE = $(XSL_STYLE_DIR)/xhtml/chunk.xsl
XSL_HTML_SINGLE_STYLE = $(XSL_STYLE_DIR)/html/docbook.xsl
XSL_EPUB_STYLE = $(XSL_STYLE_DIR)/epub3/chunk.xsl
XSL_LOCAL_STYLE = ${glibcxx_builddir}/doc/xsl/customization.xsl

${docbook_outdir}/epub:
	mkdir -p ${docbook_outdir}/epub
	mkdir -p ${docbook_outdir}/epub/OEBPS/images

${docbook_outdir}/fo:
	mkdir -p ${docbook_outdir}/fo

${docbook_outdir}/html:
	mkdir -p ${docbook_outdir}/html
	mkdir -p ${docbook_outdir}/html/images
	mkdir -p ${docbook_outdir}/html/manual

${docbook_outdir}/pdf:
	mkdir -p ${docbook_outdir}/pdf

${docbook_outdir}/latex:
	mkdir -p ${docbook_outdir}/latex

${docbook_outdir}/texinfo:
	mkdir -p ${docbook_outdir}/texinfo

${docbook_outdir}/xml:
	mkdir -p ${docbook_outdir}/xml


# XML, all one page
# Some info on canonicalization
# http://www.mail-archive.com/help-texinfo@gnu.org/msg00864.html
manual_xml = ${docbook_outdir}/xml/libstdc++-manual.xml
set_xml = ${docbook_outdir}/xml/libstdc++-set.xml
stamp-xml-single-docbook: $(xml_sources) ${docbook_outdir}/xml
	@echo "Generating XML single..."
	$(XMLLINT) $(XMLLINT_FLAGS) \
	${top_srcdir}/doc/xml/manual/spine.xml > ${manual_xml};
	$(XMLLINT) $(XMLLINT_FLAGS) \
	${top_srcdir}/doc/xml/spine.xml > ${set_xml};
	$(STAMP) stamp-xml-single-docbook

doc-xml-single-docbook: stamp-xml-single-docbook

# Validate existing XML structure.
XMLLINT = xmllint
LINT_FLAGS = --debug --xinclude --noent --noblanks --noout
SCHEMA_FLAGS = --dtdvalid http://www.docbook.org/xml/5.0/dtd/docbook.dtd
SCHEMA_RNG_FLAGS = --relaxng http://www.docbook.org/xml/5.0/rng/docbook.rng
#SCHEMA_FLAGS = --nonet --dtdvalid /usr/share/xxx/dtd/5.0/docbook.dtd
XMLLINT_VALID_FLAGS = $(LINT_FLAGS) $(SCHEMA_FLAGS)
XMLLINT_FLAGS = --xinclude --nsclean --c14n --noent --noblanks --nocdata
doc-xml-validate-docbook: doc-xml-validate-dtd-db

doc-xml-validate-dtd-db: $(xml_sources) $(xml_images)
	@echo "Generating XML validation log..."
	$(XMLLINT) $(XMLLINT_VALID_FLAGS) ${top_srcdir}/doc/xml/spine.xml

doc-xml-validate-rng-db: $(xml_sources) $(xml_images) doc-xml-single-docbook
	@echo "Generating XML RelaxNG validation log..."
	$(XMLLINT) $(LINT_FLAGS) $(SCHEMA_RNG_FLAGS) ${set_xml}

# HTML support files
stamp-html-docbook-images: stamp-html-docbook $(xml_images)
	$(INSTALL_DATA)	$(xml_images) ${docbook_outdir}/html/images
	$(STAMP) stamp-html-docbook-images

stamp-html-docbook-data: stamp-html-docbook-images
	$(STAMP) stamp-html-docbook-data

# HTML, "chunked" into index plus chapters as separate pages
stamp-html-docbook: $(xml_sources) ${docbook_outdir}/html
	@echo "Generating html files..."
	$(XSLTPROC) $(XSLT_PARAM) $(XSLT_FLAGS) -o ${docbook_outdir}/html/ \
	 --stringparam chunker.output.encoding UTF-8 \
	$(XSL_HTML_STYLE) \
	${top_srcdir}/doc/xml/spine.xml
	$(STAMP) stamp-html-docbook

doc-html-docbook: stamp-html-docbook-data

# Generate the HTML pages and copy them back to the source tree.
doc-html-docbook-regenerate: doc-html-docbook
	$(INSTALL_DATA) ${docbook_outdir}/html/*.html ${top_srcdir}/doc/html
	$(INSTALL_DATA) ${docbook_outdir}/html/images/* ${top_srcdir}/doc/html/images
	$(INSTALL_DATA) ${docbook_outdir}/html/manual/*.html ${top_srcdir}/doc/html/manual

# HTML, all one page
# NB: Have to generate customization XSL for UTF-8 output.
manual_html = ${docbook_outdir}/html/libstdc++-manual-single.html
stamp-html-single-docbook: $(xml_sources) ${docbook_outdir}/html
	@echo "Generating html single file..."
	$(XSLTPROC) $(XSLT_PARAM) $(XSLT_FLAGS) -o ${manual_html} \
	${XSL_LOCAL_STYLE} \
	${top_srcdir}/doc/xml/spine.xml
	$(STAMP) stamp-html-single-docbook

doc-html-single-docbook: stamp-html-single-docbook

# FO
stamp-fo-docbook: $(xml_sources) ${docbook_outdir}/fo
	@echo "Generating FO files..."
	$(XSLTPROC) $(XSLT_FLAGS) -o ${docbook_outdir}/fo/spine.fo \
	$(XSL_FO_STYLE) ${top_srcdir}/doc/xml/spine.xml
	$(STAMP) stamp-fo-docbook

doc-fo-docbook: stamp-fo-docbook

# PDF, via dblatex
manual_pdf = ${docbook_outdir}/pdf/libstdc++-manual.pdf
DBLATEX_FLAGS = --verbose --pdf --dump --debug --tmpdir=${docbook_outdir}/latex

doc-pdf-docbook-dirs: ${docbook_outdir}/pdf ${docbook_outdir}/latex

doc-pdf-docbook-pre: $(xml_sources) doc-pdf-docbook-dirs

stamp-pdf-docbook: doc-pdf-docbook-pre doc-xml-single-docbook
	@echo "Generating pdf dblatex files..."
	dblatex $(DBLATEX_FLAGS) -o ${manual_pdf} \
	${top_srcdir}/doc/xml/spine.xml
	$(STAMP) stamp-pdf-docbook

doc-pdf-docbook: stamp-pdf-docbook

# TEXINFO, via docbook2X
# NB: Both experimental and tempermental
manual_texi = ${docbook_outdir}/texinfo/libstdc++-manual.texi
manual_info = ${docbook_outdir}/texinfo/libstdc++-manual.info
DB2TEXI_FLAGS = \
	--encoding=utf-8//TRANSLIT \
	--string-param output-file="libstdc++-manual" \
	--string-param directory-category="GNU C++ Library" \
	--string-param explicit-node-names=true

stamp-texinfo-docbook: stamp-xml-single-docbook ${docbook_outdir}/texinfo
	@echo "Generating texinfo files..."
	db2x_docbook2texi $(DB2TEXI_FLAGS) ${set_xml}
	mv libstdc++-manual.texi ${manual_texi}
	$(STAMP) stamp-texinfo-docbook

stamp-info-docbook: stamp-texinfo-docbook
	@echo "Generating info files..."
	$(MAKEINFO) $(MAKEINFOFLAGS) ${manual_texi}
	$(STAMP) stamp-info-docbook

doc-texinfo-docbook: stamp-texinfo-docbook

doc-info-docbook: stamp-info-docbook

# EPUB version 3
# http://sourceforge.net/projects/docbook/files/epub3/
# Can verify document with epubcheck
manual_epub = ${docbook_outdir}/epub/libstdc++-manual.epub
stamp-epub-docbook: stamp-xml-single-docbook ${docbook_outdir}/epub
	@echo "Generating epub files..."
	if [ ! -d "${docbook_outdir}/images" ]; then \
	  $(LN_S) ${top_srcdir}/doc/xml/images ${docbook_outdir}/; \
	fi
	$(XSLTPROC) $(XSLT_PARAM) $(XSLT_FLAGS) \
	--stringparam base.dir "${docbook_outdir}/epub/OEBPS/" \
	$(XSL_EPUB_STYLE) ${top_srcdir}/doc/xml/spine.xml
	$(INSTALL_DATA)	$(xml_images) ${docbook_outdir}/epub/OEBPS/images
	zip -X0 ${manual_epub} ${docbook_outdir}/epub/mimetype
	zip -r -X9 ${manual_epub} ${docbook_outdir}/epub/META-INF ${docbook_outdir}/epub/OEBPS
	$(STAMP) stamp-epub-docbook

doc-epub-docbook: stamp-epub-docbook

# Performance doc and graph configuration.
# Assumes pychart, beautiful soup installed.
# Generates the plots/graph imagery for performance testing.
doc_performance_script=${top_srcdir}/scripts/make_graph.py
doc-svg-performance: ${docbook_outdir}/xml
	-@(chmod + ${doc_performance_script}; \
	${doc_performance_script} \
	${top_srcdir}/testsuite/data/make_graph_test_infos.xml \
	${glibcxx_builddir}/testsuite \
	${glibcxx_builddir}/doc/docbook/xml/images)

.PHONY: doc-doxygen-html doc-doxygen-man doc-performance

# By adding these files here, automake will remove them for 'make clean'
CLEANFILES = *.log stamp*

# To remove directories.
clean-local:
	rm -rf man html pdf fo xml doxygen docbook ./libstdc++-* db2t*

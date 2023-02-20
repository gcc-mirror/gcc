## Makefile for the toplevel directory of the GNU C++ Standard library.
##
## Copyright (C) 1997-2023 Free Software Foundation, Inc.
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

if GLIBCXX_HOSTED
## Note that python must come after src.
  hosted_source = doc po testsuite python
endif

## Keep this list sync'd with acinclude.m4:GLIBCXX_CONFIGURE.
SUBDIRS = include libsupc++ src $(hosted_source)

ACLOCAL_AMFLAGS = -I . -I .. -I ../config

# Testsuite/check forwarding targets.
check-%:
	cd testsuite && $(MAKE) $@

# Documentation forwarding targets.
doc-%:
	cd doc && $(MAKE) $@

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

# Multilib support.
MAKEOVERRIDES=

# All the machinations with string instantiations messes up the
# automake-generated TAGS rule. Make a simple one here.
TAGS: tags-recursive $(LISP)

# Work around what appears to be a GNU make bug handling MAKEFLAGS
# values defined in terms of make variables, as is the case for CC and
# friends when we are called from the top level Makefile.
AM_MAKEFLAGS = \
	"AR_FLAGS=$(AR_FLAGS)" \
	"CC_FOR_BUILD=$(CC_FOR_BUILD)" \
	"CC_FOR_TARGET=$(CC_FOR_TARGET)" \
	"CFLAGS=$(CFLAGS)" \
	"CXXFLAGS=$(CXXFLAGS)" \
	"CFLAGS_FOR_BUILD=$(CFLAGS_FOR_BUILD)" \
	"CFLAGS_FOR_TARGET=$(CFLAGS_FOR_TARGET)" \
	"INSTALL=$(INSTALL)" \
	"INSTALL_DATA=$(INSTALL_DATA)" \
	"INSTALL_PROGRAM=$(INSTALL_PROGRAM)" \
	"INSTALL_SCRIPT=$(INSTALL_SCRIPT)" \
	"LDFLAGS=$(LDFLAGS)" \
	"LIBCFLAGS=$(LIBCFLAGS)" \
	"LIBCFLAGS_FOR_TARGET=$(LIBCFLAGS_FOR_TARGET)" \
	"MAKE=$(MAKE)" \
	"MAKEINFO=$(MAKEINFO) $(MAKEINFOFLAGS)" \
	"SHELL=$(SHELL)" \
	"RUNTESTFLAGS=$(RUNTESTFLAGS)" \
	"exec_prefix=$(exec_prefix)" \
	"infodir=$(infodir)" \
	"libdir=$(libdir)" \
	"includedir=$(includedir)" \
	"prefix=$(prefix)" \
	"tooldir=$(tooldir)" \
	"gxx_include_dir=$(gxx_include_dir)" \
	"AR=$(AR)" \
	"AS=$(AS)" \
	"LD=$(LD)" \
	"RANLIB=$(RANLIB)" \
	"NM=$(NM)" \
	"NM_FOR_BUILD=$(NM_FOR_BUILD)" \
	"NM_FOR_TARGET=$(NM_FOR_TARGET)" \
	"DESTDIR=$(DESTDIR)" \
	"WERROR=$(WERROR)"

# Subdir rules rely on $(FLAGS_TO_PASS)
FLAGS_TO_PASS = $(AM_MAKEFLAGS)

# target overrides
-include $(tmake_file)

include $(top_srcdir)/../multilib.am

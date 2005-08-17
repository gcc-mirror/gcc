## Makefile for the po subdirectory of the GNU C++ Standard library.
##
## Copyright (C) 2001, 2003, 2004 Free Software Foundation, Inc.
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

# Location of installation directories.
locale_installdir = $(DESTDIR)$(datadir)/locale
locale_builddir = $(glibcxx_localedir)

# Tell automake that foo.po makes foo.mo
SUFFIXES = .po .mo

LOCALE_IN = $(glibcxx_POFILES)
LOCALE_OUT = $(glibcxx_MOFILES)

MSGFMT = msgfmt

# Necessary files.
EXTRA_DIST = string_literals.cc POTFILES.in $(PACKAGE).pot $(LOCALE_IN)

.po.mo:
	$(MSGFMT) -o $@ $<

all-local: all-local-$(USE_NLS)
all-local-no:
all-local-yes: $(LOCALE_OUT)

# 'make check' needs the catalogs constructed in build directory.
check: check-$(USE_NLS)
check-no:
check-yes:
	$(mkinstalldirs) $(locale_builddir)
	catalogs='$(LOCALE_OUT)'; \
	for cat in $$catalogs; do \
	  cat=`basename $$cat`; \
	  lang=`echo $$cat | sed 's/\.mo$$//'`; \
	  install_dir=$(locale_builddir)/$$lang/LC_MESSAGES; \
	  $(mkinstalldirs) $$install_dir; \
	  $(INSTALL_DATA) $$cat $$install_dir/$(PACKAGE).mo; \
	done

# Install rules here.
# Wish install could just `cp -R ./share $(locale_installdir)` ...
install-data-local: install-data-local-$(USE_NLS)
install-data-local-no:
install-data-local-yes: all-local-yes
	$(mkinstalldirs) $(locale_installdir)
	catalogs='$(LOCALE_OUT)'; \
	for cat in $$catalogs; do \
	  cat=`basename $$cat`; \
	  lang=`echo $$cat | sed 's/\.mo$$//'`; \
	  install_dir=$(locale_installdir)/$$lang/LC_MESSAGES; \
	  $(mkinstalldirs) $$install_dir; \
	  $(INSTALL_DATA) $$cat $$install_dir/$(PACKAGE).mo; \
	done

# Maintainence of the .po template file.  This rule is never run automatically,
# and updates the source directory.
pot:
	cd $(glibcxx_srcdir); \
	xgettext --default-domain=$(PACKAGE) --add-comments --c++ --debug \
	  --join-existing -o po/$(PACKAGE).pot --keyword=__N \
	  `grep -r -l '__N(".*")' .`

# Specify what gets cleaned up on a 'make clean'
CLEANFILES = $(LOCALE_OUT)

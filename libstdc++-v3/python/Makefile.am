## Makefile for the python subdirectory of the GNU C++ Standard library.
##
## Copyright (C) 2009 Free Software Foundation, Inc.
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

## Where to install the module code.
pythondir = $(pkgdatadir)/python

all-local: gdb.py

nobase_python_DATA = \
    libstdcxx/v6/printers.py \
    libstdcxx/v6/__init__.py \
    libstdcxx/__init__.py

gdb.py: hook.in Makefile
	sed -e 's,@dir@,$(pythondir),' < $(srcdir)/hook.in > $@

install-data-local: gdb.py
	@$(mkdir_p) $(DESTDIR)$(toolexeclibdir)
	@libname=`cd $(toolexeclibdir) && ls -r libstdc++* | fgrep -v gdb.py | sed 1q`; \
	echo " $(INSTALL_DATA) gdb.py $(DESTDIR)$(toolexeclibdir)/$$libname-gdb.py"; \
	$(INSTALL_DATA) gdb.py $(DESTDIR)$(toolexeclibdir)/$$libname-gdb.py

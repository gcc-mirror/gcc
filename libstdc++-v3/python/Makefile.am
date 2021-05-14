## Makefile for the python subdirectory of the GNU C++ Standard library.
##
## Copyright (C) 2009-2021 Free Software Foundation, Inc.
##
## This file is part of the libstdc++ version 3 distribution.
## Process this file with automake to produce Makefile.in.

## This file is part of the GNU ISO C++ Library.  This library is free
## software; you can redistribute it and/or modify it under the
## terms of the GNU General Public License as published by the
## Free Software Foundation; either version 3, or (at your option)
## any later version.
##
## This library is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License along
## with this library; see the file COPYING3.  If not see
## <http://www.gnu.org/licenses/>.

include $(top_srcdir)/fragment.am

## Where to install the module code.
if ENABLE_PYTHONDIR
pythondir = $(prefix)/$(python_mod_dir)
else
pythondir = $(datadir)/gcc-$(gcc_version)/python
endif

all-local: gdb.py

nobase_python_DATA = \
    libstdcxx/v6/printers.py \
    libstdcxx/v6/xmethods.py \
    libstdcxx/v6/__init__.py \
    libstdcxx/__init__.py

gdb.py: hook.in Makefile
	sed -e 's,@pythondir@,$(pythondir),' \
	    -e 's,@toolexeclibdir@,$(toolexeclibdir),' < $(srcdir)/hook.in > $@

install-data-local: gdb.py
	@$(mkdir_p) $(DESTDIR)$(toolexeclibdir)
## We want to install gdb.py as SOMETHING-gdb.py.  SOMETHING is the
## full name of the final library.  We use the libtool .la file to get
## the correct name.
	@libname=`sed -ne "/^library_names=/{s/.*='//;s/'$$//;s/ .*//;p;}" \
	          $(DESTDIR)$(toolexeclibdir)/libstdc++.la`; \
	echo " $(INSTALL_DATA) gdb.py $(DESTDIR)$(toolexeclibdir)/$$libname-gdb.py"; \
	$(INSTALL_DATA) gdb.py $(DESTDIR)$(toolexeclibdir)/$$libname-gdb.py

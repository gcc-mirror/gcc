# Makefile for libm2cor.
#   Copyright 2013-2022  Free Software Foundation, Inc.
#
# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

SUFFIXES = .c .cc .mod .def .o .obj .lo .a .la

ACLOCAL_AMFLAGS = -I .. -I ../config

VPATH = . @srcdir@ @srcdir@/../../gcc/m2/gm2-libs-coroutines

# Multilib support.
MAKEOVERRIDES=

gcc_version := $(shell @get_gcc_base_ver@ $(top_srcdir)/../gcc/BASE-VER)

# Directory in which the compiler finds libraries etc.
libsubdir = $(libdir)/gcc/$(target_alias)/$(gcc_version)
# Used to install the shared libgcc.
slibdir = @slibdir@

toolexeclibdir=@toolexeclibdir@
toolexecdir=@toolexecdir@
GM2_FOR_TARGET=@GM2_FOR_TARGET@

MULTIDIR := $(shell $(CC) $(CFLAGS) -print-multi-directory)
MULTIOSDIR := $(shell $(CC) $(CFLAGS) -print-multi-os-directory)

MULTIOSSUBDIR := $(shell if test x$(MULTIOSDIR) != x.; then echo /$(MULTIOSDIR); fi)
inst_libdir = $(libsubdir)$(MULTISUBDIR)
inst_slibdir = $(slibdir)$(MULTIOSSUBDIR)


# Work around what appears to be a GNU make bug handling MAKEFLAGS
# values defined in terms of make variables, as is the case for CC and
# friends when we are called from the top level Makefile.
AM_MAKEFLAGS = \
        "GCC_DIR=$(GCC_DIR)" \
        "GM2_SRC=$(GM2_SRC)" \
	"AR_FLAGS=$(AR_FLAGS)" \
	"CC_FOR_BUILD=$(CC_FOR_BUILD)" \
	"CC_FOR_TARGET=$(CC_FOR_TARGET)" \
	"GM2_FOR_TARGET=$(GM2_FOR_TARGET)" \
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
        "MULTIBUILDTOP=$(MULTIBUILDTOP)" \
        "MULTISUBDIR=$(MULTISUBDIR)" \
        "MULTIOSDIR=$(MULTIOSDIR)" \
        "MULTIFLAGS=$(MULTIFLAGS)" \
	"PICFLAG=$(PICFLAG)" \
	"PICFLAG_FOR_TARGET=$(PICFLAG_FOR_TARGET)" \
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
	"WERROR=$(WERROR)" \
        "TARGET_LIB_PATH_libgm2=$(TARGET_LIB_PATH_libgm2)"

# Subdir rules rely on $(FLAGS_TO_PASS)
FLAGS_TO_PASS = $(AM_MAKEFLAGS)


if BUILD_CORLIB
M2DEFS = Executive.def \
         KeyBoardLEDs.def  SYSTEM.def \
         TimerHandler.def

M2MODS = Executive.mod \
         SYSTEM.mod  TimerHandler.mod

toolexeclib_LTLIBRARIES = libm2cor.la

libm2cor_la_SOURCES = $(M2MODS) KeyBoardLEDs.cc

nodist_EXTRA_libm2cor_la_SOURCES = dummy.c  ## forces automake to generate the LINK definition

libm2cordir = libm2cor
libm2cor_la_DEPENDENCIES = SYSTEM.def $(addsuffix .lo, $(basename $(libm2cor_la_SOURCES)))
libm2cor_la_CFLAGS = -I. -I.. -I$(GM2_SRC)/gm2-libs -I$(GM2_SRC)/gm2-libs-iso -DBUILD_GM2_LIBS -I@srcdir@/../  -I@srcdir@/../libm2iso
libm2cor_la_M2FLAGS = \
    -fm2-pathname=m2cor -I. -I$(GM2_SRC)/gm2-libs-coroutines \
    -fm2-pathname=m2pim -I$(GM2_SRC)/gm2-libs \
    -fm2-pathname=m2iso -I$(GM2_SRC)/gm2-libs-iso \
    -fm2-g -g -Wreturn-type -fcase -fm2-prefix=m2cor
if TARGET_DARWIN
libm2cor_la_link_flags = -Wl,-undefined,dynamic_lookup
else
libm2cor_la_link_flags =
endif
libm2cor_la_LINK = $(LINK) -version-info $(libtool_VERSION) $(libm2cor_la_link_flags)
BUILT_SOURCES = SYSTEM.def
CLEANFILES = SYSTEM.def

M2LIBDIR = /m2/m2cor/

SYSTEM.def: Makefile
	bash $(GM2_SRC)/tools-src/makeSystem -fpim \
             $(GM2_SRC)/gm2-libs-coroutines/SYSTEM.def \
             $(GM2_SRC)/gm2-libs-coroutines/SYSTEM.mod \
             -I$(GM2_SRC)/gm2-libs-coroutines -I$(GM2_SRC)/gm2-libs -I$(GM2_SRC)/gm2-libs-iso \
             "$(GM2_FOR_TARGET)" $@

.mod.lo:
	$(LIBTOOL) --tag=CC $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) --mode=compile $(GM2_FOR_TARGET) -c $(CFLAGS_FOR_TARGET) $(LIBCFLAGS) $(libm2cor_la_M2FLAGS) $< -o $@

.cc.lo:
	$(LIBTOOL) --tag=CXX --mode=compile $(CXX) -c -I$(srcdir) $(CXXFLAGS) $(LIBCFLAGS) $(libm2cor_la_CFLAGS) $< -o $@

install-data-local: force
	mkdir -p $(DESTDIR)$(inst_libdir)/$(M2LIBDIR)
	for i in $(M2DEFS) $(M2MODS) ; do \
           if [ -f $$i ] ; then \
              $(INSTALL_DATA) $$i '$(DESTDIR)$(inst_libdir)/$(M2LIBDIR)'; \
           elif [ -f @srcdir@/../../gcc/m2/gm2-libs-coroutines/$$i ] ; then \
              $(INSTALL_DATA) @srcdir@/../../gcc/m2/gm2-libs-coroutines/$$i '$(DESTDIR)$(inst_libdir)/$(M2LIBDIR)'; \
           else \
              echo "cannot find $$i" ; exit 1 ; \
           fi ; \
           chmod 644 $(DESTDIR)$(inst_libdir)/$(M2LIBDIR)$$i ; \
	done

force:

endif

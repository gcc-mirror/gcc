# Makefile for libm2log.
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

SUFFIXES = .c .mod .def .o .obj .lo .a .la

ACLOCAL_AMFLAGS = -I .. -I ../config

VPATH = . @srcdir@ @srcdir@/../../gcc/m2/gm2-libs-log

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
        "MULTISUBDIR=$(MULTISUBDIR)" \
        "MULTIOSDIR=$(MULTIOSDIR)" \
        "MULTIBUILDTOP=$(MULTIBUILDTOP)" \
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


if BUILD_LOGLIB
M2DEFS = BitBlockOps.def  BitByteOps.def \
         BitWordOps.def  BlockOps.def \
         Break.def  CardinalIO.def \
         Conversions.def  DebugPMD.def \
         DebugTrace.def  Delay.def \
         Display.def  ErrorCode.def \
         FileSystem.def  FloatingUtilities.def \
         InOut.def  Keyboard.def \
         LongIO.def  NumberConversion.def \
         Random.def  RealConversions.def \
         RealInOut.def  Strings.def \
         Termbase.def  Terminal.def \
         TimeDate.def

M2MODS = BitBlockOps.mod  BitByteOps.mod \
         BitWordOps.mod  BlockOps.mod \
         CardinalIO.mod  Conversions.mod \
         DebugPMD.mod  DebugTrace.mod \
         Delay.mod  Display.mod \
         ErrorCode.mod  FileSystem.mod \
         FloatingUtilities.mod  InOut.mod \
         Keyboard.mod  LongIO.mod \
         NumberConversion.mod  Random.mod \
         RealConversions.mod  RealInOut.mod \
         Strings.mod  Termbase.mod \
         Terminal.mod  TimeDate.mod


libm2logdir = libm2log
toolexeclib_LTLIBRARIES = libm2log.la
libm2log_la_SOURCES = $(M2MODS) Break.c

libm2log_la_DEPENDENCIES = ../libm2pim/SYSTEM.def $(addsuffix .lo, $(basename $(libm2log_la_SOURCES)))
libm2log_la_CFLAGS = -I. -DBUILD_GM2_LIBS -I@srcdir@/../
libm2log_la_M2FLAGS = \
 -fm2-pathname=m2pim -I../libm2pim \
 -fm2-pathname=m2log -I$(GM2_SRC)/gm2-libs-log \
 -fm2-pathname=m2pim -I$(GM2_SRC)/gm2-libs \
 -fm2-pathname=m2iso -I$(GM2_SRC)/gm2-libs-iso \
 -Wcase-enum -Wreturn-type \
 -fcase -fm2-prefix=m2log

if TARGET_DARWIN
libm2log_la_link_flags = -Wl,-undefined,dynamic_lookup
else
libm2log_la_link_flags =
endif
if ENABLE_DARWIN_AT_RPATH
libm2log_la_link_flags += -nodefaultrpaths -Wl,-rpath,@loader_path/
endif
libm2log_la_LINK = $(LINK) -version-info $(libtool_VERSION) $(libm2log_la_link_flags)
BUILT_SOURCES = ../libm2pim/SYSTEM.def

M2LIBDIR = /m2/m2log/

../libm2pim/SYSTEM.def: ../libm2pim/Makefile
	cd ../libm2pim ; $(MAKE) $(AM_MAKEFLAGS) SYSTEM.def

.mod.lo:
	$(LIBTOOL) --tag=CC $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) --mode=compile \
                   $(GM2_FOR_TARGET) -c \
                   $(CFLAGS_FOR_TARGET) $(LIBCFLAGS) $(libm2log_la_M2FLAGS) $< -o $@

install-data-local: force
	mkdir -p $(DESTDIR)$(inst_libdir)/$(M2LIBDIR)
	for i in $(M2DEFS) $(M2MODS) ; do \
           if [ -f $$i ] ; then \
              $(INSTALL_DATA) $$i '$(DESTDIR)$(inst_libdir)/$(M2LIBDIR)'; \
           elif [ -f @srcdir@/../../gcc/m2/gm2-libs-log/$$i ] ; then \
              $(INSTALL_DATA) @srcdir@/../../gcc/m2/gm2-libs-log/$$i '$(DESTDIR)$(inst_libdir)/$(M2LIBDIR)'; \
           else \
              echo "cannot find $$i" ; exit 1 ; \
           fi ; \
           chmod 644 $(DESTDIR)$(inst_libdir)/$(M2LIBDIR)$$i ; \
	done

force:

endif

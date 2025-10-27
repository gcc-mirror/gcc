# Makefile for libm2iso.
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

VPATH = . @srcdir@ @srcdir@/../../gcc/m2/gm2-libs-iso

# Multilib support.
MAKEOVERRIDES=

gcc_version := $(shell @get_gcc_base_ver@ $(top_srcdir)/../gcc/BASE-VER)

# Directory in which the compiler finds libraries etc.
libsubdir = $(libdir)/gcc/$(target_alias)/$(gcc_version)
# Used to install the shared libgcc.
# was slibdir = @slibdir@
slibdir = $(libdir)/gcc/$(target_alias)/$(gcc_version)$(MULTISUBDIR)

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


if BUILD_ISOLIB
M2DEFS = ChanConsts.def  CharClass.def \
         ClientSocket.def  ComplexMath.def \
         ConvStringLong.def  ConvStringReal.def \
         ConvStringShort.def \
         ConvTypes.def  COROUTINES.def \
         ErrnoCategory.def  EXCEPTIONS.def \
         GeneralUserExceptions.def  IOChan.def \
         IOChanUtils.def \
         IOConsts.def  IOLink.def \
         IOResult.def  LongComplexMath.def \
         LongConv.def  LongIO.def \
         LongMath.def  LongStr.def \
         LongWholeIO.def  LowLong.def \
         LowReal.def  LowShort.def \
         M2EXCEPTION.def  M2RTS.def \
         MemStream.def \
         Preemptive.def \
         Processes.def  ProgramArgs.def \
         RandomNumber.def \
         RawIO.def  RealConv.def \
         RealIO.def  RealMath.def \
         RealStr.def  RndFile.def \
         RTco.def \
         RTdata.def  RTentity.def \
         RTfio.def  RTgen.def \
         RTgenif.def  RTio.def \
         Semaphores.def  SeqFile.def \
         ShortComplexMath.def \
         ShortConv.def \
         ShortIO.def \
         ShortMath.def ShortStr.def \
         ShortWholeIO.def \
         SimpleCipher.def  SIOResult.def \
         SLongIO.def  SLongWholeIO.def \
         SRawIO.def  SRealIO.def \
         SShortIO.def  SShortWholeIO.def \
         StdChans.def  STextIO.def \
         Storage.def  StreamFile.def \
         StringChan.def  Strings.def \
         SWholeIO.def  SysClock.def \
         SYSTEM.def  TermFile.def \
         TERMINATION.def  TextIO.def \
         TextUtil.def \
         WholeConv.def  WholeIO.def \
         WholeStr.def  wrapclock.def \
         wrapsock.def wraptime.def

M2MODS = ChanConsts.mod  CharClass.mod \
         ClientSocket.mod  ComplexMath.mod \
         ConvStringLong.mod  ConvStringReal.mod \
         ConvStringShort.mod \
         ConvTypes.mod  COROUTINES.mod \
         EXCEPTIONS.mod  GeneralUserExceptions.mod \
         IOChan.mod  IOChanUtils.mod \
         IOConsts.mod \
         IOLink.mod  IOResult.mod \
         LongComplexMath.mod  LongConv.mod \
         LongIO.mod  LongMath.mod \
         LongStr.mod  LongWholeIO.mod \
         LowLong.mod  LowReal.mod \
         LowShort.mod  M2EXCEPTION.mod \
         M2RTS.mod  MemStream.mod \
         Preemptive.mod \
         Processes.mod \
         ProgramArgs.mod  RandomNumber.mod \
         RawIO.mod  RealConv.mod \
         RealIO.mod  RealMath.mod \
         RealStr.mod  RndFile.mod \
         RTdata.mod  RTentity.mod \
         RTfio.mod  RTgenif.mod \
         RTgen.mod  RTio.mod \
         Semaphores.mod  SeqFile.mod \
         ShortComplexMath.mod \
         ShortConv.mod \
         ShortIO.mod \
         ShortMath.mod ShortStr.mod \
         ShortWholeIO.mod \
         SimpleCipher.mod  SIOResult.mod \
         SLongIO.mod  SLongWholeIO.mod \
         SRawIO.mod  SRealIO.mod \
         SShortIO.mod  SShortWholeIO.mod \
         StdChans.mod  STextIO.mod \
         Storage.mod  StreamFile.mod \
         StringChan.mod  Strings.mod \
         SWholeIO.mod  SysClock.mod \
         SYSTEM.mod  TermFile.mod \
         TERMINATION.mod  TextIO.mod \
         TextUtil.mod \
         WholeConv.mod  WholeIO.mod \
         WholeStr.mod

toolexeclib_LTLIBRARIES = libm2iso.la
libm2iso_la_SOURCES =  $(M2MODS) \
                     ErrnoCategory.cc RTco.cc wrapclock.cc wraptime.cc wrapsock.c
#  wrapsock.cc

C_INCLUDES = -I.. -I$(toplevel_srcdir)/libiberty -I$(toplevel_srcdir)/include

libm2isodir = libm2iso
libm2iso_la_DEPENDENCIES = SYSTEM.def $(addsuffix .lo, $(basename $(libm2iso_la_SOURCES)))
libm2iso_la_CFLAGS = \
   $(C_INCLUDES) -I. -I.. -I$(GM2_SRC)/gm2-libs-iso -I$(GM2_SRC)/gm2-libs \
   -DBUILD_GM2_LIBS -I@srcdir@/../ -I../../../gcc -I$(GCC_DIR) -I$(GCC_DIR)/../include \
   -I../../libgcc -I$(GCC_DIR)/../libgcc -I$(MULTIBUILDTOP)../../gcc/include

libm2iso_la_M2FLAGS = \
  -fm2-pathname=m2iso -I. -Ilibm2iso -I$(GM2_SRC)/gm2-libs-iso \
  -fm2-pathname=m2pim -I$(GM2_SRC)/gm2-libs \
  -fiso -fextended-opaque -fm2-g -g -Wcase-enum \
  -Wreturn-type -fcase -fm2-prefix=m2iso

if TARGET_DARWIN
libm2iso_la_link_flags = -Wl,-undefined,dynamic_lookup
else
libm2iso_la_link_flags =
endif
if ENABLE_DARWIN_AT_RPATH
libm2iso_la_link_flags += -nodefaultrpaths -Wl,-rpath,@loader_path/
endif

libm2iso_la_LINK = $(LINK) -version-info $(libtool_VERSION) $(libm2iso_la_link_flags) -lm
CLEANFILES = SYSTEM.def
BUILT_SOURCES = SYSTEM.def

M2LIBDIR = /m2/m2iso/

M2HEADER_FILES = m2rts.h

SYSTEM.def: Makefile
	bash $(GM2_SRC)/tools-src/makeSystem -fiso \
             $(GM2_SRC)/gm2-libs-iso/SYSTEM.def \
             $(GM2_SRC)/gm2-libs-iso/SYSTEM.mod \
             -I$(GM2_SRC)/gm2-libs-iso -I$(GM2_SRC)/gm2-libs \
             "$(GM2_FOR_TARGET)" $@

.mod.lo:
	$(LIBTOOL) --tag=CC $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) --mode=compile \
                   $(GM2_FOR_TARGET) -c \
                   $(CFLAGS_FOR_TARGET) $(LIBCFLAGS) $(libm2iso_la_M2FLAGS) $< -o $@

.cc.lo:
	$(LIBTOOL) --tag=CXX --mode=compile $(CXX) -c -I$(srcdir) \
                   $(CXXFLAGS) $(LIBCFLAGS) $(libm2iso_la_CFLAGS) $< -o $@

.c.lo:
	$(LIBTOOL) --tag=CC --mode=compile $(CC) -c -I$(srcdir) \
                   $(CFLAGS) $(LIBCFLAGS) $(libm2iso_la_CFLAGS) $< -o $@

install-data-local: force
	mkdir -p $(DESTDIR)$(inst_libdir)/$(M2LIBDIR)
	for i in $(M2DEFS) $(M2MODS) ; do \
           if [ -f $$i ] ; then \
              $(INSTALL_DATA) $$i '$(DESTDIR)$(inst_libdir)/$(M2LIBDIR)'; \
           elif [ -f @srcdir@/../../gcc/m2/gm2-libs-iso/$$i ] ; then \
              $(INSTALL_DATA) @srcdir@/../../gcc/m2/gm2-libs-iso/$$i '$(DESTDIR)$(inst_libdir)/$(M2LIBDIR)'; \
           else \
              echo "cannot find $$i" ; exit 1 ; \
           fi ; \
           chmod 644 $(DESTDIR)$(inst_libdir)/$(M2LIBDIR)$$i ; \
	done
	for i in $(M2HEADER_FILES) ; do \
           if [ -f @srcdir@/$$i ] ; then \
              $(INSTALL_DATA) @srcdir@/$$i '$(DESTDIR)$(inst_libdir)/$(M2LIBDIR)'; \
           else \
              echo "cannot find $$i" ; exit 1 ; \
           fi ; \
           chmod 644 $(DESTDIR)$(inst_libdir)/$(M2LIBDIR)$$i ; \
	done

force:

endif

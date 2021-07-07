# Makefile for gm2tools
#   Copyright (C) 2021 Free Software Foundation, Inc.
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

ACLOCAL_AMFLAGS = -I ../config

gcc_version := $(shell $(GCC_FOR_TARGET) -dumpversion)

libexecsubdir = $(libexecdir)/gcc/$(target_noncanonical)/$(gcc_version)

mkinstalldirs = $(SHELL) $(toplevel_srcdir)/mkinstalldirs
PWD_COMMAND = $${PWDCMD-pwd}
STAMP = echo timestamp >

LIBSTDCXXPATH = ../$(TARGET_SUBDIR)/libstdc++-v3/src/.libs
LIBSTDCXX = ../$(TARGET_SUBDIR)/libstdc++-v3/src/.libs/libstdc++.a
LIBM2PIMPATH = ../$(TARGET_SUBDIR)/libgm2/libm2pim/.libs
LIBM2CORPATH = ../$(TARGET_SUBDIR)/libgm2/libm2cor/.libs
LIBM2ISOPATH = ../$(TARGET_SUBDIR)/libgm2/libm2iso/.libs
LIBM2 = $(LIBM2PIMPATH)/libm2pim.a $(LIBM2ISOPATH)/libm2iso.a

libgm2dir = ../$(target_noncanonical)/libgm2
LIBGM2DEP = $(libgm2dir)/libgm2.la
m2srcdir = $(srcdir)/../gcc/m2
m2builddir = ../gcc/m2
PGE = $(m2builddir)/pge$(exe)
DEBUG_MODULE_LIST = # -fmodules

LIBS = ../libiberty/xmalloc.o ../libiberty/xexit.o \
       m2color.o m2linemap.o ../libiberty/choose-temp.o \
       ../libiberty/xstrdup.o ../libiberty/pexecute.o \
       ../libiberty/make-temp-file.o ../gcc/m2/gm2-ici/m2flex.o \
       errors.o

if NATIVE
# Use the compiler we just built.
GCCCOMPILER = $(CC_FOR_TARGET) $(XGCC_FLAGS_FOR_TARGET)
GXXCOMPILER = $(CXX_FOR_TARGET) $(XGCC_FLAGS_FOR_TARGET)
GM2COMPILER = $(GM2_FOR_TARGET) $(XGCC_FLAGS_FOR_TARGET) -B$(m2builddir)/../ -g -fm2-g
GM2LINK = $(GM2_FOR_TARGET) -B$(m2builddir)/../ -g -fm2-g
else
GCCCOMPILER = $(CC)
GM2COMPILER = $(GM2)
GM2LINK = $(GM2)
endif

GM2CFLAGS = $(CFLAGS_FOR_TARGET)
GM2COMPILE = $(GM2COMPILER) $(GM2CFLAGS)
GM2TOOLSINC = -I$(m2srcdir)/gm2-libs -I$(m2srcdir)/gm2-libiberty -I$(m2srcdir)/gm2-compiler -I$(m2srcdir)/gm2-gcc

AM_GM2CFLAGS = -I $(libgm2dir)
AM_LDFLAGS = -L $(libgm2dir) -L $(libgm2dir)/.libs

if NATIVE

GM2DEPS = ../gcc/cc1gm2$(EXEEXT) \
  ../gcc/m2/gm2-compiler-boot/gm2.a \
  ../gcc/m2/gm2-libs-boot/libgm2.a \
  ../gcc/m2/ppg$(exeext) \
  $(GM2_LIBS)

# For a native build we build the programs using the newly built libgm2
# and install them as regular programs.

# gm2m is built in gcc/m2 as it is internal to gm2.

bin_PROGRAMS = gm2l gm2lcc gm2lgen gm2lorder

MODULES = ASCII.o Assertion.o Break.o CmdArgs.o Debug.o DynamicStrings.o \
          FIO.o FormatStrings.o IO.o Indexing.o Lists.o M2ColorString.o \
          M2Configure.o M2Debug.o M2Defaults.o M2Depth.o M2DriverOptions.o \
          M2EXCEPTION.o M2Emit.o M2Error.o M2FileName.o M2LexBuf.o \
          M2Options.o M2Preprocess.o M2Printf.o M2RTS.o M2Reserved.o \
          M2Search.o M2Version.o NameKey.o NumberIO.o ObjectFiles.o \
          RTExceptions.o SArgs.o SEnvironment.o SFIO.o SYSTEM.o StdIO.o \
          Storage.o StrIO.o StrLib.o StringConvert.o SymbolKey.o SysExceptions.o \
          SysStorage.o UnixArgs.o choosetemp.o dtoa.o errno.o ldtoa.o m2color.o \
          m2linemap.o termios.o wrapc.o errors.o

# noinst_PROGRAMS = ppg$(EXEEXT)
man_MANS = gm2lgen.1 gm2l.1 gm2lcc.1 gm2lorder.1

gm2lcc: $(MODULES) gm2lcc.o
	grep -v mod_init $(m2srcdir)/init/gm2lccinit > gm2lcc.lst
	$(GM2COMPILER) -fuselist $(DEBUG_MODULE_LIST) -c -I$(m2builddir)/gm2-libs -I$(m2builddir)/gm2-compiler \
               -I$(m2srcdir)/gm2-libs -I$(m2srcdir)/gm2-compiler \
               -g -B./ $(m2srcdir)/gm2-compiler/gm2lcc.mod
	$(GM2LINK) -flibs=pim,iso -I$(m2builddir)/gm2-libs -I$(m2builddir)/gm2-compiler \
               -I$(m2srcdir)/gm2-libs -I$(m2srcdir)/gm2-compiler -g -B../gcc/stage1/gm2 -B../gcc -o $@ \
               -fuselist $(m2srcdir)/gm2-compiler/gm2lcc.mod -L$(LIBSTDCXXPATH) -L$(LIBM2PIMPATH) -L$(LIBM2ISOPATH) $(LIBS)

gm2l: $(MODULES) gm2l.o
	grep -v mod_init $(m2srcdir)/init/gm2linit > gm2l.lst
	$(GM2COMPILER) -fuselist $(DEBUG_MODULE_LIST) -c \
               -I$(m2builddir)/gm2-libs -I$(m2builddir)/gm2-compiler \
               -I$(m2builddir)/gm2-ici $(GM2TOOLSINC) -I../libiberty \
               -g -B./ gm2l.mod
	$(GM2LINK) -flibs=pim,iso -fuselist -I$(m2builddir)/gm2-libs -I$(m2builddir)/gm2-compiler \
               -I$(m2builddir)/gm2-libs -I$(m2builddir)/gm2-compiler \
               -I$(m2builddir)/gm2-ici $(GM2TOOLSINC) -I../libiberty \
               -g -B../gcc/stage1/gm2 -B../gcc -o $@ \
               gm2l.mod -L$(LIBSTDCXXPATH) -L$(LIBM2PIMPATH) -L$(LIBM2ISOPATH) $(LIBS)

gm2lgen: $(MODULES) gm2lgen.o
	grep -v mod_init $(m2srcdir)/init/gm2lgeninit > gm2lgen.lst
	$(GM2COMPILER) -fuselist $(DEBUG_MODULE_LIST) -c -I$(m2builddir)/gm2-libs -I$(m2builddir)/gm2-compiler \
               -I$(m2srcdir)/gm2-libs -I$(m2srcdir)/gm2-compiler \
               -g -B./ $(m2srcdir)/gm2-compiler/gm2lgen.mod
	$(GM2LINK) -flibs=pim,iso -I$(m2builddir)/gm2-libs -I$(m2builddir)/gm2-compiler \
               -I$(m2srcdir)/gm2-libs -I$(m2srcdir)/gm2-compiler -g -B../gcc/stage1/gm2 -B../gcc -o $@ \
               -fuselist $(m2srcdir)/gm2-compiler/gm2lgen.mod -L$(LIBSTDCXXPATH) -L$(LIBM2PIMPATH) -L$(LIBM2ISOPATH) $(LIBS)

gm2lorder: $(MODULES) gm2lorder.o
	grep -v mod_init $(m2srcdir)/init/gm2lorderinit > gm2lorder.lst
	$(GM2COMPILER) -fuselist $(DEBUG_MODULE_LIST) -c -I$(m2builddir)/gm2-libs -I$(m2builddir)/gm2-compiler \
               -I$(m2srcdir)/gm2-libs -I$(m2srcdir)/gm2-compiler \
               -g -B./ $(m2srcdir)/gm2-compiler/gm2lorder.mod
	$(GM2LINK) -flibs=pim,iso -I$(m2builddir)/gm2-libs -I$(m2builddir)/gm2-compiler \
               -I$(m2srcdir)/gm2-libs -I$(m2srcdir)/gm2-compiler -g -B../gcc/stage1/gm2 -B../gcc -o $@ \
               -fuselist $(m2srcdir)/gm2-compiler/gm2lorder.mod -L$(LIBSTDCXXPATH) -L$(LIBM2PIMPATH) -L$(LIBM2ISOPATH) $(LIBS)

gm2m: $(GM2DEPS) $(GM2_LIBS) $(GM2_LINK_TOOLS_BOOT) $(LIBSTDCXX) $(MODULES) gm2m.o
	grep -v mod_init $(m2srcdir)/init/gm2minit > gm2m.lst
	$(GM2COMPILER) -fuselist $(DEBUG_MODULE_LIST) -c \
               -I$(m2builddir)/gm2-libs -I$(m2builddir)/gm2-compiler \
               -I$(m2builddir)/gm2-ici $(GM2TOOLSINC) -I../libiberty \
               -g -B./ gm2m.mod
	$(GM2LINK) -flisb=pim,iso -fuselist -I$(m2builddir)/gm2-libs -I$(m2builddir)/gm2-compiler \
               -I$(m2builddir)/gm2-libs -I$(m2builddir)/gm2-compiler \
               -I$(m2builddir)/gm2-ici $(GM2TOOLSINC) -I../libiberty \
               -g -B../gcc/stage1/gm2 -B../gcc -o $@ \
               gm2m.mod -L$(LIBSTDCXXPATH) $(LIBS) -L$(LIBM2PIMPATH) -L$(LIBM2ISOPATH) $(LIBS)

m2color.o: m2color.c
	$(GXXCOMPILER) -g -c -g -c -I../gcc -I../gcc/m2/gm2-libiberty \
                       -I$(srcdir)/../include -I$(srcdir)/../gcc -I$(m2builddir)/../ $<

errors.o: errors.c
	$(GXXCOMPILER) -g -c -g -c -I../gcc -I../gcc/m2/gm2-libiberty \
                       -I$(srcdir)/../include -I$(srcdir)/../gcc -I$(m2builddir)/../ $<

%.o: $(m2srcdir)/gm2-ici/%.mod
	$(GM2COMPILER) -g -c -I$(m2srcdir)/gm2-libs -I$(m2srcdir)/gm2-compiler -I$(m2srcdir)/gm2-gcc \
        -I$(m2srcdir)/gm2-libiberty -I$(m2srcdir)/gm2-ici $<

%.o: $(m2srcdir)/gm2-libs/%.mod
	$(GM2COMPILER) -g -c -I$(m2srcdir)/gm2-libs $<

%.o: $(m2srcdir)/gm2-compiler/%.mod
	$(GM2COMPILER) -g -c -I$(m2srcdir)/gm2-libs -I$(m2srcdir)/gm2-compiler -I$(m2srcdir)/gm2-gcc \
        -I$(m2srcdir)/gm2-libiberty -I$(m2srcdir)/gm2-gcc $<

%.o: $(m2srcdir)/gm2-libs-ch/%.c
	$(GCCCOMPILER) -g -c -I../gcc -I../gcc/m2/gm2-libiberty \
                       -I../gcc/m2/gm2-libs-boot \
                       -I$(srcdir)/../include -I$(m2builddir)/../ -I$(m2builddir)/gm2-libs -I$(m2builddir)/gm2-gcc -I$(srcdir)/../gcc $<

%.o: $(m2builddir)/gm2-compiler-boot/%.mod
	$(GM2COMPILER) -g -c -I$(m2srcdir)/gm2-libs -I$(m2srcdir)/gm2-compiler -I$(m2srcdir)/gm2-gcc \
        -I$(srcdir)/gm2-gcc $<

m2linemap.o: $(m2srcdir)/gm2-ici/m2linemap.c
	$(GCCCOMPILER) -g -c -I$(srcdir)/../include -I$(m2builddir)/../ -I$(m2builddir)/gm2-libs -I$(m2builddir)/gm2-gcc -I$(srcdir)/../gcc $<

%.o: $(m2srcdir)/gm2-gcc/%.c
	$(GXXCOMPILER) -g -c -I$(srcdir)/../include -I$(m2builddir)/../ -I$(m2builddir)/gm2-libs -I$(m2builddir)/gm2-gcc -I$(srcdir)/../gcc -I$(srcdir)/../libcpp/include $<

gm2l.o: gm2l.mod
	$(GM2COMPILER) -g -c $(GM2TOOLSINC) $<

gm2l.mod: $(m2srcdir)/bnf/gm2l.bnf
	$(PGE) -l $< > $@

gm2m.o: gm2m.mod
	$(GM2COMPILER) -g -c -I$(m2srcdir)/gm2-libs -I$(m2srcdir)/gm2-compiler -I$(m2srcdir)/gm2-gcc $<

gm2m.mod: $(m2srcdir)/bnf/gm2m.bnf
	$(PGE) -l $< > $@

install-exec-local: $(noinst_PROGRAMS)
	$(MKDIR_P) $(DESTDIR)$(libexecsubdir)
	for f in $(noinst_PROGRAMS); do \
	  rm -f $(DESTDIR)$(libexecsubdir)/$$f; \
	  $(INSTALL_PROGRAM) $$f $(DESTDIR)$(libexecsubdir)/$$f; \
	done

uninstall-local:
	for f in $(noinst_PROGRAMS); do \
	  rm -f $(DESTDIR)$(libexecsubdir)/$$f; \
	done

else

# For a non-native build we have to build the programs using a
# previously built host (or build -> host) gm2 compiler.  We should
# only do this if such a compiler is available.

endif

# Makefile for gotools
#   Copyright (C) 2015-2016 Free Software Foundation, Inc.
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

ACLOCAL_AMFLAGS = -I ./config -I ../config

gcc_version := $(shell $(GCC_FOR_TARGET) -dumpversion)

libexecsubdir = $(libexecdir)/gcc/$(target_noncanonical)/$(gcc_version)

mkinstalldirs = $(SHELL) $(toplevel_srcdir)/mkinstalldirs
PWD_COMMAND = $${PWDCMD-pwd}
STAMP = echo timestamp >

libgodir = ../$(target_noncanonical)/libgo
LIBGODEP = $(libgodir)/libgo.la

if NATIVE
# Use the compiler we just built.
GOCOMPILER = $(GOC_FOR_TARGET) $(XGCC_FLAGS_FOR_TARGET)
else
GOCOMPILER = $(GOC)
endif

GOCFLAGS = $(CFLAGS_FOR_TARGET)
GOCOMPILE = $(GOCOMPILER) $(GOCFLAGS)

AM_LDFLAGS = -L $(libgodir) -L $(libgodir)/.libs
GOLINK = $(GOCOMPILER) $(GOCFLAGS) $(AM_GOCFLAGS) $(LDFLAGS) $(AM_LDFLAGS) -o $@

cmdsrcdir = $(srcdir)/../libgo/go/cmd

go_cmd_go_files = \
	$(cmdsrcdir)/go/alldocs.go \
	$(cmdsrcdir)/go/build.go \
	$(cmdsrcdir)/go/clean.go \
	$(cmdsrcdir)/go/context.go \
	$(cmdsrcdir)/go/discovery.go \
	$(cmdsrcdir)/go/doc.go \
	$(cmdsrcdir)/go/env.go \
	$(cmdsrcdir)/go/fix.go \
	$(cmdsrcdir)/go/fmt.go \
	$(cmdsrcdir)/go/generate.go \
	$(cmdsrcdir)/go/get.go \
	$(cmdsrcdir)/go/go11.go \
	$(cmdsrcdir)/go/help.go \
	$(cmdsrcdir)/go/http.go \
	$(cmdsrcdir)/go/list.go \
	$(cmdsrcdir)/go/main.go \
	$(cmdsrcdir)/go/note.go \
	$(cmdsrcdir)/go/pkg.go \
	$(cmdsrcdir)/go/run.go \
	$(cmdsrcdir)/go/signal.go \
	$(cmdsrcdir)/go/signal_unix.go \
	$(cmdsrcdir)/go/test.go \
	$(cmdsrcdir)/go/testflag.go \
	$(cmdsrcdir)/go/tool.go \
	$(cmdsrcdir)/go/vcs.go \
	$(cmdsrcdir)/go/version.go \
	$(cmdsrcdir)/go/vet.go \
	$(libgodir)/zstdpkglist.go

go_cmd_gofmt_files = \
	$(cmdsrcdir)/gofmt/doc.go \
	$(cmdsrcdir)/gofmt/gofmt.go \
	$(cmdsrcdir)/gofmt/internal.go \
	$(cmdsrcdir)/gofmt/rewrite.go \
	$(cmdsrcdir)/gofmt/simplify.go

go_cmd_cgo_files = \
	$(cmdsrcdir)/cgo/ast.go \
	$(cmdsrcdir)/cgo/doc.go \
	$(cmdsrcdir)/cgo/gcc.go \
	$(cmdsrcdir)/cgo/godefs.go \
	$(cmdsrcdir)/cgo/main.go \
	$(cmdsrcdir)/cgo/out.go \
	$(cmdsrcdir)/cgo/util.go

GCCGO_INSTALL_NAME := $(shell echo gccgo|sed '$(program_transform_name)')
GCC_INSTALL_NAME := $(shell echo gcc|sed '$(program_transform_name)')
GXX_INSTALL_NAME := $(shell echo g++|sed '$(program_transform_name)')

zdefaultcc.go: s-zdefaultcc; @true
s-zdefaultcc: Makefile
	echo 'package main' > zdefaultcc.go.tmp
	echo 'const defaultGCCGO = "$(bindir)/$(GCCGO_INSTALL_NAME)"' >> zdefaultcc.go.tmp
	echo 'const defaultCC = "$(bindir)/$(GCC_INSTALL_NAME)"' >> zdefaultcc.go.tmp
	echo 'const defaultCXX = "$(bindir)/$(GXX_INSTALL_NAME)"' >> zdefaultcc.go.tmp
	$(SHELL) $(srcdir)/../move-if-change zdefaultcc.go.tmp zdefaultcc.go
	$(STAMP) $@ 

MOSTLYCLEANFILES = zdefaultcc.go s-zdefaultcc

if NATIVE

# For a native build we build the programs using the newly built libgo
# and install them as regular programs.

bin_PROGRAMS = go$(EXEEXT) gofmt$(EXEEXT)
noinst_PROGRAMS = cgo$(EXEEXT)
man_MANS = go.1 gofmt.1

go$(EXEEXT): $(go_cmd_go_files) zdefaultcc.go $(LIBGODEP)
	$(GOLINK) $(go_cmd_go_files) zdefaultcc.go $(LIBS) $(NET_LIBS)
gofmt$(EXEEXT): $(go_cmd_gofmt_files) $(LIBGODEP)
	$(GOLINK) $(go_cmd_gofmt_files) $(LIBS) $(NET_LIBS)
cgo$(EXEEXT): $(go_cmd_cgo_files) zdefaultcc.go $(LIBGODEP)
	$(GOLINK) $(go_cmd_cgo_files) zdefaultcc.go $(LIBS) $(NET_LIBS)

install-exec-local: cgo$(EXEEXT)
	$(MKDIR_P) $(DESTDIR)$(libexecsubdir)
	rm -f $(DESTDIR)$(libexecsubdir)/cgo$(exeext)
	$(INSTALL_PROGRAM) cgo$(exeext) $(DESTDIR)$(libexecsubdir)/cgo$(exeext)

uninstall-local:
	rm -f $(DESTDIR)$(libexecsubdir)/cgo$(exeext)

else

# For a non-native build we have to build the programs using a
# previously built host (or build -> host) Go compiler.  We should
# only do this if such a compiler is available.  We also need to get
# the right values for GOARCH and GOOS in the default build context in
# the go/build package.  Figure this out later.

endif

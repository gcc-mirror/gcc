# Makefile for libgm2.
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
#
#
# if this file is changed then you need to run
#
# autoreconf2.64
#
# Modula-2 support.
AUTOMAKE_OPTIONS = 1.8 foreign

SUFFIXES = .c .mod .def .o .obj .lo .a

ACLOCAL_AMFLAGS = -I .. -I ../config

# Multilib support.
MAKEOVERRIDES=

AM_CFLAGS = -I $(srcdir)/../libgcc -I $(MULTIBUILDTOP)../../gcc/include

gcc_version := $(shell @get_gcc_base_ver@ $(top_srcdir)/../gcc/BASE-VER)
TOP_GCCDIR := $(shell cd $(top_srcdir) && cd .. && pwd)

GCC_DIR = $(TOP_GCCDIR)/gcc
GM2_SRC = $(GCC_DIR)/m2

toolexeclibdir=@toolexeclibdir@
toolexecdir=@toolexecdir@
GM2_FOR_TARGET=@GM2_FOR_TARGET@

SUBDIRS = libm2min libm2log libm2cor libm2iso libm2pim
GM2_BUILDDIR := $(shell pwd)
gm2_cdir = $(libdir)/gcc/$(target_alias)/$(gcc_version)/include

if ENABLE_DARWIN_AT_RPATH
DARWIN_AT_RPATH=yes
else
DARWIN_AT_RPATH=yes
endif

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
        "CFLAGS_LONGDOUBLE=$(CFLAGS_LONGDOUBLE)" \
	"EXPECT=$(EXPECT)" \
	"INSTALL=$(INSTALL)" \
	"INSTALL_DATA=$(INSTALL_DATA)" \
	"INSTALL_PROGRAM=$(INSTALL_PROGRAM)" \
	"INSTALL_SCRIPT=$(INSTALL_SCRIPT)" \
	"LDFLAGS=$(LDFLAGS)" \
	"LIBCFLAGS=$(LIBCFLAGS)" \
	"LIBCFLAGS_FOR_TARGET=$(LIBCFLAGS_FOR_TARGET)" \
	"MAKE=$(MAKE)" \
	"MAKEINFO=$(MAKEINFO) $(MAKEINFOFLAGS)" \
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
        "TARGET_LIB_PATH=$(TARGET_LIB_PATH)" \
        "TARGET_LIB_PATH_libgm2=$(TARGET_LIB_PATH_libgm2)" \
	"LIBTOOL=$(GM2_BUILDDIR)/libtool" \
	"DARWIN_AT_RPATH=$(DARWIN_AT_RPATH)"

# Subdir rules rely on $(FLAGS_TO_PASS)
FLAGS_TO_PASS = $(AM_MAKEFLAGS)

include $(top_srcdir)/../multilib.am

AM_CPPFLAGS = -I $(top_srcdir)/include 
 
# May be used by toolexeclibdir.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

DEFS = -D_GNU_SOURCE -D_DEBUG -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS 
AM_CXXFLAGS = -Wall -W -Wno-unused-parameter -Wwrite-strings -pedantic -Wno-long-long  -fPIC -fno-builtin -fno-exceptions -fomit-frame-pointer -funwind-tables -fvisibility=hidden -Wno-variadic-macros -Wno-c99-extensions 
AM_CXXFLAGS += $(LIBSTDCXX_RAW_CXX_CXXFLAGS)
ACLOCAL_AMFLAGS = -I m4

noinst_LTLIBRARIES = libinterception.la

interception_files = \
        interception_linux.cc \
        interception_mac.cc \
        interception_win.cc

libinterception_la_SOURCES = $(interception_files)

MAKEOVERRIDES=

## ################################################################


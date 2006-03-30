## Makefile for the i386 directory of the libgcc-math library.
##
## Copyright (C) 2006
## Free Software Foundation, Inc.
##

# May be used by various substitution variables.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)


noinst_LTLIBRARIES = libsse2.la

libsse2_la_CFLAGS = -I@srcdir@/../include -I.. -include @srcdir@/sse2.h \
  -Wall -std=c99 -O2 -g \
  -msse2 -msseregparm -mfpmath=sse -march=pentium3 -mtune=generic \
  -fno-math-errno -fno-trapping-math -fno-rounding-math -fno-signaling-nans

libsse2_la_SOURCES = \
  @srcdir@/../flt-32/e_acosf.c \
  @srcdir@/../flt-32/e_asinf.c \
  @srcdir@/../flt-32/e_atan2f.c \
  @srcdir@/../flt-32/s_atanf.c \
  @srcdir@/../flt-32/s_cosf.c \
  @srcdir@/../flt-32/e_expf.c \
  @srcdir@/../flt-32/e_log10f.c \
  @srcdir@/../flt-32/e_logf.c \
  @srcdir@/../flt-32/e_powf.c \
  @srcdir@/../flt-32/s_sinf.c \
  @srcdir@/../flt-32/s_tanf.c \
  @srcdir@/../flt-32/k_cosf.c \
  @srcdir@/../flt-32/k_rem_pio2f.c \
  @srcdir@/../flt-32/k_sinf.c \
  @srcdir@/../flt-32/k_tanf.c \
  @srcdir@/../flt-32/e_rem_pio2f.c \
  @srcdir@/../flt-32/e_sqrtf.c \
  @srcdir@/../flt-32/s_scalbnf.c \
  @srcdir@/../flt-32/s_floorf.c \
  @srcdir@/../flt-32/s_isinff.c


# XXX hack alert
# From libffi/Makefile.am

# Work around what appears to be a GNU make bug handling MAKEFLAGS
# values defined in terms of make variables, as is the case for CC and
# friends when we are called from the top level Makefile.
AM_MAKEFLAGS = \
	"AR_FLAGS=$(AR_FLAGS)" \
	"CC_FOR_BUILD=$(CC_FOR_BUILD)" \
	"CFLAGS=$(CFLAGS)" \
	"CXXFLAGS=$(CXXFLAGS)" \
	"CFLAGS_FOR_BUILD=$(CFLAGS_FOR_BUILD)" \
	"CFLAGS_FOR_TARGET=$(CFLAGS_FOR_TARGET)" \
	"INSTALL=$(INSTALL)" \
	"INSTALL_DATA=$(INSTALL_DATA)" \
	"INSTALL_PROGRAM=$(INSTALL_PROGRAM)" \
	"INSTALL_SCRIPT=$(INSTALL_SCRIPT)" \
	"JC1FLAGS=$(JC1FLAGS)" \
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
	"prefix=$(prefix)" \
	"includedir=$(includedir)" \
	"AR=$(AR)" \
	"AS=$(AS)" \
	"CC=$(CC)" \
	"CXX=$(CXX)" \
	"LD=$(LD)" \
	"LIBCFLAGS=$(LIBCFLAGS)" \
	"NM=$(NM)" \
	"PICFLAG=$(PICFLAG)" \
	"RANLIB=$(RANLIB)" \
	"DESTDIR=$(DESTDIR)"

MAKEOVERRIDES=

## ################################################################


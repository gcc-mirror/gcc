## Makefile for the GNU C++ Support library.
##
## Copyright (C) 2000-2018 Free Software Foundation, Inc.
##
## Process this file with automake to produce Makefile.in.
##
## This file is part of GCC.
##
## GCC is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3, or (at your option)
## any later version.
##
## GCC is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with GCC; see the file COPYING3.  If not see
## <http://www.gnu.org/licenses/>.

include $(top_srcdir)/fragment.am

# Need this library to both be part of libstdc++.a, and installed
# separately too.
# 1) separate libsupc++.la
toolexeclib_LTLIBRARIES = libsupc++.la

# 2) integrated libsupc++convenience.la that is to be a part of libstdc++.a
noinst_LTLIBRARIES = libsupc++convenience.la

std_HEADERS = \
	cxxabi.h exception initializer_list new typeinfo

bits_HEADERS = \
	atomic_lockfree_defines.h cxxabi_forced.h \
	exception_defines.h exception_ptr.h hash_bytes.h nested_exception.h exception.h cxxabi_init_exception.h

headers = $(std_HEADERS) $(bits_HEADERS)

if GLIBCXX_HOSTED
  c_sources = \
	cp-demangle.c
endif

sources = \
	array_type_info.cc \
	atexit_arm.cc \
	atexit_thread.cc \
	bad_alloc.cc \
	bad_array_length.cc \
	bad_array_new.cc \
	bad_cast.cc \
	bad_typeid.cc \
	class_type_info.cc \
	del_op.cc \
	del_ops.cc \
	del_opnt.cc \
	del_opv.cc \
	del_opvs.cc \
	del_opvnt.cc \
	dyncast.cc \
	eh_alloc.cc \
	eh_arm.cc \
	eh_aux_runtime.cc \
	eh_call.cc \
	eh_catch.cc \
	eh_exception.cc \
	eh_globals.cc \
	eh_personality.cc \
	eh_ptr.cc \
	eh_term_handler.cc \
	eh_terminate.cc \
	eh_tm.cc \
	eh_throw.cc \
	eh_type.cc \
	eh_unex_handler.cc \
	enum_type_info.cc \
	function_type_info.cc \
	fundamental_type_info.cc \
	guard.cc \
	guard_error.cc \
	hash_bytes.cc \
	nested_exception.cc \
	new_handler.cc \
	new_op.cc \
	new_opnt.cc \
	new_opv.cc \
	new_opvnt.cc \
	new_opa.cc \
	new_opant.cc \
	new_opva.cc \
	new_opvant.cc \
	del_opa.cc \
	del_opant.cc \
	del_opsa.cc \
	del_opva.cc \
	del_opvant.cc \
	del_opvsa.cc \
	pbase_type_info.cc \
	pmem_type_info.cc \
	pointer_type_info.cc \
	pure.cc \
	si_class_type_info.cc \
	tinfo.cc \
	tinfo2.cc \
	vec.cc \
	vmi_class_type_info.cc \
	vterminate.cc

if ENABLE_VTABLE_VERIFY
if !VTV_CYGMIN
  vtv_sources = \
	vtv_stubs.cc
endif
endif

libsupc___la_SOURCES = $(sources) $(c_sources) $(vtv_sources)
libsupc__convenience_la_SOURCES = $(sources) $(c_sources) $(vtv_sources)

cp-demangle.c:
	rm -f $@
	$(LN_S) $(toplevel_srcdir)/libiberty/cp-demangle.c $@
cp-demangle.lo: cp-demangle.c
	$(LTCOMPILE) -DIN_GLIBCPP_V3 -Wno-error -c $<
cp-demangle.o: cp-demangle.c
	$(C_COMPILE) -DIN_GLIBCPP_V3 -Wno-error -c $<


# Use special rules for the C++17 sources so that the proper flags are passed.
new_opa.lo: new_opa.cc
	$(LTCXXCOMPILE) -std=gnu++1z -c $<
new_opant.lo: new_opant.cc
	$(LTCXXCOMPILE) -std=gnu++1z -c $<
new_opva.lo: new_opva.cc
	$(LTCXXCOMPILE) -std=gnu++1z -c $<
new_opvant.lo: new_opvant.cc
	$(LTCXXCOMPILE) -std=gnu++1z -c $<
del_opa.lo: del_opa.cc
	$(LTCXXCOMPILE) -std=gnu++1z -c $<
del_opant.lo: del_opant.cc
	$(LTCXXCOMPILE) -std=gnu++1z -c $<
del_opsa.lo: del_opsa.cc
	$(LTCXXCOMPILE) -std=gnu++1z -c $<
del_opva.lo: del_opva.cc
	$(LTCXXCOMPILE) -std=gnu++1z -c $<
del_opvant.lo: del_opvant.cc
	$(LTCXXCOMPILE) -std=gnu++1z -c $<
del_opvsa.lo: del_opvsa.cc
	$(LTCXXCOMPILE) -std=gnu++1z -c $<

# AM_CXXFLAGS needs to be in each subdirectory so that it can be
# modified in a per-library or per-sub-library way.  Need to manually
# set this option because CONFIG_CXXFLAGS has to be after
# OPTIMIZE_CXXFLAGS on the compile line so that -O2 can be overridden
# as the occasion call for it.
AM_CXXFLAGS = \
	$(glibcxx_lt_pic_flag) $(glibcxx_compiler_shared_flag) \
	$(XTEMPLATE_FLAGS) \
	$(WARN_CXXFLAGS) $(OPTIMIZE_CXXFLAGS)  $(CONFIG_CXXFLAGS)

AM_MAKEFLAGS = \
	"gxx_include_dir=$(gxx_include_dir)"


# Use special rules for pulling things out of libiberty.  These
# objects should be compiled with the "C" compiler, not the C++
# compiler, and also should not use the C++ includes.
C_INCLUDES = -I.. -I$(toplevel_srcdir)/libiberty -I$(toplevel_srcdir)/include
C_COMPILE = \
	$(CC) $(DEFS) $(C_INCLUDES) \
	$(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS) $(EXTRA_CFLAGS)

# LTCOMPILE is copied from LTCXXCOMPILE below.
LTCOMPILE = $(LIBTOOL) --tag CC --tag disable-shared $(LIBTOOLFLAGS) --mode=compile \
	    $(CC) $(DEFS) $(C_INCLUDES) \
	    $(glibcxx_lt_pic_flag) $(glibcxx_compiler_shared_flag) \
	    $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS) $(EXTRA_CFLAGS)

# Libtool notes

# 1) In general, libtool expects an argument such as `--tag=CXX' when
# using the C++ compiler, because that will enable the settings
# detected when C++ support was being configured.  However, when no
# such flag is given in the command line, libtool attempts to figure
# it out by matching the compiler name in each configuration section
# against a prefix of the command line.  The problem is that, if the
# compiler name and its initial flags stored in the libtool
# configuration file don't match those in the command line, libtool
# can't decide which configuration to use, and it gives up.  The
# correct solution is to add `--tag CXX' to LTCXXCOMPILE and maybe
# CXXLINK, just after $(LIBTOOL), so that libtool doesn't have to
# attempt to infer which configuration to use.
# 
# The second tag argument, `--tag disable-shared` means that libtool
# only compiles each source once, for static objects. In actuality,
# glibcxx_lt_pic_flag and glibcxx_compiler_shared_flag are added to
# the libtool command that is used create the object, which is
# suitable for shared libraries.  The `--tag disable-shared` must be
# placed after --tag CXX lest things CXX undo the affect of
# disable-shared.

# 2) Need to explicitly set LTCXXCOMPILE so that EXTRA_CXX_FLAGS is
# last. (That way, things like -O2 passed down from the toplevel can
# be overridden by --enable-debug.)
LTCXXCOMPILE = \
	$(LIBTOOL) --tag CXX --tag disable-shared \
	$(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) \
	--mode=compile $(CXX) $(TOPLEVEL_INCLUDES) \
	$(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) $(EXTRA_CXX_FLAGS)

LTLDFLAGS = $(shell $(SHELL) $(top_srcdir)/../libtool-ldflags $(LDFLAGS))

# 3) We'd have a problem when building the shared libstdc++ object if
# the rules automake generates would be used.  We cannot allow g++ to
# be used since this would add -lstdc++ to the link line which of
# course is problematic at this point.  So, we get the top-level
# directory to configure libstdc++-v3 to use gcc as the C++
# compilation driver.
CXXLINK = \
	$(LIBTOOL) --tag CXX --tag disable-shared \
	$(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) \
	--mode=link $(CXX) \
	$(OPT_LDFLAGS) $(SECTION_LDFLAGS) $(AM_CXXFLAGS) $(LTLDFLAGS) -o $@

# Install notes
# We have to have rules modified from the default to counteract SUN make
# prepending each of $(*_HEADERS) with VPATH below.
stddir = $(gxx_include_dir)
bitsdir = $(gxx_include_dir)/bits

install-stdHEADERS: $(std_HEADERS)
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(stddir)
	@list='$(std_HEADERS)'; for p in $$list; do \
	  q=`echo $$p | sed -e 's,.*/,,'`; \
	  if test -f "$$p"; then d= ; else d="$(srcdir)/"; fi; \
	  echo " $(INSTALL_DATA) $$d$$p $(DESTDIR)$(stddir)/$$q"; \
	  $(INSTALL_DATA) $$d$$p $(DESTDIR)$(stddir)/$$q; \
	done

install-bitsHEADERS: $(bits_HEADERS)
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(bitsdir)
	@list='$(bits_HEADERS)'; for p in $$list; do \
	  q=`echo $$p | sed -e 's,.*/,,'`; \
	  if test -f "$$p"; then d= ; else d="$(srcdir)/"; fi; \
	  echo " $(INSTALL_DATA) $$d$$p $(DESTDIR)$(bitsdir)/$$q"; \
	  $(INSTALL_DATA) $$d$$p $(DESTDIR)$(bitsdir)/$$q; \
	done

uninstall-stdHEADERS:
	@$(NORMAL_UNINSTALL)
	list='$(std_HEADERS)'; for p in $$list; do \
	  q=`echo $$p | sed -e 's,.*/,,'`; \
	  rm -f $(DESTDIR)$(stddir)/$$q; \
	done

uninstall-bitsHEADERS:
	@$(NORMAL_UNINSTALL)
	list='$(bits_HEADERS)'; for p in $$list; do \
	  q=`echo $$p | sed -e 's,.*/,,'`; \
	  rm -f $(DESTDIR)$(bitsdir)/$$q; \
	done


# By adding these files here, automake will remove them for 'make clean'
CLEANFILES = stamp-*


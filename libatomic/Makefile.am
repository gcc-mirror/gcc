##  Copyright (C) 2012-2019 Free Software Foundation, Inc.
##  Contributed by Richard Henderson <rth@redhat.com>.
##
##  This file is part of the GNU Atomic Library (libatomic).
##
##  Libatomic is free software; you can redistribute it and/or modify it
##  under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 3 of the License, or
##  (at your option) any later version.
##
##  Libatomic is distributed in the hope that it will be useful, but WITHOUT ANY
##  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
##  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
##  more details.
##
##  Under Section 7 of GPL version 3, you are granted additional
##  permissions described in the GCC Runtime Library Exception, version
##  3.1, as published by the Free Software Foundation.
##
##  You should have received a copy of the GNU General Public License and
##  a copy of the GCC Runtime Library Exception along with this program;
##  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
##  <http://www.gnu.org/licenses/>.

ACLOCAL_AMFLAGS = -I .. -I ../config
SUBDIRS = testsuite

## May be used by toolexeclibdir.
gcc_version := $(shell @get_gcc_base_ver@ $(top_srcdir)/../gcc/BASE-VER)

config_path= @config_path@
search_path = $(addprefix $(top_srcdir)/config/, $(config_path)) \
	$(top_srcdir) $(top_builddir)

vpath % $(strip $(search_path))

DEFAULT_INCLUDES = $(addprefix -I, $(search_path))
AM_CFLAGS = $(XCFLAGS)
AM_CCASFLAGS = $(XCFLAGS)
AM_LDFLAGS = $(XLDFLAGS) $(SECTION_LDFLAGS) $(OPT_LDFLAGS)

toolexeclib_LTLIBRARIES = libatomic.la
noinst_LTLIBRARIES = libatomic_convenience.la

if LIBAT_BUILD_VERSIONED_SHLIB
if LIBAT_BUILD_VERSIONED_SHLIB_GNU
libatomic_version_script = -Wl,--version-script,$(top_srcdir)/libatomic.map
libatomic_version_dep = $(top_srcdir)/libatomic.map
endif
if LIBAT_BUILD_VERSIONED_SHLIB_SUN
libatomic_version_script = -Wl,-M,libatomic.map-sun
libatomic_version_dep = libatomic.map-sun
libatomic.map-sun : $(top_srcdir)/libatomic.map \
		$(top_srcdir)/../contrib/make_sunver.pl \
		$(libatomic_la_OBJECTS) $(libatomic_la_LIBADD)
	perl $(top_srcdir)/../contrib/make_sunver.pl \
	  $(top_srcdir)/libatomic.map \
	  $(libatomic_la_OBJECTS:%.lo=.libs/%.o) \
	 `echo $(libatomic_la_LIBADD) | \
	    sed 's,\([^/ 	]*\)\.l\([ao]\),.libs/\1.\2,g'` \
	 > $@ || (rm -f $@ ; exit 1)
endif
else
libatomic_version_script =
libatomic_version_dep =
endif
libatomic_version_info = -version-info $(libtool_VERSION)

libatomic_la_LDFLAGS = $(libatomic_version_info) $(libatomic_version_script) $(lt_host_flags)
libatomic_la_SOURCES = gload.c gstore.c gcas.c gexch.c glfree.c lock.c init.c \
	fenv.c fence.c flag.c

SIZEOBJS = load store cas exch fadd fsub fand fior fxor fnand tas
SIZES = @SIZES@

EXTRA_libatomic_la_SOURCES = $(addsuffix _n.c,$(SIZEOBJS))
libatomic_la_DEPENDENCIES = $(libatomic_la_LIBADD) $(libatomic_version_dep)

## And now our custom target patterns that allow us not to have tons of
## extra source files hanging about.  Unfortunately, the complex relation
## between source and object filenames doesn't allow us to add an explicit
## dependency here.  Fortunately that doesn't matter since auto-generated
## dependencies do the job just as well:
-include $(wildcard $(DEPDIR)/*.Ppo)

## Naming pattern: base_n_i_.lo
##
##	N	size of data
##	I	IFUNC alternative, index beginning at 1.
##
## The trailing _ in the output object file name is required to differentiate
## these objects from those which should be compiled normally.  We can only
## have one stem in the implicit rule.

empty		=
space		= $(empty) $(empty)
PAT_SPLIT	= $(subst _,$(space),$(*F))
PAT_BASE	= $(word 1,$(PAT_SPLIT))
PAT_N		= $(word 2,$(PAT_SPLIT))
PAT_S		= $(word 3,$(PAT_SPLIT))
IFUNC_DEF	= -DIFUNC_ALT=$(PAT_S)
IFUNC_OPT	= $(word $(PAT_S),$(IFUNC_OPTIONS))

M_DEPS		= -MT $@ -MD -MP -MF $(DEPDIR)/$(@F).Ppo
M_SIZE		= -DN=$(PAT_N)
M_IFUNC		= $(if $(PAT_S),$(IFUNC_DEF) $(IFUNC_OPT))
M_FILE		= $(PAT_BASE)_n.c

# The lack of explicit dependency on the source file means that VPATH cannot
# work properly.  Instead, perform this operation by hand.  First, collect a
# list of all .c files in the search path.
all_c_files	:= $(foreach dir,$(search_path),$(wildcard $(dir)/*.c))

# Then sort through them to find the one we want, and select the first.
M_SRC		= $(firstword $(filter %/$(M_FILE), $(all_c_files)))

%_.lo: Makefile
	$(LTCOMPILE) $(M_DEPS) $(M_SIZE) $(M_IFUNC) -c -o $@ $(M_SRC)

## Include all of the sizes in the "normal" set of compilation flags.
libatomic_la_LIBADD = $(foreach s,$(SIZES),$(addsuffix _$(s)_.lo,$(SIZEOBJS)))

## On a target-specific basis, include alternates to be selected by IFUNC.
if HAVE_IFUNC
if ARCH_AARCH64_LINUX
IFUNC_OPTIONS	     = -march=armv8-a+lse
libatomic_la_LIBADD += $(foreach s,$(SIZES),$(addsuffix _$(s)_1_.lo,$(SIZEOBJS)))
endif
if ARCH_ARM_LINUX
IFUNC_OPTIONS	     = -march=armv7-a+fp -DHAVE_KERNEL64
libatomic_la_LIBADD += $(foreach s,$(SIZES),$(addsuffix _$(s)_1_.lo,$(SIZEOBJS)))
libatomic_la_LIBADD += $(addsuffix _8_2_.lo,$(SIZEOBJS))
endif
if ARCH_I386
IFUNC_OPTIONS	     = -march=i586
libatomic_la_LIBADD += $(addsuffix _8_1_.lo,$(SIZEOBJS))
endif
if ARCH_X86_64
IFUNC_OPTIONS	     = -mcx16
libatomic_la_LIBADD += $(addsuffix _16_1_.lo,$(SIZEOBJS))
endif
endif

libatomic_convenience_la_SOURCES = $(libatomic_la_SOURCES)
libatomic_convenience_la_LIBADD = $(libatomic_la_LIBADD)

# Override the automake generated all-multi rule to guarantee that all-multi
# is not run in parallel with the %_.lo rules which generate $(DEPDIR)/*.Ppo
# makefile fragments to avoid broken *.Ppo getting included into the Makefile
# when it is reloaded during the build of all-multi.
all-multi: $(libatomic_la_LIBADD)
	$(MULTIDO) $(AM_MAKEFLAGS) DO=all multi-do # $(MAKE)

include $(top_srcdir)/../multilib.am

## Process this file with automake to produce Makefile.in

ACLOCAL_AMFLAGS = -I .. -I ../config
SUBDIRS = testsuite

## May be used by toolexeclibdir.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

abi_version = -fabi-version=4

config_path = @config_path@
search_path = $(addprefix $(top_srcdir)/config/, $(config_path)) $(top_srcdir)

fincludedir = $(libdir)/gcc/$(target_alias)/$(gcc_version)/finclude
libsubincludedir = $(libdir)/gcc/$(target_alias)/$(gcc_version)/include

vpath % $(strip $(search_path))

AM_CPPFLAGS = $(addprefix -I, $(search_path))
AM_CFLAGS = $(XCFLAGS)
AM_CXXFLAGS = $(XCFLAGS) -std=gnu++0x -funwind-tables -fno-exceptions \
	-fno-rtti $(abi_version)
AM_CCASFLAGS = $(XCFLAGS)
AM_LDFLAGS = $(XLDFLAGS) $(SECTION_LDFLAGS) $(OPT_LDFLAGS) $(HWCAP_LDFLAGS)

toolexeclib_LTLIBRARIES = libitm.la
nodist_toolexeclib_HEADERS = libitm.spec

if LIBITM_BUILD_VERSIONED_SHLIB
if LIBITM_BUILD_VERSIONED_SHLIB_GNU
libitm_version_script = -Wl,--version-script,$(top_srcdir)/libitm.map
libitm_version_dep = $(top_srcdir)/libitm.map
endif
if LIBITM_BUILD_VERSIONED_SHLIB_SUN
libitm_version_script = -Wl,-M,libitm.map-sun
libitm_version_dep = libitm.map-sun
libitm.map-sun : $(top_srcdir)/libitm.map \
		$(top_srcdir)/../contrib/make_sunver.pl \
		$(libitm_la_OBJECTS) $(libitm_la_LIBADD)
	perl $(top_srcdir)/../contrib/make_sunver.pl \
	  $(top_srcdir)/libitm.map \
	  $(libitm_la_OBJECTS:%.lo=.libs/%.o) \
	 `echo $(libitm_la_LIBADD) | \
	    sed 's,/\([^/.]*\)\.la,/.libs/\1.a,g'` \
	 > $@ || (rm -f $@ ; exit 1)
endif
else
libitm_version_script =
libitm_version_dep =
endif
libitm_version_info = -version-info $(libtool_VERSION)

# Force link with C, not C++.  For now, while we're using C++ we don't
# want or need libstdc++.
libitm_la_DEPENDENCIES = $(libitm_version_dep)
libitm_la_LINK = $(LINK) $(libitm_la_LDFLAGS)
libitm_la_LDFLAGS = $(libitm_version_info) $(libitm_version_script)

libitm_la_SOURCES = \
	aatree.cc alloc.cc alloc_c.cc alloc_cpp.cc barrier.cc beginend.cc \
	clone.cc eh_cpp.cc local.cc \
	query.cc retry.cc rwlock.cc useraction.cc util.cc \
	sjlj.S tls.cc method-serial.cc method-gl.cc method-ml.cc

if ARCH_ARM
libitm_la_SOURCES += hwcap.cc
endif
if ARCH_X86
libitm_la_SOURCES += x86_sse.cc x86_avx.cc
x86_sse.lo : XCFLAGS += -msse
endif
if ARCH_X86_AVX
x86_avx.lo : XCFLAGS += -mavx
endif

if ARCH_FUTEX
libitm_la_SOURCES += futex.cc
endif

# Automake Documentation:
# If your package has Texinfo files in many directories, you can use the
# variable TEXINFO_TEX to tell Automake where to find the canonical
# `texinfo.tex' for your package. The value of this variable should be
# the relative path from the current `Makefile.am' to `texinfo.tex'.
TEXINFO_TEX   = ../gcc/doc/include/texinfo.tex

# Defines info, dvi, pdf and html targets
MAKEINFOFLAGS = -I $(srcdir)/../gcc/doc/include
info_TEXINFOS = libitm.texi

# AM_CONDITIONAL on configure option --generated-files-in-srcdir
if GENINSRC
STAMP_GENINSRC = stamp-geninsrc
else
STAMP_GENINSRC =
endif

# AM_CONDITIONAL on configure check ACX_CHECK_PROG_VER([MAKEINFO])
if BUILD_INFO
STAMP_BUILD_INFO = stamp-build-info
else
STAMP_BUILD_INFO =
endif


all-local: $(STAMP_GENINSRC)

stamp-geninsrc: libitm.info
	cp -p $(top_builddir)/libitm.info $(srcdir)/libitm.info
	@touch $@

libitm.info: $(STAMP_BUILD_INFO)

stamp-build-info: libitm.texi
	$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) -I $(srcdir) -o libitm.info $(srcdir)/libitm.texi
	@touch $@


CLEANFILES = $(STAMP_GENINSRC) $(STAMP_BUILD_INFO) libitm.info
MAINTAINERCLEANFILES = $(srcdir)/libitm.info

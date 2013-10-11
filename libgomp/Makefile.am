## Process this file with automake to produce Makefile.in

ACLOCAL_AMFLAGS = -I .. -I ../config
SUBDIRS = testsuite

## May be used by toolexeclibdir.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

config_path = @config_path@
search_path = $(addprefix $(top_srcdir)/config/, $(config_path)) $(top_srcdir)

fincludedir = $(libdir)/gcc/$(target_alias)/$(gcc_version)/finclude
libsubincludedir = $(libdir)/gcc/$(target_alias)/$(gcc_version)/include

vpath % $(strip $(search_path))

AM_CPPFLAGS = $(addprefix -I, $(search_path))
AM_CFLAGS = $(XCFLAGS)
AM_LDFLAGS = $(XLDFLAGS) $(SECTION_LDFLAGS) $(OPT_LDFLAGS)

toolexeclib_LTLIBRARIES = libgomp.la
nodist_toolexeclib_HEADERS = libgomp.spec

if LIBGOMP_BUILD_VERSIONED_SHLIB
# -Wc is only a libtool option.
comma = ,
PREPROCESS = $(subst -Wc$(comma), , $(COMPILE)) -E

libgomp.ver: $(top_srcdir)/libgomp.map
	$(EGREP) -v '#(#| |$$)' $< | \
	  $(PREPROCESS) -P -include config.h - > $@ || (rm -f $@ ; exit 1)

if LIBGOMP_BUILD_VERSIONED_SHLIB_GNU
libgomp_version_script = -Wl,--version-script,libgomp.ver
libgomp_version_dep = libgomp.ver
endif
if LIBGOMP_BUILD_VERSIONED_SHLIB_SUN
libgomp_version_script = -Wl,-M,libgomp.ver-sun
libgomp_version_dep = libgomp.ver-sun
libgomp.ver-sun : libgomp.ver \
		$(top_srcdir)/../contrib/make_sunver.pl \
		$(libgomp_la_OBJECTS) $(libgomp_la_LIBADD)
	perl $(top_srcdir)/../contrib/make_sunver.pl \
	  libgomp.ver \
	  $(libgomp_la_OBJECTS:%.lo=.libs/%.o) \
	 `echo $(libgomp_la_LIBADD) | \
	    sed 's,/\([^/.]*\)\.la,/.libs/\1.a,g'` \
	 > $@ || (rm -f $@ ; exit 1)
endif
else
libgomp_version_script =
libgomp_version_dep =
endif
libgomp_version_info = -version-info $(libtool_VERSION)
libgomp_la_LDFLAGS = $(libgomp_version_info) $(libgomp_version_script) \
        $(lt_host_flags)
libgomp_la_DEPENDENCIES = $(libgomp_version_dep)
libgomp_la_LINK = $(LINK) $(libgomp_la_LDFLAGS)

libgomp_la_SOURCES = alloc.c barrier.c critical.c env.c error.c iter.c \
	iter_ull.c loop.c loop_ull.c ordered.c parallel.c sections.c single.c \
	task.c team.c work.c lock.c mutex.c proc.c sem.c bar.c ptrlock.c \
	time.c fortran.c affinity.c target.c

nodist_noinst_HEADERS = libgomp_f.h
nodist_libsubinclude_HEADERS = omp.h
if USE_FORTRAN
nodist_finclude_HEADERS = omp_lib.h omp_lib.f90 omp_lib.mod omp_lib_kinds.mod
endif

LTLDFLAGS = $(shell $(SHELL) $(top_srcdir)/../libtool-ldflags $(LDFLAGS))

LINK = $(LIBTOOL) --tag CC $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) --mode=link \
	$(CCLD) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LTLDFLAGS) -o $@

omp_lib_kinds.mod: omp_lib.mod
	:
omp_lib.mod: omp_lib.f90
	$(FC) $(FCFLAGS) -fsyntax-only omp_lib.f90
fortran.lo: libgomp_f.h
fortran.o: libgomp_f.h
env.lo: libgomp_f.h
env.o: libgomp_f.h


# Automake Documentation:
# If your package has Texinfo files in many directories, you can use the
# variable TEXINFO_TEX to tell Automake where to find the canonical
# `texinfo.tex' for your package. The value of this variable should be
# the relative path from the current `Makefile.am' to `texinfo.tex'.
TEXINFO_TEX   = ../gcc/doc/include/texinfo.tex

# Defines info, dvi, pdf and html targets
MAKEINFOFLAGS = -I $(srcdir)/../gcc/doc/include
info_TEXINFOS = libgomp.texi

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

stamp-geninsrc: libgomp.info
	cp -p $(top_builddir)/libgomp.info $(srcdir)/libgomp.info
	@touch $@

libgomp.info: $(STAMP_BUILD_INFO)

stamp-build-info: libgomp.texi
	$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) -I $(srcdir) -o libgomp.info $(srcdir)/libgomp.texi
	@touch $@


CLEANFILES = $(STAMP_GENINSRC) $(STAMP_BUILD_INFO) libgomp.info
MAINTAINERCLEANFILES = $(srcdir)/libgomp.info

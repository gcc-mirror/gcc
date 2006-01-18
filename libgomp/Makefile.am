## Process this file with automake to produce Makefile.in

ACLOCAL_AMFLAGS = -I ../config
SUBDIRS = testsuite

## May be used by toolexeclibdir.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

config_path = @config_path@
search_path = $(addprefix $(top_srcdir)/config/, $(config_path)) $(top_srcdir)

empty =
space = $(empty) $(empty)
VPATH = $(subst $(space),:,$(strip $(search_path)))

AM_CPPFLAGS = $(addprefix -I, $(search_path))
AM_CFLAGS = $(XCFLAGS)
AM_LDFLAGS = $(XLDFLAGS) $(SECTION_LDFLAGS) $(OPT_LDFLAGS)

toolexeclib_LTLIBRARIES = libgomp.la
nodist_toolexeclib_HEADERS = libgomp.spec

if LIBGOMP_BUILD_VERSIONED_SHLIB
libgomp_version_script = -Wl,--version-script,$(top_srcdir)/libgomp.map
else
libgomp_version_script =
endif
libgomp_version_info = -version-info $(libtool_VERSION)
libgomp_la_LDFLAGS = $(libgomp_version_info) $(libgomp_version_script)

libgomp_la_SOURCES = alloc.c barrier.c critical.c env.c error.c iter.c \
	loop.c ordered.c parallel.c sections.c single.c team.c work.c \
	lock.c mutex.c proc.c sem.c bar.c time.c fortran.c

nodist_noinst_HEADERS = libgomp_f.h
nodist_include_HEADERS = omp.h omp_lib.h omp_lib.f90
if USE_FORTRAN
nodist_include_HEADERS += omp_lib.mod omp_lib_kinds.mod
endif

omp.h: omp.h.in mkomp_h.pl
	$(PERL) -w $(srcdir)/mkomp_h.pl "$(COMPILE)" $(srcdir)/omp.h.in omp.h
omp_lib.h: omp_lib.h.in mkomp_h.pl
	$(PERL) -w $(srcdir)/mkomp_h.pl "$(COMPILE)" $(srcdir)/omp_lib.h.in \
	omp_lib.h
omp_lib.f90: omp_lib.f90.in mkomp_h.pl
	$(PERL) -w $(srcdir)/mkomp_h.pl "$(COMPILE)" $(srcdir)/omp_lib.f90.in \
	omp_lib.f90
libgomp_f.h: libgomp_f.h.in mkomp_h.pl
	$(PERL) -w $(srcdir)/mkomp_h.pl "$(COMPILE)" $(srcdir)/libgomp_f.h.in \
	libgomp_f.h
omp_lib_kinds.mod: omp_lib.mod
	:
omp_lib.mod: omp_lib.f90
	$(FC) $(FCFLAGS) -fsyntax-only omp_lib.f90
fortran.lo: libgomp_f.h
fortran.o: libgomp_f.h
env.lo: libgomp_f.h
env.o: libgomp_f.h

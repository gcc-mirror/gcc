## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = 1.8 foreign


## Skip over everything if the quadlib is not available:
if BUILD_LIBQUADMATH
ACLOCAL_AMFLAGS = -I .. -I ../config

## May be used by toolexeclibdir.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

## Symbol versioning (copied from libssp).
if LIBQUAD_USE_SYMVER
if LIBQUAD_USE_SYMVER_GNU
version_arg = -Wl,--version-script=$(srcdir)/quadmath.map
version_dep = $(srcdir)/quadmath.map
endif
if LIBQUAD_USE_SYMVER_SUN
version_arg = -Wl,-M,quadmath.map-sun
version_dep = quadmath.map-sun
quadmath.map-sun : $(srcdir)/quadmath.map \
                $(top_srcdir)/../contrib/make_sunver.pl \
                $(libquadmath_la_OBJECTS) $(libquadmath_la_LIBADD)
        perl $(top_srcdir)/../contrib/make_sunver.pl \
          $(srcdir)/quadmath.map \
          $(libquadmath_la_OBJECTS:%.lo=.libs/%.o) \
         `echo $(libquadmath_la_LIBADD) | \
            sed 's,/\([^/.]*\)\.la,/.libs/\1.a,g'` \
         > $@ || (rm -f $@ ; exit 1)
endif
else
version_arg =
version_dep =
endif

LTLDFLAGS = $(shell $(SHELL) $(top_srcdir)/../libtool-ldflags $(LDFLAGS)) \
            -no-undefined


toolexeclib_LTLIBRARIES = libquadmath.la
libquadmath_la_LIBADD = -lm
libquadmath_la_LDFLAGS = $(LTLDFLAGS) -version-info `grep -v '^\#' $(srcdir)/libtool-version` $(version_arg)
libquadmath_la_LINK = $(LINK) $(libgfortran_la_LDFLAGS)
libquadmath_la_DEPENDENCIES = $(version_dep)

include_HEADERS = quadmath.h quadmath_weak.h

libquadmath_la_SOURCES = \
  gdtoa/arith.h gdtoa/gdtoa_fltrnds.h gdtoa/gd_qnan.h gdtoa/gdtoaimp.h \
  gdtoa/gdtoa.h quadmath-imp.h \
  gdtoa/dmisc.c gdtoa/gdtoa.c gdtoa/hd_init.c gdtoa/smisc.c gdtoa/sum.c \
  gdtoa/g_Qfmt.c gdtoa/gethex.c gdtoa/hexnan.c gdtoa/strtodg.c \
  gdtoa/ulp.c gdtoa/g__fmt.c gdtoa/gmisc.c gdtoa/misc.c gdtoa/strtopQ.c \
  quadmath_io.c \
  math/acoshq.c math/fmodq.c math/acosq.c math/frexpq.c \
  math/rem_pio2q.c math/asinhq.c math/hypotq.c math/remainderq.c \
  math/asinq.c math/rintq.c math/atan2q.c math/isinfq.c \
  math/roundq.c math/atanhq.c math/isnanq.c math/scalblnq.c math/atanq.c \
  math/j0q.c math/scalbnq.c math/cbrtq.c math/j1q.c math/signbitq.c \
  math/ceilq.c math/jnq.c math/sincos_table.c math/complex.c math/ldexpq.c \
  math/sincosq.c math/copysignq.c math/lgammaq.c math/sincosq_kernel.c \
  math/coshq.c math/llroundq.c math/sinhq.c math/cosq.c math/log10q.c \
  math/sinq.c math/cosq_kernel.c math/log1pq.c math/sinq_kernel.c \
  math/erfq.c math/logq.c math/sqrtq.c math/expm1q.c math/lroundq.c \
  math/tanhq.c math/expq.c math/modfq.c math/tanq.c math/fabsq.c \
  math/nanq.c math/tgammaq.c math/finiteq.c math/nextafterq.c \
  math/truncq.c math/floorq.c math/powq.c

endif

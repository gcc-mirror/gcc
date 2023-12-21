## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = foreign info-in-builddir

## Skip over everything if the quadlib is not available:
if BUILD_LIBQUADMATH
ACLOCAL_AMFLAGS = -I .. -I ../config
AM_CPPFLAGS = -I $(top_srcdir)/../include
AM_CFLAGS = $(XCFLAGS)

## May be used by toolexeclibdir.
gcc_version := $(shell @get_gcc_base_ver@ $(top_srcdir)/../gcc/BASE-VER)

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
	 `echo $(libquadmath_la_OBJECTS) $(libquadmath_la_LIBADD) | \
	   sed 's,\([^/ 	]*\)\.l\([ao]\),.libs/\1.\2,g'` \
	 > $@ || (rm -f $@ ; exit 1)
endif
else
version_arg =
version_dep =
endif

toolexeclib_LTLIBRARIES = libquadmath.la
libquadmath_la_LIBADD = 

if ENABLE_DARWIN_AT_RPATH
libquadmath_darwin_rpath = -Wc,-nodefaultrpaths
libquadmath_darwin_rpath += -Wl,-rpath,@loader_path
endif
libquadmath_la_LDFLAGS = -version-info `grep -v '^\#' $(srcdir)/libtool-version` \
	$(version_arg) $(lt_host_flags) $(LIBM) $(libquadmath_darwin_rpath)
libquadmath_la_DEPENDENCIES = $(version_dep) $(libquadmath_la_LIBADD)

nodist_libsubinclude_HEADERS = quadmath.h quadmath_weak.h
libsubincludedir = $(libdir)/gcc/$(target_alias)/$(gcc_version)/include

libquadmath_la_SOURCES = \
  math/x2y2m1q.c math/acoshq.c math/fmodq.c \
  math/acosq.c math/frexpq.c \
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
  math/truncq.c math/floorq.c math/powq.c math/fmaq.c math/logbq.c \
  math/exp2q.c math/issignalingq.c math/lgammaq_neg.c math/lgammaq_product.c \
  math/tanq_kernel.c math/tgammaq_product.c math/casinhq_kernel.c \
  math/cacoshq.c math/cacosq.c math/casinhq.c math/casinq.c \
  math/catanhq.c math/catanq.c math/cimagq.c math/conjq.c math/cprojq.c \
  math/crealq.c math/fdimq.c math/fmaxq.c math/fminq.c math/ilogbq.c \
  math/llrintq.c math/log2q.c math/lrintq.c math/nearbyintq.c math/remquoq.c \
  math/ccoshq.c math/cexpq.c math/clog10q.c math/clogq.c math/csinq.c \
  math/csinhq.c math/csqrtq.c math/ctanq.c math/ctanhq.c \
  printf/addmul_1.c printf/add_n.c printf/cmp.c printf/divrem.c \
  printf/flt1282mpn.c printf/fpioconst.c printf/lshift.c printf/mul_1.c \
  printf/mul_n.c printf/mul.c printf/printf_fphex.c printf/printf_fp.c \
  printf/quadmath-printf.c printf/rshift.c printf/submul_1.c printf/sub_n.c \
  strtod/strtoflt128.c strtod/mpn2flt128.c strtod/tens_in_limb.c


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
	"EXPECT=$(EXPECT)" \
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

# Subdir rules rely on $(FLAGS_TO_PASS)
FLAGS_TO_PASS = $(AM_MAKEFLAGS)

MAKEOVERRIDES=

# AM_CONDITIONAL on configure option --generated-files-in-srcdir
if GENINSRC
STAMP_GENINSRC = stamp-geninsrc
else
STAMP_GENINSRC =
endif
ALL_LOCAL_DEPS = $(STAMP_GENINSRC)

# AM_CONDITIONAL on configure check ACX_CHECK_PROG_VER([MAKEINFO])
if BUILD_INFO
STAMP_BUILD_INFO = stamp-build-info
else
STAMP_BUILD_INFO =
endif


stamp-geninsrc: libquadmath.info
	cp -p $(top_builddir)/libquadmath.info $(srcdir)/libquadmath.info
	@touch $@

stamp-build-info: libquadmath.texi $(libquadmath_TEXINFOS)
	$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) -I $(srcdir) -o libquadmath.info $(srcdir)/libquadmath.texi
	@touch $@

CLEANFILES = $(STAMP_GENINSRC) $(STAMP_BUILD_INFO)
MAINTAINERCLEANFILES = $(srcdir)/libquadmath.info

endif BUILD_LIBQUADMATH

all-local: $(ALL_LOCAL_DEPS)

# Unconditionally override this target, so that automake's definition
# does not wrongly interfere.
libquadmath.info: $(STAMP_BUILD_INFO)


# Automake Documentation:
# If your package has Texinfo files in many directories, you can use the
# variable TEXINFO_TEX to tell Automake where to find the canonical
# `texinfo.tex' for your package. The value of this variable should be
# the relative path from the current `Makefile.am' to `texinfo.tex'.
TEXINFO_TEX   = ../gcc/doc/include/texinfo.tex

# Defines info, dvi, pdf and html targets
MAKEINFOFLAGS = -I $(srcdir)/../gcc/doc/include

if BUILD_LIBQUADMATH
info_TEXINFOS = libquadmath.texi
else
info_TEXINFOS = 
endif

libquadmath_TEXINFOS = libquadmath-vers.texi

libquadmath-vers.texi:
	echo "@set BUGURL $(REPORT_BUGS_TEXI)" > $@

include $(top_srcdir)/../multilib.am

# Process this file with automake to produce Makefile.in

 WARN_CFLAGS =
if LIBGUPC_SMP_RUNTIME
  targ_runtime_dir = smp
  WARN_CFLAGS += -W -Wall -Wwrite-strings -Wstrict-prototypes -Werror
  INC_CFLAGS =
endif

ACLOCAL_AMFLAGS = -I .. -I ../config

## May be used by toolexeclibdir.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

gcc_objdir = $(MULTIBUILDTOP)../../$(host_subdir)/gcc

config_path = @config_path@

upc_crtstuff_mak = $(top_srcdir)/@upc_crtstuff_mak@

libsubincludedir = $(libdir)/gcc/$(target_alias)/$(gcc_version)/include

LTLDFLAGS = $(shell $(SHELL) $(top_srcdir)/../libtool-ldflags $(LDFLAGS))

LINK = $(LIBTOOL) --tag CC $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) --mode=link \
        $(CCLD) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LTLDFLAGS) -o $@

UPC_HDRS = include/gasp.h include/gasp_upc.h include/gcc-upc.h \
           include/pupc.h include/upc_collective.h include/upc.h \
           include/upc_relaxed.h include/upc_strict.h

libsubinclude_HEADERS = $(UPC_HDRS)
nodist_noinst_HEADERS =
nodist_libsubinclude_HEADERS =


IN_LIB_CPPFLAGS  = -DIN_GCC -DIN_TARGET_LIBS
AM_CPPFLAGS = $(IN_LIB_CPPFLAGS)
AM_CFLAGS = $(WARN_CFLAGS)
AM_UPCFLAGS = $(WARN_CFLAGS)


SUBDIRS = testsuite


if LIBGUPC_GENERIC_COLLECTIVES
  collectives_dir = $(addprefix $(top_srcdir)/, collectives)
else
  collectives_dir =
endif

search_path =   $(addprefix $(top_srcdir)/config/, $(config_path)) \
                $(addprefix $(top_srcdir)/, $(targ_runtime_dir)) \
		$(collectives_dir) \
                $(addprefix $(top_srcdir)/, include) \
                $(top_srcdir)

vpath % $(strip $(search_path))

AM_CPPFLAGS += $(addprefix -I, $(search_path))
AM_CFLAGS += $(XCFLAGS)
AM_UPCFLAGS += $(XCFLAGS)
AM_LDFLAGS = $(XLDFLAGS) $(SECTION_LDFLAGS) $(OPT_LDFLAGS)

### Explicit build rules

LIBGUPC_SRCDIR = $(top_srcdir)/$(targ_runtime_dir)

EXTRA_DIST =
MOSTLYCLEANFILES =
UPC_RUNTIME_SRC =

if LIBGUPC_SMP_RUNTIME

EXTRA_DIST += gcc-upc-lib.in \
              gen-inline-libgupc.pl \
              gen-upc-ld-script.pl

if LIBGUPC_PTHREADS
libgupc_pt = libgupc_pt.la
else
libgupc_pt =
endif

# Even though we're using vpath, we have to qualify the
# filenames with "smp/", because the libgupc_pt.la rules
# won't find the dependents because the name of the target
# doesn't match (ie, upc_access.c compiles into pt_upc_access_pt.lo)

if LIBGUPC_AFFINITY
  UPC_AFFINITY_SUP = smp/upc_affinity.c
else
  UPC_AFFINITY_SUP = smp/upc_affinity_stub.c
endif
if LIBGUPC_GUM
  UPC_GUM_SUP = smp/upc_gum.c
else
  UPC_GUM_SUP =
endif
if LIBGUPC_NUMA
  UPC_NUMA_SUP = smp/upc_numa.c
else
  UPC_NUMA_SUP = smp/upc_numa_stub.c
endif


UPC_RUNTIME_SRC += smp/upc_access.c smp/upc_accessg.c smp/upc_access.h \
	smp/upc_addr.c smp/upc_affinity.h $(UPC_AFFINITY_SUP) \
	smp/upc_allocg.upc smp/upc_alloc.upc smp/upc_barrier.c \
	smp/upc_config.h smp/upc_debug.h \
	smp/upc_defs.h smp/upc_gasp.c $(UPC_GUM_SUP) smp/upc_libg.c \
	smp/upc_lib.h smp/upc_lock.c smp/upc_main.c smp/upc_mem.c \
	smp/upc_mem.h smp/upc_numa.h $(UPC_NUMA_SUP) smp/upc_pgm_info.c \
	smp/upc_pts.h smp/upc_pupc.c smp/upc_pupc.h smp/upc_sup.h \
	smp/upc_sync.h smp/upc_sysdep.c smp/upc_sysdep.h smp/upc_vm.c

UPC_RUNTIME_SRC_INLINE = config.h smp/upc_access.c smp/upc_access.h \
        smp/upc_config.h smp/upc_defs.h smp/upc_mem.h smp/upc_pts.h \
        smp/upc_sup.h smp/upc_sync.h

endif   ### LIBGUPC_SMP_RUNTIME ####

if LIBGUPC_GENERIC_COLLECTIVES

# We need full pathnames here, because vpath won't find
# the files if they don't exist yet.
LIBGUPC_COLL_SRCDIR=$(top_srcdir)/collectives
UPC_COLL_PREFIX_REDUCE_UPC = $(LIBGUPC_COLL_SRCDIR)/upc_coll_prefix_reduce.upc
UPC_COLL_REDUCE_UPC = $(LIBGUPC_COLL_SRCDIR)/upc_coll_reduce.upc

if MAINTAINER_MODE
$(UPC_COLL_PREFIX_REDUCE_UPC): gen-upc-coll-reduce.pl \
                               upc_coll_prefix_reduce.in 
	$(PERL) $+ > $@

$(UPC_COLL_REDUCE_UPC): gen-upc-coll-reduce.pl upc_coll_reduce.in 
	$(PERL) $+ > $@
endif

UPC_COLLECTIVES_SRC = collectives/upc_coll_broadcast.upc \
	collectives/upc_coll_err.upc collectives/upc_coll_exchange.upc \
	collectives/upc_coll_gather_all.upc collectives/upc_coll_gather.upc \
	collectives/upc_coll.h collectives/upc_coll_init.upc \
	collectives/upc_coll_permute.upc $(UPC_COLL_PREFIX_REDUCE_UPC) \
	$(UPC_COLL_REDUCE_UPC) collectives/upc_coll_scatter.upc \
	collectives/upc_coll_sort.upc

UPC_RUNTIME_SRC += $(UPC_COLLECTIVES_SRC)

EXTRA_DIST += gen-upc-coll-reduce.pl \
             upc_coll_prefix_reduce.in \
             upc_coll_reduce.in

endif

toolexeclib_LTLIBRARIES = libgupc.la $(libgupc_pt)
nodist_toolexeclib_HEADERS = upc-crtbegin.spec libgupc.spec upc-crtend.spec

gcc-upc-lib.h: gen-inline-libgupc.pl gcc-upc-lib.in \
               $(UPC_RUNTIME_SRC_INLINE)
	$(PERL) $+ > $@

libsubinclude_HEADERS += gcc-upc-lib.h

BUILT_SOURCES = gcc-upc-lib.h

if LIBGUPC_LINK_SCRIPT
UPC_LINK_SCRIPT = gupc.ld
$(UPC_LINK_SCRIPT): gen-upc-ld-script.pl
	-$(CC) $(LDFLAGS) -Xlinker --verbose -x none /dev/null 2>&1 | \
	 $(PERL) $+ > $@
BUILT_SOURCES += $(UPC_LINK_SCRIPT)
else
UPC_LINK_SCRIPT =
endif
nodist_toolexeclib_HEADERS += $(UPC_LINK_SCRIPT)


libgupc_la_SOURCES = $(UPC_RUNTIME_SRC)
libgupc_version_info = -version-info $(libtool_VERSION)
libgupc_la_LDFLAGS = $(libgupc_version_info)
libgupc_la_LINK = $(LINK) $(libgupc_la_LDFLAGS)
libgupc_la_LIBADD =
libgupc_la_DEPENDENCIES = $(libgupc_la_LIBADD)

if LIBGUPC_PTHREADS
nodist_libgupc_pt_la_SOURCES = $(UPC_RUNTIME_SRC)
libgupc_pt_la_LIBADD =
libgupc_pt_la_DEPENDENCIES = $(libgupc_pt_la_LIBADD)
libgupc_pt_la_CPPFLAGS = $(AM_CPPFLAGS) -DGUPCR_USE_PTHREADS=1
libgupc_pt_la_UPCFLAGS = $(AM_UPCFLAGS) -fupc-pthreads-model-tls
libgupc_pt_la_LINK = $(libgupc_la_LINK)
endif

if LIBGUPC_CRTSTUFF

toolexeclib_DATA = @upc_crtstuff_objs@

include $(gcc_objdir)/libgcc.mvars
include $(upc_crtstuff_mak)

endif

# Automake Documentation:
# If your package has Texinfo files in many directories, you can use the
# variable TEXINFO_TEX to tell Automake where to find the canonical
# `texinfo.tex' for your package. The value of this variable should be
# the relative path from the current `Makefile.am' to `texinfo.tex'.
TEXINFO_TEX   = ../../gcc/doc/include/texinfo.tex

# Defines info, dvi, pdf and html targets
MAKEINFOFLAGS = -I $(srcdir)/../gcc/doc/include
info_TEXINFOS = libgupc.texi

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

stamp-geninsrc: libgupc.info
	cp -p $(top_builddir)/libgupc.info $(srcdir)/libgupc.info
	@touch $@

libgupc.info: $(STAMP_BUILD_INFO)

stamp-build-info: libgupc.texi
	$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) -I $(srcdir) -o libgupc.info $(srcdir)/libgupc.texi
	@touch $@

CLEANFILES = $(STAMP_GENINSRC) $(STAMP_BUILD_INFO) libgupc.info
MAINTAINERCLEANFILES = $(srcdir)/libgupc.info

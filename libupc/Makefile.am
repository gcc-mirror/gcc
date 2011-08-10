# Process this file with automake to produce Makefile.in

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

if LIBUPC_SMP_RUNTIME

IN_LIB_CPPFLAGS  = -DIN_GCC -DIN_TARGET_LIBS
WARN_CFLAGS = -W -Wall -Wwrite-strings -Wstrict-prototypes -Werror
AM_CPPFLAGS = $(IN_LIB_CPPFLAGS)
AM_CFLAGS = $(WARN_CFLAGS)
AM_UPCFLAGS = $(WARN_CFLAGS)


SUBDIRS = testsuite

search_path =   $(addprefix $(top_srcdir)/config/, $(config_path)) \
                $(addprefix $(top_srcdir)/, smp) \
                $(addprefix $(top_srcdir)/, include) \
                $(top_srcdir)

vpath % $(strip $(search_path))

AM_CPPFLAGS += $(addprefix -I, $(search_path))
AM_CFLAGS += $(XCFLAGS)
AM_UPCFLAGS += $(XCFLAGS)
AM_LDFLAGS = $(XLDFLAGS) $(SECTION_LDFLAGS) $(OPT_LDFLAGS)

### Explicit build rules

LIBUPC_SMP_SRCDIR = $(top_srcdir)/smp

# We need full pathnames here, because vpath won't find
# the files if they don't exist yet.
UPC_COLL_PREFIX_REDUCE_UPC = $(LIBUPC_SMP_SRCDIR)/upc_coll_prefix_reduce.upc
UPC_COLL_REDUCE_UPC = $(LIBUPC_SMP_SRCDIR)/upc_coll_reduce.upc

if MAINTAINER_MODE
$(UPC_COLL_PREFIX_REDUCE_UPC): gen-upc-coll-reduce.pl upc_coll_prefix_reduce.in 
	$(PERL) $+ > $@

$(UPC_COLL_REDUCE_UPC): gen-upc-coll-reduce.pl upc_coll_reduce.in 
	$(PERL) $+ > $@
endif

EXTRA_DIST = gcc-upc-lib.in \
             gen-gccupc-inline-lib.pl \
             gen-gccupc-ld-script.pl \
	     gen-upc-coll-reduce.pl \
             upc_coll_prefix_reduce.in \
             upc_coll_reduce.in

if LIBUPC_PTHREADS
libupc_pt = libupc_pt.la
else
libupc_pt =
endif

toolexeclib_LTLIBRARIES = libupc.la $(libupc_pt)
nodist_toolexeclib_HEADERS = upc-crtbegin.spec libupc.spec upc-crtend.spec

# Even though we're using vpath, we have to qualify the
# filenames with "smp/", because the libupc_pt.la rules
# won't find the dependents because the name of the target
# doesn't match (ie, upc_access.c compiles into pt_upc_access_pt.lo)

if LIBUPC_AFFINITY
  UPC_AFFINITY_SUP = smp/upc_affinity.c
else
  UPC_AFFINITY_SUP = smp/upc_affinity_stub.c
endif
if LIBUPC_GUM
  UPC_GUM_SUP = smp/upc_gum.c
else
  UPC_GUM_SUP =
endif
if LIBUPC_NUMA
  UPC_NUMA_SUP = smp/upc_numa.c
else
  UPC_NUMA_SUP = smp/upc_numa_stub.c
endif

UPC_RUNTIME_SRC = smp/upc_access.c smp/upc_accessg.c smp/upc_access.h \
        smp/upc_addr.c smp/upc_affinity.h $(UPC_AFFINITY_SUP) \
        smp/upc_allocg.upc smp/upc_alloc.upc smp/upc_barrier.c \
        smp/upc_coll_broadcast.upc smp/upc_coll_err.upc \
        smp/upc_coll_exchange.upc smp/upc_coll_gather_all.upc \
        smp/upc_coll_gather.upc smp/upc_coll.h smp/upc_coll_init.upc \
        smp/upc_coll_permute.upc $(UPC_COLL_PREFIX_REDUCE_UPC) \
        $(UPC_COLL_REDUCE_UPC) smp/upc_coll_scatter.upc \
        smp/upc_coll_sort.upc smp/upc_config.h smp/upc_debug.h \
        smp/upc_defs.h smp/upc_gasp.c $(UPC_GUM_SUP) smp/upc_libg.c \
        smp/upc_lib.h smp/upc_lock.c smp/upc_main.c smp/upc_mem.c \
        smp/upc_mem.h smp/upc_numa.h $(UPC_NUMA_SUP) smp/upc_pgm_info.c \
        smp/upc_pts.h smp/upc_pupc.c smp/upc_pupc.h smp/upc_sup.h \
        smp/upc_sync.h smp/upc_sysdep.c smp/upc_sysdep.h smp/upc_vm.c

UPC_RUNTIME_SRC_INLINE = config.h smp/upc_access.c smp/upc_access.h \
        smp/upc_config.h smp/upc_defs.h smp/upc_mem.h smp/upc_pts.h \
        smp/upc_sup.h smp/upc_sync.h

gcc-upc-lib.h: gen-gccupc-inline-lib.pl gcc-upc-lib.in \
               $(UPC_RUNTIME_SRC_INLINE)
	$(PERL) $+ > $@

libsubinclude_HEADERS += gcc-upc-lib.h

BUILT_SOURCES = gcc-upc-lib.h

if LIBUPC_LINK_SCRIPT
UPC_LINK_SCRIPT = gccupc.ld
$(UPC_LINK_SCRIPT): gen-gccupc-ld-script.pl
	-$(CC) $(LDFLAGS) -Xlinker --verbose -x none /dev/null 2>&1 | \
	 $(PERL) $+ > $@
BUILT_SOURCES += $(UPC_LINK_SCRIPT)
else
UPC_LINK_SCRIPT =
endif
toolexeclib_SCRIPTS = $(UPC_LINK_SCRIPT)


libupc_la_SOURCES = $(UPC_RUNTIME_SRC)
libupc_version_info = -version-info $(libtool_VERSION)
libupc_la_LDFLAGS = $(libupc_version_info)
libupc_la_LINK = $(LINK) $(libupc_la_LDFLAGS)
libupc_la_LIBADD =
libupc_la_DEPENDENCIES = $(libupc_la_LIBADD)

if LIBUPC_PTHREADS
nodist_libupc_pt_la_SOURCES = $(UPC_RUNTIME_SRC)
libupc_pt_la_LIBADD =
libupc_pt_la_DEPENDENCIES = $(libupc_pt_la_LIBADD)
libupc_pt_la_CPPFLAGS = $(AM_CPPFLAGS) -DGUPCR_USE_PTHREADS=1
libupc_pt_la_UPCFLAGS = $(AM_UPCFLAGS) -fupc-pthreads-model-tls
libupc_pt_la_LINK = $(libupc_la_LINK)
endif

if LIBUPC_CRTSTUFF

toolexeclib_DATA = @upc_crtstuff_objs@

include $(gcc_objdir)/libgcc.mvars
include $(upc_crtstuff_mak)

endif

endif    ### LIBUPC_SMP_RUNTIME

# Automake Documentation:
# If your package has Texinfo files in many directories, you can use the
# variable TEXINFO_TEX to tell Automake where to find the canonical
# `texinfo.tex' for your package. The value of this variable should be
# the relative path from the current `Makefile.am' to `texinfo.tex'.
TEXINFO_TEX   = ../../gcc/doc/include/texinfo.tex

# Defines info, dvi, pdf and html targets
MAKEINFOFLAGS = -I $(srcdir)/../gcc/doc/include
info_TEXINFOS = libupc.texi

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

stamp-geninsrc: libupc.info
	cp -p $(top_builddir)/libupc.info $(srcdir)/libupc.info
	@touch $@

libupc.info: $(STAMP_BUILD_INFO)

stamp-build-info: libupc.texi
	$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) -I $(srcdir) -o libupc.info $(srcdir)/libupc.texi
	@touch $@

CLEANFILES = $(STAMP_GENINSRC) $(STAMP_BUILD_INFO) libupc.info
MAINTAINERCLEANFILES = $(srcdir)/libupc.info

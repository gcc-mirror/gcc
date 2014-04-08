# Process this file with automake to produce Makefile.in

 WARN_CFLAGS =
if LIBGUPC_SMP_RUNTIME
  targ_runtime_dir = smp
  WARN_CFLAGS += -W -Wall -Wwrite-strings -Wstrict-prototypes -Werror
  INC_CFLAGS =
endif
if LIBGUPC_PORTALS4_RUNTIME
  targ_runtime_dir = portals4
  WARN_CFLAGS += -W -Wall -Wwrite-strings -Wstrict-prototypes -Werror
  PORTALS4_IFLAG  = @portals4inc@
  INC_CFLAGS = $(PORTALS4_IFLAG)
endif

ACLOCAL_AMFLAGS = -I . -I .. -I ../config

## May be used by toolexeclibdir.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

gcc_objdir = $(MULTIBUILDTOP)../../$(host_subdir)/gcc

config_path = @config_path@

upc_crtstuff_mak = $(top_srcdir)/@upc_crtstuff_mak@

libsubincludedir = $(libdir)/gcc/$(target_alias)/$(gcc_version)/include

LTLDFLAGS = $(shell $(SHELL) $(top_srcdir)/../libtool-ldflags $(LDFLAGS))

LINK = $(LIBTOOL) --tag CC $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) --mode=link \
        $(CCLD) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LTLDFLAGS) -o $@


UPC_HDRS = include/upc_atomic.h include/upc_castable.h \
           include/gasp.h include/gasp_upc.h include/gcc-upc.h \
           include/pupc.h include/upc_collective.h include/upc.h \
           include/upc_nb.h \
           include/upc_relaxed.h include/upc_strict.h \
           include/upc_tick.h include/upc_types.h

libsubinclude_HEADERS = $(UPC_HDRS)
nodist_noinst_HEADERS =
nodist_libsubinclude_HEADERS =


IN_LIB_CPPFLAGS  = -DIN_GCC -DIN_TARGET_LIBS
AM_CPPFLAGS = $(IN_LIB_CPPFLAGS)
AM_CFLAGS = $(WARN_CFLAGS) $(INC_CFLAGS)
AM_UPCFLAGS = $(WARN_CFLAGS) $(INC_CFLAGS) -fno-strict-aliasing


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
AM_CPPFLAGS += $(addprefix -isystem, '.')
AM_CPPFLAGS += @bfdinc@
AM_CFLAGS += $(XCFLAGS)
AM_UPCFLAGS += $(XCFLAGS)
AM_LDFLAGS = $(XLDFLAGS) $(SECTION_LDFLAGS) $(OPT_LDFLAGS)

### Explicit build rules

LIBGUPC_SRCDIR = $(top_srcdir)/$(targ_runtime_dir)

MOSTLYCLEANFILES =
UPC_RUNTIME_SRC =

EXTRA_DIST = gen-inline-libgupc.pl gen-upc-ld-script.pl

LIBGUPC_COLL_SRCDIR = $(top_srcdir)/collectives
LIBGUPC_INC_SRCDIR = $(top_srcdir)/include
LIBGUPC_PORTALS4_SRCDIR = $(top_srcdir)/portals4
LIBGUPC_SMP_SRCDIR = $(top_srcdir)/smp
UPC_OPS_DEF = $(LIBGUPC_INC_SRCDIR)/upc_ops.def
UPC_TYPES_DEF = $(LIBGUPC_INC_SRCDIR)/upc_types.def

EXTRA_DIST += $(UPC_OPS_DEFS) $(UPC_TYPES_DEF)

AUTOGEN = autogen

UPC_BACKTRACE_SUP =
if LIBGUPC_BACKTRACE
if LIBGUPC_PORTALS4_RUNTIME
    UPC_BACKTRACE_SUP += portals4/gupcr_backtrace.h portals4/gupcr_backtrace.c
else
    UPC_BACKTRACE_SUP += smp/upc_backtrace.h smp/upc_backtrace.c
endif
endif
if LIBGUPC_SMP_RUNTIME

EXTRA_DIST += smp/gcc-upc-lib.in

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

if MAINTAINER_MODE

UPC_ATOMIC_DEF = $(LIBGUPC_SMP_SRCDIR)/upc_atomic.def
UPC_ATOMIC_TPL = $(LIBGUPC_SMP_SRCDIR)/upc_atomic.tpl
UPC_ATOMIC_UPC = $(LIBGUPC_SMP_SRCDIR)/upc_atomic.upc

EXTRA_DIST += $(UPC_ATOMIC_DEF) $(UPC_ATOMIC_TPL)

$(UPC_ATOMIC_UPC): $(UPC_ATOMIC_DEF) $(UPC_ATOMIC_TPL) \
			$(UPC_OP_DEF) $(UPC_TYPE_DEF)
	cd $(LIBGUPC_SMP_SRCDIR); \
	$(AUTOGEN) -L ../include upc_atomic.def
endif

UPC_RUNTIME_SRC += smp/upc_access.c smp/upc_accessg.c smp/upc_access.h \
	smp/upc_addr.c smp/upc_affinity.h $(UPC_AFFINITY_SUP) \
	smp/upc_allocg.upc smp/upc_alloc.upc \
	smp/upc_atomic.upc \
	$(UPC_BACKTRACE_SUP) \
	smp/upc_barrier.upc smp/upc_castable.upc smp/upc_config.h \
	smp/upc_debug.h smp/upc_debug.c \
	smp/upc_defs.h smp/upc_gasp.c $(UPC_GUM_SUP) \
	smp/upc_libat_lock.c smp/upc_libat_lock.h \
	smp/upc_libg.c smp/upc_lib.h \
	smp/upc_lock.upc smp/upc_lock.h \
	smp/upc_lock_sup.h smp/upc_lock_sup.c \
	smp/upc_main.c smp/upc_mem.c smp/upc_mem.h \
	smp/upc_nb.upc smp/upc_numa.h $(UPC_NUMA_SUP) \
	smp/upc_pgm_info.c smp/upc_pts.h smp/upc_pupc.c \
	smp/upc_pupc.h smp/upc_sup.h smp/upc_sync.h smp/upc_sysdep.c \
	smp/upc_sysdep.h smp/upc_tick.c smp/upc_vm.c

UPC_RUNTIME_SRC_INLINE = config.h smp/upc_access.c smp/upc_access.h \
        smp/upc_config.h smp/upc_defs.h smp/upc_mem.h smp/upc_pts.h \
        smp/upc_sup.h smp/upc_sync.h

endif   ### LIBGUPC_SMP_RUNTIME ####

if LIBGUPC_PORTALS4_RUNTIME

EXTRA_DIST += portals4/gcc-upc-lib.in

UPC_RUNTIME_SRC += portals4/gupcr_access.c portals4/gupcr_access.h \
	portals4/gupcr_addr.c \
	portals4/gupcr_alloc.upc portals4/gupcr_alloc.h \
	portals4/gupcr_atomic.upc \
	portals4/gupcr_atomic_sup.h portals4/gupcr_atomic_sup.c \
	$(UPC_BACKTRACE_SUP) \
	portals4/gupcr_barrier.h portals4/gupcr_barrier.c \
	portals4/gupcr_broadcast.h portals4/gupcr_broadcast.c \
	portals4/gupcr_castable.upc \
	portals4/gupcr_clock.c \
	portals4/gupcr_coll_sup.h portals4/gupcr_coll_sup.c \
	portals4/gupcr_config.h portals4/gupcr_defs.h \
	portals4/gupcr_env.c portals4/gupcr_gmem.h portals4/gupcr_gmem.c \
	portals4/gupcr_lib.h \
	portals4/gupcr_lock.upc portals4/gupcr_lock.h \
	portals4/gupcr_lock_sup.c portals4/gupcr_lock_sup.h \
	portals4/gupcr_main.c \
	portals4/gupcr_nb.upc \
	portals4/gupcr_nb_sup.c portals4/gupcr_nb_sup.h \
	portals4/gupcr_node.c portals4/gupcr_node.h \
	portals4/gupcr_mem.c portals4/gupcr_portals.c \
	portals4/gupcr_pgm_info.c portals4/gupcr_pts.h \
	portals4/gupcr_runtime.h portals4/gupcr_runtime.c \
	portals4/gupcr_shutdown.h portals4/gupcr_shutdown.c \
	portals4/gupcr_sup.h portals4/gupcr_sync.h \
	portals4/gupcr_tick.c \
	portals4/gupcr_utils.h portals4/gupcr_utils.c

UPC_RUNTIME_SRC_INLINE = config.h portals4/gupcr_access.c \
	portals4/gupcr_access.h portals4/gupcr_config.h \
	portals4/gupcr_defs.h portals4/gupcr_gmem.h \
	portals4/gupcr_node.h \
	portals4/gupcr_portals.h portals4/gupcr_pts.h \
	portals4/gupcr_sup.h portals4/gupcr_sync.h \
	portals4/gupcr_utils.h

if LIBGUPC_NODE_LOCAL_MEM_POSIX
  UPC_RUNTIME_SRC += portals4/gupcr_node_mem_posix.c
endif
if LIBGUPC_NODE_LOCAL_MEM_MMAP
  UPC_RUNTIME_SRC += portals4/gupcr_node_mem_mmap.c
endif

include doxygen-include.am
MOSTLYCLEANFILES += $(DX_CLEANFILES)
EXTRA_DIST += $(DX_CONFIG)

# Portals runtime only partialy uses generic collectives

# We need full pathnames here, because vpath won't find
# the files if they don't exist yet.
UPC_COLL_PREFIX_REDUCE_UPC = $(LIBGUPC_COLL_SRCDIR)/upc_coll_prefix_reduce.upc
UPC_COLL_REDUCE_UPC = $(LIBGUPC_PORTALS4_SRCDIR)/gupcr_coll_reduce.upc

if MAINTAINER_MODE
$(UPC_COLL_PREFIX_REDUCE_UPC): gen-upc-coll-reduce.pl \
                               upc_coll_prefix_reduce.in
	$(PERL) $+ > $@

$(UPC_COLL_REDUCE_UPC): gen-upc-coll-reduce.pl gupcr_coll_reduce.in
	$(PERL) $+ > $@
endif

UPC_RUNTIME_SRC += \
	portals4/upc_coll.h portals4/gupcr_coll_broadcast.upc \
	portals4/gupcr_coll_init.upc $(UPC_COLL_REDUCE_UPC)

UPC_COLLECTIVES_SRC = \
	collectives/upc_coll_err.upc collectives/upc_coll_exchange.upc \
	collectives/upc_coll_gather_all.upc collectives/upc_coll_gather.upc \
	collectives/upc_coll_permute.upc $(UPC_COLL_PREFIX_REDUCE_UPC) \
	collectives/upc_coll_scatter.upc collectives/upc_coll_sort.upc

UPC_RUNTIME_SRC += $(UPC_COLLECTIVES_SRC)

EXTRA_DIST += gen-upc-coll-reduce.pl \
             upc_coll_prefix_reduce.in \
             gupcr_coll_reduce.in

endif   ### LIBGUPC_PORTALS_RUNTIME ####

if LIBGUPC_GENERIC_COLLECTIVES

# We need full pathnames here, because vpath won't find
# the files if they don't exist yet.
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
libgupc_la_LIBADD = \
	../libatomic/libatomic_convenience_no_lock.la
libgupc_la_DEPENDENCIES = $(libgupc_la_LIBADD)

if LIBGUPC_PTHREADS
nodist_libgupc_pt_la_SOURCES = $(UPC_RUNTIME_SRC)
libgupc_pt_la_LIBADD = \
	../libatomic/libatomic_convenience_no_lock.la
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

CLEANFILES = $(STAMP_GENINSRC) $(STAMP_BUILD_INFO) libgupc.info \
	$(nodist_toolexeclib_HEADERS) gupc.ld gcc-upc-lib.h
MAINTAINERCLEANFILES = $(srcdir)/libgupc.info

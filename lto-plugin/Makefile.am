## Process this file with automake to produce Makefile.in.

ACLOCAL_AMFLAGS = -I .. -I ../config
AUTOMAKE_OPTIONS = no-dependencies

gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)
target_noncanonical := @target_noncanonical@
libexecsubdir := $(libexecdir)/gcc/$(real_target_noncanonical)/$(gcc_version)$(accel_dir_suffix)

AM_CPPFLAGS = -I$(top_srcdir)/../include $(DEFS)
AM_CFLAGS = @ac_lto_plugin_warn_cflags@
AM_LDFLAGS = @ac_lto_plugin_ldflags@
AM_LIBTOOLFLAGS = --tag=disable-static
override CFLAGS := $(filter-out -fsanitize=address,$(CFLAGS))
override LDFLAGS := $(filter-out -fsanitize=address,$(LDFLAGS))

libexecsub_LTLIBRARIES = liblto_plugin.la
gcc_build_dir = @gcc_build_dir@
in_gcc_libs = $(foreach lib, $(libexecsub_LTLIBRARIES), $(gcc_build_dir)/$(lib))

liblto_plugin_la_SOURCES = lto-plugin.c
# Note that we intentionally override the bindir supplied by ACX_LT_HOST_FLAGS.
liblto_plugin_la_LDFLAGS = $(AM_LDFLAGS) \
	$(lt_host_flags) -module -bindir $(libexecsubdir)
# Can be simplified when libiberty becomes a normal convenience library.
libiberty = $(with_libiberty)/libiberty.a
libiberty_noasan = $(with_libiberty)/noasan/libiberty.a
libiberty_pic = $(with_libiberty)/pic/libiberty.a
Wc=-Wc,
liblto_plugin_la_LIBADD = \
	$(if $(wildcard $(libiberty_noasan)),$(Wc)$(libiberty_noasan), \
	$(if $(wildcard $(libiberty_pic)),$(Wc)$(libiberty_pic),))
liblto_plugin_la_LDFLAGS += \
	$(if $(wildcard $(libiberty_noasan)),, \
	$(if $(wildcard $(libiberty_pic)),,-Wc,$(libiberty)))
liblto_plugin_la_DEPENDENCIES = \
	$(if $(wildcard $(libiberty_noasan)),$(libiberty_noasan), \
	$(if $(wildcard $(libiberty_pic)),$(libiberty_pic),))
LTLDFLAGS = $(shell $(SHELL) $(top_srcdir)/../libtool-ldflags $(LDFLAGS))
liblto_plugin_la_LINK = $(LIBTOOL) --tag=CC $(AM_LIBTOOLFLAGS) \
	$(LIBTOOLFLAGS) --mode=link $(CCLD) $(AM_CFLAGS) $(CFLAGS) \
	$(liblto_plugin_la_LDFLAGS) $(LTLDFLAGS) -o $@

all-local: $(in_gcc_libs)

$(in_gcc_libs) : $(gcc_build_dir)/%: %
	@if test "X`dlname=; . ./$*; echo dlname:$$dlname`" = "Xdlname:"; then \
	  echo WARNING: $* is static, not copying to $@ >&2 ; \
	else \
	  $(mkinstalldirs) $(gcc_build_dir) && \
	  $(LIBTOOL) $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) --mode=install $(INSTALL) $(INSTALL_STRIP_FLAG) $* `pwd`/$@ ; \
	fi

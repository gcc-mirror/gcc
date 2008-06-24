nativeexeclib_LTLIBRARIES = libjawt.la

libjawt_la_SOURCES = jawt.c
libjawt_la_LIBADD = $(top_builddir)/native/jni/gtk-peer/libgtkpeer.la
libjawt_la_LDFLAGS = $(AM_LDFLAGS) -avoid-version

AM_LDFLAGS = @CLASSPATH_MODULE@ @GTK_LIBS@ @PANGOFT2_LIBS@ @X_LIBS@ @XTEST_LIBS@
AM_CPPFLAGS = @CLASSPATH_INCLUDES@

# Just the WARNING_CFLAGS. We cannot use the strict flags since the gtk
# headers contain broken prototypes (by design, see gtkitemfactory.h).
AM_CFLAGS = @WARNING_CFLAGS@ @ERROR_CFLAGS@ \
            @GTK_CFLAGS@ @PANGOFT2_CFLAGS@ @EXTRA_CFLAGS@

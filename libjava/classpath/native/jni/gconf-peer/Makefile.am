nativeexeclib_LTLIBRARIES = libgconfpeer.la

libgconfpeer_la_SOURCES = GConfNativePeer.c

libgconfpeer_la_LIBADD = $(top_builddir)/native/jni/classpath/jcl.lo

libgconfpeer_la_LDFLAGS = $(AM_LDFLAGS) -avoid-version

AM_LDFLAGS = @CLASSPATH_MODULE@ @GCONF_LIBS@ @GDK_LIBS@

AM_CPPFLAGS = @CLASSPATH_INCLUDES@

AM_CFLAGS = @WARNING_CFLAGS@ @ERROR_CFLAGS@ @GCONF_CFLAGS@ @GDK_CFLAGS@ \
            @EXTRA_CFLAGS@

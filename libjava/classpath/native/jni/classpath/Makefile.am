# Header needed for jawt implementations such as the one found in ../gtk-peer.
EXTRA_DIST = classpath_jawt.h

noinst_LTLIBRARIES = libclasspath.la

libclasspath_la_SOURCES = jcl.c jcl.h \
			  jnilink.c jnilink.h \
			  native_state.c native_state.h

AM_LDFLAGS = @CLASSPATH_MODULE@
AM_CPPFLAGS = @CLASSPATH_INCLUDES@
AM_CFLAGS = @WARNING_CFLAGS@ @STRICT_WARNING_CFLAGS@ @ERROR_CFLAGS@

noinst_LTLIBRARIES = libclasspath.la

libclasspath_la_SOURCES = jcl.c jcl.h \
			  jnilink.c jnilink.h \
			  primlib.c primlib.h \
			  native_state.c native_state.h

AM_LDFLAGS = @CLASSPATH_MODULE@
AM_CPPFLAGS = @CLASSPATH_INCLUDES@
AM_CFLAGS = @WARNING_CFLAGS@ @STRICT_WARNING_CFLAGS@ @ERROR_CFLAGS@

nativeexeclib_LTLIBRARIES = libjavamath.la

libjavamath_la_SOURCES = gnu_java_math_GMP.c

libjavamath_la_LIBADD = $(top_builddir)/native/jni/classpath/jcl.lo

libjavamath_la_LDFLAGS = $(AM_LDFLAGS) -avoid-version

AM_LDFLAGS = @CLASSPATH_MODULE@ @GMP_LIBS@
AM_CPPFLAGS = @CLASSPATH_INCLUDES@
AM_CFLAGS = @WARNING_CFLAGS@ @ERROR_CFLAGS@ @GMP_CFLAGS@

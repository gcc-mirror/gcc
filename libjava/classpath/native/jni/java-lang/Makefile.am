pkglib_LTLIBRARIES = libjavalang.la libjavalangreflect.la

libjavalang_la_SOURCES = java_lang_VMSystem.c \
			 java_lang_VMFloat.c \
			 java_lang_VMDouble.c \
			 java_lang_Math.c \
			 java_lang_VMProcess.c

libjavalang_la_LIBADD = $(wildcard $(top_builddir)/native/fdlibm/*.lo) \
			$(top_builddir)/native/jni/classpath/jcl.lo

libjavalangreflect_la_SOURCES = java_lang_reflect_Array.c

AM_LDFLAGS = @CLASSPATH_MODULE@
AM_CPPFLAGS = @CLASSPATH_INCLUDES@ -I$(top_srcdir)/native/fdlibm
AM_CFLAGS = @WARNING_CFLAGS@ @STRICT_WARNING_CFLAGS@ @ERROR_CFLAGS@

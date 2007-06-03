nativeexeclib_LTLIBRARIES = libjavalang.la libjavalangreflect.la libjavalangmanagement.la

libjavalang_la_SOURCES = java_lang_VMSystem.c \
			 java_lang_VMFloat.c \
			 java_lang_VMDouble.c \
			 java_lang_VMMath.c \
			 java_lang_VMProcess.c

libjavalang_la_LIBADD = $(top_builddir)/native/fdlibm/libfdlibm.la \
			$(top_builddir)/native/jni/classpath/jcl.lo \
			$(top_builddir)/native/jni/native-lib/libclasspathnative.la

libjavalangreflect_la_SOURCES = java_lang_reflect_VMArray.c

libjavalangmanagement_la_SOURCES = gnu_java_lang_management_VMOperatingSystemMXBeanImpl.c

AM_LDFLAGS = @CLASSPATH_MODULE@
AM_CPPFLAGS = @CLASSPATH_INCLUDES@ -I$(top_srcdir)/native/fdlibm
AM_CFLAGS = @WARNING_CFLAGS@ @STRICT_WARNING_CFLAGS@ @ERROR_CFLAGS@

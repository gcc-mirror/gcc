nativeexeclib_LTLIBRARIES = libjavaio.la 

libjavaio_la_SOURCES = 	java_io_VMFile.c \
			java_io_VMObjectInputStream.c \
			java_io_VMObjectStreamClass.c

libjavaio_la_LIBADD = $(top_builddir)/native/jni/classpath/jcl.lo \
		      $(top_builddir)/native/jni/native-lib/libclasspathnative.la

AM_LDFLAGS = @CLASSPATH_MODULE@
AM_CPPFLAGS = @CLASSPATH_INCLUDES@
AM_CFLAGS = @WARNING_CFLAGS@ @STRICT_WARNING_CFLAGS@ @ERROR_CFLAGS@

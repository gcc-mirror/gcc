nativeexeclib_LTLIBRARIES = libjavanio.la

libjavanio_la_SOURCES = gnu_java_nio_VMPipe.c \
			gnu_java_nio_VMChannel.c \
			gnu_java_nio_VMSelector.c \
			gnu_java_nio_charset_iconv_IconvDecoder.c \
			gnu_java_nio_charset_iconv_IconvEncoder.c \
			java_nio_MappedByteBufferImpl.c \
			java_nio_VMDirectByteBuffer.c \
			gnu_java_nio_EpollSelectorImpl.c \
			gnu_java_nio_KqueueSelectorImpl.c \
			javanio.h

libjavanio_la_LIBADD = $(top_builddir)/native/jni/classpath/jcl.lo \
		       $(top_builddir)/native/jni/native-lib/libclasspathnative.la \
		       $(LTLIBICONV) 

# Directly included through javanio.h
EXTRA_DIST = javanio.c

AM_LDFLAGS = @CLASSPATH_MODULE@
AM_CPPFLAGS = @CLASSPATH_INCLUDES@
AM_CFLAGS = @WARNING_CFLAGS@ @STRICT_WARNING_CFLAGS@ @ERROR_CFLAGS@

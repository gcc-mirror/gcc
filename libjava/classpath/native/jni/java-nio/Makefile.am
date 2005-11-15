nativelib_LTLIBRARIES = libjavanio.la

libjavanio_la_SOURCES = gnu_java_nio_VMPipe.c \
			gnu_java_nio_VMSelector.c \
			gnu_java_nio_channels_FileChannelImpl.c \
			gnu_java_nio_charset_iconv_IconvDecoder.c \
			gnu_java_nio_charset_iconv_IconvEncoder.c \
			java_nio_MappedByteBufferImpl.c \
			java_nio_VMDirectByteBuffer.c

libjavanio_la_LIBADD = $(top_builddir)/native/jni/classpath/jcl.lo \
		       $(LTLIBICONV) 

AM_LDFLAGS = @CLASSPATH_MODULE@
AM_CPPFLAGS = @CLASSPATH_INCLUDES@
AM_CFLAGS = @WARNING_CFLAGS@ @STRICT_WARNING_CFLAGS@ @ERROR_CFLAGS@

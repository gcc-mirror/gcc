nativelib_LTLIBRARIES = libjavanet.la

libjavanet_la_SOURCES = javanet.c \
			javanet.h \
			java_net_VMInetAddress.c \
			java_net_VMNetworkInterface.c \
			gnu_java_net_PlainDatagramSocketImpl.c \
                        gnu_java_net_PlainSocketImpl.c

libjavanet_la_LIBADD = $(top_builddir)/native/jni/classpath/jcl.lo

AM_LDFLAGS = @CLASSPATH_MODULE@
AM_CPPFLAGS = @CLASSPATH_INCLUDES@
AM_CFLAGS = @WARNING_CFLAGS@ @STRICT_WARNING_CFLAGS@ @ERROR_CFLAGS@

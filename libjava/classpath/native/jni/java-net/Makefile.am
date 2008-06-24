nativeexeclib_LTLIBRARIES = libjavanet.la

if ENABLE_LOCAL_SOCKETS
local_sources = gnu_java_net_local_LocalSocketImpl.c \
		local.c \
		local.h
else
local_sources = gnu_java_net_local_LocalSocketImpl.c
endif

libjavanet_la_SOURCES = javanet.c \
			javanet.h \
			java_net_VMInetAddress.c \
			java_net_VMNetworkInterface.c \
			java_net_VMURLConnection.c \
                        gnu_java_net_VMPlainSocketImpl.c \
			$(local_sources)

libjavanet_la_LIBADD = $(top_builddir)/native/jni/classpath/jcl.lo \
		       $(top_builddir)/native/jni/native-lib/libclasspathnative.la \
		       $(LIBMAGIC)

AM_LDFLAGS = @CLASSPATH_MODULE@
AM_CPPFLAGS = @CLASSPATH_INCLUDES@
AM_CFLAGS = @WARNING_CFLAGS@ @STRICT_WARNING_CFLAGS@ @ERROR_CFLAGS@ \
            @EXTRA_CFLAGS@

nativeexeclib_LTLIBRARIES = libgstreamerpeer.la

libgstreamerpeer_la_SOURCES = GStreamerIOPeer.c \
                              gstinputstream.c \
														  gstclasspathsrc.c \
                              gstclasspathsrc.h \
                              gstinputstream.h

libgstreamerpeer_la_LIBADD = $(top_builddir)/native/jni/classpath/jcl.lo

libgstreamerpeer_la_LDFLAGS = $(AM_LDFLAGS) @GST_PLUGIN_LDFLAGS@ -avoid-version

AM_LDFLAGS = @CLASSPATH_MODULE@ @GSTREAMER_LIBS@ @GSTREAMER_BASE_LIBS@ \
             @GSTREAMER_PLUGINS_BASE_LIBS@ @GDK_LIBS@

AM_CPPFLAGS = @CLASSPATH_INCLUDES@

# We cannot use -Wwrite-strings and the strict flags since
# gstreamer contain broken prototypes (by design).
AM_CFLAGS = @WARNING_CFLAGS@ -Wno-write-strings -Wno-missing-field-initializers \
            @ERROR_CFLAGS@ -Wno-unused-parameter @GSTREAMER_BASE_CFLAGS@ \
            @GDK_CFLAGS@ @GSTREAMER_CFLAGS@ @GSTREAMER_PLUGINS_BASE_CFLAGS@ 

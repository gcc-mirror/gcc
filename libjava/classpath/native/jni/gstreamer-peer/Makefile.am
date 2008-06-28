nativeexeclib_LTLIBRARIES = libgstreamerpeer.la

libgstreamerpeer_la_SOURCES = gst_peer.c \
                              gstreamer_io_peer.c \
                              gst_native_data_line.c \
                              gst_input_stream.c \
                              gst_native_pipeline.c \
                              gst_classpath_src.c \
                              gst_peer.h \
                              gst_classpath_src.h \
                              gst_input_stream.h \
                              gst_native_pipeline.h

libgstreamerpeer_la_LIBADD = $(top_builddir)/native/jni/classpath/jcl.lo

libgstreamerpeer_la_LDFLAGS = $(AM_LDFLAGS) @GST_PLUGIN_LDFLAGS@ -avoid-version

AM_LDFLAGS = @CLASSPATH_MODULE@ @GSTREAMER_LIBS@ @GSTREAMER_BASE_LIBS@ \
             @GSTREAMER_PLUGINS_BASE_LIBS@ @GDK_LIBS@

AM_CPPFLAGS = @CLASSPATH_INCLUDES@

# We cannot use -Wwrite-strings and the strict flags since
# gstreamer contain broken prototypes (by design).
AM_CFLAGS = @WARNING_CFLAGS@ -Wno-write-strings \
            -Wno-missing-field-initializers \
            @ERROR_CFLAGS@ -Wno-unused-parameter @GSTREAMER_BASE_CFLAGS@ \
            @GDK_CFLAGS@ @GSTREAMER_CFLAGS@ @GSTREAMER_PLUGINS_BASE_CFLAGS@ \
            @EXTRA_CFLAGS@

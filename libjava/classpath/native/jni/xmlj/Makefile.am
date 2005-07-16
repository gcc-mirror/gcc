pkglib_LTLIBRARIES = libxmlj.la

libxmlj_la_SOURCES = \
xmlj_dom.c \
xmlj_dom.h \
xmlj_error.c \
xmlj_error.h \
xmlj_io.c \
xmlj_io.h \
xmlj_node.c \
xmlj_node.h \
xmlj_sax.c \
xmlj_sax.h \
xmlj_transform.c \
xmlj_util.c \
xmlj_util.h \
xmlj_xpath.c

AM_LDFLAGS = @CLASSPATH_MODULE@ @XML_LIBS@ @XSLT_LIBS@
AM_CPPFLAGS = @CLASSPATH_INCLUDES@

# Don't enable ERROR flags. Code isn't warning free yet.
AM_CFLAGS = @WARNING_CFLAGS@ @STRICT_WARNING_CFLAGS@ @XML_CFLAGS@ @XSLT_CFLAGS@

EXTRA_DIST = BUGS

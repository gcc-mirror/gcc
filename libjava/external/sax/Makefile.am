## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign subdir-objects

# May be used by various substitution variables.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

## The compiler with whatever flags we want for both -c and -C
## compiles.
GCJ_WITH_FLAGS = $(GCJ) --encoding=UTF-8 -Wno-deprecated -fclasspath= -fbootclasspath=$(BOOTCLASSPATH)

BOOTCLASSPATH = $(here)'$(CLASSPATH_SEPARATOR)'$(srcdir)'$(CLASSPATH_SEPARATOR)'$(top_srcdir)'$(CLASSPATH_SEPARATOR)'$(top_builddir)

AM_GCJFLAGS = \
	-B$(top_builddir) \
	@LIBGCJ_JAVAFLAGS@ \
	-fclasspath= -fbootclasspath=$(BOOTCLASSPATH) \
	--encoding=UTF-8 \
	-Wno-deprecated

BUILT_SOURCES = classes.stamp

sax.jar: classes.stamp
	find org -name '*.class' -print | $(ZIP) -cfME@ $@

classes.stamp:	$(source_files)
	here=`pwd`; cd $(srcdir); \
	$(GCJ_WITH_FLAGS) -C -d $$here $(source_files)
	echo > classes.stamp

mostlyclean-local:
	-find . -name '*.class' | xargs rm
	-rm classes.stamp sax.jar

noinst_LTLIBRARIES = libsax-gcj.la

libsax_gcj_la_SOURCES =	sax.jar
libsax_gcj_la_GCJFLAGS = -findirect-dispatch -fbootstrap-classes $(AM_GCJFLAGS)

source_files = \
org/xml/sax/SAXNotSupportedException.java \
org/xml/sax/helpers/NamespaceSupport.java \
org/xml/sax/helpers/AttributesImpl.java	\
org/xml/sax/helpers/LocatorImpl.java \
org/xml/sax/helpers/DefaultHandler.java	\
org/xml/sax/helpers/AttributeListImpl.java \
org/xml/sax/helpers/ParserFactory.java \
org/xml/sax/helpers/NewInstance.java \
org/xml/sax/helpers/XMLFilterImpl.java \
org/xml/sax/helpers/ParserAdapter.java \
org/xml/sax/helpers/XMLReaderAdapter.java \
org/xml/sax/helpers/XMLReaderFactory.java \
org/xml/sax/HandlerBase.java \
org/xml/sax/SAXException.java \
org/xml/sax/ContentHandler.java	\
org/xml/sax/SAXNotRecognizedException.java \
org/xml/sax/ErrorHandler.java \
org/xml/sax/AttributeList.java \
org/xml/sax/Locator.java \
org/xml/sax/Attributes.java \
org/xml/sax/SAXParseException.java \
org/xml/sax/XMLFilter.java \
org/xml/sax/EntityResolver.java	\
org/xml/sax/XMLReader.java \
org/xml/sax/ext/Locator2.java \
org/xml/sax/ext/LexicalHandler.java \
org/xml/sax/ext/Attributes2Impl.java \
org/xml/sax/ext/DeclHandler.java \
org/xml/sax/ext/Attributes2.java \
org/xml/sax/ext/EntityResolver2.java \
org/xml/sax/ext/Locator2Impl.java \
org/xml/sax/ext/DefaultHandler2.java \
org/xml/sax/InputSource.java \
org/xml/sax/DocumentHandler.java \
org/xml/sax/DTDHandler.java \
org/xml/sax/Parser.java

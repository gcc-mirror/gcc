## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign

ACLOCAL_AMFLAGS = -I .

if TESTSUBDIR
SUBDIRS = $(DIRLTDL) testsuite gcj include
else
SUBDIRS = $(DIRLTDL) gcj include
endif

# write_entries_to_file - writes each entry in a list
# to the specified file. Each entry is written individually
# to accomodate systems with severe command-line-length
# limitations.
# Parameters:
# $(1): variable containing entries to iterate over
# $(2): output file
write_entries_to_file = $(shell rm -f $(2) || :) $(shell touch $(2)) $(foreach object,$(1),$(shell echo $(object) >> $(2)))

## ################################################################

##
## What gets installed, and where.
##

if XLIB_AWT
cond_x_ltlibrary = lib-gnu-awt-xlib.la
## We require libstdc++-v3 to be in the same build tree.
xlib_includes = -I../libstdc++-v3/include -I../libstdc++-v3/include/$(target_alias) -I$(srcdir)/../libstdc++-v3/libsupc++
else
cond_x_ltlibrary =
xlib_includes =
endif

if GTK_AWT
cond_gtk_ltlibrary = lib-gnu-java-awt-peer-gtk.la
else
cond_gtk_ltlibrary =
endif

toolexeclib_LTLIBRARIES = libgcj.la lib-org-xml-sax.la lib-org-w3c-dom.la \
    $(cond_gtk_ltlibrary) $(cond_x_ltlibrary)
toolexecmainlib_DATA = libgcj.spec

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libgcj.pc

jardir = $(datadir)/java
jar_DATA = libgcj-@gcc_version@.jar

## FIXME: Using libdir violates GNU coding standards.
secdir = $(libdir)/security
## Where to install default logging property file.
propdir = $(libdir)

## For now, only on native systems.  FIXME.
if NATIVE
bin_PROGRAMS = jv-convert gij rmic rmiregistry
endif

bin_SCRIPTS = addr2name.awk

## ################################################################

##
## Compilers and compilation flags.
##

## CANADIAN is a misnomer.  Really we check to see if we must pick up
## the tools from the path.
if CANADIAN
if NULL_TARGET
## In this case, gcj is found outside the build tree.  However, zip is
## found in the build tree.
ZIP = $(MULTIBUILDTOP)../$(COMPPATH)/fastjar/jar
GCJH = gcjh
else
ZIP = jar
GCJH = $(target_alias)-gcjh
endif
else # CANADIAN
GCJH = $(MULTIBUILDTOP)../$(COMPPATH)/gcc/gcjh
ZIP = $(MULTIBUILDTOP)../$(COMPPATH)/fastjar/jar
endif # CANADIAN

## The compiler with whatever flags we want for both -c and -C
## compiles.
GCJ_WITH_FLAGS = $(GCJ) --encoding=UTF-8 -Wno-deprecated

GCJCOMPILE = $(LIBTOOL) --tag=GCJ --mode=compile $(GCJ_WITH_FLAGS) -fclasspath= -fbootclasspath=$(here) $(JC1FLAGS) -MD -MT $@ -MF $(@:.lo=.d) -c
GCJLINK = $(LIBTOOL) --tag=GCJ --mode=link $(GCJ) -L$(here) $(JC1FLAGS) $(LDFLAGS) -o $@
LIBLINK = $(LIBTOOL) --tag=CXX --mode=link $(CXX) -L$(here) $(JC1FLAGS) $(LDFLAGS) -o $@

## We define this because otherwise libtool can be run with different
## values of `CXX' and will then get confused and fail to work.  So,
## we use --tag explicitly.  Someday automake will be changed to do
## this automatically.
LTCXXCOMPILE = $(LIBTOOL) --tag=CXX --mode=compile $(CXX) $(DEFS) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS)

JAVAC = $(GCJ_WITH_FLAGS) -C

GCC_UNWIND_INCLUDE = @GCC_UNWIND_INCLUDE@

WARNINGS = -W -Wall
## We need _GNU_SOURCE defined for some Linux builds.  It doesn't hurt
## to always define it.
AM_CXXFLAGS = -fno-rtti -fnon-call-exceptions $(THREADCXXFLAGS) \
## Some systems don't allow `$' in identifiers by default, so we force it.
	-fdollars-in-identifiers \
## Detect bugs in the verifier implementation, and maybe other places.
	-Wswitch-enum \
	@LIBGCJ_CXXFLAGS@ @X_CFLAGS@ $(WARNINGS) -D_GNU_SOURCE \
	-DPREFIX="\"$(prefix)\"" -DLIBDIR="\"$(libdir)\"" \
	-DBOOT_CLASS_PATH="\"$(jardir)/$(jar_DATA)\""
if USING_GCC
AM_CFLAGS = @LIBGCJ_CFLAGS@ $(WARNINGS)
else
AM_CFLAGS = @LIBGCJ_CFLAGS@
endif

JCFLAGS = -g
JC1FLAGS = @LIBGCJ_JAVAFLAGS@ $(GCJFLAGS)

LIBFFIINCS = @LIBFFIINCS@

INCLUDES = -I$(top_srcdir) -Iinclude -I$(top_srcdir)/include \
	$(GCINCS) $(THREADINCS) $(INCLTDL) $(xlib_includes) \
	$(GCC_UNWIND_INCLUDE) $(ZINCS) $(LIBFFIINCS)

## ################################################################

##
## How to build libgcj.a and libgcj.jar
##

## Objects from C++ sources in subdirs.
nat_files = $(nat_source_files:.cc=.lo)
x_nat_files = $(x_nat_source_files:.cc=.lo)
## Objects from C sources in subdirs.
c_files = $(c_source_files:.c=.lo)
extra_cc_files = $(extra_cc_source_files:.cc=.lo)
## Objects from gtk-related C sources in subdirs.
gtk_c_files = $(gtk_c_source_files:.c=.lo)
## Objects from Java sources in subdirs.
javao_files = $(java_source_files:.java=.lo) \
	$(built_java_source_files:.java=.lo)
x_javao_files = $(x_java_source_files:.java=.lo)

libgcj_la_SOURCES = prims.cc jni.cc exception.cc \
	resolve.cc defineclass.cc interpret.cc verify.cc \
	$(nat_source_files)
EXTRA_libgcj_la_SOURCES = boehm.cc nogc.cc posix-threads.cc no-threads.cc \
	win32-threads.cc posix.cc win32.cc $(c_source_files) \
	$(extra_cc_source_files) $(java_source_files) $(built_java_source_files)
libgcj_la_DEPENDENCIES = libgcj-@gcc_version@.jar $(javao_files) \
	$(c_files) $(extra_cc_files) $(GCOBJS) $(THREADOBJS) \
       $(PLATFORMOBJS) $(LIBLTDL) $(LIBFFI) $(ZLIBS) $(GCLIBS)

libgcj_la_LIBADD = $(javao_files) $(c_files) $(extra_cc_files) $(GCOBJS) \
	$(THREADOBJS) $(PLATFORMOBJS)
# Include THREADLIBS here to ensure that the correct version of
# certain linuxthread functions get linked:
libgcj_la_LDFLAGS = -rpath $(toolexeclibdir) $(THREADLDFLAGS) $(THREADLIBS) \
	$(LIBLTDL) $(SYS_ZLIBS) \
## The mysterious backslash is consumed by make.
	-version-info `grep -v '^\#' $(srcdir)/libtool-version`
libgcj_la_LINK = $(LIBLINK)

# Gtk/Cairo JNI sources.
if GTK_CAIRO
gtk_cairo_c_source_files = \
jni/gtk-peer/gnu_java_awt_peer_gtk_GdkClasspathFontPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GdkClasspathFontPeerMetrics.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GdkGlyphVector.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GdkGraphics2D.c
else
gtk_cairo_c_source_files = 
endif

## Gtk JNI sources.
gtk_c_source_files = \
$(gtk_cairo_c_source_files) \
jni/gtk-peer/gnu_java_awt_peer_gtk_GdkFontMetrics.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GdkGraphics.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GdkPixbufDecoder.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkButtonPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkCanvasPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkCheckboxMenuItemPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkCheckboxPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkChoicePeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkClipboard.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkComponentPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkEmbeddedWindowPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkEvents.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkFileDialogPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkImagePainter.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkLabelPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkListPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkMainThread.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkMenuBarPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkMenuItemPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkMenuPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkPanelPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkPopupMenuPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkScrollBarPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkScrollPanePeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkTextAreaPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkTextComponentPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkTextFieldPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkToolkit.c	\
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkWindowPeer.c \
jni/gtk-peer/gthread-jni.c \
jni/classpath/jcl.c \
jni/classpath/jnilink.c	\
jni/classpath/native_state.c \
jni/classpath/primlib.c

## Java sources for Gtk peers.
gtk_awt_peer_sources = \
gnu/java/awt/peer/gtk/GdkClasspathFontPeer.java \
gnu/java/awt/peer/gtk/GdkClasspathFontPeerMetrics.java \
gnu/java/awt/peer/gtk/GdkFontMetrics.java \
gnu/java/awt/peer/gtk/GdkGlyphVector.java \
gnu/java/awt/peer/gtk/GdkGraphics.java \
gnu/java/awt/peer/gtk/GdkGraphics2D.java \
gnu/java/awt/peer/gtk/GdkPixbufDecoder.java \
gnu/java/awt/peer/gtk/GtkArg.java \
gnu/java/awt/peer/gtk/GtkArgList.java \
gnu/java/awt/peer/gtk/GtkButtonPeer.java \
gnu/java/awt/peer/gtk/GtkCanvasPeer.java \
gnu/java/awt/peer/gtk/GtkCheckboxGroupPeer.java	\
gnu/java/awt/peer/gtk/GtkCheckboxMenuItemPeer.java \
gnu/java/awt/peer/gtk/GtkCheckboxPeer.java \
gnu/java/awt/peer/gtk/GtkChoicePeer.java \
gnu/java/awt/peer/gtk/GtkClipboard.java	\
gnu/java/awt/peer/gtk/GtkComponentPeer.java \
gnu/java/awt/peer/gtk/GtkContainerPeer.java \
gnu/java/awt/peer/gtk/GtkDialogPeer.java \
gnu/java/awt/peer/gtk/GtkEmbeddedWindowPeer.java \
gnu/java/awt/peer/gtk/GtkFileDialogPeer.java \
gnu/java/awt/peer/gtk/GtkFontPeer.java \
gnu/java/awt/peer/gtk/GtkFramePeer.java	\
gnu/java/awt/peer/gtk/GtkGenericPeer.java \
gnu/java/awt/peer/gtk/GtkImage.java \
gnu/java/awt/peer/gtk/GtkImagePainter.java \
gnu/java/awt/peer/gtk/GtkLabelPeer.java	\
gnu/java/awt/peer/gtk/GtkListPeer.java \
gnu/java/awt/peer/gtk/GtkMainThread.java \
gnu/java/awt/peer/gtk/GtkMenuBarPeer.java \
gnu/java/awt/peer/gtk/GtkMenuComponentPeer.java	\
gnu/java/awt/peer/gtk/GtkMenuItemPeer.java \
gnu/java/awt/peer/gtk/GtkMenuPeer.java \
gnu/java/awt/peer/gtk/GtkOffScreenImage.java \
gnu/java/awt/peer/gtk/GtkPanelPeer.java	\
gnu/java/awt/peer/gtk/GtkPopupMenuPeer.java \
gnu/java/awt/peer/gtk/GtkScrollPanePeer.java \
gnu/java/awt/peer/gtk/GtkScrollbarPeer.java \
gnu/java/awt/peer/gtk/GtkTextAreaPeer.java \
gnu/java/awt/peer/gtk/GtkTextComponentPeer.java	\
gnu/java/awt/peer/gtk/GtkTextFieldPeer.java \
gnu/java/awt/peer/gtk/GtkToolkit.java \
gnu/java/awt/peer/gtk/GtkWindowPeer.java

gtk_c_headers = $(patsubst %.java,jniinclude/%.h,$(subst /,_,$(gtk_awt_peer_sources)))

$(gtk_c_headers): $(gtk_awt_peer_sources)
	@input=`echo $@ | sed -e 's,jniinclude/,,' -e 's,_,.,g' -e 's,.h$$,,'`; \
	echo "$(GCJH) -jni -d jniinclude -classpath '' -bootclasspath $(top_builddir) $$input"; \
	$(GCJH) -jni -d jniinclude -classpath '' -bootclasspath $(top_builddir) $$input

lib_gnu_java_awt_peer_gtk_la_SOURCES = \
$(gtk_awt_peer_sources) \
$(gtk_c_source_files) \
jni/gtk-peer/gthread-jni.h \
jni/gtk-peer/gtkpeer.h \
jni/classpath/jcl.h \
jni/classpath/jnilink.h	\
jni/classpath/native_state.h \
jni/classpath/primlib.h

lib_gnu_java_awt_peer_gtk_la_LIBADD = $(GTK_LIBS) $(GLIB_LIBS) $(LIBART_LIBS) $(CAIRO_LIBS) $(PANGOFT2_LIBS)
lib_gnu_java_awt_peer_gtk_la_LDFLAGS = \
## The mysterious backslash is consumed by make.
	-version-info `grep -v '^\#' $(srcdir)/libtool-version`

lib_org_w3c_dom_la_SOURCES = org/w3c/dom/Attr.java \
org/w3c/dom/CDATASection.java \
org/w3c/dom/CharacterData.java \
org/w3c/dom/Comment.java \
org/w3c/dom/DOMException.java \
org/w3c/dom/DOMImplementation.java \
org/w3c/dom/Document.java \
org/w3c/dom/DocumentFragment.java \
org/w3c/dom/DocumentType.java \
org/w3c/dom/Element.java \
org/w3c/dom/Entity.java \
org/w3c/dom/EntityReference.java \
org/w3c/dom/NamedNodeMap.java \
org/w3c/dom/Node.java \
org/w3c/dom/NodeList.java \
org/w3c/dom/Notation.java \
org/w3c/dom/ProcessingInstruction.java \
org/w3c/dom/Text.java \
org/w3c/dom/ranges/DocumentRange.java \
org/w3c/dom/ranges/Range.java \
org/w3c/dom/ranges/RangeException.java \
org/w3c/dom/traversal/DocumentTraversal.java \
org/w3c/dom/traversal/NodeFilter.java \
org/w3c/dom/traversal/NodeIterator.java \
org/w3c/dom/traversal/TreeWalker.java
## See jv_convert_LDADD.
lib_org_w3c_dom_la_LIBADD = -L$(here)/.libs libgcj.la
lib_org_w3c_dom_la_LDFLAGS = -rpath $(toolexeclibdir) \
## The mysterious backslash is consumed by make.
        -version-info `grep -v '^\#' $(srcdir)/libtool-version`

lib_org_xml_sax_la_SOURCES = org/xml/sax/ext/DeclHandler.java \
org/xml/sax/ext/LexicalHandler.java \
org/xml/sax/helpers/AttributeListImpl.java \
org/xml/sax/helpers/AttributesImpl.java \
org/xml/sax/helpers/DefaultHandler.java \
org/xml/sax/helpers/LocatorImpl.java \
org/xml/sax/helpers/NamespaceSupport.java \
org/xml/sax/helpers/NewInstance.java \
org/xml/sax/helpers/ParserAdapter.java \
org/xml/sax/helpers/ParserFactory.java \
org/xml/sax/helpers/XMLFilterImpl.java \
org/xml/sax/helpers/XMLReaderAdapter.java \
org/xml/sax/helpers/XMLReaderFactory.java \
org/xml/sax/AttributeList.java \
org/xml/sax/Attributes.java \
org/xml/sax/ContentHandler.java \
org/xml/sax/DTDHandler.java \
org/xml/sax/DocumentHandler.java \
org/xml/sax/EntityResolver.java \
org/xml/sax/ErrorHandler.java \
org/xml/sax/HandlerBase.java \
org/xml/sax/InputSource.java \
org/xml/sax/Locator.java \
org/xml/sax/Parser.java \
org/xml/sax/SAXException.java \
org/xml/sax/SAXNotRecognizedException.java \
org/xml/sax/SAXNotSupportedException.java \
org/xml/sax/SAXParseException.java \
org/xml/sax/XMLFilter.java \
org/xml/sax/XMLReader.java
## See jv_convert_LDADD.
lib_org_xml_sax_la_LIBADD = -L$(here)/.libs libgcj.la
lib_org_xml_sax_la_LDFLAGS = -rpath $(toolexeclibdir) \
## The mysterious backslash is consumed by make.
        -version-info `grep -v '^\#' $(srcdir)/libtool-version`

lib_gnu_awt_xlib_la_SOURCES = $(x_nat_source_files)
EXTRA_lib_gnu_awt_xlib_la_SOURCES = $(x_java_source_files)
lib_gnu_awt_xlib_la_DEPENDENCIES = libgcj-@gcc_version@.jar $(x_javao_files)
lib_gnu_awt_xlib_la_LIBADD = $(x_javao_files)
lib_gnu_awt_xlib_la_LDFLAGS = ../libstdc++-v3/src/libstdc++.la \
	@X_PRE_LIBS@ @X_LIBS@ -lX11 @X_EXTRA_LIBS@ \
        -rpath $(toolexeclibdir) \
## The mysterious backslash is consumed by make.
        -version-info `grep -v '^\#' $(srcdir)/libtool-version`
lib_gnu_awt_xlib_la_LINK = $(LIBLINK)

all_java_source_files = \
    $(java_source_files) \
    $(built_java_source_files) \
    $(lib_org_xml_sax_la_SOURCES) \
    $(lib_org_w3c_dom_la_SOURCES) \
    $(gtk_awt_peer_sources) \
    $(x_java_source_files)

all_java_class_files = $(all_java_source_files:.java=.class)

if ONESTEP

# Compile all classfiles in one go.

libgcj-@gcc_version@.jar: $(all_java_source_files)
	-@rm -f libgcj-@gcc_version@.jar
	@echo Compiling Java sourcefiles...
	@: $(call write_entries_to_file,$?,libgcj.sourcelist)
	$(JAVAC) $(JCFLAGS) -classpath '' -bootclasspath $(here):$(srcdir) -d $(here) @libgcj.sourcelist
## Note that we explicitly want to include directory information.
	find java gnu javax org -type d -o -type f -name '*.class' | \
	  sed -e '/\/\./d' -e '/\/xlib/d' | \
	  $(ZIP) cfM0E@ $@

# This next rule seems backward, but reflects the fact
# that 1) all classfiles are compiled in one go when the
# libgcj jarfile is built and 2) anything which depends
# on a particular .class file must wait until the jarfile
# is built.
$(all_java_class_files): libgcj-@gcc_version@.jar

else # !ONESTEP

# Compile each classfile individually.

.java.class:
	$(JAVAC) $(JCFLAGS) -classpath '' -bootclasspath $(here):$(srcdir) -d $(here) $<

libgcj-@gcc_version@.jar: $(all_java_class_files)
	-@rm -f libgcj-@gcc_version@.jar
## Note that we explicitly want to include directory information.
	find java gnu javax org -type d -o -type f -name '*.class' | \
	  sed -e '/\/\./d' -e '/\/xlib/d' | \
	  $(ZIP) cfM0E@ $@

endif

# Note: The libtool objects are removed by mostlyclean-local
# because of command-line-length issues.
MOSTLYCLEANFILES = $(nat_headers) $(x_nat_headers)

CLEANFILES = libgcj-@gcc_version@.jar

mostlyclean-local:
## Use libtool rm to remove each libtool object
	find . -name '*.lo' -print | xargs $(LIBTOOL) rm -f

clean-local:
## Remove every .class file that was created.
	find . -name '*.class' -print | xargs rm -f

distclean-local:
## Remove every .d file that was created.
	find . -name '*.d' -print | xargs rm -f

# Just remove the objects from C++ sources, for testing the C++ compiler.
clean-nat:
	rm -f $(nat_files) $(x_nat_files)

SUFFIXES = .class .java .h

.class.lo:
	$(GCJCOMPILE) -o $@ $<

.java.lo:
	$(GCJCOMPILE) -o $@ $<

## FIXME: For the .o files in subdirs, use a special rule.  The
## standard automake rule can't be overridden (this is a bug in
## automake), and it also won't put the .o files into subdirs.
$(nat_files) $(x_nat_files): %.lo: %.cc
	@echo '$(LTCXXCOMPILE) -MD -MT $@ -MF $(@:.lo=.pp) -c -o $@ $<'; \
	$(LTCXXCOMPILE) -MD -MT $@ -MF $(@:.lo=.pp) -c -o $@ $<
	@-mv $(@:.lo=.pp) $(@:.lo=.d)

## FIXME: see above
$(c_files): %.lo: %.c
	$(LTCOMPILE) -c -o $@ $<

## A special rule for interpret.lo for the time being.  Our current
## approach to stack-trace handling is fragile and will not work with
## unit-at-a-time.  So, for now we disable it.  This will be fixed by
## a larger patch in the future.
interpret.lo: interpret.cc
	$(LTCXXCOMPILE) -fno-unit-at-a-time -c -o $@ $<

$(extra_cc_files): %.lo: %.cc
	$(LTCXXCOMPILE) -c -o $@ $<

$(c_files): java/lang/fdlibm.h java/lang/ieeefp.h java/lang/mprec.h

## FIXME: see above
$(gtk_c_files): %.lo: %.c
	$(LTCOMPILE) -c -Ijniinclude -I$(srcdir)/jni/classpath -I$(srcdir)/jni/gtk-peer \
	$(GTK_CFLAGS) $(LIBART_CFLAGS) $(CAIRO_LIBS) $(PANGOFT2_LIBS) -o $@ $<

$(gtk_c_files): $(gtk_c_headers)

## FIXME: see above.

## Note: we omit StackTrace here, since it has an explicit rule a bit
## later, and GNU make will warn in this case.
$(filter-out gnu/gcj/runtime/StackTrace.lo, $(javao_files)) $(x_javao_files) $(lib_org_w3c_dom_la_OBJECTS) $(lib_org_xml_sax_la_OBJECTS): %.lo: %.java
	$(GCJCOMPILE) -o $@ $<

$(gtk_awt_peer_sources:.java=.lo): %.lo: %.java
	$(GCJCOMPILE) -fjni -o $@ $<

## A special case.  The sibcall optimization can change the number of
## frames on the stack, and StackTrace makes assumptions about this
## number.
gnu/gcj/runtime/StackTrace.lo: gnu/gcj/runtime/StackTrace.java
	$(GCJCOMPILE) -fno-optimize-sibling-calls -o $@ $<

## Pass the list of object files to libtool in a temporary file to 
## avoid tripping platform command line length limits.
libgcj.la: $(libgcj_la_OBJECTS) $(libgcj_la_DEPENDENCIES)
	@echo Creating list of files to link...
	@: $(call write_entries_to_file,$(libgcj_la_OBJECTS) $(libgcj_la_LIBADD),libgcj.objectlist)
	$(libgcj_la_LINK) -objectlist libgcj.objectlist \
	@GCLIBS@ @LIBFFI@ @ZLIBS@ \
	-rpath $(toolexeclibdir) $(libgcj_la_LDFLAGS) $(LIBS)

lib-gnu-awt-xlib.la: $(lib_gnu_awt_xlib_la_OBJECTS) $(lib_gnu_awt_xlib_la_DEPENDENCIES)
	@echo Creating list of files to link...
	@: $(call write_entries_to_file,$(lib_gnu_awt_xlib_la_OBJECTS) $(lib_gnu_awt_xlib_la_LIBADD),lib_gnu_awt_xlib.objectlist)
	$(lib_gnu_awt_xlib_la_LINK) -objectlist lib_gnu_awt_xlib.objectlist \
	-rpath $(toolexeclibdir) $(lib_gnu_awt_xlib_la_LDFLAGS) $(LIBS)

## ################################################################

##
## How to build header files.
##

.class.h:
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) $(basename $<)

## Header files used when compiling some of the nat* files.
ordinary_nat_headers = $(ordinary_java_source_files:.java=.h) \
	$(built_java_source_files:.java=.h)

inner_nat_headers = java/io/ObjectOutputStream$$PutField.h \
	java/io/ObjectInputStream$$GetField.h \
	java/lang/reflect/Proxy$$ProxyData.h \
	java/lang/reflect/Proxy$$ProxyType.h \
	gnu/java/net/PlainSocketImpl$$SocketInputStream.h \
	gnu/java/net/PlainSocketImpl$$SocketOutputStream.h

nat_headers = $(ordinary_nat_headers) $(inner_nat_headers)
nat_headers_install = $(ordinary_nat_headers)

x_nat_headers = $(x_java_source_files:.java=.h)

$(ordinary_nat_headers) $(x_nat_headers): %.h: %.class

java/lang/ClassLoader.h: java/lang/ClassLoader.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
		-friend 'jclass _Jv_FindClass (_Jv_Utf8Const *name, java::lang::ClassLoader *loader);' \
		-friend 'void _Jv_RunMain (jclass klass, const char *name, int argc, const char **argv, bool is_jar);' \
		$(basename $<)

java/lang/Thread.h: java/lang/Thread.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
		-prepend 'class _Jv_JNIEnv;' \
		-prepend '#define _JV_NOT_OWNER 1' \
		-prepend '#define _JV_INTERRUPTED 2' \
		-friend '_Jv_JNIEnv * _Jv_GetCurrentJNIEnv ();' \
		-friend 'void _Jv_SetCurrentJNIEnv (_Jv_JNIEnv *env);' \
		-friend 'void _Jv_ThreadRun (java::lang::Thread* thread);' \
		-friend 'jint _Jv_AttachCurrentThread(java::lang::Thread* thread);' \
		-friend 'java::lang::Thread* _Jv_AttachCurrentThread(jstring name, java::lang::ThreadGroup* group);' \
		-friend 'java::lang::Thread* _Jv_AttachCurrentThreadAsDaemon(jstring name, java::lang::ThreadGroup* group);' \
		-friend 'jint _Jv_DetachCurrentThread ();' \
		$(basename $<)

java/lang/String.h: java/lang/String.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	    -friend 'jchar* _Jv_GetStringChars (jstring str);' \
	    -friend 'jstring* _Jv_StringFindSlot (jchar*, jint, jint);' \
	    -friend 'jstring* _Jv_StringGetSlot (jstring);' \
	    -friend 'jstring _Jv_NewStringUtf8Const (_Jv_Utf8Const* str);' \
	    -friend 'jstring _Jv_NewStringLatin1 (const char*, jsize);' \
	    -friend 'jstring _Jv_AllocString (jsize);' \
	    $(basename $<)

java/lang/reflect/Constructor.h: java/lang/reflect/Constructor.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	    -friend 'jmethodID _Jv_FromReflectedConstructor (java::lang::reflect::Constructor *);' \
	    -friend 'class java::lang::Class;' \
	    -friend 'jobject _Jv_JNI_ToReflectedMethod (_Jv_JNIEnv *, jclass, jmethodID, jboolean);' \
	    $(basename $<)

java/lang/reflect/Field.h: java/lang/reflect/Field.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	    -friend 'jfieldID _Jv_FromReflectedField (java::lang::reflect::Field *);' \
	    -friend 'jobject _Jv_JNI_ToReflectedField (_Jv_JNIEnv*, jclass, jfieldID, jboolean);' \
	    -friend 'class java::lang::Class;' \
	    $(basename $<)

java/lang/reflect/Method.h: java/lang/reflect/Method.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	    -friend 'jmethodID _Jv_FromReflectedMethod (java::lang::reflect::Method *);' \
	    -friend 'class java::lang::Class;' \
	    -friend 'class java::io::ObjectInputStream;' \
	    -friend 'jobject _Jv_JNI_ToReflectedMethod (_Jv_JNIEnv *, jclass, jmethodID, jboolean);' \
	    $(basename $<)

java/lang/reflect/Proxy.h: java/lang/reflect/Proxy.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	    $(basename $<)

java/lang/reflect/Proxy$$ProxyData.h: java/lang/reflect/Proxy.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	'java/lang/reflect/Proxy$$ProxyData'

java/lang/reflect/Proxy$$ProxyType.h: java/lang/reflect/Proxy.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	'java/lang/reflect/Proxy$$ProxyType'

gnu/gcj/runtime/VMClassLoader.h: gnu/gcj/runtime/VMClassLoader.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	    -friend 'class ::java::lang::ClassLoader;' \
	    $(basename $<)

java/io/ObjectInputStream$$GetField.h: java/io/ObjectInputStream.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	'java/io/ObjectInputStream$$GetField'

java/io/ObjectOutputStream$$PutField.h: java/io/ObjectOutputStream.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	'java/io/ObjectOutputStream$$PutField'

gnu/java/net/PlainSocketImpl$$SocketInputStream.h: gnu/java/net/PlainSocketImpl.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	'gnu/java/net/PlainSocketImpl$$SocketInputStream'

gnu/java/net/PlainSocketImpl$$SocketOutputStream.h: gnu/java/net/PlainSocketImpl.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	'gnu/java/net/PlainSocketImpl$$SocketOutputStream'

## Headers we maintain by hand and which we want to install.
extra_headers = java/lang/Object.h java/lang/Class.h

$(extra_headers) $(srcdir)/java/lang/Object.h $(srcdir)/java/lang/Class.h:
	@:

## Install the headers.  It is fairly ugly that we have to do this by
## hand.
install-data-local:
	$(PRE_INSTALL)
## We use a GNU make trick here so that we don't go over the command
## length limit of some shells.
	@echo Creating list of headers to install...
	@: $(call write_entries_to_file,$(nat_headers_install) $(extra_headers),tmp-ilist)
	@cat tmp-ilist | while read f; do \
	  d="`echo $$f | sed -e 's,/[^/]*$$,,'`"; \
	  $(mkinstalldirs) $(DESTDIR)$(includedir)/$$d; \
	  if test -f $(srcdir)/$$f; then p=$(srcdir)/$$f; else p=$$f; fi; \
	  echo " $(INSTALL_DATA) $$p $(DESTDIR)$(includedir)/$$f"; \
	  $(INSTALL_DATA) $$p $(DESTDIR)$(includedir)/$$f; \
	done
	-@rm -f tmp-ilist
## FIXME: the obvious approach using lib_DATA doesn't work with
## automake 1.4.
	$(mkinstalldirs) $(DESTDIR)$(secdir)
	@for f in classpath.security libgcj.security; do \
	  echo " $(INSTALL_DATA) $(srcdir)/java/security/$$f $(DESTDIR)$(secdir)/$$f"; \
	  $(INSTALL_DATA) $(srcdir)/java/security/$$f $(DESTDIR)$(secdir)/$$f; \
	done
## Install inner class headers.
	$(INSTALL_DATA) 'java/io/ObjectOutputStream$$PutField.h' $(DESTDIR)$(includedir)/java/io/
	$(INSTALL_DATA) 'java/io/ObjectInputStream$$GetField.h' $(DESTDIR)$(includedir)/java/io/
	$(INSTALL_DATA) 'java/lang/reflect/Proxy$$ProxyData.h' $(DESTDIR)$(includedir)/java/lang/reflect/
	$(INSTALL_DATA) 'java/lang/reflect/Proxy$$ProxyType.h' $(DESTDIR)$(includedir)/java/lang/reflect/
	$(INSTALL_DATA) 'gnu/java/net/PlainSocketImpl$$SocketInputStream.h' $(DESTDIR)$(includedir)/gnu/java/net/
	$(INSTALL_DATA) 'gnu/java/net/PlainSocketImpl$$SocketOutputStream.h' $(DESTDIR)$(includedir)/gnu/java/net/
	$(INSTALL_DATA) $(srcdir)/java/util/logging/logging.properties $(DESTDIR)$(propdir)/logging.properties

## ################################################################

##
## Additional `check' targets for maintainer convenience.
##

## This is used for maintainer-check.  FIXME: should set from
## configure using AC_CHECK_TOOL.
NM = nm

## Try to make sure our library doesn't stomp the namespace.
maintainer-check: libgcj.la
	$(NM) .libs/libgcj.a | grep ' T ' \
## Anything with `4java' is assumed to be from .java source.
	  | grep -v '4java' \
## Anything with Jv is ok.
	  | grep -v 'Jv' \
## `terminate' and `unexpected' are part of the runtime.
	  | grep -v 'terminate__Fv' | grep -v 'unexpected__Fv'

## This rule can be used to see if the headers are more or less
## correct.
header-check: libgcj-@gcc_version@.jar $(nat_headers)
	rm -f htest.cc; \
	for h in $(nat_headers); do \
	  echo "#include \"$$h\"" >> htest.cc; \
	done; \
	$(CXXCOMPILE) -fsyntax-only htest.cc

## This rule can be used to see if all the .class files verify
## correctly.
class-check: libgcj-@gcc_version@.jar
	@ok=0; find . -name '*.class' -print | fgrep -v testsuite | \
	while read f; do \
	  echo "$(GCJ_WITH_FLAGS) --syntax-only $$f"; \
	  if $(GCJ_WITH_FLAGS) --syntax-only $$f; then \
	  :; else ok=1; fi; \
	done; exit $$ok

## This rule checks whether write_entries_to_file works properly.
write-entries-to-file-check:
	@echo Creating list of files to link...
	@: $(call write_entries_to_file,$(libgcj_la_OBJECTS) $(libgcj_la_LIBADD),libgcj.objectlist)

## ################################################################

##
## The `jv-convert' program and code to rebuild the converter header
## files.
##

## it only makes sense to try to rebuild the JIS .h files on native
## systems.
if NATIVE
if MAINTAINER_MODE
noinst_PROGRAMS = gen-from-JIS
endif
endif 

CONVERT_DIR = gnu/gcj/convert

jv_convert_SOURCES =
EXTRA_jv_convert_SOURCES = $(convert_source_files)
## We need -nodefaultlibs because we want to avoid gcj's `-lgcj'.  We
## need this because we are explicitly using libtool to link using the
## `.la' file.
jv_convert_LDFLAGS = --main=gnu.gcj.convert.Convert \
	-rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS)
jv_convert_LINK = $(GCJLINK)
## We don't explicitly link in the libraries we need; libgcj.la brings
## in all dependencies.  We need the -L so that gcj can find libgcj
## with `-lgcj', but it must come first, otherwise the -L flags
## brought in from libgcj.la would cause the install directories to be
## searched before the build-tree ones, and we'd get errors because of
## different libraries with the same SONAME from picky linkers such as
## Solaris'.  FIXME: should be _libs on some systems.
jv_convert_LDADD = -L$(here)/.libs libgcj.la
## Depend on the spec file to make sure it is up to date before
## linking this program.
jv_convert_DEPENDENCIES = $(convert_source_files:.java=.lo) \
	libgcj.la libgcj.spec

gij_SOURCES = gij.cc
## We need -nodefaultlibs because we want to avoid gcj's `-lgcj'.  We
## need this because we are explicitly using libtool to link using the
## `.la' file.
gij_LDFLAGS = -rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS)
gij_LINK = $(GCJLINK)
## See jv_convert_LDADD.
gij_LDADD = -L$(here)/.libs libgcj.la
## Depend on the spec file to make sure it is up to date before
## linking this program.
gij_DEPENDENCIES = libgcj.la libgcj.spec

rmic_SOURCES =
## This is a dummy definition.
EXTRA_rmic_SOURCES = $(rmi_java_source_files)
rmic_LDFLAGS = --main=gnu.java.rmi.rmic.RMIC \
	-rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS)
rmic_LINK = $(GCJLINK)
## See jv_convert_LDADD.
rmic_LDADD = -L$(here)/.libs libgcj.la
## Depend on the spec file to make sure it is up to date before
## linking this program.
rmic_DEPENDENCIES = libgcj.la libgcj.spec

rmiregistry_SOURCES =
## This is a dummy definition.
EXTRA_rmiregistry_SOURCES = $(rmi_java_source_files)
rmiregistry_LDFLAGS = --main=gnu.java.rmi.registry.RegistryImpl \
	-rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS)
rmiregistry_LINK = $(GCJLINK)
## See jv_convert_LDADD.
rmiregistry_LDADD = -L$(here)/.libs libgcj.la
## Depend on the spec file to make sure it is up to date before
## linking this program.
rmiregistry_DEPENDENCIES = libgcj.la libgcj.spec

# The Unicode consortium does not permit re-distributing the file JIS0201.TXT.
# You can get it from ftp://ftp.unicode.org/Public/MAPPINGS/EASTASIA/JIS/.

$(srcdir)/$(CONVERT_DIR)/JIS0201.h:  # $(CONVERT_DIR)/JIS0201.TXT
	echo '/* This file is automatically generated from Unicode tables */' > tmp-0201; \
	tr  -d '\r' <$(CONVERT_DIR)/JIS0201.TXT \
	| sed -n -e 's|\(0x..\).*\(0x....\).*#\(.*\)$$|MAP(0x00, \1, \2)  /*\3 */|p' \
	>> tmp-0201; \
	mv tmp-0201 $(srcdir)/$(CONVERT_DIR)/JIS0201.h

# The Unicode consortium does not permit re-distributing the file JIS0208.TXT.
# You can get it from ftp://ftp.unicode.org/Public/MAPPINGS/EASTASIA/JIS/.

$(srcdir)/$(CONVERT_DIR)/JIS0208.h:  # $(CONVERT_DIR)/JIS0208.TXT
	echo '/* This file is automatically generated from Unicode tables */' > tmp-0208; \
	tr  -d '\r' <$(CONVERT_DIR)/JIS0208.TXT \
	| sed -n -e 's|\(0x....\).*0x\(..\)\(..\).*\(0x....\).*#\(.*\)$$|MAP(0x\2, 0x\3, \4)  /*\5 */|p' \
	>> tmp-0208; \
	mv tmp-0208 $(srcdir)/$(CONVERT_DIR)/JIS0208.h

# The Unicode consortium does not permit re-distributing the file JIS0212.TXT.
# You can get it from ftp://ftp.unicode.org/Public/MAPPINGS/EASTASIA/JIS/.

$(srcdir)/$(CONVERT_DIR)/JIS0212.h:  # $(CONVERT_DIR)/JIS0212.TXT
	echo '/* This file is automatically generated from Unicode tables */' > tmp-0212; \
	tr  -d '\r' <$(CONVERT_DIR)/JIS0212.TXT \
	| sed -n -e 's|0x\(..\)\(..\).*\(0x....\).*#\(.*\)$$|MAP(0x\1, 0x\2, \3)  /*\4 */|p' \
	>> tmp-0212; \
	mv tmp-0212 $(srcdir)/$(CONVERT_DIR)/JIS0212.h


gen_from_JIS_SOURCES =
EXTRA_gen_from_JIS_SOURCES = $(srcdir)/$(CONVERT_DIR)/gen-from-JIS.c \
		$(srcdir)/$(CONVERT_DIR)/make-trie.c \
		$(srcdir)/$(CONVERT_DIR)/JIS0201.h \
		$(srcdir)/$(CONVERT_DIR)/JIS0208.h \
		$(srcdir)/$(CONVERT_DIR)/JIS0212.h
gen_from_JIS_LDADD = $(CONVERT_DIR)/gen-from-JIS.o $(CONVERT_DIR)/make-trie.o
gen_from_JIS_DEPENDENCIES = $(CONVERT_DIR)/gen-from-JIS.o \
		$(CONVERT_DIR)/make-trie.o

$(CONVERT_DIR)/gen-from-JIS.o: $(srcdir)/$(CONVERT_DIR)/gen-from-JIS.c \
		$(srcdir)/$(CONVERT_DIR)/JIS0201.h \
		$(srcdir)/$(CONVERT_DIR)/JIS0208.h \
		$(srcdir)/$(CONVERT_DIR)/JIS0212.h
	$(COMPILE) -c -o $(CONVERT_DIR)/gen-from-JIS.o \
		$(srcdir)/$(CONVERT_DIR)/gen-from-JIS.c

$(CONVERT_DIR)/make-trie.o: $(srcdir)/$(CONVERT_DIR)/make-trie.c
	$(COMPILE) -c -o $(CONVERT_DIR)/make-trie.o \
		$(srcdir)/$(CONVERT_DIR)/make-trie.c

if NATIVE
if MAINTAINER_MODE

$(srcdir)/$(CONVERT_DIR)/JIS0208_to_Unicode.cc: ./gen-from-JIS$(EXEEXT)
	 ./gen-from-JIS JIS0208>$(srcdir)/$(CONVERT_DIR)/JIS0208_to_Unicode.cc

$(srcdir)/$(CONVERT_DIR)/JIS0212_to_Unicode.cc: ./gen-from-JIS$(EXEEXT)
	 ./gen-from-JIS JIS0212>$(srcdir)/$(CONVERT_DIR)/JIS0212_to_Unicode.cc

$(srcdir)/$(CONVERT_DIR)/Unicode_to_JIS.cc: ./gen-from-JIS$(EXEEXT)
	 ./gen-from-JIS toJIS>$(srcdir)/$(CONVERT_DIR)/Unicode_to_JIS.cc

endif # MAINTAINER_MODE
endif # NATIVE


## ################################################################

##
## This section lists all the source files we care about.
##

convert_source_files = \
gnu/gcj/convert/BytesToUnicode.java \
gnu/gcj/convert/Convert.java \
gnu/gcj/convert/Input_8859_1.java \
gnu/gcj/convert/Input_ASCII.java \
gnu/gcj/convert/Input_EUCJIS.java \
gnu/gcj/convert/Input_JavaSrc.java \
gnu/gcj/convert/Input_SJIS.java \
gnu/gcj/convert/Input_UTF8.java	\
gnu/gcj/convert/Input_iconv.java \
gnu/gcj/convert/IOConverter.java \
gnu/gcj/convert/Output_8859_1.java \
gnu/gcj/convert/Output_ASCII.java \
gnu/gcj/convert/Output_EUCJIS.java \
gnu/gcj/convert/Output_JavaSrc.java \
gnu/gcj/convert/Output_SJIS.java \
gnu/gcj/convert/Output_UTF8.java \
gnu/gcj/convert/Output_iconv.java \
gnu/gcj/convert/UnicodeToBytes.java

## List of all .java files for which the .h file is maintained by
## hand.
special_java_source_files = java/lang/Class.java java/lang/Object.java

awt_java_source_files =	\
gnu/awt/LightweightRedirector.java \
gnu/awt/j2d/AbstractGraphicsState.java \
gnu/awt/j2d/DirectRasterGraphics.java \
gnu/awt/j2d/Graphics2DImpl.java \
gnu/awt/j2d/IntegerGraphicsState.java \
gnu/awt/j2d/MappedRaster.java \
gnu/java/awt/BitMaskExtent.java \
gnu/java/awt/Buffers.java \
gnu/java/awt/BitwiseXORComposite.java \
gnu/java/awt/ComponentDataBlitOp.java \
gnu/java/awt/ClasspathToolkit.java \
gnu/java/awt/EmbeddedWindow.java \
gnu/java/awt/EmbeddedWindowSupport.java \
gnu/java/awt/EventModifier.java \
gnu/java/awt/image/ImageDecoder.java \
gnu/java/awt/image/XBMDecoder.java \
gnu/java/awt/peer/EmbeddedWindowPeer.java \
gnu/java/awt/peer/GLightweightPeer.java \
gnu/java/awt/peer/ClasspathFontPeer.java \
gnu/java/beans/editors/ColorEditor.java	\
gnu/java/beans/editors/FontEditor.java \
gnu/java/beans/editors/NativeBooleanEditor.java	\
gnu/java/beans/editors/NativeByteEditor.java \
gnu/java/beans/editors/NativeDoubleEditor.java \
gnu/java/beans/editors/NativeFloatEditor.java \
gnu/java/beans/editors/NativeIntEditor.java \
gnu/java/beans/editors/NativeLongEditor.java \
gnu/java/beans/editors/NativeShortEditor.java \
gnu/java/beans/editors/StringEditor.java \
gnu/java/beans/info/ComponentBeanInfo.java \
gnu/java/beans/BeanInfoEmbryo.java \
gnu/java/beans/EmptyBeanInfo.java \
gnu/java/beans/ExplicitBeanInfo.java \
gnu/java/beans/IntrospectionIncubator.java \
gnu/javax/rmi/CORBA/DelegateFactory.java \
gnu/javax/rmi/CORBA/GetDelegateInstanceException.java \
gnu/javax/rmi/CORBA/PortableRemoteObjectDelegateImpl.java \
gnu/javax/rmi/CORBA/StubDelegateImpl.java \
gnu/javax/rmi/CORBA/UtilDelegateImpl.java \
gnu/javax/rmi/CORBA/ValueHandlerImpl.java \
gnu/javax/rmi/PortableServer.java \
java/applet/Applet.java \
java/applet/AppletStub.java \
java/applet/AppletContext.java \
java/applet/AudioClip.java \
java/awt/AWTError.java \
java/awt/AWTEvent.java \
java/awt/AWTEventMulticaster.java \
java/awt/AWTException.java \
java/awt/AWTPermission.java \
java/awt/ActiveEvent.java \
java/awt/Adjustable.java \
java/awt/BorderLayout.java \
java/awt/Button.java \
java/awt/Canvas.java \
java/awt/CardLayout.java \
java/awt/Checkbox.java \
java/awt/CheckboxGroup.java \
java/awt/CheckboxMenuItem.java \
java/awt/Choice.java \
java/awt/Color.java \
java/awt/Component.java \
java/awt/ComponentOrientation.java \
java/awt/Container.java \
java/awt/Cursor.java \
java/awt/Dialog.java \
java/awt/Dimension.java \
java/awt/Event.java \
java/awt/EventDispatchThread.java \
java/awt/EventQueue.java \
java/awt/FileDialog.java \
java/awt/FlowLayout.java \
java/awt/Font.java \
java/awt/FontMetrics.java \
java/awt/Frame.java \
java/awt/Graphics.java \
java/awt/Graphics2D.java \
java/awt/GraphicsConfiguration.java \
java/awt/GridBagConstraints.java \
java/awt/GridBagLayout.java \
java/awt/GridBagLayoutInfo.java \
java/awt/GridLayout.java \
java/awt/IllegalComponentStateException.java \
java/awt/Image.java \
java/awt/Insets.java \
java/awt/ItemSelectable.java \
java/awt/Label.java \
java/awt/LayoutManager.java \
java/awt/LayoutManager2.java \
java/awt/List.java \
java/awt/Menu.java \
java/awt/MenuBar.java \
java/awt/MenuComponent.java \
java/awt/MenuContainer.java \
java/awt/MenuItem.java \
java/awt/MenuShortcut.java \
java/awt/Paint.java \
java/awt/PaintContext.java \
java/awt/Panel.java \
java/awt/Point.java \
java/awt/PopupMenu.java \
java/awt/Rectangle.java \
java/awt/RenderingHints.java \
java/awt/ScrollPane.java \
java/awt/Scrollbar.java \
java/awt/Shape.java \
java/awt/TextArea.java \
java/awt/TextComponent.java \
java/awt/TextField.java \
java/awt/Toolkit.java \
java/awt/Transparency.java \
java/awt/Window.java \
java/awt/color/ColorSpace.java \
java/awt/color/ICC_ColorSpace.java \
java/awt/color/ICC_Profile.java \
java/awt/color/ICC_ProfileGray.java \
java/awt/color/ICC_ProfileRGB.java \
java/awt/color/CMMException.java \
java/awt/color/ProfileDataException.java \
java/awt/datatransfer/Clipboard.java \
java/awt/datatransfer/ClipboardOwner.java \
java/awt/datatransfer/DataFlavor.java \
java/awt/datatransfer/FlavorMap.java \
java/awt/datatransfer/MimeTypeParseException.java \
java/awt/datatransfer/StringSelection.java \
java/awt/datatransfer/SystemFlavorMap.java \
java/awt/datatransfer/Transferable.java \
java/awt/datatransfer/UnsupportedFlavorException.java \
java/awt/datatransfer/FlavorTable.java \
java/awt/event/AWTEventListener.java \
java/awt/event/ActionEvent.java \
java/awt/event/ActionListener.java \
java/awt/event/AdjustmentEvent.java \
java/awt/event/AdjustmentListener.java \
java/awt/event/ComponentAdapter.java \
java/awt/event/ComponentEvent.java \
java/awt/event/ComponentListener.java \
java/awt/event/ContainerAdapter.java \
java/awt/event/ContainerEvent.java \
java/awt/event/ContainerListener.java \
java/awt/event/FocusAdapter.java \
java/awt/event/FocusEvent.java \
java/awt/event/FocusListener.java \
java/awt/event/HierarchyBoundsAdapter.java \
java/awt/event/HierarchyBoundsListener.java \
java/awt/event/HierarchyEvent.java \
java/awt/event/HierarchyListener.java \
java/awt/event/InputEvent.java \
java/awt/event/InputMethodEvent.java \
java/awt/event/InputMethodListener.java \
java/awt/event/InvocationEvent.java \
java/awt/event/ItemEvent.java \
java/awt/event/ItemListener.java \
java/awt/event/KeyAdapter.java \
java/awt/event/KeyEvent.java \
java/awt/event/KeyListener.java \
java/awt/event/MouseAdapter.java \
java/awt/event/MouseEvent.java \
java/awt/event/MouseListener.java \
java/awt/event/MouseMotionAdapter.java \
java/awt/event/MouseMotionListener.java \
java/awt/event/PaintEvent.java \
java/awt/event/TextEvent.java \
java/awt/event/TextListener.java \
java/awt/event/WindowAdapter.java \
java/awt/event/WindowEvent.java \
java/awt/event/WindowListener.java \
java/awt/event/AWTEventListenerProxy.java \
java/awt/event/MouseWheelEvent.java \
java/awt/event/MouseWheelListener.java \
java/awt/event/WindowFocusListener.java \
java/awt/event/WindowStateListener.java \
java/awt/font/FontRenderContext.java \
java/awt/font/ShapeGraphicAttribute.java \
java/awt/font/MultipleMaster.java \
java/awt/font/TransformAttribute.java \
java/awt/font/GlyphJustificationInfo.java \
java/awt/font/LineBreakMeasurer.java \
java/awt/font/TextMeasurer.java \
java/awt/font/TextLayout.java \
java/awt/font/LineMetrics.java \
java/awt/font/TextAttribute.java \
java/awt/font/GlyphMetrics.java \
java/awt/font/OpenType.java \
java/awt/font/GlyphVector.java \
java/awt/font/GraphicAttribute.java \
java/awt/font/ImageGraphicAttribute.java \
java/awt/font/NumericShaper.java \
java/awt/geom/AffineTransform.java \
java/awt/geom/Dimension2D.java \
java/awt/geom/Ellipse2D.java \
java/awt/geom/IllegalPathStateException.java \
java/awt/geom/NoninvertibleTransformException.java \
java/awt/geom/PathIterator.java \
java/awt/geom/Point2D.java \
java/awt/geom/Rectangle2D.java \
java/awt/geom/RectangularShape.java \
java/awt/geom/RoundRectangle2D.java \
java/awt/geom/Line2D.java \
java/awt/geom/Arc2D.java \
java/awt/geom/Area.java \
java/awt/geom/CubicCurve2D.java \
java/awt/geom/FlatteningPathIterator.java \
java/awt/geom/GeneralPath.java \
java/awt/geom/QuadCurve2D.java \
java/awt/image/BufferedImage.java \
java/awt/image/ColorModel.java \
java/awt/image/ComponentColorModel.java \
java/awt/image/ComponentSampleModel.java \
java/awt/image/DataBuffer.java \
java/awt/image/DataBufferByte.java \
java/awt/image/DataBufferInt.java \
java/awt/image/DataBufferUShort.java \
java/awt/image/DirectColorModel.java \
java/awt/image/ImageConsumer.java \
java/awt/image/ImageObserver.java \
java/awt/image/ImageProducer.java \
java/awt/image/IndexColorModel.java \
java/awt/image/PackedColorModel.java \
java/awt/image/Raster.java \
java/awt/image/RasterOp.java \
java/awt/image/SampleModel.java \
java/awt/image/SinglePixelPackedSampleModel.java \
java/awt/image/WritableRaster.java \
java/awt/image/AreaAveragingScaleFilter.java \
java/awt/image/CropImageFilter.java \
java/awt/image/FilteredImageSource.java \
java/awt/image/ImageFilter.java \
java/awt/image/MemoryImageSource.java \
java/awt/image/PixelGrabber.java \
java/awt/image/RGBImageFilter.java \
java/awt/image/ReplicateScaleFilter.java \
java/awt/image/BufferStrategy.java \
java/awt/image/BufferedImageOp.java \
java/awt/image/ImagingOpException.java \
java/awt/image/RasterFormatException.java \
java/awt/image/RenderedImage.java \
java/awt/image/TileObserver.java \
java/awt/image/VolatileImage.java \
java/awt/image/WritableRenderedImage.java \
java/awt/image/renderable/ContextualRenderedImageFactory.java \
java/awt/image/renderable/ParameterBlock.java \
java/awt/image/renderable/RenderContext.java \
java/awt/image/renderable/RenderableImage.java \
java/awt/image/renderable/RenderableImageOp.java \
java/awt/image/renderable/RenderableImageProducer.java \
java/awt/image/renderable/RenderedImageFactory.java \
java/awt/peer/ButtonPeer.java \
java/awt/peer/CanvasPeer.java \
java/awt/peer/CheckboxMenuItemPeer.java \
java/awt/peer/CheckboxPeer.java \
java/awt/peer/ChoicePeer.java \
java/awt/peer/ComponentPeer.java \
java/awt/peer/ContainerPeer.java \
java/awt/peer/DialogPeer.java \
java/awt/peer/FileDialogPeer.java \
java/awt/peer/FontPeer.java \
java/awt/peer/FramePeer.java \
java/awt/peer/LabelPeer.java \
java/awt/peer/LightweightPeer.java \
java/awt/peer/ListPeer.java \
java/awt/peer/MenuBarPeer.java \
java/awt/peer/MenuComponentPeer.java \
java/awt/peer/MenuItemPeer.java \
java/awt/peer/MenuPeer.java \
java/awt/peer/PanelPeer.java \
java/awt/peer/PopupMenuPeer.java \
java/awt/peer/RobotPeer.java \
java/awt/peer/ScrollPanePeer.java \
java/awt/peer/ScrollbarPeer.java \
java/awt/peer/TextAreaPeer.java \
java/awt/peer/TextComponentPeer.java \
java/awt/peer/TextFieldPeer.java \
java/awt/peer/WindowPeer.java \
java/awt/SystemColor.java \
java/awt/Polygon.java \
java/awt/MediaTracker.java \
java/awt/PrintGraphics.java \
java/awt/PrintJob.java \
java/awt/AWTKeyStroke.java \
java/awt/AlphaComposite.java \
java/awt/AttributeValue.java \
java/awt/BasicStroke.java \
java/awt/BufferCapabilities.java \
java/awt/ColorPaintContext.java \
java/awt/Composite.java \
java/awt/CompositeContext.java \
java/awt/ContainerOrderFocusTraversalPolicy.java \
java/awt/DefaultFocusTraversalPolicy.java \
java/awt/DefaultKeyboardFocusManager.java \
java/awt/DisplayMode.java \
java/awt/FocusTraversalPolicy.java \
java/awt/FontFormatException.java \
java/awt/GradientPaint.java \
java/awt/GraphicsConfigTemplate.java \
java/awt/GraphicsDevice.java \
java/awt/GraphicsEnvironment.java \
java/awt/HeadlessException.java \
java/awt/ImageCapabilities.java \
java/awt/JobAttributes.java \
java/awt/KeyEventDispatcher.java \
java/awt/KeyEventPostProcessor.java \
java/awt/KeyboardFocusManager.java \
java/awt/PageAttributes.java \
java/awt/Robot.java \
java/awt/ScrollPaneAdjustable.java \
java/awt/Stroke.java \
java/awt/TexturePaint.java \
java/awt/dnd/peer/DragSourceContextPeer.java \
java/awt/dnd/peer/DropTargetContextPeer.java \
java/awt/dnd/peer/DropTargetPeer.java \
java/awt/dnd/DnDConstants.java \
java/awt/dnd/DnDEventMulticaster.java \
java/awt/dnd/DragGestureEvent.java \
java/awt/dnd/DragGestureListener.java \
java/awt/dnd/DragGestureRecognizer.java	\
java/awt/dnd/DragSource.java \
java/awt/dnd/DragSourceAdapter.java \
java/awt/dnd/DragSourceContext.java \
java/awt/dnd/DragSourceDragEvent.java \
java/awt/dnd/DragSourceEvent.java \
java/awt/dnd/DragSourceListener.java \
java/awt/dnd/DragSourceMotionListener.java \
java/awt/dnd/DropTarget.java \
java/awt/dnd/InvalidDnDOperationException.java \
java/awt/dnd/Autoscroll.java \
java/awt/dnd/DragSourceDropEvent.java \
java/awt/dnd/DropTargetAdapter.java \
java/awt/dnd/DropTargetContext.java \
java/awt/dnd/DropTargetDragEvent.java \
java/awt/dnd/DropTargetDropEvent.java \
java/awt/dnd/DropTargetEvent.java \
java/awt/dnd/DropTargetListener.java \
java/awt/dnd/MouseDragGestureRecognizer.java \
java/awt/font/TextHitInfo.java \
java/awt/im/spi/InputMethod.java \
java/awt/im/spi/InputMethodContext.java \
java/awt/im/spi/InputMethodDescriptor.java \
java/awt/im/InputContext.java \
java/awt/im/InputMethodHighlight.java \
java/awt/im/InputMethodRequests.java \
java/awt/im/InputSubset.java \
java/awt/print/Book.java \
java/awt/print/PageFormat.java \
java/awt/print/Pageable.java \
java/awt/print/Paper.java \
java/awt/print/Printable.java \
java/awt/print/PrinterAbortException.java \
java/awt/print/PrinterException.java \
java/awt/print/PrinterGraphics.java \
java/awt/print/PrinterIOException.java \
java/awt/print/PrinterJob.java \
java/beans/beancontext/BeanContext.java \
java/beans/beancontext/BeanContextChild.java \
java/beans/beancontext/BeanContextChildComponentProxy.java \
java/beans/beancontext/BeanContextChildSupport.java \
java/beans/beancontext/BeanContextContainerProxy.java \
java/beans/beancontext/BeanContextEvent.java \
java/beans/beancontext/BeanContextMembershipEvent.java \
java/beans/beancontext/BeanContextMembershipListener.java \
java/beans/beancontext/BeanContextProxy.java \
java/beans/beancontext/BeanContextServiceAvailableEvent.java \
java/beans/beancontext/BeanContextServiceProvider.java \
java/beans/beancontext/BeanContextServiceProviderBeanInfo.java \
java/beans/beancontext/BeanContextServiceRevokedEvent.java \
java/beans/beancontext/BeanContextServiceRevokedListener.java \
java/beans/beancontext/BeanContextServices.java \
java/beans/beancontext/BeanContextServicesListener.java \
java/beans/beancontext/BeanContextServicesSupport.java \
java/beans/beancontext/BeanContextSupport.java \
java/beans/BeanDescriptor.java \
java/beans/BeanInfo.java \
java/beans/Beans.java \
java/beans/Customizer.java \
java/beans/DesignMode.java \
java/beans/EventSetDescriptor.java \
java/beans/ExceptionListener.java \
java/beans/FeatureDescriptor.java \
java/beans/IndexedPropertyDescriptor.java \
java/beans/IntrospectionException.java \
java/beans/Introspector.java \
java/beans/MethodDescriptor.java \
java/beans/ParameterDescriptor.java \
java/beans/PropertyChangeEvent.java \
java/beans/PropertyChangeListener.java \
java/beans/PropertyChangeListenerProxy.java \
java/beans/PropertyChangeSupport.java \
java/beans/PropertyDescriptor.java \
java/beans/PropertyEditor.java \
java/beans/PropertyEditorManager.java \
java/beans/PropertyEditorSupport.java \
java/beans/PropertyVetoException.java \
java/beans/SimpleBeanInfo.java \
java/beans/VetoableChangeListener.java \
java/beans/VetoableChangeListenerProxy.java \
java/beans/VetoableChangeSupport.java \
java/beans/Visibility.java \
java/beans/AppletInitializer.java \
javax/rmi/BAD_OPERATION.java \
javax/rmi/ORB.java \
javax/rmi/PortableRemoteObject.java \
javax/rmi/CORBA/ClassDesc.java \
javax/rmi/CORBA/ObjectImpl.java	\
javax/rmi/CORBA/PortableRemoteObjectDelegate.java \
javax/rmi/CORBA/StubDelegate.java \
javax/rmi/CORBA/Stub.java \
javax/rmi/CORBA/SystemException.java \
javax/rmi/CORBA/Tie.java \
javax/rmi/CORBA/UtilDelegate.java \
javax/rmi/CORBA/Util.java \
javax/rmi/CORBA/ValueHandler.java \
javax/swing/border/AbstractBorder.java \
javax/swing/border/BevelBorder.java \
javax/swing/border/Border.java \
javax/swing/border/CompoundBorder.java \
javax/swing/border/EmptyBorder.java \
javax/swing/border/EtchedBorder.java \
javax/swing/border/LineBorder.java \
javax/swing/border/MatteBorder.java \
javax/swing/border/SoftBevelBorder.java \
javax/swing/border/TitledBorder.java \
javax/swing/GrayFilter.java \
javax/swing/AbstractAction.java \
javax/swing/AbstractButton.java \
javax/swing/plaf/basic/BasicButtonUI.java \
javax/swing/plaf/basic/BasicCheckBoxUI.java \
javax/swing/plaf/basic/BasicDefaults.java \
javax/swing/plaf/basic/BasicGraphicsUtils.java \
javax/swing/plaf/basic/BasicLabelUI.java \
javax/swing/plaf/basic/BasicListUI.java \
javax/swing/plaf/basic/BasicOptionPaneUI.java \
javax/swing/plaf/basic/BasicPanelUI.java \
javax/swing/plaf/basic/BasicRadioButtonUI.java \
javax/swing/plaf/basic/BasicScrollPaneUI.java \
javax/swing/plaf/basic/BasicSplitPaneDivider.java \
javax/swing/plaf/basic/BasicSplitPaneUI.java \
javax/swing/plaf/basic/BasicTabbedPaneUI.java \
javax/swing/plaf/basic/BasicTextUI.java \
javax/swing/plaf/basic/BasicToggleButtonUI.java \
javax/swing/plaf/basic/BasicTreeUI.java \
javax/swing/plaf/basic/BasicViewportUI.java \
javax/swing/plaf/basic/BasicLookAndFeel.java \
javax/swing/plaf/basic/BasicBorders.java \
javax/swing/plaf/basic/BasicIconFactory.java \
javax/swing/plaf/BorderUIResource.java \
javax/swing/plaf/UIResource.java \
javax/swing/plaf/ButtonUI.java \
javax/swing/plaf/ComponentUI.java \
javax/swing/plaf/LabelUI.java \
javax/swing/plaf/ListUI.java \
javax/swing/plaf/OptionPaneUI.java \
javax/swing/plaf/PanelUI.java \
javax/swing/plaf/ScrollPaneUI.java \
javax/swing/plaf/TabbedPaneUI.java \
javax/swing/plaf/TextUI.java \
javax/swing/plaf/TreeUI.java \
javax/swing/plaf/ViewportUI.java \
javax/swing/plaf/metal/MetalLookAndFeel.java \
javax/swing/plaf/ColorUIResource.java \
javax/swing/plaf/DimensionUIResource.java \
javax/swing/plaf/FontUIResource.java \
javax/swing/plaf/IconUIResource.java \
javax/swing/plaf/InsetsUIResource.java \
javax/swing/plaf/ActionMapUIResource.java \
javax/swing/plaf/ColorChooserUI.java \
javax/swing/plaf/ComboBoxUI.java \
javax/swing/plaf/ComponentInputMapUIResource.java \
javax/swing/plaf/DesktopIconUI.java \
javax/swing/plaf/DesktopPaneUI.java \
javax/swing/plaf/FileChooserUI.java \
javax/swing/plaf/InputMapUIResource.java \
javax/swing/plaf/InternalFrameUI.java \
javax/swing/plaf/MenuBarUI.java \
javax/swing/plaf/MenuItemUI.java \
javax/swing/plaf/PopupMenuUI.java \
javax/swing/plaf/ProgressBarUI.java \
javax/swing/plaf/RootPaneUI.java \
javax/swing/plaf/ScrollBarUI.java \
javax/swing/plaf/SeparatorUI.java \
javax/swing/plaf/SliderUI.java \
javax/swing/plaf/SpinnerUI.java \
javax/swing/plaf/SplitPaneUI.java \
javax/swing/plaf/TableHeaderUI.java \
javax/swing/plaf/TableUI.java \
javax/swing/plaf/ToolBarUI.java \
javax/swing/plaf/ToolTipUI.java \
javax/swing/AbstractListModel.java \
javax/swing/AbstractSet.java \
javax/swing/colorchooser/AbstractColorChooserPanel.java \
javax/swing/colorchooser/ColorChooserComponentFactory.java \
javax/swing/colorchooser/ColorSelectionModel.java \
javax/swing/colorchooser/DefaultColorSelectionModel.java \
javax/swing/filechooser/FileFilter.java \
javax/swing/filechooser/FileSystemView.java \
javax/swing/filechooser/FileView.java \
javax/swing/table/AbstractTableModel.java \
javax/swing/table/DefaultTableColumnModel.java \
javax/swing/table/DefaultTableModel.java \
javax/swing/table/JTableHeader.java \
javax/swing/table/TableCellEditor.java \
javax/swing/table/TableCellRenderer.java \
javax/swing/table/TableColumn.java \
javax/swing/table/TableColumnModel.java \
javax/swing/table/TableModel.java \
javax/swing/table/DefaultTableCellRenderer.java \
javax/swing/Action.java \
javax/swing/BorderFactory.java \
javax/swing/Box.java \
javax/swing/BoxLayout.java \
javax/swing/ButtonGroup.java \
javax/swing/ButtonModel.java \
javax/swing/ComponentInputMap.java \
javax/swing/DefaultButtonModel.java \
javax/swing/DefaultCellRenderer.java \
javax/swing/DefaultListModel.java \
javax/swing/DefaultListSelectionModel.java \
javax/swing/Icon.java \
javax/swing/ImageIcon.java \
javax/swing/InputMap.java \
javax/swing/JApplet.java \
javax/swing/JButton.java \
javax/swing/JCheckBox.java \
javax/swing/JComponent.java \
javax/swing/JDialog.java \
javax/swing/JEditorPane.java \
javax/swing/JFormattedTextField.java \
javax/swing/JFrame.java \
javax/swing/JLabel.java \
javax/swing/JLayeredPane.java \
javax/swing/JList.java \
javax/swing/JMenuBar.java \
javax/swing/JOptionPane.java \
javax/swing/JPanel.java \
javax/swing/JRadioButton.java \
javax/swing/JRootPane.java \
javax/swing/JScrollBar.java \
javax/swing/JScrollPane.java \
javax/swing/JTabbedPane.java \
javax/swing/JTextField.java \
javax/swing/JToggleButton.java \
javax/swing/JToolTip.java \
javax/swing/JTree.java \
javax/swing/JViewport.java \
javax/swing/JWindow.java \
javax/swing/KeyStroke.java \
javax/swing/ListCellRenderer.java \
javax/swing/ListModel.java \
javax/swing/ListSelectionModel.java \
javax/swing/LookAndFeel.java \
javax/swing/Scrollable.java \
javax/swing/SwingConstants.java \
javax/swing/SwingUtilities.java \
javax/swing/Timer.java \
javax/swing/ToggleButtonModel.java \
javax/swing/UIDefaults.java \
javax/swing/UIManager.java \
javax/swing/UnsupportedLookAndFeelException.java \
javax/swing/event/AncestorEvent.java \
javax/swing/event/AncestorListener.java \
javax/swing/event/ChangeEvent.java \
javax/swing/event/ChangeListener.java \
javax/swing/event/DocumentEvent.java \
javax/swing/event/DocumentListener.java \
javax/swing/event/HyperlinkEvent.java \
javax/swing/event/HyperlinkListener.java \
javax/swing/event/ListDataEvent.java \
javax/swing/event/ListDataListener.java \
javax/swing/event/ListSelectionEvent.java \
javax/swing/event/ListSelectionListener.java \
javax/swing/event/MenuEvent.java \
javax/swing/event/UndoableEditEvent.java \
javax/swing/event/UndoableEditListener.java \
javax/swing/event/CaretEvent.java \
javax/swing/event/CaretListener.java \
javax/swing/event/CellEditorListener.java \
javax/swing/event/EventListenerList.java \
javax/swing/event/InternalFrameAdapter.java \
javax/swing/event/InternalFrameEvent.java \
javax/swing/event/InternalFrameListener.java \
javax/swing/event/MenuDragMouseEvent.java \
javax/swing/event/MenuDragMouseListener.java \
javax/swing/event/MenuKeyEvent.java \
javax/swing/event/MenuKeyListener.java \
javax/swing/event/MenuListener.java \
javax/swing/event/MouseInputAdapter.java \
javax/swing/event/MouseInputListener.java \
javax/swing/event/PopupMenuEvent.java \
javax/swing/event/PopupMenuListener.java \
javax/swing/event/SwingPropertyChangeSupport.java \
javax/swing/event/TableColumnModelEvent.java \
javax/swing/event/TableColumnModelListener.java \
javax/swing/event/TableModelEvent.java \
javax/swing/event/TableModelListener.java \
javax/swing/event/TreeExpansionEvent.java \
javax/swing/event/TreeExpansionListener.java \
javax/swing/event/TreeModelEvent.java \
javax/swing/event/TreeModelListener.java \
javax/swing/event/TreeSelectionEvent.java \
javax/swing/event/TreeSelectionListener.java \
javax/swing/event/TreeWillExpandListener.java \
javax/swing/ActionMap.java \
javax/swing/text/AbstractDocument.java \
javax/swing/text/AttributeSet.java \
javax/swing/text/BadLocationException.java \
javax/swing/text/Caret.java \
javax/swing/text/CharacterIterator.java \
javax/swing/text/ComponentView.java \
javax/swing/text/DefaultCaret.java \
javax/swing/text/DefaultEditorKit.java \
javax/swing/text/Document.java \
javax/swing/text/DocumentFilter.java \
javax/swing/text/EditorKit.java \
javax/swing/text/Element.java \
javax/swing/text/GapContent.java \
javax/swing/text/JTextComponent.java \
javax/swing/text/Keymap.java \
javax/swing/text/PlainDocument.java \
javax/swing/text/PlainEditorKit.java \
javax/swing/text/Position.java \
javax/swing/text/Segment.java \
javax/swing/text/Style.java \
javax/swing/text/View.java \
javax/swing/text/ViewFactory.java \
javax/swing/text/MutableAttributeSet.java \
javax/swing/text/NavigationFilter.java \
javax/swing/text/StyledDocument.java \
javax/swing/text/StyledEditorKit.java \
javax/swing/text/TextAction.java \
javax/swing/text/html/HTML.java \
javax/swing/text/html/parser/ParserDelegator.java \
javax/swing/tree/TreeNode.java \
javax/swing/tree/AbstractLayoutCache.java \
javax/swing/tree/DefaultMutableTreeNode.java \
javax/swing/tree/DefaultTreeCellEditor.java \
javax/swing/tree/DefaultTreeCellRenderer.java \
javax/swing/tree/DefaultTreeModel.java \
javax/swing/tree/DefaultTreeSelectionModel.java \
javax/swing/tree/ExpandVetoException.java \
javax/swing/tree/FixedHeightLayoutCache.java \
javax/swing/tree/MutableTreeNode.java \
javax/swing/tree/RowMapper.java \
javax/swing/tree/TreeCellEditor.java \
javax/swing/tree/TreeCellRenderer.java \
javax/swing/tree/TreeModel.java \
javax/swing/tree/TreePath.java \
javax/swing/tree/TreeSelectionModel.java \
javax/swing/tree/VariableHeightLayoutCache.java \
javax/swing/undo/UndoableEdit.java \
javax/swing/undo/AbstractUndoableEdit.java \
javax/swing/undo/CannotRedoException.java \
javax/swing/undo/CannotUndoException.java \
javax/swing/undo/CompoundEdit.java \
javax/swing/undo/StateEdit.java \
javax/swing/undo/StateEditable.java \
javax/swing/undo/UndoManager.java \
javax/swing/undo/UndoableEditSupport.java \
javax/swing/BoundedRangeModel.java \
javax/swing/CellEditor.java \
javax/swing/ComboBoxEditor.java \
javax/swing/ComboBoxModel.java \
javax/swing/DefaultBoundedRangeModel.java \
javax/swing/DesktopManager.java \
javax/swing/InputVerifier.java \
javax/swing/JInternalFrame.java \
javax/swing/JTable.java \
javax/swing/MenuElement.java \
javax/swing/MenuSelectionManager.java \
javax/swing/MutableComboBoxModel.java \
javax/swing/Renderer.java \
javax/swing/RootPaneContainer.java \
javax/swing/ScrollPaneConstants.java \
javax/swing/SingleSelectionModel.java \
javax/swing/SizeRequirements.java \
javax/swing/SizeSequence.java \
javax/swing/WindowConstants.java \
javax/swing/AbstractCellEditor.java \
javax/swing/CellRendererPane.java \
javax/swing/DebugGraphics.java \
javax/swing/DefaultCellEditor.java \
javax/swing/DefaultComboBoxModel.java \
javax/swing/DefaultDesktopManager.java \
javax/swing/DefaultFocusManager.java \
javax/swing/DefaultListCellRenderer.java \
javax/swing/DefaultSingleSelectionModel.java \
javax/swing/FocusManager.java \
javax/swing/JCheckBoxMenuItem.java \
javax/swing/JColorChooser.java \
javax/swing/JComboBox.java \
javax/swing/JDesktopPane.java \
javax/swing/JFileChooser.java \
javax/swing/JMenu.java \
javax/swing/JMenuItem.java \
javax/swing/JPasswordField.java \
javax/swing/JPopupMenu.java \
javax/swing/JProgressBar.java \
javax/swing/JRadioButtonMenuItem.java \
javax/swing/JSeparator.java \
javax/swing/JSlider.java \
javax/swing/JSplitPane.java \
javax/swing/JTextPane.java \
javax/swing/JToolBar.java \
javax/swing/OverlayLayout.java \
javax/swing/Popup.java \
javax/swing/PopupFactory.java \
javax/swing/ProgressMonitor.java \
javax/swing/ProgressMonitorInputStream.java \
javax/swing/RepaintManager.java \
javax/swing/ScrollPaneLayout.java \
javax/swing/ToolTipManager.java \
javax/swing/ViewportLayout.java

rmi_java_source_files =	\
java/rmi/activation/Activatable.java \
java/rmi/activation/ActivateFailedException.java \
java/rmi/activation/ActivationDesc.java	\
java/rmi/activation/ActivationException.java \
java/rmi/activation/ActivationGroup.java \
java/rmi/activation/ActivationGroupDesc.java \
java/rmi/activation/ActivationGroupID.java \
java/rmi/activation/ActivationID.java \
java/rmi/activation/ActivationInstantiator.java	\
java/rmi/activation/ActivationMonitor.java \
java/rmi/activation/ActivationSystem.java \
java/rmi/activation/Activator.java \
java/rmi/activation/UnknownGroupException.java \
java/rmi/activation/UnknownObjectException.java	\
java/rmi/AccessException.java \
java/rmi/AlreadyBoundException.java \
java/rmi/ConnectException.java \
java/rmi/ConnectIOException.java \
java/rmi/MarshalException.java \
java/rmi/MarshalledObject.java \
java/rmi/Naming.java \
java/rmi/NoSuchObjectException.java \
java/rmi/NotBoundException.java	\
java/rmi/RMISecurityException.java \
java/rmi/RMISecurityManager.java \
java/rmi/Remote.java \
java/rmi/RemoteException.java \
java/rmi/ServerError.java \
java/rmi/ServerException.java \
java/rmi/ServerRuntimeException.java \
java/rmi/StubNotFoundException.java \
java/rmi/UnexpectedException.java \
java/rmi/UnknownHostException.java \
java/rmi/UnmarshalException.java \
java/rmi/dgc/DGC.java \
java/rmi/dgc/Lease.java	\
java/rmi/dgc/VMID.java \
java/rmi/registry/LocateRegistry.java \
java/rmi/registry/Registry.java	\
java/rmi/registry/RegistryHandler.java \
java/rmi/server/ExportException.java \
java/rmi/server/LoaderHandler.java \
java/rmi/server/LogStream.java \
java/rmi/server/ObjID.java \
java/rmi/server/Operation.java \
java/rmi/server/RMIClassLoader.java \
java/rmi/server/RMIClassLoaderSpi.java \
java/rmi/server/RMIClientSocketFactory.java \
java/rmi/server/RMIFailureHandler.java \
java/rmi/server/RMIServerSocketFactory.java \
java/rmi/server/RMISocketFactory.java \
java/rmi/server/RemoteCall.java	\
java/rmi/server/RemoteObject.java \
java/rmi/server/RemoteRef.java \
java/rmi/server/RemoteServer.java \
java/rmi/server/RemoteStub.java	\
java/rmi/server/ServerCloneException.java \
java/rmi/server/ServerNotActiveException.java \
java/rmi/server/ServerRef.java \
java/rmi/server/Skeleton.java \
java/rmi/server/SkeletonMismatchException.java \
java/rmi/server/SkeletonNotFoundException.java \
java/rmi/server/SocketSecurityException.java \
java/rmi/server/UID.java \
java/rmi/server/UnicastRemoteObject.java \
java/rmi/server/Unreferenced.java \
gnu/java/rmi/RMIMarshalledObjectInputStream.java \
gnu/java/rmi/RMIMarshalledObjectOutputStream.java \
gnu/java/rmi/dgc/DGCImpl.java \
gnu/java/rmi/dgc/DGCImpl_Skel.java \
gnu/java/rmi/dgc/DGCImpl_Stub.java \
gnu/java/rmi/registry/RegistryImpl.java	\
gnu/java/rmi/registry/RegistryImpl_Skel.java \
gnu/java/rmi/registry/RegistryImpl_Stub.java \
gnu/java/rmi/rmic/Compile_gcj.java \
gnu/java/rmi/rmic/Compile_jikes.java \
gnu/java/rmi/rmic/Compile_kjc.java \
gnu/java/rmi/rmic/Compiler.java	\
gnu/java/rmi/rmic/CompilerProcess.java \
gnu/java/rmi/rmic/RMIC.java \
gnu/java/rmi/rmic/RMICException.java \
gnu/java/rmi/rmic/TabbedWriter.java \
gnu/java/rmi/server/ConnectionRunnerPool.java \
gnu/java/rmi/server/ProtocolConstants.java \
gnu/java/rmi/server/RMIDefaultSocketFactory.java \
gnu/java/rmi/server/RMIHashes.java \
gnu/java/rmi/server/RMIObjectInputStream.java \
gnu/java/rmi/server/RMIObjectOutputStream.java \
gnu/java/rmi/server/RMIVoidValue.java \
gnu/java/rmi/server/UnicastConnection.java \
gnu/java/rmi/server/UnicastConnectionManager.java \
gnu/java/rmi/server/UnicastRef.java \
gnu/java/rmi/server/UnicastRemoteCall.java \
gnu/java/rmi/server/UnicastRemoteStub.java \
gnu/java/rmi/server/UnicastServer.java \
gnu/java/rmi/server/UnicastServerRef.java

javax_source_files = \
javax/accessibility/Accessible.java \
javax/accessibility/AccessibleAction.java \
javax/accessibility/AccessibleBundle.java \
javax/accessibility/AccessibleComponent.java \
javax/accessibility/AccessibleContext.java \
javax/accessibility/AccessibleEditableText.java \
javax/accessibility/AccessibleExtendedComponent.java \
javax/accessibility/AccessibleExtendedTable.java \
javax/accessibility/AccessibleHyperlink.java \
javax/accessibility/AccessibleHypertext.java \
javax/accessibility/AccessibleIcon.java \
javax/accessibility/AccessibleKeyBinding.java \
javax/accessibility/AccessibleRelation.java \
javax/accessibility/AccessibleRelationSet.java \
javax/accessibility/AccessibleResourceBundle.java \
javax/accessibility/AccessibleRole.java \
javax/accessibility/AccessibleSelection.java \
javax/accessibility/AccessibleState.java \
javax/accessibility/AccessibleStateSet.java \
javax/accessibility/AccessibleTable.java \
javax/accessibility/AccessibleTableModelChange.java \
javax/accessibility/AccessibleText.java \
javax/accessibility/AccessibleValue.java \
javax/naming/directory/Attribute.java \
javax/naming/directory/AttributeInUseException.java \
javax/naming/directory/AttributeModificationException.java \
javax/naming/directory/Attributes.java \
javax/naming/directory/DirContext.java \
javax/naming/directory/InvalidAttributeIdentifierException.java	\
javax/naming/directory/InvalidAttributeValueException.java \
javax/naming/directory/InvalidAttributesException.java \
javax/naming/directory/InvalidSearchControlsException.java \
javax/naming/directory/InvalidSearchFilterException.java \
javax/naming/directory/NoSuchAttributeException.java \
javax/naming/directory/SchemaViolationException.java \
javax/naming/directory/BasicAttribute.java \
javax/naming/directory/BasicAttributes.java \
javax/naming/directory/ModificationItem.java \
javax/naming/directory/SearchControls.java \
javax/naming/directory/SearchResult.java \
javax/naming/directory/InitialDirContext.java \
javax/naming/AuthenticationException.java \
javax/naming/AuthenticationNotSupportedException.java \
javax/naming/CannotProceedException.java \
javax/naming/CommunicationException.java \
javax/naming/ConfigurationException.java \
javax/naming/Context.java \
javax/naming/ContextNotEmptyException.java \
javax/naming/InsufficientResourcesException.java \
javax/naming/InterruptedNamingException.java \
javax/naming/BinaryRefAddr.java	\
javax/naming/LimitExceededException.java \
javax/naming/LinkException.java	\
javax/naming/LinkLoopException.java \
javax/naming/MalformedLinkException.java \
javax/naming/Name.java \
javax/naming/NameAlreadyBoundException.java \
javax/naming/NameNotFoundException.java	\
javax/naming/NameParser.java \
javax/naming/NamingEnumeration.java \
javax/naming/InitialContext.java \
javax/naming/NamingSecurityException.java \
javax/naming/NoInitialContextException.java \
javax/naming/NoPermissionException.java	\
javax/naming/NotContextException.java \
javax/naming/NamingException.java \
javax/naming/PartialResultException.java \
javax/naming/event/EventContext.java \
javax/naming/event/EventDirContext.java	\
javax/naming/event/NamespaceChangeListener.java	\
javax/naming/event/NamingEvent.java \
javax/naming/event/NamingExceptionEvent.java \
javax/naming/event/NamingListener.java \
javax/naming/event/ObjectChangeListener.java \
javax/naming/RefAddr.java \
javax/naming/Reference.java \
javax/naming/Referenceable.java	\
javax/naming/ReferralException.java \
javax/naming/ServiceUnavailableException.java \
javax/naming/SizeLimitExceededException.java \
javax/naming/StringRefAddr.java	\
javax/naming/TimeLimitExceededException.java \
javax/naming/spi/InitialContextFactory.java \
javax/naming/spi/InitialContextFactoryBuilder.java \
javax/naming/spi/NamingManager.java \
javax/naming/spi/ObjectFactory.java \
javax/naming/spi/Resolver.java \
javax/naming/spi/DirObjectFactory.java \
javax/naming/spi/DirStateFactory.java \
javax/naming/spi/DirectoryManager.java \
javax/naming/spi/ObjectFactoryBuilder.java \
javax/naming/spi/ResolveResult.java \
javax/naming/spi/StateFactory.java \
javax/naming/InvalidNameException.java \
javax/naming/Binding.java \
javax/naming/CompositeName.java	\
javax/naming/CompoundName.java \
javax/naming/LinkRef.java \
javax/naming/NameClassPair.java	\
javax/naming/ldap/Control.java \
javax/naming/ldap/ControlFactory.java \
javax/naming/ldap/ExtendedRequest.java \
javax/naming/ldap/ExtendedResponse.java	\
javax/naming/ldap/HasControls.java \
javax/naming/ldap/InitialLdapContext.java \
javax/naming/ldap/LdapContext.java \
javax/naming/ldap/LdapReferralException.java \
javax/naming/ldap/UnsolicitedNotification.java \
javax/naming/ldap/UnsolicitedNotificationEvent.java \
javax/naming/ldap/UnsolicitedNotificationListener.java \
javax/naming/OperationNotSupportedException.java \
javax/print/attribute/Attribute.java \
javax/print/attribute/AttributeSet.java \
javax/print/attribute/AttributeSetUtilities.java \
javax/print/attribute/DateTimeSyntax.java \
javax/print/attribute/DocAttribute.java \
javax/print/attribute/DocAttributeSet.java \
javax/print/attribute/EnumSyntax.java \
javax/print/attribute/HashAttributeSet.java \
javax/print/attribute/HashDocAttributeSet.java \
javax/print/attribute/HashPrintJobAttributeSet.java \
javax/print/attribute/HashPrintRequestAttributeSet.java \
javax/print/attribute/HashPrintServiceAttributeSet.java \
javax/print/attribute/IntegerSyntax.java \
javax/print/attribute/PrintJobAttribute.java \
javax/print/attribute/PrintJobAttributeSet.java \
javax/print/attribute/PrintRequestAttribute.java \
javax/print/attribute/PrintRequestAttributeSet.java \
javax/print/attribute/PrintServiceAttribute.java \
javax/print/attribute/PrintServiceAttributeSet.java \
javax/print/attribute/ResolutionSyntax.java \
javax/print/attribute/SetOfIntegerSyntax.java \
javax/print/attribute/Size2DSyntax.java \
javax/print/attribute/SupportedValuesAttribute.java \
javax/print/attribute/TextSyntax.java \
javax/print/attribute/UnmodifiableSetException.java \
javax/print/attribute/URISyntax.java \
javax/print/attribute/standard/Copies.java \
javax/print/attribute/standard/DateTimeAtCompleted.java \
javax/print/attribute/standard/DateTimeAtCreation.java \
javax/print/attribute/standard/DateTimeAtProcessing.java \
javax/print/attribute/standard/DocumentName.java \
javax/print/attribute/standard/JobHoldUntil.java \
javax/print/attribute/standard/JobImpressions.java \
javax/print/attribute/standard/JobImpressionsCompleted.java \
javax/print/attribute/standard/JobKOctets.java \
javax/print/attribute/standard/JobKOctetsProcessed.java \
javax/print/attribute/standard/JobMediaSheets.java \
javax/print/attribute/standard/JobMediaSheetsCompleted.java \
javax/print/attribute/standard/JobMessageFromOperator.java \
javax/print/attribute/standard/JobName.java \
javax/print/attribute/standard/JobOriginatingUserName.java \
javax/print/attribute/standard/JobPriority.java \
javax/print/attribute/standard/JobPrioritySupported.java \
javax/print/attribute/standard/NumberOfDocuments.java \
javax/print/attribute/standard/NumberOfInterveningJobs.java \
javax/print/attribute/standard/NumberUp.java \
javax/print/attribute/standard/OutputDeviceAssigned.java \
javax/print/attribute/standard/PagesPerMinute.java \
javax/print/attribute/standard/PagesPerMinuteColor.java \
javax/print/attribute/standard/PrinterInfo.java \
javax/print/attribute/standard/PrinterLocation.java \
javax/print/attribute/standard/PrinterMakeAndModel.java \
javax/print/attribute/standard/PrinterMessageFromOperator.java \
javax/print/attribute/standard/PrinterName.java \
javax/print/attribute/standard/QueuedJobCount.java \
javax/print/attribute/standard/RequestingUserName.java \
javax/security/auth/x500/X500Principal.java \
javax/sql/ConnectionEvent.java \
javax/sql/ConnectionEventListener.java \
javax/sql/ConnectionPoolDataSource.java \
javax/sql/DataSource.java \
javax/sql/PooledConnection.java \
javax/sql/RowSetEvent.java \
javax/sql/RowSetInternal.java \
javax/sql/RowSet.java \
javax/sql/RowSetListener.java \
javax/sql/RowSetMetaData.java \
javax/sql/RowSetReader.java \
javax/sql/RowSetWriter.java \
javax/sql/XAConnection.java \
javax/sql/XADataSource.java \
javax/transaction/xa/XAException.java \
javax/transaction/xa/XAResource.java \
javax/transaction/xa/Xid.java \
javax/transaction/HeuristicCommitException.java	\
javax/transaction/HeuristicMixedException.java \
javax/transaction/HeuristicRollbackException.java \
javax/transaction/InvalidTransactionException.java \
javax/transaction/NotSupportedException.java \
javax/transaction/RollbackException.java \
javax/transaction/Status.java \
javax/transaction/Synchronization.java \
javax/transaction/SystemException.java \
javax/transaction/Transaction.java \
javax/transaction/TransactionManager.java \
javax/transaction/TransactionRequiredException.java \
javax/transaction/TransactionRolledbackException.java \
javax/transaction/UserTransaction.java


## Java files which are created by configure and thus are in the build
## directory.
built_java_source_files = java/lang/ConcreteProcess.java \
                          gnu/classpath/Configuration.java

## Java files in the core packages java.lang, java.io, and java.util.
## These are built before the other source files, in order to reduce 
## build time.
core_java_source_files = \
java/lang/AbstractMethodError.java \
java/lang/ArithmeticException.java \
java/lang/ArrayIndexOutOfBoundsException.java \
java/lang/ArrayStoreException.java \
java/lang/AssertionError.java \
java/lang/Boolean.java \
java/lang/Byte.java \
java/lang/Character.java \
java/lang/CharSequence.java \
java/lang/ClassCastException.java \
java/lang/ClassCircularityError.java \
java/lang/ClassFormatError.java	\
java/lang/ClassLoader.java \
java/lang/ClassNotFoundException.java \
java/lang/CloneNotSupportedException.java \
java/lang/Cloneable.java \
java/lang/Comparable.java \
java/lang/Compiler.java	\
java/lang/Double.java \
java/lang/Error.java \
java/lang/Exception.java \
java/lang/ExceptionInInitializerError.java \
java/lang/Float.java \
java/lang/IllegalAccessError.java \
java/lang/IllegalAccessException.java \
java/lang/IllegalArgumentException.java	\
java/lang/IllegalMonitorStateException.java \
java/lang/IllegalStateException.java \
java/lang/IllegalThreadStateException.java \
java/lang/IncompatibleClassChangeError.java \
java/lang/IndexOutOfBoundsException.java \
java/lang/InheritableThreadLocal.java \
java/lang/InstantiationError.java \
java/lang/InstantiationException.java \
java/lang/Integer.java \
java/lang/InternalError.java \
java/lang/InterruptedException.java \
java/lang/LinkageError.java \
java/lang/Long.java \
java/lang/Math.java \
java/lang/NegativeArraySizeException.java \
java/lang/NoClassDefFoundError.java \
java/lang/NoSuchFieldError.java	\
java/lang/NoSuchFieldException.java \
java/lang/NoSuchMethodError.java \
java/lang/NoSuchMethodException.java \
java/lang/NullPointerException.java \
java/lang/Number.java \
java/lang/NumberFormatException.java \
java/lang/OutOfMemoryError.java	\
java/lang/Package.java \
java/lang/Process.java \
java/lang/Runnable.java	\
java/lang/Runtime.java \
java/lang/RuntimeException.java	\
java/lang/RuntimePermission.java \
java/lang/SecurityException.java \
java/lang/SecurityManager.java \
java/lang/Short.java \
java/lang/StackOverflowError.java \
java/lang/StackTraceElement.java \
java/lang/StrictMath.java \
java/lang/String.java \
java/lang/StringBuffer.java \
java/lang/StringIndexOutOfBoundsException.java \
java/lang/System.java \
java/lang/Thread.java \
java/lang/ThreadDeath.java \
java/lang/ThreadGroup.java \
java/lang/ThreadLocal.java \
java/lang/Throwable.java \
java/lang/UnknownError.java \
java/lang/UnsatisfiedLinkError.java \
java/lang/UnsupportedClassVersionError.java \
java/lang/UnsupportedOperationException.java \
java/lang/VerifyError.java \
java/lang/VirtualMachineError.java \
java/lang/VMClassLoader.java \
java/lang/VMSecurityManager.java \
java/lang/VMThrowable.java \
java/lang/Void.java \
java/io/BufferedInputStream.java \
java/io/BufferedOutputStream.java \
java/io/BufferedReader.java \
java/io/BufferedWriter.java \
java/io/ByteArrayInputStream.java \
java/io/ByteArrayOutputStream.java \
java/io/CharArrayReader.java \
java/io/CharArrayWriter.java \
java/io/CharConversionException.java \
java/io/DataInput.java \
java/io/DataInputStream.java \
java/io/DataOutput.java	\
java/io/DataOutputStream.java \
java/io/EOFException.java \
java/io/Externalizable.java \
java/io/File.java \
java/io/FileDescriptor.java \
java/io/FileFilter.java \
java/io/FileInputStream.java \
java/io/FileNotFoundException.java \
java/io/FileOutputStream.java \
java/io/FilePermission.java \
java/io/FileReader.java	\
java/io/FileWriter.java	\
java/io/FilenameFilter.java \
java/io/FilterInputStream.java \
java/io/FilterOutputStream.java	\
java/io/FilterReader.java \
java/io/FilterWriter.java \
java/io/IOException.java \
java/io/InputStream.java \
java/io/InputStreamReader.java \
java/io/InterruptedIOException.java \
java/io/InvalidClassException.java \
java/io/InvalidObjectException.java \
java/io/LineNumberInputStream.java \
java/io/LineNumberReader.java \
java/io/NotActiveException.java	\
java/io/NotSerializableException.java \
java/io/ObjectInput.java \
java/io/ObjectInputStream.java \
java/io/ObjectInputValidation.java \
java/io/ObjectOutput.java \
java/io/ObjectOutputStream.java	\
java/io/ObjectStreamClass.java \
java/io/ObjectStreamConstants.java \
java/io/ObjectStreamException.java \
java/io/ObjectStreamField.java \
java/io/OptionalDataException.java \
java/io/OutputStream.java \
java/io/OutputStreamWriter.java	\
java/io/PipedInputStream.java \
java/io/PipedOutputStream.java \
java/io/PipedReader.java \
java/io/PipedWriter.java \
java/io/PrintStream.java \
java/io/PrintWriter.java \
java/io/PushbackInputStream.java \
java/io/PushbackReader.java \
java/io/RandomAccessFile.java \
java/io/Reader.java \
java/io/SequenceInputStream.java \
java/io/Serializable.java \
java/io/SerializablePermission.java \
java/io/StreamCorruptedException.java \
java/io/StreamTokenizer.java \
java/io/StringBufferInputStream.java \
java/io/StringReader.java \
java/io/StringWriter.java \
java/io/SyncFailedException.java \
java/io/UTFDataFormatException.java \
java/io/UnsupportedEncodingException.java \
java/io/VMObjectStreamClass.java \
java/io/WriteAbortedException.java \
java/io/Writer.java \
java/util/AbstractCollection.java \
java/util/AbstractList.java \
java/util/AbstractMap.java \
java/util/AbstractSequentialList.java \
java/util/AbstractSet.java \
java/util/ArrayList.java \
java/util/Arrays.java \
java/util/BitSet.java \
java/util/Calendar.java	\
java/util/Collection.java \
java/util/Collections.java \
java/util/Comparator.java \
java/util/ConcurrentModificationException.java \
java/util/Currency.java \
java/util/Date.java \
java/util/Dictionary.java \
java/util/EmptyStackException.java \
java/util/Enumeration.java \
java/util/EventListener.java \
java/util/EventListenerProxy.java \
java/util/EventObject.java \
java/util/GregorianCalendar.java \
java/util/HashMap.java \
java/util/HashSet.java \
java/util/Hashtable.java \
java/util/IdentityHashMap.java \
java/util/Iterator.java	\
java/util/LinkedList.java \
java/util/LinkedHashMap.java \
java/util/LinkedHashSet.java \
java/util/List.java \
java/util/ListIterator.java \
java/util/ListResourceBundle.java \
java/util/Locale.java \
java/util/Map.java \
java/util/MissingResourceException.java	\
java/util/NoSuchElementException.java \
java/util/Observable.java \
java/util/Observer.java	\
java/util/Properties.java \
java/util/PropertyPermission.java \
java/util/PropertyPermissionCollection.java \
java/util/PropertyResourceBundle.java \
java/util/Random.java \
java/util/RandomAccess.java \
java/util/ResourceBundle.java \
java/util/Set.java \
java/util/SimpleTimeZone.java \
java/util/SortedMap.java \
java/util/SortedSet.java \
java/util/Stack.java \
java/util/StringTokenizer.java \
java/util/TimeZone.java	\
java/util/Timer.java \
java/util/TimerTask.java \
java/util/TreeMap.java \
java/util/TreeSet.java \
java/util/TooManyListenersException.java \
java/util/Vector.java \
java/util/WeakHashMap.java \
java/util/logging/ConsoleHandler.java \
java/util/logging/ErrorManager.java \
java/util/logging/FileHandler.java \
java/util/logging/Filter.java \
java/util/logging/Formatter.java \
java/util/logging/Handler.java \
java/util/logging/Level.java \
java/util/logging/Logger.java \
java/util/logging/LoggingPermission.java \
java/util/logging/LogManager.java \
java/util/logging/LogRecord.java \
java/util/logging/MemoryHandler.java \
java/util/logging/SimpleFormatter.java \
java/util/logging/SocketHandler.java \
java/util/logging/StreamHandler.java \
java/util/logging/XMLFormatter.java \
java/util/prefs/NodeChangeListener.java \
java/util/prefs/Preferences.java \
java/util/prefs/PreferenceChangeListener.java \
java/util/prefs/NodeChangeEvent.java \
java/util/prefs/InvalidPreferencesFormatException.java \
java/util/prefs/AbstractPreferences.java \
java/util/prefs/BackingStoreException.java \
java/util/prefs/PreferenceChangeEvent.java \
java/util/prefs/PreferencesFactory.java \
java/util/regex/Matcher.java \
java/util/regex/Pattern.java \
java/util/regex/PatternSyntaxException.java

## List of all .java files to be compiled.  Please keep this list
## alphabetical.  Please put files from java/lang, java/io, and java/util
## into core_java_source_files. Please put files from gnu/gcj/convert 
## into convert_source_files and files from java/awt into
## awt_java_source_files.  If the .java file has a hand-maintained
## header, please list it in special_java_source_files.
ordinary_java_source_files = $(core_java_source_files) \
gnu/gcj/Core.java \
gnu/gcj/RawData.java \
gnu/gcj/io/DefaultMimeTypes.java \
gnu/gcj/io/MimeTypes.java \
gnu/gcj/io/SimpleSHSStream.java	\
gnu/gcj/runtime/FileDeleter.java \
gnu/gcj/runtime/FinalizerThread.java \
gnu/gcj/runtime/FirstThread.java \
gnu/gcj/runtime/JNIWeakRef.java \
gnu/gcj/runtime/MethodRef.java \
gnu/gcj/runtime/NameFinder.java \
gnu/gcj/runtime/SharedLibHelper.java \
gnu/gcj/runtime/SharedLibLoader.java \
gnu/gcj/runtime/StackTrace.java \
gnu/gcj/runtime/StringBuffer.java \
gnu/gcj/runtime/VMClassLoader.java \
gnu/java/io/ASN1ParsingException.java \
gnu/java/io/Base64InputStream.java \
gnu/java/io/ClassLoaderObjectInputStream.java \
gnu/java/io/NullOutputStream.java \
gnu/java/io/ObjectIdentityWrapper.java \
gnu/java/lang/ArrayHelper.java \
gnu/java/lang/ClassHelper.java \
gnu/java/lang/CharData.java \
gnu/java/lang/reflect/TypeSignature.java \
gnu/java/locale/Calendar.java \
gnu/java/locale/Calendar_de.java \
gnu/java/locale/Calendar_en.java \
gnu/java/locale/Calendar_nl.java \
gnu/java/locale/LocaleInformation.java \
gnu/java/locale/LocaleInformation_af_ZA.java \
gnu/java/locale/LocaleInformation_ar_AE.java \
gnu/java/locale/LocaleInformation_ar_BH.java \
gnu/java/locale/LocaleInformation_ar_DZ.java \
gnu/java/locale/LocaleInformation_ar_EG.java \
gnu/java/locale/LocaleInformation_ar_IN.java \
gnu/java/locale/LocaleInformation_ar_IQ.java \
gnu/java/locale/LocaleInformation_ar_JO.java \
gnu/java/locale/LocaleInformation_ar_KW.java \
gnu/java/locale/LocaleInformation_ar_LB.java \
gnu/java/locale/LocaleInformation_ar_LY.java \
gnu/java/locale/LocaleInformation_ar_MA.java \
gnu/java/locale/LocaleInformation_ar_OM.java \
gnu/java/locale/LocaleInformation_ar_QA.java \
gnu/java/locale/LocaleInformation_ar_SD.java \
gnu/java/locale/LocaleInformation_ar_SY.java \
gnu/java/locale/LocaleInformation_ar_TN.java \
gnu/java/locale/LocaleInformation_ar_YE.java \
gnu/java/locale/LocaleInformation_be_BY.java \
gnu/java/locale/LocaleInformation_bn_IN.java \
gnu/java/locale/LocaleInformation_br_FR.java \
gnu/java/locale/LocaleInformation_bs_BA.java \
gnu/java/locale/LocaleInformation_ca_ES.java \
gnu/java/locale/LocaleInformation_cs_CZ.java \
gnu/java/locale/LocaleInformation_cy_GB.java \
gnu/java/locale/LocaleInformation_da_DK.java \
gnu/java/locale/LocaleInformation_de.java \
gnu/java/locale/LocaleInformation_de_AT.java \
gnu/java/locale/LocaleInformation_de_BE.java \
gnu/java/locale/LocaleInformation_de_CH.java \
gnu/java/locale/LocaleInformation_de_DE.java \
gnu/java/locale/LocaleInformation_de_LU.java \
gnu/java/locale/LocaleInformation_el_GR.java \
gnu/java/locale/LocaleInformation_en.java \
gnu/java/locale/LocaleInformation_en_AU.java \
gnu/java/locale/LocaleInformation_en_BW.java \
gnu/java/locale/LocaleInformation_en_CA.java \
gnu/java/locale/LocaleInformation_en_DK.java \
gnu/java/locale/LocaleInformation_en_GB.java \
gnu/java/locale/LocaleInformation_en_HK.java \
gnu/java/locale/LocaleInformation_en_IE.java \
gnu/java/locale/LocaleInformation_en_IN.java \
gnu/java/locale/LocaleInformation_en_NZ.java \
gnu/java/locale/LocaleInformation_en_PH.java \
gnu/java/locale/LocaleInformation_en_SG.java \
gnu/java/locale/LocaleInformation_en_US.java \
gnu/java/locale/LocaleInformation_en_ZA.java \
gnu/java/locale/LocaleInformation_en_ZW.java \
gnu/java/locale/LocaleInformation_es_AR.java \
gnu/java/locale/LocaleInformation_es_BO.java \
gnu/java/locale/LocaleInformation_es_CL.java \
gnu/java/locale/LocaleInformation_es_CO.java \
gnu/java/locale/LocaleInformation_es_CR.java \
gnu/java/locale/LocaleInformation_es_DO.java \
gnu/java/locale/LocaleInformation_es_EC.java \
gnu/java/locale/LocaleInformation_es_ES.java \
gnu/java/locale/LocaleInformation_es_GT.java \
gnu/java/locale/LocaleInformation_es_HN.java \
gnu/java/locale/LocaleInformation_es_MX.java \
gnu/java/locale/LocaleInformation_es_NI.java \
gnu/java/locale/LocaleInformation_es_PA.java \
gnu/java/locale/LocaleInformation_es_PE.java \
gnu/java/locale/LocaleInformation_es_PR.java \
gnu/java/locale/LocaleInformation_es_PY.java \
gnu/java/locale/LocaleInformation_es_SV.java \
gnu/java/locale/LocaleInformation_es_US.java \
gnu/java/locale/LocaleInformation_es_UY.java \
gnu/java/locale/LocaleInformation_es_VE.java \
gnu/java/locale/LocaleInformation_et_EE.java \
gnu/java/locale/LocaleInformation_eu_ES.java \
gnu/java/locale/LocaleInformation_fa_IR.java \
gnu/java/locale/LocaleInformation_fi_FI.java \
gnu/java/locale/LocaleInformation_fo_FO.java \
gnu/java/locale/LocaleInformation_fr_BE.java \
gnu/java/locale/LocaleInformation_fr_CA.java \
gnu/java/locale/LocaleInformation_fr_CH.java \
gnu/java/locale/LocaleInformation_fr_FR.java \
gnu/java/locale/LocaleInformation_fr_LU.java \
gnu/java/locale/LocaleInformation_ga_IE.java \
gnu/java/locale/LocaleInformation_gd_GB.java \
gnu/java/locale/LocaleInformation_gl_ES.java \
gnu/java/locale/LocaleInformation_gv_GB.java \
gnu/java/locale/LocaleInformation_he_IL.java \
gnu/java/locale/LocaleInformation_hi_IN.java \
gnu/java/locale/LocaleInformation_hr_HR.java \
gnu/java/locale/LocaleInformation_hu_HU.java \
gnu/java/locale/LocaleInformation_id_ID.java \
gnu/java/locale/LocaleInformation_it_CH.java \
gnu/java/locale/LocaleInformation_it_IT.java \
gnu/java/locale/LocaleInformation_iw_IL.java \
gnu/java/locale/LocaleInformation_ja_JP.java \
gnu/java/locale/LocaleInformation_ka_GE.java \
gnu/java/locale/LocaleInformation_kl_GL.java \
gnu/java/locale/LocaleInformation_ko_KR.java \
gnu/java/locale/LocaleInformation_kw_GB.java \
gnu/java/locale/LocaleInformation_lt_LT.java \
gnu/java/locale/LocaleInformation_lv_LV.java \
gnu/java/locale/LocaleInformation_mi_NZ.java \
gnu/java/locale/LocaleInformation_mk_MK.java \
gnu/java/locale/LocaleInformation_mr_IN.java \
gnu/java/locale/LocaleInformation_mt_MT.java \
gnu/java/locale/LocaleInformation_nl.java \
gnu/java/locale/LocaleInformation_nl_BE.java \
gnu/java/locale/LocaleInformation_nl_NL.java \
gnu/java/locale/LocaleInformation_nn_NO.java \
gnu/java/locale/LocaleInformation_no_NO.java \
gnu/java/locale/LocaleInformation_oc_FR.java \
gnu/java/locale/LocaleInformation_pl_PL.java \
gnu/java/locale/LocaleInformation_pt_BR.java \
gnu/java/locale/LocaleInformation_pt_PT.java \
gnu/java/locale/LocaleInformation_ro_RO.java \
gnu/java/locale/LocaleInformation_ru_RU.java \
gnu/java/locale/LocaleInformation_ru_UA.java \
gnu/java/locale/LocaleInformation_se_NO.java \
gnu/java/locale/LocaleInformation_sk_SK.java \
gnu/java/locale/LocaleInformation_sl_SI.java \
gnu/java/locale/LocaleInformation_sq_AL.java \
gnu/java/locale/LocaleInformation_sr_YU.java \
gnu/java/locale/LocaleInformation_sv_FI.java \
gnu/java/locale/LocaleInformation_sv_SE.java \
gnu/java/locale/LocaleInformation_ta_IN.java \
gnu/java/locale/LocaleInformation_te_IN.java \
gnu/java/locale/LocaleInformation_tg_TJ.java \
gnu/java/locale/LocaleInformation_tl_PH.java \
gnu/java/locale/LocaleInformation_tr_TR.java \
gnu/java/locale/LocaleInformation_uk_UA.java \
gnu/java/locale/LocaleInformation_ur_PK.java \
gnu/java/locale/LocaleInformation_uz_UZ.java \
gnu/java/locale/LocaleInformation_vi_VN.java \
gnu/java/locale/LocaleInformation_yi_US.java \
gnu/java/locale/LocaleInformation_zh_CN.java \
gnu/java/locale/LocaleInformation_zh_HK.java \
gnu/java/locale/LocaleInformation_zh_SG.java \
gnu/java/locale/LocaleInformation_zh_TW.java \
gnu/java/math/MPN.java \
gnu/java/net/HeaderFieldHelper.java \
gnu/java/net/PlainDatagramSocketImpl.java \
gnu/java/net/PlainSocketImpl.java \
gnu/java/net/URLParseError.java \
gnu/java/net/protocol/core/Connection.java \
gnu/java/net/protocol/core/Handler.java \
gnu/java/net/protocol/core/CoreInputStream.java \
gnu/java/net/protocol/file/Connection.java \
gnu/java/net/protocol/file/Handler.java \
gnu/java/net/protocol/http/Connection.java \
gnu/java/net/protocol/http/Handler.java \
gnu/java/net/protocol/jar/Connection.java \
gnu/java/net/protocol/jar/Handler.java \
gnu/java/net/protocol/gcjlib/Connection.java \
gnu/java/net/protocol/gcjlib/Handler.java \
gnu/java/nio/DatagramChannelImpl.java \
gnu/java/nio/DatagramChannelSelectionKey.java \
gnu/java/nio/FileLockImpl.java \
gnu/java/nio/NIOConstants.java \
gnu/java/nio/NIODatagramSocket.java \
gnu/java/nio/NIOServerSocket.java \
gnu/java/nio/NIOSocket.java \
gnu/java/nio/ChannelInputStream.java \
gnu/java/nio/ChannelOutputStream.java \
gnu/java/nio/InputStreamChannel.java \
gnu/java/nio/OutputStreamChannel.java \
gnu/java/nio/PipeImpl.java \
gnu/java/nio/SelectionKeyImpl.java \
gnu/java/nio/SelectorImpl.java \
gnu/java/nio/SelectorProviderImpl.java \
gnu/java/nio/ServerSocketChannelImpl.java \
gnu/java/nio/ServerSocketChannelSelectionKey.java \
gnu/java/nio/SocketChannelImpl.java \
gnu/java/nio/SocketChannelSelectionKey.java \
gnu/java/nio/charset/ISO_8859_1.java \
gnu/java/nio/charset/Provider.java \
gnu/java/nio/charset/US_ASCII.java \
gnu/java/nio/charset/UTF_16.java \
gnu/java/nio/charset/UTF_16BE.java \
gnu/java/nio/charset/UTF_16Decoder.java \
gnu/java/nio/charset/UTF_16Encoder.java \
gnu/java/nio/charset/UTF_16LE.java \
gnu/java/nio/charset/UTF_8.java \
gnu/java/security/Engine.java \
gnu/java/security/OID.java \
gnu/java/security/der/BitString.java \
gnu/java/security/der/DER.java \
gnu/java/security/der/DEREncodingException.java \
gnu/java/security/der/DERReader.java \
gnu/java/security/der/DERValue.java \
gnu/java/security/der/DERWriter.java \
gnu/java/security/provider/DefaultPolicy.java \
gnu/java/security/provider/DSAKeyFactory.java \
gnu/java/security/provider/DSAKeyPairGenerator.java \
gnu/java/security/provider/DSAParameterGenerator.java \
gnu/java/security/provider/DSAParameters.java \
gnu/java/security/provider/DSASignature.java \
gnu/java/security/provider/Gnu.java \
gnu/java/security/provider/GnuDSAPrivateKey.java \
gnu/java/security/provider/GnuDSAPublicKey.java \
gnu/java/security/provider/MD5.java \
gnu/java/security/provider/SHA.java \
gnu/java/security/provider/SHA1PRNG.java \
gnu/java/security/provider/X509CertificateFactory.java \
gnu/java/security/util/Prime.java \
gnu/java/security/x509/X500DistinguishedName.java \
gnu/java/security/x509/X509Certificate.java \
gnu/java/security/x509/X509CRLEntry.java \
gnu/java/security/x509/X509CRL.java \
gnu/java/text/BaseBreakIterator.java \
gnu/java/text/CharacterBreakIterator.java \
gnu/java/text/LineBreakIterator.java \
gnu/java/text/SentenceBreakIterator.java \
gnu/java/text/WordBreakIterator.java \
gnu/java/util/DoubleEnumeration.java \
gnu/java/util/EmptyEnumeration.java \
gnu/java/util/prefs/MemoryBasedFactory.java \
gnu/java/util/prefs/NodeReader.java \
gnu/java/util/prefs/MemoryBasedPreferences.java \
gnu/java/util/prefs/FileBasedFactory.java \
gnu/java/util/prefs/NodeWriter.java \
java/lang/ref/PhantomReference.java \
java/lang/ref/Reference.java \
java/lang/ref/ReferenceQueue.java \
java/lang/ref/SoftReference.java \
java/lang/ref/WeakReference.java \
java/lang/reflect/AccessibleObject.java	\
java/lang/reflect/Array.java \
java/lang/reflect/Constructor.java \
java/lang/reflect/Field.java \
java/lang/reflect/InvocationHandler.java \
java/lang/reflect/InvocationTargetException.java \
java/lang/reflect/Member.java \
java/lang/reflect/Method.java \
java/lang/reflect/Modifier.java	\
java/lang/reflect/Proxy.java \
java/lang/reflect/ReflectPermission.java \
java/lang/reflect/UndeclaredThrowableException.java \
java/math/BigDecimal.java \
java/math/BigInteger.java \
java/net/Authenticator.java \
java/net/BindException.java \
java/net/ConnectException.java \
java/net/ContentHandler.java \
java/net/ContentHandlerFactory.java \
java/net/DatagramPacket.java \
java/net/DatagramSocket.java \
java/net/DatagramSocketImpl.java \
java/net/DatagramSocketImplFactory.java \
java/net/FileNameMap.java \
java/net/HttpURLConnection.java	\
java/net/InetAddress.java \
java/net/Inet4Address.java \
java/net/Inet6Address.java \
java/net/InetSocketAddress.java \
java/net/JarURLConnection.java \
java/net/MalformedURLException.java \
java/net/MulticastSocket.java \
java/net/NetPermission.java \
java/net/NetworkInterface.java \
java/net/NoRouteToHostException.java \
java/net/PasswordAuthentication.java \
java/net/PortUnreachableException.java \
java/net/ProtocolException.java	\
java/net/ServerSocket.java \
java/net/Socket.java \
java/net/SocketAddress.java \
java/net/SocketException.java \
java/net/SocketImpl.java \
java/net/SocketImplFactory.java	\
java/net/SocketOptions.java \
java/net/SocketPermission.java \
java/net/SocketTimeoutException.java \
java/net/URI.java \
java/net/URISyntaxException.java \
java/net/URL.java \
java/net/URLClassLoader.java \
java/net/URLConnection.java \
java/net/URLDecoder.java \
java/net/URLEncoder.java \
java/net/URLStreamHandler.java \
java/net/URLStreamHandlerFactory.java \
java/net/UnknownHostException.java \
java/net/UnknownServiceException.java \
java/nio/Buffer.java \
java/nio/BufferOverflowException.java \
java/nio/BufferUnderflowException.java \
java/nio/ByteBuffer.java \
java/nio/ByteBufferHelper.java \
java/nio/ByteBufferImpl.java \
java/nio/ByteOrder.java \
java/nio/CharBuffer.java \
java/nio/CharBufferImpl.java \
java/nio/CharViewBufferImpl.java \
java/nio/DirectByteBufferImpl.java \
java/nio/DoubleBuffer.java \
java/nio/DoubleBufferImpl.java \
java/nio/DoubleViewBufferImpl.java \
java/nio/FloatBuffer.java \
java/nio/FloatBufferImpl.java \
java/nio/FloatViewBufferImpl.java \
java/nio/IntBuffer.java \
java/nio/IntBufferImpl.java \
java/nio/IntViewBufferImpl.java \
java/nio/InvalidMarkException.java \
java/nio/LongBuffer.java \
java/nio/LongBufferImpl.java \
java/nio/LongViewBufferImpl.java \
java/nio/MappedByteBuffer.java \
java/nio/MappedByteBufferImpl.java \
java/nio/ReadOnlyBufferException.java \
java/nio/ShortBuffer.java \
java/nio/ShortBufferImpl.java \
java/nio/ShortViewBufferImpl.java \
java/nio/channels/AlreadyConnectedException.java \
java/nio/channels/AsynchronousCloseException.java \
java/nio/channels/ByteChannel.java \
java/nio/channels/CancelledKeyException.java \
java/nio/channels/Channel.java \
java/nio/channels/Channels.java \
java/nio/channels/ClosedByInterruptException.java \
java/nio/channels/ClosedChannelException.java \
java/nio/channels/ClosedSelectorException.java \
java/nio/channels/ConnectionPendingException.java \
java/nio/channels/DatagramChannel.java \
java/nio/channels/FileChannel.java \
java/nio/channels/FileChannelImpl.java \
java/nio/channels/FileLock.java \
java/nio/channels/FileLockInterruptionException.java \
java/nio/channels/GatheringByteChannel.java \
java/nio/channels/IllegalBlockingModeException.java \
java/nio/channels/IllegalSelectorException.java \
java/nio/channels/InterruptibleChannel.java \
java/nio/channels/NoConnectionPendingException.java \
java/nio/channels/NonReadableChannelException.java \
java/nio/channels/NonWritableChannelException.java \
java/nio/channels/NotYetBoundException.java \
java/nio/channels/NotYetConnectedException.java \
java/nio/channels/OverlappingFileLockException.java \
java/nio/channels/Pipe.java \
java/nio/channels/ReadableByteChannel.java \
java/nio/channels/ScatteringByteChannel.java \
java/nio/channels/SelectableChannel.java \
java/nio/channels/SelectionKey.java \
java/nio/channels/Selector.java \
java/nio/channels/ServerSocketChannel.java \
java/nio/channels/SocketChannel.java \
java/nio/channels/UnresolvedAddressException.java \
java/nio/channels/UnsupportedAddressTypeException.java \
java/nio/channels/WritableByteChannel.java \
java/nio/channels/spi/AbstractSelectableChannel.java \
java/nio/channels/spi/AbstractInterruptibleChannel.java \
java/nio/channels/spi/AbstractSelectionKey.java \
java/nio/channels/spi/AbstractSelector.java \
java/nio/channels/spi/SelectorProvider.java \
java/nio/charset/Charset.java \
java/nio/charset/CharacterCodingException.java \
java/nio/charset/CharsetDecoder.java \
java/nio/charset/CharsetEncoder.java \
java/nio/charset/CoderMalfunctionError.java \
java/nio/charset/CoderResult.java \
java/nio/charset/CodingErrorAction.java \
java/nio/charset/IllegalCharsetNameException.java \
java/nio/charset/MalformedInputException.java \
java/nio/charset/UnmappableCharacterException.java \
java/nio/charset/UnsupportedCharsetException.java \
java/nio/charset/spi/CharsetProvider.java \
java/security/AccessControlContext.java \
java/security/AccessControlException.java \
java/security/AccessController.java \
java/security/AlgorithmParameterGenerator.java \
java/security/AlgorithmParameters.java \
java/security/AlgorithmParameterGeneratorSpi.java \
java/security/AlgorithmParametersSpi.java \
java/security/AllPermission.java \
java/security/BasicPermission.java \
java/security/Certificate.java \
java/security/CodeSource.java \
java/security/DigestException.java \
java/security/DigestOutputStream.java \
java/security/DummyKeyPairGenerator.java \
java/security/DummySignature.java \
java/security/DigestInputStream.java \
java/security/DomainCombiner.java \
java/security/DummyMessageDigest.java \
java/security/GeneralSecurityException.java \
java/security/Guard.java \
java/security/GuardedObject.java \
java/security/Identity.java \
java/security/IdentityScope.java \
java/security/InvalidAlgorithmParameterException.java \
java/security/InvalidKeyException.java \
java/security/InvalidParameterException.java \
java/security/Key.java \
java/security/KeyFactorySpi.java \
java/security/KeyPairGenerator.java \
java/security/KeyStoreException.java \
java/security/KeyException.java \
java/security/KeyManagementException.java \
java/security/KeyPairGeneratorSpi.java \
java/security/KeyStoreSpi.java \
java/security/KeyFactory.java \
java/security/KeyPair.java \
java/security/KeyStore.java \
java/security/MessageDigest.java \
java/security/MessageDigestSpi.java \
java/security/NoSuchAlgorithmException.java \
java/security/NoSuchProviderException.java \
java/security/Permission.java \
java/security/Principal.java \
java/security/PrivilegedExceptionAction.java \
java/security/PublicKey.java \
java/security/PermissionCollection.java \
java/security/PrivateKey.java \
java/security/ProtectionDomain.java \
java/security/Permissions.java \
java/security/PrivilegedAction.java \
java/security/Provider.java \
java/security/Policy.java \
java/security/PrivilegedActionException.java \
java/security/ProviderException.java \
java/security/SecureClassLoader.java \
java/security/SecureRandomSpi.java \
java/security/SecurityPermission.java \
java/security/SignatureException.java \
java/security/SignedObject.java \
java/security/SecureRandom.java \
java/security/Security.java \
java/security/Signature.java \
java/security/SignatureSpi.java \
java/security/Signer.java \
java/security/UnrecoverableKeyException.java \
java/security/UnresolvedPermission.java \
java/security/acl/Acl.java \
java/security/acl/AclNotFoundException.java \
java/security/acl/LastOwnerException.java \
java/security/acl/Owner.java \
java/security/acl/AclEntry.java \
java/security/acl/Group.java \
java/security/acl/NotOwnerException.java \
java/security/acl/Permission.java \
java/security/cert/CRL.java \
java/security/cert/CRLException.java \
java/security/cert/CRLSelector.java \
java/security/cert/Certificate.java \
java/security/cert/CertificateEncodingException.java \
java/security/cert/CertificateException.java \
java/security/cert/CertificateExpiredException.java \
java/security/cert/CertificateFactory.java \
java/security/cert/CertificateFactorySpi.java \
java/security/cert/CertificateNotYetValidException.java \
java/security/cert/CertificateParsingException.java \
java/security/cert/CertPath.java \
java/security/cert/CertPathBuilder.java \
java/security/cert/CertPathBuilderException.java \
java/security/cert/CertPathBuilderResult.java \
java/security/cert/CertPathBuilderSpi.java \
java/security/cert/CertPathParameters.java \
java/security/cert/CertPathValidator.java \
java/security/cert/CertPathValidatorException.java \
java/security/cert/CertPathValidatorResult.java \
java/security/cert/CertPathValidatorSpi.java \
java/security/cert/CertSelector.java \
java/security/cert/CertStore.java \
java/security/cert/CertStoreException.java \
java/security/cert/CertStoreParameters.java \
java/security/cert/CertStoreSpi.java \
java/security/cert/CollectionCertStoreParameters.java \
java/security/cert/LDAPCertStoreParameters.java \
java/security/cert/PKIXBuilderParameters.java \
java/security/cert/PKIXCertPathBuilderResult.java \
java/security/cert/PKIXCertPathChecker.java \
java/security/cert/PKIXCertPathValidatorResult.java \
java/security/cert/PKIXParameters.java \
java/security/cert/PolicyNode.java \
java/security/cert/PolicyQualifierInfo.java \
java/security/cert/TrustAnchor.java \
java/security/cert/X509CRL.java \
java/security/cert/X509CRLEntry.java \
java/security/cert/X509Certificate.java \
java/security/cert/X509Extension.java \
java/security/interfaces/DSAKey.java \
java/security/interfaces/DSAKeyPairGenerator.java \
java/security/interfaces/DSAParams.java \
java/security/interfaces/DSAPrivateKey.java \
java/security/interfaces/DSAPublicKey.java \
java/security/interfaces/RSAKey.java \
java/security/interfaces/RSAMultiPrimePrivateCrtKey.java \
java/security/interfaces/RSAPrivateCrtKey.java \
java/security/interfaces/RSAPrivateKey.java \
java/security/interfaces/RSAPublicKey.java \
java/security/spec/AlgorithmParameterSpec.java \
java/security/spec/DSAParameterSpec.java \
java/security/spec/DSAPrivateKeySpec.java \
java/security/spec/DSAPublicKeySpec.java \
java/security/spec/EncodedKeySpec.java \
java/security/spec/InvalidKeySpecException.java \
java/security/spec/InvalidParameterSpecException.java \
java/security/spec/KeySpec.java \
java/security/spec/PKCS8EncodedKeySpec.java \
java/security/spec/PSSParameterSpec.java \
java/security/spec/RSAKeyGenParameterSpec.java \
java/security/spec/RSAMultiPrimePrivateCrtKeySpec.java\
java/security/spec/RSAOtherPrimeInfo.java \
java/security/spec/RSAPrivateCrtKeySpec.java \
java/security/spec/RSAPrivateKeySpec.java \
java/security/spec/RSAPublicKeySpec.java \
java/security/spec/X509EncodedKeySpec.java \
java/sql/Array.java \
java/sql/BatchUpdateException.java \
java/sql/Blob.java \
java/sql/CallableStatement.java	\
java/sql/Clob.java \
java/sql/Connection.java \
java/sql/DataTruncation.java \
java/sql/DatabaseMetaData.java \
java/sql/Date.java \
java/sql/Driver.java \
java/sql/DriverManager.java \
java/sql/DriverPropertyInfo.java \
java/sql/ParameterMetaData.java \
java/sql/PreparedStatement.java	\
java/sql/Ref.java \
java/sql/ResultSet.java	\
java/sql/ResultSetMetaData.java	\
java/sql/Savepoint.java \
java/sql/SQLData.java \
java/sql/SQLException.java \
java/sql/SQLInput.java \
java/sql/SQLOutput.java \
java/sql/SQLPermission.java \
java/sql/SQLWarning.java \
java/sql/Statement.java	\
java/sql/Struct.java	\
java/sql/Time.java \
java/sql/Timestamp.java	\
java/sql/Types.java \
java/text/Annotation.java \
java/text/AttributedCharacterIterator.java \
java/text/AttributedString.java	\
java/text/AttributedStringIterator.java	\
java/text/BreakIterator.java \
java/text/CharacterIterator.java \
java/text/ChoiceFormat.java \
java/text/CollationElementIterator.java	\
java/text/CollationKey.java \
java/text/Collator.java	\
java/text/DateFormat.java \
java/text/DateFormatSymbols.java \
java/text/DecimalFormat.java \
java/text/DecimalFormatSymbols.java \
java/text/FieldPosition.java \
java/text/Format.java \
java/text/FormatCharacterIterator.java \
java/text/MessageFormat.java \
java/text/NumberFormat.java \
java/text/ParseException.java \
java/text/ParsePosition.java \
java/text/RuleBasedCollator.java \
java/text/SimpleDateFormat.java	\
java/text/StringCharacterIterator.java \
java/util/jar/Attributes.java \
java/util/jar/JarEntry.java \
java/util/jar/JarException.java \
java/util/jar/JarFile.java \
java/util/jar/JarInputStream.java \
java/util/jar/JarOutputStream.java \
java/util/jar/Manifest.java \
java/util/zip/Adler32.java \
java/util/zip/CRC32.java \
java/util/zip/CheckedInputStream.java \
java/util/zip/CheckedOutputStream.java \
java/util/zip/Checksum.java \
java/util/zip/DataFormatException.java \
java/util/zip/Deflater.java \
java/util/zip/DeflaterOutputStream.java	\
java/util/zip/GZIPInputStream.java \
java/util/zip/GZIPOutputStream.java \
java/util/zip/Inflater.java \
java/util/zip/InflaterInputStream.java \
java/util/zip/ZipConstants.java	\
java/util/zip/ZipEntry.java \
java/util/zip/ZipException.java	\
java/util/zip/ZipFile.java \
java/util/zip/ZipInputStream.java \
java/util/zip/ZipOutputStream.java \
$(rmi_java_source_files) \
$(awt_java_source_files) \
$(convert_source_files) \
$(javax_source_files)


java_source_files = $(special_java_source_files) $(ordinary_java_source_files)

## Math library: C, not C++
c_source_files = \
  java/lang/dtoa.c        java/lang/k_rem_pio2.c  java/lang/s_tan.c       \
  java/lang/e_acos.c      java/lang/k_sin.c       java/lang/strtod.c      \
  java/lang/e_asin.c      java/lang/k_tan.c       java/lang/w_acos.c      \
  java/lang/e_atan2.c     java/lang/mprec.c       java/lang/w_asin.c      \
  java/lang/e_exp.c       java/lang/s_atan.c      java/lang/w_atan2.c     \
  java/lang/e_fmod.c      java/lang/s_ceil.c      java/lang/w_exp.c       \
  java/lang/e_log.c       java/lang/s_copysign.c  java/lang/w_fmod.c      \
  java/lang/e_pow.c       java/lang/s_cos.c       java/lang/w_log.c       \
  java/lang/e_rem_pio2.c  java/lang/s_fabs.c      java/lang/w_pow.c       \
  java/lang/e_remainder.c java/lang/s_floor.c     java/lang/w_remainder.c \
  java/lang/e_scalb.c     java/lang/s_rint.c      java/lang/w_sqrt.c      \
  java/lang/e_sqrt.c      java/lang/s_scalbn.c    java/lang/sf_rint.c     \
  java/lang/k_cos.c       java/lang/s_sin.c       java/lang/sf_fabs.c

extra_cc_source_files = $(EXTRA_CC_FILES)

#java/awt/natToolkit.cc

## This lists all the C++ source files in subdirectories.
nat_source_files = \
gnu/gcj/natCore.cc \
gnu/gcj/convert/JIS0208_to_Unicode.cc \
gnu/gcj/convert/JIS0212_to_Unicode.cc \
gnu/gcj/convert/Unicode_to_JIS.cc \
gnu/gcj/convert/natIconv.cc \
gnu/gcj/convert/natInput_EUCJIS.cc \
gnu/gcj/convert/natInput_SJIS.cc \
gnu/gcj/convert/natOutput_EUCJIS.cc \
gnu/gcj/convert/natOutput_SJIS.cc \
gnu/gcj/io/natSimpleSHSStream.cc \
gnu/gcj/io/shs.cc \
gnu/gcj/runtime/natFinalizerThread.cc \
gnu/gcj/runtime/natFirstThread.cc \
gnu/gcj/runtime/natNameFinder.cc \
gnu/gcj/runtime/natSharedLibLoader.cc \
gnu/gcj/runtime/natStackTrace.cc \
gnu/gcj/runtime/natStringBuffer.cc \
gnu/gcj/runtime/natVMClassLoader.cc \
gnu/java/awt/natEmbeddedWindow.cc \
gnu/java/net/natPlainDatagramSocketImpl.cc \
gnu/java/net/natPlainSocketImpl.cc \
gnu/java/net/protocol/core/natCoreInputStream.cc \
gnu/java/nio/natFileLockImpl.cc \
gnu/java/nio/natPipeImpl.cc \
gnu/java/nio/natSelectorImpl.cc \
gnu/java/nio/natNIOServerSocket.cc \
java/io/natFile.cc \
java/io/natFileDescriptor.cc \
java/io/natObjectInputStream.cc \
java/io/natVMObjectStreamClass.cc \
java/lang/natCharacter.cc \
java/lang/natClass.cc \
java/lang/natClassLoader.cc \
java/lang/natConcreteProcess.cc \
java/lang/natDouble.cc \
java/lang/natFloat.cc \
java/lang/natMath.cc \
java/lang/natObject.cc \
java/lang/natRuntime.cc	\
java/lang/natString.cc \
java/lang/natStringBuffer.cc \
java/lang/natSystem.cc \
java/lang/natThread.cc \
java/lang/natVMSecurityManager.cc \
java/lang/ref/natReference.cc \
java/lang/reflect/natArray.cc \
java/lang/reflect/natConstructor.cc \
java/lang/reflect/natField.cc \
java/lang/reflect/natMethod.cc \
java/lang/reflect/natProxy.cc \
java/net/natNetworkInterface.cc \
java/net/natInetAddress.cc \
java/nio/natDirectByteBufferImpl.cc \
java/nio/channels/natFileChannelImpl.cc \
java/text/natCollator.cc \
java/util/natResourceBundle.cc \
java/util/natTimeZone.cc \
java/util/zip/natDeflater.cc \
java/util/zip/natInflater.cc

x_java_source_files = \
gnu/gcj/xlib/Clip.java \
gnu/gcj/xlib/Colormap.java \
gnu/gcj/xlib/Display.java \
gnu/gcj/xlib/Drawable.java \
gnu/gcj/xlib/Font.java \
gnu/gcj/xlib/GC.java \
gnu/gcj/xlib/Pixmap.java \
gnu/gcj/xlib/Screen.java \
gnu/gcj/xlib/Visual.java \
gnu/gcj/xlib/WMSizeHints.java \
gnu/gcj/xlib/Window.java \
gnu/gcj/xlib/WindowAttributes.java \
gnu/gcj/xlib/XAnyEvent.java \
gnu/gcj/xlib/XButtonEvent.java \
gnu/gcj/xlib/XColor.java \
gnu/gcj/xlib/XConfigureEvent.java \
gnu/gcj/xlib/XConnectException.java \
gnu/gcj/xlib/XEvent.java \
gnu/gcj/xlib/XException.java \
gnu/gcj/xlib/XExposeEvent.java \
gnu/gcj/xlib/XID.java \
gnu/gcj/xlib/XImage.java \
gnu/gcj/xlib/XUnmapEvent.java \
gnu/awt/xlib/XCanvasPeer.java \
gnu/awt/xlib/XEventLoop.java \
gnu/awt/xlib/XEventQueue.java \
gnu/awt/xlib/XFontMetrics.java \
gnu/awt/xlib/XFramePeer.java \
gnu/awt/xlib/XGraphics.java \
gnu/awt/xlib/XGraphicsConfiguration.java \
gnu/awt/xlib/XOffScreenImage.java \
gnu/awt/xlib/XPanelPeer.java \
gnu/awt/xlib/XToolkit.java

x_nat_source_files = \
gnu/gcj/xlib/natClip.cc \
gnu/gcj/xlib/natColormap.cc \
gnu/gcj/xlib/natDisplay.cc \
gnu/gcj/xlib/natDrawable.cc \
gnu/gcj/xlib/natFont.cc \
gnu/gcj/xlib/natGC.cc \
gnu/gcj/xlib/natPixmap.cc \
gnu/gcj/xlib/natScreen.cc \
gnu/gcj/xlib/natVisual.cc \
gnu/gcj/xlib/natWMSizeHints.cc \
gnu/gcj/xlib/natWindow.cc \
gnu/gcj/xlib/natWindowAttributes.cc \
gnu/gcj/xlib/natXAnyEvent.cc \
gnu/gcj/xlib/natXButtonEvent.cc \
gnu/gcj/xlib/natXColor.cc \
gnu/gcj/xlib/natXConfigureEvent.cc \
gnu/gcj/xlib/natXException.cc \
gnu/gcj/xlib/natXExposeEvent.cc \
gnu/gcj/xlib/natXImage.cc \
gnu/gcj/xlib/natXUnmapEvent.cc

## ################################################################

##
## javadoc to texinfo conversion
##

TexinfoDoclet.class: $(srcdir)/scripts/TexinfoDoclet.java
	javac -d . $(srcdir)/scripts/TexinfoDoclet.java

texinfo: TexinfoDoclet.class
	langsource=`ls $(srcdir)/java/lang/*.java`; \
	lang=`echo $$langsource | sed -e 's/[^ ].*EcosProcess.java//g' | sed -e 's/[^ ].*PosixProcess.java//g'`; \
	javadoc -outfile $(srcdir)/doc/java-lang.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $$lang
	javadoc -outfile $(srcdir)/doc/java-lang-ref.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/lang/ref/*.java
	javadoc -outfile $(srcdir)/doc/java-lang-reflect.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/lang/reflect/*.java
	javadoc -outfile $(srcdir)/doc/java-applet.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/applet/*.java
	javadoc -outfile $(srcdir)/doc/java-awt.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/awt/*.java
	javadoc -outfile $(srcdir)/doc/java-awt-color.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/awt/color/*.java
	javadoc -outfile $(srcdir)/doc/java-awt-datatransfer.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/awt/datatransfer/*.java
	javadoc -outfile $(srcdir)/doc/java-awt-event.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/awt/event/*.java
	javadoc -outfile $(srcdir)/doc/java-awt-geom.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/awt/geom/*.java
	javadoc -outfile $(srcdir)/doc/java-awt-image.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/awt/image/*.java
	javadoc -outfile $(srcdir)/doc/java-awt-peer.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/awt/peer/*.java
	javadoc -outfile $(srcdir)/doc/java-beans.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/beans/*.java
	javadoc -outfile $(srcdir)/doc/java-beans-beancontext.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/beans/beancontext/*.java
	javadoc -outfile $(srcdir)/doc/java-io.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/io/*.java
	javadoc -outfile $(srcdir)/doc/java-math.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/math/*.java
	javadoc -outfile $(srcdir)/doc/java-net.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/net/*.java
	javadoc -outfile $(srcdir)/doc/java-security.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/security/*.java
	javadoc -outfile $(srcdir)/doc/java-security-cert.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/security-cert/*.java
	javadoc -outfile $(srcdir)/doc/java-security-spec.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/security/spec/*.java
	javadoc -outfile $(srcdir)/doc/java-security-interfaces.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/security/spec/interfaces/*.java
	javadoc -outfile $(srcdir)/doc/java-sql.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/sql/*.java
	javadoc -outfile $(srcdir)/doc/java-text.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/text/*.java
	javadoc -outfile $(srcdir)/doc/java-util.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/util/*.java
	javadoc -outfile $(srcdir)/doc/java-util-jar.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/util/jar/*.java
	javadoc -outfile $(srcdir)/doc/java-util-zip.texi -doclet TexinfoDoclet -sourcepath .:$(srcdir) $(srcdir)/java/util/zip/*.java

## ################################################################

##
## Dependency tracking madness.
##

-include deps.mk

## This is an evil hack to work around an automake limitation.  We
## need to ensure that all CNI headers are built, not just the ones used
## internally by libgcj. We can't make the .o files depend on nat_headers, 
## because in that case we'll force a complete rebuild of
## the C++ code whenever any .java file is touched.
## Also force all the class files to build first. This makes them build in
## the right order to improve performance.
all-recursive: libgcj-@gcc_version@.jar $(nat_headers) $(x_nat_headers)

## ################################################################

##
## This section is for make and multilib madness.
##

# Work around what appears to be a GNU make bug handling MAKEFLAGS
# values defined in terms of make variables, as is the case for CC and
# friends when we are called from the top level Makefile.
AM_MAKEFLAGS = \
	"AR_FLAGS=$(AR_FLAGS)" \
	"CC_FOR_BUILD=$(CC_FOR_BUILD)" \
	"CFLAGS=$(CFLAGS)" \
	"CXXFLAGS=$(CXXFLAGS)" \
	"CPPFLAGS=$(CPPFLAGS)" \
	"CFLAGS_FOR_BUILD=$(CFLAGS_FOR_BUILD)" \
	"CFLAGS_FOR_TARGET=$(CFLAGS_FOR_TARGET)" \
	"INSTALL=$(INSTALL)" \
	"INSTALL_DATA=$(INSTALL_DATA)" \
	"INSTALL_PROGRAM=$(INSTALL_PROGRAM)" \
	"INSTALL_SCRIPT=$(INSTALL_SCRIPT)" \
	"GCJFLAGS=$(GCJFLAGS)" \
	"LDFLAGS=$(LDFLAGS)" \
	"LIBCFLAGS=$(LIBCFLAGS)" \
	"LIBCFLAGS_FOR_TARGET=$(LIBCFLAGS_FOR_TARGET)" \
	"MAKE=$(MAKE)" \
	"MAKEINFO=$(MAKEINFO) $(MAKEINFOFLAGS)" \
	"PICFLAG=$(PICFLAG)" \
	"PICFLAG_FOR_TARGET=$(PICFLAG_FOR_TARGET)" \
	"SHELL=$(SHELL)" \
	"RUNTESTFLAGS=$(RUNTESTFLAGS)" \
	"exec_prefix=$(exec_prefix)" \
	"infodir=$(infodir)" \
	"libdir=$(libdir)" \
	"prefix=$(prefix)" \
	"AR=$(AR)" \
	"AS=$(AS)" \
	"LD=$(LD)" \
	"LIBCFLAGS=$(LIBCFLAGS)" \
	"NM=$(NM)" \
	"PICFLAG=$(PICFLAG)" \
	"RANLIB=$(RANLIB)" \
	"DESTDIR=$(DESTDIR)"

# Subdir rules rely on $(FLAGS_TO_PASS)
FLAGS_TO_PASS = $(AM_MAKEFLAGS)

CONFIG_STATUS_DEPENDENCIES = $(srcdir)/configure.host

MAKEOVERRIDES=

# Multilib support variables.
MULTISRCTOP =
MULTIBUILDTOP =
MULTIDIRS =
MULTISUBDIR =
MULTIDO = true
MULTICLEAN = true

# Multilib support.
.PHONY: all-multi mostlyclean-multi clean-multi distclean-multi \
	maintainer-clean-multi

all-recursive: all-multi
install-recursive: install-multi
mostlyclean-recursive: mostlyclean-multi
clean-recursive: clean-multi
distclean-recursive: distclean-multi
maintainer-clean-recursive: maintainer-clean-multi

all-multi:
	: $(MAKE) ; exec $(MULTIDO) $(AM_MAKEFLAGS) DO=all multi-do
install-multi:
	$(MULTIDO) $(AM_MAKEFLAGS) DO=install multi-do
mostlyclean-multi:
	$(MULTICLEAN) $(AM_MAKEFLAGS) DO=mostlyclean multi-clean
clean-multi:
	$(MULTICLEAN) $(AM_MAKEFLAGS) DO=clean multi-clean
distclean-multi:
	$(MULTICLEAN) $(AM_MAKEFLAGS) DO=distclean multi-clean
maintainer-clean-multi:
	$(MULTICLEAN) $(AM_MAKEFLAGS) DO=maintainer-clean multi-clean

## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign subdir-objects

ACLOCAL_AMFLAGS = -I . -I ../config

## Garbage collector conditionals.
if USING_BOEHMGC
BOEHMGC_SRC = boehm.cc
else
BOEHMGC_SRC =
endif

if USING_NOGC
NOGC_SRC = nogc.cc
else
NOGC_SRC =
endif

## Broken backtrace conditional.
if SUPPLY_BACKTRACE
BACKTRACE_SRC = sysdep/dwarf2-backtrace.cc
else
BACKTRACE_SRC =
endif

## Conditionals for platform-specific helper functions.
if USING_POSIX_PLATFORM
POSIX_PLATFORM_SRC = posix.cc
else
POSIX_PLATFORM_SRC =
endif

if USING_WIN32_PLATFORM
WIN32_PLATFORM_SRC = win32.cc
else
WIN32_PLATFORM_SRC =
endif

if USING_ECOS_PLATFORM
ECOS_PLATFORM_SRC = posix.cc
else
ECOS_PLATFORM_SRC =
endif

## Conditionals for platform-specific thread support.
if USING_POSIX_THREADS
POSIX_THREAD_SRC = posix-threads.cc
else
POSIX_THREAD_SRC =
endif

if USING_WIN32_THREADS
WIN32_THREAD_SRC = win32-threads.cc
else
WIN32_THREAD_SRC =
endif

if USING_NO_THREADS
NO_THREAD_SRC = no-threads.cc
else
NO_THREAD_SRC =
endif

## Testsuite conditional.
if TESTSUBDIR
SUBDIRS = $(DIRLTDL) testsuite gcj include external
else
SUBDIRS = $(DIRLTDL) gcj include external
endif

## Darwin uses the jcr section, but doesn't use crtstuff.
if USING_DARWIN_CRT
DARWIN_CRT_SRC = darwin.cc
else
DARWIN_CRT_SRC = 
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

# autoconf2.13's target_alias
target_noncanonical = @target_noncanonical@

##
## What gets installed, and where.
##

if XLIB_AWT
cond_xlib_ltlibrary = lib-gnu-awt-xlib.la
else
cond_xlib_ltlibrary =
endif

if GTK_AWT
cond_gtk_ltlibrary = lib-gnu-java-awt-peer-gtk.la
cond_gtk_jawt_ltlibrary = libjawt.la
else
cond_gtk_ltlibrary =
cond_gtk_jawt_ltlibrary =
endif

toolexeclib_LTLIBRARIES = libgcj.la libgij.la \
    $(cond_gtk_ltlibrary) $(cond_gtk_jawt_ltlibrary) $(cond_xlib_ltlibrary)
toolexecmainlib_DATA = libgcj.spec

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libgcj.pc

jardir = $(datadir)/java
jar_DATA = libgcj-@gcc_version@.jar

if JAVA_HOME_SET
JAVA_HOME_DIR = $(JAVA_HOME)
BOOT_CLASS_PATH_DIR = $(JAVA_HOME)/lib/rt.jar
else
JAVA_HOME_DIR = $(prefix)
BOOT_CLASS_PATH_DIR = $(jardir)/$(jar_DATA)
endif

## FIXME: Using libdir violates GNU coding standards.
secdir = $(libdir)/security
## Where to install default logging property file.
propdir = $(libdir)

## Where the standard .db file is found.
dbexecdir = $(libdir)/gcj-@gcc_version@
## Name of the default .db.
db_name = classmap.db
## Compiler specific component of the .db file
db_pathtail = gcj-@gcc_version@/$(db_name)

## For now, only on native systems.  FIXME.
if NATIVE
bin_PROGRAMS = jv-convert gij grmic grmiregistry gcj-dbtool

## It is convenient to actually build and install the default database
## when gcj-dbtool is available.
dbexec_DATA = $(db_name)
endif

bin_SCRIPTS = addr2name.awk

## ################################################################

##
## Compilers and compilation flags.
##

GCJH = @GCJH@
ZIP = @ZIP@

## The compiler with whatever flags we want for both -c and -C
## compiles.
GCJ_WITH_FLAGS = $(GCJ) --encoding=UTF-8 -Wno-deprecated

GCJLINK = $(LIBTOOL) --tag=GCJ --mode=link $(GCJ) -L$(here) $(JC1FLAGS) $(LDFLAGS) -o $@
LIBLINK = $(LIBTOOL) --tag=CXX --mode=link $(CXX) -L$(here) $(JC1FLAGS) $(LDFLAGS) -o $@

JAVAC = $(GCJ_WITH_FLAGS) -C

GCC_UNWIND_INCLUDE = @GCC_UNWIND_INCLUDE@

WARNINGS = -Wextra -Wall
## Some systems don't allow `$' in identifiers by default, so we force
## it with -fdollars-in-identifiers.  -Wswitch-enum detects bugs in
## the verifier implementation, and maybe other places.  We need
## _GNU_SOURCE defined for some Linux builds.  It doesn't hurt to
## always define it.  Some systems, including Linux, need
## -D_FILE_OFFSET_BITS=64 to enable > 2GB file support.
AM_CXXFLAGS = \
	-fno-rtti \
	-fnon-call-exceptions \
	$(THREADCXXFLAGS) \
	-fdollars-in-identifiers \
	-Wswitch-enum \
	-D_FILE_OFFSET_BITS=64 \
	@LIBGCJ_CXXFLAGS@ \
	@X_CFLAGS@ \
	$(WARNINGS) \
	-D_GNU_SOURCE \
	-DPREFIX="\"$(prefix)\"" \
	-DLIBDIR="\"$(libdir)\"" \
        -DJAVA_HOME="\"$(JAVA_HOME_DIR)\"" \
        -DBOOT_CLASS_PATH="\"$(BOOT_CLASS_PATH_DIR)\"" \
	-DJAVA_EXT_DIRS="\"$(jardir)/ext\"" \
	-DGCJ_ENDORSED_DIRS="\"$(jardir)/gcj-endorsed\"" \
	-DLIBGCJ_DEFAULT_DATABASE="\"$(dbexecdir)/$(db_name)\"" \
	-DLIBGCJ_DEFAULT_DATABASE_PATH_TAIL="\"$(db_pathtail)\"" \
	-DTOOLEXECLIBDIR="\"$(toolexeclibdir)\""

AM_GCJFLAGS = \
	@LIBGCJ_JAVAFLAGS@ \
	-fclasspath= -fbootclasspath=$(BOOTCLASSPATH) \
	--encoding=UTF-8 \
	-Wno-deprecated

if USING_GCC
AM_CFLAGS = @LIBGCJ_CFLAGS@ $(WARNINGS)
else
AM_CFLAGS = @LIBGCJ_CFLAGS@
endif

## Extra CFLAGS used for JNI C sources shared with GNU Classpath.
PEDANTIC_CFLAGS = -ansi -pedantic -Wall -Wno-long-long

JCFLAGS = -g
JC1FLAGS = @LIBGCJ_JAVAFLAGS@ $(GCJFLAGS)

LIBFFIINCS = @LIBFFIINCS@

AM_CPPFLAGS = -I$(top_srcdir) -Iinclude -I$(top_srcdir)/include \
	$(GCINCS) $(THREADINCS) $(INCLTDL) \
	$(GCC_UNWIND_INCLUDE) $(ZINCS) $(LIBFFIINCS)

BOOTCLASSPATH = $(here)'$(CLASSPATH_SEPARATOR)'$(srcdir)'$(CLASSPATH_SEPARATOR)'$(srcdir)/external/w3c_dom'$(CLASSPATH_SEPARATOR)'$(srcdir)/external/sax

## ################################################################

##
## How to build libgcj.a and libgcj.jar
##

libgij_la_SOURCES = gij.cc
libgij_la_DEPENDENCIES = libgcj.la libgcj.spec
## See jv_convert_LDADD.
libgij_la_LIBADD = -L$(here)/.libs libgcj.la
## The mysterious backslash in the grep pattern is consumed by make.
libgij_la_LDFLAGS = -rpath $(toolexeclibdir) \
        -version-info `grep -v '^\#' $(srcdir)/libtool-version` $(LIBGCJ_LD_SYMBOLIC)

# Work around libtool wanting to do piece-wise linking with -r.  Using a
# convenience library suddenly invokes the --whole-archive path instead.
# This allows the build to succeed for targets that allocate multiple got
# subsections in the linker, such as Alpha and MIPS.
libgcj0_convenience_la_SOURCES = prims.cc jni.cc exception.cc \
	link.cc defineclass.cc interpret.cc verify.cc \
	$(nat_source_files) $(math_c_source_files) $(java_source_files) \
	$(built_java_source_files) \
	$(BOEHMGC_SRC) $(NOGC_SRC) \
	$(BACKTRACE_SRC) \
	$(POSIX_PLATFORM_SRC) $(WIN32_PLATFORM_SRC) $(ECOS_PLATFORM_SRC) \
	$(DARWIN_CRT_SRC) \
	$(POSIX_THREAD_SRC) $(WIN32_THREAD_SRC) $(NO_THREAD_SRC)
libgcj0_convenience_la_LIBADD = \
	gnu-xml.lo javax-imageio.lo \
	javax-xml.lo gnu-java-beans.lo \
	external/sax/libsax-gcj.la \
	external/w3c_dom/libw3c-gcj.la

noinst_LTLIBRARIES = libgcj0_convenience.la

libgcj_la_SOURCES =

## Objects from C++ sources in subdirs.
nat_files = $(nat_source_files:.cc=.lo)
xlib_nat_files = $(xlib_nat_source_files:.cc=.lo)

# Include THREADLIBS here to ensure that the correct version of
# certain linuxthread functions get linked:
## The mysterious backslash in the grep pattern is consumed by make.
libgcj_la_LDFLAGS = -rpath $(toolexeclibdir) $(THREADLDFLAGS) $(THREADLIBS) \
	$(LIBLTDL) $(SYS_ZLIBS) \
	-version-info `grep -v '^\#' $(srcdir)/libtool-version`
libgcj_la_LIBADD = \
	libgcj0_convenience.la \
	$(LIBFFI) $(ZLIBS) $(GCLIBS) $(propertyo_files)
libgcj_la_DEPENDENCIES = libgcj-$(gcc_version).jar \
	java/lang/fdlibm.h java/lang/ieeefp.h java/lang/mprec.h \
	$(LIBLTDL) $(libgcj_la_LIBADD)
libgcj_la_LINK = $(LIBLINK)


## The .db file.  This rule is only used for native builds, so it is
## safe to invoke gcj-dbtool.
$(db_name): gcj-dbtool$(EXEEXT)
## In case it exists already.
	@rm -f $(db_name)
## We don't actually care if it fails -- if it does, just make an
## empty file.  This is simpler than trying to discover when mmap is
## not available.
	./gcj-dbtool -n $(db_name) || touch $(db_name)

# Gtk/Cairo JNI sources.
if GTK_CAIRO
gtk_cairo_c_source_files = \
jni/gtk-peer/gnu_java_awt_peer_gtk_GdkGraphics2D.c
else
gtk_cairo_c_source_files = 
endif

## Gtk JNI sources.
gtk_c_source_files = \
$(gtk_cairo_c_source_files) \
jni/gtk-peer/gnu_java_awt_peer_gtk_GdkTextLayout.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GdkFontPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GdkGraphics.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GdkGraphicsEnvironment.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GdkPixbufDecoder.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GdkRobotPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkButtonPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkCanvasPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkCheckboxGroupPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkCheckboxMenuItemPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkCheckboxPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkChoicePeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkClipboard.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkComponentPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkEmbeddedWindowPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkEvents.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkFileDialogPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkFramePeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkGenericPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkImagePainter.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkLabelPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkListPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkMenuBarPeer.c \
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkMenuComponentPeer.c \
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
jni/gtk-peer/gnu_java_awt_peer_gtk_GThreadNativeMethodRunner.c \
jni/gtk-peer/gthread-jni.c \
jni/gtk-peer/gtk_jawt.c \
jni/classpath/native_state.c

## Java sources for Gtk peers.
gtk_awt_peer_sources = \
gnu/java/awt/peer/gtk/GdkTextLayout.java \
gnu/java/awt/peer/gtk/GdkFontMetrics.java \
gnu/java/awt/peer/gtk/GdkFontPeer.java \
gnu/java/awt/peer/gtk/GdkGlyphVector.java \
gnu/java/awt/peer/gtk/GdkGraphics.java \
gnu/java/awt/peer/gtk/GdkGraphics2D.java \
gnu/java/awt/peer/gtk/GdkGraphicsConfiguration.java \
gnu/java/awt/peer/gtk/GdkGraphicsEnvironment.java \
gnu/java/awt/peer/gtk/GdkPixbufDecoder.java \
gnu/java/awt/peer/gtk/GdkRobotPeer.java \
gnu/java/awt/peer/gtk/GdkScreenGraphicsDevice.java \
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
gnu/java/awt/peer/gtk/GtkFramePeer.java	\
gnu/java/awt/peer/gtk/GtkGenericPeer.java \
gnu/java/awt/peer/gtk/GtkImage.java \
gnu/java/awt/peer/gtk/GtkImagePainter.java \
gnu/java/awt/peer/gtk/GtkLabelPeer.java	\
gnu/java/awt/peer/gtk/GtkListPeer.java \
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
gnu/java/awt/peer/gtk/GtkWindowPeer.java \
gnu/java/awt/peer/gtk/GThreadMutex.java \
gnu/java/awt/peer/gtk/GThreadNativeMethodRunner.java


gtk_jni_headers = $(patsubst %.java,jniinclude/%.h,$(subst /,_,$(gtk_awt_peer_sources)))

jniinclude/gnu_java_awt_peer_gtk_GdkGraphics.h: gnu/java/awt/peer/gtk/GdkGraphics.java
jniinclude/gnu_java_awt_peer_gtk_GdkGraphics2D.h: gnu/java/awt/peer/gtk/GdkGraphics2D.java
jniinclude/gnu_java_awt_peer_gtk_GdkGraphicsEnvironment.h: gnu/java/awt/peer/gtk/GdkGraphicsEnvironment.java
jniinclude/gnu_java_awt_peer_gtk_GdkPixbufDecoder.h: gnu/java/awt/peer/gtk/GdkPixbufDecoder.java
jniinclude/gnu_java_awt_peer_gtk_GtkButtonPeer.h: gnu/java/awt/peer/gtk/GtkButtonPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkCanvasPeer.h: gnu/java/awt/peer/gtk/GtkCanvasPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkCheckboxGroupPeer.h: gnu/java/awt/peer/gtk/GtkCheckboxGroupPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkCheckboxMenuItemPeer.h: gnu/java/awt/peer/gtk/GtkCheckboxMenuItemPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkCheckboxPeer.h: gnu/java/awt/peer/gtk/GtkCheckboxPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkChoicePeer.h: gnu/java/awt/peer/gtk/GtkChoicePeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkClipboard.h: gnu/java/awt/peer/gtk/GtkClipboard.java
jniinclude/gnu_java_awt_peer_gtk_GtkComponentPeer.h: gnu/java/awt/peer/gtk/GtkComponentPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkContainerPeer.h: gnu/java/awt/peer/gtk/GtkContainerPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkDialogPeer.h: gnu/java/awt/peer/gtk/GtkDialogPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkEmbeddedWindowPeer.h: gnu/java/awt/peer/gtk/GtkEmbeddedWindowPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkFileDialogPeer.h: gnu/java/awt/peer/gtk/GtkFileDialogPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkFontPeer.h: gnu/java/awt/peer/gtk/GtkFontPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkFramePeer.h: gnu/java/awt/peer/gtk/GtkFramePeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkGenericPeer.h: gnu/java/awt/peer/gtk/GtkGenericPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkImage.h: gnu/java/awt/peer/gtk/GtkImage.java
jniinclude/gnu_java_awt_peer_gtk_GtkImagePainter.h: gnu/java/awt/peer/gtk/GtkImagePainter.java
jniinclude/gnu_java_awt_peer_gtk_GtkLabelPeer.h: gnu/java/awt/peer/gtk/GtkLabelPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkListPeer.h: gnu/java/awt/peer/gtk/GtkListPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkMenuBarPeer.h: gnu/java/awt/peer/gtk/GtkMenuBarPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkMenuComponentPeer.h: gnu/java/awt/peer/gtk/GtkMenuComponentPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkMenuItemPeer.h: gnu/java/awt/peer/gtk/GtkMenuItemPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkMenuPeer.h: gnu/java/awt/peer/gtk/GtkMenuPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkOffScreenImage.h: gnu/java/awt/peer/gtk/GtkOffScreenImage.java
jniinclude/gnu_java_awt_peer_gtk_GtkPanelPeer.h: gnu/java/awt/peer/gtk/GtkPanelPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkPopupMenuPeer.h: gnu/java/awt/peer/gtk/GtkPopupMenuPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkScrollPanePeer.h: gnu/java/awt/peer/gtk/GtkScrollPanePeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkScrollbarPeer.h: gnu/java/awt/peer/gtk/GtkScrollbarPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkTextAreaPeer.h: gnu/java/awt/peer/gtk/GtkTextAreaPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkTextComponentPeer.h: gnu/java/awt/peer/gtk/GtkTextComponentPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkTextFieldPeer.h: gnu/java/awt/peer/gtk/GtkTextFieldPeer.java
jniinclude/gnu_java_awt_peer_gtk_GtkToolkit.h: gnu/java/awt/peer/gtk/GtkToolkit.java
jniinclude/gnu_java_awt_peer_gtk_GtkWindowPeer.h: gnu/java/awt/peer/gtk/GtkWindowPeer.java
jniinclude/gnu_java_awt_peer_gtk_GThreadMutex.h: gnu/java/awt/peer/gtk/GThreadMutex.java
jniinclude/gnu_java_awt_peer_gtk_GThreadNativeMethodRunner.h: gnu/java/awt/peer/gtk/GThreadNativeMethodRunner.java
jniinclude/gnu_java_awt_peer_gtk_GdkTextLayout.h: gnu/java/awt/peer/gtk/GdkTextLayout.java
jniinclude/gnu_java_awt_peer_gtk_GdkFontPeer.h: gnu/java/awt/peer/gtk/GdkFontPeer.java

$(gtk_jni_headers): %.h:
	@input=`echo $@ | sed -e 's,jniinclude/,,' -e 's,_,.,g' -e 's,.h$$,,'`; \
	echo "$(GCJH) -jni -d jniinclude -classpath '' -bootclasspath $(top_builddir) $$input"; \
	$(GCJH) -jni -d jniinclude -classpath '' -bootclasspath $(top_builddir) $$input

jni/gtk-peer/gnu_java_awt_peer_gtk_GdkFontPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GdkFontPeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GdkGraphics2D.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GdkGraphics2D.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GdkGraphicsEnvironment.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GdkGraphicsEnvironment.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GdkGraphics.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GdkGraphics.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GdkPixbufDecoder.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GdkPixbufDecoder.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GdkTextLayout.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GdkTextLayout.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GThreadNativeMethodRunner.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GThreadNativeMethodRunner.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkButtonPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkButtonPeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkCanvasPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkCanvasPeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkCheckboxGroupPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkCheckboxGroupPeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkCheckboxMenuItemPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkCheckboxMenuItemPeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkCheckboxPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkCheckboxPeer.h $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkComponentPeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkChoicePeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkChoicePeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkClipboard.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkClipboard.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkComponentPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkComponentPeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkEmbeddedWindowPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkEmbeddedWindowPeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkFileDialogPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkFileDialogPeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkFramePeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkFramePeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkGenericPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkGenericPeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkImagePainter.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkImagePainter.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkLabelPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkLabelPeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkListPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkListPeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkMenuBarPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkMenuBarPeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkMenuComponentPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkMenuComponentPeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkMenuItemPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkMenuItemPeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkMenuPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkMenuPeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkPanelPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkPanelPeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkPopupMenuPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkPopupMenuPeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkScrollBarPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkScrollbarPeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkScrollPanePeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkScrollPanePeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkTextAreaPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkTextAreaPeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkTextComponentPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkTextComponentPeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkTextFieldPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkTextFieldPeer.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkToolkit.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkToolkit.h
jni/gtk-peer/gnu_java_awt_peer_gtk_GtkWindowPeer.lo: $(top_builddir)/jniinclude/gnu_java_awt_peer_gtk_GtkWindowPeer.h


## For the peer library, DEPENDENCIES need to come before OBJECTS so
## that JNI headers are built before JNI C files.
$(lib_gnu_java_awt_peer_gtk_la_OBJECTS): $(lib_gnu_java_awt_peer_gtk_la_DEPENDENCIES)

lib_gnu_java_awt_peer_gtk_la_SOURCES = \
$(gtk_c_source_files) \
jni/gtk-peer/gthread-jni.h \
jni/gtk-peer/gtkpeer.h \
jni/classpath/native_state.h

lib_gnu_java_awt_peer_gtk_la_CFLAGS = \
	-Ijniinclude -I$(srcdir)/jni/classpath \
	-I$(srcdir)/jni/gtk-peer $(PEDANTIC_CFLAGS) $(GTK_CFLAGS) $(LIBART_CFLAGS) \
	$(X_CFLAGS) $(CAIRO_CFLAGS) $(PANGOFT2_CFLAGS)
lib_gnu_java_awt_peer_gtk_la_GCJFLAGS = $(AM_GCJFLAGS) -fjni
lib_gnu_java_awt_peer_gtk_la_LIBADD = gtk-awt-peer.lo $(GTK_LIBS) $(GLIB_LIBS) $(LIBART_LIBS) $(CAIRO_LIBS) $(PANGOFT2_LIBS)
lib_gnu_java_awt_peer_gtk_la_DEPENDENCIES = gtk-awt-peer.lo $(gtk_jni_headers) libgcj-$(gcc_version).jar libgcj.la libgcj.spec
## The mysterious backslash in the grep pattern is consumed by make.
lib_gnu_java_awt_peer_gtk_la_LDFLAGS = \
        -version-info `grep -v '^\#' $(srcdir)/libtool-version` $(LIBGCJ_LD_SYMBOLIC) $(X_LIBS) -lXtst
lib_gnu_java_awt_peer_gtk_la_LINK = $(LIBLINK)

libjawt_la_SOURCES = jawt.c
libjawt_la_CFLAGS = -I$(srcdir)/jni/classpath $(PEDANTIC_CFLAGS) $(X_CFLAGS)
## See jv_convert_LDADD.
libjawt_la_LIBADD = -L$(here)/.libs lib-gnu-java-awt-peer-gtk.la
libjawt_la_LDFLAGS = \
	-version-info `grep -v '^\#' $(srcdir)/libtool-version` \
	$(LIBGCJ_LD_SYMBOLIC)
libjawt_la_LINK = $(LIBLINK)

lib_gnu_awt_xlib_la_SOURCES = \
	$(xlib_java_source_files) \
	$(xlib_nat_source_files)
lib_gnu_awt_xlib_la_DEPENDENCIES = libgcj-@gcc_version@.jar libgcj.la libgcj.spec
lib_gnu_awt_xlib_la_LIBADD = $(xlib_javao_files)
## We require libstdc++-v3 to be in the same build tree.
lib_gnu_awt_xlib_la_CPPFLAGS = \
	$(AM_CPPFLAGS) \
	-I../libstdc++-v3/include \
	-I../libstdc++-v3/include/$(target_noncanonical) \
	-I$(srcdir)/../libstdc++-v3/libsupc++
## The mysterious backslash in the grep pattern is consumed by make.
lib_gnu_awt_xlib_la_LDFLAGS = ../libstdc++-v3/src/libstdc++.la \
	@X_PRE_LIBS@ @X_LIBS@ -lX11 @X_EXTRA_LIBS@ \
        -rpath $(toolexeclibdir) \
        -version-info `grep -v '^\#' $(srcdir)/libtool-version` $(LIBGCJ_LD_SYMBOLIC)
lib_gnu_awt_xlib_la_LINK = $(LIBLINK)

all_java_source_files = \
    $(java_source_files) \
    $(built_java_source_files) \
    $(gtk_awt_peer_sources) \
    $(xlib_java_source_files) \
    $(gnu_xml_source_files) \
    $(javax_imageio_source_files) \
    $(javax_xml_source_files) \
    $(gnu_java_beans_source_files)

all_java_class_files = $(all_java_source_files:.java=.class)

all_property_files = $(property_files) \
    java/util/logging/logging.properties

## Build property files into the library.
property_files = \
gnu/regexp/MessagesBundle.properties \
gnu/regexp/MessagesBundle_fr.properties \
org/ietf/jgss/MessagesBundle.properties

propertyo_files = $(property_files:.properties=.properties.lo)

%.properties.lo: %.properties
	$(LTGCJCOMPILE) -o $@ -c $< -Wc,--resource,`echo $@ | sed "s/\.lo$$//"`

if ONESTEP

# Compile all classfiles in one go.

classes.stamp: $(all_java_source_files) gnu/gcj/tools/gcj_dbtool/Main.java
	@echo Compiling Java sourcefiles...
	@: $(call write_entries_to_file,$?,libgcj.sourcelist)
	$(JAVAC) $(JCFLAGS) -classpath '' -bootclasspath $(BOOTCLASSPATH) -d $(here) @libgcj.sourcelist
	echo > classes.stamp
# This next rule seems backward, but reflects the fact that 1) all
# classfiles are compiled in one go when classes.stamp is built and 2)
# anything which depends on a particular .class file must wait until
# this file is built.
$(all_java_class_files) gnu/gcj/tools/gcj_dbtool/Main.class: classes.stamp

else # !ONESTEP

# Compile each classfile individually.

.java.class:
	$(JAVAC) $(JCFLAGS) -classpath '' -bootclasspath $(BOOTCLASSPATH) -d $(here) $<

classes.stamp: $(all_java_class_files) $(all_property_files)
	echo > classes.stamp

endif

libgcj-@gcc_version@.jar: classes.stamp
	-@rm -f libgcj-@gcc_version@.jar
## Note that we explicitly want to include directory information.
	(find java gnu javax org -type d -o -type f -name '*.class'; \
## Ugly code to avoid "echo -C".  Must separate each entry by a newline
## Gross but easy.
	for file in $(all_property_files); do \
	  echo "x-C" | sed -e 's/^.//'; \
	  echo $(srcdir); \
	  echo $$file; \
	done) | \
	  sed -e '/\/xlib/d' -e '/\/\.libs/d' -e '/\/\.deps/d' | \
	  $(ZIP) -cfM0E@ $@
## Now include the classes from external/.
	for dir in sax w3c_dom; do \
	  (cd external/$$dir; \
	  find org -type f -name '*.class' -print | while read file; do \
	    echo "x-C" | sed -e 's/^.//'; \
	    echo external/$$dir; \
	    echo $$file; \
          done) | \
	  $(ZIP) -ufM0E@ $@; \
	done

CLEANFILES = libgcj-@gcc_version@.jar classes.stamp

mostlyclean-local:
## Use libtool rm to remove each libtool object
	find . -name '*.lo' -print | xargs $(LIBTOOL) rm -f
## Remove headers.  We use GNU make trickery because otherwise
## the command line is too long.
	@: $(foreach header,$(nat_headers) $(xlib_nat_headers) $(gtk_jni_headers),$(shell rm -f '$(header)'))

clean-local:
## Remove every .class file that was created.
	find . -name '*.class' -print | xargs rm -f

distclean-local:
## Remove every .d file that was created.
	find . -name '*.d' -print | xargs rm -f

# Just remove the objects from C++ sources, for testing the C++ compiler.
clean-nat:
	rm -f $(nat_files) $(xlib_nat_files)

SUFFIXES = .class .java .h .properties

## Note: we omit StackTrace here, since it has an explicit rule a bit
## later, and GNU make will warn in this case.
$(filter-out gnu/gcj/runtime/StackTrace.lo, $(javao_files)) $(xlib_javao_files): %.lo: %.java
	$(LTGCJCOMPILE) -o $@ -c $<

## A special case.  The sibcall optimization can change the number of
## frames on the stack, and StackTrace makes assumptions about this
## number.
gnu/gcj/runtime/StackTrace.lo: gnu/gcj/runtime/StackTrace.java
	$(LTGCJCOMPILE) -fno-optimize-sibling-calls -o $@ -c $<

## Pass the list of object files to libtool in a temporary file to
## avoid tripping platform command line length limits.
libgcj.la: $(libgcj_la_OBJECTS) $(libgcj_la_DEPENDENCIES)
	@echo Creating list of files to link...
	@: $(call write_entries_to_file,$(libgcj_la_OBJECTS),libgcj.objectlist)
	$(libgcj_la_LINK) -objectlist libgcj.objectlist \
	$(libgcj_la_LIBADD) \
	-rpath $(toolexeclibdir) $(libgcj_la_LDFLAGS) $(LIBS)

lib-gnu-awt-xlib.la: $(lib_gnu_awt_xlib_la_OBJECTS) $(lib_gnu_awt_xlib_la_DEPENDENCIES)
	@echo Creating list of files to link...
	@: $(call write_entries_to_file,$(lib_gnu_awt_xlib_la_OBJECTS) $(lib_gnu_awt_xlib_la_LIBADD),lib_gnu_awt_xlib.objectlist)
	$(lib_gnu_awt_xlib_la_LINK) -objectlist lib_gnu_awt_xlib.objectlist \
	-rpath $(toolexeclibdir) $(lib_gnu_awt_xlib_la_LDFLAGS) $(LIBS)

## ################################################################

##
## Some packages must be built with the binary compatibility ABI.  We
## compile each such package into a .so, broken down more or less by
## conceptual unit.
##

## Depend on the sources, even though we are going to compile the
## classes.
gnu-xml.lo: $(gnu_xml_source_files)
## FIXME: this is ugly.  We want to make sure the .class files have
## been built, but we don't want a real dependency on them as this
## would cause our target to be rebuilt whenever any .java file is
## touched.
	$(MAKE) classes.stamp
	$(LTGCJCOMPILE) -fjni -findirect-dispatch -c -o gnu-xml.lo \
		`find gnu/xml -name '*.class' -print | sort -r`

## Depend on the sources, even though we are going to compile the
## classes.
javax-imageio.lo: $(javax_imageio_source_files)
## FIXME: this is ugly.  We want to make sure the .class files have
## been built, but we don't want a real dependency on them as this
## would cause our target to be rebuilt whenever any .java file is
## touched.
	$(MAKE) classes.stamp
	$(LTGCJCOMPILE) -findirect-dispatch -c -o javax-imageio.lo \
		`find javax/imageio -name '*.class' -print | sort -r`

## Depend on the sources, even though we are going to compile the
## classes.
javax-xml.lo: $(javax_xml_source_files)
## FIXME: this is ugly.  We want to make sure the .class files have
## been built, but we don't want a real dependency on them as this
## would cause our target to be rebuilt whenever any .java file is
## touched.
	$(MAKE) classes.stamp
	$(LTGCJCOMPILE) -findirect-dispatch -c -o javax-xml.lo \
		`find javax/xml -name '*.class' -print | sort -r`

## Depend on the sources, even though we are going to compile the
## classes.
gnu-java-beans.lo: $(gnu_java_beans_source_files)
## FIXME: this is ugly.  We want to make sure the .class files have
## been built, but we don't want a real dependency on them as this
## would cause our target to be rebuilt whenever any .java file is
## touched.
	$(MAKE) classes.stamp
	$(LTGCJCOMPILE) -findirect-dispatch -c -o gnu-java-beans.lo \
		`find gnu/java/beans -name '*.class' -print | sort -r`

## Depend on the sources, even though we are going to compile the
## classes.
gtk-awt-peer.lo: $(gtk_awt_peer_sources)
## FIXME: this is ugly.  We want to make sure the .class files have
## been built, but we don't want a real dependency on them as this
## would cause our target to be rebuilt whenever any .java file is
## touched.
	$(MAKE) classes.stamp
	$(LTGCJCOMPILE) -findirect-dispatch -fjni -c -o gtk-awt-peer.lo \
		`find gnu/java/awt/peer/gtk -name '*.class' -print | sort -r`

## ################################################################

##
## How to build header files.
##

%.h: %.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) $(basename $<)

## Header files used when compiling some of the nat* files.
ordinary_nat_headers = \
    $(ordinary_java_source_files:.java=.h) \
    $(built_java_source_files:.java=.h)

inner_nat_headers = java/io/ObjectOutputStream$$PutField.h \
	java/io/ObjectInputStream$$GetField.h \
	java/nio/DirectByteBufferImpl$$ReadWrite.h \
	java/nio/channels/Pipe$$SinkChannel.h \
	java/nio/channels/Pipe$$SourceChannel.h \
	java/lang/reflect/Proxy$$ProxyData.h \
	java/lang/reflect/Proxy$$ProxyType.h \
	gnu/java/net/PlainSocketImpl$$SocketInputStream.h \
	gnu/java/net/PlainSocketImpl$$SocketOutputStream.h \
	gnu/java/nio/PipeImpl$$SinkChannelImpl.h \
	gnu/java/nio/PipeImpl$$SourceChannelImpl.h \
	$(PLATFORM_INNER_NAT_HDRS)

nat_headers = $(ordinary_nat_headers) $(inner_nat_headers) gnu/gcj/tools/gcj_dbtool/Main.h
nat_headers_install = $(ordinary_nat_headers)

xlib_nat_headers = $(xlib_java_source_files:.java=.h)

$(ordinary_nat_headers) $(xlib_nat_headers): %.h: %.class

java/lang/ClassLoader.h: java/lang/ClassLoader.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
		-prepend 'jclass _Jv_FindClass (_Jv_Utf8Const *name, java::lang::ClassLoader *loader);' \
		-prepend 'void _Jv_RunMain (jclass klass, const char *name, int argc, const char **argv, bool is_jar);' \
		-friend 'jclass (::_Jv_FindClass) (_Jv_Utf8Const *name, java::lang::ClassLoader *loader);' \
		-friend 'void ::_Jv_RunMain (jclass klass, const char *name, int argc, const char **argv, bool is_jar);' \
		$(basename $<)

java/lang/Thread.h: java/lang/Thread.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
		-prepend 'class _Jv_JNIEnv;' \
		-prepend '#define _JV_NOT_OWNER 1' \
		-prepend '#define _JV_INTERRUPTED 2' \
		-prepend '_Jv_JNIEnv * _Jv_GetCurrentJNIEnv ();' \
		-prepend 'void _Jv_SetCurrentJNIEnv (_Jv_JNIEnv *env);' \
		-prepend 'void _Jv_ThreadRun (java::lang::Thread* thread);' \
		-prepend 'jint _Jv_AttachCurrentThread(java::lang::Thread* thread);' \
		-prepend 'java::lang::Thread* _Jv_AttachCurrentThread (jstring name, java::lang::ThreadGroup* group);' \
		-prepend 'java::lang::Thread* _Jv_AttachCurrentThreadAsDaemon (jstring name, java::lang::ThreadGroup* group);' \
		-prepend 'jint _Jv_DetachCurrentThread ();' \
		-friend '_Jv_JNIEnv * ::_Jv_GetCurrentJNIEnv ();' \
		-friend 'void ::_Jv_SetCurrentJNIEnv (_Jv_JNIEnv *env);' \
		-friend 'void ::_Jv_ThreadRun (java::lang::Thread* thread);' \
		-friend 'jint (::_Jv_AttachCurrentThread) (java::lang::Thread* thread);' \
		-friend 'java::lang::Thread* ::_Jv_AttachCurrentThread (jstring name, java::lang::ThreadGroup* group);' \
		-friend 'java::lang::Thread* ::_Jv_AttachCurrentThreadAsDaemon (jstring name, java::lang::ThreadGroup* group);' \
		-friend 'jint (::_Jv_DetachCurrentThread) ();' \
		$(basename $<)

java/lang/String.h: java/lang/String.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	    -prepend 'jchar* _Jv_GetStringChars (jstring str);' \
	    -prepend 'jstring* _Jv_StringFindSlot (jchar*, jint, jint);' \
	    -prepend 'jstring* _Jv_StringGetSlot (jstring);' \
	    -prepend 'jstring _Jv_NewStringUtf8Const (_Jv_Utf8Const* str);' \
	    -prepend 'jstring _Jv_NewStringLatin1 (const char*, jsize);' \
	    -prepend 'jstring _Jv_AllocString (jsize);' \
	    -friend 'jchar* ::_Jv_GetStringChars (jstring str);' \
	    -friend 'jstring* ::_Jv_StringFindSlot (jchar*, jint, jint);' \
	    -friend 'jstring* ::_Jv_StringGetSlot (jstring);' \
	    -friend 'jstring (::_Jv_NewStringUtf8Const) (_Jv_Utf8Const* str);' \
	    -friend 'jstring (::_Jv_NewStringLatin1) (const char*, jsize);' \
	    -friend 'jstring (::_Jv_AllocString) (jsize);' \
	    $(basename $<)

java/lang/reflect/Constructor.h: java/lang/reflect/Constructor.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	    -prepend 'jmethodID _Jv_FromReflectedConstructor (java::lang::reflect::Constructor *);' \
	    -prepend 'jobject _Jv_JNI_ToReflectedMethod (_Jv_JNIEnv *, jclass, jmethodID, jboolean);' \
	    -friend 'jmethodID (::_Jv_FromReflectedConstructor) (java::lang::reflect::Constructor *);' \
	    -friend 'jobject (::_Jv_JNI_ToReflectedMethod) (_Jv_JNIEnv *, jclass, jmethodID, jboolean);' \
	    -friend 'class java::lang::Class;' \
	    $(basename $<)

java/lang/reflect/Field.h: java/lang/reflect/Field.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	    -prepend 'jfieldID _Jv_FromReflectedField (java::lang::reflect::Field *);' \
	    -prepend 'jobject _Jv_JNI_ToReflectedField (_Jv_JNIEnv*, jclass, jfieldID, jboolean);' \
	    -friend 'jfieldID (::_Jv_FromReflectedField) (java::lang::reflect::Field *);' \
	    -friend 'jobject (::_Jv_JNI_ToReflectedField) (_Jv_JNIEnv*, jclass, jfieldID, jboolean);' \
	    -friend 'class java::lang::Class;' \
	    $(basename $<)

java/lang/reflect/Method.h: java/lang/reflect/Method.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	    -prepend 'jmethodID _Jv_FromReflectedMethod (java::lang::reflect::Method *);' \
	    -prepend 'jobject _Jv_JNI_ToReflectedMethod (_Jv_JNIEnv *, jclass, jmethodID, jboolean);' \
	    -friend 'jmethodID (::_Jv_FromReflectedMethod) (java::lang::reflect::Method *);' \
	    -friend 'jobject (::_Jv_JNI_ToReflectedMethod) (_Jv_JNIEnv *, jclass, jmethodID, jboolean);' \
	    -friend 'class java::lang::Class;' \
	    -friend 'class java::io::ObjectInputStream;' \
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

gnu/gcj/runtime/ExtensionClassLoader.h: gnu/gcj/runtime/ExtensionClassLoader.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	    -friend 'class ::java::lang::ClassLoader;' \
	    $(basename $<)

java/io/ObjectInputStream$$GetField.h: java/io/ObjectInputStream.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	'java/io/ObjectInputStream$$GetField'

java/io/ObjectOutputStream$$PutField.h: java/io/ObjectOutputStream.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	'java/io/ObjectOutputStream$$PutField'

java/nio/DirectByteBufferImpl$$ReadWrite.h: java/nio/DirectByteBufferImpl.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	'java/nio/DirectByteBufferImpl$$ReadWrite'

java/nio/channels/Pipe$$SinkChannel.h: java/nio/channels/Pipe.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	'java/nio/channels/Pipe$$SinkChannel'

java/nio/channels/Pipe$$SourceChannel.h: java/nio/channels/Pipe.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	'java/nio/channels/Pipe$$SourceChannel'

gnu/java/net/PlainSocketImpl$$SocketInputStream.h: gnu/java/net/PlainSocketImpl.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	'gnu/java/net/PlainSocketImpl$$SocketInputStream'

gnu/java/net/PlainSocketImpl$$SocketOutputStream.h: gnu/java/net/PlainSocketImpl.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	'gnu/java/net/PlainSocketImpl$$SocketOutputStream'

gnu/java/nio/PipeImpl$$SinkChannelImpl.h: gnu/java/nio/PipeImpl.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	'gnu/java/nio/PipeImpl$$SinkChannelImpl'

gnu/java/nio/PipeImpl$$SourceChannelImpl.h: gnu/java/nio/PipeImpl.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	'gnu/java/nio/PipeImpl$$SourceChannelImpl'

## Only used by PosixProcess.java
java/lang/ConcreteProcess$$ProcessManager.h: java/lang/ConcreteProcess.class
	$(GCJH) -classpath '' -bootclasspath $(top_builddir) \
	'java/lang/ConcreteProcess$$ProcessManager'

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
	  $(mkinstalldirs) $(DESTDIR)$(gxx_include_dir)/$$d; \
	  if test -f $(srcdir)/$$f; then p=$(srcdir)/$$f; else p=$$f; fi; \
	  echo " $(INSTALL_DATA) $$p $(DESTDIR)$(gxx_include_dir)/$$f"; \
	  $(INSTALL_DATA) $$p $(DESTDIR)$(gxx_include_dir)/$$f; \
	done
	-@rm -f tmp-ilist
## FIXME: the obvious approach using lib_DATA doesn't work with
## automake 1.4.
	$(mkinstalldirs) $(DESTDIR)$(secdir)
	@for f in classpath.security libgcj.security; do \
	  echo " $(INSTALL_DATA) $(srcdir)/java/security/$$f $(DESTDIR)$(secdir)/$$f"; \
	  $(INSTALL_DATA) $(srcdir)/java/security/$$f $(DESTDIR)$(secdir)/$$f; \
	done
	$(INSTALL_DATA) $(srcdir)/java/util/logging/logging.properties $(DESTDIR)$(propdir)/logging.properties
## Install inner class headers.
	$(INSTALL_DATA) 'java/io/ObjectOutputStream$$PutField.h' $(DESTDIR)$(gxx_include_dir)/java/io/
	$(INSTALL_DATA) 'java/io/ObjectInputStream$$GetField.h' $(DESTDIR)$(gxx_include_dir)/java/io/
	$(INSTALL_DATA) 'java/nio/channels/Pipe$$SinkChannel.h' $(DESTDIR)$(gxx_include_dir)/java/nio/channels/
	$(INSTALL_DATA) 'java/nio/channels/Pipe$$SourceChannel.h' $(DESTDIR)$(gxx_include_dir)/java/nio/channels/
	$(INSTALL_DATA) 'java/lang/reflect/Proxy$$ProxyData.h' $(DESTDIR)$(gxx_include_dir)/java/lang/reflect/
	$(INSTALL_DATA) 'java/lang/reflect/Proxy$$ProxyType.h' $(DESTDIR)$(gxx_include_dir)/java/lang/reflect/
	$(INSTALL_DATA) 'gnu/java/net/PlainSocketImpl$$SocketInputStream.h' $(DESTDIR)$(gxx_include_dir)/gnu/java/net/
	$(INSTALL_DATA) 'gnu/java/net/PlainSocketImpl$$SocketOutputStream.h' $(DESTDIR)$(gxx_include_dir)/gnu/java/net/
	$(INSTALL_DATA) 'gnu/java/nio/PipeImpl$$SinkChannelImpl.h' $(DESTDIR)$(gxx_include_dir)/gnu/java/nio/
	$(INSTALL_DATA) 'gnu/java/nio/PipeImpl$$SourceChannelImpl.h' $(DESTDIR)$(gxx_include_dir)/gnu/java/nio/
## Don't install java/nio/DirectByteBufferImpl$$ReadWrite.h here. It's for internal use only.


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

gen_from_JIS_SOURCES = \
	gnu/gcj/convert/gen-from-JIS.c \
	gnu/gcj/convert/make-trie.c

gen_from_JIS_DEPENDENCIES = \
	gnu/gcj/convert/JIS0201.h \
	gnu/gcj/convert/JIS0208.h \
	gnu/gcj/convert/JIS0212.h

$(srcdir)/gnu/gcj/convert/JIS0208_to_Unicode.cc: ./gen-from-JIS$(EXEEXT)
	 ./gen-from-JIS JIS0208 >$(srcdir)/gnu/gcj/convert/JIS0208_to_Unicode.cc

$(srcdir)/gnu/gcj/convert/JIS0212_to_Unicode.cc: ./gen-from-JIS$(EXEEXT)
	 ./gen-from-JIS JIS0212 >$(srcdir)/gnu/gcj/convert/JIS0212_to_Unicode.cc

$(srcdir)/gnu/gcj/convert/Unicode_to_JIS.cc: ./gen-from-JIS$(EXEEXT)
	 ./gen-from-JIS toJIS >$(srcdir)/gnu/gcj/convert/Unicode_to_JIS.cc

# The Unicode consortium does not permit re-distributing the file JIS0201.TXT.
# You can get it from
# ftp://ftp.unicode.org/Public/MAPPINGS/OBSOLETE/EASTASIA/JIS/

$(srcdir)/gnu/gcj/convert/JIS0201.h: # gnu/gcj/convert/JIS0201.TXT
	echo '/* This file is automatically generated from Unicode tables */' > tmp-0201; \
	tr  -d '\r' <$(srcdir)/gnu/gcj/convert/JIS0201.TXT \
	| sed -n -e 's|\(0x..\).*\(0x....\).*#\(.*\)$$|MAP(0x00, \1, \2)  /*\3 */|p' \
	>> tmp-0201; \
	mv tmp-0201 $(srcdir)/gnu/gcj/convert/JIS0201.h

# The Unicode consortium does not permit re-distributing the file JIS0208.TXT.
# You can get it from
# ftp://ftp.unicode.org/Public/MAPPINGS/OBSOLETE/EASTASIA/JIS/

$(srcdir)/gnu/gcj/convert/JIS0208.h: # gnu/gcj/convert/JIS0208.TXT
	echo '/* This file is automatically generated from Unicode tables */' > tmp-0208; \
	tr  -d '\r' <$(srcdir)/gnu/gcj/convert/JIS0208.TXT \
	| sed -n -e 's|\(0x....\).*0x\(..\)\(..\).*\(0x....\).*#\(.*\)$$|MAP(0x\2, 0x\3, \4)  /*\5 */|p' \
	>> tmp-0208; \
	mv tmp-0208 $(srcdir)/gnu/gcj/convert/JIS0208.h

# The Unicode consortium does not permit re-distributing the file JIS0212.TXT.
# You can get it from
# ftp://ftp.unicode.org/Public/MAPPINGS/OBSOLETE/EASTASIA/JIS/

$(srcdir)/gnu/gcj/convert/JIS0212.h: # gnu/gcj/convert/JIS0212.TXT
	echo '/* This file is automatically generated from Unicode tables */' > tmp-0212; \
	tr  -d '\r' <$(srcdir)/gnu/gcj/convert/JIS0212.TXT \
	| sed -n -e 's|0x\(..\)\(..\).*\(0x....\).*#\(.*\)$$|MAP(0x\1, 0x\2, \3)  /*\4 */|p' \
	>> tmp-0212; \
	mv tmp-0212 $(srcdir)/gnu/gcj/convert/JIS0212.h

endif
endif 


jv_convert_SOURCES =
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
jv_convert_DEPENDENCIES = libgcj.la libgcj.spec

gcj_dbtool_SOURCES = gnu/gcj/tools/gcj_dbtool/Main.java \
	gnu/gcj/tools/gcj_dbtool/natMain.cc
## We need -nodefaultlibs because we want to avoid gcj's `-lgcj'.  We
## need this because we are explicitly using libtool to link using the
## `.la' file.
gcj_dbtool_LDFLAGS = --main=gnu.gcj.tools.gcj_dbtool.Main \
	-rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS)
gcj_dbtool_LINK = $(GCJLINK)
## We don't explicitly link in the libraries we need; libgcj.la brings
## in all dependencies.  We need the -L so that gcj can find libgcj
## with `-lgcj', but it must come first, otherwise the -L flags
## brought in from libgcj.la would cause the install directories to be
## searched before the build-tree ones, and we'd get errors because of
## different libraries with the same SONAME from picky linkers such as
## Solaris'.  FIXME: should be _libs on some systems.
gcj_dbtool_LDADD = -L$(here)/.libs libgcj.la
## Depend on the spec file to make sure it is up to date before
## linking this program.
gcj_dbtool_DEPENDENCIES = libgcj.la libgcj.spec

gij_SOURCES = 
## We need -nodefaultlibs because we want to avoid gcj's `-lgcj'.  We
## need this because we are explicitly using libtool to link using the
## `.la' file.
gij_LDFLAGS = -rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS)
gij_LINK = $(GCJLINK)
## See jv_convert_LDADD.
gij_LDADD = -L$(here)/.libs libgij.la
## Depend on the spec file to make sure it is up to date before
## linking this program.
gij_DEPENDENCIES = libgij.la

## This is a dummy definition.
grmic_SOURCES =
grmic_LDFLAGS = --main=gnu.java.rmi.rmic.RMIC \
	-rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS)
grmic_LINK = $(GCJLINK)
## See jv_convert_LDADD.
grmic_LDADD = -L$(here)/.libs libgcj.la
## Depend on the spec file to make sure it is up to date before
## linking this program.
grmic_DEPENDENCIES = libgcj.la libgcj.spec

## This is a dummy definition.
grmiregistry_SOURCES =
grmiregistry_LDFLAGS = --main=gnu.java.rmi.registry.RegistryImpl \
	-rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS)
grmiregistry_LINK = $(GCJLINK)
## See jv_convert_LDADD.
grmiregistry_LDADD = -L$(here)/.libs libgcj.la
## Depend on the spec file to make sure it is up to date before
## linking this program.
grmiregistry_DEPENDENCIES = libgcj.la libgcj.spec

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
gnu/gcj/convert/Input_UnicodeBig.java \
gnu/gcj/convert/Input_UnicodeLittle.java \
gnu/gcj/convert/Input_UTF8.java	\
gnu/gcj/convert/Input_iconv.java \
gnu/gcj/convert/IOConverter.java \
gnu/gcj/convert/Output_8859_1.java \
gnu/gcj/convert/Output_ASCII.java \
gnu/gcj/convert/Output_EUCJIS.java \
gnu/gcj/convert/Output_JavaSrc.java \
gnu/gcj/convert/Output_SJIS.java \
gnu/gcj/convert/Output_UnicodeLittleUnmarked.java \
gnu/gcj/convert/Output_UTF8.java \
gnu/gcj/convert/Output_iconv.java \
gnu/gcj/convert/UnicodeToBytes.java

## List of all .java files for which the .h file is maintained by
## hand.
special_java_source_files = java/lang/Class.java java/lang/Object.java

gnu_java_beans_source_files = \
gnu/java/beans/decoder/AbstractContext.java \
gnu/java/beans/decoder/AbstractCreatableObjectContext.java \
gnu/java/beans/decoder/AbstractElementHandler.java \
gnu/java/beans/decoder/AbstractObjectContext.java \
gnu/java/beans/decoder/ArrayContext.java \
gnu/java/beans/decoder/ArrayHandler.java \
gnu/java/beans/decoder/AssemblyException.java \
gnu/java/beans/decoder/BooleanHandler.java \
gnu/java/beans/decoder/ByteHandler.java \
gnu/java/beans/decoder/CharHandler.java \
gnu/java/beans/decoder/ClassHandler.java \
gnu/java/beans/decoder/ConstructorContext.java \
gnu/java/beans/decoder/Context.java \
gnu/java/beans/decoder/DecoderContext.java \
gnu/java/beans/decoder/DefaultExceptionListener.java \
gnu/java/beans/decoder/DoubleHandler.java \
gnu/java/beans/decoder/DummyContext.java \
gnu/java/beans/decoder/DummyHandler.java \
gnu/java/beans/decoder/ElementHandler.java \
gnu/java/beans/decoder/FloatHandler.java \
gnu/java/beans/decoder/GrowableArrayContext.java \
gnu/java/beans/decoder/IndexContext.java \
gnu/java/beans/decoder/IntHandler.java \
gnu/java/beans/decoder/JavaHandler.java \
gnu/java/beans/decoder/LongHandler.java \
gnu/java/beans/decoder/MethodContext.java \
gnu/java/beans/decoder/MethodFinder.java \
gnu/java/beans/decoder/NullHandler.java \
gnu/java/beans/decoder/ObjectContext.java \
gnu/java/beans/decoder/ObjectHandler.java \
gnu/java/beans/decoder/PersistenceParser.java \
gnu/java/beans/decoder/PropertyContext.java \
gnu/java/beans/decoder/ShortHandler.java \
gnu/java/beans/decoder/SimpleHandler.java \
gnu/java/beans/decoder/StaticMethodContext.java \
gnu/java/beans/decoder/StringHandler.java \
gnu/java/beans/decoder/VoidHandler.java \
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
gnu/java/beans/BeanInfoEmbryo.java \
gnu/java/beans/DummyAppletContext.java \
gnu/java/beans/DummyAppletStub.java \
gnu/java/beans/ExplicitBeanInfo.java \
gnu/java/beans/IntrospectionIncubator.java

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
gnu/java/awt/color/CieXyzConverter.java \
gnu/java/awt/color/ClutProfileConverter.java \
gnu/java/awt/color/ColorLookUpTable.java \
gnu/java/awt/color/ColorSpaceConverter.java \
gnu/java/awt/color/GrayProfileConverter.java \
gnu/java/awt/color/GrayScaleConverter.java \
gnu/java/awt/color/LinearRGBConverter.java \
gnu/java/awt/color/ProfileHeader.java \
gnu/java/awt/color/PyccConverter.java \
gnu/java/awt/color/RgbProfileConverter.java \
gnu/java/awt/color/SrgbConverter.java \
gnu/java/awt/color/TagEntry.java \
gnu/java/awt/color/ToneReproductionCurve.java \
gnu/java/awt/image/ImageDecoder.java \
gnu/java/awt/image/XBMDecoder.java \
gnu/java/awt/peer/EmbeddedWindowPeer.java \
gnu/java/awt/peer/GLightweightPeer.java \
gnu/java/awt/peer/ClasspathFontPeer.java \
gnu/java/awt/peer/ClasspathTextLayoutPeer.java \
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
java/awt/image/BandCombineOp.java \
java/awt/image/BandedSampleModel.java \
java/awt/image/AffineTransformOp.java \
java/awt/image/BufferedImage.java \
java/awt/image/BufferedImageFilter.java \
java/awt/image/ByteLookupTable.java \
java/awt/image/ColorConvertOp.java \
java/awt/image/ColorModel.java \
java/awt/image/ComponentColorModel.java \
java/awt/image/ComponentSampleModel.java \
java/awt/image/ConvolveOp.java \
java/awt/image/DataBuffer.java \
java/awt/image/DataBufferByte.java \
java/awt/image/DataBufferDouble.java \
java/awt/image/DataBufferFloat.java \
java/awt/image/DataBufferInt.java \
java/awt/image/DataBufferShort.java \
java/awt/image/DataBufferUShort.java \
java/awt/image/DirectColorModel.java \
java/awt/image/ImageConsumer.java \
java/awt/image/ImageObserver.java \
java/awt/image/ImageProducer.java \
java/awt/image/IndexColorModel.java \
java/awt/image/Kernel.java \
java/awt/image/LookupOp.java \
java/awt/image/LookupTable.java \
java/awt/image/MultiPixelPackedSampleModel.java \
java/awt/image/PackedColorModel.java \
java/awt/image/PixelInterleavedSampleModel.java \
java/awt/image/Raster.java \
java/awt/image/RasterOp.java \
java/awt/image/RescaleOp.java \
java/awt/image/SampleModel.java \
java/awt/image/ShortLookupTable.java \
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
java/beans/EventHandler.java \
java/beans/EventSetDescriptor.java \
java/beans/ExceptionListener.java \
java/beans/Expression.java \
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
java/beans/Statement.java \
java/beans/SimpleBeanInfo.java \
java/beans/VetoableChangeListener.java \
java/beans/VetoableChangeListenerProxy.java \
java/beans/VetoableChangeSupport.java \
java/beans/Visibility.java \
java/beans/AppletInitializer.java \
java/beans/XMLDecoder.java \
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
javax/swing/AbstractSpinnerModel.java \
javax/swing/plaf/basic/BasicArrowButton.java \
javax/swing/plaf/basic/BasicButtonListener.java \
javax/swing/plaf/basic/BasicButtonUI.java \
javax/swing/plaf/basic/BasicCheckBoxUI.java \
javax/swing/plaf/basic/BasicColorChooserUI.java \
javax/swing/plaf/basic/BasicComboBoxUI.java \
javax/swing/plaf/basic/BasicComboBoxEditor.java \
javax/swing/plaf/basic/BasicComboBoxRenderer.java \
javax/swing/plaf/basic/BasicComboPopup.java \
javax/swing/plaf/basic/BasicDesktopIconUI.java \
javax/swing/plaf/basic/BasicDesktopPaneUI.java \
javax/swing/plaf/basic/BasicEditorPaneUI.java \
javax/swing/plaf/basic/BasicFormattedTextFieldUI.java \
javax/swing/plaf/basic/BasicGraphicsUtils.java \
javax/swing/plaf/basic/BasicInternalFrameTitlePane.java \
javax/swing/plaf/basic/BasicInternalFrameUI.java \
javax/swing/plaf/basic/BasicLabelUI.java \
javax/swing/plaf/basic/BasicListUI.java \
javax/swing/plaf/basic/BasicMenuUI.java \
javax/swing/plaf/basic/BasicMenuBarUI.java \
javax/swing/plaf/basic/BasicMenuItemUI.java \
javax/swing/plaf/basic/BasicOptionPaneUI.java \
javax/swing/plaf/basic/BasicPanelUI.java \
javax/swing/plaf/basic/BasicPasswordFieldUI.java \
javax/swing/plaf/basic/BasicPopupMenuUI.java \
javax/swing/plaf/basic/BasicPopupMenuSeparatorUI.java \
javax/swing/plaf/basic/BasicProgressBarUI.java \
javax/swing/plaf/basic/BasicRootPaneUI.java \
javax/swing/plaf/basic/BasicRadioButtonUI.java \
javax/swing/plaf/basic/BasicRadioButtonMenuItemUI.java \
javax/swing/plaf/basic/BasicCheckBoxMenuItemUI.java \
javax/swing/plaf/basic/BasicScrollBarUI.java \
javax/swing/plaf/basic/BasicScrollPaneUI.java \
javax/swing/plaf/basic/BasicSeparatorUI.java \
javax/swing/plaf/basic/BasicSliderUI.java \
javax/swing/plaf/basic/BasicSpinnerUI.java \
javax/swing/plaf/basic/BasicSplitPaneDivider.java \
javax/swing/plaf/basic/BasicSplitPaneUI.java \
javax/swing/plaf/basic/BasicTabbedPaneUI.java \
javax/swing/plaf/basic/BasicTableUI.java \
javax/swing/plaf/basic/BasicTableHeaderUI.java \
javax/swing/plaf/basic/BasicTextAreaUI.java \
javax/swing/plaf/basic/BasicTextFieldUI.java \
javax/swing/plaf/basic/BasicTextPaneUI.java \
javax/swing/plaf/basic/BasicTextUI.java \
javax/swing/plaf/basic/BasicToggleButtonUI.java \
javax/swing/plaf/basic/BasicToolBarSeparatorUI.java \
javax/swing/plaf/basic/BasicToolBarUI.java \
javax/swing/plaf/basic/BasicToolTipUI.java \
javax/swing/plaf/basic/BasicTreeUI.java \
javax/swing/plaf/basic/BasicViewportUI.java \
javax/swing/plaf/basic/BasicLookAndFeel.java \
javax/swing/plaf/basic/BasicBorders.java \
javax/swing/plaf/basic/BasicIconFactory.java \
javax/swing/plaf/basic/ComboPopup.java \
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
javax/swing/plaf/metal/DefaultMetalTheme.java \
javax/swing/plaf/metal/MetalLookAndFeel.java \
javax/swing/plaf/metal/MetalTheme.java \
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
javax/swing/colorchooser/AbstractColorChooserPanel.java \
javax/swing/colorchooser/ColorChooserComponentFactory.java \
javax/swing/colorchooser/ColorSelectionModel.java \
javax/swing/colorchooser/DefaultColorSelectionModel.java \
javax/swing/colorchooser/DefaultPreviewPanel.java \
javax/swing/colorchooser/DefaultSwatchChooserPanel.java \
javax/swing/colorchooser/DefaultHSBChooserPanel.java \
javax/swing/colorchooser/DefaultRGBChooserPanel.java \
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
javax/swing/DefaultListModel.java \
javax/swing/DefaultListSelectionModel.java \
javax/swing/Icon.java \
javax/swing/ImageIcon.java \
javax/swing/InputMap.java \
javax/swing/InternalFrameFocusTraversalPolicy.java \
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
javax/swing/JSpinner.java \
javax/swing/JTabbedPane.java \
javax/swing/JTextArea.java \
javax/swing/JTextField.java \
javax/swing/JToggleButton.java \
javax/swing/JToolTip.java \
javax/swing/JTree.java \
javax/swing/JViewport.java \
javax/swing/JWindow.java \
javax/swing/KeyStroke.java \
javax/swing/LayoutFocusTraversalPolicy.java \
javax/swing/ListCellRenderer.java \
javax/swing/ListModel.java \
javax/swing/ListSelectionModel.java \
javax/swing/LookAndFeel.java \
javax/swing/Scrollable.java \
javax/swing/SortingFocusTraversalPolicy.java \
javax/swing/SpinnerDateModel.java \
javax/swing/SpinnerListModel.java \
javax/swing/SpinnerModel.java \
javax/swing/SpinnerNumberModel.java \
javax/swing/Spring.java \
javax/swing/SpringLayout.java \
javax/swing/SwingConstants.java \
javax/swing/SwingUtilities.java \
javax/swing/Timer.java \
javax/swing/ToolTipManager.java \
javax/swing/TransferHandler.java \
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
javax/swing/text/ComponentView.java \
javax/swing/text/DefaultCaret.java \
javax/swing/text/DefaultEditorKit.java \
javax/swing/text/DefaultHighlighter.java \
javax/swing/text/DefaultStyledDocument.java \
javax/swing/text/Document.java \
javax/swing/text/DocumentFilter.java \
javax/swing/text/EditorKit.java \
javax/swing/text/Element.java \
javax/swing/text/FieldView.java \
javax/swing/text/GapContent.java \
javax/swing/text/Highlighter.java \
javax/swing/text/JTextComponent.java \
javax/swing/text/Keymap.java \
javax/swing/text/LayeredHighlighter.java \
javax/swing/text/MutableAttributeSet.java \
javax/swing/text/NavigationFilter.java \
javax/swing/text/PasswordView.java \
javax/swing/text/PlainDocument.java \
javax/swing/text/PlainView.java \
javax/swing/text/Position.java \
javax/swing/text/Segment.java \
javax/swing/text/SimpleAttributeSet.java \
javax/swing/text/StringContent.java \
javax/swing/text/Style.java \
javax/swing/text/StyleConstants.java \
javax/swing/text/StyleContext.java \
javax/swing/text/StyledDocument.java \
javax/swing/text/StyledEditorKit.java \
javax/swing/text/TabableView.java \
javax/swing/text/TabExpander.java \
javax/swing/text/TabSet.java \
javax/swing/text/TabStop.java \
javax/swing/text/TextAction.java \
javax/swing/text/Utilities.java \
javax/swing/text/View.java \
javax/swing/text/ViewFactory.java \
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
gnu/java/rmi/server/RMIIncomingThread.java \
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

javax_imageio_source_files = \
javax/imageio/ImageWriteParam.java \
javax/imageio/ImageReader.java \
javax/imageio/ImageWriter.java \
javax/imageio/IIOException.java	\
javax/imageio/IIOParamController.java \
javax/imageio/ImageReadParam.java \
javax/imageio/IIOParam.java \
javax/imageio/spi/IIORegistry.java \
javax/imageio/spi/ImageWriterSpi.java \
javax/imageio/spi/ImageOutputStreamSpi.java \
javax/imageio/spi/ServiceRegistry.java \
javax/imageio/spi/ImageTranscoderSpi.java \
javax/imageio/spi/ImageReaderSpi.java \
javax/imageio/spi/IIOServiceProvider.java \
javax/imageio/spi/ImageReaderWriterSpi.java \
javax/imageio/spi/ImageInputStreamSpi.java \
javax/imageio/spi/RegisterableService.java \
javax/imageio/metadata/IIOAttr.java \
javax/imageio/metadata/IIONamedNodeMap.java \
javax/imageio/metadata/IIONodeList.java \
javax/imageio/metadata/IIOMetadataFormatImpl.java \
javax/imageio/metadata/IIOInvalidTreeException.java \
javax/imageio/metadata/IIOMetadataFormat.java \
javax/imageio/metadata/IIOMetadataController.java \
javax/imageio/metadata/IIOMetadataNode.java \
javax/imageio/metadata/IIOMetadata.java	\
javax/imageio/stream/FileImageOutputStream.java	\
javax/imageio/stream/ImageInputStream.java \
javax/imageio/stream/IIOByteBuffer.java	\
javax/imageio/stream/FileCacheImageInputStream.java \
javax/imageio/stream/ImageInputStreamImpl.java \
javax/imageio/stream/ImageOutputStreamImpl.java	\
javax/imageio/stream/MemoryCacheImageInputStream.java \
javax/imageio/stream/FileCacheImageOutputStream.java \
javax/imageio/stream/ImageOutputStream.java \
javax/imageio/stream/MemoryCacheImageOutputStream.java \
javax/imageio/stream/FileImageInputStream.java \
javax/imageio/event/IIOReadProgressListener.java \
javax/imageio/event/IIOWriteWarningListener.java \
javax/imageio/event/IIOReadWarningListener.java	\
javax/imageio/event/IIOReadUpdateListener.java \
javax/imageio/event/IIOWriteProgressListener.java \
javax/imageio/ImageTranscoder.java \
javax/imageio/ImageTypeSpecifier.java \
javax/imageio/ImageIO.java \
javax/imageio/IIOImage.java

javax_xml_source_files = \
javax/xml/xpath/XPathConstants.java \
javax/xml/xpath/XPathFunction.java \
javax/xml/xpath/XPathVariableResolver.java \
javax/xml/xpath/XPathExpressionException.java \
javax/xml/xpath/XPathFunctionResolver.java \
javax/xml/xpath/XPath.java \
javax/xml/xpath/XPathFactoryConfigurationException.java	\
javax/xml/xpath/XPathFactory.java \
javax/xml/xpath/XPathException.java \
javax/xml/xpath/XPathExpression.java \
javax/xml/xpath/XPathFunctionException.java \
javax/xml/validation/ValidatorHandler.java \
javax/xml/validation/Validator.java \
javax/xml/validation/TypeInfoProvider.java \
javax/xml/validation/SchemaFactory.java	\
javax/xml/validation/Schema.java \
javax/xml/parsers/SAXParserFactory.java	\
javax/xml/parsers/FactoryConfigurationError.java \
javax/xml/parsers/SAXParser.java \
javax/xml/parsers/DocumentBuilderFactory.java \
javax/xml/parsers/ParserConfigurationException.java \
javax/xml/parsers/DocumentBuilder.java \
javax/xml/datatype/DatatypeFactory.java	\
javax/xml/datatype/XMLGregorianCalendar.java \
javax/xml/datatype/Duration.java \
javax/xml/datatype/DatatypeConfigurationException.java \
javax/xml/datatype/DatatypeConstants.java \
javax/xml/XMLConstants.java \
javax/xml/namespace/NamespaceContext.java \
javax/xml/namespace/QName.java \
javax/xml/transform/TransformerException.java \
javax/xml/transform/TransformerFactoryConfigurationError.java \
javax/xml/transform/sax/SAXResult.java \
javax/xml/transform/sax/TransformerHandler.java	\
javax/xml/transform/sax/SAXTransformerFactory.java \
javax/xml/transform/sax/SAXSource.java \
javax/xml/transform/sax/TemplatesHandler.java \
javax/xml/transform/OutputKeys.java \
javax/xml/transform/stream/StreamResult.java \
javax/xml/transform/stream/StreamSource.java \
javax/xml/transform/Source.java	\
javax/xml/transform/SourceLocator.java \
javax/xml/transform/ErrorListener.java \
javax/xml/transform/TransformerConfigurationException.java \
javax/xml/transform/Templates.java \
javax/xml/transform/Result.java	\
javax/xml/transform/URIResolver.java \
javax/xml/transform/dom/DOMSource.java \
javax/xml/transform/dom/DOMLocator.java	\
javax/xml/transform/dom/DOMResult.java \
javax/xml/transform/Transformer.java \
javax/xml/transform/TransformerFactory.java

javax_source_files = \
javax/net/VanillaSocketFactory.java \
javax/net/ssl/TrustManagerFactorySpi.java \
javax/net/ssl/SSLKeyException.java \
javax/net/ssl/TrustManager.java	\
javax/net/ssl/SSLServerSocket.java \
javax/net/ssl/TrustManagerFactory.java \
javax/net/ssl/KeyManager.java \
javax/net/ssl/SSLSocketFactory.java \
javax/net/ssl/SSLPermission.java \
javax/net/ssl/SSLContext.java \
javax/net/ssl/TrivialHostnameVerifier.java \
javax/net/ssl/X509KeyManager.java \
javax/net/ssl/SSLServerSocketFactory.java \
javax/net/ssl/SSLSessionBindingListener.java \
javax/net/ssl/SSLSessionContext.java \
javax/net/ssl/KeyManagerFactory.java \
javax/net/ssl/SSLProtocolException.java	\
javax/net/ssl/SSLSocket.java \
javax/net/ssl/HostnameVerifier.java \
javax/net/ssl/HandshakeCompletedListener.java \
javax/net/ssl/HttpsURLConnection.java \
javax/net/ssl/KeyManagerFactorySpi.java	\
javax/net/ssl/SSLHandshakeException.java \
javax/net/ssl/SSLPeerUnverifiedException.java \
javax/net/ssl/X509TrustManager.java \
javax/net/ssl/SSLSession.java \
javax/net/ssl/SSLContextSpi.java \
javax/net/ssl/HandshakeCompletedEvent.java \
javax/net/ssl/SSLSessionBindingEvent.java \
javax/net/ssl/SSLException.java	\
javax/net/ssl/ManagerFactoryParameters.java \
javax/net/SocketFactory.java \
javax/net/VanillaServerSocketFactory.java \
javax/net/ServerSocketFactory.java \
javax/print/attribute/HashPrintServiceAttributeSet.java	\
javax/print/attribute/Attribute.java \
javax/print/attribute/DocAttributeSet.java \
javax/print/attribute/PrintRequestAttributeSet.java \
javax/print/attribute/standard/Chromaticity.java \
javax/print/attribute/standard/Destination.java \
javax/print/attribute/standard/MediaPrintableArea.java \
javax/print/attribute/standard/MediaSize.java \
javax/print/attribute/standard/NumberOfInterveningJobs.java \
javax/print/attribute/standard/PrinterMakeAndModel.java	\
javax/print/attribute/standard/PrinterStateReason.java \
javax/print/attribute/standard/JobImpressions.java \
javax/print/attribute/standard/JobHoldUntil.java \
javax/print/attribute/standard/JobKOctetsSupported.java	\
javax/print/attribute/standard/JobPriority.java	\
javax/print/attribute/standard/QueuedJobCount.java \
javax/print/attribute/standard/JobOriginatingUserName.java \
javax/print/attribute/standard/PrinterName.java	\
javax/print/attribute/standard/PDLOverrideSupported.java \
javax/print/attribute/standard/PrinterStateReasons.java	\
javax/print/attribute/standard/JobMediaSheets.java \
javax/print/attribute/standard/JobKOctets.java \
javax/print/attribute/standard/MultipleDocumentHandling.java \
javax/print/attribute/standard/PrinterMessageFromOperator.java \
javax/print/attribute/standard/Severity.java \
javax/print/attribute/standard/ReferenceUriSchemesSupported.java \
javax/print/attribute/standard/DocumentName.java \
javax/print/attribute/standard/PrinterMoreInfo.java \
javax/print/attribute/standard/JobMessageFromOperator.java \
javax/print/attribute/standard/JobStateReasons.java \
javax/print/attribute/standard/PrinterIsAcceptingJobs.java \
javax/print/attribute/standard/DateTimeAtProcessing.java \
javax/print/attribute/standard/PagesPerMinute.java \
javax/print/attribute/standard/PrinterResolution.java \
javax/print/attribute/standard/JobMediaSheetsSupported.java \
javax/print/attribute/standard/NumberUpSupported.java \
javax/print/attribute/standard/Finishings.java \
javax/print/attribute/standard/DateTimeAtCompleted.java	\
javax/print/attribute/standard/NumberOfDocuments.java \
javax/print/attribute/standard/PrinterInfo.java	\
javax/print/attribute/standard/PrintQuality.java \
javax/print/attribute/standard/JobSheets.java \
javax/print/attribute/standard/Media.java \
javax/print/attribute/standard/Fidelity.java \
javax/print/attribute/standard/PrinterLocation.java \
javax/print/attribute/standard/PrinterMoreInfoManufacturer.java	\
javax/print/attribute/standard/OrientationRequested.java \
javax/print/attribute/standard/JobState.java \
javax/print/attribute/standard/DateTimeAtCreation.java \
javax/print/attribute/standard/PresentationDirection.java \
javax/print/attribute/standard/JobName.java \
javax/print/attribute/standard/PagesPerMinuteColor.java	\
javax/print/attribute/standard/CopiesSupported.java \
javax/print/attribute/standard/PrinterURI.java \
javax/print/attribute/standard/PageRanges.java \
javax/print/attribute/standard/RequestingUserName.java \
javax/print/attribute/standard/JobPrioritySupported.java \
javax/print/attribute/standard/JobMediaSheetsCompleted.java \
javax/print/attribute/standard/JobKOctetsProcessed.java	\
javax/print/attribute/standard/JobImpressionsCompleted.java \
javax/print/attribute/standard/Compression.java	\
javax/print/attribute/standard/JobImpressionsSupported.java \
javax/print/attribute/standard/Sides.java \
javax/print/attribute/standard/JobStateReason.java \
javax/print/attribute/standard/SheetCollate.java \
javax/print/attribute/standard/Copies.java \
javax/print/attribute/standard/MediaSizeName.java \
javax/print/attribute/standard/OutputDeviceAssigned.java \
javax/print/attribute/standard/PrinterState.java \
javax/print/attribute/standard/NumberUp.java \
javax/print/attribute/standard/ColorSupported.java \
javax/print/attribute/HashAttributeSet.java \
javax/print/attribute/SetOfIntegerSyntax.java \
javax/print/attribute/HashDocAttributeSet.java \
javax/print/attribute/PrintRequestAttribute.java \
javax/print/attribute/DateTimeSyntax.java \
javax/print/attribute/PrintServiceAttribute.java \
javax/print/attribute/UnmodifiableSetException.java \
javax/print/attribute/HashPrintJobAttributeSet.java \
javax/print/attribute/PrintJobAttribute.java \
javax/print/attribute/AttributeSet.java	\
javax/print/attribute/ResolutionSyntax.java \
javax/print/attribute/URISyntax.java \
javax/print/attribute/AttributeSetUtilities.java \
javax/print/attribute/HashPrintRequestAttributeSet.java	\
javax/print/attribute/TextSyntax.java \
javax/print/attribute/PrintJobAttributeSet.java	\
javax/print/attribute/SupportedValuesAttribute.java \
javax/print/attribute/EnumSyntax.java \
javax/print/attribute/DocAttribute.java	\
javax/print/attribute/Size2DSyntax.java	\
javax/print/attribute/IntegerSyntax.java \
javax/print/attribute/PrintServiceAttributeSet.java \
javax/print/PrintService.java \
javax/print/DocFlavor.java \
javax/print/DocPrintJob.java \
javax/print/event/PrintEvent.java \
javax/print/event/PrintJobAdapter.java \
javax/print/event/PrintServiceAttributeEvent.java \
javax/print/event/PrintJobAttributeEvent.java \
javax/print/event/PrintServiceAttributeListener.java \
javax/print/event/PrintJobEvent.java \
javax/print/event/PrintJobListener.java	\
javax/print/event/PrintJobAttributeListener.java \
javax/print/PrintServiceLookup.java \
javax/print/URIException.java \
javax/print/StreamPrintService.java \
javax/print/MultiDoc.java \
javax/print/FlavorException.java \
javax/print/Doc.java \
javax/print/MultiDocPrintJob.java \
javax/print/CancelablePrintJob.java \
javax/print/MultiDocPrintService.java \
javax/print/ServiceUIFactory.java \
javax/print/AttributeException.java \
javax/print/PrintException.java	\
javax/naming/ServiceUnavailableException.java \
javax/naming/directory/Attribute.java \
javax/naming/directory/SchemaViolationException.java \
javax/naming/directory/InitialDirContext.java \
javax/naming/directory/BasicAttribute.java \
javax/naming/directory/NoSuchAttributeException.java \
javax/naming/directory/InvalidAttributesException.java \
javax/naming/directory/InvalidAttributeValueException.java \
javax/naming/directory/BasicAttributes.java \
javax/naming/directory/AttributeModificationException.java \
javax/naming/directory/InvalidAttributeIdentifierException.java	\
javax/naming/directory/DirContext.java \
javax/naming/directory/Attributes.java \
javax/naming/directory/InvalidSearchControlsException.java \
javax/naming/directory/SearchControls.java \
javax/naming/directory/AttributeInUseException.java \
javax/naming/directory/InvalidSearchFilterException.java \
javax/naming/directory/ModificationItem.java \
javax/naming/directory/SearchResult.java \
javax/naming/Binding.java \
javax/naming/ContextNotEmptyException.java \
javax/naming/Referenceable.java	\
javax/naming/InterruptedNamingException.java \
javax/naming/CommunicationException.java \
javax/naming/LinkException.java	\
javax/naming/BinaryRefAddr.java	\
javax/naming/LinkLoopException.java \
javax/naming/AuthenticationException.java \
javax/naming/NameAlreadyBoundException.java \
javax/naming/MalformedLinkException.java \
javax/naming/NotContextException.java \
javax/naming/spi/DirStateFactory.java \
javax/naming/spi/ResolveResult.java \
javax/naming/spi/ObjectFactoryBuilder.java \
javax/naming/spi/DirObjectFactory.java \
javax/naming/spi/DirectoryManager.java \
javax/naming/spi/InitialContextFactoryBuilder.java \
javax/naming/spi/ObjectFactory.java \
javax/naming/spi/Resolver.java \
javax/naming/spi/StateFactory.java \
javax/naming/spi/InitialContextFactory.java \
javax/naming/spi/NamingManager.java \
javax/naming/InvalidNameException.java \
javax/naming/NamingEnumeration.java \
javax/naming/LinkRef.java \
javax/naming/event/NamingListener.java \
javax/naming/event/NamingEvent.java \
javax/naming/event/NamespaceChangeListener.java	\
javax/naming/event/ObjectChangeListener.java \
javax/naming/event/EventContext.java \
javax/naming/event/NamingExceptionEvent.java \
javax/naming/event/EventDirContext.java	\
javax/naming/NamingException.java \
javax/naming/CannotProceedException.java \
javax/naming/PartialResultException.java \
javax/naming/TimeLimitExceededException.java \
javax/naming/NoPermissionException.java	\
javax/naming/CompoundName.java \
javax/naming/SizeLimitExceededException.java \
javax/naming/NoInitialContextException.java \
javax/naming/NameNotFoundException.java	\
javax/naming/InitialContext.java \
javax/naming/NamingSecurityException.java \
javax/naming/Context.java \
javax/naming/ConfigurationException.java \
javax/naming/InsufficientResourcesException.java \
javax/naming/AuthenticationNotSupportedException.java \
javax/naming/StringRefAddr.java	\
javax/naming/ldap/InitialLdapContext.java \
javax/naming/ldap/ExtendedRequest.java \
javax/naming/ldap/Control.java \
javax/naming/ldap/UnsolicitedNotificationListener.java \
javax/naming/ldap/LdapReferralException.java \
javax/naming/ldap/UnsolicitedNotificationEvent.java \
javax/naming/ldap/HasControls.java \
javax/naming/ldap/LdapContext.java \
javax/naming/ldap/ExtendedResponse.java	\
javax/naming/ldap/UnsolicitedNotification.java \
javax/naming/ldap/ControlFactory.java \
javax/naming/OperationNotSupportedException.java \
javax/naming/ReferralException.java \
javax/naming/LimitExceededException.java \
javax/naming/Reference.java \
javax/naming/NameParser.java \
javax/naming/NameClassPair.java	\
javax/naming/RefAddr.java \
javax/naming/CompositeName.java	\
javax/naming/Name.java \
javax/security/cert/CertificateNotYetValidException.java \
javax/security/cert/Certificate.java \
javax/security/cert/X509Certificate.java \
javax/security/cert/X509CertBridge.java	\
javax/security/cert/CertificateEncodingException.java \
javax/security/cert/CertificateExpiredException.java \
javax/security/cert/CertificateException.java \
javax/security/cert/CertificateParsingException.java \
javax/security/auth/Destroyable.java \
javax/security/auth/x500/X500PrivateCredential.java \
javax/security/auth/x500/X500Principal.java \
javax/security/auth/spi/LoginModule.java \
javax/security/auth/DestroyFailedException.java	\
javax/security/auth/SubjectDomainCombiner.java \
javax/security/auth/AuthPermission.java	\
javax/security/auth/login/CredentialExpiredException.java \
javax/security/auth/login/NullConfiguration.java \
javax/security/auth/login/AccountExpiredException.java \
javax/security/auth/login/LoginException.java \
javax/security/auth/login/AppConfigurationEntry.java \
javax/security/auth/login/Configuration.java \
javax/security/auth/login/LoginContext.java \
javax/security/auth/login/FailedLoginException.java \
javax/security/auth/PrivateCredentialPermission.java \
javax/security/auth/Subject.java \
javax/security/auth/Policy.java	\
javax/security/auth/Refreshable.java \
javax/security/auth/callback/TextInputCallback.java \
javax/security/auth/callback/LanguageCallback.java \
javax/security/auth/callback/TextOutputCallback.java \
javax/security/auth/callback/UnsupportedCallbackException.java \
javax/security/auth/callback/Callback.java \
javax/security/auth/callback/ConfirmationCallback.java \
javax/security/auth/callback/ChoiceCallback.java \
javax/security/auth/callback/NameCallback.java \
javax/security/auth/callback/PasswordCallback.java \
javax/security/auth/callback/CallbackHandler.java \
javax/security/auth/RefreshFailedException.java	\
javax/security/sasl/RealmCallback.java \
javax/security/sasl/SaslClient.java \
javax/security/sasl/RealmChoiceCallback.java \
javax/security/sasl/AuthenticationException.java \
javax/security/sasl/Sasl.java \
javax/security/sasl/SaslClientFactory.java \
javax/security/sasl/AuthorizeCallback.java \
javax/security/sasl/SaslServerFactory.java \
javax/security/sasl/SaslException.java \
javax/security/sasl/SaslServer.java \
javax/transaction/TransactionRolledbackException.java \
javax/transaction/Synchronization.java \
javax/transaction/TransactionRequiredException.java \
javax/transaction/RollbackException.java \
javax/transaction/Status.java \
javax/transaction/HeuristicMixedException.java \
javax/transaction/HeuristicRollbackException.java \
javax/transaction/SystemException.java \
javax/transaction/UserTransaction.java \
javax/transaction/NotSupportedException.java \
javax/transaction/HeuristicCommitException.java	\
javax/transaction/Transaction.java \
javax/transaction/TransactionManager.java \
javax/transaction/InvalidTransactionException.java \
javax/transaction/xa/XAException.java \
javax/transaction/xa/XAResource.java \
javax/transaction/xa/Xid.java \
javax/accessibility/AccessibleValue.java \
javax/accessibility/AccessibleBundle.java \
javax/accessibility/AccessibleContext.java \
javax/accessibility/AccessibleRelationSet.java \
javax/accessibility/Accessible.java \
javax/accessibility/AccessibleTableModelChange.java \
javax/accessibility/AccessibleState.java \
javax/accessibility/AccessibleIcon.java	\
javax/accessibility/AccessibleHypertext.java \
javax/accessibility/AccessibleComponent.java \
javax/accessibility/AccessibleText.java	\
javax/accessibility/AccessibleRole.java	\
javax/accessibility/AccessibleEditableText.java	\
javax/accessibility/AccessibleRelation.java \
javax/accessibility/AccessibleTable.java \
javax/accessibility/AccessibleStateSet.java \
javax/accessibility/AccessibleSelection.java \
javax/accessibility/AccessibleExtendedTable.java \
javax/accessibility/AccessibleResourceBundle.java \
javax/accessibility/AccessibleExtendedComponent.java \
javax/accessibility/AccessibleHyperlink.java \
javax/accessibility/AccessibleKeyBinding.java \
javax/accessibility/AccessibleAction.java \
javax/sql/ConnectionEventListener.java \
javax/sql/RowSetEvent.java \
javax/sql/ConnectionPoolDataSource.java	\
javax/sql/RowSetWriter.java \
javax/sql/XAConnection.java \
javax/sql/RowSet.java \
javax/sql/DataSource.java \
javax/sql/RowSetInternal.java \
javax/sql/RowSetReader.java \
javax/sql/RowSetMetaData.java \
javax/sql/PooledConnection.java	\
javax/sql/XADataSource.java \
javax/sql/RowSetListener.java \
javax/sql/ConnectionEvent.java \
javax/crypto/Cipher.java \
javax/crypto/IllegalBlockSizeException.java \
javax/crypto/MacSpi.java \
javax/crypto/SecretKeyFactorySpi.java \
javax/crypto/ShortBufferException.java \
javax/crypto/KeyAgreement.java \
javax/crypto/ExemptionMechanism.java \
javax/crypto/CipherOutputStream.java \
javax/crypto/SealedObject.java \
javax/crypto/NullCipher.java \
javax/crypto/KeyAgreementSpi.java \
javax/crypto/SecretKeyFactory.java \
javax/crypto/SecretKey.java \
javax/crypto/NullCipherImpl.java \
javax/crypto/spec/DESKeySpec.java \
javax/crypto/spec/DHPrivateKeySpec.java	\
javax/crypto/spec/DHParameterSpec.java \
javax/crypto/spec/PBEParameterSpec.java	\
javax/crypto/spec/DESedeKeySpec.java \
javax/crypto/spec/DHGenParameterSpec.java \
javax/crypto/spec/DHPublicKeySpec.java \
javax/crypto/spec/PBEKeySpec.java \
javax/crypto/spec/RC5ParameterSpec.java	\
javax/crypto/spec/RC2ParameterSpec.java	\
javax/crypto/spec/IvParameterSpec.java \
javax/crypto/spec/SecretKeySpec.java \
javax/crypto/interfaces/DHPublicKey.java \
javax/crypto/interfaces/DHKey.java \
javax/crypto/interfaces/DHPrivateKey.java \
javax/crypto/interfaces/PBEKey.java \
javax/crypto/NoSuchPaddingException.java \
javax/crypto/KeyGeneratorSpi.java \
javax/crypto/BadPaddingException.java \
javax/crypto/CipherSpi.java \
javax/crypto/KeyGenerator.java \
javax/crypto/CipherInputStream.java \
javax/crypto/ExemptionMechanismException.java \
javax/crypto/ExemptionMechanismSpi.java	\
javax/crypto/EncryptedPrivateKeyInfo.java \
javax/crypto/Mac.java

org_source_files = \
org/ietf/jgss/GSSManager.java \
org/ietf/jgss/GSSException.java	\
org/ietf/jgss/MessageProp.java \
org/ietf/jgss/GSSName.java \
org/ietf/jgss/GSSContext.java \
org/ietf/jgss/Oid.java \
org/ietf/jgss/GSSCredential.java \
org/ietf/jgss/ChannelBinding.java

gnu_xml_source_files = \
gnu/xml/xpath/EqualityExpr.java	\
gnu/xml/xpath/NumberFunction.java \
gnu/xml/xpath/StringLengthFunction.java	\
gnu/xml/xpath/NameFunction.java	\
gnu/xml/xpath/SumFunction.java \
gnu/xml/xpath/Pattern.java \
gnu/xml/xpath/StringFunction.java \
gnu/xml/xpath/NegativeExpr.java	\
gnu/xml/xpath/UnionExpr.java \
gnu/xml/xpath/SubstringFunction.java \
gnu/xml/xpath/Expr.java	\
gnu/xml/xpath/XPathFactoryImpl.java \
gnu/xml/xpath/NormalizeSpaceFunction.java \
gnu/xml/xpath/Constant.java \
gnu/xml/xpath/Predicate.java \
gnu/xml/xpath/Path.java	\
gnu/xml/xpath/OrExpr.java \
gnu/xml/xpath/LangFunction.java	\
gnu/xml/xpath/StartsWithFunction.java \
gnu/xml/xpath/SubstringAfterFunction.java \
gnu/xml/xpath/FloorFunction.java \
gnu/xml/xpath/NotFunction.java \
gnu/xml/xpath/Root.java	\
gnu/xml/xpath/PositionFunction.java \
gnu/xml/xpath/VariableReference.java \
gnu/xml/xpath/DocumentOrderComparator.java \
gnu/xml/xpath/LocalNameFunction.java \
gnu/xml/xpath/NamespaceUriFunction.java	\
gnu/xml/xpath/TranslateFunction.java \
gnu/xml/xpath/FalseFunction.java \
gnu/xml/xpath/AndExpr.java \
gnu/xml/xpath/XPathParser.java \
gnu/xml/xpath/NamespaceTest.java \
gnu/xml/xpath/ConcatFunction.java \
gnu/xml/xpath/NameTest.java \
gnu/xml/xpath/CountFunction.java \
gnu/xml/xpath/IdFunction.java \
gnu/xml/xpath/LastFunction.java	\
gnu/xml/xpath/XPathTokenizer.java \
gnu/xml/xpath/Steps.java \
gnu/xml/xpath/TrueFunction.java	\
gnu/xml/xpath/BooleanFunction.java \
gnu/xml/xpath/ParenthesizedExpr.java \
gnu/xml/xpath/XPathImpl.java \
gnu/xml/xpath/Selector.java \
gnu/xml/xpath/RoundFunction.java \
gnu/xml/xpath/SubstringBeforeFunction.java \
gnu/xml/xpath/Function.java \
gnu/xml/xpath/CeilingFunction.java \
gnu/xml/xpath/RelationalExpr.java \
gnu/xml/xpath/FunctionCall.java	\
gnu/xml/xpath/NodeTypeTest.java	\
gnu/xml/xpath/ArithmeticExpr.java \
gnu/xml/xpath/Test.java	\
gnu/xml/xpath/ContainsFunction.java \
gnu/xml/pipeline/EventFilter.java \
gnu/xml/pipeline/NSFilter.java \
gnu/xml/pipeline/XsltFilter.java \
gnu/xml/pipeline/ValidationConsumer.java \
gnu/xml/pipeline/PipelineFactory.java \
gnu/xml/pipeline/TextConsumer.java \
gnu/xml/pipeline/LinkFilter.java \
gnu/xml/pipeline/TeeConsumer.java \
gnu/xml/pipeline/DomConsumer.java \
gnu/xml/pipeline/EventConsumer.java \
gnu/xml/pipeline/WellFormednessFilter.java \
gnu/xml/pipeline/XIncludeFilter.java \
gnu/xml/pipeline/CallFilter.java \
gnu/xml/aelfred2/XmlParser.java	\
gnu/xml/aelfred2/XmlReader.java	\
gnu/xml/aelfred2/JAXPFactory.java \
gnu/xml/aelfred2/ContentHandler2.java \
gnu/xml/aelfred2/SAXDriver.java	\
gnu/xml/util/XCat.java \
gnu/xml/util/DomParser.java \
gnu/xml/util/XMLWriter.java \
gnu/xml/util/Resolver.java \
gnu/xml/util/DoParse.java \
gnu/xml/util/XHTMLWriter.java \
gnu/xml/util/SAXNullTransformerFactory.java \
gnu/xml/dom/DomXPathNSResolver.java \
gnu/xml/dom/ls/FilteredSAXEventSink.java \
gnu/xml/dom/ls/DomLSException.java \
gnu/xml/dom/ls/DomLSSerializer.java \
gnu/xml/dom/ls/DomLSInput.java \
gnu/xml/dom/ls/DomLSOutput.java	\
gnu/xml/dom/ls/SAXEventSink.java \
gnu/xml/dom/ls/DomLSParser.java	\
gnu/xml/dom/ls/ReaderInputStream.java \
gnu/xml/dom/ls/WriterOutputStream.java \
gnu/xml/dom/DomElement.java \
gnu/xml/dom/DomNsNode.java \
gnu/xml/dom/DomAttr.java \
gnu/xml/dom/DTDAttributeTypeInfo.java \
gnu/xml/dom/DomDocumentBuilder.java \
gnu/xml/dom/DTDElementTypeInfo.java \
gnu/xml/dom/DomDocument.java \
gnu/xml/dom/DomXPathExpression.java \
gnu/xml/dom/DomDocumentConfiguration.java \
gnu/xml/dom/DomDocumentFragment.java \
gnu/xml/dom/DomProcessingInstruction.java \
gnu/xml/dom/Consumer.java \
gnu/xml/dom/DomComment.java \
gnu/xml/dom/DomCharacterData.java \
gnu/xml/dom/DomExtern.java \
gnu/xml/dom/DomEntityReference.java \
gnu/xml/dom/DomNamedNodeMap.java \
gnu/xml/dom/ImplementationList.java \
gnu/xml/dom/ImplementationSource.java \
gnu/xml/dom/DomImpl.java \
gnu/xml/dom/DomDoctype.java \
gnu/xml/dom/DomNode.java \
gnu/xml/dom/DomXPathResult.java	\
gnu/xml/dom/DomDocumentBuilderFactory.java \
gnu/xml/dom/DomText.java \
gnu/xml/dom/DomNSResolverContext.java \
gnu/xml/dom/DomNodeIterator.java \
gnu/xml/dom/DomNotation.java \
gnu/xml/dom/JAXPFactory.java \
gnu/xml/dom/DomIterator.java \
gnu/xml/dom/DomEvent.java \
gnu/xml/dom/DomEntity.java \
gnu/xml/dom/DomCDATASection.java \
gnu/xml/dom/DomDOMException.java \
gnu/xml/transform/GenerateIdFunction.java \
gnu/xml/transform/FormatNumberFunction.java \
gnu/xml/transform/ValueOfNode.java \
gnu/xml/transform/ApplyImportsNode.java	\
gnu/xml/transform/WithParam.java \
gnu/xml/transform/DocumentFunction.java	\
gnu/xml/transform/SAXSerializer.java \
gnu/xml/transform/FunctionAvailableFunction.java \
gnu/xml/transform/ElementAvailableFunction.java	\
gnu/xml/transform/IfNode.java \
gnu/xml/transform/ApplyTemplatesNode.java \
gnu/xml/transform/SortKey.java \
gnu/xml/transform/Key.java \
gnu/xml/transform/ElementNode.java \
gnu/xml/transform/CurrentFunction.java \
gnu/xml/transform/XSLComparator.java \
gnu/xml/transform/CallTemplateNode.java	\
gnu/xml/transform/TemplatesImpl.java \
gnu/xml/transform/ChooseNode.java \
gnu/xml/transform/NumberNode.java \
gnu/xml/transform/CopyOfNode.java \
gnu/xml/transform/ParameterNode.java \
gnu/xml/transform/TransformerFactoryImpl.java \
gnu/xml/transform/TextNode.java	\
gnu/xml/transform/CopyNode.java	\
gnu/xml/transform/SystemPropertyFunction.java \
gnu/xml/transform/Stylesheet.java \
gnu/xml/transform/ProcessingInstructionNode.java \
gnu/xml/transform/UnparsedEntityUriFunction.java \
gnu/xml/transform/URIResolverEntityResolver.java \
gnu/xml/transform/ErrorListenerErrorHandler.java \
gnu/xml/transform/Bindings.java	\
gnu/xml/transform/AttributeSet.java \
gnu/xml/transform/Template.java	\
gnu/xml/transform/MessageNode.java \
gnu/xml/transform/LiteralNode.java \
gnu/xml/transform/StreamSerializer.java	\
gnu/xml/transform/XSLURIResolver.java \
gnu/xml/transform/AttributeNode.java \
gnu/xml/transform/CommentNode.java \
gnu/xml/transform/ForEachNode.java \
gnu/xml/transform/KeyFunction.java \
gnu/xml/transform/TransformerImpl.java \
gnu/xml/transform/WhenNode.java	\
gnu/xml/transform/NodeNumberNode.java \
gnu/xml/transform/OtherwiseNode.java \
gnu/xml/transform/AbstractNumberNode.java \
gnu/xml/transform/TemplateNode.java \
gnu/xml/transform/TransformerOutputProperties.java \
gnu/xml/transform/DOMSourceLocator.java

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
java/lang/VMCompiler.java \
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
java/util/VMTimeZone.java	\
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
## into core_java_source_files.  Please put files from gnu/gcj/convert 
## into convert_source_files and files from java/awt into
## awt_java_source_files.  If the .java file has a hand-maintained
## header, please list it in special_java_source_files.
ordinary_java_source_files = $(core_java_source_files) \
gnu/classpath/ServiceFactory.java \
gnu/classpath/ServiceProviderLoadingAction.java \
gnu/gcj/Core.java \
gnu/gcj/RawData.java \
gnu/gcj/RawDataManaged.java \
gnu/gcj/io/DefaultMimeTypes.java \
gnu/gcj/io/MimeTypes.java \
gnu/gcj/io/SimpleSHSStream.java	\
gnu/gcj/runtime/BootClassLoader.java \
gnu/gcj/runtime/ExtensionClassLoader.java \
gnu/gcj/runtime/FileDeleter.java \
gnu/gcj/runtime/FinalizerThread.java \
gnu/gcj/runtime/HelperClassLoader.java \
gnu/gcj/runtime/JNIWeakRef.java \
gnu/gcj/runtime/MethodRef.java \
gnu/gcj/runtime/NameFinder.java \
gnu/gcj/runtime/PersistentByteMap.java \
gnu/gcj/runtime/SharedLibHelper.java \
gnu/gcj/runtime/SharedLibLoader.java \
gnu/gcj/runtime/StringBuffer.java \
gnu/gcj/runtime/StackTrace.java \
gnu/gcj/runtime/SystemClassLoader.java \
gnu/gcj/util/Debug.java \
gnu/java/io/ASN1ParsingException.java \
gnu/java/io/Base64InputStream.java \
gnu/java/io/ClassLoaderObjectInputStream.java \
gnu/java/io/NullOutputStream.java \
gnu/java/io/ObjectIdentityWrapper.java \
gnu/java/lang/ArrayHelper.java \
gnu/java/lang/ClassHelper.java \
gnu/java/lang/MainThread.java \
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
gnu/java/net/BASE64.java \
gnu/java/net/CRLFInputStream.java \
gnu/java/net/CRLFOutputStream.java \
gnu/java/net/DefaultContentHandlerFactory.java \
gnu/java/net/EmptyX509TrustManager.java \
gnu/java/net/GetLocalHostAction.java \
gnu/java/net/HeaderFieldHelper.java \
gnu/java/net/LineInputStream.java \
gnu/java/net/PlainDatagramSocketImpl.java \
gnu/java/net/PlainSocketImpl.java \
gnu/java/net/URLParseError.java \
gnu/java/net/protocol/core/Connection.java \
gnu/java/net/protocol/core/CoreInputStream.java \
gnu/java/net/protocol/core/Handler.java \
gnu/java/net/protocol/file/Connection.java \
gnu/java/net/protocol/file/Handler.java \
gnu/java/net/protocol/ftp/ActiveModeDTP.java \
gnu/java/net/protocol/ftp/BlockInputStream.java \
gnu/java/net/protocol/ftp/BlockOutputStream.java \
gnu/java/net/protocol/ftp/CompressedInputStream.java \
gnu/java/net/protocol/ftp/CompressedOutputStream.java \
gnu/java/net/protocol/ftp/DTP.java \
gnu/java/net/protocol/ftp/DTPInputStream.java \
gnu/java/net/protocol/ftp/DTPOutputStream.java \
gnu/java/net/protocol/ftp/FTPConnection.java \
gnu/java/net/protocol/ftp/FTPException.java \
gnu/java/net/protocol/ftp/FTPResponse.java \
gnu/java/net/protocol/ftp/FTPURLConnection.java \
gnu/java/net/protocol/ftp/Handler.java \
gnu/java/net/protocol/ftp/PassiveModeDTP.java \
gnu/java/net/protocol/ftp/StreamInputStream.java \
gnu/java/net/protocol/ftp/StreamOutputStream.java \
gnu/java/net/protocol/gcjlib/Connection.java \
gnu/java/net/protocol/gcjlib/Handler.java \
gnu/java/net/protocol/http/Authenticator.java \
gnu/java/net/protocol/http/ByteArrayRequestBodyWriter.java \
gnu/java/net/protocol/http/ByteArrayResponseBodyReader.java \
gnu/java/net/protocol/http/ChunkedInputStream.java \
gnu/java/net/protocol/http/Cookie.java \
gnu/java/net/protocol/http/CookieManager.java \
gnu/java/net/protocol/http/Credentials.java \
gnu/java/net/protocol/http/HTTPConnection.java \
gnu/java/net/protocol/http/HTTPDateFormat.java \
gnu/java/net/protocol/http/HTTPURLConnection.java \
gnu/java/net/protocol/http/Handler.java \
gnu/java/net/protocol/http/Headers.java \
gnu/java/net/protocol/http/Request.java \
gnu/java/net/protocol/http/RequestBodyWriter.java \
gnu/java/net/protocol/http/Response.java \
gnu/java/net/protocol/http/ResponseBodyReader.java \
gnu/java/net/protocol/http/ResponseHeaderHandler.java \
gnu/java/net/protocol/http/SimpleCookieManager.java \
gnu/java/net/protocol/http/event/ConnectionEvent.java \
gnu/java/net/protocol/http/event/ConnectionListener.java \
gnu/java/net/protocol/http/event/RequestEvent.java \
gnu/java/net/protocol/http/event/RequestListener.java \
gnu/java/net/protocol/https/Handler.java \
gnu/java/net/protocol/jar/Connection.java \
gnu/java/net/protocol/jar/Handler.java \
gnu/java/nio/ChannelInputStream.java \
gnu/java/nio/ChannelOutputStream.java \
gnu/java/nio/ChannelReader.java \
gnu/java/nio/DatagramChannelImpl.java \
gnu/java/nio/DatagramChannelSelectionKey.java \
gnu/java/nio/FileLockImpl.java \
gnu/java/nio/InputStreamChannel.java \
gnu/java/nio/NIOConstants.java \
gnu/java/nio/NIODatagramSocket.java \
gnu/java/nio/NIOServerSocket.java \
gnu/java/nio/NIOSocket.java \
gnu/java/nio/OutputStreamChannel.java \
gnu/java/nio/PipeImpl.java \
gnu/java/nio/SelectionKeyImpl.java \
gnu/java/nio/SelectorImpl.java \
gnu/java/nio/SelectorProviderImpl.java \
gnu/java/nio/ServerSocketChannelImpl.java \
gnu/java/nio/ServerSocketChannelSelectionKey.java \
gnu/java/nio/SocketChannelImpl.java \
gnu/java/nio/SocketChannelSelectionKey.java \
gnu/java/nio/VMPipe.java \
gnu/java/nio/VMSelector.java \
gnu/java/nio/channels/FileChannelImpl.java \
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
gnu/java/security/PolicyFile.java \
gnu/java/security/action/GetPropertyAction.java \
gnu/java/security/action/GetSecurityPropertyAction.java \
gnu/java/security/action/SetAccessibleAction.java \
gnu/java/security/ber/BER.java \
gnu/java/security/ber/BEREncodingException.java \
gnu/java/security/ber/BERReader.java \
gnu/java/security/ber/BERValue.java \
gnu/java/security/der/BitString.java \
gnu/java/security/der/DER.java \
gnu/java/security/der/DEREncodingException.java \
gnu/java/security/der/DERReader.java \
gnu/java/security/der/DERValue.java \
gnu/java/security/der/DERWriter.java \
gnu/java/security/pkcs/PKCS7SignedData.java \
gnu/java/security/pkcs/SignerInfo.java \
gnu/java/security/provider/CollectionCertStoreImpl.java \
gnu/java/security/provider/DSAKeyFactory.java \
gnu/java/security/provider/DSAKeyPairGenerator.java \
gnu/java/security/provider/DSAParameterGenerator.java \
gnu/java/security/provider/DSAParameters.java \
gnu/java/security/provider/DSASignature.java \
gnu/java/security/provider/DefaultPolicy.java \
gnu/java/security/provider/EncodedKeyFactory.java \
gnu/java/security/provider/Gnu.java \
gnu/java/security/provider/GnuDHPublicKey.java \
gnu/java/security/provider/GnuDSAPrivateKey.java \
gnu/java/security/provider/GnuDSAPublicKey.java \
gnu/java/security/provider/GnuRSAPrivateKey.java \
gnu/java/security/provider/GnuRSAPublicKey.java \
gnu/java/security/provider/MD2withRSA.java \
gnu/java/security/provider/MD4withRSA.java \
gnu/java/security/provider/MD5.java \
gnu/java/security/provider/MD5withRSA.java \
gnu/java/security/provider/PKIXCertPathValidatorImpl.java \
gnu/java/security/provider/RSA.java \
gnu/java/security/provider/RSAKeyFactory.java \
gnu/java/security/provider/SHA.java \
gnu/java/security/provider/SHA1PRNG.java \
gnu/java/security/provider/SHA1withRSA.java \
gnu/java/security/provider/X509CertificateFactory.java \
gnu/java/security/util/Prime.java \
gnu/java/security/x509/GnuPKIExtension.java \
gnu/java/security/x509/PolicyNodeImpl.java \
gnu/java/security/x509/Util.java \
gnu/java/security/x509/X500DistinguishedName.java \
gnu/java/security/x509/X509CRL.java \
gnu/java/security/x509/X509CRLEntry.java \
gnu/java/security/x509/X509CRLSelectorImpl.java \
gnu/java/security/x509/X509Certificate.java \
gnu/java/security/x509/X509CertPath.java \
gnu/java/security/x509/X509CertSelectorImpl.java \
gnu/java/security/x509/ext/AuthorityKeyIdentifier.java \
gnu/java/security/x509/ext/BasicConstraints.java \
gnu/java/security/x509/ext/CRLNumber.java \
gnu/java/security/x509/ext/CertificatePolicies.java \
gnu/java/security/x509/ext/ExtendedKeyUsage.java \
gnu/java/security/x509/ext/Extension.java \
gnu/java/security/x509/ext/GeneralNames.java \
gnu/java/security/x509/ext/IssuerAlternativeNames.java \
gnu/java/security/x509/ext/KeyUsage.java \
gnu/java/security/x509/ext/PolicyConstraint.java \
gnu/java/security/x509/ext/PolicyMappings.java \
gnu/java/security/x509/ext/PrivateKeyUsagePeriod.java \
gnu/java/security/x509/ext/ReasonCode.java \
gnu/java/security/x509/ext/SubjectAlternativeNames.java \
gnu/java/security/x509/ext/SubjectKeyIdentifier.java \
gnu/java/text/AttributedFormatBuffer.java \
gnu/java/text/BaseBreakIterator.java \
gnu/java/text/CharacterBreakIterator.java \
gnu/java/text/FormatBuffer.java \
gnu/java/text/FormatCharacterIterator.java \
gnu/java/text/LineBreakIterator.java \
gnu/java/text/SentenceBreakIterator.java \
gnu/java/text/StringFormatBuffer.java \
gnu/java/text/WordBreakIterator.java \
gnu/java/util/DoubleEnumeration.java \
gnu/java/util/EmptyEnumeration.java \
gnu/java/util/prefs/FileBasedFactory.java \
gnu/java/util/prefs/MemoryBasedFactory.java \
gnu/java/util/prefs/MemoryBasedPreferences.java \
gnu/java/util/prefs/NodeReader.java \
gnu/java/util/prefs/NodeWriter.java \
gnu/regexp/CharIndexed.java \
gnu/regexp/CharIndexedCharArray.java \
gnu/regexp/CharIndexedInputStream.java \
gnu/regexp/CharIndexedString.java \
gnu/regexp/CharIndexedStringBuffer.java \
gnu/regexp/RE.java \
gnu/regexp/REException.java \
gnu/regexp/REFilterInputStream.java \
gnu/regexp/REMatch.java \
gnu/regexp/REMatchEnumeration.java \
gnu/regexp/RESyntax.java \
gnu/regexp/REToken.java \
gnu/regexp/RETokenAny.java \
gnu/regexp/RETokenBackRef.java \
gnu/regexp/RETokenChar.java \
gnu/regexp/RETokenEnd.java \
gnu/regexp/RETokenEndSub.java \
gnu/regexp/RETokenLookAhead.java \
gnu/regexp/RETokenOneOf.java \
gnu/regexp/RETokenPOSIX.java \
gnu/regexp/RETokenRange.java \
gnu/regexp/RETokenRepeated.java \
gnu/regexp/RETokenStart.java \
gnu/regexp/RETokenWordBoundary.java \
gnu/regexp/UncheckedRE.java \
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
java/net/Inet4Address.java \
java/net/Inet6Address.java \
java/net/InetAddress.java \
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
java/nio/VMDirectByteBuffer.java \
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
java/nio/channels/spi/AbstractInterruptibleChannel.java \
java/nio/channels/spi/AbstractSelectableChannel.java \
java/nio/channels/spi/AbstractSelectionKey.java \
java/nio/channels/spi/AbstractSelector.java \
java/nio/channels/spi/SelectorProvider.java \
java/nio/charset/CharacterCodingException.java \
java/nio/charset/Charset.java \
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
java/security/AlgorithmParameterGeneratorSpi.java \
java/security/AlgorithmParameters.java \
java/security/AlgorithmParametersSpi.java \
java/security/AllPermission.java \
java/security/BasicPermission.java \
java/security/Certificate.java \
java/security/CodeSource.java \
java/security/DigestException.java \
java/security/DigestInputStream.java \
java/security/DigestOutputStream.java \
java/security/DomainCombiner.java \
java/security/DummyKeyPairGenerator.java \
java/security/DummyMessageDigest.java \
java/security/DummySignature.java \
java/security/GeneralSecurityException.java \
java/security/Guard.java \
java/security/GuardedObject.java \
java/security/Identity.java \
java/security/IdentityScope.java \
java/security/InvalidAlgorithmParameterException.java \
java/security/InvalidKeyException.java \
java/security/InvalidParameterException.java \
java/security/Key.java \
java/security/KeyException.java \
java/security/KeyFactory.java \
java/security/KeyFactorySpi.java \
java/security/KeyManagementException.java \
java/security/KeyPair.java \
java/security/KeyPairGenerator.java \
java/security/KeyPairGeneratorSpi.java \
java/security/KeyStore.java \
java/security/KeyStoreException.java \
java/security/KeyStoreSpi.java \
java/security/MessageDigest.java \
java/security/MessageDigestSpi.java \
java/security/NoSuchAlgorithmException.java \
java/security/NoSuchProviderException.java \
java/security/Permission.java \
java/security/PermissionCollection.java \
java/security/Permissions.java \
java/security/Policy.java \
java/security/Principal.java \
java/security/PrivateKey.java \
java/security/PrivilegedAction.java \
java/security/PrivilegedActionException.java \
java/security/PrivilegedExceptionAction.java \
java/security/ProtectionDomain.java \
java/security/Provider.java \
java/security/ProviderException.java \
java/security/PublicKey.java \
java/security/SecureClassLoader.java \
java/security/SecureRandom.java \
java/security/SecureRandomSpi.java \
java/security/Security.java \
java/security/SecurityPermission.java \
java/security/Signature.java \
java/security/SignatureException.java \
java/security/SignatureSpi.java \
java/security/SignedObject.java \
java/security/Signer.java \
java/security/UnrecoverableKeyException.java \
java/security/UnresolvedPermission.java \
java/security/acl/Acl.java \
java/security/acl/AclEntry.java \
java/security/acl/AclNotFoundException.java \
java/security/acl/Group.java \
java/security/acl/LastOwnerException.java \
java/security/acl/NotOwnerException.java \
java/security/acl/Owner.java \
java/security/acl/Permission.java \
java/security/cert/CRL.java \
java/security/cert/CRLException.java \
java/security/cert/CRLSelector.java \
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
java/security/cert/Certificate.java \
java/security/cert/CertificateEncodingException.java \
java/security/cert/CertificateException.java \
java/security/cert/CertificateExpiredException.java \
java/security/cert/CertificateFactory.java \
java/security/cert/CertificateFactorySpi.java \
java/security/cert/CertificateNotYetValidException.java \
java/security/cert/CertificateParsingException.java \
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
java/security/cert/X509CRLSelector.java \
java/security/cert/X509CertSelector.java \
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
java/sql/SQLData.java \
java/sql/SQLException.java \
java/sql/SQLInput.java \
java/sql/SQLOutput.java \
java/sql/SQLPermission.java \
java/sql/SQLWarning.java \
java/sql/Savepoint.java \
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
$(javax_source_files) \
$(org_source_files)


java_source_files = $(special_java_source_files) $(ordinary_java_source_files)

## Math library: C, not C++
math_c_source_files = \
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
gnu/gcj/runtime/natNameFinder.cc \
gnu/gcj/runtime/natSharedLibLoader.cc \
gnu/gcj/runtime/natStackTrace.cc \
gnu/gcj/runtime/natStringBuffer.cc \
gnu/gcj/util/natDebug.cc \
gnu/java/lang/natMainThread.cc \
gnu/java/net/natPlainDatagramSocketImpl.cc \
gnu/java/net/natPlainSocketImpl.cc \
gnu/java/net/protocol/core/natCoreInputStream.cc \
gnu/java/nio/natPipeImpl.cc \
gnu/java/nio/natSelectorImpl.cc \
gnu/java/nio/natNIOServerSocket.cc \
gnu/java/nio/channels/natFileChannelImpl.cc \
java/io/natFile.cc \
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
java/lang/natVMClassLoader.cc \
java/lang/natVMSecurityManager.cc \
java/lang/ref/natReference.cc \
java/lang/reflect/natArray.cc \
java/lang/reflect/natConstructor.cc \
java/lang/reflect/natField.cc \
java/lang/reflect/natMethod.cc \
java/lang/reflect/natProxy.cc \
java/net/natNetworkInterface.cc \
java/net/natInetAddress.cc \
java/nio/channels/natChannels.cc \
java/nio/natDirectByteBufferImpl.cc \
java/text/natCollator.cc \
java/util/natResourceBundle.cc \
java/util/natVMTimeZone.cc \
java/util/logging/natLogger.cc \
java/util/zip/natDeflater.cc \
java/util/zip/natInflater.cc

xlib_java_source_files = \
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
gnu/awt/xlib/XFontPeer.java \
gnu/awt/xlib/XToolkit.java

xlib_nat_source_files = \
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
	javadoc -outfile $(srcdir)/doc/java-lang.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $$lang
	javadoc -outfile $(srcdir)/doc/java-lang-ref.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/lang/ref/*.java
	javadoc -outfile $(srcdir)/doc/java-lang-reflect.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/lang/reflect/*.java
	javadoc -outfile $(srcdir)/doc/java-applet.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/applet/*.java
	javadoc -outfile $(srcdir)/doc/java-awt.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/awt/*.java
	javadoc -outfile $(srcdir)/doc/java-awt-color.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/awt/color/*.java
	javadoc -outfile $(srcdir)/doc/java-awt-datatransfer.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/awt/datatransfer/*.java
	javadoc -outfile $(srcdir)/doc/java-awt-event.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/awt/event/*.java
	javadoc -outfile $(srcdir)/doc/java-awt-geom.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/awt/geom/*.java
	javadoc -outfile $(srcdir)/doc/java-awt-image.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/awt/image/*.java
	javadoc -outfile $(srcdir)/doc/java-awt-peer.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/awt/peer/*.java
	javadoc -outfile $(srcdir)/doc/java-beans.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/beans/*.java
	javadoc -outfile $(srcdir)/doc/java-beans-beancontext.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/beans/beancontext/*.java
	javadoc -outfile $(srcdir)/doc/java-io.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/io/*.java
	javadoc -outfile $(srcdir)/doc/java-math.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/math/*.java
	javadoc -outfile $(srcdir)/doc/java-net.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/net/*.java
	javadoc -outfile $(srcdir)/doc/java-security.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/security/*.java
	javadoc -outfile $(srcdir)/doc/java-security-cert.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/security-cert/*.java
	javadoc -outfile $(srcdir)/doc/java-security-spec.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/security/spec/*.java
	javadoc -outfile $(srcdir)/doc/java-security-interfaces.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/security/spec/interfaces/*.java
	javadoc -outfile $(srcdir)/doc/java-sql.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/sql/*.java
	javadoc -outfile $(srcdir)/doc/java-text.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/text/*.java
	javadoc -outfile $(srcdir)/doc/java-util.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/util/*.java
	javadoc -outfile $(srcdir)/doc/java-util-jar.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/util/jar/*.java
	javadoc -outfile $(srcdir)/doc/java-util-zip.texi -doclet TexinfoDoclet -sourcepath .'$(CLASSPATH_SEPARATOR)'$(srcdir) $(srcdir)/java/util/zip/*.java

## ################################################################

##
## Creating and installing sources.zip
##

## Create a zip holding all the sources.  This can be meaningfully
## used in Eclipse.
src.zip:
	-rm -f src.zip
	here=`pwd`; \
	( ( cd $(srcdir); \
	  find java gnu javax org -name '*.java' -print | \
	  while read file; do \
## Ugly code to avoid "echo -C".  Must separate each entry by a newline
## Gross but easy.
	    echo "x-C" | sed -e 's/^.//'; \
	    echo $(srcdir); \
	    echo $$file; \
	  done ); \
## Now the build tree.
	  find gnu java -name '*.java' -print) | \
## Many of the above circumlocutions are because ZIP will most likely
## be a relative path to fastjar.
	$(ZIP) -cfM@ $$here/src.zip
## Now include the files from external/.
	for dir in sax w3c_dom; do \
	  (cd $(srcdir)/external/$$dir; \
	  find org -type f -name '*.java' -print | while read file; do \
	    echo "x-C" | sed -e 's/^.//'; \
	    echo $(srcdir)/external/$$dir; \
	    echo $$file; \
          done) | \
	  $(ZIP) -ufM0E@ src.zip; \
	done

## We use a variable for this in case the user wants to override it.
sourcesdir = $(jardir)

install-src.zip: src.zip
	$(INSTALL_DATA) src.zip $(DESTDIR)$(sourcesdir)/src-@gcc_version@.zip


## ################################################################

##
## Dependency tracking madness.
##

## This is an evil hack to work around an automake limitation.  We
## need to ensure that all CNI headers are built, not just the ones used
## internally by libgcj. We can't make the .o files depend on nat_headers, 
## because in that case we'll force a complete rebuild of
## the C++ code whenever any .java file is touched.
## Also force all the class files to build first. This makes them build in
## the right order to improve performance.
all-recursive: classes.stamp $(nat_headers) $(xlib_nat_headers)

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
	"gxx_include_dir=$(gxx_include_dir)" \
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

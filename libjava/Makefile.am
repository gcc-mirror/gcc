## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign subdir-objects

ACLOCAL_AMFLAGS = -I . -I .. -I ../config

# May be used by various substitution variables.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

SUBDIRS = $(DIRLTDL) gcj include classpath
if TESTSUBDIR
SUBDIRS += testsuite
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

toolexeclib_LTLIBRARIES = libgcj.la libgij.la
toolexecmainlib_DATA = libgcj.spec

if XLIB_AWT
toolexeclib_LTLIBRARIES += lib-gnu-awt-xlib.la
endif

if GTK_AWT
toolexeclib_LTLIBRARIES += lib-gnu-java-awt-peer-gtk.la libgcjawt.la
endif

if QT_AWT
toolexeclib_LTLIBRARIES += lib-gnu-java-awt-peer-qt.la
endif

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libgcj.pc

jardir = $(datadir)/java
jar_DATA = libgcj-$(gcc_version).jar

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
dbexecdir = $(libdir)/gcj-$(gcc_version)
## Name of the default .db.
db_name = classmap.db
## Compiler specific component of the .db file
db_pathtail = gcj-$(gcc_version)/$(db_name)

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

extra_ldflags_libjava = @extra_ldflags_libjava@

GCJLINK = $(LIBTOOL) --tag=GCJ --mode=link $(GCJ) -L$(here) $(JC1FLAGS) \
          $(LDFLAGS) -o $@
LIBLINK = $(LIBTOOL) --tag=CXX --mode=link $(CXX) -L$(here) $(JC1FLAGS) \
          $(LDFLAGS) $(extra_ldflags_libjava) -o $@

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
	-Wno-deprecated -fbootstrap-classes

AM_CFLAGS = @LIBGCJ_CFLAGS@
if USING_GCC
AM_CFLAGS += $(WARNINGS)
endif

## Extra CFLAGS used for JNI C sources shared with GNU Classpath.
PEDANTIC_CFLAGS = -ansi -pedantic -Wall -Wno-long-long

JCFLAGS = -g
JC1FLAGS = @LIBGCJ_JAVAFLAGS@ $(GCJFLAGS)

LIBFFIINCS = @LIBFFIINCS@

AM_CPPFLAGS = -I$(top_srcdir) \
	-Iinclude -I$(top_srcdir)/include \
	-I$(top_srcdir)/classpath/include \
	-I$(top_srcdir)/classpath/native/fdlibm \
	$(GCINCS) $(THREADINCS) $(INCLTDL) \
	$(GCC_UNWIND_INCLUDE) $(ZINCS) $(LIBFFIINCS)

BOOTCLASSPATH = $(here)/classpath/lib

## ################################################################

all_property_files = $(property_files) \
    java/util/logging/logging.properties

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

libgcj_la_SOURCES = prims.cc jni.cc exception.cc stacktrace.cc \
	link.cc defineclass.cc interpret.cc verify.cc \
	java/lang/Class.java java/lang/Object.java \
	$(nat_source_files)

if USING_BOEHMGC
libgcj_la_SOURCES += boehm.cc
endif

if USING_NOGC
libgcj_la_SOURCES += nogc.cc
endif

if USING_POSIX_PLATFORM
libgcj_la_SOURCES += posix.cc
endif

if USING_WIN32_PLATFORM
libgcj_la_SOURCES += win32.cc
endif

if USING_DARWIN_CRT
libgcj_la_SOURCES += darwin.cc
endif

if USING_POSIX_THREADS
libgcj_la_SOURCES += posix-threads.cc
endif

if USING_WIN32_THREADS
libgcj_la_SOURCES += win32-threads.cc
endif

if USING_NO_THREADS
libgcj_la_SOURCES += no-threads.cc
endif

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
	classpath/native/fdlibm/libfdlibm.la \
	$(all_packages_source_files:.list=.lo) \
	$(bc_objects) \
	$(propertyo_files) \
	$(LIBFFI) $(ZLIBS) $(GCLIBS)
libgcj_la_DEPENDENCIES = libgcj-$(gcc_version).jar \
	$(all_packages_source_files:.list=.lo) \
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

## For the peer library, DEPENDENCIES need to come before OBJECTS so
## that JNI headers are built before JNI C files.
$(lib_gnu_java_awt_peer_gtk_la_OBJECTS): $(lib_gnu_java_awt_peer_gtk_la_DEPENDENCIES)

lib_gnu_java_awt_peer_gtk_la_SOURCES =
lib_gnu_java_awt_peer_gtk_la_LIBADD = \
	gnu-java-awt-peer-gtk.lo \
	classpath/native/jni/gtk-peer/libgtkpeer.la \
	$(GTK_LIBS) $(GLIB_LIBS) $(LIBART_LIBS) $(CAIRO_LIBS) $(PANGOFT2_LIBS)
lib_gnu_java_awt_peer_gtk_la_DEPENDENCIES = gnu-java-awt-peer-gtk.lo \
	classpath/native/jni/gtk-peer/libgtkpeer.la \
	libgcj-$(gcc_version).jar libgcj.la libgcj.spec
## The mysterious backslash in the grep pattern is consumed by make.
lib_gnu_java_awt_peer_gtk_la_LDFLAGS = \
        -version-info `grep -v '^\#' $(srcdir)/libtool-version` $(LIBGCJ_LD_SYMBOLIC) $(X_LIBS) -lXtst
lib_gnu_java_awt_peer_gtk_la_LINK = $(LIBLINK)

libgcjawt_la_SOURCES = classpath/native/jawt/jawt.c
libgcjawt_la_CFLAGS = -I$(srcdir)/classpath/native/jni/classpath \
	$(PEDANTIC_CFLAGS) $(X_CFLAGS)
## See jv_convert_LDADD.
libgcjawt_la_LIBADD = -L$(here)/.libs lib-gnu-java-awt-peer-gtk.la
libgcjawt_la_LDFLAGS = \
	-version-info `grep -v '^\#' $(srcdir)/libtool-version` \
	$(LIBGCJ_LD_SYMBOLIC)
libgcjawt_la_LINK = $(LIBLINK)

lib_gnu_java_awt_peer_qt_la_SOURCES =
lib_gnu_java_awt_peer_qt_la_LIBADD = \
	gnu-java-awt-peer-qt.lo \
	classpath/native/jni/qt-peer/libqtpeer.la
lib_gnu_java_awt_peer_qt_la_DEPENDENCIES = gnu-java-awt-peer-qt.lo \
	classpath/native/jni/qt-peer/libqtpeer.la \
	libgcj-$(gcc_version).jar libgcj.la libgcj.spec
## The mysterious backslash in the grep pattern is consumed by make.
lib_gnu_java_awt_peer_qt_la_LDFLAGS = \
        -version-info `grep -v '^\#' $(srcdir)/libtool-version` $(LIBGCJ_LD_SYMBOLIC) $(X_LIBS) -lXtst
lib_gnu_java_awt_peer_qt_la_LINK = $(LIBLINK)

lib_gnu_awt_xlib_la_SOURCES = $(xlib_nat_source_files)
lib_gnu_awt_xlib_la_LIBADD = gnu/awt/xlib.lo gnu/gcj/xlib.lo
lib_gnu_awt_xlib_la_DEPENDENCIES = libgcj-$(gcc_version).jar \
	libgcj.la libgcj.spec \
	$(lib_gnu_awt_xlib_la_LIBADD)
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

## Build property files into the library.
property_files = \
classpath/resource/org/ietf/jgss/MessagesBundle.properties \
classpath/resource/java/util/iso4217.properties	\
classpath/resource/gnu/java/awt/peer/gtk/font.properties \
classpath/resource/gnu/regexp/MessagesBundle_it.properties \
classpath/resource/gnu/regexp/MessagesBundle_fr.properties \
classpath/resource/gnu/regexp/MessagesBundle.properties	\
classpath/resource/META-INF/services/org.xml.sax.driver	\
classpath/resource/META-INF/services/javax.xml.parsers.SAXParserFactory	\
classpath/resource/META-INF/services/javax.xml.parsers.DocumentBuilderFactory \
classpath/resource/META-INF/services/javax.xml.parsers.TransformerFactory

propertyo_files = $(patsubst classpath/resource/%,%,$(addsuffix .lo,$(property_files)))

$(propertyo_files): %.lo: classpath/resource/%
	$(mkinstalldirs) `dirname $@`; \
	$(LTGCJCOMPILE) -o $@ -c $< -Wc,--resource,$(@:.lo=)

libgcj-$(gcc_version).jar:
	jardir=`dirname $(ZIP)`; \
	jardir=`cd $$jardir && pwd`; \
	jarname=`basename $(ZIP)`; \
	cd classpath/lib; $$jardir/$$jarname -cfM \
	    ../../libgcj-$(gcc_version).jar gnu java javax org

CLEANFILES = libgcj-$(gcc_version).jar \
	$(all_java_deps_files) $(all_java_filelist_files)
DISTCLEANFILES = native.dirs

mostlyclean-local:
## Use libtool rm to remove each libtool object
	find . -name '*.lo' -print | xargs $(LIBTOOL) rm -f

distclean-local:
## Remove every .d file that was created.
	find . -name '*.d' -print | xargs rm -f

# Just remove the objects from C++ sources, for testing the C++ compiler.
clean-nat:
	rm -f $(nat_files) $(xlib_nat_files)

SUFFIXES = .class .java .h .properties .list

## Pass the list of object files to libtool in a temporary file to
## avoid tripping platform command line length limits.
lib-gnu-awt-xlib.la: $(lib_gnu_awt_xlib_la_OBJECTS) $(lib_gnu_awt_xlib_la_DEPENDENCIES)
	@echo Creating list of files to link...
	@: $(call write_entries_to_file,$(lib_gnu_awt_xlib_la_OBJECTS) $(lib_gnu_awt_xlib_la_LIBADD),lib_gnu_awt_xlib.objectlist)
	$(lib_gnu_awt_xlib_la_LINK) -objectlist lib_gnu_awt_xlib.objectlist \
	-rpath $(toolexeclibdir) $(lib_gnu_awt_xlib_la_LDFLAGS) $(LIBS)

## ################################################################

## Compiling a list of java sources to a single .o.

%.lo: %.list
	$(LTGCJCOMPILE) -c -o $@ -MT $@ -MD -MP -MF $(basename $@).deps @$<

## ################################################################

## This pulls in a number of variable and target definitions.
include sources.am

## ################################################################

##
## How to build header files.
##

## We have special rules for certain headers.
omitted_headers = java/lang/ClassLoader.h java/lang/Thread.h \
	java/lang/String.h java/lang/reflect/Constructor.h \
	java/lang/reflect/Field.h java/lang/reflect/Method.h \
	java/lang/reflect/Proxy.h gnu/gcj/runtime/ExtensionClassLoader.h

generic_header_files = $(filter-out $(omitted_headers),$(ordinary_header_files) $(xlib_nat_headers)) \
	gnu/gcj/tools/gcj_dbtool/Main.h

$(generic_header_files): %.h: classpath/lib/%.class
	name=`echo $< | sed -e 's/\.class$$//' -e 's,classpath/lib/,,'`; \
	$(mkinstalldirs) `dirname $$name`; \
	$(GCJH) -d . -classpath '' -bootclasspath classpath/lib $$name

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

nat_headers = $(ordinary_header_files) $(inner_nat_headers) \
	gnu/gcj/tools/gcj_dbtool/Main.h
nat_headers_install = $(ordinary_header_files)

xlib_nat_headers = $(gnu_awt_xlib_header_files) $(gnu_gcj_xlib_header_files)

java/lang/ClassLoader.h: classpath/lib/java/lang/ClassLoader.class
	$(GCJH) -classpath '' -bootclasspath classpath/lib \
		-prepend 'jclass _Jv_FindClass (_Jv_Utf8Const *name, java::lang::ClassLoader *loader);' \
		-prepend 'void _Jv_RunMain (jclass klass, const char *name, int argc, const char **argv, bool is_jar);' \
		-friend 'jclass (::_Jv_FindClass) (_Jv_Utf8Const *name, java::lang::ClassLoader *loader);' \
		-friend 'void ::_Jv_RunMain (jclass klass, const char *name, int argc, const char **argv, bool is_jar);' \
		java/lang/ClassLoader

java/lang/Thread.h: classpath/lib/java/lang/Thread.class
	$(GCJH) -classpath '' -bootclasspath classpath/lib \
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
		java/lang/Thread

java/lang/String.h: classpath/lib/java/lang/String.class
	$(GCJH) -classpath '' -bootclasspath classpath/lib \
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
	    java/lang/String

java/lang/reflect/Constructor.h: classpath/lib/java/lang/reflect/Constructor.class
	$(GCJH) -classpath '' -bootclasspath classpath/lib \
	    -prepend 'jmethodID _Jv_FromReflectedConstructor (java::lang::reflect::Constructor *);' \
	    -prepend 'jobject _Jv_JNI_ToReflectedMethod (_Jv_JNIEnv *, jclass, jmethodID, jboolean);' \
	    -friend 'jmethodID (::_Jv_FromReflectedConstructor) (java::lang::reflect::Constructor *);' \
	    -friend 'jobject (::_Jv_JNI_ToReflectedMethod) (_Jv_JNIEnv *, jclass, jmethodID, jboolean);' \
	    -friend 'class java::lang::Class;' \
	    java/lang/reflect/Constructor

java/lang/reflect/Field.h: classpath/lib/java/lang/reflect/Field.class
	$(GCJH) -classpath '' -bootclasspath classpath/lib \
	    -prepend 'jfieldID _Jv_FromReflectedField (java::lang::reflect::Field *);' \
	    -prepend 'jobject _Jv_JNI_ToReflectedField (_Jv_JNIEnv*, jclass, jfieldID, jboolean);' \
	    -friend 'jfieldID (::_Jv_FromReflectedField) (java::lang::reflect::Field *);' \
	    -friend 'jobject (::_Jv_JNI_ToReflectedField) (_Jv_JNIEnv*, jclass, jfieldID, jboolean);' \
	    -friend 'class java::lang::Class;' \
	    java/lang/reflect/Field

java/lang/reflect/Method.h: classpath/lib/java/lang/reflect/Method.class
	$(GCJH) -classpath '' -bootclasspath classpath/lib \
	    -prepend 'jmethodID _Jv_FromReflectedMethod (java::lang::reflect::Method *);' \
	    -prepend 'jobject _Jv_JNI_ToReflectedMethod (_Jv_JNIEnv *, jclass, jmethodID, jboolean);' \
	    -friend 'jmethodID (::_Jv_FromReflectedMethod) (java::lang::reflect::Method *);' \
	    -friend 'jobject (::_Jv_JNI_ToReflectedMethod) (_Jv_JNIEnv *, jclass, jmethodID, jboolean);' \
	    -friend 'class java::lang::Class;' \
	    -friend 'class java::io::ObjectInputStream;' \
	    java/lang/reflect/Method

java/lang/reflect/Proxy.h: classpath/lib/java/lang/reflect/Proxy.class
	$(GCJH) -classpath '' -bootclasspath classpath/lib \
	    java/lang/reflect/Proxy

java/lang/reflect/Proxy$$ProxyData.h: classpath/lib/java/lang/reflect/Proxy.class
	$(GCJH) -classpath '' -bootclasspath classpath/lib \
	'java/lang/reflect/Proxy$$ProxyData'

java/lang/reflect/Proxy$$ProxyType.h: classpath/lib/java/lang/reflect/Proxy.class
	$(GCJH) -classpath '' -bootclasspath classpath/lib \
	'java/lang/reflect/Proxy$$ProxyType'

gnu/gcj/runtime/ExtensionClassLoader.h: classpath/lib/gnu/gcj/runtime/ExtensionClassLoader.class
	$(GCJH) -classpath '' -bootclasspath classpath/lib \
	    -friend 'class ::java::lang::ClassLoader;' \
	    gnu/gcj/runtime/ExtensionClassLoader

java/io/ObjectInputStream$$GetField.h: classpath/lib/java/io/ObjectInputStream.class
	$(GCJH) -classpath '' -bootclasspath classpath/lib \
	'java/io/ObjectInputStream$$GetField'

java/io/ObjectOutputStream$$PutField.h: classpath/lib/java/io/ObjectOutputStream.class
	$(GCJH) -classpath '' -bootclasspath classpath/lib \
	'java/io/ObjectOutputStream$$PutField'

java/nio/DirectByteBufferImpl$$ReadWrite.h: classpath/lib/java/nio/DirectByteBufferImpl.class
	$(GCJH) -classpath '' -bootclasspath classpath/lib \
	'java/nio/DirectByteBufferImpl$$ReadWrite'

java/nio/channels/Pipe$$SinkChannel.h: classpath/lib/java/nio/channels/Pipe.class
	$(GCJH) -classpath '' -bootclasspath classpath/lib \
	'java/nio/channels/Pipe$$SinkChannel'

java/nio/channels/Pipe$$SourceChannel.h: classpath/lib/java/nio/channels/Pipe.class
	$(GCJH) -classpath '' -bootclasspath classpath/lib \
	'java/nio/channels/Pipe$$SourceChannel'

gnu/java/net/PlainSocketImpl$$SocketInputStream.h: classpath/lib/gnu/java/net/PlainSocketImpl.class
	$(GCJH) -classpath '' -bootclasspath classpath/lib \
	'gnu/java/net/PlainSocketImpl$$SocketInputStream'

gnu/java/net/PlainSocketImpl$$SocketOutputStream.h: classpath/lib/gnu/java/net/PlainSocketImpl.class
	$(GCJH) -classpath '' -bootclasspath classpath/lib \
	'gnu/java/net/PlainSocketImpl$$SocketOutputStream'

gnu/java/nio/PipeImpl$$SinkChannelImpl.h: classpath/lib/gnu/java/nio/PipeImpl.class
	$(GCJH) -classpath '' -bootclasspath classpath/lib \
	'gnu/java/nio/PipeImpl$$SinkChannelImpl'

gnu/java/nio/PipeImpl$$SourceChannelImpl.h: classpath/lib/gnu/java/nio/PipeImpl.class
	$(GCJH) -classpath '' -bootclasspath classpath/lib \
	'gnu/java/nio/PipeImpl$$SourceChannelImpl'

## Only used by PosixProcess.java
java/lang/ConcreteProcess$$ProcessManager.h: classpath/lib/java/lang/ConcreteProcess.class
	$(GCJH) -classpath '' -bootclasspath classpath/lib \
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
	@for f in libgcj.security; do \
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
header-check: libgcj-$(gcc_version).jar $(nat_headers)
	rm -f htest.cc; \
	for h in $(nat_headers); do \
	  echo "#include \"$$h\"" >> htest.cc; \
	done; \
	$(CXXCOMPILE) -fsyntax-only htest.cc

## This rule can be used to see if all the .class files verify
## correctly.
class-check: libgcj-$(gcc_version).jar
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

gcj_dbtool_SOURCES = \
gnu/gcj/tools/gcj_dbtool/Main.java \
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

## This lists all the C++ source files in subdirectories.
nat_source_files = \
gnu/classpath/natSystemProperties.cc \
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
gnu/gcj/runtime/natSharedLibLoader.cc \
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
java/lang/natStringBuilder.cc \
java/lang/natSystem.cc \
java/lang/natThread.cc \
java/lang/natVMClassLoader.cc \
java/lang/natVMSecurityManager.cc \
java/lang/natVMThrowable.cc \
java/lang/ref/natReference.cc \
java/lang/reflect/natArray.cc \
java/lang/reflect/natConstructor.cc \
java/lang/reflect/natField.cc \
java/lang/reflect/natMethod.cc \
java/net/natVMNetworkInterface.cc \
java/net/natInetAddress.cc \
java/nio/channels/natChannels.cc \
java/nio/natDirectByteBufferImpl.cc \
java/text/natCollator.cc \
java/util/natResourceBundle.cc \
java/util/natVMTimeZone.cc \
java/util/logging/natLogger.cc \
java/util/zip/natDeflater.cc \
java/util/zip/natInflater.cc

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

## We use a variable for this in case the user wants to override it.
sourcesdir = $(jardir)

install-src.zip: src.zip
	$(INSTALL_DATA) src.zip $(DESTDIR)$(sourcesdir)/src-$(gcc_version).zip


## ################################################################

##
## Dependency tracking madness.
##

## This is an evil hack to work around an automake limitation.  We
## need to ensure that all CNI headers are built, not just the ones
## used internally by libgcj. We can't make the .o files depend on
## nat_headers, because in that case we'll force a complete rebuild of
## the C++ code whenever any .java file is touched.  So instead we
## have a dummy rule which is only used once, namely the first time a
## build is done.  On subsequent builds, the dependency tracking for
## the .cc compilations will have picked up the .h files, and these
## will be built directly as needed.

headers.stamp:
## Note that we don't use a real dependency here, since we don't want
## to rebuild all the headers here when the header list changes.  If
## we did rebuild here, then any addition of a .java file would cause
## a large number of recompilations.
	$(MAKE) create-headers
	@echo > headers.stamp

headers_to_make = $(nat_headers)
if XLIB_AWT
headers_to_make += $(xlib_nat_headers)
endif

create-headers: $(headers_to_make)

.PHONY: create-headers

$(libgcj_la_OBJECTS) $(gcj_dbtool_OBJECTS) $(xlib_nat_files): headers.stamp
$(libgij_la_OBJECTS): headers.stamp

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

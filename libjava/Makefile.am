## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign no-installinfo

if TESTSUBDIR
SUBDIRS = $(DIRLTDL) testsuite gcj include
else
SUBDIRS = $(DIRLTDL) gcj include
endif

## ################################################################

##
## What gets installed, and where.
##

## Install a library built with a cross compiler in tooldir, not
## libdir.
if USE_LIBDIR
toolexeclibdir = $(libdir)$(MULTISUBDIR)
else
toolexecdir = $(exec_prefix)/$(target_alias)
toolexeclibdir = $(toolexecdir)/lib$(MULTISUBDIR)
endif

## Some versions of automake can't handle a conditional _LTLIBRARIES.
if AWT
hack1 = libgcjawt.la
endif
toolexeclib_LTLIBRARIES = libgcj.la $(hack1)
toolexeclib_DATA = libgcj.spec
data_DATA = libgcj.zip

if NEEDS_DATA_START
toolexeclib_LIBRARIES = libgcjdata.a
libgcjdata_a_SOURCES = libgcjdata.c
endif

## For now, only on native systems.  FIXME.
if NATIVE
bin_PROGRAMS = jv-convert gij
endif

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
GCJ = gcj
ZIP = $(MULTIBUILDTOP)../$(COMPPATH)/zip/zip$(EXEEXT)
else
GCJ = $(target_alias)-gcj
ZIP = zip
endif
GCJH = gcjh
else # CANADIAN
## JAVAC is set to `$(GCJ) -C'.  However, JAVAC is run from the srcdir
## (due to problems running it in builddir).  In this case the obvious
## definition of GCJ won't work; instead we resort to this ugly
## definition that gives us absolute paths.
expanded = `cd $(MULTIBUILDTOP)../$(COMPPATH)/gcc && pwd`
GCJ = $(expanded)/gcj$(EXEEXT) -B$(expanded)/
GCJH = $(MULTIBUILDTOP)../$(COMPPATH)/gcc/gcjh$(EXEEXT)
ZIP = $(MULTIBUILDTOP)../$(COMPPATH)/zip/zip$(EXEEXT)
endif # CANADIAN

GCJCOMPILE = CLASSPATH=$(here) $(LIBTOOL) --mode=compile $(GCJ) -fassume-compiled -L$(here) $(JC1FLAGS) -c

JAVAC = $(GCJ) -C

EH_COMMON_INCLUDE = @EH_COMMON_INCLUDE@

WARNINGS = -W -Wall
## We need _GNU_SOURCE defined for some Linux builds.  It doesn't hurt
## to always define it.
## Note that we need -fasynchronous-exceptions because gcc is
## currently broken with respect to exception handling in leaf
## functions.
AM_CXXFLAGS = -fno-rtti -fvtable-thunks -fasynchronous-exceptions \
	@LIBGCJ_CXXFLAGS@ @EXCEPTIONSPEC@ $(WARNINGS) -D_GNU_SOURCE
if USING_GCC
AM_CFLAGS = @LIBGCJ_CFLAGS@ $(WARNINGS)
else
AM_CFLAGS = @LIBGCJ_CFLAGS@
endif

JCFLAGS = -g
JC1FLAGS = -g @LIBGCJ_JAVAFLAGS@

LIBFFIINCS = -I$(top_srcdir)/../libffi/include -I$(MULTIBUILDTOP)../libffi/include

INCLUDES = -I$(top_srcdir) -Iinclude -I$(top_srcdir)/include \
	$(GCINCS) $(THREADINCS) $(INCLTDL) \
	$(EH_COMMON_INCLUDE) $(ZINCS) $(LIBFFIINCS)


## ################################################################

##
## How to build libgcj.a and libgcj.zip
##

## Objects from C++ sources in subdirs.
nat_files = $(nat_source_files:.cc=.lo)
## Objects from C sources in subdirs.
c_files = $(c_source_files:.c=.lo)
## Objects from Java sources in subdirs.
javao_files = $(java_source_files:.java=.lo) \
	$(built_java_source_files:.java=.lo)

awto_files = $(awt_java_source_files:.java=.lo)

## Extract the libffi object file names.
libffi_files = `$(AR) t ../libffi/.libs/libffi.a 2>/dev/null | sed 's/\.o/\.lo/g' | sed 's/^/..\/libffi\//g'`

libgcj_la_SOURCES = prims.cc jni.cc exception.cc \
	resolve.cc defineclass.cc interpret.cc name-finder.cc
EXTRA_libgcj_la_SOURCES = boehm.cc nogc.cc posix-threads.cc no-threads.cc \
	$(c_source_files) $(java_source_files) $(built_java_source_files)
libgcj_la_DEPENDENCIES = libgcj.zip $(javao_files) $(nat_files) \
	$(c_files) $(GCOBJS) $(THREADOBJS) $(LIBLTDL)
libgcj_la_LIBADD = $(javao_files) $(nat_files) $(c_files) $(GCOBJS) \
	$(THREADOBJS) $(libffi_files) $(LIBLTDL)
libgcj_la_LDFLAGS = -L../libstdc++ -rpath $(toolexeclibdir) \
## The mysterious backslash is consumed by make.
	-version-info `grep -v '^\#' $(srcdir)/libtool-version`

libgcjawt_la_SOURCES = ## Nothing
EXTRA_libgcjawt_la_SOURCES = $(awt_java_source_files)
libgcjawt_la_DEPENDENCIES = libgcj.zip $(awto_files)
libgcjawt_la_LIBADD = $(awto_files)
libgcjawt_la_LDFLAGS = -rpath $(toolexeclibdir) \
## The mysterious backslash is consumed by make.
	-version-info `grep -v '^\#' $(srcdir)/libtool-version`

## Make the .class files depend on the .zip file.  This seems
## backwards, but is right.  This doesn't catch all the .class files,
## but that is ok, because the ones it fails to pick up are defined in
## a .java file with some other class which is caught.  Note that we
## only want to create headers for those files which do not have
## hand-maintained headers.
$(java_source_files:.java=.class): libgcj.zip
$(awt_java_source_files:.java=.class): libgcj.zip

## FIXME: this isn't really correct.
$(built_java_source_files:.java=.class): $(built_java_source_files)
## This little nastiness is here so that the backquoted stuff in the
## GCJ definition can be correctly expanded, if required.
	javac="$(JAVAC)"; \
	$$javac -L$(here) $(JCFLAGS) -classpath $(here):`cd $(srcdir) && /bin/pwd` \
	  -d $(here) $?

## We have the zip file depend on the java sources and not the class
## files, because we don't know the names of all the class files.
## FIXME: this method fails in a peculiar case: if libgcj.zip is
## up-to-date, and foo.class is removed, and bar.java is touched, then
## `make libgcj.zip' will not rebuilt foo.class.  That's because
## libgcj.zip is not out-of-date with respect to foo.java.
libgcj.zip: $(java_source_files) $(cond_awt_java_source_files)
## FIXME: this ought to depend on built_java_source_files, but right
## now it can't.  Ugly.
	$(MAKE) $(built_java_source_files:.java=.class)
## This little nastiness is here so that the backquoted stuff in the
## GCJ definition can be correctly expanded, if required.
	javac="$(JAVAC)"; cd $(srcdir); \
	  $$javac -L$(here) $(JCFLAGS) -classpath $(here):`/bin/pwd` -d $(here) \
	    $(subst $(srcdir)/,,$?)
	-@rm -f libgcj.zip
## Note that we explicitly want to include directory information.
	$(ZIP) -r libgcj java gnu -n .class -i '*.class' -i '*/'

MOSTLYCLEANFILES = $(javao_files) $(nat_files) $(nat_headers) $(c_files) \
	$(awto_files)
CLEANFILES = libgcj.zip

clean-local:
## We just remove every .class file that was created.
	find . -name '*.class' -print | xargs rm -f

SUFFIXES = .class .java .h

.class.lo:
	$(GCJCOMPILE) -o $@ $<

.java.lo:
	$(GCJCOMPILE) -o $@ $<

## This is GNU make specific.  For the .o files in subdirs, use a
## special rule.  The standard automake rule can't be overridden (this
## is a bug in automake), and it also won't put the .o files into
## subdirs.  FIXME.
$(nat_files): %.lo: %.cc
	$(LTCXXCOMPILE) -c -o $@ $<

$(nat_files) $(GCOBJS) $(THREADOBJS) $(libgcj_la_OBJECTS) $(gij_OBJECTS): \
		include/config.h include/java-signal.h $(nat_headers)

## FIXME: GNU make.
$(c_files): %.lo: %.c
	$(LTCOMPILE) -c -o $@ $<

$(c_files): java/lang/fdlibm.h java/lang/ieeefp.h java/lang/mprec.h

## FIXME: GNU make.
$(awto_files) $(javao_files): %.lo: %.java
	$(GCJCOMPILE) -o $@ $<

$(awto_files) $(javao_files): libgcj.zip

## ################################################################

##
## How to build header files.
##

.class.h:
## FIXME: GNU make specific.
	$(GCJH) -classpath $(top_builddir) $(basename $<)

## Header files used when compiling some of the nat* files.
nat_headers = $(ordinary_java_source_files:.java=.h) \
	$(built_java_source_files:.java=.h) \
	$(cond_awt_java_source_files:.java=.h)

$(nat_headers): libgcj.zip

java/lang/ClassLoader.h: java/lang/ClassLoader.class libgcj.zip
	$(GCJH) -classpath $(top_builddir) \
		-friend 'jclass _Jv_FindClass (_Jv_Utf8Const *name, java::lang::ClassLoader *loader);' \
		$(basename $<)

## Our internal main program needs to be able to create a FirstThread.
gnu/gcj/runtime/FirstThread.h: gnu/gcj/runtime/FirstThread.class libgcj.zip
	$(GCJH) -classpath $(top_builddir) \
		-friend 'void JvRunMain (jclass, int, const char **);' \
		-friend 'void _Jv_RunMain (const char*, int, const char **);' \
		$(basename $<)

java/lang/Thread.h: java/lang/Thread.class libgcj.zip
	$(GCJH) -classpath $(top_builddir) \
		-prepend 'class _Jv_JNIEnv;' \
		-prepend 'extern "Java" { namespace gnu { namespace gcj { namespace jni { class NativeThread; } } } };' \
		-prepend '#define _JV_NOT_OWNER 1' \
		-prepend '#define _JV_INTERRUPTED 2' \
		-friend '_Jv_JNIEnv * _Jv_GetCurrentJNIEnv ();' \
		-friend 'void _Jv_SetCurrentJNIEnv (_Jv_JNIEnv *env);' \
		-friend 'class gnu::gcj::jni::NativeThread;' \
		-friend 'int _Jv_CondWait (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *mu, jlong millis, jint nanos);' \
		-friend 'int _Jv_CondNotify (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *mu);' \
		-friend 'void _Jv_ThreadInterrupt (_Jv_Thread_t *data);' \
		$(basename $<)

java/lang/String.h: java/lang/String.class libgcj.zip
	$(GCJH) -classpath $(top_builddir) \
	    -friend 'jchar* _Jv_GetStringChars (jstring str);' \
	    -friend 'jstring* _Jv_StringFindSlot (jchar*, jint, jint);' \
	    -friend 'jstring* _Jv_StringGetSlot (jstring);' \
	    -friend 'jstring _Jv_NewStringUtf8Const (_Jv_Utf8Const* str);' \
	    -friend 'jstring _Jv_NewStringLatin1 (const char*, jsize);' \
	    -friend 'jstring _Jv_AllocString (jsize);' \
	    $(basename $<)

java/lang/reflect/Constructor.h: java/lang/reflect/Constructor.class libgcj.zip
	$(GCJH) -classpath $(top_builddir) \
	    -friend 'jmethodID _Jv_FromReflectedConstructor (java::lang::reflect::Constructor *);' \
	    -friend 'java::lang::Class;' \
	    -friend 'jobject _Jv_JNI_ToReflectedMethod (_Jv_JNIEnv *, jclass, jmethodID, jboolean);' \
	    $(basename $<)

java/lang/reflect/Field.h: java/lang/reflect/Field.class libgcj.zip
	$(GCJH) -classpath $(top_builddir) \
	    -friend 'jfieldID _Jv_FromReflectedField (java::lang::reflect::Field *);' \
	    -friend 'jobject _Jv_JNI_ToReflectedField (_Jv_JNIEnv*, jclass, jfieldID, jboolean);' \
	    -friend 'java::lang::Class;' \
	    $(basename $<)

java/lang/reflect/Method.h: java/lang/reflect/Method.class libgcj.zip
	$(GCJH) -classpath $(top_builddir) \
	    -friend 'jmethodID _Jv_FromReflectedMethod (java::lang::reflect::Method *);' \
	    -friend 'java::lang::Class;' \
	    -friend 'jobject _Jv_JNI_ToReflectedMethod (_Jv_JNIEnv *, jclass, jmethodID, jboolean);' \
	    $(basename $<)

gnu/gcj/runtime/VMClassLoader.h: gnu/gcj/runtime/VMClassLoader.class libgcj.zip
	$(GCJH) -classpath $(top_builddir) \
	    -friend 'java::lang::ClassLoader;' \
	    $(basename $<)

## Headers we maintain by hand and which we want to install.
extra_headers = java/lang/Object.h java/lang/Class.h

## Install the headers.  It is fairly ugly that we have to do this by
## hand.
install-data-local:
	$(PRE_INSTALL)
	@for f in $(nat_headers) $(extra_headers); do \
	  d="`echo $$f | sed -e 's,/[^/]*$$,,'`"; \
	  $(mkinstalldirs) $(DESTDIR)$(includedir)/$$d; \
	  if test -f $(srcdir)/$$f; then p=$(srcdir)/$$f; else p=$$f; fi; \
	  echo " $(INSTALL_DATA) $$p $(DESTDIR)$(includedir)/$$f"; \
	  $(INSTALL_DATA) $$p $(DESTDIR)$(includedir)/$$f; \
	done

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
header-check: libgcj.zip $(nat_headers)
	rm -f htest.cc; \
	for h in $(nat_headers); do \
	  echo "#include \"$$h\"" >> htest.cc; \
	done; \
	$(CXXCOMPILE) -fsyntax-only htest.cc


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
	-rpath `if test -d ../libstdc++; then cd ../libstdc++; pwd; fi` -rpath $(toolexeclibdir)
jv_convert_LINK = $(LIBTOOL) --mode=link $(GCJ) -L$(here) $(JC1FLAGS) $(LDFLAGS) \
	-o jv-convert
## We explicitly link in the libraries we need.  This way we don't
## need -nodefaultlibs, so we can still rely on gcj picking up the
## system libraries we need (via the specs file).
## We need the -L so that gcj can find libgcj with `-lgcj'.
## FIXME: should be _libs on some systems.
jv_convert_LDADD = $(convert_source_files:.java=.lo) libgcj.la \
	$(GCLIBS) $(THREADLIBS) $(ZLIBS) -L$(here)/.libs
## Depend on the spec file to make sure it is up to date before
## linking this program.
jv_convert_DEPENDENCIES = $(convert_source_files:.java=.lo) \
	$(GCDEPS) $(THREADDEPS) $(ZDEPS) libgcj.la libgcj.spec

gij_SOURCES = gij.cc
## We need -nodefaultlibs because we want to avoid gcj's `-lgcj'.  We
## need this because we are explicitly using libtool to link using the
## `.la' file.
gij_LDFLAGS = -rpath `if test -d ../libstdc++; then cd ../libstdc++; pwd; fi` -rpath $(toolexeclibdir)
gij_LINK = $(LIBTOOL) --mode=link $(GCJ) -L$(here) $(JC1FLAGS) $(LDFLAGS) \
	-o gij
## We explicitly link in the libraries we need.  This way we don't
## need -nodefaultlibs, so we can still rely on gcj picking up the
## system libraries we need (via the specs file).
## We need the -L so that gcj can find libgcj with `-lgcj'.
## FIXME: should be _libs on some systems.
gij_LDADD = libgcj.la $(GCLIBS) $(THREADLIBS) $(ZLIBS) -L$(here)/.libs
## Depend on the spec file to make sure it is up to date before
## linking this program.
gij_DEPENDENCIES = $(GCDEPS) $(THREADDEPS) $(ZDEPS) libgcj.la libgcj.spec

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
gnu/gcj/convert/Input_EUCJIS.java \
gnu/gcj/convert/Input_JavaSrc.java \
gnu/gcj/convert/Input_SJIS.java \
gnu/gcj/convert/Input_UTF8.java	\
gnu/gcj/convert/Input_iconv.java \
gnu/gcj/convert/Output_8859_1.java \
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
java/awt/AWTError.java \
java/awt/AWTEvent.java \
java/awt/AWTException.java \
java/awt/ActiveEvent.java \
java/awt/Adjustable.java \
java/awt/BorderLayout.java \
java/awt/Color.java \
java/awt/Component.java	\
java/awt/Container.java	\
java/awt/Dimension.java	\
java/awt/Event.java \
java/awt/Font.java \
java/awt/Frame.java \
java/awt/Graphics.java \
java/awt/IllegalComponentStateException.java \
java/awt/Image.java \
java/awt/ItemSelectable.java \
java/awt/LayoutManager.java \
java/awt/LayoutManager2.java \
java/awt/Menu.java \
java/awt/MenuBar.java \
java/awt/MenuComponent.java \
java/awt/MenuContainer.java \
java/awt/MenuItem.java \
java/awt/Paint.java \
java/awt/PaintContext.java \
java/awt/Point.java \
java/awt/Rectangle.java	\
java/awt/Shape.java \
java/awt/TextArea.java \
java/awt/TextComponent.java \
java/awt/Toolkit.java \
java/awt/Transparency.java \
java/awt/Window.java \
java/awt/event/AWTEventListener.java \
java/awt/event/ActionEvent.java	\
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
java/awt/event/InputEvent.java \
java/awt/event/InputMethodEvent.java \
java/awt/event/InputMethodListener.java	\
java/awt/event/InvocationEvent.java \
java/awt/event/ItemEvent.java \
java/awt/event/ItemListener.java \
java/awt/event/KeyAdapter.java \
java/awt/event/KeyEvent.java \
java/awt/event/KeyListener.java	\
java/awt/event/MouseAdapter.java \
java/awt/event/MouseEvent.java \
java/awt/event/MouseListener.java \
java/awt/event/MouseMotionAdapter.java \
java/awt/event/MouseMotionListener.java	\
java/awt/event/PaintEvent.java \
java/awt/event/TextEvent.java \
java/awt/event/TextListener.java \
java/awt/event/WindowAdapter.java \
java/awt/event/WindowEvent.java	\
java/awt/event/WindowListener.java \
java/awt/geom/Dimension2D.java \
java/awt/geom/Point2D.java \
java/awt/peer/ComponentPeer.java \
java/awt/peer/ContainerPeer.java \
java/awt/peer/FramePeer.java \
java/awt/peer/WindowPeer.java

if AWT
cond_awt_java_source_files = $(awt_java_source_files)
else
cond_awt_java_source_files =
endif

## Java files which are created by configure and thus are in the build
## directory.
built_java_source_files = java/lang/ConcreteProcess.java

## List of all .java files to be compiled.  Please keep this list
## alphabetical.  Please put files from gnu/gcj/convert into
## convert_source_files.  If the .java file has a hand-maintained
## header, please list it in special_java_source_files.
ordinary_java_source_files =  $(convert_source_files) \
gnu/gcj/io/DefaultMimeTypes.java \
gnu/gcj/io/MimeTypes.java \
gnu/gcj/jni/NativeThread.java \
gnu/gcj/runtime/VMClassLoader.java \
gnu/gcj/runtime/FirstThread.java \
gnu/gcj/text/BaseBreakIterator.java \
gnu/gcj/text/CharacterBreakIterator.java \
gnu/gcj/text/LineBreakIterator.java \
gnu/gcj/text/LocaleData_en.java	\
gnu/gcj/text/LocaleData_en_US.java \
gnu/gcj/text/SentenceBreakIterator.java	\
gnu/gcj/text/WordBreakIterator.java \
gnu/gcj/math/MPN.java \
gnu/gcj/protocol/file/Connection.java \
gnu/gcj/protocol/file/Handler.java \
gnu/gcj/protocol/http/Connection.java \
gnu/gcj/protocol/http/Handler.java \
gnu/gcj/protocol/jar/Handler.java \
gnu/gcj/protocol/jar/Connection.java \
gnu/gcj/RawData.java \
gnu/gcj/util/EnumerationChain.java \
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
java/io/File.java \
java/io/FileDescriptor.java \
java/io/FileInputStream.java \
java/io/FileNotFoundException.java \
java/io/FileOutputStream.java \
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
java/io/LineNumberInputStream.java \
java/io/LineNumberReader.java \
java/io/ObjectStreamException.java \
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
java/io/StreamCorruptedException.java \
java/io/StreamTokenizer.java \
java/io/StringBufferInputStream.java \
java/io/StringReader.java \
java/io/StringWriter.java \
java/io/SyncFailedException.java \
java/io/UTFDataFormatException.java \
java/io/UnsupportedEncodingException.java \
java/io/Writer.java \
java/lang/AbstractMethodError.java \
java/lang/ArithmeticException.java \
java/lang/ArrayIndexOutOfBoundsException.java \
java/lang/ArrayStoreException.java \
java/lang/Boolean.java \
java/lang/Byte.java \
java/lang/Character.java \
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
java/lang/Process.java \
java/lang/Runnable.java	\
java/lang/Runtime.java \
java/lang/RuntimeException.java	\
java/lang/SecurityException.java \
java/lang/SecurityManager.java \
java/lang/Short.java \
java/lang/StackOverflowError.java \
java/lang/String.java \
java/lang/StringBuffer.java \
java/lang/StringIndexOutOfBoundsException.java \
java/lang/System.java \
java/lang/Thread.java \
java/lang/ThreadDeath.java \
java/lang/ThreadGroup.java \
java/lang/Throwable.java \
java/lang/UnknownError.java \
java/lang/UnsatisfiedLinkError.java \
java/lang/UnsupportedOperationException.java \
java/lang/VerifyError.java \
java/lang/VirtualMachineError.java \
java/lang/Void.java \
java/lang/reflect/AccessibleObject.java	\
java/lang/reflect/Array.java \
java/lang/reflect/Constructor.java \
java/lang/reflect/Field.java \
java/lang/reflect/InvocationTargetException.java \
java/lang/reflect/Member.java \
java/lang/reflect/Method.java \
java/lang/reflect/Modifier.java	\
java/math/BigDecimal.java \
java/math/BigInteger.java \
java/net/BindException.java \
java/net/ConnectException.java \
java/net/ContentHandler.java \
java/net/ContentHandlerFactory.java \
java/net/DatagramPacket.java \
java/net/DatagramSocket.java \
java/net/DatagramSocketImpl.java \
java/net/FileNameMap.java \
java/net/HttpURLConnection.java \
java/net/InetAddress.java \
java/net/JarURLConnection.java \
java/net/MalformedURLException.java \
java/net/MulticastSocket.java \
java/net/NoRouteToHostException.java \
java/net/PlainDatagramSocketImpl.java \
java/net/PlainSocketImpl.java \
java/net/ProtocolException.java	\
java/net/ServerSocket.java \
java/net/Socket.java \
java/net/SocketException.java \
java/net/SocketImpl.java \
java/net/SocketImplFactory.java	\
java/net/SocketOptions.java \
java/net/URL.java \
java/net/URLClassLoader.java \
java/net/URLConnection.java \
java/net/URLDecoder.java \
java/net/URLEncoder.java \
java/net/URLStreamHandler.java \
java/net/URLStreamHandlerFactory.java \
java/net/UnknownHostException.java \
java/net/UnknownServiceException.java \
java/security/AlgorithmParameterGeneratorSpi.java \
java/security/DigestException.java \
java/security/GeneralSecurityException.java \
java/security/InvalidAlgorithmParameterException.java \
java/security/InvalidKeyException.java \
java/security/InvalidParameterException.java \
java/security/Key.java \
java/security/KeyException.java \
java/security/KeyPair.java \
java/security/KeyPairGenerator.java \
java/security/KeyPairGeneratorSpi.java \
java/security/MessageDigest.java \
java/security/NoSuchAlgorithmException.java \
java/security/NoSuchProviderException.java \
java/security/PrivateKey.java \
java/security/Provider.java \
java/security/PublicKey.java \
java/security/SecureClassLoader.java \
java/security/SecureRandom.java \
java/security/Security.java \
java/security/Signature.java \
java/security/SignatureException.java \
java/security/interfaces/DSAKey.java \
java/security/interfaces/DSAParams.java \
java/security/interfaces/DSAPrivateKey.java \
java/security/interfaces/DSAPublicKey.java \
java/security/interfaces/RSAPrivateCrtKey.java \
java/security/interfaces/RSAPrivateKey.java \
java/security/interfaces/RSAPublicKey.java \
java/security/spec/AlgorithmParameterSpec.java \
java/security/spec/InvalidKeySpecException.java \
java/security/spec/InvalidParameterSpecException.java \
java/security/spec/KeySpec.java \
java/security/spec/RSAPrivateCrtKeySpec.java \
java/security/spec/RSAPrivateKeySpec.java \
java/security/spec/RSAPublicKeySpec.java \
java/sql/CallableStatement.java \
java/sql/Connection.java \
java/sql/DataTruncation.java \
java/sql/DatabaseMetaData.java \
java/sql/Date.java \
java/sql/Driver.java \
java/sql/DriverManager.java \
java/sql/DriverPropertyInfo.java \
java/sql/PreparedStatement.java \
java/sql/ResultSet.java \
java/sql/ResultSetMetaData.java \
java/sql/SQLException.java \
java/sql/SQLWarning.java \
java/sql/Statement.java \
java/sql/Time.java \
java/sql/Timestamp.java \
java/sql/Types.java \
java/text/BreakIterator.java \
java/text/CharacterIterator.java \
java/text/ChoiceFormat.java \
java/text/CollationElementIterator.java \
java/text/CollationKey.java \
java/text/Collator.java \
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
java/util/BitSet.java \
java/util/Calendar.java	\
java/util/Collection.java \
java/util/Comparator.java \
java/util/ConcurrentModificationException.java \
java/util/Date.java \
java/util/Dictionary.java \
java/util/EmptyStackException.java \
java/util/Enumeration.java \
java/util/EventListener.java \
java/util/EventObject.java \
java/util/GregorianCalendar.java \
java/util/Hashtable.java \
java/util/Iterator.java \
java/util/List.java \
java/util/ListIterator.java \
java/util/ListResourceBundle.java \
java/util/Locale.java \
java/util/MissingResourceException.java	\
java/util/NoSuchElementException.java \
java/util/Observable.java \
java/util/Observer.java	\
java/util/Properties.java \
java/util/PropertyResourceBundle.java \
java/util/Random.java \
java/util/ResourceBundle.java \
java/util/SimpleTimeZone.java \
java/util/Stack.java \
java/util/StringTokenizer.java \
java/util/TimeZone.java	\
java/util/TooManyListenersException.java \
java/util/Vector.java \
java/util/jar/JarFile.java \
java/util/jar/JarInputStream.java \
java/util/jar/JarEntry.java \
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
java/util/zip/InflaterInputStream.java	\
java/util/zip/ZipConstants.java	\
java/util/zip/ZipEntry.java \
java/util/zip/ZipException.java	\
java/util/zip/ZipFile.java \
java/util/zip/ZipInputStream.java \
java/util/zip/ZipOutputStream.java

java_source_files = $(ordinary_java_source_files) $(special_java_source_files)

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
  java/lang/k_cos.c       java/lang/s_sin.c

#java/awt/natToolkit.cc

## This lists all the C++ source files in subdirectories.
nat_source_files = \
gnu/gcj/convert/JIS0208_to_Unicode.cc \
gnu/gcj/convert/JIS0212_to_Unicode.cc \
gnu/gcj/convert/Unicode_to_JIS.cc \
gnu/gcj/convert/natIconv.cc \
gnu/gcj/convert/natInput_EUCJIS.cc \
gnu/gcj/convert/natInput_SJIS.cc \
gnu/gcj/convert/natOutput_EUCJIS.cc \
gnu/gcj/convert/natOutput_SJIS.cc \
gnu/gcj/jni/natNativeThread.cc \
gnu/gcj/runtime/natFirstThread.cc \
java/io/natFile.cc \
java/io/natFileDescriptor.cc \
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
java/lang/natSystem.cc \
java/lang/natThread.cc \
java/lang/natThrowable.cc \
java/lang/reflect/natArray.cc \
java/lang/reflect/natConstructor.cc \
java/lang/reflect/natField.cc \
java/lang/reflect/natMethod.cc \
java/net/natInetAddress.cc \
java/net/natPlainDatagramSocketImpl.cc \
java/net/natPlainSocketImpl.cc \
java/text/natCollator.cc \
java/util/natGregorianCalendar.cc \
java/util/zip/natDeflater.cc \
java/util/zip/natInflater.cc

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
	"CFLAGS_FOR_BUILD=$(CFLAGS_FOR_BUILD)" \
	"CFLAGS_FOR_TARGET=$(CFLAGS_FOR_TARGET)" \
	"INSTALL=$(INSTALL)" \
	"INSTALL_DATA=$(INSTALL_DATA)" \
	"INSTALL_PROGRAM=$(INSTALL_PROGRAM)" \
	"INSTALL_SCRIPT=$(INSTALL_SCRIPT)" \
	"JC1FLAGS=$(JC1FLAGS)" \
	"LDFLAGS=$(LDFLAGS)" \
	"LIBCFLAGS=$(LIBCFLAGS)" \
	"LIBCFLAGS_FOR_TARGET=$(LIBCFLAGS_FOR_TARGET)" \
	"MAKE=$(MAKE)" \
	"MAKEINFO=$(MAKEINFO) $(MAKEINFOFLAGS)" \
	"PICFLAG=$(PICFLAG)" \
	"PICFLAG_FOR_TARGET=$(PICFLAG_FOR_TARGET)" \
	"SHELL=$(SHELL)" \
	"exec_prefix=$(exec_prefix)" \
	"infodir=$(infodir)" \
	"libdir=$(libdir)" \
	"prefix=$(prefix)" \
	"AR=$(AR)" \
	"AS=$(AS)" \
	"CC=$(CC)" \
	"CXX=$(CXX)" \
	"LD=$(LD)" \
	"LIBCFLAGS=$(LIBCFLAGS)" \
	"NM=$(NM)" \
	"PICFLAG=$(PICFLAG)" \
	"RANLIB=$(RANLIB)" \
	"DESTDIR=$(DESTDIR)"

CONFIG_STATUS_DEPENDENCIES = $(libgcj_basedir)/configure.host

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
	$(MULTIDO) $(AM_MAKEFLAGS) DO=all multi-do
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


## ################################################################


## See above.
cygnus_hack =



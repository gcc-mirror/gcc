## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign

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

if XLIB_AWT
cond_x_ltlibrary = libgcjx.la
else
cond_x_ltlibrary =
endif

toolexeclib_LTLIBRARIES = libgcj.la $(cond_x_ltlibrary)
toolexeclib_DATA = libgcj.spec
data_DATA = libgcj.jar

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
ZIP = $(MULTIBUILDTOP)../$(COMPPATH)/fastjar/jar$(EXEEXT)
else
ZIP = jar
endif
GCJH = gcjh
else # CANADIAN
GCJH = $(MULTIBUILDTOP)../$(COMPPATH)/gcc/gcjh$(EXEEXT)
ZIP = $(MULTIBUILDTOP)../$(COMPPATH)/fastjar/jar$(EXEEXT)
endif # CANADIAN

## The compiler with whatever flags we want for both -c and -C
## compiles.
GCJ_WITH_FLAGS = $(GCJ) --encoding=UTF-8

GCJCOMPILE = $(LIBTOOL) --tag=GCJ --mode=compile $(GCJ_WITH_FLAGS) -fassume-compiled -fclasspath=$(here) -L$(here) $(JC1FLAGS) -MD -MT $@ -MF $(@:.lo=.d) -c
GCJLINK = $(LIBTOOL) --tag=GCJ --mode=link $(GCJ) -L$(here) $(JC1FLAGS) $(LDFLAGS) -o $@
LIBLINK = $(LIBTOOL) --tag=CC --mode=link $(CC) -L$(here) $(JC1FLAGS) $(LDFLAGS) -o $@

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
AM_CXXFLAGS = -fno-rtti -fnon-call-exceptions \
## Some systems don't allow `$' in identifiers by default, so we force it.
	-fdollars-in-identifiers \
	@LIBGCJ_CXXFLAGS@ @X_CFLAGS@ $(WARNINGS) -D_GNU_SOURCE \
	-DPREFIX="\"$(prefix)\""
if USING_GCC
AM_CFLAGS = @LIBGCJ_CFLAGS@ $(WARNINGS)
else
AM_CFLAGS = @LIBGCJ_CFLAGS@
endif

JCFLAGS = -g
JC1FLAGS = @LIBGCJ_JAVAFLAGS@ $(GCJFLAGS)

LIBFFIINCS = @LIBFFIINCS@

INCLUDES = -I$(top_srcdir) -Iinclude -I$(top_srcdir)/include \
	$(GCINCS) $(THREADINCS) $(INCLTDL) \
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
## Objects from Java sources in subdirs.
javao_files = $(java_source_files:.java=.lo) \
	$(built_java_source_files:.java=.lo)
x_javao_files = $(x_java_source_files:.java=.lo)

libgcj_la_SOURCES = prims.cc posix.cc jni.cc exception.cc \
	resolve.cc defineclass.cc interpret.cc name-finder.cc \
	$(nat_source_files)
EXTRA_libgcj_la_SOURCES = boehm.cc nogc.cc posix-threads.cc no-threads.cc \
	$(c_source_files) $(java_source_files) $(built_java_source_files)
libgcj_la_DEPENDENCIES = libgcj.jar $(javao_files) \
	$(c_files) $(GCOBJS) $(THREADOBJS) $(LIBLTDL)

libgcj_la_LIBADD = $(javao_files) $(c_files) $(GCOBJS) \
	$(THREADOBJS)
# Include THREADLIBS here to ensure that the correct version of
# certain linuxthread functions get linked:
libgcj_la_LDFLAGS = -rpath $(toolexeclibdir) $(THREADLIBS) $(LIBLTDL) \
	$(GCLIBS) $(ZLIBS) \
## The mysterious backslash is consumed by make.
	-version-info `grep -v '^\#' $(srcdir)/libtool-version`
libgcj_la_LINK = $(LIBLINK)

libgcjx_la_SOURCES = $(x_nat_source_files)
EXTRA_libgcjx_la_SOURCES = $(x_java_source_files)
libgcjx_la_DEPENDENCIES = libgcj.jar $(x_javao_files)
libgcjx_la_LIBADD = $(x_javao_files)
libgcjx_la_LDFLAGS = @X_PRE_LIBS@ @X_LIBS@ -lX11 @X_EXTRA_LIBS@ \
        -rpath $(toolexeclibdir) \
## The mysterious backslash is consumed by make.
        -version-info `grep -v '^\#' $(srcdir)/libtool-version`
libgcjx_la_LINK = $(LIBLINK)

## Make a symlink for each `libgcjx' file that was installed.
install-exec-hook:
	cd $(DESTDIR)$(libdir) && \
	if test -f libgcjx.la; then \
	  rm -f gnu-awt-xlib.la; \
	  $(LN_S) libgcjx.la gnu-awt-xlib.la; \
	fi

## Make the .class files depend on the .zip file.  This seems
## backwards, but is right.  This doesn't catch all the .class files,
## but that is ok, because the ones it fails to pick up are defined in
## a .java file with some other class which is caught.  Note that we
## only want to create headers for those files which do not have
## hand-maintained headers.
$(built_java_source_files:.java=.class): libgcj.jar
$(java_source_files:.java=.class): libgcj.jar

## The .class files for X will not be included in libgcj.jar, but the
## rule for libgcj.jar will cause all out-of-date .class files to be
## built. We need this to generate headers for the nat-files.
$(x_java_source_files:.java=.class): libgcj.jar

## We have the zip file depend on the java sources and not the class
## files, because we don't know the names of all the class files.
## FIXME: this method fails in a peculiar case: if libgcj.jar is
## up-to-date, and foo.class is removed, and bar.java is touched, then
## `make libgcj.jar' will not rebuilt foo.class.  That's because
## libgcj.jar is not out-of-date with respect to foo.java.
libgcj.jar: $(built_java_source_files) $(java_source_files) $(x_java_source_files)
## Create a list of all Java sources, without exceeding any shell limits.
	@: $(shell echo Creating list of files to compile...) $(shell rm -f tmp-list || :) $(shell touch tmp-list) $(foreach source,$?,$(shell echo $(source) >> tmp-list))
	@set fnord $(MAKEFLAGS); amf=$$2; fail=no; \
	javac="$(JAVAC)"; \
	cat tmp-list | (while read f; do \
	  echo $$javac $(JCFLAGS) -classpath $(here):$(srcdir) -d $(here) $$f; \
	  $$javac $(JCFLAGS) -classpath $(here):$(srcdir) -d $(here) $$f \
	    || case "$$amf" in *=*) exit 1;; *k*) fail=yes ;; *) exit 1;; esac; \
	done; \
	test "$$fail" = no)
	-@rm -f tmp-list libgcj.jar
## Note that we explicitly want to include directory information.
	find java gnu org -type d -o -type f -name '*.class' | \
	  sed -e '/\/\./d' -e '/\/xlib/d' | \
	  $(ZIP) cfM0E@ $@

MOSTLYCLEANFILES = $(javao_files) $(nat_files) $(nat_headers) $(c_files) $(x_javao_files) $(x_nat_files) $(x_nat_headers)
CLEANFILES = tmp-list libgcj.jar

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
$(nat_files) $(x_nat_files): %.lo: %.cc
	@echo '$(LTCXXCOMPILE) -c -o $@ $<'; \
	$(LTCXXCOMPILE) -Wp,-MD,.deps/$(*F).pp -c -o $@ $<
	@-sed -e 's/^\([^:]*\)\.o[ 	]*:/\1.lo \1.o :/' \
	  < .deps/$(*F).pp > .deps/$(*F).P; \
	tr ' ' '\012' < .deps/$(*F).pp \
	  | sed -e 's/^\\$$//' -e '/^$$/ d' -e '/:$$/ d' -e 's/$$/ :/' \
	    >> .deps/$(*F).P; \
	rm -f .deps/$(*F).pp

## FIXME: GNU make.
$(c_files): %.lo: %.c
	$(LTCOMPILE) -c -o $@ $<

$(c_files): java/lang/fdlibm.h java/lang/ieeefp.h java/lang/mprec.h

## FIXME: GNU make.
$(javao_files) $(x_javao_files): %.lo: %.java
	$(GCJCOMPILE) -o $@ $<

## Pass the list of object files to libtool in a temporary file to 
## avoid tripping platform command line length limits.
libgcj.la: $(libgcj_la_OBJECTS) $(libgcj_la_DEPENDENCIES)
	@: $(shell echo Creating list of files to link...) $(shell rm -f libgcj.objectlist || :) $(shell touch libgcj.objectlist) $(foreach object,$(libgcj_la_OBJECTS) $(libgcj_la_LIBADD),$(shell echo $(object) >> libgcj.objectlist))
	$(libgcj_la_LINK) -objectlist libgcj.objectlist \
	@LIBFFI@ \
	-rpath $(toolexeclibdir) $(libgcj_la_LDFLAGS) $(LIBS)

libgcjx.la: $(libgcjx_la_OBJECTS) $(libgcjx_la_DEPENDENCIES)
	@: $(shell echo Creating list of files to link...) $(shell rm -f libgcjx.objectlist || :) $(shell touch libgcjx.objectlist) $(foreach object,$(libgcjx_la_OBJECTS) $(libgcjx_la_LIBADD),$(shell echo $(object) >> libgcjx.objectlist))
	$(libgcjx_la_LINK) -objectlist libgcjx.objectlist \
	-rpath $(toolexeclibdir) $(libgcjx_la_LDFLAGS) $(LIBS)

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
	java/io/ObjectOutputStream$$PutField.h \
	java/io/ObjectInputStream$$GetField.h

x_nat_headers = $(x_java_source_files:.java=.h)

$(nat_headers) $(x_nat_headers): libgcj.jar

java/lang/ClassLoader.h: java/lang/ClassLoader.class libgcj.jar
	$(GCJH) -classpath $(top_builddir) \
		-friend 'jclass _Jv_FindClass (_Jv_Utf8Const *name, java::lang::ClassLoader *loader);' \
		-friend 'void _Jv_RunMain (jclass klass, const char *name, int argc, const char **argv, bool is_jar);' \
		$(basename $<)

## Our internal main program needs to be able to create a FirstThread.
gnu/gcj/runtime/FirstThread.h: gnu/gcj/runtime/FirstThread.class libgcj.jar
	$(GCJH) -classpath $(top_builddir) \
		-friend 'void JvRunMain (jclass, int, const char **);' \
		-friend 'void _Jv_RunMain (const char*, int, const char **);' \
		$(basename $<)

java/lang/Thread.h: java/lang/Thread.class libgcj.jar
	$(GCJH) -classpath $(top_builddir) \
		-prepend 'class _Jv_JNIEnv;' \
		-prepend '#define _JV_NOT_OWNER 1' \
		-prepend '#define _JV_INTERRUPTED 2' \
		-friend '_Jv_JNIEnv * _Jv_GetCurrentJNIEnv ();' \
		-friend 'void _Jv_SetCurrentJNIEnv (_Jv_JNIEnv *env);' \
		-friend 'void _Jv_ThreadRun (java::lang::Thread* thread);' \
		-friend 'jint _Jv_AttachCurrentThread(java::lang::Thread* thread);' \
		-friend 'java::lang::Thread* _Jv_AttachCurrentThread(jstring name, java::lang::ThreadGroup* group);' \
		-friend 'jint _Jv_DetachCurrentThread ();' \
		$(basename $<)

java/lang/String.h: java/lang/String.class libgcj.jar
	$(GCJH) -classpath $(top_builddir) \
	    -friend 'jchar* _Jv_GetStringChars (jstring str);' \
	    -friend 'jstring* _Jv_StringFindSlot (jchar*, jint, jint);' \
	    -friend 'jstring* _Jv_StringGetSlot (jstring);' \
	    -friend 'jstring _Jv_NewStringUtf8Const (_Jv_Utf8Const* str);' \
	    -friend 'jstring _Jv_NewStringLatin1 (const char*, jsize);' \
	    -friend 'jstring _Jv_AllocString (jsize);' \
	    $(basename $<)

java/lang/reflect/Constructor.h: java/lang/reflect/Constructor.class libgcj.jar
	$(GCJH) -classpath $(top_builddir) \
	    -friend 'jmethodID _Jv_FromReflectedConstructor (java::lang::reflect::Constructor *);' \
	    -friend 'class java::lang::Class;' \
	    -friend 'jobject _Jv_JNI_ToReflectedMethod (_Jv_JNIEnv *, jclass, jmethodID, jboolean);' \
	    $(basename $<)

java/lang/reflect/Field.h: java/lang/reflect/Field.class libgcj.jar
	$(GCJH) -classpath $(top_builddir) \
	    -friend 'jfieldID _Jv_FromReflectedField (java::lang::reflect::Field *);' \
	    -friend 'jobject _Jv_JNI_ToReflectedField (_Jv_JNIEnv*, jclass, jfieldID, jboolean);' \
	    -friend 'class java::lang::Class;' \
	    $(basename $<)

java/lang/reflect/Method.h: java/lang/reflect/Method.class libgcj.jar
	$(GCJH) -classpath $(top_builddir) \
	    -friend 'jmethodID _Jv_FromReflectedMethod (java::lang::reflect::Method *);' \
	    -friend 'class java::lang::Class;' \
	    -friend 'jobject _Jv_JNI_ToReflectedMethod (_Jv_JNIEnv *, jclass, jmethodID, jboolean);' \
	    $(basename $<)

gnu/gcj/runtime/VMClassLoader.h: gnu/gcj/runtime/VMClassLoader.class libgcj.jar
	$(GCJH) -classpath $(top_builddir) \
	    -friend 'class java::lang::ClassLoader;' \
	    $(basename $<)

java/io/ObjectInputStream$$GetField.h: libgcj.jar
	$(GCJH) -classpath $(top_builddir) \
	'java/io/ObjectInputStream$$GetField'

java/io/ObjectOutputStream$$PutField.h: libgcj.jar
	$(GCJH) -classpath $(top_builddir) \
	'java/io/ObjectOutputStream$$PutField'

## Headers we maintain by hand and which we want to install.
extra_headers = java/lang/Object.h java/lang/Class.h

$(extra_headers):
	@:

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
header-check: libgcj.jar $(nat_headers)
	rm -f htest.cc; \
	for h in $(nat_headers); do \
	  echo "#include \"$$h\"" >> htest.cc; \
	done; \
	$(CXXCOMPILE) -fsyntax-only htest.cc

## This rule can be used to see if all the .class files verify
## correctly.
class-check: libgcj.jar
	@ok=0; find . -name '*.class' -print | fgrep -v testsuite | \
	while read f; do \
	  echo "$(GCJ_WITH_FLAGS) --syntax-only $$f"; \
	  if $(GCJ_WITH_FLAGS) --syntax-only $$f; then \
	  :; else ok=1; fi; \
	done; exit $$ok

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
	-rpath $(toolexeclibdir) -shared-libgcc
jv_convert_LINK = $(GCJLINK)
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
gij_LDFLAGS = -rpath $(toolexeclibdir) -shared-libgcc
gij_LINK = $(GCJLINK)
## We explicitly link in the libraries we need.  This way we don't
## need -nodefaultlibs, so we can still rely on gcj picking up the
## system libraries we need (via the specs file).
## We need the -L so that gcj can find libgcj with `-lgcj'.
## FIXME: should be _libs on some systems.
gij_LDADD = libgcj.la $(GCLIBS) $(THREADLIBS) $(ZLIBS) -L$(here)/.libs
## Depend on the spec file to make sure it is up to date before
## linking this program.
gij_DEPENDENCIES = $(GCDEPS) $(THREADDEPS) $(ZDEPS) libgcj.la libgcj.spec

rmic_SOURCES =
## This is a dummy definition.
EXTRA_rmic_SOURCES = $(rmi_java_source_files)
rmic_LDFLAGS = --main=gnu.java.rmi.rmic.RMIC \
	-rpath $(toolexeclibdir) -shared-libgcc
rmic_LINK = $(GCJLINK)
## We explicitly link in the libraries we need.  This way we don't
## need -nodefaultlibs, so we can still rely on gcj picking up the
## system libraries we need (via the specs file).
## We need the -L so that gcj can find libgcj with `-lgcj'.
## FIXME: should be _libs on some systems.
rmic_LDADD = libgcj.la $(GCLIBS) $(THREADLIBS) $(ZLIBS) -L$(here)/.libs
## Depend on the spec file to make sure it is up to date before
## linking this program.
rmic_DEPENDENCIES = $(GCDEPS) $(THREADDEPS) $(ZDEPS) libgcj.la libgcj.spec

rmiregistry_SOURCES =
## This is a dummy definition.
EXTRA_rmiregistry_SOURCES = $(rmi_java_source_files)
rmiregistry_LDFLAGS = --main=gnu.java.rmi.registry.RegistryImpl \
	-rpath $(toolexeclibdir) -shared-libgcc
rmiregistry_LINK = $(GCJLINK)
## We explicitly link in the libraries we need.  This way we don't
## need -nodefaultlibs, so we can still rely on gcj picking up the
## system libraries we need (via the specs file).
## We need the -L so that gcj can find libgcj with `-lgcj'.
## FIXME: should be _libs on some systems.
rmiregistry_LDADD = libgcj.la $(GCLIBS) $(THREADLIBS) $(ZLIBS) -L$(here)/.libs
## Depend on the spec file to make sure it is up to date before
## linking this program.
rmiregistry_DEPENDENCIES = $(GCDEPS) $(THREADDEPS) $(ZDEPS) libgcj.la libgcj.spec

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
gnu/gcj/awt/BitMaskExtent.java \
gnu/gcj/awt/Buffers.java \
gnu/gcj/awt/ComponentDataBlitOp.java \
gnu/gcj/awt/GLightweightPeer.java \
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
java/awt/Component.java	\
java/awt/ComponentOrientation.java \
java/awt/Container.java	\
java/awt/Cursor.java \
java/awt/Dialog.java \
java/awt/Dimension.java	\
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
java/awt/Polygon.java \
java/awt/PopupMenu.java	\
java/awt/Rectangle.java	\
java/awt/RenderingHints.java \
java/awt/ScrollPane.java \
java/awt/Scrollbar.java	\
java/awt/Shape.java \
java/awt/SystemColor.java \
java/awt/TextArea.java \
java/awt/TextComponent.java \
java/awt/TextField.java	\
java/awt/Toolkit.java \
java/awt/Transparency.java \
java/awt/Window.java \
java/awt/color/ColorSpace.java \
java/awt/color/ICC_ColorSpace.java \
java/awt/color/ICC_Profile.java	\
java/awt/datatransfer/Clipboard.java \
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
java/awt/event/HierarchyBoundsAdapter.java \
java/awt/event/HierarchyBoundsListener.java \
java/awt/event/HierarchyEvent.java \
java/awt/event/HierarchyListener.java \
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
java/awt/geom/AffineTransform.java \
java/awt/geom/Dimension2D.java \
java/awt/geom/Ellipse2D.java \
java/awt/geom/IllegalPathStateException.java \
java/awt/geom/Line2D.java \
java/awt/geom/NoninvertibleTransformException.java \
java/awt/geom/PathIterator.java	\
java/awt/geom/Point2D.java \
java/awt/geom/Rectangle2D.java \
java/awt/geom/RectangularShape.java \
java/awt/geom/RoundRectangle2D.java \
java/awt/image/BufferedImage.java \
java/awt/image/ColorModel.java \
java/awt/image/ComponentColorModel.java	\
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
java/awt/image/SampleModel.java	\
java/awt/image/SinglePixelPackedSampleModel.java \
java/awt/image/WritableRaster.java \
java/awt/peer/ButtonPeer.java \
java/awt/peer/CanvasPeer.java \
java/awt/peer/CheckboxMenuItemPeer.java	\
java/awt/peer/CheckboxPeer.java	\
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
java/awt/peer/MenuItemPeer.java	\
java/awt/peer/MenuPeer.java \
java/awt/peer/PanelPeer.java \
java/awt/peer/PopupMenuPeer.java \
java/awt/peer/ScrollPanePeer.java \
java/awt/peer/ScrollbarPeer.java \
java/awt/peer/TextAreaPeer.java	\
java/awt/peer/TextComponentPeer.java \
java/awt/peer/TextFieldPeer.java \
java/awt/peer/WindowPeer.java \
java/beans/beancontext/BeanContext.java	\
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
java/beans/beancontext/BeanContextServices.java	\
java/beans/beancontext/BeanContextServicesListener.java	\
java/beans/BeanDescriptor.java \
java/beans/BeanInfo.java \
java/beans/Beans.java \
java/beans/Customizer.java \
java/beans/DesignMode.java \
java/beans/EventSetDescriptor.java \
java/beans/FeatureDescriptor.java \
java/beans/IndexedPropertyDescriptor.java \
java/beans/IntrospectionException.java \
java/beans/Introspector.java \
java/beans/MethodDescriptor.java \
java/beans/ParameterDescriptor.java \
java/beans/PropertyChangeEvent.java \
java/beans/PropertyChangeListener.java \
java/beans/PropertyChangeSupport.java \
java/beans/PropertyDescriptor.java \
java/beans/PropertyEditor.java \
java/beans/PropertyEditorManager.java \
java/beans/PropertyEditorSupport.java \
java/beans/PropertyVetoException.java \
java/beans/SimpleBeanInfo.java \
java/beans/VetoableChangeListener.java \
java/beans/VetoableChangeSupport.java \
java/beans/Visibility.java

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
gnu/java/rmi/dgc/DGCImpl.java \
gnu/java/rmi/dgc/DGCImpl_Skel.java \
gnu/java/rmi/dgc/DGCImpl_Stub.java \
gnu/java/rmi/registry/RegistryImpl.java	\
gnu/java/rmi/registry/RegistryImpl_Skel.java \
gnu/java/rmi/registry/RegistryImpl_Stub.java \
gnu/java/rmi/rmic/RMIC.java \
gnu/java/rmi/rmic/TabbedWriter.java \
gnu/java/rmi/server/ProtocolConstants.java \
gnu/java/rmi/server/RMIDefaultSocketFactory.java \
gnu/java/rmi/server/RMIHashes.java \
gnu/java/rmi/server/RMIObjectInputStream.java \
gnu/java/rmi/server/RMIObjectOutputStream.java \
gnu/java/rmi/server/UnicastConnection.java \
gnu/java/rmi/server/UnicastConnectionManager.java \
gnu/java/rmi/server/UnicastRef.java \
gnu/java/rmi/server/UnicastRemoteCall.java \
gnu/java/rmi/server/UnicastRemoteStub.java \
gnu/java/rmi/server/UnicastServer.java \
gnu/java/rmi/server/UnicastServerRef.java

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
java/io/WriteAbortedException.java \
java/io/Writer.java \
java/util/AbstractCollection.java \
java/util/AbstractList.java \
java/util/AbstractMap.java \
java/util/AbstractSequentialList.java \
java/util/AbstractSet.java \
java/util/ArrayList.java \
java/util/Arrays.java \
java/util/BasicMapEntry.java \
java/util/BitSet.java \
java/util/Calendar.java	\
java/util/Collection.java \
java/util/Collections.java \
java/util/Comparator.java \
java/util/ConcurrentModificationException.java \
java/util/Date.java \
java/util/Dictionary.java \
java/util/EmptyStackException.java \
java/util/Enumeration.java \
java/util/EventListener.java \
java/util/EventObject.java \
java/util/GregorianCalendar.java \
java/util/HashMap.java \
java/util/HashSet.java \
java/util/Hashtable.java \
java/util/IdentityHashMap.java \
java/util/Iterator.java	\
java/util/LinkedList.java \
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
java/util/PropertyResourceBundle.java \
java/util/Random.java \
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
java/util/WeakHashMap.java

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
gnu/gcj/math/MPN.java \
gnu/gcj/protocol/core/Connection.java \
gnu/gcj/protocol/core/Handler.java \
gnu/gcj/protocol/core/CoreInputStream.java \
gnu/gcj/protocol/file/Connection.java \
gnu/gcj/protocol/file/Handler.java \
gnu/gcj/protocol/http/Connection.java \
gnu/gcj/protocol/http/Handler.java \
gnu/gcj/protocol/jar/Connection.java \
gnu/gcj/protocol/jar/Handler.java \
gnu/gcj/runtime/FileDeleter.java \
gnu/gcj/runtime/FirstThread.java \
gnu/gcj/runtime/SharedLibLoader.java \
gnu/gcj/runtime/VMClassLoader.java \
gnu/java/io/ClassLoaderObjectInputStream.java \
gnu/java/io/NullOutputStream.java \
gnu/java/io/ObjectIdentityWrapper.java \
gnu/java/lang/ArrayHelper.java \
gnu/java/lang/ClassHelper.java \
gnu/java/lang/reflect/TypeSignature.java \
gnu/java/locale/Calendar.java \
gnu/java/locale/Calendar_de.java \
gnu/java/locale/Calendar_en.java \
gnu/java/locale/Calendar_nl.java \
gnu/java/locale/LocaleInformation.java \
gnu/java/locale/LocaleInformation_de.java \
gnu/java/locale/LocaleInformation_en.java \
gnu/java/locale/LocaleInformation_en_US.java \
gnu/java/locale/LocaleInformation_nl.java \
gnu/java/security/provider/DefaultPolicy.java \
gnu/java/security/provider/Gnu.java \
gnu/java/security/provider/SHA.java \
gnu/java/security/provider/SHA1PRNG.java \
gnu/java/text/BaseBreakIterator.java \
gnu/java/text/CharacterBreakIterator.java \
gnu/java/text/LineBreakIterator.java \
gnu/java/text/SentenceBreakIterator.java	\
gnu/java/text/WordBreakIterator.java \
gnu/java/util/DoubleEnumeration.java \
java/lang/ref/PhantomReference.java \
java/lang/ref/Reference.java \
java/lang/ref/ReferenceQueue.java \
java/lang/ref/SoftReference.java \
java/lang/ref/WeakReference.java \
java/lang/reflect/AccessibleObject.java	\
java/lang/reflect/Array.java \
java/lang/reflect/Constructor.java \
java/lang/reflect/Field.java \
java/lang/reflect/InvocationTargetException.java \
java/lang/reflect/Member.java \
java/lang/reflect/Method.java \
java/lang/reflect/Modifier.java	\
java/lang/reflect/ReflectPermission.java \
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
java/net/HttpURLConnection.java	\
java/net/InetAddress.java \
java/net/JarURLConnection.java \
java/net/MalformedURLException.java \
java/net/MulticastSocket.java \
java/net/NetPermission.java \
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
java/net/SocketPermission.java \
java/net/URL.java \
java/net/URLClassLoader.java \
java/net/URLConnection.java \
java/net/URLDecoder.java \
java/net/URLEncoder.java \
java/net/URLStreamHandler.java \
java/net/URLStreamHandlerFactory.java \
java/net/UnknownHostException.java \
java/net/UnknownServiceException.java \
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
java/security/cert/Certificate.java \
java/security/cert/CertificateEncodingException.java \
java/security/cert/CertificateException.java \
java/security/cert/CertificateExpiredException.java \
java/security/cert/CertificateFactory.java \
java/security/cert/CertificateFactorySpi.java \
java/security/cert/CertificateNotYetValidException.java \
java/security/cert/CertificateParsingException.java \
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
java/security/spec/RSAKeyGenParameterSpec.java \
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
java/sql/PreparedStatement.java	\
java/sql/Ref.java \
java/sql/ResultSet.java	\
java/sql/ResultSetMetaData.java	\
java/sql/SQLData.java \
java/sql/SQLException.java \
java/sql/SQLInput.java \
java/sql/SQLOutput.java \
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
org/w3c/dom/Attr.java \
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
org/w3c/dom/traversal/TreeWalker.java \
org/xml/sax/ext/DeclHandler.java \
org/xml/sax/ext/LexicalHandler.java \
org/xml/sax/helpers/AttributeListImpl.java \
org/xml/sax/helpers/AttributesImpl.java \
org/xml/sax/helpers/DefaultHandler.java \
org/xml/sax/helpers/LocatorImpl.java \
org/xml/sax/helpers/NamespaceSupport.java \
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
org/xml/sax/XMLReader.java \
$(rmi_java_source_files) \
$(awt_java_source_files) \
$(convert_source_files)


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
gnu/gcj/protocol/core/natCoreInputStream.cc \
gnu/gcj/runtime/natFirstThread.cc \
gnu/gcj/runtime/natSharedLibLoader.cc \
java/io/natFile.cc \
java/io/natFileDescriptor.cc \
java/io/natObjectInputStream.cc \
java/io/natObjectOutputStream.cc \
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
java/lang/natThrowable.cc \
java/lang/reflect/natArray.cc \
java/lang/reflect/natConstructor.cc \
java/lang/reflect/natField.cc \
java/lang/reflect/natMethod.cc \
java/net/natInetAddress.cc \
java/net/natPlainDatagramSocketImpl.cc \
java/net/natPlainSocketImpl.cc \
java/text/natCollator.cc \
java/util/natResourceBundle.cc \
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

## FIXME: depends on GNU make.
-include deps.mk

## This is an evil hack to work around an automake limitation.  We
## need to ensure that the built headers are built before we try to
## compile the C++ sources, but we can't make the .o files depend on
## the headers, because in that case we'll force a complete rebuild of
## the C++ code whenever any .java file is touched.
all-recursive: $(nat_headers) $(x_nat_headers)

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
	"RUNTESTFLAGS=$(RUNTESTFLAGS)" \
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

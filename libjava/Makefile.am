## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign subdir-objects

ACLOCAL_AMFLAGS = -I . -I .. -I ../config -I libltdl

# May be used by various substitution variables.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

SUBDIRS = $(DIRLTDL) gcj include classpath
if TESTSUBDIR
SUBDIRS += testsuite
endif

# write_entries_to_file - writes each entry in a list
# to the specified file.  Entries are written in chunks of
# $(write_entries_to_file_split) to accomodate systems with
# severe command-line-length limitations.
# Parameters:
# $(1): variable containing entries to iterate over
# $(2): output file
write_entries_to_file_split = 50
write_entries_to_file = $(shell rm -f $(2) || :) $(shell touch $(2)) \
	$(foreach range, \
	  $(shell i=1; while test $$i -le $(words $(1)); do \
	     echo $$i; i=`expr $$i + $(write_entries_to_file_split)`; done), \
	  $(shell echo $(wordlist $(range), \
			  $(shell expr $(range) + $(write_entries_to_file_split) - 1), $(1)) \
	     | tr ' ' '\012' >> $(2)))

## ################################################################

# autoconf2.13's target_alias
target_noncanonical = @target_noncanonical@

# This is required by TL_AC_GXX_INCLUDE_DIR.
libsubdir = $(libdir)/gcc/$(target_noncanonical)/$(gcc_version)

libexecsubdir = $(libexecdir)/gcc/$(target_noncanonical)/$(gcc_version)

##
## What gets installed, and where.
##

toolexeclib_LTLIBRARIES = libgcj.la libgij.la libgcj-tools.la
toolexecmainlib_DATA = libgcj.spec

##
## These files form a closed dependency group at the lower
## edge of libjava's dependency graph, so can be separated out.
## For details of how the list was generated, see the GCC wiki:
##
##   http://gcc.gnu.org/wiki/Internal_dependencies_of_libgcj
##
LOWER_PACKAGE_FILES_LO = \
	$(propertyo_files) \
	gnu-CORBA.lo \
	gnu-java-awt-dnd-peer-gtk.lo \
	gnu-java-awt-peer-gtk.lo \
	gnu-java-awt-peer-swing.lo \
	gnu-java-lang-management.lo \
	gnu-javax-management.lo \
	gnu-javax-rmi.lo \
	gnu-javax-sound-midi.lo \
	gnu-xml-aelfred2.lo \
	gnu-xml-dom.lo \
	gnu-xml-libxmlj.lo \
	gnu-xml-pipeline.lo \
	gnu-xml-stream.lo \
	gnu-xml-transform.lo \
	gnu-xml-util.lo \
	gnu-xml-validation.lo \
	gnu-xml-xpath.lo \
	java-lang-management.lo \
	javax-imageio.lo \
	javax-rmi.lo \
	jni-libjvm.lo \
	org-omg-CORBA.lo \
	org-omg-CORBA_2_3.lo \
	org-omg-CosNaming.lo \
	org-omg-Dynamic.lo \
	org-omg-DynamicAny.lo \
	org-omg-IOP.lo \
	org-omg-Messaging.lo \
	org-omg-PortableInterceptor.lo \
	org-omg-PortableServer.lo \
	org-omg-SendingContext.lo \
	org-omg-stub.lo \
	org-relaxng.lo \
	org-xml.lo \
	META-INF/services/java.util.prefs.PreferencesFactory.in.lo \
	META-INF/services/java.util.prefs.PreferencesFactory.lo \
	META-INF/services/javax.sound.midi.spi.MidiDeviceProvider.lo \
	META-INF/services/javax.sound.midi.spi.MidiFileReader.lo \
	META-INF/services/javax.sound.midi.spi.MidiFileWriter.lo \
	META-INF/services/javax.sound.sampled.spi.AudioFileReader.lo \
	classpath/native/jni/classpath/jcl.lo \
	classpath/native/jni/classpath/jnilink.lo \
	classpath/native/jni/java-math/gnu_java_math_GMP.lo \
	classpath/tools/libgcj_tools_la-tools.lo \
	gnu/awt.lo \
	gnu/awt/j2d.lo \
	gnu/gcj/io.lo \
	gnu/gcj/io/natSimpleSHSStream.lo \
	gnu/gcj/io/shs.lo \
	gnu/gcj/tools/gcj_dbtool.lo \
	gnu/gcj/util/natDebug.lo \
	gnu/gcj/util/natGCInfo.lo \
	gnu/java/awt/dnd.lo \
	gnu/java/awt/font.lo \
	gnu/java/awt/image.lo \
	gnu/java/awt/print.lo \
	gnu/java/awt/font/autofit.lo \
	gnu/java/awt/font/opentype.lo \
	gnu/java/awt/font/opentype/truetype.lo \
	gnu/java/lang/management/natVMClassLoadingMXBeanImpl.lo \
	gnu/java/lang/management/natVMCompilationMXBeanImpl.lo \
	gnu/java/lang/management/natVMGarbageCollectorMXBeanImpl.lo \
	gnu/java/lang/management/natVMMemoryMXBeanImpl.lo \
	gnu/java/lang/management/natVMMemoryManagerMXBeanImpl.lo \
	gnu/java/lang/management/natVMMemoryPoolMXBeanImpl.lo \
	gnu/java/lang/management/natVMOperatingSystemMXBeanImpl.lo \
	gnu/java/lang/management/natVMRuntimeMXBeanImpl.lo \
	gnu/java/lang/management/natVMThreadMXBeanImpl.lo \
	gnu/java/net/local.lo \
	gnu/java/net/protocol/ftp.lo \
	gnu/java/net/protocol/gcjlib.lo \
	gnu/java/net/protocol/https.lo \
	gnu/javax/imageio.lo \
	gnu/javax/print.lo \
	gnu/javax/sound.lo \
	gnu/javax/activation/viewers.lo \
	gnu/javax/imageio/bmp.lo \
	gnu/javax/imageio/gif.lo \
	gnu/javax/imageio/jpeg.lo \
	gnu/javax/imageio/png.lo \
	gnu/javax/naming/giop.lo \
	gnu/javax/naming/ictxImpl/trans.lo \
	gnu/javax/naming/jndi/url/corbaname.lo \
	gnu/javax/naming/jndi/url/rmi.lo \
	gnu/javax/print/ipp.lo \
	gnu/javax/print/ipp/attribute.lo \
	gnu/javax/print/ipp/attribute/defaults.lo \
	gnu/javax/print/ipp/attribute/job.lo \
	gnu/javax/print/ipp/attribute/printer.lo \
	gnu/javax/print/ipp/attribute/supported.lo \
	gnu/javax/security/auth/login.lo \
	gnu/javax/sound/sampled/AU.lo \
	gnu/javax/sound/sampled/WAV.lo \
	gnu/javax/swing/plaf/gnu.lo \
	gnu/javax/swing/plaf/metal.lo \
	java/sql.lo \
	java/awt/im.lo \
	java/awt/print.lo \
	java/awt/im/spi.lo \
	java/security/acl.lo \
	javax/activation.lo \
	javax/activity.lo \
	javax/management.lo \
	javax/naming.lo \
	javax/print.lo \
	javax/sql.lo \
	javax/tools.lo \
	javax/transaction.lo \
	javax/management/loading.lo \
	javax/management/openmbean.lo \
	javax/management/remote.lo \
	javax/management/remote/rmi.lo \
	javax/naming/directory.lo \
	javax/naming/event.lo \
	javax/naming/ldap.lo \
	javax/naming/spi.lo \
	javax/print/attribute.lo \
	javax/print/event.lo \
	javax/print/attribute/standard.lo \
	javax/security/cert.lo \
	javax/security/auth/kerberos.lo \
	javax/security/auth/login.lo \
	javax/security/auth/spi.lo \
	javax/sound/midi.lo \
	javax/sound/midi/spi.lo \
	javax/swing/plaf/multi.lo \
	javax/swing/plaf/synth.lo \
	javax/swing/text/rtf.lo \
	javax/swing/text/html/default.css.lo \
	javax/transaction/xa.lo \
	org/ietf/jgss.lo \
	sun/awt.lo

##
## Lists of all .lo files, and filtered core and non-core lists.
## 
ALL_PACKAGE_SOURCE_FILES_LO = $(all_packages_source_files:.list=.lo) $(propertyo_files) $(bc_objects)
NONCORE_PACKAGE_SOURCE_FILES_LO = $(filter $(LOWER_PACKAGE_FILES_LO),$(ALL_PACKAGE_SOURCE_FILES_LO))
CORE_PACKAGE_SOURCE_FILES_LO = $(filter-out $(LOWER_PACKAGE_FILES_LO),$(ALL_PACKAGE_SOURCE_FILES_LO))

##
## Assign objects to output libraries.
##
if BUILD_SUBLIBS
toolexeclib_LTLIBRARIES += libgcj-noncore.la
libgcj_noncore_la_LIBADD_SUBOBJECTS = $(NONCORE_PACKAGE_SOURCE_FILES_LO)
libgcj_la_LIBADD_SUBOBJECTS = $(CORE_PACKAGE_SOURCE_FILES_LO)
LIBJAVA_LDFLAGS_NOUNDEF = $(LIBGCJ_SUBLIB_LTFLAGS)
LIBJAVA_CORE_EXTRA = @LIBGCJ_SUBLIB_CORE_EXTRA_DEPS@
else
# If not building sublibraries, everything goes in libgcj,
# and it cannot be usefully built shared on PE platforms.
libgcj_la_LIBADD_SUBOBJECTS = $(ALL_PACKAGE_SOURCE_FILES_LO)
LIBJAVA_LDFLAGS_NOUNDEF = 
LIBJAVA_CORE_EXTRA = 
endif

if USE_LIBGCJ_BC
toolexeclib_LTLIBRARIES += libgcj_bc.la
endif

if XLIB_AWT
toolexeclib_LTLIBRARIES += lib-gnu-awt-xlib.la
endif

dbexec_LTLIBRARIES = libjvm.la

pkgconfigdir = $(libdir)/pkgconfig

jardir = $(datadir)/java
jar_DATA = libgcj-$(gcc_version).jar libgcj-tools-$(gcc_version).jar
if INSTALL_ECJ_JAR
jar_DATA += $(ECJ_BUILD_JAR)
endif

if JAVA_HOME_SET
JAVA_HOME_DIR = $(JAVA_HOME)
BOOT_CLASS_PATH_DIR = $(JAVA_HOME)/lib/rt.jar
else
JAVA_HOME_DIR = $(prefix)
BOOT_CLASS_PATH_DIR = $(jardir)/libgcj-$(gcc_version).jar
endif

## Name of the default .db.
db_name = classmap.db
## Compiler specific component of the .db file
gcjsubdir = @gcjsubdir@
db_pathtail = $(gcjsubdir)/$(db_name)

## For now, only on native systems.  FIXME.
if NATIVE
bin_programs = jv-convert gij grmic grmiregistry gcj-dbtool \
	gappletviewer gjarsigner gkeytool gjar gcjh gjavah gnative2ascii \
	gorbd grmid gserialver gtnameserv gc-analyze
if CREATE_GJDOC
bin_programs += gjdoc
endif

noinst_PROGRAMS =
if INSTALL_BINARIES
bin_PROGRAMS = $(bin_programs)
else
noinst_PROGRAMS += $(bin_programs)
endif

## It is convenient to actually build and install the default database
## when gcj-dbtool is available.
dbexec_DATA = $(db_name)
endif 

bin_SCRIPTS = contrib/rebuild-gcj-db contrib/aot-compile

if INSTALL_AOT_RPM
bin_SCRIPTS += contrib/aot-compile-rpm
endif

if BUILD_ECJ1
## We build ecjx and not ecj1 because in one mode, ecjx will not work
## until after 'make install', and we don't want it to be picked up in
## the build tree by gcj via a -B option.
libexecsub_PROGRAMS = ecjx
endif

## ################################################################

##
## Compilers and compilation flags.
##

## The compiler with whatever flags we want for both -c and -C
## compiles.
GCJ_WITH_FLAGS = $(GCJ) --encoding=UTF-8 -Wno-deprecated

extra_ldflags_libjava = @extra_ldflags_libjava@
extra_ldflags = @extra_ldflags@

if ANONVERSCRIPT
if ANONVERSCRIPT_GNU
version_arg = -Wl,--version-script=$(srcdir)/libgcj.ver
libgcj_la_version_arg = $(version_arg)
libgcj_tools_la_version_arg = $(version_arg)
lib_gnu_awt_xlib_la_version_arg = $(version_arg)
libgcj_bc_la_version_arg = $(version_arg)

version_dep = $(srcdir)/libgcj.ver
libgcj_la_version_dep = $(version_dep)
libgcj_tools_la_version_dep = $(version_dep)
lib_gnu_awt_xlib_la_version_dep = $(version_dep)
libgcj_bc_la_version_dep = $(version_dep)
endif
if ANONVERSCRIPT_SUN
libgcj_la_version_arg = -Wl,-M,libgcj.ver-sun
libgcj_tools_la_version_arg = -Wl,-M,libgcj_tools.ver-sun
lib_gnu_awt_xlib_la_version_arg = -Wl,-M,lib_gnu_awt_xlib.ver-sun
libgcj_bc_la_version_arg = -Wl,-M,libgcj_bc.ver-sun

libgcj_la_version_dep = libgcj.ver-sun
libgcj_tools_la_version_dep = libgcj_tools.ver-sun
lib_gnu_awt_xlib_la_version_dep = lib_gnu_awt_xlib.ver-sun
libgcj_bc_la_version_dep = libgcj_bc.ver-sun

# The pattern rule necessary to build the *.ver-sun mapfiles is at the end
# of the file, see below.
endif
else
version_arg =
libgcj_la_version_arg = $(version_arg)
libgcj_tools_la_version_arg = $(version_arg)
lib_gnu_awt_xlib_la_version_arg = $(version_arg)
libgcj_bc_la_version_arg = $(version_arg)

version_dep =
libgcj_la_version_dep = $(version_dep)
libgcj_tools_la_version_dep = $(version_dep)
lib_gnu_awt_xlib_la_version_dep = $(version_dep)
libgcj_bc_la_version_dep = $(version_dep)
endif

LTLDFLAGS = $(shell $(top_srcdir)/../libtool-ldflags $(LDFLAGS))
GCJLINK = $(LIBTOOL) --tag=GCJ $(LIBTOOLFLAGS) --mode=link $(GCJ) -L$(here) \
	  $(JC1FLAGS) $(LTLDFLAGS) -o $@
GCJ_FOR_ECJX = @GCJ_FOR_ECJX@
GCJ_FOR_ECJX_LINK = $(GCJ_FOR_ECJX) -o $@
LIBLINK = $(LIBTOOL) --tag=CXX $(LIBTOOLFLAGS) --mode=link $(CXX) -L$(here) \
	  $(JC1FLAGS) $(LTLDFLAGS) $(extra_ldflags_libjava) $(extra_ldflags) -o $@
CXXLINK = $(LIBTOOL) --tag=CXX $(LIBTOOLFLAGS) --mode=link $(CXXLD) \
	  $(AM_CXXFLAGS) $(CXXFLAGS) $(AM_LDFLAGS) $(LTLDFLAGS) -o $@

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
	$(WARNINGS) \
	-D_GNU_SOURCE \
	-DPREFIX="\"$(prefix)\"" \
	-DTOOLEXECLIBDIR="\"$(toolexeclibdir)\"" \
        -DJAVA_HOME="\"$(JAVA_HOME_DIR)\"" \
        -DBOOT_CLASS_PATH="\"$(BOOT_CLASS_PATH_DIR)\"" \
	-DJAVA_EXT_DIRS="\"$(jardir)/ext\"" \
	-DGCJ_ENDORSED_DIRS="\"$(jardir)/gcj-endorsed\"" \
	-DGCJ_VERSIONED_LIBDIR="\"$(dbexecdir)\"" \
	-DPATH_SEPARATOR="\"$(CLASSPATH_SEPARATOR)\"" \
	-DECJ_JAR_FILE="\"$(ECJ_JAR)\"" \
	-DLIBGCJ_DEFAULT_DATABASE="\"$(dbexecdir)/$(db_name)\"" \
	-DLIBGCJ_DEFAULT_DATABASE_PATH_TAIL="\"$(db_pathtail)\""

AM_GCJFLAGS = \
	@LIBGCJ_JAVAFLAGS@ \
	-fclasspath= -fbootclasspath=$(BOOTCLASSPATH) \
	--encoding=UTF-8 \
	-Wno-deprecated -fbootstrap-classes

AM_CFLAGS = @LIBGCJ_CFLAGS@
if USING_GCC
AM_CFLAGS += $(WARNINGS)
endif

if SUPPRESS_LIBGCJ_BC
LIBGCJ_BC_FLAGS =
else
LIBGCJ_BC_FLAGS = -findirect-dispatch -fno-indirect-classes
endif

if BUILD_LIBGCJ_REDUCED_REFLECTION
LIBGCJ_REDUCED_REFLECTION_FLAGS = -freduced-reflection
else
LIBGCJ_REDUCED_REFLECTION_FLAGS =
endif

## Extra CFLAGS used for JNI C sources shared with GNU Classpath.
PEDANTIC_CFLAGS = -ansi -pedantic -Wall -Wno-long-long

JCFLAGS = -g
JC1FLAGS = @LIBGCJ_JAVAFLAGS@ $(GCJFLAGS)

LIBFFIINCS = @LIBFFIINCS@

AM_CPPFLAGS = -I$(top_srcdir) \
	-Iinclude -I$(top_srcdir)/include \
	-I$(top_srcdir)/classpath/include \
	-Iclasspath/include \
	-I$(top_srcdir)/classpath/native/fdlibm \
	$(GCINCS) $(THREADINCS) $(INCLTDL) \
	$(GCC_UNWIND_INCLUDE) $(ZINCS) $(LIBFFIINCS)

BOOTCLASSPATH = $(srcdir)/classpath/lib

## ################################################################

##
## How to build libgcj.a and libgcj.jar
##

libgij_la_SOURCES = gij.cc
libgij_la_DEPENDENCIES = libgcj.la libgcj.spec
if BUILD_SUBLIBS
libgij_la_DEPENDENCIES += libgcj-noncore.la
endif
## See jv_convert_LDADD.
libgij_la_LIBADD = -L$(here)/.libs libgcj.la
## The mysterious backslash in the grep pattern is consumed by make.
libgij_la_LDFLAGS = -rpath $(toolexeclibdir) $(LIBJAVA_LDFLAGS_NOUNDEF) \
        -version-info `grep -v '^\#' $(srcdir)/libtool-version` $(LIBGCJ_LD_SYMBOLIC)
libgij_la_LINK = $(CXXLINK) $(libgij_la_LDFLAGS)

if INTERPRETER
libgcj_interpret_source_files = jvmti.cc interpret.cc
else
libgcj_interpret_source_files = 
endif

libgcj_la_SOURCES = prims.cc jni.cc exception.cc stacktrace.cc \
	link.cc defineclass.cc verify.cc $(libgcj_interpret_source_files) \
	$(nat_source_files)

## We need to compile at least the interpreter this way.
interpret.lo:  AM_CXXFLAGS += -fwrapv
## Necessary so EH works on 64-bit Solaris 2.
prims.lo: AM_CXXFLAGS += -fno-omit-frame-pointer

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
LIBJAVA_LDFLAGS_LIBMATH =
else
LIBJAVA_LDFLAGS_LIBMATH = -lm
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
libgcj_la_LDFLAGS = -rpath $(toolexeclibdir) $(THREADLDFLAGS) $(extra_ldflags) $(THREADLIBS) \
	$(LIBLTDL) $(SYS_ZLIBS) $(LIBJAVA_LDFLAGS_NOUNDEF) \
	-version-info `grep -v '^\#' $(srcdir)/libtool-version` \
	$(LIBGCJ_LD_SYMBOLIC_FUNCTIONS)
libgcj_la_LIBADD = \
	classpath/native/fdlibm/libfdlibm.la \
	java/lang/Object.lo \
	java/lang/Class.lo \
	java/process-$(PLATFORM).lo \
	$(libgcj_la_LIBADD_SUBOBJECTS) \
	$(LIBFFI) $(ZLIBS) $(GCLIBS)
libgcj_la_DEPENDENCIES = libgcj-$(gcc_version).jar \
	java/lang/Object.lo \
	java/lang/Class.lo \
	java/process-$(PLATFORM).lo \
	$(ALL_PACKAGE_SOURCE_FILES_LO) \
	$(LIBLTDL) $(libgcj_la_LIBADD) \
	$(LIBJAVA_CORE_EXTRA) \
	$(libgcj_la_version_dep)
libgcj_la_LINK = $(LIBLINK) $(libgcj_la_LDFLAGS) $(libgcj_la_version_arg)

## A hack to make sure the various gcj-related macros, like
## LTGCJCOMPILE, are defined by automake.  This is never actually
## compiled.
EXTRA_libgcj_la_SOURCES = java/lang/Object.java

if BUILD_SUBLIBS
## This extra target is invoked on windows hosts only by adding it
## to LIBGCJ_SUBLIB_CORE_EXTRA_SRCS in configure.host, which causes
## it to be linked in to the core DLL and generate a circular import
## dependency loop between the two DLLs.  This is required to fix
## PR42811 by ensuring that any application that links against
## either always loads both at runtime.
DLL_VERSION=`expr \`grep -v '^\#' $(srcdir)/libtool-version | sed -e 's/\(.*\):\(.*\):.*/\1 + \2/'\``
libgcj-noncore-dummy.dll.a : $(srcdir)/libgcj-noncore-dummy.def
	$(DLLTOOL) -d $^ -l $@ --dllname cyggcj-noncore-$(DLL_VERSION).dll 

## These are the libtool definitions for the noncore library.
libgcj_noncore_la_SOURCES =
libgcj_noncore_la_LDFLAGS = $(libgcj_la_LDFLAGS)
libgcj_noncore_la_LIBADD = $(libgcj_noncore_la_LIBADD_SUBOBJECTS) libgcj.la
libgcj_noncore_la_DEPENDENCIES = libgcj-$(gcc_version).jar $(LIBLTDL) \
		$(libgcj_noncore_la_LIBADD) libgcj.la $(libgcj_la_version_dep)
libgcj_noncore_la_LINK = $(libgcj_la_LINK)

endif	# BUILD_SUBLIBS

# We compile libgcj_tools with -findirect-dispatch so that they can
# depend on external classes: in particular, gjdoc uses antlr.  In
# addition, -fno-bootstrap-classes ensures that the tools are loaded
# by the system class loader rather than the bootstrap class loader:
# only core library classes should be loaded by the bootstrap loader.
libgcj_tools_la_SOURCES = classpath/tools/tools.zip
libgcj_tools_la_GCJFLAGS = $(AM_GCJFLAGS) -findirect-dispatch \
 -fno-bootstrap-classes -fno-indirect-classes \
 -fsource-filename=$(here)/classpath/tools/all-classes.lst
libgcj_tools_la_LDFLAGS = -rpath $(toolexeclibdir) \
 -version-info `grep -v '^\#' $(srcdir)/libtool-version` \
 $(LIBGCJ_LD_SYMBOLIC_FUNCTIONS) $(LIBJAVA_LDFLAGS_NOUNDEF) \
 $(LIBJAVA_LDFLAGS_LIBMATH)

libgcj_tools_la_LIBADD = libgcj.la
libgcj_tools_la_DEPENDENCIES = libgcj.la libgcj.spec \
 $(libgcj_tools_la_version_dep)
if BUILD_SUBLIBS
libgcj_tools_la_DEPENDENCIES += libgcj-noncore.la
endif
libgcj_tools_la_LINK = $(LIBLINK) $(libgcj_tools_la_LDFLAGS) \
 $(libgcj_tools_la_version_arg)

## libjvm.so
libjvm_la_SOURCES = jni-libjvm.cc
libjvm_la_DEPENDENCIES = libgcj.la libgcj.spec
if BUILD_SUBLIBS
libjvm_la_DEPENDENCIES += libgcj-noncore.la
endif
## See jv_convert_LDADD.
libjvm_la_LIBADD = -L$(here)/.libs libgcj.la
libjvm_la_LDFLAGS = -avoid-version $(LIBGCJ_LD_SYMBOLIC) \
 $(LIBJAVA_LDFLAGS_NOUNDEF)
libjvm_la_LINK = $(CXXLINK) $(libjvm_la_LDFLAGS)

## The .db file.  This rule is only used for native builds, so it is
## safe to invoke gcj-dbtool.
$(db_name): gcj-dbtool$(EXEEXT)
## In case it exists already.
	@rm -f $(db_name)
## We don't actually care if it fails -- if it does, just make an
## empty file.  This is simpler than trying to discover when mmap is
## not available.
	./gcj-dbtool -n $(db_name) || touch $(db_name)

lib_gnu_awt_xlib_la_SOURCES = $(xlib_nat_source_files)
lib_gnu_awt_xlib_la_LIBADD = gnu/awt/xlib.lo gnu/gcj/xlib.lo
lib_gnu_awt_xlib_la_DEPENDENCIES = libgcj-$(gcc_version).jar \
	libgcj.la libgcj.spec \
	$(lib_gnu_awt_xlib_la_version_dep) \
	$(lib_gnu_awt_xlib_la_LIBADD)
if BUILD_SUBLIBS
lib_gnu_awt_xlib_la_DEPENDENCIES += libgcj-noncore.la
endif
## We require libstdc++-v3 to be in the same build tree.
lib_gnu_awt_xlib_la_CPPFLAGS = \
	$(AM_CPPFLAGS) \
	-I../libstdc++-v3/include \
	-I../libstdc++-v3/include/$(target_noncanonical) \
	-I$(srcdir)/../libstdc++-v3/libsupc++
## The mysterious backslash in the grep pattern is consumed by make.
lib_gnu_awt_xlib_la_LDFLAGS = ../libstdc++-v3/src/libstdc++.la \
	@X_PRE_LIBS@ @X_LIBS@ -lX11 @X_EXTRA_LIBS@ \
        -rpath $(toolexeclibdir) $(LIBJAVA_LDFLAGS_NOUNDEF) \
        -version-info `grep -v '^\#' $(srcdir)/libtool-version` $(LIBGCJ_LD_SYMBOLIC)
lib_gnu_awt_xlib_la_LINK = $(LIBLINK) $(lib_gnu_awt_xlib_la_LDFLAGS) \
	$(lib_gnu_awt_xlib_la_version_arg)

## Support for libgcj_bc: dummy shared library.
##
## This lets us have one soname in BC objects and another in C++ ABI objects.
## This library is not linked against libgcj.
libgcj_bc_la_SOURCES = libgcj_bc.c
libgcj_bc_la_LDFLAGS = -rpath $(toolexeclibdir) -no-static -version-info 1:0:0 \
	$(LIBGCJ_LD_SYMBOLIC_FUNCTIONS) $(LIBJAVA_LDFLAGS_NOUNDEF)
libgcj_bc_la_DEPENDENCIES = libgcj.la $(libgcj_bc_la_version_dep)
libgcj_bc_la_LINK = $(LIBLINK) $(libgcj_bc_la_LDFLAGS) \
	$(libgcj_bc_la_version_arg)
## This is specific to Linux/{Free,Net,Open}BSD/Hurd and perhaps few others.
## USE_LIBGCJ_BC shouldn't be set on other targets.
libgcj_bc_dummy_LINK = $(CC) -L$(here)/.libs $(CFLAGS) $(LDFLAGS) -shared \
	-fPIC -nostdlib

## This rule creates the libgcj_bc dummy library in the .libs directory, for use
## when testing.
libgcj_bc.la: $(libgcj_bc_la_OBJECTS) $(libgcj_bc_la_DEPENDENCIES)
	$(libgcj_bc_la_LINK) $(am_libgcj_bc_la_rpath) $(libgcj_bc_la_LDFLAGS) \
	$(libgcj_bc_la_OBJECTS) $(libgcj_bc_la_LIBADD) $(LIBS) || exit; \
	rm .libs/libgcj_bc.so; \
	mv .libs/libgcj_bc.so.1.0.0 .libs/libgcj_bc.so; \
	$(libgcj_bc_dummy_LINK) -xc /dev/null -Wl,-soname,libgcj_bc.so.1 \
	-o .libs/libgcj_bc.so.1.0.0 -lgcj || exit; \
	rm .libs/libgcj_bc.so.1; \
	$(LN_S) libgcj_bc.so.1.0.0 .libs/libgcj_bc.so.1

## Note that property_files is defined in sources.am.
propertyo_files = $(patsubst classpath/resource/%,%,$(addsuffix .lo,$(property_files)))

$(propertyo_files): %.lo: classpath/resource/%
	$(mkinstalldirs) `dirname $@`; \
	$(LTGCJCOMPILE) -o $@ -c $< -Wc,--resource,$(@:.lo=)

libgcj-$(gcc_version).jar: classpath/lib/compile-classes
## Note that this now omits the property files.
## It doesn't matter since we don't use the jar at runtime.
	here=`pwd`; cd $(srcdir)/classpath/lib; \
	find gnu java javax org sun -name .svn -prune -o -name '*.class' -print | \
	$(JAR) -cfM@ $$here/libgcj-$(gcc_version).jar

libgcj-tools-$(gcc_version).jar: classpath/tools/tools.zip
	cp $< $@

CLEANFILES = libgcj-$(gcc_version).jar libgcj-tools-$(gcc_version).jar
DISTCLEANFILES = native.dirs

mostlyclean-local:
## Use libtool rm to remove each libtool object
	find . -name '*.lo' -print | xargs $(LIBTOOL) --mode=clean rm -f

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

# Concurrent things use reflection internally.
java/util/concurrent.lo \
java/util/concurrent/atomic.lo \
java/util/concurrent/locks.lo: LIBGCJ_REDUCED_REFLECTION_FLAGS =

%.lo: %.list
	$(LTGCJCOMPILE) $(LIBGCJ_REDUCED_REFLECTION_FLAGS) -c -o $@ -fsource-filename=$(here)/classpath/lib/classes -MT $@ -MD -MP -MF $(basename $@).deps @$<

java/lang/Object.lo: classpath/lib/java/lang/Object.class
	$(LTGCJCOMPILE) -c -o $@ -fsource-filename=$(srcdir)/$(basename $@).java $<

java/lang/Class.lo: classpath/lib/java/lang/Class.class
	$(LTGCJCOMPILE) -c -o $@  -fsource-filename=$(srcdir)/$(basename $@).java $<

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

inner_nat_headers = java/io/ObjectOutputStream$$PutField.h \
	java/io/ObjectInputStream$$GetField.h \
	java/nio/DirectByteBufferImpl$$ReadWrite.h \
	java/nio/channels/Pipe$$SinkChannel.h \
	java/nio/channels/Pipe$$SourceChannel.h \
	java/lang/VMProcess$ImmediateEOFInputStream.h \
	java/lang/reflect/Proxy$$ProxyData.h \
	java/lang/reflect/Proxy$$ProxyType.h \
	gnu/java/net/PlainSocketImpl$$SocketInputStream.h \
	gnu/java/net/PlainSocketImpl$$SocketOutputStream.h \
	gnu/java/nio/PipeImpl$$SinkChannelImpl.h \
	gnu/java/nio/PipeImpl$$SourceChannelImpl.h

generic_header_files = $(ordinary_header_files) $(inner_nat_headers) \
	$(xlib_nat_headers)

MYGCJH = gjavah

$(generic_header_files): gcjh.stamp

gcjh.stamp: classpath/lib/compile-classes
if JAVA_MAINTAINER_MODE
	$(MYGCJH) --cni --all $(srcdir)/classpath/lib \
	    --cmdfile=$(srcdir)/headers.txt -d $(srcdir) --force
endif
	echo > gcjh.stamp

nat_headers = $(ordinary_header_files) $(inner_nat_headers)
nat_headers_install = $(ordinary_header_files)

xlib_nat_headers = $(gnu_awt_xlib_header_files) $(gnu_gcj_xlib_header_files)

## Headers we maintain by hand and which we want to install.
extra_headers = java/lang/Object.h java/lang/Class.h

$(extra_headers) $(srcdir)/java/lang/Object.h $(srcdir)/java/lang/Class.h:
	@:

install-exec-hook: install-binPROGRAMS install-toolexeclibLTLIBRARIES \
	install-libexecsubPROGRAMS
## Support for libgcj_bc: dummy shared library used only at link-time.
if USE_LIBGCJ_BC
## Install libgcj_bc dummy lib in the target directory. We also need to delete
## libtool's .la file, this prevents libtool resetting the lib again 
## later.
	@echo Installing dummy lib libgcj_bc.so.1.0.0; \
	rm $(DESTDIR)$(toolexeclibdir)/libgcj_bc.so; \
	mv $(DESTDIR)$(toolexeclibdir)/libgcj_bc.so.1.0.0 $(DESTDIR)$(toolexeclibdir)/libgcj_bc.so; \
	$(libgcj_bc_dummy_LINK) -xc /dev/null -Wl,-soname,libgcj_bc.so.1 \
	-o $(DESTDIR)$(toolexeclibdir)/libgcj_bc.so.1.0.0 -lgcj || exit; \
	rm $(DESTDIR)$(toolexeclibdir)/libgcj_bc.so.1; \
	$(LN_S) libgcj_bc.so.1.0.0 $(DESTDIR)$(toolexeclibdir)/libgcj_bc.so.1; \
	rm $(DESTDIR)$(toolexeclibdir)/libgcj_bc.la;
endif
if BUILD_ECJ1
## Undo any prepending of the target prefix and transform
## to the actual host's executable suffix.
	mv $(DESTDIR)$(libexecsubdir)/`echo ecjx | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` $(DESTDIR)$(libexecsubdir)/ecj1$(host_exeext)
endif BUILD_ECJ1

## Install the headers.  It is fairly ugly that we have to do this by
## hand.
install_data_local_split = 50
install-data-local:
	$(PRE_INSTALL)
## Install the .pc file.
	@pc_version=`echo $(GCJVERSION) | sed -e 's/[.][^.]*$$//'`; \
	file="libgcj-$${pc_version}.pc"; \
	$(mkinstalldirs) $(DESTDIR)$(pkgconfigdir); \
	echo "  $(INSTALL_DATA) libgcj.pc $(DESTDIR)$(pkgconfigdir)/$$file"; \
	$(INSTALL_DATA) libgcj.pc $(DESTDIR)$(pkgconfigdir)/$$file
## We use a GNU make trick here so that we don't go over the command
## length limit of some shells.
	@echo Creating list of headers to install...
	@: $(call write_entries_to_file,$(nat_headers_install) $(extra_headers),tmp-ilist)
	@srcdirstrip=`echo "$(srcdir)" | sed 's/[].[^$$\\*]/\\\\&/g'`; \
	while read file; do echo "$$file $$file"; done < tmp-ilist | \
	sed "s| $$srcdirstrip/| |;"' / .*\//!s/ .*/ ./; s,\( .*\)/[^/]*$$,\1,' | \
	$(AWK) 'BEGIN { files["."] = "" } \
	  { files[$$2] = files[$$2] " " $$1; \
	    if (++n[$$2] == $(install_data_local_split)) { print $$2, files[$$2]; n[$$2] = 0; files[$$2] = "" } } \
	     END { for (dir in files) print dir, files[dir] }' | \
	while read dir files; do \
	  xfiles=; for file in $$files; do \
	    if test -f "$$file"; then xfiles="$$xfiles $$file"; \
	    else xfiles="$$xfiles $(srcdir)/$$file"; fi; done; \
	  test -z "$$xfiles" || { \
	    test "x$$dir" = x. || { \
	      echo "$(mkinstalldirs) $(DESTDIR)$(gxx_include_dir)/$$dir"; \
	      $(mkinstalldirs) $(DESTDIR)$(gxx_include_dir)/$$dir; }; \
	    echo " $(INSTALL_DATA) $$xfiles $(DESTDIR)$(gxx_include_dir)/$$dir"; \
	    $(INSTALL_DATA) $$xfiles $(DESTDIR)$(gxx_include_dir)/$$dir; }; \
	done
	-@rm -f tmp-ilist
## Install inner class headers.
	$(INSTALL_DATA) $(srcdir)/'java/io/ObjectOutputStream$$PutField.h' $(DESTDIR)$(gxx_include_dir)/java/io/
	$(INSTALL_DATA) $(srcdir)/'java/io/ObjectInputStream$$GetField.h' $(DESTDIR)$(gxx_include_dir)/java/io/
	$(INSTALL_DATA) $(srcdir)/'java/nio/channels/Pipe$$SinkChannel.h' $(DESTDIR)$(gxx_include_dir)/java/nio/channels/
	$(INSTALL_DATA) $(srcdir)/'java/nio/channels/Pipe$$SourceChannel.h' $(DESTDIR)$(gxx_include_dir)/java/nio/channels/
	$(INSTALL_DATA) $(srcdir)/'java/lang/reflect/Proxy$$ProxyData.h' $(DESTDIR)$(gxx_include_dir)/java/lang/reflect/
	$(INSTALL_DATA) $(srcdir)/'java/lang/reflect/Proxy$$ProxyType.h' $(DESTDIR)$(gxx_include_dir)/java/lang/reflect/
	$(INSTALL_DATA) $(srcdir)/'gnu/java/net/PlainSocketImpl$$SocketInputStream.h' $(DESTDIR)$(gxx_include_dir)/gnu/java/net/
	$(INSTALL_DATA) $(srcdir)/'gnu/java/net/PlainSocketImpl$$SocketOutputStream.h' $(DESTDIR)$(gxx_include_dir)/gnu/java/net/
	$(INSTALL_DATA) $(srcdir)/'gnu/java/nio/PipeImpl$$SinkChannelImpl.h' $(DESTDIR)$(gxx_include_dir)/gnu/java/nio/
	$(INSTALL_DATA) $(srcdir)/'gnu/java/nio/PipeImpl$$SourceChannelImpl.h' $(DESTDIR)$(gxx_include_dir)/gnu/java/nio/
## Don't install java/nio/DirectByteBufferImpl$$ReadWrite.h here. It's for internal use only.
## Install Python modules for aot-compile.
	$(mkinstalldirs) $(DESTDIR)$(python_mod_dir); \
	$(INSTALL_DATA) $(srcdir)/contrib/classfile.py \
	  $(DESTDIR)$(python_mod_dir)/classfile.py; \
	$(INSTALL_DATA) contrib/aotcompile.py \
	  $(DESTDIR)$(python_mod_dir)/aotcompile.py;
if CREATE_JAVA_HOME
## Create sdk style directories
	$(mkinstalldirs) $(DESTDIR)$(JRE_BIN_DIR)
	$(mkinstalldirs) $(DESTDIR)$(SDK_BIN_DIR)
	$(mkinstalldirs) $(DESTDIR)$(JRE_LIB_DIR)
	$(mkinstalldirs) $(DESTDIR)$(JRE_LIB_DIR)/$(CPU)
	$(mkinstalldirs) $(DESTDIR)$(SDK_INCLUDE_DIR)
	$(mkinstalldirs) $(DESTDIR)$(SDK_INCLUDE_DIR)/$(OS)
	relative() { \
	  $(PERL) -e 'use File::Spec; \
	    print File::Spec->abs2rel($$ARGV[0], $$ARGV[1])' $$1 $$2; \
	}; \
	RELATIVE=$$(relative $(DESTDIR)$(bindir) $(DESTDIR)$(SDK_BIN_DIR)); \
	ln -sf $$RELATIVE/`echo gij | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` \
	  $(DESTDIR)$(SDK_BIN_DIR)/java; \
	ln -sf $$RELATIVE/`echo gjar | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` \
	  $(DESTDIR)$(SDK_BIN_DIR)/jar; \
	ln -sf $$RELATIVE/`echo gjdoc | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` \
	  $(DESTDIR)$(SDK_BIN_DIR)/javadoc; \
	ln -sf $$RELATIVE/`echo grmic | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` \
	  $(DESTDIR)$(SDK_BIN_DIR)/rmic; \
	ln -sf $$RELATIVE/`echo gjavah | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` \
	  $(DESTDIR)$(SDK_BIN_DIR)/javah; \
	ln -sf $$RELATIVE/`echo gappletviewer | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` \
	  $(DESTDIR)$(SDK_BIN_DIR)/appletviewer; \
	ln -sf $$RELATIVE/`echo gjarsigner | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` \
	  $(DESTDIR)$(SDK_BIN_DIR)/jarsigner; \
	ln -sf $$RELATIVE/`echo grmiregistry | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` \
	  $(DESTDIR)$(SDK_BIN_DIR)/rmiregistry; \
	ln -sf $$RELATIVE/`echo gkeytool | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` \
	  $(DESTDIR)$(SDK_BIN_DIR)/keytool; \
	ln -sf $$RELATIVE/`echo gorbd | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` \
	  $(DESTDIR)$(SDK_BIN_DIR)/orbd; \
	ln -sf $$RELATIVE/`echo gnative2ascii | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` \
	  $(DESTDIR)$(SDK_BIN_DIR)/native2ascii; \
	ln -sf $$RELATIVE/`echo grmid | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` \
	  $(DESTDIR)$(SDK_BIN_DIR)/rmid; \
	ln -sf $$RELATIVE/`echo gtnameserv | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` \
	  $(DESTDIR)$(SDK_BIN_DIR)/tnameserv; \
	ln -sf $$RELATIVE/`echo gserialver | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` \
	  $(DESTDIR)$(SDK_BIN_DIR)/serialver; \
	RELATIVE=$$(relative $(DESTDIR)$(bindir) $(DESTDIR)$(JRE_BIN_DIR)); \
	ln -sf $$RELATIVE/`echo grmiregistry | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` \
	  $(DESTDIR)$(JRE_BIN_DIR)/rmiregistry; \
	ln -sf $$RELATIVE/`echo gkeytool | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` \
	  $(DESTDIR)$(JRE_BIN_DIR)/keytool; \
	ln -sf $$RELATIVE/`echo gij | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` \
	  $(DESTDIR)$(JRE_BIN_DIR)/java; \
	ln -sf $$RELATIVE/`echo gorbd | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` \
	  $(DESTDIR)$(JRE_BIN_DIR)/orbd; \
	ln -sf $$RELATIVE/`echo grmid | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` \
	  $(DESTDIR)$(JRE_BIN_DIR)/rmid; \
	ln -sf $$RELATIVE/`echo gtnameserv | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` \
	  $(DESTDIR)$(JRE_BIN_DIR)/tnameserv; \
	$(mkinstalldirs) $(DESTDIR)$(JVM_JAR_DIR); \
	version=$(JAVA_VERSION).$(BUILD_VERSION); \
	working_dir=`pwd`; \
	cd $(DESTDIR)$(JVM_JAR_DIR); \
	  RELATIVE=$$(relative $(DESTDIR)$(JRE_LIB_DIR) \
	    $(DESTDIR)$(JVM_JAR_DIR)); \
	  for jarname in jaas jce jdbc-stdext jndi jndi-cos jndi-dns \
	    jndi-ldap jndi-rmi jsse sasl; \
	  do \
	    ln -sf $$RELATIVE/rt.jar $$jarname-$$version.jar; \
	  done; \
	  RELATIVE=$$(relative $(DESTDIR)$(jardir) \
	    $(DESTDIR)$(JVM_JAR_DIR)); \
	  for jar in *-$$version.jar; \
	  do \
	    ln -sf $$jar $$(echo $$jar | sed "s|-$$version.jar|-$(JAVA_VERSION).jar|g"); \
	    ln -sf $$jar $$(echo $$jar | sed "s|-$$version.jar|.jar|g"); \
	  done; \
	cd $$working_dir; \
	$(mkinstalldirs) $(DESTDIR)$(JRE_LIB_DIR)/security; \
	RELATIVE=$$(relative $(DESTDIR)$(toolexeclibdir)/security \
	  $(DESTDIR)$(JRE_LIB_DIR)/security); \
	cd $(DESTDIR)$(JRE_LIB_DIR)/security; \
	  ln -sf $$RELATIVE/classpath.security java.security; \
	cd $$working_dir; \
	$(mkinstalldirs) $(DESTDIR)$(SDK_INCLUDE_DIR)/linux; \
	$(mkinstalldirs) $(DESTDIR)$(JRE_LIB_DIR)/$(CPU)/client; \
	$(mkinstalldirs) $(DESTDIR)$(JRE_LIB_DIR)/$(CPU)/server; \
	$(mkinstalldirs) $(DESTDIR)$(SDK_LIB_DIR); \
	RELATIVE=$$(relative $(DESTDIR)$(datadir)/java \
	  $(DESTDIR)$(JRE_LIB_DIR)); \
	ln -sf $$RELATIVE/libgcj-$(gcc_version).jar \
	  $(DESTDIR)$(JRE_LIB_DIR)/rt.jar; \
	RELATIVE=$$(relative $(DESTDIR)$(dbexecdir) \
	  $(DESTDIR)$(JRE_LIB_DIR)/$(CPU)); \
	ln -sf $$RELATIVE/libjawt.so \
	  $(DESTDIR)$(JRE_LIB_DIR)/$(CPU)/libjawt.so; \
	RELATIVE=$$(relative $(DESTDIR)$(dbexecdir) \
	  $(DESTDIR)$(JRE_LIB_DIR)/$(CPU)/client); \
	ln -sf $$RELATIVE/libjvm.so \
	  $(DESTDIR)$(JRE_LIB_DIR)/$(CPU)/client/libjvm.so; \
	RELATIVE=$$(relative $(DESTDIR)$(dbexecdir) \
	  $(DESTDIR)$(JRE_LIB_DIR)/$(CPU)/server); \
	ln -sf $$RELATIVE/libjvm.so \
	  $(DESTDIR)$(JRE_LIB_DIR)/$(CPU)/server/libjvm.so;	\
	RELATIVE=$$(relative $(DESTDIR)$(datadir)/java \
	  $(DESTDIR)$(SDK_LIB_DIR)); \
	ln -sf $$RELATIVE/libgcj-tools-$(gcc_version).jar \
	  $(DESTDIR)$(SDK_LIB_DIR)/tools.jar; \
	for headername in jawt jni; do \
	  DIRECTORY=$$(dirname $$($(DESTDIR)$(bindir)/`echo gcj | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` \
	    -print-file-name=include/$$headername.h)); \
	  RELATIVE=$$(relative $$DIRECTORY \
	    $(DESTDIR)$(SDK_INCLUDE_DIR)); \
	  ln -sf $$RELATIVE/$$headername.h \
	    $(DESTDIR)$(SDK_INCLUDE_DIR)/$$headername.h; \
	done; \
	for headername in jawt_md jni_md; do \
	  DIRECTORY=$$(dirname $$($(DESTDIR)$(bindir)/`echo gcj | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` \
	    -print-file-name=include/$$headername.h)); \
	  RELATIVE=$$(relative $$DIRECTORY \
	    $(DESTDIR)$(SDK_INCLUDE_DIR)/linux); \
	  ln -sf $$RELATIVE/$$headername.h \
	    $(DESTDIR)$(SDK_INCLUDE_DIR)/linux/$$headername.h; \
	done; \
	RELATIVE=$$(relative $(DESTDIR)$(datadir)/java \
	  $(DESTDIR)$(JVM_ROOT_DIR)/$(SDK_DIR)); 
endif

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
noinst_PROGRAMS += gen-from-JIS

gen_from_JIS_SOURCES = \
	gnu/gcj/convert/gen-from-JIS.c \
	gnu/gcj/convert/make-trie.c \
	gnu/gcj/convert/make-trie.h

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
	-rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS) $(extra_ldflags)
jv_convert_LINK = $(GCJLINK) $(jv_convert_LDFLAGS)
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
if BUILD_SUBLIBS
jv_convert_DEPENDENCIES += libgcj-noncore.la
endif

gcj_dbtool_SOURCES = \
gnu/gcj/tools/gcj_dbtool/natMain.cc

## We need -nodefaultlibs because we want to avoid gcj's `-lgcj'.  We
## need this because we are explicitly using libtool to link using the
## `.la' file.
gcj_dbtool_LDFLAGS = --main=gnu.gcj.tools.gcj_dbtool.Main \
	-rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS) $(extra_ldflags)
gcj_dbtool_LINK = $(GCJLINK) $(gcj_dbtool_LDFLAGS)
## We don't explicitly link in the libraries we need; libgcj.la brings
## in all dependencies.  We need the -L so that gcj can find libgcj
## with `-lgcj', but it must come first, otherwise the -L flags
## brought in from libgcj.la would cause the install directories to be
## searched before the build-tree ones, and we'd get errors because of
## different libraries with the same SONAME from picky linkers such as
## Solaris'.  FIXME: should be _libs on some systems.
gcj_dbtool_LDADD = gnu/gcj/tools/gcj_dbtool.lo -L$(here)/.libs libgcj.la
## Depend on the spec file to make sure it is up to date before
## linking this program.
gcj_dbtool_DEPENDENCIES = gnu/gcj/tools/gcj_dbtool.lo libgcj.la libgcj.spec
if BUILD_SUBLIBS
gcj_dbtool_DEPENDENCIES += libgcj-noncore.la
endif

gc_analyze_SOURCES =

## We need -nodefaultlibs because we want to avoid gcj's `-lgcj'.  We
## need this because we are explicitly using libtool to link using the
## `.la' file.
gc_analyze_LDFLAGS = --main=gnu.gcj.tools.gc_analyze.MemoryAnalyze \
        -rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS) $(extra_ldflags)
gc_analyze_LINK = $(GCJLINK) $(gc_analyze_LDFLAGS)
## See jv_convert_LDADD.
gc_analyze_LDADD = -L$(here)/.libs libgcj-tools.la libgcj.la
## Depend on the spec file to make sure it is up to date before
## linking this program.
gc_analyze_DEPENDENCIES = libgcj-tools.la libgcj.la libgcj.spec
if BUILD_SUBLIBS
gc_analyze_DEPENDENCIES += libgcj-noncore.la
endif

gij_SOURCES = 
## We need -nodefaultlibs because we want to avoid gcj's `-lgcj'.  We
## need this because we are explicitly using libtool to link using the
## `.la' file.
gij_LDFLAGS = -rpath $(dbexecdir) -rpath $(toolexeclibdir) \
	-shared-libgcc $(THREADLDFLAGS) $(extra_ldflags)
gij_LINK = $(GCJLINK) $(gij_LDFLAGS)
## See jv_convert_LDADD.
gij_LDADD = -L$(here)/.libs libgij.la
## Depend on the spec file to make sure it is up to date before
## linking this program.
gij_DEPENDENCIES = libgij.la

## Build an ecjx from a .jar.
ecjx_SOURCES = ecjx.cc
## We use the BC ABI here so that we don't need to compile ecj.jar.
## Hopefully the user has compiled it into his system .db.
## However, even if not it will run reasonably quickly.

ECJX_BASE_FLAGS = -findirect-dispatch \
	--main=org.eclipse.jdt.internal.compiler.batch.GCCMain

if NATIVE

ecjx_LINK = $(GCJLINK) $(ecjx_LDFLAGS)

if ENABLE_SHARED
## Use ecj.jar at runtime.
ecjx_LDFLAGS = $(ECJX_BASE_FLAGS) -Djava.class.path=$(ECJ_JAR)
else !ENABLE_SHARED
## Use ecj.jar at compile time.
ecjx_LDFLAGS = $(ECJX_BASE_FLAGS) $(ECJ_BUILD_JAR) -fbootclasspath=$(BOOTCLASSPATH)
endif !ENABLE_SHARED

ecjx_LDADD = -L$(here)/.libs $(extra_ldflags)
ecjx_DEPENDENCIES = libgcj.la libgcj.spec
if BUILD_SUBLIBS
ecjx_DEPENDENCIES += libgcj-noncore.la
endif
if USE_LIBGCJ_BC
ecjx_DEPENDENCIES += libgcj_bc.la
else
ecjx_LDADD += libgcj.la
endif

else !NATIVE

ecjx_LINK = $(GCJ_FOR_ECJX_LINK) $(ecjx_LDFLAGS)
ecjx_LDFLAGS = $(ECJX_BASE_FLAGS) $(ECJ_BUILD_JAR)
ecjx_LDADD = 
ecjx_DEPENDENCIES = 
ecjx.$(OBJEXT): ecjx.cc
	$(GCC_FOR_ECJX) -c $<

endif !NATIVE

## This is a dummy definition.
gappletviewer_SOURCES =
gappletviewer_LDFLAGS = --main=gnu.classpath.tools.appletviewer.Main \
	-rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS) $(extra_ldflags)
gappletviewer_LINK = $(GCJLINK) $(gappletviewer_LDFLAGS)
## See jv_convert_LDADD.
gappletviewer_LDADD = -L$(here)/.libs libgcj-tools.la
gappletviewer_DEPENDENCIES = libgcj-tools.la

## This is a dummy definition.
gjarsigner_SOURCES =
gjarsigner_LDFLAGS = --main=gnu.classpath.tools.jarsigner.Main \
	-rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS) $(extra_ldflags)
gjarsigner_LINK = $(GCJLINK) $(gjarsigner_LDFLAGS)
## See jv_convert_LDADD.
gjarsigner_LDADD = -L$(here)/.libs libgcj-tools.la
gjarsigner_DEPENDENCIES = libgcj-tools.la

## This is a dummy definition.
gkeytool_SOURCES =
gkeytool_LDFLAGS = --main=gnu.classpath.tools.keytool.Main \
	-rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS) $(extra_ldflags)
gkeytool_LINK = $(GCJLINK) $(gkeytool_LDFLAGS)
## See jv_convert_LDADD.
gkeytool_LDADD = -L$(here)/.libs libgcj-tools.la
gkeytool_DEPENDENCIES = libgcj-tools.la

## This is a dummy definition.
gjar_SOURCES =
gjar_LDFLAGS = --main=gnu.classpath.tools.jar.Main \
	-rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS) $(extra_ldflags)
gjar_LINK = $(GCJLINK) $(gjar_LDFLAGS)
## See jv_convert_LDADD.
gjar_LDADD = -L$(here)/.libs libgcj-tools.la
gjar_DEPENDENCIES = libgcj-tools.la

## This is a dummy definition.
gjavah_SOURCES =
gjavah_LDFLAGS = --main=gnu.classpath.tools.javah.Main \
	-rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS) $(extra_ldflags)
gjavah_LINK = $(GCJLINK) $(gjavah_LDFLAGS)
## See jv_convert_LDADD.
gjavah_LDADD = -L$(here)/.libs libgcj-tools.la
gjavah_DEPENDENCIES = libgcj-tools.la

## This is a dummy definition.
gcjh_SOURCES =
gcjh_LDFLAGS = --main=gnu.classpath.tools.javah.GcjhMain \
	-rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS) $(extra_ldflags)
gcjh_LINK = $(GCJLINK) $(gcjh_LDFLAGS)
## See jv_convert_LDADD.
gcjh_LDADD = -L$(here)/.libs libgcj-tools.la
gcjh_DEPENDENCIES = libgcj-tools.la

## This is a dummy definition.
gnative2ascii_SOURCES =
gnative2ascii_LDFLAGS = --main=gnu.classpath.tools.native2ascii.Native2ASCII \
	-rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS) $(extra_ldflags)
gnative2ascii_LINK = $(GCJLINK) $(gnative2ascii_LDFLAGS)
## See jv_convert_LDADD.
gnative2ascii_LDADD = -L$(here)/.libs libgcj-tools.la
gnative2ascii_DEPENDENCIES = libgcj-tools.la

## This is a dummy definition.
gorbd_SOURCES =
gorbd_LDFLAGS = --main=gnu.classpath.tools.orbd.Main \
	-rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS) $(extra_ldflags)
gorbd_LINK = $(GCJLINK) $(gorbd_LDFLAGS)
## See jv_convert_LDADD.
gorbd_LDADD = -L$(here)/.libs libgcj-tools.la
gorbd_DEPENDENCIES = libgcj-tools.la

## This is a dummy definition.
grmid_SOURCES =
grmid_LDFLAGS = --main=gnu.classpath.tools.rmid.Main \
	-rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS) $(extra_ldflags)
grmid_LINK = $(GCJLINK) $(grmid_LDFLAGS)
## See jv_convert_LDADD.
grmid_LDADD = -L$(here)/.libs libgcj-tools.la
grmid_DEPENDENCIES = libgcj-tools.la

## This is a dummy definition.
gserialver_SOURCES =
gserialver_LDFLAGS = --main=gnu.classpath.tools.serialver.SerialVer \
	-rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS) $(extra_ldflags)
gserialver_LINK = $(GCJLINK) $(gserialver_LDFLAGS)
## See jv_convert_LDADD.
gserialver_LDADD = -L$(here)/.libs libgcj-tools.la
gserialver_DEPENDENCIES = libgcj-tools.la

## This is a dummy definition.
gtnameserv_SOURCES =
gtnameserv_LDFLAGS = --main=gnu.classpath.tools.tnameserv.Main \
	-rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS) $(extra_ldflags)
gtnameserv_LINK = $(GCJLINK) $(gtnameserv_LDFLAGS)
## See jv_convert_LDADD.
gtnameserv_LDADD = -L$(here)/.libs libgcj-tools.la
gtnameserv_DEPENDENCIES = libgcj-tools.la

## This is a dummy definition.
grmic_SOURCES =
grmic_LDFLAGS = --main=gnu.classpath.tools.rmic.Main \
	-rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS) $(extra_ldflags)
grmic_LINK = $(GCJLINK) $(grmic_LDFLAGS)
## See jv_convert_LDADD.
grmic_LDADD = -L$(here)/.libs libgcj-tools.la
grmic_DEPENDENCIES = libgcj-tools.la

## This is a dummy definition.
grmiregistry_SOURCES =
grmiregistry_LDFLAGS = --main=gnu.classpath.tools.rmiregistry.Main \
	-rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS) $(extra_ldflags)
grmiregistry_LINK = $(GCJLINK) $(grmiregistry_LDFLAGS)
## See jv_convert_LDADD.
grmiregistry_LDADD = -L$(here)/.libs libgcj-tools.la
grmiregistry_DEPENDENCIES = libgcj-tools.la

## This is a dummy definition.
gjdoc_SOURCES =
gjdoc_LDFLAGS = --main=gnu.classpath.tools.gjdoc.Main \
	-Djava.class.path=$(ANTLR_JAR) \
	-rpath $(toolexeclibdir) -shared-libgcc $(THREADLDFLAGS) $(extra_ldflags)
gjdoc_LINK = $(GCJLINK) $(gjdoc_LDFLAGS)
## See jv_convert_LDADD.
gjdoc_LDADD = -L$(here)/.libs libgcj-tools.la
gjdoc_DEPENDENCIES = libgcj-tools.la

## ################################################################
if INTERPRETER
nat_jdwp_source_files = \
gnu/classpath/jdwp/natVMFrame.cc \
gnu/classpath/jdwp/natVMMethod.cc \
gnu/classpath/jdwp/natVMVirtualMachine.cc

nat_jvmti_source_files = \
gnu/gcj/jvmti/natBreakpoint.cc \
gnu/gcj/jvmti/natNormalBreakpoint.cc
else
nat_jdwp_source_files = 
nat_jvmti_source_files =
endif
## This lists all the C++ source files in subdirectories.
nat_source_files = \
$(nat_jdwp_source_files) \
gnu/classpath/natConfiguration.cc \
gnu/classpath/natSystemProperties.cc \
gnu/classpath/natVMStackWalker.cc \
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
$(nat_jvmti_source_files) \
gnu/gcj/runtime/natFinalizerThread.cc \
gnu/gcj/runtime/natSharedLibLoader.cc \
gnu/gcj/runtime/natSystemClassLoader.cc \
gnu/gcj/runtime/natStringBuffer.cc \
gnu/gcj/util/natDebug.cc \
gnu/gcj/util/natGCInfo.cc \
gnu/java/lang/natMainThread.cc \
gnu/java/lang/management/natVMClassLoadingMXBeanImpl.cc \
gnu/java/lang/management/natVMCompilationMXBeanImpl.cc \
gnu/java/lang/management/natVMGarbageCollectorMXBeanImpl.cc \
gnu/java/lang/management/natVMMemoryMXBeanImpl.cc \
gnu/java/lang/management/natVMMemoryManagerMXBeanImpl.cc \
gnu/java/lang/management/natVMMemoryPoolMXBeanImpl.cc \
gnu/java/lang/management/natVMOperatingSystemMXBeanImpl.cc \
gnu/java/lang/management/natVMRuntimeMXBeanImpl.cc \
gnu/java/lang/management/natVMThreadMXBeanImpl.cc \
gnu/java/net/natPlainDatagramSocketImpl.cc \
gnu/java/net/natPlainSocketImpl.cc \
gnu/java/net/protocol/core/natCoreInputStream.cc \
gnu/java/nio/natVMPipe.cc \
gnu/java/nio/natVMSelector.cc \
gnu/java/nio/natNIOServerSocket.cc \
gnu/java/nio/natVMChannel.cc \
gnu/java/nio/channels/natFileChannelImpl.cc \
gnu/java/security/jce/prng/natVMSecureRandom.cc \
java/io/natFile.cc \
java/io/natVMObjectInputStream.cc \
java/io/natVMObjectStreamClass.cc \
java/lang/natCharacter.cc \
java/lang/natClass.cc \
java/lang/natClassLoader.cc \
java/lang/natConcreteProcess.cc \
java/lang/natVMDouble.cc \
java/lang/natVMFloat.cc \
java/lang/natMath.cc \
java/lang/natObject.cc \
java/lang/natRuntime.cc	\
java/lang/natString.cc \
java/lang/natAbstractStringBuffer.cc \
java/lang/natSystem.cc \
java/lang/natThread.cc \
java/lang/natThreadLocal.cc \
java/lang/natVMClassLoader.cc \
java/lang/natVMProcess.cc \
java/lang/natVMThrowable.cc \
java/lang/ref/natReference.cc \
java/lang/reflect/natArray.cc \
java/lang/reflect/natConstructor.cc \
java/lang/reflect/natField.cc \
java/lang/reflect/natMethod.cc \
java/lang/reflect/natVMProxy.cc \
java/net/natVMInetAddress.cc \
java/net/natVMNetworkInterface.cc \
java/net/natVMURLConnection.cc \
java/nio/channels/natVMChannels.cc \
java/nio/natVMDirectByteBufferImpl.cc \
java/security/natVMAccessController.cc \
java/security/natVMAccessControlState.cc \
java/text/natCollator.cc \
java/util/natVMTimeZone.cc \
java/util/concurrent/atomic/natAtomicLong.cc \
java/util/logging/natLogger.cc \
java/util/zip/natDeflater.cc \
java/util/zip/natInflater.cc \
sun/misc/natUnsafe.cc

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
	( \
	  ( cd $(srcdir)/classpath; \
	  find java gnu javax org sun -name '*.java' -print | \
	  while read file; do \
## Ugly code to avoid "echo -C".  Must separate each entry by a newline
## Gross but easy.
	    echo "x-C" | sed -e 's/^.//'; \
	    echo $(srcdir)/classpath; \
	    echo $$file; \
	  done ); \
	  ( cd $(srcdir)/classpath/external/sax; \
	  find org -name '*.java' -print | \
	  while read file; do \
	    echo "x-C" | sed -e 's/^.//'; \
	    echo $(srcdir)/classpath/external/sax; \
	    echo $$file; \
	  done ); \
	  ( cd $(srcdir)/classpath/external/relaxngDatatype; \
	  find org -name '*.java' -print | \
	  while read file; do \
	    echo "x-C" | sed -e 's/^.//'; \
	    echo $(srcdir)/classpath/external/relaxngDatatype; \
	    echo $$file; \
	  done ); \
	  ( cd $(srcdir)/classpath/external/w3c_dom; \
	  find org -name '*.java' -print | \
	  while read file; do \
	    echo "x-C" | sed -e 's/^.//'; \
	    echo $(srcdir)/classpath/external/w3c_dom; \
	    echo $$file; \
	  done ); \
## Now the build tree.
	  ( cd classpath; \
	    find gnu java -name '*.java' -print | \
	    while read file; do \
	    echo "x-C" | sed -e 's/^.//'; \
	    echo `pwd`; \
	    echo $$file; \
	  done ); \
	) | \
## Many of the above circumlocutions were because ZIP used to be a
## relative path to fastjar.  It didn't seem worthwhile to change the
## code when we moved to an external jar.
	$(JAR) -cfM@ $$here/src.zip
## Override GNU Classpath sources with libgcj replacements.
	here=`pwd`; \
	( \
	  ( cd $(srcdir); \
	  find gnu java -name '*.java' -print | \
	  while read file; do \
	    echo "x-C" | sed -e 's/^.//'; \
	    echo $(srcdir); \
	    echo $$file; \
	  done ); \
	) | \
	$(JAR) -ufM@ $$here/src.zip

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
$(libjvm_la_OBJECTS): headers.stamp

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
	"datadir=$(datadir)" \
	"infodir=$(infodir)" \
	"libdir=$(libdir)" \
	"mandir=$(mandir)" \
	"prefix=$(prefix)" \
	"gxx_include_dir=$(gxx_include_dir)" \
	"AR=$(AR)" \
	"AS=$(AS)" \
	"DLLTOOL=$(DLLTOOL)" \
	"LD=$(LD)" \
	"LIBCFLAGS=$(LIBCFLAGS)" \
	"NM=$(NM)" \
	"PICFLAG=$(PICFLAG)" \
	"RANLIB=$(RANLIB)" \
	"DESTDIR=$(DESTDIR)" \
	"JAR=$(JAR)"

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

if ANONVERSCRIPT
if ANONVERSCRIPT_SUN
# This must be at the end of the Makefile, otherwise .SECONDEXPANSION
# causes expansion of filenames with $ in their names, which breaks the build.
# .SECONDEXPANSION is necessary to allow the use of automatic variables ($@
# in this case) in the requisites of pattern rules.
.SECONDEXPANSION:

%.ver-sun : $(srcdir)/libgcj.ver \
		$(top_srcdir)/../contrib/make_sunver.pl \
		$$($$(basename $$@)_la_OBJECTS) $$($$(basename $$@)_la_LIBADD)
	perl $(top_srcdir)/../contrib/make_sunver.pl \
	  $(srcdir)/libgcj.ver \
	 `echo $($(basename $@)_la_OBJECTS) $($(basename $@)_la_LIBADD) | \
	    sed 's,\([^/ 	]*\)\.l\([ao]\),.libs/\1.\2,g'` \
	 > $@ || (rm -f $@ ; exit 1)
endif
endif

## Input file for automake to generate the Makefile.in used by configure

## GCJ LOCAL: use srcdir to find core classes.
GLIBJ_BOOTCLASSPATH='$(top_srcdir)/lib'
GLIBJ_CLASSPATH=$(srcdir)/asm
## END GCJ LOCAL

# Setup the compiler to use the GNU Classpath library we just build
if FOUND_GCJ
JCOMPILER = $(GCJ) -C -encoding UTF-8 -bootclasspath $(GLIBJ_BOOTCLASSPATH) -classpath $(GLIBJ_CLASSPATH):.
else
if FOUND_ECJ
JCOMPILER = $(ECJ) -1.5 -encoding UTF-8 -warn:-deprecation,serial,typeHiding,unchecked,unused,varargsCast -bootclasspath $(GLIBJ_BOOTCLASSPATH) -classpath .:$(GLIBJ_CLASSPATH)
else
if FOUND_JAVAC
JCOMPILER = $(JAVAC) -encoding UTF-8 -bootclasspath $(GLIBJ_BOOTCLASSPATH) -classpath .:$(GLIBJ_CLASSPATH)
else
error dunno how to setup the JCOMPILER and compile
endif # FOUND_JAVAC
endif # FOUND_ECJ
endif # FOUND_GCJ

if CREATE_WRAPPERS
bin_SCRIPTS =
bin_PROGRAMS = gappletviewer gjarsigner gkeytool \
	gjar gnative2ascii gserialver gjavah grmiregistry \
	gtnameserv gorbd grmid grmic


AM_CPPFLAGS = -Wall \
	-I$(top_srcdir)/include \
	-DLIBJVM="\"$(libdir)/libjvm\"" \
	-DTOOLS_ZIP="\"$(TOOLSdir)/$(TOOLS_ZIP)\""

gappletviewer_SOURCES = toolwrapper.c
gappletviewer_CFLAGS = \
	-DTOOLPACKAGE="\"appletviewer\"" \
	-DTOOLNAME="\"gappletviewer\""

gjarsigner_SOURCES = toolwrapper.c
gjarsigner_CFLAGS = \
	-DTOOLPACKAGE="\"jarsigner\"" \
	-DTOOLNAME="\"gjarsigner\""

gkeytool_SOURCES = toolwrapper.c
gkeytool_CFLAGS = \
	-DTOOLPACKAGE="\"keytool\"" \
	-DTOOLNAME="\"gkeytool\""

gjar_SOURCES = toolwrapper.c
gjar_CFLAGS = \
	-DTOOLPACKAGE="\"jar\"" \
	-DTOOLNAME="\"gjar\""

gnative2ascii_SOURCES = toolwrapper.c
gnative2ascii_CFLAGS = \
	-DTOOLPACKAGE="\"native2ascii\"" \
	-DTOOLNAME="\"gnative2ascii\"" \
	-DMAINCLASS="\"Native2ASCII\""

gserialver_SOURCES = toolwrapper.c
gserialver_CFLAGS = \
	-DTOOLPACKAGE="\"serialver\"" \
	-DTOOLNAME="\"gserialver\"" \
	-DMAINCLASS="\"SerialVer\""

grmiregistry_SOURCES = toolwrapper.c
grmiregistry_CFLAGS = \
	-DTOOLPACKAGE="\"rmiregistry\"" \
	-DTOOLNAME="\"grmiregistry\""

gtnameserv_SOURCES = toolwrapper.c
gtnameserv_CFLAGS = \
	-DTOOLPACKAGE="\"tnameserv\"" \
	-DTOOLNAME="\"gtnameserv\""

gorbd_SOURCES = toolwrapper.c
gorbd_CFLAGS = \
	-DTOOLPACKAGE="\"orbd\"" \
	-DTOOLNAME="\"gorbd\""

grmid_SOURCES = toolwrapper.c
grmid_CFLAGS = \
	-DTOOLPACKAGE="\"rmid\"" \
	-DTOOLNAME="\"grmid\""

gjavah_SOURCES = toolwrapper.c
gjavah_CFLAGS = \
	-DTOOLPACKAGE="\"javah\"" \
	-DTOOLNAME="\"gjavah\""

grmic_SOURCES = toolwrapper.c
grmic_CFLAGS = \
	-DTOOLPACKAGE="\"rmic\"" \
	-DTOOLNAME="\"grmic\""

else
## GCJ LOCAL: do not install these.
noinst_SCRIPTS = gappletviewer gjarsigner gkeytool \
	gjar gnative2ascii gserialver gjavah grmiregistry \
	gtnameserv gorbd grmid grmic
bin_PROGRAMS =
endif
EXTRA_DIST = toolwrapper.c gappletviewer.in gjarsigner.in gkeytool.in \
	gjar.in gnative2ascii.in gserialver.in gjavah.in grmiregistry.in \
	gtnameserv.in gorbd.in grmid.in grmic.in

# All our example java source files
TOOLS_JAVA_FILES = $(srcdir)/gnu/classpath/tools/*/*.java \
	$(srcdir)/com/sun/javadoc/*.java \
	$(srcdir)/com/sun/tools/doclets/*.java \
	$(srcdir)/com/sun/tools/javac/*.java \
	$(srcdir)/com/sun/tools/javah/*.java \
	$(srcdir)/sun/rmi/rmic/*.java \
	$(srcdir)/external/asm/org/objectweb/asm/*.java \
	$(srcdir)/external/asm/org/objectweb/asm/attrs/*.java \
	$(srcdir)/external/asm/org/objectweb/asm/commons/*.java \
	$(srcdir)/external/asm/org/objectweb/asm/optimizer/*.java \
	$(srcdir)/external/asm/org/objectweb/asm/signature/*.java \
	$(srcdir)/external/asm/org/objectweb/asm/tree/*.java \
	$(srcdir)/external/asm/org/objectweb/asm/tree/analysis/*.java \
	$(srcdir)/external/asm/org/objectweb/asm/util/*.java \
	$(srcdir)/external/asm/org/objectweb/asm/util/attrs/*.java \
	$(srcdir)/external/asm/org/objectweb/asm/xml/*.java

# The zip files with classes we want to produce.
TOOLS_ZIP = tools.zip

# Extra objects that will not exist until configure-time
BUILT_SOURCES = $(TOOLS_ZIP)

# RMIC templates that must be included in the generated zip file.
RMIC_TEMPLATES = $(srcdir)/gnu/classpath/tools/rmic/templates/*.jav

# All the files we find "interesting"
ALL_TOOLS_FILES = $(TOOLS_JAVA_FILES) $(RMIC_TEMPLATES)

# Some architecture independent data to be installed.
# GCJ LOCAL: do not install this.
noinst_DATA = $(TOOLS_ZIP)

# Where we want these data files installed.
TOOLSdir = $(pkgdatadir)

# Make sure everything is included in the distribution.
dist-hook:
	srcdir_cnt=`echo $(srcdir) | wc -c`; \
	for file in $(ALL_TOOLS_FILES); do \
	  f=`echo $$file | cut -c$$srcdir_cnt-`; \
	  fdir=`dirname $$f`; \
	  if test ! -d $(distdir)/$$fdir; then \
	    echo "$(makeinstalldirs) $(distdir)/$$fdir"; \
	    $(mkinstalldirs) $(distdir)/$$fdir; \
	  fi; \
	  echo "cp -p $$file $(distdir)/$$f"; \
	  cp -p $$file $(distdir)/$$f; \
	done

# To generate the example zip just depend on the sources and ignore
# the class files. Always regenerate all .class files and remove them
# immediately.  And copy the template files we use to the classes dir
# so they get also included.
$(TOOLS_ZIP): $(TOOLS_JAVA_FILES)
## GCJ LOCAL: put classes in srcdir
##	@rm -rf classes asm
##	mkdir classes asm
	find $(srcdir)/external/asm -name '*.java' -print > asm.lst
	find $(srcdir)/gnu/classpath/tools \
	     $(srcdir)/com/sun/javadoc \
	     $(srcdir)/com/sun/tools/doclets \
	     $(srcdir)/com/sun/tools/javac \
	     $(srcdir)/com/sun/tools/javah \
	     $(srcdir)/sun/rmi/rmic \
	     -name '*.java' -print > classes.lst
	if [ -f $(top_builddir)/../vm-tools-packages ]; then \
	    : > vm-tools.lst; \
	    for pkg in `cat $(top_builddir)/../vm-tools-packages`; do \
	      $(FIND) $(top_srcdir)/../$$pkg -follow -name '*.java' -print >> vm-tools.lst; \
	    done \
	else \
	    echo -n > vm-tools.lst; \
	fi
	cat classes.lst asm.lst vm-tools.lst > all-classes.lst
if JAVA_MAINTAINER_MODE
## Compile ASM separately as it is latin-1 encoded.
	AC=`echo $(JCOMPILER) | sed -e 's/UTF-8/ISO-8859-1/g'`; \
	  $$AC -g -w -d $(srcdir)/asm @asm.lst
	$(JCOMPILER) -g -w -d $(srcdir)/classes @classes.lst @vm-tools.lst
## Copy over tools resource files.
	@list=`cd $(top_srcdir)/resource && find gnu/classpath/tools \
	       com/sun/javadoc com/sun/tools/doclets com/sun/tools/javac \
	       sun/rmi/rmic -name \*.properties -print`; \
	for p in $$list; do \
	  dirname=classes/`dirname $$p`; \
	  if ! test -d "$$dirname"; then mkdir -p "$$dirname"; fi; \
	  echo "  cp $(top_srcdir)/resource/$$p classes/$$p"; \
	  cp $(top_srcdir)/resource/$$p classes/$$p; \
	done
endif
## Copy over rmic template files.
	@list=`cd $(srcdir) && find gnu/classpath/tools -name \*.jav -print`; \
	for p in $$list; do \
	  dirname=classes/`dirname $$p`; \
	  if ! test -d "$$dirname"; then mkdir -p "$$dirname"; fi; \
	  echo "  cp $(srcdir)/$$p classes/$$p"; \
	  cp $(srcdir)/$$p classes/$$p; \
	done
	cp -pR $(srcdir)/asm .
	cp -pR $(srcdir)/classes .
## END GCJ LOCAL
## First add classpath tools stuff.
	(cd classes; \
	if test "$(ZIP)" != ""; then $(ZIP) -r ../$(TOOLS_ZIP) .; fi; \
	if test "$(FASTJAR)" != ""; then $(FASTJAR) cf ../$(TOOLS_ZIP) .; fi; \
	cd ..)
## Now add ASM classes.
	(cd asm; \
	if test "$(ZIP)" != ""; then $(ZIP) -u -r ../$(TOOLS_ZIP) .; fi; \
	if test "$(FASTJAR)" != ""; then $(FASTJAR) uf ../$(TOOLS_ZIP) .; fi; \
	cd ..)
	rm -rf asm classes classes.lst asm.lst

# Zip file be gone! (and make sure the classes are gone too)
clean-local:
	rm -rf $(TOOLS_ZIP) classes classes.lst asm asm.lst all-classes.lst

# FIXME: remove this when GNU Classpath includes a bootstrap VM.
installcheck-binSCRIPTS:
	:

## Input file for automake to generate the Makefile.in used by configure

if CREATE_GJDOC
## GCJ LOCAL: always put source files in srcdir
## if CREATE_GJDOC_PARSER
## gjdoc_gendir = ${top_builddir}/tools/generated
## else
gjdoc_gendir = ${top_srcdir}/tools/generated
## endif
## END GCJ LOCAL
ANTLR_CLASSPATH = $(ANTLR_JAR):$(gjdoc_gendir)
endif

## GCJ LOCAL: use srcdir to find core classes.
GLIBJ_BOOTCLASSPATH='$(top_srcdir)/lib'
GLIBJ_CLASSPATH=.:$(srcdir)/asm:$(ANTLR_CLASSPATH)
## END GCJ LOCAL

# Setup the compiler to use the GNU Classpath library we just built.
if GCJ_JAVAC
JCOMPILER = $(JAVAC) $(JAVACFLAGS) -fsource=1.5 -ftarget=1.5 --encoding=UTF-8 --bootclasspath=$(GLIBJ_BOOTCLASSPATH) --classpath=$(GLIBJ_CLASSPATH)
else
JCOMPILER = $(JAVAC) $(JAVACFLAGS) -source 1.5 -target 1.5 -encoding UTF-8 -bootclasspath $(GLIBJ_BOOTCLASSPATH) -classpath $(GLIBJ_CLASSPATH)
endif

if CREATE_WRAPPERS
bin_SCRIPTS =
bin_programs = gappletviewer gjarsigner gkeytool \
	gjar gnative2ascii gserialver gjavah grmiregistry \
	gtnameserv gorbd grmid grmic
if CREATE_GJDOC
bin_programs += gjdoc
endif
if INSTALL_BINARIES
bin_PROGRAMS = $(bin_programs)
else
noinst_PROGRAMS = $(bin_programs)
endif

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

gjdoc_SOURCES = toolwrapper.c
gjdoc_CFLAGS = \
	-DTOOLPACKAGE="\"gjdoc\"" \
	-DTOOLNAME="\"gjdoc\""

else
## GCJ LOCAL: do not install these.
noinst_SCRIPTS = gappletviewer gjarsigner gkeytool \
	gjar gnative2ascii gserialver gjavah grmiregistry \
	gtnameserv gorbd grmid grmic
if CREATE_GJDOC
noinst_SCRIPTS += gjdoc
endif
bin_PROGRAMS =
## FIXME: remove these unneeded dependency lines once we can
## require Automake 1.11.
gappletviewer: gappletviewer.in
gjarsigner: gjarsigner.in
gkeytool: gkeytool.in
gjar: gjar.in
gnative2ascii: gnative2ascii.in
gserialver: gserialver.in
gjavah: gjavah.in
grmiregistry: grmiregistry.in
gtnameserv: gtnameserv.in
gorbd: gorbd.in
grmid: grmid.in
grmic: grmic.in
if CREATE_GJDOC
gjdoc: gjdoc.in
endif
endif
EXTRA_DIST = toolwrapper.c gappletviewer.in gjarsigner.in gkeytool.in \
	gjar.in gnative2ascii.in gserialver.in gjavah.in grmiregistry.in \
	gtnameserv.in gorbd.in grmid.in grmic.in gjdoc.in

# All our example java source files
TOOLS_JAVA_FILES = \
	$(srcdir)/gnu/classpath/tools/*.java \
	$(srcdir)/gnu/classpath/tools/*/*.java \
	$(srcdir)/gnu/classpath/tools/*/*/*.java \
	$(srcdir)/gnu/classpath/tools/*/*/*/*.java \
	$(srcdir)/com/sun/javadoc/*.java \
	$(srcdir)/com/sun/tools/doclets/*.java \
	$(srcdir)/com/sun/tools/javac/*.java \
	$(srcdir)/com/sun/tools/javah/*.java \
	$(srcdir)/com/sun/tools/javadoc/*.java \
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

# Properties files that must be included in the generated zip file.
PROPERTY_FILES = $(srcdir)/external/asm/org/objectweb/asm/optimizer/shrink.properties \
	$(srcdir)/resource/com/sun/tools/javac/messages.properties \
	$(srcdir)/resource/gnu/classpath/tools/jar/messages.properties \
	$(srcdir)/resource/gnu/classpath/tools/orbd/messages.properties \
	$(srcdir)/resource/gnu/classpath/tools/rmic/messages.properties \
	$(srcdir)/resource/gnu/classpath/tools/rmid/messages.properties \
	$(srcdir)/resource/gnu/classpath/tools/serialver/messages.properties \
	$(srcdir)/resource/gnu/classpath/tools/keytool/messages.properties \
	$(srcdir)/resource/gnu/classpath/tools/native2ascii/messages.properties \
	$(srcdir)/resource/gnu/classpath/tools/appletviewer/messages.properties \
	$(srcdir)/resource/gnu/classpath/tools/common/messages.properties \
	$(srcdir)/resource/gnu/classpath/tools/getopt/messages.properties \
	$(srcdir)/resource/gnu/classpath/tools/tnameserv/messages.properties \
	$(srcdir)/resource/gnu/classpath/tools/jarsigner/messages.properties \
	$(srcdir)/resource/gnu/classpath/tools/rmiregistry/messages.properties \
	$(srcdir)/resource/sun/rmi/rmic/messages.properties

# RMIC templates that must be included in the generated zip file.
RMIC_TEMPLATES = $(srcdir)/resource/gnu/classpath/tools/rmic/templates/*.jav

# gjdoc resource files.
gnu_classpath_tools_gjdoc_jar_CSS = \
	doctranslets/html/res/gjdochtml-clean.css \
	doctranslets/html/res/gjdochtml-fixed.css \
	doctranslets/html/res/gjdochtml-sclara.css \
	doctranslets/html/res/gjdochtml.css \
	htmldoclet/gjdochtml-vanilla.css \
	htmldoclet/gjdochtml-clean-layout.css \
	htmldoclet/gjdochtml-clean-color1.css

gnu_classpath_tools_gjdoc_jar_DTDS = \
	htmldoclet/xhtml11-target10.dtd \
	dtd/gjdoc-alphaindex.dtd \
	dtd/gjdoc.dtd

gnu_classpath_tools_gjdoc_jar_ENTS = \
	dtd/ent/iso-amsa.ent \
	dtd/ent/iso-amsb.ent \
	dtd/ent/iso-amsc.ent \
	dtd/ent/iso-amsn.ent \
	dtd/ent/iso-amso.ent \
	dtd/ent/iso-amsr.ent \
	dtd/ent/iso-box.ent \
	dtd/ent/iso-cyr1.ent \
	dtd/ent/iso-cyr2.ent \
	dtd/ent/iso-dia.ent \
	dtd/ent/iso-grk1.ent \
	dtd/ent/iso-grk2.ent \
	dtd/ent/iso-grk3.ent \
	dtd/ent/iso-grk4.ent \
	dtd/ent/iso-lat1.ent \
	dtd/ent/iso-lat2.ent \
	dtd/ent/iso-num.ent \
	dtd/ent/iso-pub.ent \
	dtd/ent/iso-tech.ent

gnu_classpath_tools_gjdoc_jar_HTML = doctranslets/html/res/default_help_en.html

gnu_classpath_tools_gjdoc_jar_JS = \
	doctranslets/html/res/gjdoc.js \
	htmldoclet/gjdoc.js

gnu_classpath_tools_gjdoc_jar_PNG = \
	htmldoclet/inherit.png

gnu_classpath_tools_gjdoc_jar_MODS = dtd/dbcentx.mod

gnu_classpath_tools_gjdoc_jar_PROPERTIES = htmldoclet/HtmlDoclet.properties

gnu_classpath_tools_gjdoc_jar_RNGS = \
	rng/gjdoc-classdoc.rng \
	rng/gjdoc-common.rng \
	rng/gjdoc-index.rng

gnu_classpath_tools_gjdoc_jar_TXTS = \
	java.lang-classes-1.2.txt \
	java.lang-classes-1.3.txt \
	java.lang-classes-1.4.txt \
	java.lang-classes-1.5.txt

gnu_classpath_tools_gjdoc_jar_XHTML = htmldoclet/help.xhtml

gnu_classpath_tools_gjdoc_jar_XSLS = \
	doctranslets/gjdoc_common.xsl \
	doctranslets/html/about.xsl \
	doctranslets/html/allclasses.xsl \
	doctranslets/html/allpackages.xsl \
	doctranslets/html/alphaindex.xsl \
	doctranslets/html/alphaindex_chunked.xsl \
	doctranslets/html/classdoc-source.xsl \
	doctranslets/html/classdoc-uses.xsl \
	doctranslets/html/classdoc.xsl \
	doctranslets/html/deprecated.xsl \
	doctranslets/html/descriptor.xsl \
	doctranslets/html/doctranslet.xsl \
	doctranslets/html/fulltree.xsl \
	doctranslets/html/gjdoc.xsl \
	doctranslets/html/help.xsl \
	doctranslets/html/html_common.xsl \
	doctranslets/html/index_noframes.xsl \
	doctranslets/html/index.xsl \
	doctranslets/html/packageclasses.xsl \
	doctranslets/html/packagedoc.xsl

# All our resources.
gjdoc_resources = $(gnu_classpath_tools_gjdoc_jar_CSS) $(gnu_classpath_tools_gjdoc_jar_DTDS) \
	$(gnu_classpath_tools_gjdoc_jar_ENTS) $(gnu_classpath_tools_gjdoc_jar_HTML) \
	$(gnu_classpath_tools_gjdoc_jar_JS) $(gnu_classpath_tools_gjdoc_jar_MODS) \
	$(gnu_classpath_tools_gjdoc_jar_PNG) $(gnu_classpath_tools_gjdoc_jar_PROPERTIES) \
	$(gnu_classpath_tools_gjdoc_jar_RNGS) $(gnu_classpath_tools_gjdoc_jar_TXTS) \
	$(gnu_classpath_tools_gjdoc_jar_XHTML) $(gnu_classpath_tools_gjdoc_jar_XSLS)

if !CREATE_GJDOC
GJDOC_EX = -name gjdoc -prune -o \
	-name doclets -prune -o \
	-name taglets -prune -o \
	-name javadoc -prune -o
endif

# The zip files with classes we want to produce.
TOOLS_ZIP = tools.zip

# Extra objects that will not exist until configure-time
BUILT_SOURCES = $(TOOLS_ZIP)

# All the files we find "interesting"
ALL_TOOLS_FILES = $(TOOLS_JAVA_FILES) $(RMIC_TEMPLATES) $(PROPERTY_FILES) \
  $(addprefix $(srcdir)/resource/gnu/classpath/tools/gjdoc/,$(gjdoc_resources))\
  $(srcdir)/gnu/classpath/tools/gjdoc/expr/java-expression.g

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
	  if ! cp -p $$file $(distdir)/$$f; then \
	    exit -1 ; \
	  fi ; \
	done

# To generate the example zip just depend on the sources and ignore
# the class files. Always regenerate all .class files and remove them
# immediately.  And copy the template files we use to the classes dir
# so they get also included.
$(TOOLS_ZIP): $(ALL_TOOLS_FILES)
## GCJ LOCAL: put classes in srcdir
##	@rm -rf classes asm
##	@mkdir_p@ classes asm
if JAVA_MAINTAINER_MODE
if CREATE_GJDOC
if CREATE_GJDOC_PARSER
## Generate antlr sources.
	@mkdir_p@ $(gjdoc_gendir)/gnu/classpath/tools/gjdoc/expr
	$(ANTLR) -o $(gjdoc_gendir)/gnu/classpath/tools/gjdoc/expr/ \
	  $(srcdir)/gnu/classpath/tools/gjdoc/expr/java-expression.g
endif
endif
endif
	find $(srcdir)/external/asm -name '*.java' -print > asm.lst
	find $(srcdir)/gnu/classpath/tools \
	     $(srcdir)/com/sun/javadoc \
	     $(srcdir)/com/sun/tools/doclets \
	     $(srcdir)/com/sun/tools/javadoc \
	     $(srcdir)/com/sun/tools/javac \
	     $(srcdir)/com/sun/tools/javah \
	     $(srcdir)/sun/rmi/rmic \
	     $(GJDOC_EX) \
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
	$(JCOMPILER) $(USE_JAVAC_FLAGS) -g -d $(srcdir)/classes @classes.lst @vm-tools.lst
endif
## END GCJ LOCAL
## Copy over tools resource files.
	@list=`cd $(srcdir)/resource && find gnu/classpath/tools com/sun/tools/javac \
	       sun/rmi/rmic $(GJDOC_EX) -name \*.properties -print -o -name \*.jav -print`; \
	for p in $$list; do \
	  dirname=classes/`dirname $$p`; \
	  if ! test -d "$$dirname"; then @mkdir_p@ "$$dirname"; fi; \
	  echo "  cp $(srcdir)/resource/$$p classes/$$p"; \
	  cp $(srcdir)/resource/$$p classes/$$p; \
	done
## BEGIN GCJ LOCAL
	cp -pR $(srcdir)/asm .
	cp -pR $(srcdir)/classes .
	find asm classes -type d -exec chmod u+w \{\} \;
	if [ -d asm/.svn ]; then \
	  find asm classes -depth -type d -name .svn -exec rm -rf \{\} \;; \
	fi
## END GCJ LOCAL
if CREATE_GJDOC
## Copy over gjdoc resource files.
	for res in $(gjdoc_resources); do \
	  dir=classes/`dirname $$res`; \
	  if ! test -d "$$dir"; then @mkdir_p@ "$$dir"; fi; \
	  echo "  cp $(srcdir)/resource/gnu/classpath/tools/gjdoc/$$res classes/$$res"; \
	  cp $(srcdir)/resource/gnu/classpath/tools/gjdoc/$$res classes/$$res; \
	done 
endif

if WITH_JAR
CREATE_TOOLS_ZIP=$(JAR) cf ../$(TOOLS_ZIP) .
UPDATE_TOOLS_ZIP=$(JAR) uf ../$(TOOLS_ZIP) .
else
CREATE_TOOLS_ZIP=$(ZIP) -r ../$(TOOLS_ZIP) .
UPDATE_TOOLS_ZIP=$(ZIP) -u -r ../$(TOOLS_ZIP) .
endif

## First add classpath tools stuff.
	(cd classes; \
	$(CREATE_TOOLS_ZIP); \
	cd ..)
## Now add ASM classes.
	(cd asm; \
	$(UPDATE_TOOLS_ZIP); \
	cd ..)
	rm -rf asm classes classes.lst asm.lst

# Zip file be gone! (and make sure the classes are gone too)
clean-local:
	rm -rf $(TOOLS_ZIP) classes classes.lst asm asm.lst all-classes.lst
if CREATE_GJDOC_PARSER
	rm -rf $(gjdoc_gendir)
endif

# FIXME: remove this when GNU Classpath includes a bootstrap VM.
installcheck-binSCRIPTS:
	:

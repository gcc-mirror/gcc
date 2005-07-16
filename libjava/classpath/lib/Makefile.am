JAVA_DEPEND = java.dep

## silently try to include these, if it fails gnu make
## will remake these 'makefiles' with the rules given in
## this file and restart the make process again
sinclude $(JAVA_DEPEND)

propertydirs :=  $(shell cd $(top_srcdir)/resource && $(FIND) gnu java org META-INF -type d ! -name CVS -print)
propertyfiles :=  $(shell cd $(top_srcdir)/resource && $(FIND) gnu java org -name \*\.properties -print)
metafiles :=  $(shell cd $(top_srcdir)/resource && $(FIND) META-INF -path \*/CVS -prune -o -type f -print)
iconfiles :=  $(shell cd $(top_srcdir) && $(FIND) gnu/javax/swing/plaf/gtk/icons -name *.png -type f -print)

compile_classpath = $(vm_classes):$(top_srcdir):$(top_srcdir)/external/w3c_dom:$(top_srcdir)/external/sax:.:$(USER_CLASSLIB)

# handling source to bytecode compiler programs like gcj, jikes  and kjc
if FOUND_GCJ
# Since we don't feed all classes at once gcj needs to know where to find
# the generated classes (in top_builddir).
JAVAC = $(srcdir)/split-for-gcj.sh && $(MAKE) -f $(srcdir)/Makefile.gcj \
    GCJ='$(GCJ)' compile_classpath='$(top_builddir):$(compile_classpath)'
else
if FOUND_JIKES
JAVAC = $(JIKES) +Pno-shadow +Pno-switchcheck +F $(JIKESENCODING) -bootclasspath '' -extdirs '' -sourcepath '' --classpath $(compile_classpath) -d . @classes
else
if FOUND_KJC
## FIXME: from what I can tell, kjc does not support a -encoding option.
JAVAC = $(KJC) -classpath .:$(USER_CLASSLIB) -d . @classes
else
if FOUND_GCJX
JAVAC = $(GCJX) -encoding UTF-8 -classpath .:$(USER_CLASSLIB) -d . @classes
else
if FOUND_ECJ
JAVAC = $(ECJ) -encoding UTF-8 -warn:-deprecation,serial -proceedOnError -bootclasspath '' -classpath $(compile_classpath) -d . @classes
endif # FOUND_ECJ
endif # FOUND_GCJX
endif # FOUND_KJC
endif # FOUND_GCJ
endif # FOUND_JIKES

JAVAH = $(USER_JAVAH) -jni -classpath .:$(USER_CLASSLIB)

if INSTALL_GLIBJ_ZIP

pkgdata_DATA = glibj.zip

endif # INSTALL_GLIBJ_ZIP

if BUILD_CLASS_FILES
noinst_DATA = genclasses compile-classes resources
endif # BUILD_CLASS_FILES

if INSTALL_CLASS_FILES

install-data-local: genclasses compile-classes
	-$(top_srcdir)/mkinstalldirs $(DESTDIR)$(pkgdatadir)
	cp -R gnu $(DESTDIR)$(pkgdatadir)
	cp -R java $(DESTDIR)$(pkgdatadir)
	cp -R javax $(DESTDIR)$(pkgdatadir)
	cp -R org $(DESTDIR)$(pkgdatadir)
	cp -R META-INF $(DESTDIR)$(pkgdatadir)

# FIXME - should mimic doc/api/Makefile.am instead...
uninstall-local:
	rm -rf $(DESTDIR)$(pkgdatadir)/gnu
	rm -rf $(DESTDIR)$(pkgdatadir)/java
	rm -rf $(DESTDIR)$(pkgdatadir)/javax
	rm -rf $(DESTDIR)$(pkgdatadir)/org
	rm -rf $(DESTDIR)$(pkgdatadir)/META-INF

endif # INSTALL_CLASS_FILES

.PHONY: genclasses

glibj.zip: classes compile-classes resources
	if test "$(ZIP)" != ""; then $(ZIP) -r -D glibj.zip gnu java javax org META-INF > /dev/null; fi

resources:
	if ! [ -e gnu ]; then mkdir gnu; fi
	if ! [ -e gnu/java ]; then mkdir gnu/java; fi
	if ! [ -e gnu/java/locale ]; then mkdir gnu/java/locale; fi
	if ! [ -e gnu/javax/swing/plaf/gtk/icons ]; then mkdir -p gnu/javax/swing/plaf/gtk/icons; fi
	@list='$(propertydirs)'; for p in $$list; do \
	  if ! [ -e $$p ]; then mkdir $$p; fi; \
	done
	@list='$(propertyfiles)'; for p in $$list; do \
	  cp $(top_srcdir)/resource/$$p $$p; \
	done
	@list='$(metafiles)'; for p in $$list; do \
	  cp $(top_srcdir)/resource/$$p $$p; \
	done
	@list='$(iconfiles)'; for p in $$list; do \
	  cp $(top_srcdir)/$$p $$p; \
	done
	touch resources

classes: genclasses

$(top_builddir)/java/util/LocaleData.java: $(top_srcdir)/scripts/generate-locale-list.sh 
	mkdir -p $(top_builddir)/java/util
	$(top_srcdir)/scripts/generate-locale-list.sh > $(top_builddir)/java/util/LocaleData.java

genclasses: gen-classlist.sh standard.omit $(top_builddir)/java/util/LocaleData.java gen-xpath-parser
	top_builddir=$(top_builddir) top_srcdir=$(top_srcdir) $(SHELL) ./gen-classlist.sh standard

# Only rebuild parsers when explicitly asked to.
if REGEN_PARSERS

gen-xpath-parser: $(top_srcdir)/gnu/xml/xpath/XPathParser.java

$(top_srcdir)/gnu/xml/xpath/XPathParser.java: $(top_srcdir)/gnu/xml/xpath/XPathParser.y
	( cd $(top_srcdir)/gnu/xml/xpath; \
	  $(JAY) XPathParser.y < $(JAY_SKELETON) > XPathParser.java )
else

gen-xpath-parser:
	true

endif # REGEN_PARSER

$(JAVA_DEPEND): genclasses

compile-classes: classes $(JAVA_SRCS) Makefile
	$(JAVAC)
	touch compile-classes

EXTRA_DIST = standard.omit mkcollections.pl.in Makefile.gcj split-for-gcj.sh
CLEANFILES = compile-classes resources classes \
	classes.standard classes.awt classes.standardx classes.xml \
	glibj.zip classes.1 \
	$(top_builddir)/java/util/LocaleData.java \
	$(JAVA_DEPEND)

clean-local:
	-rm -rf gnu
	-rm -rf java
	-rm -rf javax
	-rm -rf org
	-rm -rf META-INF
	-rm -rf lists
	-rm -f Makefile.deps

dist-hook:
	mkdir -p $(distdir)
	cp -dfR $(top_srcdir)/gnu $(top_srcdir)/java $(top_srcdir)/javax $(top_srcdir)/org $(top_srcdir)/vm $(top_srcdir)/resource $(top_distdir)
# Delete not wanted files.
	$(FIND) $(top_distdir)/gnu $(top_distdir)/java $(top_distdir)/javax $(top_distdir)/org $(top_distdir)/vm $(top_distdir)/resource -name CVS -print | xargs rm -fr
	$(FIND) $(top_distdir)/gnu $(top_distdir)/java $(top_distdir)/javax $(top_distdir)/org $(top_distdir)/vm $(top_distdir)/resource -name .cvsignore -print | xargs rm -f
	$(FIND) $(top_distdir)/gnu $(top_distdir)/java $(top_distdir)/javax $(top_distdir)/org $(top_distdir)/vm -name class-dependencies.conf -print | xargs rm -f
	rm -f $(top_distdir)/gnu/classpath/Configuration.java
	rm -f $(top_distdir)/java/util/LocaleData.java
# Omit files listed in standard.omit.
	( cd $(top_distdir)/lib ; cat $(top_builddir)/lib/standard.omit | sed -e 's/\$$//' | sed -e 's/\/\.\*//' | xargs rm -fr )

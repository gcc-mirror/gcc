## Input file for automake to generate the Makefile.in used by configure

GLIBJ_CLASSPATH='$(top_builddir)/lib':'$(top_builddir)/lib/glibj.zip'

# Setup the compiler to use the GNU Classpath library we just build
if FOUND_GCJ
JCOMPILER = $(GCJ) -encoding UTF-8 --bootclasspath $(GLIBJ_CLASSPATH) --classpath . -C
else
if FOUND_JIKES
JCOMPILER = $(JIKES) $(JIKESENCODING) -bootclasspath '' -extdirs '' -sourcepath '' --classpath $(GLIBJ_CLASSPATH):.
else
if FOUND_GCJX
JCOMPILER = $(GCJX) -encoding UTF-8 -bootclasspath '' -sourcepath '' -classpath $(GLIBJ_CLASSPATH):.
else
if FOUND_ECJ
JCOMPILER = $(ECJ) -encoding UTF-8 -bootclasspath $(GLIBJ_CLASSPATH) -classpath .
else
error dunno how to setup the JCOMPILER and compile
endif
endif
endif
endif

# All our example java source files
TOOLS_JAVA_FILES = $(srcdir)/gnu/classpath/tools/*.java $(srcdir)/gnu/classpath/tools/*/*.java $(srcdir)/gnu/classpath/tools/*/*/*.java


# The zip files with classes we want to produce.
TOOLS_ZIP = tools.zip

# Extra objects that will not exist until configure-time
BUILT_SOURCES = $(TOOLS_ZIP)

# The templates that must be included into the generated zip file.
GRMIC_TEMPLATES = $(srcdir)/gnu/classpath/tools/giop/grmic/templates/*.jav 
RMIC_TEMPLATES = $(srcdir)/gnu/classpath/tools/rmi/rmic/templates/*.jav

TOOLS_TEMPLATES = $(GRMIC_TEMPLATES) $(RMIC_TEMPLATES)

# This covers the built-in help texts, both for giop and rmic subpackages.
GIOP_HELPS = $(srcdir)/gnu/classpath/tools/giop/*.txt
RMI_HELPS = $(srcdir)/gnu/classpath/tools/rmi/*.txt

TOOLS_HELPS = $(GIOP_HELPS) $(RMI_HELPS)

# The tool specific README files.
READMES = $(srcdir)/gnu/classpath/tools/giop/README

# All the files we find "interesting"
ALL_TOOLS_FILES = $(TOOLS_JAVA_FILES) $(TOOLS_TEMPLATES) $(TOOLS_HELPS) $(READMES)

# Some architecture independent data to be installed.
TOOLS_DATA = $(TOOLS_ZIP) README

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
	mkdir -p classes/gnu/classpath/tools/giop/grmic/templates
	mkdir -p classes/gnu/classpath/tools/rmi/rmic/templates
	cp $(RMIC_TEMPLATES) classes/gnu/classpath/tools/rmi/rmic/templates
	cp $(GRMIC_TEMPLATES) classes/gnu/classpath/tools/giop/grmic/templates	
	cp $(RMI_HELPS) classes/gnu/classpath/tools/rmi/
	cp $(GIOP_HELPS) classes/gnu/classpath/tools/giop/
	$(JCOMPILER) -d classes $(TOOLS_JAVA_FILES) 
	(cd classes; \
	if test "$(ZIP)" != ""; then $(ZIP) -r ../$(TOOLS_ZIP) .; fi; \
	if test "$(FASTJAR)" != ""; then $(FASTJAR) cf ../$(TOOLS_ZIP) .; fi; \
	cd ..)
	rm -rf classes

# Zip file be gone! (and make sure the classes are gone too)
clean-local:
	rm -rf $(TOOLS_ZIP) classes

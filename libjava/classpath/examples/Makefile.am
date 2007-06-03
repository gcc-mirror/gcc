## Input file for automake to generate the Makefile.in used by configure
GLIBJ_CLASSPATH='$(top_builddir)/lib':'$(top_builddir)/lib/glibj.zip':'$(top_builddir)/tools/tools.zip'

# Setup the compiler to use the GNU Classpath library we just build
#if FOUND_GCJ
#JCOMPILER = $(GCJ) --bootclasspath '$(top_builddir)/lib' --classpath . -C
#else
#if FOUND_JIKES
#JCOMPILER = $(JIKES) -bootclasspath '' -extdirs '' -sourcepath '' --classpath $(top_builddir)/lib:.
#else
if FOUND_ECJ
JCOMPILER = $(ECJ) -1.5 -encoding UTF-8 -warn:-deprecation,serial,typeHiding,unchecked,unused,varargsCast -bootclasspath $(GLIBJ_CLASSPATH) -classpath .
else
if FOUND_JAVAC
JCOMPILER = $(JAVAC) -encoding UTF-8 -bootclasspath $(GLIBJ_CLASSPATH) -classpath .
else
error dunno how to setup the JCOMPILER and compile
#endif
#endif
endif
endif

# All our example java source files
EXAMPLE_JAVA_FILES = $(srcdir)/gnu/classpath/examples/*/*.java $(srcdir)/gnu/classpath/examples/*/*/*.java $(srcdir)/gnu/classpath/examples/*/*/*/*.java

# The example C source & header files
EXAMPLE_C_FILES = $(srcdir)/gnu/classpath/examples/*/*.c
EXAMPLE_CH_FILES = $(srcdir)/gnu/classpath/examples/*/*.h

# The zip files with classes we want to produce.
EXAMPLE_ZIP = examples.zip

# Extra objects that will not exist until configure-time
BUILT_SOURCES = $(EXAMPLE_ZIP)

# the png icons we use in some of the examples.
EXAMPLE_ICONS = $(srcdir)/gnu/classpath/examples/icons/*.png
EXAMPLE_ICONS += $(srcdir)/gnu/classpath/examples/icons/*.gif

# the html pages we use in the swing demo example.
EXAMPLE_HTML = $(srcdir)/gnu/classpath/examples/swing/*.html

# The example specific README files.
READMES = $(srcdir)/gnu/classpath/examples/CORBA/swing/README.html

# All the files we find "interesting"
ALL_EXAMPLE_FILES = $(EXAMPLE_JAVA_FILES) $(EXAMPLE_C_FILES) $(EXAMPLE_CH_FILES) $(EXAMPLE_ICONS) $(EXAMPLE_HTML) $(READMES)

# Some architecture independent data to be installed.
example_DATA = $(EXAMPLE_ZIP) README

# Where we want these data files installed.
exampledir = $(pkgdatadir)/examples

# Make sure all sources and icons are also installed so users can use them.
# (Be careful to strip off the srcdir part of the path when installing.)
install-data-local:
	srcdir_cnt=`echo $(srcdir) | wc -c`; \
	for file in $(ALL_EXAMPLE_FILES); do \
	  f=`echo $$file | cut -c$$srcdir_cnt-`; \
	  fdir=`dirname $$f`; \
	  if test ! -d $(DESTDIR)$(pkgdatadir)/examples/$$fdir; then \
	    echo "$(mkinstalldirs) $(DESTDIR)$(pkgdatadir)/examples/$$fdir"; \
	    $(mkinstalldirs) $(DESTDIR)$(pkgdatadir)/examples/$$fdir; \
	  fi; \
	  echo "$(INSTALL_DATA) $$file $(DESTDIR)$(pkgdatadir)/examples/$$f"; \
	  $(INSTALL_DATA) $$file $(DESTDIR)$(pkgdatadir)/examples/$$f; \
	done
	echo "$(INSTALL_DATA) Makefile.jawt $(DESTDIR)$(pkgdatadir)/examples/"
	echo "$(INSTALL_DATA) Makefile.java2d $(DESTDIR)$(pkgdatadir)/examples/"
	$(INSTALL_DATA) Makefile.jawt $(DESTDIR)$(pkgdatadir)/examples/
	$(INSTALL_DATA) Makefile.java2d $(DESTDIR)$(pkgdatadir)/examples/

uninstall-local:
	srcdir_cnt=`echo $(srcdir) | wc -c`; \
	for file in $(ALL_EXAMPLE_FILES); do \
	  f=`echo $$file | cut -c$$srcdir_cnt-`; \
	  echo "rm -f $(DESTDIR)$(pkgdatadir)/examples/$$f"; \
	  rm -f $(DESTDIR)$(pkgdatadir)/examples/$$f; \
	done
	echo "rm -f $(DESTDIR)$(pkgdatadir)/examples/Makefile.jawt"
	echo "rm -f $(DESTDIR)$(pkgdatadir)/examples/Makefile.java2d"
	rm -f $(DESTDIR)$(pkgdatadir)/examples/Makefile.jawt
	rm -f $(DESTDIR)$(pkgdatadir)/examples/Makefile.java2d

# Make sure everything is included in the distribution.
EXTRA_DIST = README Makefile.jawt.in Makefile.java2d.in
dist-hook:
	srcdir_cnt=`echo $(srcdir) | wc -c`; \
	for file in $(ALL_EXAMPLE_FILES); do \
	  f=`echo $$file | cut -c$$srcdir_cnt-`; \
	  fdir=`dirname $$f`; \
	  if test ! -d $(distdir)/$$fdir; then \
	    echo "$(makeinstalldirs) $(distdir)/$$fdir"; \
	    $(mkinstalldirs) $(distdir)/$$fdir; \
	  fi; \
	  echo "cp -p $$file $(distdir)/$$f"; \
	  cp -p $$file $(distdir)/$$f; \
	done

# To generate the example zip just depend on the sources and ignore the
# class files. Always regenerate all .class files and remove them immediatly.
# And copy the png icons we use to the classes dir so they get also included.
$(EXAMPLE_ZIP): $(EXAMPLE_JAVA_FILES)
	mkdir -p classes/gnu/classpath/examples/icons
	cp $(EXAMPLE_ICONS) classes/gnu/classpath/examples/icons
	mkdir -p classes/gnu/classpath/examples/swing
	cp $(EXAMPLE_HTML) classes/gnu/classpath/examples/swing
	$(JCOMPILER) -d classes $(EXAMPLE_JAVA_FILES) 
	(cd classes; \
	if test "$(ZIP)" != ""; then $(ZIP) -r ../$(EXAMPLE_ZIP) .; fi; \
	if test "$(FASTJAR)" != ""; then $(FASTJAR) cf ../$(EXAMPLE_ZIP) .; fi; \
	cd ..)
	rm -rf classes

# Zip file be gone! (and make sure the classes are gone too)
clean-local:
	rm -rf $(EXAMPLE_ZIP) classes

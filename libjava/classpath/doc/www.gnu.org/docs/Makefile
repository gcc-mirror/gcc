SUBDIRS=

ifndef WMK
  export WMK=wmk
endif
ifndef WML
  export WML=wml
endif

TEXI_SRC	:= $(wildcard ../../*.texinfo)
VPATH = ../..
WML_SRC = $(patsubst ../../%, %, $(TEXI_SRC:.texinfo=.html)) $(wildcard *.wml)
WML_OBJS = $(patsubst %.wml,%.html,$(WML_SRC))

%.html : %.texinfo
	texi2html -monolithic --ifinfo --ifhtml $<
#	texi2html -monolithic $<;
#	cp $< $@
#	echo '#!wml --include=..' > $@
#	sed -e 's/<HTML>/\^\#use wml::std::lang\^\#use wml::fmt::isolatin\^\#use wml::std::case global=upper\^<lang:new id=en short>\^<lang:star:slice:>\^\#include <include\/macros.wml>\^<HTML>/' -e 's/<BODY>/<BODY BGCOLOR="#FFFFFF" TEXT="#000000" LINK="#1F00FF" ALINK="#FF0000" VLINK="#9900DD">/' -e 's/<\/BODY>/<footer>\^<\/BODY>/' $(subst .wml,.html,$(@)) > $@.sed;
#	tr "\\^" "\n" < $@.sed >> $@
#	-rm -f $(subst .wml,.html,$(@)) $@.sed

%.html: %.wml ../include/macros.wml  ../include/layout.wml
	$(WML) -o $@ $<
#	-rm -f $(patsubst %.wml,%.html,$<)

.PHONY : all
all : $(WML_OBJS) subdirs

.PHONY : clean
clean :
	$(foreach dir, $(SUBDIRS), sh -c "cd $(dir) && $(MAKE) clean";)
	-rm -f *.html
	-rm -f hacking.wml vmintegration.wml

.PHONY : subdirs
subdirs :
	$(foreach dir, $(SUBDIRS), sh -c "cd $(dir) && $(MAKE)";)

.PHONY : publish
publish : 
	@sh -c "if [ "$(CLASSPATH_WEBROOT)x" = x ]; then echo 'You must set CLASSPATH_WEBROOT in your environment first'; exit 1; fi"
	@sh -c "if [ ! -d $(CLASSPATH_WEBROOT) ]; then echo 'You must set CLASSPATH_WEBROOT in your environment first'; exit 1; fi"
	$(foreach dir, $(SUBDIRS), sh -c "cd $(dir) && $(MAKE) publish";)
	-cp -f *.html $(CLASSPATH_WEBROOT)/docs/


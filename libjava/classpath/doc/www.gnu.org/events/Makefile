ifndef WMK
  export WMK=wmk
endif
ifndef WML
  export WML=wml
endif

WML_SRC = $(wildcard *.wml)
WML_OBJS = $(patsubst %.wml,%.html,$(WML_SRC))

%.html: %.wml ../include/macros.wml  ../include/layout.wml
	$(WML) -o $@ $<
#	-rm -f $(patsubst %.wml,%.html,$<)

all : $(WML_OBJS) subdirs

.PHONY : clean
clean :
	$(foreach dir, $(SUBDIRS), sh -c "cd $(dir) && $(MAKE) clean";)
	-rm -f *.html

.PHONY : subdirs
subdirs :
	$(foreach dir, $(SUBDIRS), sh -c "cd $(dir) && $(MAKE)";)

.PHONY : publish
publish : 
	@sh -c "if [ "$(CLASSPATH_WEBROOT)x" = x ]; then echo 'You must set CLASSPATH_WEBROOT in your environment first'; exit 1; fi"
	@sh -c "if [ ! -d $(CLASSPATH_WEBROOT) ]; then echo 'You must set CLASSPATH_WEBROOT in your environment first'; exit 1; fi"
	$(foreach dir, $(SUBDIRS), sh -c "cd $(dir) && $(MAKE) publish";)
	-cp -f *.html $(CLASSPATH_WEBROOT)/events/


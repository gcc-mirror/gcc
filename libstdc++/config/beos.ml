LIBS	= $(ARLIB) $(ARLINK) $(SHLIB) mshlink $(SHLINK)
SHFLAGS	= -Wl,-soname,$(MSHLINK)
DEPLIBS	= ../$(SHLIB)
SHOPT	= -nostart

# Solaris2 with shared libm, so we can link it into the shared libstdc++.

LIBS    = $(ARLIB) $(ARLINK) $(SHLIB) $(SHLINK)
SHFLAGS = -h $(SHLIB)
SHDEPS  = -lm
DEPLIBS = ../$(SHLIB)

# Solaris2 with shared libm, so we can link it into the shared libstdc++.

LIBS    = $(ARLIB) $(SHLIB) $(SHLINK)
SHFLAGS = -h $(SHLIB)
SHDEPS  = -lm
DEPLIBS = ../$(SHLIB)

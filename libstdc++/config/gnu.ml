# Elf with shared libm, so we can link it into the shared libstdc++.

LIBS    = $(ARLIB) $(ARLINK) $(SHLIB) $(SHLINK)
SHFLAGS = -Wl,-soname,$(SHLIB)
SHDEPS  = -lm
DEPLIBS = ../$(SHLIB)

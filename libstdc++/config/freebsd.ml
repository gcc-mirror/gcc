# Elf with shared libm, so we can link it into the shared libstdc++.

LIBS    = $(ARLIB) $(ARLINK) $(SHLIB) $(SHLINK) mshlink
SHFLAGS = -Wl,-soname,$(MSHLINK)
SHDEPS  = -lm
DEPLIBS = ../$(SHLIB)

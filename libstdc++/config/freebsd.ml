# Elf with shared libm, so we can link it into the shared libstdc++.

LIBS    = $(ARLIB) $(SHLIB) $(SHLINK) mshlink
SHFLAGS = -Wl,-soname,$(MSHLINK)
SHDEPS  = -lm
DEPLIBS = ../$(SHLIB)

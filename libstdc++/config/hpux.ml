# HPUX uses the .sl suffix for shared libraries.

SHLIB   = libstdc++.sl
LIBS    = $(ARLIB) $(SHLIB)
DEPLIBS = ../$(SHLIB)
SHFLAGS = $(PICFLAG)

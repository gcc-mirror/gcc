# AIX has wierd shared/non-shared libraries.

ARLIB	 = libstdc++-ar.a 
SHLINK   = libstdc++.a
LIBS     = $(ARLIB) $(SHLIB) $(SHLINK)
DEPLIBS  = ../$(SHLIB)
SHDEPS   = -lm
SHFLAGS  = -Wl,-unix

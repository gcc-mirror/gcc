# AIX has wierd shared/non-shared libraries.

ARLIB	 = libstdc++-ar.a.$(VERSION)
ARLINK	 = libstdc++-ar.a
SHLINK   = libstdc++.a
LIBS     = $(ARLIB) $(ARLINK) $(SHLIB) $(SHLINK)
DEPLIBS  = ../$(SHLIB)
SHDEPS   = -lm
SHFLAGS  = -Wl,-unix

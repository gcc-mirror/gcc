# x86 Interix. SHLINK is defined to be .dummy to avoid running into 
# the lack of symbolic links.

SHLINK  = .dummy
SHLIB   = libstdc++.so
LIBS    = $(ARLIB) $(SHLIB)
DEPLIBS = ../$(SHLIB)
SHFLAGS = $(PICFLAG)


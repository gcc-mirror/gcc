# Base shared lib for OpenBSD i386

LIBS    = $(ARLIB) $(ARLINK) $(SHLIB) $(SHLINK) mshlink
SHFLAGS = -nostdlib -Wl,-Bshareable,-Bforcearchive
SHDEPS  = -lm
DEPLIBS = ../$(SHLIB)


# Elf without shared libm -- we have to link with the archive library, even
# for programs that don't use complex.

LIBS    = $(ARLIB) $(SHLIB) $(SHLINK)
SHFLAGS = -h $(SHLIB)
DEPLIBS = ../$(SHLIB)
LDLIBS  = -L.. -lstdc++ -lm
MLDLIBS = -L.. -lstdc++ -lm

# SunOS doesn't provide a shared libm, so we have to link with the archive
# library, even for programs that don't use complex.
# SunOS requires a version number in shared library filenames.

LIBS    = $(ARLIB) $(ARLINK) $(SHLIB)
SHFLAGS = $(PICFLAG)
DEPLIBS = ../$(SHLIB)
LDLIBS	= -L.. -lstdc++ -lm
MLDLIBS = -L.. -lstdc++ -lm

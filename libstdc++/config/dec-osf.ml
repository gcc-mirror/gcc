# We don't need -fpic on the alpha, so let's install both the shared and
# non-shared versions.

LIBS     = $(ARLIB) $(SHLIB) $(SHLINK)
DEPLIBS  = ../$(SHLIB)
SHDEPS   = -lm

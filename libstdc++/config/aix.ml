# AIX has weird shared/non-shared libraries.

ARLIB    =
ARLINK   =

# Build shared object with interface versioning in name.
# Archive resulting shared object in a library.
AIXLINK  = libstdc++.a
AIXLIB   = libstdc++-$(VERSION).a
AIXSHLIB = shr$(LIBSTDCXX_INTERFACE)$(CXX_INTERFACE).o
SHFLAGS  = -Wl,-bexpall
SHDEPS   = -lm
SHOPT    = -shared

LIBS     = $(AIXLIB) $(AIXLINK)

$(AIXSHLIB): stdlist
	$(CC) $(LIBCXXFLAGS) $(SHFLAGS) $(SHOPT) -o $(AIXSHLIB) `cat stdlist` $(SHDEPS)

# Delete $(AIXSHLIB) so that it is not included in stdlist if rebuilding.
$(AIXLIB): $(AIXSHLIB)
	-rm -f t$(AIXLIB)
	$(AR) $(AR_FLAGS) t$(AIXLIB) $(AIXSHLIB)
	mv t$(AIXLIB) $(AIXLIB)
	-rm -f $(AIXSHLIB)

$(AIXLINK):
	-rm -f $(AIXLINK)
	$(LN_S) $(AIXLIB) $(AIXLINK) || cp $(AIXLIB) $(AIXLINK)


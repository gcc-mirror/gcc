# Elf with shared libm, so we can link it into the shared libstdc++.

ARLIB   = libstdc++-$(LIBSTDCXX_INTERFACE)$(LIBC_INTERFACE)$(CXX_INTERFACE)-$(VERSION).a
MARLINK = libstdc++$(LIBC_INTERFACE)$(CXX_INTERFACE).a.$(LIBSTDCXX_INTERFACE)
SHLIB   = libstdc++-$(LIBSTDCXX_INTERFACE)$(LIBC_INTERFACE)$(CXX_INTERFACE)-$(VERSION).so
MSHLINK = libstdc++$(LIBC_INTERFACE)$(CXX_INTERFACE).so.$(LIBSTDCXX_INTERFACE)

LIBS    = $(ARLIB) marlink $(ARLINK) $(SHLIB) mshlink $(SHLINK)
SHFLAGS = -Wl,-soname,$(MSHLINK)
SHDEPS  = -lm
DEPLIBS = ../$(SHLIB)

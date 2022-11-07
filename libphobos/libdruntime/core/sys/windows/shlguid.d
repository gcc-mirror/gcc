/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_shlguid.d)
 */
module core.sys.windows.shlguid;
version (Windows):

import core.sys.windows.basetyps, core.sys.windows.w32api;

// FIXME: clean up Windows version support

// I think this is just a helper macro for other win32 headers?
//MACRO #define DEFINE_SHLGUID(n,l,w1,w2) DEFINE_GUID(n,l,w1,w2,0xC0,0,0,0,0,0,0,0x46)

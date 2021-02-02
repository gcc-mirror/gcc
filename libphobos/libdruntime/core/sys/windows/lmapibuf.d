/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_lmapibuf.d)
 */
module core.sys.windows.lmapibuf;
version (Windows):
pragma(lib, "netapi32");

import core.sys.windows.lmcons, core.sys.windows.windef;

extern (Windows) {
    NET_API_STATUS NetApiBufferAllocate(DWORD, PVOID*);
    NET_API_STATUS NetApiBufferFree(PVOID);
    NET_API_STATUS NetApiBufferReallocate(PVOID, DWORD, PVOID*);
    NET_API_STATUS NetApiBufferSize(PVOID, PDWORD);
    NET_API_STATUS NetapipBufferAllocate(DWORD, PVOID*);
}

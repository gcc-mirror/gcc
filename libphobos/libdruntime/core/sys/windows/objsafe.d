/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_objsafe.d)
 */
module core.sys.windows.objsafe;
version (Windows):

import core.sys.windows.basetyps, core.sys.windows.unknwn, core.sys.windows.windef;

enum {
    INTERFACESAFE_FOR_UNTRUSTED_CALLER = 1,
    INTERFACESAFE_FOR_UNTRUSTED_DATA
}

interface IObjectSafety : IUnknown {
    HRESULT GetInterfaceSafetyOptions(REFIID, DWORD*, DWORD*);
    HRESULT SetInterfaceSafetyOptions(REFIID, DWORD, DWORD);
}

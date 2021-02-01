/**
 * Windows API header module
 *
 * Translated from MinGW API for MS-Windows 3.10
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_isguids.d)
 */
module core.sys.windows.isguids;
version (Windows):

import core.sys.windows.basetyps;

extern (C) extern const GUID
    CLSID_InternetShortcut,
    IID_IUniformResourceLocator;

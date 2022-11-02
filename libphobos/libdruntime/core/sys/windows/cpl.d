/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_cpl.d)
 */
module core.sys.windows.cpl;
version (Windows):

version (ANSI) {} else version = Unicode;

import core.sys.windows.windef, core.sys.windows.winuser;

enum : uint {
    WM_CPL_LAUNCH = WM_USER + 1000,
    WM_CPL_LAUNCHED
}

enum : uint {
    CPL_DYNAMIC_RES,
    CPL_INIT,
    CPL_GETCOUNT,
    CPL_INQUIRE,
    CPL_SELECT,
    CPL_DBLCLK,
    CPL_STOP,
    CPL_EXIT,
    CPL_NEWINQUIRE,
    CPL_STARTWPARMSA,
    CPL_STARTWPARMSW, // = 10
    CPL_SETUP = 200
}

extern (Windows) alias LONG function(HWND, UINT, LONG, LONG) APPLET_PROC;

align(1)
struct CPLINFO {
align(1):
    int  idIcon;
    int  idName;
    int  idInfo;
    LONG_PTR  lData;
}
alias CPLINFO* LPCPLINFO;

align(1)
struct NEWCPLINFOA {
align(1):
    DWORD     dwSize = NEWCPLINFOA.sizeof;
    DWORD     dwFlags;
    DWORD     dwHelpContext;
    LONG_PTR  lData;
    HICON     hIcon;
    CHAR[32]  szName = 0;
    CHAR[64]  szInfo = 0;
    CHAR[128] szHelpFile = 0;
}
alias NEWCPLINFOA* LPNEWCPLINFOA;

align(1)
struct NEWCPLINFOW {
align(1):
    DWORD      dwSize = NEWCPLINFOW.sizeof;
    DWORD      dwFlags;
    DWORD      dwHelpContext;
    LONG_PTR   lData;
    HICON      hIcon;
    WCHAR[32]  szName = 0;
    WCHAR[64]  szInfo = 0;
    WCHAR[128] szHelpFile = 0;
}
alias NEWCPLINFOW* LPNEWCPLINFOW;

version (Unicode) {
    alias CPL_STARTWPARMSW CPL_STARTWPARMS;
    alias NEWCPLINFOW NEWCPLINFO;
} else {
    alias CPL_STARTWPARMSA CPL_STARTWPARMS;
    alias NEWCPLINFOA NEWCPLINFO;
}

alias NEWCPLINFO* LPNEWCPLINFO;

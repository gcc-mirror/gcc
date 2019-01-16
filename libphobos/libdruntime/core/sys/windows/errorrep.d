/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_errorrep.d)
 */
module core.sys.windows.errorrep;
version (Windows):

version (ANSI) {} else version = Unicode;

private import core.sys.windows.w32api, core.sys.windows.windef;

static assert (_WIN32_WINNT >= 0x501,
    "core.sys.windows.errorrep is available only if version WindowsXP, Windows2003 "
    ~ "or WindowsVista is set");

enum EFaultRepRetVal {
    frrvOk,
    frrvOkManifest,
    frrvOkQueued,
    frrvErr,
    frrvErrNoDW,
    frrvErrTimeout,
    frrvLaunchDebugger,
    frrvOkHeadless // = 7
}

extern (Windows) {
    BOOL AddERExcludedApplicationA(LPCSTR);
    BOOL AddERExcludedApplicationW(LPCWSTR);
    EFaultRepRetVal ReportFault(LPEXCEPTION_POINTERS, DWORD);
}

version (Unicode) {
    alias AddERExcludedApplicationW AddERExcludedApplication;
} else {
    alias AddERExcludedApplicationA AddERExcludedApplication;
}

/**
 * Windows API header module
 *
 * Translated from MinGW API for MS-Windows 4.0
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_w32api.d)
 */
module core.sys.windows.w32api;
version (Windows):

version (ANSI) {} else version = Unicode;

enum __W32API_VERSION = 3.17;
enum __W32API_MAJOR_VERSION = 3;
enum __W32API_MINOR_VERSION = 17;

/*  These version identifiers are used to specify the minimum version of Windows that an
 *  application will support.
 *
 *  Previously the minimum Windows 9x and Windows NT versions could be specified.  However, since
 *  Windows 9x is no longer supported, either by Microsoft or by DMD, this distinction has been
 *  removed in order to simplify the bindings.
 */
 version (Windows10) {
    enum uint _WIN32_WINNT = 0x604;
} else version (Windows8_1) {    // also Windows2012R2
    enum uint _WIN32_WINNT = 0x603;
} else version (Windows8) {      // also Windows2012
    enum uint _WIN32_WINNT = 0x602;
} else version (Windows7) {      // also Windows2008R2
    enum uint _WIN32_WINNT = 0x601;
} else version (WindowsVista) {  // also Windows2008
    enum uint _WIN32_WINNT = 0x600;
} else version (Windows2003) {   // also WindowsHomeServer, WindowsXP64
    enum uint _WIN32_WINNT = 0x502;
} else version (WindowsXP) {
    enum uint _WIN32_WINNT = 0x501;
} else version (Windows2000) {
    // Current DMD doesn't support any version of Windows older than XP,
    // but third-party compilers could use this
    enum uint _WIN32_WINNT = 0x500;
} else {
    enum uint _WIN32_WINNT = 0x501;
}

version (IE10) {
    enum uint _WIN32_IE = 0xA00;
} else version (IE9) {
    enum uint _WIN32_IE = 0x900;
} else version (IE8) {
    enum uint _WIN32_IE = 0x800;
} else version (IE7) {
    enum uint _WIN32_IE = 0x700;
} else version (IE602) {
    enum uint _WIN32_IE = 0x603;
} else version (IE601) {
    enum uint _WIN32_IE = 0x601;
} else version (IE6) {
    enum uint _WIN32_IE = 0x600;
} else version (IE56) {
    enum uint _WIN32_IE = 0x560;
} else version (IE501) {
    enum uint _WIN32_IE = 0x501;
} else version (IE5) {
    enum uint _WIN32_IE = 0x500;
} else version (IE401) {
    enum uint _WIN32_IE = 0x401;
} else version (IE4) {
    enum uint _WIN32_IE = 0x400;
} else version (IE3) {
    enum uint _WIN32_IE = 0x300;
} else static if (_WIN32_WINNT >= 0x410) {
    enum uint _WIN32_IE = 0x400;
} else {
    enum uint _WIN32_IE = 0;
}

debug (WindowsUnitTest) {
    unittest {
        printf("Windows NT version: %03x\n", _WIN32_WINNT);
        printf("IE version:         %03x\n", _WIN32_IE);
    }
}

version (Unicode) {
    enum bool _WIN32_UNICODE = true;
    package template DECLARE_AW(string name) {
        mixin("alias " ~ name ~ "W " ~ name ~ ";");
    }
} else {
    enum bool _WIN32_UNICODE = false;
    package template DECLARE_AW(string name) {
        mixin("alias " ~ name ~ "A " ~ name ~ ";");
    }
}

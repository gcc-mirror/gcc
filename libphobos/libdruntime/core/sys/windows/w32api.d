/**
 * Windows API header module
 *
 * Translated from MinGW API for MS-Windows 4.0
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_w32api.d)
 */
module core.sys.windows.w32api;
version (Windows):

import core.sys.windows.sdkddkver;

version (ANSI) {} else version = Unicode;

enum __W32API_VERSION = 3.17;
enum __W32API_MAJOR_VERSION = 3;
enum __W32API_MINOR_VERSION = 17;

enum Windows95 = 0x0400;
enum Windows98 = 0x0410;
enum WindowsME = 0x0500;

enum WindowsNT4   = 0x0400;
enum Windows2000  = 0x0500;
enum WindowsXP    = 0x0501;
enum Windows2003  = 0x0502;
enum WindowsVista = 0x0600;
enum Windows7     = 0x0601;
enum Windows8     = 0x0602;

enum IE3   = 0x0300;
enum IE301 = 0x0300;
enum IE302 = 0x0300;
enum IE4   = 0x0400;
enum IE401 = 0x0401;
enum IE5   = 0x0500;
enum IE5a  = 0x0500;
enum IE5b  = 0x0500;
enum IE501 = 0x0501;
enum IE55  = 0x0501;
enum IE56  = 0x0560;
enum IE6   = 0x0600;
enum IE601 = 0x0601;
enum IE602 = 0x0603;
enum IE7   = 0x0700;
enum IE8   = 0x0800;
enum IE9   = 0x0900;
enum IE10  = 0x0A00;

/*  These version identifiers are used to specify the minimum version of Windows that an
 *  application will support.
 *
 *  Previously the minimum Windows 9x and Windows NT versions could be specified.  However, since
 *  Windows 9x is no longer supported, either by Microsoft or by DMD, this distinction has been
 *  removed in order to simplify the bindings.
 */
version (Windows11) {
    enum uint _WIN32_WINNT = _WIN32_WINNT_WIN10;
} else version (Windows10) {
    enum uint _WIN32_WINNT = _WIN32_WINNT_WIN10;
} else version (Windows8_1) {    // also Windows2012R2
    enum uint _WIN32_WINNT = _WIN32_WINNT_WINBLUE;
} else version (Windows8) {      // also Windows2012
    enum uint _WIN32_WINNT = _WIN32_WINNT_WIN8;
} else version (Windows7) {      // also Windows2008R2
    enum uint _WIN32_WINNT = _WIN32_WINNT_WIN7;
} else version (WindowsVista) {  // also Windows2008
    enum uint _WIN32_WINNT = _WIN32_WINNT_VISTA;
} else version (Windows2003) {   // also WindowsHomeServer, WindowsXP64
    enum uint _WIN32_WINNT = _WIN32_WINNT_WS03;
} else version (WindowsXP) {
    enum uint _WIN32_WINNT = _WIN32_WINNT_WINXP;
} else version (Windows2000) {
    // Current DMD doesn't support any version of Windows older than XP,
    // but third-party compilers could use this
    enum uint _WIN32_WINNT = _WIN32_WINNT_WIN2K;
} else {
    enum uint _WIN32_WINNT = _WIN32_WINNT_WIN7;
}

version (IE11) {
    enum uint _WIN32_IE = _WIN32_IE_IE110;
} else version (IE10) {
    enum uint _WIN32_IE = _WIN32_IE_IE100;
} else version (IE9) {
    enum uint _WIN32_IE = _WIN32_IE_IE90;
} else version (IE8) {
    enum uint _WIN32_IE = _WIN32_IE_IE80;
} else version (IE7) {
    enum uint _WIN32_IE = _WIN32_IE_IE70;
} else version (IE602) {
    enum uint _WIN32_IE = _WIN32_IE_IE60SP2;
} else version (IE601) {
    enum uint _WIN32_IE = _WIN32_IE_IE60SP1;
} else version (IE6) {
    enum uint _WIN32_IE = _WIN32_IE_IE60;
} else version (IE56) {
    enum uint _WIN32_IE = _WIN32_IE_IE60;
} else version (IE55) {
    enum uint _WIN32_IE = _WIN32_IE_IE55;
} else version (IE501) {
    enum uint _WIN32_IE = _WIN32_IE_IE501;
} else version (IE5) {
    enum uint _WIN32_IE = _WIN32_IE_IE50;
} else version (IE401) {
    enum uint _WIN32_IE = _WIN32_IE_IE401;
} else version (IE4) {
    enum uint _WIN32_IE = _WIN32_IE_IE40;
} else version (IE3) {
    enum uint _WIN32_IE = _WIN32_IE_IE30;
} else static if (_WIN32_WINNT >= _WIN32_WINNT_WIN2K) {
    enum uint _WIN32_IE = _WIN32_IE_IE60;
} else static if (_WIN32_WINNT >= Windows98) { //NOTE: _WIN32_WINNT will never be set this low
    enum uint _WIN32_IE = _WIN32_IE_IE40;
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

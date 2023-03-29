/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_custcntl.d)
 */
module core.sys.windows.custcntl;
version (Windows):

version (ANSI) {} else version = Unicode;

import core.sys.windows.windef;

// FIXME: check type
enum CCF_NOTEXT = 1;

enum size_t
    CCHCCCLASS =  32,
    CCHCCDESC  =  32,
    CCHCCTEXT  = 256;

struct CCSTYLEA {
    DWORD           flStyle;
    DWORD           flExtStyle;
    CHAR[CCHCCTEXT] szText = 0;
    LANGID          lgid;
    WORD            wReserved1;
}
alias CCSTYLEA* LPCCSTYLEA;

struct CCSTYLEW {
    DWORD            flStyle;
    DWORD            flExtStyle;
    WCHAR[CCHCCTEXT] szText = 0;
    LANGID           lgid;
    WORD             wReserved1;
}
alias CCSTYLEW* LPCCSTYLEW;

struct CCSTYLEFLAGA {
    DWORD flStyle;
    DWORD flStyleMask;
    LPSTR pszStyle;
}
alias CCSTYLEFLAGA* LPCCSTYLEFLAGA;

struct CCSTYLEFLAGW {
    DWORD  flStyle;
    DWORD  flStyleMask;
    LPWSTR pszStyle;
}
alias CCSTYLEFLAGW* LPCCSTYLEFLAGW;

struct CCINFOA {
    CHAR[CCHCCCLASS]  szClass = 0;
    DWORD             flOptions;
    CHAR[CCHCCDESC]   szDesc = 0;
    UINT              cxDefault;
    UINT              cyDefault;
    DWORD             flStyleDefault;
    DWORD             flExtStyleDefault;
    DWORD             flCtrlTypeMask;
    CHAR[CCHCCTEXT]   szTextDefault = 0;
    INT               cStyleFlags;
    LPCCSTYLEFLAGA    aStyleFlags;
    LPFNCCSTYLEA      lpfnStyle;
    LPFNCCSIZETOTEXTA lpfnSizeToText;
    DWORD             dwReserved1;
    DWORD             dwReserved2;
}
alias CCINFOA* LPCCINFOA;

struct CCINFOW {
    WCHAR[CCHCCCLASS] szClass = 0;
    DWORD             flOptions;
    WCHAR[CCHCCDESC]  szDesc = 0;
    UINT              cxDefault;
    UINT              cyDefault;
    DWORD             flStyleDefault;
    DWORD             flExtStyleDefault;
    DWORD             flCtrlTypeMask;
    WCHAR[CCHCCTEXT]  szTextDefault = 0;
    INT               cStyleFlags;
    LPCCSTYLEFLAGW    aStyleFlags;
    LPFNCCSTYLEW      lpfnStyle;
    LPFNCCSIZETOTEXTW lpfnSizeToText;
    DWORD             dwReserved1;
    DWORD             dwReserved2;
}
alias CCINFOW* LPCCINFOW;

extern (Windows) {
    alias BOOL function(HWND, LPCCSTYLEA) LPFNCCSTYLEA;
    alias BOOL function(HWND, LPCCSTYLEW) LPFNCCSTYLEW;
    alias INT function(DWORD, DWORD, HFONT, LPSTR) LPFNCCSIZETOTEXTA;
    alias INT function(DWORD, DWORD, HFONT, LPWSTR) LPFNCCSIZETOTEXTW;
    alias UINT function(LPCCINFOA) LPFNCCINFOA;
    alias UINT function(LPCCINFOW) LPFNCCINFOW;
    UINT CustomControlInfoA(LPCCINFOA acci);
    UINT CustomControlInfoW(LPCCINFOW acci);
}

version (Unicode) {
    alias CCSTYLEW CCSTYLE;
    alias CCSTYLEFLAGW CCSTYLEFLAG;
    alias CCINFOW CCINFO;
    alias LPFNCCSTYLEW LPFNCCSTYLE;
    alias LPFNCCSIZETOTEXTW LPFNCCSIZETOTEXT;
    alias LPFNCCINFOW LPFNCCINFO;
} else {
    alias CCSTYLEA CCSTYLE;
    alias CCSTYLEFLAGA CCSTYLEFLAG;
    alias CCINFOA CCINFO;
    alias LPFNCCSTYLEA LPFNCCSTYLE;
    alias LPFNCCSIZETOTEXTA LPFNCCSIZETOTEXT;
    alias LPFNCCINFOA LPFNCCINFO;
}

alias CCSTYLE* LPCCSTYLE;
alias CCSTYLEFLAG* LPCCSTYLEFLAG;
alias CCINFO* LPCCINFO;

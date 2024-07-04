/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_windef.d)
 */
module core.sys.windows.windef;
version (Windows):

public import core.sys.windows.winnt;
import core.sys.windows.w32api;

enum size_t MAX_PATH = 260;

pure nothrow @nogc {
    ushort MAKEWORD(ubyte a, ubyte b) {
        return cast(ushort) ((b << 8) | a);
    }

    ushort MAKEWORD(ushort a, ushort b) {
        assert((a & 0xFF00) == 0);
        assert((b & 0xFF00) == 0);
        return MAKEWORD(cast(ubyte)a, cast(ubyte)b);
    }

    uint MAKELONG(ushort a, ushort b) {
        return cast(uint) ((b << 16) | a);
    }

    uint MAKELONG(uint a, uint b) {
        assert((a & 0xFFFF0000) == 0);
        assert((b & 0xFFFF0000) == 0);
        return MAKELONG(cast(ushort)a, cast(ushort)b);
    }

    ushort LOWORD(ulong l) {
        return cast(ushort) l;
    }

    ushort HIWORD(ulong l) {
        return cast(ushort) (l >>> 16);
    }

    ubyte LOBYTE(ushort w) {
        return cast(ubyte) w;
    }

    ubyte HIBYTE(ushort w) {
        return cast(ubyte) (w >>> 8);
    }
}

enum NULL = null;
static assert (is(typeof({
    void test(int* p) {}
    test(NULL);
})));

alias ubyte        BYTE;
alias ubyte*       PBYTE, LPBYTE;
alias ushort       USHORT, WORD, ATOM;
alias ushort*      PUSHORT, PWORD, LPWORD;
alias uint         ULONG, DWORD, UINT, COLORREF;
alias uint*        PULONG, PDWORD, LPDWORD, PUINT, LPUINT, LPCOLORREF;
alias int          WINBOOL, BOOL, INT, LONG, HFILE, HRESULT;
alias int*         PWINBOOL, LPWINBOOL, PBOOL, LPBOOL, PINT, LPINT, LPLONG;
alias float        FLOAT;
alias float*       PFLOAT;
alias const(void)* PCVOID, LPCVOID;

alias UINT_PTR WPARAM;
alias LONG_PTR LPARAM, LRESULT;

alias HHOOK = HANDLE;
alias HGLOBAL = HANDLE;
alias HLOCAL = HANDLE;
alias GLOBALHANDLE = HANDLE;
alias LOCALHANDLE = HANDLE;
alias HGDIOBJ = HANDLE;
alias HACCEL = HANDLE;
alias HBITMAP = HGDIOBJ;
alias HBRUSH = HGDIOBJ;
alias HCOLORSPACE = HANDLE;
alias HDC = HANDLE;
alias HGLRC = HANDLE;
alias HDESK = HANDLE;
alias HENHMETAFILE = HANDLE;
alias HFONT = HGDIOBJ;
alias HICON = HANDLE;
alias HINSTANCE = HANDLE;
alias HKEY = HANDLE;
alias HMENU = HANDLE;
alias HMETAFILE = HANDLE;
alias HMODULE = HANDLE;
alias HMONITOR = HANDLE;
alias HPALETTE = HANDLE;
alias HPEN = HGDIOBJ;
alias HRGN = HGDIOBJ;
alias HRSRC = HANDLE;
alias HSTR = HANDLE;
alias HTASK = HANDLE;
alias HWND = HANDLE;
alias HWINSTA = HANDLE;
alias HKL = HANDLE;
alias HCURSOR = HANDLE;
alias HKEY* PHKEY;

static if (_WIN32_WINNT >= 0x500) {
    alias HTERMINAL = HANDLE;
    alias HWINEVENTHOOK = HANDLE;
}

alias extern (Windows) INT_PTR function() nothrow FARPROC, NEARPROC, PROC;

struct RECT {
    LONG left;
    LONG top;
    LONG right;
    LONG bottom;
}
alias RECT RECTL;
alias RECT*        PRECT, NPRECT, LPRECT, PRECTL, LPRECTL;
alias const(RECT)* LPCRECT, LPCRECTL;

struct POINT {
    LONG x;
    LONG y;
}
alias POINT POINTL;
alias POINT* PPOINT, NPPOINT, LPPOINT, PPOINTL, LPPOINTL;

struct SIZE {
    LONG cx;
    LONG cy;
}
alias SIZE SIZEL;
alias SIZE* PSIZE, LPSIZE, PSIZEL, LPSIZEL;

struct POINTS {
    SHORT x;
    SHORT y;
}
alias POINTS* PPOINTS, LPPOINTS;

enum : BOOL {
    FALSE = 0,
    TRUE  = 1
}

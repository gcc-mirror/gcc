/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_windef.d)
 */
module core.sys.windows.windef;
version (Windows):
@system:

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

mixin DECLARE_HANDLE!("HHOOK");
mixin DECLARE_HANDLE!("HGLOBAL");
mixin DECLARE_HANDLE!("HLOCAL");
mixin DECLARE_HANDLE!("GLOBALHANDLE");
mixin DECLARE_HANDLE!("LOCALHANDLE");
mixin DECLARE_HANDLE!("HGDIOBJ");
mixin DECLARE_HANDLE!("HACCEL");
mixin DECLARE_HANDLE!("HBITMAP", HGDIOBJ);
mixin DECLARE_HANDLE!("HBRUSH", HGDIOBJ);
mixin DECLARE_HANDLE!("HCOLORSPACE");
mixin DECLARE_HANDLE!("HDC");
mixin DECLARE_HANDLE!("HGLRC");
mixin DECLARE_HANDLE!("HDESK");
mixin DECLARE_HANDLE!("HENHMETAFILE");
mixin DECLARE_HANDLE!("HFONT", HGDIOBJ);
mixin DECLARE_HANDLE!("HICON");
mixin DECLARE_HANDLE!("HINSTANCE");
mixin DECLARE_HANDLE!("HKEY");
mixin DECLARE_HANDLE!("HMENU");
mixin DECLARE_HANDLE!("HMETAFILE");
mixin DECLARE_HANDLE!("HMODULE");
mixin DECLARE_HANDLE!("HMONITOR");
mixin DECLARE_HANDLE!("HPALETTE");
mixin DECLARE_HANDLE!("HPEN", HGDIOBJ);
mixin DECLARE_HANDLE!("HRGN", HGDIOBJ);
mixin DECLARE_HANDLE!("HRSRC");
mixin DECLARE_HANDLE!("HSTR");
mixin DECLARE_HANDLE!("HTASK");
mixin DECLARE_HANDLE!("HWND");
mixin DECLARE_HANDLE!("HWINSTA");
mixin DECLARE_HANDLE!("HKL");
mixin DECLARE_HANDLE!("HCURSOR");
alias HKEY* PHKEY;

static if (_WIN32_WINNT >= 0x500) {
    mixin DECLARE_HANDLE!("HTERMINAL");
    mixin DECLARE_HANDLE!("HWINEVENTHOOK");
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

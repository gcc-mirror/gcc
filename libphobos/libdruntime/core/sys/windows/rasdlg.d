/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_rasdlg.d)
 */
module core.sys.windows.rasdlg;
version (Windows):
@system:

version (ANSI) {} else version = Unicode;

import core.sys.windows.ras;
import core.sys.windows.lmcons, core.sys.windows.windef;

enum {
    RASPBDEVENT_AddEntry = 1,
    RASPBDEVENT_EditEntry,
    RASPBDEVENT_RemoveEntry,
    RASPBDEVENT_DialEntry,
    RASPBDEVENT_EditGlobals,
    RASPBDEVENT_NoUser,
    RASPBDEVENT_NoUserEdit
}

enum RASPBDFLAG_PositionDlg      =  1;
enum RASPBDFLAG_ForceCloseOnDial =  2;
enum RASPBDFLAG_NoUser           = 16;

enum RASEDFLAG_PositionDlg = 1;
enum RASEDFLAG_NewEntry    = 2;
enum RASEDFLAG_CloneEntry  = 4;

enum RASDDFLAG_PositionDlg = 1;

align(4):

struct RASENTRYDLGA {
align(4):
    DWORD     dwSize = RASENTRYDLGA.sizeof;
    HWND      hwndOwner;
    DWORD     dwFlags;
    LONG      xDlg;
    LONG      yDlg;
    CHAR[RAS_MaxEntryName + 1] szEntry = 0;
    DWORD     dwError;
    ULONG_PTR reserved;
    ULONG_PTR reserved2;
}
alias RASENTRYDLGA* LPRASENTRYDLGA;

struct RASENTRYDLGW {
align(4):
    DWORD     dwSize = RASENTRYDLGW.sizeof;
    HWND      hwndOwner;
    DWORD     dwFlags;
    LONG      xDlg;
    LONG      yDlg;
    WCHAR[RAS_MaxEntryName + 1] szEntry = 0;
    DWORD     dwError;
    ULONG_PTR reserved;
    ULONG_PTR reserved2;
}
alias RASENTRYDLGW* LPRASENTRYDLGW;

struct RASDIALDLG {
align(4):
    DWORD     dwSize;
    HWND      hwndOwner;
    DWORD     dwFlags;
    LONG      xDlg;
    LONG      yDlg;
    DWORD     dwSubEntry;
    DWORD     dwError;
    ULONG_PTR reserved;
    ULONG_PTR reserved2;
}
alias RASDIALDLG* LPRASDIALDLG;

// Application-defined callback functions
extern (Windows) {
    alias VOID function(ULONG_PTR, DWORD, LPWSTR, LPVOID) RASPBDLGFUNCW;
    alias VOID function(ULONG_PTR, DWORD, LPSTR, LPVOID) RASPBDLGFUNCA;
}

struct RASPBDLGA {
align(4):
    DWORD         dwSize = RASPBDLGA.sizeof;
    HWND          hwndOwner;
    DWORD         dwFlags;
    LONG          xDlg;
    LONG          yDlg;
    ULONG_PTR     dwCallbackId;
    RASPBDLGFUNCA pCallback;
    DWORD         dwError;
    ULONG_PTR     reserved;
    ULONG_PTR     reserved2;
}
alias RASPBDLGA* LPRASPBDLGA;

struct RASPBDLGW {
align(4):
    DWORD         dwSize = RASPBDLGW.sizeof;
    HWND          hwndOwner;
    DWORD         dwFlags;
    LONG          xDlg;
    LONG          yDlg;
    ULONG_PTR     dwCallbackId;
    RASPBDLGFUNCW pCallback;
    DWORD         dwError;
    ULONG_PTR     reserved;
    ULONG_PTR     reserved2;
}
alias RASPBDLGW* LPRASPBDLGW;

struct RASNOUSERA
{
    DWORD           dwSize = RASNOUSERA.sizeof;
    DWORD           dwFlags;
    DWORD           dwTimeoutMs;
    CHAR[UNLEN + 1] szUserName = 0;
    CHAR[PWLEN + 1] szPassword = 0;
    CHAR[DNLEN + 1] szDomain = 0;
}
alias RASNOUSERA* LPRASNOUSERA;

struct RASNOUSERW {
    DWORD            dwSize = RASNOUSERW.sizeof;
    DWORD            dwFlags;
    DWORD            dwTimeoutMs;
    WCHAR[UNLEN + 1] szUserName = 0;
    WCHAR[PWLEN + 1] szPassword = 0;
    WCHAR[DNLEN + 1] szDomain = 0;
}
alias RASNOUSERW* LPRASNOUSERW;

extern (Windows) {
    BOOL RasDialDlgA(LPSTR, LPSTR, LPSTR, LPRASDIALDLG);
    BOOL RasDialDlgW(LPWSTR, LPWSTR, LPWSTR, LPRASDIALDLG);
    BOOL RasEntryDlgA(LPSTR, LPSTR, LPRASENTRYDLGA);
    BOOL RasEntryDlgW(LPWSTR, LPWSTR, LPRASENTRYDLGW);
    BOOL RasPhonebookDlgA(LPSTR, LPSTR, LPRASPBDLGA);
    BOOL RasPhonebookDlgW(LPWSTR, LPWSTR, LPRASPBDLGW);
}

version (Unicode) {
    alias RASENTRYDLGW RASENTRYDLG;
    alias RASPBDLGW RASPBDLG;
    alias RASNOUSERW RASNOUSER;
    alias RasDialDlgW RasDialDlg;
    alias RasEntryDlgW RasEntryDlg;
    alias RasPhonebookDlgW RasPhonebookDlg;
} else {
    alias RASENTRYDLGA RASENTRYDLG;
    alias RASPBDLGA RASPBDLG;
    alias RASNOUSERA RASNOUSER;
    alias RasDialDlgA RasDialDlg;
    alias RasEntryDlgA RasEntryDlg;
    alias RasPhonebookDlgA RasPhonebookDlg;
}

alias RASENTRYDLG* LPRASENTRYDLG;
alias RASPBDLG* LPRASPBDLG;
alias RASNOUSER* LPRASNOUSER;

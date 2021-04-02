/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Vladimir Vlasov
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_prsht.d)
 */
module core.sys.windows.prsht;
version (Windows):
@system:

version (ANSI) {} else version = Unicode;
pragma(lib, "comctl32");

import core.sys.windows.w32api, core.sys.windows.windef, core.sys.windows.winuser;

enum MAXPROPPAGES = 100;

enum {
    PSP_DEFAULT      = 0x00000000,
    PSP_DLGINDIRECT  = 0x00000001,
    PSP_USEHICON     = 0x00000002,
    PSP_USEICONID    = 0x00000004,
    PSP_USETITLE     = 0x00000008,
    PSP_RTLREADING   = 0x00000010,
    PSP_HASHELP      = 0x00000020,
    PSP_USEREFPARENT = 0x00000040,
    PSP_USECALLBACK  = 0x00000080,
    PSP_PREMATURE    = 0x00000400
}

static if (_WIN32_IE >= 0x400) {
    enum {
        PSP_HIDEHEADER        = 0x00000800,
        PSP_USEHEADERTITLE    = 0x00001000,
        PSP_USEHEADERSUBTITLE = 0x00002000
    }
}

enum {
    PSPCB_RELEASE = 1,
    PSPCB_CREATE
}

enum {
    PSH_DEFAULT           = 0x00000000,
    PSH_PROPTITLE         = 0x00000001,
    PSH_USEHICON          = 0x00000002,
    PSH_USEICONID         = 0x00000004,
    PSH_PROPSHEETPAGE     = 0x00000008,
    PSH_WIZARDHASFINISH   = 0x00000010,
    PSH_WIZARD            = 0x00000020,
    PSH_USEPSTARTPAGE     = 0x00000040,
    PSH_NOAPPLYNOW        = 0x00000080,
    PSH_USECALLBACK       = 0x00000100,
    PSH_HASHELP           = 0x00000200,
    PSH_MODELESS          = 0x00000400,
    PSH_RTLREADING        = 0x00000800,
    PSH_WIZARDCONTEXTHELP = 0x00001000
}

static if (_WIN32_IE >= 0x400) {
    enum {
        PSH_WATERMARK        = 0x00008000,
        PSH_USEHBMWATERMARK  = 0x00010000,
        PSH_USEHPLWATERMARK  = 0x00020000,
        PSH_STRETCHWATERMARK = 0x00040000,
        PSH_HEADER           = 0x00080000,
        PSH_USEHBMHEADER     = 0x00100000,
        PSH_USEPAGELANG      = 0x00200000
    }
    static if (_WIN32_IE < 0x0500) {
        enum {
            PSH_WIZARD97 = 0x00002000
        }
    } else {
        enum {
            PSH_WIZARD97 = 0x01000000
        }
    }
}

static if (_WIN32_IE >= 0x500) {
    enum {
        PSH_WIZARD_LITE   = 0x00400000,
        PSH_NOCONTEXTHELP = 0x02000000
    }
}

enum {
    PSCB_INITIALIZED = 1,
    PSCB_PRECREATE
}

enum {
    PSN_FIRST       = (-200),
    PSN_LAST        = (-299),
    PSN_SETACTIVE   = (-200),
    PSN_KILLACTIVE  = (-201),
    PSN_APPLY       = (-202),
    PSN_RESET       = (-203),
    PSN_HELP        = (-205),
    PSN_WIZBACK     = (-206),
    PSN_WIZNEXT     = (-207),
    PSN_WIZFINISH   = (-208),
    PSN_QUERYCANCEL = (-209)
}
static if (_WIN32_IE >= 0x400) {
    enum {
        PSN_GETOBJECT = (-210)
    }
}
static if (_WIN32_IE >= 0x500) {
    enum {
        PSN_TRANSLATEACCELERATOR = (-212),
        PSN_QUERYINITIALFOCUS    = (-213)
    }
}

enum {
    PSNRET_NOERROR,
    PSNRET_INVALID,
    PSNRET_INVALID_NOCHANGEPAGE,
    PSNRET_MESSAGEHANDLED
}

enum {
    ID_PSRESTARTWINDOWS = 0x2,
    ID_PSREBOOTSYSTEM   = ID_PSRESTARTWINDOWS | 0x1
}

enum {
    WIZ_CXDLG  = 276,
    WIZ_CYDLG  = 140,
    WIZ_CXBMP  = 80,
    WIZ_BODYX  = 92,
    WIZ_BODYCX = 184
}

enum {
    PROP_SM_CXDLG  = 212,
    PROP_SM_CYDLG  = 188,
    PROP_MED_CXDLG = 227,
    PROP_MED_CYDLG = 215,
    PROP_LG_CXDLG  = 252,
    PROP_LG_CYDLG  = 218
}

enum {
    PSBTN_BACK,
    PSBTN_NEXT,
    PSBTN_FINISH,
    PSBTN_OK,
    PSBTN_APPLYNOW,
    PSBTN_CANCEL,
    PSBTN_HELP,
    PSBTN_MAX = 6
}

enum {
    PSWIZB_BACK           = 1,
    PSWIZB_NEXT           = 2,
    PSWIZB_FINISH         = 4,
    PSWIZB_DISABLEDFINISH = 8
}

enum {
    PSM_SETCURSEL = WM_USER + 101,
    PSM_REMOVEPAGE,
    PSM_ADDPAGE,
    PSM_CHANGED,
    PSM_RESTARTWINDOWS,
    PSM_REBOOTSYSTEM,
    PSM_CANCELTOCLOSE,
    PSM_QUERYSIBLINGS,
    PSM_UNCHANGED,
    PSM_APPLY,
    PSM_SETTITLEA,
    PSM_SETWIZBUTTONS,
    PSM_PRESSBUTTON,
    PSM_SETCURSELID,
    PSM_SETFINISHTEXTA,
    PSM_GETTABCONTROL,
    PSM_ISDIALOGMESSAGE,
    PSM_GETCURRENTPAGEHWND,
    PSM_INSERTPAGE,
    PSM_SETTITLEW,
    PSM_SETFINISHTEXTW // = WM_USER + 121,
}

extern (Windows) {
    alias UINT function(HWND, UINT, LPPROPSHEETPAGEA) LPFNPSPCALLBACKA;
    alias UINT function(HWND, UINT, LPPROPSHEETPAGEW) LPFNPSPCALLBACKW;
    alias int function(HWND, UINT, LPARAM) PFNPROPSHEETCALLBACK;
}

align(4):

struct PROPSHEETPAGEA {
    DWORD dwSize = PROPSHEETPAGEA.sizeof;
    DWORD dwFlags;
    HINSTANCE hInstance;
    union {
        LPCSTR         pszTemplate;
        LPCDLGTEMPLATE pResource;
    }
    union {
        HICON  hIcon;
        LPCSTR pszIcon;
    }
    LPCSTR  pszTitle;
    DLGPROC pfnDlgProc;
    LPARAM  lParam;
    LPFNPSPCALLBACKA pfnCallback;
    UINT*     pcRefParent;
    static if (_WIN32_IE >= 0x400) {
        LPCSTR pszHeaderTitle;
        LPCSTR pszHeaderSubTitle;
    }
}
alias PROPSHEETPAGEA*        LPPROPSHEETPAGEA;
alias const(PROPSHEETPAGEA)* LPCPROPSHEETPAGEA;

struct PROPSHEETPAGEW {
    DWORD     dwSize = PROPSHEETPAGEW.sizeof;
    DWORD     dwFlags;
    HINSTANCE hInstance;
    union {
        LPCWSTR        pszTemplate;
        LPCDLGTEMPLATE pResource;
    }
    union {
        HICON   hIcon;
        LPCWSTR pszIcon;
    }
    LPCWSTR   pszTitle;
    DLGPROC   pfnDlgProc;
    LPARAM    lParam;
    LPFNPSPCALLBACKW pfnCallback;
    UINT*     pcRefParent;
    static if (_WIN32_IE >= 0x400) {
        LPCWSTR pszHeaderTitle;
        LPCWSTR pszHeaderSubTitle;
    }
}
alias PROPSHEETPAGEW*        LPPROPSHEETPAGEW;
alias const(PROPSHEETPAGEW)* LPCPROPSHEETPAGEW;

mixin DECLARE_HANDLE!("HPROPSHEETPAGE");

struct PROPSHEETHEADERA {
    DWORD dwSize = PROPSHEETHEADERA.sizeof;
    DWORD dwFlags;
    HWND  hwndParent;
    HINSTANCE hInstance;
    union {
        HICON   hIcon;
        LPCSTR  pszIcon;
    }
    LPCSTR pszCaption;
    UINT   nPages;
    union {
        UINT   nStartPage;
        LPCSTR pStartPage;
    }
    union {
        LPCPROPSHEETPAGEA ppsp;
        HPROPSHEETPAGE*   phpage;
    }
    PFNPROPSHEETCALLBACK pfnCallback;
    static if (_WIN32_IE >= 0x400) {
        union {
            HBITMAP hbmWatermark;
            LPCSTR  pszbmWatermark;
        }
        HPALETTE hplWatermark;
        union {
            HBITMAP hbmHeader;
            LPCSTR  pszbmHeader;
        }
    }
}
alias PROPSHEETHEADERA*        LPPROPSHEETHEADERA;
alias const(PROPSHEETHEADERA)* LPCPROPSHEETHEADERA;

struct PROPSHEETHEADERW {
    DWORD     dwSize = PROPSHEETHEADERW.sizeof;
    DWORD     dwFlags;
    HWND      hwndParent;
    HINSTANCE hInstance;
    union {
        HICON   hIcon;
        LPCWSTR pszIcon;
    }
    LPCWSTR   pszCaption;
    UINT      nPages;
    union {
        UINT    nStartPage;
        LPCWSTR pStartPage;
    }
    union {
        LPCPROPSHEETPAGEW ppsp;
        HPROPSHEETPAGE*   phpage;
    }
    PFNPROPSHEETCALLBACK pfnCallback;
    static if (_WIN32_IE >= 0x400) {
        union {
            HBITMAP hbmWatermark;
            LPCWSTR pszbmWatermark;
        }
        HPALETTE hplWatermark;
        union {
            HBITMAP hbmHeader;
            LPCWSTR pszbmHeader;
        }
    }
}
alias PROPSHEETHEADERW*        LPPROPSHEETHEADERW;
alias const(PROPSHEETHEADERW)* LPCPROPSHEETHEADERW;

extern (Windows) {
    alias BOOL function(HPROPSHEETPAGE, LPARAM) LPFNADDPROPSHEETPAGE;
    alias BOOL function(LPVOID, LPFNADDPROPSHEETPAGE, LPARAM)
      LPFNADDPROPSHEETPAGES;
}

struct PSHNOTIFY {
    NMHDR  hdr;
    LPARAM lParam;
}
alias PSHNOTIFY* LPPSHNOTIFY;

extern (Windows) {
    HPROPSHEETPAGE CreatePropertySheetPageA(LPCPROPSHEETPAGEA);
    HPROPSHEETPAGE CreatePropertySheetPageW(LPCPROPSHEETPAGEW);
    BOOL DestroyPropertySheetPage(HPROPSHEETPAGE);
    INT_PTR PropertySheetA(LPCPROPSHEETHEADERA);
    INT_PTR PropertySheetW(LPCPROPSHEETHEADERW);
}

version (Unicode) {
    alias LPFNPSPCALLBACKW         LPFNPSPCALLBACK;
    alias PROPSHEETPAGEW           PROPSHEETPAGE;
    alias LPPROPSHEETPAGEW         LPPROPSHEETPAGE;
    alias LPCPROPSHEETPAGEW        LPCPROPSHEETPAGE;
    alias PROPSHEETHEADERW         PROPSHEETHEADER;
    alias LPPROPSHEETHEADERW       LPPROPSHEETHEADER;
    alias LPCPROPSHEETHEADERW      LPCPROPSHEETHEADER;
    alias PSM_SETTITLEW            PSM_SETTITLE;
    alias PSM_SETFINISHTEXTW       PSM_SETFINISHTEXT;
    alias CreatePropertySheetPageW CreatePropertySheetPage;
    alias PropertySheetW           PropertySheet;
} else {
    alias LPFNPSPCALLBACKA         LPFNPSPCALLBACK;
    alias PROPSHEETPAGEA           PROPSHEETPAGE;
    alias LPPROPSHEETPAGEA         LPPROPSHEETPAGE;
    alias LPCPROPSHEETPAGEA        LPCPROPSHEETPAGE;
    alias PROPSHEETHEADERA         PROPSHEETHEADER;
    alias LPPROPSHEETHEADERA       LPPROPSHEETHEADER;
    alias LPCPROPSHEETHEADERA      LPCPROPSHEETHEADER;
    alias PSM_SETTITLEA            PSM_SETTITLE;
    alias PSM_SETFINISHTEXTA       PSM_SETFINISHTEXT;
    alias CreatePropertySheetPageA CreatePropertySheetPage;
    alias PropertySheetA           PropertySheet;
}

BOOL PropSheet_SetCurSel(HWND hPropSheetDlg, HPROPSHEETPAGE hpage,
      HPROPSHEETPAGE index) {
    return cast(BOOL) SendMessage(hPropSheetDlg, PSM_SETCURSEL,
      cast(WPARAM) index, cast(LPARAM) hpage);
}

VOID PropSheet_RemovePage(HWND hPropSheetDlg, int index, HPROPSHEETPAGE hpage) {
    SendMessage(hPropSheetDlg, PSM_REMOVEPAGE, index, cast(LPARAM) hpage);
}

BOOL PropSheet_AddPage(HWND hPropSheetDlg, HPROPSHEETPAGE hpage) {
        return cast(BOOL) SendMessage(hPropSheetDlg, PSM_ADDPAGE,
          0, cast(LPARAM) hpage);
}

VOID PropSheet_Changed(HWND hPropSheetDlg, HWND hwndPage) {
    SendMessage(hPropSheetDlg, PSM_CHANGED, cast(WPARAM) hwndPage, 0);
}

VOID PropSheet_RestartWindows(HWND hPropSheetDlg) {
    SendMessage(hPropSheetDlg, PSM_RESTARTWINDOWS, 0, 0);
}

VOID PropSheet_RebootSystem(HWND hPropSheetDlg) {
    SendMessage(hPropSheetDlg, PSM_REBOOTSYSTEM, 0, 0);
}

VOID PropSheet_CancelToClose(HWND hPropSheetDlg) {
    SendMessage(hPropSheetDlg, PSM_CANCELTOCLOSE, 0, 0);
}

int PropSheet_QuerySiblings(HWND hPropSheetDlg, WPARAM param1, LPARAM param2) {
    return cast(int) SendMessage(hPropSheetDlg, PSM_QUERYSIBLINGS, param1, param2);
}

VOID PropSheet_UnChanged(HWND hPropSheetDlg, HWND hwndPage) {
    SendMessage(hPropSheetDlg, PSM_UNCHANGED, cast(WPARAM) hwndPage, 0);
}

BOOL PropSheet_Apply(HWND hPropSheetDlg) {
    return cast(BOOL) SendMessage(hPropSheetDlg, PSM_APPLY, 0, 0);
}

VOID PropSheet_SetTitle(HWND hPropSheetDlg, DWORD wStyle, LPTSTR lpszText) {
    SendMessage(hPropSheetDlg, PSM_SETTITLE, wStyle, cast(LPARAM) lpszText);
}

VOID PropSheet_SetWizButtons(HWND hPropSheetDlg, DWORD dwFlags) {
    PostMessage(hPropSheetDlg, PSM_SETWIZBUTTONS, 0, cast(LPARAM) dwFlags);
}

BOOL PropSheet_PressButton(HWND hPropSheetDlg, int iButton) {
    return cast(BOOL) SendMessage(hPropSheetDlg, PSM_PRESSBUTTON, iButton, 0);
}

BOOL PropSheet_SetCurSelByID(HWND hPropSheetDlg, int id) {
    return cast(BOOL) SendMessage(hPropSheetDlg, PSM_SETCURSELID, 0, id);
}

VOID PropSheet_SetFinishText(HWND hPropSheetDlg, LPTSTR lpszText) {
    SendMessage(hPropSheetDlg, PSM_SETFINISHTEXT, 0, cast(LPARAM) lpszText);
}

HWND PropSheet_GetTabControl(HWND hPropSheetDlg) {
    return cast(HWND) SendMessage(hPropSheetDlg, PSM_GETTABCONTROL, 0, 0);
}

BOOL PropSheet_IsDialogMessage(HWND hDlg, LPMSG pMsg) {
    return cast(BOOL) SendMessage(hDlg, PSM_ISDIALOGMESSAGE,
      0, cast(LPARAM) pMsg);
}

HWND PropSheet_GetCurrentPageHwnd(HWND hDlg) {
    return cast(HWND) SendMessage(hDlg, PSM_GETCURRENTPAGEHWND, 0, 0);
}

BOOL PropSheet_InsertPage(HWND hPropSheetDlg, WPARAM wInsertAfter,
      HPROPSHEETPAGE hpage) {
    return cast(BOOL) SendMessage(hPropSheetDlg, PSM_INSERTPAGE,
      wInsertAfter, cast(LPARAM) hpage);
}

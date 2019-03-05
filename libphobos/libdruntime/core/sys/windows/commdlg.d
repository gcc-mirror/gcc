/**
 * Windows API header module
 *
 * Translated from MinGW API for MS-Windows 3.12
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_commdlg.d)
 */
module core.sys.windows.commdlg;
version (Windows):

version (ANSI) {} else version = Unicode;
pragma(lib, "comdlg32");

private import core.sys.windows.w32api;
import core.sys.windows.windef, core.sys.windows.winuser;
import core.sys.windows.wingdi; // for LPLOGFONTA

const TCHAR[]
    LBSELCHSTRING = "commdlg_LBSelChangedNotify",
    SHAREVISTRING = "commdlg_ShareViolation",
    FILEOKSTRING  = "commdlg_FileNameOK",
    COLOROKSTRING = "commdlg_ColorOK",
    SETRGBSTRING  = "commdlg_SetRGBColor",
    HELPMSGSTRING = "commdlg_help",
    FINDMSGSTRING = "commdlg_FindReplace";

enum : UINT {
    CDN_FIRST          = -601, // also in commctrl.h
    CDN_LAST           = -699,
    CDN_INITDONE       = CDN_FIRST,
    CDN_SELCHANGE      = CDN_FIRST - 1,
    CDN_FOLDERCHANGE   = CDN_FIRST - 2,
    CDN_SHAREVIOLATION = CDN_FIRST - 3,
    CDN_HELP           = CDN_FIRST - 4,
    CDN_FILEOK         = CDN_FIRST - 5,
    CDN_TYPECHANGE     = CDN_FIRST - 6,
}

//static if (_WIN32_WINNT >= 0x500) {
    enum : UINT {
        CDN_INCLUDEITEM    = CDN_FIRST - 7,
    }
//}

enum : UINT {
    CDM_FIRST           = WM_USER + 100,
    CDM_LAST            = WM_USER + 200,
    CDM_GETSPEC         = CDM_FIRST,
    CDM_GETFILEPATH,
    CDM_GETFOLDERPATH,
    CDM_GETFOLDERIDLIST,
    CDM_SETCONTROLTEXT,
    CDM_HIDECONTROL,
    CDM_SETDEFEXT    // = CDM_FIRST + 6
}

// flags for ChooseColor
enum : DWORD {
    CC_RGBINIT              = 0x0001,
    CC_FULLOPEN             = 0x0002,
    CC_PREVENTFULLOPEN      = 0x0004,
    CC_SHOWHELP             = 0x0008,
    CC_ENABLEHOOK           = 0x0010,
    CC_ENABLETEMPLATE       = 0x0020,
    CC_ENABLETEMPLATEHANDLE = 0x0040,
    CC_SOLIDCOLOR           = 0x0080,
    CC_ANYCOLOR             = 0x0100
}

// flags for ChooseFont
enum : DWORD {
    CF_SCREENFONTS          = 0x00000001,
    CF_PRINTERFONTS         = 0x00000002,
    CF_BOTH                 = 0x00000003,
    CF_SHOWHELP             = 0x00000004,
    CF_ENABLEHOOK           = 0x00000008,
    CF_ENABLETEMPLATE       = 0x00000010,
    CF_ENABLETEMPLATEHANDLE = 0x00000020,
    CF_INITTOLOGFONTSTRUCT  = 0x00000040,
    CF_USESTYLE             = 0x00000080,
    CF_EFFECTS              = 0x00000100,
    CF_APPLY                = 0x00000200,
    CF_ANSIONLY             = 0x00000400,
    CF_SCRIPTSONLY          = CF_ANSIONLY,
    CF_NOVECTORFONTS        = 0x00000800,
    CF_NOOEMFONTS           = 0x00000800,
    CF_NOSIMULATIONS        = 0x00001000,
    CF_LIMITSIZE            = 0x00002000,
    CF_FIXEDPITCHONLY       = 0x00004000,
    CF_WYSIWYG              = 0x00008000,
    CF_FORCEFONTEXIST       = 0x00010000,
    CF_SCALABLEONLY         = 0x00020000,
    CF_TTONLY               = 0x00040000,
    CF_NOFACESEL            = 0x00080000,
    CF_NOSTYLESEL           = 0x00100000,
    CF_NOSIZESEL            = 0x00200000,
    CF_SELECTSCRIPT         = 0x00400000,
    CF_NOSCRIPTSEL          = 0x00800000,
    CF_NOVERTFONTS          = 0x01000000
}

// Font type for ChooseFont
enum : WORD {
    BOLD_FONTTYPE      = 0x0100,
    ITALIC_FONTTYPE    = 0x0200,
    REGULAR_FONTTYPE   = 0x0400,
    SCREEN_FONTTYPE    = 0x2000,
    PRINTER_FONTTYPE   = 0x4000,
    SIMULATED_FONTTYPE = 0x8000
}

enum : UINT {
    WM_CHOOSEFONT_GETLOGFONT = WM_USER +   1,
    WM_CHOOSEFONT_SETLOGFONT = WM_USER + 101,
    WM_CHOOSEFONT_SETFLAGS   = WM_USER + 102
}

// flags for OpenFileName
enum : DWORD {
    OFN_SHAREWARN            = 0,
    OFN_SHARENOWARN          = 0x000001,
    OFN_READONLY             = 0x000001,
    OFN_SHAREFALLTHROUGH     = 0x000002,
    OFN_OVERWRITEPROMPT      = 0x000002,
    OFN_HIDEREADONLY         = 0x000004,
    OFN_NOCHANGEDIR          = 0x000008,
    OFN_SHOWHELP             = 0x000010,
    OFN_ENABLEHOOK           = 0x000020,
    OFN_ENABLETEMPLATE       = 0x000040,
    OFN_ENABLETEMPLATEHANDLE = 0x000080,
    OFN_NOVALIDATE           = 0x000100,
    OFN_ALLOWMULTISELECT     = 0x000200,
    OFN_EXTENSIONDIFFERENT   = 0x000400,
    OFN_PATHMUSTEXIST        = 0x000800,
    OFN_FILEMUSTEXIST        = 0x001000,
    OFN_CREATEPROMPT         = 0x002000,
    OFN_SHAREAWARE           = 0x004000,
    OFN_NOREADONLYRETURN     = 0x008000,
    OFN_NOTESTFILECREATE     = 0x010000,
    OFN_NONETWORKBUTTON      = 0x020000,
    OFN_NOLONGNAMES          = 0x040000,
    OFN_EXPLORER             = 0x080000,
    OFN_NODEREFERENCELINKS   = 0x100000,
    OFN_LONGNAMES            = 0x200000,
    OFN_ENABLESIZING         = 0x800000
}

enum : DWORD {
    FR_DOWN                 = 0x00000001,
    FR_WHOLEWORD            = 0x00000002,
    FR_MATCHCASE            = 0x00000004,
    FR_FINDNEXT             = 0x00000008,
    FR_REPLACE              = 0x00000010,
    FR_REPLACEALL           = 0x00000020,
    FR_DIALOGTERM           = 0x00000040,
    FR_SHOWHELP             = 0x00000080,
    FR_ENABLEHOOK           = 0x00000100,
    FR_ENABLETEMPLATE       = 0x00000200,
    FR_NOUPDOWN             = 0x00000400,
    FR_NOMATCHCASE          = 0x00000800,
    FR_NOWHOLEWORD          = 0x00001000,
    FR_ENABLETEMPLATEHANDLE = 0x00002000,
    FR_HIDEUPDOWN           = 0x00004000,
    FR_HIDEMATCHCASE        = 0x00008000,
    FR_HIDEWHOLEWORD        = 0x00010000,
    FR_MATCHDIAC            = 0x20000000,
    FR_MATCHKASHIDA         = 0x40000000,
    FR_MATCHALEFHAMZA       = 0x80000000
}

enum : DWORD {
    PD_ALLPAGES                   = 0,
    PD_SELECTION                  = 0x000001,
    PD_PAGENUMS                   = 0x000002,
    PD_NOSELECTION                = 0x000004,
    PD_NOPAGENUMS                 = 0x000008,
    PD_COLLATE                    = 0x000010,
    PD_PRINTTOFILE                = 0x000020,
    PD_PRINTSETUP                 = 0x000040,
    PD_NOWARNING                  = 0x000080,
    PD_RETURNDC                   = 0x000100,
    PD_RETURNIC                   = 0x000200,
    PD_RETURNDEFAULT              = 0x000400,
    PD_SHOWHELP                   = 0x000800,
    PD_ENABLEPRINTHOOK            = 0x001000,
    PD_ENABLESETUPHOOK            = 0x002000,
    PD_ENABLEPRINTTEMPLATE        = 0x004000,
    PD_ENABLESETUPTEMPLATE        = 0x008000,
    PD_ENABLEPRINTTEMPLATEHANDLE  = 0x010000,
    PD_ENABLESETUPTEMPLATEHANDLE  = 0x020000,
    PD_USEDEVMODECOPIES           = 0x040000,
    PD_USEDEVMODECOPIESANDCOLLATE = 0x040000,
    PD_DISABLEPRINTTOFILE         = 0x080000,
    PD_HIDEPRINTTOFILE            = 0x100000,
    PD_NONETWORKBUTTON            = 0x200000
}

//static if (_WIN32_WINNT >= 0x500) {
    enum : DWORD {
        PD_CURRENTPAGE      = 0x00400000,
        PD_NOCURRENTPAGE    = 0x00800000,
        PD_EXCLUSIONFLAGS   = 0x01000000,
        PD_USELARGETEMPLATE = 0x10000000,
    }

    enum : HRESULT {
        PD_RESULT_CANCEL,
        PD_RESULT_PRINT,
        PD_RESULT_APPLY
    }

enum DWORD START_PAGE_GENERAL = 0xFFFFFFFF;
//}

enum {
    PSD_DEFAULTMINMARGINS             = 0,
    PSD_INWININIINTLMEASURE           = 0,
    PSD_MINMARGINS                    = 0x000001,
    PSD_MARGINS                       = 0x000002,
    PSD_INTHOUSANDTHSOFINCHES         = 0x000004,
    PSD_INHUNDREDTHSOFMILLIMETERS     = 0x000008,
    PSD_DISABLEMARGINS                = 0x000010,
    PSD_DISABLEPRINTER                = 0x000020,
    PSD_NOWARNING                     = 0x000080,
    PSD_DISABLEORIENTATION            = 0x000100,
    PSD_DISABLEPAPER                  = 0x000200,
    PSD_RETURNDEFAULT                 = 0x000400,
    PSD_SHOWHELP                      = 0x000800,
    PSD_ENABLEPAGESETUPHOOK           = 0x002000,
    PSD_ENABLEPAGESETUPTEMPLATE       = 0x008000,
    PSD_ENABLEPAGESETUPTEMPLATEHANDLE = 0x020000,
    PSD_ENABLEPAGEPAINTHOOK           = 0x040000,
    PSD_DISABLEPAGEPAINTING           = 0x080000
}

enum : UINT {
    WM_PSD_PAGESETUPDLG = WM_USER,
    WM_PSD_FULLPAGERECT,
    WM_PSD_MINMARGINRECT,
    WM_PSD_MARGINRECT,
    WM_PSD_GREEKTEXTRECT,
    WM_PSD_ENVSTAMPRECT,
    WM_PSD_YAFULLPAGERECT // = WM_USER + 6
}

enum : int {
    CD_LBSELNOITEMS = -1,
    CD_LBSELCHANGE,
    CD_LBSELSUB,
    CD_LBSELADD
}

enum WORD DN_DEFAULTPRN = 1;

/+
// Both MinGW and the windows docs indicate that there are macros for the send messages
// the controls. These seem to be totally unnecessary -- and at least one of MinGW or
// Windows Docs is buggy!

int CommDlg_OpenSave_GetSpec(HWND hWndControl, LPARAM lparam, WPARAM wParam) {
    return SendMessage(hWndControl, CDM_GETSPEC, wParam, lParam);
}

int CommDlg_OpenSave_GetFilePath(HWND hWndControl, LPARAM lparam, WPARAM wParam) {
    return SendMessage(hWndControl, CDM_GETFILEPATH, wParam, lParam);
}

int CommDlg_OpenSave_GetFolderPath(HWND hWndControl, LPARAM lparam, WPARAM wParam) {
    return SendMessage(hWndControl, CDM_GETFOLDERPATH, wParam, lParam);
}

int CommDlg_OpenSave_GetFolderIDList(HWND hWndControl, LPARAM lparam, WPARAM wParam) {
    return SendMessage(hWndControl, CDM_GETFOLDERIDLIST, wParam, lParam);
}

void CommDlg_OpenSave_SetControlText(HWND hWndControl, LPARAM lparam, WPARAM wParam) {
    return SendMessage(hWndControl, CDM_SETCONTROLTEXT, wParam, lParam);
}

void CommDlg_OpenSave_HideControl(HWND hWndControl, WPARAM wParam) {
    return SendMessage(hWndControl, CDM_HIDECONTROL, wParam, 0);
}

void CommDlg_OpenSave_SetDefExt(HWND hWndControl, TCHAR* lparam) {
    return SendMessage(hWndControl, CDM_SETCONTROLTEXT, 0, cast(LPARAM)lParam);
}

// These aliases seem even more unnecessary
alias CommDlg_OpenSave_GetSpec
    CommDlg_OpenSave_GetSpecA, CommDlg_OpenSave_GetSpecW;
alias CommDlg_OpenSave_GetFilePath
    CommDlg_OpenSave_GetFilePathA, CommDlg_OpenSave_GetFilePathW;
alias CommDlg_OpenSave_GetFolderPath
    CommDlg_OpenSave_GetFolderPathA, CommDlg_OpenSave_GetFolderPathW;
+/

// Callbacks.
extern(Windows) {
alias UINT_PTR function (HWND, UINT, WPARAM, LPARAM) nothrow
    LPCCHOOKPROC, LPCFHOOKPROC, LPFRHOOKPROC, LPOFNHOOKPROC,
    LPPAGEPAINTHOOK, LPPAGESETUPHOOK, LPSETUPHOOKPROC, LPPRINTHOOKPROC;
}

//align (1): // 1 in Win32, default in Win64

struct CHOOSECOLORA {
    DWORD        lStructSize = CHOOSECOLORA.sizeof;
    HWND         hwndOwner;
    HWND         hInstance;
    COLORREF     rgbResult;
    COLORREF*    lpCustColors;
    DWORD        Flags;
    LPARAM       lCustData;
    LPCCHOOKPROC lpfnHook;
    LPCSTR       lpTemplateName;
}
alias CHOOSECOLORA* LPCHOOSECOLORA;

struct CHOOSECOLORW {
    DWORD        lStructSize = CHOOSECOLORW.sizeof;
    HWND         hwndOwner;
    HWND         hInstance;
    COLORREF     rgbResult;
    COLORREF*    lpCustColors;
    DWORD        Flags;
    LPARAM       lCustData;
    LPCCHOOKPROC lpfnHook;
    LPCWSTR      lpTemplateName;
}
alias CHOOSECOLORW* LPCHOOSECOLORW;

struct CHOOSEFONTA {
    DWORD        lStructSize = CHOOSEFONTA.sizeof;
    HWND         hwndOwner;
    HDC          hDC;
    LPLOGFONTA   lpLogFont;
    INT          iPointSize;
    DWORD        Flags;
    DWORD        rgbColors;
    LPARAM       lCustData;
    LPCFHOOKPROC lpfnHook;
    LPCSTR       lpTemplateName;
    HINSTANCE    hInstance;
    LPSTR        lpszStyle;
    WORD         nFontType;
    WORD         ___MISSING_ALIGNMENT__;
    INT          nSizeMin;
    INT          nSizeMax;
}
alias CHOOSEFONTA* LPCHOOSEFONTA;

struct CHOOSEFONTW {
    DWORD        lStructSize = CHOOSEFONTW.sizeof;
    HWND         hwndOwner;
    HDC          hDC;
    LPLOGFONTW   lpLogFont;
    INT          iPointSize;
    DWORD        Flags;
    DWORD        rgbColors;
    LPARAM       lCustData;
    LPCFHOOKPROC lpfnHook;
    LPCWSTR      lpTemplateName;
    HINSTANCE    hInstance;
    LPWSTR       lpszStyle;
    WORD         nFontType;
    WORD         ___MISSING_ALIGNMENT__;
    INT          nSizeMin;
    INT          nSizeMax;
}
alias CHOOSEFONTW* LPCHOOSEFONTW;

struct DEVNAMES {
    WORD wDriverOffset;
    WORD wDeviceOffset;
    WORD wOutputOffset;
    WORD wDefault;
}
alias DEVNAMES* LPDEVNAMES;

struct FINDREPLACEA {
    DWORD        lStructSize = FINDREPLACEA.sizeof;
    HWND         hwndOwner;
    HINSTANCE    hInstance;
    DWORD        Flags;
    LPSTR        lpstrFindWhat;
    LPSTR        lpstrReplaceWith;
    WORD         wFindWhatLen;
    WORD         wReplaceWithLen;
    LPARAM       lCustData;
    LPFRHOOKPROC lpfnHook;
    LPCSTR       lpTemplateName;
}
alias FINDREPLACEA* LPFINDREPLACEA;

struct FINDREPLACEW {
    DWORD        lStructSize = FINDREPLACEW.sizeof;
    HWND         hwndOwner;
    HINSTANCE    hInstance;
    DWORD        Flags;
    LPWSTR       lpstrFindWhat;
    LPWSTR       lpstrReplaceWith;
    WORD         wFindWhatLen;
    WORD         wReplaceWithLen;
    LPARAM       lCustData;
    LPFRHOOKPROC lpfnHook;
    LPCWSTR      lpTemplateName;
}
alias FINDREPLACEW* LPFINDREPLACEW;

struct OPENFILENAMEA {
    DWORD         lStructSize = OPENFILENAMEA.sizeof;
    HWND          hwndOwner;
    HINSTANCE     hInstance;
    LPCSTR        lpstrFilter;
    LPSTR         lpstrCustomFilter;
    DWORD         nMaxCustFilter;
    DWORD         nFilterIndex;
    LPSTR         lpstrFile;
    DWORD         nMaxFile;
    LPSTR         lpstrFileTitle;
    DWORD         nMaxFileTitle;
    LPCSTR        lpstrInitialDir;
    LPCSTR        lpstrTitle;
    DWORD         Flags;
    WORD          nFileOffset;
    WORD          nFileExtension;
    LPCSTR        lpstrDefExt;
    LPARAM        lCustData;
    LPOFNHOOKPROC lpfnHook;
    LPCSTR        lpTemplateName;

    //static if (_WIN32_WINNT >= 0x500) {
        void          *pvReserved;
        DWORD         dwReserved;
        DWORD         FlagsEx;
    //}
}
alias OPENFILENAMEA* LPOPENFILENAMEA;

struct OPENFILENAMEW {
    DWORD         lStructSize = OPENFILENAMEW.sizeof;
    HWND          hwndOwner;
    HINSTANCE     hInstance;
    LPCWSTR       lpstrFilter;
    LPWSTR        lpstrCustomFilter;
    DWORD         nMaxCustFilter;
    DWORD         nFilterIndex;
    LPWSTR        lpstrFile;
    DWORD         nMaxFile;
    LPWSTR        lpstrFileTitle;
    DWORD         nMaxFileTitle;
    LPCWSTR       lpstrInitialDir;
    LPCWSTR       lpstrTitle;
    DWORD         Flags;
    WORD          nFileOffset;
    WORD          nFileExtension;
    LPCWSTR       lpstrDefExt;
    LPARAM        lCustData;
    LPOFNHOOKPROC lpfnHook;
    LPCWSTR       lpTemplateName;

    //static if (_WIN32_WINNT >= 0x500) {
        void          *pvReserved;
        DWORD         dwReserved;
        DWORD         FlagsEx;
    //}
}
alias OPENFILENAMEW* LPOPENFILENAMEW;

enum size_t OPENFILENAME_SIZE_VERSION_400 = 76;

struct OFNOTIFYA {
    NMHDR           hdr;
    LPOPENFILENAMEA lpOFN;
    LPSTR           pszFile;
}
alias OFNOTIFYA* LPOFNOTIFYA;

struct OFNOTIFYW {
    NMHDR           hdr;
    LPOPENFILENAMEW lpOFN;
    LPWSTR          pszFile;
}
alias OFNOTIFYW* LPOFNOTIFYW;

struct PAGESETUPDLGA {
    DWORD           lStructSize = PAGESETUPDLGA.sizeof;
    HWND            hwndOwner;
    HGLOBAL         hDevMode;
    HGLOBAL         hDevNames;
    DWORD           Flags;
    POINT           ptPaperSize;
    RECT            rtMinMargin;
    RECT            rtMargin;
    HINSTANCE       hInstance;
    LPARAM          lCustData;
    LPPAGESETUPHOOK lpfnPageSetupHook;
    LPPAGEPAINTHOOK lpfnPagePaintHook;
    LPCSTR          lpPageSetupTemplateName;
    HGLOBAL         hPageSetupTemplate;
}
alias PAGESETUPDLGA* LPPAGESETUPDLGA;

struct PAGESETUPDLGW {
    DWORD           lStructSize = PAGESETUPDLGW.sizeof;
    HWND            hwndOwner;
    HGLOBAL         hDevMode;
    HGLOBAL         hDevNames;
    DWORD           Flags;
    POINT           ptPaperSize;
    RECT            rtMinMargin;
    RECT            rtMargin;
    HINSTANCE       hInstance;
    LPARAM          lCustData;
    LPPAGESETUPHOOK lpfnPageSetupHook;
    LPPAGEPAINTHOOK lpfnPagePaintHook;
    LPCWSTR         lpPageSetupTemplateName;
    HGLOBAL         hPageSetupTemplate;
}
alias PAGESETUPDLGW* LPPAGESETUPDLGW;

align (1) struct PRINTDLGA {
align(1):
    DWORD           lStructSize = PRINTDLGA.sizeof;
    version (Win64)
        DWORD       padding1;
    HWND            hwndOwner;
    HANDLE          hDevMode;
    HANDLE          hDevNames;
    HDC             hDC;
    DWORD           Flags;
    WORD            nFromPage;
    WORD            nToPage;
    WORD            nMinPage;
    WORD            nMaxPage;
    WORD            nCopies;
    version (Win64)
        WORD        padding2;
    HINSTANCE       hInstance;
    LPARAM          lCustData;
    LPPRINTHOOKPROC lpfnPrintHook;
    LPSETUPHOOKPROC lpfnSetupHook;
    LPCSTR          lpPrintTemplateName;
    LPCSTR          lpSetupTemplateName;
    HANDLE          hPrintTemplate;
    HANDLE          hSetupTemplate;
}
alias PRINTDLGA* LPPRINTDLGA;

align (1) struct PRINTDLGW {
align(1):
    DWORD           lStructSize = PRINTDLGW.sizeof;
    version (Win64)
        DWORD       padding1;
    HWND            hwndOwner;
    HANDLE          hDevMode;
    HANDLE          hDevNames;
    HDC             hDC;
    DWORD           Flags;
    WORD            nFromPage;
    WORD            nToPage;
    WORD            nMinPage;
    WORD            nMaxPage;
    WORD            nCopies;
    version (Win64)
        WORD        padding2;
    HINSTANCE       hInstance;
    LPARAM          lCustData;
    LPPRINTHOOKPROC lpfnPrintHook;
    LPSETUPHOOKPROC lpfnSetupHook;
    LPCWSTR         lpPrintTemplateName;
    LPCWSTR         lpSetupTemplateName;
    HANDLE          hPrintTemplate;
    HANDLE          hSetupTemplate;
}
alias PRINTDLGW* LPPRINTDLGW;

//static if (_WIN32_WINNT >= 0x500) {
    import core.sys.windows.unknwn; // for LPUNKNOWN
    import core.sys.windows.prsht;  // for HPROPSHEETPAGE

    struct PRINTPAGERANGE {
        DWORD  nFromPage;
        DWORD  nToPage;
    }
    alias PRINTPAGERANGE* LPPRINTPAGERANGE;

    struct PRINTDLGEXA {
        DWORD            lStructSize = PRINTDLGEXA.sizeof;
        HWND             hwndOwner;
        HGLOBAL          hDevMode;
        HGLOBAL          hDevNames;
        HDC              hDC;
        DWORD            Flags;
        DWORD            Flags2;
        DWORD            ExclusionFlags;
        DWORD            nPageRanges;
        DWORD            nMaxPageRanges;
        LPPRINTPAGERANGE lpPageRanges;
        DWORD            nMinPage;
        DWORD            nMaxPage;
        DWORD            nCopies;
        HINSTANCE        hInstance;
        LPCSTR           lpPrintTemplateName;
        LPUNKNOWN        lpCallback;
        DWORD            nPropertyPages;
        HPROPSHEETPAGE*  lphPropertyPages;
        DWORD            nStartPage;
        DWORD            dwResultAction;
    }
    alias PRINTDLGEXA* LPPRINTDLGEXA;

    struct PRINTDLGEXW {
        DWORD            lStructSize = PRINTDLGEXW.sizeof;
        HWND             hwndOwner;
        HGLOBAL          hDevMode;
        HGLOBAL          hDevNames;
        HDC              hDC;
        DWORD            Flags;
        DWORD            Flags2;
        DWORD            ExclusionFlags;
        DWORD            nPageRanges;
        DWORD            nMaxPageRanges;
        LPPRINTPAGERANGE lpPageRanges;
        DWORD            nMinPage;
        DWORD            nMaxPage;
        DWORD            nCopies;
        HINSTANCE        hInstance;
        LPCWSTR          lpPrintTemplateName;
        LPUNKNOWN        lpCallback;
        DWORD            nPropertyPages;
        HPROPSHEETPAGE*  lphPropertyPages;
        DWORD            nStartPage;
        DWORD            dwResultAction;
    }
    alias PRINTDLGEXW* LPPRINTDLGEXW;

//} // _WIN32_WINNT >= 0x500

extern (Windows) nothrow @nogc {
    BOOL ChooseColorA(LPCHOOSECOLORA);
    BOOL ChooseColorW(LPCHOOSECOLORW);
    BOOL ChooseFontA(LPCHOOSEFONTA);
    BOOL ChooseFontW(LPCHOOSEFONTW);
    DWORD CommDlgExtendedError();
    HWND FindTextA(LPFINDREPLACEA);
    HWND FindTextW(LPFINDREPLACEW);
    short GetFileTitleA(LPCSTR, LPSTR, WORD);
    short GetFileTitleW(LPCWSTR, LPWSTR, WORD);
    BOOL GetOpenFileNameA(LPOPENFILENAMEA);
    BOOL GetOpenFileNameW(LPOPENFILENAMEW);
    BOOL GetSaveFileNameA(LPOPENFILENAMEA);
    BOOL GetSaveFileNameW(LPOPENFILENAMEW);
    BOOL PageSetupDlgA(LPPAGESETUPDLGA);
    BOOL PageSetupDlgW(LPPAGESETUPDLGW);
    BOOL PrintDlgA(LPPRINTDLGA);
    BOOL PrintDlgW(LPPRINTDLGW);
    HWND ReplaceTextA(LPFINDREPLACEA);
    HWND ReplaceTextW(LPFINDREPLACEW);

    //static if (_WIN32_WINNT >= 0x500) {
        HRESULT PrintDlgExA(LPPRINTDLGEXA);
        HRESULT PrintDlgExW(LPPRINTDLGEXW);
    //}
}

version (Unicode) {
    alias CHOOSECOLORW CHOOSECOLOR;
    alias CHOOSEFONTW CHOOSEFONT;
    alias FINDREPLACEW FINDREPLACE;
    alias OPENFILENAMEW OPENFILENAME;
    alias OFNOTIFYW OFNOTIFY;
    alias PAGESETUPDLGW PAGESETUPDLG;
    alias PRINTDLGW PRINTDLG;

    alias ChooseColorW ChooseColor;
    alias ChooseFontW ChooseFont;
    alias FindTextW FindText;
    alias GetFileTitleW GetFileTitle;
    alias GetOpenFileNameW GetOpenFileName;
    alias GetSaveFileNameW GetSaveFileName;
    alias PageSetupDlgW PageSetupDlg;
    alias PrintDlgW PrintDlg;
    alias ReplaceTextW ReplaceText;

    //static if (_WIN32_WINNT >= 0x500) {
        alias PRINTDLGEXW PRINTDLGEX;
        alias PrintDlgExW PrintDlgEx;
    //}

} else { // UNICODE

    alias CHOOSECOLORA CHOOSECOLOR;
    alias CHOOSEFONTA CHOOSEFONT;
    alias FINDREPLACEA FINDREPLACE;
    alias OPENFILENAMEA OPENFILENAME;
    alias OFNOTIFYA OFNOTIFY;
    alias PAGESETUPDLGA PAGESETUPDLG;
    alias PRINTDLGA PRINTDLG;

    alias ChooseColorA ChooseColor;
    alias ChooseFontA ChooseFont;
    alias FindTextA FindText;
    alias GetFileTitleA GetFileTitle;
    alias GetOpenFileNameA GetOpenFileName;
    alias GetSaveFileNameA GetSaveFileName;
    alias PageSetupDlgA PageSetupDlg;
    alias PrintDlgA PrintDlg;
    alias ReplaceTextA ReplaceText;

    //static if (_WIN32_WINNT >= 0x500) {
        alias PRINTDLGEXA PRINTDLGEX;
        alias PrintDlgExA PrintDlgEx;
    //}

} // UNICODE

alias CHOOSECOLOR* LPCHOOSECOLOR;
alias CHOOSEFONT* LPCHOOSEFONT;
alias FINDREPLACE* LPFINDREPLACE;
alias OPENFILENAME* LPOPENFILENAME;
alias OFNOTIFY* LPOFNOTIFY;
alias PAGESETUPDLG* LPPAGESETUPDLG;
alias PRINTDLG* LPPRINTDLG;
//static if (_WIN32_WINNT >= 0x500) {
    alias PRINTDLGEX* LPPRINTDLGEX;
//}

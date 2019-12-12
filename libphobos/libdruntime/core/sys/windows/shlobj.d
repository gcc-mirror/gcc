/**
 * Windows API header module
 *
 * Translated from MinGW API for MS-Windows 4.0
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_shlobj.d)
 */
module core.sys.windows.shlobj;
version (Windows):

version (ANSI) {} else version = Unicode;
pragma(lib, "shell32");

// TODO: fix bitfields
// TODO: CMIC_VALID_SEE_FLAGS
// SHGetFolderPath in shfolder.dll on W9x, NT4, also in shell32.dll on W2K

import core.sys.windows.commctrl, core.sys.windows.ole2, core.sys.windows.shlguid, core.sys.windows.shellapi;
private import core.sys.windows.prsht, core.sys.windows.unknwn, core.sys.windows.w32api, core.sys.windows.winbase,
  core.sys.windows.winnt, core.sys.windows.winuser, core.sys.windows.wtypes, core.sys.windows.objfwd, core.sys.windows.objidl;
private import core.sys.windows.winnetwk; // for NETRESOURCE
private import core.sys.windows.oaidl : VARIANT;


// FIXME: clean up Windows version support

align(1):

enum BIF_RETURNONLYFSDIRS = 1;
enum BIF_DONTGOBELOWDOMAIN = 2;
enum BIF_STATUSTEXT = 4;
enum BIF_RETURNFSANCESTORS = 8;
enum BIF_EDITBOX = 16;
enum BIF_VALIDATE = 32;
enum BIF_NEWDIALOGSTYLE = 64;
enum BIF_BROWSEINCLUDEURLS = 128;
enum BIF_USENEWUI =  BIF_EDITBOX | BIF_NEWDIALOGSTYLE;
enum BIF_BROWSEFORCOMPUTER = 0x1000;
enum BIF_BROWSEFORPRINTER = 0x2000;
enum BIF_BROWSEINCLUDEFILES = 0x4000;
enum BIF_SHAREABLE = 0x8000;
enum BFFM_INITIALIZED = 1;
enum BFFM_SELCHANGED = 2;
enum BFFM_VALIDATEFAILEDA = 3;
enum BFFM_VALIDATEFAILEDW = 4;
enum BFFM_SETSTATUSTEXTA = WM_USER + 100;
enum BFFM_ENABLEOK = WM_USER + 101;
enum BFFM_SETSELECTIONA = WM_USER + 102;
enum BFFM_SETSELECTIONW = WM_USER + 103;
enum BFFM_SETSTATUSTEXTW = WM_USER + 104;
enum BFFM_SETOKTEXT = WM_USER + 105;
enum BFFM_SETEXPANDED = WM_USER + 106;

version (Unicode) {
    alias BFFM_SETSTATUSTEXTW BFFM_SETSTATUSTEXT;
    alias BFFM_SETSELECTIONW BFFM_SETSELECTION;
    alias BFFM_VALIDATEFAILEDW BFFM_VALIDATEFAILED;
} else {
    alias BFFM_SETSTATUSTEXTA BFFM_SETSTATUSTEXT;
    alias BFFM_SETSELECTIONA BFFM_SETSELECTION;
    alias BFFM_VALIDATEFAILEDA BFFM_VALIDATEFAILED;
}

enum DVASPECT_SHORTNAME = 2;

enum SHARD {
    SHARD_PIDL            = 1,
    SHARD_PATHA,
    SHARD_PATHW,
    SHARD_APPIDINFO,
    SHARD_APPIDINFOIDLIST,
    SHARD_LINK,
    SHARD_APPIDINFOLINK,
    SHARD_SHELLITEM,   // = 8
    SHARD_PATH = (_WIN32_UNICODE ? SHARD_PATHW : SHARD_PATHA)
}

enum SHCNE_RENAMEITEM = 1;
enum SHCNE_CREATE = 2;
enum SHCNE_DELETE = 4;
enum SHCNE_MKDIR = 8;
enum SHCNE_RMDIR = 16;
enum SHCNE_MEDIAINSERTED = 32;
enum SHCNE_MEDIAREMOVED = 64;
enum SHCNE_DRIVEREMOVED = 128;
enum SHCNE_DRIVEADD = 256;
enum SHCNE_NETSHARE = 512;
enum SHCNE_NETUNSHARE = 1024;
enum SHCNE_ATTRIBUTES = 2048;
enum SHCNE_UPDATEDIR = 4096;
enum SHCNE_UPDATEITEM = 8192;
enum SHCNE_SERVERDISCONNECT = 16384;
enum SHCNE_UPDATEIMAGE = 32768;
enum SHCNE_DRIVEADDGUI = 65536;
enum SHCNE_RENAMEFOLDER = 0x20000;
enum SHCNE_FREESPACE = 0x40000;
enum SHCNE_ASSOCCHANGED = 0x8000000;
enum SHCNE_DISKEVENTS = 0x2381F;
enum SHCNE_GLOBALEVENTS = 0xC0581E0;
enum SHCNE_ALLEVENTS = 0x7FFFFFFF;
enum SHCNE_INTERRUPT = 0x80000000;

enum SHCNF_IDLIST = 0;
enum SHCNF_PATHA = 1;
enum SHCNF_PRINTERA = 2;
enum SHCNF_DWORD = 3;
enum SHCNF_PATHW = 5;
enum SHCNF_PRINTERW = 6;
enum SHCNF_TYPE = 0xFF;
enum SHCNF_FLUSH = 0x1000;
enum SHCNF_FLUSHNOWAIT = 0x2000;

version (Unicode) {
    alias SHCNF_PATHW SHCNF_PATH;
    alias SHCNF_PRINTERW SHCNF_PRINTER;
} else {
    alias SHCNF_PATHA SHCNF_PATH;
    alias SHCNF_PRINTERA SHCNF_PRINTER;
}

enum SFGAOF : DWORD {
    SFGAO_CANCOPY         = DROPEFFECT.DROPEFFECT_COPY,
    SFGAO_CANMOVE         = DROPEFFECT.DROPEFFECT_MOVE,
    SFGAO_CANLINK         = DROPEFFECT.DROPEFFECT_LINK,
    SFGAO_CANRENAME       = 0x00000010L,
    SFGAO_CANDELETE       = 0x00000020L,
    SFGAO_HASPROPSHEET    = 0x00000040L,
    SFGAO_DROPTARGET      = 0x00000100L,
    SFGAO_CAPABILITYMASK  = 0x00000177L,
    SFGAO_ISSLOW          = 0x00004000L,
    SFGAO_GHOSTED         = 0x00008000L,
    SFGAO_LINK            = 0x00010000L,
    SFGAO_SHARE           = 0x00020000L,
    SFGAO_READONLY        = 0x00040000L,
    SFGAO_HIDDEN          = 0x00080000L,
    SFGAO_DISPLAYATTRMASK = (SFGAO_ISSLOW | SFGAO_GHOSTED | SFGAO_LINK
                            | SFGAO_SHARE | SFGAO_READONLY | SFGAO_HIDDEN),
    SFGAO_FILESYSANCESTOR = 0x10000000L,
    SFGAO_FOLDER          = 0x20000000L,
    SFGAO_FILESYSTEM      = 0x40000000L,
    SFGAO_HASSUBFOLDER    = 0x80000000L,
    SFGAO_CONTENTSMASK    = 0x80000000L,
    SFGAO_VALIDATE        = 0x01000000L,
    SFGAO_REMOVABLE       = 0x02000000L,
    SFGAO_COMPRESSED      = 0x04000000L
}
enum STRRET_WSTR = 0;
enum STRRET_OFFSET = 1;
enum STRRET_CSTR = 2;

enum {
    SHGDFIL_FINDDATA = 1,
    SHGDFIL_NETRESOURCE,
    SHGDFIL_DESCRIPTIONID
}

enum {
    SHDID_ROOT_REGITEM = 1,
    SHDID_FS_FILE,
    SHDID_FS_DIRECTORY,
    SHDID_FS_OTHER,
    SHDID_COMPUTER_DRIVE35,
    SHDID_COMPUTER_DRIVE525,
    SHDID_COMPUTER_REMOVABLE,
    SHDID_COMPUTER_FIXED,
    SHDID_COMPUTER_NETDRIVE,
    SHDID_COMPUTER_CDROM,
    SHDID_COMPUTER_RAMDISK,
    SHDID_COMPUTER_OTHER,
    SHDID_NET_DOMAIN,
    SHDID_NET_SERVER,
    SHDID_NET_SHARE,
    SHDID_NET_RESTOFNET,
    SHDID_NET_OTHER
}

const TCHAR[] REGSTR_PATH_EXPLORER = "Software\\Microsoft\\Windows\\CurrentVersion\\Explorer";
const TCHAR[] REGSTR_PATH_SPECIAL_FOLDERS=REGSTR_PATH_EXPLORER ~ "\\Shell Folders";

enum {
    CSIDL_DESKTOP            =  0,
    CSIDL_INTERNET,
    CSIDL_PROGRAMS,
    CSIDL_CONTROLS,
    CSIDL_PRINTERS,
    CSIDL_PERSONAL,
    CSIDL_FAVORITES,
    CSIDL_STARTUP,
    CSIDL_RECENT,
    CSIDL_SENDTO,
    CSIDL_BITBUCKET,
    CSIDL_STARTMENU,      // = 11
    CSIDL_MYMUSIC            = 13,
    CSIDL_MYVIDEO,        // = 14
    CSIDL_DESKTOPDIRECTORY   = 16,
    CSIDL_DRIVES,
    CSIDL_NETWORK,
    CSIDL_NETHOOD,
    CSIDL_FONTS,
    CSIDL_TEMPLATES,
    CSIDL_COMMON_STARTMENU,
    CSIDL_COMMON_PROGRAMS,
    CSIDL_COMMON_STARTUP,
    CSIDL_COMMON_DESKTOPDIRECTORY,
    CSIDL_APPDATA,
    CSIDL_PRINTHOOD,
    CSIDL_LOCAL_APPDATA,
    CSIDL_ALTSTARTUP,
    CSIDL_COMMON_ALTSTARTUP,
    CSIDL_COMMON_FAVORITES,
    CSIDL_INTERNET_CACHE,
    CSIDL_COOKIES,
    CSIDL_HISTORY,
    CSIDL_COMMON_APPDATA,
    CSIDL_WINDOWS,
    CSIDL_SYSTEM,
    CSIDL_PROGRAM_FILES,
    CSIDL_MYPICTURES,
    CSIDL_PROFILE,
    CSIDL_SYSTEMX86,
    CSIDL_PROGRAM_FILESX86,
    CSIDL_PROGRAM_FILES_COMMON,
    CSIDL_PROGRAM_FILES_COMMONX86,
    CSIDL_COMMON_TEMPLATES,
    CSIDL_COMMON_DOCUMENTS,
    CSIDL_COMMON_ADMINTOOLS,
    CSIDL_ADMINTOOLS,
    CSIDL_CONNECTIONS,  // = 49
    CSIDL_COMMON_MUSIC     = 53,
    CSIDL_COMMON_PICTURES,
    CSIDL_COMMON_VIDEO,
    CSIDL_RESOURCES,
    CSIDL_RESOURCES_LOCALIZED,
    CSIDL_COMMON_OEM_LINKS,
    CSIDL_CDBURN_AREA,  // = 59
    CSIDL_COMPUTERSNEARME  = 61,
    CSIDL_FLAG_DONT_VERIFY = 0x4000,
    CSIDL_FLAG_CREATE      = 0x8000,
    CSIDL_FLAG_MASK        = 0xFF00
}

const TCHAR[]
    CFSTR_SHELLIDLIST       = "Shell IDList Array",
    CFSTR_SHELLIDLISTOFFSET = "Shell Object Offsets",
    CFSTR_NETRESOURCES      = "Net Resource",
    CFSTR_FILECONTENTS      = "FileContents",
    CFSTR_FILENAMEA         = "FileName",
    CFSTR_FILENAMEMAPA      = "FileNameMap",
    CFSTR_FILEDESCRIPTORA   = "FileGroupDescriptor",
    CFSTR_INETURLA          = "UniformResourceLocator",
    CFSTR_SHELLURL          = CFSTR_INETURLA,
    CFSTR_FILENAMEW         = "FileNameW",
    CFSTR_FILENAMEMAPW      = "FileNameMapW",
    CFSTR_FILEDESCRIPTORW   = "FileGroupDescriptorW",
    CFSTR_INETURLW          = "UniformResourceLocatorW";

version (Unicode) {
    alias CFSTR_FILENAMEW CFSTR_FILENAME;
    alias CFSTR_FILENAMEMAPW CFSTR_FILENAMEMAP;
    alias CFSTR_FILEDESCRIPTORW CFSTR_FILEDESCRIPTOR;
    alias CFSTR_INETURLW CFSTR_INETURL;
} else {
    alias CFSTR_FILENAMEA CFSTR_FILENAME;
    alias CFSTR_FILENAMEMAPA CFSTR_FILENAMEMAP;
    alias CFSTR_FILEDESCRIPTORA CFSTR_FILEDESCRIPTOR;
    alias CFSTR_INETURLA CFSTR_INETURL;
}
const TCHAR[]
    CFSTR_PRINTERGROUP        = "PrinterFriendlyName",
    CFSTR_INDRAGLOOP          = "InShellDragLoop",
    CFSTR_PASTESUCCEEDED      = "Paste Succeeded",
    CFSTR_PERFORMEDDROPEFFECT = "Performed DropEffect",
    CFSTR_PREFERREDDROPEFFECT = "Preferred DropEffect";

enum CMF_NORMAL=0;
enum CMF_DEFAULTONLY=1;
enum CMF_VERBSONLY=2;
enum CMF_EXPLORE=4;
enum CMF_NOVERBS=8;
enum CMF_CANRENAME=16;
enum CMF_NODEFAULT=32;
enum CMF_INCLUDESTATIC=64;
enum CMF_RESERVED=0xffff0000;
enum GCS_VERBA=0;
enum GCS_HELPTEXTA=1;
enum GCS_VALIDATEA=2;
enum GCS_VERBW=4;
enum GCS_HELPTEXTW=5;
enum GCS_VALIDATEW=6;
enum GCS_UNICODE=4;

version (Unicode) {
    alias GCS_VERBW GCS_VERB;
    alias GCS_HELPTEXTW GCS_HELPTEXT;
    alias GCS_VALIDATEW GCS_VALIDATE;
} else {
    alias GCS_VERBA GCS_VERB;
    alias GCS_HELPTEXTA GCS_HELPTEXT;
    alias GCS_VALIDATEA GCS_VALIDATE;
}

const TCHAR[]
    CMDSTR_NEWFOLDER   = "NewFolder",
    CMDSTR_VIEWLIST    = "ViewList",
    CMDSTR_VIEWDETAILS = "ViewDetails";

enum CMIC_MASK_HOTKEY     = SEE_MASK_HOTKEY;
enum CMIC_MASK_ICON       = SEE_MASK_ICON;
enum CMIC_MASK_FLAG_NO_UI = SEE_MASK_FLAG_NO_UI;
enum CMIC_MASK_MODAL      = 0x80000000;
// TODO: This isn't defined anywhere in MinGW.
//const CMIC_VALID_SEE_FLAGS=SEE_VALID_CMIC_FLAGS;

enum GIL_OPENICON = 1;
enum GIL_FORSHELL = 2;
enum GIL_SIMULATEDOC = 1;
enum GIL_PERINSTANCE = 2;
enum GIL_PERCLASS = 4;
enum GIL_NOTFILENAME = 8;
enum GIL_DONTCACHE = 16;

enum FVSIF_RECT = 1;
enum FVSIF_PINNED = 2;
enum FVSIF_NEWFAILED = 0x8000000;
enum FVSIF_NEWFILE = 0x80000000;
enum FVSIF_CANVIEWIT = 0x40000000;

enum CDBOSC_SETFOCUS = 0;
enum CDBOSC_KILLFOCUS = 1;
enum CDBOSC_SELCHANGE = 2;
enum CDBOSC_RENAME = 3;

enum FCIDM_SHVIEWFIRST = 0;
enum FCIDM_SHVIEWLAST = 0x7fff;
enum FCIDM_BROWSERFIRST = 0xa000;
enum FCIDM_BROWSERLAST = 0xbf00;
enum FCIDM_GLOBALFIRST = 0x8000;
enum FCIDM_GLOBALLAST = 0x9fff;
enum FCIDM_MENU_FILE = FCIDM_GLOBALFIRST;
enum FCIDM_MENU_EDIT = FCIDM_GLOBALFIRST+0x0040;
enum FCIDM_MENU_VIEW = FCIDM_GLOBALFIRST+0x0080;
enum FCIDM_MENU_VIEW_SEP_OPTIONS = FCIDM_GLOBALFIRST+0x0081;
enum FCIDM_MENU_TOOLS = FCIDM_GLOBALFIRST+0x00c0;
enum FCIDM_MENU_TOOLS_SEP_GOTO = FCIDM_GLOBALFIRST+0x00c1;
enum FCIDM_MENU_HELP = FCIDM_GLOBALFIRST+0x0100;
enum FCIDM_MENU_FIND = FCIDM_GLOBALFIRST+0x0140;
enum FCIDM_MENU_EXPLORE = FCIDM_GLOBALFIRST+0x0150;
enum FCIDM_MENU_FAVORITES = FCIDM_GLOBALFIRST+0x0170;
enum FCIDM_TOOLBAR = FCIDM_BROWSERFIRST;
enum FCIDM_STATUS = FCIDM_BROWSERFIRST+1;

enum SBSP_DEFBROWSER = 0;
enum SBSP_SAMEBROWSER = 1;
enum SBSP_NEWBROWSER = 2;
enum SBSP_DEFMODE = 0;
enum SBSP_OPENMODE = 16;
enum SBSP_EXPLOREMODE = 32;
enum SBSP_ABSOLUTE = 0;
enum SBSP_RELATIVE = 0x1000;
enum SBSP_PARENT = 0x2000;
enum SBSP_INITIATEDBYHLINKFRAME = 0x80000000;
enum SBSP_REDIRECT = 0x40000000;

enum {
    FCW_STATUS=1,
    FCW_TOOLBAR,
    FCW_TREE
}

enum FCT_MERGE=1;
enum FCT_CONFIGABLE=2;
enum FCT_ADDTOEND=4;

enum SVSI_DESELECT=0;
enum SVSI_SELECT=1;
enum SVSI_EDIT=3;
enum SVSI_DESELECTOTHERS=4;
enum SVSI_ENSUREVISIBLE=8;
enum SVSI_FOCUSED=16;

enum SVGIO_BACKGROUND=0;
enum SVGIO_SELECTION=1;
enum SVGIO_ALLVIEW=2;

enum UINT SV2GV_CURRENTVIEW=-1;
enum UINT SV2GV_DEFAULTVIEW=-2;

alias DWORD SHGDNF;

struct CIDA {
    UINT    cidl;
    UINT[1] aoffset;
}
alias CIDA* LPIDA;

struct SHITEMID {
    USHORT  cb;
    BYTE[1] abID;
}
alias SHITEMID*        LPSHITEMID;
alias const(SHITEMID)* LPCSHITEMID;

struct ITEMIDLIST {
    SHITEMID mkid;
}
alias ITEMIDLIST*        LPITEMIDLIST;
alias const(ITEMIDLIST)* LPCITEMIDLIST;

extern (Windows) alias int function(HWND, UINT, LPARAM, LPARAM) BFFCALLBACK;

align (8) {
struct BROWSEINFOA {
    HWND          hwndOwner;
    LPCITEMIDLIST pidlRoot;
    LPSTR         pszDisplayName;
    LPCSTR        lpszTitle;
    UINT          ulFlags;
    BFFCALLBACK   lpfn;
    LPARAM        lParam;
    int           iImage;
}
alias BROWSEINFOA* PBROWSEINFOA, LPBROWSEINFOA;

struct BROWSEINFOW {
    HWND          hwndOwner;
    LPCITEMIDLIST pidlRoot;
    LPWSTR        pszDisplayName;
    LPCWSTR       lpszTitle;
    UINT          ulFlags;
    BFFCALLBACK   lpfn;
    LPARAM        lParam;
    int           iImage;
}
alias BROWSEINFOW* PBROWSEINFOW, LPBROWSEINFOW;
} // align (8)

struct CMINVOKECOMMANDINFO {
    DWORD cbSize = this.sizeof;
    DWORD fMask;
    HWND hwnd;
    LPCSTR lpVerb;
    LPCSTR lpParameters;
    LPCSTR lpDirectory;
    int nShow;
    DWORD dwHotKey;
    HANDLE hIcon;
}
alias CMINVOKECOMMANDINFO* LPCMINVOKECOMMANDINFO;

struct DROPFILES {
    DWORD pFiles;
    POINT pt;
    BOOL fNC;
    BOOL fWide;
}
alias DROPFILES* LPDROPFILES;

enum SHGNO {
    SHGDN_NORMAL             = 0,
    SHGDN_INFOLDER,
    SHGDN_FOREDITING         = 0x1000,
    SHGDN_INCLUDE_NONFILESYS = 0x2000,
    SHGDN_FORADDRESSBAR      = 0x4000,
    SHGDN_FORPARSING         = 0x8000
}

enum SHCONTF {
    SHCONTF_FOLDERS            = 32,
    SHCONTF_NONFOLDERS         = 64,
    SHCONTF_INCLUDEHIDDEN      = 128,
    SHCONTF_INIT_ON_FIRST_NEXT = 256,
    SHCONTF_NETPRINTERSRCH     = 512,
    SHCONTF_SHAREABLE          = 1024,
    SHCONTF_STORAGE            = 2048
}

align(8) struct STRRET {
    UINT uType;
    union {
        LPWSTR pOleStr;
        UINT uOffset;
        char[MAX_PATH] cStr;
    }
}
alias STRRET* LPSTRRET;

enum FD_FLAGS {
    FD_CLSID      = 1,
    FD_SIZEPOINT  = 2,
    FD_ATTRIBUTES = 4,
    FD_CREATETIME = 8,
    FD_ACCESSTIME = 16,
    FD_WRITESTIME = 32,
    FD_FILESIZE   = 64,
    FD_LINKUI     = 0x8000
}

struct FILEDESCRIPTORA {
    DWORD dwFlags;
    CLSID clsid;
    SIZEL sizel;
    POINTL pointl;
    DWORD dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD nFileSizeHigh;
    DWORD nFileSizeLow;
    CHAR[MAX_PATH] cFileName = 0;
}
alias FILEDESCRIPTORA* LPFILEDESCRIPTORA;

struct FILEDESCRIPTORW {
    DWORD dwFlags;
    CLSID clsid;
    SIZEL sizel;
    POINTL pointl;
    DWORD dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD nFileSizeHigh;
    DWORD nFileSizeLow;
    WCHAR[MAX_PATH] cFileName = 0;
}
alias FILEDESCRIPTORW* LPFILEDESCRIPTORW;

struct FILEGROUPDESCRIPTORA {
    UINT cItems;
    FILEDESCRIPTORA[1] fgd;
}
alias FILEGROUPDESCRIPTORA* LPFILEGROUPDESCRIPTORA;

struct FILEGROUPDESCRIPTORW {
    UINT cItems;
    FILEDESCRIPTORW[1] fgd;
}
alias FILEGROUPDESCRIPTORW* LPFILEGROUPDESCRIPTORW;

enum SLR_FLAGS {
    SLR_NO_UI      = 1,
    SLR_ANY_MATCH  = 2,
    SLR_UPDATE     = 4,
    SLR_NOUPDATE   = 8,
    SLR_NOSEARCH   = 16,
    SLR_NOTRACK    = 32,
    SLR_NOLINKINFO = 64,
    SLR_INVOKE_MSI = 128
}

enum SLGP_FLAGS {
    SLGP_SHORTPATH=1,
    SLGP_UNCPRIORITY=2,
    SLGP_RAWPATH=4
}

alias PBYTE LPVIEWSETTINGS;

enum FOLDERFLAGS {
    FWF_AUTOARRANGE         = 1,
    FWF_ABBREVIATEDNAMES    = 2,
    FWF_SNAPTOGRID          = 4,
    FWF_OWNERDATA           = 8,
    FWF_BESTFITWINDOW       = 16,
    FWF_DESKTOP             = 32,
    FWF_SINGLESEL           = 64,
    FWF_NOSUBFOLDERS        = 128,
    FWF_TRANSPARENT         = 256,
    FWF_NOCLIENTEDGE        = 512,
    FWF_NOSCROLL            = 0x400,
    FWF_ALIGNLEFT           = 0x800,
    FWF_SINGLECLICKACTIVATE = 0x8000
}

enum FOLDERVIEWMODE {
    FVM_ICON      = 1,
    FVM_SMALLICON,
    FVM_LIST,
    FVM_DETAILS
}

struct FOLDERSETTINGS {
    UINT ViewMode;
    UINT fFlags;
}
alias FOLDERSETTINGS*        LPFOLDERSETTINGS;
alias const(FOLDERSETTINGS)* LPCFOLDERSETTINGS;

struct FVSHOWINFO {
    DWORD cbSize = this.sizeof;
    HWND hwndOwner;
    int iShow;
    DWORD dwFlags;
    RECT rect;
    LPUNKNOWN punkRel;
    OLECHAR[MAX_PATH] strNewFile = 0;
}
alias FVSHOWINFO* LPFVSHOWINFO;

struct NRESARRAY {
    UINT cItems;
    NETRESOURCE[1] nr;
}
alias NRESARRAY* LPNRESARRAY;

enum {
    SBSC_HIDE,
    SBSC_SHOW,
    SBSC_TOGGLE,
    SBSC_QUERY
}

enum {
    SBCMDID_ENABLESHOWTREE,
    SBCMDID_SHOWCONTROL,
    SBCMDID_CANCELNAVIGATION,
    SBCMDID_MAYSAVECHANGES,
    SBCMDID_SETHLINKFRAME,
    SBCMDID_ENABLESTOP,
    SBCMDID_OPTIONS
}
enum SVUIA_STATUS {
    SVUIA_DEACTIVATE,
    SVUIA_ACTIVATE_NOFOCUS,
    SVUIA_ACTIVATE_FOCUS,
    SVUIA_INPLACEACTIVATE
}

static if (_WIN32_IE >= 0x500) {

    struct EXTRASEARCH
     {
        GUID guidSearch;
        WCHAR[80] wszFriendlyName = 0;
        WCHAR[2084] wszUrl = 0;
    }
    alias EXTRASEARCH* LPEXTRASEARCH;

    alias DWORD SHCOLSTATEF;

    struct SHCOLUMNID {
        GUID fmtid;
        DWORD pid;
    }
    alias SHCOLUMNID*        LPSHCOLUMNID;
    alias const(SHCOLUMNID)* LPCSHCOLUMNID;

    struct SHELLDETAILS {
        int fmt;
        int cxChar;
        STRRET str;
    }
    alias SHELLDETAILS* LPSHELLDETAILS;

    struct PERSIST_FOLDER_TARGET_INFO
     {
        LPITEMIDLIST pidlTargetFolder;
        WCHAR[MAX_PATH] szTargetParsingName = 0;
        WCHAR[MAX_PATH] szNetworkProvider = 0;
        DWORD dwAttributes;
        int csidl;
    }

    enum SHGFP_TYPE {
        SHGFP_TYPE_CURRENT = 0,
        SHGFP_TYPE_DEFAULT = 1,
    }

}

interface IEnumIDList : IUnknown {
    HRESULT Next(ULONG, LPITEMIDLIST*, ULONG*);
    HRESULT Skip(ULONG);
    HRESULT Reset();
    HRESULT Clone(IEnumIDList*);
}
alias IEnumIDList LPENUMIDLIST;

interface IObjMgr : IUnknown {
    HRESULT Append(IUnknown);
    HRESULT Remove(IUnknown);
}

interface IContextMenu : IUnknown {
    HRESULT QueryContextMenu(HMENU, UINT, UINT, UINT, UINT);
    HRESULT InvokeCommand(LPCMINVOKECOMMANDINFO);
    HRESULT GetCommandString(UINT_PTR, UINT, PUINT, LPSTR, UINT);
}
alias IContextMenu LPCONTEXTMENU;

interface IContextMenu2 : IContextMenu {
    HRESULT HandleMenuMsg(UINT, WPARAM, LPARAM);
};
alias IContextMenu2 LPCONTEXTMENU2;

static if (_WIN32_IE >= 0x500) {
    align(8) {
        struct SHCOLUMNINIT {
            ULONG dwFlags;
            ULONG dwReserved;
            WCHAR[MAX_PATH] wszFolder = 0;
        }
        alias SHCOLUMNINIT*        LPSHCOLUMNINIT;
        alias const(SHCOLUMNINIT)* LPCSHCOLUMNINIT;

        struct SHCOLUMNDATA {
            ULONG dwFlags;
            DWORD dwFileAttributes;
            ULONG dwReserved;
            WCHAR *pwszExt = null;
            WCHAR[MAX_PATH] wszFile = 0;
        }
        alias SHCOLUMNDATA*        LPSHCOLUMNDATA;
        alias const(SHCOLUMNDATA)* LPCSHCOLUMNDATA;
    }

enum MAX_COLUMN_NAME_LEN = 80;
enum MAX_COLUMN_DESC_LEN = 128;

    align(1) struct SHCOLUMNINFO {
        align(1):
        SHCOLUMNID scid;
        VARTYPE vt;
        DWORD fmt;
        UINT cChars;
        DWORD csFlags;
        WCHAR[MAX_COLUMN_NAME_LEN] wszTitle = 0;
        WCHAR[MAX_COLUMN_DESC_LEN] wszDescription = 0;
    }
    alias SHCOLUMNINFO*        LPSHCOLUMNINFO;
    alias const(SHCOLUMNINFO)* LPCSHCOLUMNINFO;

    enum SHCOLSTATE {
        SHCOLSTATE_TYPE_STR      = 0x00000001,
        SHCOLSTATE_TYPE_INT      = 0x00000002,
        SHCOLSTATE_TYPE_DATE     = 0x00000003,
        SHCOLSTATE_TYPEMASK      = 0x0000000f,
        SHCOLSTATE_ONBYDEFAULT   = 0x00000010,
        SHCOLSTATE_SLOW          = 0x00000020,
        SHCOLSTATE_EXTENDED      = 0x00000040,
        SHCOLSTATE_SECONDARYUI   = 0x00000080,
        SHCOLSTATE_HIDDEN        = 0x00000100,
        SHCOLSTATE_PREFER_VARCMP = 0x00000200
    }

    interface IColumnProvider : IUnknown {
        HRESULT Initialize(LPCSHCOLUMNINIT);
        HRESULT GetColumnInfo(DWORD, SHCOLUMNINFO*);
        HRESULT GetItemData(LPCSHCOLUMNID, LPCSHCOLUMNDATA, VARIANT*);
    }
}/* _WIN32_IE >= 0x500 */

interface IQueryInfo : IUnknown {
    HRESULT GetInfoTip(DWORD, WCHAR**);
    HRESULT GetInfoFlags(DWORD*);
}

interface IShellExtInit : IUnknown {
    HRESULT Initialize(LPCITEMIDLIST, LPDATAOBJECT, HKEY);
}
alias IShellExtInit LPSHELLEXTINIT;

interface IShellPropSheetExt : IUnknown {
    HRESULT AddPages(LPFNADDPROPSHEETPAGE, LPARAM);
    HRESULT ReplacePage(UINT, LPFNADDPROPSHEETPAGE, LPARAM);
}
alias IShellPropSheetExt LPSHELLPROPSHEETEXT;

interface IExtractIconA : IUnknown {
    HRESULT GetIconLocation(UINT, LPSTR, UINT, int*, PUINT);
    HRESULT Extract(LPCSTR, UINT, HICON*, HICON*, UINT);
};
alias IExtractIconA LPEXTRACTICONA;

interface IExtractIconW : IUnknown {
    HRESULT GetIconLocation(UINT, LPWSTR, UINT, int*, PUINT);
    HRESULT Extract(LPCWSTR, UINT, HICON*, HICON*, UINT);
}
alias IExtractIconW LPEXTRACTICONW;

version (Unicode) {
    alias IExtractIconW IExtractIcon;
    alias LPEXTRACTICONW LPEXTRACTICON;
} else {
    alias IExtractIconA IExtractIcon;
    alias LPEXTRACTICONA LPEXTRACTICON;
}

interface IShellLinkA : IUnknown {
    HRESULT GetPath(LPSTR, int, WIN32_FIND_DATAA*, DWORD);
    HRESULT GetIDList(LPITEMIDLIST*);
    HRESULT SetIDList(LPCITEMIDLIST);
    HRESULT GetDescription(LPSTR, int);
    HRESULT SetDescription(LPCSTR);
    HRESULT GetWorkingDirectory(LPSTR, int);
    HRESULT SetWorkingDirectory(LPCSTR);
    HRESULT GetArguments(LPSTR, int);
    HRESULT SetArguments(LPCSTR);
    HRESULT GetHotkey(PWORD);
    HRESULT SetHotkey(WORD);
    HRESULT GetShowCmd(int*);
    HRESULT SetShowCmd(int);
    HRESULT GetIconLocation(LPSTR, int, int*);
    HRESULT SetIconLocation(LPCSTR, int);
    HRESULT SetRelativePath(LPCSTR , DWORD);
    HRESULT Resolve(HWND, DWORD);
    HRESULT SetPath(LPCSTR);
}

interface IShellLinkW : IUnknown {
    HRESULT GetPath(LPWSTR, int, WIN32_FIND_DATAW*, DWORD);
    HRESULT GetIDList(LPITEMIDLIST*);
    HRESULT SetIDList(LPCITEMIDLIST);
    HRESULT GetDescription(LPWSTR, int);
    HRESULT SetDescription(LPCWSTR);
    HRESULT GetWorkingDirectory(LPWSTR, int);
    HRESULT SetWorkingDirectory(LPCWSTR);
    HRESULT GetArguments(LPWSTR, int);
    HRESULT SetArguments(LPCWSTR);
    HRESULT GetHotkey(PWORD);
    HRESULT SetHotkey(WORD);
    HRESULT GetShowCmd(int*);
    HRESULT SetShowCmd(int);
    HRESULT GetIconLocation(LPWSTR, int, int*);
    HRESULT SetIconLocation(LPCWSTR, int);
    HRESULT SetRelativePath(LPCWSTR , DWORD);
    HRESULT Resolve(HWND, DWORD);
    HRESULT SetPath(LPCWSTR);
}


interface IShellFolder : IUnknown {
    HRESULT ParseDisplayName(HWND, LPBC, LPOLESTR, PULONG, LPITEMIDLIST*, PULONG);
    HRESULT EnumObjects(HWND, DWORD, LPENUMIDLIST*);
    HRESULT BindToObject(LPCITEMIDLIST, LPBC, REFIID, PVOID*);
    HRESULT BindToStorage(LPCITEMIDLIST, LPBC, REFIID, PVOID*);
    HRESULT CompareIDs(LPARAM, LPCITEMIDLIST, LPCITEMIDLIST);
    HRESULT CreateViewObject(HWND, REFIID, PVOID*);
    HRESULT GetAttributesOf(UINT, LPCITEMIDLIST*, PULONG);
    HRESULT GetUIObjectOf(HWND, UINT, LPCITEMIDLIST*, REFIID, PUINT, PVOID*);
    HRESULT GetDisplayNameOf(LPCITEMIDLIST, DWORD, LPSTRRET);
    HRESULT SetNameOf(HWND, LPCITEMIDLIST, LPCOLESTR, DWORD, LPITEMIDLIST*);
}
alias IShellFolder LPSHELLFOLDER;

static if (_WIN32_IE >= 0x500) {

interface IEnumExtraSearch: IUnknown {
    HRESULT Next(ULONG, LPEXTRASEARCH*, ULONG*);
    HRESULT Skip(ULONG);
    HRESULT Reset();
    HRESULT Clone(IEnumExtraSearch*);
}
alias IEnumExtraSearch LPENUMEXTRASEARCH;

interface IShellFolder2 : IShellFolder {
    HRESULT ParseDisplayName(HWND, LPBC, LPOLESTR, PULONG, LPITEMIDLIST*, PULONG);
    HRESULT EnumObjects(HWND, DWORD, LPENUMIDLIST*);
    HRESULT BindToObject(LPCITEMIDLIST, LPBC, REFIID, PVOID*);
    HRESULT BindToStorage(LPCITEMIDLIST, LPBC, REFIID, PVOID*);
    HRESULT CompareIDs(LPARAM, LPCITEMIDLIST, LPCITEMIDLIST);
    HRESULT CreateViewObject(HWND, REFIID, PVOID*);
    HRESULT GetAttributesOf(UINT, LPCITEMIDLIST*, PULONG);
    HRESULT GetUIObjectOf(HWND, UINT, LPCITEMIDLIST*, REFIID, PUINT, PVOID*);
    HRESULT GetDisplayNameOf(LPCITEMIDLIST, DWORD, LPSTRRET);
    HRESULT SetNameOf(HWND, LPCITEMIDLIST, LPCOLESTR, DWORD, LPITEMIDLIST*);
    HRESULT GetDefaultSearchGUID(GUID*);
    HRESULT EnumSearches(IEnumExtraSearch*);
    HRESULT GetDefaultColumn(DWORD, ULONG*, ULONG*);
    HRESULT GetDefaultColumnState(UINT, SHCOLSTATEF*);
    HRESULT GetDetailsEx(LPCITEMIDLIST, const(SHCOLUMNID)*, VARIANT*);
    HRESULT GetDetailsOf(LPCITEMIDLIST, UINT, SHELLDETAILS*);
    HRESULT MapColumnToSCID(UINT, SHCOLUMNID*);
}
alias IShellFolder2 LPSHELLFOLDER2;

} /* _WIN32_IE >= 0x500 */

interface ICopyHook : IUnknown {
    UINT CopyCallback(HWND, UINT, UINT, LPCSTR, DWORD, LPCSTR, DWORD);
}
alias ICopyHook LPCOPYHOOK;

interface IFileViewerSite : IUnknown {
    HRESULT SetPinnedWindow(HWND);
    HRESULT GetPinnedWindow(HWND*);
}
alias IFileViewerSite LPFILEVIEWERSITE;

interface IFileViewer : IUnknown {
    HRESULT ShowInitialize(LPFILEVIEWERSITE);
    HRESULT Show(LPFVSHOWINFO);
    HRESULT PrintTo(LPSTR, BOOL);
}
alias IFileViewer LPFILEVIEWER;

interface IFileSystemBindData : IUnknown {
    HRESULT SetFindData(const(WIN32_FIND_DATAW)*);
    HRESULT GetFindData(WIN32_FIND_DATAW*);
}

interface IPersistFolder : IPersist {
    HRESULT GetClassID(CLSID*);
    HRESULT Initialize(LPCITEMIDLIST);
}
alias IPersistFolder LPPERSISTFOLDER;

static if (_WIN32_IE >= 0x400 || _WIN32_WINNT >= 0x500) {

interface IPersistFolder2 : IPersistFolder {
    HRESULT GetClassID(CLSID*);
    HRESULT Initialize(LPCITEMIDLIST);
    HRESULT GetCurFolder(LPITEMIDLIST*);
}
alias IPersistFolder2 LPPERSISTFOLDER2;

}/* _WIN32_IE >= 0x400 || _WIN32_WINNT >= 0x500 */

static if (_WIN32_IE >= 0x500) {

interface IPersistFolder3 : IPersistFolder2 {
    HRESULT GetClassID(CLSID*);
    HRESULT Initialize(LPCITEMIDLIST);
    HRESULT GetCurFolder(LPITEMIDLIST*);
    HRESULT InitializeEx(IBindCtx, LPCITEMIDLIST, const(PERSIST_FOLDER_TARGET_INFO)*);
    HRESULT GetFolderTargetInfo(PERSIST_FOLDER_TARGET_INFO*);
}
alias IPersistFolder3 LPPERSISTFOLDER3;

} /* _WIN32_IE >= 0x500 */

alias IShellBrowser LPSHELLBROWSER;
alias IShellView LPSHELLVIEW;

interface IShellBrowser : IOleWindow {
    HRESULT GetWindow(HWND*);
    HRESULT ContextSensitiveHelp(BOOL);
    HRESULT InsertMenusSB(HMENU, LPOLEMENUGROUPWIDTHS);
    HRESULT SetMenuSB(HMENU, HOLEMENU, HWND);
    HRESULT RemoveMenusSB(HMENU);
    HRESULT SetStatusTextSB(LPCOLESTR);
    HRESULT EnableModelessSB(BOOL);
    HRESULT TranslateAcceleratorSB(LPMSG, WORD);
    HRESULT BrowseObject(LPCITEMIDLIST, UINT);
    HRESULT GetViewStateStream(DWORD, LPSTREAM*);
    HRESULT GetControlWindow(UINT, HWND*);
    HRESULT SendControlMsg(UINT, UINT, WPARAM, LPARAM, LRESULT*);
    HRESULT QueryActiveShellView(LPSHELLVIEW*);
    HRESULT OnViewWindowActive(LPSHELLVIEW);
    HRESULT SetToolbarItems(LPTBBUTTON, UINT, UINT);
}

interface IShellView : IOleWindow {
    HRESULT GetWindow(HWND*);
    HRESULT ContextSensitiveHelp(BOOL);
    HRESULT TranslateAccelerator(LPMSG);
//[No] #ifdef _FIX_ENABLEMODELESS_CONFLICT
//[No]  STDMETHOD(EnableModelessSV)(THIS_ BOOL) PURE;
//[Yes] #else
    HRESULT EnableModeless(BOOL);
//[Yes] #endif
    HRESULT UIActivate(UINT);
    HRESULT Refresh();
    HRESULT CreateViewWindow(IShellView, LPCFOLDERSETTINGS, LPSHELLBROWSER, RECT*, HWND*);
    HRESULT DestroyViewWindow();
    HRESULT GetCurrentInfo(LPFOLDERSETTINGS);
    HRESULT AddPropertySheetPages(DWORD, LPFNADDPROPSHEETPAGE, LPARAM);
    HRESULT SaveViewState();
    HRESULT SelectItem(LPCITEMIDLIST, UINT);
    HRESULT GetItemObject(UINT, REFIID, PVOID*);
}

interface ICommDlgBrowser : IUnknown {
    HRESULT OnDefaultCommand(IShellView);
    HRESULT OnStateChange(IShellView, ULONG);
    HRESULT IncludeObject(IShellView, LPCITEMIDLIST);
}
alias ICommDlgBrowser LPCOMMDLGBROWSER;

alias GUID SHELLVIEWID;

struct SV2CVW2_PARAMS {
    DWORD cbSize = this.sizeof;
    IShellView psvPrev;
    FOLDERSETTINGS  *pfs;
    IShellBrowser psbOwner;
    RECT *prcView;
const(SHELLVIEWID)* pvid;
    HWND hwndView;
}
alias SV2CVW2_PARAMS* LPSV2CVW2_PARAMS;

interface IShellView2 : IShellView {
    HRESULT GetWindow(HWND*);
    HRESULT ContextSensitiveHelp(BOOL);
    HRESULT TranslateAccelerator(LPMSG);
//[No] #ifdef _FIX_ENABLEMODELESS_CONFLICT
//[No]  STDMETHOD(EnableModelessSV)(THIS_ BOOL) PURE;
//[Yes] #else
    HRESULT EnableModeless(BOOL);
//[Yes] #endif
    HRESULT UIActivate(UINT);
    HRESULT Refresh();
    HRESULT CreateViewWindow(IShellView, LPCFOLDERSETTINGS, LPSHELLBROWSER, RECT*, HWND*);
    HRESULT DestroyViewWindow();
    HRESULT GetCurrentInfo(LPFOLDERSETTINGS);
    HRESULT AddPropertySheetPages(DWORD, LPFNADDPROPSHEETPAGE, LPARAM);
    HRESULT SaveViewState();
    HRESULT SelectItem(LPCITEMIDLIST, UINT);
    HRESULT GetItemObject(UINT, REFIID, PVOID*);
    HRESULT GetView(SHELLVIEWID*, ULONG);
    HRESULT CreateViewWindow2(LPSV2CVW2_PARAMS);
}

interface IShellExecuteHookA : IUnknown {
    HRESULT Execute(LPSHELLEXECUTEINFOA);
}

interface IShellExecuteHookW : IUnknown {
    HRESULT Execute(LPSHELLEXECUTEINFOW);
}

interface IShellIcon : IUnknown {
    HRESULT GetIconOf(LPCITEMIDLIST, UINT, PINT);
}
alias IShellIcon LPSHELLICON;

struct SHELLFLAGSTATE {
    short _bf;
/*
    BOOL fShowAllObjects : 1;
    BOOL fShowExtensions : 1;
    BOOL fNoConfirmRecycle : 1;
    BOOL fShowSysFiles : 1;
    BOOL fShowCompColor : 1;
    BOOL fDoubleClickInWebView : 1;
    BOOL fDesktopHTML : 1;
    BOOL fWin95Classic : 1;
    BOOL fDontPrettyPath : 1;
    BOOL fShowAttribCol : 1;
    BOOL fMapNetDrvBtn : 1;
    BOOL fShowInfoTip : 1;
    BOOL fHideIcons : 1;
    UINT fRestFlags : 3;
*/
    @property bool fShowAllObjects()       { return cast(bool) (_bf & 0x0001); }
    @property bool fShowExtensions()       { return cast(bool) (_bf & 0x0002); }
    @property bool fNoConfirmRecycle()     { return cast(bool) (_bf & 0x0004); }
    @property bool fShowSysFiles()         { return cast(bool) (_bf & 0x0008); }
    @property bool fShowCompColor()        { return cast(bool) (_bf & 0x0010); }
    @property bool fDoubleClickInWebView() { return cast(bool) (_bf & 0x0020); }
    @property bool fDesktopHTML()          { return cast(bool) (_bf & 0x0040); }
    @property bool fWin95Classic()         { return cast(bool) (_bf & 0x0080); }
    @property bool fDontPrettyPath()       { return cast(bool) (_bf & 0x0100); }
    @property bool fShowAttribCol()        { return cast(bool) (_bf & 0x0200); }
    @property bool fMapNetDrvBtn()         { return cast(bool) (_bf & 0x0400); }
    @property bool fShowInfoTip()          { return cast(bool) (_bf & 0x0800); }
    @property bool fHideIcons()            { return cast(bool) (_bf & 0x1000); }
    @property ubyte fRestFlags()           { return cast(ubyte) (_bf >> 13); }

    @property bool fShowAllObjects(bool f)       { _bf = cast(ushort) ((_bf & ~0xFFFE) | f);        return f; }
    @property bool fShowExtensions(bool f)       { _bf = cast(ushort) ((_bf & ~0xFFFD) | (f <<  1)); return f; }
    @property bool fNoConfirmRecycle(bool f)     { _bf = cast(ushort) ((_bf & ~0xFFFB) | (f <<  2)); return f; }
    @property bool fShowSysFiles(bool f)         { _bf = cast(ushort) ((_bf & ~0xFFF8) | (f <<  3)); return f; }
    @property bool fShowCompColor(bool f)        { _bf = cast(ushort) ((_bf & ~0xFFEF) | (f <<  4)); return f; }
    @property bool fDoubleClickInWebView(bool f) { _bf = cast(ushort) ((_bf & ~0xFFDF) | (f <<  5)); return f; }
    @property bool fDesktopHTML(bool f)          { _bf = cast(ushort) ((_bf & ~0xFFBF) | (f <<  6)); return f; }
    @property bool fWin95Classic(bool f)         { _bf = cast(ushort) ((_bf & ~0xFF8F) | (f <<  7)); return f; }
    @property bool fDontPrettyPath(bool f)       { _bf = cast(ushort) ((_bf & ~0xFEFF) | (f <<  8)); return f; }
    @property bool fShowAttribCol(bool f)        { _bf = cast(ushort) ((_bf & ~0xFDFF) | (f <<  9)); return f; }
    @property bool fMapNetDrvBtn(bool f)         { _bf = cast(ushort) ((_bf & ~0xFBFF) | (f << 10)); return f; }
    @property bool fShowInfoTip(bool f)          { _bf = cast(ushort) ((_bf & ~0xF8FF) | (f << 11)); return f; }
    @property bool fHideIcons(bool f)            { _bf = cast(ushort) ((_bf & ~0xEFFF) | (f << 12)); return f; }
    @property ubyte fRestFlags(ubyte f)          { _bf = cast(ushort) ((_bf & ~0x1FFF) | (f << 13)); return cast(ubyte) (f & 7); }
}
alias SHELLFLAGSTATE* LPSHELLFLAGSTATE;

enum SSF_SHOWALLOBJECTS = 0x1;
enum SSF_SHOWEXTENSIONS = 0x2;
enum SSF_SHOWCOMPCOLOR = 0x8;
enum SSF_SHOWSYSFILES = 0x20;
enum SSF_DOUBLECLICKINWEBVIEW = 0x80;
enum SSF_SHOWATTRIBCOL = 0x100;
enum SSF_DESKTOPHTML = 0x200;
enum SSF_WIN95CLASSIC = 0x400;
enum SSF_DONTPRETTYPATH = 0x800;
enum SSF_MAPNETDRVBUTTON = 0x1000;
enum SSF_SHOWINFOTIP = 0x2000;
enum SSF_HIDEICONS = 0x4000;
enum SSF_NOCONFIRMRECYCLE = 0x8000;

interface IShellIconOverlayIdentifier : IUnknown {
    HRESULT IsMemberOf(LPCWSTR, DWORD);
    HRESULT GetOverlayInfo(LPWSTR, int, int*, DWORD*);
    HRESULT GetPriority(int*);
}

enum ISIOI_ICONFILE  = 0x00000001;
enum ISIOI_ICONINDEX = 0x00000002;

static if (_WIN32_WINNT >= 0x500) {
    struct SHELLSTATE {
        uint _bf1;
        DWORD dwWin95Unused;
        UINT uWin95Unused;
        LONG lParamSort;
        int iSortDirection;
        UINT _version;
        UINT uNotUsed;
        uint _bf2;
    /*
        BOOL fShowAllObjects : 1;
        BOOL fShowExtensions : 1;
        BOOL fNoConfirmRecycle : 1;
        BOOL fShowSysFiles : 1;
        BOOL fShowCompColor : 1;
        BOOL fDoubleClickInWebView : 1;
        BOOL fDesktopHTML : 1;
        BOOL fWin95Classic : 1;
        BOOL fDontPrettyPath : 1;
        BOOL fShowAttribCol : 1;
        BOOL fMapNetDrvBtn : 1;
        BOOL fShowInfoTip : 1;
        BOOL fHideIcons : 1;
        BOOL fWebView : 1;
        BOOL fFilter : 1;
        BOOL fShowSuperHidden : 1;
        BOOL fNoNetCrawling : 1;
    */
        @property bool fShowAllObjects()       { return cast(bool) (_bf1 & 0x00000001); }
        @property bool fShowExtensions()       { return cast(bool) (_bf1 & 0x00000002); }
        @property bool fNoConfirmRecycle()     { return cast(bool) (_bf1 & 0x00000004); }
        @property bool fShowSysFiles()         { return cast(bool) (_bf1 & 0x00000008); }
        @property bool fShowCompColor()        { return cast(bool) (_bf1 & 0x00000010); }
        @property bool fDoubleClickInWebView() { return cast(bool) (_bf1 & 0x00000020); }
        @property bool fDesktopHTML()          { return cast(bool) (_bf1 & 0x00000040); }
        @property bool fWin95Classic()         { return cast(bool) (_bf1 & 0x00000080); }
        @property bool fDontPrettyPath()       { return cast(bool) (_bf1 & 0x00000100); }
        @property bool fShowAttribCol()        { return cast(bool) (_bf1 & 0x00000200); }
        @property bool fMapNetDrvBtn()         { return cast(bool) (_bf1 & 0x00000400); }
        @property bool fShowInfoTip()          { return cast(bool) (_bf1 & 0x00000800); }
        @property bool fHideIcons()            { return cast(bool) (_bf1 & 0x00001000); }
        @property bool fWebView()              { return cast(bool) (_bf1 & 0x00002000); }
        @property bool fFilter()               { return cast(bool) (_bf1 & 0x00004000); }
        @property bool fShowSuperHidden()      { return cast(bool) (_bf1 & 0x00008000); }
        @property bool fNoNetCrawling()        { return cast(bool) (_bf1 & 0x00010000); }

        @property bool fShowAllObjects(bool f)       { _bf1 = cast(uint) ((_bf1 & ~0xFFFFFFFE) | f);         return f; }
        @property bool fShowExtensions(bool f)       { _bf1 = cast(uint) ((_bf1 & ~0xFFFFFFFD) | (f <<  1)); return f; }
        @property bool fNoConfirmRecycle(bool f)     { _bf1 = cast(uint) ((_bf1 & ~0xFFFFFFFB) | (f <<  2)); return f; }
        @property bool fShowSysFiles(bool f)         { _bf1 = cast(uint) ((_bf1 & ~0xFFFFFFF8) | (f <<  3)); return f; }
        @property bool fShowCompColor(bool f)        { _bf1 = cast(uint) ((_bf1 & ~0xFFFFFFEF) | (f <<  4)); return f; }
        @property bool fDoubleClickInWebView(bool f) { _bf1 = cast(uint) ((_bf1 & ~0xFFFFFFDF) | (f <<  5)); return f; }
        @property bool fDesktopHTML(bool f)          { _bf1 = cast(uint) ((_bf1 & ~0xFFFFFFBF) | (f <<  6)); return f; }
        @property bool fWin95Classic(bool f)         { _bf1 = cast(uint) ((_bf1 & ~0xFFFFFF8F) | (f <<  7)); return f; }
        @property bool fDontPrettyPath(bool f)       { _bf1 = cast(uint) ((_bf1 & ~0xFFFFFEFF) | (f <<  8)); return f; }
        @property bool fShowAttribCol(bool f)        { _bf1 = cast(uint) ((_bf1 & ~0xFFFFFDFF) | (f <<  9)); return f; }
        @property bool fMapNetDrvBtn(bool f)         { _bf1 = cast(uint) ((_bf1 & ~0xFFFFFBFF) | (f << 10)); return f; }
        @property bool fShowInfoTip(bool f)          { _bf1 = cast(uint) ((_bf1 & ~0xFFFFF8FF) | (f << 11)); return f; }
        @property bool fHideIcons(bool f)            { _bf1 = cast(uint) ((_bf1 & ~0xFFFFEFFF) | (f << 12)); return f; }
        @property bool fWebView(bool f)              { _bf1 = cast(uint) ((_bf1 & ~0xFFFFDFFF) | (f << 13)); return f; }
        @property bool fFilter(bool f)               { _bf1 = cast(uint) ((_bf1 & ~0xFFFFBFFF) | (f << 14)); return f; }
        @property bool fShowSuperHidden(bool f)      { _bf1 = cast(uint) ((_bf1 & ~0xFFFF8FFF) | (f << 15)); return f; }
        @property bool fNoNetCrawling(bool f)        { _bf1 = cast(uint) ((_bf1 & ~0xFFFEFFFF) | (f << 16)); return f; }
    /*
        BOOL fSepProcess : 1;
        BOOL fStartPanelOn : 1;
        BOOL fShowStartPage : 1;
        UINT fSpareFlags : 13;
    */
        @property bool fSepProcess()           { return cast(bool) (_bf2 & 0x00000001); }
        @property bool fStartPanelOn()         { return cast(bool) (_bf2 & 0x00000002); }
        @property bool fShowStartPage()        { return cast(bool) (_bf2 & 0x00000004); }
        @property ushort fSpareFlags()         { return cast(ushort) ((_bf2 & 0x0000FFF8) >> 3); }

        @property bool fSepProcess(bool f)     { _bf2 = cast(uint) ((_bf2 & ~0xFFFFFFFE) | f);        return f; }
        @property bool fStartPanelOn(bool f)   { _bf2 = cast(uint) ((_bf2 & ~0xFFFFFFFD) | (f << 1)); return f; }
        @property bool fShowStartPage(bool f)  { _bf2 = cast(uint) ((_bf2 & ~0xFFFFFFFB) | (f << 2)); return f; }
        @property ushort fSpareFlags(ushort f) {
            _bf2 = cast(ushort) ((_bf2 & ~0xFFFF0007) | ((f & 0x1FFF) << 3));
            return cast(ushort) (f & 0x1FFF);
        }
    }
    alias SHELLSTATE* LPSHELLSTATE;
}

static if (_WIN32_IE >= 0x500) {
    align(8) {
        struct SHDRAGIMAGE {
            SIZE sizeDragImage;
            POINT ptOffset;
            HBITMAP hbmpDragImage;
            COLORREF crColorKey;
        }
        alias SHDRAGIMAGE* LPSHDRAGIMAGE;
    }

    interface IDragSourceHelper : IUnknown {
        HRESULT InitializeFromBitmap(LPSHDRAGIMAGE pshdi, IDataObject pDataObject);
        HRESULT InitializeFromWindow(HWND hwnd, POINT* ppt, IDataObject pDataObject);
    }

    interface IDropTargetHelper : IUnknown {
        HRESULT DragEnter(HWND hwndTarget, IDataObject pDataObject, POINT* ppt, DWORD dwEffect);
        HRESULT DragLeave();
        HRESULT DragOver(POINT* ppt, DWORD dwEffect);
        HRESULT Drop(IDataObject pDataObject, POINT* ppt, DWORD dwEffect);
        HRESULT Show(BOOL fShow);
    }
}

extern (Windows):
void SHAddToRecentDocs(UINT, PCVOID);
LPITEMIDLIST SHBrowseForFolderA(PBROWSEINFOA);
LPITEMIDLIST SHBrowseForFolderW(PBROWSEINFOW);
void SHChangeNotify(LONG, UINT, PCVOID, PCVOID);
HRESULT SHGetDataFromIDListA(LPSHELLFOLDER, LPCITEMIDLIST, int, PVOID, int);
HRESULT SHGetDataFromIDListW(LPSHELLFOLDER, LPCITEMIDLIST, int, PVOID, int);
HRESULT SHGetDesktopFolder(LPSHELLFOLDER*);
HRESULT SHGetInstanceExplorer(IUnknown*);
HRESULT SHGetMalloc(LPMALLOC*);
BOOL SHGetPathFromIDListA(LPCITEMIDLIST, LPSTR);
BOOL SHGetPathFromIDListW(LPCITEMIDLIST, LPWSTR);
HRESULT SHGetSpecialFolderLocation(HWND, int, LPITEMIDLIST*);
HRESULT SHLoadInProc(REFCLSID);

static if (_WIN32_IE >= 0x400) {
    BOOL SHGetSpecialFolderPathA(HWND, LPSTR, int, BOOL);
    BOOL SHGetSpecialFolderPathW(HWND, LPWSTR, int, BOOL);
}

/* SHGetFolderPath in shfolder.dll on W9x, NT4, also in shell32.dll on W2K */
HRESULT SHGetFolderPathA(HWND, int, HANDLE, DWORD, LPSTR);
HRESULT SHGetFolderPathW(HWND, int, HANDLE, DWORD, LPWSTR);

static if (_WIN32_WINNT >= 0x500) {
    INT SHGetIconOverlayIndexW(LPCWSTR pszIconPath, int iIconIndex);
    INT SHGetIconOverlayIndexA(LPCSTR pszIconPath, int iIconIndex);
    HRESULT SHGetFolderLocation(HWND, int, HANDLE, DWORD, LPITEMIDLIST*);
    INT SHCreateDirectoryExA(HWND, LPCSTR, LPSECURITY_ATTRIBUTES);
    INT SHCreateDirectoryExW(HWND, LPCWSTR, LPSECURITY_ATTRIBUTES);
    HRESULT SHBindToParent(LPCITEMIDLIST, REFIID, VOID**, LPCITEMIDLIST*);
}

static if (_WIN32_WINNT >= 0x501) {
    enum {
        PRF_VERIFYEXISTS         = 0x0001,
        PRF_TRYPROGRAMEXTENSIONS = (0x0002 | PRF_VERIFYEXISTS),
        PRF_FIRSTDIRDEF          = 0x0004,
        PRF_DONTFINDLNK          = 0x0008,
        IDO_SHGIOI_SHARE         = 0x0FFFFFFF,
        IDO_SHGIOI_LINK          = 0x0FFFFFFE,
        IDO_SHGIOI_SLOWFILE      = 0x0FFFFFFD,
        IDO_SHGIOI_DEFAULT       = 0x0FFFFFFC
    }

    struct SHDESCRIPTIONID {
        DWORD dwDescriptionId;
        CLSID clsid;
    }
    alias SHDESCRIPTIONID* LPSHDESCRIPTIONID;

    BOOL PathResolve(LPWSTR, LPCWSTR*, UINT);
    HRESULT SHGetFolderPathAndSubDirA(HWND, int, HANDLE, DWORD, LPCSTR, LPSTR);
    HRESULT SHGetFolderPathAndSubDirW(HWND, int, HANDLE, DWORD, LPCWSTR, LPWSTR);
    HRESULT SHParseDisplayName(LPCWSTR, IBindCtx, LPITEMIDLIST, SFGAOF, SFGAOF*);
}

void SHGetSettings(LPSHELLFLAGSTATE, DWORD);

static if (_WIN32_WINNT >= 0x500) {
    void SHGetSetSettings(LPSHELLSTATE, DWORD, BOOL);
    BOOL ILIsEqual(LPCITEMIDLIST, LPCITEMIDLIST);
    BOOL ILIsParent(LPCITEMIDLIST, LPCITEMIDLIST, BOOL);
    BOOL ILRemoveLastID(LPITEMIDLIST);
    HRESULT ILLoadFromStream(IStream, LPITEMIDLIST*);
    HRESULT ILSaveToStream(IStream, LPCITEMIDLIST);
    LPITEMIDLIST ILAppendID(LPITEMIDLIST, LPCSHITEMID, BOOL);
    LPITEMIDLIST ILClone(LPCITEMIDLIST);
    LPITEMIDLIST ILCloneFirst(LPCITEMIDLIST);
    LPITEMIDLIST ILCombine(LPCITEMIDLIST, LPCITEMIDLIST);
    LPITEMIDLIST ILFindChild(LPCITEMIDLIST, LPCITEMIDLIST);
    LPITEMIDLIST ILFindLastID(LPCITEMIDLIST);
    LPITEMIDLIST ILGetNext(LPCITEMIDLIST);
    UINT ILGetSize(LPCITEMIDLIST);
    void ILFree(LPITEMIDLIST);

    HRESULT SHCoCreateInstance(LPCWSTR, REFCLSID, IUnknown, REFIID, void**);
}

version (Unicode) {
    alias IShellExecuteHookW IShellExecuteHook;
    alias IShellLinkW IShellLink;
    alias BROWSEINFOW BROWSEINFO;
    alias SHBrowseForFolderW SHBrowseForFolder;
    alias SHGetDataFromIDListW SHGetDataFromIDList;
    alias SHGetPathFromIDListW SHGetPathFromIDList;
    static if (_WIN32_IE >= 0x400) {
        alias SHGetSpecialFolderPathW SHGetSpecialFolderPath;
    }
    alias SHGetFolderPathW SHGetFolderPath;
    static if (_WIN32_WINNT >= 0x500) {
        alias SHGetIconOverlayIndexW SHGetIconOverlayIndex;
        alias SHCreateDirectoryExW SHCreateDirectoryEx;
    }
    static if (_WIN32_WINNT >= 0x501) {
        alias SHGetFolderPathAndSubDirW SHGetFolderPathAndSubDir;
    }
    alias FILEDESCRIPTORW FILEDESCRIPTOR;
    alias LPFILEDESCRIPTORW LPFILEDESCRIPTOR;
    alias FILEGROUPDESCRIPTORW FILEGROUPDESCRIPTOR;
    alias LPFILEGROUPDESCRIPTORW LPFILEGROUPDESCRIPTOR;

} else {
    alias IShellExecuteHookA IShellExecuteHook;
    alias IShellLinkA IShellLink;
    alias BROWSEINFOA BROWSEINFO;
    alias SHBrowseForFolderA SHBrowseForFolder;
    alias SHGetDataFromIDListA SHGetDataFromIDList;
    alias SHGetPathFromIDListA SHGetPathFromIDList;
    static if (_WIN32_IE >= 0x400) {
        alias SHGetSpecialFolderPathA SHGetSpecialFolderPath;
    }
    alias SHGetFolderPathA SHGetFolderPath;
    static if (_WIN32_WINNT >= 0x500) {
        alias SHGetIconOverlayIndexA SHGetIconOverlayIndex;
        alias SHCreateDirectoryExA SHCreateDirectoryEx;
    }
    static if (_WIN32_WINNT >= 0x501) {
        alias SHGetFolderPathAndSubDirA SHGetFolderPathAndSubDir;
    }
    alias FILEDESCRIPTORA FILEDESCRIPTOR;
    alias LPFILEDESCRIPTORA LPFILEDESCRIPTOR;
    alias FILEGROUPDESCRIPTORA FILEGROUPDESCRIPTOR;
    alias LPFILEGROUPDESCRIPTORA LPFILEGROUPDESCRIPTOR;
}
alias BROWSEINFO* PBROWSEINFO, LPBROWSEINFO;

static if (_WIN32_WINNT >= 0x501) {
    interface IFolderView : IUnknown {
       HRESULT GetAutoArrange();
       HRESULT GetCurrentViewMode(UINT);
       HRESULT GetDefaultSpacing(POINT*);
       HRESULT GetFocusedItem(int*);
       HRESULT GetFolder(REFIID, PVOID*);
       HRESULT GetItemPosition(LPCITEMIDLIST, POINT*);
       HRESULT GetSelectionMarkedItem(int*);
       HRESULT GetSpacing(POINT*);
       HRESULT Item(int, LPITEMIDLIST*);
       HRESULT ItemCount(UINT, int*);
       HRESULT Items(UINT, REFIID, PVOID*);
       HRESULT SelectAndPositionItems(UINT, LPCITEMIDLIST*, POINT*, DWORD);
       HRESULT SelectItem(int, DWORD);
       HRESULT SetCurrentViewMode(UINT);
    }
    alias IFolderView LPFOLDERVIEW;
}

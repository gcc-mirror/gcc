/**
 * Windows API header module
 *
 * Translated from MinGW API for MS-Windows 3.12
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_commctrl.d)
 */
module core.sys.windows.commctrl;
version (Windows):

version (ANSI) {} else version = Unicode;
pragma(lib, "comctl32");

import core.sys.windows.w32api, core.sys.windows.windef, core.sys.windows.winuser;
import core.sys.windows.winbase; // for SYSTEMTIME
import core.sys.windows.objfwd;  // for LPSTREAM

import core.sys.windows.prsht;

enum COMCTL32_VERSION = 6;

const TCHAR[]
    DRAGLISTMSGSTRING  = "commctrl_DragListMsg",
    HOTKEY_CLASS       = "msctls_hotkey32",
    PROGRESS_CLASS     = "msctls_progress32",
    STATUSCLASSNAME    = "msctls_statusbar32",
    TOOLBARCLASSNAME   = "ToolbarWindow32",
    TOOLTIPS_CLASS     = "tooltips_class32",
    TRACKBAR_CLASS     = "msctls_trackbar32",
    UPDOWN_CLASS       = "msctls_updown32",
    ANIMATE_CLASS      = "SysAnimate32",
    DATETIMEPICK_CLASS = "SysDateTimePick32",
    MONTHCAL_CLASS     = "SysMonthCal32",
    REBARCLASSNAME     = "ReBarWindow32",
    WC_COMBOBOXEX      = "ComboBoxEx32",
    WC_IPADDRESS       = "SysIPAddress32",
    WC_LISTVIEW        = "SysListView32",
    WC_TABCONTROL      = "SysTabControl32",
    WC_TREEVIEW        = "SysTreeView32",
    WC_HEADER          = "SysHeader32",
    WC_PAGESCROLLER    = "SysPager",
    WC_NATIVEFONTCTL   = "NativeFontCtl",
    WC_BUTTON          = "Button",
    WC_STATIC          = "Static",
    WC_EDIT            = "Edit",
    WC_LISTBOX         = "ListBox",
    WC_COMBOBOX        = "ComboBox",
    WC_SCROLLBAR       = "ScrollBar",
    WC_LINKA           = "SysLink";

enum {
    LVM_FIRST = 0x1000,
    TV_FIRST  = 0x1100,
    HDM_FIRST = 0x1200
}

enum {
    ACM_OPENA = WM_USER + 100,
    ACM_PLAY  = WM_USER + 101,
    ACM_STOP  = WM_USER + 102,
    ACM_OPENW = WM_USER + 103,
    ACM_ISPLAYING = WM_USER + 104
}

enum {
    ACN_START = 1,
    ACN_STOP
}

enum {
    CBEIF_TEXT          = 0x00000001,
    CBEIF_IMAGE         = 0x00000002,
    CBEIF_SELECTEDIMAGE = 0x00000004,
    CBEIF_OVERLAY       = 0x00000008,
    CBEIF_INDENT        = 0x00000010,
    CBEIF_LPARAM        = 0x00000020,
    CBEIF_DI_SETITEM    = 0x10000000
}

enum {
    RBN_FIRST  = -831U,
    RBN_LAST   = -859U,
    MCN_FIRST  = -750U,
    MCN_LAST   = -759U,
    DTN_FIRST  = -760U,
    DTN_LAST   = -799U,
    CBEN_FIRST = -800U,
    CBEN_LAST  = -830U
}

enum {
    CBEN_INSERTITEM = CBEN_FIRST - 1,
    CBEN_DELETEITEM = CBEN_FIRST - 2,
    CBEN_BEGINEDIT  = CBEN_FIRST - 4,
    CBEN_ENDEDITA   = CBEN_FIRST - 5,
    CBEN_ENDEDITW   = CBEN_FIRST - 6
}

enum {
    CBENF_KILLFOCUS = 1,
    CBENF_RETURN,
    CBENF_ESCAPE,
    CBENF_DROPDOWN // = 4
}

enum CBEMAXSTRLEN = 260;

enum {
    DL_BEGINDRAG  = 1157,
    DL_CANCELDRAG = 1160,
    DL_DRAGGING   = 1158,
    DL_DROPPED    = 1159,
    DL_CURSORSET  = 0,
    DL_STOPCURSOR = 1,
    DL_COPYCURSOR = 2,
    DL_MOVECURSOR = 3
}

enum {
    CCS_TOP           = 1,
    CCS_NOMOVEY       = 2,
    CCS_BOTTOM        = 3,
    CCS_NORESIZE      = 4,
    CCS_NOPARENTALIGN = 8,
    CCS_ADJUSTABLE    = 32,
    CCS_NODIVIDER     = 64
}
static if (_WIN32_IE >= 0x300) {
    enum {
        CCS_VERT    = 128,
        CCS_LEFT    = 129,
        CCS_NOMOVEX = 130,
        CCS_RIGHT   = 131
    }
}

enum {
    ACS_CENTER      = 0x0001,
    ACS_TRANSPARENT = 0x0002,
    ACS_AUTOPLAY    = 0x0004,
    ACS_TIMER       = 0x0008
}

enum {
    PGS_VERT       = 0x00000000,
    PGS_HORZ       = 0x00000001,
    PGS_AUTOSCROLL = 0x00000002,
    PGS_DRAGNDROP  = 0x00000004
}

enum CMB_MASKED = 2;

enum MINSYSCOMMAND = SC_SIZE;

enum {
    SBT_OWNERDRAW  = 0x1000,
    SBT_NOBORDERS  = 256,
    SBT_POPOUT     = 512,
    SBT_RTLREADING = 1024
}

enum {
    SB_SETTEXTA       = WM_USER + 1,
    SB_SETTEXTW       = WM_USER + 11,
    SB_GETTEXTA       = WM_USER + 2,
    SB_GETTEXTW       = WM_USER + 13,
    SB_GETTEXTLENGTHA = WM_USER + 3,
    SB_GETTEXTLENGTHW = WM_USER + 12,
    SB_SETPARTS       = WM_USER + 4,
    SB_GETPARTS       = WM_USER + 6,
    SB_GETBORDERS     = WM_USER + 7,
    SB_SETMINHEIGHT   = WM_USER + 8,
    SB_SIMPLE         = WM_USER + 9,
    SB_GETRECT        = WM_USER + 10
}

enum {
    MSGF_COMMCTRL_BEGINDRAG   = 0x4200,
    MSGF_COMMCTRL_SIZEHEADER  = 0x4201,
    MSGF_COMMCTRL_DRAGSELECT  = 0x4202,
    MSGF_COMMCTRL_TOOLBARCUST = 0x4203
}

enum {
    ILC_COLOR    = 0,
    ILC_COLOR4   = 4,
    ILC_COLOR8   = 8,
    ILC_COLOR16  = 16,
    ILC_COLOR24  = 24,
    ILC_COLOR32  = 32,
    ILC_COLORDDB = 254,
    ILC_MASK     = 1,
    ILC_PALETTE  = 2048
}

enum {
    ILCF_MOVE,
    ILCF_SWAP
}

enum {
    ILS_NORMAL        = 0,
    ILS_GLOW          = 1,
    ILS_SHADOW        = 2,
    ILS_SATURATE      = 4,
    ILS_ALPHA         = 8,
    ILD_BLEND25       = 2,
    ILD_BLEND50       = 4,
    ILD_SELECTED      = 4,
    ILD_BLEND         = 4,
    ILD_FOCUS         = 2,
    ILD_MASK          = 16,
    ILD_NORMAL        = 0,
    ILD_TRANSPARENT   = 1,
    ILD_IMAGE         = 0x0020,
    ILD_ROP           = 0x0040,
    ILD_OVERLAYMASK   = 0x0F00,
    ILD_PRESERVEALPHA = 0x1000,
    ILD_SCALE         = 0x2000,
    ILD_DPISCALE      = 0x4000
}

enum {
    HDS_HORZ    = 0,
    HDS_BUTTONS = 2,
    HDS_HIDDEN  = 8
}
static if (_WIN32_IE >= 0x400) {
    enum {
        HDS_HOTTRACK = 4,
        HDS_DRAGDROP = 0x0040,
        HDS_FULLDRAG = 0x0080
    }
}
static if (_WIN32_IE >= 0x500) {
    enum {
        HDS_FILTERBAR = 0x0100
    }
}

enum {
    NM_FIRST  = 0,
    NM_LAST   = -99U,
    LVN_FIRST = -100U,
    LVN_LAST  = -199U,
    HDN_FIRST = -300U,
    HDN_LAST  = -399U,
    TVN_FIRST = -400U,
    TVN_LAST  = -499U,
    TTN_FIRST = -520U,
    TTN_LAST  = -549U,
    TCN_FIRST = -550U,
    TCN_LAST  = -580U,
    CDN_FIRST = -601U, /* also in commdlg.h */
    CDN_LAST  = -699U,
    TBN_FIRST = -700U,
    TBN_LAST  = -720U,
    UDN_FIRST = -721U,
    UDN_LAST  = -740U
}
/*static if (_WIN32_IE >= 0x300) {
    enum {
        RBN_FIRST  = -831U,
        RBN_LAST   = -859U,
        MCN_FIRST  = -750U,
        MCN_LAST   = -759U,
        DTN_FIRST  = -760U,
        DTN_LAST   = -799U,
        CBEN_FIRST = -800U,
        CBEN_LAST  = -830U
    }
}*/
static if (_WIN32_IE >= 0x400) {
    enum {
        IPN_FIRST        = -860U,
        IPN_LAST         = -879U,
        IPN_FIELDCHANGED = IPN_FIRST,
        SBN_FIRST        = -880U,
        SBN_LAST         = -899U,
        PGN_FIRST        = -900U,
        PGN_LAST         = -950U,
        PGN_SCROLL       = PGN_FIRST-1,
        PGN_CALCSIZE     = PGN_FIRST-2
    }
}

static if (_WIN32_IE >= 0x500) {
    enum {
        WMN_FIRST = -1000U,
        WMN_LAST = -1200U,
    }
}

static if (_WIN32_WINNT >= 0x501)
{
    enum {
        BCN_FIRST = -1250U,
        BCN_LAST = -1350U,
    }
}

static if (_WIN32_WINNT >= 0x600)
{
    enum {
        TRBN_FIRST = -1501U,
        TRBN_LAST = -1519U,
    }
}

enum {
    HDI_WIDTH  = 1,
    HDI_HEIGHT = 1,
    HDI_TEXT   = 2,
    HDI_FORMAT = 4,
    HDI_LPARAM = 8,
    HDI_BITMAP = 16
}
static if (_WIN32_IE >= 0x300) {
    enum {
        HDI_IMAGE      = 32,
        HDI_DI_SETITEM = 64,
        HDI_ORDER      = 128
    }
}
static if (_WIN32_IE >= 0x500) {
    enum {
        HDI_FILTER = 256
    }
}

enum {
    CBES_EX_NOEDITIMAGE       = 0x00000001,
    CBES_EX_NOEDITIMAGEINDENT = 0x00000002,
    CBES_EX_PATHWORDBREAKPROC = 0x00000004
}
static if (_WIN32_IE >= 0x400) {
    enum {
        CBES_EX_NOSIZELIMIT   = 0x00000008,
        CBES_EX_CASESENSITIVE = 0x00000010,
        CBEN_GETDISPINFOA     = CBEN_FIRST - 0,
        CBEN_GETDISPINFOW     = CBEN_FIRST - 7,
        CBEN_DRAGBEGINA       = CBEN_FIRST - 8,
        CBEN_DRAGBEGINW       = CBEN_FIRST - 9
    }
}

enum {
    HDF_LEFT,
    HDF_RIGHT,
    HDF_CENTER,
    HDF_JUSTIFYMASK,
    HDF_RTLREADING, // = 4
    HDF_OWNERDRAW = 0x8000,
    HDF_STRING    = 0x4000,
    HDF_BITMAP    = 0x2000
}
static if (_WIN32_IE >= 0x300) {
    enum {
        HDF_BITMAP_ON_RIGHT = 0x1000,
        HDF_IMAGE           = 0x0800
    }
}

enum {
    CCM_FIRST            = 0x2000,
    CCM_LAST             = CCM_FIRST + 0x200,
    CCM_SETBKCOLOR       = 8193,
    CCM_SETCOLORSCHEME   = 8194,
    CCM_GETCOLORSCHEME   = 8195,
    CCM_GETDROPTARGET    = 8196,
    CCM_SETUNICODEFORMAT = 8197,
    CCM_GETUNICODEFORMAT = 8198,
    CCM_SETVERSION       = 0x2007,
    CCM_GETVERSION       = 0x2008,
    CCM_SETNOTIFYWINDOW  = 0x2009
}

enum {
    HDM_GETITEMCOUNT = HDM_FIRST,
    HDM_INSERTITEMA  = HDM_FIRST + 1,
    HDM_INSERTITEMW  = HDM_FIRST + 10,
    HDM_DELETEITEM   = HDM_FIRST + 2,
    HDM_GETITEMA     = HDM_FIRST + 3,
    HDM_GETITEMW     = HDM_FIRST + 11,
    HDM_SETITEMA     = HDM_FIRST + 4,
    HDM_SETITEMW     = HDM_FIRST + 12,
    HDM_LAYOUT       = HDM_FIRST + 5
}
static if (_WIN32_IE >= 0x300) {
    enum {
        HDM_GETITEMRECT     = HDM_FIRST + 7,
        HDM_SETIMAGELIST    = HDM_FIRST + 8,
        HDM_GETIMAGELIST    = HDM_FIRST + 9,
        HDM_ORDERTOINDEX    = HDM_FIRST + 15,
        HDM_CREATEDRAGIMAGE = HDM_FIRST + 16,
        HDM_GETORDERARRAY   = HDM_FIRST + 17,
        HDM_SETORDERARRAY   = HDM_FIRST + 18,
        HDM_SETHOTDIVIDER   = HDM_FIRST + 19
    }
}
static if (_WIN32_IE >= 0x400) {
    enum {
        HDM_GETUNICODEFORMAT = CCM_GETUNICODEFORMAT,
        HDM_SETUNICODEFORMAT = CCM_SETUNICODEFORMAT
    }
}
static if (_WIN32_IE >= 0x500) {
    enum {
        HDM_SETBITMAPMARGIN = HDM_FIRST + 20,
        HDM_GETBITMAPMARGIN = HDM_FIRST + 21,
        HDM_SETFILTERCHANGETIMEOUT = HDM_FIRST + 22,
        HDM_EDITFILTER = HDM_FIRST + 23,
        HDM_CLEARFILTER = HDM_FIRST + 24,
    }
}
static if (_WIN32_IE >= 0x600) {
    enum {
        HDM_GETITEMDROPDOWNRECT = HDM_FIRST + 25,
        HDM_GETOVERFLOWRECT = HDM_FIRST + 26,
        HDM_GETFOCUSEDITEM = HDM_FIRST + 27,
        HDM_SETFOCUSEDITEM = HDM_FIRST + 28,
    }
}

enum {
    HHT_NOWHERE   = 1,
    HHT_ONHEADER  = 2,
    HHT_ONDIVIDER = 4,
    HHT_ONDIVOPEN = 8,
    HHT_ABOVE     = 256,
    HHT_BELOW     = 512,
    HHT_TORIGHT   = 1024,
    HHT_TOLEFT    = 2048
}

enum {
    HDM_HITTEST = HDM_FIRST + 6
}

enum {
    HDN_ITEMCHANGINGA    = HDN_FIRST -0,
    HDN_ITEMCHANGINGW    = HDN_FIRST -20,
    HDN_ITEMCHANGEDA     = HDN_FIRST -1,
    HDN_ITEMCHANGEDW     = HDN_FIRST -21,
    HDN_ITEMCLICKA       = HDN_FIRST -2,
    HDN_ITEMCLICKW       = HDN_FIRST -22,
    HDN_ITEMDBLCLICKA    = HDN_FIRST -3,
    HDN_ITEMDBLCLICKW    = HDN_FIRST -23,
    HDN_DIVIDERDBLCLICKA = HDN_FIRST -5,
    HDN_DIVIDERDBLCLICKW = HDN_FIRST -25,
    HDN_BEGINTRACKA      = HDN_FIRST -6,
    HDN_BEGINTRACKW      = HDN_FIRST -26,
    HDN_ENDTRACKA        = HDN_FIRST -7,
    HDN_ENDTRACKW        = HDN_FIRST -27,
    HDN_TRACKA           = HDN_FIRST -8,
    HDN_TRACKW           = HDN_FIRST -28
}
static if (_WIN32_IE >= 0x300) {
    enum {
        HDN_ENDDRAG      = (HDN_FIRST-11),
        HDN_BEGINDRAG    = (HDN_FIRST-10),
        HDN_GETDISPINFOA = (HDN_FIRST-9),
        HDN_GETDISPINFOW = (HDN_FIRST-29)
    }
}

static if (_WIN32_IE >= 0x400) {
    enum {
        HICF_OTHER          = 0x00,
        HICF_MOUSE          = 0x01,
        HICF_ARROWKEYS      = 0x02,
        HICF_ACCELERATOR    = 0x04,
        HICF_DUPACCEL       = 0x08,
        HICF_ENTERING       = 0x10,
        HICF_LEAVING        = 0x20,
        HICF_RESELECT       = 0x40,
        HICF_LMOUSE         = 0x80,
        HICF_TOGGLEDROPDOWN = 0x100
    }
}

enum {
    IPM_CLEARADDRESS = WM_USER + 100,
    IPM_SETADDRESS   = WM_USER + 101,
    IPM_GETADDRESS   = WM_USER + 102,
    IPM_SETRANGE     = WM_USER + 103,
    IPM_SETFOCUS     = WM_USER + 104,
    IPM_ISBLANK      = WM_USER + 105
}

static if (_WIN32_IE >= 0x500) {
    enum {
        I_INDENTCALLBACK = -1,
        I_IMAGENONE      = -2
    }
}

enum {
    TBSTATE_CHECKED       = 1,
    TBSTATE_PRESSED       = 2,
    TBSTATE_ENABLED       = 4,
    TBSTATE_HIDDEN        = 8,
    TBSTATE_INDETERMINATE = 16,
    TBSTATE_WRAP          = 32
}
static if (_WIN32_IE >= 0x300) {
    enum {
        TBSTATE_ELLIPSES = 0x40
    }
}
static if (_WIN32_IE >= 0x400) {
    enum {
        TBSTATE_MARKED = 0x0080
    }
}

enum {
    TBSTYLE_BUTTON     = 0,
    TBSTYLE_SEP        = 1,
    TBSTYLE_CHECK      = 2,
    TBSTYLE_GROUP      = 4,
    TBSTYLE_CHECKGROUP = TBSTYLE_GROUP | TBSTYLE_CHECK
}
static if (_WIN32_IE >= 0x300) {
    enum {
        TBSTYLE_DROPDOWN = 8
    }
}
static if (_WIN32_IE >= 0x400) {
    enum {
        TBSTYLE_AUTOSIZE = 16,
        TBSTYLE_NOPREFIX = 32
    }
}
enum {
    TBSTYLE_TOOLTIPS = 256,
    TBSTYLE_WRAPABLE = 512,
    TBSTYLE_ALTDRAG  = 1024
}
static if (_WIN32_IE >= 0x300) {
    enum {
        TBSTYLE_FLAT        = 2048,
        TBSTYLE_LIST        = 4096,
        TBSTYLE_CUSTOMERASE = 8192
    }
}
static if (_WIN32_IE >= 0x400) {
    enum {
        TBSTYLE_REGISTERDROP    = 0x4000,
        TBSTYLE_TRANSPARENT     = 0x8000,
        TBSTYLE_EX_DRAWDDARROWS = 0x00000001
    }
}
static if (_WIN32_IE >= 0x501) {
    enum {
        TBSTYLE_EX_MIXEDBUTTONS       = 8,
        TBSTYLE_EX_HIDECLIPPEDBUTTONS = 16
    }
}
static if (_WIN32_WINNT >= 0x501) {
    enum {
        TBSTYLE_EX_DOUBLEBUFFER = 0x80
    }
}

static if (_WIN32_IE >= 0x500) {
    enum {
        BTNS_BUTTON        = TBSTYLE_BUTTON,
        BTNS_SEP           = TBSTYLE_SEP,
        BTNS_CHECK         = TBSTYLE_CHECK,
        BTNS_GROUP         = TBSTYLE_GROUP,
        BTNS_CHECKGROUP    = TBSTYLE_CHECKGROUP,
        BTNS_DROPDOWN      = TBSTYLE_DROPDOWN,
        BTNS_AUTOSIZE      = TBSTYLE_AUTOSIZE,
        BTNS_NOPREFIX      = TBSTYLE_NOPREFIX,
        BTNS_WHOLEDROPDOWN = 0x0080
    }
}
static if (_WIN32_IE >= 0x501) {
    enum {
        BTNS_SHOWTEXT = 0x0040
    }
}

static if (_WIN32_IE >= 0x400) {
    enum {
        TBCDRF_NOEDGES        = 0x10000,
        TBCDRF_HILITEHOTTRACK = 0x20000,
        TBCDRF_NOOFFSET       = 0x40000,
        TBCDRF_NOMARK         = 0x80000,
        TBCDRF_NOETCHEDEFFECT = 0x100000
    }
}

enum HINST_COMMCTRL = cast(HINSTANCE) (-1);

enum {
    IDB_STD_SMALL_COLOR,
    IDB_STD_LARGE_COLOR,
    IDB_VIEW_SMALL_COLOR = 4,
    IDB_VIEW_LARGE_COLOR = 5
}
static if (_WIN32_IE >= 0x300) {
    enum {
        IDB_HIST_SMALL_COLOR = 8,
        IDB_HIST_LARGE_COLOR = 9
    }
}

enum {
    STD_CUT,
    STD_COPY,
    STD_PASTE,
    STD_UNDO,
    STD_REDOW,
    STD_DELETE,
    STD_FILENEW,
    STD_FILEOPEN,
    STD_FILESAVE,
    STD_PRINTPRE,
    STD_PROPERTIES,
    STD_HELP,
    STD_FIND,
    STD_REPLACE,
    STD_PRINT // = 14
}

enum {
    VIEW_LARGEICONS,
    VIEW_SMALLICONS,
    VIEW_LIST,
    VIEW_DETAILS,
    VIEW_SORTNAME,
    VIEW_SORTSIZE,
    VIEW_SORTDATE,
    VIEW_SORTTYPE,
    VIEW_PARENTFOLDER,
    VIEW_NETCONNECT,
    VIEW_NETDISCONNECT,
    VIEW_NEWFOLDER // = 11
}

enum {
    TB_ENABLEBUTTON          = WM_USER + 1,
    TB_CHECKBUTTON,
    TB_PRESSBUTTON,
    TB_HIDEBUTTON,
    TB_INDETERMINATE, //     = WM_USER + 5,
    TB_ISBUTTONENABLED       = WM_USER + 9,
    TB_ISBUTTONCHECKED,
    TB_ISBUTTONPRESSED,
    TB_ISBUTTONHIDDEN,
    TB_ISBUTTONINDETERMINATE, // = WM_USER + 13,
    TB_SETSTATE              = WM_USER + 17,
    TB_GETSTATE              = WM_USER + 18,
    TB_ADDBITMAP             = WM_USER + 19,
    TB_DELETEBUTTON          = WM_USER + 22,
    TB_GETBUTTON,
    TB_BUTTONCOUNT,
    TB_COMMANDTOINDEX,
    TB_SAVERESTOREA,
    TB_CUSTOMIZE,
    TB_ADDSTRINGA,
    TB_GETITEMRECT,
    TB_BUTTONSTRUCTSIZE,
    TB_SETBUTTONSIZE,
    TB_SETBITMAPSIZE,
    TB_AUTOSIZE, //          = WM_USER + 33,
    TB_GETTOOLTIPS           = WM_USER + 35,
    TB_SETTOOLTIPS           = WM_USER + 36,
    TB_SETPARENT             = WM_USER + 37,
    TB_SETROWS               = WM_USER + 39,
    TB_GETROWS,
    TB_GETBITMAPFLAGS,
    TB_SETCMDID,
    TB_CHANGEBITMAP,
    TB_GETBITMAP,
    TB_GETBUTTONTEXTA,
    TB_REPLACEBITMAP, //     = WM_USER + 46,
    TB_GETBUTTONSIZE         = WM_USER + 58,
    TB_SETBUTTONWIDTH        = WM_USER + 59,
    TB_GETBUTTONTEXTW        = WM_USER + 75,
    TB_SAVERESTOREW          = WM_USER + 76,
    TB_ADDSTRINGW            = WM_USER + 77,
}
static if (_WIN32_IE >= 0x400) {
    enum {
        TB_MARKBUTTON = WM_USER + 6
    }
}
static if (_WIN32_IE >= 0x400) {
    enum {
        TB_ISBUTTONHIGHLIGHTED = WM_USER + 14
    }
}
static if (_WIN32_IE >= 0x400) {
    enum {
        TB_ADDBUTTONSA   = WM_USER + 20,
        TB_INSERTBUTTONA = WM_USER + 21
    }
} else {
    enum {
        TB_ADDBUTTONS   = WM_USER + 20,
        TB_INSERTBUTTON = WM_USER + 21
    }
}
static if (_WIN32_IE >= 0x300) {
    enum {
        TB_SETINDENT = WM_USER + 47,
        TB_SETIMAGELIST,
        TB_GETIMAGELIST,
        TB_LOADIMAGES,
        TB_GETRECT,
        TB_SETHOTIMAGELIST,
        TB_GETHOTIMAGELIST,
        TB_SETDISABLEDIMAGELIST,
        TB_GETDISABLEDIMAGELIST,
        TB_SETSTYLE,
        TB_GETSTYLE,
        //TB_GETBUTTONSIZE,
        //TB_SETBUTTONWIDTH,
        TB_SETMAXTEXTROWS,
        TB_GETTEXTROWS // = WM_USER + 61
    }
}
static if (_WIN32_IE >= 0x400) {
    enum {
        TB_GETOBJECT            = WM_USER + 62,
        TB_GETBUTTONINFOW,
        TB_SETBUTTONINFOW,
        TB_GETBUTTONINFOA,
        TB_SETBUTTONINFOA,
        TB_INSERTBUTTONW,
        TB_ADDBUTTONSW,
        TB_HITTEST, //          = WM_USER + 69
        TB_SETEXTENDEDSTYLE     = WM_USER + 84,
        TB_GETEXTENDEDSTYLE     = WM_USER + 85,
        TB_SETDRAWTEXTFLAGS     = WM_USER + 70,
        TB_GETHOTITEM,
        TB_SETHOTITEM,
        TB_SETANCHORHIGHLIGHT,
        TB_GETANCHORHIGHLIGHT, // = WM_USER + 74
        TB_MAPACCELERATORA      = WM_USER + 78,
        TB_GETINSERTMARK,
        TB_SETINSERTMARK,
        TB_INSERTMARKHITTEST,
        TB_MOVEBUTTON,
        TB_GETMAXSIZE,
        //TB_SETEXTENDEDSTYLE,
        //TB_GETEXTENDEDSTYLE,
        TB_GETPADDING,
        TB_SETPADDING,
        TB_SETINSERTMARKCOLOR,
        TB_GETINSERTMARKCOLOR,
        TB_MAPACCELERATORW,
        TB_GETSTRINGW,
        TB_GETSTRINGA, //       = WM_USER + 92
        TB_SETHOTITEM2          = WM_USER + 94,
        TB_SETLISTGAP           = WM_USER + 96,
        TB_GETIMAGELISTCOUNT    = WM_USER + 98,
        TB_GETIDEALSIZE         = WM_USER + 99,
        //TB_TRANSLATEACCELERATOR = CCM_TRANSLATEACCELERATOR,
        TB_SETCOLORSCHEME       = CCM_SETCOLORSCHEME,
        TB_GETCOLORSCHEME       = CCM_GETCOLORSCHEME,
        TB_SETUNICODEFORMAT     = CCM_SETUNICODEFORMAT,
        TB_GETUNICODEFORMAT     = CCM_GETUNICODEFORMAT
    }
}

static if (_WIN32_WINNT >= 0x501) {
    enum {
        TB_GETMETRICS = WM_USER + 101,
        TB_SETMETRICS = WM_USER + 102,
    }
}

static if (_WIN32_WINNT >= 0x600) {
    enum {
        TB_GETITEMDROPDOWNRECT = WM_USER + 103,
        TB_SETPRESSEDIMAGELIST = WM_USER + 104,
        TB_GETPRESSEDIMAGELIST = WM_USER + 105,
    }
}

enum TBBF_LARGE = 1;

enum {
    TBN_GETBUTTONINFOA = TBN_FIRST -0,
    TBN_BEGINDRAG      = TBN_FIRST -1,
    TBN_ENDDRAG        = TBN_FIRST -2,
    TBN_BEGINADJUST    = TBN_FIRST -3,
    TBN_ENDADJUST      = TBN_FIRST -4,
    TBN_RESET          = TBN_FIRST -5,
    TBN_QUERYINSERT    = TBN_FIRST -6,
    TBN_QUERYDELETE    = TBN_FIRST -7,
    TBN_TOOLBARCHANGE  = TBN_FIRST -8,
    TBN_CUSTHELP       = TBN_FIRST -9
}
static if (_WIN32_IE >= 0x300) {
    enum {
        TBN_DROPDOWN = TBN_FIRST - 10
    }
}
static if (_WIN32_IE >= 0x400) {
    enum {
        TBN_HOTITEMCHANGE  = TBN_FIRST - 13,
        TBN_DRAGOUT        = TBN_FIRST - 14,
        TBN_DELETINGBUTTON = TBN_FIRST - 15,
        TBN_GETDISPINFOA   = TBN_FIRST - 16,
        TBN_GETDISPINFOW   = TBN_FIRST - 17,
        TBN_GETINFOTIPA    = TBN_FIRST - 18,
        TBN_GETINFOTIPW    = TBN_FIRST - 19,
        TBN_GETBUTTONINFOW = TBN_FIRST - 20
    }
}
static if (_WIN32_IE >= 0x500) {
    enum {
        TBN_RESTORE       = TBN_FIRST - 21,
        TBN_SAVE          = TBN_FIRST - 22,
        TBN_INITCUSTOMIZE = TBN_FIRST - 23
    }

    enum {
        TBNRF_HIDEHELP = 1,
        TBNRF_ENDCUSTOMIZE
    }

    enum {
        TBNF_IMAGE      = 1,
        TBNF_TEXT       = 2,
        TBNF_DI_SETITEM = 0x10000000
    }
}

enum {
    TTS_ALWAYSTIP = 1,
    TTS_NOPREFIX
}
static if (_WIN32_IE >= 0x500) {
    enum {
        TTS_NOANIMATE = 0x10,
        TTS_NOFADE    = 0x20,
        TTS_BALLOON   = 0x40,
        TTS_CLOSE     = 0x80
    }
}

enum {
    TTF_IDISHWND   = 1,
    TTF_CENTERTIP  = 2,
    TTF_RTLREADING = 4,
    TTF_SUBCLASS   = 16
}
static if (_WIN32_IE >= 0x300) {
    enum {
        TTF_TRACK       = 0x0020,
        TTF_ABSOLUTE    = 0x0080,
        TTF_TRANSPARENT = 0x0100,
        TTF_DI_SETITEM  = 0x8000
    }

    static if (_WIN32_IE >= 0x501) {
        enum {
            TTF_PARSELINKS = 0x1000
        }
    }

    enum {
        TBCD_TICS = 1,
        TBCD_THUMB,
        TBCD_CHANNEL // = 3
    }
}

static if (_WIN32_IE >= 0x400) {
    enum {
        TBDDRET_DEFAULT,
        TBDDRET_NODEFAULT,
        TBDDRET_TREATPRESSED
    }

    enum {
        TBIMHT_AFTER = 1,
        TBIMHT_BACKGROUND
    }
}

enum {
    TTDT_AUTOMATIC,
    TTDT_RESHOW,
    TTDT_AUTOPOP,
    TTDT_INITIAL
}

enum {
    TTM_ACTIVATE        = WM_USER + 1,
    TTM_SETDELAYTIME    = WM_USER + 3,
    TTM_ADDTOOLA,
    TTM_DELTOOLA,
    TTM_NEWTOOLRECTA,
    TTM_RELAYEVENT,
    TTM_GETTOOLINFOA,
    TTM_SETTOOLINFOA,
    TTM_HITTESTA,
    TTM_GETTEXTA,
    TTM_UPDATETIPTEXTA,
    TTM_GETTOOLCOUNT,
    TTM_ENUMTOOLSA,
    TTM_GETCURRENTTOOLA,
    TTM_WINDOWFROMPOINT, // = WM_USER + 16
    TTM_ADDTOOLW        = WM_USER + 50,
    TTM_DELTOOLW,
    TTM_NEWTOOLRECTW,
    TTM_GETTOOLINFOW,
    TTM_SETTOOLINFOW,
    TTM_HITTESTW,
    TTM_GETTEXTW,
    TTM_UPDATETIPTEXTW,
    TTM_ENUMTOOLSW,
    TTM_GETCURRENTTOOLW // = WM_USER + 59
}

static if (_WIN32_IE >= 0x300) {
    enum {
        TTM_TRACKACTIVATE = WM_USER + 17,
        TTM_TRACKPOSITION,
        TTM_SETTIPBKCOLOR,
        TTM_SETTIPTEXTCOLOR,
        TTM_GETDELAYTIME,
        TTM_GETTIPBKCOLOR,
        TTM_GETTIPTEXTCOLOR,
        TTM_SETMAXTIPWIDTH,
        TTM_GETMAXTIPWIDTH,
        TTM_SETMARGIN,
        TTM_GETMARGIN,
        TTM_POP // = WM_USER + 28
    }
}

static if (_WIN32_IE >= 0x400) {  // IE4.0 ???
    enum {
        TTM_UPDATE = WM_USER + 29,
        TTM_GETBUBBLESIZE,
        TTM_ADJUSTRECT,
        TTM_SETTITLEA,
        TTM_SETTITLEW // = WM_USER + 33
    }
    static if (_WIN32_IE >= 0x500) {
        alias TTM_SETTITLEW TTM_SETTITLE;
    } else {
        alias TTM_SETTITLEA TTM_SETTITLE;
    }
}

static if (_WIN32_WINNT >= 0x501) {
    enum {
        TTM_POPUP = (WM_USER + 34),
        TTM_GETTITLE = (WM_USER + 35),
    }
}

enum {
    TTN_GETDISPINFOA = TTN_FIRST - 0,
    TTN_GETDISPINFOW = TTN_FIRST - 10,
    TTN_NEEDTEXTA    = TTN_GETDISPINFOA,
    TTN_NEEDTEXTW    = TTN_GETDISPINFOW,
    TTN_SHOW         = TTN_FIRST-1,
    TTN_POP          = TTN_FIRST-2
}

enum UD_MAXVAL = 0x7fff;
enum UD_MINVAL = -UD_MAXVAL;

enum {
    UDN_DELTAPOS    = UDN_FIRST-1,
    UDS_WRAP        = 1,
    UDS_SETBUDDYINT = 2,
    UDS_ALIGNRIGHT  = 4,
    UDS_ALIGNLEFT   = 8,
    UDS_AUTOBUDDY   = 16,
    UDS_ARROWKEYS   = 32,
    UDS_HORZ        = 64,
    UDS_NOTHOUSANDS = 128
}

static if (_WIN32_IE >= 0x300) {
    enum {
        UDS_HOTTRACK = 0x0100
    }
}

enum {
    UDM_SETRANGE = WM_USER + 101,
    UDM_GETRANGE,
    UDM_SETPOS,
    UDM_GETPOS,
    UDM_SETBUDDY,
    UDM_GETBUDDY,
    UDM_SETACCEL,
    UDM_GETACCEL,
    UDM_SETBASE,
    UDM_GETBASE // = WM_USER + 110
}

static if (_WIN32_IE >= 0x400) {
    enum {
        UDM_SETRANGE32 = WM_USER + 111,
        UDM_GETRANGE32,
        UDM_SETPOS32,
        UDM_GETPOS32 // = WM_USER + 114
    }
}

static if (_WIN32_IE >= 0x500) {
    enum {
        UDM_SETUNICODEFORMAT = CCM_SETUNICODEFORMAT,
        UDM_GETUNICODEFORMAT = CCM_GETUNICODEFORMAT
    }
}

/*enum {
    SB_SETTEXTA       = WM_USER + 1,
    SB_GETTEXTA,
    SB_GETTEXTLENGTHA,
    SB_SETPARTS,   // = WM_USER + 4
    SB_GETPARTS       = WM_USER + 6,
    SB_GETBORDERS,
    SB_SETMINHEIGHT,
    SB_SIMPLE,
    SB_GETRECT,
    SB_SETTEXTW,
    SB_GETTEXTLENGTHW,
    SB_GETTEXTW    // = WM_USER + 13
}*/

/*enum {
    SBT_OWNERDRAW  = 0x1000,
    SBT_NOBORDERS  = 256,
    SBT_POPOUT     = 512,
    SBT_RTLREADING = 1024
}*/

static if (_WIN32_IE >= 0x400) {
    enum {
        SBT_TOOLTIPS         = 0x0800,
        SBN_SIMPLEMODECHANGE = SBN_FIRST
    }
}

enum {
    TBS_AUTOTICKS      = 1,
    TBS_VERT           = 2,
    TBS_HORZ           = 0,
    TBS_TOP            = 4,
    TBS_BOTTOM         = 0,
    TBS_LEFT           = 4,
    TBS_RIGHT          = 0,
    TBS_BOTH           = 8,
    TBS_NOTICKS        = 16,
    TBS_ENABLESELRANGE = 32,
    TBS_FIXEDLENGTH    = 64,
    TBS_NOTHUMB        = 128
}

static if (_WIN32_IE >= 0x300) {
    enum {
        TBS_TOOLTIPS = 0x0100,
        TBTS_TOP     = 0,
        TBTS_LEFT,
        TBTS_BOTTOM,
        TBTS_RIGHT // = 3
    }
}

static if (_WIN32_IE >= 0x500) {
    enum {
        TBS_REVERSED = 0x0200
    }
}

static if (_WIN32_IE >= 0x501) {
    enum {
        TBS_DOWNISLEFT = 0x0400
    }
}

static if (_WIN32_IE >= 0x400) {
    enum {
        TBIF_BYINDEX = 0x80000000,
        TBIF_COMMAND = 32,
        TBIF_IMAGE   = 1,
        TBIF_LPARAM  = 16,
        TBIF_SIZE    = 64,
        TBIF_STATE   = 4,
        TBIF_STYLE   = 8,
        TBIF_TEXT    = 2
    }
}

enum {
    TBM_GETPOS           = WM_USER,
    TBM_GETRANGEMIN,
    TBM_GETRANGEMAX,
    TBM_GETTIC,
    TBM_SETTIC,
    TBM_SETPOS,
    TBM_SETRANGE,
    TBM_SETRANGEMIN,
    TBM_SETRANGEMAX,
    TBM_CLEARTICS,
    TBM_SETSEL,
    TBM_SETSELSTART,
    TBM_SETSELEND,    // = WM_USER+12,
    TBM_GETPTICS         = WM_USER+14,
    TBM_GETTICPOS,
    TBM_GETNUMTICS,
    TBM_GETSELSTART,
    TBM_GETSELEND,
    TBM_CLEARSEL,
    TBM_SETTICFREQ,
    TBM_SETPAGESIZE,
    TBM_GETPAGESIZE,
    TBM_SETLINESIZE,
    TBM_GETLINESIZE,
    TBM_GETTHUMBRECT,
    TBM_GETCHANNELRECT,
    TBM_SETTHUMBLENGTH,
    TBM_GETTHUMBLENGTH,
    TBM_SETTOOLTIPS,
    TBM_GETTOOLTIPS,
    TBM_SETTIPSIDE,
    TBM_SETBUDDY,
    TBM_GETBUDDY, //     = WM_USER+33,
    TBM_GETUNICODEFORMAT = CCM_GETUNICODEFORMAT,
    TBM_SETUNICODEFORMAT = CCM_SETUNICODEFORMAT
}

enum {
    TB_LINEUP,
    TB_LINEDOWN,
    TB_PAGEUP,
    TB_PAGEDOWN,
    TB_THUMBPOSITION,
    TB_THUMBTRACK,
    TB_TOP,
    TB_BOTTOM,
    TB_ENDTRACK // = 8
}

enum {
    HOTKEYF_SHIFT   = 1,
    HOTKEYF_CONTROL = 2,
    HOTKEYF_ALT     = 4,
    HOTKEYF_EXT     = 8
}

enum {
    HKCOMB_NONE = 1,
    HKCOMB_S    = 2,
    HKCOMB_C    = 4,
    HKCOMB_A    = 8,
    HKCOMB_SC   = 16,
    HKCOMB_SA   = 32,
    HKCOMB_CA   = 64,
    HKCOMB_SCA  = 128
}

enum {
    HKM_SETHOTKEY = WM_USER + 1,
    HKM_GETHOTKEY = WM_USER + 2,
    HKM_SETRULES  = WM_USER + 3
}

enum {
    PBM_SETRANGE     = WM_USER + 1,
    PBM_SETPOS,
    PBM_DELTAPOS,
    PBM_SETSTEP,
    PBM_STEPIT,   // = WM_USER + 5
    PBM_SETRANGE32   = 1030,
    PBM_GETRANGE,
    PBM_GETPOS,
    PBM_SETBARCOLOR, // = 1033
    PBM_SETBKCOLOR   = CCM_SETBKCOLOR
}

static if (_WIN32_WINNT >= 0x501) {
    enum {
        PBM_SETMARQUEE = WM_USER + 10,
    }
}

static if (_WIN32_WINNT >= 0x600) {
    enum {
        PBM_GETSTEP = WM_USER + 13,
        PBM_GETBKCOLOR,
        PBM_GETBARCOLOR,
        PBM_SETSTATE,
        PBM_GETSTATE,
    }
}

enum {
    PBS_SMOOTH   = 1,
    PBS_VERTICAL = 4
}

static if (_WIN32_WINNT >= 0x501) {
    enum {
        PBS_MARQUEE = 8,
    }
}

static if (_WIN32_WINNT >= 0x600) {
    enum {
        PBS_SMOOTHREVERSE = 16,
    }
}

enum {
    LVS_ICON,
    LVS_REPORT,
    LVS_SMALLICON,
    LVS_LIST,        // = 3
    LVS_TYPEMASK        = 3,
    LVS_SINGLESEL       = 4,
    LVS_SHOWSELALWAYS   = 8,
    LVS_SORTASCENDING   = 16,
    LVS_SORTDESCENDING  = 32,
    LVS_SHAREIMAGELISTS = 64,
    LVS_NOLABELWRAP     = 128,
    LVS_AUTOARRANGE     = 256,
    LVS_EDITLABELS      = 512,
    LVS_NOSCROLL        = 0x2000,
    LVS_TYPESTYLEMASK   = 0xFC00,
    LVS_ALIGNTOP        = 0,
    LVS_ALIGNLEFT       = 0x800,
    LVS_ALIGNMASK       = 0xC00,
    LVS_OWNERDRAWFIXED  = 0x400,
    LVS_NOCOLUMNHEADER  = 0x4000,
    LVS_NOSORTHEADER    = 0x8000
}

static if (_WIN32_IE >= 0x300) {
    enum {
        CDIS_CHECKED       = 8,
        CDIS_DEFAULT       = 32,
        CDIS_DISABLED      = 4,
        CDIS_FOCUS         = 16,
        CDIS_GRAYED        = 2,
        CDIS_HOT           = 64,
        CDIS_SELECTED      = 1,
        CDIS_MARKED        = 128,
        CDIS_INDETERMINATE = 256
    }

    static if (_WIN32_WINNT >= 0x501) {
        enum {
            CDIS_SHOWKEYBOARDCUES = 512
        }
    }

    enum {
        CDDS_POSTERASE     = 4,
        CDDS_POSTPAINT     = 2,
        CDDS_PREERASE      = 3,
        CDDS_PREPAINT      = 1,
        CDDS_ITEM          = 65536,
        CDDS_ITEMPOSTERASE = 65540,
        CDDS_ITEMPOSTPAINT = 65538,
        CDDS_ITEMPREERASE  = 65539,
        CDDS_ITEMPREPAINT  = 65537
    }

    static if (_WIN32_IE >= 0x400) {
        enum {
            CDDS_SUBITEM = 0x20000
        }
    }

    enum {
        CDRF_DODEFAULT         = 0x00,
        CDRF_NOTIFYITEMDRAW    = 0x20,
        CDRF_NOTIFYSUBITEMDRAW = 0x20,
        CDRF_NOTIFYITEMERASE   = 0x80,
        CDRF_NOTIFYPOSTERASE   = 0x40,
        CDRF_NOTIFYPOSTPAINT   = 0x10,
        CDRF_NEWFONT           = 0x02,
        CDRF_SKIPDEFAULT       = 0x04
    }

    static if (_WIN32_IE >= 0x400) {
        enum {
            LVBKIF_SOURCE_NONE    = 0x00000000,
            LVBKIF_SOURCE_HBITMAP = 0x00000001,
            LVBKIF_SOURCE_URL     = 0x00000002,
            LVBKIF_SOURCE_MASK    = 0x00000003,
            LVBKIF_STYLE_NORMAL   = 0x00000000,
            LVBKIF_STYLE_TILE     = 0x00000010,
            LVBKIF_STYLE_MASK     = 0x00000010
        }
    }

    static if (_WIN32_WINNT >= 0x501) {
        enum {
            LVBKIF_FLAG_TILEOFFSET = 0x00000100,
            LVBKIF_TYPE_WATERMARK  = 0x10000000
        }
    }

    enum {
        LVS_OWNERDATA = 4096
    }

    enum {
        LVS_EX_CHECKBOXES       = 4,
        LVS_EX_FULLROWSELECT    = 32,
        LVS_EX_GRIDLINES        = 1,
        LVS_EX_HEADERDRAGDROP   = 16,
        LVS_EX_ONECLICKACTIVATE = 64,
        LVS_EX_SUBITEMIMAGES    = 2,
        LVS_EX_TRACKSELECT      = 8,
        LVS_EX_TWOCLICKACTIVATE = 128
    }

    enum {
        LVSICF_NOINVALIDATEALL = 0x00000001,
        LVSICF_NOSCROLL        = 0x00000002
    }

    static if (_WIN32_IE >= 0x400) {
        enum {
            LVS_EX_FLATSB         = 0x00000100,
            LVS_EX_REGIONAL       = 0x00000200,
            LVS_EX_INFOTIP        = 0x00000400,
            LVS_EX_UNDERLINEHOT   = 0x00000800,
            LVS_EX_UNDERLINECOLD  = 0x00001000,
            LVS_EX_MULTIWORKAREAS = 0x00002000
        }
    }

    static if (_WIN32_IE >= 0x500) {
        enum {
            LVS_EX_LABELTIP     = 0x00004000,
            LVS_EX_BORDERSELECT = 0x00008000
        }
    }
}

enum {
    LVSIL_NORMAL,
    LVSIL_SMALL,
    LVSIL_STATE
}

enum {
    LVM_GETBKCOLOR             = LVM_FIRST,
    LVM_SETBKCOLOR,
    LVM_GETIMAGELIST,
    LVM_SETIMAGELIST,
    LVM_GETITEMCOUNT,       // = LVM_FIRST +   4
    LVM_SORTITEMSEX            = LVM_FIRST +  81,
    LVM_GETGROUPSTATE          = LVM_FIRST + 92,
    LVM_GETFOCUSEDGROUP,
    LVM_GETGROUPRECT           = LVM_FIRST + 98,
    LVM_SETVIEW                = LVM_FIRST + 142,
    LVM_GETVIEW,            // = LVM_FIRST + 143
    LVM_INSERTGROUP            = LVM_FIRST + 145,
    LVM_SETGROUPINFO           = LVM_FIRST + 147,
    LVM_GETGROUPINFO           = LVM_FIRST + 149,
    LVM_REMOVEGROUP,
    LVM_MOVEGROUP,          // = LVM_FIRST + 151
    LVM_GETGROUPCOUNT,
    LVM_GETGROUPINFOBYINDEX,
    LVM_MOVEITEMTOGROUP,
    LVM_SETGROUPMETRICS        = LVM_FIRST + 155,
    LVM_GETGROUPMETRICS,
    LVM_ENABLEGROUPVIEW,
    LVM_SORTGROUPS,
    LVM_INSERTGROUPSORTED,
    LVM_REMOVEALLGROUPS,
    LVM_HASGROUP,
    LVM_SETTILEVIEWINFO,
    LVM_GETTILEVIEWINFO,
    LVM_SETTILEINFO,
    LVM_GETTILEINFO,
    LVM_SETINSERTMARK,
    LVM_GETINSERTMARK,
    LVM_INSERTMARKHITTEST,
    LVM_GETINSERTMARKRECT,
    LVM_SETINSERTMARKCOLOR,
    LVM_GETINSERTMARKCOLOR, // = LVM_FIRST + 171
    LVM_SETINFOTIP             = LVM_FIRST + 173,
    LVM_GETSELECTEDCOLUMN,
    LVM_ISGROUPVIEWENABLED,
    LVM_GETOUTLINECOLOR,
    LVM_SETOUTLINECOLOR,    // = LVM_FIRST + 177
    LVM_CANCELEDITLABEL        = LVM_FIRST + 179,
    LVM_MAPINDEXTOID           = LVM_FIRST + 180,
    LVM_MAPIDTOINDEX           = LVM_FIRST + 181,
    LVM_ISITEMVISIBLE          = LVM_FIRST + 182,
}

static if (_WIN32_WINNT >= 0x501) {
    enum {
        LVM_SETSELECTEDCOLUMN  = LVM_FIRST + 140
    }
}

static if (_WIN32_WINNT >= 0x600) {
    enum {
        LVM_GETEMPTYTEXT = LVM_FIRST + 204,
        LVM_GETFOOTERRECT = LVM_FIRST + 205,
        LVM_GETFOOTERINFO = LVM_FIRST + 206,
        LVM_GETFOOTERITEMRECT = LVM_FIRST + 207,
        LVM_GETFOOTERITEM = (LVM_FIRST + 208),
        LVM_GETITEMINDEXRECT = (LVM_FIRST + 209),
        LVM_SETITEMINDEXSTATE = (LVM_FIRST + 210),
        LVM_GETNEXTITEMINDEX = (LVM_FIRST + 211),
    }
}

enum {
    LVIF_TEXT  = 1,
    LVIF_IMAGE = 2,
    LVIF_PARAM = 4,
    LVIF_STATE = 8
}

static if (_WIN32_IE >= 0x300) {
    enum {
        LVIF_INDENT      = 16,
        LVIF_NORECOMPUTE = 2048
    }
}

static if (_WIN32_WINNT >= 0x501) {
    enum {
        LVIF_GROUPID = 128,
        LVIF_COLUMNS = 256
    }
}

enum {
    LVIS_FOCUSED        = 1,
    LVIS_SELECTED       = 2,
    LVIS_CUT            = 4,
    LVIS_DROPHILITED    = 8,
    LVIS_OVERLAYMASK    = 0xF00,
    LVIS_STATEIMAGEMASK = 0xF000
}

enum LPWSTR LPSTR_TEXTCALLBACKW = cast(LPWSTR) -1;
enum LPSTR  LPSTR_TEXTCALLBACKA = cast(LPSTR) -1;

enum I_IMAGECALLBACK = -1;

static if (_WIN32_IE >= 0x400) {
    enum {
        LVM_SETBKIMAGEA          = LVM_FIRST + 68,
        LVM_SETBKIMAGEW          = LVM_FIRST + 138,
        LVM_GETBKIMAGEA          = LVM_FIRST + 69,
        LVM_GETBKIMAGEW          = LVM_FIRST + 139,
        LV_MAX_WORKAREAS         = 16,
        LVM_SETWORKAREAS         = LVM_FIRST + 65,
        LVM_GETWORKAREAS         = LVM_FIRST + 70,
        LVM_GETNUMBEROFWORKAREAS = LVM_FIRST + 73,
        LVM_GETSELECTIONMARK     = LVM_FIRST + 66,
        LVM_SETSELECTIONMARK     = LVM_FIRST + 67,
        LVM_SETHOVERTIME         = LVM_FIRST + 71,
        LVM_GETHOVERTIME         = LVM_FIRST + 72,
        LVM_SETTOOLTIPS          = LVM_FIRST + 74,
        LVM_GETTOOLTIPS          = LVM_FIRST + 78,
        LVM_SETUNICODEFORMAT     = CCM_SETUNICODEFORMAT,
        LVM_GETUNICODEFORMAT     = CCM_GETUNICODEFORMAT
    }
}

enum {
    LVNI_ALL,
    LVNI_FOCUSED     = 1,
    LVNI_SELECTED    = 2,
    LVNI_CUT         = 4,
    LVNI_DROPHILITED = 8,
    LVNI_ABOVE       = 256,
    LVNI_BELOW       = 512,
    LVNI_TOLEFT      = 1024,
    LVNI_TORIGHT     = 2048
}

enum {
    LVM_GETITEMA          = LVM_FIRST + 5,
    LVM_SETITEMA,
    LVM_INSERTITEMA,
    LVM_DELETEITEM,
    LVM_DELETEALLITEMS,
    LVM_GETCALLBACKMASK,
    LVM_SETCALLBACKMASK,
    LVM_GETNEXTITEM,
    LVM_FINDITEMA,
    LVM_GETITEMRECT,
    LVM_SETITEMPOSITION,
    LVM_GETITEMPOSITION,
    LVM_GETSTRINGWIDTHA,
    LVM_HITTEST,
    LVM_ENSUREVISIBLE,
    LVM_SCROLL,
    LVM_REDRAWITEMS,
    LVM_ARRANGE,
    LVM_EDITLABELA,
    LVM_GETEDITCONTROL,
    LVM_GETCOLUMNA,
    LVM_SETCOLUMNA,
    LVM_INSERTCOLUMNA,
    LVM_DELETECOLUMN,
    LVM_GETCOLUMNWIDTH,
    LVM_SETCOLUMNWIDTH, // = LVM_FIRST + 30,
    LVM_CREATEDRAGIMAGE   = LVM_FIRST + 33,
    LVM_GETVIEWRECT,
    LVM_GETTEXTCOLOR,
    LVM_SETTEXTCOLOR,
    LVM_GETTEXTBKCOLOR,
    LVM_SETTEXTBKCOLOR,
    LVM_GETTOPINDEX,
    LVM_GETCOUNTPERPAGE,
    LVM_GETORIGIN,
    LVM_UPDATE,
    LVM_SETITEMSTATE,
    LVM_GETITEMSTATE,
    LVM_GETITEMTEXTA,
    LVM_SETITEMTEXTA,
    LVM_SETITEMCOUNT,
    LVM_SORTITEMS,
    LVM_SETITEMPOSITION32,
    LVM_GETSELECTEDCOUNT,
    LVM_GETITEMSPACING,
    LVM_GETISEARCHSTRINGA, // = LVM_FIRST + 52,
    LVM_GETITEMW          = LVM_FIRST + 75,
    LVM_SETITEMW          = LVM_FIRST + 76,
    LVM_INSERTITEMW       = LVM_FIRST + 77,
    LVM_FINDITEMW         = LVM_FIRST + 83,
    LVM_GETSTRINGWIDTHW   = LVM_FIRST + 87,
    LVM_GETCOLUMNW        = LVM_FIRST + 95,
    LVM_SETCOLUMNW        = LVM_FIRST + 96,
    LVM_INSERTCOLUMNW     = LVM_FIRST + 97,
    LVM_GETITEMTEXTW      = LVM_FIRST + 115,
    LVM_SETITEMTEXTW,
    LVM_GETISEARCHSTRINGW,
    LVM_EDITLABELW     // = LVM_FIRST + 118,
}

static if (_WIN32_IE >= 0x300) {
    enum {
        LVM_GETHEADER                = LVM_FIRST + 31,
        LVM_SETICONSPACING           = LVM_FIRST + 53,
        LVM_SETEXTENDEDLISTVIEWSTYLE,
        LVM_GETEXTENDEDLISTVIEWSTYLE,
        LVM_GETSUBITEMRECT,
        LVM_SUBITEMHITTEST,
        LVM_SETCOLUMNORDERARRAY,
        LVM_GETCOLUMNORDERARRAY,
        LVM_SETHOTITEM,
        LVM_GETHOTITEM,
        LVM_SETHOTCURSOR,
        LVM_GETHOTCURSOR,
        LVM_APPROXIMATEVIEWRECT   // = LVM_FIRST + 64,
    }
}

enum {
    LVFI_PARAM     = 1,
    LVFI_STRING    = 2,
    LVFI_PARTIAL   = 8,
    LVFI_WRAP      = 32,
    LVFI_NEARESTXY = 64
}

enum {
    LVIF_DI_SETITEM = 0x1000
}

enum {
    LVIR_BOUNDS,
    LVIR_ICON,
    LVIR_LABEL,
    LVIR_SELECTBOUNDS // = 3
}

enum {
    LVHT_NOWHERE         = 1,
    LVHT_ONITEMICON      = 2,
    LVHT_ONITEMLABEL     = 4,
    LVHT_ONITEMSTATEICON = 8,
    LVHT_ONITEM          = LVHT_ONITEMICON | LVHT_ONITEMLABEL
                           | LVHT_ONITEMSTATEICON,
    LVHT_ABOVE           = 8,
    LVHT_BELOW           = 16,
    LVHT_TORIGHT         = 32,
    LVHT_TOLEFT          = 64
}

enum {
    LVA_DEFAULT    = 0,
    LVA_ALIGNLEFT  = 1,
    LVA_ALIGNTOP   = 2,
    LVA_SNAPTOGRID = 5
}

enum {
    LVCF_FMT     = 1,
    LVCF_WIDTH   = 2,
    LVCF_TEXT    = 4,
    LVCF_SUBITEM = 8
}

static if (_WIN32_IE >= 0x300) {
    enum {
        LVCF_IMAGE = 16,
        LVCF_ORDER = 32
    }
}

enum {
    LVCFMT_LEFT,
    LVCFMT_RIGHT,
    LVCFMT_CENTER,
    LVCFMT_JUSTIFYMASK // = 3
}

static if (_WIN32_IE >= 0x300) {
    enum {
        LVCFMT_IMAGE           = 2048,
        LVCFMT_BITMAP_ON_RIGHT = 4096,
        LVCFMT_COL_HAS_IMAGES  = 32768
    }
}

enum {
    LVSCW_AUTOSIZE           = -1,
    LVSCW_AUTOSIZE_USEHEADER = -2
}

enum {
    LVN_ITEMCHANGING    = LVN_FIRST,
    LVN_ITEMCHANGED     = LVN_FIRST - 1,
    LVN_INSERTITEM      = LVN_FIRST - 2,
    LVN_DELETEITEM      = LVN_FIRST - 3,
    LVN_DELETEALLITEMS  = LVN_FIRST - 4,
    LVN_BEGINLABELEDITA = LVN_FIRST - 5,
    LVN_ENDLABELEDITA   = LVN_FIRST - 6,
    LVN_COLUMNCLICK     = LVN_FIRST - 8,
    LVN_BEGINDRAG       = LVN_FIRST - 9,
    LVN_BEGINRDRAG      = LVN_FIRST - 11,
    LVN_GETDISPINFOA    = LVN_FIRST - 50,
    LVN_SETDISPINFOA    = LVN_FIRST - 51,
    LVN_KEYDOWN         = LVN_FIRST - 55,
    LVN_BEGINLABELEDITW = LVN_FIRST - 75,
    LVN_ENDLABELEDITW   = LVN_FIRST - 76,
    LVN_GETDISPINFOW    = LVN_FIRST - 77,
    LVN_SETDISPINFOW    = LVN_FIRST - 78
}

static if (_WIN32_IE >= 0x400) {
    enum {
        LVN_MARQUEEBEGIN = LVN_FIRST - 56,
        LVN_GETINFOTIPA  = LVN_FIRST - 57,
        LVN_GETINFOTIPW  = LVN_FIRST - 58,
        LVKF_ALT         = 1,
        LVKF_CONTROL     = 2,
        LVKF_SHIFT       = 4,
        LVGIT_UNFOLDED   = 1
    }
}

enum {
    TVS_HASBUTTONS      = 1,
    TVS_HASLINES        = 2,
    TVS_LINESATROOT     = 4,
    TVS_EDITLABELS      = 8,
    TVS_DISABLEDRAGDROP = 16,
    TVS_SHOWSELALWAYS   = 32
}

static if (_WIN32_IE >= 0x300) {
    enum {
        TVS_RTLREADING  = 64,
        TVS_NOTOOLTIPS  = 128,
        TVS_CHECKBOXES  = 256,
        TVS_TRACKSELECT = 512
    }
}

static if (_WIN32_IE >= 0x400) {
    enum {
        TVS_SINGLEEXPAND  = 1024,
        TVS_INFOTIP       = 2048,
        TVS_FULLROWSELECT = 4096,
        TVS_NOSCROLL      = 8192,
        TVS_NONEVENHEIGHT = 16384
    }
}

static if (_WIN32_IE >= 0x500) {
    enum {
        TVS_NOHSCROLL = 0x8000
    }
}

enum {
    TVIF_TEXT          = 1,
    TVIF_IMAGE         = 2,
    TVIF_PARAM         = 4,
    TVIF_STATE         = 8,
    TVIF_HANDLE        = 16,
    TVIF_SELECTEDIMAGE = 32,
    TVIF_CHILDREN      = 64
}

static if (_WIN32_IE >= 0x400) {
    enum {
        TVIF_INTEGRAL = 0x0080
    }
}

enum {
    TVIS_FOCUSED        = 1,
    TVIS_SELECTED       = 2,
    TVIS_CUT            = 4,
    TVIS_DROPHILITED    = 8,
    TVIS_BOLD           = 16,
    TVIS_EXPANDED       = 32,
    TVIS_EXPANDEDONCE   = 64,
    TVIS_OVERLAYMASK    = 0xF00,
    TVIS_STATEIMAGEMASK = 0xF000,
    TVIS_USERMASK       = 0xF000
}

enum {
    I_CHILDRENCALLBACK = -1
}

alias HTREEITEM = HANDLE;
alias HIMAGELIST = HANDLE;

version (Win64)
{
enum HTREEITEM
    TVI_ROOT  = cast(HTREEITEM) cast(ULONG_PTR)-0x10000,
    TVI_FIRST = cast(HTREEITEM) cast(ULONG_PTR)-0xffff,
    TVI_LAST  = cast(HTREEITEM) cast(ULONG_PTR)-0xfffe,
    TVI_SORT  = cast(HTREEITEM) cast(ULONG_PTR)-0xfffd;
} else {
enum HTREEITEM
    TVI_ROOT  = cast(HTREEITEM) 0xFFFF0000,
    TVI_FIRST = cast(HTREEITEM) 0xFFFF0001,
    TVI_LAST  = cast(HTREEITEM) 0xFFFF0002,
    TVI_SORT  = cast(HTREEITEM) 0xFFFF0003;
}

enum {
    TVSIL_NORMAL = 0,
    TVSIL_STATE  = 2
}

enum {
    TVM_INSERTITEMA       = TV_FIRST,
    TVM_DELETEITEM        = TV_FIRST + 1,
    TVM_EXPAND            = TV_FIRST + 2,
    TVM_GETITEMRECT       = TV_FIRST + 4,
    TVM_GETCOUNT,
    TVM_GETINDENT,
    TVM_SETINDENT,
    TVM_GETIMAGELIST,
    TVM_SETIMAGELIST,
    TVM_GETNEXTITEM,
    TVM_SELECTITEM,
    TVM_GETITEMA,
    TVM_SETITEMA,
    TVM_EDITLABELA,
    TVM_GETEDITCONTROL,
    TVM_GETVISIBLECOUNT,
    TVM_HITTEST,
    TVM_CREATEDRAGIMAGE,
    TVM_SORTCHILDREN,
    TVM_ENSUREVISIBLE,
    TVM_SORTCHILDRENCB,
    TVM_ENDEDITLABELNOW,
    TVM_GETISEARCHSTRINGA, // = TV_FIRST + 23
    TVM_INSERTITEMW       = TV_FIRST + 50,
    TVM_GETITEMW          = TV_FIRST + 62,
    TVM_SETITEMW          = TV_FIRST + 63,
    TVM_GETISEARCHSTRINGW = TV_FIRST + 64,
    TVM_EDITLABELW        = TV_FIRST + 65
}

static if (_WIN32_IE >= 0x300) {
    enum {
        TVM_GETTOOLTIPS = TV_FIRST + 25,
        TVM_SETTOOLTIPS = TV_FIRST + 24
    }
}

static if (_WIN32_IE >= 0x400) {
    enum {
        TVM_SETINSERTMARK      = TV_FIRST + 26,
        TVM_SETITEMHEIGHT,
        TVM_GETITEMHEIGHT,
        TVM_SETBKCOLOR,
        TVM_SETTEXTCOLOR,
        TVM_GETBKCOLOR,
        TVM_GETTEXTCOLOR,
        TVM_SETSCROLLTIME,
        TVM_GETSCROLLTIME,  // = TV_FIRST + 34
        TVM_SETINSERTMARKCOLOR = TV_FIRST + 37,
        TVM_GETINSERTMARKCOLOR = TV_FIRST + 38,
        TVM_SETUNICODEFORMAT   = CCM_SETUNICODEFORMAT,
        TVM_GETUNICODEFORMAT   = CCM_GETUNICODEFORMAT
    }
}

static if (_WIN32_IE >= 0x500) {
    enum {
        TVM_GETITEMSTATE = TV_FIRST + 39,
        TVM_SETLINECOLOR = TV_FIRST + 40,
        TVM_GETLINECOLOR = TV_FIRST + 41
    }
}

static if (_WIN32_IE >= 0x501) {
    enum {
        TVM_MAPACCIDTOHTREEITEM = TV_FIRST + 42,
        TVM_MAPHTREEITEMTOACCID = TV_FIRST + 43,
        TVM_SETEXTENDEDSTYLE = TV_FIRST + 44,
        TVM_GETEXTENDEDSTYLE = TV_FIRST + 45,
        TVM_SETAUTOSCROLLINFO = TV_FIRST + 59
    }
}

static if (_WIN32_IE >= 0x600) {
    enum {
        TVM_GETSELECTEDCOUNT = TV_FIRST + 70,
        TVM_SHOWINFOTIP = TV_FIRST + 71,
        TVM_GETITEMPARTRECT = TV_FIRST + 72,
    }
}

enum {
    TVE_COLLAPSE      = 1,
    TVE_EXPAND        = 2,
    TVE_TOGGLE        = 3,
    TVE_COLLAPSERESET = 0x8000
}

static if (_WIN32_IE >= 0x300) {
    enum {
        TVE_EXPANDPARTIAL = 0x4000
    }
}

enum {
    TVC_UNKNOWN,
    TVC_BYMOUSE,
    TVC_BYKEYBOARD // = 2
}

enum {
    TVGN_ROOT,
    TVGN_NEXT,
    TVGN_PREVIOUS,
    TVGN_PARENT,
    TVGN_CHILD,
    TVGN_FIRSTVISIBLE,
    TVGN_NEXTVISIBLE,
    TVGN_PREVIOUSVISIBLE,
    TVGN_DROPHILITE,
    TVGN_CARET // = 9
}

static if (_WIN32_IE >= 0x400) {
    enum {
        TVGN_LASTVISIBLE = 10
    }
}

static if (_WIN32_IE >= 0x600) {
    enum {
        TVGN_NEXTSELECTED = 11
    }
}

enum {
    TVN_SELCHANGINGA    = TVN_FIRST - 1,
    TVN_SELCHANGEDA     = TVN_FIRST - 2,
    TVN_GETDISPINFOA    = TVN_FIRST - 3,
    TVN_SETDISPINFOA    = TVN_FIRST - 4,
    TVN_ITEMEXPANDINGA  = TVN_FIRST - 5,
    TVN_ITEMEXPANDEDA   = TVN_FIRST - 6,
    TVN_BEGINDRAGA      = TVN_FIRST - 7,
    TVN_BEGINRDRAGA     = TVN_FIRST - 8,
    TVN_DELETEITEMA     = TVN_FIRST - 9,
    TVN_BEGINLABELEDITA = TVN_FIRST - 10,
    TVN_ENDLABELEDITA   = TVN_FIRST - 11,
    TVN_KEYDOWN         = TVN_FIRST - 12,
    TVN_SELCHANGINGW    = TVN_FIRST - 50,
    TVN_SELCHANGEDW     = TVN_FIRST - 51,
    TVN_GETDISPINFOW    = TVN_FIRST - 52,
    TVN_SETDISPINFOW    = TVN_FIRST - 53,
    TVN_ITEMEXPANDINGW  = TVN_FIRST - 54,
    TVN_ITEMEXPANDEDW   = TVN_FIRST - 55,
    TVN_BEGINDRAGW      = TVN_FIRST - 56,
    TVN_BEGINRDRAGW     = TVN_FIRST - 57,
    TVN_DELETEITEMW     = TVN_FIRST - 58,
    TVN_BEGINLABELEDITW = TVN_FIRST - 59,
    TVN_ENDLABELEDITW   = TVN_FIRST - 60
}

static if (_WIN32_IE >= 0x400) {
    enum {
        TVNRET_DEFAULT   = 0,
        TVNRET_SKIPOLD   = 1,
        TVNRET_SKIPNEW   = 2,
        TVN_GETINFOTIPA  = TVN_FIRST - 13,
        TVN_GETINFOTIPW  = TVN_FIRST - 14,
        TVN_SINGLEEXPAND = TVN_FIRST - 15
    }
}

enum {
    TVIF_DI_SETITEM = 0x1000
}

enum {
    TVHT_NOWHERE         = 1,
    TVHT_ONITEMICON      = 2,
    TVHT_ONITEMLABEL     = 4,
    TVHT_ONITEMINDENT    = 8,
    TVHT_ONITEMBUTTON    = 16,
    TVHT_ONITEMRIGHT     = 32,
    TVHT_ONITEMSTATEICON = 64,
    TVHT_ABOVE           = 256,
    TVHT_BELOW           = 512,
    TVHT_TORIGHT         = 1024,
    TVHT_TOLEFT          = 2048,
    TCHT_NOWHERE         = 1,
    TCHT_ONITEMICON      = 2,
    TCHT_ONITEMLABEL     = 4,
    TVHT_ONITEM          = TVHT_ONITEMICON | TVHT_ONITEMLABEL
                           | TVHT_ONITEMSTATEICON,
    TCHT_ONITEM          = TCHT_ONITEMICON | TCHT_ONITEMLABEL
}

enum {
    TCS_TABS              = 0,
    TCS_RIGHTJUSTIFY      = 0,
    TCS_SINGLELINE        = 0,
    TCS_FORCEICONLEFT     = 16,
    TCS_FORCELABELLEFT    = 32,
    TCS_BUTTONS           = 256,
    TCS_MULTILINE         = 512,
    TCS_FIXEDWIDTH        = 1024,
    TCS_RAGGEDRIGHT       = 2048,
    TCS_FOCUSONBUTTONDOWN = 0x1000,
    TCS_OWNERDRAWFIXED    = 0x2000,
    TCS_TOOLTIPS          = 0x4000,
    TCS_FOCUSNEVER        = 0x8000
}

static if (_WIN32_IE >= 0x300) {
    enum {
        TCS_BOTTOM         = 2,
        TCS_RIGHT          = 2,
        TCS_VERTICAL       = 128,
        TCS_SCROLLOPPOSITE = 0x0001,
        TCS_HOTTRACK       = 0x0040,
        TCS_MULTISELECT    = 0x0004
    }
}

static if (_WIN32_IE >= 0x400) {
    enum {
        TCS_FLATBUTTONS       = 0x0008,
        TCS_EX_FLATSEPARATORS = 0x00000001,
        TCS_EX_REGISTERDROP   = 0x00000002
    }
}

enum {
    TCIF_TEXT       = 1,
    TCIF_IMAGE      = 2,
    TCIF_RTLREADING = 4,
    TCIF_PARAM      = 8
}

static if (_WIN32_IE >= 0x400) {
    enum {
        TCIF_STATE = 16
    }
}

enum {
    TCIS_BUTTONPRESSED = 1
}

static if (_WIN32_IE >= 0x400) {
    enum {
        TCIS_HIGHLIGHTED = 2
    }
}

enum {
    TCM_FIRST          = 0x1300,
    TCM_GETIMAGELIST   = TCM_FIRST + 2,
    TCM_SETIMAGELIST,
    TCM_GETITEMCOUNT,
    TCM_GETITEMA,
    TCM_SETITEMA,
    TCM_INSERTITEMA,
    TCM_DELETEITEM,
    TCM_DELETEALLITEMS,
    TCM_GETITEMRECT,
    TCM_GETCURSEL,
    TCM_SETCURSEL,
    TCM_HITTEST,
    TCM_SETITEMEXTRA, // = TCM_FIRST + 14
    TCM_ADJUSTRECT     = TCM_FIRST + 40,
    TCM_SETITEMSIZE,
    TCM_REMOVEIMAGE,
    TCM_SETPADDING,
    TCM_GETROWCOUNT,
    TCM_GETTOOLTIPS,
    TCM_SETTOOLTIPS,
    TCM_GETCURFOCUS,
    TCM_SETCURFOCUS,
    TCM_SETMINTABWIDTH,
    TCM_DESELECTALL, // = TCM_FIRST + 50
    TCM_GETITEMW       = TCM_FIRST + 60,
    TCM_SETITEMW       = TCM_FIRST + 61,
    TCM_INSERTITEMW    = TCM_FIRST + 62
}

static if (_WIN32_IE >=0x0400) {
    enum {
        TCM_HIGHLIGHTITEM    = TCM_FIRST + 51,
        TCM_SETEXTENDEDSTYLE = TCM_FIRST + 52,
        TCM_GETEXTENDEDSTYLE = TCM_FIRST + 53,
        TCM_SETUNICODEFORMAT = CCM_SETUNICODEFORMAT,
        TCM_GETUNICODEFORMAT = CCM_GETUNICODEFORMAT
    }
}

enum {
    TCN_KEYDOWN     = TCN_FIRST,
    TCN_SELCHANGE   = TCN_FIRST - 1,
    TCN_SELCHANGING = TCN_FIRST - 2
}

enum {
    NM_OUTOFMEMORY     = NM_FIRST - 1,
    NM_CLICK           = NM_FIRST - 2,
    NM_DBLCLK          = NM_FIRST - 3,
    NM_RETURN          = NM_FIRST - 4,
    NM_RCLICK          = NM_FIRST - 5,
    NM_RDBLCLK         = NM_FIRST - 6,
    NM_SETFOCUS        = NM_FIRST - 7,
    NM_KILLFOCUS       = NM_FIRST - 8,
    NM_CUSTOMDRAW      = NM_FIRST - 12,
    NM_HOVER           = NM_FIRST - 13,
    NM_NCHITTEST       = NM_FIRST - 14,
    NM_KEYDOWN         = NM_FIRST - 15,
    NM_RELEASEDCAPTURE = NM_FIRST - 16,
    NM_SETCURSOR       = NM_FIRST - 17,
    NM_CHAR            = NM_FIRST - 18,
    NM_TOOLTIPSCREATED = NM_FIRST - 19
}

enum {
    SBARS_SIZEGRIP = 256
}

/*enum {
    CCM_FIRST            = 0x2000,
    CCM_LAST             = CCM_FIRST + 0x200,
    CCM_SETBKCOLOR       = 8193,
    CCM_SETCOLORSCHEME   = 8194,
    CCM_GETCOLORSCHEME   = 8195,
    CCM_GETDROPTARGET    = 8196,
    CCM_SETUNICODEFORMAT = 8197,
    CCM_GETUNICODEFORMAT = 8198,
    CCM_SETVERSION       = 0x2007,
    CCM_GETVERSION       = 0x2008,
    CCM_SETNOTIFYWINDOW  = 0x2009
}*/

static if (_WIN32_WINNT >= 0x501) {
    enum {
        CCM_SETWINDOWTHEME = 0x200b,
        CCM_DPISCALE       = 0x200c,

        RB_GETBANDMARGINS = WM_USER + 40,
        RB_SETWINDOWTHEME = CCM_SETWINDOWTHEME,
        TB_SETWINDOWTHEME  = CCM_SETWINDOWTHEME,
        TTM_SETWINDOWTHEME = CCM_SETWINDOWTHEME,
    }
}

enum {
    ICC_LISTVIEW_CLASSES = 1,
    ICC_TREEVIEW_CLASSES = 2,
    ICC_BAR_CLASSES      = 4,
    ICC_TAB_CLASSES      = 8,
    ICC_UPDOWN_CLASS     = 16,
    ICC_PROGRESS_CLASS   = 32,
    ICC_HOTKEY_CLASS     = 64,
    ICC_ANIMATE_CLASS    = 128,
    ICC_WIN95_CLASSES    = 255,
    ICC_DATE_CLASSES     = 256,
    ICC_USEREX_CLASSES   = 512,
    ICC_COOL_CLASSES     = 1024
}

static if (_WIN32_IE >= 0x400) {
    enum {
        INFOTIPSIZE            = 1024,
        ICC_INTERNET_CLASSES   = 2048,
        ICC_PAGESCROLLER_CLASS = 4096,
        ICC_NATIVEFNTCTL_CLASS = 8192
    }
}

static if (_WIN32_WINNT >= 0x501) {
    enum {
        ICC_STANDARD_CLASSES = 0x00004000,
        ICC_LINK_CLASS       = 0x00008000
    }
}

enum {
    GDTR_MIN = 1,
    GDTR_MAX = 2
}

enum {
    GMR_VISIBLE,
    GMR_DAYSTATE
}

enum {
    GDT_ERROR = -1,
    GDT_VALID = 0,
    GDT_NONE  = 1
}

enum {
    DTS_SHORTDATEFORMAT = 0,
    DTS_UPDOWN          = 1,
    DTS_SHOWNONE        = 2,
    DTS_LONGDATEFORMAT  = 4,
    DTS_TIMEFORMAT      = 9,
    DTS_APPCANPARSE     = 16,
    DTS_RIGHTALIGN      = 32
}

static if (_WIN32_IE >= 0x500) {
    enum {
        DTS_SHORTDATECENTURYFORMAT = 0x000C
    }
}

enum {
    MCS_DAYSTATE    = 1,
    MCS_MULTISELECT = 2,
    MCS_WEEKNUMBERS = 4
}

static if (_WIN32_IE >= 0x400) {
    enum {
        MCS_NOTODAYCIRCLE = 0x0008,
        MCS_NOTODAY       = 0x0010
    }
} else {
    enum {
        MCS_NOTODAY = 0x0008
    }
}

enum {
    DTM_FIRST         = 0x1000,
    DTM_GETSYSTEMTIME = 0x1001,
    DTM_SETSYSTEMTIME = 0x1002,
    DTM_GETRANGE      = 0x1003,
    DTM_SETRANGE      = 0x1004,
    DTM_SETFORMATA    = 0x1005,
    DTM_SETMCCOLOR    = 0x1006,
    DTM_GETMCCOLOR    = 0x1007,
    DTM_GETMONTHCAL   = 0x1008,
    DTM_SETMCFONT     = 0x1009,
    DTM_GETMCFONT     = 0x100a,
    DTM_SETFORMATW    = 0x1050
}

static if (_WIN32_WINNT >= 0x600) {
    enum {
        DTM_SETMCSTYLE = DTM_FIRST + 11,
        DTM_GETMCSTYLE,
        DTM_CLOSEMONTHCAL,
        DTM_GETDATETIMEPICKERINFO,
        DTM_GETIDEALSIZE,
    }
}

enum {
    DTN_USERSTRINGA    = -758U,
    DTN_USERSTRINGW    = -745U,
    DTN_WMKEYDOWNA     = -757U,
    DTN_WMKEYDOWNW     = -744U,
    DTN_FORMATA        = -756U,
    DTN_FORMATW        = -743U,
    DTN_FORMATQUERYA   = -755U,
    DTN_FORMATQUERYW   = -742U,
    DTN_DROPDOWN       = -754U,
    DTN_CLOSEUP        = -753U,
    DTN_DATETIMECHANGE = -759U,
}

enum {
    MCM_FIRST             = 0x1000,
    MCM_GETCURSEL         = 0x1001,
    MCM_SETCURSEL         = 0x1002,
    MCM_GETMAXSELCOUNT    = 0x1003,
    MCM_SETMAXSELCOUNT    = 0x1004,
    MCM_GETSELRANGE       = 0x1005,
    MCM_SETSELRANGE       = 0x1006,
    MCM_GETMONTHRANGE     = 0x1007,
    MCM_SETDAYSTATE       = 0x1008,
    MCM_GETMINREQRECT     = 0x1009,
    MCM_SETCOLOR          = 0x100a,
    MCM_GETCOLOR          = 0x100b,
    MCM_SETTODAY          = 0x100c,
    MCM_GETTODAY          = 0x100d,
    MCM_HITTEST           = 0x100e,
    MCM_SETFIRSTDAYOFWEEK = 0x100f,
    MCM_GETFIRSTDAYOFWEEK = 0x1010,
    MCM_GETRANGE          = 0x1011,
    MCM_SETRANGE          = 0x1012,
    MCM_GETMONTHDELTA     = 0x1013,
    MCM_SETMONTHDELTA     = 0x1014,
    MCM_GETMAXTODAYWIDTH  = 0x1015,
    MCM_GETUNICODEFORMAT  = CCM_GETUNICODEFORMAT,
    MCM_SETUNICODEFORMAT  = CCM_SETUNICODEFORMAT
}

static if (_WIN32_WINNT >= 0x600) {
    enum {
        MCM_GETCURRENTVIEW = MCM_FIRST + 22,
        MCM_GETCALENDARCOUNT,
        MCM_GETCALENDARGRIDINFO,
        MCM_GETCALID = MCM_FIRST + 27,
        MCM_SETCALID,
        MCM_SIZERECTTOMIN,
        MCM_SETCALENDARBORDER,
        MCM_GETCALENDARBORDER,
        MCM_SETCURRENTVIEW,
    }
}

enum {
    MCN_SELCHANGE   = -749U,
    MCN_GETDAYSTATE = -747U,
    MCN_SELECT      = -746U
}

enum {
    ODT_HEADER = 100,
    ODT_TAB,
    ODT_LISTVIEW // = 102
}

enum {
    SB_SETBKCOLOR = 0x2001
}

static if (_WIN32_IE >= 0x300) {
    enum {
        SB_ISSIMPLE = 1038
    }

    enum {
        MCSC_BACKGROUND,
        MCSC_TEXT,
        MCSC_TITLEBK,
        MCSC_TITLETEXT,
        MCSC_MONTHBK,
        MCSC_TRAILINGTEXT // = 5
    }
}

static if (_WIN32_IE >= 0x400) {
    enum {
        MCHT_TITLE            = 0x10000,
        MCHT_CALENDAR         = 0x20000,
        MCHT_TODAYLINK        = 0x30000,
        MCHT_NEXT             = 0x1000000,
        MCHT_PREV             = 0x2000000,
        MCHT_NOWHERE          = 0x00,
        MCHT_TITLEBK          = MCHT_TITLE,
        MCHT_TITLEMONTH       = MCHT_TITLE | 0x0001,
        MCHT_TITLEYEAR        = MCHT_TITLE | 0x0002,
        MCHT_TITLEBTNNEXT     = MCHT_TITLE | MCHT_NEXT | 0x0003,
        MCHT_TITLEBTNPREV     = MCHT_TITLE | MCHT_PREV | 0x0003,
        MCHT_CALENDARBK       = MCHT_CALENDAR,
        MCHT_CALENDARDATE     = MCHT_CALENDAR | 0x0001,
        MCHT_CALENDARDATENEXT = MCHT_CALENDARDATE | MCHT_NEXT,
        MCHT_CALENDARDATEPREV = MCHT_CALENDARDATE | MCHT_PREV,
        MCHT_CALENDARDAY      = MCHT_CALENDAR | 0x0002,
        MCHT_CALENDARWEEKNUM  = MCHT_CALENDAR | 0x0003
    }
}

enum {
    RBS_TOOLTIPS    = 256,
    RBS_VARHEIGHT   = 512,
    RBS_BANDBORDERS = 1024,
    RBS_FIXEDORDER  = 2048
}

enum {
    RBIM_IMAGELIST = 1
}

enum {
    RB_SETCOLORSCHEME = CCM_SETCOLORSCHEME,
    RB_GETCOLORSCHEME = CCM_GETCOLORSCHEME
}

enum {
    RBBS_BREAK          = 0x0001,
    RBBS_FIXEDSIZE      = 0x0002,
    RBBS_CHILDEDGE      = 0x0004,
    RBBS_HIDDEN         = 0x0008,
    RBBS_NOVERT         = 0x0010,
    RBBS_FIXEDBMP       = 0x0020,
    RBBS_VARIABLEHEIGHT = 0x0040,
    RBBS_GRIPPERALWAYS  = 0x0080,
    RBBS_NOGRIPPER      = 0x0100
}

static if (_WIN32_IE >= 0x500) {
    enum {
        RBBS_USECHEVRON = 0x0200
    }
}

static if (_WIN32_IE >= 0x501) {
    enum {
        RBBS_HIDETITLE = 0x0400,
        RBBS_TOPALIGN  = 0x0800
    }
}

enum {
    RBBIM_STYLE      = 1,
    RBBIM_COLORS     = 2,
    RBBIM_TEXT       = 4,
    RBBIM_IMAGE      = 8,
    RBBIM_CHILD      = 16,
    RBBIM_CHILDSIZE  = 32,
    RBBIM_SIZE       = 64,
    RBBIM_BACKGROUND = 128,
    RBBIM_ID         = 256
}

enum {
    RB_INSERTBANDA  = WM_USER + 1,
    RB_DELETEBAND,
    RB_GETBARINFO,
    RB_SETBARINFO, // = WM_USER + 4
    RB_SETBANDINFOA = WM_USER + 6,
    RB_SETPARENT    = WM_USER + 7,
    RB_INSERTBANDW  = WM_USER + 10,
    RB_SETBANDINFOW,
    RB_GETBANDCOUNT,
    RB_GETROWCOUNT,
    RB_GETROWHEIGHT // = WM_USER + 14,
}

enum {
    RBN_HEIGHTCHANGE = RBN_FIRST
}

static if (_WIN32_IE >= 0x300) {
    enum {
        LVN_ODCACHEHINT    = LVN_FIRST - 13,
        LVN_ODFINDITEMA    = LVN_FIRST - 52,
        LVN_ODFINDITEMW    = LVN_FIRST - 79,
        LVN_ITEMACTIVATE   = LVN_FIRST - 14,
        LVN_ODSTATECHANGED = LVN_FIRST - 15
    }

    version (Unicode) {
        enum {
            LVN_ODFINDITEM = LVN_ODFINDITEMW
        }
    } else {
        enum {
            LVN_ODFINDITEM = LVN_ODFINDITEMA
        }
    }
}

static if (_WIN32_IE >= 0x400) {
    enum {
        SB_SETICON          = 1039,
        SB_SETTIPTEXTA,
        SB_SETTIPTEXTW,
        SB_GETTIPTEXTA,
        SB_GETTIPTEXTW,
        SB_GETICON,      // = 1044
        SB_SETUNICODEFORMAT = 0x2005,
        SB_GETUNICODEFORMAT = 0x2006
    }

    enum {
        PGF_INVISIBLE = 0,
        PGF_NORMAL    = 1,
        PGF_GRAYED    = 2,
        PGF_DEPRESSED = 4,
        PGF_HOT       = 8
    }

    enum {
        PGB_TOPORLEFT,
        PGB_BOTTOMORRIGHT
    }

    enum {
        PGF_SCROLLUP    = 1,
        PGF_SCROLLDOWN  = 2,
        PGF_SCROLLLEFT  = 4,
        PGF_SCROLLRIGHT = 8
    }

    enum {
        PGK_SHIFT   = 1,
        PGK_CONTROL = 2,
        PGK_MENU    = 4
    }

    enum {
        PGF_CALCWIDTH  = 1,
        PGF_CALCHEIGHT = 2
    }

    enum {
        PGM_FIRST    = 0x1400,
        PGM_SETCHILD = PGM_FIRST + 1,
        PGM_RECALCSIZE,
        PGM_FORWARDMOUSE,
        PGM_SETBKCOLOR,
        PGM_GETBKCOLOR,
        PGM_SETBORDER,
        PGM_GETBORDER,
        PGM_SETPOS,
        PGM_GETPOS,
        PGM_SETBUTTONSIZE,
        PGM_GETBUTTONSIZE,
        PGM_GETBUTTONSTATE, // = PGM_FIRST + 12
        PGM_GETDROPTARGET = CCM_GETDROPTARGET
    }

    enum {
        RBS_REGISTERDROP    = 4096,
        RBS_AUTOSIZE        = 8192,
        RBS_VERTICALGRIPPER = 16384,
        RBS_DBLCLKTOGGLE    = 32768
    }

    enum {
        RBBIM_IDEALSIZE  = 512,
        RBBIM_LPARAM     = 1024,
        RBBIM_HEADERSIZE = 2048
    }

    enum {
        RB_HITTEST          = WM_USER + 8,
        RB_GETRECT          = WM_USER + 9,
        RB_IDTOINDEX        = WM_USER + 16,
        RB_GETTOOLTIPS,
        RB_SETTOOLTIPS,
        RB_SETBKCOLOR,
        RB_GETBKCOLOR,
        RB_SETTEXTCOLOR,
        RB_GETTEXTCOLOR,
        RB_SIZETORECT,
        RB_BEGINDRAG,
        RB_ENDDRAG,
        RB_DRAGMOVE,
        RB_GETBARHEIGHT,
        RB_GETBANDINFOW,
        RB_GETBANDINFOA,
        RB_MINIMIZEBAND,
        RB_MAXIMIZEBAND, // = WM_USER + 31
        RB_GETDROPTARGET    = CCM_GETDROPTARGET,
        RB_GETBANDBORDERS   = WM_USER + 34,
        RB_SHOWBAND         = WM_USER + 35,
        RB_SETPALETTE       = WM_USER + 37,
        RB_GETPALETTE       = WM_USER + 38,
        RB_MOVEBAND         = WM_USER + 39,
        RB_SETUNICODEFORMAT = CCM_SETUNICODEFORMAT,
        RB_GETUNICODEFORMAT = CCM_GETUNICODEFORMAT
    }

    enum {
        RBN_GETOBJECT     = RBN_FIRST - 1,
        RBN_LAYOUTCHANGED = RBN_FIRST - 2,
        RBN_AUTOSIZE      = RBN_FIRST - 3,
        RBN_BEGINDRAG     = RBN_FIRST - 4,
        RBN_ENDDRAG       = RBN_FIRST - 5,
        RBN_DELETINGBAND  = RBN_FIRST - 6,
        RBN_DELETEDBAND   = RBN_FIRST - 7,
        RBN_CHILDSIZE     = RBN_FIRST - 8
    }

    enum {
        RBNM_ID     = 1,
        RBNM_STYLE  = 2,
        RBNM_LPARAM = 4
    }

    enum {
        RBHT_NOWHERE = 1,
        RBHT_CAPTION,
        RBHT_CLIENT,
        RBHT_GRABBER
    }

    version (Unicode) {
        alias SB_SETTIPTEXTW SB_SETTIPTEXT;
        alias SB_GETTIPTEXTW SB_GETTIPTEXT;
        alias RB_GETBANDINFOW RB_GETBANDINFO;
    } else {
        alias SB_SETTIPTEXTA SB_SETTIPTEXT;
        alias SB_GETTIPTEXTA SB_GETTIPTEXT;
        alias RB_GETBANDINFOA RB_GETBANDINFO;
    }
} else {
    enum {
        RB_GETBANDINFO = WM_USER + 5
    }
}

static if (_WIN32_IE >= 0x500) {
    enum {
        RB_PUSHCHEVRON = WM_USER + 43,
    }
}

static if (_WIN32_IE >= 0x600) {
    enum {
        RB_SETEXTENDEDSTYLE = WM_USER + 41,
        RB_GETEXTENDEDSTYLE = WM_USER + 42,
    }
}

static if (_WIN32_WINNT >= 0x500) {
    enum {
        RB_SETBANDWIDTH = WM_USER + 44,
    }
}

static if (_WIN32_WINNT >= 0x501) {
    enum {
        ECM_FIRST = 0x1500,
        BCM_FIRST = 0x1600,

        BCM_GETIDEALSIZE = BCM_FIRST + 0x0001,
        BCM_SETIMAGELIST = BCM_FIRST + 0x0002,
        BCM_GETIMAGELIST = BCM_FIRST + 0x0003,
        BCM_SETTEXTMARGIN = BCM_FIRST + 0x0004,
        BCM_GETTEXTMARGIN = BCM_FIRST + 0x0005,

        BCN_HOTITEMCHANGE = BCN_FIRST + 0x0001,
    }

    struct NMBCHOTITEM {
        NMHDR hdr;
        DWORD dwFlags;
    }
    alias NMBCHOTITEM* LPNMBCHOTITEM;
}

static if (_WIN32_WINNT >= 0x600) {
    enum {
        BST_DROPDOWNPUSHED      = 0x0400,

        BS_SPLITBUTTON          = 0x0000_000C,
        BS_DEFSPLITBUTTON       = 0x0000_000D,
        BS_COMMANDLINK          = 0x0000_000E,
        BS_DEFCOMMANDLINK       = 0x0000_000F,

        BCSIF_GLYPH             = 0x0001,
        BCSIF_IMAGE             = 0x0002,
        BCSIF_STYLE             = 0x0004,
        BCSIF_SIZE              = 0x0008,

        BCSS_NOSPLIT            = 0x0001,
        BCSS_STRETCH            = 0x0002,
        BCSS_ALIGNLEFT          = 0x0004,
        BCSS_IMAGE              = 0x0008,

        BCM_SETDROPDOWNSTATE = BCM_FIRST + 0x0006,
        BCM_SETSPLITINFO = BCM_FIRST + 0x0007,
        BCM_GETSPLITINFO = BCM_FIRST + 0x0008,
        BCM_SETNOTE = BCM_FIRST + 0x0009,
        BCM_GETNOTE = BCM_FIRST + 0x000A,
        BCM_GETNOTELENGTH = BCM_FIRST + 0x000B,
        BCM_SETSHIELD = BCM_FIRST + 0x000C,

        BCN_DROPDOWN = BCN_FIRST + 0x0002,
    }

enum HIMAGELIST BCCL_NOGLYPH = cast(HIMAGELIST)-1;

    struct BUTTON_SPLITINFO
    {
        UINT mask;
        HIMAGELIST himlGlyph;
        UINT uSplitStyle;
        SIZE size;
    }
    alias BUTTON_SPLITINFO* PBUTTON_SPLITINFO;
}

enum {
    CBEM_INSERTITEMA = WM_USER + 1,
    CBEM_SETIMAGELIST,
    CBEM_GETIMAGELIST,
    CBEM_GETITEMA,
    CBEM_SETITEMA,
    CBEM_GETCOMBOCONTROL,
    CBEM_GETEDITCONTROL,
    CBEM_SETEXSTYLE,
    CBEM_GETEXSTYLE, // = WM_USER + 9)
    CBEM_DELETEITEM  = CB_DELETESTRING
}

static if (_WIN32_IE >= 0x400) {
    enum {
        CBEM_SETEXTENDEDSTYLE = WM_USER + 14,
        CBEM_GETEXTENDEDSTYLE = WM_USER + 9,
        CBEM_SETUNICODEFORMAT = CCM_SETUNICODEFORMAT,
        CBEM_GETUNICODEFORMAT = CCM_GETUNICODEFORMAT
    }
}

enum {
    CBEM_HASEDITCHANGED = WM_USER + 10,
    CBEM_INSERTITEMW    = WM_USER + 11,
    CBEM_SETITEMW       = WM_USER + 12,
    CBEM_GETITEMW       = WM_USER + 13
}

static if (_WIN32_WINNT >= 0x501)
{
    enum {
        CBEM_SETWINDOWTHEME = CCM_SETWINDOWTHEME
    }
}

enum {
    DA_LAST = 0x7fffffff
}

enum {
    DPA_APPEND = 0x7fffffff,
    DPA_ERR    = -1
}

enum {
    DSA_APPEND = 0x7fffffff,
    DSA_ERR    = -1
}

enum {
    DPAS_SORTED       = 1,
    DPAS_INSERTBEFORE = 2,
    DPAS_INSERTAFTER  = 4
}

static if (_WIN32_IE >= 0x400) {
    enum {
        WSB_PROP_CYVSCROLL = 1,
        WSB_PROP_CXHSCROLL = 2,
        WSB_PROP_CYHSCROLL = 4,
        WSB_PROP_CXVSCROLL = 8,
        WSB_PROP_CXHTHUMB  = 16,
        WSB_PROP_CYVTHUMB  = 32,
        WSB_PROP_VBKGCOLOR = 64,
        WSB_PROP_HBKGCOLOR = 128,
        WSB_PROP_VSTYLE    = 256,
        WSB_PROP_HSTYLE    = 512,
        WSB_PROP_WINSTYLE  = 1024,
        WSB_PROP_PALETTE   = 2048,
        WSB_PROP_MASK      = 0xfff,
        FSB_FLAT_MODE      = 2,
        FSB_ENCARTA_MODE   = 1,
        FSB_REGULAR_MODE   = 0
    }
}

static if (_WIN32_WINNT >= 0x501) {
    enum {
        LIF_ITEMINDEX = 1,
        LIF_STATE     = 2,
        LIF_ITEMID    = 4,
        LIF_URL       = 8
    }

    enum {
        LIS_FOCUSED = 1,
        LIS_ENABLED = 2,
        LIS_VISITED = 4
    }

    enum {
        LM_HITTEST        = WM_USER + 768,
        LM_GETIDEALHEIGHT,
        LM_SETITEM,
        LM_GETITEM,     // = WM_USER + 771
        LM_GETIDEALSIZE = LM_GETIDEALHEIGHT,
    }

enum size_t MAX_LINKID_TEXT  =   48;
enum size_t L_MAX_URL_LENGTH = 2084;
}


struct TBMETRICS {
    UINT  cbSize = TBMETRICS.sizeof;
    DWORD dwMask;
    int   cxPad;
    int   cyPad;
    int   cxBarPad;
    int   cyBarPad;
    int   cxButtonSpacing;
    int   cyButtonSpacing;
}
alias TBMETRICS* LPTBMETRICS;

static if (_WIN32_WINNT >= 0x501) {
    struct TTGETTITLE {
        DWORD dwSize = TTGETTITLE.sizeof;
        UINT  uTitleBitmap;
        UINT  cch;
        WCHAR* pszTitle;
    }
    alias TTGETTITLE* PTTGETTITLE;
}

static if (_WIN32_WINNT >= 0x600) {
    struct MCGRIDINFO {
        UINT cbSize;
        DWORD dwPart;
        DWORD dwFlags;
        int iCalendar;
        int iRow;
        int iCol;
        BOOL bSelected;
        SYSTEMTIME stStart;
        SYSTEMTIME stEnd;
        RECT rc;
        PWSTR pszName;
        size_t cchName;
    }
    alias MCGRIDINFO* PMCGRIDINFO;

    struct DATETIMEPICKERINFO
    {
        DWORD cbSize;
        RECT rcCheck;
        DWORD stateCheck;
        RECT rcButton;
        DWORD stateButton;
        HWND hwndEdit;
        HWND hwndUD;
        HWND hwndDropDown;
    }
    alias DATETIMEPICKERINFO* LPDATETIMEPICKERINFO;
}

struct COMBOBOXEXITEMA {
    UINT   mask;
    INT_PTR iItem;
    LPSTR  pszText;
    int    cchTextMax;
    int    iImage;
    int    iSelectedImage;
    int    iOverlay;
    int    iIndent;
    LPARAM lParam;
}
alias COMBOBOXEXITEMA*        PCOMBOBOXEXITEMA;
alias const(COMBOBOXEXITEMA)* PCCOMBOEXITEMA;

struct COMBOBOXEXITEMW {
    UINT   mask;
    INT_PTR iItem;
    LPWSTR pszText;
    int    cchTextMax;
    int    iImage;
    int    iSelectedImage;
    int    iOverlay;
    int    iIndent;
    LPARAM lParam;
}
alias COMBOBOXEXITEMW*        PCOMBOBOXEXITEMW;
alias const(COMBOBOXEXITEMW)* PCCOMBOEXITEMW;

static if (_WIN32_IE >= 0x400) {
    struct NMCOMBOBOXEXA {
        NMHDR           hdr;
        COMBOBOXEXITEMA ceItem;
    }
    alias NMCOMBOBOXEXA* PNMCOMBOBOXEXA;

    struct NMCOMBOBOXEXW {
        NMHDR           hdr;
        COMBOBOXEXITEMW ceItem;
    }
    alias NMCOMBOBOXEXW* PNMCOMBOBOXEXW;

    struct NMCBEDRAGBEGINW {
        NMHDR hdr;
        int   iItemid;
        WCHAR[CBEMAXSTRLEN] szText = 0;
    }
    alias NMCBEDRAGBEGINW* LPNMCBEDRAGBEGINW, PNMCBEDRAGBEGINW;

    struct NMCBEDRAGBEGINA {
        NMHDR hdr;
        int   iItemid;
        char[CBEMAXSTRLEN] szText = 0;
    }
    alias NMCBEDRAGBEGINA* LPNMCBEDRAGBEGINA, PNMCBEDRAGBEGINA;

    struct NMIPADDRESS {
        NMHDR hdr;
        int   iField;
        int   iValue;
    }
    alias NMIPADDRESS* LPNMIPADDRESS;

    align (1)
    struct NMLVKEYDOWN {
    align (1):
        NMHDR hdr;
        WORD  wVKey;
        UINT  flags;
    }
    alias NMLVKEYDOWN* LPNMLVKEYDOWN;

    struct NMPGCALCSIZE {
        NMHDR hdr;
        DWORD dwFlag;
        int   iWidth;
        int   iHeight;
    }
    alias NMPGCALCSIZE* LPNMPGCALCSIZE;

    align (1)
    struct NMPGSCROLL {
    align (1):
        NMHDR hdr;
        WORD  fwKeys;   // Note: this should be WORD if MSDN document says wrong
        RECT  rcParent;
        int   iDir;
        int   iXpos;
        int   iYpos;
        int   iScroll;
    }
    alias NMPGSCROLL* LPNMPGSCROLL;

    struct NMSELCHANGE {
        NMHDR      nmhdr;
        SYSTEMTIME stSelStart;
        SYSTEMTIME stSelEnd;
    }
    alias NMSELCHANGE* LPNMSELCHANGE;

    struct NMTBHOTITEM {
        NMHDR hdr;
        int   idOld;
        int   idNew;
        DWORD dwFlags;
    }
    alias NMTBHOTITEM* LPNMTBHOTITEM;

    struct NMTBDISPINFOA {
        NMHDR     hdr;
        DWORD     dwMask;
        int       idCommand;
        DWORD_PTR lParam;
        int       iImage;
        LPSTR     pszText;
        int       cchText;
    }
    alias NMTBDISPINFOA* LPNMTBDISPINFOA;

    struct NMTBDISPINFOW {
        NMHDR     hdr;
        DWORD     dwMask;
        int       idCommand;
        DWORD_PTR lParam;
        int       iImage;
        LPWSTR    pszText;
        int       cchText;
    }
    alias NMTBDISPINFOW* LPNMTBDISPINFOW;

    struct NMTBGETINFOTIPA {
        NMHDR  hdr;
        LPSTR  pszText;
        int    cchTextMax;
        int    iItem;
        LPARAM lParam;
    }
    alias NMTBGETINFOTIPA* LPNMTBGETINFOTIPA;

    struct NMTBGETINFOTIPW {
        NMHDR  hdr;
        LPWSTR pszText;
        int    cchTextMax;
        int    iItem;
        LPARAM lParam;
    }
    alias NMTBGETINFOTIPW* LPNMTBGETINFOTIPW;

    struct NMMOUSE {
        NMHDR     hdr;
        DWORD_PTR dwItemSpec;
        DWORD_PTR dwItemData;
        POINT     pt;
        LPARAM    dwHitInfo;
    }
    alias NMMOUSE* LPNMMOUSE;
}

static if (_WIN32_IE >= 0x401) {
    struct NMTOOLTIPSCREATED {
        NMHDR hdr;
        HWND  hwndToolTips;
    }
    alias NMTOOLTIPSCREATED* LPNMTOOLTIPSCREATED;
}

struct NMDATETIMECHANGE {
    NMHDR      nmhdr;
    DWORD      dwFlags;
    SYSTEMTIME st;
}
alias NMDATETIMECHANGE* LPNMDATETIMECHANGE;

struct NMCBEENDEDITW {
    NMHDR hdr;
    BOOL  fChanged;
    int   iNewSelection;
    WCHAR[CBEMAXSTRLEN] szText = 0;
    int   iWhy;
}
alias NMCBEENDEDITW* LPNMCBEENDEDITW, PNMCBEENDEDITW;

struct NMCBEENDEDITA {
    NMHDR hdr;
    BOOL  fChanged;
    int   iNewSelection;
    char[CBEMAXSTRLEN] szText = 0;
    int   iWhy;
}
alias NMCBEENDEDITA* LPNMCBEENDEDITA, PNMCBEENDEDITA;

struct COLORMAP {
    COLORREF from;
    COLORREF to;
}
alias COLORMAP* LPCOLORMAP;

struct DRAGLISTINFO {
    UINT  uNotification;
    HWND  hWnd;
    POINT ptCursor;
}
alias DRAGLISTINFO* LPDRAGLISTINFO;

struct TBBUTTON {
    int   iBitmap;
    int   idCommand;
    BYTE  fsState;
    BYTE  fsStyle;
    version (Win64){
        BYTE[6] bReserved;
    } else {
        BYTE[2] bReserved;
    }
    DWORD_PTR dwData;
    INT_PTR iString;
}
alias TBBUTTON*        PTBBUTTON, LPTBBUTTON;
alias const(TBBUTTON)* LPCTBBUTTON;

static if (_WIN32_IE >= 0x400) {
    struct TBBUTTONINFOA {
        UINT  cbSize = TBBUTTONINFOA.sizeof;
        DWORD dwMask;
        int   idCommand;
        int   iImage;
        BYTE  fsState;
        BYTE  fsStyle;
        WORD  cx;
        DWORD_PTR lParam;
        LPSTR pszText;
        int   cchText;
    }
    alias TBBUTTONINFOA* LPTBBUTTONINFOA;

    struct TBBUTTONINFOW {
        UINT   cbSize = TBBUTTONINFOW.sizeof;
        DWORD  dwMask;
        int    idCommand;
        int    iImage;
        BYTE   fsState;
        BYTE   fsStyle;
        WORD   cx;
        DWORD_PTR lParam;
        LPWSTR pszText;
        int    cchText;
    }
    alias TBBUTTONINFOW* LPTBBUTTONINFOW;

    struct TBINSERTMARK {
        int   iButton;
        DWORD dwFlags;
    }
    alias TBINSERTMARK* LPTBINSERTMARK;

    struct LVBKIMAGEA {
        ULONG   ulFlags;
        HBITMAP hbm;
        LPSTR   pszImage;
        UINT    cchImageMax;
        int     xOffsetPercent;
        int     yOffsetPercent;
    }
    alias LVBKIMAGEA* LPLVBKIMAGEA;

    struct LVBKIMAGEW {
        ULONG   ulFlags;
        HBITMAP hbm;
        LPWSTR  pszImage;
        UINT    cchImageMax;
        int     xOffsetPercent;
        int     yOffsetPercent;
    }
    alias LVBKIMAGEW* LPLVBKIMAGEW;
}

/*struct TBNOTIFY {
    NMHDR    hdr;
    int      iItem;
    TBBUTTON tbButton;
    int      cchText;
    LPTSTR   pszText;
}
alias TBNOTIFY* LPTBNOTIFY;
*/

/*struct TBSAVEPARAMS {
    HKEY    hkr;
    LPCTSTR pszSubKey;
    LPCTSTR pszValueName;
}*/

struct IMAGEINFO {
    HBITMAP hbmImage;
    HBITMAP hbmMask;
    int     Unused1;
    int     Unused2;
    RECT    rcImage;
}
alias IMAGEINFO* LPIMAGEINFO;

static if (_WIN32_IE >= 0x500) {
    struct HDITEMA {
        UINT    mask;
        int     cxy;
        LPSTR   pszText;
        HBITMAP hbm;
        int     cchTextMax;
        int     fmt;
        LPARAM  lParam;
        int     iImage;
        int     iOrder;
        UINT    type;
        LPVOID  pvFilter;
    }

    struct HDITEMW {
        UINT    mask;
        int     cxy;
        LPWSTR  pszText;
        HBITMAP hbm;
        int     cchTextMax;
        int     fmt;
        LPARAM  lParam;
        int     iImage;
        int     iOrder;
        UINT    type;
        LPVOID  pvFilter;
    }
} else static if (_WIN32_IE >= 0x300) {
    struct HDITEMA {
        UINT    mask;
        int     cxy;
        LPSTR   pszText;
        HBITMAP hbm;
        int     cchTextMax;
        int     fmt;
        LPARAM  lParam;
        int     iImage;
        int     iOrder;
    }

    struct HDITEMW {
        UINT    mask;
        int     cxy;
        LPWSTR  pszText;
        HBITMAP hbm;
        int     cchTextMax;
        int     fmt;
        LPARAM  lParam;
        int     iImage;
        int     iOrder;
    }
} else {
    struct HDITEMA {
        UINT    mask;
        int     cxy;
        LPSTR   pszText;
        HBITMAP hbm;
        int     cchTextMax;
        int     fmt;
        LPARAM  lParam;
    }

    struct HDITEMW {
        UINT    mask;
        int     cxy;
        LPWSTR  pszText;
        HBITMAP hbm;
        int     cchTextMax;
        int     fmt;
        LPARAM  lParam;
    }
}
alias HDITEMA* LPHDITEMA;
alias HDITEMW* LPHDITEMW;

deprecated {
    alias HDITEMA HD_ITEMA;
    alias HDITEMW HD_ITEMW;
    //alias HDITEM HD_ITEM; fixme
}

struct HD_LAYOUT {
    RECT*      prc;
    WINDOWPOS* pwpos;
}
alias HD_LAYOUT* LPHDLAYOUT;
deprecated alias HD_LAYOUT HDLAYOUT;

struct HD_HITTESTINFO {
    POINT pt;
    UINT  flags;
    int   iItem;
}
alias HD_HITTESTINFO* LPHDHITTESTINFO;

struct HD_NOTIFYA {
    NMHDR    hdr;
    int      iItem;
    int      iButton;
    HDITEMA* pitem;
}

struct HD_NOTIFYW {
    NMHDR    hdr;
    int      iItem;
    int      iButton;
    HDITEMW* pitem;
}

/* FIXME: NMHEADER structure (base for all events of the comctl controls)
   is the same as HD_NOTIFY depending on the value of _WIN32_IE macro.
   I'm defining both for now. */
struct NMHEADERA {
    NMHDR    hdr;
    int      iItem;
    int      iButton;
    HDITEMA* pitem;
}
alias NMHEADERA* LPNMHEADERA;

struct NMHEADERW {
    NMHDR    hdr;
    int      iItem;
    int      iButton;
    HDITEMW* pitem;
}
alias NMHEADERW* LPNMHEADERW;

version (Unicode) {
    alias NMHEADERW NMHEADER;
    alias LPNMHEADERW LPNMHEADER;
} else {
    alias NMHEADERA NMHEADER;
    alias LPNMHEADERA LPNMHEADER;
}
// End FIXME

struct NMHDDISPINFOA {
    NMHDR  hdr;
    int    iItem;
    UINT   mask;
    LPSTR  pszText;
    int    cchTextMax;
    int    iImage;
    LPARAM lParam;
}
alias NMHDDISPINFOA* LPNMHDDISPINFOA;

struct NMHDDISPINFOW {
    NMHDR  hdr;
    int    iItem;
    UINT   mask;
    LPWSTR pszText;
    int    cchTextMax;
    int    iImage;
    LPARAM lParam;
}
alias NMHDDISPINFOW* LPNMHDDISPINFOW;

struct NMCUSTOMDRAW {
    NMHDR  hdr;
    DWORD  dwDrawStage;
    HDC    hdc;
    RECT   rc;
    DWORD_PTR dwItemSpec;
    UINT   uItemState;
    LPARAM lItemlParam;
}
alias NMCUSTOMDRAW* LPNMCUSTOMDRAW;

static if (_WIN32_IE >= 0x400) {
    struct NMLVCUSTOMDRAW {
        NMCUSTOMDRAW nmcd;
        COLORREF     clrText;
        COLORREF     clrTextBk;
        int          iSubItem;
    }
} else {
    struct NMLVCUSTOMDRAW {
        NMCUSTOMDRAW nmcd;
        COLORREF     clrText;
        COLORREF     clrTextBk;
    }
}
alias NMLVCUSTOMDRAW* LPNMLVCUSTOMDRAW;

static if (_WIN32_IE >= 0x400) {
    struct NMLVGETINFOTIPA {
        NMHDR  hdr;
        DWORD  dwFlags;
        LPSTR  pszText;
        int    cchTextMax;
        int    iItem;
        int    iSubItem;
        LPARAM lParam;
    }
    alias NMLVGETINFOTIPA* LPNMLVGETINFOTIPA;

    struct NMLVGETINFOTIPW {
        NMHDR  hdr;
        DWORD  dwFlags;
        LPWSTR pszText;
        int    cchTextMax;
        int    iItem;
        int    iSubItem;
        LPARAM lParam;
    }
    alias NMLVGETINFOTIPW* LPNMLVGETINFOTIPW;
}

static if (_WIN32_IE >= 0x400) {
    struct NMTVCUSTOMDRAW {
        NMCUSTOMDRAW nmcd;
        COLORREF     clrText;
        COLORREF     clrTextBk;
        int          iLevel;
    }
} else {
    struct NMTVCUSTOMDRAW {
        NMCUSTOMDRAW nmcd;
        COLORREF     clrText;
        COLORREF     clrTextBk;
    }
}
alias NMTVCUSTOMDRAW* LPNMTVCUSTOMDRAW;

static if (_WIN32_IE >= 0x400) {
    static if (_WIN32_WINNT >= 0x501) {
        struct NMTBCUSTOMDRAW {
            NMCUSTOMDRAW nmcd;
            HBRUSH       hbrMonoDither;
            HBRUSH       hbrLines;
            HPEN         hpenLines;
            COLORREF     clrText;
            COLORREF     clrMark;
            COLORREF     clrTextHighlight;
            COLORREF     clrBtnFace;
            COLORREF     clrBtnHighlight;
            COLORREF     clrHighlightHotTrack;
            RECT         rcText;
            int          nStringBkMode;
            int          nHLStringBkMode;
            int          iListGap;
        }
    } else {
        struct NMTBCUSTOMDRAW {
            NMCUSTOMDRAW nmcd;
            HBRUSH       hbrMonoDither;
            HBRUSH       hbrLines;
            HPEN         hpenLines;
            COLORREF     clrText;
            COLORREF     clrMark;
            COLORREF     clrTextHighlight;
            COLORREF     clrBtnFace;
            COLORREF     clrBtnHighlight;
            COLORREF     clrHighlightHotTrack;
            RECT         rcText;
            int          nStringBkMode;
            int          nHLStringBkMode;
        }
    }
    alias NMTBCUSTOMDRAW* LPNMTBCUSTOMDRAW;

    struct NMITEMACTIVATE {
        NMHDR  hdr;
        int    iItem;
        int    iSubItem;
        UINT   uNewState;
        UINT   uOldState;
        UINT   uChanged;
        POINT  ptAction;
        LPARAM lParam;
        UINT   uKeyFlags;
    }
    alias NMITEMACTIVATE* LPNMITEMACTIVATE;
}

struct TBADDBITMAP {
    HINSTANCE hInst;
    UINT_PTR  nID;
}
alias TBADDBITMAP* LPTBADDBITMAP;

struct TBSAVEPARAMSA {
    HKEY   hkr;
    LPCSTR pszSubKey;
    LPCSTR pszValueName;
}

struct TBSAVEPARAMSW {
    HKEY    hkr;
    LPCWSTR pszSubKey;
    LPCWSTR pszValueName;
}

struct TBREPLACEBITMAP {
    HINSTANCE hInstOld;
    UINT_PTR  nIDOld;
    HINSTANCE hInstNew;
    UINT_PTR  nIDNew;
    int       nButtons;
}
alias TBREPLACEBITMAP* LPTBREPLACEBITMAP;

static if (_WIN32_IE >= 0x500) {
    struct NMTOOLBARA {
        NMHDR    hdr;
        int      iItem;
        TBBUTTON tbButton;
        int      cchText;
        LPSTR    pszText;
        RECT     rcButton;
    }

    struct NMTOOLBARW {
        NMHDR    hdr;
        int      iItem;
        TBBUTTON tbButton;
        int      cchText;
        LPWSTR   pszText;
        RECT     rcButton;
    }
} else {
    struct NMTOOLBARA {
        NMHDR    hdr;
        int      iItem;
        TBBUTTON tbButton;
        int      cchText;
        LPSTR    pszText;
    }

    struct NMTOOLBARW {
        NMHDR    hdr;
        int      iItem;
        TBBUTTON tbButton;
        int      cchText;
        LPWSTR   pszText;
    }
}
alias NMTOOLBARA* LPNMTOOLBARA;
alias NMTOOLBARW* LPNMTOOLBARW;

alias NMTOOLBARA TBNOTIFYA;
alias LPNMTOOLBARA LPTBNOTIFYA;

alias NMTOOLBARW TBNOTIFYW;
alias LPNMTOOLBARW LPTBNOTIFYW;

static if (_WIN32_WINNT >= 0x501) {
    struct TOOLINFOA {
        UINT      cbSize = TOOLINFOA.sizeof;
        UINT      uFlags;
        HWND      hwnd;
        UINT_PTR  uId;
        RECT      rect;
        HINSTANCE hinst;
        LPSTR     lpszText;
        LPARAM    lParam;
        void*     lpReserved;
    }

    struct TOOLINFOW {
        UINT      cbSize = TOOLINFOW.sizeof;
        UINT      uFlags;
        HWND      hwnd;
        UINT_PTR  uId;
        RECT      rect;
        HINSTANCE hinst;
        LPWSTR    lpszText;
        LPARAM    lParam;
        void*     lpReserved;
    }

enum size_t
        TTTOOLINFOA_V1_SIZE = TOOLINFOA.lParam.offsetof,
        TTTOOLINFOW_V1_SIZE = TOOLINFOW.lParam.offsetof,
        TTTOOLINFOA_V2_SIZE = TOOLINFOA.lpReserved.offsetof,
        TTTOOLINFOW_V2_SIZE = TOOLINFOW.lpReserved.offsetof,
        TTTOOLINFOA_V3_SIZE = TOOLINFOA.sizeof,
        TTTOOLINFOW_V3_SIZE = TOOLINFOW.sizeof;
} else static if (_WIN32_IE >= 0x300) {
    struct TOOLINFOA {
        UINT      cbSize = TOOLINFOA.sizeof;
        UINT      uFlags;
        HWND      hwnd;
        UINT      uId;
        RECT      rect;
        HINSTANCE hinst;
        LPSTR     lpszText;
        LPARAM    lParam;
    }

    struct TOOLINFOW {
        UINT      cbSize = TOOLINFOW.sizeof;
        UINT      uFlags;
        HWND      hwnd;
        UINT      uId;
        RECT      rect;
        HINSTANCE hinst;
        LPWSTR    lpszText;
        LPARAM    lParam;
    }

enum size_t
        TTTOOLINFOA_V1_SIZE = TOOLINFOA.lParam.offsetof,
        TTTOOLINFOW_V1_SIZE = TOOLINFOW.lParam.offsetof,
        TTTOOLINFOA_V2_SIZE = TOOLINFOA.sizeof,
        TTTOOLINFOW_V2_SIZE = TOOLINFOW.sizeof;
} else {
    struct TOOLINFOA {
        UINT      cbSize = TOOLINFOA.sizeof;
        UINT      uFlags;
        HWND      hwnd;
        UINT      uId;
        RECT      rect;
        HINSTANCE hinst;
        LPSTR     lpszText;
    }

    struct TOOLINFOW {
        UINT      cbSize = TOOLINFOW.sizeof;
        UINT      uFlags;
        HWND      hwnd;
        UINT      uId;
        RECT      rect;
        HINSTANCE hinst;
        LPWSTR    lpszText;
    }

enum size_t
        TTTOOLINFOA_V1_SIZE = TOOLINFOA.sizeof,
        TTTOOLINFOW_V1_SIZE = TOOLINFOW.sizeof;
}
alias TOOLINFOA TTTOOLINFOA;
alias TOOLINFOW TTTOOLINFOW;
alias TTTOOLINFOA* LPTTTOOLINFOA, PTOOLINFOA, LPTOOLINFOA;
alias TTTOOLINFOW* LPTTTOOLINFOW, PTOOLINFOW, LPTOOLINFOW;

struct TTHITTESTINFOA {
    HWND      hwnd;
    POINT     pt;
    TOOLINFOA ti;
}
alias TTHITTESTINFOA* LPTTHITTESTINFOA, LPHITTESTINFOA;

struct TTHITTESTINFOW {
    HWND      hwnd;
    POINT     pt;
    TOOLINFOW ti;
}
alias TTHITTESTINFOW* LPTTHITTESTINFOW, LPHITTESTINFOW;

static if (_WIN32_IE >= 0x300) {
    struct NMTTDISPINFOA {
        NMHDR     hdr;
        LPSTR     lpszText;
        char[80]  szText = 0;
        HINSTANCE hinst;
        UINT      uFlags;
        LPARAM    lParam;
    }

    struct NMTTDISPINFOW {
        NMHDR     hdr;
        LPWSTR    lpszText;
        WCHAR[80] szText = 0;
        HINSTANCE hinst;
        UINT      uFlags;
        LPARAM    lParam;
    }
} else {
    struct NMTTDISPINFOA {
        NMHDR     hdr;
        LPSTR     lpszText;
        char[80]  szText = 0;
        HINSTANCE hinst;
        UINT      uFlags;
    }

    struct NMTTDISPINFOW {
        NMHDR     hdr;
        LPWSTR    lpszText;
        WCHAR[80] szText = 0;
        HINSTANCE hinst;
        UINT      uFlags;
    }
}
alias NMTTDISPINFOA* LPNMTTDISPINFOA;
alias NMTTDISPINFOW* LPNMTTDISPINFOW;
alias NMTTDISPINFOA TOOLTIPTEXTA;
alias LPNMTTDISPINFOA LPTOOLTIPTEXTA;
alias NMTTDISPINFOW TOOLTIPTEXTW;
alias LPNMTTDISPINFOW LPTOOLTIPTEXTW;

struct UDACCEL {
    UINT nSec;
    UINT nInc;
}
alias UDACCEL* LPUDACCEL;

struct NMUPDOWN {
    NMHDR hdr;
    int   iPos;
    int   iDelta;
}
alias NMUPDOWN* LPNMUPDOWN;

deprecated {
    alias NMUPDOWN NM_UPDOWN;
    alias LPNMUPDOWN LPNM_UPDOWN;
}

static if (_WIN32_WINNT >= 0x501) {
    struct LVITEMA {
        UINT   mask;
        int    iItem;
        int    iSubItem;
        UINT   state;
        UINT   stateMask;
        LPSTR  pszText;
        int    cchTextMax;
        int    iImage;
        LPARAM lParam;
        int    iIndent;
        int    iGroupId;
        UINT   cColumns;
        PUINT  puColumns;
    }

    struct LVITEMW {
        UINT   mask;
        int    iItem;
        int    iSubItem;
        UINT   state;
        UINT   stateMask;
        LPWSTR pszText;
        int    cchTextMax;
        int    iImage;
        LPARAM lParam;
        int    iIndent;
        int    iGroupId;
        UINT   cColumns;
        PUINT  puColumns;
    }
} else static if (_WIN32_IE >= 0x300) {
    struct LVITEMA {
        UINT   mask;
        int    iItem;
        int    iSubItem;
        UINT   state;
        UINT   stateMask;
        LPSTR  pszText;
        int    cchTextMax;
        int    iImage;
        LPARAM lParam;
        int    iIndent;
    }

    struct LVITEMW {
        UINT   mask;
        int    iItem;
        int    iSubItem;
        UINT   state;
        UINT   stateMask;
        LPWSTR pszText;
        int    cchTextMax;
        int    iImage;
        LPARAM lParam;
        int    iIndent;
    }
} else {
    struct LVITEMA {
        UINT   mask;
        int    iItem;
        int    iSubItem;
        UINT   state;
        UINT   stateMask;
        LPSTR  pszText;
        int    cchTextMax;
        int    iImage;
        LPARAM lParam;
    }

    struct LVITEMW {
        UINT   mask;
        int    iItem;
        int    iSubItem;
        UINT   state;
        UINT   stateMask;
        LPWSTR pszText;
        int    cchTextMax;
        int    iImage;
        LPARAM lParam;
    }
}
alias LVITEMA* LPLVITEMA;
alias LVITEMW* LPLVITEMW;
alias LVITEMA LV_ITEMA;
alias LVITEMW LV_ITEMW;

struct LVFINDINFOA {
    UINT   flags;
    LPCSTR psz;
    LPARAM lParam;
    POINT  pt;
    UINT   vkDirection;
}

struct LVFINDINFOW {
    UINT    flags;
    LPCWSTR psz;
    LPARAM  lParam;
    POINT   pt;
    UINT    vkDirection;
}

alias LVFINDINFOA* LPFINDINFOA;
alias LVFINDINFOA LV_FINDINFOA;
alias LVFINDINFOW* LPFINDINFOW;
alias LVFINDINFOW LV_FINDINFOW;

struct NMLVFINDITEMA {
    NMHDR       hdr;
    int         iStart;
    LVFINDINFOA lvfi;
}

struct NMLVFINDITEMW {
    NMHDR       hdr;
    int         iStart;
    LVFINDINFOW lvfi;
}

alias NMLVFINDITEMA* PNMLVFINDITEMA, LPNMLVFINDITEMA;
alias NMLVFINDITEMW* PNMLVFINDITEMW, LPNMLVFINDITEMW;

static if (_WIN32_IE >= 0x300) {
    struct LVHITTESTINFO {
        POINT pt;
        UINT  flags;
        int   iItem;
        int   iSubItem;
    }
} else {
    struct LVHITTESTINFO {
        POINT pt;
        UINT  flags;
        int   iItem;
    }
}
alias LVHITTESTINFO* LPLVHITTESTINFO;
alias LVHITTESTINFO LV_HITTESTINFO;

static if (_WIN32_IE >= 0x300) {
    struct LVCOLUMNA {
        UINT  mask;
        int   fmt;
        int   cx;
        LPSTR pszText;
        int   cchTextMax;
        int   iSubItem;
        int   iImage;
        int   iOrder;
    }
    struct LVCOLUMNW {
        UINT   mask;
        int    fmt;
        int    cx;
        LPWSTR pszText;
        int    cchTextMax;
        int    iSubItem;
        int    iImage;
        int    iOrder;
    }
} else {
    struct LVCOLUMNA {
        UINT  mask;
        int   fmt;
        int   cx;
        LPSTR pszText;
        int   cchTextMax;
        int   iSubItem;
    }
    struct LVCOLUMNW {
        UINT   mask;
        int    fmt;
        int    cx;
        LPWSTR pszText;
        int    cchTextMax;
        int    iSubItem;
    }
}
alias LVCOLUMNA* LPLVCOLUMNA;
alias LVCOLUMNW* LPLVCOLUMNW;
alias LVCOLUMNA LV_COLUMNA;
alias LVCOLUMNW LV_COLUMNW;

static if (_WIN32_WINNT >= 0x501) {
    /*  SG: The definitions in this static if block are from the MSDN docs.
     *  They are not in MinGW, but nonetheless required for macros that are.
     */
    struct LVGROUP {
        UINT    cbSize = LVGROUP.sizeof;
        UINT    mask;
        LPWSTR  pszHeader;
        int     cchHeader;
        LPWSTR  pszFooter;
        int     cchFooter;
        int     iGroupId;
        UINT    stateMask;
        UINT    state;
        UINT    uAlign;
        static if (_WIN32_WINNT >= 0x600) {
            LPWSTR  pszSubtitle;
            UINT    cchSubtitle;
            LPWSTR  pszTask;
            UINT    cchTask;
            LPWSTR  pszDescriptionTop;
            UINT    cchDescriptionTop;
            LPWSTR  pszDescriptionBottom;
            UINT    cchDescriptionBottom;
            int     iTitleImage;
            int     iExtendedImage;
            int     iFirstItem;         // Read only
            UINT    cItems;             // Read only
            LPWSTR  pszSubsetTitle;     // NULL if group is not subset
            UINT    cchSubsetTitle;
        }
    }
    alias LVGROUP* PLVGROUP;

    struct LVGROUPMETRICS {
        UINT     cbSize = LVGROUPMETRICS.sizeof;
        UINT     mask;
        UINT     Left;
        UINT     Top;
        UINT     Right;
        UINT     Bottom;
        COLORREF crLeft;
        COLORREF crTop;
        COLORREF crRight;
        COLORREF crBottom;
        COLORREF crHeader;
        COLORREF crFooter;
    }
    alias LVGROUPMETRICS* PLVGROUPMETRICS;

    struct LVINSERTMARK {
        UINT  cbSize = LVINSERTMARK.sizeof;
        DWORD dwFlags;
        int   iItem;
        DWORD dwReserved;
    }
    alias LVINSERTMARK* PLVINSERTMARK;
    alias LVINSERTMARK* LPLVINSERTMARK;

    struct LVTILEINFO {
        UINT     cbSize = LVTILEINFO.sizeof;
        int      iItem;
        UINT     cColumns;
        PUINT    puColumns;
        static if (_WIN32_WINNT >= 0x600) {
            int* piColFmt;
        }
    }
    alias LVTILEINFO* PLVTILEINFO;

    struct LVTILEVIEWINFO {
        UINT  cbSize = LVTILEVIEWINFO.sizeof;
        DWORD dwMask;
        DWORD dwFlags;
        SIZE  sizeTile;
        int   cLines;
        RECT  rcLabelMargin;
    }
    alias LVTILEVIEWINFO* PLVTILEVIEWINFO;

    struct LVINSERTGROUPSORTED {
        PFNLVGROUPCOMPARE pfnGroupCompare;
        LPVOID* pvData;
        LVGROUP lvGroup;
    }
    alias LVINSERTGROUPSORTED* PLVINSERTGROUPSORTED;

    extern (Windows) alias int function(INT, INT, VOID*) PFNLVGROUPCOMPARE;

    struct LVSETINFOTIP {
        UINT    cbSize = LVSETINFOTIP.sizeof;
        DWORD   dwFlags;
        LPWSTR  pszText;
        int     iItem;
        int     iSubItem;
        HBITMAP hbmp;
    }
    alias LVSETINFOTIP* PLVSETINFOTIP;

    struct BUTTON_IMAGELIST {
        HIMAGELIST himl;
        RECT margin;
        UINT uAlign;
    }
    alias BUTTON_IMAGELIST* PBUTTON_IMAGELIST;
}

static if (_WIN32_WINNT >= 0x600) {
    struct LVITEMINDEX
    {
        int iItem;
        int iGroup;
    }
    alias LVITEMINDEX* PLVITEMINDEX;

    struct LVFOOTERINFO
    {
        UINT mask;
        LPWSTR pszText;
        int cchTextMax;
        UINT cItems;
    }
    alias LVFOOTERINFO* LPLVFOOTERINFO;

    struct LVFOOTERITEM
    {
        UINT mask;
        int iItem;
        LPWSTR pszText;
        int cchTextMax;
        UINT state;
        UINT stateMask;
    }
    alias LVFOOTERITEM *LPLVFOOTERITEM;

    alias UINT TVITEMPART;
    enum {
        TVGIPR_BUTTON  = 0x0001,
    }
}

extern (Windows) alias int function(LPARAM, LPARAM, LPARAM) PFNLVCOMPARE;

struct NMLISTVIEW {
    NMHDR  hdr;
    int    iItem;
    int    iSubItem;
    UINT   uNewState;
    UINT   uOldState;
    UINT   uChanged;
    POINT  ptAction;
    LPARAM lParam;
}
alias NMLISTVIEW* LPNMLISTVIEW;

deprecated {
    alias NMLISTVIEW NM_LISTVIEW;
    alias LPNMLISTVIEW LPNM_LISTVIEW;
}

struct NMLVDISPINFOA {
    NMHDR    hdr;
    LV_ITEMA item;
}
alias NMLVDISPINFOA* LPNMLVDISPINFOA;
alias NMLVDISPINFOA LV_DISPINFOA;

struct NMLVDISPINFOW {
    NMHDR    hdr;
    LV_ITEMW item;
}
alias NMLVDISPINFOW* LPNMLVDISPINFOW;
alias NMLVDISPINFOW LV_DISPINFOW;

align (1)
struct LV_KEYDOWN {
align (1):
    NMHDR hdr;
    WORD  wVKey;
    UINT  flags;
}

struct NMLVCACHEHINT {
    NMHDR hdr;
    int   iFrom;
    int   iTo;
}
alias NMLVCACHEHINT* LPNMLVCACHEHINT, PNM_CACHEHINT, LPNM_CACHEHINT;
alias NMLVCACHEHINT NM_CACHEHINT;

struct TVITEMA {
    UINT      mask;
    HTREEITEM hItem;
    UINT      state;
    UINT      stateMask;
    LPSTR     pszText;
    int       cchTextMax;
    int       iImage;
    int       iSelectedImage;
    int       cChildren;
    LPARAM    lParam;
}
alias TVITEMA* LPTVITEMA, LPTV_ITEMA;
alias TVITEMA TV_ITEMA;

struct TVITEMW {
    UINT      mask;
    HTREEITEM hItem;
    UINT      state;
    UINT      stateMask;
    LPWSTR    pszText;
    int       cchTextMax;
    int       iImage;
    int       iSelectedImage;
    int       cChildren;
    LPARAM    lParam;
}
alias TVITEMW* LPTVITEMW, LPTV_ITEMW;
alias TVITEMW TV_ITEMW;

static if (_WIN32_IE >= 0x400) {
    struct TVITEMEXA {
        UINT      mask;
        HTREEITEM hItem;
        UINT      state;
        UINT      stateMask;
        LPSTR     pszText;
        int       cchTextMax;
        int       iImage;
        int       iSelectedImage;
        int       cChildren;
        LPARAM    lParam;
        int       iIntegral;
    }
    alias TVITEMEXA* LPTVITEMEXA;

    struct TVITEMEXW {
        UINT      mask;
        HTREEITEM hItem;
        UINT      state;
        UINT      stateMask;
        LPWSTR    pszText;
        int       cchTextMax;
        int       iImage;
        int       iSelectedImage;
        int       cChildren;
        LPARAM    lParam;
        int       iIntegral;
    }
    alias TVITEMEXW* LPTVITEMEXW;
}

static if (_WIN32_IE >= 0x400) {
    struct TVINSERTSTRUCTA {
        HTREEITEM hParent;
        HTREEITEM hInsertAfter;
        union {
            TVITEMEXA itemex;
            TV_ITEMA  item;
        }
    }

    struct TVINSERTSTRUCTW {
        HTREEITEM hParent;
        HTREEITEM hInsertAfter;
        union {
            TVITEMEXW itemex;
            TV_ITEMW  item;
        }
    }
} else {
    struct TVINSERTSTRUCTA {
        HTREEITEM hParent;
        HTREEITEM hInsertAfter;
        TV_ITEMA  item;
    }

    struct TVINSERTSTRUCTW {
        HTREEITEM hParent;
        HTREEITEM hInsertAfter;
        TV_ITEMW  item;
    }
}
alias TVINSERTSTRUCTA* LPTVINSERTSTRUCTA, LPTV_INSERTSTRUCTA;
alias TVINSERTSTRUCTA TV_INSERTSTRUCTA;
alias TVINSERTSTRUCTW* LPTVINSERTSTRUCTW, LPTV_INSERTSTRUCTW;
alias TVINSERTSTRUCTW TV_INSERTSTRUCTW;

struct TVHITTESTINFO {
    POINT     pt;
    UINT      flags;
    HTREEITEM hItem;
}
alias TVHITTESTINFO* LPTVHITTESTINFO, LPTV_HITTESTINFO;
alias TVHITTESTINFO TV_HITTESTINFO;

static if (_WIN32_WINNT >= 0x600) {
    struct TVGETITEMPARTRECTINFO {
        HTREEITEM hti;
        RECT*     prc;
        TVITEMPART partID;
    }
}

extern (Windows) alias int function(LPARAM, LPARAM, LPARAM) PFNTVCOMPARE;
struct TVSORTCB {
    HTREEITEM    hParent;
    PFNTVCOMPARE lpfnCompare;
    LPARAM       lParam;
}
alias TVSORTCB* LPTVSORTCB, LPTV_SORTCB;
alias TVSORTCB TV_SORTCB;

struct NMTREEVIEWA {
    NMHDR    hdr;
    UINT     action;
    TV_ITEMA itemOld;
    TV_ITEMA itemNew;
    POINT    ptDrag;
}
alias NMTREEVIEWA* LPNMTREEVIEWA, LPNM_TREEVIEWA;
alias NMTREEVIEWA NM_TREEVIEWA;

struct NMTREEVIEWW {
    NMHDR    hdr;
    UINT     action;
    TV_ITEMW itemOld;
    TV_ITEMW itemNew;
    POINT    ptDrag;
}
alias NMTREEVIEWW* LPNMTREEVIEWW, LPNM_TREEVIEWW;
alias NMTREEVIEWW NM_TREEVIEWW;

struct NMTVDISPINFOA {
    NMHDR   hdr;
    TVITEMA item;
}
alias NMTVDISPINFOA* LPNMTVDISPINFOA;
alias NMTVDISPINFOA TV_DISPINFOA;

struct NMTVDISPINFOW {
    NMHDR   hdr;
    TVITEMW item;
}
alias NMTVDISPINFOW* LPNMTVDISPINFOW;
alias NMTVDISPINFOW TV_DISPINFOW;

static if (_WIN32_IE >= 0x400) {
    struct NMTVGETINFOTIPA {
        NMHDR     hdr;
        LPSTR     pszText;
        int       cchTextMax;
        HTREEITEM hItem;
        LPARAM    lParam;
    }
    alias NMTVGETINFOTIPA* LPNMTVGETINFOTIPA;

    struct NMTVGETINFOTIPW {
        NMHDR     hdr;
        LPWSTR    pszText;
        int       cchTextMax;
        HTREEITEM hItem;
        LPARAM    lParam;
    }
    alias NMTVGETINFOTIPW* LPNMTVGETINFOTIPW;
}

align (1)
struct TV_KEYDOWN {
align (1):
    NMHDR hdr;
    WORD  wVKey;
    UINT  flags;
}

struct TC_ITEMHEADERA {
    UINT  mask;
    UINT  lpReserved1;
    UINT  lpReserved2;
    LPSTR pszText;
    int   cchTextMax;
    int   iImage;
}

struct TC_ITEMHEADERW {
    UINT   mask;
    UINT   lpReserved1;
    UINT   lpReserved2;
    LPWSTR pszText;
    int    cchTextMax;
    int    iImage;
}

static if (_WIN32_IE >= 0x300) {
    struct TCITEMA {
        UINT   mask;
        DWORD  dwState;
        DWORD  dwStateMask;
        LPSTR  pszText;
        int    cchTextMax;
        int    iImage;
        LPARAM lParam;
    }

    struct TCITEMW {
        UINT   mask;
        DWORD  dwState;
        DWORD  dwStateMask;
        LPWSTR pszText;
        int    cchTextMax;
        int    iImage;
        LPARAM lParam;
    }
} else {
    struct TCITEMA {
        UINT   mask;
        UINT   lpReserved1;
        UINT   lpReserved2;
        LPSTR  pszText;
        int    cchTextMax;
        int    iImage;
        LPARAM lParam;
    }

    struct TCITEMW {
        UINT   mask;
        UINT   lpReserved1;
        UINT   lpReserved2;
        LPWSTR pszText;
        int    cchTextMax;
        int    iImage;
        LPARAM lParam;
    }
}
alias TCITEMA* LPTCITEMA;
alias TCITEMA TC_ITEMA;
alias TCITEMW* LPTCITEMW;
alias TCITEMW TC_ITEMW;

struct TCHITTESTINFO {
    POINT pt;
    UINT  flags;
}
alias TCHITTESTINFO* LPTCHITTESTINFO, LPTC_HITTESTINFO;
alias TCHITTESTINFO TC_HITTESTINFO;

align (1)
struct TC_KEYDOWN {
align (1):
    NMHDR hdr;
    WORD wVKey;
    UINT flags;
}

static if (_WIN32_IE >= 0x300) {
    struct INITCOMMONCONTROLSEX {
        DWORD dwSize = INITCOMMONCONTROLSEX.sizeof;
        DWORD dwICC;
    }
    alias INITCOMMONCONTROLSEX* LPINITCOMMONCONTROLSEX;
}

struct PBRANGE {
    int iLow;
    int iHigh;
}
alias PBRANGE* PPBRANGE;

struct COLORSCHEME {
    DWORD    dwSize = COLORSCHEME.sizeof;
    COLORREF clrBtnHighlight;
    COLORREF clrBtnShadow;
}
alias COLORSCHEME* LPCOLORSCHEME;

struct MCHITTESTINFO {
    UINT       cbSize = MCHITTESTINFO.sizeof;
    POINT      pt;
    UINT       uHit;
    SYSTEMTIME st;
}
alias MCHITTESTINFO* PMCHITTESTINFO;

alias DWORD MONTHDAYSTATE;
alias MONTHDAYSTATE* LPMONTHDAYSTATE;

struct NMDAYSTATE {
    NMHDR           nmhdr;
    SYSTEMTIME      stStart;
    int             cDayState;
    LPMONTHDAYSTATE prgDayState;
}
alias NMDAYSTATE* LPNMDAYSTATE;

struct REBARINFO {
    UINT       cbSize = REBARINFO.sizeof;
    UINT       fMask;
    HIMAGELIST himl;
}
alias REBARINFO* LPREBARINFO;

static if (_WIN32_IE >= 0x400) {
    struct REBARBANDINFOA {
        UINT     cbSize = REBARBANDINFOA.sizeof;
        UINT     fMask;
        UINT     fStyle;
        COLORREF clrFore;
        COLORREF clrBack;
        LPSTR    lpText;
        UINT     cch;
        int      iImage;
        HWND     hwndChild;
        UINT     cxMinChild;
        UINT     cyMinChild;
        UINT     cx;
        HBITMAP  hbmBack;
        UINT     wID;
        UINT     cyChild;
        UINT     cyMaxChild;
        UINT     cyIntegral;
        UINT     cxIdeal;
        LPARAM   lParam;
        UINT     cxHeader;
    }

    struct REBARBANDINFOW {
        UINT     cbSize = REBARBANDINFOW.sizeof;
        UINT     fMask;
        UINT     fStyle;
        COLORREF clrFore;
        COLORREF clrBack;
        LPWSTR   lpText;
        UINT     cch;
        int      iImage;
        HWND     hwndChild;
        UINT     cxMinChild;
        UINT     cyMinChild;
        UINT     cx;
        HBITMAP  hbmBack;
        UINT     wID;
        UINT     cyChild;
        UINT     cyMaxChild;
        UINT     cyIntegral;
        UINT     cxIdeal;
        LPARAM   lParam;
        UINT     cxHeader;
    }

    enum : size_t {
        REBARBANDINFOA_V3_SIZE = REBARBANDINFOA.cyChild.offsetof,
        REBARBANDINFOW_V3_SIZE = REBARBANDINFOW.cyChild.offsetof
    }
} else {
    struct REBARBANDINFOA {
        UINT     cbSize = REBARBANDINFOA.sizeof;
        UINT     fMask;
        UINT     fStyle;
        COLORREF clrFore;
        COLORREF clrBack;
        LPSTR    lpText;
        UINT     cch;
        int      iImage;
        HWND     hwndChild;
        UINT     cxMinChild;
        UINT     cyMinChild;
        UINT     cx;
        HBITMAP  hbmBack;
        UINT     wID;
    }

    struct REBARBANDINFOW {
        UINT     cbSize = REBARBANDINFOW.sizeof;
        UINT     fMask;
        UINT     fStyle;
        COLORREF clrFore;
        COLORREF clrBack;
        LPWSTR   lpText;
        UINT     cch;
        int      iImage;
        HWND     hwndChild;
        UINT     cxMinChild;
        UINT     cyMinChild;
        UINT     cx;
        HBITMAP  hbmBack;
        UINT     wID;
    }

    enum : size_t {
        REBARBANDINFOA_V3_SIZE = REBARBANDINFOA.sizeof,
        REBARBANDINFOW_V3_SIZE = REBARBANDINFOW.sizeof
    }
}
alias REBARBANDINFOA*        LPREBARBANDINFOA;
alias const(REBARBANDINFOA)* LPCREBARBANDINFOA;
alias REBARBANDINFOW*        LPREBARBANDINFOW;
alias const(REBARBANDINFOW)* LPCREBARBANDINFOW;

static if (_WIN32_IE >= 0x300) {
    struct NMLVODSTATECHANGE {
        NMHDR hdr;
        int iFrom;
        int iTo;
        UINT uNewState;
        UINT uOldState;
    }
    alias NMLVODSTATECHANGE* LPNMLVODSTATECHANGE;

    static if (_WIN32_WINNT >= 0x501) {
        struct IMAGELISTDRAWPARAMS {
            DWORD      cbSize = IMAGELISTDRAWPARAMS.sizeof;
            HIMAGELIST himl;
            int        i;
            HDC        hdcDst;
            int        x;
            int        y;
            int        cx;
            int        cy;
            int        xBitmap;
            int        yBitmap;
            COLORREF   rgbBk;
            COLORREF   rgbFg;
            UINT       fStyle;
            DWORD      dwRop;
            DWORD      fState;
            DWORD      Frame;
            COLORREF   crEffect;
        }
    } else {
        struct IMAGELISTDRAWPARAMS {
            DWORD      cbSize = IMAGELISTDRAWPARAMS.sizeof;
            HIMAGELIST himl;
            int        i;
            HDC        hdcDst;
            int        x;
            int        y;
            int        cx;
            int        cy;
            int        xBitmap;
            int        yBitmap;
            COLORREF   rgbBk;
            COLORREF   rgbFg;
            UINT       fStyle;
            DWORD      dwRop;
        }
    }
    alias IMAGELISTDRAWPARAMS* LPIMAGELISTDRAWPARAMS;
}

static if (_WIN32_IE >= 0x400) {
    struct NMREBARCHILDSIZE {
        NMHDR hdr;
        UINT  uBand;
        UINT  wID;
        RECT  rcChild;
        RECT  rcBand;
    }
    alias NMREBARCHILDSIZE* LPNMREBARCHILDSIZE;

    struct NMREBAR {
        NMHDR  hdr;
        DWORD  dwMask;
        UINT   uBand;
        UINT   fStyle;
        UINT   wID;
        LPARAM lParam;
    }
    alias NMREBAR* LPNMREBAR;

    struct NMRBAUTOSIZE {
        NMHDR hdr;
        BOOL  fChanged;
        RECT  rcTarget;
        RECT  rcActual;
    }
    alias NMRBAUTOSIZE* LPNMRBAUTOSIZE;

    static if (_WIN32_IE >= 0x500) {
        struct NMREBARCHEVRON {
            NMHDR  hdr;
            UINT   uBand;
            UINT   wID;
            LPARAM lParam;
            RECT   rc;
            LPARAM lParamNM;
        }
        alias NMREBARCHEVRON* LPNMREBARCHEVRON;
    }

    struct RBHITTESTINFO {
        POINT pt;
        UINT  flags;
        int   iBand;
    }
    alias RBHITTESTINFO* LPRBHITTESTINFO;
}

alias HDSA = HANDLE;
alias HDPA = HANDLE;

version (Unicode) {
    alias HDITEMW HDITEM;
    alias LPHDITEMW LPHDITEM;
    alias TOOLINFOW TOOLINFO;
    alias TOOLINFOW* PTOOLINFO, LPTOOLINFO;
    alias TTHITTESTINFOW TTHITTESTINFO;
    alias TTHITTESTINFOW* LPHITTESTINFO, LPTTHITTESTINFO;
    alias TOOLTIPTEXTW TOOLTIPTEXT;
    alias TOOLTIPTEXTW* LPTOOLTIPTEXT;
    alias NMTTDISPINFOW NMTTDISPINFO;
    alias NMTTDISPINFOW* LPNMTTDISPINFO;
    alias TV_ITEMW TV_ITEM;
    alias TV_ITEMW* LPTV_ITEM;
    alias TVITEMW TVITEM;
    alias TVITEMW* LPTVITEM;

    static if (_WIN32_IE >= 0x400) {
        alias TVITEMEXW TVITEMEX;
        alias TVITEMEXW* LPTVITEMEX;
    }

    alias TV_INSERTSTRUCTW TV_INSERTSTRUCT;
    alias TV_INSERTSTRUCTW* LPTV_INSERTSTRUCT;
    alias TVINSERTSTRUCTW TVINSERTSTRUCT;
    alias TVINSERTSTRUCTW* LPTVINSERTSTRUCT;
    alias NM_TREEVIEWW NM_TREEVIEW;
    alias NM_TREEVIEWW* LPNM_TREEVIEW;
    alias NMTREEVIEWW NMTREEVIEW;
    alias NMTREEVIEWW* LPNMTREEVIEW;
    alias NMHDDISPINFOW NMHDDISPINFO;
    alias NMHDDISPINFOW* LPNMHDDISPINFO;

    alias ACM_OPENW ACM_OPEN;
    alias COMBOBOXEXITEMW COMBOBOXEXITEM;
    alias PCOMBOBOXEXITEMW PCOMBOBOXEXITEM;
    //alias PCCOMBOBOXEXITEMW PCCOMBOBOXEXITEM; fixme
    alias CBEM_INSERTITEMW CBEM_INSERTITEM;
    alias CBEM_SETITEMW CBEM_SETITEM;
    alias CBEM_GETITEMW CBEM_GETITEM;
    alias CBEN_ENDEDITW CBEN_ENDEDIT;
    alias NMCBEENDEDITW NMCBEENDEDIT;
    alias LPNMCBEENDEDITW LPNMCBEENDEDIT;
    alias PNMCBEENDEDITW PNMCBEENDEDIT;

    static if (_WIN32_IE >= 0x400) {
        alias NMCOMBOBOXEXW NMCOMBOBOXEX;
        alias PNMCOMBOBOXEXW PNMCOMBOBOXEX;
        alias CBEN_GETDISPINFOW CBEN_GETDISPINFO;
        alias CBEN_DRAGBEGINW CBEN_DRAGBEGIN;
        alias NMCBEDRAGBEGINW NMCBEDRAGBEGIN;
        alias LPNMCBEDRAGBEGINW LPNMCBEDRAGBEGIN;
        alias PNMCBEDRAGBEGINW PNMCBEDRAGBEGIN;
    }

    alias SB_GETTEXTW SB_GETTEXT;
    alias SB_SETTEXTW SB_SETTEXT;
    alias SB_GETTEXTLENGTHW SB_GETTEXTLENGTH;
    alias HDM_INSERTITEMW HDM_INSERTITEM;
    alias HDM_GETITEMW HDM_GETITEM;
    alias HDM_SETITEMW HDM_SETITEM;
    alias HDN_ITEMCHANGINGW HDN_ITEMCHANGING;
    alias HDN_ITEMCHANGEDW HDN_ITEMCHANGED;
    alias HDN_ITEMCLICKW HDN_ITEMCLICK;
    alias HDN_ITEMDBLCLICKW HDN_ITEMDBLCLICK;
    alias HDN_DIVIDERDBLCLICKW HDN_DIVIDERDBLCLICK;
    alias HDN_BEGINTRACKW HDN_BEGINTRACK;
    alias HDN_ENDTRACKW HDN_ENDTRACK;
    alias HDN_TRACKW HDN_TRACK;

    static if (_WIN32_IE >= 0x300) {
        alias HDN_GETDISPINFOW HDN_GETDISPINFO;
    }

    alias HD_NOTIFYW HD_NOTIFY;
    alias TBSAVEPARAMSW TBSAVEPARAMS;
    alias TB_GETBUTTONTEXTW TB_GETBUTTONTEXT;
    alias TB_SAVERESTOREW TB_SAVERESTORE;
    alias TB_ADDSTRINGW TB_ADDSTRING;

    static if (_WIN32_IE >= 0x400) {
        alias TBN_GETBUTTONINFOW TBN_GETBUTTONINFO;  // fixme
        alias TB_GETBUTTONINFOW TB_GETBUTTONINFO;
        alias TB_SETBUTTONINFOW TB_SETBUTTONINFO;
        alias TB_INSERTBUTTONW TB_INSERTBUTTON;
        alias TB_ADDBUTTONSW TB_ADDBUTTONS;
        alias TB_MAPACCELERATORW TB_MAPACCELERATOR;
        alias TB_GETSTRINGW TB_GETSTRING;
        alias TBBUTTONINFOW TBBUTTONINFO;
        alias LPTBBUTTONINFOW LPTBBUTTONINFO;
        alias TBN_GETDISPINFOW TBN_GETDISPINFO;
        alias NMTBDISPINFOW NMTBDISPINFO;
        alias LPNMTBDISPINFOW LPNMTBDISPINFO;
        alias NMTBGETINFOTIPW NMTBGETINFOTIP;
        alias LPNMTBGETINFOTIPW LPNMTBGETINFOTIP;
    }

    alias TBNOTIFYW TBNOTIFY;
    alias LPTBNOTIFYW LPTBNOTIFY;
    alias NMTOOLBARW NMTOOLBAR;
    alias LPNMTOOLBARW LPNMTOOLBAR;
    alias TTM_ADDTOOLW TTM_ADDTOOL;
    alias TTM_DELTOOLW TTM_DELTOOL;
    alias TTM_NEWTOOLRECTW TTM_NEWTOOLRECT;
    alias TTM_GETTOOLINFOW TTM_GETTOOLINFO;
    alias TTM_SETTOOLINFOW TTM_SETTOOLINFO;
    alias TTM_HITTESTW TTM_HITTEST;
    alias TTM_GETTEXTW TTM_GETTEXT;
    alias TTM_UPDATETIPTEXTW TTM_UPDATETIPTEXT;
    alias TTM_ENUMTOOLSW TTM_ENUMTOOLS;
    alias TTM_GETCURRENTTOOLW TTM_GETCURRENTTOOL;
    alias TTN_NEEDTEXTW TTN_NEEDTEXT;
    alias TTN_GETDISPINFOW TTN_GETDISPINFO;
    //alias SB_GETTEXTW SB_GETTEXT;
    //alias SB_SETTEXTW SB_SETTEXT;
    //alias SB_GETTEXTLENGTHW SB_GETTEXTLENGTH;
    alias LV_ITEMW LV_ITEM;
    alias LVITEMW LVITEM;
    alias LVITEM* LPLVITEM;
    alias LPSTR_TEXTCALLBACKW LPSTR_TEXTCALLBACK;

    static if (_WIN32_IE >= 0x400) {
        alias LVBKIMAGEW LVBKIMAGE;
        alias LPLVBKIMAGEW LPLVBKIMAGE;
        alias LVM_SETBKIMAGEW LVM_SETBKIMAGE;
        alias LVM_GETBKIMAGEW LVM_GETBKIMAGE;
    }

    alias LVM_GETITEMW LVM_GETITEM;
    alias LVM_SETITEMW LVM_SETITEM;
    alias LVM_INSERTITEMW LVM_INSERTITEM;
    alias LV_FINDINFOW LV_FINDINFO;
    alias LVFINDINFOW LVFINDINFO;
    alias LPFINDINFOW LPFINDINFO;
    alias NMLVFINDITEMW NMLVFINDITEM;
    alias PNMLVFINDITEMW PNMLVFINDITEM;
    alias LPNMLVFINDITEMW LPNMLVFINDITEM;
    alias LVM_FINDITEMW LVM_FINDITEM;
    alias LVM_GETSTRINGWIDTHW LVM_GETSTRINGWIDTH;
    alias LVM_EDITLABELW LVM_EDITLABEL;
    alias LV_COLUMNW LV_COLUMN;
    alias LVCOLUMNW LVCOLUMN;
    alias LVCOLUMNW* LPLVCOLUMN;
    alias LVM_GETCOLUMNW LVM_GETCOLUMN;
    alias LVM_SETCOLUMNW LVM_SETCOLUMN;
    alias LVM_INSERTCOLUMNW LVM_INSERTCOLUMN;
    alias LVM_GETITEMTEXTW LVM_GETITEMTEXT;
    alias LVM_SETITEMTEXTW LVM_SETITEMTEXT;
    alias LVM_GETISEARCHSTRINGW LVM_GETISEARCHSTRING;
    alias LVN_BEGINLABELEDITW LVN_BEGINLABELEDIT;
    alias LVN_ENDLABELEDITW LVN_ENDLABELEDIT;
    alias LVN_GETDISPINFOW LVN_GETDISPINFO;
    alias LVN_SETDISPINFOW LVN_SETDISPINFO;

    static if (_WIN32_IE >= 0x400) {
        alias LVN_GETINFOTIPW LVN_GETINFOTIP;
        alias NMLVGETINFOTIPW NMLVGETINFOTIP;
        alias LPNMLVGETINFOTIPW LPNMLVGETINFOTIP;
    }

    alias LV_DISPINFOW LV_DISPINFO;
    alias NMLVDISPINFOW NMLVDISPINFO;
    alias LPNMLVDISPINFOW LPNMLVDISPINFO;
    alias TVM_INSERTITEMW TVM_INSERTITEM;
    alias TVM_GETITEMW TVM_GETITEM;
    alias TVM_SETITEMW TVM_SETITEM;
    alias TVM_EDITLABELW TVM_EDITLABEL;
    alias TVM_GETISEARCHSTRINGW TVM_GETISEARCHSTRING;
    alias NMTVDISPINFOW TV_DISPINFO;
    alias NMTVDISPINFOW NMTVDISPINFO;
    alias LPNMTVDISPINFOW LPNMTVDISPINFO;

    static if (_WIN32_IE >= 0x400) {
        alias NMTVGETINFOTIPW NMTVGETINFOTIP;
        alias LPNMTVGETINFOTIPW LPNMTVGETINFOTIP;
        alias TVN_GETINFOTIPW TVN_GETINFOTIP;
    }

    alias TVN_SELCHANGINGW TVN_SELCHANGING;
    alias TVN_SELCHANGEDW TVN_SELCHANGED;
    alias TVN_GETDISPINFOW TVN_GETDISPINFO;
    alias TVN_SETDISPINFOW TVN_SETDISPINFO;
    alias TVN_ITEMEXPANDINGW TVN_ITEMEXPANDING;
    alias TVN_ITEMEXPANDEDW TVN_ITEMEXPANDED;
    alias TVN_BEGINDRAGW TVN_BEGINDRAG;
    alias TVN_BEGINRDRAGW TVN_BEGINRDRAG;
    alias TVN_DELETEITEMW TVN_DELETEITEM;
    alias TVN_BEGINLABELEDITW TVN_BEGINLABELEDIT;
    alias TVN_ENDLABELEDITW TVN_ENDLABELEDIT;
    alias TC_ITEMHEADERW TC_ITEMHEADER;
    alias TC_ITEMW TC_ITEM;
    alias TCITEMW TCITEM;
    alias LPTCITEMW LPTCITEM;
    alias TCM_GETITEMW TCM_GETITEM;
    alias TCM_SETITEMW TCM_SETITEM;
    alias TCM_INSERTITEMW TCM_INSERTITEM;
    alias CreateStatusWindowW CreateStatusWindow;
    alias DrawStatusTextW DrawStatusText;
    alias ImageList_LoadImageW ImageList_LoadImage;
    alias DTM_SETFORMATW DTM_SETFORMAT;
    alias DTN_USERSTRINGW DTN_USERSTRING;
    alias DTN_WMKEYDOWNW DTN_WMKEYDOWN;
    alias DTN_FORMATW DTN_FORMAT;
    alias DTN_FORMATQUERYW DTN_FORMATQUERY;
    alias REBARBANDINFOW REBARBANDINFO;
    alias REBARBANDINFO* LPREBARBANDINFO;
    alias LPCREBARBANDINFOW LPCREBARBANDINFO;
    alias REBARBANDINFOW_V3_SIZE REBARBANDINFO_V3_SIZE;
    alias RB_INSERTBANDW RB_INSERTBAND;
    alias RB_SETBANDINFOW RB_SETBANDINFO;
} else {
    alias HDITEMA HDITEM;
    alias LPHDITEMA LPHDITEM;
    alias TOOLINFOA TOOLINFO;
    alias TOOLINFOA* PTOOLINFO, LPTOOLINFO;
    alias TTHITTESTINFOA TTHITTESTINFO;
    alias TTHITTESTINFOA* LPHITTESTINFO, LPTTHITTESTINFO;
    alias TOOLTIPTEXTA TOOLTIPTEXT;
    alias TOOLTIPTEXTA* LPTOOLTIPTEXT;
    alias NMTTDISPINFOA NMTTDISPINFO;
    alias NMTTDISPINFOA* LPNMTTDISPINFO;
    alias TV_ITEMA TV_ITEM;
    alias TV_ITEMA* LPTV_ITEM;
    alias TVITEMA TVITEM;
    alias TVITEMA* LPTVITEM;

    static if (_WIN32_IE >= 0x400) {
        alias TVITEMEXA TVITEMEX;
        alias TVITEMEXA* LPTVITEMEX;
    }

    alias TV_INSERTSTRUCTA TV_INSERTSTRUCT;
    alias TV_INSERTSTRUCTA* LPTV_INSERTSTRUCT;
    alias TVINSERTSTRUCTA TVINSERTSTRUCT;
    alias TVINSERTSTRUCTA* LPTVINSERTSTRUCT;
    alias NM_TREEVIEWA NM_TREEVIEW;
    alias NM_TREEVIEWA* LPNM_TREEVIEW;
    alias NMTREEVIEWA NMTREEVIEW;
    alias NMTREEVIEWA* LPNMTREEVIEW;
    alias NMHDDISPINFOW NMHDDISPINFO;
    alias NMHDDISPINFOW* LPNMHDDISPINFO;

    alias ACM_OPENA ACM_OPEN;
    alias COMBOBOXEXITEMA COMBOBOXEXITEM;
    alias PCOMBOBOXEXITEMA PCOMBOBOXEXITEM;
    //alias PCCOMBOBOXEXITEMA PCCOMBOBOXEXITEM; fixme
    alias CBEM_INSERTITEMA CBEM_INSERTITEM;
    alias CBEM_SETITEMA CBEM_SETITEM;
    alias CBEM_GETITEMA CBEM_GETITEM;
    alias CBEN_ENDEDITA CBEN_ENDEDIT;
    alias NMCBEENDEDITA NMCBEENDEDIT;
    alias LPNMCBEENDEDITA LPNMCBEENDEDIT;
    alias PNMCBEENDEDITA PNMCBEENDEDIT;

    static if (_WIN32_IE >= 0x400) {
        alias TB_GETBUTTONINFOA TB_GETBUTTONINFO;
        alias TB_SETBUTTONINFOA TB_SETBUTTONINFO;
        alias TB_INSERTBUTTONA TB_INSERTBUTTON;
        alias TB_ADDBUTTONSA TB_ADDBUTTONS;
        alias TB_MAPACCELERATORA TB_MAPACCELERATOR;
        alias TB_GETSTRINGA TB_GETSTRING;
        alias NMCOMBOBOXEXA NMCOMBOBOXEX;
        alias PNMCOMBOBOXEXA PNMCOMBOBOXEX;
        alias CBEN_DRAGBEGINA CBEN_DRAGBEGIN;
        alias CBEN_GETDISPINFOA CBEN_GETDISPINFO;
        alias NMCBEDRAGBEGINA NMCBEDRAGBEGIN;
        alias LPNMCBEDRAGBEGINA LPNMCBEDRAGBEGIN;
        alias PNMCBEDRAGBEGINA PNMCBEDRAGBEGIN;
        alias TBN_GETDISPINFOA TBN_GETDISPINFO;
        alias NMTBDISPINFOA NMTBDISPINFO;
        alias LPNMTBDISPINFOA LPNMTBDISPINFO;
        alias NMTBGETINFOTIPA NMTBGETINFOTIP;
        alias LPNMTBGETINFOTIPA LPNMTBGETINFOTIP;
    }

    alias SB_GETTEXTA SB_GETTEXT;
    alias SB_SETTEXTA SB_SETTEXT;
    alias SB_GETTEXTLENGTHA SB_GETTEXTLENGTH;
    alias HDM_INSERTITEMA HDM_INSERTITEM;
    alias HDM_GETITEMA HDM_GETITEM;
    alias HDM_SETITEMA HDM_SETITEM;
    alias HDN_ITEMCHANGINGA HDN_ITEMCHANGING;
    alias HDN_ITEMCHANGEDA HDN_ITEMCHANGED;
    alias HDN_ITEMCLICKA HDN_ITEMCLICK;
    alias HDN_ITEMDBLCLICKA HDN_ITEMDBLCLICK;
    alias HDN_DIVIDERDBLCLICKA HDN_DIVIDERDBLCLICK;
    alias HDN_BEGINTRACKA HDN_BEGINTRACK;
    alias HDN_ENDTRACKA HDN_ENDTRACK;
    alias HDN_TRACKA HDN_TRACK;

    static if (_WIN32_IE >= 0x300) {
        alias HDN_GETDISPINFOA HDN_GETDISPINFO;
    }

    alias HD_NOTIFYA HD_NOTIFY;
    alias TBSAVEPARAMSA TBSAVEPARAMS;
    alias TB_GETBUTTONTEXTA TB_GETBUTTONTEXT;
    alias TB_SAVERESTOREA TB_SAVERESTORE;
    alias TB_ADDSTRINGA TB_ADDSTRING;
    alias TBN_GETBUTTONINFOA TBN_GETBUTTONINFO;

    static if (_WIN32_IE >= 0x400) {
        alias TBBUTTONINFOA TBBUTTONINFO;
        alias LPTBBUTTONINFOA LPTBBUTTONINFO;
    }

    alias TBNOTIFYA TBNOTIFY;
    alias LPTBNOTIFYA LPTBNOTIFY;
    alias NMTOOLBARA NMTOOLBAR;
    alias LPNMTOOLBARA LPNMTOOLBAR;
    alias TTM_ADDTOOLA TTM_ADDTOOL;
    alias TTM_DELTOOLA TTM_DELTOOL;
    alias TTM_NEWTOOLRECTA TTM_NEWTOOLRECT;
    alias TTM_GETTOOLINFOA TTM_GETTOOLINFO;
    alias TTM_SETTOOLINFOA TTM_SETTOOLINFO;
    alias TTM_HITTESTA TTM_HITTEST;
    alias TTM_GETTEXTA TTM_GETTEXT;
    alias TTM_UPDATETIPTEXTA TTM_UPDATETIPTEXT;
    alias TTM_ENUMTOOLSA TTM_ENUMTOOLS;
    alias TTM_GETCURRENTTOOLA TTM_GETCURRENTTOOL;
    alias TTN_NEEDTEXTA TTN_NEEDTEXT;
    alias TTN_GETDISPINFOA TTN_GETDISPINFO;
    alias LV_ITEMA LV_ITEM;
    alias LVITEMA LVITEM;
    alias LVITEM* LPLVITEM;
    alias LPSTR_TEXTCALLBACKA LPSTR_TEXTCALLBACK;

    static if (_WIN32_IE >= 0x400) {
        alias LVBKIMAGEA LVBKIMAGE;
        alias LPLVBKIMAGEA LPLVBKIMAGE;
        alias LVM_SETBKIMAGEA LVM_SETBKIMAGE;
        alias LVM_GETBKIMAGEA LVM_GETBKIMAGE;
    }

    alias LVM_GETITEMA LVM_GETITEM;
    alias LVM_SETITEMA LVM_SETITEM;
    alias LVM_INSERTITEMA LVM_INSERTITEM;
    alias LV_FINDINFOA LV_FINDINFO;
    alias LVFINDINFOA LVFINDINFO;
    alias LPFINDINFOA LPFINDINFO;
    alias NMLVFINDITEMA NMLVFINDITEM;
    alias PNMLVFINDITEMA PNMLVFINDITEM;
    alias LPNMLVFINDITEMA LPNMLVFINDITEM;
    alias LVM_FINDITEMA LVM_FINDITEM;
    alias LVM_GETSTRINGWIDTHA LVM_GETSTRINGWIDTH;
    alias LVM_EDITLABELA LVM_EDITLABEL;
    alias LV_COLUMNA LV_COLUMN;
    alias LVCOLUMNA LVCOLUMN;
    alias LVCOLUMNA* LPLVCOLUMN;
    alias LVM_GETCOLUMNA LVM_GETCOLUMN;
    alias LVM_SETCOLUMNA LVM_SETCOLUMN;
    alias LVM_INSERTCOLUMNA LVM_INSERTCOLUMN;
    alias LVM_GETITEMTEXTA LVM_GETITEMTEXT;
    alias LVM_SETITEMTEXTA LVM_SETITEMTEXT;
    alias LVM_GETISEARCHSTRINGA LVM_GETISEARCHSTRING;
    alias LVN_BEGINLABELEDITA LVN_BEGINLABELEDIT;
    alias LVN_ENDLABELEDITA LVN_ENDLABELEDIT;
    alias LVN_GETDISPINFOA LVN_GETDISPINFO;
    alias LVN_SETDISPINFOA LVN_SETDISPINFO;

    static if (_WIN32_IE >= 0x400) {
        alias LVN_GETINFOTIPA LVN_GETINFOTIP;
        alias NMLVGETINFOTIPA NMLVGETINFOTIP;
        alias LPNMLVGETINFOTIPA LPNMLVGETINFOTIP;
    }

    alias LV_DISPINFOA LV_DISPINFO;
    alias NMLVDISPINFOA NMLVDISPINFO;
    alias LPNMLVDISPINFOA LPNMLVDISPINFO;
    alias TVM_INSERTITEMA TVM_INSERTITEM;
    alias TVM_GETITEMA TVM_GETITEM;
    alias TVM_SETITEMA TVM_SETITEM;
    alias TVM_EDITLABELA TVM_EDITLABEL;
    alias TVM_GETISEARCHSTRINGA TVM_GETISEARCHSTRING;
    alias NMTVDISPINFOA TV_DISPINFO;
    alias NMTVDISPINFOA NMTVDISPINFO;
    alias LPNMTVDISPINFOA LPNMTVDISPINFO;

    static if (_WIN32_IE >= 0x400) {
        alias NMTVGETINFOTIPA NMTVGETINFOTIP;
        alias LPNMTVGETINFOTIPA LPNMTVGETINFOTIP;
        alias TVN_GETINFOTIPA TVN_GETINFOTIP;
    }

    alias TVN_SELCHANGINGA TVN_SELCHANGING;
    alias TVN_SELCHANGEDA TVN_SELCHANGED;
    alias TVN_GETDISPINFOA TVN_GETDISPINFO;
    alias TVN_SETDISPINFOA TVN_SETDISPINFO;
    alias TVN_ITEMEXPANDINGA TVN_ITEMEXPANDING;
    alias TVN_ITEMEXPANDEDA TVN_ITEMEXPANDED;
    alias TVN_BEGINDRAGA TVN_BEGINDRAG;
    alias TVN_BEGINRDRAGA TVN_BEGINRDRAG;
    alias TVN_DELETEITEMA TVN_DELETEITEM;
    alias TVN_BEGINLABELEDITA TVN_BEGINLABELEDIT;
    alias TVN_ENDLABELEDITA TVN_ENDLABELEDIT;
    alias TC_ITEMHEADERA TC_ITEMHEADER;
    alias TC_ITEMA TC_ITEM;
    alias TCITEMA TCITEM;
    alias LPTCITEMA LPTCITEM;
    alias TCM_GETITEMA TCM_GETITEM;
    alias TCM_SETITEMA TCM_SETITEM;
    alias TCM_INSERTITEMA TCM_INSERTITEM;
    alias CreateStatusWindowA CreateStatusWindow;
    alias DrawStatusTextA DrawStatusText;
    alias ImageList_LoadImageA ImageList_LoadImage;
    alias DTM_SETFORMATA DTM_SETFORMAT;
    alias DTN_USERSTRINGA DTN_USERSTRING;
    alias DTN_WMKEYDOWNA DTN_WMKEYDOWN;
    alias DTN_FORMATA DTN_FORMAT;
    alias DTN_FORMATQUERYA DTN_FORMATQUERY;
    alias REBARBANDINFOA REBARBANDINFO;
    alias REBARBANDINFOA* LPREBARBANDINFO;
    alias LPCREBARBANDINFOA LPCREBARBANDINFO;
    alias REBARBANDINFOA_V3_SIZE REBARBANDINFO_V3_SIZE;
    alias RB_INSERTBANDA RB_INSERTBAND;
    alias RB_SETBANDINFOA RB_SETBANDINFO;
}


extern (Windows) {
alias INT function(PVOID, PVOID) PFNDPAENUMCALLBACK;
alias INT function(PVOID, PVOID) PFNDSAENUMCALLBACK;
alias INT function(PVOID, PVOID, LPARAM) PFNDPACOMPARE;
}

static if (_WIN32_WINNT >= 0x501) {
    extern (Windows)
    alias LRESULT function(HWND, UINT, WPARAM, LPARAM, UINT_PTR, DWORD_PTR)
      SUBCLASSPROC;

    struct LITEM {
        UINT mask;
        int  iLink;
        UINT state;
        UINT stateMask;
        WCHAR[MAX_LINKID_TEXT]  szID = 0;
        WCHAR[L_MAX_URL_LENGTH] szUrl = 0;
    }
    alias LITEM* PLITEM;

    struct LHITTESTINFO {
        POINT pt;
        LITEM item;
    }
    alias LHITTESTINFO* PLHITTESTINFO;

    struct NMLINK {
        NMHDR hdr;
        LITEM item;
    }
    alias NMLINK* PNMLINK;
}

uint INDEXTOOVERLAYMASK(uint i) { return i << 8; }
uint INDEXTOSTATEIMAGEMASK(uint i) { return i << 12; }

template HANDLE_WM_NOTIFY(R) {
    private alias _prm_HANDLE_WM_NOTIFY = extern (Windows)
        R function(HWND, int, NMHDR*); // to inject linkage type
    R HANDLE_WM_NOTIFY(HWND hwnd, WPARAM wParam, LPARAM lParam, _prm_HANDLE_WM_NOTIFY fn) {
        return fn(hwnd, wParam, cast(NMHDR*) lParam);
    }
}
private alias _prm_FORWARD_WM_NOTIFY = extern (Windows)
    LRESULT function(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam); // to inject linkage type
LRESULT FORWARD_WM_NOTIFY(HWND hwnd, int idFrom, NMHDR* pnmhdr, _prm_FORWARD_WM_NOTIFY fn) {
    return fn(hwnd, WM_NOTIFY, idFrom, cast(LPARAM) pnmhdr);
}

//#define CCSIZEOF_STRUCT(s, m) (((int)((PBYTE)(&((s*)0)->m)-((PBYTE)((s*)0))))+sizeof(((s*)0)->m))

LPARAM MAKEIPADDRESS(ubyte b1, ubyte b2, ubyte b3, ubyte b4) {
    return (cast(DWORD) b1 << 24)
         | (cast(DWORD) b2 << 16)
         | (cast(DWORD) b3 << 8)
         | (cast(DWORD) b4);
}

LPARAM MAKEIPRANGE(ubyte low, ubyte high) {
    return (cast(int) high << 8) | low;
}

ubyte FIRST_IPADDRESS(LPARAM x) {
    return cast(ubyte) (x >> 24);
}

ubyte SECOND_IPADDRESS(LPARAM x) {
    return cast(ubyte) (x >> 16);
}

ubyte THIRD_IPADDRESS(LPARAM x) {
    return cast(ubyte) (x >> 8);
}

ubyte FOURTH_IPADDRESS(LPARAM x) {
    return cast(ubyte) x;
}

HWND Animate_Create(HWND hwndP, UINT id, DWORD dwStyle,
      HINSTANCE hInstance) {
    return CreateWindow(cast(TCHAR*)ANIMATE_CLASS.ptr, null, dwStyle, 0, 0, 0, 0, hwndP,
      cast(HMENU) id, hInstance, null);
}

BOOL Animate_Open(HWND hwnd, LPTSTR szName) {
    return cast(BOOL) SendMessage(hwnd, ACM_OPEN, 0, cast(LPARAM) szName);
}

BOOL Animate_OpenEx(HWND hwnd, HINSTANCE hInst, LPTSTR szName) {
    return cast(BOOL) SendMessage(hwnd, ACM_OPEN, cast(WPARAM) hInst,
      cast(LPARAM) szName);
}

BOOL Animate_Play(HWND hwnd, int from, int to, int rep) {
    return cast(BOOL) SendMessage(hwnd, ACM_PLAY, rep,
      MAKELONG(cast(ushort) from, cast(ushort) to));
}

BOOL Animate_Stop(HWND hwnd) {
    return cast(BOOL) SendMessage(hwnd, ACM_STOP, 0, 0);
}

BOOL Animate_Close(HWND hwnd) {
    return Animate_Open(hwnd, null);
}

BOOL Animate_Seek(HWND hwnd, int frame) {
    return Animate_Play(hwnd, frame, frame, 1);
}

extern (Windows) nothrow @nogc {
    HBITMAP CreateMappedBitmap(HINSTANCE, INT_PTR, UINT, LPCOLORMAP, int);
    HWND CreateStatusWindowA(LONG, LPCSTR, HWND, UINT);
    HWND CreateStatusWindowW(LONG, LPCWSTR, HWND, UINT);
    HWND CreateToolbarEx(HWND, DWORD, UINT, int, HINSTANCE, UINT_PTR,
      LPCTBBUTTON, int, int, int, int, int, UINT);
    HWND CreateUpDownControl(DWORD, int, int, int, int, HWND, int, HINSTANCE,
      HWND, int, int, int);
}

HWND DateTime_GetMonthCal(HWND hwnd) {
    return cast(HWND) SendMessage(hwnd, DTM_GETMONTHCAL, 0, 0);
}

COLORREF DateTime_GetMonthCalColor(HWND hwnd, int iColor) {
    return cast(COLORREF) SendMessage(hwnd, DTM_GETMCCOLOR, iColor, 0);
}

HFONT DateTime_GetMonthCalFont(HWND hwnd) {
    return cast(HFONT) SendMessage(hwnd, DTM_GETMCFONT, 0, 0);
}

DWORD DateTime_GetRange(HWND hwnd, LPSYSTEMTIME lpSysTimeArray) {
    return cast(DWORD) SendMessage(hwnd, DTM_GETRANGE, 0, cast(LPARAM) lpSysTimeArray);
}

DWORD DateTime_GetSystemtime(HWND hwnd, LPSYSTEMTIME lpSysTime) {
    return cast(DWORD) SendMessage(hwnd, DTM_GETSYSTEMTIME, 0, cast(LPARAM) lpSysTime);
}

BOOL DateTime_SetFormat(HWND hwnd, LPCTSTR lpszFormat) {
    return cast(BOOL) SendMessage(hwnd, DTM_SETFORMAT, 0,
      cast(LPARAM) lpszFormat);
}

LRESULT DateTime_SetMonthCalColor(HWND hwnd, int iColor, COLORREF clr) {
    return SendMessage(hwnd, DTM_SETMCCOLOR, cast(WPARAM) iColor,
      cast(LPARAM) clr);
}

void DateTime_SetMonthCalFont(HWND hwnd, HFONT hfont, BOOL fRedraw) {
    SendMessage(hwnd, DTM_SETMCFONT, cast(WPARAM) hfont, fRedraw);
}

BOOL DateTime_SetRange(HWND hwnd, WPARAM flags, LPSYSTEMTIME lpSysTimeArray) {
    return cast(BOOL) SendMessage(hwnd, DTM_SETRANGE, flags,
      cast(LPARAM) lpSysTimeArray);
}

BOOL DateTime_SetSystemtime(HWND hwnd, WPARAM flag, LPSYSTEMTIME lpSysTime) {
    return cast(BOOL) SendMessage(hwnd, DTM_SETSYSTEMTIME, flag,
      cast(LPARAM) lpSysTime);
}

extern (Windows) nothrow @nogc {
    void DrawInsert(HWND, HWND, int);
    void DrawStatusTextA(HDC, LPRECT, LPCSTR, UINT);
    void DrawStatusTextW(HDC, LPRECT, LPCWSTR, UINT);
    void GetEffectiveClientRect(HWND, LPRECT, LPINT);
}

int Header_GetItemCount(HWND w) {
    return cast(int) SendMessage(w, HDM_GETITEMCOUNT, 0, 0);
}

int Header_InsertItem(HWND w, int i, const(HDITEM)* phdi) {
    return cast(int) SendMessage(w, HDM_INSERTITEM, i, cast(LPARAM) phdi);
}

BOOL Header_DeleteItem(HWND w, int i) {
    return cast(BOOL) SendMessage(w, HDM_DELETEITEM, i, 0);
}

BOOL Header_GetItem(HWND w, int i, LPHDITEM phdi) {
    return cast(BOOL) SendMessage(w, HDM_GETITEM, i, cast(LPARAM) phdi);
}

BOOL Header_SetItem(HWND w, int i, const(HDITEM)* phdi) {
    return cast(BOOL) SendMessage(w, HDM_SETITEM, i, cast(LPARAM) phdi);
}

BOOL Header_Layout(HWND w, LPHDLAYOUT playout) {
    return cast(BOOL) SendMessage(w, HDM_LAYOUT, 0, cast(LPARAM) playout);
}

static if (_WIN32_IE >= 0x300) {
    int Header_OrderToIndex(HWND w, int i) {
        return cast(int) SendMessage(w, HDM_ORDERTOINDEX, i, 0);
    }

    BOOL Header_GetItemRect(HWND w, int i, RECT* r) {
        return cast(BOOL) SendMessage(w, HDM_GETITEMRECT, i, cast(LPARAM) r);
    }

    BOOL Header_GetOrderArray(HWND w, int iSize, LPINT lpiArray) {
        return cast(BOOL) SendMessage(w, HDM_GETORDERARRAY, iSize,
          cast(LPARAM) lpiArray);
    }

    BOOL Header_SetOrderArray(HWND w, int iSize, LPINT lpiArray) {
        return cast(BOOL) SendMessage(w, HDM_SETORDERARRAY, iSize,
          cast(LPARAM) lpiArray);
    }

    HIMAGELIST Header_CreateDragImage(HWND w, int i) {
        return cast(HIMAGELIST) SendMessage(w, HDM_CREATEDRAGIMAGE, i, 0);
    }

    HIMAGELIST Header_SetImageList(HWND w, HIMAGELIST himl) {
        return cast(HIMAGELIST) SendMessage(w, HDM_SETIMAGELIST, 0,
          cast(LPARAM) himl);
    }

    HIMAGELIST Header_GetImageList(HWND w) {
        return cast(HIMAGELIST) SendMessage(w, HDM_GETIMAGELIST, 0, 0);
    }
}

static if (_WIN32_IE >= 0x400) {
    BOOL Header_GetUnicodeFormat(HWND w) {
        return cast(BOOL) SendMessage(w, HDM_GETUNICODEFORMAT, 0, 0);
    }

    BOOL Header_SetUnicodeFormat(HWND w, BOOL fUnicode) {
        return cast(BOOL) SendMessage(w, HDM_SETUNICODEFORMAT, fUnicode, 0);
    }
}

extern (Windows) nothrow @nogc {
    HDSA DSA_Create(INT, INT);
    BOOL DSA_Destroy(HDSA);
    VOID DSA_DestroyCallback(HDSA, PFNDSAENUMCALLBACK, PVOID);
    PVOID DSA_GetItemPtr(HDSA, INT);
    INT DSA_InsertItem(HDSA, INT, PVOID);
    HDPA DPA_Create(INT);
    BOOL DPA_Destroy(HDPA);
    PVOID DPA_DeletePtr(HDPA, INT);
    BOOL DPA_DeleteAllPtrs(HDPA);
    VOID DPA_EnumCallback(HDPA, PFNDPAENUMCALLBACK, PVOID);
    VOID DPA_DestroyCallback(HDPA, PFNDPAENUMCALLBACK, PVOID);
    BOOL DPA_SetPtr(HDPA, INT, PVOID);
    INT DPA_InsertPtr(HDPA, INT, PVOID);
    PVOID DPA_GetPtr(HDPA, INT_PTR);
    BOOL DPA_Sort(HDPA, PFNDPACOMPARE, LPARAM);
    INT DPA_Search(HDPA, PVOID, INT, PFNDPACOMPARE, LPARAM, UINT);
    BOOL Str_SetPtrW(LPWSTR*, LPCWSTR);

    static if (_WIN32_IE >= 0x400) {
        BOOL FlatSB_EnableScrollBar(HWND, INT, UINT);
        BOOL FlatSB_ShowScrollBar(HWND, INT, BOOL);
        BOOL FlatSB_GetScrollRange(HWND, INT, LPINT, LPINT);
        BOOL FlatSB_GetScrollInfo(HWND, INT, LPSCROLLINFO);
        INT FlatSB_GetScrollPos(HWND, INT);
        BOOL FlatSB_GetScrollProp(HWND, INT, LPINT);
        version (Win64) {
            BOOL FlatSB_GetScrollPropPtr(HWND, INT, PINT_PTR);
        } else {
            alias FlatSB_GetScrollProp FlatSB_GetScrollPropPtr;
        }
        INT FlatSB_SetScrollPos(HWND, INT, INT, BOOL);
        INT FlatSB_SetScrollInfo(HWND, INT, LPSCROLLINFO, BOOL);
        INT FlatSB_SetScrollRange(HWND, INT, INT, INT, BOOL);
        BOOL FlatSB_SetScrollProp(HWND, UINT, INT_PTR, BOOL);
        alias FlatSB_SetScrollProp FlatSB_SetScrollPropPtr;
        BOOL InitializeFlatSB(HWND);
        HRESULT UninitializeFlatSB(HWND);
    }

    static if (_WIN32_WINNT >= 0x501) {
        BOOL SetWindowSubclass(HWND, SUBCLASSPROC, UINT_PTR, DWORD_PTR);
        BOOL GetWindowSubclass(HWND, SUBCLASSPROC, UINT_PTR, DWORD_PTR*);
        BOOL RemoveWindowSubclass(HWND, SUBCLASSPROC, UINT_PTR);
        LRESULT DefSubclassProc(HWND, UINT, WPARAM, LPARAM);
        INT DrawShadowText(HDC, LPCWSTR, UINT, RECT*, DWORD, COLORREF,
          COLORREF, INT, INT);
    }

    int ImageList_Add(HIMAGELIST, HBITMAP, HBITMAP);
    int ImageList_AddMasked(HIMAGELIST, HBITMAP, COLORREF);
    BOOL ImageList_BeginDrag(HIMAGELIST, int, int, int);
    HIMAGELIST ImageList_Create(int, int, UINT, int, int);
    BOOL ImageList_Destroy(HIMAGELIST);
    BOOL ImageList_DragEnter(HWND, int, int);
    BOOL ImageList_DragLeave(HWND);
    BOOL ImageList_DragMove(int, int);
    BOOL ImageList_DragShowNolock(BOOL);
    BOOL ImageList_Draw(HIMAGELIST, int, HDC, int, int, UINT);
    BOOL ImageList_DrawEx(HIMAGELIST, int, HDC, int, int, int, int, COLORREF,
      COLORREF, UINT);
    void ImageList_EndDrag();
    COLORREF ImageList_GetBkColor(HIMAGELIST);
    HIMAGELIST ImageList_GetDragImage(LPPOINT, LPPOINT);
    HICON ImageList_GetIcon(HIMAGELIST, int, UINT);
    BOOL ImageList_GetIconSize(HIMAGELIST, int*, int*);
    int ImageList_GetImageCount(HIMAGELIST);
    BOOL ImageList_GetImageInfo(HIMAGELIST, int, IMAGEINFO*);
    HIMAGELIST ImageList_LoadImageA(HINSTANCE, LPCSTR, int, int, COLORREF,
      UINT, UINT);
    HIMAGELIST ImageList_LoadImageW(HINSTANCE, LPCWSTR, int, int, COLORREF,
      UINT, UINT);
    HIMAGELIST ImageList_Merge(HIMAGELIST, int, HIMAGELIST, int, int, int);
    BOOL ImageList_Remove(HIMAGELIST, int);
    BOOL ImageList_Replace(HIMAGELIST, int, HBITMAP, HBITMAP);
    int ImageList_ReplaceIcon(HIMAGELIST, int, HICON);
    COLORREF ImageList_SetBkColor(HIMAGELIST, COLORREF);
    BOOL ImageList_SetDragCursorImage(HIMAGELIST, int, int, int);
    BOOL ImageList_SetIconSize(HIMAGELIST, int, int);
    BOOL ImageList_SetOverlayImage(HIMAGELIST, int, int);

    //#ifdef _OBJIDL_H
    HIMAGELIST ImageList_Read(LPSTREAM);
    BOOL ImageList_Write(HIMAGELIST, LPSTREAM);
    //#endif

    static if (_WIN32_IE >= 0x400) {
        HIMAGELIST ImageList_Duplicate(HIMAGELIST himl);
    }

    void InitCommonControls();

    static if (_WIN32_IE >= 0x300) {
        BOOL InitCommonControlsEx(LPINITCOMMONCONTROLSEX);
    }

    int LBItemFromPt(HWND, POINT, BOOL);
}

int ImageList_AddIcon(HIMAGELIST himl, HICON hicon) {
    return ImageList_ReplaceIcon(himl, -1, hicon);
}

HICON ImageList_ExtractIcon(HINSTANCE hi, HIMAGELIST himl, int i) {
    return ImageList_GetIcon(himl, i, 0);
}

HIMAGELIST ImageList_LoadBitmap(HINSTANCE hi, LPCTSTR lpbmp, int cx,
      int cGrow, COLORREF crMask) {
    return ImageList_LoadImage(hi, lpbmp, cx, cGrow, crMask, IMAGE_BITMAP, 0);
}

BOOL ImageList_RemoveAll(HIMAGELIST himl) {
    return ImageList_Remove(himl, -1);
}

COLORREF ListView_GetBkColor(HWND w) {
    return cast(COLORREF) SendMessage(w, LVM_GETBKCOLOR, 0, 0);
}

HIMAGELIST ListView_GetImageList(HWND w, int i) {
    return cast(HIMAGELIST) SendMessage(w, LVM_GETIMAGELIST, i, 0);
}

int ListView_GetItemCount(HWND w) {
    return cast(int) SendMessage(w, LVM_GETITEMCOUNT, 0, 0);
}

BOOL ListView_GetItem(HWND w, LPLVITEM pitem) {
    return cast(BOOL) SendMessage(w, LVM_GETITEM, 0, cast(LPARAM) pitem);
}

BOOL ListView_SetBkColor(HWND w, COLORREF c) {
    return cast(BOOL) SendMessage(w, LVM_SETBKCOLOR, 0, cast(LPARAM) c);
}

HIMAGELIST ListView_SetImageList(HWND w, HIMAGELIST h, int i) {
    return cast(HIMAGELIST) SendMessage(w, LVM_SETIMAGELIST, i,
      cast(LPARAM) h);
}

BOOL ListView_SetItem(HWND w, const(LV_ITEM)* i) {
    return cast(BOOL) SendMessage(w, LVM_SETITEM, 0, cast(LPARAM) i);
}

int ListView_InsertItem(HWND w, const(LV_ITEM)* i) {
    return cast(int) SendMessage(w, LVM_INSERTITEM, 0, cast(LPARAM) i);
}

BOOL ListView_DeleteItem(HWND w, int i) {
    return cast(BOOL) SendMessage(w, LVM_DELETEITEM, i, 0);
}

BOOL ListView_DeleteAllItems(HWND w) {
    return cast(BOOL) SendMessage(w, LVM_DELETEALLITEMS, 0, 0);
}

UINT ListView_GetCallbackMask(HWND w) {
    return cast(UINT) SendMessage(w, LVM_GETCALLBACKMASK, 0, 0);
}

BOOL ListView_SetCallbackMask(HWND w, UINT m) {
    return cast(BOOL) SendMessage(w, LVM_SETCALLBACKMASK, m, 0);
}

int ListView_GetNextItem(HWND w, int i, UINT f) {
    return cast(int) SendMessage(w, LVM_GETNEXTITEM, i, MAKELPARAM(cast(ushort)f, 0));
}

int ListView_FindItem(HWND w, int i, const(LV_FINDINFO)* p) {
    return cast(int) SendMessage(w, LVM_FINDITEM, i, cast(LPARAM) p);
}

BOOL ListView_GetItemRect(HWND w, int i, LPRECT p, int c) {
    if (p)
        p.left = c;
    return cast(BOOL) SendMessage(w, LVM_GETITEMRECT, i, cast(LPARAM) p);
}

BOOL ListView_SetItemPosition(HWND w, int i, int x, int y) {
    return cast(BOOL) SendMessage(w, LVM_SETITEMPOSITION, i, MAKELPARAM(cast(ushort)x, cast(ushort)y));
}

BOOL ListView_GetItemPosition(HWND w, int i, POINT* p) {
    return cast(BOOL) SendMessage(w, LVM_GETITEMPOSITION, i, cast(LPARAM) p);
}

DWORD ListView_GetItemSpacing(HWND w, BOOL f) {
    return cast(DWORD) SendMessage(w, LVM_GETITEMSPACING, f, 0);
}

int ListView_GetStringWidth(HWND w, LPCSTR s) {
    return cast(int) SendMessage(w, LVM_GETSTRINGWIDTH, 0, cast(LPARAM) s);
}

int ListView_HitTest(HWND w, LPLVHITTESTINFO p) {
    return cast(int) SendMessage(w, LVM_HITTEST, 0, cast(LPARAM) p);
}

BOOL ListView_EnsureVisible(HWND w, int i, BOOL f) {
    return cast(BOOL) SendMessage(w, LVM_ENSUREVISIBLE, i, MAKELPARAM(cast(ushort)f, 0));
}

BOOL ListView_Scroll(HWND w, int dx, int dy) {
    return cast(BOOL) SendMessage(w, LVM_SCROLL, dx, dy);
}

BOOL ListView_RedrawItems(HWND w, int f, int l) {
    return cast(BOOL) SendMessage(w, LVM_REDRAWITEMS, f, l);
}

BOOL ListView_Arrange(HWND w, UINT c) {
    return cast(BOOL) SendMessage(w, LVM_ARRANGE, c, 0);
}

HWND ListView_EditLabel(HWND w, int i) {
    return cast(HWND) SendMessage(w, LVM_EDITLABEL, i, 0);
}

HWND ListView_GetEditControl(HWND w) {
    return cast(HWND) SendMessage(w, LVM_GETEDITCONTROL, 0, 0);
}

BOOL ListView_GetColumn(HWND w, int i, LPLVCOLUMN p) {
    return cast(BOOL) SendMessage(w, LVM_GETCOLUMN, i, cast(LPARAM) p);
}

BOOL ListView_SetColumn(HWND w, int i, const(LV_COLUMN)* p) {
    return cast(BOOL) SendMessage(w, LVM_SETCOLUMN, i, cast(LPARAM) p);
}

int ListView_InsertColumn(HWND w, int i, const(LV_COLUMN)* p) {
    return cast(int) SendMessage(w, LVM_INSERTCOLUMN, i, cast(LPARAM) p);
}

BOOL ListView_DeleteColumn(HWND w, int i) {
    return cast(BOOL) SendMessage(w, LVM_DELETECOLUMN, i, 0);
}

int ListView_GetColumnWidth(HWND w, int i) {
    return cast(int) SendMessage(w, LVM_GETCOLUMNWIDTH, i, 0);
}

BOOL ListView_SetColumnWidth(HWND w, int i, int x) {
    return cast(BOOL) SendMessage(w, LVM_SETCOLUMNWIDTH, i, MAKELPARAM(cast(ushort)x, 0));
}

HIMAGELIST ListView_CreateDragImage(HWND w, int i, LPPOINT p) {
    return cast(HIMAGELIST) SendMessage(w, LVM_CREATEDRAGIMAGE, i,
      cast(LPARAM) p);
}

BOOL ListView_GetViewRect(HWND w, RECT* p) {
    return cast(BOOL) SendMessage(w, LVM_GETVIEWRECT, 0, cast(LPARAM) p);
}

COLORREF ListView_GetTextColor(HWND w) {
    return cast(COLORREF) SendMessage(w, LVM_GETTEXTCOLOR, 0, 0);
}

BOOL ListView_SetTextColor(HWND w, COLORREF c) {
    return cast(BOOL) SendMessage(w, LVM_SETTEXTCOLOR, 0, cast(LPARAM) c);
}

COLORREF ListView_GetTextBkColor(HWND w) {
    return cast(COLORREF) SendMessage(w, LVM_GETTEXTBKCOLOR, 0, 0);
}

BOOL ListView_SetTextBkColor(HWND w, COLORREF c) {
    return cast(BOOL) SendMessage(w, LVM_SETTEXTBKCOLOR, 0, cast(LPARAM) c);
}

int ListView_GetTopIndex(HWND w) {
    return cast(int) SendMessage(w, LVM_GETTOPINDEX, 0, 0);
}

int ListView_GetCountPerPage(HWND w) {
    return cast(int) SendMessage(w, LVM_GETCOUNTPERPAGE, 0, 0);
}

BOOL ListView_GetOrigin(HWND w, LPPOINT p) {
    return cast(BOOL) SendMessage(w, LVM_GETORIGIN, 0, cast(LPARAM) p);
}

BOOL ListView_Update(HWND w, WPARAM i) {
    return cast(BOOL) SendMessage(w, LVM_UPDATE, i, 0);
}

void ListView_SetItemState(HWND w, int i, UINT d, UINT m) {
    LV_ITEM _lvi;
    _lvi.stateMask = m;
    _lvi.state = d;
    SendMessage(w, LVM_SETITEMSTATE, i, cast(LPARAM) &_lvi);
}

UINT ListView_GetItemState(HWND w, int i, UINT m) {
    return cast(UINT) SendMessage(w, LVM_GETITEMSTATE, i, m);
}

void ListView_GetItemText(HWND w, int i, int iS, LPTSTR s, int n) {
    LV_ITEM _lvi;
    _lvi.iSubItem = iS;
    _lvi.cchTextMax = n;
    _lvi.pszText = s;
    SendMessage(w, LVM_GETITEMTEXT, i, cast(LPARAM) &_lvi);
}

void ListView_SetItemText(HWND w, int i, int iS, LPTSTR s) {
    LV_ITEM _lvi;
    _lvi.iSubItem = iS;
    _lvi.pszText = s;
    SendMessage(w, LVM_SETITEMTEXT, i, cast(LPARAM) &_lvi);
}

void ListView_SetItemCount(HWND w, int n) {
    SendMessage(w, LVM_SETITEMCOUNT, n, 0);
}

BOOL ListView_SortItems(HWND w, PFNLVCOMPARE f, LPARAM l) {
    return cast(BOOL) SendMessage(w, LVM_SORTITEMS, l, cast(LPARAM) f);
}

void ListView_SetItemPosition32(HWND w, int i, int x, int y) {
    POINT p;
    p.x = x;
    p.y = y;
    SendMessage(w, LVM_SETITEMPOSITION32, i, cast(LPARAM) &p);
}

UINT ListView_GetSelectedCount(HWND w) {
    return cast(UINT) SendMessage(w, LVM_GETSELECTEDCOUNT, 0, 0);
}

UINT ListView_GetCheckState(HWND w, UINT i) {
    return ((cast(UINT) SendMessage(w, LVM_GETITEMSTATE, i, LVIS_STATEIMAGEMASK)) >> 12) - 1;
}

void ListView_SetCheckState(HWND w, UINT i, BOOL f) {
    ListView_SetItemState(w, i, INDEXTOSTATEIMAGEMASK(f ? 2 : 1),
      LVIS_STATEIMAGEMASK);
}

BOOL ListView_GetISearchString(HWND w, LPSTR lpsz) {
    return cast(BOOL) SendMessage(w, LVM_GETISEARCHSTRING, 0,
      cast(LPARAM) lpsz);
}

void ListView_CancelEditLabel(HWND w) {
    SendMessage(w, LVM_CANCELEDITLABEL, 0, 0);
}

int ListView_EnableGroupView(HWND w, BOOL i) {
    return cast(int) SendMessage(w, LVM_ENABLEGROUPVIEW, i, 0);
}

//static if (_WIN32_WINNT >= 0x500 || _WIN32_IE >= 0x500) {
    BOOL ListView_SortItemsEx(HWND w, PFNLVCOMPARE c, LPARAM p) {
        return cast(BOOL) SendMessage(w, LVM_SORTITEMSEX, cast(WPARAM) p, cast(LPARAM)c);
    }
//}

static if (_WIN32_WINNT >= 0x501) {
    int ListView_GetGroupInfo(HWND w, int i, PLVGROUP p) {
        return cast(int) SendMessage(w, LVM_GETGROUPINFO, i, cast(LPARAM) p);
    }

    void ListView_GetGroupMetrics(HWND w, PLVGROUPMETRICS p) {
        SendMessage(w, LVM_GETGROUPMETRICS, 0, cast(LPARAM) p);
    }

    BOOL ListView_GetInsertMark(HWND w, PLVINSERTMARK p) {
        return cast(BOOL) SendMessage(w, LVM_GETINSERTMARK, 0, cast(LPARAM) p);
    }

    COLORREF ListView_GetInsertMarkColor(HWND w) {
        return cast(COLORREF) SendMessage(w, LVM_GETINSERTMARKCOLOR, 0, 0);
    }

    int ListView_GetInsertMarkRect(HWND w, LPRECT p) {
        return cast(int) SendMessage(w, LVM_GETINSERTMARKRECT, 0, cast(LPARAM) p);
    }

    COLORREF ListView_GetOutlineColor(HWND w) {
        return cast(COLORREF) SendMessage(w, LVM_GETOUTLINECOLOR, 0, 0);
    }

    UINT ListView_GetSelectedColumn(HWND w) {
        return cast(UINT) SendMessage(w, LVM_GETSELECTEDCOLUMN, 0, 0);
    }

    void ListView_GetTileInfo(HWND w, PLVTILEINFO p) {
        SendMessage(w, LVM_GETTILEINFO, 0, cast(LPARAM) p);
    }

    void ListView_GetTileViewInfo(HWND w, PLVTILEVIEWINFO p) {
        SendMessage(w, LVM_GETTILEVIEWINFO, 0, cast(LPARAM) p);
    }

    DWORD ListView_GetView(HWND w) {
        return cast(DWORD) SendMessage(w, LVM_GETVIEW, 0, 0);
    }

    BOOL ListView_HasGroup(HWND w, int i) {
        return cast(BOOL) SendMessage(w, LVM_HASGROUP, i, 0);
    }

    int ListView_InsertGroup(HWND w, int i, PLVGROUP p) {
        return cast(int) SendMessage(w, LVM_INSERTGROUP, i, cast(LPARAM) p);
    }

    void ListView_InsertGroupSorted(HWND w, PLVINSERTGROUPSORTED p) {
        SendMessage(w, LVM_INSERTGROUPSORTED, cast(WPARAM) p, 0);
    }

    BOOL ListView_InsertMarkHitTest(HWND w, LPPOINT p, PLVINSERTMARK t) {
        return cast(BOOL) SendMessage(w, LVM_INSERTMARKHITTEST, cast(WPARAM) p, cast(LPARAM) t);
    }

    BOOL ListView_IsGroupViewEnabled(HWND w) {
        return cast(BOOL) SendMessage(w, LVM_ISGROUPVIEWENABLED, 0, 0);
    }

    UINT ListView_MapIDToIndex(HWND w, UINT i) {
        return cast(UINT) SendMessage(w, LVM_MAPIDTOINDEX, i, 0);
    }

    /*  ??? MSDN documents this as "Not implemented", except in relation to
     *  Windows CE/Mobile.
     */
    void ListView_MoveGroup(HWND w, int i, int t) {
        SendMessage(w, LVM_MOVEGROUP, i, t);
    }

    void ListView_RemoveAllGroups(HWND w) {
        SendMessage(w, LVM_REMOVEALLGROUPS, 0, 0);
    }

    int ListView_RemoveGroup(HWND w, int i) {
        return cast(int) SendMessage(w, LVM_REMOVEGROUP, i, 0);
    }

    int ListView_SetGroupInfo(HWND w, int i, PLVGROUP p) {
        return cast(int) SendMessage(w, LVM_SETGROUPINFO, i, cast(LPARAM) p);
    }

    void ListView_SetGroupMetrics(HWND w, PLVGROUPMETRICS p) {
        SendMessage(w, LVM_SETGROUPMETRICS, 0, cast(LPARAM) p);
    }

    BOOL ListView_SetInfoTip(HWND w, PLVSETINFOTIP p) {
        return cast(BOOL) SendMessage(w, LVM_SETINFOTIP, 0, cast(LPARAM) p);
    }

    BOOL ListView_SetInsertMark(HWND w, PLVINSERTMARK p) {
        return cast(BOOL) SendMessage(w, LVM_SETINSERTMARK, 0, cast(LPARAM) p);
    }

    COLORREF ListView_SetInsertMarkColor(HWND w, COLORREF c) {
        return cast(COLORREF) SendMessage(w, LVM_SETINSERTMARKCOLOR, 0, c);
    }

    COLORREF ListView_SetOutlineColor(HWND w, COLORREF c) {
        return cast(COLORREF) SendMessage(w, LVM_SETOUTLINECOLOR, 0, c);
    }

    void ListView_SetSelectedColumn(HWND w, int i) {
        SendMessage(w, LVM_SETSELECTEDCOLUMN, i, 0);
    }

    BOOL ListView_SetTileInfo(HWND w, PLVTILEINFO p) {
        return cast(BOOL) SendMessage(w, LVM_SETTILEINFO, 0, cast(LPARAM) p);
    }

    BOOL ListView_SetTileViewInfo(HWND w, PLVTILEVIEWINFO p) {
        return cast(BOOL) SendMessage(w, LVM_SETTILEVIEWINFO, 0, cast(LPARAM) p);
    }

    int ListView_SetView(HWND w, DWORD i) {
        return cast(int) SendMessage(w, LVM_SETVIEW, i, 0);
    }

    int ListView_SortGroups(HWND w, PFNLVGROUPCOMPARE c, LPVOID p) {
        return cast(int) SendMessage(w, LVM_SORTGROUPS, cast(WPARAM) c, cast(LPARAM) p);
    }
}

static if (_WIN32_WINNT >= 0x501) {
    enum {
        CBM_FIRST        = 0x1700,
        CB_SETMINVISIBLE = CBM_FIRST + 1,
        CB_GETMINVISIBLE = CBM_FIRST + 2,
        CB_SETCUEBANNER = CBM_FIRST + 3,
        CB_GETCUEBANNER = CBM_FIRST + 4,
    }

    BOOL ComboBox_SetMinVisible(HWND w, INT i) {
        return cast(BOOL) SendMessage(w, CB_SETMINVISIBLE, cast(WPARAM) i, 0);
    }

    int ComboBox_GetMinVisible(HWND w) {
        return cast(int) SendMessage(w, CB_GETMINVISIBLE, 0, 0);
    }
}

extern (Windows) BOOL MakeDragList(HWND);
extern (Windows) void MenuHelp(UINT, WPARAM, LPARAM, HMENU, HINSTANCE, HWND,
  PUINT);

COLORREF MonthCal_GetColor(HWND hwnd, INT icolor) {
    return cast(COLORREF) SendMessage(hwnd, MCM_GETCOLOR,
      cast(WPARAM) icolor, 0);
}

BOOL MonthCal_GetCurSel(HWND hwnd, LPSYSTEMTIME lpsystime) {
    return cast(BOOL) SendMessage(hwnd, MCM_GETCURSEL, 0,
      cast(LPARAM) lpsystime);
}

DWORD MonthCal_GetFirstDayOfWeek(HWND hwnd) {
    return cast(DWORD) SendMessage(hwnd, MCM_GETFIRSTDAYOFWEEK, 0, 0);
}

DWORD MonthCal_GetMaxSelCount(HWND hwnd) {
    return cast(DWORD) SendMessage(hwnd, MCM_GETMAXSELCOUNT, 0, 0);
}

DWORD MonthCal_GetMaxTodayWidth(HWND hwnd) {
    return cast(DWORD) SendMessage(hwnd, MCM_GETMAXTODAYWIDTH, 0, 0);
}

BOOL MonthCal_GetMinReqRect(HWND hwnd, LPRECT lpRectInfo) {
    return cast(BOOL) SendMessage(hwnd, MCM_GETMINREQRECT, 0,
      cast(LPARAM) lpRectInfo);
}

INT MonthCal_GetMonthDelta(HWND hwnd) {
    return cast(INT) SendMessage(hwnd, MCM_GETMONTHDELTA, 0, 0);
}

INT MonthCal_GetMonthRange(HWND hwnd, DWORD flag, LPSYSTEMTIME systimearray) {
    return cast(INT) SendMessage(hwnd, MCM_GETMONTHRANGE, cast(WPARAM) flag,
      cast(LPARAM) systimearray);
}

DWORD MonthCal_GetRange(HWND hwnd, LPSYSTEMTIME systimearray) {
    return cast(DWORD) SendMessage(hwnd, MCM_GETRANGE, 0,
      cast(LPARAM) systimearray);
}

BOOL MonthCal_GetSelRange(HWND hwnd, LPSYSTEMTIME systimearray) {
    return cast(BOOL) SendMessage(hwnd, MCM_GETSELRANGE, 0,
      cast(LPARAM) systimearray);
}

BOOL MonthCal_GetToday(HWND hwnd, LPSYSTEMTIME systime) {
    return cast(BOOL) SendMessage(hwnd, MCM_GETTODAY, 0,
      cast(LPARAM) systime);
}

BOOL MonthCal_GetUnicodeFormat(HWND hwnd) {
    return cast(BOOL) SendMessage(hwnd, MCM_GETUNICODEFORMAT, 0, 0);
}

DWORD MonthCal_HitTest(HWND hwnd, PMCHITTESTINFO pmchittest) {
    return cast(DWORD) SendMessage(hwnd, MCM_HITTEST, 0,
      cast(LPARAM) pmchittest);
}

COLORREF MonthCal_SetColor(HWND hwnd, INT icolor, COLORREF clr) {
    return cast(COLORREF) SendMessage(hwnd, MCM_SETCOLOR, cast(WPARAM) icolor,
      cast(LPARAM) clr);
}

BOOL MonthCal_SetCurSel(HWND hwnd, LPSYSTEMTIME lpsystime) {
    return cast(BOOL) SendMessage(hwnd, MCM_SETCURSEL, 0,
      cast(LPARAM) lpsystime);
}

BOOL MonthCal_SetDayState(HWND hwnd, INT imonths, LPMONTHDAYSTATE lpdatestatearray) {
    return cast(BOOL) SendMessage(hwnd, MCM_SETDAYSTATE, cast(WPARAM) imonths,
      cast(LPARAM) lpdatestatearray);
}

DWORD MonthCal_SetFirstDayOfWeek(HWND hwnd, INT iday) {
    return cast(DWORD) SendMessage(hwnd, MCM_SETFIRSTDAYOFWEEK, 0,
      cast(LPARAM) iday);
}

BOOL MonthCal_SetMaxSelCount(HWND hwnd, UINT imax) {
    return cast(BOOL) SendMessage(hwnd, MCM_SETMAXSELCOUNT,
      cast(WPARAM) imax, 0);
}

INT MonthCal_SetMonthDelta(HWND hwnd, INT idelta) {
    return cast(INT) SendMessage(hwnd, MCM_SETMONTHDELTA, cast(WPARAM) idelta, 0);
}

BOOL MonthCal_SetSelRange(HWND hwnd, LPSYSTEMTIME systimearray) {
    return cast(BOOL) SendMessage(hwnd, MCM_SETSELRANGE, 0,
      cast(LPARAM) systimearray);
}

void MonthCal_SetToday(HWND hwnd, LPSYSTEMTIME systime) {
    SendMessage(hwnd, MCM_SETTODAY, 0, cast(LPARAM) systime);
}

BOOL MonthCal_SetUnicodeFormat(HWND hwnd, BOOL unicode) {
    return cast(BOOL) SendMessage(hwnd, MCM_SETUNICODEFORMAT,
      cast(WPARAM) unicode, 0);
}

BOOL MonthCal_SetRange(HWND w, DWORD f, LPSYSTEMTIME st) {
    return cast(BOOL) SendMessage(w, MCM_SETRANGE, cast(WPARAM) f,
      cast(LPARAM) st);
}

extern (Windows) nothrow @nogc BOOL ShowHideMenuCtl(HWND, UINT_PTR, PINT);

BOOL TabCtrl_GetItem(HWND w, int i, LPTCITEM p) {
    return cast(BOOL) SendMessage(w, TCM_GETITEM, i, cast(LPARAM) p);
}

BOOL TabCtrl_SetItem(HWND w, int i, LPTCITEM p) {
    return cast(BOOL) SendMessage(w, TCM_SETITEM, i, cast(LPARAM) p);
}

int TabCtrl_InsertItem(HWND w, int i, const(TC_ITEM)* p) {
    return cast(int) SendMessage(w, TCM_INSERTITEM, i, cast(LPARAM) p);
}

BOOL TabCtrl_DeleteItem(HWND w, int i) {
    return cast(BOOL) SendMessage(w, TCM_DELETEITEM, i, 0);
}

BOOL TabCtrl_DeleteAllItems(HWND w) {
    return cast(BOOL) SendMessage(w, TCM_DELETEALLITEMS, 0, 0);
}

BOOL TabCtrl_GetItemRect(HWND w, int i, LPRECT p) {
    return cast(BOOL) SendMessage(w, TCM_GETITEMRECT, i, cast(LPARAM) p);
}

int TabCtrl_GetCurSel(HWND w) {
    return cast(int) SendMessage(w, TCM_GETCURSEL, 0, 0);
}

int TabCtrl_SetCurSel(HWND w, int i) {
    return cast(int) SendMessage(w, TCM_SETCURSEL, i, 0);
}

int TabCtrl_HitTest(HWND w, LPTCHITTESTINFO p) {
    return cast(int) SendMessage(w, TCM_HITTEST, 0, cast(LPARAM) p);
}

BOOL TabCtrl_SetItemExtra(HWND w, int c) {
    return cast(BOOL) SendMessage(w, TCM_SETITEMEXTRA, c, 0);
}

int TabCtrl_AdjustRect(HWND w, BOOL b, LPRECT p) {
    return cast(int) SendMessage(w, TCM_ADJUSTRECT, b, cast(LPARAM) p);
}

DWORD TabCtrl_SetItemSize(HWND w, int x, int y) {
    return cast(DWORD) SendMessage(w, TCM_SETITEMSIZE, 0, MAKELPARAM(cast(ushort)x, cast(ushort)y));
}

void TabCtrl_RemoveImage(HWND w, int i) {
    SendMessage(w, TCM_REMOVEIMAGE, i, 0);
}

void TabCtrl_SetPadding(HWND w, int x, int y) {
    SendMessage(w, TCM_SETPADDING, 0, MAKELPARAM(cast(ushort)x, cast(ushort)y));
}

int TabCtrl_GetRowCount(HWND w) {
    return cast(int) SendMessage(w, TCM_GETROWCOUNT, 0, 0);
}

HWND TabCtrl_GetToolTips(HWND w) {
    return cast(HWND) SendMessage(w, TCM_GETTOOLTIPS, 0, 0);
}

void TabCtrl_SetToolTips(HWND w, HWND t) {
    SendMessage(w, TCM_SETTOOLTIPS, cast(WPARAM) t, 0);
}

int TabCtrl_GetCurFocus(HWND w) {
    return cast(int) SendMessage(w, TCM_GETCURFOCUS, 0, 0);
}

void TabCtrl_SetCurFocus(HWND w, int i) {
    SendMessage(w, TCM_SETCURFOCUS, i, 0);
}

HIMAGELIST TabCtrl_GetImageList(HWND w) {
    return cast(HIMAGELIST) SendMessage(w, TCM_GETIMAGELIST, 0, 0);
}

HIMAGELIST TabCtrl_SetImageList(HWND w, HIMAGELIST h) {
    return cast(HIMAGELIST) SendMessage(w, TCM_SETIMAGELIST, 0,
      cast(LPARAM) h);
}

int TabCtrl_GetItemCount(HWND w) {
    return cast(int) SendMessage(w, TCM_GETITEMCOUNT, 0, 0);
}

extern (Windows) BOOL _TrackMouseEvent(LPTRACKMOUSEEVENT);

HTREEITEM TreeView_InsertItem(HWND w, LPTVINSERTSTRUCT i) {
    return cast(HTREEITEM) SendMessage(w, TVM_INSERTITEM, 0, cast(LPARAM) i);
}

BOOL TreeView_DeleteItem(HWND w, HTREEITEM i) {
    return cast(BOOL) SendMessage(w, TVM_DELETEITEM, 0, cast(LPARAM) i);
}

BOOL TreeView_DeleteAllItems(HWND w) {
    return cast(BOOL) SendMessage(w, TVM_DELETEITEM, 0, cast(LPARAM) TVI_ROOT);
}

BOOL TreeView_Expand(HWND w, HTREEITEM i, UINT c) {
    return cast(BOOL) SendMessage(w, TVM_EXPAND, c, cast(LPARAM) i);
}

BOOL TreeView_GetItemRect(HWND w, HTREEITEM i, LPRECT p, BOOL c) {
    *cast(HTREEITEM*) p = i;
    return cast(BOOL) SendMessage(w, TVM_GETITEMRECT, c, cast(LPARAM) p);
}

UINT TreeView_GetCount(HWND w) {
    return cast(UINT) SendMessage(w, TVM_GETCOUNT, 0, 0);
}

UINT TreeView_GetIndent(HWND w) {
    return cast(UINT) SendMessage(w, TVM_GETINDENT, 0, 0);
}

BOOL TreeView_SetIndent(HWND w, INT i) {
    return cast(BOOL) SendMessage(w, TVM_SETINDENT, i, 0);
}

HIMAGELIST TreeView_GetImageList(HWND w, INT i) {
    return cast(HIMAGELIST) SendMessage(w, TVM_GETIMAGELIST, i, 0);
}

HIMAGELIST TreeView_SetImageList(HWND w, HIMAGELIST h, INT i) {
    return cast(HIMAGELIST) SendMessage(w, TVM_SETIMAGELIST, i,
      cast(LPARAM) h);
}

HTREEITEM TreeView_GetNextItem(HWND w, HTREEITEM i, UINT c) {
    return cast(HTREEITEM) SendMessage(w, TVM_GETNEXTITEM, c, cast(LPARAM) i);
}

HTREEITEM TreeView_GetChild(HWND w, HTREEITEM i) {
    return TreeView_GetNextItem(w, i, TVGN_CHILD);
}

HTREEITEM TreeView_GetNextSibling(HWND w, HTREEITEM i) {
    return TreeView_GetNextItem(w, i, TVGN_NEXT);
}

HTREEITEM TreeView_GetPrevSibling(HWND w, HTREEITEM i) {
    return TreeView_GetNextItem(w, i, TVGN_PREVIOUS);
}

HTREEITEM TreeView_GetParent(HWND w, HTREEITEM i) {
    return TreeView_GetNextItem(w, i, TVGN_PARENT);
}

HTREEITEM TreeView_GetFirstVisible(HWND w) {
    return TreeView_GetNextItem(w, null, TVGN_FIRSTVISIBLE);
}

HTREEITEM TreeView_GetNextVisible(HWND w, HTREEITEM i) {
    return TreeView_GetNextItem(w, i, TVGN_NEXTVISIBLE);
}

HTREEITEM TreeView_GetPrevVisible(HWND w, HTREEITEM i) {
    return TreeView_GetNextItem(w, i, TVGN_PREVIOUSVISIBLE);
}

HTREEITEM TreeView_GetSelection(HWND w) {
    return TreeView_GetNextItem(w, null, TVGN_CARET);
}

HTREEITEM TreeView_GetDropHilight(HTREEITEM w) {
    return TreeView_GetNextItem(w, null, TVGN_DROPHILITE);
}

HTREEITEM TreeView_GetRoot(HWND w) {
    return TreeView_GetNextItem(w, null, TVGN_ROOT);
}

BOOL TreeView_Select(HWND w, HTREEITEM i, UINT c) {
    return cast(BOOL) SendMessage(w, TVM_SELECTITEM, c, cast(LPARAM) i);
}

BOOL TreeView_SelectItem(HWND w, HTREEITEM i) {
    return TreeView_Select(w, i, TVGN_CARET);
}

BOOL TreeView_SelectDropTarget(HWND w, HTREEITEM i) {
    return TreeView_Select(w, i, TVGN_DROPHILITE);
}

BOOL TreeView_SelectSetFirstVisible(HWND w, HTREEITEM i) {
    return TreeView_Select(w, i, TVGN_FIRSTVISIBLE);
}

BOOL TreeView_GetItem(HWND w, LPTVITEM i) {
 return cast(BOOL) SendMessage(w, TVM_GETITEM, 0, cast(LPARAM) i);
}

BOOL TreeView_SetItem(HWND w, const(TV_ITEM)* i) {
    return cast(BOOL) SendMessage(w, TVM_SETITEM, 0, cast(LPARAM) i);
}

HWND TreeView_EditLabel(HWND w, HTREEITEM i) {
    return cast(HWND) SendMessage(w, TVM_EDITLABEL, 0, cast(LPARAM) i);
}

HWND TreeView_GetEditControl(HWND w) {
    return cast(HWND) SendMessage(w, TVM_GETEDITCONTROL, 0, 0);
}

UINT TreeView_GetVisibleCount(HWND w) {
    return cast(UINT) SendMessage(w, TVM_GETVISIBLECOUNT, 0, 0);
}

HTREEITEM TreeView_HitTest(HWND w, LPTVHITTESTINFO p) {
    return cast(HTREEITEM) SendMessage(w, TVM_HITTEST, 0, cast(LPARAM) p);
}

HIMAGELIST TreeView_CreateDragImage(HWND w, HTREEITEM i) {
    return cast(HIMAGELIST) SendMessage(w, TVM_CREATEDRAGIMAGE, 0,
      cast(LPARAM) i);
}

BOOL TreeView_SortChildren(HWND w, HTREEITEM i, BOOL r) {
    return cast(BOOL) SendMessage(w, TVM_SORTCHILDREN, r, cast(LPARAM) i);
}

BOOL TreeView_EnsureVisible(HWND w, HTREEITEM i) {
    return cast(BOOL) SendMessage(w, TVM_ENSUREVISIBLE, 0, cast(LPARAM) i);
}

BOOL TreeView_SortChildrenCB(HWND w, LPTVSORTCB s, BOOL r) {
    return cast(BOOL) SendMessage(w, TVM_SORTCHILDRENCB, r, cast(LPARAM) s);
}

BOOL TreeView_EndEditLabelNow(HWND w, BOOL f) {
    return cast(BOOL) SendMessage(w, TVM_ENDEDITLABELNOW, f, 0);
}

BOOL TreeView_GetISearchString(HWND w, LPTSTR s) {
    return cast(BOOL) SendMessage(w, TVM_GETISEARCHSTRING, 0, cast(LPARAM) s);
}

static if (_WIN32_IE >= 0x300) {
    DWORD ListView_ApproximateViewRect(HWND w, int iw, int ih, int i) {
        return cast(DWORD) SendMessage(w, LVM_APPROXIMATEVIEWRECT, i,
          MAKELPARAM(cast(ushort)iw, cast(ushort)ih));
    }

    DWORD ListView_SetExtendedListViewStyle(HWND w, DWORD s) {
        return cast(DWORD) SendMessage(w, LVM_SETEXTENDEDLISTVIEWSTYLE, 0, s);
    }

    DWORD ListView_GetExtendedListViewStyle(HWND w) {
        return cast(DWORD) SendMessage(w, LVM_GETEXTENDEDLISTVIEWSTYLE, 0, 0);
    }

    BOOL ListView_SetColumnOrderArray(HWND w, int i, int* a) {
        return cast(BOOL) SendMessage(w, LVM_SETCOLUMNORDERARRAY,
          cast(WPARAM) i, cast(LPARAM) a);
    }

    BOOL ListView_GetColumnOrderArray(HWND w, int i, int* a) {
        return cast(BOOL) SendMessage(w, LVM_GETCOLUMNORDERARRAY,
          cast(WPARAM) i, cast(LPARAM) a);
    }

    HWND ListView_GetHeader(HWND w) {
        return cast(HWND) SendMessage(w, LVM_GETHEADER, 0, 0);
    }

    HCURSOR ListView_GetHotCursor(HWND w) {
        return cast(HCURSOR) SendMessage(w, LVM_GETHOTCURSOR, 0, 0);
    }

    INT ListView_GetHotItem(HWND w) {
        return cast(INT) SendMessage(w, LVM_GETHOTITEM, 0, 0);
    }

    BOOL ListView_GetSubItemRect(HWND w, int i, int isi, int c, LPRECT p) {
        if (p)
        {
            p.left = c;
            p.top = isi;
        }
        return cast(BOOL) SendMessage(w, LVM_GETSUBITEMRECT, i, cast(LPARAM) p);
    }

    HCURSOR ListView_SetHotCursor(HWND w, HCURSOR c) {
        return cast(HCURSOR) SendMessage(w, LVM_SETHOTCURSOR, 0,
          cast(LPARAM) c);
    }

    INT ListView_SetHotItem(HWND w, INT i) {
        return cast(INT) SendMessage(w, LVM_SETHOTITEM, cast(WPARAM) i, 0);
    }

    DWORD ListView_SetIconSpacing(HWND w, int x, int y) {
        return cast(DWORD) SendMessage(w, LVM_SETICONSPACING, 0,
          MAKELONG(cast(ushort)x, cast(ushort)y));
    }

    INT ListView_SubItemHitTest(HWND w, LPLVHITTESTINFO p) {
        return cast(INT) SendMessage(w, LVM_SUBITEMHITTEST, 0, cast(LPARAM) p);
    }

    BOOL ListView_SetItemCountEx(HWND w, int i, DWORD f) {
        return cast(BOOL) SendMessage(w, LVM_SETITEMCOUNT, i, cast(LPARAM) f);
    }

    extern (Windows) nothrow @nogc {
        WINBOOL ImageList_SetImageCount(HIMAGELIST, UINT);
        WINBOOL ImageList_Copy(HIMAGELIST, int, HIMAGELIST, int, UINT);
        WINBOOL ImageList_DrawIndirect(IMAGELISTDRAWPARAMS*);
    }

    int TabCtrl_SetMinTabWidth(HWND hwnd, int x) {
        return cast(int) SendMessage(hwnd, TCM_SETMINTABWIDTH, 0, x);
    }

    VOID TabCtrl_DeselectAll(HWND hwnd, UINT fExcludeFocus) {
        SendMessage(hwnd, TCM_DESELECTALL, fExcludeFocus, 0);
    }

    HWND TreeView_GetToolTips(HWND w) {
        return cast(HWND) SendMessage(w, TVM_GETTOOLTIPS, 0, 0);
    }

    HWND TreeView_SetToolTips(HWND w, HWND wt) {
        return cast(HWND) SendMessage(w, TVM_SETTOOLTIPS, cast(WPARAM) wt, 0);
    }
}

static if (_WIN32_IE >= 0x400) {
    BOOL ListView_GetBkImage(HWND h, LPLVBKIMAGE plvbki) {
        return cast(BOOL) SendMessage(h, LVM_GETBKIMAGE, 0,
          cast(LPARAM) plvbki);
    }

    BOOL ListView_SetBkImage(HWND h, LPLVBKIMAGE plvbki) {
        return cast(BOOL) SendMessage(h, LVM_SETBKIMAGE, 0,
          cast(LPARAM) plvbki);
    }

    DWORD ListView_SetExtendedListViewStyleEx(HWND w, DWORD m, DWORD s) {
        return cast(DWORD) SendMessage(w, LVM_SETEXTENDEDLISTVIEWSTYLE, m, s);
    }

    VOID ListView_SetWorkAreas(HWND w, INT n, LPRECT r) {
        SendMessage(w, LVM_SETWORKAREAS, cast(WPARAM) n, cast(LPARAM) r);
    }

    VOID ListView_GetWorkAreas(HWND w, INT n, LPRECT r) {
        SendMessage(w, LVM_GETWORKAREAS, cast(WPARAM) n, cast(LPARAM) r);
    }

    BOOL ListView_GetNumberOfWorkAreas(HWND w, LPUINT n) {
        return cast(BOOL) SendMessage(w, LVM_GETNUMBEROFWORKAREAS, 0,
          cast(LPARAM) n);
    }

    DWORD ListView_SetHoverTime(HWND w, DWORD t) {
        return cast(DWORD) SendMessage(w, LVM_SETHOVERTIME, 0,
          cast(LPARAM) t);
    }

    DWORD ListView_GetHoverTime(HWND w) {
        return cast(DWORD) SendMessage(w, LVM_GETHOVERTIME, 0, 0);
    }

    INT ListView_GetSelectionMark(HWND w) {
        return cast(INT) SendMessage(w, LVM_GETSELECTIONMARK, 0, 0);
    }

    INT ListView_SetSelectionMark(HWND w, INT i) {
        return cast(INT) SendMessage(w, LVM_SETSELECTIONMARK, 0, cast(LPARAM) i);
    }

    HWND ListView_SetToolTips(HWND w, HWND n) {
        return cast(HWND) SendMessage(w, LVM_SETTOOLTIPS, cast(WPARAM) n, 0);
    }

    HWND ListView_GetToolTips(HWND w) {
        return cast(HWND) SendMessage(w, LVM_GETTOOLTIPS, 0, 0);
    }

    BOOL ListView_SetUnicodeFormat(HWND w, BOOL f) {
        return cast(BOOL) SendMessage(w, LVM_SETUNICODEFORMAT,
          cast(WPARAM) f, 0);
    }

    BOOL ListView_GetUnicodeFormat(HWND w) {
        return cast(BOOL) SendMessage(w, LVM_GETUNICODEFORMAT, 0, 0);
    }

    BOOL TabCtrl_HighlightItem(HWND hwnd, INT i, WORD fHighlight) {
        return cast(BOOL) SendMessage(hwnd, TCM_HIGHLIGHTITEM,
          cast(WPARAM) i, cast(LPARAM) MAKELONG(fHighlight, 0));
    }

    DWORD TabCtrl_SetExtendedStyle(HWND hwnd, DWORD dw) {
        return cast(DWORD) SendMessage(hwnd, TCM_SETEXTENDEDSTYLE, 0, dw);
    }

    DWORD TabCtrl_GetExtendedStyle(HWND hwnd) {
        return cast(DWORD) SendMessage(hwnd, TCM_GETEXTENDEDSTYLE, 0, 0);
    }

    BOOL TabCtrl_SetUnicodeFormat(HWND hwnd, HWND fUnicode) {
        return cast(BOOL) SendMessage(hwnd, TCM_SETUNICODEFORMAT,
          cast(WPARAM) fUnicode, 0);
    }

    BOOL TabCtrl_GetUnicodeFormat(HWND hwnd) {
        return cast(BOOL) SendMessage(hwnd, TCM_GETUNICODEFORMAT, 0, 0);
    }

    COLORREF TreeView_GetBkColor(HWND w) {
        return cast(COLORREF) SendMessage(w, TVM_GETBKCOLOR, 0, 0);
    }

    COLORREF TreeView_GetInsertMarkColor(HWND w) {
        return cast(COLORREF) SendMessage(w, TVM_GETINSERTMARKCOLOR, 0, 0);
    }

    int TreeView_GetItemHeight(HWND w) {
        return cast(int) SendMessage(w, TVM_GETITEMHEIGHT, 0, 0);
    }

    UINT TreeView_GetScrollTime(HWND w) {
        return cast(UINT) SendMessage(w, TVM_GETSCROLLTIME, 0, 0);
    }

    COLORREF TreeView_GetTextColor(HWND w) {
        return cast(COLORREF) SendMessage(w, TVM_GETTEXTCOLOR, 0, 0);
    }

    COLORREF TreeView_SetBkColor(HWND w, COLORREF c) {
        return cast(COLORREF) SendMessage(w, TVM_SETBKCOLOR, 0,
          cast(LPARAM) c);
    }

    COLORREF TreeView_SetInsertMarkColor(HWND w, COLORREF c) {
        return cast(COLORREF) SendMessage(w, TVM_SETINSERTMARKCOLOR, 0,
          cast(LPARAM) c);
    }

    int TreeView_SetItemHeight(HWND w, SHORT h) {
        return cast(int) SendMessage(w, TVM_SETITEMHEIGHT, cast(WPARAM) h, 0);
    }

    UINT TreeView_SetScrollTime(HWND w, UINT t) {
        return cast(UINT) SendMessage(w, TVM_SETSCROLLTIME, cast(WPARAM) t, 0);
    }

    COLORREF TreeView_SetTextColor(HWND w, COLORREF c) {
        return cast(COLORREF) SendMessage(w, TVM_SETTEXTCOLOR, 0,
          cast(LPARAM) c);
    }

    BOOL TreeView_SetInsertMark(HWND w, HTREEITEM i, BOOL a) {
        return cast(BOOL) SendMessage(w, TVM_SETINSERTMARK, cast(WPARAM) a,
          cast(LPARAM) i);
    }

    BOOL TreeView_SetUnicodeFormat(HWND w, BOOL u) {
        return cast(BOOL) SendMessage(w, TVM_SETUNICODEFORMAT,
          cast(WPARAM) u, 0);
    }

    BOOL TreeView_GetUnicodeFormat(HWND w) {
        return cast(BOOL) SendMessage(w, TVM_GETUNICODEFORMAT, 0, 0);
    }

    HTREEITEM TreeView_GetLastVisible(HWND w) {
        return TreeView_GetNextItem(w, null, TVGN_LASTVISIBLE);
    }
}

static if (_WIN32_IE >= 0x500) {
    UINT TreeView_GetItemState(HWND w, HTREEITEM i, UINT m) {
        return cast(UINT) SendMessage(w, TVM_GETITEMSTATE, cast(WPARAM) i,
          cast(LPARAM) m);
    }

    BOOL TreeView_SetItemState(HWND w, HTREEITEM i, UINT d, UINT m) {
        TVITEM _tvi;
        _tvi.mask = TVIF_STATE;
        _tvi.hItem = i;
        _tvi.stateMask = m;
        _tvi.state = d;
        return cast(BOOL) SendMessage(w, TVM_SETITEM, 0, cast(LPARAM) &_tvi);
    }
}


//#ifdef _WIN32_WCE               // these are PPC only
/+
extern (Windows) {
    HWND  CommandBar_Create(HINSTANCE, HWND, int);
    BOOL  CommandBar_Show(HWND, BOOL);
    int   CommandBar_AddBitmap(HWND, HINSTANCE, int, int, int, int);
    HWND  CommandBar_InsertComboBox(HWND, HINSTANCE, int, UINT, WORD, WORD);
    BOOL  CommandBar_InsertMenubar(HWND, HINSTANCE, WORD, WORD );
    BOOL  CommandBar_InsertMenubarEx(HWND, HINSTANCE, LPTSTR, WORD);
    BOOL  CommandBar_DrawMenuBar(HWND, WORD);
    HMENU CommandBar_GetMenu(HWND, WORD);
    BOOL  CommandBar_AddAdornments(HWND, DWORD, DWORD);
    int   CommandBar_Height(HWND hwndCB);
}

// MinGW: These two are not in the DLL
void CommandBar_InsertButton(HWND hwnd, int i, LPTBBUTTON lptbbutton) {
    SendMessage(hwnd, TB_INSERTBUTTON, i, lptbbutton);
}
alias DestroyWindow CommandBar_Destroy;
+/
//#endif // _WIN32_WCE


static if (_WIN32_WINNT >= 0x501) {
    struct EDITBALLOONTIP
    {
        DWORD cbStruct;
        LPCWSTR pszTitle;
        LPCWSTR pszText;
        INT ttiIcon;
    }
    alias EDITBALLOONTIP* PEDITBALLOONTIP;

enum EM_SETCUEBANNER = ECM_FIRST + 1;
enum EM_GETCUEBANNER = ECM_FIRST + 2;
enum EM_SHOWBALLOONTIP = ECM_FIRST + 3;
enum EM_HIDEBALLOONTIP = ECM_FIRST + 4;
}

static if (_WIN32_WINNT >= 0x600) {
enum EM_SETHILITE = ECM_FIRST + 5;
enum EM_GETHILITE = ECM_FIRST + 6;
}

/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_ddeml.d)
 */
module core.sys.windows.ddeml;
version (Windows):

version (ANSI) {} else version = Unicode;
pragma(lib, "user32");

private import core.sys.windows.basetsd, core.sys.windows.windef, core.sys.windows.winnt;

enum : int {
    CP_WINANSI    = 1004,
    CP_WINUNICODE = 1200
}

enum : UINT {
    XTYPF_NOBLOCK = 2,
    XTYPF_NODATA  = 4,
    XTYPF_ACKREQ  = 8
}

enum : UINT {
    XCLASS_MASK         = 0xFC00,
    XCLASS_BOOL         = 0x1000,
    XCLASS_DATA         = 0x2000,
    XCLASS_FLAGS        = 0x4000,
    XCLASS_NOTIFICATION = 0x8000
}

enum : UINT {
    XST_NULL,
    XST_INCOMPLETE,
    XST_CONNECTED,
    XST_INIT1,
    XST_INIT2,
    XST_REQSENT,
    XST_DATARCVD,
    XST_POKESENT,
    XST_POKEACKRCVD,
    XST_EXECSENT,
    XST_EXECACKRCVD,
    XST_ADVSENT,
    XST_UNADVSENT,
    XST_ADVACKRCVD,
    XST_UNADVACKRCVD,
    XST_ADVDATASENT,
    XST_ADVDATAACKRCVD // = 16
}

enum : UINT {
    XTYP_ERROR           = XCLASS_NOTIFICATION | XTYPF_NOBLOCK,
    XTYP_ADVDATA         = 0x0010 | XCLASS_FLAGS,
    XTYP_ADVREQ          = 0x0020 | XCLASS_DATA | XTYPF_NOBLOCK,
    XTYP_ADVSTART        = 0x0030 | XCLASS_BOOL,
    XTYP_ADVSTOP         = 0x0040 | XCLASS_NOTIFICATION,
    XTYP_EXECUTE         = 0x0050 | XCLASS_FLAGS,
    XTYP_CONNECT         = 0x0060 | XCLASS_BOOL | XTYPF_NOBLOCK,
    XTYP_CONNECT_CONFIRM = 0x0070 | XCLASS_NOTIFICATION | XTYPF_NOBLOCK,
    XTYP_XACT_COMPLETE   = 0x0080 | XCLASS_NOTIFICATION,
    XTYP_POKE            = 0x0090 | XCLASS_FLAGS,
    XTYP_REGISTER        = 0x00A0 | XCLASS_NOTIFICATION | XTYPF_NOBLOCK,
    XTYP_REQUEST         = 0x00B0 | XCLASS_DATA,
    XTYP_DISCONNECT      = 0x00C0 | XCLASS_NOTIFICATION | XTYPF_NOBLOCK,
    XTYP_UNREGISTER      = 0x00D0 | XCLASS_NOTIFICATION | XTYPF_NOBLOCK,
    XTYP_WILDCONNECT     = 0x00E0 | XCLASS_DATA | XTYPF_NOBLOCK,
    XTYP_MONITOR         = 0X00F0 | XCLASS_NOTIFICATION | XTYPF_NOBLOCK,
    XTYP_MASK            = 0x00F0,
    XTYP_SHIFT           = 4
}

/+
#define TIMEOUT_ASYNC  0xFFFFFFFF
#define QID_SYNC       0xFFFFFFFF
+/

enum : UINT {
    ST_CONNECTED  =   1,
    ST_ADVISE     =   2,
    ST_ISLOCAL    =   4,
    ST_BLOCKED    =   8,
    ST_CLIENT     =  16,
    ST_TERMINATED =  32,
    ST_INLIST     =  64,
    ST_BLOCKNEXT  = 128,
    ST_ISSELF     = 256
}

/+
#define CADV_LATEACK 0xFFFF
+/

enum : UINT {
    DMLERR_NO_ERROR      = 0,
    DMLERR_FIRST         = 0x4000,
    DMLERR_ADVACKTIMEOUT = DMLERR_FIRST,
    DMLERR_BUSY,
    DMLERR_DATAACKTIMEOUT,
    DMLERR_DLL_NOT_INITIALIZED,
    DMLERR_DLL_USAGE,
    DMLERR_EXECACKTIMEOUT,
    DMLERR_INVALIDPARAMETER,
    DMLERR_LOW_MEMORY,
    DMLERR_MEMORY_ERROR,
    DMLERR_NOTPROCESSED,
    DMLERR_NO_CONV_ESTABLISHED,
    DMLERR_POKEACKTIMEOUT,
    DMLERR_POSTMSG_FAILED,
    DMLERR_REENTRANCY,
    DMLERR_SERVER_DIED,
    DMLERR_SYS_ERROR,
    DMLERR_UNADVACKTIMEOUT,
    DMLERR_UNFOUND_QUEUE_ID, // = 0x4011
    DMLERR_LAST          = DMLERR_UNFOUND_QUEUE_ID
}

/+
#define DDE_FACK    0x8000
#define DDE_FBUSY   0x4000
#define DDE_FDEFERUPD   0x4000
#define DDE_FACKREQ 0x8000
#define DDE_FRELEASE    0x2000
#define DDE_FREQUESTED  0x1000
#define DDE_FAPPSTATUS  0x00ff
#define DDE_FNOTPROCESSED   0
#define DDE_FACKRESERVED    (~(DDE_FACK|DDE_FBUSY|DDE_FAPPSTATUS))
#define DDE_FADVRESERVED    (~(DDE_FACKREQ|DDE_FDEFERUPD))
#define DDE_FDATRESERVED    (~(DDE_FACKREQ|DDE_FRELEASE|DDE_FREQUESTED))
#define DDE_FPOKRESERVED    (~DDE_FRELEASE)
#define MSGF_DDEMGR 0x8001
#define CBR_BLOCK   ((HDDEDATA)0xffffffff)
+/

enum DWORD
    APPCLASS_STANDARD         = 0,
    APPCLASS_MONITOR          = 0x00000001,
    APPCLASS_MASK             = 0x0000000F,
    APPCMD_CLIENTONLY         = 0x00000010,
    APPCMD_FILTERINITS        = 0x00000020,
    APPCMD_MASK               = 0x00000FF0,
    CBF_FAIL_SELFCONNECTIONS  = 0x00001000,
    CBF_FAIL_CONNECTIONS      = 0x00002000,
    CBF_FAIL_ADVISES          = 0x00004000,
    CBF_FAIL_EXECUTES         = 0x00008000,
    CBF_FAIL_POKES            = 0x00010000,
    CBF_FAIL_REQUESTS         = 0x00020000,
    CBF_FAIL_ALLSVRXACTIONS   = 0x0003f000,
    CBF_SKIP_CONNECT_CONFIRMS = 0x00040000,
    CBF_SKIP_REGISTRATIONS    = 0x00080000,
    CBF_SKIP_UNREGISTRATIONS  = 0x00100000,
    CBF_SKIP_DISCONNECTS      = 0x00200000,
    CBF_SKIP_ALLNOTIFICATIONS = 0x003c0000,
    MF_HSZ_INFO               = 0x01000000,
    MF_SENDMSGS               = 0x02000000,
    MF_POSTMSGS               = 0x04000000,
    MF_CALLBACKS              = 0x08000000,
    MF_ERRORS                 = 0x10000000,
    MF_LINKS                  = 0x20000000,
    MF_CONV                   = 0x40000000,
    MF_MASK                   = 0xFF000000;

enum : UINT {
    EC_ENABLEALL    = 0,
    EC_ENABLEONE    = ST_BLOCKNEXT,
    EC_DISABLE      = ST_BLOCKED,
    EC_QUERYWAITING = 2
}

enum : UINT {
    DNS_REGISTER   = 1,
    DNS_UNREGISTER = 2,
    DNS_FILTERON   = 4,
    DNS_FILTEROFF  = 8
}

/+
#define HDATA_APPOWNED  1
#define MAX_MONITORS    4
+/

enum : int {
    MH_CREATE  = 1,
    MH_KEEP    = 2,
    MH_DELETE  = 3,
    MH_CLEANUP = 4
}

mixin DECLARE_HANDLE!("HCONVLIST");
mixin DECLARE_HANDLE!("HCONV");
mixin DECLARE_HANDLE!("HSZ");
mixin DECLARE_HANDLE!("HDDEDATA");

extern (Windows) alias HDDEDATA
  function(UINT, UINT, HCONV, HSZ, HSZ, HDDEDATA, ULONG_PTR, ULONG_PTR) PFNCALLBACK;

struct HSZPAIR {
    HSZ hszSvc;
    HSZ hszTopic;
}
alias HSZPAIR* PHSZPAIR;

struct CONVCONTEXT {
    UINT                        cb = CONVCONTEXT.sizeof;
    UINT                        wFlags;
    UINT                        wCountryID;
    int                         iCodePage;
    DWORD                       dwLangID;
    DWORD                       dwSecurity;
    SECURITY_QUALITY_OF_SERVICE qos;
}
alias CONVCONTEXT* PCONVCONTEXT;

struct CONVINFO {
    DWORD       cb = CONVINFO.sizeof;
    DWORD_PTR   hUser;
    HCONV       hConvPartner;
    HSZ         hszSvcPartner;
    HSZ         hszServiceReq;
    HSZ         hszTopic;
    HSZ         hszItem;
    UINT        wFmt;
    UINT        wType;
    UINT        wStatus;
    UINT        wConvst;
    UINT        wLastError;
    HCONVLIST   hConvList;
    CONVCONTEXT ConvCtxt;
    HWND        hwnd;
    HWND        hwndPartner;
}
alias CONVINFO* PCONVINFO;

struct DDEML_MSG_HOOK_DATA {
    UINT_PTR uiLo;
    UINT_PTR uiHi;
    DWORD    cbData;
    DWORD[8] Data;
}

struct MONHSZSTRUCT {
    UINT     cb = MONHSZSTRUCT.sizeof;
    int      fsAction;
    DWORD    dwTime;
    HSZ      hsz;
    HANDLE   hTask;
    TCHAR[1] _str = 0;

    TCHAR* str() return { return _str.ptr; }
}
alias MONHSZSTRUCT* PMONHSZSTRUCT;

struct MONLINKSTRUCT {
    UINT   cb = MONLINKSTRUCT.sizeof;
    DWORD  dwTime;
    HANDLE hTask;
    BOOL   fEstablished;
    BOOL   fNoData;
    HSZ    hszSvc;
    HSZ    hszTopic;
    HSZ    hszItem;
    UINT   wFmt;
    BOOL   fServer;
    HCONV  hConvServer;
    HCONV  hConvClient;
}
alias MONLINKSTRUCT* PMONLINKSTRUCT;

struct MONCONVSTRUCT {
    UINT   cb = MONCONVSTRUCT.sizeof;
    BOOL   fConnect;
    DWORD  dwTime;
    HANDLE hTask;
    HSZ    hszSvc;
    HSZ    hszTopic;
    HCONV  hConvClient;
    HCONV  hConvServer;
}
alias MONCONVSTRUCT* PMONCONVSTRUCT;

struct MONCBSTRUCT {
    UINT        cb = MONCBSTRUCT.sizeof;
    DWORD       dwTime;
    HANDLE      hTask;
    DWORD       dwRet;
    UINT        wType;
    UINT        wFmt;
    HCONV       hConv;
    HSZ         hsz1;
    HSZ         hsz2;
    HDDEDATA    hData;
    ULONG_PTR   dwData1;
    ULONG_PTR   dwData2;
    CONVCONTEXT cc;
    DWORD       cbData;
    DWORD[8]    Data;
}
alias MONCBSTRUCT* PMONCBSTRUCT;

struct MONERRSTRUCT {
    UINT   cb = MONERRSTRUCT.sizeof;
    UINT   wLastError;
    DWORD  dwTime;
    HANDLE hTask;
}
alias MONERRSTRUCT* PMONERRSTRUCT;

struct MONMSGSTRUCT {
    UINT   cb = MONMSGSTRUCT.sizeof;
    HWND   hwndTo;
    DWORD  dwTime;
    HANDLE hTask;
    UINT   wMsg;
    WPARAM wParam;
    LPARAM lParam;
    DDEML_MSG_HOOK_DATA dmhd;
}
alias MONMSGSTRUCT* PMONMSGSTRUCT;

extern (Windows) {
    BOOL DdeAbandonTransaction(DWORD, HCONV, DWORD);
    PBYTE DdeAccessData(HDDEDATA, PDWORD);
    HDDEDATA DdeAddData(HDDEDATA, PBYTE, DWORD, DWORD);
    HDDEDATA DdeClientTransaction(PBYTE, DWORD, HCONV, HSZ, UINT, UINT,
      DWORD, PDWORD);
    int DdeCmpStringHandles(HSZ, HSZ);
    HCONV DdeConnect(DWORD, HSZ, HSZ, PCONVCONTEXT);
    HCONVLIST DdeConnectList(DWORD, HSZ, HSZ, HCONVLIST, PCONVCONTEXT);
    HDDEDATA DdeCreateDataHandle(DWORD, PBYTE, DWORD, DWORD, HSZ, UINT,
      UINT);
    HSZ DdeCreateStringHandleA(DWORD, LPSTR, int);
    HSZ DdeCreateStringHandleW(DWORD, LPWSTR, int);
    BOOL DdeDisconnect(HCONV);
    BOOL DdeDisconnectList(HCONVLIST);
    BOOL DdeEnableCallback(DWORD, HCONV, UINT);
    BOOL DdeFreeDataHandle(HDDEDATA);
    BOOL DdeFreeStringHandle(DWORD, HSZ);
    DWORD DdeGetData(HDDEDATA, PBYTE, DWORD, DWORD);
    UINT DdeGetLastError(DWORD);
    BOOL DdeImpersonateClient(HCONV);
    UINT DdeInitializeA(PDWORD, PFNCALLBACK, DWORD, DWORD);
    UINT DdeInitializeW(PDWORD, PFNCALLBACK, DWORD, DWORD);
    BOOL DdeKeepStringHandle(DWORD, HSZ);
    HDDEDATA DdeNameService(DWORD, HSZ, HSZ, UINT);
    BOOL DdePostAdvise(DWORD, HSZ, HSZ);
    UINT DdeQueryConvInfo(HCONV, DWORD, PCONVINFO);
    HCONV DdeQueryNextServer(HCONVLIST, HCONV);
    DWORD DdeQueryStringA(DWORD, HSZ, LPSTR, DWORD, int);
    DWORD DdeQueryStringW(DWORD, HSZ, LPWSTR, DWORD, int);
    HCONV DdeReconnect(HCONV);
    BOOL DdeSetUserHandle(HCONV, DWORD, DWORD_PTR);
    BOOL DdeUnaccessData(HDDEDATA);
    BOOL DdeUninitialize(DWORD);
}

const TCHAR[]
    SZDDESYS_TOPIC         = "System",
    SZDDESYS_ITEM_TOPICS   = "Topics",
    SZDDESYS_ITEM_SYSITEMS = "SysItems",
    SZDDESYS_ITEM_RTNMSG   = "ReturnMessage",
    SZDDESYS_ITEM_STATUS   = "Status",
    SZDDESYS_ITEM_FORMATS  = "Formats",
    SZDDESYS_ITEM_HELP     = "Help",
    SZDDE_ITEM_ITEMLIST    = "TopicItemList";

version (Unicode) {
    alias DdeCreateStringHandleW DdeCreateStringHandle;
    alias DdeInitializeW DdeInitialize;
    alias DdeQueryStringW DdeQueryString;
} else {
    alias DdeCreateStringHandleA DdeCreateStringHandle;
    alias DdeInitializeA DdeInitialize;
    alias DdeQueryStringA DdeQueryString;
}

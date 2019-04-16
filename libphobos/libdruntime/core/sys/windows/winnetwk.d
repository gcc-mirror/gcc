/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_winnetwk.d)
 */
module core.sys.windows.winnetwk;
version (Windows):

version (ANSI) {} else version = Unicode;
pragma(lib, "mpr");

private import core.sys.windows.winbase, core.sys.windows.winerror, core.sys.windows.winnt;

enum : DWORD {
    WNNC_NET_MSNET       = 0x00010000,
    WNNC_NET_LANMAN      = 0x00020000,
    WNNC_NET_NETWARE     = 0x00030000,
    WNNC_NET_VINES       = 0x00040000,
    WNNC_NET_10NET       = 0x00050000,
    WNNC_NET_LOCUS       = 0x00060000,
    WNNC_NET_SUN_PC_NFS  = 0x00070000,
    WNNC_NET_LANSTEP     = 0x00080000,
    WNNC_NET_9TILES      = 0x00090000,
    WNNC_NET_LANTASTIC   = 0x000A0000,
    WNNC_NET_AS400       = 0x000B0000,
    WNNC_NET_FTP_NFS     = 0x000C0000,
    WNNC_NET_PATHWORKS   = 0x000D0000,
    WNNC_NET_LIFENET     = 0x000E0000,
    WNNC_NET_POWERLAN    = 0x000F0000,
    WNNC_NET_BWNFS       = 0x00100000,
    WNNC_NET_COGENT      = 0x00110000,
    WNNC_NET_FARALLON    = 0x00120000,
    WNNC_NET_APPLETALK   = 0x00130000,
    WNNC_NET_INTERGRAPH  = 0x00140000,
    WNNC_NET_SYMFONET    = 0x00150000,
    WNNC_NET_CLEARCASE   = 0x00160000,
    WNNC_NET_FRONTIER    = 0x00170000,
    WNNC_NET_BMC         = 0x00180000,
    WNNC_NET_DCE         = 0x00190000,
    WNNC_NET_AVID        = 0x001A0000,
    WNNC_NET_DOCUSPACE   = 0x001B0000,
    WNNC_NET_MANGOSOFT   = 0x001C0000,
    WNNC_NET_SERNET      = 0x001D0000,
    WNNC_NET_DECORB      = 0x00200000,
    WNNC_NET_PROTSTOR    = 0x00210000,
    WNNC_NET_FJ_REDIR    = 0x00220000,
    WNNC_NET_DISTINCT    = 0x00230000,
    WNNC_NET_TWINS       = 0x00240000,
    WNNC_NET_RDR2SAMPLE  = 0x00250000,
    WNNC_NET_CSC         = 0x00260000,
    WNNC_NET_3IN1        = 0x00270000,
    WNNC_NET_EXTENDNET   = 0x00290000,
    WNNC_NET_OBJECT_DIRE = 0x00300000,
    WNNC_NET_MASFAX      = 0x00310000,
    WNNC_NET_HOB_NFS     = 0x00320000,
    WNNC_NET_SHIVA       = 0x00330000,
    WNNC_NET_IBMAL       = 0x00340000,
    WNNC_CRED_MANAGER    = 0xFFFF0000
}

enum : DWORD {
    RESOURCE_CONNECTED  = 1,
    RESOURCE_GLOBALNET  = 2,
    RESOURCE_REMEMBERED = 3,
    RESOURCE_RECENT     = 4,
    RESOURCE_CONTEXT    = 5
}

enum DWORD
    RESOURCETYPE_ANY      = 0,
    RESOURCETYPE_DISK     = 1,
    RESOURCETYPE_PRINT    = 2,
    RESOURCETYPE_RESERVED = 8,
    RESOURCETYPE_UNKNOWN  = 0xFFFFFFFF;

enum DWORD
    RESOURCEUSAGE_CONNECTABLE   = 0x00000001,
    RESOURCEUSAGE_CONTAINER     = 0x00000002,
    RESOURCEUSAGE_NOLOCALDEVICE = 0x00000004,
    RESOURCEUSAGE_SIBLING       = 0x00000008,
    RESOURCEUSAGE_ATTACHED      = 0x00000010,
    RESOURCEUSAGE_ALL           = (RESOURCEUSAGE_CONNECTABLE
                                  | RESOURCEUSAGE_CONTAINER
                                  | RESOURCEUSAGE_ATTACHED),
    RESOURCEUSAGE_RESERVED      = 0x80000000;

enum : DWORD {
    RESOURCEDISPLAYTYPE_GENERIC,
    RESOURCEDISPLAYTYPE_DOMAIN,
    RESOURCEDISPLAYTYPE_SERVER,
    RESOURCEDISPLAYTYPE_SHARE,
    RESOURCEDISPLAYTYPE_FILE,
    RESOURCEDISPLAYTYPE_GROUP,
    RESOURCEDISPLAYTYPE_NETWORK,
    RESOURCEDISPLAYTYPE_ROOT,
    RESOURCEDISPLAYTYPE_SHAREADMIN,
    RESOURCEDISPLAYTYPE_DIRECTORY,
    RESOURCEDISPLAYTYPE_TREE // = 10
}

enum NETPROPERTY_PERSISTENT = 1;

enum DWORD
    CONNECT_UPDATE_PROFILE =   1,
    CONNECT_UPDATE_RECENT  =   2,
    CONNECT_TEMPORARY      =   4,
    CONNECT_INTERACTIVE    =   8,
    CONNECT_PROMPT         =  16,
    CONNECT_NEED_DRIVE     =  32,
    CONNECT_REFCOUNT       =  64,
    CONNECT_REDIRECT       = 128,
    CONNECT_LOCALDRIVE     = 256,
    CONNECT_CURRENT_MEDIA  = 512;

enum DWORD
    CONNDLG_RO_PATH     =  1,
    CONNDLG_CONN_POINT  =  2,
    CONNDLG_USE_MRU     =  4,
    CONNDLG_HIDE_BOX    =  8,
    CONNDLG_PERSIST     = 16,
    CONNDLG_NOT_PERSIST = 32;

enum DWORD
    DISC_UPDATE_PROFILE =  1,
    DISC_NO_FORCE       = 64;

enum DWORD
    WNFMT_MULTILINE   =  1,
    WNFMT_ABBREVIATED =  2,
    WNFMT_INENUM      = 16,
    WNFMT_CONNECTION  = 32;

enum : DWORD {
    WN_SUCCESS                   = NO_ERROR,
    WN_NO_ERROR                  = NO_ERROR,
    WN_NOT_SUPPORTED             = ERROR_NOT_SUPPORTED,
    WN_CANCEL                    = ERROR_CANCELLED,
    WN_RETRY                     = ERROR_RETRY,
    WN_NET_ERROR                 = ERROR_UNEXP_NET_ERR,
    WN_MORE_DATA                 = ERROR_MORE_DATA,
    WN_BAD_POINTER               = ERROR_INVALID_ADDRESS,
    WN_BAD_VALUE                 = ERROR_INVALID_PARAMETER,
    WN_BAD_USER                  = ERROR_BAD_USERNAME,
    WN_BAD_PASSWORD              = ERROR_INVALID_PASSWORD,
    WN_ACCESS_DENIED             = ERROR_ACCESS_DENIED,
    WN_FUNCTION_BUSY             = ERROR_BUSY,
    WN_WINDOWS_ERROR             = ERROR_UNEXP_NET_ERR,
    WN_OUT_OF_MEMORY             = ERROR_NOT_ENOUGH_MEMORY,
    WN_NO_NETWORK                = ERROR_NO_NETWORK,
    WN_EXTENDED_ERROR            = ERROR_EXTENDED_ERROR,
    WN_BAD_LEVEL                 = ERROR_INVALID_LEVEL,
    WN_BAD_HANDLE                = ERROR_INVALID_HANDLE,
    WN_NOT_INITIALIZING          = ERROR_ALREADY_INITIALIZED,
    WN_NO_MORE_DEVICES           = ERROR_NO_MORE_DEVICES,
    WN_NOT_CONNECTED             = ERROR_NOT_CONNECTED,
    WN_OPEN_FILES                = ERROR_OPEN_FILES,
    WN_DEVICE_IN_USE             = ERROR_DEVICE_IN_USE,
    WN_BAD_NETNAME               = ERROR_BAD_NET_NAME,
    WN_BAD_LOCALNAME             = ERROR_BAD_DEVICE,
    WN_ALREADY_CONNECTED         = ERROR_ALREADY_ASSIGNED,
    WN_DEVICE_ERROR              = ERROR_GEN_FAILURE,
    WN_CONNECTION_CLOSED         = ERROR_CONNECTION_UNAVAIL,
    WN_NO_NET_OR_BAD_PATH        = ERROR_NO_NET_OR_BAD_PATH,
    WN_BAD_PROVIDER              = ERROR_BAD_PROVIDER,
    WN_CANNOT_OPEN_PROFILE       = ERROR_CANNOT_OPEN_PROFILE,
    WN_BAD_PROFILE               = ERROR_BAD_PROFILE,
    WN_BAD_DEV_TYPE              = ERROR_BAD_DEV_TYPE,
    WN_DEVICE_ALREADY_REMEMBERED = ERROR_DEVICE_ALREADY_REMEMBERED,
    WN_NO_MORE_ENTRIES           = ERROR_NO_MORE_ITEMS,
    WN_NOT_CONTAINER             = ERROR_NOT_CONTAINER,
    WN_NOT_AUTHENTICATED         = ERROR_NOT_AUTHENTICATED,
    WN_NOT_LOGGED_ON             = ERROR_NOT_LOGGED_ON,
    WN_NOT_VALIDATED             = ERROR_NO_LOGON_SERVERS
}

enum : DWORD {
    UNIVERSAL_NAME_INFO_LEVEL = 1,
    REMOTE_NAME_INFO_LEVEL
}

enum DWORD
    NETINFO_DLL16      = 1,
    NETINFO_DISKRED    = 4,
    NETINFO_PRINTERRED = 8;

enum DWORD
    RP_LOGON   = 1,
    RP_INIFILE = 2;

enum DWORD PP_DISPLAYERRORS = 1;

enum DWORD
    WNCON_FORNETCARD = 1,
    WNCON_NOTROUTED  = 2,
    WNCON_SLOWLINK   = 4,
    WNCON_DYNAMIC    = 8;

struct NETRESOURCEA {
    DWORD dwScope;
    DWORD dwType;
    DWORD dwDisplayType;
    DWORD dwUsage;
    LPSTR lpLocalName;
    LPSTR lpRemoteName;
    LPSTR lpComment;
    LPSTR lpProvider;
}
alias NETRESOURCEA* LPNETRESOURCEA;

struct NETRESOURCEW {
    DWORD  dwScope;
    DWORD  dwType;
    DWORD  dwDisplayType;
    DWORD  dwUsage;
    LPWSTR lpLocalName;
    LPWSTR lpRemoteName;
    LPWSTR lpComment ;
    LPWSTR lpProvider;
}
alias NETRESOURCEW* LPNETRESOURCEW;

struct CONNECTDLGSTRUCTA {
    DWORD          cbStructure;
    HWND           hwndOwner;
    LPNETRESOURCEA lpConnRes;
    DWORD          dwFlags;
    DWORD          dwDevNum;
}
alias CONNECTDLGSTRUCTA* LPCONNECTDLGSTRUCTA;

struct CONNECTDLGSTRUCTW {
    DWORD          cbStructure;
    HWND           hwndOwner;
    LPNETRESOURCEW lpConnRes;
    DWORD          dwFlags;
    DWORD          dwDevNum;
}
alias CONNECTDLGSTRUCTW* LPCONNECTDLGSTRUCTW;

struct DISCDLGSTRUCTA {
    DWORD cbStructure;
    HWND  hwndOwner;
    LPSTR lpLocalName;
    LPSTR lpRemoteName;
    DWORD dwFlags;
}
alias DISCDLGSTRUCTA* LPDISCDLGSTRUCTA;

struct DISCDLGSTRUCTW {
    DWORD  cbStructure;
    HWND   hwndOwner;
    LPWSTR lpLocalName;
    LPWSTR lpRemoteName;
    DWORD  dwFlags;
}
alias DISCDLGSTRUCTW* LPDISCDLGSTRUCTW;

struct UNIVERSAL_NAME_INFOA {
    LPSTR lpUniversalName;
}
alias UNIVERSAL_NAME_INFOA* LPUNIVERSAL_NAME_INFOA;

struct UNIVERSAL_NAME_INFOW {
    LPWSTR lpUniversalName;
}
alias UNIVERSAL_NAME_INFOW* LPUNIVERSAL_NAME_INFOW;

struct REMOTE_NAME_INFOA {
    LPSTR lpUniversalName;
    LPSTR lpConnectionName;
    LPSTR lpRemainingPath;
}
alias REMOTE_NAME_INFOA* LPREMOTE_NAME_INFOA;

struct REMOTE_NAME_INFOW {
    LPWSTR lpUniversalName;
    LPWSTR lpConnectionName;
    LPWSTR lpRemainingPath;
}
alias REMOTE_NAME_INFOW* LPREMOTE_NAME_INFOW;

struct NETINFOSTRUCT {
    DWORD cbStructure;
    DWORD dwProviderVersion;
    DWORD dwStatus;
    DWORD dwCharacteristics;
    ULONG_PTR dwHandle;
    WORD  wNetType;
    DWORD dwPrinters;
    DWORD dwDrives;
}
alias NETINFOSTRUCT* LPNETINFOSTRUCT;

extern (Windows) {
    alias UINT function(LPCSTR, LPSTR, UINT) PFNGETPROFILEPATHA;
    alias UINT function(LPCWSTR, LPWSTR, UINT) PFNGETPROFILEPATHW;
    alias UINT function(LPCSTR, LPCSTR, DWORD) PFNRECONCILEPROFILEA;
    alias UINT function(LPCWSTR, LPCWSTR, DWORD) PFNRECONCILEPROFILEW;
    alias BOOL function(HWND, LPCSTR, LPCSTR, LPCSTR, DWORD)
      PFNPROCESSPOLICIESA;
    alias BOOL function(HWND, LPCWSTR, LPCWSTR, LPCWSTR, DWORD)
      PFNPROCESSPOLICIESW;
}

struct NETCONNECTINFOSTRUCT {
    DWORD cbStructure;
    DWORD dwFlags;
    DWORD dwSpeed;
    DWORD dwDelay;
    DWORD dwOptDataSize;
}
alias NETCONNECTINFOSTRUCT* LPNETCONNECTINFOSTRUCT;

extern (Windows) {
    DWORD WNetAddConnection2A(LPNETRESOURCEA, LPCSTR, LPCSTR, DWORD);
    DWORD WNetAddConnection2W(LPNETRESOURCEW, LPCWSTR, LPCWSTR, DWORD);
    DWORD WNetAddConnection3A(HWND, LPNETRESOURCEA, LPCSTR, LPCSTR, DWORD);
    DWORD WNetAddConnection3W(HWND, LPNETRESOURCEW, LPCWSTR, LPCWSTR, DWORD);
    DWORD WNetCancelConnection2A(LPCSTR, DWORD, BOOL);
    DWORD WNetCancelConnection2W(LPCWSTR, DWORD, BOOL);
    DWORD WNetGetConnectionA(LPCSTR, LPSTR, PDWORD);
    DWORD WNetGetConnectionW(LPCWSTR, LPWSTR, PDWORD);
    DWORD WNetUseConnectionA(HWND, LPNETRESOURCEA, LPCSTR, LPCSTR, DWORD,
      LPSTR, PDWORD, PDWORD);
    DWORD WNetUseConnectionW(HWND, LPNETRESOURCEW, LPCWSTR, LPCWSTR, DWORD,
      LPWSTR, PDWORD, PDWORD);
    DWORD WNetSetConnectionA(LPCSTR, DWORD, PVOID);
    DWORD WNetSetConnectionW(LPCWSTR, DWORD, PVOID);
    DWORD WNetConnectionDialog(HWND, DWORD);
    DWORD WNetDisconnectDialog(HWND, DWORD);
    DWORD WNetConnectionDialog1A(LPCONNECTDLGSTRUCTA);
    DWORD WNetConnectionDialog1W(LPCONNECTDLGSTRUCTW);
    DWORD WNetDisconnectDialog1A(LPDISCDLGSTRUCTA);
    DWORD WNetDisconnectDialog1W(LPDISCDLGSTRUCTW);
    DWORD WNetOpenEnumA(DWORD, DWORD, DWORD, LPNETRESOURCEA, LPHANDLE);
    DWORD WNetOpenEnumW(DWORD, DWORD, DWORD, LPNETRESOURCEW, LPHANDLE);
    DWORD WNetEnumResourceA(HANDLE, PDWORD, PVOID, PDWORD);
    DWORD WNetEnumResourceW(HANDLE, PDWORD, PVOID, PDWORD);
    DWORD WNetCloseEnum(HANDLE);
    DWORD WNetGetUniversalNameA(LPCSTR, DWORD, PVOID, PDWORD);
    DWORD WNetGetUniversalNameW(LPCWSTR, DWORD, PVOID, PDWORD);
    DWORD WNetGetUserA(LPCSTR, LPSTR, PDWORD);
    DWORD WNetGetUserW(LPCWSTR, LPWSTR, PDWORD);
    DWORD WNetGetProviderNameA(DWORD, LPSTR, PDWORD);
    DWORD WNetGetProviderNameW(DWORD, LPWSTR, PDWORD);
    DWORD WNetGetNetworkInformationA(LPCSTR, LPNETINFOSTRUCT);
    DWORD WNetGetNetworkInformationW(LPCWSTR, LPNETINFOSTRUCT);
    DWORD WNetGetResourceInformationA(LPNETRESOURCEA, LPVOID, LPDWORD,
      LPSTR*);
    DWORD WNetGetResourceInformationW(LPNETRESOURCEA, LPVOID, LPDWORD,
      LPWSTR*);
    DWORD WNetGetResourceParentA(LPNETRESOURCEA, LPVOID, LPDWORD);
    DWORD WNetGetResourceParentW(LPNETRESOURCEW, LPVOID, LPDWORD);
    DWORD WNetGetLastErrorA(PDWORD, LPSTR, DWORD, LPSTR, DWORD);
    DWORD WNetGetLastErrorW(PDWORD, LPWSTR, DWORD, LPWSTR, DWORD);
    DWORD MultinetGetConnectionPerformanceA(LPNETRESOURCEA,
      LPNETCONNECTINFOSTRUCT);
    DWORD MultinetGetConnectionPerformanceW(LPNETRESOURCEW,
      LPNETCONNECTINFOSTRUCT);
    deprecated {
        DWORD WNetAddConnectionA(LPCSTR, LPCSTR, LPCSTR);
        DWORD WNetAddConnectionW(LPCWSTR, LPCWSTR, LPCWSTR);
        DWORD WNetCancelConnectionA(LPCSTR, BOOL);
        DWORD WNetCancelConnectionW(LPCWSTR, BOOL);
    }
}

version (Unicode) {
    alias PFNGETPROFILEPATHW PFNGETPROFILEPATH;
    alias PFNRECONCILEPROFILEW PFNRECONCILEPROFILE;
    alias PFNPROCESSPOLICIESW PFNPROCESSPOLICIES;
    alias NETRESOURCEW NETRESOURCE;
    alias CONNECTDLGSTRUCTW CONNECTDLGSTRUCT;
    alias DISCDLGSTRUCTW DISCDLGSTRUCT;
    alias REMOTE_NAME_INFOW REMOTE_NAME_INFO;
    alias UNIVERSAL_NAME_INFOW UNIVERSAL_NAME_INFO;
    alias WNetAddConnection2W WNetAddConnection2;
    alias WNetAddConnection3W WNetAddConnection3;
    alias WNetCancelConnection2W WNetCancelConnection2;
    alias WNetGetConnectionW WNetGetConnection;
    alias WNetUseConnectionW WNetUseConnection;
    alias WNetSetConnectionW WNetSetConnection;
    alias WNetConnectionDialog1W WNetConnectionDialog1;
    alias WNetDisconnectDialog1W WNetDisconnectDialog1;
    alias WNetOpenEnumW WNetOpenEnum;
    alias WNetEnumResourceW WNetEnumResource;
    alias WNetGetUniversalNameW WNetGetUniversalName;
    alias WNetGetUserW WNetGetUser;
    alias WNetGetProviderNameW WNetGetProviderName;
    alias WNetGetNetworkInformationW WNetGetNetworkInformation;
    alias WNetGetResourceInformationW WNetGetResourceInformation;
    alias WNetGetResourceParentW WNetGetResourceParent;
    alias WNetGetLastErrorW WNetGetLastError;
    alias MultinetGetConnectionPerformanceW MultinetGetConnectionPerformance;
    deprecated {
        alias WNetAddConnectionW WNetAddConnection;
        alias WNetCancelConnectionW WNetCancelConnection;
    }
} else {
    alias PFNGETPROFILEPATHA PFNGETPROFILEPATH;
    alias PFNRECONCILEPROFILEA PFNRECONCILEPROFILE;
    alias PFNPROCESSPOLICIESA PFNPROCESSPOLICIES;
    alias NETRESOURCEA NETRESOURCE;
    alias CONNECTDLGSTRUCTA CONNECTDLGSTRUCT;
    alias DISCDLGSTRUCTA DISCDLGSTRUCT;
    alias REMOTE_NAME_INFOA REMOTE_NAME_INFO;
    alias UNIVERSAL_NAME_INFOA UNIVERSAL_NAME_INFO;
    alias WNetAddConnection2A WNetAddConnection2;
    alias WNetAddConnection3A WNetAddConnection3;
    alias WNetCancelConnection2A WNetCancelConnection2;
    alias WNetGetConnectionA WNetGetConnection;
    alias WNetUseConnectionA WNetUseConnection;
    alias WNetSetConnectionA WNetSetConnection;
    alias WNetConnectionDialog1A WNetConnectionDialog1;
    alias WNetDisconnectDialog1A WNetDisconnectDialog1;
    alias WNetOpenEnumA WNetOpenEnum;
    alias WNetEnumResourceA WNetEnumResource;
    alias WNetGetUniversalNameA WNetGetUniversalName;
    alias WNetGetUserA WNetGetUser;
    alias WNetGetProviderNameA WNetGetProviderName;
    alias WNetGetNetworkInformationA WNetGetNetworkInformation;
    alias WNetGetResourceInformationA WNetGetResourceInformation;
    alias WNetGetResourceParentA WNetGetResourceParent;
    alias WNetGetLastErrorA WNetGetLastError;
    alias MultinetGetConnectionPerformanceA MultinetGetConnectionPerformance;
    deprecated {
        alias WNetAddConnectionA WNetAddConnection;
        alias WNetCancelConnectionA WNetCancelConnection;
    }
}

alias NETRESOURCE* LPNETRESOURCE;
alias CONNECTDLGSTRUCT* LPCONNECTDLGSTRUCT;
alias DISCDLGSTRUCT* LPDISCDLGSTRUCT;
alias REMOTE_NAME_INFO* LPREMOTE_NAME_INFO;
alias UNIVERSAL_NAME_INFO* LPUNIVERSAL_NAME_INFO;

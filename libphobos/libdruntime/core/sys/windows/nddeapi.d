/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_nddeapi.d)
 */
module core.sys.windows.nddeapi;
version (Windows):

version (ANSI) {} else version = Unicode;

private import core.sys.windows.windef;

// FIXME: check types and grouping of constants

/+
#ifndef CNLEN /* also in lmcons.h */
#define CNLEN 15
#define UNCLEN (CNLEN + 2)
#endif
+/

enum char    SEP_CHAR  = ',';
const char[]  BAR_CHAR  = "|";
enum wchar   SEP_WCHAR = ',';
const wchar[] BAR_WCHAR = "|";

enum {
    NDDE_NO_ERROR,
    NDDE_ACCESS_DENIED,
    NDDE_BUF_TOO_SMALL,
    NDDE_ERROR_MORE_DATA,
    NDDE_INVALID_SERVER,
    NDDE_INVALID_SHARE,
    NDDE_INVALID_PARAMETER,
    NDDE_INVALID_LEVEL,
    NDDE_INVALID_PASSWORD,
    NDDE_INVALID_ITEMNAME,
    NDDE_INVALID_TOPIC,
    NDDE_INTERNAL_ERROR,
    NDDE_OUT_OF_MEMORY,
    NDDE_INVALID_APPNAME,
    NDDE_NOT_IMPLEMENTED,
    NDDE_SHARE_ALREADY_EXIST,
    NDDE_SHARE_NOT_EXIST,
    NDDE_INVALID_FILENAME,
    NDDE_NOT_RUNNING,
    NDDE_INVALID_WINDOW,
    NDDE_INVALID_SESSION,
    NDDE_INVALID_ITEM_LIST,
    NDDE_SHARE_DATA_CORRUPTED,
    NDDE_REGISTRY_ERROR,
    NDDE_CANT_ACCESS_SERVER,
    NDDE_INVALID_SPECIAL_COMMAND,
    NDDE_INVALID_SECURITY_DESC,
    NDDE_TRUST_SHARE_FAIL
}

enum size_t
    MAX_NDDESHARENAME = 256,
    MAX_DOMAINNAME = 15,
    MAX_USERNAME = 15,
    MAX_APPNAME = 255,
    MAX_TOPICNAME = 255,
    MAX_ITEMNAME = 255;

enum NDDEF_NOPASSWORDPROMPT = 1;
enum NDDEF_NOCACHELOOKUP    = 2;
enum NDDEF_STRIP_NDDE       = 4;

enum SHARE_TYPE_OLD         = 1;
enum SHARE_TYPE_NEW         = 2;
enum SHARE_TYPE_STATIC      = 4;

enum uint
    NDDE_CMD_SHOW_MASK     = 0x0000FFFF,
    NDDE_TRUST_CMD_SHOW    = 0x10000000,
    NDDE_TRUST_SHARE_DEL   = 0x20000000,
    NDDE_TRUST_SHARE_INIT  = 0x40000000,
    NDDE_TRUST_SHARE_START = 0x80000000;

struct NDdeShareInfo_tag {
    LONG    lRevision;
    LPTSTR  lpszShareName;
    LONG    lShareType;
    LPTSTR  lpszAppTopicList;
    LONG    fSharedFlag;
    LONG    fService;
    LONG    fStartAppFlag;
    LONG    nCmdShow;
    LONG[2] qModifyId;
    LONG    cNumItems;
    LPTSTR  lpszItemList;
}
extern (C) {    // huh?
    NDdeShareInfo_tag  NDDESHAREINFO;
    NDdeShareInfo_tag* PNDDESHAREINFO;
}

extern (Windows) {
    UINT NDdeGetErrorStringA(UINT, LPSTR, DWORD);
    UINT NDdeGetErrorStringW(UINT, LPWSTR, DWORD);
    UINT NDdeGetShareSecurityA(LPSTR, LPSTR, SECURITY_INFORMATION,
      PSECURITY_DESCRIPTOR, DWORD, PDWORD);
    UINT NDdeGetShareSecurityW(LPWSTR, LPWSTR, SECURITY_INFORMATION,
      PSECURITY_DESCRIPTOR, DWORD, PDWORD);
    UINT NDdeGetTrustedShareA(LPSTR, LPSTR, PDWORD, PDWORD, PDWORD);
    UINT NDdeGetTrustedShareW(LPWSTR, LPWSTR, PDWORD, PDWORD, PDWORD);
    BOOL NDdeIsValidShareNameA(LPSTR);
    BOOL NDdeIsValidShareNameW(LPWSTR);
    BOOL NDdeIsValidAppTopicListA(LPSTR);
    BOOL NDdeIsValidAppTopicListW(LPWSTR);
    UINT NDdeSetShareSecurityA(LPSTR, LPSTR, SECURITY_INFORMATION,
      PSECURITY_DESCRIPTOR);
    UINT NDdeSetShareSecurityW(LPWSTR, LPWSTR, SECURITY_INFORMATION,
      PSECURITY_DESCRIPTOR);
    UINT NDdeSetTrustedShareA(LPSTR, LPSTR, DWORD);
    UINT NDdeSetTrustedShareW(LPWSTR, LPWSTR, DWORD);
    UINT NDdeShareAddA(LPSTR, UINT, PSECURITY_DESCRIPTOR, PBYTE, DWORD);
    UINT NDdeShareAddW(LPWSTR, UINT, PSECURITY_DESCRIPTOR, PBYTE, DWORD);
    UINT NDdeShareDelA(LPSTR, LPSTR, UINT);
    UINT NDdeShareDelW(LPWSTR, LPWSTR, UINT);
    UINT NDdeShareEnumA(LPSTR, UINT, PBYTE, DWORD, PDWORD, PDWORD);
    UINT NDdeShareEnumW(LPWSTR, UINT, PBYTE, DWORD, PDWORD, PDWORD);
    UINT NDdeShareGetInfoA(LPSTR, LPSTR, UINT, PBYTE, DWORD, PDWORD, PWORD);
    UINT NDdeShareGetInfoW(LPWSTR, LPWSTR, UINT, PBYTE, DWORD, PDWORD, PWORD);
    UINT NDdeShareSetInfoA(LPSTR, LPSTR, UINT, PBYTE, DWORD, WORD);
    UINT NDdeShareSetInfoW(LPWSTR, LPWSTR, UINT, PBYTE, DWORD, WORD);
    UINT NDdeTrustedShareEnumA(LPSTR, UINT, PBYTE, DWORD, PDWORD, PDWORD);
    UINT NDdeTrustedShareEnumW(LPWSTR, UINT, PBYTE, DWORD, PDWORD, PDWORD);
}

version (Unicode) {
    alias NDdeShareAddW NDdeShareAdd;
    alias NDdeShareDelW NDdeShareDel;
    alias NDdeSetShareSecurityW NDdeSetShareSecurity;
    alias NDdeGetShareSecurityW NDdeGetShareSecurity;
    alias NDdeShareEnumW NDdeShareEnum;
    alias NDdeShareGetInfoW NDdeShareGetInfo;
    alias NDdeShareSetInfoW NDdeShareSetInfo;
    alias NDdeGetErrorStringW NDdeGetErrorString;
    alias NDdeIsValidShareNameW NDdeIsValidShareName;
    alias NDdeIsValidAppTopicListW NDdeIsValidAppTopicList;
    alias NDdeSetTrustedShareW NDdeSetTrustedShare;
    alias NDdeGetTrustedShareW NDdeGetTrustedShare;
    alias NDdeTrustedShareEnumW NDdeTrustedShareEnum;
} else {
    alias NDdeShareAddA NDdeShareAdd;
    alias NDdeShareDelA NDdeShareDel;
    alias NDdeSetShareSecurityA NDdeSetShareSecurity;
    alias NDdeGetShareSecurityA NDdeGetShareSecurity;
    alias NDdeShareEnumA NDdeShareEnum;
    alias NDdeShareGetInfoA NDdeShareGetInfo;
    alias NDdeShareSetInfoA NDdeShareSetInfo;
    alias NDdeGetErrorStringA NDdeGetErrorString;
    alias NDdeIsValidShareNameA NDdeIsValidShareName;
    alias NDdeIsValidAppTopicListA NDdeIsValidAppTopicList;
    alias NDdeSetTrustedShareA NDdeSetTrustedShare;
    alias NDdeGetTrustedShareA NDdeGetTrustedShare;
    alias NDdeTrustedShareEnumA NDdeTrustedShareEnum;
}

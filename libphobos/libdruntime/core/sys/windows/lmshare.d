/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_lmshare.d)
 */
module core.sys.windows.lmshare;
version (Windows):
pragma(lib, "netapi32");

import core.sys.windows.lmcons;
import core.sys.windows.w32api, core.sys.windows.windef;


enum SHARE_NETNAME_PARMNUM      = 1;
enum SHARE_TYPE_PARMNUM         = 3;
enum SHARE_REMARK_PARMNUM       = 4;
enum SHARE_PERMISSIONS_PARMNUM  = 5;
enum SHARE_MAX_USES_PARMNUM     = 6;
enum SHARE_CURRENT_USES_PARMNUM = 7;
enum SHARE_PATH_PARMNUM         = 8;
enum SHARE_PASSWD_PARMNUM       = 9;
enum SHARE_FILE_SD_PARMNUM      = 501;
enum SHARE_REMARK_INFOLEVEL   = PARMNUM_BASE_INFOLEVEL + SHARE_REMARK_PARMNUM;
enum SHARE_MAX_USES_INFOLEVEL = PARMNUM_BASE_INFOLEVEL + SHARE_MAX_USES_PARMNUM;
enum SHARE_FILE_SD_INFOLEVEL  = PARMNUM_BASE_INFOLEVEL + SHARE_FILE_SD_PARMNUM;

enum SHI1_NUM_ELEMENTS = 4;
enum SHI2_NUM_ELEMENTS = 10;

enum STYPE_DISKTREE = 0;
enum STYPE_PRINTQ   = 1;
enum STYPE_DEVICE   = 2;
enum STYPE_IPC      = 3;
enum STYPE_DFS      = 100;
enum STYPE_SPECIAL  = 0x80000000;

enum DWORD SHI_USES_UNLIMITED = -1;

enum SESS_GUEST = 1;
enum SESS_NOENCRYPTION = 2;
enum SESI1_NUM_ELEMENTS = 8;
enum SESI2_NUM_ELEMENTS = 9;

enum PERM_FILE_READ   = 1;
enum PERM_FILE_WRITE  = 2;
enum PERM_FILE_CREATE = 4;

struct FILE_INFO_2 {
    DWORD fi2_id;
}
alias FILE_INFO_2* PFILE_INFO_2, LPFILE_INFO_2;

struct FILE_INFO_3 {
    DWORD fi3_id;
    DWORD fi3_permissions;
    DWORD fi3_num_locks;
    LPTSTR fi3_pathname;
    LPTSTR fi3_username;
}
alias FILE_INFO_3* PFILE_INFO_3, LPFILE_INFO_3;

struct SHARE_INFO_0 {
    LPTSTR shi0_netname;
}
alias SHARE_INFO_0* PSHARE_INFO_0, LPSHARE_INFO_0;

struct SHARE_INFO_1 {
    LPTSTR shi1_netname;
    DWORD shi1_type;
    LPTSTR shi1_remark;
}
alias SHARE_INFO_1* PSHARE_INFO_1, LPSHARE_INFO_1;

struct SHARE_INFO_2 {
    LPTSTR shi2_netname;
    DWORD shi2_type;
    LPTSTR shi2_remark;
    DWORD shi2_permissions;
    DWORD shi2_max_uses;
    DWORD shi2_current_uses;
    LPTSTR shi2_path;
    LPTSTR shi2_passwd;
}
alias SHARE_INFO_2* PSHARE_INFO_2, LPSHARE_INFO_2;

struct SHARE_INFO_502 {
    LPTSTR shi502_netname;
    DWORD shi502_type;
    LPTSTR shi502_remark;
    DWORD shi502_permissions;
    DWORD shi502_max_uses;
    DWORD shi502_current_uses;
    LPTSTR shi502_path;
    LPTSTR shi502_passwd;
    DWORD shi502_reserved;
    PSECURITY_DESCRIPTOR shi502_security_descriptor;
}
alias SHARE_INFO_502* PSHARE_INFO_502, LPSHARE_INFO_502;

struct SHARE_INFO_1004 {
    LPTSTR shi1004_remark;
}
alias SHARE_INFO_1004* PSHARE_INFO_1004, LPSHARE_INFO_1004;

struct SHARE_INFO_1006 {
    DWORD shi1006_max_uses;
}
alias SHARE_INFO_1006* PSHARE_INFO_1006, LPSHARE_INFO_1006;

struct SHARE_INFO_1501 {
    DWORD shi1501_reserved;
    PSECURITY_DESCRIPTOR shi1501_security_descriptor;
}
alias SHARE_INFO_1501* PSHARE_INFO_1501, LPSHARE_INFO_1501;

struct SESSION_INFO_0 {
    LPWSTR sesi0_cname;
}
alias SESSION_INFO_0* PSESSION_INFO_0, LPSESSION_INFO_0;

struct SESSION_INFO_1 {
    LPTSTR sesi1_cname;
    LPTSTR sesi1_username;
    DWORD sesi1_num_opens;
    DWORD sesi1_time;
    DWORD sesi1_idle_time;
    DWORD sesi1_user_flags;
}
alias SESSION_INFO_1* PSESSION_INFO_1, LPSESSION_INFO_1;

struct SESSION_INFO_2 {
    LPTSTR sesi2_cname;
    LPTSTR sesi2_username;
    DWORD sesi2_num_opens;
    DWORD sesi2_time;
    DWORD sesi2_idle_time;
    DWORD sesi2_user_flags;
    LPWSTR sesi2_cltype_name;
}
alias SESSION_INFO_2* PSESSION_INFO_2, LPSESSION_INFO_2;

struct SESSION_INFO_10 {
    LPWSTR sesi10_cname;
    LPWSTR sesi10_username;
    DWORD sesi10_time;
    DWORD sesi10_idle_time;
}
alias SESSION_INFO_10* PSESSION_INFO_10, LPSESSION_INFO_10;

struct SESSION_INFO_502 {
    LPWSTR sesi502_cname;
    LPWSTR sesi502_username;
    DWORD sesi502_num_opens;
    DWORD sesi502_time;
    DWORD sesi502_idle_time;
    DWORD sesi502_user_flags;
    LPWSTR sesi502_cltype_name;
    LPWSTR sesi502_transport;
}
alias SESSION_INFO_502* PSESSION_INFO_502, LPSESSION_INFO_502;

struct CONNECTION_INFO_0 {
    DWORD coni0_id;
}
alias CONNECTION_INFO_0* PCONNECTION_INFO_0, LPCONNECTION_INFO_0;

struct CONNECTION_INFO_1 {
    DWORD coni1_id;
    DWORD coni1_type;
    DWORD coni1_num_opens;
    DWORD coni1_num_users;
    DWORD coni1_time;
    LPWSTR coni1_username;
    LPWSTR coni1_netname;
}
alias CONNECTION_INFO_1* PCONNECTION_INFO_1, LPCONNECTION_INFO_1;

extern (Windows) {
NET_API_STATUS NetShareAdd(LPWSTR,DWORD,PBYTE,PDWORD);
NET_API_STATUS NetShareEnum(LPWSTR,DWORD,PBYTE*,DWORD,PDWORD,PDWORD,PDWORD);
NET_API_STATUS NetShareEnumSticky(LPWSTR,DWORD,PBYTE*,DWORD,PDWORD,PDWORD,PDWORD resume_handle);
NET_API_STATUS NetShareGetInfo(LPWSTR,LPWSTR,DWORD,PBYTE*);
NET_API_STATUS NetShareSetInfo(LPWSTR,LPWSTR,DWORD,PBYTE,PDWORD);
NET_API_STATUS NetShareDel(LPWSTR,LPWSTR,DWORD);
NET_API_STATUS NetShareDelSticky(LPWSTR,LPWSTR,DWORD);
NET_API_STATUS NetShareCheck(LPWSTR,LPWSTR,PDWORD);
NET_API_STATUS NetSessionEnum(LPWSTR,LPWSTR,LPWSTR,DWORD,PBYTE*,DWORD,PDWORD,PDWORD,PDWORD);
NET_API_STATUS NetSessionDel(LPWSTR,LPWSTR,LPWSTR);
NET_API_STATUS NetSessionGetInfo(LPWSTR,LPWSTR,LPWSTR,DWORD,PBYTE*);
NET_API_STATUS NetConnectionEnum(LPWSTR,LPWSTR,DWORD,PBYTE*,DWORD,PDWORD,PDWORD,PDWORD);
NET_API_STATUS NetFileClose(LPWSTR,DWORD);
NET_API_STATUS NetFileEnum(LPWSTR,LPWSTR,LPWSTR,DWORD,PBYTE*,DWORD,PDWORD,PDWORD,PDWORD);
NET_API_STATUS NetFileGetInfo(LPWSTR,DWORD,DWORD,PBYTE*);
}

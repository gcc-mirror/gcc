/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_lmuse.d)
 */
module core.sys.windows.lmuse;
version (Windows):
@system:
pragma(lib, "netapi32");

import core.sys.windows.lmuseflg;
import core.sys.windows.lmcons, core.sys.windows.windef;

enum {
    USE_LOCAL_PARMNUM = 1,
    USE_REMOTE_PARMNUM,
    USE_PASSWORD_PARMNUM,
    USE_ASGTYPE_PARMNUM,
    USE_USERNAME_PARMNUM,
    USE_DOMAINNAME_PARMNUM
}

enum {
    USE_OK,
    USE_PAUSED,
    USE_SESSLOST,
    USE_DISCONN = USE_SESSLOST,
    USE_NETERR,
    USE_CONN,
    USE_RECONN
}

enum DWORD USE_WILDCARD = -1;

enum {
    USE_DISKDEV,
    USE_SPOOLDEV,
    USE_CHARDEV,
    USE_IPC
}

struct USE_INFO_0 {
    LPWSTR ui0_local;
    LPWSTR ui0_remote;
}
alias USE_INFO_0* PUSE_INFO_0, LPUSE_INFO_0;

struct USE_INFO_1 {
    LPWSTR ui1_local;
    LPWSTR ui1_remote;
    LPWSTR ui1_password;
    DWORD ui1_status;
    DWORD ui1_asg_type;
    DWORD ui1_refcount;
    DWORD ui1_usecount;
}
alias USE_INFO_1* PUSE_INFO_1, LPUSE_INFO_1;

struct USE_INFO_2 {
    LPWSTR ui2_local;
    LPWSTR ui2_remote;
    LPWSTR ui2_password;
    DWORD ui2_status;
    DWORD ui2_asg_type;
    DWORD ui2_refcount;
    DWORD ui2_usecount;
    LPWSTR ui2_username;
    LPWSTR ui2_domainname;
}
alias USE_INFO_2* PUSE_INFO_2, LPUSE_INFO_2;

extern (Windows) {
    NET_API_STATUS NetUseAdd(LPWSTR, DWORD, PBYTE, PDWORD);
    NET_API_STATUS NetUseDel(LPWSTR, LPWSTR, DWORD);
    NET_API_STATUS NetUseEnum(LPWSTR, DWORD, PBYTE*, DWORD, PDWORD, PDWORD,
      PDWORD);
    NET_API_STATUS NetUseGetInfo(LPWSTR, LPWSTR, DWORD, PBYTE*);
}

/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_lmaudit.d)
 */
// COMMENT: This file may be deprecated.
module core.sys.windows.lmaudit;
version (Windows):

private import core.sys.windows.lmcons, core.sys.windows.windef;

enum LOGFLAGS_FORWARD  = 0;
enum LOGFLAGS_BACKWARD = 1;
enum LOGFLAGS_SEEK     = 2;

enum ACTION_LOCKOUT     = 0;
enum ACTION_ADMINUNLOCK = 1;

enum AE_GUEST=0;
enum AE_USER=1;
enum AE_ADMIN=2;
enum AE_NORMAL=0;
enum AE_USERLIMIT=0;
enum AE_GENERAL=0;
enum AE_ERROR=1;
enum AE_SESSDIS=1;
enum AE_BADPW=1;
enum AE_AUTODIS=2;
enum AE_UNSHARE=2;
enum AE_ADMINPRIVREQD=2;
enum AE_ADMINDIS=3;
enum AE_NOACCESSPERM=3;
enum AE_ACCRESTRICT=4;
enum AE_NORMAL_CLOSE=0;
enum AE_SES_CLOSE=1;
enum AE_ADMIN_CLOSE=2;
enum AE_LIM_UNKNOWN=0;
enum AE_LIM_LOGONHOURS=1;
enum AE_LIM_EXPIRED=2;
enum AE_LIM_INVAL_WKSTA=3;
enum AE_LIM_DISABLED=4;
enum AE_LIM_DELETED=5;
enum AE_MOD=0;
enum AE_DELETE=1;
enum AE_ADD=2;

enum AE_UAS_USER   = 0;
enum AE_UAS_GROUP  = 1;
enum AE_UAS_MODALS = 2;

enum SVAUD_SERVICE       = 1;
enum SVAUD_GOODSESSLOGON = 6;
enum SVAUD_BADSESSLOGON  = 24;
enum SVAUD_SESSLOGON     = SVAUD_GOODSESSLOGON|SVAUD_BADSESSLOGON;
enum SVAUD_GOODNETLOGON  = 96;
enum SVAUD_BADNETLOGON   = 384;
enum SVAUD_NETLOGON      = SVAUD_GOODNETLOGON|SVAUD_BADNETLOGON;
enum SVAUD_LOGON         = SVAUD_NETLOGON|SVAUD_SESSLOGON;
enum SVAUD_GOODUSE       = 0x600;
enum SVAUD_BADUSE        = 0x1800;
enum SVAUD_USE           = SVAUD_GOODUSE|SVAUD_BADUSE;
enum SVAUD_USERLIST      = 8192;
enum SVAUD_PERMISSIONS   = 16384;
enum SVAUD_RESOURCE      = 32768;
enum SVAUD_LOGONLIM      = 65536;

enum AA_AUDIT_ALL=1;
enum AA_A_OWNER=4;
enum AA_CLOSE=8;
enum AA_S_OPEN=16;
enum AA_S_WRITE=32;
enum AA_S_CREATE=32;
enum AA_S_DELETE=64;
enum AA_S_ACL=128;
enum AA_S_ALL=253;
enum AA_F_OPEN=256;
enum AA_F_WRITE=512;
enum AA_F_CREATE=512;
enum AA_F_DELETE=1024;
enum AA_F_ACL=2048;
enum AA_F_ALL = AA_F_OPEN|AA_F_WRITE|AA_F_DELETE|AA_F_ACL;
enum AA_A_OPEN=2048;
enum AA_A_WRITE=4096;
enum AA_A_CREATE=8192;
enum AA_A_DELETE=16384;
enum AA_A_ACL=32768;
enum AA_A_ALL = AA_F_OPEN|AA_F_WRITE|AA_F_DELETE|AA_F_ACL;

struct AUDIT_ENTRY{
    DWORD ae_len;
    DWORD ae_reserved;
    DWORD ae_time;
    DWORD ae_type;
    DWORD ae_data_offset;
    DWORD ae_data_size;
}
alias AUDIT_ENTRY* PAUDIT_ENTRY, LPAUDIT_ENTRY;

struct HLOG{
    DWORD time;
    DWORD last_flags;
    DWORD offset;
    DWORD rec_offset;
}
alias HLOG* PHLOG, LPHLOG;

struct AE_SRVSTATUS{
    DWORD ae_sv_status;
}
alias AE_SRVSTATUS* PAE_SRVSTATUS, LPAE_SRVSTATUS;

struct AE_SESSLOGON{
    DWORD ae_so_compname;
    DWORD ae_so_username;
    DWORD ae_so_privilege;
}
alias AE_SESSLOGON* PAE_SESSLOGON, LPAE_SESSLOGON;

struct AE_SESSLOGOFF{
    DWORD ae_sf_compname;
    DWORD ae_sf_username;
    DWORD ae_sf_reason;
}
alias AE_SESSLOGOFF* PAE_SESSLOGOFF, LPAE_SESSLOGOFF;

struct AE_SESSPWERR{
    DWORD ae_sp_compname;
    DWORD ae_sp_username;
}
alias AE_SESSPWERR* PAE_SESSPWERR, LPAE_SESSPWERR;

struct AE_CONNSTART{
    DWORD ae_ct_compname;
    DWORD ae_ct_username;
    DWORD ae_ct_netname;
    DWORD ae_ct_connid;
}
alias AE_CONNSTART* PAE_CONNSTART, LPAE_CONNSTART;

struct AE_CONNSTOP{
    DWORD ae_cp_compname;
    DWORD ae_cp_username;
    DWORD ae_cp_netname;
    DWORD ae_cp_connid;
    DWORD ae_cp_reason;
}
alias AE_CONNSTOP* PAE_CONNSTOP, LPAE_CONNSTOP;

struct AE_CONNREJ{
    DWORD ae_cr_compname;
    DWORD ae_cr_username;
    DWORD ae_cr_netname;
    DWORD ae_cr_reason;
}
alias AE_CONNREJ* PAE_CONNREJ, LPAE_CONNREJ;

struct AE_RESACCESS{
    DWORD ae_ra_compname;
    DWORD ae_ra_username;
    DWORD ae_ra_resname;
    DWORD ae_ra_operation;
    DWORD ae_ra_returncode;
    DWORD ae_ra_restype;
    DWORD ae_ra_fileid;
}
alias AE_RESACCESS* PAE_RESACCESS, LPAE_RESACCESS;

struct AE_RESACCESSREJ{
    DWORD ae_rr_compname;
    DWORD ae_rr_username;
    DWORD ae_rr_resname;
    DWORD ae_rr_operation;
}
alias AE_RESACCESSREJ* PAE_RESACCESSREJ, LPAE_RESACCESSREJ;

struct AE_CLOSEFILE{
    DWORD ae_cf_compname;
    DWORD ae_cf_username;
    DWORD ae_cf_resname;
    DWORD ae_cf_fileid;
    DWORD ae_cf_duration;
    DWORD ae_cf_reason;
}
alias AE_CLOSEFILE* PAE_CLOSEFILE, LPAE_CLOSEFILE;

struct AE_SERVICESTAT{
    DWORD ae_ss_compname;
    DWORD ae_ss_username;
    DWORD ae_ss_svcname;
    DWORD ae_ss_status;
    DWORD ae_ss_code;
    DWORD ae_ss_text;
    DWORD ae_ss_returnval;
}
alias AE_SERVICESTAT* PAE_SERVICESTAT, LPAE_SERVICESTAT;

struct AE_ACLMOD{
    DWORD ae_am_compname;
    DWORD ae_am_username;
    DWORD ae_am_resname;
    DWORD ae_am_action;
    DWORD ae_am_datalen;
}
alias AE_ACLMOD* PAE_ACLMOD, LPAE_ACLMOD;

struct AE_UASMOD{
    DWORD ae_um_compname;
    DWORD ae_um_username;
    DWORD ae_um_resname;
    DWORD ae_um_rectype;
    DWORD ae_um_action;
    DWORD ae_um_datalen;
}
alias AE_UASMOD* PAE_UASMOD, LPAE_UASMOD;

struct AE_NETLOGON{
    DWORD ae_no_compname;
    DWORD ae_no_username;
    DWORD ae_no_privilege;
    DWORD ae_no_authflags;
}
alias AE_NETLOGON* PAE_NETLOGON, LPAE_NETLOGON;

struct AE_NETLOGOFF{
    DWORD ae_nf_compname;
    DWORD ae_nf_username;
    DWORD ae_nf_reserved1;
    DWORD ae_nf_reserved2;
}
alias AE_NETLOGOFF* PAE_NETLOGOFF, LPAE_NETLOGOFF;

struct AE_ACCLIM{
    DWORD ae_al_compname;
    DWORD ae_al_username;
    DWORD ae_al_resname;
    DWORD ae_al_limit;
}
alias AE_ACCLIM* PAE_ACCLIM, LPAE_ACCLIM;

struct AE_LOCKOUT{
    DWORD ae_lk_compname;
    DWORD ae_lk_username;
    DWORD ae_lk_action;
    DWORD ae_lk_bad_pw_count;
}
alias AE_LOCKOUT* PAE_LOCKOUT, LPAE_LOCKOUT;

struct AE_GENERIC{
    DWORD ae_ge_msgfile;
    DWORD ae_ge_msgnum;
    DWORD ae_ge_params;
    DWORD ae_ge_param1;
    DWORD ae_ge_param2;
    DWORD ae_ge_param3;
    DWORD ae_ge_param4;
    DWORD ae_ge_param5;
    DWORD ae_ge_param6;
    DWORD ae_ge_param7;
    DWORD ae_ge_param8;
    DWORD ae_ge_param9;
}
alias AE_GENERIC* PAE_GENERIC, LPAE_GENERIC;

extern (Windows) {
deprecated {
NET_API_STATUS NetAuditClear(LPCWSTR,LPCWSTR,LPCWSTR);
NET_API_STATUS NetAuditRead(LPTSTR,LPTSTR,LPHLOG,DWORD,PDWORD,DWORD,DWORD,PBYTE*,DWORD,PDWORD,PDWORD);
NET_API_STATUS NetAuditWrite(DWORD,PBYTE,DWORD,LPTSTR,PBYTE);
}
}

/+
/* MinGW: These conflict with struct typedefs, why? */
enum AE_SRVSTATUS=0;
enum AE_SESSLOGON=1;
enum AE_SESSLOGOFF=2;
enum AE_SESSPWERR=3;
enum AE_CONNSTART=4;
enum AE_CONNSTOP=5;
enum AE_CONNREJ=6;
enum AE_RESACCESS=7;
enum AE_RESACCESSREJ=8;
enum AE_CLOSEFILE=9;
enum AE_SERVICESTAT=11;
enum AE_ACLMOD=12;
enum AE_UASMOD=13;
enum AE_NETLOGON=14;
enum AE_NETLOGOFF=15;
enum AE_NETLOGDENIED=16;
enum AE_ACCLIMITEXCD=17;
enum AE_RESACCESS2=18;
enum AE_ACLMODFAIL=19;
enum AE_LOCKOUT=20;
enum AE_GENERIC_TYPE=21;
enum AE_SRVSTART=0;
enum AE_SRVPAUSED=1;
enum AE_SRVCONT=2;
enum AE_SRVSTOP=3;
+/

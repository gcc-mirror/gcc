/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_lmwksta.d)
 */
module core.sys.windows.lmwksta;
version (Windows):
pragma(lib, "netapi32");

import core.sys.windows.lmuseflg;
private import core.sys.windows.lmcons, core.sys.windows.windef;

pragma(lib, "Netapi32");

enum {
    WKSTA_COMPUTERNAME_PARMNUM     = 1,
    WKSTA_LANGROUP_PARMNUM,     // = 2
    WKSTA_VER_MAJOR_PARMNUM        = 4,
    WKSTA_VER_MINOR_PARMNUM,
    WKSTA_LOGGED_ON_USERS_PARMNUM,
    WKSTA_LANROOT_PARMNUM,
    WKSTA_LOGON_DOMAIN_PARMNUM,
    WKSTA_LOGON_SERVER_PARMNUM,
    WKSTA_CHARWAIT_PARMNUM,
    WKSTA_CHARTIME_PARMNUM,
    WKSTA_CHARCOUNT_PARMNUM,
    WKSTA_KEEPCONN_PARMNUM,
    WKSTA_KEEPSEARCH_PARMNUM,
    WKSTA_MAXCMDS_PARMNUM,
    WKSTA_NUMWORKBUF_PARMNUM,
    WKSTA_MAXWRKCACHE_PARMNUM,
    WKSTA_SESSTIMEOUT_PARMNUM,
    WKSTA_SIZERROR_PARMNUM,
    WKSTA_NUMALERTS_PARMNUM,
    WKSTA_NUMSERVICES_PARMNUM,
    WKSTA_NUMCHARBUF_PARMNUM,
    WKSTA_SIZCHARBUF_PARMNUM,     // = 23
    WKSTA_ERRLOGSZ_PARMNUM           = 27,
    WKSTA_PRINTBUFTIME_PARMNUM,
    WKSTA_SIZWORKBUF_PARMNUM,
    WKSTA_MAILSLOTS_PARMNUM,
    WKSTA_NUMDGRAMBUF_PARMNUM,
    WKSTA_WRKHEURISTICS_PARMNUM,
    WKSTA_MAXTHREADS_PARMNUM,     // = 33
    WKSTA_LOCKQUOTA_PARMNUM          = 41,
    WKSTA_LOCKINCREMENT_PARMNUM,
    WKSTA_LOCKMAXIMUM_PARMNUM,
    WKSTA_PIPEINCREMENT_PARMNUM,
    WKSTA_PIPEMAXIMUM_PARMNUM,
    WKSTA_DORMANTFILELIMIT_PARMNUM,
    WKSTA_CACHEFILETIMEOUT_PARMNUM,
    WKSTA_USEOPPORTUNISTICLOCKING_PARMNUM,
    WKSTA_USEUNLOCKBEHIND_PARMNUM,
    WKSTA_USECLOSEBEHIND_PARMNUM,
    WKSTA_BUFFERNAMEDPIPES_PARMNUM,
    WKSTA_USELOCKANDREADANDUNLOCK_PARMNUM,
    WKSTA_UTILIZENTCACHING_PARMNUM,
    WKSTA_USERAWREAD_PARMNUM,
    WKSTA_USERAWWRITE_PARMNUM,
    WKSTA_USEWRITERAWWITHDATA_PARMNUM,
    WKSTA_USEENCRYPTION_PARMNUM,
    WKSTA_BUFFILESWITHDENYWRITE_PARMNUM,
    WKSTA_BUFFERREADONLYFILES_PARMNUM,
    WKSTA_FORCECORECREATEMODE_PARMNUM,
    WKSTA_USE512BYTESMAXTRANSFER_PARMNUM,
    WKSTA_READAHEADTHRUPUT_PARMNUM,    // = 62
    WKSTA_PLATFORM_ID_PARMNUM             = 100,
    WKSTA_OTH_DOMAINS_PARMNUM             = 101,
    TRANSPORT_QUALITYOFSERVICE_PARMNUM    = 201,
    TRANSPORT_NAME_PARMNUM                = 202
}

struct WKSTA_INFO_100{
    DWORD wki100_platform_id;
    LPWSTR wki100_computername;
    LPWSTR wki100_langroup;
    DWORD wki100_ver_major;
    DWORD wki100_ver_minor;
}
alias WKSTA_INFO_100* PWKSTA_INFO_100, LPWKSTA_INFO_100;

struct WKSTA_INFO_101{
    DWORD wki101_platform_id;
    LPWSTR wki101_computername;
    LPWSTR wki101_langroup;
    DWORD wki101_ver_major;
    DWORD wki101_ver_minor;
    LPWSTR wki101_lanroot;
}
alias WKSTA_INFO_101* PWKSTA_INFO_101, LPWKSTA_INFO_101;

struct WKSTA_INFO_102{
    DWORD wki102_platform_id;
    LPWSTR wki102_computername;
    LPWSTR wki102_langroup;
    DWORD wki102_ver_major;
    DWORD wki102_ver_minor;
    LPWSTR wki102_lanroot;
    DWORD wki102_logged_on_users;
}
alias WKSTA_INFO_102* PWKSTA_INFO_102, LPWKSTA_INFO_102;

struct WKSTA_INFO_302{
    DWORD wki302_char_wait;
    DWORD wki302_collection_time;
    DWORD wki302_maximum_collection_count;
    DWORD wki302_keep_conn;
    DWORD wki302_keep_search;
    DWORD wki302_max_cmds;
    DWORD wki302_num_work_buf;
    DWORD wki302_siz_work_buf;
    DWORD wki302_max_wrk_cache;
    DWORD wki302_sess_timeout;
    DWORD wki302_siz_error;
    DWORD wki302_num_alerts;
    DWORD wki302_num_services;
    DWORD wki302_errlog_sz;
    DWORD wki302_print_buf_time;
    DWORD wki302_num_char_buf;
    DWORD wki302_siz_char_buf;
    LPWSTR wki302_wrk_heuristics;
    DWORD wki302_mailslots;
    DWORD wki302_num_dgram_buf;
}
alias WKSTA_INFO_302* PWKSTA_INFO_302, LPWKSTA_INFO_302;

struct WKSTA_INFO_402{
    DWORD wki402_char_wait;
    DWORD wki402_collection_time;
    DWORD wki402_maximum_collection_count;
    DWORD wki402_keep_conn;
    DWORD wki402_keep_search;
    DWORD wki402_max_cmds;
    DWORD wki402_num_work_buf;
    DWORD wki402_siz_work_buf;
    DWORD wki402_max_wrk_cache;
    DWORD wki402_sess_timeout;
    DWORD wki402_siz_error;
    DWORD wki402_num_alerts;
    DWORD wki402_num_services;
    DWORD wki402_errlog_sz;
    DWORD wki402_print_buf_time;
    DWORD wki402_num_char_buf;
    DWORD wki402_siz_char_buf;
    LPWSTR wki402_wrk_heuristics;
    DWORD wki402_mailslots;
    DWORD wki402_num_dgram_buf;
    DWORD wki402_max_threads;
}
alias WKSTA_INFO_402* PWKSTA_INFO_402, LPWKSTA_INFO_402;

struct WKSTA_INFO_502{
    DWORD wki502_char_wait;
    DWORD wki502_collection_time;
    DWORD wki502_maximum_collection_count;
    DWORD wki502_keep_conn;
    DWORD wki502_max_cmds;
    DWORD wki502_sess_timeout;
    DWORD wki502_siz_char_buf;
    DWORD wki502_max_threads;
    DWORD wki502_lock_quota;
    DWORD wki502_lock_increment;
    DWORD wki502_lock_maximum;
    DWORD wki502_pipe_increment;
    DWORD wki502_pipe_maximum;
    DWORD wki502_cache_file_timeout;
    DWORD wki502_dormant_file_limit;
    DWORD wki502_read_ahead_throughput;
    DWORD wki502_num_mailslot_buffers;
    DWORD wki502_num_srv_announce_buffers;
    DWORD wki502_max_illegal_datagram_events;
    DWORD wki502_illegal_datagram_event_reset_frequency;
    BOOL wki502_log_election_packets;
    BOOL wki502_use_opportunistic_locking;
    BOOL wki502_use_unlock_behind;
    BOOL wki502_use_close_behind;
    BOOL wki502_buf_named_pipes;
    BOOL wki502_use_lock_read_unlock;
    BOOL wki502_utilize_nt_caching;
    BOOL wki502_use_raw_read;
    BOOL wki502_use_raw_write;
    BOOL wki502_use_write_raw_data;
    BOOL wki502_use_encryption;
    BOOL wki502_buf_files_deny_write;
    BOOL wki502_buf_read_only_files;
    BOOL wki502_force_core_create_mode;
    BOOL wki502_use_512_byte_max_transfer;
}
alias WKSTA_INFO_502* PWKSTA_INFO_502, LPWKSTA_INFO_502;

struct WKSTA_INFO_1010 {
    DWORD wki1010_char_wait;
}
alias WKSTA_INFO_1010* PWKSTA_INFO_1010, LPWKSTA_INFO_1010;

struct WKSTA_INFO_1011 {
    DWORD wki1011_collection_time;
}
alias WKSTA_INFO_1011* PWKSTA_INFO_1011, LPWKSTA_INFO_1011;

struct WKSTA_INFO_1012 {
    DWORD wki1012_maximum_collection_count;
}
alias WKSTA_INFO_1012* PWKSTA_INFO_1012, LPWKSTA_INFO_1012;

struct WKSTA_INFO_1027 {
    DWORD wki1027_errlog_sz;
}
alias WKSTA_INFO_1027* PWKSTA_INFO_1027, LPWKSTA_INFO_1027;

struct WKSTA_INFO_1028 {
    DWORD wki1028_print_buf_time;
}
alias WKSTA_INFO_1028* PWKSTA_INFO_1028, LPWKSTA_INFO_1028;

struct WKSTA_INFO_1032 {
    DWORD wki1032_wrk_heuristics;
}
alias WKSTA_INFO_1032* PWKSTA_INFO_1032, LPWKSTA_INFO_1032;

struct WKSTA_INFO_1013 {
    DWORD wki1013_keep_conn;
}
alias WKSTA_INFO_1013* PWKSTA_INFO_1013, LPWKSTA_INFO_1013;

struct WKSTA_INFO_1018 {
    DWORD wki1018_sess_timeout;
}
alias WKSTA_INFO_1018* PWKSTA_INFO_1018, LPWKSTA_INFO_1018;

struct WKSTA_INFO_1023 {
    DWORD wki1023_siz_char_buf;
}
alias WKSTA_INFO_1023* PWKSTA_INFO_1023, LPWKSTA_INFO_1023;

struct WKSTA_INFO_1033 {
    DWORD wki1033_max_threads;
}
alias WKSTA_INFO_1033* PWKSTA_INFO_1033, LPWKSTA_INFO_1033;

struct WKSTA_INFO_1041 {
    DWORD wki1041_lock_quota;
}
alias WKSTA_INFO_1041* PWKSTA_INFO_1041, LPWKSTA_INFO_1041;

struct WKSTA_INFO_1042 {
    DWORD wki1042_lock_increment;
}
alias WKSTA_INFO_1042* PWKSTA_INFO_1042, LPWKSTA_INFO_1042;

struct WKSTA_INFO_1043 {
    DWORD wki1043_lock_maximum;
}
alias WKSTA_INFO_1043* PWKSTA_INFO_1043, LPWKSTA_INFO_1043;

struct WKSTA_INFO_1044 {
    DWORD wki1044_pipe_increment;
}
alias WKSTA_INFO_1044* PWKSTA_INFO_1044, LPWKSTA_INFO_1044;

struct WKSTA_INFO_1045 {
    DWORD wki1045_pipe_maximum;
}
alias WKSTA_INFO_1045* PWKSTA_INFO_1045, LPWKSTA_INFO_1045;

struct WKSTA_INFO_1046 {
    DWORD wki1046_dormant_file_limit;
}
alias WKSTA_INFO_1046* PWKSTA_INFO_1046, LPWKSTA_INFO_1046;

struct WKSTA_INFO_1047 {
    DWORD wki1047_cache_file_timeout;
}
alias WKSTA_INFO_1047* PWKSTA_INFO_1047, LPWKSTA_INFO_1047;

struct WKSTA_INFO_1048 {
    BOOL wki1048_use_opportunistic_locking;
}
alias WKSTA_INFO_1048* PWKSTA_INFO_1048, LPWKSTA_INFO_1048;

struct WKSTA_INFO_1049 {
    BOOL wki1049_use_unlock_behind;
}
alias WKSTA_INFO_1049* PWKSTA_INFO_1049, LPWKSTA_INFO_1049;

struct WKSTA_INFO_1050 {
    BOOL wki1050_use_close_behind;
}
alias WKSTA_INFO_1050* PWKSTA_INFO_1050, LPWKSTA_INFO_1050;

struct WKSTA_INFO_1051 {
    BOOL wki1051_buf_named_pipes;
}
alias WKSTA_INFO_1051* PWKSTA_INFO_1051, LPWKSTA_INFO_1051;

struct WKSTA_INFO_1052 {
    BOOL wki1052_use_lock_read_unlock;
}
alias WKSTA_INFO_1052* PWKSTA_INFO_1052, LPWKSTA_INFO_1052;

struct WKSTA_INFO_1053 {
    BOOL wki1053_utilize_nt_caching;
}
alias WKSTA_INFO_1053* PWKSTA_INFO_1053, LPWKSTA_INFO_1053;

struct WKSTA_INFO_1054 {
    BOOL wki1054_use_raw_read;
}
alias WKSTA_INFO_1054* PWKSTA_INFO_1054, LPWKSTA_INFO_1054;

struct WKSTA_INFO_1055 {
    BOOL wki1055_use_raw_write;
}
alias WKSTA_INFO_1055* PWKSTA_INFO_1055, LPWKSTA_INFO_1055;

struct WKSTA_INFO_1056 {
    BOOL wki1056_use_write_raw_data;
}
alias WKSTA_INFO_1056* PWKSTA_INFO_1056, LPWKSTA_INFO_1056;

struct WKSTA_INFO_1057 {
    BOOL wki1057_use_encryption;
}
alias WKSTA_INFO_1057* PWKSTA_INFO_1057, LPWKSTA_INFO_1057;

struct WKSTA_INFO_1058 {
    BOOL wki1058_buf_files_deny_write;
}
alias WKSTA_INFO_1058* PWKSTA_INFO_1058, LPWKSTA_INFO_1058;

struct WKSTA_INFO_1059 {
    BOOL wki1059_buf_read_only_files;
}
alias WKSTA_INFO_1059* PWKSTA_INFO_1059, LPWKSTA_INFO_1059;

struct WKSTA_INFO_1060 {
    BOOL wki1060_force_core_create_mode;
}
alias WKSTA_INFO_1060* PWKSTA_INFO_1060, LPWKSTA_INFO_1060;

struct WKSTA_INFO_1061 {
    BOOL wki1061_use_512_byte_max_transfer;
}
alias WKSTA_INFO_1061* PWKSTA_INFO_1061, LPWKSTA_INFO_1061;

struct WKSTA_INFO_1062 {
    DWORD wki1062_read_ahead_throughput;
}
alias WKSTA_INFO_1062* PWKSTA_INFO_1062, LPWKSTA_INFO_1062;

struct WKSTA_USER_INFO_0 {
    LPWSTR wkui0_username;
}
alias WKSTA_USER_INFO_0* PWKSTA_USER_INFO_0, LPWKSTA_USER_INFO_0;

struct WKSTA_USER_INFO_1{
    LPWSTR wkui1_username;
    LPWSTR wkui1_logon_domain;
    LPWSTR wkui1_oth_domains;
    LPWSTR wkui1_logon_server;
}
alias WKSTA_USER_INFO_1* PWKSTA_USER_INFO_1, LPWKSTA_USER_INFO_1;

struct WKSTA_USER_INFO_1101 {
    LPWSTR wkui1101_oth_domains;
}
alias WKSTA_USER_INFO_1101* PWKSTA_USER_INFO_1101, LPWKSTA_USER_INFO_1101;

struct WKSTA_TRANSPORT_INFO_0{
    DWORD wkti0_quality_of_service;
    DWORD wkti0_number_of_vcs;
    LPWSTR wkti0_transport_name;
    LPWSTR wkti0_transport_address;
    BOOL wkti0_wan_ish;
}
alias WKSTA_TRANSPORT_INFO_0* PWKSTA_TRANSPORT_INFO_0, LPWKSTA_TRANSPORT_INFO_0;

extern (Windows) {
NET_API_STATUS NetWkstaGetInfo(LPWSTR,DWORD,PBYTE*);
NET_API_STATUS NetWkstaSetInfo(LPWSTR,DWORD,PBYTE,PDWORD);
NET_API_STATUS NetWkstaUserGetInfo(LPWSTR,DWORD,PBYTE*);
NET_API_STATUS NetWkstaUserSetInfo(LPWSTR,DWORD,PBYTE,PDWORD);
NET_API_STATUS NetWkstaUserEnum(LPWSTR,DWORD,PBYTE*,DWORD,PDWORD,PDWORD,PDWORD);
NET_API_STATUS NetWkstaTransportAdd(LPWSTR,DWORD,PBYTE,PDWORD);
NET_API_STATUS NetWkstaTransportDel(LPWSTR,LPWSTR,DWORD);
NET_API_STATUS NetWkstaTransportEnum(LPWSTR,DWORD,PBYTE*,DWORD,PDWORD,PDWORD,PDWORD);
}

/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_lmalert.d)
 */
module core.sys.windows.lmalert;
version (Windows):
pragma(lib, "netapi32");

import core.sys.windows.lmcons, core.sys.windows.windef;

const TCHAR[]
    ALERTER_MAILSLOT     = `\\.\MAILSLOT\Alerter`,
    ALERT_PRINT_EVENT    = "PRINTING",
    ALERT_MESSAGE_EVENT  = "MESSAGE",
    ALERT_ERRORLOG_EVENT = "ERRORLOG",
    ALERT_ADMIN_EVENT    = "ADMIN",
    ALERT_USER_EVENT     = "USER";
//MACRO #define ALERT_OTHER_INFO(x) ((PBYTE)(x)+sizeof(STD_ALERT))

//MACRO #define ALERT_VAR_DATA(p) ((PBYTE)(p)+sizeof(*p))

enum PRJOB_QSTATUS     = 3;
enum PRJOB_DEVSTATUS   = 508;
enum PRJOB_COMPLETE    = 4;
enum PRJOB_INTERV      = 8;
enum PRJOB_            = 16;
enum PRJOB_DESTOFFLINE = 32;
enum PRJOB_DESTPAUSED  = 64;
enum PRJOB_NOTIFY      = 128;
enum PRJOB_DESTNOPAPER = 256;
enum PRJOB_DELETED     = 32768;
enum PRJOB_QS_QUEUED   = 0;
enum PRJOB_QS_PAUSED   = 1;
enum PRJOB_QS_SPOOLING = 2;
enum PRJOB_QS_PRINTING = 3;

struct ADMIN_OTHER_INFO{
    DWORD alrtad_errcode;
    DWORD alrtad_numstrings;
}
alias ADMIN_OTHER_INFO* PADMIN_OTHER_INFO, LPADMIN_OTHER_INFO;

struct STD_ALERT{
    DWORD alrt_timestamp;
    TCHAR[EVLEN+1] alrt_eventname = 0;
    TCHAR[SNLEN+1] alrt_servicename = 0;
}
alias STD_ALERT* PSTD_ALERT, LPSTD_ALERT;

struct ERRLOG_OTHER_INFO{
    DWORD alrter_errcode;
    DWORD alrter_offset;
}
alias ERRLOG_OTHER_INFO* PERRLOG_OTHER_INFO, LPERRLOG_OTHER_INFO;

struct PRINT_OTHER_INFO{
    DWORD alrtpr_jobid;
    DWORD alrtpr_status;
    DWORD alrtpr_submitted;
    DWORD alrtpr_size;
}
alias PRINT_OTHER_INFO* PPRINT_OTHER_INFO, LPPRINT_OTHER_INFO;

struct USER_OTHER_INFO{
    DWORD alrtus_errcode;
    DWORD alrtus_numstrings;
}
alias USER_OTHER_INFO* PUSER_OTHER_INFO, LPUSER_OTHER_INFO;

extern (Windows) nothrow @nogc {
NET_API_STATUS NetAlertRaise(LPCWSTR,PVOID,DWORD);
NET_API_STATUS NetAlertRaiseEx(LPCWSTR,PVOID,DWORD,LPCWSTR);
}

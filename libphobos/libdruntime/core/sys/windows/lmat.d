/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_lmat.d)
 */
module core.sys.windows.lmat;
version (Windows):
pragma(lib, "netapi32");

private import core.sys.windows.lmcons, core.sys.windows.windef;

enum JOB_RUN_PERIODICALLY = 1;
enum JOB_EXEC_ERROR       = 2;
enum JOB_RUNS_TODAY       = 4;
enum JOB_ADD_CURRENT_DATE = 8;
enum JOB_NONINTERACTIVE   = 16;
enum JOB_INPUT_FLAGS      = JOB_RUN_PERIODICALLY | JOB_ADD_CURRENT_DATE
                             | JOB_NONINTERACTIVE;
enum JOB_OUTPUT_FLAGS     = JOB_RUN_PERIODICALLY | JOB_EXEC_ERROR
                             | JOB_RUNS_TODAY | JOB_NONINTERACTIVE;

struct AT_ENUM {
    DWORD JobId;
    DWORD_PTR JobTime;
    DWORD DaysOfMonth;
    UCHAR DaysOfWeek;
    UCHAR Flags;
    LPWSTR Command;
}
alias AT_ENUM* PAT_ENUM, LPAT_ENUM;

struct AT_INFO {
    DWORD_PTR JobTime;
    DWORD DaysOfMonth;
    UCHAR DaysOfWeek;
    UCHAR Flags;
    LPWSTR Command;
}
alias AT_INFO* PAT_INFO, LPAT_INFO;

extern (Windows) {
    NET_API_STATUS NetScheduleJobAdd(LPWSTR, PBYTE, LPDWORD);
    NET_API_STATUS NetScheduleJobDel(LPWSTR, DWORD, DWORD);
    NET_API_STATUS NetScheduleJobEnum(LPWSTR, PBYTE*, DWORD, PDWORD, PDWORD,
      PDWORD);
    NET_API_STATUS NetScheduleJobGetInfo(LPWSTR, DWORD, PBYTE*);
}

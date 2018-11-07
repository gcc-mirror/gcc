/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_lmremutl.d)
 */
module core.sys.windows.lmremutl;
version (Windows):
pragma(lib, "netapi32");

// D Conversion Note: DESC_CHAR is defined as TCHAR.

private import core.sys.windows.lmcons, core.sys.windows.windef;

enum SUPPORTS_REMOTE_ADMIN_PROTOCOL =  2;
enum SUPPORTS_RPC                   =  4;
enum SUPPORTS_SAM_PROTOCOL          =  8;
enum SUPPORTS_UNICODE               = 16;
enum SUPPORTS_LOCAL                 = 32;
enum SUPPORTS_ANY                   = 0xFFFFFFFF;

enum NO_PERMISSION_REQUIRED = 1;
enum ALLOCATE_RESPONSE      = 2;
enum USE_SPECIFIC_TRANSPORT = 0x80000000;

//[Yes] #ifndef DESC_CHAR_UNICODE
//alias CHAR DESC_CHAR;
//} else {
//[No] #else
//[No] typedef WCHAR DESC_CHAR;
//[No] #endif
// FIXME (D): Is this OK?
alias TCHAR DESC_CHAR;

alias DESC_CHAR* LPDESC;

struct TIME_OF_DAY_INFO {
    DWORD tod_elapsedt;
    DWORD tod_msecs;
    DWORD tod_hours;
    DWORD tod_mins;
    DWORD tod_secs;
    DWORD tod_hunds;
    LONG  tod_timezone;
    DWORD tod_tinterval;
    DWORD tod_day;
    DWORD tod_month;
    DWORD tod_year;
    DWORD tod_weekday;
}
alias TIME_OF_DAY_INFO* PTIME_OF_DAY_INFO, LPTIME_OF_DAY_INFO;

extern (Windows) {
    NET_API_STATUS NetRemoteTOD(LPCWSTR, PBYTE*);
    NET_API_STATUS NetRemoteComputerSupports(LPCWSTR, DWORD, PDWORD);
    NET_API_STATUS RxRemoteApi(DWORD, LPCWSTR, LPDESC, LPDESC, LPDESC,
      LPDESC, LPDESC, LPDESC, LPDESC, DWORD, ...);
}

/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_lmbrowsr.d)
 */
module core.sys.windows.lmbrowsr;
version (Windows):

import core.sys.windows.lmcons, core.sys.windows.windef;

enum BROWSER_ROLE_PDC = 1;
enum BROWSER_ROLE_BDC = 2;

struct BROWSER_STATISTICS {
    LARGE_INTEGER StatisticsStartTime;
    LARGE_INTEGER NumberOfServerAnnouncements;
    LARGE_INTEGER NumberOfDomainAnnouncements;
    ULONG NumberOfElectionPackets;
    ULONG NumberOfMailslotWrites;
    ULONG NumberOfGetBrowserServerListRequests;
    ULONG NumberOfServerEnumerations;
    ULONG NumberOfDomainEnumerations;
    ULONG NumberOfOtherEnumerations;
    ULONG NumberOfMissedServerAnnouncements;
    ULONG NumberOfMissedMailslotDatagrams;
    ULONG NumberOfMissedGetBrowserServerListRequests;
    ULONG NumberOfFailedServerAnnounceAllocations;
    ULONG NumberOfFailedMailslotAllocations;
    ULONG NumberOfFailedMailslotReceives;
    ULONG NumberOfFailedMailslotWrites;
    ULONG NumberOfFailedMailslotOpens;
    ULONG NumberOfDuplicateMasterAnnouncements;
    LARGE_INTEGER NumberOfIllegalDatagrams;
}
alias BROWSER_STATISTICS* PBROWSER_STATISTICS, LPBROWSER_STATISTICS;

struct BROWSER_STATISTICS_100 {
    LARGE_INTEGER StartTime;
    LARGE_INTEGER NumberOfServerAnnouncements;
    LARGE_INTEGER NumberOfDomainAnnouncements;
    ULONG NumberOfElectionPackets;
    ULONG NumberOfMailslotWrites;
    ULONG NumberOfGetBrowserServerListRequests;
    LARGE_INTEGER NumberOfIllegalDatagrams;
}
alias BROWSER_STATISTICS_100* PBROWSER_STATISTICS_100;

struct BROWSER_STATISTICS_101 {
    LARGE_INTEGER StartTime;
    LARGE_INTEGER NumberOfServerAnnouncements;
    LARGE_INTEGER NumberOfDomainAnnouncements;
    ULONG NumberOfElectionPackets;
    ULONG NumberOfMailslotWrites;
    ULONG NumberOfGetBrowserServerListRequests;
    LARGE_INTEGER NumberOfIllegalDatagrams;
    ULONG NumberOfMissedServerAnnouncements;
    ULONG NumberOfMissedMailslotDatagrams;
    ULONG NumberOfMissedGetBrowserServerListRequests;
    ULONG NumberOfFailedServerAnnounceAllocations;
    ULONG NumberOfFailedMailslotAllocations;
    ULONG NumberOfFailedMailslotReceives;
    ULONG NumberOfFailedMailslotWrites;
    ULONG NumberOfFailedMailslotOpens;
    ULONG NumberOfDuplicateMasterAnnouncements;
}
alias BROWSER_STATISTICS_101* PBROWSER_STATISTICS_101;

extern (Windows) {
    NET_API_STATUS I_BrowserServerEnum(LPCWSTR, LPCWSTR, LPCWSTR, DWORD,
      PBYTE*, DWORD, PDWORD, PDWORD, DWORD, LPCWSTR, PDWORD);
    NET_API_STATUS I_BrowserServerEnumEx(LPCWSTR, LPCWSTR, LPCWSTR, DWORD,
      PBYTE*, DWORD, PDWORD, PDWORD, DWORD, LPCWSTR, LPCWSTR);
    NET_API_STATUS I_BrowserQueryEmulatedDomains(LPWSTR, PBYTE*, PDWORD);
    NET_API_STATUS I_BrowserQueryOtherDomains(LPCWSTR, PBYTE*, PDWORD, PDWORD);
    NET_API_STATUS I_BrowserResetNetlogonState(LPCWSTR);
    NET_API_STATUS I_BrowserSetNetlogonState(LPWSTR, LPWSTR, LPWSTR, DWORD);
    NET_API_STATUS I_BrowserQueryStatistics(LPCWSTR, LPBROWSER_STATISTICS*);
    NET_API_STATUS I_BrowserResetStatistics(LPCWSTR);
    WORD I_BrowserServerEnumForXactsrv(LPCWSTR, LPCWSTR, ULONG, USHORT, PVOID,
      WORD, DWORD, PDWORD, PDWORD, DWORD, LPCWSTR, LPCWSTR, PWORD);
    NET_API_STATUS I_BrowserDebugTrace(PWCHAR, PCHAR);
}

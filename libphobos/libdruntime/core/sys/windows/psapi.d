/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_psapi.d)
 */
/* Comment from MinGW
 *   Process status API (PSAPI)
 *   http://windowssdk.msdn.microsoft.com/library/ms684884.aspx
 */

module core.sys.windows.psapi;
version (Windows):

version (ANSI) {} else version = Unicode;

import core.sys.windows.w32api;
import core.sys.windows.winbase;
import core.sys.windows.windef;

struct MODULEINFO {
    LPVOID lpBaseOfDll;
    DWORD SizeOfImage;
    LPVOID EntryPoint;
}
alias MODULEINFO* LPMODULEINFO;

struct PSAPI_WS_WATCH_INFORMATION {
    LPVOID FaultingPc;
    LPVOID FaultingVa;
}
alias PSAPI_WS_WATCH_INFORMATION* PPSAPI_WS_WATCH_INFORMATION;

struct PSAPI_WS_WATCH_INFORMATION_EX {
    PSAPI_WS_WATCH_INFORMATION BasicInfo;
    ULONG_PTR FaultingThreadId;
    ULONG_PTR Flags;
}
alias PSAPI_WS_WATCH_INFORMATION_EX* PPSAPI_WS_WATCH_INFORMATION_EX;

struct PROCESS_MEMORY_COUNTERS {
    DWORD cb;
    DWORD PageFaultCount;
    SIZE_T PeakWorkingSetSize;
    SIZE_T WorkingSetSize;
    SIZE_T QuotaPeakPagedPoolUsage;
    SIZE_T QuotaPagedPoolUsage;
    SIZE_T QuotaPeakNonPagedPoolUsage;
    SIZE_T QuotaNonPagedPoolUsage;
    SIZE_T PagefileUsage;
    SIZE_T PeakPagefileUsage;
}
alias PROCESS_MEMORY_COUNTERS* PPROCESS_MEMORY_COUNTERS;

struct PERFORMANCE_INFORMATION {
    DWORD cb;
    SIZE_T CommitTotal;
    SIZE_T CommitLimit;
    SIZE_T CommitPeak;
    SIZE_T PhysicalTotal;
    SIZE_T PhysicalAvailable;
    SIZE_T SystemCache;
    SIZE_T KernelTotal;
    SIZE_T KernelPaged;
    SIZE_T KernelNonpaged;
    SIZE_T PageSize;
    DWORD HandleCount;
    DWORD ProcessCount;
    DWORD ThreadCount;
}
alias PERFORMANCE_INFORMATION* PPERFORMANCE_INFORMATION;

struct ENUM_PAGE_FILE_INFORMATION {
    DWORD cb;
    DWORD Reserved;
    SIZE_T TotalSize;
    SIZE_T TotalInUse;
    SIZE_T PeakUsage;
}
alias ENUM_PAGE_FILE_INFORMATION* PENUM_PAGE_FILE_INFORMATION;

/* application-defined callback function used with the EnumPageFiles()
 * http://windowssdk.msdn.microsoft.com/library/ms682627.aspx */
alias BOOL function(LPVOID, PENUM_PAGE_FILE_INFORMATION, LPCWSTR)
    PENUM_PAGE_FILE_CALLBACKW;
alias BOOL function(LPVOID, PENUM_PAGE_FILE_INFORMATION, LPCSTR)
    PENUM_PAGE_FILE_CALLBACKA;


// Grouped by application, not in alphabetical order.
extern (Windows) {
    /* Process Information
     * http://windowssdk.msdn.microsoft.com/library/ms684870.aspx */
    BOOL EnumProcesses(DWORD*, DWORD, DWORD*); /* NT/2000/XP/Server2003/Vista/Longhorn */
    DWORD GetProcessImageFileNameA(HANDLE, LPSTR, DWORD); /* XP/Server2003/Vista/Longhorn */
    DWORD GetProcessImageFileNameW(HANDLE, LPWSTR, DWORD); /* XP/Server2003/Vista/Longhorn */

    /* Module Information
     * http://windowssdk.msdn.microsoft.com/library/ms684232.aspx */
    BOOL EnumProcessModules(HANDLE, HMODULE*, DWORD, LPDWORD);
    BOOL EnumProcessModulesEx(HANDLE, HMODULE*, DWORD, LPDWORD, DWORD); /* Vista/Longhorn */
    DWORD GetModuleBaseNameA(HANDLE, HMODULE, LPSTR, DWORD);
    DWORD GetModuleBaseNameW(HANDLE, HMODULE, LPWSTR, DWORD);
    DWORD GetModuleFileNameExA(HANDLE, HMODULE, LPSTR, DWORD);
    DWORD GetModuleFileNameExW(HANDLE, HMODULE, LPWSTR, DWORD);
    BOOL GetModuleInformation(HANDLE, HMODULE, LPMODULEINFO, DWORD);

    /* Device Driver Information
     * http://windowssdk.msdn.microsoft.com/library/ms682578.aspx */
    BOOL EnumDeviceDrivers(LPVOID*, DWORD, LPDWORD);
    DWORD GetDeviceDriverBaseNameA(LPVOID, LPSTR, DWORD);
    DWORD GetDeviceDriverBaseNameW(LPVOID, LPWSTR, DWORD);
    DWORD GetDeviceDriverFileNameA(LPVOID, LPSTR, DWORD);
    DWORD GetDeviceDriverFileNameW(LPVOID, LPWSTR, DWORD);

    /* Process Memory Usage Information
     * http://windowssdk.msdn.microsoft.com/library/ms684879.aspx */
    BOOL GetProcessMemoryInfo(HANDLE, PPROCESS_MEMORY_COUNTERS, DWORD);

    /* Working Set Information
     * http://windowssdk.msdn.microsoft.com/library/ms687398.aspx */
    BOOL EmptyWorkingSet(HANDLE);
    BOOL GetWsChanges(HANDLE, PPSAPI_WS_WATCH_INFORMATION, DWORD);
    BOOL GetWsChangesEx(HANDLE, PPSAPI_WS_WATCH_INFORMATION_EX, DWORD); /* Vista/Longhorn */
    BOOL InitializeProcessForWsWatch(HANDLE);
    BOOL QueryWorkingSet(HANDLE, PVOID, DWORD);
    BOOL QueryWorkingSetEx(HANDLE, PVOID, DWORD);

    /* Memory-Mapped File Information
     * http://windowssdk.msdn.microsoft.com/library/ms684212.aspx */
    DWORD GetMappedFileNameW(HANDLE, LPVOID, LPWSTR, DWORD);
    DWORD GetMappedFileNameA(HANDLE, LPVOID, LPSTR, DWORD);

    /* Resources Information */
    BOOL GetPerformanceInfo(PPERFORMANCE_INFORMATION, DWORD); /* XP/Server2003/Vista/Longhorn */
    BOOL EnumPageFilesW(PENUM_PAGE_FILE_CALLBACKW, LPVOID); /* 2000/XP/Server2003/Vista/Longhorn */
    BOOL EnumPageFilesA(PENUM_PAGE_FILE_CALLBACKA, LPVOID); /* 2000/XP/Server2003/Vista/Longhorn */
}

version (Unicode) {
    alias PENUM_PAGE_FILE_CALLBACKW PENUM_PAGE_FILE_CALLBACK;
    alias GetModuleBaseNameW GetModuleBaseName;
    alias GetModuleFileNameExW GetModuleFileNameEx;
    alias GetMappedFileNameW GetMappedFileName;
    alias GetDeviceDriverBaseNameW GetDeviceDriverBaseName;
    alias GetDeviceDriverFileNameW GetDeviceDriverFileName;
    alias EnumPageFilesW EnumPageFiles;
    alias GetProcessImageFileNameW GetProcessImageFileName;
} else {
    alias PENUM_PAGE_FILE_CALLBACKA PENUM_PAGE_FILE_CALLBACK;
    alias GetModuleBaseNameA GetModuleBaseName;
    alias GetModuleFileNameExA GetModuleFileNameEx;
    alias GetMappedFileNameA GetMappedFileName;
    alias GetDeviceDriverBaseNameA GetDeviceDriverBaseName;
    alias GetDeviceDriverFileNameA GetDeviceDriverFileName;
    alias EnumPageFilesA EnumPageFiles;
    alias GetProcessImageFileNameA GetProcessImageFileName;
}

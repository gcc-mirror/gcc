/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_winsvc.d)
 */
module core.sys.windows.winsvc;
version (Windows):
@system:

version (ANSI) {} else version = Unicode;
pragma(lib, "advapi32");

import core.sys.windows.w32api, core.sys.windows.windef;

// FIXME: check Windows version support

const TCHAR[]
    SERVICES_ACTIVE_DATABASE = "ServicesActive",
    SERVICES_FAILED_DATABASE = "ServicesFailed";

const TCHAR SC_GROUP_IDENTIFIER = '+';

enum DWORD
    SC_MANAGER_ALL_ACCESS         = 0xf003f,
    SC_MANAGER_CONNECT            =  1,
    SC_MANAGER_CREATE_SERVICE     =  2,
    SC_MANAGER_ENUMERATE_SERVICE  =  4,
    SC_MANAGER_LOCK               =  8,
    SC_MANAGER_QUERY_LOCK_STATUS  = 16,
    SC_MANAGER_MODIFY_BOOT_CONFIG = 32;

enum DWORD SERVICE_NO_CHANGE = 0xffffffff;

enum : DWORD {
    SERVICE_STOPPED = 1,
    SERVICE_START_PENDING,
    SERVICE_STOP_PENDING,
    SERVICE_RUNNING,
    SERVICE_CONTINUE_PENDING,
    SERVICE_PAUSE_PENDING,
    SERVICE_PAUSED // = 7
}

enum DWORD
    SERVICE_ACCEPT_STOP                  =   1,
    SERVICE_ACCEPT_PAUSE_CONTINUE        =   2,
    SERVICE_ACCEPT_SHUTDOWN              =   4,
    SERVICE_ACCEPT_PARAMCHANGE           =   8,
    SERVICE_ACCEPT_NETBINDCHANGE         =  16,
    SERVICE_ACCEPT_HARDWAREPROFILECHANGE =  32,
    SERVICE_ACCEPT_POWEREVENT            =  64,
    SERVICE_ACCEPT_SESSIONCHANGE         = 128;

enum : DWORD {
    SERVICE_CONTROL_STOP = 1,
    SERVICE_CONTROL_PAUSE,
    SERVICE_CONTROL_CONTINUE,
    SERVICE_CONTROL_INTERROGATE,
    SERVICE_CONTROL_SHUTDOWN,
    SERVICE_CONTROL_PARAMCHANGE,
    SERVICE_CONTROL_NETBINDADD,
    SERVICE_CONTROL_NETBINDREMOVE,
    SERVICE_CONTROL_NETBINDENABLE,
    SERVICE_CONTROL_NETBINDDISABLE,
    SERVICE_CONTROL_DEVICEEVENT,
    SERVICE_CONTROL_HARDWAREPROFILECHANGE,
    SERVICE_CONTROL_POWEREVENT,
    SERVICE_CONTROL_SESSIONCHANGE, // = 14
}

enum : DWORD {
    SERVICE_ACTIVE = 1,
    SERVICE_INACTIVE,
    SERVICE_STATE_ALL
}

enum DWORD
    SERVICE_QUERY_CONFIG         = 0x0001,
    SERVICE_CHANGE_CONFIG        = 0x0002,
    SERVICE_QUERY_STATUS         = 0x0004,
    SERVICE_ENUMERATE_DEPENDENTS = 0x0008,
    SERVICE_START                = 0x0010,
    SERVICE_STOP                 = 0x0020,
    SERVICE_PAUSE_CONTINUE       = 0x0040,
    SERVICE_INTERROGATE          = 0x0080,
    SERVICE_USER_DEFINED_CONTROL = 0x0100,
    SERVICE_ALL_ACCESS           = 0x01FF | STANDARD_RIGHTS_REQUIRED;

// This is not documented on the MSDN site
enum SERVICE_RUNS_IN_SYSTEM_PROCESS = 1;

enum : DWORD {
    SERVICE_CONFIG_DESCRIPTION         = 1,
    SERVICE_CONFIG_FAILURE_ACTIONS,
    SERVICE_CONFIG_DELAYED_AUTO_START_INFO,
    SERVICE_CONFIG_FAILURE_ACTIONS_FLAG,
    SERVICE_CONFIG_SERVICE_SID_INFO,
    SERVICE_CONFIG_REQUIRED_PRIVILEGES_INFO,
    SERVICE_CONFIG_PRESHUTDOWN_INFO // = 7
}

struct SERVICE_STATUS {
    DWORD dwServiceType;
    DWORD dwCurrentState;
    DWORD dwControlsAccepted;
    DWORD dwWin32ExitCode;
    DWORD dwServiceSpecificExitCode;
    DWORD dwCheckPoint;
    DWORD dwWaitHint;
}
alias SERVICE_STATUS* LPSERVICE_STATUS;

struct ENUM_SERVICE_STATUSA {
    LPSTR          lpServiceName;
    LPSTR          lpDisplayName;
    SERVICE_STATUS ServiceStatus;
}
alias ENUM_SERVICE_STATUSA* LPENUM_SERVICE_STATUSA;

struct ENUM_SERVICE_STATUSW {
    LPWSTR         lpServiceName;
    LPWSTR         lpDisplayName;
    SERVICE_STATUS ServiceStatus;
}
alias ENUM_SERVICE_STATUSW* LPENUM_SERVICE_STATUSW;

struct QUERY_SERVICE_CONFIGA {
    DWORD dwServiceType;
    DWORD dwStartType;
    DWORD dwErrorControl;
    LPSTR lpBinaryPathName;
    LPSTR lpLoadOrderGroup;
    DWORD dwTagId;
    LPSTR lpDependencies;
    LPSTR lpServiceStartName;
    LPSTR lpDisplayName;
}
alias QUERY_SERVICE_CONFIGA* LPQUERY_SERVICE_CONFIGA;

struct QUERY_SERVICE_CONFIGW {
    DWORD  dwServiceType;
    DWORD  dwStartType;
    DWORD  dwErrorControl;
    LPWSTR lpBinaryPathName;
    LPWSTR lpLoadOrderGroup;
    DWORD  dwTagId;
    LPWSTR lpDependencies;
    LPWSTR lpServiceStartName;
    LPWSTR lpDisplayName;
}
alias QUERY_SERVICE_CONFIGW* LPQUERY_SERVICE_CONFIGW;

struct QUERY_SERVICE_LOCK_STATUSA {
    DWORD fIsLocked;
    LPSTR lpLockOwner;
    DWORD dwLockDuration;
}
alias QUERY_SERVICE_LOCK_STATUSA* LPQUERY_SERVICE_LOCK_STATUSA;

struct QUERY_SERVICE_LOCK_STATUSW {
    DWORD  fIsLocked;
    LPWSTR lpLockOwner;
    DWORD  dwLockDuration;
}
alias QUERY_SERVICE_LOCK_STATUSW* LPQUERY_SERVICE_LOCK_STATUSW;

extern (Windows) {
    alias void function(DWORD, LPSTR*)  LPSERVICE_MAIN_FUNCTIONA;
    alias void function(DWORD, LPWSTR*) LPSERVICE_MAIN_FUNCTIONW;
}

struct SERVICE_TABLE_ENTRYA {
    LPSTR                    lpServiceName;
    LPSERVICE_MAIN_FUNCTIONA lpServiceProc;
}
alias SERVICE_TABLE_ENTRYA* LPSERVICE_TABLE_ENTRYA;

struct SERVICE_TABLE_ENTRYW {
    LPWSTR                   lpServiceName;
    LPSERVICE_MAIN_FUNCTIONW lpServiceProc;
}
alias SERVICE_TABLE_ENTRYW* LPSERVICE_TABLE_ENTRYW;

mixin DECLARE_HANDLE!("SC_HANDLE");
alias SC_HANDLE* LPSC_HANDLE;
alias void* SC_LOCK;
mixin DECLARE_HANDLE!("SERVICE_STATUS_HANDLE");

extern (Windows) {
    alias void function(DWORD) LPHANDLER_FUNCTION;
    alias DWORD function(DWORD, DWORD, LPVOID, LPVOID) LPHANDLER_FUNCTION_EX;
}

static if (_WIN32_WINNT >= 0x500) {
    struct SERVICE_STATUS_PROCESS {
        DWORD dwServiceType;
        DWORD dwCurrentState;
        DWORD dwControlsAccepted;
        DWORD dwWin32ExitCode;
        DWORD dwServiceSpecificExitCode;
        DWORD dwCheckPoint;
        DWORD dwWaitHint;
        DWORD dwProcessId;
        DWORD dwServiceFlags;
    }
    alias SERVICE_STATUS_PROCESS* LPSERVICE_STATUS_PROCESS;

    enum SC_STATUS_TYPE {
        SC_STATUS_PROCESS_INFO = 0
    }

    enum SC_ENUM_TYPE {
        SC_ENUM_PROCESS_INFO = 0
    }

    struct ENUM_SERVICE_STATUS_PROCESSA {
        LPSTR                  lpServiceName;
        LPSTR                  lpDisplayName;
        SERVICE_STATUS_PROCESS ServiceStatusProcess;
    }
    alias ENUM_SERVICE_STATUS_PROCESSA* LPENUM_SERVICE_STATUS_PROCESSA;

    struct ENUM_SERVICE_STATUS_PROCESSW {
        LPWSTR                 lpServiceName;
        LPWSTR                 lpDisplayName;
        SERVICE_STATUS_PROCESS ServiceStatusProcess;
    }
    alias ENUM_SERVICE_STATUS_PROCESSW* LPENUM_SERVICE_STATUS_PROCESSW;

    struct SERVICE_DESCRIPTIONA {
        LPSTR lpDescription;
    }
    alias SERVICE_DESCRIPTIONA* LPSERVICE_DESCRIPTIONA;

    struct SERVICE_DESCRIPTIONW {
        LPWSTR lpDescription;
    }
    alias SERVICE_DESCRIPTIONW* LPSERVICE_DESCRIPTIONW;

    enum SC_ACTION_TYPE {
        SC_ACTION_NONE,
        SC_ACTION_RESTART,
        SC_ACTION_REBOOT,
        SC_ACTION_RUN_COMMAND
    }

    struct SC_ACTION {
        SC_ACTION_TYPE Type;
        DWORD          Delay;
    }
    alias SC_ACTION* LPSC_ACTION;

    struct SERVICE_FAILURE_ACTIONSA {
        DWORD      dwResetPeriod;
        LPSTR      lpRebootMsg;
        LPSTR      lpCommand;
        DWORD      cActions;
        SC_ACTION* lpsaActions;
    }
    alias SERVICE_FAILURE_ACTIONSA* LPSERVICE_FAILURE_ACTIONSA;

    struct SERVICE_FAILURE_ACTIONSW {
        DWORD      dwResetPeriod;
        LPWSTR     lpRebootMsg;
        LPWSTR     lpCommand;
        DWORD      cActions;
        SC_ACTION* lpsaActions;
    }
    alias SERVICE_FAILURE_ACTIONSW* LPSERVICE_FAILURE_ACTIONSW;
}

extern (Windows) {
    BOOL ChangeServiceConfigA(SC_HANDLE, DWORD, DWORD, DWORD, LPCSTR,
      LPCSTR, LPDWORD, LPCSTR, LPCSTR, LPCSTR, LPCSTR);
    BOOL ChangeServiceConfigW(SC_HANDLE, DWORD, DWORD, DWORD, LPCWSTR,
      LPCWSTR, LPDWORD, LPCWSTR, LPCWSTR, LPCWSTR, LPCWSTR);
    BOOL CloseServiceHandle(SC_HANDLE);
    BOOL ControlService(SC_HANDLE, DWORD, LPSERVICE_STATUS);
    SC_HANDLE CreateServiceA(SC_HANDLE, LPCSTR, LPCSTR, DWORD, DWORD,
      DWORD, DWORD, LPCSTR, LPCSTR, PDWORD, LPCSTR, LPCSTR, LPCSTR);
    SC_HANDLE CreateServiceW(SC_HANDLE, LPCWSTR, LPCWSTR, DWORD, DWORD,
      DWORD, DWORD, LPCWSTR, LPCWSTR, PDWORD, LPCWSTR, LPCWSTR, LPCWSTR);
    BOOL DeleteService(SC_HANDLE);
    BOOL EnumDependentServicesA(SC_HANDLE, DWORD, LPENUM_SERVICE_STATUSA,
      DWORD, PDWORD, PDWORD);
    BOOL EnumDependentServicesW(SC_HANDLE, DWORD, LPENUM_SERVICE_STATUSW,
      DWORD, PDWORD, PDWORD);
    BOOL EnumServicesStatusA(SC_HANDLE, DWORD, DWORD, LPENUM_SERVICE_STATUSA,
      DWORD, PDWORD, PDWORD, PDWORD);
    BOOL EnumServicesStatusW(SC_HANDLE, DWORD, DWORD, LPENUM_SERVICE_STATUSW,
      DWORD, PDWORD, PDWORD, PDWORD);
    BOOL GetServiceDisplayNameA(SC_HANDLE, LPCSTR, LPSTR, PDWORD);
    BOOL GetServiceDisplayNameW(SC_HANDLE, LPCWSTR, LPWSTR, PDWORD);
    BOOL GetServiceKeyNameA(SC_HANDLE, LPCSTR, LPSTR, PDWORD);
    BOOL GetServiceKeyNameW(SC_HANDLE, LPCWSTR, LPWSTR, PDWORD);
    SC_LOCK LockServiceDatabase(SC_HANDLE);
    BOOL NotifyBootConfigStatus(BOOL);
    SC_HANDLE OpenSCManagerA(LPCSTR, LPCSTR, DWORD);
    SC_HANDLE OpenSCManagerW(LPCWSTR, LPCWSTR, DWORD);
    SC_HANDLE OpenServiceA(SC_HANDLE, LPCSTR, DWORD);
    SC_HANDLE OpenServiceW(SC_HANDLE, LPCWSTR, DWORD);
    BOOL QueryServiceConfigA(SC_HANDLE, LPQUERY_SERVICE_CONFIGA, DWORD,
      PDWORD);
    BOOL QueryServiceConfigW(SC_HANDLE, LPQUERY_SERVICE_CONFIGW, DWORD,
      PDWORD);
    BOOL QueryServiceLockStatusA(SC_HANDLE, LPQUERY_SERVICE_LOCK_STATUSA,
      DWORD, PDWORD);
    BOOL QueryServiceLockStatusW(SC_HANDLE, LPQUERY_SERVICE_LOCK_STATUSW,
      DWORD, PDWORD);
    BOOL QueryServiceObjectSecurity(SC_HANDLE, SECURITY_INFORMATION,
      PSECURITY_DESCRIPTOR, DWORD, LPDWORD);
    BOOL QueryServiceStatus(SC_HANDLE, LPSERVICE_STATUS);
    SERVICE_STATUS_HANDLE RegisterServiceCtrlHandlerA(LPCSTR,
      LPHANDLER_FUNCTION);
    SERVICE_STATUS_HANDLE RegisterServiceCtrlHandlerW(LPCWSTR,
      LPHANDLER_FUNCTION);
    BOOL SetServiceObjectSecurity(SC_HANDLE, SECURITY_INFORMATION,
      PSECURITY_DESCRIPTOR);
    BOOL SetServiceStatus(SERVICE_STATUS_HANDLE, LPSERVICE_STATUS);
    BOOL StartServiceA(SC_HANDLE, DWORD, LPCSTR*);
    BOOL StartServiceW(SC_HANDLE, DWORD, LPCWSTR*);
    BOOL StartServiceCtrlDispatcherA(LPSERVICE_TABLE_ENTRYA);
    BOOL StartServiceCtrlDispatcherW(LPSERVICE_TABLE_ENTRYW);
    BOOL UnlockServiceDatabase(SC_LOCK);

    static if (_WIN32_WINNT >= 0x500) {
        BOOL EnumServicesStatusExA(SC_HANDLE, SC_ENUM_TYPE, DWORD, DWORD, LPBYTE,
          DWORD, LPDWORD, LPDWORD, LPDWORD, LPCSTR);
        BOOL EnumServicesStatusExW(SC_HANDLE, SC_ENUM_TYPE, DWORD, DWORD, LPBYTE,
          DWORD, LPDWORD, LPDWORD, LPDWORD, LPCWSTR);
        BOOL QueryServiceConfig2A(SC_HANDLE, DWORD, LPBYTE, DWORD, LPDWORD);
        BOOL QueryServiceConfig2W(SC_HANDLE, DWORD, LPBYTE, DWORD, LPDWORD);
        BOOL QueryServiceStatusEx(SC_HANDLE, SC_STATUS_TYPE, LPBYTE, DWORD,
          LPDWORD);
        SERVICE_STATUS_HANDLE RegisterServiceCtrlHandlerExA(LPCSTR,
          LPHANDLER_FUNCTION_EX, LPVOID);
        SERVICE_STATUS_HANDLE RegisterServiceCtrlHandlerExW(LPCWSTR,
          LPHANDLER_FUNCTION_EX, LPVOID);
    }

    static if (_WIN32_WINNT >= 0x501) {
        BOOL ChangeServiceConfig2A(SC_HANDLE, DWORD, LPVOID);
        BOOL ChangeServiceConfig2W(SC_HANDLE, DWORD, LPVOID);
    }
}

version (Unicode) {
    alias ENUM_SERVICE_STATUSW ENUM_SERVICE_STATUS;
    alias QUERY_SERVICE_CONFIGW QUERY_SERVICE_CONFIG;
    alias QUERY_SERVICE_LOCK_STATUSW QUERY_SERVICE_LOCK_STATUS;
    alias LPSERVICE_MAIN_FUNCTIONW LPSERVICE_MAIN_FUNCTION;
    alias SERVICE_TABLE_ENTRYW SERVICE_TABLE_ENTRY;
    alias ChangeServiceConfigW ChangeServiceConfig;
    alias CreateServiceW CreateService;
    alias EnumDependentServicesW EnumDependentServices;
    alias EnumServicesStatusW EnumServicesStatus;
    alias GetServiceDisplayNameW GetServiceDisplayName;
    alias GetServiceKeyNameW GetServiceKeyName;
    alias OpenSCManagerW OpenSCManager;
    alias OpenServiceW OpenService;
    alias QueryServiceConfigW QueryServiceConfig;
    alias QueryServiceLockStatusW QueryServiceLockStatus;
    alias RegisterServiceCtrlHandlerW RegisterServiceCtrlHandler;
    alias StartServiceW StartService;
    alias StartServiceCtrlDispatcherW StartServiceCtrlDispatcher;

    static if (_WIN32_WINNT >= 0x500) {
        alias ENUM_SERVICE_STATUS_PROCESSW ENUM_SERVICE_STATUS_PROCESS;
        alias SERVICE_DESCRIPTIONW SERVICE_DESCRIPTION;
        alias SERVICE_FAILURE_ACTIONSW SERVICE_FAILURE_ACTIONS;
        alias EnumServicesStatusExW EnumServicesStatusEx;
        alias QueryServiceConfig2W QueryServiceConfig2;
        alias RegisterServiceCtrlHandlerExW RegisterServiceCtrlHandlerEx;
    }

    static if (_WIN32_WINNT >= 0x501) {
        alias ChangeServiceConfig2W ChangeServiceConfig2;
    }

} else {
    alias ENUM_SERVICE_STATUSA ENUM_SERVICE_STATUS;
    alias QUERY_SERVICE_CONFIGA QUERY_SERVICE_CONFIG;
    alias QUERY_SERVICE_LOCK_STATUSA QUERY_SERVICE_LOCK_STATUS;
    alias LPSERVICE_MAIN_FUNCTIONA LPSERVICE_MAIN_FUNCTION;
    alias SERVICE_TABLE_ENTRYA SERVICE_TABLE_ENTRY;
    alias ChangeServiceConfigA ChangeServiceConfig;
    alias CreateServiceA CreateService;
    alias EnumDependentServicesA EnumDependentServices;
    alias EnumServicesStatusA EnumServicesStatus;
    alias GetServiceDisplayNameA GetServiceDisplayName;
    alias GetServiceKeyNameA GetServiceKeyName;
    alias OpenSCManagerA OpenSCManager;
    alias OpenServiceA OpenService;
    alias QueryServiceConfigA QueryServiceConfig;
    alias QueryServiceLockStatusA QueryServiceLockStatus;
    alias RegisterServiceCtrlHandlerA RegisterServiceCtrlHandler;
    alias StartServiceA StartService;
    alias StartServiceCtrlDispatcherA StartServiceCtrlDispatcher;

    static if (_WIN32_WINNT >= 0x500) {
        alias ENUM_SERVICE_STATUS_PROCESSA ENUM_SERVICE_STATUS_PROCESS;
        alias SERVICE_DESCRIPTIONA SERVICE_DESCRIPTION;
        alias SERVICE_FAILURE_ACTIONSA SERVICE_FAILURE_ACTIONS;
        alias EnumServicesStatusExA EnumServicesStatusEx;
        alias QueryServiceConfig2A QueryServiceConfig2;
        alias RegisterServiceCtrlHandlerExA RegisterServiceCtrlHandlerEx;
    }

    static if (_WIN32_WINNT >= 0x501) {
        alias ChangeServiceConfig2A ChangeServiceConfig2;
    }

}

alias ENUM_SERVICE_STATUS* LPENUM_SERVICE_STATUS;
alias QUERY_SERVICE_CONFIG* LPQUERY_SERVICE_CONFIG;
alias QUERY_SERVICE_LOCK_STATUS* LPQUERY_SERVICE_LOCK_STATUS;
alias SERVICE_TABLE_ENTRY* LPSERVICE_TABLE_ENTRY;

static if (_WIN32_WINNT >= 0x500) {
    alias ENUM_SERVICE_STATUS_PROCESS* LPENUM_SERVICE_STATUS_PROCESS;
    alias SERVICE_DESCRIPTION* LPSERVICE_DESCRIPTION;
    alias SERVICE_FAILURE_ACTIONS* LPSERVICE_FAILURE_ACTIONS;
}

/**
 * Windows API header module
 *
 * Translated from MinGW-w64 API
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_wtsapi32.d)
 */
module core.sys.windows.wtsapi32;
version (Windows):

version (ANSI) {} else version = Unicode;
pragma(lib, "wtsapi32");
import core.sys.windows.w32api;
import core.sys.windows.windef;

enum {
    WTS_CURRENT_SERVER        = null,
    WTS_CURRENT_SERVER_HANDLE = null,
    WTS_CURRENT_SERVER_NAME   = null
}

enum DWORD WTS_CURRENT_SESSION = cast(DWORD) -1;

enum {
    IDTIMEOUT = 32000,
    IDASYNC   = 32001
}

enum {
    WTS_WSD_LOGOFF     = 0x01,
    WTS_WSD_SHUTDOWN   = 0x02,
    WTS_WSD_REBOOT     = 0x04,
    WTS_WSD_POWEROFF   = 0x08,
    WTS_WSD_FASTREBOOT = 0x10
}

enum WTS_CONNECTSTATE_CLASS {
    WTSActive,
    WTSConnected,
    WTSConnectQuery,
    WTSShadow,
    WTSDisconnected,
    WTSIdle,
    WTSListen,
    WTSReset,
    WTSDown,
    WTSInit
}

struct WTS_SERVER_INFOW {
    LPWSTR pServerName;
}
alias WTS_SERVER_INFOW* PWTS_SERVER_INFOW;

struct WTS_SERVER_INFOA {
    LPSTR pServerName;
}
alias WTS_SERVER_INFOA* PWTS_SERVER_INFOA;

version (Unicode) {
    alias WTS_SERVER_INFOW  WTS_SERVER_INFO;
    alias PWTS_SERVER_INFOW PWTS_SERVER_INFO;
} else {
    alias WTS_SERVER_INFOA  WTS_SERVER_INFO;
    alias PWTS_SERVER_INFOA PWTS_SERVER_INFO;
}

struct WTS_SESSION_INFOW {
    DWORD SessionId;
    LPWSTR pWinStationName;
    WTS_CONNECTSTATE_CLASS State;
}
alias WTS_SESSION_INFOW* PWTS_SESSION_INFOW;

struct WTS_SESSION_INFOA {
    DWORD SessionId;
    LPSTR pWinStationName;
    WTS_CONNECTSTATE_CLASS State;
}
alias WTS_SESSION_INFOA* PWTS_SESSION_INFOA;

version (Unicode) {
    alias WTS_SESSION_INFOW  WTS_SESSION_INFO;
    alias PWTS_SESSION_INFOW PWTS_SESSION_INFO;
} else {
    alias WTS_SESSION_INFOA  WTS_SESSION_INFO;
    alias PWTS_SESSION_INFOA PWTS_SESSION_INFO;
}

struct WTS_PROCESS_INFOW {
    DWORD SessionId;
    DWORD ProcessId;
    LPWSTR pProcessName;
    PSID pUserSid;
}
alias WTS_PROCESS_INFOW* PWTS_PROCESS_INFOW;

struct WTS_PROCESS_INFOA {
    DWORD SessionId;
    DWORD ProcessId;
    LPSTR pProcessName;
    PSID pUserSid;
}
alias WTS_PROCESS_INFOA* PWTS_PROCESS_INFOA;

version (Unicode) {
    alias WTS_PROCESS_INFOW  WTS_PROCESS_INFO;
    alias PWTS_PROCESS_INFOW PWTS_PROCESS_INFO;
} else {
    alias WTS_PROCESS_INFOA  WTS_PROCESS_INFO;
    alias PWTS_PROCESS_INFOA PWTS_PROCESS_INFO;
}

enum {
    WTS_PROTOCOL_TYPE_CONSOLE,
    WTS_PROTOCOL_TYPE_ICA,
    WTS_PROTOCOL_TYPE_RDP
}

enum WTS_INFO_CLASS {
    WTSInitialProgram,
    WTSApplicationName,
    WTSWorkingDirectory,
    WTSOEMId,
    WTSSessionId,
    WTSUserName,
    WTSWinStationName,
    WTSDomainName,
    WTSConnectState,
    WTSClientBuildNumber,
    WTSClientName,
    WTSClientDirectory,
    WTSClientProductId,
    WTSClientHardwareId,
    WTSClientAddress,
    WTSClientDisplay,
    WTSClientProtocolType,
    WTSIdleTime,
    WTSLogonTime,
    WTSIncomingBytes,
    WTSOutgoingBytes,
    WTSIncomingFrames,
    WTSOutgoingFrames,
    WTSClientInfo,
    WTSSessionInfo, // = 24
}

struct WTS_CLIENT_ADDRESS {
    DWORD    AddressFamily;
    BYTE[20] Address;
}
alias WTS_CLIENT_ADDRESS* PWTS_CLIENT_ADDRESS;

struct WTS_CLIENT_DISPLAY {
    DWORD HorizontalResolution;
    DWORD VerticalResolution;
    DWORD ColorDepth;
}
alias WTS_CLIENT_DISPLAY* PWTS_CLIENT_DISPLAY;

enum WTS_CONFIG_CLASS {
    WTSUserConfigInitialProgram,
    WTSUserConfigWorkingDirectory,
    WTSUserConfigfInheritInitialProgram,
    WTSUserConfigfAllowLogonTerminalServer,
    WTSUserConfigTimeoutSettingsConnections,
    WTSUserConfigTimeoutSettingsDisconnections,
    WTSUserConfigTimeoutSettingsIdle,
    WTSUserConfigfDeviceClientDrives,
    WTSUserConfigfDeviceClientPrinters,
    WTSUserConfigfDeviceClientDefaultPrinter,
    WTSUserConfigBrokenTimeoutSettings,
    WTSUserConfigReconnectSettings,
    WTSUserConfigModemCallbackSettings,
    WTSUserConfigModemCallbackPhoneNumber,
    WTSUserConfigShadowingSettings,
    WTSUserConfigTerminalServerProfilePath,
    WTSUserConfigTerminalServerHomeDir,
    WTSUserConfigTerminalServerHomeDirDrive,
    WTSUserConfigfTerminalServerRemoteHomeDir
}

enum {
    WTS_EVENT_NONE        = 0x0,
    WTS_EVENT_CREATE      = 0x1,
    WTS_EVENT_DELETE      = 0x2,
    WTS_EVENT_RENAME      = 0x4,
    WTS_EVENT_CONNECT     = 0x8,
    WTS_EVENT_DISCONNECT  = 0x10,
    WTS_EVENT_LOGON       = 0x20,
    WTS_EVENT_LOGOFF      = 0x40,
    WTS_EVENT_STATECHANGE = 0x80,
    WTS_EVENT_LICENSE     = 0x100,
    WTS_EVENT_ALL         = 0x7fffffff,
    WTS_EVENT_FLUSH       = 0x80000000
}

enum WTS_VIRTUAL_CLASS {
    WTSVirtualClientData,
    WTSVirtualFileHandle
}

version (Unicode) {
    alias WTSEnumerateServersW WTSEnumerateServers;
    alias WTSOpenServerW WTSOpenServer;
    alias WTSEnumerateSessionsW WTSEnumerateSessions;
    alias WTSEnumerateProcessesW WTSEnumerateProcesses;
    alias WTSQuerySessionInformationW WTSQuerySessionInformation;
    alias WTSQueryUserConfigW WTSQueryUserConfig;
    alias WTSSetUserConfigW WTSSetUserConfig;
    alias WTSSendMessageW WTSSendMessage;
} else {
    alias WTSEnumerateServersA WTSEnumerateServers;
    alias WTSOpenServerA WTSOpenServer;
    alias WTSEnumerateSessionsA WTSEnumerateSessions;
    alias WTSEnumerateProcessesA WTSEnumerateProcesses;
    alias WTSQuerySessionInformationA WTSQuerySessionInformation;
    alias WTSQueryUserConfigA WTSQueryUserConfig;
    alias WTSSetUserConfigA WTSSetUserConfig;
    alias WTSSendMessageA WTSSendMessage;
}

extern(Windows) {
    WINBOOL WTSEnumerateServersW(LPWSTR pDomainName, DWORD Reserved, DWORD Version, PWTS_SERVER_INFOW* ppServerInfo, DWORD* pCount);
    WINBOOL WTSEnumerateServersA(LPSTR pDomainName, DWORD Reserved, DWORD Version, PWTS_SERVER_INFOA* ppServerInfo, DWORD* pCount);
    HANDLE WTSOpenServerW(LPWSTR pServerName);
    HANDLE WTSOpenServerA(LPSTR pServerName);
    VOID WTSCloseServer(HANDLE hServer);
    WINBOOL WTSEnumerateSessionsW(HANDLE hServer, DWORD Reserved, DWORD Version, PWTS_SESSION_INFOW* ppSessionInfo, DWORD* pCount);
    WINBOOL WTSEnumerateSessionsA(HANDLE hServer, DWORD Reserved, DWORD Version, PWTS_SESSION_INFOA* ppSessionInfo, DWORD* pCount);
    WINBOOL WTSEnumerateProcessesW(HANDLE hServer, DWORD Reserved, DWORD Version, PWTS_PROCESS_INFOW* ppProcessInfo, DWORD* pCount);
    WINBOOL WTSEnumerateProcessesA(HANDLE hServer, DWORD Reserved, DWORD Version, PWTS_PROCESS_INFOA* ppProcessInfo, DWORD* pCount);
    WINBOOL WTSTerminateProcess(HANDLE hServer, DWORD ProcessId, DWORD ExitCode);
    WINBOOL WTSQuerySessionInformationW(HANDLE hServer, DWORD SessionId, WTS_INFO_CLASS WTSInfoClass, LPWSTR* ppBuffer, DWORD* pBytesReturned);
    WINBOOL WTSQuerySessionInformationA(HANDLE hServer, DWORD SessionId, WTS_INFO_CLASS WTSInfoClass, LPSTR* ppBuffer, DWORD* pBytesReturned);
    WINBOOL WTSQueryUserConfigW(LPWSTR pServerName, LPWSTR pUserName, WTS_CONFIG_CLASS WTSConfigClass, LPWSTR* ppBuffer, DWORD* pBytesReturned);
    WINBOOL WTSQueryUserConfigA(LPSTR pServerName, LPSTR pUserName, WTS_CONFIG_CLASS WTSConfigClass, LPSTR* ppBuffer, DWORD* pBytesReturned);
    WINBOOL WTSSetUserConfigW(LPWSTR pServerName, LPWSTR pUserName, WTS_CONFIG_CLASS WTSConfigClass, LPWSTR pBuffer, DWORD DataLength);
    WINBOOL WTSSetUserConfigA(LPSTR pServerName, LPSTR pUserName, WTS_CONFIG_CLASS WTSConfigClass, LPSTR pBuffer, DWORD DataLength);
    WINBOOL WTSSendMessageW(HANDLE hServer, DWORD SessionId, LPWSTR pTitle, DWORD TitleLength, LPWSTR pMessage, DWORD MessageLength, DWORD Style, DWORD Timeout, DWORD* pResponse, WINBOOL bWait);
    WINBOOL WTSSendMessageA(HANDLE hServer, DWORD SessionId, LPSTR pTitle, DWORD TitleLength, LPSTR pMessage, DWORD MessageLength, DWORD Style, DWORD Timeout, DWORD* pResponse, WINBOOL bWait);
    WINBOOL WTSDisconnectSession(HANDLE hServer, DWORD SessionId, WINBOOL bWait);
    WINBOOL WTSLogoffSession(HANDLE hServer, DWORD SessionId, WINBOOL bWait);
    WINBOOL WTSShutdownSystem(HANDLE hServer, DWORD ShutdownFlag);
    WINBOOL WTSWaitSystemEvent(HANDLE hServer, DWORD EventMask, DWORD* pEventFlags);
    HANDLE WTSVirtualChannelOpen(HANDLE hServer, DWORD SessionId, LPSTR pVirtualName);
    WINBOOL WTSVirtualChannelClose(HANDLE hChannelHandle);
    WINBOOL WTSVirtualChannelRead(HANDLE hChannelHandle, ULONG TimeOut, PCHAR Buffer, ULONG BufferSize, PULONG pBytesRead);
    WINBOOL WTSVirtualChannelWrite(HANDLE hChannelHandle, PCHAR Buffer, ULONG Length, PULONG pBytesWritten);
    WINBOOL WTSVirtualChannelPurgeInput(HANDLE hChannelHandle);
    WINBOOL WTSVirtualChannelPurgeOutput(HANDLE hChannelHandle);
    WINBOOL WTSVirtualChannelQuery(HANDLE hChannelHandle, WTS_VIRTUAL_CLASS, PVOID* ppBuffer, DWORD* pBytesReturned);
    VOID WTSFreeMemory(PVOID pMemory);

    WINBOOL WTSRegisterSessionNotification(HWND hWnd, DWORD dwFlags);
    WINBOOL WTSUnRegisterSessionNotification(HWND hWnd);
    WINBOOL WTSQueryUserToken(ULONG SessionId, PHANDLE phToken);
}

enum {
    NOTIFY_FOR_ALL_SESSIONS = 1,
    NOTIFY_FOR_THIS_SESSION = 0
}

enum {
    USERNAME_LENGTH       = 20,
    CLIENTNAME_LENGTH     = 20,
    CLIENTADDRESS_LENGTH  = 30,
    WINSTATIONNAME_LENGTH = 32,
    DOMAIN_LENGTH         = 17
}

static if (_WIN32_WINNT >= 0x600) {
    struct WTSCLIENTW {
        WCHAR[CLIENTNAME_LENGTH + 1]      ClientName = 0;
        WCHAR[DOMAIN_LENGTH + 1]          Domain = 0;
        WCHAR[USERNAME_LENGTH + 1]        UserName = 0;
        WCHAR[MAX_PATH + 1]               WorkDirectory = 0;
        WCHAR[MAX_PATH + 1]               InitialProgram = 0;
        BYTE                              EncryptionLevel;
        ULONG                             ClientAddressFamily;
        USHORT[CLIENTADDRESS_LENGTH + 1]  ClientAddress;
        USHORT                            HRes;
        USHORT                            VRes;
        USHORT                            ColorDepth;
        WCHAR[MAX_PATH + 1]               ClientDirectory = 0;
        ULONG                             ClientBuildNumber;
        ULONG                             ClientHardwareId;
        USHORT                            ClientProductId;
        USHORT                            OutBufCountHost;
        USHORT                            OutBufCountClient;
        USHORT                            OutBufLength;
        WCHAR[MAX_PATH + 1]               DeviceId = 0;
    }
    alias WTSCLIENTW* PWTSCLIENTW;

    struct WTSCLIENTA {
        CHAR[CLIENTNAME_LENGTH + 1]       ClientName = 0;
        CHAR[DOMAIN_LENGTH + 1 ]          Domain = 0;
        CHAR[USERNAME_LENGTH + 1]         UserName = 0;
        CHAR[MAX_PATH + 1]                WorkDirectory = 0;
        CHAR[MAX_PATH + 1]                InitialProgram = 0;
        BYTE                              EncryptionLevel;
        ULONG                             ClientAddressFamily;
        USHORT[CLIENTADDRESS_LENGTH + 1]  ClientAddress;
        USHORT                            HRes;
        USHORT                            VRes;
        USHORT                            ColorDepth;
        CHAR[MAX_PATH + 1]                ClientDirectory = 0;
        ULONG                             ClientBuildNumber;
        ULONG                             ClientHardwareId;
        USHORT                            ClientProductId;
        USHORT                            OutBufCountHost;
        USHORT                            OutBufCountClient;
        USHORT                            OutBufLength;
        CHAR[MAX_PATH + 1]                DeviceId = 0;
    }
    alias WTSCLIENTA* PWTSCLIENTA;

    version (Unicode) {
        alias WTSCLIENTW  WTSCLIENT;
        alias PWTSCLIENTW PWTSCLIENT;
    } else {
        alias WTSCLIENTA  WTSCLIENT;
        alias PWTSCLIENTA PWTSCLIENT;
    }

    struct WTSINFOW {
        WTS_CONNECTSTATE_CLASS       State;
        DWORD                        SessionId;
        DWORD                        IncomingBytes;
        DWORD                        OutgoingBytes;
        DWORD                        IncomingFrames;
        DWORD                        OutgoingFrames;
        DWORD                        IncomingCompressedBytes;
        DWORD                        OutgoingCompressedBytes;
        WCHAR[WINSTATIONNAME_LENGTH] WinStationName = 0;
        WCHAR[DOMAIN_LENGTH]         Domain = 0;
        WCHAR[USERNAME_LENGTH+1]     UserName = 0;
        LARGE_INTEGER                ConnectTime;
        LARGE_INTEGER                DisconnectTime;
        LARGE_INTEGER                LastInputTime;
        LARGE_INTEGER                LogonTime;
        LARGE_INTEGER                CurrentTime;
    }
    alias WTSINFOW* PWTSINFOW;

    struct WTSINFOA {
        WTS_CONNECTSTATE_CLASS      State;
        DWORD                       SessionId;
        DWORD                       IncomingBytes;
        DWORD                       OutgoingBytes;
        DWORD                       IncomingFrames;
        DWORD                       OutgoingFrames;
        DWORD                       IncomingCompressedBytes;
        DWORD                       OutgoingCompressedBytes;
        CHAR[WINSTATIONNAME_LENGTH] WinStationName = 0;
        CHAR[DOMAIN_LENGTH]         Domain = 0;
        CHAR[USERNAME_LENGTH+1]     UserName = 0;
        LARGE_INTEGER               ConnectTime;
        LARGE_INTEGER               DisconnectTime;
        LARGE_INTEGER               LastInputTime;
        LARGE_INTEGER               LogonTime;
        LARGE_INTEGER               CurrentTime;
    }
    alias WTSINFOA* PWTSINFOA;

    version (Unicode) {
        alias WTSINFOW  WTSINFO;
        alias PWTSINFOW PWTSINFO;
    } else {
        alias WTSINFOA  WTSINFO;
        alias PWTSINFOA PWTSINFO;
    }

    extern(Windows) {
        WINBOOL WTSConnectSessionA(
            ULONG LogonId,
            ULONG TargetLogonId,
            PSTR   pPassword,
            WINBOOL bWait
        );

        WINBOOL WTSConnectSessionW(
            ULONG LogonId,
            ULONG TargetLogonId,
            PWSTR  pPassword,
            WINBOOL bWait
        );

        WINBOOL WTSRegisterSessionNotificationEx(
            HANDLE hServer,
            HWND hWnd,
            DWORD dwFlags
        );

        WINBOOL WTSStartRemoteControlSessionA(
            LPSTR pTargetServerName,
            ULONG TargetLogonId,
            BYTE HotkeyVk,
            USHORT HotkeyModifiers
        );

        WINBOOL WTSStartRemoteControlSessionW(
            LPWSTR pTargetServerName,
            ULONG TargetLogonId,
            BYTE HotkeyVk,
            USHORT HotkeyModifiers
        );

        version (Unicode) {
            alias WTSStartRemoteControlSessionW WTSStartRemoteControlSession;
            alias WTSConnectSessionW WTSConnectSession;
        } else {
            alias WTSStartRemoteControlSessionA WTSStartRemoteControlSession;
            alias WTSConnectSessionA WTSConnectSession;
        }

        WINBOOL WTSStopRemoteControlSession(
            ULONG LogonId
        );

        WINBOOL WTSUnRegisterSessionNotificationEx(
            HANDLE hServer,
            HWND hWnd
        );

        HANDLE WTSVirtualChannelOpenEx(
            DWORD SessionId,
            LPSTR pVirtualName,
            DWORD flags
        );
    } /* extern(Windows) */
} /* static if (_WIN32_WINNT >= 0x600) */

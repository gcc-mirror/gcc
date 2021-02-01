/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_ras.d)
 */
module core.sys.windows.ras;
version (Windows):
@system:

version (ANSI) {} else version = Unicode;
pragma(lib, "rasapi32");

import core.sys.windows.basetyps, core.sys.windows.lmcons, core.sys.windows.w32api, core.sys.windows.windef;

align(4):

enum RAS_MaxDeviceType = 16;
enum RAS_MaxPhoneNumber = 128;
enum RAS_MaxIpAddress = 15;
enum RAS_MaxIpxAddress = 21;
enum RAS_MaxEntryName = 256;
enum RAS_MaxDeviceName = 128;
enum RAS_MaxCallbackNumber = RAS_MaxPhoneNumber;
enum RAS_MaxAreaCode = 10;
enum RAS_MaxPadType = 32;
enum RAS_MaxX25Address = 200;
enum RAS_MaxFacilities = 200;
enum RAS_MaxUserData = 200;
enum RAS_MaxReplyMessage = 1024;

enum RDEOPT_UsePrefixSuffix           = 0x00000001;
enum RDEOPT_PausedStates              = 0x00000002;
enum RDEOPT_IgnoreModemSpeaker        = 0x00000004;
enum RDEOPT_SetModemSpeaker           = 0x00000008;
enum RDEOPT_IgnoreSoftwareCompression = 0x00000010;
enum RDEOPT_SetSoftwareCompression    = 0x00000020;
enum RDEOPT_DisableConnectedUI        = 0x00000040;
enum RDEOPT_DisableReconnectUI        = 0x00000080;
enum RDEOPT_DisableReconnect          = 0x00000100;
enum RDEOPT_NoUser                    = 0x00000200;
enum RDEOPT_PauseOnScript             = 0x00000400;
enum RDEOPT_Router                    = 0x00000800;

enum REN_User = 0x00000000;
enum REN_AllUsers = 0x00000001;
enum VS_Default = 0;
enum VS_PptpOnly = 1;
enum VS_PptpFirst = 2;
enum VS_L2tpOnly = 3;
enum VS_L2tpFirst = 4;

enum RASDIALEVENT = "RasDialEvent";
enum WM_RASDIALEVENT = 0xCCCD;

enum RASEO_UseCountryAndAreaCodes = 0x00000001;
enum RASEO_SpecificIpAddr = 0x00000002;
enum RASEO_SpecificNameServers = 0x00000004;
enum RASEO_IpHeaderCompression = 0x00000008;
enum RASEO_RemoteDefaultGateway = 0x00000010;
enum RASEO_DisableLcpExtensions = 0x00000020;
enum RASEO_TerminalBeforeDial = 0x00000040;
enum RASEO_TerminalAfterDial = 0x00000080;
enum RASEO_ModemLights = 0x00000100;
enum RASEO_SwCompression = 0x00000200;
enum RASEO_RequireEncryptedPw = 0x00000400;
enum RASEO_RequireMsEncryptedPw = 0x00000800;
enum RASEO_RequireDataEncryption = 0x00001000;
enum RASEO_NetworkLogon = 0x00002000;
enum RASEO_UseLogonCredentials = 0x00004000;
enum RASEO_PromoteAlternates = 0x00008000;
enum RASNP_NetBEUI = 0x00000001;
enum RASNP_Ipx = 0x00000002;
enum RASNP_Ip = 0x00000004;
enum RASFP_Ppp = 0x00000001;
enum RASFP_Slip = 0x00000002;
enum RASFP_Ras = 0x00000004;

const TCHAR[]
    RASDT_Modem = "modem",
    RASDT_Isdn = "isdn",
    RASDT_X25 = "x25",
    RASDT_Vpn = "vpn",
    RASDT_Pad = "pad",
    RASDT_Generic = "GENERIC",
    RASDT_Serial = "SERIAL",
    RASDT_FrameRelay = "FRAMERELAY",
    RASDT_Atm = "ATM",
    RASDT_Sonet = "SONET",
    RASDT_SW56 = "SW56",
    RASDT_Irda = "IRDA",
    RASDT_Parallel = "PARALLEL";

enum RASET_Phone = 1;
enum RASET_Vpn = 2;
enum RASET_Direct = 3;
enum RASET_Internet = 4;

static if (_WIN32_WINNT >= 0x401) {
enum RASEO_SecureLocalFiles = 0x00010000;
enum RASCN_Connection = 0x00000001;
enum RASCN_Disconnection = 0x00000002;
enum RASCN_BandwidthAdded = 0x00000004;
enum RASCN_BandwidthRemoved = 0x00000008;
enum RASEDM_DialAll = 1;
enum RASEDM_DialAsNeeded = 2;
enum RASIDS_Disabled = 0xffffffff;
enum RASIDS_UseGlobalValue = 0;
enum RASADFLG_PositionDlg = 0x00000001;
enum RASCM_UserName = 0x00000001;
enum RASCM_Password = 0x00000002;
enum RASCM_Domain = 0x00000004;
enum RASADP_DisableConnectionQuery = 0;
enum RASADP_LoginSessionDisable = 1;
enum RASADP_SavedAddressesLimit = 2;
enum RASADP_FailedConnectionTimeout = 3;
enum RASADP_ConnectionQueryTimeout = 4;
}
//static if (_WIN32_WINNT >= 0x500) {
enum RDEOPT_CustomDial = 0x00001000;
enum RASLCPAP_PAP = 0xC023;
enum RASLCPAP_SPAP = 0xC027;
enum RASLCPAP_CHAP = 0xC223;
enum RASLCPAP_EAP = 0xC227;
enum RASLCPAD_CHAP_MD5 = 0x05;
enum RASLCPAD_CHAP_MS = 0x80;
enum RASLCPAD_CHAP_MSV2 = 0x81;
enum RASLCPO_PFC    = 0x00000001;
enum RASLCPO_ACFC   = 0x00000002;
enum RASLCPO_SSHF   = 0x00000004;
enum RASLCPO_DES_56 = 0x00000008;
enum RASLCPO_3_DES  = 0x00000010;

enum RASCCPCA_MPPC = 0x00000006;
enum RASCCPCA_STAC = 0x00000005;

enum RASCCPO_Compression      = 0x00000001;
enum RASCCPO_HistoryLess      = 0x00000002;
enum RASCCPO_Encryption56bit  = 0x00000010;
enum RASCCPO_Encryption40bit  = 0x00000020;
enum RASCCPO_Encryption128bit = 0x00000040;

enum RASEO_RequireEAP          = 0x00020000;
enum RASEO_RequirePAP          = 0x00040000;
enum RASEO_RequireSPAP         = 0x00080000;
enum RASEO_Custom              = 0x00100000;
enum RASEO_PreviewPhoneNumber  = 0x00200000;
enum RASEO_SharedPhoneNumbers  = 0x00800000;
enum RASEO_PreviewUserPw       = 0x01000000;
enum RASEO_PreviewDomain       = 0x02000000;
enum RASEO_ShowDialingProgress = 0x04000000;
enum RASEO_RequireCHAP         = 0x08000000;
enum RASEO_RequireMsCHAP       = 0x10000000;
enum RASEO_RequireMsCHAP2      = 0x20000000;
enum RASEO_RequireW95MSCHAP    = 0x40000000;
enum RASEO_CustomScript        = 0x80000000;

enum RASIPO_VJ = 0x00000001;
enum RCD_SingleUser = 0;
enum RCD_AllUsers = 0x00000001;
enum RCD_Eap = 0x00000002;
enum RASEAPF_NonInteractive = 0x00000002;
enum RASEAPF_Logon = 0x00000004;
enum RASEAPF_Preview = 0x00000008;
enum ET_40Bit = 1;
enum ET_128Bit = 2;
enum ET_None = 0;
enum ET_Require = 1;
enum ET_RequireMax = 2;
enum ET_Optional = 3;
//}

enum RASCS_PAUSED = 0x1000;
enum RASCS_DONE = 0x2000;
enum RASCONNSTATE {
    RASCS_OpenPort = 0,
    RASCS_PortOpened,
    RASCS_ConnectDevice,
    RASCS_DeviceConnected,
    RASCS_AllDevicesConnected,
    RASCS_Authenticate,
    RASCS_AuthNotify,
    RASCS_AuthRetry,
    RASCS_AuthCallback,
    RASCS_AuthChangePassword,
    RASCS_AuthProject,
    RASCS_AuthLinkSpeed,
    RASCS_AuthAck,
    RASCS_ReAuthenticate,
    RASCS_Authenticated,
    RASCS_PrepareForCallback,
    RASCS_WaitForModemReset,
    RASCS_WaitForCallback,
    RASCS_Projected,
    RASCS_StartAuthentication,
    RASCS_CallbackComplete,
    RASCS_LogonNetwork,
    RASCS_SubEntryConnected,
    RASCS_SubEntryDisconnected,
    RASCS_Interactive = RASCS_PAUSED,
    RASCS_RetryAuthentication,
    RASCS_CallbackSetByCaller,
    RASCS_PasswordExpired,
//  static if (_WIN32_WINNT >= 0x500) {
        RASCS_InvokeEapUI,
//  }
    RASCS_Connected = RASCS_DONE,
    RASCS_Disconnected
}
alias RASCONNSTATE* LPRASCONNSTATE;

enum RASPROJECTION {
    RASP_Amb =      0x10000,
    RASP_PppNbf =   0x803F,
    RASP_PppIpx =   0x802B,
    RASP_PppIp =    0x8021,
//  static if (_WIN32_WINNT >= 0x500) {
        RASP_PppCcp =   0x80FD,
//  }
    RASP_PppLcp =   0xC021,
    RASP_Slip =     0x20000
}
alias RASPROJECTION* LPRASPROJECTION;

alias TypeDef!(HANDLE) HRASCONN;
alias HRASCONN* LPHRASCONN;

struct RASCONNW {
align(4):
    DWORD dwSize;
    HRASCONN hrasconn;
    align {
    WCHAR[RAS_MaxEntryName + 1] szEntryName = 0;
    WCHAR[RAS_MaxDeviceType + 1] szDeviceType = 0;
    WCHAR[RAS_MaxDeviceName + 1] szDeviceName = 0;
    }
    //static if (_WIN32_WINNT >= 0x401) {
        WCHAR[MAX_PATH] szPhonebook = 0;
        DWORD dwSubEntry;
    //}
    //static if (_WIN32_WINNT >= 0x500) {
        GUID guidEntry;
    //}
    static if (_WIN32_WINNT >= 0x501) {
        DWORD dwFlags;
        LUID luid;
    }
}
alias RASCONNW* LPRASCONNW;

struct RASCONNA {
align(4):
    DWORD dwSize;
    HRASCONN hrasconn;
    align {
    CHAR[RAS_MaxEntryName + 1] szEntryName = 0;
    CHAR[RAS_MaxDeviceType + 1] szDeviceType = 0;
    CHAR[RAS_MaxDeviceName + 1] szDeviceName = 0;
    }
    //static if (_WIN32_WINNT >= 0x401) {
        CHAR[MAX_PATH] szPhonebook = 0;
        DWORD dwSubEntry;
    //}
    //static if (_WIN32_WINNT >= 0x500) {
        GUID guidEntry;
    //}
    static if (_WIN32_WINNT >= 0x501) {
        DWORD dwFlags;
        LUID luid;
    }
}
alias RASCONNA* LPRASCONNA;

struct RASCONNSTATUSW {
    DWORD dwSize;
    RASCONNSTATE rasconnstate;
    DWORD dwError;
    WCHAR[RAS_MaxDeviceType + 1] szDeviceType = 0;
    WCHAR[RAS_MaxDeviceName + 1] szDeviceName = 0;
    static if (_WIN32_WINNT >= 0x401) {
        WCHAR[RAS_MaxPhoneNumber + 1] szPhoneNumber = 0;
    }
}
alias RASCONNSTATUSW* LPRASCONNSTATUSW;

struct RASCONNSTATUSA {
    DWORD dwSize;
    RASCONNSTATE rasconnstate;
    DWORD dwError;
    CHAR[RAS_MaxDeviceType + 1] szDeviceType = 0;
    CHAR[RAS_MaxDeviceName + 1] szDeviceName = 0;
    static if (_WIN32_WINNT >= 0x401) {
        CHAR[RAS_MaxPhoneNumber + 1] szPhoneNumber = 0;
    }
}
alias RASCONNSTATUSA* LPRASCONNSTATUSA;

struct RASDIALPARAMSW {
align(4):
    DWORD dwSize;
align {
    WCHAR[RAS_MaxEntryName + 1] szEntryName = 0;
    WCHAR[RAS_MaxPhoneNumber + 1] szPhoneNumber = 0;
    WCHAR[RAS_MaxCallbackNumber + 1] szCallbackNumber = 0;
    WCHAR[UNLEN + 1] szUserName = 0;
    WCHAR[PWLEN + 1] szPassword = 0;
    WCHAR[DNLEN + 1] szDomain = 0;
}
    static if (_WIN32_WINNT >= 0x401) {
        DWORD dwSubEntry;
        ULONG_PTR dwCallbackId;
    }
}
alias RASDIALPARAMSW* LPRASDIALPARAMSW;

struct RASDIALPARAMSA{
align(4):
    DWORD dwSize;
align {
    CHAR[RAS_MaxEntryName + 1] szEntryName = 0;
    CHAR[RAS_MaxPhoneNumber + 1] szPhoneNumber = 0;
    CHAR[RAS_MaxCallbackNumber + 1] szCallbackNumber = 0;
    CHAR[UNLEN + 1] szUserName = 0;
    CHAR[PWLEN + 1] szPassword = 0;
    CHAR[DNLEN + 1] szDomain = 0;
}
    static if (_WIN32_WINNT >= 0x401) {
        DWORD dwSubEntry;
        ULONG_PTR dwCallbackId;
    }
}
alias RASDIALPARAMSA* LPRASDIALPARAMSA;

//static if (_WIN32_WINNT >= 0x500) {
    struct RASEAPINFO {
    align(4):
        DWORD dwSizeofEapInfo;
        BYTE *pbEapInfo;
    }
//}

struct RASDIALEXTENSIONS {
align(4):
    DWORD dwSize;
    DWORD dwfOptions;
    HWND hwndParent;
    ULONG_PTR reserved;
    //static if (_WIN32_WINNT >= 0x500) {
        ULONG_PTR reserved1;
        RASEAPINFO RasEapInfo;
    //}
}
alias RASDIALEXTENSIONS* LPRASDIALEXTENSIONS;

struct RASENTRYNAMEW {
    DWORD dwSize;
    WCHAR[RAS_MaxEntryName + 1] szEntryName = 0;
    //static if (_WIN32_WINNT >= 0x500) {
        DWORD dwFlags;
        WCHAR[MAX_PATH + 1] szPhonebookPath = 0;
    //}
}
alias RASENTRYNAMEW* LPRASENTRYNAMEW;

struct RASENTRYNAMEA{
    DWORD dwSize;
    CHAR[RAS_MaxEntryName + 1] szEntryName = 0;
    //static if (_WIN32_WINNT >= 0x500) {
        DWORD dwFlags;
        CHAR[MAX_PATH + 1] szPhonebookPath = 0;
    //}
}
alias RASENTRYNAMEA* LPRASENTRYNAMEA;

struct RASAMBW{
    DWORD dwSize;
    DWORD dwError;
    WCHAR[NETBIOS_NAME_LEN + 1] szNetBiosError = 0;
    BYTE bLana;
}
alias RASAMBW* LPRASAMBW;

struct RASAMBA{
    DWORD dwSize;
    DWORD dwError;
    CHAR[NETBIOS_NAME_LEN + 1] szNetBiosError = 0;
    BYTE bLana;
}
alias RASAMBA* LPRASAMBA;

struct RASPPPNBFW{
    DWORD dwSize;
    DWORD dwError;
    DWORD dwNetBiosError;
    WCHAR[NETBIOS_NAME_LEN + 1] szNetBiosError = 0;
    WCHAR[NETBIOS_NAME_LEN + 1] szWorkstationName = 0;
    BYTE bLana;
}
alias RASPPPNBFW* LPRASPPPNBFW;

struct RASPPPNBFA{
    DWORD dwSize;
    DWORD dwError;
    DWORD dwNetBiosError;
    CHAR[NETBIOS_NAME_LEN + 1] szNetBiosError = 0;
    CHAR[NETBIOS_NAME_LEN + 1] szWorkstationName = 0;
    BYTE bLana;
}
alias RASPPPNBFA* LPRASPPPNBFA;

struct RASPPPIPXW {
    DWORD dwSize;
    DWORD dwError;
    WCHAR[RAS_MaxIpxAddress + 1] szIpxAddress = 0;
}
alias RASPPPIPXW* LPRASPPPIPXW;

struct RASPPPIPXA {
    DWORD dwSize;
    DWORD dwError;
    CHAR[RAS_MaxIpxAddress + 1] szIpxAddress = 0;
}
alias RASPPPIPXA* LPRASPPPIPXA;

struct RASPPPIPW{
    DWORD dwSize;
    DWORD dwError;
    WCHAR[RAS_MaxIpAddress + 1] szIpAddress = 0;
    //#ifndef WINNT35COMPATIBLE
    WCHAR[RAS_MaxIpAddress + 1] szServerIpAddress = 0;
    //#endif
    //static if (_WIN32_WINNT >= 0x500) {
        DWORD dwOptions;
        DWORD dwServerOptions;
    //}
}
alias RASPPPIPW* LPRASPPPIPW;

struct RASPPPIPA{
    DWORD dwSize;
    DWORD dwError;
    CHAR[RAS_MaxIpAddress + 1] szIpAddress = 0;
    //#ifndef WINNT35COMPATIBLE
    CHAR[RAS_MaxIpAddress + 1] szServerIpAddress = 0;
    //#endif
    //static if (_WIN32_WINNT >= 0x500) {
        DWORD dwOptions;
        DWORD dwServerOptions;
    //}
}
alias RASPPPIPA* LPRASPPPIPA;

struct RASPPPLCPW{
    DWORD dwSize;
    BOOL fBundled;
    //static if (_WIN32_WINNT >= 0x500) {
        DWORD dwError;
        DWORD dwAuthenticationProtocol;
        DWORD dwAuthenticationData;
        DWORD dwEapTypeId;
        DWORD dwServerAuthenticationProtocol;
        DWORD dwServerAuthenticationData;
        DWORD dwServerEapTypeId;
        BOOL fMultilink;
        DWORD dwTerminateReason;
        DWORD dwServerTerminateReason;
        WCHAR[RAS_MaxReplyMessage] szReplyMessage = 0;
        DWORD dwOptions;
        DWORD dwServerOptions;
    //}
}
alias RASPPPLCPW* LPRASPPPLCPW;

struct RASPPPLCPA{
    DWORD dwSize;
    BOOL fBundled;
    //static if (_WIN32_WINNT >= 0x500) {
        DWORD dwError;
        DWORD dwAuthenticationProtocol;
        DWORD dwAuthenticationData;
        DWORD dwEapTypeId;
        DWORD dwServerAuthenticationProtocol;
        DWORD dwServerAuthenticationData;
        DWORD dwServerEapTypeId;
        BOOL fMultilink;
        DWORD dwTerminateReason;
        DWORD dwServerTerminateReason;
        CHAR[RAS_MaxReplyMessage] szReplyMessage = 0;
        DWORD dwOptions;
        DWORD dwServerOptions;
    //}
}
alias RASPPPLCPA* LPRASPPPLCPA;

struct RASSLIPW{
    DWORD dwSize;
    DWORD dwError;
    WCHAR[RAS_MaxIpAddress + 1] szIpAddress = 0;
}
alias RASSLIPW* LPRASSLIPW;

struct RASSLIPA{
    DWORD dwSize;
    DWORD dwError;
    CHAR[RAS_MaxIpAddress + 1] szIpAddress = 0;
}
alias RASSLIPA* LPRASSLIPA;

struct RASDEVINFOW{
    DWORD dwSize;
    WCHAR[RAS_MaxDeviceType + 1] szDeviceType = 0;
    WCHAR[RAS_MaxDeviceName + 1] szDeviceName = 0;
}
alias RASDEVINFOW* LPRASDEVINFOW;

struct RASDEVINFOA{
    DWORD dwSize;
    CHAR[RAS_MaxDeviceType + 1] szDeviceType = 0;
    CHAR[RAS_MaxDeviceName + 1] szDeviceName = 0;
}
alias RASDEVINFOA* LPRASDEVINFOA;

struct RASCTRYINFO {
    DWORD dwSize;
    DWORD dwCountryID;
    DWORD dwNextCountryID;
    DWORD dwCountryCode;
    DWORD dwCountryNameOffset;
}
alias RASCTRYINFO* LPRASCTRYINFO;
alias RASCTRYINFO  RASCTRYINFOW, RASCTRYINFOA;
alias RASCTRYINFOW* LPRASCTRYINFOW;
alias RASCTRYINFOA* LPRASCTRYINFOA;

struct RASIPADDR {
    BYTE a;
    BYTE b;
    BYTE c;
    BYTE d;
}

struct RASENTRYW {
    DWORD dwSize;
    DWORD dwfOptions;
    DWORD dwCountryID;
    DWORD dwCountryCode;
    WCHAR[RAS_MaxAreaCode + 1] szAreaCode = 0;
    WCHAR[RAS_MaxPhoneNumber + 1] szLocalPhoneNumber = 0;
    DWORD dwAlternateOffset;
    RASIPADDR ipaddr;
    RASIPADDR ipaddrDns;
    RASIPADDR ipaddrDnsAlt;
    RASIPADDR ipaddrWins;
    RASIPADDR ipaddrWinsAlt;
    DWORD dwFrameSize;
    DWORD dwfNetProtocols;
    DWORD dwFramingProtocol;
    WCHAR[MAX_PATH] szScript = 0;
    WCHAR[MAX_PATH] szAutodialDll = 0;
    WCHAR[MAX_PATH] szAutodialFunc = 0;
    WCHAR[RAS_MaxDeviceType + 1] szDeviceType = 0;
    WCHAR[RAS_MaxDeviceName + 1] szDeviceName = 0;
    WCHAR[RAS_MaxPadType + 1] szX25PadType = 0;
    WCHAR[RAS_MaxX25Address + 1] szX25Address = 0;
    WCHAR[RAS_MaxFacilities + 1] szX25Facilities = 0;
    WCHAR[RAS_MaxUserData + 1] szX25UserData = 0;
    DWORD dwChannels;
    DWORD dwReserved1;
    DWORD dwReserved2;
    //static if (_WIN32_WINNT >= 0x401) {
        DWORD dwSubEntries;
        DWORD dwDialMode;
        DWORD dwDialExtraPercent;
        DWORD dwDialExtraSampleSeconds;
        DWORD dwHangUpExtraPercent;
        DWORD dwHangUpExtraSampleSeconds;
        DWORD dwIdleDisconnectSeconds;
    //}
    //static if (_WIN32_WINNT >= 0x500) {
        DWORD dwType;
        DWORD dwEncryptionType;
        DWORD dwCustomAuthKey;
        GUID guidId;
        WCHAR[MAX_PATH] szCustomDialDll = 0;
        DWORD dwVpnStrategy;
    //}
}
alias RASENTRYW* LPRASENTRYW;

struct RASENTRYA {
    DWORD dwSize;
    DWORD dwfOptions;
    DWORD dwCountryID;
    DWORD dwCountryCode;
    CHAR[RAS_MaxAreaCode + 1] szAreaCode = 0;
    CHAR[RAS_MaxPhoneNumber + 1] szLocalPhoneNumber = 0;
    DWORD dwAlternateOffset;
    RASIPADDR ipaddr;
    RASIPADDR ipaddrDns;
    RASIPADDR ipaddrDnsAlt;
    RASIPADDR ipaddrWins;
    RASIPADDR ipaddrWinsAlt;
    DWORD dwFrameSize;
    DWORD dwfNetProtocols;
    DWORD dwFramingProtocol;
    CHAR[MAX_PATH] szScript = 0;
    CHAR[MAX_PATH] szAutodialDll = 0;
    CHAR[MAX_PATH] szAutodialFunc = 0;
    CHAR[RAS_MaxDeviceType + 1] szDeviceType = 0;
    CHAR[RAS_MaxDeviceName + 1] szDeviceName = 0;
    CHAR[RAS_MaxPadType + 1] szX25PadType = 0;
    CHAR[RAS_MaxX25Address + 1] szX25Address = 0;
    CHAR[RAS_MaxFacilities + 1] szX25Facilities = 0;
    CHAR[RAS_MaxUserData + 1] szX25UserData = 0;
    DWORD dwChannels;
    DWORD dwReserved1;
    DWORD dwReserved2;
    //static if (_WIN32_WINNT >= 0x401) {
        DWORD dwSubEntries;
        DWORD dwDialMode;
        DWORD dwDialExtraPercent;
        DWORD dwDialExtraSampleSeconds;
        DWORD dwHangUpExtraPercent;
        DWORD dwHangUpExtraSampleSeconds;
        DWORD dwIdleDisconnectSeconds;
    //}
    //static if (_WIN32_WINNT >= 0x500) {
        DWORD dwType;
        DWORD dwEncryptionType;
        DWORD dwCustomAuthKey;
        GUID guidId;
        CHAR[MAX_PATH] szCustomDialDll = 0;
        DWORD dwVpnStrategy;
    //}
}
alias RASENTRYA* LPRASENTRYA;


//static if (_WIN32_WINNT >= 0x401) {
    struct RASADPARAMS {
    align(4):
        DWORD dwSize;
        HWND hwndOwner;
        DWORD dwFlags;
        LONG xDlg;
        LONG yDlg;
    }
    alias RASADPARAMS* LPRASADPARAMS;

    struct RASSUBENTRYW{
        DWORD dwSize;
        DWORD dwfFlags;
        WCHAR[RAS_MaxDeviceType + 1] szDeviceType = 0;
        WCHAR[RAS_MaxDeviceName + 1] szDeviceName = 0;
        WCHAR[RAS_MaxPhoneNumber + 1] szLocalPhoneNumber = 0;
        DWORD dwAlternateOffset;
    }
    alias RASSUBENTRYW* LPRASSUBENTRYW;

    struct RASSUBENTRYA{
        DWORD dwSize;
        DWORD dwfFlags;
        CHAR[RAS_MaxDeviceType + 1] szDeviceType = 0;
        CHAR[RAS_MaxDeviceName + 1] szDeviceName = 0;
        CHAR[RAS_MaxPhoneNumber + 1] szLocalPhoneNumber = 0;
        DWORD dwAlternateOffset;
    }
    alias RASSUBENTRYA* LPRASSUBENTRYA;

    struct RASCREDENTIALSW{
        DWORD dwSize;
        DWORD dwMask;
        WCHAR[UNLEN + 1] szUserName = 0;
        WCHAR[PWLEN + 1] szPassword = 0;
        WCHAR[DNLEN + 1] szDomain = 0;
    }
    alias RASCREDENTIALSW* LPRASCREDENTIALSW;

    struct RASCREDENTIALSA{
        DWORD dwSize;
        DWORD dwMask;
        CHAR[UNLEN + 1] szUserName = 0;
        CHAR[PWLEN + 1] szPassword = 0;
        CHAR[DNLEN + 1] szDomain = 0;
    }
    alias RASCREDENTIALSA* LPRASCREDENTIALSA;

    struct RASAUTODIALENTRYW{
        DWORD dwSize;
        DWORD dwFlags;
        DWORD dwDialingLocation;
        WCHAR[RAS_MaxEntryName + 1] szEntry = 0;
    }
    alias RASAUTODIALENTRYW* LPRASAUTODIALENTRYW;

    struct RASAUTODIALENTRYA{
        DWORD dwSize;
        DWORD dwFlags;
        DWORD dwDialingLocation;
        CHAR[RAS_MaxEntryName + 1] szEntry = 0;
    }
    alias RASAUTODIALENTRYA* LPRASAUTODIALENTRYA;
//}

//static if (_WIN32_WINNT >= 0x500) {
    struct RASPPPCCP{
        DWORD dwSize;
        DWORD dwError;
        DWORD dwCompressionAlgorithm;
        DWORD dwOptions;
        DWORD dwServerCompressionAlgorithm;
        DWORD dwServerOptions;
    }
    alias RASPPPCCP* LPRASPPPCCP;

    struct RASEAPUSERIDENTITYW{
        WCHAR[UNLEN + 1] szUserName = 0;
        DWORD dwSizeofEapInfo;
        BYTE[1] pbEapInfo;
    }
    alias RASEAPUSERIDENTITYW* LPRASEAPUSERIDENTITYW;

    struct RASEAPUSERIDENTITYA{
        CHAR[UNLEN + 1] szUserName = 0;
        DWORD dwSizeofEapInfo;
        BYTE[1] pbEapInfo;
    }
    alias RASEAPUSERIDENTITYA* LPRASEAPUSERIDENTITYA;

    struct RAS_STATS{
        DWORD dwSize;
        DWORD dwBytesXmited;
        DWORD dwBytesRcved;
        DWORD dwFramesXmited;
        DWORD dwFramesRcved;
        DWORD dwCrcErr;
        DWORD dwTimeoutErr;
        DWORD dwAlignmentErr;
        DWORD dwHardwareOverrunErr;
        DWORD dwFramingErr;
        DWORD dwBufferOverrunErr;
        DWORD dwCompressionRatioIn;
        DWORD dwCompressionRatioOut;
        DWORD dwBps;
        DWORD dwConnectDuration;
    }
    alias RAS_STATS* PRAS_STATS;
//}


/* UNICODE typedefs for structures*/
version (Unicode) {
    alias RASCONNW RASCONN;
    alias RASENTRYW RASENTRY;
    alias RASCONNSTATUSW RASCONNSTATUS;
    alias RASDIALPARAMSW RASDIALPARAMS;
    alias RASAMBW RASAMB;
    alias RASPPPNBFW RASPPPNBF;
    alias RASPPPIPXW RASPPPIPX;
    alias RASPPPIPW RASPPPIP;
    alias RASPPPLCPW RASPPPLCP;
    alias RASSLIPW RASSLIP;
    alias RASDEVINFOW RASDEVINFO;
    alias RASENTRYNAMEW RASENTRYNAME;

    //static if (_WIN32_WINNT >= 0x401) {
        alias RASSUBENTRYW RASSUBENTRY;
        alias RASCREDENTIALSW RASCREDENTIALS;
        alias RASAUTODIALENTRYW RASAUTODIALENTRY;
    //}

    //static if (_WIN32_WINNT >= 0x500) {
        alias RASEAPUSERIDENTITYW RASEAPUSERIDENTITY;
    //}

} else { // ! defined UNICODE

    alias RASCONNA RASCONN;
    alias RASENTRYA  RASENTRY;
    alias RASCONNSTATUSA RASCONNSTATUS;
    alias RASDIALPARAMSA RASDIALPARAMS;
    alias RASAMBA RASAMB;
    alias RASPPPNBFA RASPPPNBF;
    alias RASPPPIPXA RASPPPIPX;
    alias RASPPPIPA RASPPPIP;
    alias RASPPPLCPA RASPPPLCP;
    alias RASSLIPA RASSLIP;
    alias RASDEVINFOA  RASDEVINFO;
    alias RASENTRYNAMEA RASENTRYNAME;

    //static if (_WIN32_WINNT >= 0x401) {
        alias RASSUBENTRYA RASSUBENTRY;
        alias RASCREDENTIALSA RASCREDENTIALS;
        alias RASAUTODIALENTRYA RASAUTODIALENTRY;
    //}
    //static if (_WIN32_WINNT >= 0x500) {
        alias RASEAPUSERIDENTITYA RASEAPUSERIDENTITY;
    //}
}// ! UNICODE


alias RASCONN* LPRASCONN;
alias RASENTRY* LPRASENTRY;
alias RASCONNSTATUS* LPRASCONNSTATUS;
alias RASDIALPARAMS* LPRASDIALPARAMS;
alias RASAMB* LPRASAM;
alias RASPPPNBF* LPRASPPPNBF;
alias RASPPPIPX* LPRASPPPIPX;
alias RASPPPIP* LPRASPPPIP;
alias RASPPPLCP* LPRASPPPLCP;
alias RASSLIP* LPRASSLIP;
alias RASDEVINFO* LPRASDEVINFO;
alias RASENTRYNAME* LPRASENTRYNAME;

//static if (_WIN32_WINNT >= 0x401) {
    alias RASSUBENTRY* LPRASSUBENTRY;
    alias RASCREDENTIALS* LPRASCREDENTIALS;
    alias RASAUTODIALENTRY* LPRASAUTODIALENTRY;
//}
//static if (_WIN32_WINNT >= 0x500) {
    alias RASEAPUSERIDENTITY* LPRASEAPUSERIDENTITY;
//}

/* Callback prototypes */
extern (Windows) { /* WINAPI */
    deprecated {
        alias BOOL function (HWND, LPSTR, DWORD, LPDWORD) ORASADFUNC;
    }

    alias void function (UINT, RASCONNSTATE, DWORD) RASDIALFUNC;
    alias void function(HRASCONN, UINT, RASCONNSTATE, DWORD, DWORD) RASDIALFUNC1;
    alias DWORD function (ULONG_PTR, DWORD, HRASCONN, UINT,
    RASCONNSTATE, DWORD, DWORD) RASDIALFUNC2;

    /* External functions */
    DWORD RasDialA(LPRASDIALEXTENSIONS, LPCSTR, LPRASDIALPARAMSA, DWORD, LPVOID, LPHRASCONN);
    DWORD RasDialW(LPRASDIALEXTENSIONS, LPCWSTR, LPRASDIALPARAMSW, DWORD, LPVOID, LPHRASCONN);
    DWORD RasEnumConnectionsA(LPRASCONNA, LPDWORD, LPDWORD);
    DWORD RasEnumConnectionsW(LPRASCONNW, LPDWORD, LPDWORD);
    DWORD RasEnumEntriesA(LPCSTR, LPCSTR, LPRASENTRYNAMEA, LPDWORD, LPDWORD);
    DWORD RasEnumEntriesW(LPCWSTR, LPCWSTR, LPRASENTRYNAMEW, LPDWORD, LPDWORD);
    DWORD RasGetConnectStatusA(HRASCONN, LPRASCONNSTATUSA);
    DWORD RasGetConnectStatusW(HRASCONN, LPRASCONNSTATUSW);
    DWORD RasGetErrorStringA(UINT, LPSTR, DWORD);
    DWORD RasGetErrorStringW(UINT, LPWSTR, DWORD);
    DWORD RasHangUpA(HRASCONN);
    DWORD RasHangUpW(HRASCONN);
    DWORD RasGetProjectionInfoA(HRASCONN, RASPROJECTION, LPVOID, LPDWORD);
    DWORD RasGetProjectionInfoW(HRASCONN, RASPROJECTION, LPVOID, LPDWORD);
    DWORD RasCreatePhonebookEntryA(HWND, LPCSTR);
    DWORD RasCreatePhonebookEntryW(HWND, LPCWSTR);
    DWORD RasEditPhonebookEntryA(HWND, LPCSTR, LPCSTR);
    DWORD RasEditPhonebookEntryW(HWND, LPCWSTR, LPCWSTR);
    DWORD RasSetEntryDialParamsA(LPCSTR, LPRASDIALPARAMSA, BOOL);
    DWORD RasSetEntryDialParamsW(LPCWSTR, LPRASDIALPARAMSW, BOOL);
    DWORD RasGetEntryDialParamsA(LPCSTR, LPRASDIALPARAMSA, LPBOOL);
    DWORD RasGetEntryDialParamsW(LPCWSTR, LPRASDIALPARAMSW, LPBOOL);
    DWORD RasEnumDevicesA(LPRASDEVINFOA, LPDWORD, LPDWORD);
    DWORD RasEnumDevicesW(LPRASDEVINFOW, LPDWORD, LPDWORD);
    DWORD RasGetCountryInfoA(LPRASCTRYINFOA, LPDWORD);
    DWORD RasGetCountryInfoW(LPRASCTRYINFOW, LPDWORD);
    DWORD RasGetEntryPropertiesA(LPCSTR, LPCSTR, LPRASENTRYA, LPDWORD, LPBYTE, LPDWORD);
    DWORD RasGetEntryPropertiesW(LPCWSTR, LPCWSTR, LPRASENTRYW, LPDWORD, LPBYTE, LPDWORD);
    DWORD RasSetEntryPropertiesA(LPCSTR, LPCSTR, LPRASENTRYA, DWORD, LPBYTE, DWORD);
    DWORD RasSetEntryPropertiesW(LPCWSTR, LPCWSTR, LPRASENTRYW, DWORD, LPBYTE, DWORD);
    DWORD RasRenameEntryA(LPCSTR, LPCSTR, LPCSTR);
    DWORD RasRenameEntryW(LPCWSTR, LPCWSTR, LPCWSTR);
    DWORD RasDeleteEntryA(LPCSTR, LPCSTR);
    DWORD RasDeleteEntryW(LPCWSTR, LPCWSTR);
    DWORD RasValidateEntryNameA(LPCSTR, LPCSTR);
    DWORD RasValidateEntryNameW(LPCWSTR, LPCWSTR);

//static if (_WIN32_WINNT >= 0x401) {
    alias BOOL function(LPSTR, LPSTR, LPRASADPARAMS, LPDWORD) RASADFUNCA;
    alias BOOL function(LPWSTR, LPWSTR, LPRASADPARAMS, LPDWORD) RASADFUNCW;

    DWORD RasGetSubEntryHandleA(HRASCONN, DWORD, LPHRASCONN);
    DWORD RasGetSubEntryHandleW(HRASCONN, DWORD, LPHRASCONN);
    DWORD RasGetCredentialsA(LPCSTR, LPCSTR, LPRASCREDENTIALSA);
    DWORD RasGetCredentialsW(LPCWSTR, LPCWSTR, LPRASCREDENTIALSW);
    DWORD RasSetCredentialsA(LPCSTR, LPCSTR, LPRASCREDENTIALSA, BOOL);
    DWORD RasSetCredentialsW(LPCWSTR, LPCWSTR, LPRASCREDENTIALSW, BOOL);
    DWORD RasConnectionNotificationA(HRASCONN, HANDLE, DWORD);
    DWORD RasConnectionNotificationW(HRASCONN, HANDLE, DWORD);
    DWORD RasGetSubEntryPropertiesA(LPCSTR, LPCSTR, DWORD, LPRASSUBENTRYA, LPDWORD, LPBYTE, LPDWORD);
    DWORD RasGetSubEntryPropertiesW(LPCWSTR, LPCWSTR, DWORD, LPRASSUBENTRYW, LPDWORD, LPBYTE, LPDWORD);
    DWORD RasSetSubEntryPropertiesA(LPCSTR, LPCSTR, DWORD, LPRASSUBENTRYA, DWORD, LPBYTE, DWORD);
    DWORD RasSetSubEntryPropertiesW(LPCWSTR, LPCWSTR, DWORD, LPRASSUBENTRYW, DWORD, LPBYTE, DWORD);
    DWORD RasGetAutodialAddressA(LPCSTR, LPDWORD, LPRASAUTODIALENTRYA, LPDWORD, LPDWORD);
    DWORD RasGetAutodialAddressW(LPCWSTR, LPDWORD, LPRASAUTODIALENTRYW, LPDWORD, LPDWORD);
    DWORD RasSetAutodialAddressA(LPCSTR, DWORD, LPRASAUTODIALENTRYA, DWORD, DWORD);
    DWORD RasSetAutodialAddressW(LPCWSTR, DWORD, LPRASAUTODIALENTRYW, DWORD, DWORD);
    DWORD RasEnumAutodialAddressesA(LPSTR*, LPDWORD, LPDWORD);
    DWORD RasEnumAutodialAddressesW(LPWSTR*, LPDWORD, LPDWORD);
    DWORD RasGetAutodialEnableA(DWORD, LPBOOL);
    DWORD RasGetAutodialEnableW(DWORD, LPBOOL);
    DWORD RasSetAutodialEnableA(DWORD, BOOL);
    DWORD RasSetAutodialEnableW(DWORD, BOOL);
    DWORD RasGetAutodialParamA(DWORD, LPVOID, LPDWORD);
    DWORD RasGetAutodialParamW(DWORD, LPVOID, LPDWORD);
    DWORD RasSetAutodialParamA(DWORD, LPVOID, DWORD);
    DWORD RasSetAutodialParamW(DWORD, LPVOID, DWORD);
//}

static if (_WIN32_WINNT >= 0x500) {
    alias DWORD function(HRASCONN) RasCustomHangUpFn;
    alias DWORD function(LPCTSTR, LPCTSTR, DWORD) RasCustomDeleteEntryNotifyFn;
    alias DWORD function(HINSTANCE, LPRASDIALEXTENSIONS, LPCTSTR, LPRASDIALPARAMS, DWORD, LPVOID,
                         LPHRASCONN, DWORD) RasCustomDialFn;

    DWORD RasInvokeEapUI(HRASCONN, DWORD, LPRASDIALEXTENSIONS, HWND);
    DWORD RasGetLinkStatistics(HRASCONN, DWORD, RAS_STATS*);
    DWORD RasGetConnectionStatistics(HRASCONN, RAS_STATS*);
    DWORD RasClearLinkStatistics(HRASCONN, DWORD);
    DWORD RasClearConnectionStatistics(HRASCONN);
    DWORD RasGetEapUserDataA(HANDLE, LPCSTR, LPCSTR, BYTE*, DWORD*);
    DWORD RasGetEapUserDataW(HANDLE, LPCWSTR, LPCWSTR, BYTE*, DWORD*);
    DWORD RasSetEapUserDataA(HANDLE, LPCSTR, LPCSTR, BYTE*, DWORD);
    DWORD RasSetEapUserDataW(HANDLE, LPCWSTR, LPCWSTR, BYTE*, DWORD);
    DWORD RasGetCustomAuthDataA(LPCSTR, LPCSTR, BYTE*, DWORD*);
    DWORD RasGetCustomAuthDataW(LPCWSTR, LPCWSTR, BYTE*, DWORD*);
    DWORD RasSetCustomAuthDataA(LPCSTR, LPCSTR, BYTE*, DWORD);
    DWORD RasSetCustomAuthDataW(LPCWSTR, LPCWSTR, BYTE*, DWORD);
    DWORD RasGetEapUserIdentityW(LPCWSTR, LPCWSTR, DWORD, HWND, LPRASEAPUSERIDENTITYW*);
    DWORD RasGetEapUserIdentityA(LPCSTR, LPCSTR, DWORD, HWND, LPRASEAPUSERIDENTITYA*);
    void RasFreeEapUserIdentityW(LPRASEAPUSERIDENTITYW);
    void RasFreeEapUserIdentityA(LPRASEAPUSERIDENTITYA);
}
} // extern (Windows)


/* UNICODE defines for functions */
version (Unicode) {
    alias RasDialW RasDial;
    alias RasEnumConnectionsW RasEnumConnections;
    alias RasEnumEntriesW RasEnumEntries;
    alias RasGetConnectStatusW RasGetConnectStatus;
    alias RasGetErrorStringW RasGetErrorString;
    alias RasHangUpW RasHangUp;
    alias RasGetProjectionInfoW RasGetProjectionInfo;
    alias RasCreatePhonebookEntryW RasCreatePhonebookEntry;
    alias RasEditPhonebookEntryW RasEditPhonebookEntry;
    alias RasSetEntryDialParamsW RasSetEntryDialParams;
    alias RasGetEntryDialParamsW RasGetEntryDialParams;
    alias RasEnumDevicesW RasEnumDevices;
    alias RasGetCountryInfoW RasGetCountryInfo;
    alias RasGetEntryPropertiesW RasGetEntryProperties;
    alias RasSetEntryPropertiesW RasSetEntryProperties;
    alias RasRenameEntryW RasRenameEntry;
    alias RasDeleteEntryW RasDeleteEntry;
    alias RasValidateEntryNameW RasValidateEntryName;

    //static if (_WIN32_WINNT >= 0x401) {
        alias RASADFUNCW RASADFUNC;
        alias RasGetSubEntryHandleW RasGetSubEntryHandle;
        alias RasConnectionNotificationW RasConnectionNotification;
        alias RasGetSubEntryPropertiesW RasGetSubEntryProperties;
        alias RasSetSubEntryPropertiesW RasSetSubEntryProperties;
        alias RasGetCredentialsW RasGetCredentials;
        alias RasSetCredentialsW RasSetCredentials;
        alias RasGetAutodialAddressW RasGetAutodialAddress;
        alias RasSetAutodialAddressW RasSetAutodialAddress;
        alias RasEnumAutodialAddressesW RasEnumAutodialAddresses;
        alias RasGetAutodialEnableW RasGetAutodialEnable;
        alias RasSetAutodialEnableW RasSetAutodialEnable;
        alias RasGetAutodialParamW RasGetAutodialParam;
        alias RasSetAutodialParamW RasSetAutodialParam;
    //}

    //static if (_WIN32_WINNT >= 0x500) {
        alias RasGetEapUserDataW RasGetEapUserData;
        alias RasSetEapUserDataW RasSetEapUserData;
        alias RasGetCustomAuthDataW RasGetCustomAuthData;
        alias RasSetCustomAuthDataW RasSetCustomAuthData;
        alias RasGetEapUserIdentityW RasGetEapUserIdentity;
        alias RasFreeEapUserIdentityW RasFreeEapUserIdentity;
    //}

} else { // !Unicode
    alias RasDialA RasDial;
    alias RasEnumConnectionsA RasEnumConnections;
    alias RasEnumEntriesA RasEnumEntries;
    alias RasGetConnectStatusA RasGetConnectStatus;
    alias RasGetErrorStringA RasGetErrorString;
    alias RasHangUpA RasHangUp;
    alias RasGetProjectionInfoA RasGetProjectionInfo;
    alias RasCreatePhonebookEntryA RasCreatePhonebookEntry;
    alias RasEditPhonebookEntryA RasEditPhonebookEntry;
    alias RasSetEntryDialParamsA RasSetEntryDialParams;
    alias RasGetEntryDialParamsA RasGetEntryDialParams;
    alias RasEnumDevicesA RasEnumDevices;
    alias RasGetCountryInfoA RasGetCountryInfo;
    alias RasGetEntryPropertiesA RasGetEntryProperties;
    alias RasSetEntryPropertiesA RasSetEntryProperties;
    alias RasRenameEntryA RasRenameEntry;
    alias RasDeleteEntryA RasDeleteEntry;
    alias RasValidateEntryNameA RasValidateEntryName;

    //static if (_WIN32_WINNT >= 0x401) {
        alias RASADFUNCA RASADFUNC;
        alias RasGetSubEntryHandleA RasGetSubEntryHandle;
        alias RasConnectionNotificationA RasConnectionNotification;
        alias RasGetSubEntryPropertiesA RasGetSubEntryProperties;
        alias RasSetSubEntryPropertiesA RasSetSubEntryProperties;
        alias RasGetCredentialsA RasGetCredentials;
        alias RasSetCredentialsA RasSetCredentials;
        alias RasGetAutodialAddressA RasGetAutodialAddress;
        alias RasSetAutodialAddressA RasSetAutodialAddress;
        alias RasEnumAutodialAddressesA RasEnumAutodialAddresses;
        alias RasGetAutodialEnableA RasGetAutodialEnable;
        alias RasSetAutodialEnableA RasSetAutodialEnable;
        alias RasGetAutodialParamA RasGetAutodialParam;
        alias RasSetAutodialParamA RasSetAutodialParam;
    //}

    //static if (_WIN32_WINNT >= 0x500) {
        alias RasGetEapUserDataA RasGetEapUserData;
        alias RasSetEapUserDataA RasSetEapUserData;
        alias RasGetCustomAuthDataA RasGetCustomAuthData;
        alias RasSetCustomAuthDataA RasSetCustomAuthData;
        alias RasGetEapUserIdentityA RasGetEapUserIdentity;
        alias RasFreeEapUserIdentityA RasFreeEapUserIdentity;
    //}
} //#endif // !Unicode

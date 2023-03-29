/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_nspapi.d)
 */
module core.sys.windows.nspapi;
version (Windows):

version (ANSI) {} else version = Unicode;

import core.sys.windows.basetyps, core.sys.windows.windef;

// FIXME: check types of constants

enum {
    NS_ALL         =  0,

    NS_SAP,
    NS_NDS,
    NS_PEER_BROWSE,

    NS_TCPIP_LOCAL = 10,
    NS_TCPIP_HOSTS,
    NS_DNS,
    NS_NETBT,
    NS_WINS,

    NS_NBP         = 20,

    NS_MS          = 30,
    NS_STDA,
    NS_NTDS,

    NS_X500        = 40,
    NS_NIS,
    NS_NISPLUS,

    NS_WRQ         = 50
}

enum {
    SERVICE_REGISTER   = 1,
    SERVICE_DEREGISTER = 2,
    SERVICE_FLUSH      = 3,
    SERVICE_FLAG_HARD  = 2
}

import core.sys.windows.winsock2;

struct SOCKET_ADDRESS {
    LPSOCKADDR lpSockaddr;
    INT        iSockaddrLength;
}
alias SOCKET_ADDRESS* PSOCKET_ADDRESS, LPSOCKET_ADDRESS;

struct CSADDR_INFO {
    SOCKET_ADDRESS LocalAddr;
    SOCKET_ADDRESS RemoteAddr;
    INT            iSocketType;
    INT            iProtocol;
}
alias CSADDR_INFO* PCSADDR_INFO, LPCSADDR_INFO;

struct BLOB {
    ULONG cbSize;
    BYTE* pBlobData;
}
alias BLOB* PBLOB, LPBLOB;

struct SERVICE_ADDRESS {
    DWORD dwAddressType;
    DWORD dwAddressFlags;
    DWORD dwAddressLength;
    DWORD dwPrincipalLength;
    BYTE* lpAddress;
    BYTE* lpPrincipal;
}

struct SERVICE_ADDRESSES {
    DWORD           dwAddressCount;
    SERVICE_ADDRESS _Addresses;

    SERVICE_ADDRESS* Addresses() return { return &_Addresses; }
}
alias SERVICE_ADDRESSES* PSERVICE_ADDRESSES, LPSERVICE_ADDRESSES;

struct SERVICE_INFOA {
    LPGUID lpServiceType;
    LPSTR  lpServiceName;
    LPSTR  lpComment;
    LPSTR  lpLocale;
    DWORD  dwDisplayHint;
    DWORD  dwVersion;
    DWORD  dwTime;
    LPSTR  lpMachineName;
    LPSERVICE_ADDRESSES lpServiceAddress;
    BLOB   ServiceSpecificInfo;
}
alias SERVICE_INFOA* LPSERVICE_INFOA;

struct SERVICE_INFOW {
    LPGUID lpServiceType;
    LPWSTR lpServiceName;
    LPWSTR lpComment;
    LPWSTR lpLocale;
    DWORD  dwDisplayHint;
    DWORD  dwVersion;
    DWORD  dwTime;
    LPWSTR lpMachineName;
    LPSERVICE_ADDRESSES lpServiceAddress;
    BLOB   ServiceSpecificInfo;
}
alias SERVICE_INFOW* LPSERVICE_INFOW;

alias void* LPSERVICE_ASYNC_INFO;

extern (Windows) {
    INT SetServiceA(DWORD, DWORD, DWORD, LPSERVICE_INFOA,
      LPSERVICE_ASYNC_INFO, LPDWORD);
    INT SetServiceW(DWORD, DWORD, DWORD, LPSERVICE_INFOW,
      LPSERVICE_ASYNC_INFO, LPDWORD);
    INT GetAddressByNameA(DWORD, LPGUID, LPSTR, LPINT, DWORD,
      LPSERVICE_ASYNC_INFO, LPVOID, LPDWORD, LPSTR, LPDWORD);
    INT GetAddressByNameW(DWORD, LPGUID, LPWSTR, LPINT, DWORD,
      LPSERVICE_ASYNC_INFO, LPVOID, LPDWORD, LPWSTR, LPDWORD);
}

version (Unicode) {
    alias SERVICE_INFOW SERVICE_INFO;
    alias SetServiceW SetService;
    alias GetAddressByNameW GetAddressByName;
} else {
    alias SERVICE_INFOA SERVICE_INFO;
    alias SetServiceA SetService;
    alias GetAddressByNameA GetAddressByName;
}

alias SERVICE_INFO _SERVICE_INFO;
alias SERVICE_INFO* LPSERVICE_INFO;

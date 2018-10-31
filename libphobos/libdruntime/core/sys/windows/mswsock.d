/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Daniel Keep
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_mswsock.d)
 */
module core.sys.windows.mswsock;
version (Windows):

import core.sys.windows.winbase, core.sys.windows.windef;
private import core.sys.windows.basetyps, core.sys.windows.w32api;

import core.sys.windows.winsock2;

//static if (_WIN32_WINNT >= 0x500) {
    enum {
        /* WinNT5+:
           ms-help://MS.MSDNQTR.2003FEB.1033/winsock/winsock/socket_options.htm */
        SO_MAXDG             = 0x7009,
        SO_MAXPATHDG         = 0x700A,
    }
//}

enum {
    /* WinNT4+:
       ms-help://MS.MSDNQTR.2003FEB.1033/winsock/winsock/socket_options_for_windows_nt_4_0_2.htm */
    SO_CONNDATA              = 0x7000,
    SO_CONNOPT               = 0x7001,
    SO_DISCDATA              = 0x7002,
    SO_DISCOPT               = 0x7003,
    SO_CONNDATALEN           = 0x7004,
    SO_CONNOPTLEN            = 0x7005,
    SO_DISCDATALEN           = 0x7006,
    SO_DISCOPTLEN            = 0x7007,

    /* WinNT4:
       ms-help://MS.MSDNQTR.2003FEB.1033/winsock/winsock/socket_options.htm */
    SO_UPDATE_ACCEPT_CONTENT = 0x700B,
}

enum {
    /* Win95+, WinNT4+ but apparently shouldn't used: mark as deprecated?
       ms-help://MS.MSDNQTR.2003FEB.1033/winsock/winsock/socket_options.htm */
    SO_OPENTYPE                  = 0x7008,

    /* Win95+; these two are passed to the SO_OPENTYPE option as arguments,
       so would they be deprecated as well?
       ms-help://MS.MSDNQTR.2003FEB.1033/winsock/winsock/socket_options.htm */
    SO_SYNCHRONOUS_ALERT         = 0x0010,
    SO_SYNCHRONOUS_NONALERT      = 0x0020,

    /* Win95:
       ms-help://MS.MSDNQTR.2003FEB.1033/winsock/winsock/socket_options.htm */
    SO_CONNECT_TIME              = 0x700C,
}


enum {
    TCP_BSDURGENT = 0x7000,
}

/* These *appear* to be constants for passing to the TransmitFile /
   TransmitPackets functions, which are available in WinNT3.51+
   ms-help://MS.MSDNQTR.2003FEB.1033/winsock/winsock/transmitfile_2.htm */
enum {
    TF_DISCONNECT         = 1,
    TF_REUSE_SOCKET       = 2,
    TF_WRITE_BEHIND       = 4,
    TF_USE_DEFAULT_WORKER = 0,
    TF_USE_SYSTEM_THREAD  = 16,
    TF_USE_KERNEL_APC     = 32
}

/* Win95+, WinNT3.51+
   ms-help://MS.MSDNQTR.2003FEB.1033/winsock/winsock/transmit_file_buffers_2.htm */
struct TRANSMIT_FILE_BUFFERS {
    PVOID Head;
    DWORD HeadLength;
    PVOID Tail;
    DWORD TailLength;
}
alias TRANSMIT_FILE_BUFFERS* PTRANSMIT_FILE_BUFFERS, LPTRANSMIT_FILE_BUFFERS;

extern(Windows) {
    /* Win95+, WinNT3.51+
       ms-help://MS.MSDNQTR.2003FEB.1033/winsock/winsock/wsarecvex_2.htm */
    int WSARecvEx(SOCKET, char*, int, int*);

    /* Win95+, WinNT3.51+
       ms-help://MS.MSDNQTR.2003FEB.1033/winsock/winsock/getacceptexSOCKADDRs_2.htm */
    VOID GetAcceptExSockaddrs(PVOID, DWORD, DWORD, DWORD, SOCKADDR**, LPINT, SOCKADDR**, LPINT);

    /* WinNT3.51+
       ms-help://MS.MSDNQTR.2003FEB.1033/winsock/winsock/transmitfile_2.htm */
    BOOL TransmitFile(SOCKET, HANDLE, DWORD, DWORD, LPOVERLAPPED, LPTRANSMIT_FILE_BUFFERS, DWORD);

    /* WinNT3.51+
       ms-help://MS.MSDNQTR.2003FEB.1033/winsock/winsock/acceptex_2.htm */
    alias BOOL function(SOCKET, SOCKET, PVOID, DWORD, DWORD, DWORD, LPDWORD, LPOVERLAPPED) LPFN_ACCEPTEX;
    const GUID WSAID_ACCEPTEX = {0xb5367df1,0xcbac,0x11cf,[0x95,0xca,0x00,0x80,0x5f,0x48,0xa1,0x92]};

    alias BOOL function(SOCKET, SOCKADDR*, int, PVOID, DWORD, LPDWORD, LPOVERLAPPED) LPFN_CONNECTEX;
    const GUID WSAID_CONNECTEX = {0x25a207b9,0xddf3,0x4660,[0x8e,0xe9,0x76,0xe5,0x8c,0x74,0x06,0x3e]};
}

static if (_WIN32_WINNT >= 0x501) {
    /*  These appear to be constants for the TRANSMIT_PACKETS_ELEMENT
     *  structure below, so I've given them the same minimum version
     */
    enum {
        TP_ELEMENT_FILE   = 1,
        TP_ELEMENT_MEMORY = 2,
        TP_ELEMENT_EOP    = 4
    }

    /*  WinXP+, Srv2k3+
     *  ms-help://MS.MSDNQTR.2003FEB.1033/winsock/winsock/transmit_packets_element_2.htm
     */
    struct TRANSMIT_PACKETS_ELEMENT {
        ULONG dwElFlags;
        ULONG cLength;
        union {
            struct {
                LARGE_INTEGER nFileOffset;
                HANDLE        hFile;
            }
            PVOID pBuffer;
        }
    }

    struct WSABUF {
        ULONG len;
        CHAR* buf;
    }

    alias WSABUF* LPWSABUF;

    /*  WinXP+, Server2003+:
     *  ms-help://MS.MSDNQTR.2003FEB.1033/winsock/winsock/wsamsg_2.htm
     */
    struct WSAMSG {
        LPSOCKADDR name;
        INT        namelen;
        LPWSABUF   lpBuffers;
        DWORD      dwBufferCount;
        WSABUF     Control;
        DWORD      dwFlags;
    }

    alias WSAMSG* PWSAMSG, LPWSAMSG;

    /* According to MSDN docs, the WSAMSG.Control buffer starts with a
       cmsghdr header of the following form.  See also RFC 2292. */
    /* DK: Confirmed.  So I suppose these should get the same version as
       WSAMSG... */
    struct WSACMSGHDR {
        SIZE_T cmsg_len;
        INT  cmsg_level;
        INT  cmsg_type;
        // followed by UCHAR cmsg_data[];
    }
}

static if (_WIN32_WINNT >= 0x600) {
    /* TODO: Standard Posix.1g macros as per RFC 2292, with WSA_uglification. */
    /* DK: MinGW doesn't define these, and neither does the MSDN docs.  Might have
       to actually look up RFC 2292... */
    /+
    #if 0
    #define WSA_CMSG_FIRSTHDR(mhdr)
    #define WSA_CMSG_NXTHDR(mhdr, cmsg)
    #define WSA_CMSG_SPACE(length)
    #define WSA_CMSG_LEN(length)
    #endif
    +/

    /*  Win Vista+, Srv2k3+
     *  ms-help://MS.MSDNQTR.2003FEB.1033/winsock/winsock/disconnectex_2.htm
     */
    extern(Windows) BOOL DisconnectEx(SOCKET, LPOVERLAPPED, DWORD, DWORD);
}

static if (_WIN32_WINNT >= 0x501) {
    /*  WinXP+, Srv2k3+
     *  ms-help://MS.MSDNQTR.2003FEB.1033/winsock/winsock/wsarecvmsg_2.htm
     */
    extern(Windows) int WSARecvMsg(SOCKET, LPWSAMSG, LPDWORD, LPWSAOVERLAPPED, LPWSAOVERLAPPED_COMPLETION_ROUTINE);
}

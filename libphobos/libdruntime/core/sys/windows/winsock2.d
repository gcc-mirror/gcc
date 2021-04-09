/*
    Written by Christopher E. Miller
    $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
*/


module core.sys.windows.winsock2;
version (Windows):
@system:

pragma(lib, "ws2_32");

extern(Windows):
nothrow:

alias SOCKET = size_t;
alias socklen_t = int;

enum SOCKET INVALID_SOCKET = cast(SOCKET)~0;
enum int SOCKET_ERROR = -1;

enum WSADESCRIPTION_LEN = 256;
enum WSASYS_STATUS_LEN = 128;

struct WSADATA
{
    ushort wVersion;
    ushort wHighVersion;
    char[WSADESCRIPTION_LEN + 1] szDescription = 0;
    char[WSASYS_STATUS_LEN + 1] szSystemStatus = 0;
    ushort iMaxSockets;
    ushort iMaxUdpDg;
    char* lpVendorInfo;
}
alias LPWSADATA = WSADATA*;


enum int IOCPARM_MASK =  0x7F;
enum int IOC_IN =        cast(int)0x80000000;
enum int FIONBIO =       cast(int)(IOC_IN | ((uint.sizeof & IOCPARM_MASK) << 16) | (102 << 8) | 126);

enum NI_MAXHOST = 1025;
enum NI_MAXSERV = 32;

@nogc
{
int WSAStartup(ushort wVersionRequested, LPWSADATA lpWSAData);
int WSACleanup();
SOCKET socket(int af, int type, int protocol);
int ioctlsocket(SOCKET s, int cmd, uint* argp);
int bind(SOCKET s, const(sockaddr)* name, socklen_t namelen);
int connect(SOCKET s, const(sockaddr)* name, socklen_t namelen);
int listen(SOCKET s, int backlog);
SOCKET accept(SOCKET s, sockaddr* addr, socklen_t* addrlen);
int closesocket(SOCKET s);
int shutdown(SOCKET s, int how);
int getpeername(SOCKET s, sockaddr* name, socklen_t* namelen);
int getsockname(SOCKET s, sockaddr* name, socklen_t* namelen);
int send(SOCKET s, const(void)* buf, int len, int flags);
int sendto(SOCKET s, const(void)* buf, int len, int flags, const(sockaddr)* to, socklen_t tolen);
int recv(SOCKET s, void* buf, int len, int flags);
int recvfrom(SOCKET s, void* buf, int len, int flags, sockaddr* from, socklen_t* fromlen);
int getsockopt(SOCKET s, int level, int optname, void* optval, socklen_t* optlen);
int setsockopt(SOCKET s, int level, int optname, const(void)* optval, socklen_t optlen);
uint inet_addr(const char* cp);
int select(int nfds, fd_set* readfds, fd_set* writefds, fd_set* errorfds, const(timeval)* timeout);
char* inet_ntoa(in_addr ina);
hostent* gethostbyname(const char* name);
hostent* gethostbyaddr(const(void)* addr, int len, int type);
protoent* getprotobyname(const char* name);
protoent* getprotobynumber(int number);
servent* getservbyname(const char* name, const char* proto);
servent* getservbyport(int port, const char* proto);
}

enum: int
{
    NI_NOFQDN =          0x01,
    NI_NUMERICHOST =     0x02,
    NI_NAMEREQD =        0x04,
    NI_NUMERICSERV =     0x08,
    NI_DGRAM  =          0x10,
}

@nogc
{
int gethostname(const char* name, int namelen);
int getaddrinfo(const(char)* nodename, const(char)* servname, const(addrinfo)* hints, addrinfo** res);
void freeaddrinfo(addrinfo* ai);
int getnameinfo(const(sockaddr)* sa, socklen_t salen, char* host, uint hostlen, char* serv, uint servlen, int flags);
}

enum WSABASEERR = 10000;

enum: int
{
    /*
     * Windows Sockets definitions of regular Microsoft C error constants
     */
    WSAEINTR = (WSABASEERR+4),
    WSAEBADF = (WSABASEERR+9),
    WSAEACCES = (WSABASEERR+13),
    WSAEFAULT = (WSABASEERR+14),
    WSAEINVAL = (WSABASEERR+22),
    WSAEMFILE = (WSABASEERR+24),

    /*
     * Windows Sockets definitions of regular Berkeley error constants
     */
    WSAEWOULDBLOCK = (WSABASEERR+35),
    WSAEINPROGRESS = (WSABASEERR+36),
    WSAEALREADY = (WSABASEERR+37),
    WSAENOTSOCK = (WSABASEERR+38),
    WSAEDESTADDRREQ = (WSABASEERR+39),
    WSAEMSGSIZE = (WSABASEERR+40),
    WSAEPROTOTYPE = (WSABASEERR+41),
    WSAENOPROTOOPT = (WSABASEERR+42),
    WSAEPROTONOSUPPORT = (WSABASEERR+43),
    WSAESOCKTNOSUPPORT = (WSABASEERR+44),
    WSAEOPNOTSUPP = (WSABASEERR+45),
    WSAEPFNOSUPPORT = (WSABASEERR+46),
    WSAEAFNOSUPPORT = (WSABASEERR+47),
    WSAEADDRINUSE = (WSABASEERR+48),
    WSAEADDRNOTAVAIL = (WSABASEERR+49),
    WSAENETDOWN = (WSABASEERR+50),
    WSAENETUNREACH = (WSABASEERR+51),
    WSAENETRESET = (WSABASEERR+52),
    WSAECONNABORTED = (WSABASEERR+53),
    WSAECONNRESET = (WSABASEERR+54),
    WSAENOBUFS = (WSABASEERR+55),
    WSAEISCONN = (WSABASEERR+56),
    WSAENOTCONN = (WSABASEERR+57),
    WSAESHUTDOWN = (WSABASEERR+58),
    WSAETOOMANYREFS = (WSABASEERR+59),
    WSAETIMEDOUT = (WSABASEERR+60),
    WSAECONNREFUSED = (WSABASEERR+61),
    WSAELOOP = (WSABASEERR+62),
    WSAENAMETOOLONG = (WSABASEERR+63),
    WSAEHOSTDOWN = (WSABASEERR+64),
    WSAEHOSTUNREACH = (WSABASEERR+65),
    WSAENOTEMPTY = (WSABASEERR+66),
    WSAEPROCLIM = (WSABASEERR+67),
    WSAEUSERS = (WSABASEERR+68),
    WSAEDQUOT = (WSABASEERR+69),
    WSAESTALE = (WSABASEERR+70),
    WSAEREMOTE = (WSABASEERR+71),

    /*
     * Extended Windows Sockets error constant definitions
     */
    WSASYSNOTREADY = (WSABASEERR+91),
    WSAVERNOTSUPPORTED = (WSABASEERR+92),
    WSANOTINITIALISED = (WSABASEERR+93),

    /* Authoritative Answer: Host not found */
    WSAHOST_NOT_FOUND = (WSABASEERR+1001),
    HOST_NOT_FOUND = WSAHOST_NOT_FOUND,

    /* Non-Authoritative: Host not found, or SERVERFAIL */
    WSATRY_AGAIN = (WSABASEERR+1002),
    TRY_AGAIN = WSATRY_AGAIN,

    /* Non recoverable errors, FORMERR, REFUSED, NOTIMP */
    WSANO_RECOVERY = (WSABASEERR+1003),
    NO_RECOVERY = WSANO_RECOVERY,

    /* Valid name, no data record of requested type */
    WSANO_DATA = (WSABASEERR+1004),
    NO_DATA = WSANO_DATA,

    /* no address, look for MX record */
    WSANO_ADDRESS = WSANO_DATA,
    NO_ADDRESS = WSANO_ADDRESS
}

/*
 * Windows Sockets errors redefined as regular Berkeley error constants
 */
enum: int
{
    EWOULDBLOCK = WSAEWOULDBLOCK,
    EINPROGRESS = WSAEINPROGRESS,
    EALREADY = WSAEALREADY,
    ENOTSOCK = WSAENOTSOCK,
    EDESTADDRREQ = WSAEDESTADDRREQ,
    EMSGSIZE = WSAEMSGSIZE,
    EPROTOTYPE = WSAEPROTOTYPE,
    ENOPROTOOPT = WSAENOPROTOOPT,
    EPROTONOSUPPORT = WSAEPROTONOSUPPORT,
    ESOCKTNOSUPPORT = WSAESOCKTNOSUPPORT,
    EOPNOTSUPP = WSAEOPNOTSUPP,
    EPFNOSUPPORT = WSAEPFNOSUPPORT,
    EAFNOSUPPORT = WSAEAFNOSUPPORT,
    EADDRINUSE = WSAEADDRINUSE,
    EADDRNOTAVAIL = WSAEADDRNOTAVAIL,
    ENETDOWN = WSAENETDOWN,
    ENETUNREACH = WSAENETUNREACH,
    ENETRESET = WSAENETRESET,
    ECONNABORTED = WSAECONNABORTED,
    ECONNRESET = WSAECONNRESET,
    ENOBUFS = WSAENOBUFS,
    EISCONN = WSAEISCONN,
    ENOTCONN = WSAENOTCONN,
    ESHUTDOWN = WSAESHUTDOWN,
    ETOOMANYREFS = WSAETOOMANYREFS,
    ETIMEDOUT = WSAETIMEDOUT,
    ECONNREFUSED = WSAECONNREFUSED,
    ELOOP = WSAELOOP,
    ENAMETOOLONG = WSAENAMETOOLONG,
    EHOSTDOWN = WSAEHOSTDOWN,
    EHOSTUNREACH = WSAEHOSTUNREACH,
    ENOTEMPTY = WSAENOTEMPTY,
    EPROCLIM = WSAEPROCLIM,
    EUSERS = WSAEUSERS,
    EDQUOT = WSAEDQUOT,
    ESTALE = WSAESTALE,
    EREMOTE = WSAEREMOTE
}

enum: int
{
    EAI_NONAME    = WSAHOST_NOT_FOUND,
}

int WSAGetLastError() @trusted @nogc;


enum: int
{
    AF_UNSPEC =     0,

    AF_UNIX =       1,
    AF_INET =       2,
    AF_IMPLINK =    3,
    AF_PUP =        4,
    AF_CHAOS =      5,
    AF_NS =         6,
    AF_IPX =        AF_NS,
    AF_ISO =        7,
    AF_OSI =        AF_ISO,
    AF_ECMA =       8,
    AF_DATAKIT =    9,
    AF_CCITT =      10,
    AF_SNA =        11,
    AF_DECnet =     12,
    AF_DLI =        13,
    AF_LAT =        14,
    AF_HYLINK =     15,
    AF_APPLETALK =  16,
    AF_NETBIOS =    17,
    AF_VOICEVIEW =  18,
    AF_FIREFOX =    19,
    AF_UNKNOWN1 =   20,
    AF_BAN =        21,
    AF_ATM =        22,
    AF_INET6 =      23,
    AF_CLUSTER =    24,
    AF_12844 =      25,
    AF_IRDA =       26,
    AF_NETDES =     28,

    AF_MAX =        29,


    PF_UNSPEC     = AF_UNSPEC,

    PF_UNIX =       AF_UNIX,
    PF_INET =       AF_INET,
    PF_IMPLINK =    AF_IMPLINK,
    PF_PUP =        AF_PUP,
    PF_CHAOS =      AF_CHAOS,
    PF_NS =         AF_NS,
    PF_IPX =        AF_IPX,
    PF_ISO =        AF_ISO,
    PF_OSI =        AF_OSI,
    PF_ECMA =       AF_ECMA,
    PF_DATAKIT =    AF_DATAKIT,
    PF_CCITT =      AF_CCITT,
    PF_SNA =        AF_SNA,
    PF_DECnet =     AF_DECnet,
    PF_DLI =        AF_DLI,
    PF_LAT =        AF_LAT,
    PF_HYLINK =     AF_HYLINK,
    PF_APPLETALK =  AF_APPLETALK,
    PF_VOICEVIEW =  AF_VOICEVIEW,
    PF_FIREFOX =    AF_FIREFOX,
    PF_UNKNOWN1 =   AF_UNKNOWN1,
    PF_BAN =        AF_BAN,
    PF_INET6 =      AF_INET6,

    PF_MAX        = AF_MAX,
}


enum: int
{
    SOL_SOCKET = 0xFFFF,
}


enum: int
{
    SO_DEBUG =        0x0001,
    SO_ACCEPTCONN =   0x0002,
    SO_REUSEADDR =    0x0004,
    SO_KEEPALIVE =    0x0008,
    SO_DONTROUTE =    0x0010,
    SO_BROADCAST =    0x0020,
    SO_USELOOPBACK =  0x0040,
    SO_LINGER =       0x0080,
    SO_DONTLINGER =   ~SO_LINGER,
    SO_OOBINLINE =    0x0100,
    SO_SNDBUF =       0x1001,
    SO_RCVBUF =       0x1002,
    SO_SNDLOWAT =     0x1003,
    SO_RCVLOWAT =     0x1004,
    SO_SNDTIMEO =     0x1005,
    SO_RCVTIMEO =     0x1006,
    SO_ERROR =        0x1007,
    SO_TYPE =         0x1008,
    SO_EXCLUSIVEADDRUSE = ~SO_REUSEADDR,

    TCP_NODELAY =    1,

    IP_OPTIONS                  = 1,

    IP_HDRINCL                  = 2,
    IP_TOS                      = 3,
    IP_TTL                      = 4,
    IP_MULTICAST_IF             = 9,
    IP_MULTICAST_TTL            = 10,
    IP_MULTICAST_LOOP           = 11,
    IP_ADD_MEMBERSHIP           = 12,
    IP_DROP_MEMBERSHIP          = 13,
    IP_DONTFRAGMENT             = 14,
    IP_ADD_SOURCE_MEMBERSHIP    = 15,
    IP_DROP_SOURCE_MEMBERSHIP   = 16,
    IP_BLOCK_SOURCE             = 17,
    IP_UNBLOCK_SOURCE           = 18,
    IP_PKTINFO                  = 19,

    IPV6_UNICAST_HOPS =    4,
    IPV6_MULTICAST_IF =    9,
    IPV6_MULTICAST_HOPS =  10,
    IPV6_MULTICAST_LOOP =  11,
    IPV6_ADD_MEMBERSHIP =  12,
    IPV6_DROP_MEMBERSHIP = 13,
    IPV6_JOIN_GROUP =      IPV6_ADD_MEMBERSHIP,
    IPV6_LEAVE_GROUP =     IPV6_DROP_MEMBERSHIP,
    IPV6_V6ONLY = 27,
}


/// Default FD_SETSIZE value.
/// In C/C++, it is redefinable by #define-ing the macro before #include-ing
/// winsock.h. In D, use the $(D FD_CREATE) function to allocate a $(D fd_set)
/// of an arbitrary size.
enum int FD_SETSIZE = 64;


struct fd_set_custom(uint SETSIZE)
{
    uint fd_count;
    SOCKET[SETSIZE] fd_array;
}

alias fd_set = fd_set_custom!FD_SETSIZE;

// Removes.
void FD_CLR(SOCKET fd, fd_set* set) pure @nogc
{
    uint c = set.fd_count;
    SOCKET* start = set.fd_array.ptr;
    SOCKET* stop = start + c;

    for (; start != stop; start++)
    {
        if (*start == fd)
            goto found;
    }
    return; //not found

    found:
    for (++start; start != stop; start++)
    {
        *(start - 1) = *start;
    }

    set.fd_count = c - 1;
}


// Tests.
int FD_ISSET(SOCKET fd, const(fd_set)* set) pure @nogc
{
const(SOCKET)* start = set.fd_array.ptr;
const(SOCKET)* stop = start + set.fd_count;

    for (; start != stop; start++)
    {
        if (*start == fd)
            return true;
    }
    return false;
}


// Adds.
void FD_SET(SOCKET fd, fd_set* set)     pure @nogc
{
    uint c = set.fd_count;
    set.fd_array.ptr[c] = fd;
    set.fd_count = c + 1;
}


// Resets to zero.
void FD_ZERO(fd_set* set) pure @nogc
{
    set.fd_count = 0;
}


/// Creates a new $(D fd_set) with the specified capacity.
fd_set* FD_CREATE(uint capacity) pure
{
    // Take into account alignment (SOCKET may be 64-bit and require 64-bit alignment on 64-bit systems)
    size_t size = (fd_set_custom!1).sizeof - SOCKET.sizeof + (SOCKET.sizeof * capacity);
    auto data = new ubyte[size];
    auto set = cast(fd_set*)data.ptr;
    FD_ZERO(set);
    return set;
}

struct linger
{
    ushort l_onoff;
    ushort l_linger;
}


struct protoent
{
    char* p_name;
    char** p_aliases;
    short p_proto;
}


struct servent
{
    char* s_name;
    char** s_aliases;

    version (Win64)
    {
        char* s_proto;
        short s_port;
    }
    else version (Win32)
    {
        short s_port;
        char* s_proto;
    }
}


/+
union in6_addr
{
    private union _u_t
    {
        ubyte[16] Byte;
        ushort[8] Word;
    }
    _u_t u;
}


struct in_addr6
{
    ubyte[16] s6_addr;
}
+/

@safe pure @nogc
{
    ushort htons(ushort x);
    uint htonl(uint x);
    ushort ntohs(ushort x);
    uint ntohl(uint x);
}


enum: int
{
    SOCK_STREAM =     1,
    SOCK_DGRAM =      2,
    SOCK_RAW =        3,
    SOCK_RDM =        4,
    SOCK_SEQPACKET =  5,
}


enum: int
{
    IPPROTO_IP =    0,
    IPPROTO_ICMP =  1,
    IPPROTO_IGMP =  2,
    IPPROTO_GGP =   3,
    IPPROTO_TCP =   6,
    IPPROTO_PUP =   12,
    IPPROTO_UDP =   17,
    IPPROTO_IDP =   22,
    IPPROTO_IPV6 =  41,
    IPPROTO_ND =    77,
    IPPROTO_RAW =   255,

    IPPROTO_MAX =   256,
}


enum: int
{
    MSG_OOB =        0x1,
    MSG_PEEK =       0x2,
    MSG_DONTROUTE =  0x4
}


enum: int
{
    SD_RECEIVE =  0,
    SD_SEND =     1,
    SD_BOTH =     2,
}


enum: uint
{
    INADDR_ANY =        0,
    INADDR_LOOPBACK =   0x7F000001,
    INADDR_BROADCAST =  0xFFFFFFFF,
    INADDR_NONE =       0xFFFFFFFF,
    ADDR_ANY =          INADDR_ANY,
}


enum: int
{
    AI_PASSIVE = 0x1,
    AI_CANONNAME = 0x2,
    AI_NUMERICHOST = 0x4,
    AI_ADDRCONFIG = 0x0400,
    AI_NON_AUTHORITATIVE = 0x04000,
    AI_SECURE = 0x08000,
    AI_RETURN_PREFERRED_NAMES = 0x010000,
}


struct timeval
{
    int tv_sec;
    int tv_usec;
}


union in_addr
{
    private union _S_un_t
    {
        private struct _S_un_b_t
        {
            ubyte s_b1, s_b2, s_b3, s_b4;
        }
        _S_un_b_t S_un_b;

        private struct _S_un_w_t
        {
            ushort s_w1, s_w2;
        }
        _S_un_w_t S_un_w;

        uint S_addr;
    }
    _S_un_t S_un;

    uint s_addr;

    struct
    {
        ubyte s_net, s_host;

        union
        {
            ushort s_imp;

            struct
            {
                ubyte s_lh, s_impno;
            }
        }
    }
}


union in6_addr
{
    private union _in6_u_t
    {
        ubyte[16] u6_addr8;
        ushort[8] u6_addr16;
        uint[4] u6_addr32;
    }
    _in6_u_t in6_u;

    ubyte[16] s6_addr8;
    ushort[8] s6_addr16;
    uint[4] s6_addr32;

    alias s6_addr = s6_addr8;
}


enum in6_addr IN6ADDR_ANY = { s6_addr8: [0] };
enum in6_addr IN6ADDR_LOOPBACK = { s6_addr8: [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1] };
//alias IN6ADDR_ANY_INIT = IN6ADDR_ANY;
//alias IN6ADDR_LOOPBACK_INIT = IN6ADDR_LOOPBACK;

enum int INET_ADDRSTRLEN = 16;
enum int INET6_ADDRSTRLEN = 46;




struct sockaddr
{
    short sa_family;
    ubyte[14] sa_data;
}
alias sockaddr SOCKADDR;
alias SOCKADDR* PSOCKADDR, LPSOCKADDR;

struct sockaddr_storage
{
    short     ss_family;
    char[6]   __ss_pad1 = void;
    long      __ss_align;
    char[112] __ss_pad2 = void;
}
alias sockaddr_storage SOCKADDR_STORAGE;
alias SOCKADDR_STORAGE* PSOCKADDR_STORAGE;

struct sockaddr_in
{
    short sin_family = AF_INET;
    ushort sin_port;
    in_addr sin_addr;
    ubyte[8] sin_zero;
}
alias sockaddr_in SOCKADDR_IN;
alias SOCKADDR_IN* PSOCKADDR_IN, LPSOCKADDR_IN;


struct sockaddr_in6
{
    short sin6_family = AF_INET6;
    ushort sin6_port;
    uint sin6_flowinfo;
    in6_addr sin6_addr;
    uint sin6_scope_id;
}


struct addrinfo
{
    int ai_flags;
    int ai_family;
    int ai_socktype;
    int ai_protocol;
    size_t ai_addrlen;
    char* ai_canonname;
    sockaddr* ai_addr;
    addrinfo* ai_next;
}


struct hostent
{
    char* h_name;
    char** h_aliases;
    short h_addrtype;
    short h_length;
    char** h_addr_list;


    char* h_addr() @safe pure nothrow @nogc
    {
        return h_addr_list[0];
    }
}

// Note: These are Winsock2!!
struct WSAOVERLAPPED;
alias LPWSAOVERLAPPED = WSAOVERLAPPED*;
alias LPWSAOVERLAPPED_COMPLETION_ROUTINE = void function(uint, uint, LPWSAOVERLAPPED, uint);
int WSAIoctl(SOCKET s, uint dwIoControlCode,
    void* lpvInBuffer, uint cbInBuffer,
    void* lpvOutBuffer, uint cbOutBuffer,
    uint* lpcbBytesReturned,
    LPWSAOVERLAPPED lpOverlapped, LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine);


enum IOC_VENDOR = 0x18000000;
enum SIO_KEEPALIVE_VALS = IOC_IN | IOC_VENDOR | 4;

/* Argument structure for SIO_KEEPALIVE_VALS */
struct tcp_keepalive
{
    uint onoff;
    uint keepalivetime;
    uint keepaliveinterval;
}

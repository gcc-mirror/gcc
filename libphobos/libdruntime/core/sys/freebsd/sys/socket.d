//Written in the D programming language

/++
    D header file for FreeBSD's extensions to POSIX's sys/socket.h.

    Copyright: Copyright 2023
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTP jmdavisprog.com, Jonathan M Davis)
 +/
module core.sys.freebsd.sys.socket;

public import core.sys.posix.sys.socket;

version (FreeBSD):
extern(C):
@nogc:
nothrow:

// Creation flags, OR'ed into socket() and socketpair() type argument.
enum : uint
{
    SOCK_CLOEXEC  = 0x10000000,
    SOCK_NONBLOCK = 0x20000000,
}

// Option flags per-socket.
enum : uint
{
    SO_USELOOPBACK  = 0x00000040,
    SO_TIMESTAMP    = 0x00000400,
    SO_ACCEPTFILTER = 0x00001000,
    SO_BINTIME      = 0x00002000,

    SO_NO_OFFLOAD   = 0x00004000,
    SO_NO_DDP       = 0x00008000,
    SO_REUSEPORT_LB = 0x00010000,
    SO_RERROR       = 0x00020000,
}


// Additional options, not kept in so_options.
enum : uint
{
    SO_LABEL           = 0x1009,
    SO_PEERLABEL       = 0x1010,
    SO_LISTENQLIMIT    = 0x1011,
    SO_LISTENQLEN      = 0x1012,
    SO_LISTENINCQLEN   = 0x1013,
    SO_SETFIB          = 0x1014,
    SO_USER_COOKIE     = 0x1015,
    SO_PROTOCOL        = 0x1016,
    SO_PROTOTYPE       = SO_PROTOCOL,
    SO_TS_CLOCK        = 0x1017,
    SO_MAX_PACING_RATE = 0x1018,
    SO_DOMAIN          = 0x1019,

    SO_TS_REALTIME_MICRO = 0,
    SO_TS_BINTIME        = 1,
    SO_TS_REALTIME       = 2,
    SO_TS_MONOTONIC      = 3,
    SO_TS_DEFAULT        = SO_TS_REALTIME_MICRO,
    SO_TS_CLOCK_MAX      = SO_TS_MONOTONIC,
}

/+
   Space reserved for new socket options added by third-party vendors.
   This range applies to all socket option levels.  New socket options
   in FreeBSD should always use an option value less than SO_VENDOR.
 +/
enum : uint
{
    SO_VENDOR = 0x80000000,
}

struct accept_filter_arg
{
    char[16]      af_name;
    char [256-16] af_arg;
}

// Address families.
enum : uint
{
    AF_LOCAL = AF_UNIX,

    AF_IMPLINK = 3,
    AF_PUP     = 4,
    AF_CHAOS   = 5,
    AF_NETBIOS = 6,
    AF_ISO     = 7,
    AF_OSI     = AF_ISO,
    AF_ECMA    = 8,
    AF_DATAKIT = 9,
    AF_CCITT   = 10,
    AF_SNA     = 11,
    AF_DECnet  = 12,
    AF_DLI     = 13,
    AF_LAT     = 14,
    AF_HYLINK  = 15,

    AF_ROUTE       = 17,
    AF_LINK        = 18,
    pseudo_AF_XTP  = 19,
    AF_COIP        = 20,
    AF_CNT         = 21,
    pseudo_AF_RTIP = 22,

    AF_SIP        = 24,
    pseudo_AF_PIP = 25,
    AF_ISDN       = 26,
    AF_E164       = AF_ISDN,
    pseudo_AF_KEY = 27,

    AF_NATM            = 29,
    AF_ATM             = 30,
    pseudo_AF_HDRCMPLT = 31,
    AF_NETGRAPH        = 32,
    AF_SLOW            = 33,
    AF_SCLUSTER        = 34,
    AF_ARP             = 35,
    AF_BLUETOOTH       = 36,
    AF_IEEE80211       = 37,
    AF_NETLINK         = 38,

    AF_INET_SDP = 40,

    AF_INET6_SDP = 42,
    AF_HYPERV    = 43,
    AF_DIVERT    = 44,
    AF_MAX       = 44,
}

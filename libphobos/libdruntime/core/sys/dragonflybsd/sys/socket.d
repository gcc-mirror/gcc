/**
 * D header file for DragonFlyBSD
 *
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 */
module core.sys.dragonflybsd.sys.socket;

public import core.sys.posix.sys.socket;

version (DragonFlyBSD):
extern(C):
@nogc:
nothrow:

enum
{
    AF_IMPLINK          = 3,
    AF_PUP              = 4,
    AF_CHAOS            = 5,
    AF_NETBIOS          = 6,
    AF_ISO              = 7,
    AF_OSI              = AF_ISO,
    AF_ECMA             = 8,
    AF_DATAKIT          = 9,
    AF_CCITT            = 10,
    AF_SNA              = 11,
    AF_DECnet           = 12,
    AF_DLI              = 13,
    AF_LAT              = 14,
    AF_HYLINK           = 15,
    AF_ROUTE            = 17,
    AF_LINK             = 18,
    pseudo_AF_XTP       = 19,
    AF_COIP             = 20,
    AF_CNT              = 21,
    pseudo_AF_RTIP      = 22,
    AF_SIP              = 24,
    pseudo_AF_PIP       = 25,
    AF_ISDN             = 26,
    AF_E164             = AF_ISDN,
    pseudo_AF_KEY       = 27,
    AF_NATM             = 29,
    AF_ATM              = 30,
    pseudo_AF_HDRCMPLT  = 31,
    AF_NETGRAPH         = 32,
    AF_BLUETOOTH        = 33,
    AF_MPLS             = 34,
    AF_IEEE80211        = 35,
}

/* protocol families */
enum PF_UNSPEC          = AF_UNSPEC;
enum PF_LOCAL           = AF_LOCAL;
enum PF_UNIX            = PF_LOCAL;
enum PF_INET            = AF_INET;
enum PF_IMPLINK         = AF_IMPLINK;
enum PF_PUP             = AF_PUP;
enum PF_CHAOS           = AF_CHAOS;
enum PF_NETBIOS         = AF_NETBIOS;
enum PF_ISO             = AF_ISO;
enum PF_OSI             = AF_ISO;
enum PF_ECMA            = AF_ECMA;
enum PF_DATAKIT         = AF_DATAKIT;
enum PF_CCITT           = AF_CCITT;
enum PF_SNA             = AF_SNA;
enum PF_DECnet          = AF_DECnet;
enum PF_DLI             = AF_DLI;
enum PF_LAT             = AF_LAT;
enum PF_HYLINK          = AF_HYLINK;
enum PF_APPLETALK       = AF_APPLETALK;
enum PF_ROUTE           = AF_ROUTE;
enum PF_LINK            = AF_LINK;
enum PF_XTP             = pseudo_AF_XTP;
enum PF_COIP            = AF_COIP;
enum PF_CNT             = AF_CNT;
enum PF_SIP             = AF_SIP;
enum PF_IPX             = AF_IPX;
enum PF_RTIP            = pseudo_AF_RTIP;
enum PF_PIP             = pseudo_AF_PIP;
enum PF_ISDN            = AF_ISDN;
enum PF_KEY             = pseudo_AF_KEY;
enum PF_INET6           = AF_INET6;
enum PF_NATM            = AF_NATM;
enum PF_ATM             = AF_ATM;
enum PF_NETGRAPH        = AF_NETGRAPH;
enum PF_BLUETOOTH       = AF_BLUETOOTH;

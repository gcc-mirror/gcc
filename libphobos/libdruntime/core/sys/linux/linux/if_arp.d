//Written in the D programming language

/++
    D header file for Linux's linux/if_arp.h.

    Copyright: Copyright 2023
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTP jmdavisprog.com, Jonathan M Davis)
 +/
module core.sys.linux.linux.if_arp;

version (linux):
extern(C):
@nogc:
nothrow:

import core.sys.posix.net.if_ : IF_NAMESIZE;
import core.sys.posix.sys.socket : sockaddr;

enum : ushort
{
    ARPHRD_NETROM     = 0,
    ARPHRD_ETHER      = 1,
    ARPHRD_EETHER     = 2,
    ARPHRD_AX25       = 3,
    ARPHRD_PRONET     = 4,
    ARPHRD_CHAOS      = 5,
    ARPHRD_IEEE802    = 6,
    ARPHRD_ARCNET     = 7,
    ARPHRD_APPLETLK   = 8,
    ARPHRD_DLCI       =15,
    ARPHRD_ATM        =19,
    ARPHRD_METRICOM   = 23,
    ARPHRD_IEEE1394   = 24,
    ARPHRD_EUI64      = 27,
    ARPHRD_INFINIBAND = 32,

    ARPHRD_SLIP    = 256,
    ARPHRD_CSLIP   = 257,
    ARPHRD_SLIP6   = 258,
    ARPHRD_CSLIP6  = 259,
    ARPHRD_RSRVD   = 260,
    ARPHRD_ADAPT   = 264,
    ARPHRD_ROSE    = 270,
    ARPHRD_X25     = 271,
    ARPHRD_HWX25   = 272,
    ARPHRD_CAN     = 280,
    ARPHRD_MCTP    = 290,
    ARPHRD_PPP     = 512,
    ARPHRD_CISCO   = 513,
    ARPHRD_HDLC    = ARPHRD_CISCO,
    ARPHRD_LAPB    = 516,
    ARPHRD_DDCMP   = 517,
    ARPHRD_RAWHDLC = 518,
    ARPHRD_RAWIP   = 519,

    ARPHRD_TUNNEL   = 768,
    ARPHRD_TUNNEL6  = 769,
    ARPHRD_FRAD     = 770,
    ARPHRD_SKIP     = 771,
    ARPHRD_LOOPBACK = 772,
    ARPHRD_LOCALTLK = 773,
    ARPHRD_FDDI     = 774,
    ARPHRD_BIF      = 775,
    ARPHRD_SIT      = 776,
    ARPHRD_IPDDP    = 777,
    ARPHRD_IPGRE    = 778,
    ARPHRD_PIMREG   = 779,
    ARPHRD_HIPPI    = 780,
    ARPHRD_ASH      = 781,
    ARPHRD_ECONET   = 782,
    ARPHRD_IRDA     = 783,

    ARPHRD_FCPP     = 784,
    ARPHRD_FCAL     = 785,
    ARPHRD_FCPL     = 786,
    ARPHRD_FCFABRIC = 787,

    ARPHRD_IEEE802_TR         = 800,
    ARPHRD_IEEE80211          = 801,
    ARPHRD_IEEE80211_PRISM    = 802,
    ARPHRD_IEEE80211_RADIOTAP = 803,
    ARPHRD_IEEE802154         = 804,
    ARPHRD_IEEE802154_MONITOR = 805,

    ARPHRD_PHONET      = 820,
    ARPHRD_PHONET_PIPE = 821,
    ARPHRD_CAIF        = 822,
    ARPHRD_IP6GRE      = 823,
    ARPHRD_NETLINK     = 824,
    ARPHRD_6LOWPAN     = 825,
    ARPHRD_VSOCKMON    = 826,

    ARPHRD_VOID = 0xFFFF,
    ARPHRD_NONE = 0xFFFE,
}

enum : ushort
{
    ARPOP_REQUEST   = 1,
    ARPOP_REPLY     = 2,
    ARPOP_RREQUEST  = 3,
    ARPOP_RREPLY    = 4,
    ARPOP_InREQUEST = 8,
    ARPOP_InREPLY   = 9,
    ARPOP_NAK       = 10,
}

struct arpreq
{
    sockaddr          arp_pa;
    sockaddr          arp_ha;
    int               arp_flags;
    sockaddr          arp_netmask;
    char[IF_NAMESIZE] arp_dev;
}

enum
{
    ATF_COM         = 0x02,
    ATF_PERM        = 0x04,
    ATF_PUBL        = 0x08,
    ATF_USETRAILERS = 0x10,
    ATF_NETMASK     = 0x20,

    ATF_DONTPUB     = 0x40,
}

struct arphdr
{
    ushort ar_hrd;
    ushort ar_pro;
    ubyte  ar_hln;
    ubyte  ar_pln;
    ushort ar_op;
}

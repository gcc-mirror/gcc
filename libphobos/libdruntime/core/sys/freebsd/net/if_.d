//Written in the D programming language

/++
    D header file for FreeBSD's net/if.h.

    Copyright: Copyright 2024
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTP jmdavisprog.com, Jonathan M Davis)
 +/
module core.sys.freebsd.net.if_;

public import core.sys.posix.net.if_;

version (FreeBSD):
extern(C):
@nogc:
nothrow:

import core.stdc.config;
import core.sys.freebsd.sys.types : caddr_t;
import core.sys.posix.sys.socket : sockaddr;
import core.sys.posix.sys.time : time_t, timeval;

enum IF_MAXUNIT = 0x7fff;

struct if_clonereq
{
    int    ifcr_total;
    int    ifcr_count;
    ubyte* ifcr_buffer;
}

struct if_data
{
    ubyte  ifi_type;
    ubyte  ifi_physical;
    ubyte  ifi_addrlen;
    ubyte  ifi_hdrlen;
    ubyte  ifi_link_state;
    ubyte  ifi_vhid;
    ushort ifi_datalen;
    uint   ifi_mtu;
    uint   ifi_metric;
    ulong  ifi_baudrate;

    ulong ifi_ipackets;
    ulong ifi_ierrors;
    ulong ifi_opackets;
    ulong ifi_oerrors;
    ulong ifi_collisions;
    ulong ifi_ibytes;
    ulong ifi_obytes;
    ulong ifi_imcasts;
    ulong ifi_omcasts;
    ulong ifi_iqdrops;
    ulong ifi_oqdrops;
    ulong ifi_noproto;
    ulong ifi_hwassist;

    union
    {
        time_t ifi_epoch;
        private ulong  ph;
    }

    union
    {
        timeval ifi_lastchange;
        struct
        {
            private ulong ph1;
            private ulong ph2;
        }
    }
}

enum IFF_UP          = 0x1;
enum IFF_BROADCAST   = 0x2;
enum IFF_DEBUG       = 0x4;
enum IFF_LOOPBACK    = 0x8;
enum IFF_POINTOPOINT = 0x10;
enum IFF_NEEDSEPOCH  = 0x20;
enum IFF_DRV_RUNNING = 0x40;
enum IFF_NOARP       = 0x80;
enum IFF_PROMISC     = 0x100;
enum IFF_ALLMULTI    = 0x200;
enum IFF_DRV_OACTIVE = 0x400;
enum IFF_SIMPLEX     = 0x800;
enum IFF_LINK0       = 0x1000;
enum IFF_LINK1       = 0x2000;
enum IFF_LINK2       = 0x4000;
enum IFF_ALTPHYS     = IFF_LINK2;
enum IFF_MULTICAST   = 0x8000;
enum IFF_CANTCONFIG  = 0x10000;
enum IFF_PPROMISC    = 0x20000;
enum IFF_MONITOR     = 0x40000;
enum IFF_STATICARP   = 0x80000;
enum IFF_STICKYARP   = 0x100000;
enum IFF_DYING       = 0x200000;
enum IFF_RENAMING    = 0x400000;
enum IFF_SPARE       = 0x800000;
enum IFF_NETLINK_1   = 0x1000000;

enum IFF_RUNNING = IFF_DRV_RUNNING;
enum IFF_OACTIVE = IFF_DRV_OACTIVE;

enum IFF_CANTCHANGE = IFF_BROADCAST |
                      IFF_POINTOPOINT |
                      IFF_DRV_RUNNING |
                      IFF_DRV_OACTIVE |
                      IFF_SIMPLEX |
                      IFF_MULTICAST |
                      IFF_ALLMULTI |
                      IFF_PROMISC |
                      IFF_DYING |
                      IFF_CANTCONFIG |
                      IFF_NEEDSEPOCH;

enum LINK_STATE_UNKNOWN = 0;
enum LINK_STATE_DOWN    = 1;
enum LINK_STATE_UP      = 2;

auto IF_Kbps(T)(T x) { return uintmax_t(x) * 1000; }
auto IF_Mbps(T)(T x) { return IF_Kbps(x * 1000); }
auto IF_Gbps(T)(T x) { return IF_Mbps(x * 1000); }

enum IFCAP_B_RXCSUM         = 0;
enum IFCAP_B_TXCSUM         = 1;
enum IFCAP_B_NETCONS        = 2;
enum IFCAP_B_VLAN_MTU       = 3;
enum IFCAP_B_VLAN_HWTAGGING = 4;
enum IFCAP_B_JUMBO_MTU      = 5;
enum IFCAP_B_POLLING        = 6;
enum IFCAP_B_VLAN_HWCSUM    = 7;
enum IFCAP_B_TSO4           = 8;
enum IFCAP_B_TSO6           = 9;
enum IFCAP_B_LRO            = 10;
enum IFCAP_B_WOL_UCAST      = 11;
enum IFCAP_B_WOL_MCAST      = 12;
enum IFCAP_B_WOL_MAGIC      = 13;
enum IFCAP_B_TOE4           = 14;
enum IFCAP_B_TOE6           = 15;
enum IFCAP_B_VLAN_HWFILTER  = 16;
enum IFCAP_B_NV             = 17;
enum IFCAP_B_VLAN_HWTSO     = 18;
enum IFCAP_B_LINKSTATE      = 19;
enum IFCAP_B_NETMAP         = 20;
enum IFCAP_B_RXCSUM_IPV6    = 21;
enum IFCAP_B_TXCSUM_IPV6    = 22;
enum IFCAP_B_HWSTATS        = 23;
enum IFCAP_B_TXRTLMT        = 24;
enum IFCAP_B_HWRXTSTMP      = 25;
enum IFCAP_B_MEXTPG         = 26;
enum IFCAP_B_TXTLS4         = 27;
enum IFCAP_B_TXTLS6         = 28;
enum IFCAP_B_VXLAN_HWCSUM   = 29;
enum IFCAP_B_VXLAN_HWTSO    = 30;
enum IFCAP_B_TXTLS_RTLMT    = 31;
enum IFCAP_B_RXTLS4         = 32;
enum IFCAP_B_RXTLS6         = 33;
enum __IFCAP_B_SIZE         = 34;

// IFCAP_B_MAX is defined in net/if.h, but __IFCAP_B_MAX doesn't seem to be defined anywhere.
// enum IFCAP_B_MAX = __IFCAP_B_MAX - 1;
enum IFCAP_B_SIZE = __IFCAP_B_SIZE;

auto IFCAP_BIT(T)(T x) { return 1 << x; }

enum IFCAP_RXCSUM         = IFCAP_BIT(IFCAP_B_RXCSUM);
enum IFCAP_TXCSUM         = IFCAP_BIT(IFCAP_B_TXCSUM);
enum IFCAP_NETCONS        = IFCAP_BIT(IFCAP_B_NETCONS);
enum IFCAP_VLAN_MTU       = IFCAP_BIT(IFCAP_B_VLAN_MTU);
enum IFCAP_VLAN_HWTAGGING = IFCAP_BIT(IFCAP_B_VLAN_HWTAGGING);
enum IFCAP_JUMBO_MTU      = IFCAP_BIT(IFCAP_B_JUMBO_MTU);
enum IFCAP_POLLING        = IFCAP_BIT(IFCAP_B_POLLING);
enum IFCAP_VLAN_HWCSUM    = IFCAP_BIT(IFCAP_B_VLAN_HWCSUM);
enum IFCAP_TSO4           = IFCAP_BIT(IFCAP_B_TSO4);
enum IFCAP_TSO6           = IFCAP_BIT(IFCAP_B_TSO6);
enum IFCAP_LRO            = IFCAP_BIT(IFCAP_B_LRO);
enum IFCAP_WOL_UCAST      = IFCAP_BIT(IFCAP_B_WOL_UCAST);
enum IFCAP_WOL_MCAST      = IFCAP_BIT(IFCAP_B_WOL_MCAST);
enum IFCAP_WOL_MAGIC      = IFCAP_BIT(IFCAP_B_WOL_MAGIC);
enum IFCAP_TOE4           = IFCAP_BIT(IFCAP_B_TOE4);
enum IFCAP_TOE6           = IFCAP_BIT(IFCAP_B_TOE6);
enum IFCAP_VLAN_HWFILTER  = IFCAP_BIT(IFCAP_B_VLAN_HWFILTER);
enum IFCAP_NV             = IFCAP_BIT(IFCAP_B_NV);
enum IFCAP_VLAN_HWTSO     = IFCAP_BIT(IFCAP_B_VLAN_HWTSO);
enum IFCAP_LINKSTATE      = IFCAP_BIT(IFCAP_B_LINKSTATE);
enum IFCAP_NETMAP         = IFCAP_BIT(IFCAP_B_NETMAP);
enum IFCAP_RXCSUM_IPV6    = IFCAP_BIT(IFCAP_B_RXCSUM_IPV6);
enum IFCAP_TXCSUM_IPV6    = IFCAP_BIT(IFCAP_B_TXCSUM_IPV6);
enum IFCAP_HWSTATS        = IFCAP_BIT(IFCAP_B_HWSTATS);
enum IFCAP_TXRTLMT        = IFCAP_BIT(IFCAP_B_TXRTLMT);
enum IFCAP_HWRXTSTMP      = IFCAP_BIT(IFCAP_B_HWRXTSTMP);
enum IFCAP_MEXTPG         = IFCAP_BIT(IFCAP_B_MEXTPG);
enum IFCAP_TXTLS4         = IFCAP_BIT(IFCAP_B_TXTLS4);
enum IFCAP_TXTLS6         = IFCAP_BIT(IFCAP_B_TXTLS6);
enum IFCAP_VXLAN_HWCSUM   = IFCAP_BIT(IFCAP_B_VXLAN_HWCSUM);
enum IFCAP_VXLAN_HWTSO    = IFCAP_BIT(IFCAP_B_VXLAN_HWTSO);
enum IFCAP_TXTLS_RTLMT    = IFCAP_BIT(IFCAP_B_TXTLS_RTLMT);

enum IFCAP2_RXTLS4 = IFCAP_B_RXTLS4 - 32;
enum IFCAP2_RXTLS6 = IFCAP_B_RXTLS6 - 32;

auto IFCAP2_BIT(T)(T x) { return 1UL << x; }

enum IFCAP_HWCSUM_IPV6 = IFCAP_RXCSUM_IPV6 | IFCAP_TXCSUM_IPV6;

enum IFCAP_HWCSUM = IFCAP_RXCSUM | IFCAP_TXCSUM;
enum IFCAP_TSO    = IFCAP_TSO4 | IFCAP_TSO6;
enum IFCAP_WOL    = IFCAP_WOL_UCAST | IFCAP_WOL_MCAST | IFCAP_WOL_MAGIC;
enum IFCAP_TOE    = IFCAP_TOE4 | IFCAP_TOE6;
enum IFCAP_TXTLS  = IFCAP_TXTLS4 | IFCAP_TXTLS6;

enum IFCAP_CANTCHANGE = IFCAP_NETMAP | IFCAP_NV;
enum IFCAP_ALLCAPS    = 0xffffffff;

enum IFQ_MAXLEN   = 50;
enum IFNET_SLOWHZ = 1;

struct if_msghdr
{
    ushort  ifm_msglen;
    ubyte   ifm_version;
    ubyte   ifm_type;
    int     ifm_addrs;
    int     ifm_flags;
    ushort  ifm_index;
    ushort  _ifm_spare1;
    if_data ifm_data;
}

auto IF_MSGHDRL_IFM_DATA(T)(T _l) { return cast(if_data*) (cast(ubyte*) _l + _l.ifm_data_off); }

auto IF_MSGHDRL_RTA(T)(T_l) { return cast(void*) (cast(uintptr_t) _l + _l.ifm_len); }

struct if_msghdrl
{
    ushort  ifm_msglen;
    ubyte   ifm_version;
    ubyte   ifm_type;
    int     ifm_addrs;
    int     ifm_flags;
    ushort  ifm_index;
    ushort  _ifm_spare1;
    ushort  ifm_len;
    ushort  ifm_data_off;
    int     _ifm_spare2;
    if_data ifm_data;
}

struct ifa_msghdr
{
    ushort ifam_msglen;
    ubyte  ifam_version;
    ubyte  ifam_type;
    int    ifam_addrs;
    int    ifam_flags;
    ushort ifam_index;
    ushort _ifam_spare1;
    int    ifam_metric;
}

auto IFA_MSGHDRL_IFAM_DATA(T)(T _l) { return cast(if_data*) (cast(ubyte*) _l + _l.ifam_data_off); }

auto IFA_MSGHDRL_RTA(T)(T _l) { return cast(void*) (cast(uintptr_t) _l + _l.ifam_len); }

struct ifa_msghdrl
{
    ushort  ifam_msglen;
    ubyte   ifam_version;
    ubyte   ifam_type;
    int     ifam_addrs;
    int     ifam_flags;
    ushort  ifam_index;
    ushort  _ifam_spare1;
    ushort  ifam_len;
    ushort  ifam_data_off;
    int     ifam_metric;
    if_data ifam_data;
}

struct ifma_msghdr
{
    ushort ifmam_msglen;
    ubyte  ifmam_version;
    ubyte  ifmam_type;
    int    ifmam_addrs;
    int    ifmam_flags;
    ushort ifmam_index;
    ushort _ifmam_spare1;
}

struct if_announcemsghdr
{
    ushort            ifan_msglen;
    ubyte             ifan_version;
    ubyte             ifan_type;
    ushort            ifan_index;
    char[IF_NAMESIZE] ifan_name;
    ushort            ifan_what;
}

enum IFAN_ARRIVAL   = 0;
enum IFAN_DEPARTURE = 1;

struct ifreq_buffer
{
    size_t length;
    void*  buffer;
}

struct ifreq_nv_req
{
    uint  buf_length;
    uint  length;
    void* buffer;
}

enum IFR_CAP_NV_MAXBUFSIZE = 2 * 1024 * 1024;

struct ifreq
{
    char[IF_NAMESIZE] ifr_name;

    union
    {
        sockaddr         ifr_addr;
        sockaddr         ifr_dstaddr;
        sockaddr         ifr_broadaddr;
        ifreq_buffer     ifr_buffer;
        private short[2] ifru_flags;
        short            ifr_index;
        int              ifr_jid;
        int              ifr_metric;
        int              ifr_mtu;
        int              ifr_phys;
        int              ifr_media;
        caddr_t          ifr_data;
        private int[2]   ifru_cap;
        uint             ifr_fib;
        ubyte            ifr_vlan_pcp;
        ifreq_nv_req     ifr_cap_nv;
    }

    @property ref ifr_flags()     { return ifru_flags[0]; }
    @property ref ifr_flagshigh() { return ifru_flags[1]; }
    @property ref ifr_reqcap()    { return ifru_cap[0]; }
    @property ref ifr_curcap()    { return ifru_cap[1]; }
    alias ifr_lan_pcp = ifr_vlan_pcp;
}

auto _SIZEOF_ADDR_IFREQ(T)(T ifr) { return ifr.ifr_addr.sa_len > sockaddr.sizeof ?
                                           ifreq.sizeof - sockaddr.sizeof + ifr.ifr_addr.sa_len :
                                           ifreq.sizeof; }

struct ifaliasreq
{
    char[IF_NAMESIZE] ifra_name;
    sockaddr          ifra_addr;
    sockaddr          ifra_broadaddr;
    sockaddr          ifra_mask;
    int               ifra_vhid;
}

struct oifaliasreq
{
    char[IF_NAMESIZE] ifra_name;
    sockaddr          ifra_addr;
    sockaddr          ifra_broadaddr;
    sockaddr          ifra_mask;
 }

struct ifmediareq
{
    char[IF_NAMESIZE] ifm_name;
    int               ifm_current;
    int               ifm_mask;
    int               ifm_status;
    int               ifm_active;
    int               ifm_count;
    int*              ifm_ulist;
}

struct ifdrv
{
    char[IF_NAMESIZE] ifd_name;
    c_ulong           ifd_cmd;
    size_t            ifd_len;
    void*             ifd_data;
}

enum IFSTATMAX = 800;
struct ifstat
{
    char[IF_NAMESIZE]   ifs_name;
    char[IFSTATMAX + 1] ascii;
}

struct ifconf
{
    int ifc_len;
    union
    {
        caddr_t ifc_buf;
        ifreq*  ifc_req;
    }
}

enum IFG_ALL = "all";
enum IFG_EGRESS = "egress";

struct ifg_req
{
    union
    {
        char[IF_NAMESIZE] ifgrq_group;
        char[IF_NAMESIZE] ifgrq_member;
    }
}

struct ifgroupreq
{
    char[IF_NAMESIZE] ifgr_name;
    uint              ifgr_len;
    union
    {
        char[IF_NAMESIZE] ifgr_group;
        ifg_req*          ifgr_groups;
    }
}

struct ifi2creq
{
    ubyte    dev_addr;
    ubyte    offset;
    ubyte    len;
    ubyte    spare0;
    uint     spare1;
    ubyte[8] data;
}

enum RSS_FUNC_NONE     = 0;
enum RSS_FUNC_PRIVATE  = 1;
enum RSS_FUNC_TOEPLITZ = 2;

enum RSS_TYPE_IPV4        = 0x00000001;
enum RSS_TYPE_TCP_IPV4    = 0x00000002;
enum RSS_TYPE_IPV6        = 0x00000004;
enum RSS_TYPE_IPV6_EX     = 0x00000008;
enum RSS_TYPE_TCP_IPV6    = 0x00000010;
enum RSS_TYPE_TCP_IPV6_EX = 0x00000020;
enum RSS_TYPE_UDP_IPV4    = 0x00000040;
enum RSS_TYPE_UDP_IPV6    = 0x00000080;
enum RSS_TYPE_UDP_IPV6_EX = 0x00000100;

enum RSS_KEYLEN = 128;

struct ifrsskey
{
    char[IF_NAMESIZE]  ifrk_name;
    ubyte              ifrk_func;
    ubyte              ifrk_spare0;
    ushort             ifrk_keylen;
    ubyte[RSS_KEYLEN]  ifrk_key;
}

struct ifrsshash
{
    char[IF_NAMESIZE] ifrh_name;
    ubyte             ifrh_func;
    ubyte             ifrh_spare0;
    ushort            ifrh_spare1;
    uint              ifrh_types;
}

enum IFNET_PCP_NONE = 0xff;

enum IFDR_MSG_SIZE      = 64;
enum IFDR_REASON_MSG    = 1;
enum IFDR_REASON_VENDOR = 2;

struct ifdownreason
{
    char[IF_NAMESIZE]   ifdr_name;
    uint                ifdr_reason;
    uint                ifdr_vendor;
    char[IFDR_MSG_SIZE] ifdr_msg;
}

// FIXME It's not clear where ifnet is supposed to be coming from, so this is
// commented out for now.
//alias if_t = ifnet*;

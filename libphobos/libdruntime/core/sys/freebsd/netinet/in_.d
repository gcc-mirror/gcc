//Written in the D programming language

/++
    D header file for FreeBSD's extensions to POSIX's netinet/in.h.

    Copyright: Copyright 2017 -
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTP jmdavisprog.com, Jonathan M Davis)
 +/
module core.sys.freebsd.netinet.in_;

import core.sys.freebsd.sys.cdefs;

public import core.sys.posix.netinet.in_;

version (FreeBSD):

extern(C) nothrow @nogc:

static if (__BSD_VISIBLE)
{
    enum IPPROTO_HOPOPTS   = 0;

    enum IPPROTO_IPV4      = 4;
    enum IPPROTO_IPIP      = IPPROTO_IPV4;
    enum IPPROTO_ST        = 7;
    enum IPPROTO_EGP       = 8;
    enum IPPROTO_PIGP      = 9;
    enum IPPROTO_RCCMON    = 10;
    enum IPPROTO_NVPII     = 11;

    enum IPPROTO_ARGUS     = 13;
    enum IPPROTO_EMCON     = 14;
    enum IPPROTO_XNET      = 15;
    enum IPPROTO_CHAOS     = 16;
    enum IPPROTO_MUX       = 18;
    enum IPPROTO_MEAS      = 19;
    enum IPPROTO_HMP       = 20;
    enum IPPROTO_PRM       = 21;

    enum IPPROTO_TRUNK1    = 23;
    enum IPPROTO_TRUNK2    = 24;
    enum IPPROTO_LEAF1     = 25;
    enum IPPROTO_LEAF2     = 26;
    enum IPPROTO_RDP       = 27;
    enum IPPROTO_IRTP      = 28;
    enum IPPROTO_TP        = 29;
    enum IPPROTO_BLT       = 30;
    enum IPPROTO_NSP       = 31;
    enum IPPROTO_INP       = 32;
    enum IPPROTO_SEP       = 33;
    enum IPPROTO_3PC       = 34;
    enum IPPROTO_IDPR      = 35;
    enum IPPROTO_XTP       = 36;
    enum IPPROTO_DDP       = 37;
    enum IPPROTO_CMTP      = 38;
    enum IPPROTO_TPXX      = 39;
    enum IPPROTO_IL        = 40;
    enum IPPROTO_SDRP      = 42;
    enum IPPROTO_ROUTING   = 43;
    enum IPPROTO_FRAGMENT  = 44;
    enum IPPROTO_IDRP      = 45;
    enum IPPROTO_RSVP      = 46;
    enum IPPROTO_GRE       = 47;
    enum IPPROTO_MHRP      = 48;
    enum IPPROTO_BHA       = 49;
    enum IPPROTO_ESP       = 50;
    enum IPPROTO_AH        = 51;
    enum IPPROTO_INLSP     = 52;
    enum IPPROTO_SWIPE     = 53;
    enum IPPROTO_NHRP      = 54;
    enum IPPROTO_MOBILE    = 55;
    enum IPPROTO_TLSP      = 56;
    enum IPPROTO_SKIP      = 57;
    enum IPPROTO_ICMPV6    = 58;
    enum IPPROTO_NONE      = 59;
    enum IPPROTO_DSTOPTS   = 60;
    enum IPPROTO_AHIP      = 61;
    enum IPPROTO_CFTP      = 62;
    enum IPPROTO_HELLO     = 63;
    enum IPPROTO_SATEXPAK  = 64;
    enum IPPROTO_KRYPTOLAN = 65;
    enum IPPROTO_RVD       = 66;
    enum IPPROTO_IPPC      = 67;
    enum IPPROTO_ADFS      = 68;
    enum IPPROTO_SATMON    = 69;
    enum IPPROTO_VISA      = 70;
    enum IPPROTO_IPCV      = 71;
    enum IPPROTO_CPNX      = 72;
    enum IPPROTO_CPHB      = 73;
    enum IPPROTO_WSN       = 74;
    enum IPPROTO_PVP       = 75;
    enum IPPROTO_BRSATMON  = 76;

    enum IPPROTO_WBMON     = 78;
    enum IPPROTO_WBEXPAK   = 79;
    enum IPPROTO_EON       = 80;
    enum IPPROTO_VMTP      = 81;
    enum IPPROTO_SVMTP     = 82;
    enum IPPROTO_VINES     = 83;
    enum IPPROTO_TTP       = 84;
    enum IPPROTO_IGP       = 85;
    enum IPPROTO_DGP       = 86;
    enum IPPROTO_TCF       = 87;
    enum IPPROTO_IGRP      = 88;
    enum IPPROTO_OSPFIGP   = 89;
    enum IPPROTO_SRPC      = 90;
    enum IPPROTO_LARP      = 91;
    enum IPPROTO_MTP       = 92;
    enum IPPROTO_AX25      = 93;
    enum IPPROTO_IPEIP     = 94;
    enum IPPROTO_MICP      = 95;
    enum IPPROTO_SCCSP     = 96;
    enum IPPROTO_ETHERIP   = 97;
    enum IPPROTO_ENCAP     = 98;
    enum IPPROTO_APES      = 99;
    enum IPPROTO_GMTP      = 100;
    enum IPPROTO_IPCOMP    = 108;
    enum IPPROTO_SCTP      = 132;
    enum IPPROTO_MH        = 135;
    enum IPPROTO_UDPLITE   = 136;
    enum IPPROTO_HIP       = 139;
    enum IPPROTO_SHIM6     = 140;

    enum IPPROTO_PIM          = 103;
    enum IPPROTO_CARP         = 112;
    enum IPPROTO_PGM          = 113;
    enum IPPROTO_MPLS         = 137;
    enum IPPROTO_PFSYNC       = 240;
    enum IPPROTO_RESERVED_253 = 253;
    enum IPPROTO_RESERVED_254 = 254;

    enum IPPROTO_DONE = 257;

    enum IPPORT_RESERVED = 1024;

    enum IPPORT_EPHEMERALFIRST = 10000;
    enum IPPORT_EPHEMERALLAST  = 65535;

    enum IPPORT_HIFIRSTAUTO = 49152;
    enum IPPORT_HILASTAUTO  = 65535;

    enum IPPORT_RESERVEDSTART = 600;

    enum IPPORT_MAX = 65535;

    extern(D) bool IN_CLASSA(in_addr_t i) pure @safe { return (i & 0x80000000) == 0; }
    enum IN_CLASSA_NET    = 0xff000000;
    enum IN_CLASSA_NSHIFT = 24;
    enum IN_CLASSA_HOST   = 0x00ffffff;
    enum IN_CLASSA_MAX    = 128;

    extern(D) bool IN_CLASSB(in_addr_t i) pure @safe { return (i & 0xc0000000) == 0x80000000; }
    enum IN_CLASSB_NET    = 0xffff0000;
    enum IN_CLASSB_NSHIFT = 16;
    enum IN_CLASSB_HOST   = 0x0000ffff;
    enum IN_CLASSB_MAX    = 65536;

    extern(D) bool IN_CLASSC(in_addr_t i) pure @safe { return (i & 0xe0000000) == 0xc0000000; }
    enum IN_CLASSC_NET    = 0xffffff00;
    enum IN_CLASSC_NSHIFT = 8;
    enum IN_CLASSC_HOST   = 0x000000ff;

    extern(D) bool IN_CLASSD(in_addr_t i) pure @safe { return (i & 0xf0000000) == 0xe0000000; }
    enum IN_CLASSD_NET     = 0xf0000000;
    enum IN_CLASSD_NSHIFT  = 28;
    enum IN_CLASSD_HOST    = 0x0fffffff;
    extern(D) bool IN_MULTICAST(in_addr_t i) { return IN_CLASSD(i); }

    // The fact that these are identical looks suspicious (they're not quite
    // identical on Linux). However, this _is_ how they're defined in FreeBSD
    // and on OS X. So, while it _might_ be a bug, if it is, it's an upstream
    // one, and we're compatible with it.
    extern(D) bool IN_EXPERIMENTAL(in_addr_t i) pure @safe { return (i & 0xf0000000) == 0xf0000000; }
    extern(D) bool IN_BADCLASS(in_addr_t i) pure @safe { return (i & 0xf0000000) == 0xf0000000; }

    extern(D) bool IN_LINKLOCAL(in_addr_t i) pure @safe { return (i & 0xffff0000) == 0xa9fe0000; }
    extern(D) bool IN_LOOPBACK(in_addr_t i) pure @safe { return (i & 0xff000000) == 0x7f000000; }
    extern(D) bool IN_ZERONET(in_addr_t i) pure @safe { return (i & 0xff000000) == 0; }

    extern(D) bool IN_PRIVATE(in_addr_t i) pure @safe
    {
        return  (i & 0xff000000) == 0x0a000000 ||
                (i & 0xfff00000) == 0xac100000 ||
                (i & 0xffff0000) == 0xc0a80000;
    }

    extern(D) bool IN_LOCAL_GROUP(in_addr_t i) pure @safe { return (i & 0xffffff00) == 0xe0000000; }

    extern(D) bool IN_ANY_LOCAL(in_addr_t i) pure @safe { return IN_LINKLOCAL(i) || IN_LOCAL_GROUP(i); }

    enum INADDR_UNSPEC_GROUP    = 0xe0000000;
    enum INADDR_ALLHOSTS_GROUP  = 0xe0000001;
    enum INADDR_ALLRTRS_GROUP   = 0xe0000002;
    enum INADDR_ALLRPTS_GROUP   = 0xe0000016;
    enum INADDR_CARP_GROUP      = 0xe0000012;
    enum INADDR_PFSYNC_GROUP    = 0xe00000f0;
    enum INADDR_ALLMDNS_GROUP   = 0xe00000fb;
    enum INADDR_MAX_LOCAL_GROUP = 0xe00000ff;

    enum IN_LOOPBACKNET = 127;

    enum IN_RFC3021_MASK = 0xfffffffe;

    enum IP_OPTIONS      = 1;
    enum IP_HDRINCL      = 2;
    enum IP_TOS          = 3;
    enum IP_TTL          = 4;
    enum IP_RECVOPTS     = 5;
    enum IP_RECVRETOPTS  = 6;
    enum IP_RECVDSTADDR  = 7;
    enum IP_SENDSRCADDR  = IP_RECVDSTADDR;
    enum IP_RETOPTS      = 8;
    enum IP_MULTICAST_IF = 9;

    enum IP_MULTICAST_TTL   = 10;
    enum IP_MULTICAST_LOOP  = 11;
    enum IP_ADD_MEMBERSHIP  = 12;
    enum IP_DROP_MEMBERSHIP = 13;
    enum IP_MULTICAST_VIF   = 14;
    enum IP_RSVP_ON         = 15;
    enum IP_RSVP_OFF        = 16;
    enum IP_RSVP_VIF_ON     = 17;
    enum IP_RSVP_VIF_OFF    = 18;
    enum IP_PORTRANGE       = 19;
    enum IP_RECVIF          = 20;

    enum IP_IPSEC_POLICY = 21;

    enum IP_ONESBCAST         = 23;
    enum IP_BINDANY           = 24;
    enum IP_BINDMULTI         = 25;
    enum IP_RSS_LISTEN_BUCKET = 26;
    enum IP_ORIGDSTADDR       = 27;
    enum IP_RECVORIGDSTADDR   = IP_ORIGDSTADDR;

    enum IP_FW3       = 48;
    enum IP_DUMMYNET3 = 49;

    enum IP_ADD_SOURCE_MEMBERSHIP  = 70;
    enum IP_DROP_SOURCE_MEMBERSHIP = 71;
    enum IP_BLOCK_SOURCE           = 72;
    enum IP_UNBLOCK_SOURCE         = 73;

    enum MCAST_JOIN_GROUP         = 80;
    enum MCAST_LEAVE_GROUP        = 81;
    enum MCAST_JOIN_SOURCE_GROUP  = 82;
    enum MCAST_LEAVE_SOURCE_GROUP = 83;
    enum MCAST_BLOCK_SOURCE       = 84;
    enum MCAST_UNBLOCK_SOURCE     = 85;

    enum IP_FLOWID          = 90;
    enum IP_FLOWTYPE        = 91;
    enum IP_RSSBUCKETID     = 92;
    enum IP_RECVFLOWID      = 93;
    enum IP_RECVRSSBUCKETID = 94;

    enum IP_DEFAULT_MULTICAST_TTL  = 1;
    enum IP_DEFAULT_MULTICAST_LOOP = 1;

    enum IP_MIN_MEMBERSHIPS   = 31;
    enum IP_MAX_MEMBERSHIPS   = 4095;

    enum IP_MAX_GROUP_SRC_FILTER = 512;
    enum IP_MAX_SOCK_SRC_FILTER  = 128;

    struct ip_mreq
    {
        in_addr imr_multiaddr;
        in_addr imr_interface;
    };

    struct ip_mreqn
    {
        in_addr imr_multiaddr;
        in_addr imr_address;
        int     imr_ifindex;
    };

    struct ip_mreq_source
    {
        in_addr imr_multiaddr;
        in_addr imr_sourceaddr;
        in_addr imr_interface;
    };

    struct group_req
    {
        uint gr_interface;
        sockaddr_storage gr_group;
    };

    struct group_source_req
    {
        uint gsr_interface;
        sockaddr_storage gsr_group;
        sockaddr_storage gsr_source;
    };

    int setipv4sourcefilter(int, in_addr, in_addr, uint, uint, in_addr*);
    int getipv4sourcefilter(int, in_addr, in_addr, uint*, uint*, in_addr*);
    int setsourcefilter(int, uint, sockaddr*, socklen_t, uint, uint, sockaddr_storage*);
    int getsourcefilter(int, uint, sockaddr*, socklen_t, uint*, uint*, sockaddr_storage*);

    enum MCAST_UNDEFINED = 0;
    enum MCAST_INCLUDE   = 1;
    enum MCAST_EXCLUDE   = 2;

    enum IP_PORTRANGE_DEFAULT = 0;
    enum IP_PORTRANGE_HIGH    = 1;
    enum IP_PORTRANGE_LOW     = 2;

    enum IPCTL_FORWARDING        = 1;
    enum IPCTL_SENDREDIRECTS     = 2;
    enum IPCTL_DEFTTL            = 3;
    enum IPCTL_DEFMTU            = 4;
    enum IPCTL_SOURCEROUTE       = 8;
    enum IPCTL_DIRECTEDBROADCAST = 9;
    enum IPCTL_INTRQMAXLEN       = 10;
    enum IPCTL_INTRQDROPS        = 11;
    enum IPCTL_STATS             = 12;
    enum IPCTL_ACCEPTSOURCEROUTE = 13;
    enum IPCTL_FASTFORWARDING    = 14;
    enum IPCTL_GIF_TTL           = 16;
    enum IPCTL_INTRDQMAXLEN      = 17;
    enum IPCTL_INTRDQDROPS       = 18;
}

// =============================================================================
// What follows is from netinet6/in6.h, but since netinet6/in6.h specifically
// says that it should only be #included by #including netinet/in.h, it makes
// more sense to put them in here so that the way they're imported corresponds
// with the correct way of #including them in C/C++.
// =============================================================================

// The #if was around the #include of in6.h at the end of in.h.
static if (__POSIX_VISIBLE)
{
    static if (__BSD_VISIBLE)
    {
        enum IPV6PORT_RESERVED    = 1024;
        enum IPV6PORT_ANONMIN     = 49152;
        enum IPV6PORT_ANONMAX     = 65535;
        enum IPV6PORT_RESERVEDMIN = 600;
        enum IPV6PORT_RESERVEDMAX = IPV6PORT_RESERVED - 1;

        enum IN6ADDR_ANY_INIT = in6_addr.init;
        enum IN6ADDR_LOOPBACK_INIT = in6_addr([0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                                               0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]);
        enum IN6ADDR_NODELOCAL_ALLNODES_INIT = in6_addr([0xff, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                                                         0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]);
        enum IN6ADDR_INTFACELOCAL_ALLNODES_INIT = in6_addr([0xff, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                                                            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]);
        enum IN6ADDR_LINKLOCAL_ALLNODES_INIT = in6_addr([0xff, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                                                         0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]);
        enum IN6ADDR_LINKLOCAL_ALLROUTERS_INIT = in6_addr([0xff, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                                                           0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02]);
        enum IN6ADDR_LINKLOCAL_ALLV2ROUTERS_INIT = in6_addr([0xff, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                                                             0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x16]);

        __gshared const in6_addr in6addr_nodelocal_allnodes;
        __gshared const in6_addr in6addr_linklocal_allnodes;
        __gshared const in6_addr in6addr_linklocal_allrouters;
        __gshared const in6_addr in6addr_linklocal_allv2routers;

        extern(D) bool IN6_ARE_ADDR_EQUAL(in6_addr* a, in6_addr* b) pure @safe { return *a == *b; }
    }

    enum __IPV6_ADDR_SCOPE_NODELOCAL    = 0x01;
    enum __IPV6_ADDR_SCOPE_INTFACELOCAL = 0x01;
    enum __IPV6_ADDR_SCOPE_LINKLOCAL    = 0x02;
    enum __IPV6_ADDR_SCOPE_SITELOCAL    = 0x05;
    enum __IPV6_ADDR_SCOPE_GLOBAL       = 0x0e;

    static if (__BSD_VISIBLE)
    {
// TODO - requires declarations from elsewhere that we don't currently have bindings for.
/+
        struct route_in6
        {
            rtentry* ro_rt;
            llentry* ro_lle;
            char*    ro_prepend;
            ushort   ro_plen;
            ushort   ro_flags;
            ushort   ro_mtu;
            ushort   spare;
            sockaddr_in6 ro_dst;
        };
 +/
    }

    enum IPV6_SOCKOPT_RESERVED1 = 3;
    enum IPV6_PORTRANGE         = 14;
    enum ICMP6_FILTER           = 18;

    enum IPV6_CHECKSUM = 26;

    enum IPV6_IPSEC_POLICY = 28;

    enum IPV6_FW_ADD   = 30;
    enum IPV6_FW_DEL   = 31;
    enum IPV6_FW_FLUSH = 32;
    enum IPV6_FW_ZERO  = 33;
    enum IPV6_FW_GET   = 34;

    enum IPV6_RTHDRDSTOPTS = 35;

    enum IPV6_RECVPKTINFO  = 36;
    enum IPV6_RECVHOPLIMIT = 37;
    enum IPV6_RECVRTHDR    = 38;
    enum IPV6_RECVHOPOPTS  = 39;
    enum IPV6_RECVDSTOPTS  = 40;

    enum IPV6_USE_MIN_MTU = 42;
    enum IPV6_RECVPATHMTU = 43;

    enum IPV6_PATHMTU = 44;

    enum IPV6_PKTINFO  = 46;
    enum IPV6_HOPLIMIT = 47;
    enum IPV6_NEXTHOP  = 48;
    enum IPV6_HOPOPTS  = 49;
    enum IPV6_DSTOPTS  = 50;
    enum IPV6_RTHDR    = 51;

    enum IPV6_RECVTCLASS = 57;

    enum IPV6_AUTOFLOWLABEL = 59;

    enum IPV6_TCLASS   = 61;
    enum IPV6_DONTFRAG = 62;

    enum IPV6_PREFER_TEMPADDR = 63;

    enum IPV6_BINDANY   = 64;

    enum IPV6_BINDMULTI         = 65;
    enum IPV6_RSS_LISTEN_BUCKET = 66;
    enum IPV6_FLOWID            = 67;
    enum IPV6_FLOWTYPE          = 68;
    enum IPV6_RSSBUCKETID       = 69;
    enum IPV6_RECVFLOWID        = 70;
    enum IPV6_RECVRSSBUCKETID   = 71;

    enum IPV6_ORIGDSTADDR     = 72;
    enum IPV6_RECVORIGDSTADDR = IPV6_ORIGDSTADDR;

    enum IPV6_RTHDR_LOOSE  = 0;
    enum IPV6_RTHDR_STRICT = 1;
    enum IPV6_RTHDR_TYPE_0 = 0;

    enum IPV6_DEFAULT_MULTICAST_HOPS = 1;
    enum IPV6_DEFAULT_MULTICAST_LOOP = 1;

    enum IPV6_MIN_MEMBERSHIPS = 31;
    enum IPV6_MAX_MEMBERSHIPS = 4095;

    enum IPV6_MAX_GROUP_SRC_FILTER = 512;
    enum IPV6_MAX_SOCK_SRC_FILTER  = 128;

    struct in6_pktinfo
    {
        in6_addr ipi6_addr;
        uint     ipi6_ifindex;
    };

    struct ip6_mtuinfo
    {
        sockaddr_in6 ip6m_addr;
        uint         ip6m_mtu;
    };

    enum IPV6_PORTRANGE_DEFAULT = 0;
    enum IPV6_PORTRANGE_HIGH    = 1;
    enum IPV6_PORTRANGE_LOW     = 2;

    static if (__BSD_VISIBLE)
    {
        enum IPV6PROTO_MAXID =  IPPROTO_PIM + 1;

        enum IPV6CTL_FORWARDING         = 1;
        enum IPV6CTL_SENDREDIRECTS      = 2;
        enum IPV6CTL_DEFHLIM            = 3;
        enum IPV6CTL_DEFMTU             = 4;
        enum IPV6CTL_FORWSRCRT          = 5;
        enum IPV6CTL_STATS              = 6;
        enum IPV6CTL_MRTSTATS           = 7;
        enum IPV6CTL_MRTPROTO           = 8;
        enum IPV6CTL_MAXFRAGPACKETS     = 9;
        enum IPV6CTL_SOURCECHECK        = 10;
        enum IPV6CTL_SOURCECHECK_LOGINT = 11;
        enum IPV6CTL_ACCEPT_RTADV       = 12;

        enum IPV6CTL_LOG_INTERVAL   = 14;
        enum IPV6CTL_HDRNESTLIMIT   = 15;
        enum IPV6CTL_DAD_COUNT      = 16;
        enum IPV6CTL_AUTO_FLOWLABEL = 17;
        enum IPV6CTL_DEFMCASTHLIM   = 18;
        enum IPV6CTL_GIF_HLIM       = 19;
        enum IPV6CTL_KAME_VERSION   = 20;
        enum IPV6CTL_USE_DEPRECATED = 21;
        enum IPV6CTL_RR_PRUNE       = 22;
        enum IPV6CTL_V6ONLY         = 24;

        enum IPV6CTL_USETEMPADDR     = 32;
        enum IPV6CTL_TEMPPLTIME      = 33;
        enum IPV6CTL_TEMPVLTIME      = 34;
        enum IPV6CTL_AUTO_LINKLOCAL  = 35;
        enum IPV6CTL_RIP6STATS       = 36;
        enum IPV6CTL_PREFER_TEMPADDR = 37;
        enum IPV6CTL_ADDRCTLPOLICY   = 38;
        enum IPV6CTL_USE_DEFAULTZONE = 39;

        enum IPV6CTL_MAXFRAGS   = 41;
        enum IPV6CTL_MCAST_PMTU = 44;

        enum IPV6CTL_STEALTH = 45;

        enum ICMPV6CTL_ND6_ONLINKNSRFC4861 = 47;
        enum IPV6CTL_NO_RADR     = 48;
        enum IPV6CTL_NORBIT_RAIF = 49;

        enum IPV6CTL_RFC6204W3 = 50;

        enum IPV6CTL_INTRQMAXLEN  = 51;
        enum IPV6CTL_INTRDQMAXLEN = 52;

        enum IPV6CTL_MAXID = 53;
    }

// TODO - require declarations from elsewhere that we don't currently have bindings for.
/+
    enum M_FASTFWD_OURS  = M_PROTO1;
    enum M_IP6_NEXTHOP   = M_PROTO2;
    enum M_IP_NEXTHOP    = M_PROTO2;
    enum M_SKIP_FIREWALL = M_PROTO3;
    enum M_AUTHIPHDR     = M_PROTO4;
    enum M_DECRYPTED     = M_PROTO5;
    enum M_LOOP          = M_PROTO6;
    enum M_AUTHIPDGM     = M_PROTO7;
    enum M_RTALERT_MLD   = M_PROTO8;
 +/

    static if (__BSD_VISIBLE)
    {
        size_t inet6_rthdr_space(int, int) @trusted;
        cmsghdr* inet6_rthdr_init(void*, int);
        int inet6_rthdr_add(cmsghdr*, const in6_addr*, uint);
        int inet6_rthdr_lasthop(cmsghdr*, uint);
        int inet6_rthdr_segments(const cmsghdr*);
        in6_addr* inet6_rthdr_getaddr(cmsghdr*, int);
        int inet6_rthdr_getflags(const cmsghdr*, int);

        int inet6_opt_init(void*, socklen_t);
        int inet6_opt_append(void*, socklen_t, int, ubyte, socklen_t, ubyte, void**);
        int inet6_opt_finish(void*, socklen_t, int);
        int inet6_opt_set_val(void*, int, void*, socklen_t);

        int inet6_opt_next(void*, socklen_t, int, ubyte*, socklen_t*, void**);
        int inet6_opt_find(void*, socklen_t, int, ubyte, socklen_t*, void**);
        int inet6_opt_get_val(void*, int, void*, socklen_t);
        socklen_t inet6_rth_space(int, int) @trusted;
        void* inet6_rth_init(void*, socklen_t, int, int);
        int inet6_rth_add(void*, const in6_addr*);
        int inet6_rth_reverse(const void*, void*);
        int inet6_rth_segments(const void*);
        in6_addr* inet6_rth_getaddr(const void*, int);
    }
}

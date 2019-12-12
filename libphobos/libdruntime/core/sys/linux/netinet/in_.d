//Written in the D programming language

/++
    D header file for Linux's extensions to POSIX's netinet/in.h.

    Copyright: Copyright 2017 -
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTP jmdavisprog.com, Jonathan M Davis)
 +/
module core.sys.linux.netinet.in_;

import core.sys.posix.config;

public import core.sys.posix.netinet.in_;

version (CRuntime_Glibc)         version = linux_libc;
else version (CRuntime_Musl)     version = linux_libc;
else version (CRuntime_Bionic)   version = linux_libc;
else version (CRuntime_UClibc)   version = linux_libc;

version (CRuntime_Glibc)         version = gnu_libc;
else version (CRuntime_UClibc)   version = gnu_libc;

version (linux_libc)
{
    extern(C) nothrow @nogc:

    enum IPPROTO_IPIP    = 4;
    enum IPPROTO_EGP     = 8;
    enum IPPROTO_TP      = 29;
    enum IPPROTO_DCCP    = 33;
    enum IPPROTO_RSVP    = 46;
    enum IPPROTO_GRE     = 47;
    enum IPPROTO_ESP     = 50;
    enum IPPROTO_AH      = 51;
    enum IPPROTO_MTP     = 92;
    enum IPPROTO_BEETPH  = 94;
    enum IPPROTO_ENCAP   = 98;
    enum IPPROTO_PIM     = 103;
    enum IPPROTO_COMP    = 108;
    enum IPPROTO_SCTP    = 132;
    enum IPPROTO_UDPLITE = 136;
    enum IPPROTO_MPLS    = 137;

    enum IPPROTO_HOPOPTS  = 0;
    enum IPPROTO_ROUTING  = 43;
    enum IPPROTO_FRAGMENT = 44;
    enum IPPROTO_ICMPV6   = 58;
    enum IPPROTO_NONE     = 59;
    enum IPPROTO_DSTOPTS  = 60;
    enum IPPROTO_MH       = 135;

    enum IPPORT_ECHO       = 7;
    enum IPPORT_DISCARD    = 9;
    enum IPPORT_SYSTAT     = 11;
    enum IPPORT_DAYTIME    = 13;
    enum IPPORT_NETSTAT    = 15;
    enum IPPORT_FTP        = 21;
    enum IPPORT_TELNET     = 23;
    enum IPPORT_SMTP       = 25;
    enum IPPORT_TIMESERVER = 37;
    enum IPPORT_NAMESERVER = 42;
    enum IPPORT_WHOIS      = 43;
    enum IPPORT_MTP        = 57;

    enum IPPORT_TFTP    = 69;
    enum IPPORT_RJE     = 77;
    enum IPPORT_FINGER  = 79;
    enum IPPORT_TTYLINK = 87;
    enum IPPORT_SUPDUP  = 95;

    enum IPPORT_EXECSERVER = 512;
    enum IPPORT_LOGINSERVER = 513;
    enum IPPORT_CMDSERVER = 514;
    enum IPPORT_EFSSERVER = 520;

    enum IPPORT_BIFFUDP = 512;
    enum IPPORT_WHOSERVER = 513;
    enum IPPORT_ROUTESERVER = 520;

    enum IPPORT_RESERVED = 1024;

    enum IPPORT_USERRESERVED = 5000;

    extern(D) bool IN_CLASSA(in_addr_t i) pure @safe { return (i & 0x80000000) == 0; }
    enum IN_CLASSA_NET    = 0xff000000;
    enum IN_CLASSA_NSHIFT = 24;
    enum IN_CLASSA_HOST   = 0xffffffff & ~IN_CLASSA_NET;
    enum IN_CLASSA_MAX    = 128;

    extern(D) bool IN_CLASSB(in_addr_t i) pure @safe { return (i & 0xc0000000) == 0x80000000; }
    enum IN_CLASSB_NET    = 0xffff0000;
    enum IN_CLASSB_NSHIFT = 16;
    enum IN_CLASSB_HOST   = 0xffffffff & ~IN_CLASSB_NET;
    enum IN_CLASSB_MAX    = 65536;

    extern(D) bool IN_CLASSC(in_addr_t i) pure @safe { return (i & 0xe0000000) == 0xc0000000; }
    enum IN_CLASSC_NET    = 0xffffff00;
    enum IN_CLASSC_NSHIFT = 8;
    enum IN_CLASSC_HOST   = 0xffffffff & ~IN_CLASSC_NET;

    extern(D) bool IN_CLASSD(in_addr_t i) pure @safe { return (i & 0xf0000000) == 0xe0000000; }
    extern(D) bool IN_MULTICAST(in_addr_t i) { return IN_CLASSD(i); }

    extern(D) bool IN_EXPERIMENTAL(in_addr_t i) pure @safe { return (i & 0xe0000000) == 0xe0000000; }
    extern(D) bool IN_BADCLASS(in_addr_t i) pure @safe { return (i & 0xf0000000) == 0xf0000000; }

    enum IN_LOOPBACKNET = 127;

    enum INADDR_UNSPEC_GROUP    = 0xe0000000;
    enum INADDR_ALLHOSTS_GROUP  = 0xe0000001;
    enum INADDR_ALLRTRS_GROUP   = 0xe0000002;
    enum INADDR_MAX_LOCAL_GROUP = 0xe00000ff;

    enum IN6ADDR_ANY_INIT      = in6_addr.init;
    enum IN6ADDR_LOOPBACK_INIT = in6_addr([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]);

    version (gnu_libc) static if (__USE_MISC)
    {
        struct ip_mreq
        {
            in_addr imr_multiaddr;
            in_addr imr_interface;
        };

        struct ip_mreq_source
        {
            in_addr imr_multiaddr;
            in_addr imr_interface;
            in_addr imr_sourceaddr;
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

        struct ip_msfilter
        {
            in_addr imsf_multiaddr;
            in_addr imsf_interface;
            uint imsf_fmode;
            uint imsf_numsrc;
            in_addr[1] imsf_slist;
        };

        extern(D) size_t IP_MSFILTER_SIZE(int numsrc)
        {
            return ip_msfilter.sizeof - in_addr.sizeof + numsrc * in_addr.sizeof;
        }

        struct group_filter
        {
            uint gf_interface;
            sockaddr_storage gf_group;
            uint gf_fmode;
            uint gf_numsrc;
            sockaddr_storage[1] gf_slist;
        };

        extern(D) size_t GROUP_FILTER_SIZE(int numsrc) pure @safe
        {
            return group_filter.sizeof - sockaddr_storage.sizeof + numsrc * sockaddr_storage.sizeof;
        }
    }

    extern(D) bool IN6_ARE_ADDR_EQUAL(in6_addr* a, in6_addr* b) pure @safe { return *a == *b; }

    version (gnu_libc) static if (__USE_MISC)
    {
        int bindresvport(int __sockfd, sockaddr_in* __sock_in);
        int bindresvport6(int __sockfd, sockaddr_in6* _);
    }

    version (gnu_libc) static if (__USE_GNU)
    {
        struct in6_pktinfo
        {
            in6_addr ipi6_addr;
            uint ipi6_ifindex;
        };

        struct ip6_mtuinfo
        {
            sockaddr_in6 ip6m_addr;
            uint ip6m_mtu;
        };

        int inet6_opt_init(void* __extbuf, socklen_t __extlen);
        int inet6_opt_append(void* __extbuf, socklen_t __extlen, int __offset,
                             ubyte __type, socklen_t __len, ubyte __align, void** __databufp);
        int inet6_opt_finish(void* __extbuf, socklen_t __extlen, int __offset);
        int inet6_opt_set_val(void* __databuf, int __offset, void* __val, socklen_t __vallen);
        int inet6_opt_next(void* __extbuf, socklen_t __extlen, int __offset,
                           ubyte* __typep, socklen_t* __lenp, void** __databufp);
        int inet6_opt_find(void* __extbuf, socklen_t __extlen, int __offset,
                           ubyte __type, socklen_t* __lenp, void** __databufp);
        int inet6_opt_get_val(void* __databuf, int __offset, void* __val, socklen_t __vallen);

        socklen_t inet6_rth_space(int __type, int __segments);
        void* inet6_rth_init(void* __bp, socklen_t __bp_len, int __type, int __segments);
        int inet6_rth_add(void* __bp, const in6_addr* __addr);
        int inet6_rth_reverse(const void* __in, void* __out);
        int inet6_rth_segments(const void* __bp);
        in6_addr* inet6_rth_getaddr(const void* __bp, int __index);

        int getipv4sourcefilter(int __s, in_addr __interface_addr, in_addr __group,
                                uint* __fmode, uint* __numsrc, in_addr* __slist);

        int setipv4sourcefilter(int __s, in_addr __interface_addr, in_addr __group,
                                uint __fmode, uint __numsrc, const in_addr* __slist);


        int getsourcefilter(int __s, uint __interface_addr, const sockaddr* __group,
                            socklen_t __grouplen, uint* __fmode, uint* __numsrc,
                            sockaddr_storage* __slist);

        int setsourcefilter(int __s, uint __interface_addr, const sockaddr* __group,
                            socklen_t __grouplen, uint __fmode, uint __numsrc,
                            const sockaddr_storage* __slist);
    }

    // =============================================================================
    // What follows is from bits/in.h, but since bits/in.h specifically says that it
    // should only be #included by #including netinet/in.h, it makes more sense to
    // put them in here so that the way they're imported corresponds with the
    // correct way of #including them in C/C++.
    // =============================================================================

    enum IP_OPTIONS  = 4;
    enum IP_HDRINCL  = 3;
    enum IP_TOS      = 1;
    enum IP_TTL      = 2;
    enum IP_RECVOPTS = 6;

    enum IP_RECVRETOPTS            = IP_RETOPTS;
    enum IP_RETOPTS                = 7;
    enum IP_MULTICAST_IF           = 32;
    enum IP_MULTICAST_TTL          = 33;
    enum IP_MULTICAST_LOOP         = 34;
    enum IP_ADD_MEMBERSHIP         = 35;
    enum IP_DROP_MEMBERSHIP        = 36;
    enum IP_UNBLOCK_SOURCE         = 37;
    enum IP_BLOCK_SOURCE           = 38;
    enum IP_ADD_SOURCE_MEMBERSHIP  = 39;
    enum IP_DROP_SOURCE_MEMBERSHIP = 40;
    enum IP_MSFILTER               = 41;

    version (gnu_libc) static if (__USE_MISC)
    {
        enum MCAST_JOIN_GROUP         = 42;
        enum MCAST_BLOCK_SOURCE       = 43;
        enum MCAST_UNBLOCK_SOURCE     = 44;
        enum MCAST_LEAVE_GROUP        = 45;
        enum MCAST_JOIN_SOURCE_GROUP  = 46;
        enum MCAST_LEAVE_SOURCE_GROUP = 47;
        enum MCAST_MSFILTER           = 48;
        enum IP_MULTICAST_ALL         = 49;
        enum IP_UNICAST_IF            = 50;

        enum MCAST_EXCLUDE = 0;
        enum MCAST_INCLUDE = 1;
    }

    enum IP_ROUTER_ALERT  = 5;
    enum IP_PKTINFO       = 8;
    enum IP_PKTOPTIONS    = 9;
    enum IP_PMTUDISC      = 10;
    enum IP_MTU_DISCOVER  = 10;
    enum IP_RECVERR       = 11;
    enum IP_RECVTTL       = 12;
    enum IP_RECVTOS       = 13;
    enum IP_MTU           = 14;
    enum IP_FREEBIND      = 15;
    enum IP_IPSEC_POLICY  = 16;
    enum IP_XFRM_POLICY   = 17;
    enum IP_PASSSEC       = 18;
    enum IP_TRANSPARENT   = 19;
    enum IP_MULTICAST_ALL = 49;

    enum IP_ORIGDSTADDR     = 20;
    enum IP_RECVORIGDSTADDR = IP_ORIGDSTADDR;

    enum IP_MINTTL               = 21;
    enum IP_NODEFRAG             = 22;
    enum IP_CHECKSUM             = 23;
    enum IP_BIND_ADDRESS_NO_PORT = 24;

    enum IP_PMTUDISC_DONT      = 0;
    enum IP_PMTUDISC_WANT      = 1;
    enum IP_PMTUDISC_DO        = 2;
    enum IP_PMTUDISC_PROBE     = 3;
    enum IP_PMTUDISC_INTERFACE = 4;
    enum IP_PMTUDISC_OMIT      = 5;

    enum SOL_IP = 0;

    enum IP_DEFAULT_MULTICAST_TTL  = 1;
    enum IP_DEFAULT_MULTICAST_LOOP = 1;
    enum IP_MAX_MEMBERSHIPS        = 20;

    version (gnu_libc) static if (__USE_MISC)
    {
        struct ip_opts
        {
            in_addr ip_dst;
            char[40] ip_opts = 0;
        };

        struct ip_mreqn
        {
            in_addr imr_multiaddr;
            in_addr imr_address;
            int imr_ifindex;
        };

        struct in_pktinfo
        {
            int ipi_ifindex;
            in_addr ipi_spec_dst;
            in_addr ipi_addr;
        };
    }

    enum IPV6_ADDRFORM       = 1;
    enum IPV6_2292PKTINFO    = 2;
    enum IPV6_2292HOPOPTS    = 3;
    enum IPV6_2292DSTOPTS    = 4;
    enum IPV6_2292RTHDR      = 5;
    enum IPV6_2292PKTOPTIONS = 6;
    enum IPV6_CHECKSUM       = 7;
    enum IPV6_2292HOPLIMIT   = 8;

    enum IPV6_NEXTHOP        = 9;
    enum IPV6_AUTHHDR        = 10;
    enum IPV6_UNICAST_HOPS   = 16;
    enum IPV6_MULTICAST_IF   = 17;
    enum IPV6_MULTICAST_HOPS = 18;
    enum IPV6_MULTICAST_LOOP = 19;
    enum IPV6_JOIN_GROUP     = 20;
    enum IPV6_LEAVE_GROUP    = 21;
    enum IPV6_ROUTER_ALERT   = 22;
    enum IPV6_MTU_DISCOVER   = 23;
    enum IPV6_MTU            = 24;
    enum IPV6_RECVERR        = 25;
    enum IPV6_V6ONLY         = 26;
    enum IPV6_JOIN_ANYCAST   = 27;
    enum IPV6_LEAVE_ANYCAST  = 28;
    enum IPV6_IPSEC_POLICY   = 34;
    enum IPV6_XFRM_POLICY    = 35;
    enum IPV6_HDRINCL        = 36;

    enum IPV6_RECVPKTINFO  = 49;
    enum IPV6_PKTINFO      = 50;
    enum IPV6_RECVHOPLIMIT = 51;
    enum IPV6_HOPLIMIT     = 52;
    enum IPV6_RECVHOPOPTS  = 53;
    enum IPV6_HOPOPTS      = 54;
    enum IPV6_RTHDRDSTOPTS = 55;
    enum IPV6_RECVRTHDR    = 56;
    enum IPV6_RTHDR        = 57;
    enum IPV6_RECVDSTOPTS  = 58;
    enum IPV6_DSTOPTS      = 59;
    enum IPV6_RECVPATHMTU  = 60;
    enum IPV6_PATHMTU      = 61;
    enum IPV6_DONTFRAG     = 62;

    enum IPV6_RECVTCLASS = 66;
    enum IPV6_TCLASS     = 67;

    enum IPV6_PMTUDISC_DONT      = 0;
    enum IPV6_PMTUDISC_WANT      = 1;
    enum IPV6_PMTUDISC_DO        = 2;
    enum IPV6_PMTUDISC_PROBE     = 3;
    enum IPV6_PMTUDISC_INTERFACE = 4;
    enum IPV6_PMTUDISC_OMIT      = 5;

    enum SOL_IPV6   = 41;
    enum SOL_ICMPV6 = 58;

    enum IPV6_RTHDR_LOOSE  = 0;
    enum IPV6_RTHDR_STRICT = 1;

    enum IPV6_RTHDR_TYPE_0 = 0;
}

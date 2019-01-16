/**
 * D header file for POSIX.
 *
 * Copyright: Copyright David Nadlinger 2011.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   David Nadlinger, Sean Kelly, Alex Rønne Petersen
 * Standards: The Open Group Base Specifications Issue 6, IEEE Std 1003.1, 2004 Edition
 */

/*          Copyright David Nadlinger 2011.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.posix.netdb;

private import core.sys.posix.config;
public import core.stdc.inttypes;         // for uint32_t
public import core.sys.posix.netinet.in_; // for in_port_t, in_addr_t
public import core.sys.posix.sys.types;   // for ino_t
public import core.sys.posix.sys.socket;  // for socklen_t

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Posix):
extern (C):
nothrow:
@nogc:

//
// Required
//
/*
struct hostent
{
    char*   h_name;
    char**  h_aliases;
    int     h_addrtype;
    int     h_length;
    char**  h_addr_list;
}

struct netent
{
    char*   n_name;
    char**  n_aliase;
    int     n_addrtype;
    uint32_t n_net;
}

struct protoent
{
    char*   p_name;
    char**  p_aliases;
    int     p_proto;
}

struct servent
{
    char*   s_name;
    char**  s_aliases;
    int     s_port;
    char*   s_proto;
}

IPPORT_RESERVED

h_errno

HOST_NOT_FOUND
NO_DATA
NO_RECOVERY
TRY_AGAIN

struct addrinfo
{
    int         ai_flags;
    int         ai_family;
    int         ai_socktype;
    int         ai_protocol;
    socklen_t   ai_addrlen;
    sockaddr*   ai_addr;
    char*       ai_canonname;
    addrinfo*   ai_next;
}

AI_PASSIVE
AI_CANONNAME
AI_NUMERICHOST
AI_NUMERICSERV
AI_V4MAPPED
AI_ALL
AI_ADDRCONFIG

NI_NOFQDN
NI_NUMERICHOST
NI_NAMEREQD
NI_NUMERICSERV
NI_NUMERICSCOPE
NI_DGRAM

EAI_AGAIN
EAI_BADFLAGS
EAI_FAIL
EAI_FAMILY
EAI_MEMORY
EAI_NONAME

EAI_SERVICE
EAI_SOCKTYPE
EAI_SYSTEM
EAI_OVERFLOW

void         endhostent();
void         endnetent();
void         endprotoent();
void         endservent();
void         freeaddrinfo(addrinfo*);
const(char)* gai_strerror(int);
int          getaddrinfo(const(char)*, const(char)*, const(addrinfo)*, addrinfo**);
hostent*     gethostbyaddr(const(void)*, socklen_t, int);
hostent*     gethostbyname(const(char)*);
hostent*     gethostent();
int          getnameinfo(const(sockaddr)*, socklen_t, char*, socklen_t, char*, socklen_t, int);
netent*      getnetbyaddr(uint32_t, int);
netent*      getnetbyname(const(char)*);
netent*      getnetent();
protoent*    getprotobyname(const(char)*);
protoent*    getprotobynumber(int);
protoent*    getprotoent();
servent*     getservbyname(const(char)*, const(char)*);
servent*     getservbyport(int, const(char)*);
servent*     getservent();
void         sethostent(int);
void         setnetent(int);
void         setprotoent(int);
void         setservent(int);
*/

version (CRuntime_Glibc)
{
    struct hostent
    {
        char*   h_name;
        char**  h_aliases;
        int     h_addrtype;
        int     h_length;
        char**  h_addr_list;
        char*   h_addr() @property { return h_addr_list[0]; } // non-standard
    }

    struct netent
    {
        char*   n_name;
        char**  n_aliases;
        int     n_addrtype;
        uint32_t n_net;
    }

    struct protoent
    {
        char*   p_name;
        char**  p_aliases;
        int     p_proto;
    }

    struct servent
    {
        char*   s_name;
        char**  s_aliases;
        int     s_port;
        char*   s_proto;
    }

    enum IPPORT_RESERVED = 1024;

    //h_errno

    enum HOST_NOT_FOUND = 1;
    enum NO_DATA        = 4;
    enum NO_RECOVERY    = 3;
    enum TRY_AGAIN      = 2;

    struct addrinfo
    {
        int         ai_flags;
        int         ai_family;
        int         ai_socktype;
        int         ai_protocol;
        socklen_t   ai_addrlen;
        sockaddr*   ai_addr;
        char*       ai_canonname;
        addrinfo*   ai_next;
    }

    enum AI_PASSIVE         = 0x1;
    enum AI_CANONNAME       = 0x2;
    enum AI_NUMERICHOST     = 0x4;
    enum AI_NUMERICSERV     = 0x400;
    enum AI_V4MAPPED        = 0x8;
    enum AI_ALL             = 0x10;
    enum AI_ADDRCONFIG      = 0x20;

    enum NI_NOFQDN          = 4;
    enum NI_NUMERICHOST     = 1;
    enum NI_NAMEREQD        = 8;
    enum NI_NUMERICSERV     = 2;
    //enum NI_NUMERICSCOPE    = ?;
    enum NI_DGRAM           = 16;
    enum NI_MAXHOST         = 1025; // non-standard
    enum NI_MAXSERV         = 32;   // non-standard

    enum EAI_AGAIN          = -3;
    enum EAI_BADFLAGS       = -1;
    enum EAI_FAIL           = -4;
    enum EAI_FAMILY         = -6;
    enum EAI_MEMORY         = -10;
    enum EAI_NONAME         = -2;
    enum EAI_SERVICE        = -8;
    enum EAI_SOCKTYPE       = -7;
    enum EAI_SYSTEM         = -11;
    enum EAI_OVERFLOW       = -12;
}
else version (Darwin)
{
    struct hostent
    {
        char*   h_name;
        char**  h_aliases;
        int     h_addrtype;
        int     h_length;
        char**  h_addr_list;
        char*   h_addr() @property { return h_addr_list[0]; } // non-standard
    }

    struct netent
    {
        char*   n_name;
        char**  n_aliases;
        int     n_addrtype;
        uint32_t n_net;
    }

    struct protoent
    {
        char*   p_name;
        char**  p_aliases;
        int     p_proto;
    }

    struct servent
    {
        char*   s_name;
        char**  s_aliases;
        int     s_port;
        char*   s_proto;
    }

    enum IPPORT_RESERVED = 1024;

    //h_errno

    enum HOST_NOT_FOUND = 1;
    enum NO_DATA        = 4;
    enum NO_RECOVERY    = 3;
    enum TRY_AGAIN      = 2;

    struct addrinfo
    {
        int         ai_flags;
        int         ai_family;
        int         ai_socktype;
        int         ai_protocol;
        socklen_t   ai_addrlen;
        char*       ai_canonname;
        sockaddr*   ai_addr;
        addrinfo*   ai_next;
    }

    enum AI_PASSIVE         = 0x1;
    enum AI_CANONNAME       = 0x2;
    enum AI_NUMERICHOST     = 0x4;
    enum AI_NUMERICSERV     = 0x1000;
    enum AI_V4MAPPED        = 0x800;
    enum AI_ALL             = 0x100;
    enum AI_ADDRCONFIG      = 0x400;

    enum NI_NOFQDN          = 0x1;
    enum NI_NUMERICHOST     = 0x2;
    enum NI_NAMEREQD        = 0x4;
    enum NI_NUMERICSERV     = 0x8;
    //enum NI_NUMERICSCOPE    = ?;
    enum NI_DGRAM           = 0x10;
    enum NI_MAXHOST         = 1025; // non-standard
    enum NI_MAXSERV         = 32;   // non-standard

    enum EAI_AGAIN          = 2;
    enum EAI_BADFLAGS       = 3;
    enum EAI_FAIL           = 4;
    enum EAI_FAMILY         = 5;
    enum EAI_MEMORY         = 6;
    enum EAI_NONAME         = 8;
    enum EAI_SERVICE        = 9;
    enum EAI_SOCKTYPE       = 10;
    enum EAI_SYSTEM         = 11;
    enum EAI_OVERFLOW       = 14;
}
else version (FreeBSD)
{
    struct hostent
    {
        char*   h_name;
        char**  h_aliases;
        int     h_addrtype;
        int     h_length;
        char**  h_addr_list;
        extern (D) char* h_addr() @property { return h_addr_list[0]; } // non-standard
    }

    struct netent
    {
        char*   n_name;
        char**  n_aliases;
        int     n_addrtype;
        uint32_t n_net;
    }

    struct protoent
    {
        char*   p_name;
        char**  p_aliases;
        int     p_proto;
    }

    struct servent
    {
        char*   s_name;
        char**  s_aliases;
        int     s_port;
        char*   s_proto;
    }

    enum IPPORT_RESERVED = 1024;

    //h_errno

    enum HOST_NOT_FOUND = 1;
    enum NO_DATA        = 4;
    enum NO_RECOVERY    = 3;
    enum TRY_AGAIN      = 2;

    struct addrinfo
    {
        int         ai_flags;
        int         ai_family;
        int         ai_socktype;
        int         ai_protocol;
        socklen_t   ai_addrlen;
        char*       ai_canonname;
        sockaddr*   ai_addr;
        addrinfo*   ai_next;
    }

    enum AI_PASSIVE         = 0x1;
    enum AI_CANONNAME       = 0x2;
    enum AI_NUMERICHOST     = 0x4;
    enum AI_NUMERICSERV     = 0x8;
    enum AI_V4MAPPED        = 0x800;
    enum AI_ALL             = 0x100;
    enum AI_ADDRCONFIG      = 0x400;

    enum NI_NOFQDN          = 0x1;
    enum NI_NUMERICHOST     = 0x2;
    enum NI_NAMEREQD        = 0x4;
    enum NI_NUMERICSERV     = 0x8;
    //enum NI_NUMERICSCOPE    = ?;
    enum NI_DGRAM           = 0x10;
    enum NI_MAXHOST         = 1025; // non-standard
    enum NI_MAXSERV         = 32;   // non-standard

    enum EAI_AGAIN          = 2;
    enum EAI_BADFLAGS       = 3;
    enum EAI_FAIL           = 4;
    enum EAI_FAMILY         = 5;
    enum EAI_MEMORY         = 6;
    enum EAI_NONAME         = 8;
    enum EAI_SERVICE        = 9;
    enum EAI_SOCKTYPE       = 10;
    enum EAI_SYSTEM         = 11;
    enum EAI_OVERFLOW       = 14;
}
else version (NetBSD)
{
    struct hostent
    {
        char*   h_name;
        char**  h_aliases;
        int     h_addrtype;
        int     h_length;
        char**  h_addr_list;
        extern (D) char* h_addr() @property { return h_addr_list[0]; } // non-standard
    }

    struct netent
    {
        char*   n_name;
        char**  n_aliases;
        int     n_addrtype;
        uint32_t n_net;
/+ todo
#if (defined(__sparc__) && defined(_LP64)) || \
    (defined(__sh__) && defined(_LP64) && (_BYTE_ORDER == _BIG_ENDIAN))
        int             __n_pad0;       /* ABI compatibility */
#endif
        uint32_t        n_net;          /*%< network # */
#if defined(__alpha__) || (defined(__i386__) && defined(_LP64)) || \
    (defined(__sh__) && defined(_LP64) && (_BYTE_ORDER == _LITTLE_ENDIAN))
        int             __n_pad0;       /* ABI compatibility */
#endif

+/
    }

    struct protoent
    {
        char*   p_name;
        char**  p_aliases;
        int     p_proto;
    }

    struct servent
    {
        char*   s_name;
        char**  s_aliases;
        int     s_port;
        char*   s_proto;
    }

    enum IPPORT_RESERVED = 1024;

    //h_errno

    enum HOST_NOT_FOUND = 1;
    enum NO_DATA        = 4;
    enum NO_RECOVERY    = 3;
    enum TRY_AGAIN      = 2;

    struct addrinfo
    {
        int         ai_flags;
        int         ai_family;
        int         ai_socktype;
        int         ai_protocol;
/+todo
#if defined(__sparc__) && defined(_LP64)
        int             __ai_pad0;      /* ABI compatibility */
#endif
+/
        socklen_t   ai_addrlen;
/+todo
#if defined(__alpha__) || (defined(__i386__) && defined(_LP64))
        int             __ai_pad0;      /* ABI compatibility */
#endif
+/
        char*       ai_canonname;
        sockaddr*   ai_addr;
        addrinfo*   ai_next;
    }

    enum AI_PASSIVE         = 0x1;
    enum AI_CANONNAME       = 0x2;
    enum AI_NUMERICHOST     = 0x4;
    enum AI_NUMERICSERV     = 0x8;
    enum AI_V4MAPPED        = 0x800;
    enum AI_ALL             = 0x100;
    enum AI_ADDRCONFIG      = 0x400;

    enum NI_NOFQDN          = 0x1;
    enum NI_NUMERICHOST     = 0x2;
    enum NI_NAMEREQD        = 0x4;
    enum NI_NUMERICSERV     = 0x8;
    enum NI_DGRAM           = 0x10;
    enum NI_WITHSCOPEID     = 0x00000020;
    enum NI_NUMERICSCOPE    = 0x00000040;
    enum NI_MAXHOST         = 1025; // non-standard
    enum NI_MAXSERV         = 32;   // non-standard

    enum EAI_AGAIN          = 2;
    enum EAI_BADFLAGS       = 3;
    enum EAI_FAIL           = 4;
    enum EAI_FAMILY         = 5;
    enum EAI_MEMORY         = 6;
    enum EAI_NONAME         = 8;
    enum EAI_SERVICE        = 9;
    enum EAI_SOCKTYPE       = 10;
    enum EAI_SYSTEM         = 11;
    enum EAI_OVERFLOW       = 14;
}
else version (OpenBSD)
{
    struct hostent
    {
        char*     h_name;
        char**    h_aliases;
        int       h_addrtype;
        int       h_length;
        char**    h_addr_list;
        extern (D) char* h_addr() @property { return h_addr_list[0]; } // non-standard
    }

    struct netent
    {
        char*     n_name;
        char**    n_aliases;
        int       n_addrtype;
        in_addr_t n_net;
    }

    struct protoent
    {
        char*     p_name;
        char**    p_aliases;
        int       p_proto;
    }

    struct servent
    {
        char*     s_name;
        char**    s_aliases;
        int       s_port;
        char*     s_proto;
    }

    enum IPPORT_RESERVED = 1024;

    //h_errno

    enum NETDB_INTERNAL = -1;
    enum NETDB_SUCCESS  = 0;
    enum HOST_NOT_FOUND = 1;
    enum NO_DATA        = 4;
    enum NO_RECOVERY    = 3;
    enum TRY_AGAIN      = 2;

    struct addrinfo
    {
        int         ai_flags;
        int         ai_family;
        int         ai_socktype;
        int         ai_protocol;
        socklen_t   ai_addrlen;
        char*       ai_canonname;
        sockaddr*   ai_addr;
        addrinfo*   ai_next;
    }

    enum AI_PASSIVE         = 0x1;
    enum AI_CANONNAME       = 0x2;
    enum AI_NUMERICHOST     = 0x4;
    enum AI_EXT             = 0x8;
    enum AI_NUMERICSERV     = 0x10;
    enum AI_FQDN            = 0x20;
    enum AI_ADDRCONFIG      = 0x40;
    enum AI_MASK            = AI_PASSIVE | AI_CANONNAME | AI_NUMERICHOST | AI_NUMERICSERV | AI_FQDN | AI_ADDRCONFIG;

    enum NI_NUMERICHOST     = 1;
    enum NI_NUMERICSERV     = 2;
    enum NI_NOFQDN          = 4;
    enum NI_NAMEREQD        = 8;
    enum NI_DGRAM           = 16;
    //enum NI_NUMERICSCOPE    = 32;
    enum NI_MAXHOST         = 256; // non-standard
    enum NI_MAXSERV         = 32;  // non-standard

    enum EAI_NONAME         = -1;
    enum EAI_BADFLAGS       = -2;
    enum EAI_AGAIN          = -3;
    enum EAI_FAIL           = -4;
    enum EAI_NODATA         = -5;
    enum EAI_FAMILY         = -6;
    enum EAI_SOCKTYPE       = -7;
    enum EAI_SERVICE        = -8;
    enum EAI_ADDRFAMILY     = -9;
    enum EAI_MEMORY         = -10;
    enum EAI_SYSTEM         = -11;
    enum EAI_BADHINTS       = -12;
    enum EAI_PROTOCOL       = -13;
    enum EAI_OVERFLOW       = -14;
}
else version (DragonFlyBSD)
{
    /*
     * Error return codes from gethostbyname() and gethostbyaddr()
     * (left in h_errno).
     */
    struct hostent
    {
        char*   h_name;
        char**  h_aliases;
        int     h_addrtype;
        int     h_length;
        char**  h_addr_list;
        extern (D) char* h_addr() @property { return h_addr_list[0]; } // non-standard
    }

    struct netent
    {
        char*   n_name;
        char**  n_aliases;
        int     n_addrtype;
        uint32_t n_net;
    }

    struct protoent
    {
        char*   p_name;
        char**  p_aliases;
        int     p_proto;
    }

    struct servent
    {
        char*   s_name;
        char**  s_aliases;
        int     s_port;
        char*   s_proto;
    }

    struct addrinfo
    {
        int         ai_flags;
        int         ai_family;
        int         ai_socktype = SOCK_STREAM;           /* socktype default value required to be able to perform getAddrInfo on DragonFlyBSD
                                                          * without socktype set, you get 'servname not supported for ai_socktype'
                                                          */
        int         ai_protocol;
        socklen_t   ai_addrlen;
        char*       ai_canonname;
        sockaddr*   ai_addr;
        addrinfo*   ai_next;
    }

    enum IPPORT_RESERVED = 1024;

    enum NETDB_INTERNAL = -1;
    enum NETDB_SUCCESS  = 0;
    enum HOST_NOT_FOUND = 1;
    enum TRY_AGAIN      = 2;
    enum NO_RECOVERY    = 3;
    enum NO_DATA        = 4;
    enum NO_ADDRESS     = NO_DATA;

    //enum EAI_ADDRFAMILY     = 1; // deprecated
    enum EAI_AGAIN          = 2;
    enum EAI_BADFLAGS       = 3;
    enum EAI_FAIL           = 4;
    enum EAI_FAMILY         = 5;
    enum EAI_MEMORY         = 6;
    //enum EAI_NODATA         = 7; // deprecated
    enum EAI_NONAME         = 8;
    enum EAI_SERVICE        = 9;
    enum EAI_SOCKTYPE       = 10;
    enum EAI_SYSTEM         = 11;
    enum EAI_BADHINTS       = 12;
    enum EAI_PROTOCOL       = 13;
    enum EAI_OVERFLOW       = 14;

    enum AI_PASSIVE         = 0x001;
    enum AI_CANONNAME       = 0x002;
    enum AI_NUMERICHOST     = 0x004;
    enum AI_NUMERICSERV     = 0x008;
    enum AI_MASK            = (AI_PASSIVE | AI_CANONNAME | AI_NUMERICHOST | AI_NUMERICSERV | AI_ADDRCONFIG);   // valid flags for addrinfo (not a standard def, apps should not use it)
    enum AI_ALL             = 0x100;
    enum AI_V4MAPPED_CFG    = 0x200;
    enum AI_ADDRCONFIG      = 0x400;
    enum AI_V4MAPPED        = 0x800;
    enum AI_DEFAULT         = (AI_V4MAPPED_CFG | AI_ADDRCONFIG);

    enum NI_MAXHOST         = 1025; // non-standard
    enum NI_MAXSERV         = 32;   // non-standard

    enum NI_NOFQDN          = 0x01;
    enum NI_NUMERICHOST     = 0x02;
    enum NI_NAMEREQD        = 0x04;
    enum NI_NUMERICSERV     = 0x08;
    enum NI_DGRAM           = 0x10;
    //enum NI_WITHSCOPEID     = 0x20; // deprecated
    enum NI_NUMERICSCOPE    = 0x40;

}
else version (Solaris)
{
    struct hostent
    {
        char* h_name;
        char** h_aliases;
        int h_addrtype;
        int h_length;
        char** h_addr_list;

        extern (D) char* h_addr() @property { return h_addr_list[0]; } // non-standard
    }

    struct netent
    {
        char* n_name;
        char** n_aliases;
        int n_addrtype;
        uint32_t n_net;
    }

    struct protoent
    {
        char* p_name;
        char** p_aliases;
        int p_proto;
    }

    struct servent
    {
        char* s_name;
        char** s_aliases;
        int s_port;
        char* s_proto;
    }

    enum HOST_NOT_FOUND = 1;
    enum TRY_AGAIN = 2;
    enum NO_RECOVERY = 3;
    enum NO_DATA = 4;

    struct addrinfo
    {
        int ai_flags;
        int ai_family;
        int ai_socktype;
        int ai_protocol;

        version (SPARC)
            int _ai_pad;
        else version (SPARC64)
            int _ai_pad;

        socklen_t ai_addrlen;
        char* ai_canonname;
        sockaddr* ai_addr;
        addrinfo* ai_next;
    }

    enum AI_PASSIVE = 0x0008;
    enum AI_CANONNAME = 0x0010;
    enum AI_NUMERICHOST = 0x0020;
    enum AI_NUMERICSERV = 0x0040;
    enum AI_V4MAPPED = 0x0001;
    enum AI_ALL = 0x0002;
    enum AI_ADDRCONFIG = 0x0004;

    enum NI_NOFQDN = 0x0001;
    enum NI_NUMERICHOST = 0x0002;
    enum NI_NAMEREQD = 0x0004;
    enum NI_NUMERICSERV = 0x0008;
    enum NI_DGRAM = 0x0010;
    enum NI_WITHSCOPEID = 0x0020;
    enum NI_NUMERICSCOPE = 0x0040;
    enum NI_MAXHOST = 1025;
    enum NI_MAXSERV = 32;

    enum EAI_AGAIN = 2;
    enum EAI_BADFLAGS = 3;
    enum EAI_FAIL = 4;
    enum EAI_FAMILY = 5;
    enum EAI_MEMORY = 6;
    enum EAI_NONAME = 8;
    enum EAI_SERVICE = 9;
    enum EAI_SOCKTYPE = 10;
    enum EAI_SYSTEM = 11;
    enum EAI_OVERFLOW = 14;
    enum EAI_PROTOCOL = 13;
    enum EAI_MAX = 14;
}
else version (CRuntime_Bionic)
{
    struct hostent
    {
        char*   h_name;
        char**  h_aliases;
        int     h_addrtype;
        int     h_length;
        char**  h_addr_list;
        extern (D) char* h_addr() @property { return h_addr_list[0]; } // non-standard
    }

    struct netent
    {
        char*   n_name;
        char**  n_aliases;
        int     n_addrtype;
        uint32_t n_net;
    }

    struct protoent
    {
        char*   p_name;
        char**  p_aliases;
        int     p_proto;
    }

    struct servent
    {
        char*   s_name;
        char**  s_aliases;
        int     s_port;
        char*   s_proto;
    }

    enum IPPORT_RESERVED = 1024;

    enum HOST_NOT_FOUND = 1;
    enum NO_DATA        = 4;
    enum NO_RECOVERY    = 3;
    enum TRY_AGAIN      = 2;

    struct addrinfo
    {
        int         ai_flags;
        int         ai_family;
        int         ai_socktype;
        int         ai_protocol;
        socklen_t   ai_addrlen;
        char*       ai_canonname;
        sockaddr*   ai_addr;
        addrinfo*   ai_next;
    }

    enum AI_PASSIVE         = 0x1;
    enum AI_CANONNAME       = 0x2;
    enum AI_NUMERICHOST     = 0x4;
    enum AI_NUMERICSERV     = 0x8;
    enum AI_V4MAPPED        = 0x800;
    enum AI_ALL             = 0x100;
    enum AI_ADDRCONFIG      = 0x400;

    enum NI_NOFQDN          = 0x1;
    enum NI_NUMERICHOST     = 0x2;
    enum NI_NAMEREQD        = 0x4;
    enum NI_NUMERICSERV     = 0x8;
    enum NI_DGRAM           = 0x10;
    enum NI_MAXHOST         = 1025; // non-standard
    enum NI_MAXSERV         = 32;   // non-standard

    enum EAI_AGAIN          = 2;
    enum EAI_BADFLAGS       = 3;
    enum EAI_FAIL           = 4;
    enum EAI_FAMILY         = 5;
    enum EAI_MEMORY         = 6;
    enum EAI_NONAME         = 8;
    enum EAI_SERVICE        = 9;
    enum EAI_SOCKTYPE       = 10;
    enum EAI_SYSTEM         = 11;
    enum EAI_OVERFLOW       = 14;
}
else version (CRuntime_Musl)
{
    struct hostent
    {
        char*   h_name;
        char**  h_aliases;
        int     h_addrtype;
        int     h_length;
        char**  h_addr_list;
        char*   h_addr() @property { return h_addr_list[0]; } // non-standard
    }

    struct netent
    {
        char*     n_name;
        char**    n_aliases;
        int       n_addrtype;
        uint32_t n_net;
    }

    struct protoent
    {
        char*   p_name;
        char**  p_aliases;
        int     p_proto;
    }

    struct servent
    {
        char*     s_name;
        char**    s_aliases;
        int       s_port;
        char*     s_proto;
    }

    struct addrinfo
    {
        int         ai_flags;
        int         ai_family;
        int         ai_socktype;
        int         ai_protocol;
        socklen_t   ai_addrlen;
        sockaddr*   ai_addr;
        char*       ai_canonname;
        addrinfo*   ai_next;
    }

    enum {
        AI_PASSIVE         = 0x1,
        AI_CANONNAME       = 0x2,
        AI_NUMERICHOST     = 0x4,
        AI_NUMERICSERV     = 0x400,
        AI_V4MAPPED        = 0x8,
        AI_ALL             = 0x10,
        AI_ADDRCONFIG      = 0x20,
    }
    enum {
        NI_NUMERICHOST     = 1,
        NI_NUMERICSERV     = 2,
        NI_NOFQDN          = 4,
        NI_NAMEREQD        = 8,
        NI_DGRAM           = 16,
        NI_MAXSERV         = 32,
        NI_MAXHOST         = 255,
    }
    enum {
        EAI_BADFLAGS       = -1,
        EAI_NONAME         = -2,
        EAI_AGAIN          = -3,
        EAI_FAIL           = -4,
        EAI_FAMILY         = -6,
        EAI_SOCKTYPE       = -7,
        EAI_SERVICE        = -8,
        EAI_MEMORY         = -10,
        EAI_SYSTEM         = -11,
        EAI_OVERFLOW       = -12,
    }
}
else version (CRuntime_UClibc)
{
    struct hostent
    {
        char*   h_name;
        char**  h_aliases;
        int     h_addrtype;
        int     h_length;
        char**  h_addr_list;
        extern (D) char* h_addr() @property { return h_addr_list[0]; } // non-standard
    }

    struct netent
    {
        char*   n_name;
        char**  n_aliases;
        int     n_addrtype;
        uint32_t n_net;
    }

    struct protoent
    {
        char*   p_name;
        char**  p_aliases;
        int     p_proto;
    }

    struct servent
    {
        char*   s_name;
        char**  s_aliases;
        int     s_port;
        char*   s_proto;
    }

    enum IPPORT_RESERVED = 1024;

    enum HOST_NOT_FOUND = 1;
    enum NO_DATA        = 4;
    enum NO_RECOVERY    = 3;
    enum TRY_AGAIN      = 2;

    struct addrinfo
    {
        int         ai_flags;
        int         ai_family;
        int         ai_socktype;
        int         ai_protocol;
        socklen_t   ai_addrlen;
        sockaddr*   ai_addr;
        char*       ai_canonname;
        addrinfo*   ai_next;
    }

    enum AI_PASSIVE         = 0x1;
    enum AI_CANONNAME       = 0x2;
    enum AI_NUMERICHOST     = 0x4;
    enum AI_NUMERICSERV     = 0x400;
    enum AI_V4MAPPED        = 0x8;
    enum AI_ALL             = 0x10;
    enum AI_ADDRCONFIG      = 0x20;

    enum NI_NOFQDN          = 4;
    enum NI_NUMERICHOST     = 1;
    enum NI_NAMEREQD        = 8;
    enum NI_NUMERICSERV     = 2;
    enum NI_DGRAM           = 16;
    enum NI_MAXHOST         = 1025; // non-standard
    enum NI_MAXSERV         = 32;   // non-standard

    enum EAI_AGAIN          = -3;
    enum EAI_BADFLAGS       = -1;
    enum EAI_FAIL           = -4;
    enum EAI_FAMILY         = -6;
    enum EAI_MEMORY         = -10;
    enum EAI_NONAME         = -2;
    enum EAI_SERVICE        = -8;
    enum EAI_SOCKTYPE       = -7;
    enum EAI_SYSTEM         = -11;
    enum EAI_OVERFLOW       = -12;

    enum EAI_NODATA         = -5;
    enum EAI_ADDRFAMILY     = -9;
    enum EAI_INPROGRESS     = -100;
    enum EAI_CANCELED       = -101;
    enum EAI_NOTCANCELED    = -102;
    enum EAI_ALLDONE        = -103;
    enum EAI_INTR           = -104;
    enum EAI_IDN_ENCODE     = -105;
}
else
{
    static assert(false, "Unsupported platform");
}

void         endhostent();
void         endnetent();
void         endprotoent();
void         endservent();
void         freeaddrinfo(addrinfo*);
const(char)* gai_strerror(int);
int          getaddrinfo(const(char)*, const(char)*, const(addrinfo)*, addrinfo**);
hostent*     gethostbyaddr(const(void)*, socklen_t, int);
hostent*     gethostbyname(const(char)*);
hostent*     gethostent();
int          getnameinfo(const(sockaddr)*, socklen_t, char*, socklen_t, char*, socklen_t, int);
netent*      getnetbyaddr(uint32_t, int);
netent*      getnetbyname(const(char)*);
netent*      getnetent();
protoent*    getprotobyname(const(char)*);
protoent*    getprotobynumber(int);
protoent*    getprotoent();
servent*     getservbyname(const(char)*, const(char)*);
servent*     getservbyport(int, const(char)*);
servent*     getservent();
void         sethostent(int);
void         setnetent(int);
void         setprotoent(int);
void         setservent(int);

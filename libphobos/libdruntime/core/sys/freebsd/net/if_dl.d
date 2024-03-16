//Written in the D programming language

/++
    D header file for FreeBSD's net/if_dl.h.

    Copyright: Copyright 2023
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTP jmdavisprog.com, Jonathan M Davis)
 +/
module core.sys.freebsd.net.if_dl;

version (FreeBSD):
extern(C):
@nogc:
nothrow:

import core.sys.freebsd.sys.types : caddr_t, c_caddr_t;
import core.sys.posix.sys.socket : sa_family_t, sockaddr;

struct sockaddr_dl
{
    ubyte       sdl_len;
    sa_family_t sdl_family;
    ushort      sdl_index;
    ubyte       sdl_type;
    ubyte       sdl_nlen;
    ubyte       sdl_alen;
    ubyte       sdl_slen;
    ubyte[46]   sdl_data;
}

auto LLADDR()(sockaddr_dl* s) { return cast(caddr_t)(s.sdl_data.ptr + s.sdl_nlen); }
auto CLLADDR()(const sockaddr_dl* s) { return cast(c_caddr_t)(s.sdl_data.ptr + s.sdl_nlen); }
ushort LLINDEX()(const sockaddr_dl* s) { return s.sdl_index; }

struct ifnet;
sockaddr_dl* link_alloc_sdl(size_t, int);
void link_free_sdl(sockaddr* sa);
sockaddr_dl* link_init_sdl(ifnet*, sockaddr*, ubyte);

void  link_addr(const char*, sockaddr_dl*);
char* link_ntoa(const sockaddr_dl*);

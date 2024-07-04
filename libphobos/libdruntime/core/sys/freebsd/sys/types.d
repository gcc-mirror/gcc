//Written in the D programming language

/++
    D header file for FreeBSD's extensions to POSIX's sys/types.h.

    Copyright: Copyright 2023
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTP jmdavisprog.com, Jonathan M Davis)
 +/
module core.sys.freebsd.sys.types;

public import core.sys.posix.sys.types;

version (FreeBSD):
extern(C):
@nogc:
nothrow:

import core.stdc.config;

alias caddr_t = ubyte*;
alias c_caddr_t = const(ubyte)*;

alias cpuwhich_t = int;
alias cpulevel_t = int;
alias cpusetid_t = int;

alias critical_t = size_t;
alias daddr_t = long;

alias fixpt_t = uint;

alias accmode_t = int;

alias register_t = size_t;

alias sbintime_t = long;

alias segsz_t = size_t;

alias u_register_t = size_t;

alias cap_ioctl_t = size_t;

alias kpaddr_t = ulong;
alias kvaddr_t = ulong;
alias ksize_t = ulong;
alias kssize_t = long;

alias vm_offset_t = size_t;
alias vm_ooffset_t = ulong;
alias vm_paddr_t = ulong;
alias vm_pindex_t = ulong;
alias vm_size_t = size_t;

alias rman_res_t = ulong;

alias syscallarg_t = register_t;

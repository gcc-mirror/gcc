//Written in the D programming language

/++
    D header file for FreeBSD's extensions to POSIX's unistd.h.

    Copyright: Copyright 2018
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTP jmdavisprog.com, Jonathan M Davis)
 +/
module core.sys.freebsd.unistd;

public import core.sys.posix.unistd;

version (FreeBSD):
extern(C):
@nogc:
nothrow:

int getosreldate() pure @trusted;

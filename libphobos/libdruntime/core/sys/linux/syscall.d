/**
 * D header file for GNU/Linux
 *
 * $(LINK2 https://sourceware.org/git/?p=glibc.git;a=blob;f=misc/syscall.h, glibc syscall.h)
 */

module core.sys.linux.syscall;

version (linux):
extern (C):
@nogc:
nothrow:

public import core.sys.linux.sys.syscall;

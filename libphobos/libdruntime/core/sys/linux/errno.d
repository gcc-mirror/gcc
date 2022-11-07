/**
 * D header file for GNU/Linux
 *
 * $(LINK2 http://sourceware.org/git/?p=glibc.git;a=blob;f=stdlib/errno.h, glibc stdlib/errno.h)
 */
module core.sys.linux.errno;

version (linux):
extern (C):
nothrow:

public import core.stdc.errno;
import core.sys.linux.config;

static if (_GNU_SOURCE)
{
    extern __gshared char* program_invocation_name, program_invocation_short_name;
    alias error_t = int;
}

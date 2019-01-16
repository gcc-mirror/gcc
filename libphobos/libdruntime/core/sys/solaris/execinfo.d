/**
 * D header file for Solaris.
 *
 * Copyright: Copyright Martin Nowak 2012.
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Martin Nowak
 */
module core.sys.solaris.execinfo;

// The interface is exactly the same as linux, so copied from linux's execinfo.d

version (Solaris):
extern (C):
nothrow:

int backtrace(void** buffer, int size);
char** backtrace_symbols(const(void*)* buffer, int size);
void backtrace_symbols_fd(const(void*)* buffer, int size, int fd);

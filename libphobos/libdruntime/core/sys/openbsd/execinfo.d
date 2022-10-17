/**
 * OpenBSD implementation of glibc's $(LINK2 http://www.gnu.org/software/libc/manual/html_node/Backtraces.html backtrace) facility.
 *
 * Copyright: Copyright Martin Nowak 2012.
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Martin Nowak
 * Source:    $(DRUNTIMESRC core/sys/openbsd/_execinfo.d)
 */
module core.sys.openbsd.execinfo;

version (OpenBSD):
extern (C):
nothrow:
@nogc:

size_t backtrace(void**, size_t);
char** backtrace_symbols(const(void*)*, size_t);
void backtrace_symbols_fd(const(void*)*, size_t, int);
char** backtrace_symbols_fmt(const(void*)*, size_t, const char*);
int backtrace_symbols_fd_fmt(const(void*)*, size_t, int, const char*);

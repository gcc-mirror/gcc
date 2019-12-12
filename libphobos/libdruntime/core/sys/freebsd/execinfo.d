/**
 * FreeBSD implementation of glibc's $(LINK2 http://www.gnu.org/software/libc/manual/html_node/Backtraces.html backtrace) facility.
 *
 * Copyright: Copyright Martin Nowak 2012.
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Martin Nowak
 * Source:    $(DRUNTIMESRC core/sys/freebsd/_execinfo.d)
 */
module core.sys.freebsd.execinfo;

version (FreeBSD):
extern (C):
nothrow:

version (GNU)
    version = BacktraceExternal;

version (BacktraceExternal)
{
    size_t backtrace(void**, size_t);
    char** backtrace_symbols(const(void*)*, size_t);
    void backtrace_symbols_fd(const(void*)*, size_t, int);
    char** backtrace_symbols_fmt(const(void*)*, size_t, const char*);
    int backtrace_symbols_fd_fmt(const(void*)*, size_t, int, const char*);
}
else
{
    import core.sys.freebsd.dlfcn;

    // Use extern (D) so that these functions don't collide with libexecinfo.

    extern (D) int backtrace(void** buffer, int size)
    {
        import core.thread : thread_stackBottom;

        void** p, pend=cast(void**)thread_stackBottom();
        version (D_InlineAsm_X86)
            asm nothrow @trusted { mov p[EBP], EBP; }
        else version (D_InlineAsm_X86_64)
            asm nothrow @trusted { mov p[RBP], RBP; }
        else
            static assert(false, "Architecture not supported.");

        int i;
        for (; i < size && p < pend; ++i)
        {
            buffer[i] = *(p + 1);
            auto pnext = cast(void**)*p;
            if (pnext <= p) break;
            p = pnext;
        }
        return i;
    }


    extern (D) char** backtrace_symbols(const(void*)* buffer, int size)
    {
        static void* realloc(void* p, size_t len) nothrow
        {
            static import cstdlib=core.stdc.stdlib;
            auto res = cstdlib.realloc(p, len);
            if (res is null) cstdlib.free(p);
            return res;
        }

        if (size <= 0) return null;

        size_t pos = size * (char*).sizeof;
        char** p = cast(char**)realloc(null, pos);
        if (p is null) return null;

        Dl_info info;
        foreach (i, addr; buffer[0 .. size])
        {
            if (dladdr(addr, &info) == 0)
                (cast(ubyte*)&info)[0 .. info.sizeof] = 0;
            fixupDLInfo(addr, info);

            immutable len = formatStackFrame(null, 0, addr, info);
            assert(len > 0);

            p = cast(char**)realloc(p, pos + len);
            if (p is null) return null;

            formatStackFrame(cast(char*)p + pos, len, addr, info) == len || assert(0);

            p[i] = cast(char*)pos;
            pos += len;
        }
        foreach (i; 0 .. size)
        {
            pos = cast(size_t)p[i];
            p[i] = cast(char*)p + pos;
        }
        return p;
    }


    extern (D) void backtrace_symbols_fd(const(void*)* buffer, int size, int fd)
    {
        import core.sys.posix.unistd : write;
        import core.stdc.stdlib : alloca;

        if (size <= 0) return;

        Dl_info info;
        foreach (i, addr; buffer[0 .. size])
        {
            if (dladdr(addr, &info) == 0)
                (cast(ubyte*)&info)[0 .. info.sizeof] = 0;
            fixupDLInfo(addr, info);

            enum maxAlloca = 1024;
            enum min = (size_t a, size_t b) => a <= b ? a : b;
            immutable len = min(formatStackFrame(null, 0, addr, info), maxAlloca);
            assert(len > 0);

            auto p = cast(char*)alloca(len);
            if (p is null) return;

            formatStackFrame(p, len, addr, info) >= len || assert(0);
            p[len - 1] = '\n';
            write(fd, p, len);
        }
    }


    private void fixupDLInfo(const(void)* addr, ref Dl_info info)
    {
        if (info.dli_fname is null) info.dli_fname = "???";
        if (info.dli_fbase is null) info.dli_fbase = null;
        if (info.dli_sname is null) info.dli_sname = "???";
        if (info.dli_saddr is null) info.dli_saddr = cast(void*)addr;
    }


    private size_t formatStackFrame(char* p, size_t plen, const(void)* addr, const ref Dl_info info)
    {
        import core.stdc.stdio : snprintf;

        immutable off = addr - info.dli_saddr;
        immutable len = snprintf(p, plen, "%p <%s+%zd> at %s",
                                 addr, info.dli_sname, off, info.dli_fname);
        assert(len > 0);
        return cast(size_t)len + 1; // + '\0'
    }
}

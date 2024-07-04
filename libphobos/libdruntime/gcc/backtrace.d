// GNU D Compiler routines for stack backtrace support.
// Copyright (C) 2013-2024 Free Software Foundation, Inc.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

module gcc.backtrace;

import gcc.libbacktrace;

// Max size per line of the traceback.
private enum MAX_BUFSIZE = 1536;

static if (BACKTRACE_SUPPORTED && !BACKTRACE_USES_MALLOC)
{
    import core.stdc.stdint, core.stdc.string, core.stdc.stdio;
    private enum MAXFRAMES = 128;

    extern(C) int simpleCallback(void* data, uintptr_t pc)
    {
        auto context = cast(LibBacktrace)data;

        if (context.numPCs == MAXFRAMES)
            return 1;

        context.pcs[context.numPCs++] = pc;
        return 0;
    }

    /*
     * Used for backtrace_create_state and backtrace_simple
     */
    extern(C) void simpleErrorCallback(void* data, const(char)* msg, int errnum) @nogc
    {
        if (data) // context is not available in backtrace_create_state
        {
            auto context = cast(LibBacktrace)data;
            strncpy(context.errorBuf.ptr, msg, context.errorBuf.length - 1);
            context.error = errnum;
        }
    }

    /*
     * Used for backtrace_pcinfo
     */
    extern(C) int pcinfoCallback(void* data, uintptr_t pc, const(char)* filename,
                                 int lineno, const(char)* func)
    {
        auto context = cast(SymbolCallbackInfo*)data;

        // Try to get the function name via backtrace_syminfo
        if (func is null)
        {
            SymbolCallbackInfo2 info;
            info.base = context;
            info.filename = filename;
            info.lineno = lineno;
            if (backtrace_syminfo(context.state, pc, &syminfoCallback2, null, &info) != 0)
            {
                return context.retval;
            }
        }

        auto sym = SymbolOrError(0, SymbolInfo(func, filename, lineno, cast(void*)pc));
        context.retval = context.applyCB(context.num, sym);
        context.num++;

        return context.retval;
    }

    /*
     * Used for backtrace_pcinfo and backtrace_syminfo
     */
    extern(C) void pcinfoErrorCallback(void* data, const(char)* msg, int errnum)
    {
        auto context = cast(SymbolCallbackInfo*)data;

        if (errnum == -1)
        {
            context.noInfo = true;
            return;
        }

        SymbolOrError symError;
        symError.errnum = errnum;
        symError.msg = msg;

        size_t i = 0;
        context.retval = context.applyCB(i, symError);
    }

    /*
     * Used for backtrace_syminfo (in opApply)
     */
    extern(C) void syminfoCallback(void* data, uintptr_t pc,
                                   const(char)* symname, uintptr_t symval)
    {
        auto context = cast(SymbolCallbackInfo*)data;

        auto sym = SymbolOrError(0, SymbolInfo(symname, null, 0, cast(void*)pc));
        context.retval = context.applyCB(context.num, sym);

        context.num++;
    }

    /*
     * This callback is used if backtrace_syminfo is called from the pcinfoCallback
     * callback. It merges it's information with the information from pcinfoCallback.
     */
    extern(C) void syminfoCallback2(void* data, uintptr_t pc,
                                    const(char)* symname, uintptr_t symval)
    {
        auto context = cast(SymbolCallbackInfo2*)data;

        auto sym = SymbolOrError(0, SymbolInfo(symname, context.filename, context.lineno,
            cast(void*)pc));
        context.base.retval = context.base.applyCB(context.base.num, sym);

        context.base.num++;
    }

    /*
     * The callback type used with the opApply overload which returns a SymbolOrError
     */
    private alias int delegate(ref size_t, ref SymbolOrError) ApplyCallback;

    /*
     * Passed to syminfoCallback, pcinfoCallback and pcinfoErrorCallback
     */
    struct SymbolCallbackInfo
    {
        bool noInfo = false;       // True if debug info / symbol table is not available
        size_t num = 0;            // Counter for opApply
        int retval;                // Value returned by applyCB
        backtrace_state* state;

        // info.fileName / funcName / errmsg may become invalid after this delegate returned
        ApplyCallback applyCB;

        void reset()
        {
            noInfo = false;
            num = 0;
        }
    }

    /*
     * Passed to the syminfoCallback2 callback. That function merges it's
     * funcName with this information and updates base as all other callbacks do.
     */
    struct SymbolCallbackInfo2
    {
        SymbolCallbackInfo* base;
        const(char)* filename;
        int lineno;
    }

    /*
     * Contains a valid symbol or an error message if errnum is != 0.
     */
    struct SymbolOrError
    {
        int errnum; // == 0: No error
        union
        {
            SymbolInfo symbol;
            const(char)* msg;
        }
    }

    // FIXME: state is never freed as libbacktrace doesn't provide a free function...
    public class LibBacktrace : Throwable.TraceInfo
    {
        static void initLibBacktrace() @nogc
        {
            if (!initialized)
            {
                state = backtrace_create_state(null, false, &simpleErrorCallback, null);
                initialized = true;
            }
        }

        this(int firstFrame) @nogc
        {
            _firstFrame = firstFrame;

            initLibBacktrace();

            if (state)
            {
                backtrace_simple(state, _firstFrame, &simpleCallback,
                                 &simpleErrorCallback, cast(void*)this);
            }
        }

        override int opApply(scope int delegate(ref const(char[])) dg) const
        {
            return opApply(
                (ref size_t, ref const(char[]) buf)
                {
                    return dg(buf);
                }
            );
        }

        override int opApply(scope int delegate(ref size_t, ref const(char[])) dg) const
        {
            return opApply(
                (ref size_t i, ref SymbolOrError sym)
                {
                    char[MAX_BUFSIZE] buffer = void;
                    char[] msg;
                    if (sym.errnum != 0)
                    {
                        auto retval = snprintf(buffer.ptr, buffer.length,
                                               "libbacktrace error: '%s' errno: %d", sym.msg, sym.errnum);
                        if (retval >= buffer.length)
                            retval = buffer.length - 1; // Ignore zero terminator
                        if (retval > 0)
                            msg = buffer[0 .. retval];
                        return dg(i, msg);
                    }
                    else
                    {
                        msg = formatLine(sym.symbol, buffer);
                        int ret = dg(i, msg);

                        if (!ret && sym.symbol.funcName && strcmp(sym.symbol.funcName, "_Dmain") == 0)
                            return 1;
                        return ret;
                    }
                }
            );
        }

        int opApply(scope ApplyCallback dg) const
        {
            initLibBacktrace();

            // If backtrace_simple produced an error report it and exit
            if (!state || error != 0)
            {
                size_t pos = 0;
                SymbolOrError symError;
                if (!state)
                {
                    symError.msg = "libbacktrace failed to initialize\0";
                    symError.errnum = 1;
                }
                else
                {
                    symError.errnum = error;
                    symError.msg = errorBuf.ptr;
                }

                return dg(pos, symError);
            }

            SymbolCallbackInfo cinfo;
            cinfo.applyCB = dg;
            cinfo.state = cast(backtrace_state*)state;

            // Try using debug info first
            foreach (i, pc; pcs[0 .. numPCs])
            {
                // FIXME: We may violate const guarantees here...
                if (backtrace_pcinfo(cast(backtrace_state*)state, pc, &pcinfoCallback,
                    &pcinfoErrorCallback, &cinfo) != 0)
                {
                    break; // User delegate requested abort or no debug info at all
                }
            }

            // If no error or other error which has already been reported via callback
            if (!cinfo.noInfo)
                return cinfo.retval;

            // Try using symbol table
            cinfo.reset();
            foreach (pc; pcs[0 .. numPCs])
            {
                if (backtrace_syminfo(cast(backtrace_state*)state, pc, &syminfoCallback,
                    &pcinfoErrorCallback, &cinfo) == 0)
                {
                    break;
                }
            }

            if (!cinfo.noInfo)
                return cinfo.retval;

            // No symbol table
            foreach (i, pc; pcs[0 .. numPCs])
            {
                auto sym = SymbolOrError(0, SymbolInfo(null, null, 0, cast(void*)pc));
                if (auto ret = dg(i, sym) != 0)
                    return ret;
            }

            return 0;
        }

        override string toString() const
        {
            string buf;
            foreach (i, const(char[]) line; this)
                buf ~= i ? "\n" ~ line : line;
            return buf;
        }

    private:
        static backtrace_state* state = null;
        static bool initialized       = false;
        size_t                 numPCs = 0;
        uintptr_t[MAXFRAMES] pcs;

        int       error = 0;
        int _firstFrame = 0;
        char[128] errorBuf = "\0";
    }
}
else
{
    /*
     * Our fallback backtrace implementation using libgcc's unwind
     * and backtrace support. In theory libbacktrace should be available
     * everywhere where this code works. We keep it anyway till libbacktrace
     * is well-tested.
     */
    public class UnwindBacktrace : Throwable.TraceInfo
    {
        this(int firstFrame) @nogc
        {
            _firstFrame = firstFrame;
            _callstack = getBacktrace();
            _framelist = getBacktraceSymbols(_callstack);
        }

        override int opApply(scope int delegate(ref const(char[])) dg) const
        {
            return opApply(
                (ref size_t, ref const(char[]) buf)
                {
                    return dg(buf);
                }
            );
        }

        override int opApply(scope int delegate(ref size_t, ref const(char[])) dg) const
        {
            char[MAX_BUFSIZE] buffer = void;
            int ret = 0;

            for (int i = _firstFrame; i < _framelist.entries; ++i)
            {
                auto pos = cast(size_t)(i - _firstFrame);
                auto msg = formatLine(_framelist.symbols[i], buffer);
                ret = dg(pos, msg);
                if (ret)
                    break;
            }
            return ret;
        }

        override string toString() const
        {
            string buf;
            foreach (i, line; this)
                buf ~= i ? "\n" ~ line : line;
            return buf;
        }

    private:
        BTSymbolData     _framelist;
        UnwindBacktraceData _callstack;
        int              _firstFrame = 0;
    }

    // Implementation details
private:
    import gcc.unwind;

    version (linux)
        import core.sys.linux.dlfcn;
    else version (OSX)
        import core.sys.darwin.dlfcn;
    else version (FreeBSD)
        import core.sys.freebsd.dlfcn;
    else version (NetBSD)
        import core.sys.netbsd.dlfcn;
    else version (OpenBSD)
        import core.sys.openbsd.dlfcn;
    else version (Solaris)
        import core.sys.solaris.dlfcn;
    else version (Posix)
        import core.sys.posix.dlfcn;

    private enum MAXFRAMES = 128;

    struct UnwindBacktraceData
    {
        void*[MAXFRAMES] callstack;
        int numframes = 0;
    }

    struct BTSymbolData
    {
        size_t entries;
        SymbolInfo[MAXFRAMES] symbols;
    }

    static extern (C) _Unwind_Reason_Code unwindCB(_Unwind_Context *ctx, void *d)
    {
        UnwindBacktraceData* bt = cast(UnwindBacktraceData*)d;
        if (bt.numframes >= MAXFRAMES)
            return _URC_NO_REASON;

        bt.callstack[bt.numframes] = cast(void*)_Unwind_GetIP(ctx);
        bt.numframes++;
        return _URC_NO_REASON;
    }

    UnwindBacktraceData getBacktrace() @nogc
    {
        UnwindBacktraceData stackframe;
        _Unwind_Backtrace(&unwindCB, &stackframe);
        return stackframe;
    }

    BTSymbolData getBacktraceSymbols(UnwindBacktraceData data) @nogc
    {
        BTSymbolData symData;

        for (auto i = 0; i < data.numframes; i++)
        {
            static if ( __traits(compiles, Dl_info))
            {
                Dl_info funcInfo;

                if (data.callstack[i] !is null && dladdr(data.callstack[i], &funcInfo) != 0)
                {
                    symData.symbols[symData.entries].funcName = funcInfo.dli_sname;

                    symData.symbols[symData.entries].address = data.callstack[i];
                    symData.entries++;
                }
                else
                {
                    symData.symbols[symData.entries].address = data.callstack[i];
                    symData.entries++;
                }
            }
            else
            {
                symData.symbols[symData.entries].address = data.callstack[i];
                symData.entries++;
            }
        }

        return symData;
    }
}

/*
 * Struct representing a symbol (function) in the backtrace
 */
struct SymbolInfo
{
    const(char)* funcName, fileName;
    size_t line;
    const(void)* address;
}

/*
 * Format one output line for symbol sym.
 * Returns a slice of buffer.
 */
char[] formatLine(const SymbolInfo sym, return ref char[MAX_BUFSIZE] buffer)
{
    import core.demangle, core.stdc.config;
    import core.stdc.stdio : snprintf, printf;
    import core.stdc.string : strlen;

    size_t bufferLength = 0;

    void appendToBuffer(Args...)(const(char)* format, Args args)
    {
        const count = snprintf(buffer.ptr + bufferLength,
                               buffer.length - bufferLength,
                               format, args);
        assert(count >= 0);
        bufferLength += count;
        if (bufferLength >= buffer.length)
            bufferLength = buffer.length - 1;
    }


    if (sym.fileName is null)
        appendToBuffer("??:? ");
    else
        appendToBuffer("%s:%d ", sym.fileName, sym.line);

    if (sym.funcName is null)
        appendToBuffer("???");
    else
    {
        char[1024] symbol = void;
        auto demangled = demangle(sym.funcName[0 .. strlen(sym.funcName)], symbol);

        if (demangled.length > 0)
            appendToBuffer("%.*s ", cast(int) demangled.length, demangled.ptr);
    }

    version (D_LP64)
        appendToBuffer("[0x%llx]", cast(size_t)sym.address);
    else
        appendToBuffer("[0x%x]", cast(size_t)sym.address);

    return buffer[0 .. bufferLength];
}


unittest
{
    char[MAX_BUFSIZE] sbuf = '\0';
    char[] result;
    string longString;
    for (size_t i = 0; i < 60; i++)
        longString ~= "abcdefghij";
    longString ~= '\0';

    auto symbol = SymbolInfo(null, null, 0, null);
    result = formatLine(symbol, sbuf);
    assert(result.length < MAX_BUFSIZE && result.ptr[result.length] == '\0' && sbuf[$-1] == '\0');

    symbol = SymbolInfo(longString.ptr, null, 0, null);
    result = formatLine(symbol, sbuf);
    assert(result.length < MAX_BUFSIZE && result.ptr[result.length] == '\0' && sbuf[$-1] == '\0');

    symbol = SymbolInfo("func", "test.d", 0, null);
    result = formatLine(symbol, sbuf);
    assert(result.length < MAX_BUFSIZE && result.ptr[result.length] == '\0' && sbuf[$-1] == '\0');

    symbol = SymbolInfo("func", longString.ptr, 0, null);
    result = formatLine(symbol, sbuf);
    assert(result.length < MAX_BUFSIZE && result.ptr[result.length] == '\0' && sbuf[$-1] == '\0');

    symbol = SymbolInfo(longString.ptr, "test.d", 0, null);
    result = formatLine(symbol, sbuf);
    assert(result.length < MAX_BUFSIZE && result.ptr[result.length] == '\0' && sbuf[$-1] == '\0');

    symbol = SymbolInfo(longString.ptr, longString.ptr, 0, null);
    result = formatLine(symbol, sbuf);
    assert(result.length < MAX_BUFSIZE && result.ptr[result.length] == '\0' && sbuf[$-1] == '\0');

    symbol = SymbolInfo("func", "test.d", 1000, null);
    result = formatLine(symbol, sbuf);
    assert(result.length < MAX_BUFSIZE && result.ptr[result.length] == '\0' && sbuf[$-1] == '\0');

    symbol = SymbolInfo(null, (longString[0..500] ~ '\0').ptr, 100000000, null);
    result = formatLine(symbol, sbuf);
    assert(result.length < MAX_BUFSIZE && result.ptr[result.length] == '\0' && sbuf[$-1] == '\0');

    symbol = SymbolInfo("func", "test.d", 0, cast(void*)0x100000);
    result = formatLine(symbol, sbuf);
    assert(result.length < MAX_BUFSIZE && result.ptr[result.length] == '\0' && sbuf[$-1] == '\0');

    symbol = SymbolInfo("func", null, 0, cast(void*)0x100000);
    result = formatLine(symbol, sbuf);
    assert(result.length < MAX_BUFSIZE && result.ptr[result.length] == '\0' && sbuf[$-1] == '\0');

    symbol = SymbolInfo(null, "test.d", 0, cast(void*)0x100000);
    result = formatLine(symbol, sbuf);
    assert(result.length < MAX_BUFSIZE && result.ptr[result.length] == '\0' && sbuf[$-1] == '\0');

    symbol = SymbolInfo(longString.ptr, "test.d", 0, cast(void*)0x100000);
    result = formatLine(symbol, sbuf);
    assert(result.length < MAX_BUFSIZE && result.ptr[result.length] == '\0' && sbuf[$-1] == '\0');
}

/**
 * ...
 *
 * Copyright: Copyright Benjamin Thaut 2010 - 2013.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Benjamin Thaut, Sean Kelly
 * Source:    $(DRUNTIMESRC core/sys/windows/_stacktrace.d)
 */

module core.sys.windows.stacktrace;
version (Windows):

import core.demangle;
import core.stdc.stdlib;
import core.stdc.string;
import core.sys.windows.dbghelp;
import core.sys.windows.imagehlp /+: ADDRESS_MODE+/;
import core.sys.windows.winbase;
import core.sys.windows.windef;

//debug=PRINTF;
debug(PRINTF) import core.stdc.stdio;


extern(Windows) void RtlCaptureContext(CONTEXT* ContextRecord) @nogc;
extern(Windows) DWORD GetEnvironmentVariableA(LPCSTR lpName, LPSTR pBuffer, DWORD nSize);

extern(Windows) alias USHORT function(ULONG FramesToSkip, ULONG FramesToCapture, PVOID *BackTrace, PULONG BackTraceHash) @nogc RtlCaptureStackBackTraceFunc;

private __gshared RtlCaptureStackBackTraceFunc RtlCaptureStackBackTrace;
private __gshared CRITICAL_SECTION mutex; // cannot use core.sync.mutex.Mutex unfortunately (cyclic dependency...)
private __gshared immutable bool initialized;


class StackTrace : Throwable.TraceInfo
{
public:
    /**
     * Constructor
     * Params:
     *  skip = The number of stack frames to skip.
     *  context = The context to receive the stack trace from. Can be null.
     */
    this(size_t skip, CONTEXT* context) @nogc
    {
        if (context is null)
        {
            version (Win64)
                enum INTERNALFRAMES = 3;
            else version (Win32)
                enum INTERNALFRAMES = 2;

            skip += INTERNALFRAMES; //skip the stack frames within the StackTrace class
        }
        else
        {
            //When a exception context is given the first stack frame is repeated for some reason
            version (Win64)
                enum INTERNALFRAMES = 1;
            else version (Win32)
                enum INTERNALFRAMES = 1;

            skip += INTERNALFRAMES;
        }
        if (initialized)
            m_trace = trace(tracebuf[], skip, context);
    }

    override int opApply( scope int delegate(ref const(char[])) dg ) const
    {
        return opApply( (ref size_t, ref const(char[]) buf)
                        {
                            return dg( buf );
                        });
    }


    override int opApply( scope int delegate(ref size_t, ref const(char[])) dg ) const
    {
        int result;
        foreach ( i, e; resolve(m_trace) )
        {
            if ( (result = dg( i, e )) != 0 )
                break;
        }
        return result;
    }


    @trusted override string toString() const
    {
        string result;

        foreach ( e; this )
        {
            result ~= e ~ "\n";
        }
        return result;
    }

    /**
     * Receive a stack trace in the form of an address list. One form accepts
     * an allocated buffer, the other form automatically allocates the buffer.
     *
     * Params:
     *  skip = How many stack frames should be skipped.
     *  context = The context that should be used. If null the current context is used.
     *  buffer = The buffer to use for the trace. This should be at least 63 elements.
     * Returns:
     *  A list of addresses that can be passed to resolve at a later point in time.
     */
    static ulong[] trace(size_t skip = 0, CONTEXT* context = null)
    {
        return trace(new ulong[63], skip, context);
    }

    /// ditto
    static ulong[] trace(ulong[] buffer, size_t skip = 0, CONTEXT* context = null) @nogc
    {
        EnterCriticalSection(&mutex);
        scope(exit) LeaveCriticalSection(&mutex);

        return traceNoSync(buffer, skip, context);
    }

    /**
     * Resolve a stack trace.
     * Params:
     *  addresses = A list of addresses to resolve.
     * Returns:
     *  An array of strings with the results.
     */
    @trusted static char[][] resolve(const(ulong)[] addresses)
    {
        // FIXME: make @nogc to avoid having to disable resolution within finalizers
        import core.memory : GC;
        if (GC.inFinalizer)
            return null;

        EnterCriticalSection(&mutex);
        scope(exit) LeaveCriticalSection(&mutex);

        return resolveNoSync(addresses);
    }

private:
    ulong[128] tracebuf;
    ulong[] m_trace;


    static ulong[] traceNoSync(ulong[] buffer, size_t skip, CONTEXT* context) @nogc
    {
        auto dbghelp  = DbgHelp.get();
        if (dbghelp is null)
            return []; // dbghelp.dll not available

        if (buffer.length >= 63 && RtlCaptureStackBackTrace !is null &&
            context is null)
        {
            version (Win64)
            {
                auto bufptr = cast(void**)buffer.ptr;
            }
            version (Win32)
            {
                size_t[63] bufstorage = void; // On windows xp the sum of "frames to skip" and "frames to capture" can't be greater then 63
                auto bufptr = cast(void**)bufstorage.ptr;
            }
            auto backtraceLength = RtlCaptureStackBackTrace(cast(ULONG)skip, cast(ULONG)(63 - skip), bufptr, null);

            // If we get a backtrace and it does not have the maximum length use it.
            // Otherwise rely on tracing through StackWalk64 which is slower but works when no frame pointers are available.
            if (backtraceLength > 1 && backtraceLength < 63 - skip)
            {
                debug(PRINTF) printf("Using result from RtlCaptureStackBackTrace\n");
                version (Win32)
                {
                    foreach (i, ref e; buffer[0 .. backtraceLength])
                    {
                        e = bufstorage[i];
                    }
                }
                return buffer[0..backtraceLength];
            }
        }

        HANDLE       hThread  = GetCurrentThread();
        HANDLE       hProcess = GetCurrentProcess();
        CONTEXT      ctxt;

        if (context is null)
        {
            ctxt.ContextFlags = CONTEXT_FULL;
            RtlCaptureContext(&ctxt);
        }
        else
        {
            ctxt = *context;
        }

        //x86
        STACKFRAME64 stackframe;
        with (stackframe)
        {
            version (X86)
            {
                enum Flat = ADDRESS_MODE.AddrModeFlat;
                AddrPC.Offset    = ctxt.Eip;
                AddrPC.Mode      = Flat;
                AddrFrame.Offset = ctxt.Ebp;
                AddrFrame.Mode   = Flat;
                AddrStack.Offset = ctxt.Esp;
                AddrStack.Mode   = Flat;
            }
        else version (X86_64)
            {
                enum Flat = ADDRESS_MODE.AddrModeFlat;
                AddrPC.Offset    = ctxt.Rip;
                AddrPC.Mode      = Flat;
                AddrFrame.Offset = ctxt.Rbp;
                AddrFrame.Mode   = Flat;
                AddrStack.Offset = ctxt.Rsp;
                AddrStack.Mode   = Flat;
            }
        }

        version (X86)         enum imageType = IMAGE_FILE_MACHINE_I386;
        else version (X86_64) enum imageType = IMAGE_FILE_MACHINE_AMD64;
        else                  static assert(0, "unimplemented");

        size_t frameNum = 0;
        size_t nframes = 0;

        // do ... while so that we don't skip the first stackframe
        do
        {
            if (frameNum >= skip)
            {
                buffer[nframes++] = stackframe.AddrPC.Offset;
                if (nframes >= buffer.length)
                    break;
            }
            frameNum++;
        }
        while (dbghelp.StackWalk64(imageType, hProcess, hThread, &stackframe,
                                   &ctxt, null, null, null, null));
        return buffer[0 .. nframes];
    }

    static char[][] resolveNoSync(const(ulong)[] addresses)
    {
        auto dbghelp  = DbgHelp.get();
        if (dbghelp is null)
            return []; // dbghelp.dll not available

        HANDLE hProcess = GetCurrentProcess();

        static struct BufSymbol
        {
        align(1):
            IMAGEHLP_SYMBOLA64 _base;
            TCHAR[1024] _buf = void;
        }
        BufSymbol bufSymbol=void;
        IMAGEHLP_SYMBOLA64* symbol = &bufSymbol._base;
        symbol.SizeOfStruct = IMAGEHLP_SYMBOLA64.sizeof;
        symbol.MaxNameLength = bufSymbol._buf.length;

        char[][] trace;
        foreach (pc; addresses)
        {
            char[] res;
            if (dbghelp.SymGetSymFromAddr64(hProcess, pc, null, symbol) &&
                *symbol.Name.ptr)
            {
                DWORD disp;
                IMAGEHLP_LINEA64 line=void;
                line.SizeOfStruct = IMAGEHLP_LINEA64.sizeof;

                if (dbghelp.SymGetLineFromAddr64(hProcess, pc, &disp, &line))
                    res = formatStackFrame(cast(void*)pc, symbol.Name.ptr,
                                           line.FileName, line.LineNumber);
                else
                    res = formatStackFrame(cast(void*)pc, symbol.Name.ptr);
            }
            else
                res = formatStackFrame(cast(void*)pc);
            trace ~= res;
        }
        return trace;
    }

    static char[] formatStackFrame(void* pc)
    {
        import core.stdc.stdio : snprintf;
        char[2+2*size_t.sizeof+1] buf=void;

        immutable len = snprintf(buf.ptr, buf.length, "0x%p", pc);
        cast(uint)len < buf.length || assert(0);
        return buf[0 .. len].dup;
    }

    static char[] formatStackFrame(void* pc, char* symName)
    {
        char[2048] demangleBuf=void;

        auto res = formatStackFrame(pc);
        res ~= " in ";
        const(char)[] tempSymName = symName[0 .. strlen(symName)];
        res ~= demangle(tempSymName, demangleBuf);
        return res;
    }

    static char[] formatStackFrame(void* pc, char* symName,
                                   const scope char* fileName, uint lineNum)
    {
        import core.stdc.stdio : snprintf;
        char[11] buf=void;

        auto res = formatStackFrame(pc, symName);
        res ~= " at ";
        res ~= fileName[0 .. strlen(fileName)];
        res ~= "(";
        immutable len = snprintf(buf.ptr, buf.length, "%u", lineNum);
        cast(uint)len < buf.length || assert(0);
        res ~= buf[0 .. len];
        res ~= ")";
        return res;
    }
}


private string generateSearchPath()
{
    __gshared string[3] defaultPathList = ["_NT_SYMBOL_PATH",
                                           "_NT_ALTERNATE_SYMBOL_PATH",
                                           "SYSTEMROOT"];

    string path;
    char[2048] temp = void;
    DWORD len;

    foreach ( e; defaultPathList )
    {
        if ( (len = GetEnvironmentVariableA( e.ptr, temp.ptr, temp.length )) > 0 )
        {
            path ~= temp[0 .. len];
            path ~= ";";
        }
    }
    path ~= "\0";
    return path;
}


shared static this()
{
    auto dbghelp = DbgHelp.get();

    if ( dbghelp is null )
        return; // dbghelp.dll not available

    auto kernel32Handle = LoadLibraryA( "kernel32.dll" );
    if (kernel32Handle !is null)
    {
        RtlCaptureStackBackTrace = cast(RtlCaptureStackBackTraceFunc) GetProcAddress(kernel32Handle, "RtlCaptureStackBackTrace");
        debug(PRINTF)
        {
            if (RtlCaptureStackBackTrace !is null)
                printf("Found RtlCaptureStackBackTrace\n");
        }
    }

    debug(PRINTF)
    {
        API_VERSION* dbghelpVersion = dbghelp.ImagehlpApiVersion();
        printf("DbgHelp Version %d.%d.%d\n", dbghelpVersion.MajorVersion, dbghelpVersion.MinorVersion, dbghelpVersion.Revision);
    }

    HANDLE hProcess = GetCurrentProcess();

    DWORD symOptions = dbghelp.SymGetOptions();
    symOptions |= SYMOPT_LOAD_LINES;
    symOptions |= SYMOPT_FAIL_CRITICAL_ERRORS;
    symOptions |= SYMOPT_DEFERRED_LOAD;
    symOptions  = dbghelp.SymSetOptions( symOptions );

    debug(PRINTF) printf("Search paths: %s\n", generateSearchPath().ptr);

    if (!dbghelp.SymInitialize(hProcess, generateSearchPath().ptr, TRUE))
        return;

    InitializeCriticalSection(&mutex);
    initialized = true;
}

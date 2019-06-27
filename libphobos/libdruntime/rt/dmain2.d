/**
 * Contains druntime startup and shutdown routines.
 *
 * Copyright: Copyright Digital Mars 2000 - 2013.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Walter Bright, Sean Kelly
 * Source: $(DRUNTIMESRC src/rt/_dmain2.d)
 */

/* NOTE: This file has been patched from the original DMD distribution to
 * work with the GDC compiler.
 */
module rt.dmain2;

private
{
    import rt.memory;
    import rt.sections;
    import core.atomic;
    import core.stdc.stddef;
    import core.stdc.stdlib;
    import core.stdc.string;
    import core.stdc.stdio;   // for printf()
    import core.stdc.errno : errno;
}

version (Windows)
{
    private import core.stdc.wchar_;
    private import core.sys.windows.windows;

    pragma(lib, "shell32.lib"); // needed for CommandLineToArgvW
}

version (FreeBSD)
{
    import core.stdc.fenv;
}
version (NetBSD)
{
    import core.stdc.fenv;
}
version (DragonFlyBSD)
{
    import core.stdc.fenv;
}

extern (C) void _d_monitor_staticctor();
extern (C) void _d_monitor_staticdtor();
extern (C) void _d_critical_init();
extern (C) void _d_critical_term();
extern (C) void gc_init();
extern (C) void gc_term();
extern (C) void lifetime_init();
extern (C) void rt_moduleCtor();
extern (C) void rt_moduleTlsCtor();
extern (C) void rt_moduleDtor();
extern (C) void rt_moduleTlsDtor();
extern (C) void thread_joinAll();
extern (C) bool runModuleUnitTests();
extern (C) void _d_initMonoTime();

version (OSX)
{
    // The bottom of the stack
    extern (C) __gshared void* __osx_stack_end = cast(void*)0xC0000000;
}

version (CRuntime_Microsoft)
{
    extern(C) void init_msvc();
}

/***********************************
 * These are a temporary means of providing a GC hook for DLL use.  They may be
 * replaced with some other similar functionality later.
 */
extern (C)
{
    void* gc_getProxy();
    void  gc_setProxy(void* p);
    void  gc_clrProxy();

    alias void* function()      gcGetFn;
    alias void  function(void*) gcSetFn;
    alias void  function()      gcClrFn;
}

version (Windows)
{
    /*******************************************
     * Loads a DLL written in D with the name 'name'.
     * Returns:
     *      opaque handle to the DLL if successfully loaded
     *      null if failure
     */
    extern (C) void* rt_loadLibrary(const char* name)
    {
        return initLibrary(.LoadLibraryA(name));
    }

    extern (C) void* rt_loadLibraryW(const wchar_t* name)
    {
        return initLibrary(.LoadLibraryW(name));
    }

    void* initLibrary(void* mod)
    {
        // BUG: LoadLibrary() call calls rt_init(), which fails if proxy is not set!
        // (What? LoadLibrary() is a Windows API call, it shouldn't call rt_init().)
        if (mod is null)
            return mod;
        gcSetFn gcSet = cast(gcSetFn) GetProcAddress(mod, "gc_setProxy");
        if (gcSet !is null)
        {   // BUG: Set proxy, but too late
            gcSet(gc_getProxy());
        }
        return mod;
    }

    /*************************************
     * Unloads DLL that was previously loaded by rt_loadLibrary().
     * Input:
     *      ptr     the handle returned by rt_loadLibrary()
     * Returns:
     *      1   succeeded
     *      0   some failure happened
     */
    extern (C) int rt_unloadLibrary(void* ptr)
    {
        gcClrFn gcClr  = cast(gcClrFn) GetProcAddress(ptr, "gc_clrProxy");
        if (gcClr !is null)
            gcClr();
        return FreeLibrary(ptr) != 0;
    }
}

/* To get out-of-band access to the args[] passed to main().
 */

__gshared string[] _d_args = null;

extern (C) string[] rt_args()
{
    return _d_args;
}

// make arguments passed to main available for being filtered by runtime initializers
extern(C) __gshared char[][] _d_main_args = null;

// This variable is only ever set by a debugger on initialization so it should
// be fine to leave it as __gshared.
extern (C) __gshared bool rt_trapExceptions = true;

alias void delegate(Throwable) ExceptionHandler;

/**
 * Keep track of how often rt_init/rt_term were called.
 */
shared size_t _initCount;

/**********************************************
 * Initialize druntime.
 * If a C program wishes to call D code, and there's no D main(), then it
 * must call rt_init() and rt_term().
 */
extern (C) int rt_init()
{
    /* @@BUG 11380 @@ Need to synchronize rt_init/rt_term calls for
       version (Shared) druntime, because multiple C threads might
       initialize different D libraries without knowing about the
       shared druntime. Also we need to attach any thread that calls
       rt_init. */
    if (atomicOp!"+="(_initCount, 1) > 1) return 1;

    version (CRuntime_Microsoft)
        init_msvc();

    _d_monitor_staticctor();
    _d_critical_init();

    try
    {
        initSections();
        // this initializes mono time before anything else to allow usage
        // in other druntime systems.
        _d_initMonoTime();
        gc_init();
        initStaticDataGC();
        lifetime_init();
        rt_moduleCtor();
        rt_moduleTlsCtor();
        return 1;
    }
    catch (Throwable t)
    {
        _initCount = 0;
        _d_print_throwable(t);
    }
    _d_critical_term();
    _d_monitor_staticdtor();
    return 0;
}

/**********************************************
 * Terminate use of druntime.
 */
extern (C) int rt_term()
{
    if (!_initCount) return 0; // was never initialized
    if (atomicOp!"-="(_initCount, 1)) return 1;

    try
    {
        rt_moduleTlsDtor();
        thread_joinAll();
        rt_moduleDtor();
        gc_term();
        return 1;
    }
    catch (Throwable t)
    {
        _d_print_throwable(t);
    }
    finally
    {
        finiSections();
        _d_critical_term();
        _d_monitor_staticdtor();
    }
    return 0;
}

/**********************************************
 * Trace handler
 */
alias Throwable.TraceInfo function(void* ptr) TraceHandler;
private __gshared TraceHandler traceHandler = null;


/**
 * Overrides the default trace hander with a user-supplied version.
 *
 * Params:
 *  h = The new trace handler.  Set to null to use the default handler.
 */
extern (C) void  rt_setTraceHandler(TraceHandler h)
{
    traceHandler = h;
}

/**
 * Return the current trace handler
 */
extern (C) TraceHandler rt_getTraceHandler()
{
    return traceHandler;
}

/**
 * This function will be called when an exception is constructed.  The
 * user-supplied trace handler will be called if one has been supplied,
 * otherwise no trace will be generated.
 *
 * Params:
 *  ptr = A pointer to the location from which to generate the trace, or null
 *        if the trace should be generated from within the trace handler
 *        itself.
 *
 * Returns:
 *  An object describing the current calling context or null if no handler is
 *  supplied.
 */
extern (C) Throwable.TraceInfo _d_traceContext(void* ptr = null)
{
    if (traceHandler is null)
        return null;
    return traceHandler(ptr);
}

/***********************************
 * Provide out-of-band access to the original C argc/argv
 * passed to this program via main(argc,argv).
 */

struct CArgs
{
    int argc;
    char** argv;
}

__gshared CArgs _cArgs;

extern (C) CArgs rt_cArgs() @nogc
{
    return _cArgs;
}

/***********************************
 * Run the given main function.
 * Its purpose is to wrap the D main()
 * function and catch any unhandled exceptions.
 */
private alias extern(C) int function(char[][] args) MainFunc;

extern (C) int _d_run_main(int argc, char **argv, MainFunc mainFunc)
{
    // Remember the original C argc/argv
    _cArgs.argc = argc;
    _cArgs.argv = argv;

    int result;

    version (OSX)
    {   /* OSX does not provide a way to get at the top of the
         * stack, except for the magic value 0xC0000000.
         * But as far as the gc is concerned, argv is at the top
         * of the main thread's stack, so save the address of that.
         */
        __osx_stack_end = cast(void*)&argv;
    }

    version (FreeBSD) version (D_InlineAsm_X86)
    {
        /*
         * FreeBSD/i386 sets the FPU precision mode to 53 bit double.
         * Make it 64 bit extended.
         */
        ushort fpucw;
        asm
        {
            fstsw   fpucw;
            or      fpucw, 0b11_00_111111; // 11: use 64 bit extended-precision
                                           // 111111: mask all FP exceptions
            fldcw   fpucw;
        }
    }
    version (CRuntime_Microsoft)
    {
        // enable full precision for reals
        version (GNU)
        {
            size_t fpu_cw;
            asm { "fstcw %0" : "=m" (fpu_cw); }
            fpu_cw |= 0b11_00_111111;  // 11: use 64 bit extended-precision
                                       // 111111: mask all FP exceptions
            asm { "fldcw %0" : "=m" (fpu_cw); }
        }
        else version (Win64)
            asm
            {
                push    RAX;
                fstcw   word ptr [RSP];
                or      [RSP], 0b11_00_111111; // 11: use 64 bit extended-precision
                                               // 111111: mask all FP exceptions
                fldcw   word ptr [RSP];
                pop     RAX;
            }
        else version (Win32)
        {
            asm
            {
                push    EAX;
                fstcw   word ptr [ESP];
                or      [ESP], 0b11_00_111111; // 11: use 64 bit extended-precision
                // 111111: mask all FP exceptions
                fldcw   word ptr [ESP];
                pop     EAX;
            }
        }
    }

    version (Windows)
    {
        /* Because we want args[] to be UTF-8, and Windows doesn't guarantee that,
         * we ignore argc/argv and go get the Windows command line again as UTF-16.
         * Then, reparse into wargc/wargs, and then use Windows API to convert
         * to UTF-8.
         */
        const wchar_t* wCommandLine = GetCommandLineW();
        immutable size_t wCommandLineLength = wcslen(wCommandLine);
        int wargc;
        wchar_t** wargs = CommandLineToArgvW(wCommandLine, &wargc);
        // assert(wargc == argc); /* argc can be broken by Unicode arguments */

        // Allocate args[] on the stack - use wargc
        char[][] args = (cast(char[]*) alloca(wargc * (char[]).sizeof))[0 .. wargc];

        // This is required because WideCharToMultiByte requires int as input.
        assert(wCommandLineLength <= cast(size_t) int.max, "Wide char command line length must not exceed int.max");

        immutable size_t totalArgsLength = WideCharToMultiByte(CP_UTF8, 0, wCommandLine, cast(int)wCommandLineLength, null, 0, null, null);
        {
            char* totalArgsBuff = cast(char*) alloca(totalArgsLength);
            size_t j = 0;
            foreach (i; 0 .. wargc)
            {
                immutable size_t wlen = wcslen(wargs[i]);
                assert(wlen <= cast(size_t) int.max, "wlen cannot exceed int.max");
                immutable int len = WideCharToMultiByte(CP_UTF8, 0, &wargs[i][0], cast(int) wlen, null, 0, null, null);
                args[i] = totalArgsBuff[j .. j + len];
                if (len == 0)
                    continue;
                j += len;
                assert(j <= totalArgsLength);
                WideCharToMultiByte(CP_UTF8, 0, &wargs[i][0], cast(int) wlen, &args[i][0], len, null, null);
            }
        }
        LocalFree(wargs);
        wargs = null;
        wargc = 0;
    }
    else version (Posix)
    {
        // Allocate args[] on the stack
        char[][] args = (cast(char[]*) alloca(argc * (char[]).sizeof))[0 .. argc];

        size_t totalArgsLength = 0;
        foreach (i, ref arg; args)
        {
            arg = argv[i][0 .. strlen(argv[i])];
            totalArgsLength += arg.length;
        }
    }
    else
        static assert(0);

    /* Create a copy of args[] on the stack to be used for main, so that rt_args()
     * cannot be modified by the user.
     * Note that when this function returns, _d_args will refer to garbage.
     */
    {
        _d_args = cast(string[]) args;
        auto buff = cast(char[]*) alloca(args.length * (char[]).sizeof + totalArgsLength);

        char[][] argsCopy = buff[0 .. args.length];
        auto argBuff = cast(char*) (buff + args.length);
        size_t j = 0;
        foreach (arg; args)
        {
            if (arg.length < 6 || arg[0..6] != "--DRT-") // skip D runtime options
            {
                argsCopy[j++] = (argBuff[0 .. arg.length] = arg[]);
                argBuff += arg.length;
            }
        }
        args = argsCopy[0..j];
    }

    bool trapExceptions = rt_trapExceptions;

    version (Windows)
    {
        if (IsDebuggerPresent())
            trapExceptions = false;
        version (GNU)
        {
            /* IsDebuggerPresent doesn't detect GDC.  Would be nice to have
               some way of detecting valid console output */
            trapExceptions = true;
        }
    }

    void tryExec(scope void delegate() dg)
    {
        if (trapExceptions)
        {
            try
            {
                dg();
            }
            catch (Throwable t)
            {
                _d_print_throwable(t);
                result = EXIT_FAILURE;
            }
        }
        else
        {
            dg();
        }
    }

    // NOTE: The lifetime of a process is much like the lifetime of an object:
    //       it is initialized, then used, then destroyed.  If initialization
    //       fails, the successive two steps are never reached.  However, if
    //       initialization succeeds, then cleanup will occur even if the use
    //       step fails in some way.  Here, the use phase consists of running
    //       the user's main function.  If main terminates with an exception,
    //       the exception is handled and then cleanup begins.  An exception
    //       thrown during cleanup, however, will abort the cleanup process.
    void runAll()
    {
        if (rt_init() && runModuleUnitTests())
            tryExec({ result = mainFunc(args); });
        else
            result = EXIT_FAILURE;

        if (!rt_term())
            result = (result == EXIT_SUCCESS) ? EXIT_FAILURE : result;
    }

    tryExec(&runAll);

    // Issue 10344: flush stdout and return nonzero on failure
    if (.fflush(.stdout) != 0)
    {
        .fprintf(.stderr, "Failed to flush stdout: %s\n", .strerror(.errno));
        if (result == 0)
        {
            result = EXIT_FAILURE;
        }
    }

    return result;
}

private void formatThrowable(Throwable t, scope void delegate(in char[] s) nothrow sink)
{
    for (; t; t = t.next)
    {
        t.toString(sink); sink("\n");

        auto e = cast(Error)t;
        if (e is null || e.bypassedException is null) continue;

        sink("=== Bypassed ===\n");
        for (auto t2 = e.bypassedException; t2; t2 = t2.next)
        {
            t2.toString(sink); sink("\n");
        }
        sink("=== ~Bypassed ===\n");
    }
}

extern (C) void _d_print_throwable(Throwable t)
{
    // On Windows, a console may not be present to print the output to.
    // Show a message box instead. If the console is present, convert to
    // the correct encoding.
    version (Windows)
    {
        static struct WSink
        {
            wchar_t* ptr; size_t len;

            void sink(in char[] s) scope nothrow
            {
                if (!s.length) return;
                int swlen = MultiByteToWideChar(
                        CP_UTF8, 0, s.ptr, cast(int)s.length, null, 0);
                if (!swlen) return;

                auto newPtr = cast(wchar_t*)realloc(ptr,
                        (this.len + swlen + 1) * wchar_t.sizeof);
                if (!newPtr) return;
                ptr = newPtr;
                auto written = MultiByteToWideChar(
                        CP_UTF8, 0, s.ptr, cast(int)s.length, ptr+len, swlen);
                len += written;
            }

            wchar_t* get() { if (ptr) ptr[len] = 0; return ptr; }

            void free() { .free(ptr); }
        }

        HANDLE windowsHandle(int fd)
        {
            version (CRuntime_Microsoft)
                return cast(HANDLE)_get_osfhandle(fd);
            else
                return _fdToHandle(fd);
        }

        auto hStdErr = windowsHandle(fileno(stderr));
        CONSOLE_SCREEN_BUFFER_INFO sbi;
        bool isConsole = GetConsoleScreenBufferInfo(hStdErr, &sbi) != 0;

        // ensure the exception is shown at the beginning of the line, while also
        // checking whether stderr is a valid file
        int written = fprintf(stderr, "\n");
        if (written <= 0)
        {
            WSink buf;
            formatThrowable(t, &buf.sink);

            if (buf.ptr)
            {
                WSink caption;
                if (t)
                    caption.sink(t.classinfo.name);

                // Avoid static user32.dll dependency for console applications
                // by loading it dynamically as needed
                auto user32 = LoadLibraryW("user32.dll");
                if (user32)
                {
                    alias typeof(&MessageBoxW) PMessageBoxW;
                    auto pMessageBoxW = cast(PMessageBoxW)
                        GetProcAddress(user32, "MessageBoxW");
                    if (pMessageBoxW)
                        pMessageBoxW(null, buf.get(), caption.get(), MB_ICONERROR);
                }
                FreeLibrary(user32);
                caption.free();
                buf.free();
            }
            return;
        }
        else if (isConsole)
        {
            WSink buf;
            formatThrowable(t, &buf.sink);

            if (buf.ptr)
            {
                uint codepage = GetConsoleOutputCP();
                int slen = WideCharToMultiByte(codepage, 0,
                        buf.ptr, cast(int)buf.len, null, 0, null, null);
                auto sptr = cast(char*)malloc(slen * char.sizeof);
                if (sptr)
                {
                    WideCharToMultiByte(codepage, 0,
                        buf.ptr, cast(int)buf.len, sptr, slen, null, null);
                    WriteFile(hStdErr, sptr, slen, null, null);
                    free(sptr);
                }
                buf.free();
            }
            return;
        }
    }

    void sink(in char[] buf) scope nothrow
    {
        fprintf(stderr, "%.*s", cast(int)buf.length, buf.ptr);
    }
    formatThrowable(t, &sink);
}

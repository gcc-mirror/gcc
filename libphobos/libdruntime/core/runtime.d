/**
 * The runtime module exposes information specific to the D runtime code.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Sean Kelly
 * Source:    $(DRUNTIMESRC core/_runtime.d)
 */

/*          Copyright Sean Kelly 2005 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */

/* NOTE: This file has been patched from the original DMD distribution to
 * work with the GDC compiler.
 */
module core.runtime;

version (Windows) import core.stdc.wchar_ : wchar_t;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

/// C interface for Runtime.loadLibrary
extern (C) void* rt_loadLibrary(const char* name);
/// ditto
version (Windows) extern (C) void* rt_loadLibraryW(const wchar_t* name);
/// C interface for Runtime.unloadLibrary, returns 1/0 instead of bool
extern (C) int rt_unloadLibrary(void* ptr);

/// C interface for Runtime.initialize, returns 1/0 instead of bool
extern(C) int rt_init();
/// C interface for Runtime.terminate, returns 1/0 instead of bool
extern(C) int rt_term();

private
{
    alias bool function() ModuleUnitTester;
    alias bool function(Object) CollectHandler;
    alias Throwable.TraceInfo function( void* ptr ) TraceHandler;

    extern (C) void rt_setCollectHandler( CollectHandler h );
    extern (C) CollectHandler rt_getCollectHandler();

    extern (C) void rt_setTraceHandler( TraceHandler h );
    extern (C) TraceHandler rt_getTraceHandler();

    alias void delegate( Throwable ) ExceptionHandler;
    extern (C) void _d_print_throwable(Throwable t);

    extern (C) void* thread_stackBottom();

    extern (C) string[] rt_args();
    extern (C) CArgs rt_cArgs() @nogc;
}


static this()
{
    // NOTE: Some module ctors will run before this handler is set, so it's
    //       still possible the app could exit without a stack trace.  If
    //       this becomes an issue, the handler could be set in C main
    //       before the module ctors are run.
    Runtime.traceHandler = &defaultTraceHandler;
}


///////////////////////////////////////////////////////////////////////////////
// Runtime
///////////////////////////////////////////////////////////////////////////////

/**
 * Stores the unprocessed arguments supplied when the
 * process was started.
 */
struct CArgs
{
    int argc; /// The argument count.
    char** argv; /// The arguments as a C array of strings.
}

/**
 * This struct encapsulates all functionality related to the underlying runtime
 * module for the calling context.
 */
struct Runtime
{
    /**
     * Initializes the runtime.  This call is to be used in instances where the
     * standard program initialization process is not executed.  This is most
     * often in shared libraries or in libraries linked to a C program.
     * If the runtime was already successfully initialized this returns true.
     * Each call to initialize must be paired by a call to $(LREF terminate).
     *
     * Returns:
     *  true if initialization succeeded or false if initialization failed.
     */
    static bool initialize()
    {
        return !!rt_init();
    }

    deprecated("Please use the overload of Runtime.initialize that takes no argument.")
    static bool initialize(ExceptionHandler dg = null)
    {
        return !!rt_init();
    }


    /**
     * Terminates the runtime.  This call is to be used in instances where the
     * standard program termination process will not be not executed.  This is
     * most often in shared libraries or in libraries linked to a C program.
     * If the runtime was not successfully initialized the function returns false.
     *
     * Returns:
     *  true if termination succeeded or false if termination failed.
     */
    static bool terminate()
    {
        return !!rt_term();
    }

    deprecated("Please use the overload of Runtime.terminate that takes no argument.")
    static bool terminate(ExceptionHandler dg = null)
    {
        return !!rt_term();
    }


    /**
     * Returns the arguments supplied when the process was started.
     *
     * Returns:
     *  The arguments supplied when this process was started.
     */
    static @property string[] args()
    {
        return rt_args();
    }

    /**
     * Returns the unprocessed C arguments supplied when the process was started.
     * Use this when you need to supply argc and argv to C libraries.
     *
     * Returns:
     *  A $(LREF CArgs) struct with the arguments supplied when this process was started.
     *
     * Example:
     * ---
     * import core.runtime;
     *
     * // A C library function requiring char** arguments
     * extern(C) void initLibFoo(int argc, char** argv);
     *
     * void main()
     * {
     *     auto args = Runtime.cArgs;
     *     initLibFoo(args.argc, args.argv);
     * }
     * ---
     */
    static @property CArgs cArgs() @nogc
    {
        return rt_cArgs();
    }

    /**
     * Locates a dynamic library with the supplied library name and dynamically
     * loads it into the caller's address space.  If the library contains a D
     * runtime it will be integrated with the current runtime.
     *
     * Params:
     *  name = The name of the dynamic library to load.
     *
     * Returns:
     *  A reference to the library or null on error.
     */
    static void* loadLibrary()(in char[] name)
    {
        import core.stdc.stdlib : free, malloc;
        version (Windows)
        {
            import core.sys.windows.windows;

            if (name.length == 0) return null;
            // Load a DLL at runtime
            auto len = MultiByteToWideChar(
                CP_UTF8, 0, name.ptr, cast(int)name.length, null, 0);
            if (len == 0)
                return null;

            auto buf = cast(wchar_t*)malloc((len+1) * wchar_t.sizeof);
            if (buf is null) return null;
            scope (exit) free(buf);

            len = MultiByteToWideChar(
                CP_UTF8, 0, name.ptr, cast(int)name.length, buf, len);
            if (len == 0)
                return null;

            buf[len] = '\0';

            return rt_loadLibraryW(buf);
        }
        else version (Posix)
        {
            /* Need a 0-terminated C string for the dll name
             */
            immutable len = name.length;
            auto buf = cast(char*)malloc(len + 1);
            if (!buf) return null;
            scope (exit) free(buf);

            buf[0 .. len] = name[];
            buf[len] = 0;

            return rt_loadLibrary(buf);
        }
    }


    /**
     * Unloads the dynamic library referenced by p.  If this library contains a
     * D runtime then any necessary finalization or cleanup of that runtime
     * will be performed.
     *
     * Params:
     *  p = A reference to the library to unload.
     */
    static bool unloadLibrary()(void* p)
    {
        return !!rt_unloadLibrary(p);
    }


    /**
     * Overrides the default trace mechanism with a user-supplied version.  A
     * trace represents the context from which an exception was thrown, and the
     * trace handler will be called when this occurs.  The pointer supplied to
     * this routine indicates the base address from which tracing should occur.
     * If the supplied pointer is null then the trace routine should determine
     * an appropriate calling context from which to begin the trace.
     *
     * Params:
     *  h = The new trace handler.  Set to null to use the default handler.
     */
    static @property void traceHandler( TraceHandler h )
    {
        rt_setTraceHandler( h );
    }

    /**
     * Gets the current trace handler.
     *
     * Returns:
     *  The current trace handler or null if none has been set.
     */
    static @property TraceHandler traceHandler()
    {
        return rt_getTraceHandler();
    }

    /**
     * Overrides the default collect hander with a user-supplied version.  This
     * routine will be called for each resource object that is finalized in a
     * non-deterministic manner--typically during a garbage collection cycle.
     * If the supplied routine returns true then the object's dtor will called
     * as normal, but if the routine returns false than the dtor will not be
     * called.  The default behavior is for all object dtors to be called.
     *
     * Params:
     *  h = The new collect handler.  Set to null to use the default handler.
     */
    static @property void collectHandler( CollectHandler h )
    {
        rt_setCollectHandler( h );
    }


    /**
     * Gets the current collect handler.
     *
     * Returns:
     *  The current collect handler or null if none has been set.
     */
    static @property CollectHandler collectHandler()
    {
        return rt_getCollectHandler();
    }


    /**
     * Overrides the default module unit tester with a user-supplied version.
     * This routine will be called once on program initialization.  The return
     * value of this routine indicates to the runtime whether the tests ran
     * without error.
     *
     * Params:
     *  h = The new unit tester.  Set to null to use the default unit tester.
     *
     * Example:
     * ---------
     * version (unittest) shared static this()
     * {
     *     import core.runtime;
     *
     *     Runtime.moduleUnitTester = &customModuleUnitTester;
     * }
     *
     * bool customModuleUnitTester()
     * {
     *     import std.stdio;
     *
     *     writeln("Using customModuleUnitTester");
     *
     *     // Do the same thing as the default moduleUnitTester:
     *     size_t failed = 0;
     *     foreach (m; ModuleInfo)
     *     {
     *         if (m)
     *         {
     *             auto fp = m.unitTest;
     *
     *             if (fp)
     *             {
     *                 try
     *                 {
     *                     fp();
     *                 }
     *                 catch (Throwable e)
     *                 {
     *                     writeln(e);
     *                     failed++;
     *                 }
     *             }
     *         }
     *     }
     *     return failed == 0;
     * }
     * ---------
     */
    static @property void moduleUnitTester( ModuleUnitTester h )
    {
        sm_moduleUnitTester = h;
    }


    /**
     * Gets the current module unit tester.
     *
     * Returns:
     *  The current module unit tester handler or null if none has been set.
     */
    static @property ModuleUnitTester moduleUnitTester()
    {
        return sm_moduleUnitTester;
    }


private:

    // NOTE: This field will only ever be set in a static ctor and should
    //       never occur within any but the main thread, so it is safe to
    //       make it __gshared.
    __gshared ModuleUnitTester sm_moduleUnitTester = null;
}

/**
 * Set source file path for coverage reports.
 *
 * Params:
 *  path = The new path name.
 * Note:
 *  This is a dmd specific setting.
 */
extern (C) void dmd_coverSourcePath(string path);

/**
 * Set output path for coverage reports.
 *
 * Params:
 *  path = The new path name.
 * Note:
 *  This is a dmd specific setting.
 */
extern (C) void dmd_coverDestPath(string path);

/**
 * Enable merging of coverage reports with existing data.
 *
 * Params:
 *  flag = enable/disable coverage merge mode
 * Note:
 *  This is a dmd specific setting.
 */
extern (C) void dmd_coverSetMerge(bool flag);

/**
 * Set the output file name for profile reports (-profile switch).
 * An empty name will set the output to stdout.
 *
 * Params:
 *  name = file name
 * Note:
 *  This is a dmd specific setting.
 */
extern (C) void trace_setlogfilename(string name);

/**
 * Set the output file name for the optimized profile linker DEF file (-profile switch).
 * An empty name will set the output to stdout.
 *
 * Params:
 *  name = file name
 * Note:
 *  This is a dmd specific setting.
 */
extern (C) void trace_setdeffilename(string name);

/**
 * Set the output file name for memory profile reports (-profile=gc switch).
 * An empty name will set the output to stdout.
 *
 * Params:
 *  name = file name
 * Note:
 *  This is a dmd specific setting.
 */
extern (C) void profilegc_setlogfilename(string name);

///////////////////////////////////////////////////////////////////////////////
// Overridable Callbacks
///////////////////////////////////////////////////////////////////////////////


/**
 * This routine is called by the runtime to run module unit tests on startup.
 * The user-supplied unit tester will be called if one has been supplied,
 * otherwise all unit tests will be run in sequence.
 *
 * Returns:
 *  true if execution should continue after testing is complete and false if
 *  not.  Default behavior is to return true.
 */
extern (C) bool runModuleUnitTests()
{
    // backtrace
    version (GNU)
        import gcc.backtrace;
    version (CRuntime_Glibc)
        import core.sys.linux.execinfo;
    else version (Darwin)
        import core.sys.darwin.execinfo;
    else version (FreeBSD)
        import core.sys.freebsd.execinfo;
    else version (NetBSD)
        import core.sys.netbsd.execinfo;
    else version (DragonFlyBSD)
        import core.sys.dragonflybsd.execinfo;
    else version (Windows)
        import core.sys.windows.stacktrace;
    else version (Solaris)
        import core.sys.solaris.execinfo;
    else version (CRuntime_UClibc)
        import core.sys.linux.execinfo;

    static if ( __traits( compiles, new LibBacktrace(0) ) )
    {
        import core.sys.posix.signal; // segv handler

        static extern (C) void unittestSegvHandler( int signum, siginfo_t* info, void* ptr )
        {
            import core.stdc.stdio;
            fprintf(stderr, "Segmentation fault while running unittests:\n");
            fprintf(stderr, "----------------\n");

            enum alignment = LibBacktrace.MaxAlignment;
            enum classSize = __traits(classInstanceSize, LibBacktrace);

            void[classSize + alignment] bt_store = void;
            void* alignedAddress = cast(byte*)((cast(size_t)(bt_store.ptr + alignment - 1))
                & ~(alignment - 1));

            (alignedAddress[0 .. classSize]) = typeid(LibBacktrace).initializer[];
            auto bt = cast(LibBacktrace)(alignedAddress);
            // First frame is LibBacktrace ctor. Second is signal handler, but include that for now
            bt.__ctor(1);

            foreach (size_t i, const(char[]) msg; bt)
                fprintf(stderr, "%s\n", msg.ptr ? msg.ptr : "???");
        }

        sigaction_t action = void;
        sigaction_t oldseg = void;
        sigaction_t oldbus = void;

        (cast(byte*) &action)[0 .. action.sizeof] = 0;
        sigfillset( &action.sa_mask ); // block other signals
        action.sa_flags = SA_SIGINFO | SA_RESETHAND;
        action.sa_sigaction = &unittestSegvHandler;
        sigaction( SIGSEGV, &action, &oldseg );
        sigaction( SIGBUS, &action, &oldbus );
        scope( exit )
        {
            sigaction( SIGSEGV, &oldseg, null );
            sigaction( SIGBUS, &oldbus, null );
        }
    }
    else static if ( __traits( compiles, backtrace ) )
    {
        import core.sys.posix.signal; // segv handler

        static extern (C) void unittestSegvHandler( int signum, siginfo_t* info, void* ptr ) nothrow
        {
            static enum MAXFRAMES = 128;
            void*[MAXFRAMES]  callstack;

            auto numframes = backtrace( callstack.ptr, MAXFRAMES );
            backtrace_symbols_fd( callstack.ptr, numframes, 2 );
        }

        sigaction_t action = void;
        sigaction_t oldseg = void;
        sigaction_t oldbus = void;

        (cast(byte*) &action)[0 .. action.sizeof] = 0;
        sigfillset( &action.sa_mask ); // block other signals
        action.sa_flags = SA_SIGINFO | SA_RESETHAND;
        action.sa_sigaction = &unittestSegvHandler;
        sigaction( SIGSEGV, &action, &oldseg );
        sigaction( SIGBUS, &action, &oldbus );
        scope( exit )
        {
            sigaction( SIGSEGV, &oldseg, null );
            sigaction( SIGBUS, &oldbus, null );
        }
    }

    if ( Runtime.sm_moduleUnitTester is null )
    {
        size_t failed = 0;
        foreach ( m; ModuleInfo )
        {
            if ( m )
            {
                auto fp = m.unitTest;

                if ( fp )
                {
                    try
                    {
                        fp();
                    }
                    catch ( Throwable e )
                    {
                        _d_print_throwable(e);
                        failed++;
                    }
                }
            }
        }
        return failed == 0;
    }
    return Runtime.sm_moduleUnitTester();
}


///////////////////////////////////////////////////////////////////////////////
// Default Implementations
///////////////////////////////////////////////////////////////////////////////


/**
 *
 */
Throwable.TraceInfo defaultTraceHandler( void* ptr = null )
{
    // backtrace
    version (GNU)
        import gcc.backtrace;
    version (CRuntime_Glibc)
        import core.sys.linux.execinfo;
    else version (Darwin)
        import core.sys.darwin.execinfo;
    else version (FreeBSD)
        import core.sys.freebsd.execinfo;
    else version (NetBSD)
        import core.sys.netbsd.execinfo;
    else version (DragonFlyBSD)
        import core.sys.dragonflybsd.execinfo;
    else version (Windows)
        import core.sys.windows.stacktrace;
    else version (Solaris)
        import core.sys.solaris.execinfo;
    else version (CRuntime_UClibc)
        import core.sys.linux.execinfo;

    // avoid recursive GC calls in finalizer, trace handlers should be made @nogc instead
    import core.memory : gc_inFinalizer;
    if (gc_inFinalizer)
        return null;

    //printf("runtime.defaultTraceHandler()\n");
    static if ( __traits( compiles, new LibBacktrace(0) ) )
    {
        version (Posix)
        {
            static enum FIRSTFRAME = 4;
        }
        else version (Win64)
        {
            static enum FIRSTFRAME = 4;
        }
        else
        {
            static enum FIRSTFRAME = 0;
        }
        return new LibBacktrace(FIRSTFRAME);
    }
    else static if ( __traits( compiles, new UnwindBacktrace(0) ) )
    {
        version (Posix)
        {
            static enum FIRSTFRAME = 5;
        }
        else version (Win64)
        {
            static enum FIRSTFRAME = 4;
        }
        else
        {
            static enum FIRSTFRAME = 0;
        }
        return new UnwindBacktrace(FIRSTFRAME);
    }
    else static if ( __traits( compiles, backtrace ) )
    {
        import core.demangle;
        import core.stdc.stdlib : free;
        import core.stdc.string : strlen, memchr, memmove;

        class DefaultTraceInfo : Throwable.TraceInfo
        {
            this()
            {
                numframes = 0; //backtrace( callstack, MAXFRAMES );
                if (numframes < 2) // backtrace() failed, do it ourselves
                {
                    static void** getBasePtr()
                    {
                        version (D_InlineAsm_X86)
                            asm { naked; mov EAX, EBP; ret; }
                        else
                        version (D_InlineAsm_X86_64)
                            asm { naked; mov RAX, RBP; ret; }
                        else
                            return null;
                    }

                    auto  stackTop    = getBasePtr();
                    auto  stackBottom = cast(void**) thread_stackBottom();
                    void* dummy;

                    if ( stackTop && &dummy < stackTop && stackTop < stackBottom )
                    {
                        auto stackPtr = stackTop;

                        for ( numframes = 0; stackTop <= stackPtr &&
                                            stackPtr < stackBottom &&
                                            numframes < MAXFRAMES; )
                        {
                            enum CALL_INSTRUCTION_SIZE = 1; // it may not be 1 but it is good enough to get
                                                            // in CALL instruction address range for backtrace
                            callstack[numframes++] = *(stackPtr + 1) - CALL_INSTRUCTION_SIZE;
                            stackPtr = cast(void**) *stackPtr;
                        }
                    }
                }
            }

            override int opApply( scope int delegate(ref const(char[])) dg ) const
            {
                return opApply( (ref size_t, ref const(char[]) buf)
                                {
                                    return dg( buf );
                                } );
            }

            override int opApply( scope int delegate(ref size_t, ref const(char[])) dg ) const
            {
                version (Posix)
                {
                    // NOTE: The first 4 frames with the current implementation are
                    //       inside core.runtime and the object code, so eliminate
                    //       these for readability.  The alternative would be to
                    //       exclude the first N frames that are in a list of
                    //       mangled function names.
                    enum FIRSTFRAME = 4;
                }
                else version (Windows)
                {
                    // NOTE: On Windows, the number of frames to exclude is based on
                    //       whether the exception is user or system-generated, so
                    //       it may be necessary to exclude a list of function names
                    //       instead.
                    enum FIRSTFRAME = 0;
                }

                version (linux) enum enableDwarf = true;
                else version (FreeBSD) enum enableDwarf = true;
                else version (DragonFlyBSD) enum enableDwarf = true;
                else version (Darwin) enum enableDwarf = true;
                else enum enableDwarf = false;

                static if (enableDwarf)
                {
                    import core.internal.traits : externDFunc;

                    alias traceHandlerOpApplyImpl = externDFunc!(
                        "rt.backtrace.dwarf.traceHandlerOpApplyImpl",
                        int function(const void*[], scope int delegate(ref size_t, ref const(char[])))
                    );

                    if (numframes >= FIRSTFRAME)
                    {
                        return traceHandlerOpApplyImpl(
                            callstack[FIRSTFRAME .. numframes],
                            dg
                        );
                    }
                    else
                    {
                        return 0;
                    }
                }
                else
                {
                    const framelist = backtrace_symbols( callstack.ptr, numframes );
                    scope(exit) free(cast(void*) framelist);

                    int ret = 0;
                    for ( int i = FIRSTFRAME; i < numframes; ++i )
                    {
                        char[4096] fixbuf;
                        auto buf = framelist[i][0 .. strlen(framelist[i])];
                        auto pos = cast(size_t)(i - FIRSTFRAME);
                        buf = fixline( buf, fixbuf );
                        ret = dg( pos, buf );
                        if ( ret )
                            break;
                    }
                    return ret;
                }

            }

            override string toString() const
            {
                string buf;
                foreach ( i, line; this )
                    buf ~= i ? "\n" ~ line : line;
                return buf;
            }

        private:
            int     numframes;
            static enum MAXFRAMES = 128;
            void*[MAXFRAMES]  callstack = void;

        private:
            const(char)[] fixline( const(char)[] buf, return ref char[4096] fixbuf ) const
            {
                size_t symBeg, symEnd;
                version (Darwin)
                {
                    // format is:
                    //  1  module    0x00000000 D6module4funcAFZv + 0
                    for ( size_t i = 0, n = 0; i < buf.length; i++ )
                    {
                        if ( ' ' == buf[i] )
                        {
                            n++;
                            while ( i < buf.length && ' ' == buf[i] )
                                i++;
                            if ( 3 > n )
                                continue;
                            symBeg = i;
                            while ( i < buf.length && ' ' != buf[i] )
                                i++;
                            symEnd = i;
                            break;
                        }
                    }
                }
                else version (CRuntime_Glibc)
                {
                    // format is:  module(_D6module4funcAFZv) [0x00000000]
                    // or:         module(_D6module4funcAFZv+0x78) [0x00000000]
                    auto bptr = cast(char*) memchr( buf.ptr, '(', buf.length );
                    auto eptr = cast(char*) memchr( buf.ptr, ')', buf.length );
                    auto pptr = cast(char*) memchr( buf.ptr, '+', buf.length );

                    if (pptr && pptr < eptr)
                        eptr = pptr;

                    if ( bptr++ && eptr )
                    {
                        symBeg = bptr - buf.ptr;
                        symEnd = eptr - buf.ptr;
                    }
                }
                else version (FreeBSD)
                {
                    // format is: 0x00000000 <_D6module4funcAFZv+0x78> at module
                    auto bptr = cast(char*) memchr( buf.ptr, '<', buf.length );
                    auto eptr = cast(char*) memchr( buf.ptr, '+', buf.length );

                    if ( bptr++ && eptr )
                    {
                        symBeg = bptr - buf.ptr;
                        symEnd = eptr - buf.ptr;
                    }
                }
                else version (NetBSD)
                {
                    // format is: 0x00000000 <_D6module4funcAFZv+0x78> at module
                    auto bptr = cast(char*) memchr( buf.ptr, '<', buf.length );
                    auto eptr = cast(char*) memchr( buf.ptr, '+', buf.length );

                    if ( bptr++ && eptr )
                    {
                        symBeg = bptr - buf.ptr;
                        symEnd = eptr - buf.ptr;
                    }
                }
                else version (DragonFlyBSD)
                {
                    // format is: 0x00000000 <_D6module4funcAFZv+0x78> at module
                    auto bptr = cast(char*) memchr( buf.ptr, '<', buf.length );
                    auto eptr = cast(char*) memchr( buf.ptr, '+', buf.length );

                    if ( bptr++ && eptr )
                    {
                        symBeg = bptr - buf.ptr;
                        symEnd = eptr - buf.ptr;
                    }
                }
                else version (Solaris)
                {
                    // format is object'symbol+offset [pc]
                    auto bptr = cast(char*) memchr( buf.ptr, '\'', buf.length );
                    auto eptr = cast(char*) memchr( buf.ptr, '+', buf.length );

                    if ( bptr++ && eptr )
                    {
                        symBeg = bptr - buf.ptr;
                        symEnd = eptr - buf.ptr;
                    }
                }
                else
                {
                    // fallthrough
                }

                assert(symBeg < buf.length && symEnd < buf.length);
                assert(symBeg <= symEnd);

                enum min = (size_t a, size_t b) => a <= b ? a : b;
                if (symBeg == symEnd || symBeg >= fixbuf.length)
                {
                    immutable len = min(buf.length, fixbuf.length);
                    fixbuf[0 .. len] = buf[0 .. len];
                    return fixbuf[0 .. len];
                }
                else
                {
                    fixbuf[0 .. symBeg] = buf[0 .. symBeg];

                    auto sym = demangle(buf[symBeg .. symEnd], fixbuf[symBeg .. $]);

                    if (sym.ptr !is fixbuf.ptr + symBeg)
                    {
                        // demangle reallocated the buffer, copy the symbol to fixbuf
                        immutable len = min(fixbuf.length - symBeg, sym.length);
                        memmove(fixbuf.ptr + symBeg, sym.ptr, len);
                        if (symBeg + len == fixbuf.length)
                            return fixbuf[];
                    }

                    immutable pos = symBeg + sym.length;
                    assert(pos < fixbuf.length);
                    immutable tail = buf.length - symEnd;
                    immutable len = min(fixbuf.length - pos, tail);
                    fixbuf[pos .. pos + len] = buf[symEnd .. symEnd + len];
                    return fixbuf[0 .. pos + len];
                }
            }
        }

        return new DefaultTraceInfo;
    }
    else static if ( __traits( compiles, new StackTrace(0, null) ) )
    {
        version (Win64)
        {
            static enum FIRSTFRAME = 4;
        }
        else version (Win32)
        {
            static enum FIRSTFRAME = 0;
        }
        import core.sys.windows.winnt : CONTEXT;
        auto s = new StackTrace(FIRSTFRAME, cast(CONTEXT*)ptr);
        return s;
    }
    else
    {
        return null;
    }
}

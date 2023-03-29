/**
 * The runtime module exposes information specific to the D runtime code.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Sean Kelly
 * Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/druntime/src/core/runtime.d, _runtime.d)
 * Documentation: https://dlang.org/phobos/core_runtime.html
 */

/* NOTE: This file has been patched from the original DMD distribution to
 * work with the GDC compiler.
 */
module core.runtime;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (GNU)
{
    import gcc.backtrace;
    // This shouldn't be necessary but ensure that code doesn't get mixed
    // It does however prevent the unittest SEGV handler to be installed,
    // which is desireable as it uses backtrace directly.
    private enum hasExecinfo = false;
}
else version (DRuntime_Use_Libunwind)
{
    import core.internal.backtrace.libunwind;
    // This shouldn't be necessary but ensure that code doesn't get mixed
    // It does however prevent the unittest SEGV handler to be installed,
    // which is desireable as it uses backtrace directly.
    private enum hasExecinfo = false;
}
else
    import core.internal.execinfo;

/// C interface for Runtime.loadLibrary
extern (C) void* rt_loadLibrary(const char* name);
/// ditto
version (Windows) extern (C) void* rt_loadLibraryW(const wchar* name);

/// C interface for Runtime.unloadLibrary, returns 1/0 instead of bool
extern (C) int rt_unloadLibrary(void* ptr);

/// C interface for Runtime.initialize, returns 1/0 instead of bool
extern(C) int rt_init();
/// C interface for Runtime.terminate, returns 1/0 instead of bool
extern(C) int rt_term();

/**
 * This type is returned by the module unit test handler to indicate testing
 * results.
 */
struct UnitTestResult
{
    /**
     * Number of modules which were tested
     */
    size_t executed;

    /**
     * Number of modules passed the unittests
     */
    size_t passed;

    /**
     * Should the main function be run or not? This is ignored if any tests
     * failed.
     */
    bool runMain;

    /**
     * Should we print a summary of the results?
     */
    bool summarize;

    /**
     * Simple check for whether execution should continue after unit tests
     * have been run. Works with legacy code that expected a bool return.
     *
     * Returns:
     *    true if execution should continue after testing is complete, false if
     *    not.
     */
    bool opCast(T : bool)() const
    {
        return runMain && (executed == passed);
    }

    /// Simple return code that says unit tests pass, and main should be run
    enum UnitTestResult pass = UnitTestResult(0, 0, true, false);
    /// Simple return code that says unit tests failed.
    enum UnitTestResult fail = UnitTestResult(1, 0, false, false);
}

/// Legacy module unit test handler
alias bool function() ModuleUnitTester;
/// Module unit test handler
alias UnitTestResult function() ExtendedModuleUnitTester;
private
{
    alias bool function(Object) CollectHandler;
    alias Throwable.TraceInfo function( void* ptr ) TraceHandler;

    alias void delegate( Throwable ) ExceptionHandler;
    extern (C) void _d_print_throwable(Throwable t);

    extern (C) void* thread_stackBottom() nothrow @nogc;
}


shared static this()
{
    // NOTE: Some module ctors will run before this handler is set, so it's
    //       still possible the app could exit without a stack trace.  If
    //       this becomes an issue, the handler could be set in C main
    //       before the module ctors are run.
    Runtime.traceHandler(&defaultTraceHandler, &defaultTraceDeallocator);
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

    /**
     * Returns the arguments supplied when the process was started.
     *
     * Returns:
     *  The arguments supplied when this process was started.
     */
    extern(C) pragma(mangle, "rt_args") static @property string[] args();

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
    extern(C) pragma(mangle, "rt_cArgs") static @property CArgs cArgs() @nogc;

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
    static void* loadLibrary()(const scope char[] name)
    {
        import core.stdc.stdlib : free, malloc;
        version (Windows)
        {
            import core.sys.windows.winnls : CP_UTF8, MultiByteToWideChar;
            import core.sys.windows.winnt : WCHAR;

            if (name.length == 0) return null;
            // Load a DLL at runtime
            auto len = MultiByteToWideChar(
                CP_UTF8, 0, name.ptr, cast(int)name.length, null, 0);
            if (len == 0)
                return null;

            auto buf = cast(WCHAR*)malloc((len+1) * WCHAR.sizeof);
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
     * If the deallocator is set, then it is called with the traceinfo when the
     * exception is finalized. The deallocator is only set in the exception if
     * the default handler is used to generate the trace info.
     *
     * Params:
     *  h = The new trace handler.  Set to null to disable exception backtracing.
     *  d = The new trace deallocator. If non-null, this will be called on
     *      exception destruction with the trace info, only when the trace
     *      handler is used to generate TraceInfo.
     */
    extern(C) pragma(mangle, "rt_setTraceHandler") static @property void traceHandler(TraceHandler h,
                    Throwable.TraceDeallocator d = null);

    /**
     * Gets the current trace handler.
     *
     * Returns:
     *  The current trace handler or null if none has been set.
     */
    extern(C) pragma(mangle, "rt_getTraceHandler") static @property TraceHandler traceHandler();

    /**
     * Gets the current trace deallocator.
     *
     * Returns:
     *  The current trace deallocator or null if none has been set.
     */
    extern(C) pragma(mangle, "rt_getTraceDeallocator") static @property Throwable.TraceDeallocator traceDeallocator();

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
    extern(C) pragma(mangle, "rt_setCollectHandler") static @property void collectHandler( CollectHandler h );


    /**
     * Gets the current collect handler.
     *
     * Returns:
     *  The current collect handler or null if none has been set.
     */
    extern(C) pragma(mangle, "rt_getCollectHandler") static @property CollectHandler collectHandler();


    /**
     * Overrides the default module unit tester with a user-supplied version.
     * This routine will be called once on program initialization.  The return
     * value of this routine indicates to the runtime whether the tests ran
     * without error.
     *
     * There are two options for handlers. The `bool` version is deprecated but
     * will be kept for legacy support. Returning `true` from the handler is
     * equivalent to returning `UnitTestResult.pass` from the extended version.
     * Returning `false` from the handler is equivalent to returning
     * `UnitTestResult.fail` from the extended version.
     *
     * See the documentation for `UnitTestResult` to see how you should set up
     * the return structure.
     *
     * See the documentation for `runModuleUnitTests` for how the default
     * algorithm works, or read the example below.
     *
     * Params:
     *  h = The new unit tester.  Set both to null to use the default unit
     *  tester.
     *
     * Example:
     * ---------
     * shared static this()
     * {
     *     import core.runtime;
     *
     *     Runtime.extendedModuleUnitTester = &customModuleUnitTester;
     * }
     *
     * UnitTestResult customModuleUnitTester()
     * {
     *     import std.stdio;
     *
     *     writeln("Using customModuleUnitTester");
     *
     *     // Do the same thing as the default moduleUnitTester:
     *     UnitTestResult result;
     *     foreach (m; ModuleInfo)
     *     {
     *         if (m)
     *         {
     *             auto fp = m.unitTest;
     *
     *             if (fp)
     *             {
     *                 ++result.executed;
     *                 try
     *                 {
     *                     fp();
     *                     ++result.passed;
     *                 }
     *                 catch (Throwable e)
     *                 {
     *                     writeln(e);
     *                 }
     *             }
     *         }
     *     }
     *     if (result.executed != result.passed)
     *     {
     *         result.runMain = false;  // don't run main
     *         result.summarize = true; // print failure
     *     }
     *     else
     *     {
     *         result.runMain = true;    // all UT passed
     *         result.summarize = false; // be quiet about it.
     *     }
     *     return result;
     * }
     * ---------
     */
    static @property void extendedModuleUnitTester( ExtendedModuleUnitTester h )
    {
        sm_extModuleUnitTester = h;
    }

    /// Ditto
    static @property void moduleUnitTester( ModuleUnitTester h )
    {
        sm_moduleUnitTester = h;
    }

    /**
     * Gets the current legacy module unit tester.
     *
     * This property should not be used, but is supported for legacy purposes.
     *
     * Note that if the extended unit test handler is set, this handler will
     * be ignored.
     *
     * Returns:
     *  The current legacy module unit tester handler or null if none has been
     *  set.
     */
    static @property ModuleUnitTester moduleUnitTester()
    {
        return sm_moduleUnitTester;
    }

    /**
     * Gets the current module unit tester.
     *
     * This handler overrides any legacy module unit tester set by the
     * moduleUnitTester property.
     *
     * Returns:
     *  The current  module unit tester handler or null if none has been
     *  set.
     */
    static @property ExtendedModuleUnitTester extendedModuleUnitTester()
    {
        return sm_extModuleUnitTester;
    }

private:

    // NOTE: This field will only ever be set in a static ctor and should
    //       never occur within any but the main thread, so it is safe to
    //       make it __gshared.
    __gshared ExtendedModuleUnitTester sm_extModuleUnitTester = null;
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
 * The user-supplied unit tester will be called if one has been set,
 * otherwise all unit tests will be run in sequence.
 *
 * If the extended unittest handler is registered, this function returns the
 * result from that handler directly.
 *
 * If a legacy boolean returning custom handler is used, `false` maps to
 * `UnitTestResult.fail`, and `true` maps to `UnitTestResult.pass`. This was
 * the original behavior of the unit testing system.
 *
 * If no unittest custom handlers are registered, the following algorithm is
 * executed (the behavior can be affected by the `--DRT-testmode` switch
 * below):
 * 1. Execute any unittests present. For each that fails, print the stack
 *    trace and continue.
 * 2. If no unittests were present, set summarize to false, and runMain to
 *    true.
 * 3. Otherwise, set summarize to true, and runMain to false.
 *
 * See the documentation for `UnitTestResult` for details on how the runtime
 * treats the return value from this function.
 *
 * If the switch `--DRT-testmode` is passed to the executable, it can have
 * one of 3 values:
 * 1. "run-main": even if unit tests are run (and all pass), runMain is set
      to true.
 * 2. "test-or-main": any unit tests present will cause the program to
 *    summarize the results and exit regardless of the result. This is the
 *    default.
 * 3. "test-only", runMain is set to false, even with no tests present.
 *
 * This command-line parameter does not affect custom unit test handlers.
 *
 * Returns:
 *   A `UnitTestResult` struct indicating the result of running unit tests.
 */
extern (C) UnitTestResult runModuleUnitTests()
{
    version (Windows)
        import core.sys.windows.stacktrace;

    static if (__traits(compiles, new LibBacktrace(0)))
    {
        import core.sys.posix.signal; // segv handler

        static extern (C) void unittestSegvHandler(int signum, siginfo_t* info, void* ptr)
        {
            import core.stdc.stdio;
            fprintf(stderr, "Segmentation fault while running unittests:\n");
            fprintf(stderr, "----------------\n");

            // First frame is LibBacktrace ctor. Second is signal handler,
            // but include that for now
            scope bt = new LibBacktrace(1);

            foreach (size_t i, const(char[]) msg; bt)
                fprintf(stderr, "%s\n", msg.ptr ? msg.ptr : "???");
        }

        sigaction_t action = void;
        sigaction_t oldseg = void;
        sigaction_t oldbus = void;

        (cast(byte*) &action)[0 .. action.sizeof] = 0;
        sigfillset(&action.sa_mask); // block other signals
        action.sa_flags = SA_SIGINFO | SA_RESETHAND;
        action.sa_sigaction = &unittestSegvHandler;
        sigaction(SIGSEGV, &action, &oldseg);
        sigaction(SIGBUS, &action, &oldbus);
        scope (exit)
        {
            sigaction(SIGSEGV, &oldseg, null);
            sigaction(SIGBUS, &oldbus, null);
        }
    }
    else static if (hasExecinfo)
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

    if (Runtime.sm_extModuleUnitTester !is null)
        return Runtime.sm_extModuleUnitTester();
    else if (Runtime.sm_moduleUnitTester !is null)
        return Runtime.sm_moduleUnitTester() ? UnitTestResult.pass : UnitTestResult.fail;
    UnitTestResult results;
    foreach ( m; ModuleInfo )
    {
        if ( !m )
            continue;
        auto fp = m.unitTest;
        if ( !fp )
            continue;

        import core.exception;
        ++results.executed;
        try
        {
            fp();
            ++results.passed;
        }
        catch ( Throwable e )
        {
            if ( typeid(e) == typeid(AssertError) )
            {
                // Crude heuristic to figure whether the assertion originates in
                // the unittested module. TODO: improve.
                auto moduleName = m.name;
                if (moduleName.length && e.file.length > moduleName.length
                    && e.file[0 .. moduleName.length] == moduleName)
                {
                    import core.stdc.stdio;
                    printf("%.*s(%llu): [unittest] %.*s\n",
                        cast(int) e.file.length, e.file.ptr, cast(ulong) e.line,
                        cast(int) e.message.length, e.message.ptr);

                    // Exception originates in the same module, don't print
                    // the stack trace.
                    // TODO: omit stack trace only if assert was thrown
                    // directly by the unittest.
                    continue;
                }
            }
            // TODO: perhaps indent all of this stuff.
            _d_print_throwable(e);
        }
    }

    import core.internal.parseoptions : rt_configOption;

    if (results.passed != results.executed)
    {
        // by default, we always print a summary if there are failures.
        results.summarize = true;
    }
    else switch (rt_configOption("testmode", null, false))
    {
    case "run-main":
        results.runMain = true;
        break;
    case "test-only":
        // Never run main, always summarize
        results.summarize = true;
        break;
    case "":
        // By default, do not run main if tests are present.
    case "test-or-main":
        // only run main if there were no tests. Only summarize if we are not
        // running main.
        results.runMain = (results.executed == 0);
        results.summarize = !results.runMain;
        break;
    default:
        assert(0, "Unknown --DRT-testmode option: " ~ rt_configOption("testmode", null, false));
    }

    return results;
}

/**
 * Get the default `Throwable.TraceInfo` implementation for the platform
 *
 * This functions returns a trace handler, allowing to inspect the
 * current stack trace.
 *
 * IMPORTANT NOTE! the returned trace is potentially not GC allocated, and so
 * you must call `defaultTraceDeallocator` when you are finished with the
 * `TraceInfo`
 *
 * Params:
 *   ptr = (Windows only) The context to get the stack trace from.
 *         When `null` (the default), start from the current frame.
 *
 * Returns:
 *   A `Throwable.TraceInfo` implementation suitable to iterate over the stack,
 *   or `null`. If called from a finalizer (destructor), always returns `null`
 *   as trace handlers allocate.
 */
Throwable.TraceInfo defaultTraceHandler( void* ptr = null ) // @nogc
{
    // NOTE: with traces now being allocated using C malloc, no need to worry
    // about GC reentrancy. This code left commented out for reference.
    //
    // avoid recursive GC calls in finalizer, trace handlers should be made @nogc instead
    /*import core.memory : GC;
    if (GC.inFinalizer)
        return null;*/

    static T allocate(T, Args...)(auto ref Args args) @nogc
    {
        import core.lifetime : emplace;
        import core.stdc.stdlib : malloc;
        auto result = cast(T)malloc(__traits(classInstanceSize, T));
        return emplace(result, args);
    }
    static if (__traits(compiles, allocate!LibBacktrace(0)))
    {
        version (Posix)
            static enum FIRSTFRAME = 4;
        else version (Win64)
            static enum FIRSTFRAME = 4;
        else
            static enum FIRSTFRAME = 0;
        return allocate!LibBacktrace(FIRSTFRAME);
    }
    else static if (__traits(compiles, allocate!UnwindBacktrace(0)))
    {
        version (Posix)
            static enum FIRSTFRAME = 5;
        else version (Win64)
            static enum FIRSTFRAME = 4;
        else
            static enum FIRSTFRAME = 0;
        return allocate!UnwindBacktrace(FIRSTFRAME);
    }
    else version (Windows)
    {
        import core.sys.windows.stacktrace;
        static if (__traits(compiles, allocate!StackTrace(0, null)))
        {
            import core.sys.windows.winnt : CONTEXT;
            version (Win64)
                enum FIRSTFRAME = 4;
            else version (Win32)
                enum FIRSTFRAME = 0;
            return allocate!StackTrace(FIRSTFRAME, cast(CONTEXT*)ptr);
        }
        else
            return null;
    }
    else static if (__traits(compiles, allocate!DefaultTraceInfo()))
        return allocate!DefaultTraceInfo();
    else
        return null;
}

/// Example of a simple program printing its stack trace
unittest
{
    import core.runtime;
    import core.stdc.stdio;

    void main()
    {
        auto trace = defaultTraceHandler(null);
        foreach (line; trace)
        {
            printf("%.*s\n", cast(int)line.length, line.ptr);
        }
        defaultTraceDeallocator(trace);
    }
}

/***
 * Deallocate a traceinfo generated by deaultTraceHander.
 *
 * Call this function on a TraceInfo generated via `defaultTraceHandler` when
 * you are done with it. If necessary, this cleans up any manually managed
 * resources from the `TraceInfo`, and invalidates it. After this, the object
 * is no longer valid.
 *
 * Params:
 *      info = The `TraceInfo` to deallocate. This should only be a value that
 *             was returned by `defaultTraceHandler`.
 */
void defaultTraceDeallocator(Throwable.TraceInfo info) nothrow
{
    if (info is null)
        return;
    auto obj = cast(Object)info;
    destroy(obj);
    import core.stdc.stdlib : free;
    free(cast(void *)obj);
}

version (DRuntime_Use_Libunwind)
{
    import core.internal.backtrace.handler;

    alias DefaultTraceInfo = LibunwindHandler;
}
/// Default implementation for most POSIX systems
else static if (hasExecinfo) private class DefaultTraceInfo : Throwable.TraceInfo
{
    import core.demangle;
    import core.stdc.stdlib : free;
    import core.stdc.string : strlen, memchr, memmove;

    this() @nogc
    {
        // it may not be 1 but it is good enough to get
        // in CALL instruction address range for backtrace
        enum CALL_INSTRUCTION_SIZE = 1;

        static if (__traits(compiles, backtrace((void**).init, int.init)))
            numframes = cast(int) backtrace(this.callstack.ptr, MAXFRAMES);
        // Backtrace succeeded, adjust the frame to point to the caller
        if (numframes >= 2)
            foreach (ref elem; this.callstack)
                elem -= CALL_INSTRUCTION_SIZE;
        else // backtrace() failed, do it ourselves
        {
            static void** getBasePtr() @nogc
            {
                version (D_InlineAsm_X86)
                    asm @nogc { naked; mov EAX, EBP; ret; }
                else
                    version (D_InlineAsm_X86_64)
                        asm @nogc { naked; mov RAX, RBP; ret; }
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
        version (linux) enum enableDwarf = true;
        else version (FreeBSD) enum enableDwarf = true;
        else version (DragonFlyBSD) enum enableDwarf = true;
        else version (OpenBSD) enum enableDwarf = true;
        else version (Darwin) enum enableDwarf = true;
        else enum enableDwarf = false;

        const framelist = backtrace_symbols( callstack.ptr, numframes );
        scope(exit) free(cast(void*) framelist);

        static if (enableDwarf)
        {
            import core.internal.backtrace.dwarf;
            return traceHandlerOpApplyImpl(numframes,
                i => callstack[i],
                (i) { auto str = framelist[i][0 .. strlen(framelist[i])]; return getMangledSymbolName(str); },
                dg);
        }
        else
        {
            int ret = 0;
            for (size_t pos = 0; pos < numframes; ++pos)
            {
                char[4096] fixbuf = void;
                auto buf = framelist[pos][0 .. strlen(framelist[pos])];
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

        getMangledSymbolName(buf, symBeg, symEnd);

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

            auto sym = demangle(buf[symBeg .. symEnd], fixbuf[symBeg .. $], getCXXDemangler());

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

/**
 * The thread module provides support for thread creation and management.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2012.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly, Walter Bright, Alex Rønne Petersen, Martin Nowak
 * Source:    $(DRUNTIMESRC core/_thread.d)
 */

/* NOTE: This file has been patched from the original DMD distribution to
 * work with the GDC compiler.
 */
module core.thread;


public import core.time; // for Duration
import core.exception : onOutOfMemoryError;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

private
{
    // interface to rt.tlsgc
    import core.internal.traits : externDFunc;

    alias rt_tlsgc_init = externDFunc!("rt.tlsgc.init", void* function() nothrow @nogc);
    alias rt_tlsgc_destroy = externDFunc!("rt.tlsgc.destroy", void function(void*) nothrow @nogc);

    alias ScanDg = void delegate(void* pstart, void* pend) nothrow;
    alias rt_tlsgc_scan =
        externDFunc!("rt.tlsgc.scan", void function(void*, scope ScanDg) nothrow);

    alias rt_tlsgc_processGCMarks =
        externDFunc!("rt.tlsgc.processGCMarks", void function(void*, scope IsMarkedDg) nothrow);
}

version (Solaris)
{
    import core.sys.solaris.sys.priocntl;
    import core.sys.solaris.sys.types;
}

version (GNU)
{
    import gcc.builtins;
    version (GNU_StackGrowsDown)
        version = StackGrowsDown;
}
else
{
    // this should be true for most architectures
    version = StackGrowsDown;
}

/**
 * Returns the process ID of the calling process, which is guaranteed to be
 * unique on the system. This call is always successful.
 *
 * Example:
 * ---
 * writefln("Current process id: %s", getpid());
 * ---
 */
version (Posix)
{
    alias getpid = core.sys.posix.unistd.getpid;
}
else version (Windows)
{
    alias getpid = core.sys.windows.windows.GetCurrentProcessId;
}


///////////////////////////////////////////////////////////////////////////////
// Thread and Fiber Exceptions
///////////////////////////////////////////////////////////////////////////////


/**
 * Base class for thread exceptions.
 */
class ThreadException : Exception
{
    @safe pure nothrow this(string msg, string file = __FILE__, size_t line = __LINE__, Throwable next = null)
    {
        super(msg, file, line, next);
    }

    @safe pure nothrow this(string msg, Throwable next, string file = __FILE__, size_t line = __LINE__)
    {
        super(msg, file, line, next);
    }
}


/**
* Base class for thread errors to be used for function inside GC when allocations are unavailable.
*/
class ThreadError : Error
{
    @safe pure nothrow this(string msg, string file = __FILE__, size_t line = __LINE__, Throwable next = null)
    {
        super(msg, file, line, next);
    }

    @safe pure nothrow this(string msg, Throwable next, string file = __FILE__, size_t line = __LINE__)
    {
        super(msg, file, line, next);
    }
}

private
{
    import core.atomic, core.memory, core.sync.mutex;

    // Handling unaligned mutexes are not supported on all platforms, so we must
    // ensure that the address of all shared data are appropriately aligned.
    import core.internal.traits : classInstanceAlignment;

    enum mutexAlign = classInstanceAlignment!Mutex;
    enum mutexClassInstanceSize = __traits(classInstanceSize, Mutex);

    //
    // exposed by compiler runtime
    //
    extern (C) void  rt_moduleTlsCtor();
    extern (C) void  rt_moduleTlsDtor();

    /**
     * Hook for whatever EH implementation is used to save/restore some data
     * per stack.
     *
     * Params:
     *     newContext = The return value of the prior call to this function
     *         where the stack was last swapped out, or null when a fiber stack
     *         is switched in for the first time.
     */
    extern(C) void* _d_eh_swapContext(void* newContext) nothrow @nogc;

    version (DigitalMars)
    {
        version (Windows)
            alias swapContext = _d_eh_swapContext;
        else
        {
            extern(C) void* _d_eh_swapContextDwarf(void* newContext) nothrow @nogc;

            void* swapContext(void* newContext) nothrow @nogc
            {
                /* Detect at runtime which scheme is being used.
                 * Eventually, determine it statically.
                 */
                static int which = 0;
                final switch (which)
                {
                    case 0:
                    {
                        assert(newContext == null);
                        auto p = _d_eh_swapContext(newContext);
                        auto pdwarf = _d_eh_swapContextDwarf(newContext);
                        if (p)
                        {
                            which = 1;
                            return p;
                        }
                        else if (pdwarf)
                        {
                            which = 2;
                            return pdwarf;
                        }
                        return null;
                    }
                    case 1:
                        return _d_eh_swapContext(newContext);
                    case 2:
                        return _d_eh_swapContextDwarf(newContext);
                }
            }
        }
    }
    else
        alias swapContext = _d_eh_swapContext;
}


///////////////////////////////////////////////////////////////////////////////
// Thread Entry Point and Signal Handlers
///////////////////////////////////////////////////////////////////////////////


version (Windows)
{
    private
    {
        import core.stdc.stdint : uintptr_t; // for _beginthreadex decl below
        import core.stdc.stdlib;             // for malloc, atexit
        import core.sys.windows.windows;
        import core.sys.windows.threadaux;   // for OpenThreadHandle

        extern (Windows) alias btex_fptr = uint function(void*);
        extern (C) uintptr_t _beginthreadex(void*, uint, btex_fptr, void*, uint, uint*) nothrow;

        //
        // Entry point for Windows threads
        //
        extern (Windows) uint thread_entryPoint( void* arg ) nothrow
        {
            Thread  obj = cast(Thread) arg;
            assert( obj );

            assert( obj.m_curr is &obj.m_main );
            obj.m_main.bstack = getStackBottom();
            obj.m_main.tstack = obj.m_main.bstack;
            obj.m_tlsgcdata = rt_tlsgc_init();

            Thread.setThis(obj);
            Thread.add(obj);
            scope (exit)
            {
                Thread.remove(obj);
            }
            Thread.add(&obj.m_main);

            // NOTE: No GC allocations may occur until the stack pointers have
            //       been set and Thread.getThis returns a valid reference to
            //       this thread object (this latter condition is not strictly
            //       necessary on Windows but it should be followed for the
            //       sake of consistency).

            // TODO: Consider putting an auto exception object here (using
            //       alloca) forOutOfMemoryError plus something to track
            //       whether an exception is in-flight?

            void append( Throwable t )
            {
                if ( obj.m_unhandled is null )
                    obj.m_unhandled = t;
                else
                {
                    Throwable last = obj.m_unhandled;
                    while ( last.next !is null )
                        last = last.next;
                    last.next = t;
                }
            }

            version (D_InlineAsm_X86)
            {
                asm nothrow @nogc { fninit; }
            }

            try
            {
                rt_moduleTlsCtor();
                try
                {
                    obj.run();
                }
                catch ( Throwable t )
                {
                    append( t );
                }
                rt_moduleTlsDtor();
            }
            catch ( Throwable t )
            {
                append( t );
            }
            return 0;
        }


        HANDLE GetCurrentThreadHandle() nothrow @nogc
        {
            const uint DUPLICATE_SAME_ACCESS = 0x00000002;

            HANDLE curr = GetCurrentThread(),
                   proc = GetCurrentProcess(),
                   hndl;

            DuplicateHandle( proc, curr, proc, &hndl, 0, TRUE, DUPLICATE_SAME_ACCESS );
            return hndl;
        }
    }
}
else version (Posix)
{
    private
    {
        import core.stdc.errno;
        import core.sys.posix.semaphore;
        import core.sys.posix.stdlib; // for malloc, valloc, free, atexit
        import core.sys.posix.pthread;
        import core.sys.posix.signal;
        import core.sys.posix.time;

        version (Darwin)
        {
            import core.sys.darwin.mach.thread_act;
            import core.sys.darwin.pthread : pthread_mach_thread_np;
        }

        //
        // Entry point for POSIX threads
        //
        extern (C) void* thread_entryPoint( void* arg ) nothrow
        {
            version (Shared)
            {
                import rt.sections;
                Thread obj = cast(Thread)(cast(void**)arg)[0];
                auto loadedLibraries = (cast(void**)arg)[1];
                .free(arg);
            }
            else
            {
                Thread obj = cast(Thread)arg;
            }
            assert( obj );

            // loadedLibraries need to be inherited from parent thread
            // before initilizing GC for TLS (rt_tlsgc_init)
            version (Shared) inheritLoadedLibraries(loadedLibraries);

            assert( obj.m_curr is &obj.m_main );
            obj.m_main.bstack = getStackBottom();
            obj.m_main.tstack = obj.m_main.bstack;
            obj.m_tlsgcdata = rt_tlsgc_init();

            atomicStore!(MemoryOrder.raw)(obj.m_isRunning, true);
            Thread.setThis(obj); // allocates lazy TLS (see Issue 11981)
            Thread.add(obj);     // can only receive signals from here on
            scope (exit)
            {
                Thread.remove(obj);
                atomicStore!(MemoryOrder.raw)(obj.m_isRunning, false);
            }
            Thread.add(&obj.m_main);

            static extern (C) void thread_cleanupHandler( void* arg ) nothrow @nogc
            {
                Thread  obj = cast(Thread) arg;
                assert( obj );

                // NOTE: If the thread terminated abnormally, just set it as
                //       not running and let thread_suspendAll remove it from
                //       the thread list.  This is safer and is consistent
                //       with the Windows thread code.
                atomicStore!(MemoryOrder.raw)(obj.m_isRunning,false);
            }

            // NOTE: Using void to skip the initialization here relies on
            //       knowledge of how pthread_cleanup is implemented.  It may
            //       not be appropriate for all platforms.  However, it does
            //       avoid the need to link the pthread module.  If any
            //       implementation actually requires default initialization
            //       then pthread_cleanup should be restructured to maintain
            //       the current lack of a link dependency.
            static if ( __traits( compiles, pthread_cleanup ) )
            {
                pthread_cleanup cleanup = void;
                cleanup.push( &thread_cleanupHandler, cast(void*) obj );
            }
            else static if ( __traits( compiles, pthread_cleanup_push ) )
            {
                pthread_cleanup_push( &thread_cleanupHandler, cast(void*) obj );
            }
            else
            {
                static assert( false, "Platform not supported." );
            }

            // NOTE: No GC allocations may occur until the stack pointers have
            //       been set and Thread.getThis returns a valid reference to
            //       this thread object (this latter condition is not strictly
            //       necessary on Windows but it should be followed for the
            //       sake of consistency).

            // TODO: Consider putting an auto exception object here (using
            //       alloca) forOutOfMemoryError plus something to track
            //       whether an exception is in-flight?

            void append( Throwable t )
            {
                if ( obj.m_unhandled is null )
                    obj.m_unhandled = t;
                else
                {
                    Throwable last = obj.m_unhandled;
                    while ( last.next !is null )
                        last = last.next;
                    last.next = t;
                }
            }

            try
            {
                rt_moduleTlsCtor();
                try
                {
                    obj.run();
                }
                catch ( Throwable t )
                {
                    append( t );
                }
                rt_moduleTlsDtor();
                version (Shared) cleanupLoadedLibraries();
            }
            catch ( Throwable t )
            {
                append( t );
            }

            // NOTE: Normal cleanup is handled by scope(exit).

            static if ( __traits( compiles, pthread_cleanup ) )
            {
                cleanup.pop( 0 );
            }
            else static if ( __traits( compiles, pthread_cleanup_push ) )
            {
                pthread_cleanup_pop( 0 );
            }

            return null;
        }


        //
        // Used to track the number of suspended threads
        //
        __gshared sem_t suspendCount;


        extern (C) void thread_suspendHandler( int sig ) nothrow
        in
        {
            assert( sig == suspendSignalNumber );
        }
        body
        {
            void op(void* sp) nothrow
            {
                // NOTE: Since registers are being pushed and popped from the
                //       stack, any other stack data used by this function should
                //       be gone before the stack cleanup code is called below.
                Thread obj = Thread.getThis();
                assert(obj !is null);

                if ( !obj.m_lock )
                {
                    obj.m_curr.tstack = getStackTop();
                }

                sigset_t    sigres = void;
                int         status;

                status = sigfillset( &sigres );
                assert( status == 0 );

                status = sigdelset( &sigres, resumeSignalNumber );
                assert( status == 0 );

                version (FreeBSD) obj.m_suspendagain = false;
                status = sem_post( &suspendCount );
                assert( status == 0 );

                sigsuspend( &sigres );

                if ( !obj.m_lock )
                {
                    obj.m_curr.tstack = obj.m_curr.bstack;
                }
            }

            // avoid deadlocks on FreeBSD, see Issue 13416
            version (FreeBSD)
            {
                auto obj = Thread.getThis();
                if (THR_IN_CRITICAL(obj.m_addr))
                {
                    obj.m_suspendagain = true;
                    if (sem_post(&suspendCount)) assert(0);
                    return;
                }
            }

            callWithStackShell(&op);
        }


        extern (C) void thread_resumeHandler( int sig ) nothrow
        in
        {
            assert( sig == resumeSignalNumber );
        }
        body
        {

        }

        // HACK libthr internal (thr_private.h) macro, used to
        // avoid deadlocks in signal handler, see Issue 13416
        version (FreeBSD) bool THR_IN_CRITICAL(pthread_t p) nothrow @nogc
        {
            import core.sys.posix.config : c_long;
            import core.sys.posix.sys.types : lwpid_t;

            // If the begin of pthread would be changed in libthr (unlikely)
            // we'll run into undefined behavior, compare with thr_private.h.
            static struct pthread
            {
                c_long tid;
                static struct umutex { lwpid_t owner; uint flags; uint[2] ceilings; uint[4] spare; }
                umutex lock;
                uint cycle;
                int locklevel;
                int critical_count;
                // ...
            }
            auto priv = cast(pthread*)p;
            return priv.locklevel > 0 || priv.critical_count > 0;
        }
    }
}
else
{
    // NOTE: This is the only place threading versions are checked.  If a new
    //       version is added, the module code will need to be searched for
    //       places where version-specific code may be required.  This can be
    //       easily accomlished by searching for 'Windows' or 'Posix'.
    static assert( false, "Unknown threading implementation." );
}


///////////////////////////////////////////////////////////////////////////////
// Thread
///////////////////////////////////////////////////////////////////////////////


/**
 * This class encapsulates all threading functionality for the D
 * programming language.  As thread manipulation is a required facility
 * for garbage collection, all user threads should derive from this
 * class, and instances of this class should never be explicitly deleted.
 * A new thread may be created using either derivation or composition, as
 * in the following example.
 */
class Thread
{
    ///////////////////////////////////////////////////////////////////////////
    // Initialization
    ///////////////////////////////////////////////////////////////////////////


    /**
     * Initializes a thread object which is associated with a static
     * D function.
     *
     * Params:
     *  fn = The thread function.
     *  sz = The stack size for this thread.
     *
     * In:
     *  fn must not be null.
     */
    this( void function() fn, size_t sz = 0 ) @safe pure nothrow @nogc
    in
    {
        assert( fn );
    }
    body
    {
        this(sz);
        () @trusted { m_fn   = fn; }();
        m_call = Call.FN;
        m_curr = &m_main;
    }


    /**
     * Initializes a thread object which is associated with a dynamic
     * D function.
     *
     * Params:
     *  dg = The thread function.
     *  sz = The stack size for this thread.
     *
     * In:
     *  dg must not be null.
     */
    this( void delegate() dg, size_t sz = 0 ) @safe pure nothrow @nogc
    in
    {
        assert( dg );
    }
    body
    {
        this(sz);
        () @trusted { m_dg   = dg; }();
        m_call = Call.DG;
        m_curr = &m_main;
    }


    /**
     * Cleans up any remaining resources used by this object.
     */
    ~this() nothrow @nogc
    {
        if ( m_addr == m_addr.init )
        {
            return;
        }

        version (Windows)
        {
            m_addr = m_addr.init;
            CloseHandle( m_hndl );
            m_hndl = m_hndl.init;
        }
        else version (Posix)
        {
            pthread_detach( m_addr );
            m_addr = m_addr.init;
        }
        version (Darwin)
        {
            m_tmach = m_tmach.init;
        }
        rt_tlsgc_destroy( m_tlsgcdata );
        m_tlsgcdata = null;
    }


    ///////////////////////////////////////////////////////////////////////////
    // General Actions
    ///////////////////////////////////////////////////////////////////////////


    /**
     * Starts the thread and invokes the function or delegate passed upon
     * construction.
     *
     * In:
     *  This routine may only be called once per thread instance.
     *
     * Throws:
     *  ThreadException if the thread fails to start.
     */
    final Thread start() nothrow
    in
    {
        assert( !next && !prev );
    }
    body
    {
        auto wasThreaded  = multiThreadedFlag;
        multiThreadedFlag = true;
        scope( failure )
        {
            if ( !wasThreaded )
                multiThreadedFlag = false;
        }

        version (Windows) {} else
        version (Posix)
        {
            pthread_attr_t  attr;

            if ( pthread_attr_init( &attr ) )
                onThreadError( "Error initializing thread attributes" );
            if ( m_sz && pthread_attr_setstacksize( &attr, m_sz ) )
                onThreadError( "Error initializing thread stack size" );
        }

        version (Windows)
        {
            // NOTE: If a thread is just executing DllMain()
            //       while another thread is started here, it holds an OS internal
            //       lock that serializes DllMain with CreateThread. As the code
            //       might request a synchronization on slock (e.g. in thread_findByAddr()),
            //       we cannot hold that lock while creating the thread without
            //       creating a deadlock
            //
            // Solution: Create the thread in suspended state and then
            //       add and resume it with slock acquired
            assert(m_sz <= uint.max, "m_sz must be less than or equal to uint.max");
            m_hndl = cast(HANDLE) _beginthreadex( null, cast(uint) m_sz, &thread_entryPoint, cast(void*) this, CREATE_SUSPENDED, &m_addr );
            if ( cast(size_t) m_hndl == 0 )
                onThreadError( "Error creating thread" );
        }

        slock.lock_nothrow();
        scope(exit) slock.unlock_nothrow();
        {
            ++nAboutToStart;
            pAboutToStart = cast(Thread*)realloc(pAboutToStart, Thread.sizeof * nAboutToStart);
            pAboutToStart[nAboutToStart - 1] = this;
            version (Windows)
            {
                if ( ResumeThread( m_hndl ) == -1 )
                    onThreadError( "Error resuming thread" );
            }
            else version (Posix)
            {
                // NOTE: This is also set to true by thread_entryPoint, but set it
                //       here as well so the calling thread will see the isRunning
                //       state immediately.
                atomicStore!(MemoryOrder.raw)(m_isRunning, true);
                scope( failure ) atomicStore!(MemoryOrder.raw)(m_isRunning, false);

                version (Shared)
                {
                    import rt.sections;
                    auto libs = pinLoadedLibraries();
                    auto ps = cast(void**).malloc(2 * size_t.sizeof);
                    if (ps is null) onOutOfMemoryError();
                    ps[0] = cast(void*)this;
                    ps[1] = cast(void*)libs;
                    if ( pthread_create( &m_addr, &attr, &thread_entryPoint, ps ) != 0 )
                    {
                        unpinLoadedLibraries(libs);
                        .free(ps);
                        onThreadError( "Error creating thread" );
                    }
                }
                else
                {
                    if ( pthread_create( &m_addr, &attr, &thread_entryPoint, cast(void*) this ) != 0 )
                        onThreadError( "Error creating thread" );
                }
            }
            version (Darwin)
            {
                m_tmach = pthread_mach_thread_np( m_addr );
                if ( m_tmach == m_tmach.init )
                    onThreadError( "Error creating thread" );
            }

            return this;
        }
    }

    /**
     * Waits for this thread to complete.  If the thread terminated as the
     * result of an unhandled exception, this exception will be rethrown.
     *
     * Params:
     *  rethrow = Rethrow any unhandled exception which may have caused this
     *            thread to terminate.
     *
     * Throws:
     *  ThreadException if the operation fails.
     *  Any exception not handled by the joined thread.
     *
     * Returns:
     *  Any exception not handled by this thread if rethrow = false, null
     *  otherwise.
     */
    final Throwable join( bool rethrow = true )
    {
        version (Windows)
        {
            if ( WaitForSingleObject( m_hndl, INFINITE ) != WAIT_OBJECT_0 )
                throw new ThreadException( "Unable to join thread" );
            // NOTE: m_addr must be cleared before m_hndl is closed to avoid
            //       a race condition with isRunning. The operation is done
            //       with atomicStore to prevent compiler reordering.
            atomicStore!(MemoryOrder.raw)(*cast(shared)&m_addr, m_addr.init);
            CloseHandle( m_hndl );
            m_hndl = m_hndl.init;
        }
        else version (Posix)
        {
            if ( pthread_join( m_addr, null ) != 0 )
                throw new ThreadException( "Unable to join thread" );
            // NOTE: pthread_join acts as a substitute for pthread_detach,
            //       which is normally called by the dtor.  Setting m_addr
            //       to zero ensures that pthread_detach will not be called
            //       on object destruction.
            m_addr = m_addr.init;
        }
        if ( m_unhandled )
        {
            if ( rethrow )
                throw m_unhandled;
            return m_unhandled;
        }
        return null;
    }


    ///////////////////////////////////////////////////////////////////////////
    // General Properties
    ///////////////////////////////////////////////////////////////////////////


    /**
     * Gets the OS identifier for this thread.
     *
     * Returns:
     *  If the thread hasn't been started yet, returns $(LREF ThreadID)$(D.init).
     *  Otherwise, returns the result of $(D GetCurrentThreadId) on Windows,
     *  and $(D pthread_self) on POSIX.
     *
     *  The value is unique for the current process.
     */
    final @property ThreadID id() @safe @nogc
    {
        synchronized( this )
        {
            return m_addr;
        }
    }


    /**
     * Gets the user-readable label for this thread.
     *
     * Returns:
     *  The name of this thread.
     */
    final @property string name() @safe @nogc
    {
        synchronized( this )
        {
            return m_name;
        }
    }


    /**
     * Sets the user-readable label for this thread.
     *
     * Params:
     *  val = The new name of this thread.
     */
    final @property void name( string val ) @safe @nogc
    {
        synchronized( this )
        {
            m_name = val;
        }
    }


    /**
     * Gets the daemon status for this thread.  While the runtime will wait for
     * all normal threads to complete before tearing down the process, daemon
     * threads are effectively ignored and thus will not prevent the process
     * from terminating.  In effect, daemon threads will be terminated
     * automatically by the OS when the process exits.
     *
     * Returns:
     *  true if this is a daemon thread.
     */
    final @property bool isDaemon() @safe @nogc
    {
        synchronized( this )
        {
            return m_isDaemon;
        }
    }


    /**
     * Sets the daemon status for this thread.  While the runtime will wait for
     * all normal threads to complete before tearing down the process, daemon
     * threads are effectively ignored and thus will not prevent the process
     * from terminating.  In effect, daemon threads will be terminated
     * automatically by the OS when the process exits.
     *
     * Params:
     *  val = The new daemon status for this thread.
     */
    final @property void isDaemon( bool val ) @safe @nogc
    {
        synchronized( this )
        {
            m_isDaemon = val;
        }
    }


    /**
     * Tests whether this thread is running.
     *
     * Returns:
     *  true if the thread is running, false if not.
     */
    final @property bool isRunning() nothrow @nogc
    {
        if ( m_addr == m_addr.init )
        {
            return false;
        }

        version (Windows)
        {
            uint ecode = 0;
            GetExitCodeThread( m_hndl, &ecode );
            return ecode == STILL_ACTIVE;
        }
        else version (Posix)
        {
            return atomicLoad(m_isRunning);
        }
    }


    ///////////////////////////////////////////////////////////////////////////
    // Thread Priority Actions
    ///////////////////////////////////////////////////////////////////////////

    version (Windows)
    {
        @property static int PRIORITY_MIN() @nogc nothrow pure @safe
        {
            return THREAD_PRIORITY_IDLE;
        }

        @property static const(int) PRIORITY_MAX() @nogc nothrow pure @safe
        {
            return THREAD_PRIORITY_TIME_CRITICAL;
        }

        @property static int PRIORITY_DEFAULT() @nogc nothrow pure @safe
        {
            return THREAD_PRIORITY_NORMAL;
        }
    }
    else
    {
        private struct Priority
        {
            int PRIORITY_MIN = int.min;
            int PRIORITY_DEFAULT = int.min;
            int PRIORITY_MAX = int.min;
        }

        /*
        Lazily loads one of the members stored in a hidden global variable of
        type `Priority`. Upon the first access of either member, the entire
        `Priority` structure is initialized. Multiple initializations from
        different threads calling this function are tolerated.

        `which` must be one of `PRIORITY_MIN`, `PRIORITY_DEFAULT`,
        `PRIORITY_MAX`.
        */
        private static int loadGlobal(string which)()
        {
            static shared Priority cache;
            auto local = atomicLoad(mixin("cache." ~ which));
            if (local != local.min) return local;
            // There will be benign races
            cache = loadPriorities;
            return atomicLoad(mixin("cache." ~ which));
        }

        /*
        Loads all priorities and returns them as a `Priority` structure. This
        function is thread-neutral.
        */
        private static Priority loadPriorities() @nogc nothrow @trusted
        {
            Priority result;
            version (Solaris)
            {
                pcparms_t pcParms;
                pcinfo_t pcInfo;

                pcParms.pc_cid = PC_CLNULL;
                if (priocntl(idtype_t.P_PID, P_MYID, PC_GETPARMS, &pcParms) == -1)
                    assert( 0, "Unable to get scheduling class" );

                pcInfo.pc_cid = pcParms.pc_cid;
                // PC_GETCLINFO ignores the first two args, use dummy values
                if (priocntl(idtype_t.P_PID, 0, PC_GETCLINFO, &pcInfo) == -1)
                    assert( 0, "Unable to get scheduling class info" );

                pri_t* clparms = cast(pri_t*)&pcParms.pc_clparms;
                pri_t* clinfo = cast(pri_t*)&pcInfo.pc_clinfo;

                result.PRIORITY_MAX = clparms[0];

                if (pcInfo.pc_clname == "RT")
                {
                    m_isRTClass = true;

                    // For RT class, just assume it can't be changed
                    result.PRIORITY_MIN = clparms[0];
                    result.PRIORITY_DEFAULT = clparms[0];
                }
                else
                {
                    m_isRTClass = false;

                    // For all other scheduling classes, there are
                    // two key values -- uprilim and maxupri.
                    // maxupri is the maximum possible priority defined
                    // for the scheduling class, and valid priorities
                    // range are in [-maxupri, maxupri].
                    //
                    // However, uprilim is an upper limit that the
                    // current thread can set for the current scheduling
                    // class, which can be less than maxupri.  As such,
                    // use this value for priorityMax since this is
                    // the effective maximum.

                    // maxupri
                    result.PRIORITY_MIN = -clinfo[0];
                    // by definition
                    result.PRIORITY_DEFAULT = 0;
                }
            }
            else version (Posix)
            {
                int         policy;
                sched_param param;
                pthread_getschedparam( pthread_self(), &policy, &param ) == 0
                    || assert(0, "Internal error in pthread_getschedparam");

                result.PRIORITY_MIN = sched_get_priority_min( policy );
                result.PRIORITY_MIN != -1
                    || assert(0, "Internal error in sched_get_priority_min");
                result.PRIORITY_DEFAULT = param.sched_priority;
                result.PRIORITY_MAX = sched_get_priority_max( policy );
                result.PRIORITY_MAX != -1 ||
                    assert(0, "Internal error in sched_get_priority_max");
            }
            else
            {
                static assert(0, "Your code here.");
            }
            return result;
        }

        /**
         * The minimum scheduling priority that may be set for a thread.  On
         * systems where multiple scheduling policies are defined, this value
         * represents the minimum valid priority for the scheduling policy of
         * the process.
         */
        @property static int PRIORITY_MIN() @nogc nothrow pure @trusted
        {
            return (cast(int function() @nogc nothrow pure @safe)
                &loadGlobal!"PRIORITY_MIN")();
        }

        /**
         * The maximum scheduling priority that may be set for a thread.  On
         * systems where multiple scheduling policies are defined, this value
         * represents the maximum valid priority for the scheduling policy of
         * the process.
         */
        @property static const(int) PRIORITY_MAX() @nogc nothrow pure @trusted
        {
            return (cast(int function() @nogc nothrow pure @safe)
                &loadGlobal!"PRIORITY_MAX")();
        }

        /**
         * The default scheduling priority that is set for a thread.  On
         * systems where multiple scheduling policies are defined, this value
         * represents the default priority for the scheduling policy of
         * the process.
         */
        @property static int PRIORITY_DEFAULT() @nogc nothrow pure @trusted
        {
            return (cast(int function() @nogc nothrow pure @safe)
                &loadGlobal!"PRIORITY_DEFAULT")();
        }
    }

    version (NetBSD)
    {
        //NetBSD does not support priority for default policy
        // and it is not possible change policy without root access
        int fakePriority = int.max;
    }

    /**
     * Gets the scheduling priority for the associated thread.
     *
     * Note: Getting the priority of a thread that already terminated
     * might return the default priority.
     *
     * Returns:
     *  The scheduling priority of this thread.
     */
    final @property int priority()
    {
        version (Windows)
        {
            return GetThreadPriority( m_hndl );
        }
        else version (NetBSD)
        {
           return fakePriority==int.max? PRIORITY_DEFAULT : fakePriority;
        }
        else version (Posix)
        {
            int         policy;
            sched_param param;

            if (auto err = pthread_getschedparam(m_addr, &policy, &param))
            {
                // ignore error if thread is not running => Bugzilla 8960
                if (!atomicLoad(m_isRunning)) return PRIORITY_DEFAULT;
                throw new ThreadException("Unable to get thread priority");
            }
            return param.sched_priority;
        }
    }


    /**
     * Sets the scheduling priority for the associated thread.
     *
     * Note: Setting the priority of a thread that already terminated
     * might have no effect.
     *
     * Params:
     *  val = The new scheduling priority of this thread.
     */
    final @property void priority( int val )
    in
    {
        assert(val >= PRIORITY_MIN);
        assert(val <= PRIORITY_MAX);
    }
    body
    {
        version (Windows)
        {
            if ( !SetThreadPriority( m_hndl, val ) )
                throw new ThreadException( "Unable to set thread priority" );
        }
        else version (Solaris)
        {
            // the pthread_setschedprio(3c) and pthread_setschedparam functions
            // are broken for the default (TS / time sharing) scheduling class.
            // instead, we use priocntl(2) which gives us the desired behavior.

            // We hardcode the min and max priorities to the current value
            // so this is a no-op for RT threads.
            if (m_isRTClass)
                return;

            pcparms_t   pcparm;

            pcparm.pc_cid = PC_CLNULL;
            if (priocntl(idtype_t.P_LWPID, P_MYID, PC_GETPARMS, &pcparm) == -1)
                throw new ThreadException( "Unable to get scheduling class" );

            pri_t* clparms = cast(pri_t*)&pcparm.pc_clparms;

            // clparms is filled in by the PC_GETPARMS call, only necessary
            // to adjust the element that contains the thread priority
            clparms[1] = cast(pri_t) val;

            if (priocntl(idtype_t.P_LWPID, P_MYID, PC_SETPARMS, &pcparm) == -1)
                throw new ThreadException( "Unable to set scheduling class" );
        }
        else version (NetBSD)
        {
           fakePriority = val;
        }
        else version (Posix)
        {
            static if (__traits(compiles, pthread_setschedprio))
            {
                if (auto err = pthread_setschedprio(m_addr, val))
                {
                    // ignore error if thread is not running => Bugzilla 8960
                    if (!atomicLoad(m_isRunning)) return;
                    throw new ThreadException("Unable to set thread priority");
                }
            }
            else
            {
                // NOTE: pthread_setschedprio is not implemented on Darwin, FreeBSD, OpenBSD,
                //       or DragonFlyBSD, so use the more complicated get/set sequence below.
                int         policy;
                sched_param param;

                if (auto err = pthread_getschedparam(m_addr, &policy, &param))
                {
                    // ignore error if thread is not running => Bugzilla 8960
                    if (!atomicLoad(m_isRunning)) return;
                    throw new ThreadException("Unable to set thread priority");
                }
                param.sched_priority = val;
                if (auto err = pthread_setschedparam(m_addr, policy, &param))
                {
                    // ignore error if thread is not running => Bugzilla 8960
                    if (!atomicLoad(m_isRunning)) return;
                    throw new ThreadException("Unable to set thread priority");
                }
            }
        }
    }


    unittest
    {
        auto thr = Thread.getThis();
        immutable prio = thr.priority;
        scope (exit) thr.priority = prio;

        assert(prio == PRIORITY_DEFAULT);
        assert(prio >= PRIORITY_MIN && prio <= PRIORITY_MAX);
        thr.priority = PRIORITY_MIN;
        assert(thr.priority == PRIORITY_MIN);
        thr.priority = PRIORITY_MAX;
        assert(thr.priority == PRIORITY_MAX);
    }

    unittest // Bugzilla 8960
    {
        import core.sync.semaphore;

        auto thr = new Thread({});
        thr.start();
        Thread.sleep(1.msecs);       // wait a little so the thread likely has finished
        thr.priority = PRIORITY_MAX; // setting priority doesn't cause error
        auto prio = thr.priority;    // getting priority doesn't cause error
        assert(prio >= PRIORITY_MIN && prio <= PRIORITY_MAX);
    }

    ///////////////////////////////////////////////////////////////////////////
    // Actions on Calling Thread
    ///////////////////////////////////////////////////////////////////////////


    /**
     * Suspends the calling thread for at least the supplied period.  This may
     * result in multiple OS calls if period is greater than the maximum sleep
     * duration supported by the operating system.
     *
     * Params:
     *  val = The minimum duration the calling thread should be suspended.
     *
     * In:
     *  period must be non-negative.
     *
     * Example:
     * ------------------------------------------------------------------------
     *
     * Thread.sleep( dur!("msecs")( 50 ) );  // sleep for 50 milliseconds
     * Thread.sleep( dur!("seconds")( 5 ) ); // sleep for 5 seconds
     *
     * ------------------------------------------------------------------------
     */
    static void sleep( Duration val ) @nogc nothrow
    in
    {
        assert( !val.isNegative );
    }
    body
    {
        version (Windows)
        {
            auto maxSleepMillis = dur!("msecs")( uint.max - 1 );

            // avoid a non-zero time to be round down to 0
            if ( val > dur!"msecs"( 0 ) && val < dur!"msecs"( 1 ) )
                val = dur!"msecs"( 1 );

            // NOTE: In instances where all other threads in the process have a
            //       lower priority than the current thread, the current thread
            //       will not yield with a sleep time of zero.  However, unlike
            //       yield(), the user is not asking for a yield to occur but
            //       only for execution to suspend for the requested interval.
            //       Therefore, expected performance may not be met if a yield
            //       is forced upon the user.
            while ( val > maxSleepMillis )
            {
                Sleep( cast(uint)
                       maxSleepMillis.total!"msecs" );
                val -= maxSleepMillis;
            }
            Sleep( cast(uint) val.total!"msecs" );
        }
        else version (Posix)
        {
            timespec tin  = void;
            timespec tout = void;

            val.split!("seconds", "nsecs")(tin.tv_sec, tin.tv_nsec);
            if ( val.total!"seconds" > tin.tv_sec.max )
                tin.tv_sec  = tin.tv_sec.max;
            while ( true )
            {
                if ( !nanosleep( &tin, &tout ) )
                    return;
                if ( errno != EINTR )
                    assert(0, "Unable to sleep for the specified duration");
                tin = tout;
            }
        }
    }


    /**
     * Forces a context switch to occur away from the calling thread.
     */
    static void yield() @nogc nothrow
    {
        version (Windows)
            SwitchToThread();
        else version (Posix)
            sched_yield();
    }


    ///////////////////////////////////////////////////////////////////////////
    // Thread Accessors
    ///////////////////////////////////////////////////////////////////////////

    /**
     * Provides a reference to the calling thread.
     *
     * Returns:
     *  The thread object representing the calling thread.  The result of
     *  deleting this object is undefined.  If the current thread is not
     *  attached to the runtime, a null reference is returned.
     */
    static Thread getThis() @safe nothrow @nogc
    {
        // NOTE: This function may not be called until thread_init has
        //       completed.  See thread_suspendAll for more information
        //       on why this might occur.
        return sm_this;
    }


    /**
     * Provides a list of all threads currently being tracked by the system.
     * Note that threads in the returned array might no longer run (see
     * $(D Thread.)$(LREF isRunning)).
     *
     * Returns:
     *  An array containing references to all threads currently being
     *  tracked by the system.  The result of deleting any contained
     *  objects is undefined.
     */
    static Thread[] getAll()
    {
        static void resize(ref Thread[] buf, size_t nlen)
        {
            buf.length = nlen;
        }
        return getAllImpl!resize();
    }


    /**
     * Operates on all threads currently being tracked by the system.  The
     * result of deleting any Thread object is undefined.
     * Note that threads passed to the callback might no longer run (see
     * $(D Thread.)$(LREF isRunning)).
     *
     * Params:
     *  dg = The supplied code as a delegate.
     *
     * Returns:
     *  Zero if all elemented are visited, nonzero if not.
     */
    static int opApply(scope int delegate(ref Thread) dg)
    {
        import core.stdc.stdlib : free, realloc;

        static void resize(ref Thread[] buf, size_t nlen)
        {
            buf = (cast(Thread*)realloc(buf.ptr, nlen * Thread.sizeof))[0 .. nlen];
        }
        auto buf = getAllImpl!resize;
        scope(exit) if (buf.ptr) free(buf.ptr);

        foreach (t; buf)
        {
            if (auto res = dg(t))
                return res;
        }
        return 0;
    }

    unittest
    {
        auto t1 = new Thread({
            foreach (_; 0 .. 20)
                Thread.getAll;
        }).start;
        auto t2 = new Thread({
            foreach (_; 0 .. 20)
                GC.collect;
        }).start;
        t1.join();
        t2.join();
    }

    private static Thread[] getAllImpl(alias resize)()
    {
        import core.atomic;

        Thread[] buf;
        while (true)
        {
            immutable len = atomicLoad!(MemoryOrder.raw)(*cast(shared)&sm_tlen);
            resize(buf, len);
            assert(buf.length == len);
            synchronized (slock)
            {
                if (len == sm_tlen)
                {
                    size_t pos;
                    for (Thread t = sm_tbeg; t; t = t.next)
                        buf[pos++] = t;
                    return buf;
                }
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // Stuff That Should Go Away
    ///////////////////////////////////////////////////////////////////////////


private:
    //
    // Initializes a thread object which has no associated executable function.
    // This is used for the main thread initialized in thread_init().
    //
    this(size_t sz = 0) @safe pure nothrow @nogc
    {
        if (sz)
        {
            version (Posix)
            {
                // stack size must be a multiple of PAGESIZE
                sz += PAGESIZE - 1;
                sz -= sz % PAGESIZE;
                // and at least PTHREAD_STACK_MIN
                if (PTHREAD_STACK_MIN > sz)
                    sz = PTHREAD_STACK_MIN;
            }
            m_sz = sz;
        }
        m_call = Call.NO;
        m_curr = &m_main;
    }


    //
    // Thread entry point.  Invokes the function or delegate passed on
    // construction (if any).
    //
    final void run()
    {
        switch ( m_call )
        {
        case Call.FN:
            m_fn();
            break;
        case Call.DG:
            m_dg();
            break;
        default:
            break;
        }
    }


private:
    //
    // The type of routine passed on thread construction.
    //
    enum Call
    {
        NO,
        FN,
        DG
    }


    //
    // Standard types
    //
    version (Windows)
    {
        alias TLSKey = uint;
    }
    else version (Posix)
    {
        alias TLSKey = pthread_key_t;
    }


    //
    // Local storage
    //
    static Thread       sm_this;


    //
    // Main process thread
    //
    __gshared Thread    sm_main;

    version (FreeBSD)
    {
        // set when suspend failed and should be retried, see Issue 13416
        shared bool m_suspendagain;
    }


    //
    // Standard thread data
    //
    version (Windows)
    {
        HANDLE          m_hndl;
    }
    else version (Darwin)
    {
        mach_port_t     m_tmach;
    }
    ThreadID            m_addr;
    Call                m_call;
    string              m_name;
    union
    {
        void function() m_fn;
        void delegate() m_dg;
    }
    size_t              m_sz;
    version (Posix)
    {
        shared bool     m_isRunning;
    }
    bool                m_isDaemon;
    bool                m_isInCriticalRegion;
    Throwable           m_unhandled;

    version (Solaris)
    {
        __gshared bool m_isRTClass;
    }

private:
    ///////////////////////////////////////////////////////////////////////////
    // Storage of Active Thread
    ///////////////////////////////////////////////////////////////////////////


    //
    // Sets a thread-local reference to the current thread object.
    //
    static void setThis( Thread t ) nothrow @nogc
    {
        sm_this = t;
    }


private:
    ///////////////////////////////////////////////////////////////////////////
    // Thread Context and GC Scanning Support
    ///////////////////////////////////////////////////////////////////////////


    final void pushContext( Context* c ) nothrow @nogc
    in
    {
        assert( !c.within );
    }
    body
    {
        m_curr.ehContext = swapContext(c.ehContext);
        c.within = m_curr;
        m_curr = c;
    }


    final void popContext() nothrow @nogc
    in
    {
        assert( m_curr && m_curr.within );
    }
    body
    {
        Context* c = m_curr;
        m_curr = c.within;
        c.ehContext = swapContext(m_curr.ehContext);
        c.within = null;
    }


    final Context* topContext() nothrow @nogc
    in
    {
        assert( m_curr );
    }
    body
    {
        return m_curr;
    }


    static struct Context
    {
        void*           bstack,
                        tstack;

        /// Slot for the EH implementation to keep some state for each stack
        /// (will be necessary for exception chaining, etc.). Opaque as far as
        /// we are concerned here.
        void*           ehContext;

        Context*        within;
        Context*        next,
                        prev;
    }


    Context             m_main;
    Context*            m_curr;
    bool                m_lock;
    void*               m_tlsgcdata;

    version (Windows)
    {
      version (X86)
      {
        uint[8]         m_reg; // edi,esi,ebp,esp,ebx,edx,ecx,eax
      }
      else version (X86_64)
      {
        ulong[16]       m_reg; // rdi,rsi,rbp,rsp,rbx,rdx,rcx,rax
                               // r8,r9,r10,r11,r12,r13,r14,r15
      }
      else
      {
        static assert(false, "Architecture not supported." );
      }
    }
    else version (Darwin)
    {
      version (X86)
      {
        uint[8]         m_reg; // edi,esi,ebp,esp,ebx,edx,ecx,eax
      }
      else version (X86_64)
      {
        ulong[16]       m_reg; // rdi,rsi,rbp,rsp,rbx,rdx,rcx,rax
                               // r8,r9,r10,r11,r12,r13,r14,r15
      }
      else
      {
        static assert(false, "Architecture not supported." );
      }
    }


private:
    ///////////////////////////////////////////////////////////////////////////
    // GC Scanning Support
    ///////////////////////////////////////////////////////////////////////////


    // NOTE: The GC scanning process works like so:
    //
    //          1. Suspend all threads.
    //          2. Scan the stacks of all suspended threads for roots.
    //          3. Resume all threads.
    //
    //       Step 1 and 3 require a list of all threads in the system, while
    //       step 2 requires a list of all thread stacks (each represented by
    //       a Context struct).  Traditionally, there was one stack per thread
    //       and the Context structs were not necessary.  However, Fibers have
    //       changed things so that each thread has its own 'main' stack plus
    //       an arbitrary number of nested stacks (normally referenced via
    //       m_curr).  Also, there may be 'free-floating' stacks in the system,
    //       which are Fibers that are not currently executing on any specific
    //       thread but are still being processed and still contain valid
    //       roots.
    //
    //       To support all of this, the Context struct has been created to
    //       represent a stack range, and a global list of Context structs has
    //       been added to enable scanning of these stack ranges.  The lifetime
    //       (and presence in the Context list) of a thread's 'main' stack will
    //       be equivalent to the thread's lifetime.  So the Ccontext will be
    //       added to the list on thread entry, and removed from the list on
    //       thread exit (which is essentially the same as the presence of a
    //       Thread object in its own global list).  The lifetime of a Fiber's
    //       context, however, will be tied to the lifetime of the Fiber object
    //       itself, and Fibers are expected to add/remove their Context struct
    //       on construction/deletion.


    //
    // All use of the global thread lists/array should synchronize on this lock.
    //
    // Careful as the GC acquires this lock after the GC lock to suspend all
    // threads any GC usage with slock held can result in a deadlock through
    // lock order inversion.
    @property static Mutex slock() nothrow @nogc
    {
        return cast(Mutex)_slock.ptr;
    }

    @property static Mutex criticalRegionLock() nothrow @nogc
    {
        return cast(Mutex)_criticalRegionLock.ptr;
    }

    __gshared align(mutexAlign) void[mutexClassInstanceSize] _slock;
    __gshared align(mutexAlign) void[mutexClassInstanceSize] _criticalRegionLock;

    static void initLocks()
    {
        _slock[] = typeid(Mutex).initializer[];
        (cast(Mutex)_slock.ptr).__ctor();

        _criticalRegionLock[] = typeid(Mutex).initializer[];
        (cast(Mutex)_criticalRegionLock.ptr).__ctor();
    }

    static void termLocks()
    {
        (cast(Mutex)_slock.ptr).__dtor();
        (cast(Mutex)_criticalRegionLock.ptr).__dtor();
    }

    __gshared Context*  sm_cbeg;

    __gshared Thread    sm_tbeg;
    __gshared size_t    sm_tlen;

    // can't use rt.util.array in public code
    __gshared Thread* pAboutToStart;
    __gshared size_t nAboutToStart;

    //
    // Used for ordering threads in the global thread list.
    //
    Thread              prev;
    Thread              next;


    ///////////////////////////////////////////////////////////////////////////
    // Global Context List Operations
    ///////////////////////////////////////////////////////////////////////////


    //
    // Add a context to the global context list.
    //
    static void add( Context* c ) nothrow @nogc
    in
    {
        assert( c );
        assert( !c.next && !c.prev );
    }
    body
    {
        slock.lock_nothrow();
        scope(exit) slock.unlock_nothrow();
        assert(!suspendDepth); // must be 0 b/c it's only set with slock held

        if (sm_cbeg)
        {
            c.next = sm_cbeg;
            sm_cbeg.prev = c;
        }
        sm_cbeg = c;
    }


    //
    // Remove a context from the global context list.
    //
    // This assumes slock being acquired. This isn't done here to
    // avoid double locking when called from remove(Thread)
    static void remove( Context* c ) nothrow @nogc
    in
    {
        assert( c );
        assert( c.next || c.prev );
    }
    body
    {
        if ( c.prev )
            c.prev.next = c.next;
        if ( c.next )
            c.next.prev = c.prev;
        if ( sm_cbeg == c )
            sm_cbeg = c.next;
        // NOTE: Don't null out c.next or c.prev because opApply currently
        //       follows c.next after removing a node.  This could be easily
        //       addressed by simply returning the next node from this
        //       function, however, a context should never be re-added to the
        //       list anyway and having next and prev be non-null is a good way
        //       to ensure that.
    }


    ///////////////////////////////////////////////////////////////////////////
    // Global Thread List Operations
    ///////////////////////////////////////////////////////////////////////////


    //
    // Add a thread to the global thread list.
    //
    static void add( Thread t, bool rmAboutToStart = true ) nothrow @nogc
    in
    {
        assert( t );
        assert( !t.next && !t.prev );
    }
    body
    {
        slock.lock_nothrow();
        scope(exit) slock.unlock_nothrow();
        assert(t.isRunning); // check this with slock to ensure pthread_create already returned
        assert(!suspendDepth); // must be 0 b/c it's only set with slock held

        if (rmAboutToStart)
        {
            size_t idx = -1;
            foreach (i, thr; pAboutToStart[0 .. nAboutToStart])
            {
                if (thr is t)
                {
                    idx = i;
                    break;
                }
            }
            assert(idx != -1);
            import core.stdc.string : memmove;
            memmove(pAboutToStart + idx, pAboutToStart + idx + 1, Thread.sizeof * (nAboutToStart - idx - 1));
            pAboutToStart =
                cast(Thread*)realloc(pAboutToStart, Thread.sizeof * --nAboutToStart);
        }

        if (sm_tbeg)
        {
            t.next = sm_tbeg;
            sm_tbeg.prev = t;
        }
        sm_tbeg = t;
        ++sm_tlen;
    }


    //
    // Remove a thread from the global thread list.
    //
    static void remove( Thread t ) nothrow @nogc
    in
    {
        assert( t );
    }
    body
    {
        // Thread was already removed earlier, might happen b/c of thread_detachInstance
        if (!t.next && !t.prev)
            return;
        slock.lock_nothrow();
        {
            // NOTE: When a thread is removed from the global thread list its
            //       main context is invalid and should be removed as well.
            //       It is possible that t.m_curr could reference more
            //       than just the main context if the thread exited abnormally
            //       (if it was terminated), but we must assume that the user
            //       retains a reference to them and that they may be re-used
            //       elsewhere.  Therefore, it is the responsibility of any
            //       object that creates contexts to clean them up properly
            //       when it is done with them.
            remove( &t.m_main );

            if ( t.prev )
                t.prev.next = t.next;
            if ( t.next )
                t.next.prev = t.prev;
            if ( sm_tbeg is t )
                sm_tbeg = t.next;
            t.prev = t.next = null;
            --sm_tlen;
        }
        // NOTE: Don't null out t.next or t.prev because opApply currently
        //       follows t.next after removing a node.  This could be easily
        //       addressed by simply returning the next node from this
        //       function, however, a thread should never be re-added to the
        //       list anyway and having next and prev be non-null is a good way
        //       to ensure that.
        slock.unlock_nothrow();
    }
}

///
unittest
{
    class DerivedThread : Thread
    {
        this()
        {
            super(&run);
        }

    private:
        void run()
        {
            // Derived thread running.
        }
    }

    void threadFunc()
    {
        // Composed thread running.
    }

    // create and start instances of each type
    auto derived = new DerivedThread().start();
    auto composed = new Thread(&threadFunc).start();
    new Thread({
        // Codes to run in the newly created thread.
    }).start();
}

unittest
{
    int x = 0;

    new Thread(
    {
        x++;
    }).start().join();
    assert( x == 1 );
}


unittest
{
    enum MSG = "Test message.";
    string caughtMsg;

    try
    {
        new Thread(
        {
            throw new Exception( MSG );
        }).start().join();
        assert( false, "Expected rethrown exception." );
    }
    catch ( Throwable t )
    {
        assert( t.msg == MSG );
    }
}


///////////////////////////////////////////////////////////////////////////////
// GC Support Routines
///////////////////////////////////////////////////////////////////////////////

version (CoreDdoc)
{
    /**
     * Instruct the thread module, when initialized, to use a different set of
     * signals besides SIGUSR1 and SIGUSR2 for suspension and resumption of threads.
     * This function should be called at most once, prior to thread_init().
     * This function is Posix-only.
     */
    extern (C) void thread_setGCSignals(int suspendSignalNo, int resumeSignalNo) nothrow @nogc
    {
    }
}
else version (Posix)
{
    extern (C) void thread_setGCSignals(int suspendSignalNo, int resumeSignalNo) nothrow @nogc
    in
    {
        assert(suspendSignalNumber == 0);
        assert(resumeSignalNumber  == 0);
        assert(suspendSignalNo != 0);
        assert(resumeSignalNo  != 0);
    }
    out
    {
        assert(suspendSignalNumber != 0);
        assert(resumeSignalNumber  != 0);
    }
    body
    {
        suspendSignalNumber = suspendSignalNo;
        resumeSignalNumber  = resumeSignalNo;
    }
}

version (Posix)
{
    __gshared int suspendSignalNumber;
    __gshared int resumeSignalNumber;
}

/**
 * Initializes the thread module.  This function must be called by the
 * garbage collector on startup and before any other thread routines
 * are called.
 */
extern (C) void thread_init()
{
    // NOTE: If thread_init itself performs any allocations then the thread
    //       routines reserved for garbage collector use may be called while
    //       thread_init is being processed.  However, since no memory should
    //       exist to be scanned at this point, it is sufficient for these
    //       functions to detect the condition and return immediately.

    Thread.initLocks();
    // The Android VM runtime intercepts SIGUSR1 and apparently doesn't allow
    // its signal handler to run, so swap the two signals on Android, since
    // thread_resumeHandler does nothing.
    version (Android) thread_setGCSignals(SIGUSR2, SIGUSR1);

    version (Darwin)
    {
    }
    else version (Posix)
    {
        if ( suspendSignalNumber == 0 )
        {
            suspendSignalNumber = SIGUSR1;
        }

        if ( resumeSignalNumber == 0 )
        {
            resumeSignalNumber = SIGUSR2;
        }

        int         status;
        sigaction_t sigusr1 = void;
        sigaction_t sigusr2 = void;

        // This is a quick way to zero-initialize the structs without using
        // memset or creating a link dependency on their static initializer.
        (cast(byte*) &sigusr1)[0 .. sigaction_t.sizeof] = 0;
        (cast(byte*) &sigusr2)[0 .. sigaction_t.sizeof] = 0;

        // NOTE: SA_RESTART indicates that system calls should restart if they
        //       are interrupted by a signal, but this is not available on all
        //       Posix systems, even those that support multithreading.
        static if ( __traits( compiles, SA_RESTART ) )
            sigusr1.sa_flags = SA_RESTART;
        else
            sigusr1.sa_flags   = 0;
        sigusr1.sa_handler = &thread_suspendHandler;
        // NOTE: We want to ignore all signals while in this handler, so fill
        //       sa_mask to indicate this.
        status = sigfillset( &sigusr1.sa_mask );
        assert( status == 0 );

        // NOTE: Since resumeSignalNumber should only be issued for threads within the
        //       suspend handler, we don't want this signal to trigger a
        //       restart.
        sigusr2.sa_flags   = 0;
        sigusr2.sa_handler = &thread_resumeHandler;
        // NOTE: We want to ignore all signals while in this handler, so fill
        //       sa_mask to indicate this.
        status = sigfillset( &sigusr2.sa_mask );
        assert( status == 0 );

        status = sigaction( suspendSignalNumber, &sigusr1, null );
        assert( status == 0 );

        status = sigaction( resumeSignalNumber, &sigusr2, null );
        assert( status == 0 );

        status = sem_init( &suspendCount, 0, 0 );
        assert( status == 0 );
    }
    Thread.sm_main = thread_attachThis();
}


/**
 * Terminates the thread module. No other thread routine may be called
 * afterwards.
 */
extern (C) void thread_term()
{
    assert(Thread.sm_tbeg && Thread.sm_tlen == 1);
    assert(!Thread.nAboutToStart);
    if (Thread.pAboutToStart) // in case realloc(p, 0) doesn't return null
    {
        free(Thread.pAboutToStart);
        Thread.pAboutToStart = null;
    }
    Thread.termLocks();
}


/**
 *
 */
extern (C) bool thread_isMainThread() nothrow @nogc
{
    return Thread.getThis() is Thread.sm_main;
}


/**
 * Registers the calling thread for use with the D Runtime.  If this routine
 * is called for a thread which is already registered, no action is performed.
 *
 * NOTE: This routine does not run thread-local static constructors when called.
 *       If full functionality as a D thread is desired, the following function
 *       must be called after thread_attachThis:
 *
 *       extern (C) void rt_moduleTlsCtor();
 */
extern (C) Thread thread_attachThis()
{
    GC.disable(); scope(exit) GC.enable();

    if (auto t = Thread.getThis())
        return t;

    Thread          thisThread  = new Thread();
    Thread.Context* thisContext = &thisThread.m_main;
    assert( thisContext == thisThread.m_curr );

    version (Windows)
    {
        thisThread.m_addr  = GetCurrentThreadId();
        thisThread.m_hndl  = GetCurrentThreadHandle();
        thisContext.bstack = getStackBottom();
        thisContext.tstack = thisContext.bstack;
    }
    else version (Posix)
    {
        thisThread.m_addr  = pthread_self();
        thisContext.bstack = getStackBottom();
        thisContext.tstack = thisContext.bstack;

        atomicStore!(MemoryOrder.raw)(thisThread.m_isRunning, true);
    }
    thisThread.m_isDaemon = true;
    thisThread.m_tlsgcdata = rt_tlsgc_init();
    Thread.setThis( thisThread );

    version (Darwin)
    {
        thisThread.m_tmach = pthread_mach_thread_np( thisThread.m_addr );
        assert( thisThread.m_tmach != thisThread.m_tmach.init );
    }

    Thread.add( thisThread, false );
    Thread.add( thisContext );
    if ( Thread.sm_main !is null )
        multiThreadedFlag = true;
    return thisThread;
}


version (Windows)
{
    // NOTE: These calls are not safe on Posix systems that use signals to
    //       perform garbage collection.  The suspendHandler uses getThis()
    //       to get the thread handle so getThis() must be a simple call.
    //       Mutexes can't safely be acquired inside signal handlers, and
    //       even if they could, the mutex needed (Thread.slock) is held by
    //       thread_suspendAll().  So in short, these routines will remain
    //       Windows-specific.  If they are truly needed elsewhere, the
    //       suspendHandler will need a way to call a version of getThis()
    //       that only does the TLS lookup without the fancy fallback stuff.

    /// ditto
    extern (C) Thread thread_attachByAddr( ThreadID addr )
    {
        return thread_attachByAddrB( addr, getThreadStackBottom( addr ) );
    }


    /// ditto
    extern (C) Thread thread_attachByAddrB( ThreadID addr, void* bstack )
    {
        GC.disable(); scope(exit) GC.enable();

        if (auto t = thread_findByAddr(addr))
            return t;

        Thread          thisThread  = new Thread();
        Thread.Context* thisContext = &thisThread.m_main;
        assert( thisContext == thisThread.m_curr );

        thisThread.m_addr  = addr;
        thisContext.bstack = bstack;
        thisContext.tstack = thisContext.bstack;

        thisThread.m_isDaemon = true;

        if ( addr == GetCurrentThreadId() )
        {
            thisThread.m_hndl = GetCurrentThreadHandle();
            thisThread.m_tlsgcdata = rt_tlsgc_init();
            Thread.setThis( thisThread );
        }
        else
        {
            thisThread.m_hndl = OpenThreadHandle( addr );
            impersonate_thread(addr,
            {
                thisThread.m_tlsgcdata = rt_tlsgc_init();
                Thread.setThis( thisThread );
            });
        }

        Thread.add( thisThread, false );
        Thread.add( thisContext );
        if ( Thread.sm_main !is null )
            multiThreadedFlag = true;
        return thisThread;
    }
}


/**
 * Deregisters the calling thread from use with the runtime.  If this routine
 * is called for a thread which is not registered, the result is undefined.
 *
 * NOTE: This routine does not run thread-local static destructors when called.
 *       If full functionality as a D thread is desired, the following function
 *       must be called after thread_detachThis, particularly if the thread is
 *       being detached at some indeterminate time before program termination:
 *
 *       $(D extern(C) void rt_moduleTlsDtor();)
 */
extern (C) void thread_detachThis() nothrow @nogc
{
    if (auto t = Thread.getThis())
        Thread.remove(t);
}


/**
 * Deregisters the given thread from use with the runtime.  If this routine
 * is called for a thread which is not registered, the result is undefined.
 *
 * NOTE: This routine does not run thread-local static destructors when called.
 *       If full functionality as a D thread is desired, the following function
 *       must be called by the detached thread, particularly if the thread is
 *       being detached at some indeterminate time before program termination:
 *
 *       $(D extern(C) void rt_moduleTlsDtor();)
 */
extern (C) void thread_detachByAddr( ThreadID addr )
{
    if ( auto t = thread_findByAddr( addr ) )
        Thread.remove( t );
}


/// ditto
extern (C) void thread_detachInstance( Thread t ) nothrow @nogc
{
    Thread.remove( t );
}


unittest
{
    import core.sync.semaphore;
    auto sem = new Semaphore();

    auto t = new Thread(
    {
        sem.notify();
        Thread.sleep(100.msecs);
    }).start();

    sem.wait(); // thread cannot be detached while being started
    thread_detachInstance(t);
    foreach (t2; Thread)
        assert(t !is t2);
    t.join();
}


/**
 * Search the list of all threads for a thread with the given thread identifier.
 *
 * Params:
 *  addr = The thread identifier to search for.
 * Returns:
 *  The thread object associated with the thread identifier, null if not found.
 */
static Thread thread_findByAddr( ThreadID addr )
{
    Thread.slock.lock_nothrow();
    scope(exit) Thread.slock.unlock_nothrow();

    // also return just spawned thread so that
    // DLL_THREAD_ATTACH knows it's a D thread
    foreach (t; Thread.pAboutToStart[0 .. Thread.nAboutToStart])
        if (t.m_addr == addr)
            return t;

    foreach (t; Thread)
        if (t.m_addr == addr)
            return t;

    return null;
}


/**
 * Sets the current thread to a specific reference. Only to be used
 * when dealing with externally-created threads (in e.g. C code).
 * The primary use of this function is when Thread.getThis() must
 * return a sensible value in, for example, TLS destructors. In
 * other words, don't touch this unless you know what you're doing.
 *
 * Params:
 *  t = A reference to the current thread. May be null.
 */
extern (C) void thread_setThis(Thread t) nothrow @nogc
{
    Thread.setThis(t);
}


/**
 * Joins all non-daemon threads that are currently running.  This is done by
 * performing successive scans through the thread list until a scan consists
 * of only daemon threads.
 */
extern (C) void thread_joinAll()
{
 Lagain:
    Thread.slock.lock_nothrow();
    // wait for just spawned threads
    if (Thread.nAboutToStart)
    {
        Thread.slock.unlock_nothrow();
        Thread.yield();
        goto Lagain;
    }

    // join all non-daemon threads, the main thread is also a daemon
    auto t = Thread.sm_tbeg;
    while (t)
    {
        if (!t.isRunning)
        {
            auto tn = t.next;
            Thread.remove(t);
            t = tn;
        }
        else if (t.isDaemon)
        {
            t = t.next;
        }
        else
        {
            Thread.slock.unlock_nothrow();
            t.join(); // might rethrow
            goto Lagain; // must restart iteration b/c of unlock
        }
    }
    Thread.slock.unlock_nothrow();
}


/**
 * Performs intermediate shutdown of the thread module.
 */
shared static ~this()
{
    // NOTE: The functionality related to garbage collection must be minimally
    //       operable after this dtor completes.  Therefore, only minimal
    //       cleanup may occur.
    auto t = Thread.sm_tbeg;
    while (t)
    {
        auto tn = t.next;
        if (!t.isRunning)
            Thread.remove(t);
        t = tn;
    }
}


// Used for needLock below.
private __gshared bool multiThreadedFlag = false;

// Calls the given delegate, passing the current thread's stack pointer to it.
private void callWithStackShell(scope void delegate(void* sp) nothrow fn) nothrow
in
{
    assert(fn);
}
body
{
    // The purpose of the 'shell' is to ensure all the registers get
    // put on the stack so they'll be scanned. We only need to push
    // the callee-save registers.
    void *sp = void;

    version (GNU)
    {
        __builtin_unwind_init();
        sp = &sp;
    }
    else version (AsmX86_Posix)
    {
        size_t[3] regs = void;
        asm pure nothrow @nogc
        {
            mov [regs + 0 * 4], EBX;
            mov [regs + 1 * 4], ESI;
            mov [regs + 2 * 4], EDI;

            mov sp[EBP], ESP;
        }
    }
    else version (AsmX86_Windows)
    {
        size_t[3] regs = void;
        asm pure nothrow @nogc
        {
            mov [regs + 0 * 4], EBX;
            mov [regs + 1 * 4], ESI;
            mov [regs + 2 * 4], EDI;

            mov sp[EBP], ESP;
        }
    }
    else version (AsmX86_64_Posix)
    {
        size_t[5] regs = void;
        asm pure nothrow @nogc
        {
            mov [regs + 0 * 8], RBX;
            mov [regs + 1 * 8], R12;
            mov [regs + 2 * 8], R13;
            mov [regs + 3 * 8], R14;
            mov [regs + 4 * 8], R15;

            mov sp[RBP], RSP;
        }
    }
    else version (AsmX86_64_Windows)
    {
        size_t[7] regs = void;
        asm pure nothrow @nogc
        {
            mov [regs + 0 * 8], RBX;
            mov [regs + 1 * 8], RSI;
            mov [regs + 2 * 8], RDI;
            mov [regs + 3 * 8], R12;
            mov [regs + 4 * 8], R13;
            mov [regs + 5 * 8], R14;
            mov [regs + 6 * 8], R15;

            mov sp[RBP], RSP;
        }
    }
    else
    {
        static assert(false, "Architecture not supported.");
    }

    fn(sp);
}

// Used for suspendAll/resumeAll below.
private __gshared uint suspendDepth = 0;

/**
 * Suspend the specified thread and load stack and register information for
 * use by thread_scanAll.  If the supplied thread is the calling thread,
 * stack and register information will be loaded but the thread will not
 * be suspended.  If the suspend operation fails and the thread is not
 * running then it will be removed from the global thread list, otherwise
 * an exception will be thrown.
 *
 * Params:
 *  t = The thread to suspend.
 *
 * Throws:
 *  ThreadError if the suspend operation fails for a running thread.
 * Returns:
 *  Whether the thread is now suspended (true) or terminated (false).
 */
private bool suspend( Thread t ) nothrow
{
    Duration waittime = dur!"usecs"(10);
 Lagain:
    if (!t.isRunning)
    {
        Thread.remove(t);
        return false;
    }
    else if (t.m_isInCriticalRegion)
    {
        Thread.criticalRegionLock.unlock_nothrow();
        Thread.sleep(waittime);
        if (waittime < dur!"msecs"(10)) waittime *= 2;
        Thread.criticalRegionLock.lock_nothrow();
        goto Lagain;
    }

    version (Windows)
    {
        if ( t.m_addr != GetCurrentThreadId() && SuspendThread( t.m_hndl ) == 0xFFFFFFFF )
        {
            if ( !t.isRunning )
            {
                Thread.remove( t );
                return false;
            }
            onThreadError( "Unable to suspend thread" );
        }

        CONTEXT context = void;
        context.ContextFlags = CONTEXT_INTEGER | CONTEXT_CONTROL;

        if ( !GetThreadContext( t.m_hndl, &context ) )
            onThreadError( "Unable to load thread context" );
        version (X86)
        {
            if ( !t.m_lock )
                t.m_curr.tstack = cast(void*) context.Esp;
            // eax,ebx,ecx,edx,edi,esi,ebp,esp
            t.m_reg[0] = context.Eax;
            t.m_reg[1] = context.Ebx;
            t.m_reg[2] = context.Ecx;
            t.m_reg[3] = context.Edx;
            t.m_reg[4] = context.Edi;
            t.m_reg[5] = context.Esi;
            t.m_reg[6] = context.Ebp;
            t.m_reg[7] = context.Esp;
        }
        else version (X86_64)
        {
            if ( !t.m_lock )
                t.m_curr.tstack = cast(void*) context.Rsp;
            // rax,rbx,rcx,rdx,rdi,rsi,rbp,rsp
            t.m_reg[0] = context.Rax;
            t.m_reg[1] = context.Rbx;
            t.m_reg[2] = context.Rcx;
            t.m_reg[3] = context.Rdx;
            t.m_reg[4] = context.Rdi;
            t.m_reg[5] = context.Rsi;
            t.m_reg[6] = context.Rbp;
            t.m_reg[7] = context.Rsp;
            // r8,r9,r10,r11,r12,r13,r14,r15
            t.m_reg[8]  = context.R8;
            t.m_reg[9]  = context.R9;
            t.m_reg[10] = context.R10;
            t.m_reg[11] = context.R11;
            t.m_reg[12] = context.R12;
            t.m_reg[13] = context.R13;
            t.m_reg[14] = context.R14;
            t.m_reg[15] = context.R15;
        }
        else
        {
            static assert(false, "Architecture not supported." );
        }
    }
    else version (Darwin)
    {
        if ( t.m_addr != pthread_self() && thread_suspend( t.m_tmach ) != KERN_SUCCESS )
        {
            if ( !t.isRunning )
            {
                Thread.remove( t );
                return false;
            }
            onThreadError( "Unable to suspend thread" );
        }

        version (X86)
        {
            x86_thread_state32_t    state = void;
            mach_msg_type_number_t  count = x86_THREAD_STATE32_COUNT;

            if ( thread_get_state( t.m_tmach, x86_THREAD_STATE32, &state, &count ) != KERN_SUCCESS )
                onThreadError( "Unable to load thread state" );
            if ( !t.m_lock )
                t.m_curr.tstack = cast(void*) state.esp;
            // eax,ebx,ecx,edx,edi,esi,ebp,esp
            t.m_reg[0] = state.eax;
            t.m_reg[1] = state.ebx;
            t.m_reg[2] = state.ecx;
            t.m_reg[3] = state.edx;
            t.m_reg[4] = state.edi;
            t.m_reg[5] = state.esi;
            t.m_reg[6] = state.ebp;
            t.m_reg[7] = state.esp;
        }
        else version (X86_64)
        {
            x86_thread_state64_t    state = void;
            mach_msg_type_number_t  count = x86_THREAD_STATE64_COUNT;

            if ( thread_get_state( t.m_tmach, x86_THREAD_STATE64, &state, &count ) != KERN_SUCCESS )
                onThreadError( "Unable to load thread state" );
            if ( !t.m_lock )
                t.m_curr.tstack = cast(void*) state.rsp;
            // rax,rbx,rcx,rdx,rdi,rsi,rbp,rsp
            t.m_reg[0] = state.rax;
            t.m_reg[1] = state.rbx;
            t.m_reg[2] = state.rcx;
            t.m_reg[3] = state.rdx;
            t.m_reg[4] = state.rdi;
            t.m_reg[5] = state.rsi;
            t.m_reg[6] = state.rbp;
            t.m_reg[7] = state.rsp;
            // r8,r9,r10,r11,r12,r13,r14,r15
            t.m_reg[8]  = state.r8;
            t.m_reg[9]  = state.r9;
            t.m_reg[10] = state.r10;
            t.m_reg[11] = state.r11;
            t.m_reg[12] = state.r12;
            t.m_reg[13] = state.r13;
            t.m_reg[14] = state.r14;
            t.m_reg[15] = state.r15;
        }
        else
        {
            static assert(false, "Architecture not supported." );
        }
    }
    else version (Posix)
    {
        if ( t.m_addr != pthread_self() )
        {
            if ( pthread_kill( t.m_addr, suspendSignalNumber ) != 0 )
            {
                if ( !t.isRunning )
                {
                    Thread.remove( t );
                    return false;
                }
                onThreadError( "Unable to suspend thread" );
            }
        }
        else if ( !t.m_lock )
        {
            t.m_curr.tstack = getStackTop();
        }
    }
    return true;
}

/**
 * Suspend all threads but the calling thread for "stop the world" garbage
 * collection runs.  This function may be called multiple times, and must
 * be followed by a matching number of calls to thread_resumeAll before
 * processing is resumed.
 *
 * Throws:
 *  ThreadError if the suspend operation fails for a running thread.
 */
extern (C) void thread_suspendAll() nothrow
{
    // NOTE: We've got an odd chicken & egg problem here, because while the GC
    //       is required to call thread_init before calling any other thread
    //       routines, thread_init may allocate memory which could in turn
    //       trigger a collection.  Thus, thread_suspendAll, thread_scanAll,
    //       and thread_resumeAll must be callable before thread_init
    //       completes, with the assumption that no other GC memory has yet
    //       been allocated by the system, and thus there is no risk of losing
    //       data if the global thread list is empty.  The check of
    //       Thread.sm_tbeg below is done to ensure thread_init has completed,
    //       and therefore that calling Thread.getThis will not result in an
    //       error.  For the short time when Thread.sm_tbeg is null, there is
    //       no reason not to simply call the multithreaded code below, with
    //       the expectation that the foreach loop will never be entered.
    if ( !multiThreadedFlag && Thread.sm_tbeg )
    {
        if ( ++suspendDepth == 1 )
            suspend( Thread.getThis() );

        return;
    }

    Thread.slock.lock_nothrow();
    {
        if ( ++suspendDepth > 1 )
            return;

        Thread.criticalRegionLock.lock_nothrow();
        scope (exit) Thread.criticalRegionLock.unlock_nothrow();
        size_t cnt;
        auto t = Thread.sm_tbeg;
        while (t)
        {
            auto tn = t.next;
            if (suspend(t))
                ++cnt;
            t = tn;
        }

        version (Darwin)
        {}
        else version (Posix)
        {
            // subtract own thread
            assert(cnt >= 1);
            --cnt;
        Lagain:
            // wait for semaphore notifications
            for (; cnt; --cnt)
            {
                while (sem_wait(&suspendCount) != 0)
                {
                    if (errno != EINTR)
                        onThreadError("Unable to wait for semaphore");
                    errno = 0;
                }
            }
            version (FreeBSD)
            {
                // avoid deadlocks, see Issue 13416
                t = Thread.sm_tbeg;
                while (t)
                {
                    auto tn = t.next;
                    if (t.m_suspendagain && suspend(t))
                        ++cnt;
                    t = tn;
                }
                if (cnt)
                    goto Lagain;
             }
        }
    }
}

/**
 * Resume the specified thread and unload stack and register information.
 * If the supplied thread is the calling thread, stack and register
 * information will be unloaded but the thread will not be resumed.  If
 * the resume operation fails and the thread is not running then it will
 * be removed from the global thread list, otherwise an exception will be
 * thrown.
 *
 * Params:
 *  t = The thread to resume.
 *
 * Throws:
 *  ThreadError if the resume fails for a running thread.
 */
private void resume( Thread t ) nothrow
{
    version (Windows)
    {
        if ( t.m_addr != GetCurrentThreadId() && ResumeThread( t.m_hndl ) == 0xFFFFFFFF )
        {
            if ( !t.isRunning )
            {
                Thread.remove( t );
                return;
            }
            onThreadError( "Unable to resume thread" );
        }

        if ( !t.m_lock )
            t.m_curr.tstack = t.m_curr.bstack;
        t.m_reg[0 .. $] = 0;
    }
    else version (Darwin)
    {
        if ( t.m_addr != pthread_self() && thread_resume( t.m_tmach ) != KERN_SUCCESS )
        {
            if ( !t.isRunning )
            {
                Thread.remove( t );
                return;
            }
            onThreadError( "Unable to resume thread" );
        }

        if ( !t.m_lock )
            t.m_curr.tstack = t.m_curr.bstack;
        t.m_reg[0 .. $] = 0;
    }
    else version (Posix)
    {
        if ( t.m_addr != pthread_self() )
        {
            if ( pthread_kill( t.m_addr, resumeSignalNumber ) != 0 )
            {
                if ( !t.isRunning )
                {
                    Thread.remove( t );
                    return;
                }
                onThreadError( "Unable to resume thread" );
            }
        }
        else if ( !t.m_lock )
        {
            t.m_curr.tstack = t.m_curr.bstack;
        }
    }
}

/**
 * Resume all threads but the calling thread for "stop the world" garbage
 * collection runs.  This function must be called once for each preceding
 * call to thread_suspendAll before the threads are actually resumed.
 *
 * In:
 *  This routine must be preceded by a call to thread_suspendAll.
 *
 * Throws:
 *  ThreadError if the resume operation fails for a running thread.
 */
extern (C) void thread_resumeAll() nothrow
in
{
    assert( suspendDepth > 0 );
}
body
{
    // NOTE: See thread_suspendAll for the logic behind this.
    if ( !multiThreadedFlag && Thread.sm_tbeg )
    {
        if ( --suspendDepth == 0 )
            resume( Thread.getThis() );
        return;
    }

    scope(exit) Thread.slock.unlock_nothrow();
    {
        if ( --suspendDepth > 0 )
            return;

        for ( Thread t = Thread.sm_tbeg; t; t = t.next )
        {
            // NOTE: We do not need to care about critical regions at all
            //       here. thread_suspendAll takes care of everything.
            resume( t );
        }
    }
}

/**
 * Indicates the kind of scan being performed by $(D thread_scanAllType).
 */
enum ScanType
{
    stack, /// The stack and/or registers are being scanned.
    tls, /// TLS data is being scanned.
}

alias ScanAllThreadsFn = void delegate(void*, void*) nothrow; /// The scanning function.
alias ScanAllThreadsTypeFn = void delegate(ScanType, void*, void*) nothrow; /// ditto

/**
 * The main entry point for garbage collection.  The supplied delegate
 * will be passed ranges representing both stack and register values.
 *
 * Params:
 *  scan        = The scanner function.  It should scan from p1 through p2 - 1.
 *
 * In:
 *  This routine must be preceded by a call to thread_suspendAll.
 */
extern (C) void thread_scanAllType( scope ScanAllThreadsTypeFn scan ) nothrow
in
{
    assert( suspendDepth > 0 );
}
body
{
    callWithStackShell(sp => scanAllTypeImpl(scan, sp));
}


private void scanAllTypeImpl( scope ScanAllThreadsTypeFn scan, void* curStackTop ) nothrow
{
    Thread  thisThread  = null;
    void*   oldStackTop = null;

    if ( Thread.sm_tbeg )
    {
        thisThread  = Thread.getThis();
        if ( !thisThread.m_lock )
        {
            oldStackTop = thisThread.m_curr.tstack;
            thisThread.m_curr.tstack = curStackTop;
        }
    }

    scope( exit )
    {
        if ( Thread.sm_tbeg )
        {
            if ( !thisThread.m_lock )
            {
                thisThread.m_curr.tstack = oldStackTop;
            }
        }
    }

    // NOTE: Synchronizing on Thread.slock is not needed because this
    //       function may only be called after all other threads have
    //       been suspended from within the same lock.
    if (Thread.nAboutToStart)
        scan(ScanType.stack, Thread.pAboutToStart, Thread.pAboutToStart + Thread.nAboutToStart);

    for ( Thread.Context* c = Thread.sm_cbeg; c; c = c.next )
    {
        version (StackGrowsDown)
        {
            // NOTE: We can't index past the bottom of the stack
            //       so don't do the "+1" for StackGrowsDown.
            if ( c.tstack && c.tstack < c.bstack )
                scan( ScanType.stack, c.tstack, c.bstack );
        }
        else
        {
            if ( c.bstack && c.bstack < c.tstack )
                scan( ScanType.stack, c.bstack, c.tstack + 1 );
        }
    }

    for ( Thread t = Thread.sm_tbeg; t; t = t.next )
    {
        version (Windows)
        {
            // Ideally, we'd pass ScanType.regs or something like that, but this
            // would make portability annoying because it only makes sense on Windows.
            scan( ScanType.stack, t.m_reg.ptr, t.m_reg.ptr + t.m_reg.length );
        }

        if (t.m_tlsgcdata !is null)
            rt_tlsgc_scan(t.m_tlsgcdata, (p1, p2) => scan(ScanType.tls, p1, p2));
    }
}

/**
 * The main entry point for garbage collection.  The supplied delegate
 * will be passed ranges representing both stack and register values.
 *
 * Params:
 *  scan        = The scanner function.  It should scan from p1 through p2 - 1.
 *
 * In:
 *  This routine must be preceded by a call to thread_suspendAll.
 */
extern (C) void thread_scanAll( scope ScanAllThreadsFn scan ) nothrow
{
    thread_scanAllType((type, p1, p2) => scan(p1, p2));
}


/**
 * Signals that the code following this call is a critical region. Any code in
 * this region must finish running before the calling thread can be suspended
 * by a call to thread_suspendAll.
 *
 * This function is, in particular, meant to help maintain garbage collector
 * invariants when a lock is not used.
 *
 * A critical region is exited with thread_exitCriticalRegion.
 *
 * $(RED Warning):
 * Using critical regions is extremely error-prone. For instance, using locks
 * inside a critical region can easily result in a deadlock when another thread
 * holding the lock already got suspended.
 *
 * The term and concept of a 'critical region' comes from
 * $(LINK2 https://github.com/mono/mono/blob/521f4a198e442573c400835ef19bbb36b60b0ebb/mono/metadata/sgen-gc.h#L925 Mono's SGen garbage collector).
 *
 * In:
 *  The calling thread must be attached to the runtime.
 */
extern (C) void thread_enterCriticalRegion() @nogc
in
{
    assert(Thread.getThis());
}
body
{
    synchronized (Thread.criticalRegionLock)
        Thread.getThis().m_isInCriticalRegion = true;
}


/**
 * Signals that the calling thread is no longer in a critical region. Following
 * a call to this function, the thread can once again be suspended.
 *
 * In:
 *  The calling thread must be attached to the runtime.
 */
extern (C) void thread_exitCriticalRegion() @nogc
in
{
    assert(Thread.getThis());
}
body
{
    synchronized (Thread.criticalRegionLock)
        Thread.getThis().m_isInCriticalRegion = false;
}


/**
 * Returns true if the current thread is in a critical region; otherwise, false.
 *
 * In:
 *  The calling thread must be attached to the runtime.
 */
extern (C) bool thread_inCriticalRegion() @nogc
in
{
    assert(Thread.getThis());
}
body
{
    synchronized (Thread.criticalRegionLock)
        return Thread.getThis().m_isInCriticalRegion;
}


/**
* A callback for thread errors in D during collections. Since an allocation is not possible
*  a preallocated ThreadError will be used as the Error instance
*
* Throws:
*  ThreadError.
*/
private void onThreadError(string msg = null, Throwable next = null) nothrow
{
    __gshared ThreadError error = new ThreadError(null);
    error.msg = msg;
    error.next = next;
    import core.exception : SuppressTraceInfo;
    error.info = SuppressTraceInfo.instance;
    throw error;
}


unittest
{
    assert(!thread_inCriticalRegion());

    {
        thread_enterCriticalRegion();

        scope (exit)
            thread_exitCriticalRegion();

        assert(thread_inCriticalRegion());
    }

    assert(!thread_inCriticalRegion());
}

unittest
{
    // NOTE: This entire test is based on the assumption that no
    //       memory is allocated after the child thread is
    //       started. If an allocation happens, a collection could
    //       trigger, which would cause the synchronization below
    //       to cause a deadlock.
    // NOTE: DO NOT USE LOCKS IN CRITICAL REGIONS IN NORMAL CODE.

    import core.sync.semaphore;

    auto sema = new Semaphore(),
         semb = new Semaphore();

    auto thr = new Thread(
    {
        thread_enterCriticalRegion();
        assert(thread_inCriticalRegion());
        sema.notify();

        semb.wait();
        assert(thread_inCriticalRegion());

        thread_exitCriticalRegion();
        assert(!thread_inCriticalRegion());
        sema.notify();

        semb.wait();
        assert(!thread_inCriticalRegion());
    });

    thr.start();

    sema.wait();
    synchronized (Thread.criticalRegionLock)
        assert(thr.m_isInCriticalRegion);
    semb.notify();

    sema.wait();
    synchronized (Thread.criticalRegionLock)
        assert(!thr.m_isInCriticalRegion);
    semb.notify();

    thr.join();
}

unittest
{
    import core.sync.semaphore;

    shared bool inCriticalRegion;
    auto sema = new Semaphore(),
         semb = new Semaphore();

    auto thr = new Thread(
    {
        thread_enterCriticalRegion();
        inCriticalRegion = true;
        sema.notify();
        semb.wait();

        Thread.sleep(dur!"msecs"(1));
        inCriticalRegion = false;
        thread_exitCriticalRegion();
    });
    thr.start();

    sema.wait();
    assert(inCriticalRegion);
    semb.notify();

    thread_suspendAll();
    assert(!inCriticalRegion);
    thread_resumeAll();
}

/**
 * Indicates whether an address has been marked by the GC.
 */
enum IsMarked : int
{
         no, /// Address is not marked.
        yes, /// Address is marked.
    unknown, /// Address is not managed by the GC.
}

alias IsMarkedDg = int delegate( void* addr ) nothrow; /// The isMarked callback function.

/**
 * This routine allows the runtime to process any special per-thread handling
 * for the GC.  This is needed for taking into account any memory that is
 * referenced by non-scanned pointers but is about to be freed.  That currently
 * means the array append cache.
 *
 * Params:
 *  isMarked = The function used to check if $(D addr) is marked.
 *
 * In:
 *  This routine must be called just prior to resuming all threads.
 */
extern(C) void thread_processGCMarks( scope IsMarkedDg isMarked ) nothrow
{
    for ( Thread t = Thread.sm_tbeg; t; t = t.next )
    {
        /* Can be null if collection was triggered between adding a
         * thread and calling rt_tlsgc_init.
         */
        if (t.m_tlsgcdata !is null)
            rt_tlsgc_processGCMarks(t.m_tlsgcdata, isMarked);
    }
}


extern (C) @nogc nothrow
{
    version (CRuntime_Glibc) int pthread_getattr_np(pthread_t thread, pthread_attr_t* attr);
    version (FreeBSD) int pthread_attr_get_np(pthread_t thread, pthread_attr_t* attr);
    version (NetBSD) int pthread_attr_get_np(pthread_t thread, pthread_attr_t* attr);
    version (OpenBSD) int pthread_stackseg_np(pthread_t thread, stack_t* sinfo);
    version (DragonFlyBSD) int pthread_attr_get_np(pthread_t thread, pthread_attr_t* attr);
    version (Solaris) int thr_stksegment(stack_t* stk);
    version (CRuntime_Bionic) int pthread_getattr_np(pthread_t thid, pthread_attr_t* attr);
    version (CRuntime_Musl) int pthread_getattr_np(pthread_t, pthread_attr_t*);
    version (CRuntime_UClibc) int pthread_getattr_np(pthread_t thread, pthread_attr_t* attr);
}


private void* getStackTop() nothrow @nogc
{
    version (D_InlineAsm_X86)
        asm pure nothrow @nogc { naked; mov EAX, ESP; ret; }
    else version (D_InlineAsm_X86_64)
        asm pure nothrow @nogc { naked; mov RAX, RSP; ret; }
    else version (GNU)
        return __builtin_frame_address(0);
    else
        static assert(false, "Architecture not supported.");
}


private void* getStackBottom() nothrow @nogc
{
    version (Windows)
    {
        version (D_InlineAsm_X86)
            asm pure nothrow @nogc { naked; mov EAX, FS:4; ret; }
        else version (D_InlineAsm_X86_64)
            asm pure nothrow @nogc
            {    naked;
                 mov RAX, 8;
                 mov RAX, GS:[RAX];
                 ret;
            }
        else version (GNU_InlineAsm)
        {
            void *bottom;

            version (X86)
                asm pure nothrow @nogc { "movl %%fs:4, %0;" : "=r" bottom; }
            else version (X86_64)
                asm pure nothrow @nogc { "movq %%gs:8, %0;" : "=r" bottom; }
            else
                static assert(false, "Platform not supported.");

            return bottom;
        }
        else
            static assert(false, "Architecture not supported.");
    }
    else version (Darwin)
    {
        import core.sys.darwin.pthread;
        return pthread_get_stackaddr_np(pthread_self());
    }
    else version (CRuntime_Glibc)
    {
        pthread_attr_t attr;
        void* addr; size_t size;

        pthread_getattr_np(pthread_self(), &attr);
        pthread_attr_getstack(&attr, &addr, &size);
        pthread_attr_destroy(&attr);
        version (StackGrowsDown)
            addr += size;
        return addr;
    }
    else version (FreeBSD)
    {
        pthread_attr_t attr;
        void* addr; size_t size;

        pthread_attr_init(&attr);
        pthread_attr_get_np(pthread_self(), &attr);
        pthread_attr_getstack(&attr, &addr, &size);
        pthread_attr_destroy(&attr);
        version (StackGrowsDown)
            addr += size;
        return addr;
    }
    else version (NetBSD)
    {
        pthread_attr_t attr;
        void* addr; size_t size;

        pthread_attr_init(&attr);
        pthread_attr_get_np(pthread_self(), &attr);
        pthread_attr_getstack(&attr, &addr, &size);
        pthread_attr_destroy(&attr);
        version (StackGrowsDown)
            addr += size;
        return addr;
    }
    else version (OpenBSD)
    {
        stack_t stk;

        pthread_stackseg_np(pthread_self(), &stk);
        return stk.ss_sp;
    }
    else version (DragonFlyBSD)
    {
        pthread_attr_t attr;
        void* addr; size_t size;

        pthread_attr_init(&attr);
        pthread_attr_get_np(pthread_self(), &attr);
        pthread_attr_getstack(&attr, &addr, &size);
        pthread_attr_destroy(&attr);
        version (StackGrowsDown)
            addr += size;
        return addr;
    }
    else version (Solaris)
    {
        stack_t stk;

        thr_stksegment(&stk);
        return stk.ss_sp;
    }
    else version (CRuntime_Bionic)
    {
        pthread_attr_t attr;
        void* addr; size_t size;

        pthread_getattr_np(pthread_self(), &attr);
        pthread_attr_getstack(&attr, &addr, &size);
        pthread_attr_destroy(&attr);
        version (StackGrowsDown)
            addr += size;
        return addr;
    }
    else version (CRuntime_Musl)
    {
        pthread_attr_t attr;
        void* addr; size_t size;

        pthread_getattr_np(pthread_self(), &attr);
        pthread_attr_getstack(&attr, &addr, &size);
        pthread_attr_destroy(&attr);
        version (StackGrowsDown)
            addr += size;
        return addr;
    }
    else version (CRuntime_UClibc)
    {
        pthread_attr_t attr;
        void* addr; size_t size;

        pthread_getattr_np(pthread_self(), &attr);
        pthread_attr_getstack(&attr, &addr, &size);
        pthread_attr_destroy(&attr);
        version (StackGrowsDown)
            addr += size;
        return addr;
    }
    else
        static assert(false, "Platform not supported.");
}


/**
 * Returns the stack top of the currently active stack within the calling
 * thread.
 *
 * In:
 *  The calling thread must be attached to the runtime.
 *
 * Returns:
 *  The address of the stack top.
 */
extern (C) void* thread_stackTop() nothrow @nogc
in
{
    // Not strictly required, but it gives us more flexibility.
    assert(Thread.getThis());
}
body
{
    return getStackTop();
}


/**
 * Returns the stack bottom of the currently active stack within the calling
 * thread.
 *
 * In:
 *  The calling thread must be attached to the runtime.
 *
 * Returns:
 *  The address of the stack bottom.
 */
extern (C) void* thread_stackBottom() nothrow @nogc
in
{
    assert(Thread.getThis());
}
body
{
    return Thread.getThis().topContext().bstack;
}


///////////////////////////////////////////////////////////////////////////////
// Thread Group
///////////////////////////////////////////////////////////////////////////////


/**
 * This class is intended to simplify certain common programming techniques.
 */
class ThreadGroup
{
    /**
     * Creates and starts a new Thread object that executes fn and adds it to
     * the list of tracked threads.
     *
     * Params:
     *  fn = The thread function.
     *
     * Returns:
     *  A reference to the newly created thread.
     */
    final Thread create( void function() fn )
    {
        Thread t = new Thread( fn ).start();

        synchronized( this )
        {
            m_all[t] = t;
        }
        return t;
    }


    /**
     * Creates and starts a new Thread object that executes dg and adds it to
     * the list of tracked threads.
     *
     * Params:
     *  dg = The thread function.
     *
     * Returns:
     *  A reference to the newly created thread.
     */
    final Thread create( void delegate() dg )
    {
        Thread t = new Thread( dg ).start();

        synchronized( this )
        {
            m_all[t] = t;
        }
        return t;
    }


    /**
     * Add t to the list of tracked threads if it is not already being tracked.
     *
     * Params:
     *  t = The thread to add.
     *
     * In:
     *  t must not be null.
     */
    final void add( Thread t )
    in
    {
        assert( t );
    }
    body
    {
        synchronized( this )
        {
            m_all[t] = t;
        }
    }


    /**
     * Removes t from the list of tracked threads.  No operation will be
     * performed if t is not currently being tracked by this object.
     *
     * Params:
     *  t = The thread to remove.
     *
     * In:
     *  t must not be null.
     */
    final void remove( Thread t )
    in
    {
        assert( t );
    }
    body
    {
        synchronized( this )
        {
            m_all.remove( t );
        }
    }


    /**
     * Operates on all threads currently tracked by this object.
     */
    final int opApply( scope int delegate( ref Thread ) dg )
    {
        synchronized( this )
        {
            int ret = 0;

            // NOTE: This loop relies on the knowledge that m_all uses the
            //       Thread object for both the key and the mapped value.
            foreach ( Thread t; m_all.keys )
            {
                ret = dg( t );
                if ( ret )
                    break;
            }
            return ret;
        }
    }


    /**
     * Iteratively joins all tracked threads.  This function will block add,
     * remove, and opApply until it completes.
     *
     * Params:
     *  rethrow = Rethrow any unhandled exception which may have caused the
     *            current thread to terminate.
     *
     * Throws:
     *  Any exception not handled by the joined threads.
     */
    final void joinAll( bool rethrow = true )
    {
        synchronized( this )
        {
            // NOTE: This loop relies on the knowledge that m_all uses the
            //       Thread object for both the key and the mapped value.
            foreach ( Thread t; m_all.keys )
            {
                t.join( rethrow );
            }
        }
    }


private:
    Thread[Thread]  m_all;
}


///////////////////////////////////////////////////////////////////////////////
// Fiber Platform Detection and Memory Allocation
///////////////////////////////////////////////////////////////////////////////


private
{
    version (D_InlineAsm_X86)
    {
        version (Windows)
            version = AsmX86_Windows;
        else version (Posix)
            version = AsmX86_Posix;

        version (Darwin)
            version = AlignFiberStackTo16Byte;
    }
    else version (D_InlineAsm_X86_64)
    {
        version (Windows)
        {
            version = AsmX86_64_Windows;
            version = AlignFiberStackTo16Byte;
        }
        else version (Posix)
        {
            version = AsmX86_64_Posix;
            version = AlignFiberStackTo16Byte;
        }
    }
    else version (X86)
    {
        version = AsmExternal;

        version (MinGW)
        {
            version = GNU_AsmX86_Windows;
            version = AlignFiberStackTo16Byte;
        }
        else version (Posix)
        {
            version = AsmX86_Posix;
            version (OSX)
                version = AlignFiberStackTo16Byte;
        }
    }
    else version (X86_64)
    {
        version (D_X32)
        {
            // let X32 be handled by ucontext swapcontext
        }
        else
        {
            version = AsmExternal;
            version = AlignFiberStackTo16Byte;

            version (MinGW)
                version = GNU_AsmX86_64_Windows;
            else version (Posix)
                version = AsmX86_64_Posix;
        }
    }
    else version (PPC)
    {
        version (Posix)
        {
            version = AsmPPC_Posix;
            version = AsmExternal;
        }
    }
    else version (PPC64)
    {
        version (Posix)
        {
            version = AlignFiberStackTo16Byte;
        }
    }
    else version (MIPS_O32)
    {
        version (Posix)
        {
            version = AsmMIPS_O32_Posix;
            version = AsmExternal;
        }
    }
    else version (AArch64)
    {
        version (Posix)
        {
            version = AsmAArch64_Posix;
            version = AsmExternal;
            version = AlignFiberStackTo16Byte;
        }
    }
    else version (ARM)
    {
        version (Posix)
        {
            version = AsmARM_Posix;
            version = AsmExternal;
        }
    }
    else version (SPARC)
    {
        // NOTE: The SPARC ABI specifies only doubleword alignment.
        version = AlignFiberStackTo16Byte;
    }
    else version (SPARC64)
    {
        version = AlignFiberStackTo16Byte;
    }

    version (Posix)
    {
        import core.sys.posix.unistd;   // for sysconf

        version (AsmX86_Windows)    {} else
        version (AsmX86_Posix)      {} else
        version (AsmX86_64_Windows) {} else
        version (AsmX86_64_Posix)   {} else
        version (AsmExternal)       {} else
        {
            // NOTE: The ucontext implementation requires architecture specific
            //       data definitions to operate so testing for it must be done
            //       by checking for the existence of ucontext_t rather than by
            //       a version identifier.  Please note that this is considered
            //       an obsolescent feature according to the POSIX spec, so a
            //       custom solution is still preferred.
            import core.sys.posix.ucontext;
        }
    }

    static immutable size_t PAGESIZE;
    version (Posix) static immutable size_t PTHREAD_STACK_MIN;
}


shared static this()
{
    version (Windows)
    {
        SYSTEM_INFO info;
        GetSystemInfo(&info);

        PAGESIZE = info.dwPageSize;
        assert(PAGESIZE < int.max);
    }
    else version (Posix)
    {
        PAGESIZE = cast(size_t)sysconf(_SC_PAGESIZE);
        PTHREAD_STACK_MIN = cast(size_t)sysconf(_SC_THREAD_STACK_MIN);
    }
    else
    {
        static assert(0, "unimplemented");
    }
}


///////////////////////////////////////////////////////////////////////////////
// Fiber Entry Point and Context Switch
///////////////////////////////////////////////////////////////////////////////


private
{
    extern (C) void fiber_entryPoint() nothrow
    {
        Fiber   obj = Fiber.getThis();
        assert( obj );

        assert( Thread.getThis().m_curr is obj.m_ctxt );
        atomicStore!(MemoryOrder.raw)(*cast(shared)&Thread.getThis().m_lock, false);
        obj.m_ctxt.tstack = obj.m_ctxt.bstack;
        obj.m_state = Fiber.State.EXEC;

        try
        {
            obj.run();
        }
        catch ( Throwable t )
        {
            obj.m_unhandled = t;
        }

        static if ( __traits( compiles, ucontext_t ) )
          obj.m_ucur = &obj.m_utxt;

        obj.m_state = Fiber.State.TERM;
        obj.switchOut();
    }

  // Look above the definition of 'class Fiber' for some information about the implementation of this routine
  version (AsmExternal)
  {
      extern (C) void fiber_switchContext( void** oldp, void* newp ) nothrow @nogc;
      version (AArch64)
          extern (C) void fiber_trampoline() nothrow;
  }
  else
    extern (C) void fiber_switchContext( void** oldp, void* newp ) nothrow @nogc
    {
        // NOTE: The data pushed and popped in this routine must match the
        //       default stack created by Fiber.initStack or the initial
        //       switch into a new context will fail.

        version (AsmX86_Windows)
        {
            asm pure nothrow @nogc
            {
                naked;

                // save current stack state
                push EBP;
                mov  EBP, ESP;
                push EDI;
                push ESI;
                push EBX;
                push dword ptr FS:[0];
                push dword ptr FS:[4];
                push dword ptr FS:[8];
                push EAX;

                // store oldp again with more accurate address
                mov EAX, dword ptr 8[EBP];
                mov [EAX], ESP;
                // load newp to begin context switch
                mov ESP, dword ptr 12[EBP];

                // load saved state from new stack
                pop EAX;
                pop dword ptr FS:[8];
                pop dword ptr FS:[4];
                pop dword ptr FS:[0];
                pop EBX;
                pop ESI;
                pop EDI;
                pop EBP;

                // 'return' to complete switch
                pop ECX;
                jmp ECX;
            }
        }
        else version (AsmX86_64_Windows)
        {
            asm pure nothrow @nogc
            {
                naked;

                // save current stack state
                // NOTE: When changing the layout of registers on the stack,
                //       make sure that the XMM registers are still aligned.
                //       On function entry, the stack is guaranteed to not
                //       be aligned to 16 bytes because of the return address
                //       on the stack.
                push RBP;
                mov  RBP, RSP;
                push R12;
                push R13;
                push R14;
                push R15;
                push RDI;
                push RSI;
                // 7 registers = 56 bytes; stack is now aligned to 16 bytes
                sub RSP, 160;
                movdqa [RSP + 144], XMM6;
                movdqa [RSP + 128], XMM7;
                movdqa [RSP + 112], XMM8;
                movdqa [RSP + 96], XMM9;
                movdqa [RSP + 80], XMM10;
                movdqa [RSP + 64], XMM11;
                movdqa [RSP + 48], XMM12;
                movdqa [RSP + 32], XMM13;
                movdqa [RSP + 16], XMM14;
                movdqa [RSP], XMM15;
                push RBX;
                xor  RAX,RAX;
                push qword ptr GS:[RAX];
                push qword ptr GS:8[RAX];
                push qword ptr GS:16[RAX];

                // store oldp
                mov [RCX], RSP;
                // load newp to begin context switch
                mov RSP, RDX;

                // load saved state from new stack
                pop qword ptr GS:16[RAX];
                pop qword ptr GS:8[RAX];
                pop qword ptr GS:[RAX];
                pop RBX;
                movdqa XMM15, [RSP];
                movdqa XMM14, [RSP + 16];
                movdqa XMM13, [RSP + 32];
                movdqa XMM12, [RSP + 48];
                movdqa XMM11, [RSP + 64];
                movdqa XMM10, [RSP + 80];
                movdqa XMM9, [RSP + 96];
                movdqa XMM8, [RSP + 112];
                movdqa XMM7, [RSP + 128];
                movdqa XMM6, [RSP + 144];
                add RSP, 160;
                pop RSI;
                pop RDI;
                pop R15;
                pop R14;
                pop R13;
                pop R12;
                pop RBP;

                // 'return' to complete switch
                pop RCX;
                jmp RCX;
            }
        }
        else version (AsmX86_Posix)
        {
            asm pure nothrow @nogc
            {
                naked;

                // save current stack state
                push EBP;
                mov  EBP, ESP;
                push EDI;
                push ESI;
                push EBX;
                push EAX;

                // store oldp again with more accurate address
                mov EAX, dword ptr 8[EBP];
                mov [EAX], ESP;
                // load newp to begin context switch
                mov ESP, dword ptr 12[EBP];

                // load saved state from new stack
                pop EAX;
                pop EBX;
                pop ESI;
                pop EDI;
                pop EBP;

                // 'return' to complete switch
                pop ECX;
                jmp ECX;
            }
        }
        else version (AsmX86_64_Posix)
        {
            asm pure nothrow @nogc
            {
                naked;

                // save current stack state
                push RBP;
                mov  RBP, RSP;
                push RBX;
                push R12;
                push R13;
                push R14;
                push R15;

                // store oldp
                mov [RDI], RSP;
                // load newp to begin context switch
                mov RSP, RSI;

                // load saved state from new stack
                pop R15;
                pop R14;
                pop R13;
                pop R12;
                pop RBX;
                pop RBP;

                // 'return' to complete switch
                pop RCX;
                jmp RCX;
            }
        }
        else static if ( __traits( compiles, ucontext_t ) )
        {
            Fiber   cfib = Fiber.getThis();
            void*   ucur = cfib.m_ucur;

            *oldp = &ucur;
            swapcontext( **(cast(ucontext_t***) oldp),
                          *(cast(ucontext_t**)  newp) );
        }
        else
            static assert(0, "Not implemented");
    }
}


///////////////////////////////////////////////////////////////////////////////
// Fiber
///////////////////////////////////////////////////////////////////////////////
/*
 * Documentation of Fiber internals:
 *
 * The main routines to implement when porting Fibers to new architectures are
 * fiber_switchContext and initStack. Some version constants have to be defined
 * for the new platform as well, search for "Fiber Platform Detection and Memory Allocation".
 *
 * Fibers are based on a concept called 'Context'. A Context describes the execution
 * state of a Fiber or main thread which is fully described by the stack, some
 * registers and a return address at which the Fiber/Thread should continue executing.
 * Please note that not only each Fiber has a Context, but each thread also has got a
 * Context which describes the threads stack and state. If you call Fiber fib; fib.call
 * the first time in a thread you switch from Threads Context into the Fibers Context.
 * If you call fib.yield in that Fiber you switch out of the Fibers context and back
 * into the Thread Context. (However, this is not always the case. You can call a Fiber
 * from within another Fiber, then you switch Contexts between the Fibers and the Thread
 * Context is not involved)
 *
 * In all current implementations the registers and the return address are actually
 * saved on a Contexts stack.
 *
 * The fiber_switchContext routine has got two parameters:
 * void** a:  This is the _location_ where we have to store the current stack pointer,
 *            the stack pointer of the currently executing Context (Fiber or Thread).
 * void*  b:  This is the pointer to the stack of the Context which we want to switch into.
 *            Note that we get the same pointer here as the one we stored into the void** a
 *            in a previous call to fiber_switchContext.
 *
 * In the simplest case, a fiber_switchContext rountine looks like this:
 * fiber_switchContext:
 *     push {return Address}
 *     push {registers}
 *     copy {stack pointer} into {location pointed to by a}
 *     //We have now switch to the stack of a different Context!
 *     copy {b} into {stack pointer}
 *     pop {registers}
 *     pop {return Address}
 *     jump to {return Address}
 *
 * The GC uses the value returned in parameter a to scan the Fibers stack. It scans from
 * the stack base to that value. As the GC dislikes false pointers we can actually optimize
 * this a little: By storing registers which can not contain references to memory managed
 * by the GC outside of the region marked by the stack base pointer and the stack pointer
 * saved in fiber_switchContext we can prevent the GC from scanning them.
 * Such registers are usually floating point registers and the return address. In order to
 * implement this, we return a modified stack pointer from fiber_switchContext. However,
 * we have to remember that when we restore the registers from the stack!
 *
 * --------------------------- <= Stack Base
 * |          Frame          | <= Many other stack frames
 * |          Frame          |
 * |-------------------------| <= The last stack frame. This one is created by fiber_switchContext
 * | registers with pointers |
 * |                         | <= Stack pointer. GC stops scanning here
 * |   return address        |
 * |floating point registers |
 * --------------------------- <= Real Stack End
 *
 * fiber_switchContext:
 *     push {registers with pointers}
 *     copy {stack pointer} into {location pointed to by a}
 *     push {return Address}
 *     push {Floating point registers}
 *     //We have now switch to the stack of a different Context!
 *     copy {b} into {stack pointer}
 *     //We now have to adjust the stack pointer to point to 'Real Stack End' so we can pop
 *     //the FP registers
 *     //+ or - depends on if your stack grows downwards or upwards
 *     {stack pointer} = {stack pointer} +- ({FPRegisters}.sizeof + {return address}.sizeof}
 *     pop {Floating point registers}
 *     pop {return Address}
 *     pop {registers with pointers}
 *     jump to {return Address}
 *
 * So the question now is which registers need to be saved? This depends on the specific
 * architecture ABI of course, but here are some general guidelines:
 * - If a register is callee-save (if the callee modifies the register it must saved and
 *   restored by the callee) it needs to be saved/restored in switchContext
 * - If a register is caller-save it needn't be saved/restored. (Calling fiber_switchContext
 *   is a function call and the compiler therefore already must save these registers before
 *   calling fiber_switchContext)
 * - Argument registers used for passing parameters to functions needn't be saved/restored
 * - The return register needn't be saved/restored (fiber_switchContext hasn't got a return type)
 * - All scratch registers needn't be saved/restored
 * - The link register usually needn't be saved/restored (but sometimes it must be cleared -
 *   see below for details)
 * - The frame pointer register - if it exists - is usually callee-save
 * - All current implementations do not save control registers
 *
 * What happens on the first switch into a Fiber? We never saved a state for this fiber before,
 * but the initial state is prepared in the initStack routine. (This routine will also be called
 * when a Fiber is being resetted). initStack must produce exactly the same stack layout as the
 * part of fiber_switchContext which saves the registers. Pay special attention to set the stack
 * pointer correctly if you use the GC optimization mentioned before. the return Address saved in
 * initStack must be the address of fiber_entrypoint.
 *
 * There's now a small but important difference between the first context switch into a fiber and
 * further context switches. On the first switch, Fiber.call is used and the returnAddress in
 * fiber_switchContext will point to fiber_entrypoint. The important thing here is that this jump
 * is a _function call_, we call fiber_entrypoint by jumping before it's function prologue. On later
 * calls, the user used yield() in a function, and therefore the return address points into a user
 * function, after the yield call. So here the jump in fiber_switchContext is a _function return_,
 * not a function call!
 *
 * The most important result of this is that on entering a function, i.e. fiber_entrypoint, we
 * would have to provide a return address / set the link register once fiber_entrypoint
 * returns. Now fiber_entrypoint does never return and therefore the actual value of the return
 * address / link register is never read/used and therefore doesn't matter. When fiber_switchContext
 * performs a _function return_ the value in the link register doesn't matter either.
 * However, the link register will still be saved to the stack in fiber_entrypoint and some
 * exception handling / stack unwinding code might read it from this stack location and crash.
 * The exact solution depends on your architecture, but see the ARM implementation for a way
 * to deal with this issue.
 *
 * The ARM implementation is meant to be used as a kind of documented example implementation.
 * Look there for a concrete example.
 *
 * FIXME: fiber_entrypoint might benefit from a @noreturn attribute, but D doesn't have one.
 */

/**
 * This class provides a cooperative concurrency mechanism integrated with the
 * threading and garbage collection functionality.  Calling a fiber may be
 * considered a blocking operation that returns when the fiber yields (via
 * Fiber.yield()).  Execution occurs within the context of the calling thread
 * so synchronization is not necessary to guarantee memory visibility so long
 * as the same thread calls the fiber each time.  Please note that there is no
 * requirement that a fiber be bound to one specific thread.  Rather, fibers
 * may be freely passed between threads so long as they are not currently
 * executing.  Like threads, a new fiber thread may be created using either
 * derivation or composition, as in the following example.
 *
 * Warning:
 * Status registers are not saved by the current implementations. This means
 * floating point exception status bits (overflow, divide by 0), rounding mode
 * and similar stuff is set per-thread, not per Fiber!
 *
 * Warning:
 * On ARM FPU registers are not saved if druntime was compiled as ARM_SoftFloat.
 * If such a build is used on a ARM_SoftFP system which actually has got a FPU
 * and other libraries are using the FPU registers (other code is compiled
 * as ARM_SoftFP) this can cause problems. Druntime must be compiled as
 * ARM_SoftFP in this case.
 *
 * Example:
 * ----------------------------------------------------------------------
 *
 * class DerivedFiber : Fiber
 * {
 *     this()
 *     {
 *         super( &run );
 *     }
 *
 * private :
 *     void run()
 *     {
 *         printf( "Derived fiber running.\n" );
 *     }
 * }
 *
 * void fiberFunc()
 * {
 *     printf( "Composed fiber running.\n" );
 *     Fiber.yield();
 *     printf( "Composed fiber running.\n" );
 * }
 *
 * // create instances of each type
 * Fiber derived = new DerivedFiber();
 * Fiber composed = new Fiber( &fiberFunc );
 *
 * // call both fibers once
 * derived.call();
 * composed.call();
 * printf( "Execution returned to calling context.\n" );
 * composed.call();
 *
 * // since each fiber has run to completion, each should have state TERM
 * assert( derived.state == Fiber.State.TERM );
 * assert( composed.state == Fiber.State.TERM );
 *
 * ----------------------------------------------------------------------
 *
 * Authors: Based on a design by Mikola Lysenko.
 */
class Fiber
{
    ///////////////////////////////////////////////////////////////////////////
    // Initialization
    ///////////////////////////////////////////////////////////////////////////


    /**
     * Initializes a fiber object which is associated with a static
     * D function.
     *
     * Params:
     *  fn = The fiber function.
     *  sz = The stack size for this fiber.
     *  guardPageSize = size of the guard page to trap fiber's stack
     *                    overflows
     *
     * In:
     *  fn must not be null.
     */
    this( void function() fn, size_t sz = PAGESIZE*4,
          size_t guardPageSize = PAGESIZE ) nothrow
    in
    {
        assert( fn );
    }
    body
    {
        allocStack( sz, guardPageSize );
        reset( fn );
    }


    /**
     * Initializes a fiber object which is associated with a dynamic
     * D function.
     *
     * Params:
     *  dg = The fiber function.
     *  sz = The stack size for this fiber.
     *  guardPageSize = size of the guard page to trap fiber's stack
     *                    overflows
     *
     * In:
     *  dg must not be null.
     */
    this( void delegate() dg, size_t sz = PAGESIZE*4,
          size_t guardPageSize = PAGESIZE ) nothrow
    in
    {
        assert( dg );
    }
    body
    {
        allocStack( sz, guardPageSize);
        reset( dg );
    }


    /**
     * Cleans up any remaining resources used by this object.
     */
    ~this() nothrow @nogc
    {
        // NOTE: A live reference to this object will exist on its associated
        //       stack from the first time its call() method has been called
        //       until its execution completes with State.TERM.  Thus, the only
        //       times this dtor should be called are either if the fiber has
        //       terminated (and therefore has no active stack) or if the user
        //       explicitly deletes this object.  The latter case is an error
        //       but is not easily tested for, since State.HOLD may imply that
        //       the fiber was just created but has never been run.  There is
        //       not a compelling case to create a State.INIT just to offer a
        //       means of ensuring the user isn't violating this object's
        //       contract, so for now this requirement will be enforced by
        //       documentation only.
        freeStack();
    }


    ///////////////////////////////////////////////////////////////////////////
    // General Actions
    ///////////////////////////////////////////////////////////////////////////


    /**
     * Transfers execution to this fiber object.  The calling context will be
     * suspended until the fiber calls Fiber.yield() or until it terminates
     * via an unhandled exception.
     *
     * Params:
     *  rethrow = Rethrow any unhandled exception which may have caused this
     *            fiber to terminate.
     *
     * In:
     *  This fiber must be in state HOLD.
     *
     * Throws:
     *  Any exception not handled by the joined thread.
     *
     * Returns:
     *  Any exception not handled by this fiber if rethrow = false, null
     *  otherwise.
     */
    // Not marked with any attributes, even though `nothrow @nogc` works
    // because it calls arbitrary user code. Most of the implementation
    // is already `@nogc nothrow`, but in order for `Fiber.call` to
    // propagate the attributes of the user's function, the Fiber
    // class needs to be templated.
    final Throwable call( Rethrow rethrow = Rethrow.yes )
    {
        return rethrow ? call!(Rethrow.yes)() : call!(Rethrow.no);
    }

    /// ditto
    final Throwable call( Rethrow rethrow )()
    {
        callImpl();
        if ( m_unhandled )
        {
            Throwable t = m_unhandled;
            m_unhandled = null;
            static if ( rethrow )
                throw t;
            else
                return t;
        }
        return null;
    }

    /// ditto
    deprecated("Please pass Fiber.Rethrow.yes or .no instead of a boolean.")
    final Throwable call( bool rethrow )
    {
        return rethrow ? call!(Rethrow.yes)() : call!(Rethrow.no);
    }

    private void callImpl() nothrow @nogc
    in
    {
        assert( m_state == State.HOLD );
    }
    body
    {
        Fiber   cur = getThis();

        static if ( __traits( compiles, ucontext_t ) )
          m_ucur = cur ? &cur.m_utxt : &Fiber.sm_utxt;

        setThis( this );
        this.switchIn();
        setThis( cur );

        static if ( __traits( compiles, ucontext_t ) )
          m_ucur = null;

        // NOTE: If the fiber has terminated then the stack pointers must be
        //       reset.  This ensures that the stack for this fiber is not
        //       scanned if the fiber has terminated.  This is necessary to
        //       prevent any references lingering on the stack from delaying
        //       the collection of otherwise dead objects.  The most notable
        //       being the current object, which is referenced at the top of
        //       fiber_entryPoint.
        if ( m_state == State.TERM )
        {
            m_ctxt.tstack = m_ctxt.bstack;
        }
    }

    /// Flag to control rethrow behavior of $(D $(LREF call))
    enum Rethrow : bool { no, yes }

    /**
     * Resets this fiber so that it may be re-used, optionally with a
     * new function/delegate.  This routine should only be called for
     * fibers that have terminated, as doing otherwise could result in
     * scope-dependent functionality that is not executed.
     * Stack-based classes, for example, may not be cleaned up
     * properly if a fiber is reset before it has terminated.
     *
     * In:
     *  This fiber must be in state TERM or HOLD.
     */
    final void reset() nothrow @nogc
    in
    {
        assert( m_state == State.TERM || m_state == State.HOLD );
    }
    body
    {
        m_ctxt.tstack = m_ctxt.bstack;
        m_state = State.HOLD;
        initStack();
        m_unhandled = null;
    }

    /// ditto
    final void reset( void function() fn ) nothrow @nogc
    {
        reset();
        m_fn    = fn;
        m_call  = Call.FN;
    }

    /// ditto
    final void reset( void delegate() dg ) nothrow @nogc
    {
        reset();
        m_dg    = dg;
        m_call  = Call.DG;
    }

    ///////////////////////////////////////////////////////////////////////////
    // General Properties
    ///////////////////////////////////////////////////////////////////////////


    /**
     * A fiber may occupy one of three states: HOLD, EXEC, and TERM.  The HOLD
     * state applies to any fiber that is suspended and ready to be called.
     * The EXEC state will be set for any fiber that is currently executing.
     * And the TERM state is set when a fiber terminates.  Once a fiber
     * terminates, it must be reset before it may be called again.
     */
    enum State
    {
        HOLD,   ///
        EXEC,   ///
        TERM    ///
    }


    /**
     * Gets the current state of this fiber.
     *
     * Returns:
     *  The state of this fiber as an enumerated value.
     */
    final @property State state() const @safe pure nothrow @nogc
    {
        return m_state;
    }


    ///////////////////////////////////////////////////////////////////////////
    // Actions on Calling Fiber
    ///////////////////////////////////////////////////////////////////////////


    /**
     * Forces a context switch to occur away from the calling fiber.
     */
    static void yield() nothrow @nogc
    {
        Fiber   cur = getThis();
        assert( cur, "Fiber.yield() called with no active fiber" );
        assert( cur.m_state == State.EXEC );

        static if ( __traits( compiles, ucontext_t ) )
          cur.m_ucur = &cur.m_utxt;

        cur.m_state = State.HOLD;
        cur.switchOut();
        cur.m_state = State.EXEC;
    }


    /**
     * Forces a context switch to occur away from the calling fiber and then
     * throws obj in the calling fiber.
     *
     * Params:
     *  t = The object to throw.
     *
     * In:
     *  t must not be null.
     */
    static void yieldAndThrow( Throwable t ) nothrow @nogc
    in
    {
        assert( t );
    }
    body
    {
        Fiber   cur = getThis();
        assert( cur, "Fiber.yield() called with no active fiber" );
        assert( cur.m_state == State.EXEC );

        static if ( __traits( compiles, ucontext_t ) )
          cur.m_ucur = &cur.m_utxt;

        cur.m_unhandled = t;
        cur.m_state = State.HOLD;
        cur.switchOut();
        cur.m_state = State.EXEC;
    }


    ///////////////////////////////////////////////////////////////////////////
    // Fiber Accessors
    ///////////////////////////////////////////////////////////////////////////


    /**
     * Provides a reference to the calling fiber or null if no fiber is
     * currently active.
     *
     * Returns:
     *  The fiber object representing the calling fiber or null if no fiber
     *  is currently active within this thread. The result of deleting this object is undefined.
     */
    static Fiber getThis() @safe nothrow @nogc
    {
        return sm_this;
    }


    ///////////////////////////////////////////////////////////////////////////
    // Static Initialization
    ///////////////////////////////////////////////////////////////////////////


    version (Posix)
    {
        static this()
        {
            static if ( __traits( compiles, ucontext_t ) )
            {
              int status = getcontext( &sm_utxt );
              assert( status == 0 );
            }
        }
    }

private:
    //
    // Initializes a fiber object which has no associated executable function.
    //
    this() @safe pure nothrow @nogc
    {
        m_call = Call.NO;
    }


    //
    // Fiber entry point.  Invokes the function or delegate passed on
    // construction (if any).
    //
    final void run()
    {
        switch ( m_call )
        {
        case Call.FN:
            m_fn();
            break;
        case Call.DG:
            m_dg();
            break;
        default:
            break;
        }
    }


private:
    //
    // The type of routine passed on fiber construction.
    //
    enum Call
    {
        NO,
        FN,
        DG
    }


    //
    // Standard fiber data
    //
    Call                m_call;
    union
    {
        void function() m_fn;
        void delegate() m_dg;
    }
    bool                m_isRunning;
    Throwable           m_unhandled;
    State               m_state;


private:
    ///////////////////////////////////////////////////////////////////////////
    // Stack Management
    ///////////////////////////////////////////////////////////////////////////


    //
    // Allocate a new stack for this fiber.
    //
    final void allocStack( size_t sz, size_t guardPageSize ) nothrow
    in
    {
        assert( !m_pmem && !m_ctxt );
    }
    body
    {
        // adjust alloc size to a multiple of PAGESIZE
        sz += PAGESIZE - 1;
        sz -= sz % PAGESIZE;

        // NOTE: This instance of Thread.Context is dynamic so Fiber objects
        //       can be collected by the GC so long as no user level references
        //       to the object exist.  If m_ctxt were not dynamic then its
        //       presence in the global context list would be enough to keep
        //       this object alive indefinitely.  An alternative to allocating
        //       room for this struct explicitly would be to mash it into the
        //       base of the stack being allocated below.  However, doing so
        //       requires too much special logic to be worthwhile.
        m_ctxt = new Thread.Context;

        static if ( __traits( compiles, VirtualAlloc ) )
        {
            // reserve memory for stack
            m_pmem = VirtualAlloc( null,
                                   sz + guardPageSize,
                                   MEM_RESERVE,
                                   PAGE_NOACCESS );
            if ( !m_pmem )
                onOutOfMemoryError();

            version (StackGrowsDown)
            {
                void* stack = m_pmem + guardPageSize;
                void* guard = m_pmem;
                void* pbase = stack + sz;
            }
            else
            {
                void* stack = m_pmem;
                void* guard = m_pmem + sz;
                void* pbase = stack;
            }

            // allocate reserved stack segment
            stack = VirtualAlloc( stack,
                                  sz,
                                  MEM_COMMIT,
                                  PAGE_READWRITE );
            if ( !stack )
                onOutOfMemoryError();

            if (guardPageSize)
            {
                // allocate reserved guard page
                guard = VirtualAlloc( guard,
                                      guardPageSize,
                                      MEM_COMMIT,
                                      PAGE_READWRITE | PAGE_GUARD );
                if ( !guard )
                    onOutOfMemoryError();
            }

            m_ctxt.bstack = pbase;
            m_ctxt.tstack = pbase;
            m_size = sz;
        }
        else
        {
            version (Posix) import core.sys.posix.sys.mman; // mmap
            version (FreeBSD) import core.sys.freebsd.sys.mman : MAP_ANON;
            version (NetBSD) import core.sys.netbsd.sys.mman : MAP_ANON;
            version (OpenBSD) import core.sys.openbsd.sys.mman : MAP_ANON;
            version (DragonFlyBSD) import core.sys.dragonflybsd.sys.mman : MAP_ANON;
            version (CRuntime_Glibc) import core.sys.linux.sys.mman : MAP_ANON;
            version (Darwin) import core.sys.darwin.sys.mman : MAP_ANON;
            version (CRuntime_UClibc) import core.sys.linux.sys.mman : MAP_ANON;

            static if ( __traits( compiles, mmap ) )
            {
                // Allocate more for the memory guard
                sz += guardPageSize;

                m_pmem = mmap( null,
                               sz,
                               PROT_READ | PROT_WRITE,
                               MAP_PRIVATE | MAP_ANON,
                               -1,
                               0 );
                if ( m_pmem == MAP_FAILED )
                    m_pmem = null;
            }
            else static if ( __traits( compiles, valloc ) )
            {
                m_pmem = valloc( sz );
            }
            else static if ( __traits( compiles, malloc ) )
            {
                m_pmem = malloc( sz );
            }
            else
            {
                m_pmem = null;
            }

            if ( !m_pmem )
                onOutOfMemoryError();

            version (StackGrowsDown)
            {
                m_ctxt.bstack = m_pmem + sz;
                m_ctxt.tstack = m_pmem + sz;
                void* guard = m_pmem;
            }
            else
            {
                m_ctxt.bstack = m_pmem;
                m_ctxt.tstack = m_pmem;
                void* guard = m_pmem + sz - guardPageSize;
            }
            m_size = sz;

            static if ( __traits( compiles, mmap ) )
            {
                if (guardPageSize)
                {
                    // protect end of stack
                    if ( mprotect(guard, guardPageSize, PROT_NONE) == -1 )
                        abort();
                }
            }
            else
            {
                // Supported only for mmap allocated memory - results are
                // undefined if applied to memory not obtained by mmap
            }
        }

        Thread.add( m_ctxt );
    }


    //
    // Free this fiber's stack.
    //
    final void freeStack() nothrow @nogc
    in
    {
        assert( m_pmem && m_ctxt );
    }
    body
    {
        // NOTE: m_ctxt is guaranteed to be alive because it is held in the
        //       global context list.
        Thread.slock.lock_nothrow();
        scope(exit) Thread.slock.unlock_nothrow();
        Thread.remove( m_ctxt );

        static if ( __traits( compiles, VirtualAlloc ) )
        {
            VirtualFree( m_pmem, 0, MEM_RELEASE );
        }
        else
        {
            import core.sys.posix.sys.mman; // munmap

            static if ( __traits( compiles, mmap ) )
            {
                munmap( m_pmem, m_size );
            }
            else static if ( __traits( compiles, valloc ) )
            {
                free( m_pmem );
            }
            else static if ( __traits( compiles, malloc ) )
            {
                free( m_pmem );
            }
        }
        m_pmem = null;
        m_ctxt = null;
    }


    //
    // Initialize the allocated stack.
    // Look above the definition of 'class Fiber' for some information about the implementation of this routine
    //
    final void initStack() nothrow @nogc
    in
    {
        assert( m_ctxt.tstack && m_ctxt.tstack == m_ctxt.bstack );
        assert( cast(size_t) m_ctxt.bstack % (void*).sizeof == 0 );
    }
    body
    {
        void* pstack = m_ctxt.tstack;
        scope( exit )  m_ctxt.tstack = pstack;

        void push( size_t val ) nothrow
        {
            version (StackGrowsDown)
            {
                pstack -= size_t.sizeof;
                *(cast(size_t*) pstack) = val;
            }
            else
            {
                pstack += size_t.sizeof;
                *(cast(size_t*) pstack) = val;
            }
        }

        // NOTE: On OS X the stack must be 16-byte aligned according
        // to the IA-32 call spec. For x86_64 the stack also needs to
        // be aligned to 16-byte according to SysV AMD64 ABI.
        version (AlignFiberStackTo16Byte)
        {
            version (StackGrowsDown)
            {
                pstack = cast(void*)(cast(size_t)(pstack) - (cast(size_t)(pstack) & 0x0F));
            }
            else
            {
                pstack = cast(void*)(cast(size_t)(pstack) + (cast(size_t)(pstack) & 0x0F));
            }
        }

        version (AsmX86_Windows)
        {
            version (StackGrowsDown) {} else static assert( false );

            // On Windows Server 2008 and 2008 R2, an exploit mitigation
            // technique known as SEHOP is activated by default. To avoid
            // hijacking of the exception handler chain, the presence of a
            // Windows-internal handler (ntdll.dll!FinalExceptionHandler) at
            // its end is tested by RaiseException. If it is not present, all
            // handlers are disregarded, and the program is thus aborted
            // (see http://blogs.technet.com/b/srd/archive/2009/02/02/
            // preventing-the-exploitation-of-seh-overwrites-with-sehop.aspx).
            // For new threads, this handler is installed by Windows immediately
            // after creation. To make exception handling work in fibers, we
            // have to insert it for our new stacks manually as well.
            //
            // To do this, we first determine the handler by traversing the SEH
            // chain of the current thread until its end, and then construct a
            // registration block for the last handler on the newly created
            // thread. We then continue to push all the initial register values
            // for the first context switch as for the other implementations.
            //
            // Note that this handler is never actually invoked, as we install
            // our own one on top of it in the fiber entry point function.
            // Thus, it should not have any effects on OSes not implementing
            // exception chain verification.

            alias fp_t = void function(); // Actual signature not relevant.
            static struct EXCEPTION_REGISTRATION
            {
                EXCEPTION_REGISTRATION* next; // sehChainEnd if last one.
                fp_t handler;
            }
            enum sehChainEnd = cast(EXCEPTION_REGISTRATION*) 0xFFFFFFFF;

            __gshared static fp_t finalHandler = null;
            if ( finalHandler is null )
            {
                static EXCEPTION_REGISTRATION* fs0() nothrow
                {
                    asm pure nothrow @nogc
                    {
                        naked;
                        mov EAX, FS:[0];
                        ret;
                    }
                }
                auto reg = fs0();
                while ( reg.next != sehChainEnd ) reg = reg.next;

                // Benign races are okay here, just to avoid re-lookup on every
                // fiber creation.
                finalHandler = reg.handler;
            }

            // When linking with /safeseh (supported by LDC, but not DMD)
            // the exception chain must not extend to the very top
            // of the stack, otherwise the exception chain is also considered
            // invalid. Reserving additional 4 bytes at the top of the stack will
            // keep the EXCEPTION_REGISTRATION below that limit
            size_t reserve = EXCEPTION_REGISTRATION.sizeof + 4;
            pstack -= reserve;
            *(cast(EXCEPTION_REGISTRATION*)pstack) =
                EXCEPTION_REGISTRATION( sehChainEnd, finalHandler );

            push( cast(size_t) &fiber_entryPoint );                 // EIP
            push( cast(size_t) m_ctxt.bstack - reserve );           // EBP
            push( 0x00000000 );                                     // EDI
            push( 0x00000000 );                                     // ESI
            push( 0x00000000 );                                     // EBX
            push( cast(size_t) m_ctxt.bstack - reserve );           // FS:[0]
            push( cast(size_t) m_ctxt.bstack );                     // FS:[4]
            push( cast(size_t) m_ctxt.bstack - m_size );            // FS:[8]
            push( 0x00000000 );                                     // EAX
        }
        else version (AsmX86_64_Windows)
        {
            // Using this trampoline instead of the raw fiber_entryPoint
            // ensures that during context switches, source and destination
            // stacks have the same alignment. Otherwise, the stack would need
            // to be shifted by 8 bytes for the first call, as fiber_entryPoint
            // is an actual function expecting a stack which is not aligned
            // to 16 bytes.
            static void trampoline()
            {
                asm pure nothrow @nogc
                {
                    naked;
                    sub RSP, 32; // Shadow space (Win64 calling convention)
                    call fiber_entryPoint;
                    xor RCX, RCX; // This should never be reached, as
                    jmp RCX;      // fiber_entryPoint must never return.
                }
            }

            push( cast(size_t) &trampoline );                       // RIP
            push( 0x00000000_00000000 );                            // RBP
            push( 0x00000000_00000000 );                            // R12
            push( 0x00000000_00000000 );                            // R13
            push( 0x00000000_00000000 );                            // R14
            push( 0x00000000_00000000 );                            // R15
            push( 0x00000000_00000000 );                            // RDI
            push( 0x00000000_00000000 );                            // RSI
            push( 0x00000000_00000000 );                            // XMM6 (high)
            push( 0x00000000_00000000 );                            // XMM6 (low)
            push( 0x00000000_00000000 );                            // XMM7 (high)
            push( 0x00000000_00000000 );                            // XMM7 (low)
            push( 0x00000000_00000000 );                            // XMM8 (high)
            push( 0x00000000_00000000 );                            // XMM8 (low)
            push( 0x00000000_00000000 );                            // XMM9 (high)
            push( 0x00000000_00000000 );                            // XMM9 (low)
            push( 0x00000000_00000000 );                            // XMM10 (high)
            push( 0x00000000_00000000 );                            // XMM10 (low)
            push( 0x00000000_00000000 );                            // XMM11 (high)
            push( 0x00000000_00000000 );                            // XMM11 (low)
            push( 0x00000000_00000000 );                            // XMM12 (high)
            push( 0x00000000_00000000 );                            // XMM12 (low)
            push( 0x00000000_00000000 );                            // XMM13 (high)
            push( 0x00000000_00000000 );                            // XMM13 (low)
            push( 0x00000000_00000000 );                            // XMM14 (high)
            push( 0x00000000_00000000 );                            // XMM14 (low)
            push( 0x00000000_00000000 );                            // XMM15 (high)
            push( 0x00000000_00000000 );                            // XMM15 (low)
            push( 0x00000000_00000000 );                            // RBX
            push( 0xFFFFFFFF_FFFFFFFF );                            // GS:[0]
            version (StackGrowsDown)
            {
                push( cast(size_t) m_ctxt.bstack );                 // GS:[8]
                push( cast(size_t) m_ctxt.bstack - m_size );        // GS:[16]
            }
            else
            {
                push( cast(size_t) m_ctxt.bstack );                 // GS:[8]
                push( cast(size_t) m_ctxt.bstack + m_size );        // GS:[16]
            }
        }
        else version (AsmX86_Posix)
        {
            push( 0x00000000 );                                     // Return address of fiber_entryPoint call
            push( cast(size_t) &fiber_entryPoint );                 // EIP
            push( cast(size_t) m_ctxt.bstack );                     // EBP
            push( 0x00000000 );                                     // EDI
            push( 0x00000000 );                                     // ESI
            push( 0x00000000 );                                     // EBX
            push( 0x00000000 );                                     // EAX
        }
        else version (AsmX86_64_Posix)
        {
            push( 0x00000000_00000000 );                            // Return address of fiber_entryPoint call
            push( cast(size_t) &fiber_entryPoint );                 // RIP
            push( cast(size_t) m_ctxt.bstack );                     // RBP
            push( 0x00000000_00000000 );                            // RBX
            push( 0x00000000_00000000 );                            // R12
            push( 0x00000000_00000000 );                            // R13
            push( 0x00000000_00000000 );                            // R14
            push( 0x00000000_00000000 );                            // R15
        }
        else version (AsmPPC_Posix)
        {
            version (StackGrowsDown)
            {
                pstack -= int.sizeof * 5;
            }
            else
            {
                pstack += int.sizeof * 5;
            }

            push( cast(size_t) &fiber_entryPoint );     // link register
            push( 0x00000000 );                         // control register
            push( 0x00000000 );                         // old stack pointer

            // GPR values
            version (StackGrowsDown)
            {
                pstack -= int.sizeof * 20;
            }
            else
            {
                pstack += int.sizeof * 20;
            }

            assert( (cast(size_t) pstack & 0x0f) == 0 );
        }
        else version (AsmMIPS_O32_Posix)
        {
            version (StackGrowsDown) {}
            else static assert(0);

            /* We keep the FP registers and the return address below
             * the stack pointer, so they don't get scanned by the
             * GC. The last frame before swapping the stack pointer is
             * organized like the following.
             *
             *     |-----------|<= frame pointer
             *     |    $gp    |
             *     |   $s0-8   |
             *     |-----------|<= stack pointer
             *     |    $ra    |
             *     |  align(8) |
             *     |  $f20-30  |
             *     |-----------|
             *
             */
            enum SZ_GP = 10 * size_t.sizeof; // $gp + $s0-8
            enum SZ_RA = size_t.sizeof;      // $ra
            version (MIPS_HardFloat)
            {
                enum SZ_FP = 6 * 8;          // $f20-30
                enum ALIGN = -(SZ_FP + SZ_RA) & (8 - 1);
            }
            else
            {
                enum SZ_FP = 0;
                enum ALIGN = 0;
            }

            enum BELOW = SZ_FP + ALIGN + SZ_RA;
            enum ABOVE = SZ_GP;
            enum SZ = BELOW + ABOVE;

            (cast(ubyte*)pstack - SZ)[0 .. SZ] = 0;
            pstack -= ABOVE;
            *cast(size_t*)(pstack - SZ_RA) = cast(size_t)&fiber_entryPoint;
        }
        else version (AsmAArch64_Posix)
        {
            // Like others, FP registers and return address (lr) are kept
            // below the saved stack top (tstack) to hide from GC scanning.
            // fiber_switchContext expects newp sp to look like this:
            //   19: x19
            //   ...
            //    9: x29 (fp)  <-- newp tstack
            //    8: x30 (lr)  [&fiber_entryPoint]
            //    7: d8
            //   ...
            //    0: d15

            version (StackGrowsDown) {}
            else
                static assert(false, "Only full descending stacks supported on AArch64");

            // Only need to set return address (lr).  Everything else is fine
            // zero initialized.
            pstack -= size_t.sizeof * 11;    // skip past x19-x29
            push(cast(size_t) &fiber_trampoline); // see threadasm.S for docs
            pstack += size_t.sizeof;         // adjust sp (newp) above lr
        }
        else version (AsmARM_Posix)
        {
            /* We keep the FP registers and the return address below
             * the stack pointer, so they don't get scanned by the
             * GC. The last frame before swapping the stack pointer is
             * organized like the following.
             *
             *   |  |-----------|<= 'frame starts here'
             *   |  |     fp    | (the actual frame pointer, r11 isn't
             *   |  |   r10-r4  |  updated and still points to the previous frame)
             *   |  |-----------|<= stack pointer
             *   |  |     lr    |
             *   |  | 4byte pad |
             *   |  |   d15-d8  |(if FP supported)
             *   |  |-----------|
             *   Y
             *   stack grows down: The pointer value here is smaller than some lines above
             */
            // frame pointer can be zero, r10-r4 also zero initialized
            version (StackGrowsDown)
                pstack -= int.sizeof * 8;
            else
                static assert(false, "Only full descending stacks supported on ARM");

            // link register
            push( cast(size_t) &fiber_entryPoint );
            /*
             * We do not push padding and d15-d8 as those are zero initialized anyway
             * Position the stack pointer above the lr register
             */
            pstack += int.sizeof * 1;
        }
        else version (GNU_AsmX86_Windows)
        {
            version (StackGrowsDown) {} else static assert( false );

            // Currently, MinGW doesn't utilize SEH exceptions.
            // See DMD AsmX86_Windows If this code ever becomes fails and SEH is used.

            push( 0x00000000 );                                     // Return address of fiber_entryPoint call
            push( cast(size_t) &fiber_entryPoint );                 // EIP
            push( 0x00000000 );                                     // EBP
            push( 0x00000000 );                                     // EDI
            push( 0x00000000 );                                     // ESI
            push( 0x00000000 );                                     // EBX
            push( 0xFFFFFFFF );                                     // FS:[0] - Current SEH frame
            push( cast(size_t) m_ctxt.bstack );                     // FS:[4] - Top of stack
            push( cast(size_t) m_ctxt.bstack - m_size );            // FS:[8] - Bottom of stack
            push( 0x00000000 );                                     // EAX
        }
        else version (GNU_AsmX86_64_Windows)
        {
            push( 0x00000000_00000000 );                            // Return address of fiber_entryPoint call
            push( cast(size_t) &fiber_entryPoint );                 // RIP
            push( 0x00000000_00000000 );                            // RBP
            push( 0x00000000_00000000 );                            // RBX
            push( 0x00000000_00000000 );                            // R12
            push( 0x00000000_00000000 );                            // R13
            push( 0x00000000_00000000 );                            // R14
            push( 0x00000000_00000000 );                            // R15
            push( 0xFFFFFFFF_FFFFFFFF );                            // GS:[0] - Current SEH frame
            version (StackGrowsDown)
            {
                push( cast(size_t) m_ctxt.bstack );                 // GS:[8]  - Top of stack
                push( cast(size_t) m_ctxt.bstack - m_size );        // GS:[16] - Bottom of stack
            }
            else
            {
                push( cast(size_t) m_ctxt.bstack );                 // GS:[8]  - Top of stack
                push( cast(size_t) m_ctxt.bstack + m_size );        // GS:[16] - Bottom of stack
            }
        }
        else static if ( __traits( compiles, ucontext_t ) )
        {
            getcontext( &m_utxt );
            m_utxt.uc_stack.ss_sp   = m_pmem;
            m_utxt.uc_stack.ss_size = m_size;
            makecontext( &m_utxt, &fiber_entryPoint, 0 );
            // NOTE: If ucontext is being used then the top of the stack will
            //       be a pointer to the ucontext_t struct for that fiber.
            push( cast(size_t) &m_utxt );
        }
        else
            static assert(0, "Not implemented");
    }


    Thread.Context* m_ctxt;
    size_t          m_size;
    void*           m_pmem;

    static if ( __traits( compiles, ucontext_t ) )
    {
        // NOTE: The static ucontext instance is used to represent the context
        //       of the executing thread.
        static ucontext_t       sm_utxt = void;
        ucontext_t              m_utxt  = void;
        ucontext_t*             m_ucur  = null;
    }


private:
    ///////////////////////////////////////////////////////////////////////////
    // Storage of Active Fiber
    ///////////////////////////////////////////////////////////////////////////


    //
    // Sets a thread-local reference to the current fiber object.
    //
    static void setThis( Fiber f ) nothrow @nogc
    {
        sm_this = f;
    }

    static Fiber sm_this;


private:
    ///////////////////////////////////////////////////////////////////////////
    // Context Switching
    ///////////////////////////////////////////////////////////////////////////


    //
    // Switches into the stack held by this fiber.
    //
    final void switchIn() nothrow @nogc
    {
        Thread  tobj = Thread.getThis();
        void**  oldp = &tobj.m_curr.tstack;
        void*   newp = m_ctxt.tstack;

        // NOTE: The order of operations here is very important.  The current
        //       stack top must be stored before m_lock is set, and pushContext
        //       must not be called until after m_lock is set.  This process
        //       is intended to prevent a race condition with the suspend
        //       mechanism used for garbage collection.  If it is not followed,
        //       a badly timed collection could cause the GC to scan from the
        //       bottom of one stack to the top of another, or to miss scanning
        //       a stack that still contains valid data.  The old stack pointer
        //       oldp will be set again before the context switch to guarantee
        //       that it points to exactly the correct stack location so the
        //       successive pop operations will succeed.
        *oldp = getStackTop();
        atomicStore!(MemoryOrder.raw)(*cast(shared)&tobj.m_lock, true);
        tobj.pushContext( m_ctxt );

        fiber_switchContext( oldp, newp );

        // NOTE: As above, these operations must be performed in a strict order
        //       to prevent Bad Things from happening.
        tobj.popContext();
        atomicStore!(MemoryOrder.raw)(*cast(shared)&tobj.m_lock, false);
        tobj.m_curr.tstack = tobj.m_curr.bstack;
    }


    //
    // Switches out of the current stack and into the enclosing stack.
    //
    final void switchOut() nothrow @nogc
    {
        Thread  tobj = Thread.getThis();
        void**  oldp = &m_ctxt.tstack;
        void*   newp = tobj.m_curr.within.tstack;

        // NOTE: The order of operations here is very important.  The current
        //       stack top must be stored before m_lock is set, and pushContext
        //       must not be called until after m_lock is set.  This process
        //       is intended to prevent a race condition with the suspend
        //       mechanism used for garbage collection.  If it is not followed,
        //       a badly timed collection could cause the GC to scan from the
        //       bottom of one stack to the top of another, or to miss scanning
        //       a stack that still contains valid data.  The old stack pointer
        //       oldp will be set again before the context switch to guarantee
        //       that it points to exactly the correct stack location so the
        //       successive pop operations will succeed.
        *oldp = getStackTop();
        atomicStore!(MemoryOrder.raw)(*cast(shared)&tobj.m_lock, true);

        fiber_switchContext( oldp, newp );

        // NOTE: As above, these operations must be performed in a strict order
        //       to prevent Bad Things from happening.
        // NOTE: If use of this fiber is multiplexed across threads, the thread
        //       executing here may be different from the one above, so get the
        //       current thread handle before unlocking, etc.
        tobj = Thread.getThis();
        atomicStore!(MemoryOrder.raw)(*cast(shared)&tobj.m_lock, false);
        tobj.m_curr.tstack = tobj.m_curr.bstack;
    }
}


version (unittest)
{
    class TestFiber : Fiber
    {
        this()
        {
            super(&run);
        }

        void run()
        {
            foreach (i; 0 .. 1000)
            {
                sum += i;
                Fiber.yield();
            }
        }

        enum expSum = 1000 * 999 / 2;
        size_t sum;
    }

    void runTen()
    {
        TestFiber[10] fibs;
        foreach (ref fib; fibs)
            fib = new TestFiber();

        bool cont;
        do {
            cont = false;
            foreach (fib; fibs) {
                if (fib.state == Fiber.State.HOLD)
                {
                    fib.call();
                    cont |= fib.state != Fiber.State.TERM;
                }
            }
        } while (cont);

        foreach (fib; fibs)
        {
            assert(fib.sum == TestFiber.expSum);
        }
    }
}


// Single thread running separate fibers
unittest
{
    runTen();
}


// Multiple threads running separate fibers
unittest
{
    auto group = new ThreadGroup();
    foreach (_; 0 .. 4)
    {
        group.create(&runTen);
    }
    group.joinAll();
}


// Multiple threads running shared fibers
version (PPC)   version = UnsafeFiberMigration;
version (PPC64) version = UnsafeFiberMigration;

version (UnsafeFiberMigration)
{
    // XBUG: core.thread fibers are supposed to be safe to migrate across
    // threads, however, there is a problem: GCC always assumes that the
    // address of thread-local variables don't change while on a given stack.
    // In consequence, migrating fibers between threads currently is an unsafe
    // thing to do, and will break on some targets (possibly PR26461).
}
else
{
    version = FiberMigrationUnittest;
}

version (FiberMigrationUnittest)
unittest
{
    shared bool[10] locks;
    TestFiber[10] fibs;

    void runShared()
    {
        bool cont;
        do {
            cont = false;
            foreach (idx; 0 .. 10)
            {
                if (cas(&locks[idx], false, true))
                {
                    if (fibs[idx].state == Fiber.State.HOLD)
                    {
                        fibs[idx].call();
                        cont |= fibs[idx].state != Fiber.State.TERM;
                    }
                    locks[idx] = false;
                }
                else
                {
                    cont = true;
                }
            }
        } while (cont);
    }

    foreach (ref fib; fibs)
    {
        fib = new TestFiber();
    }

    auto group = new ThreadGroup();
    foreach (_; 0 .. 4)
    {
        group.create(&runShared);
    }
    group.joinAll();

    foreach (fib; fibs)
    {
        assert(fib.sum == TestFiber.expSum);
    }
}


// Test exception handling inside fibers.
version (Win32) {
    // broken on win32 under windows server 2012: bug 13821
} else unittest {
    enum MSG = "Test message.";
    string caughtMsg;
    (new Fiber({
        try
        {
            throw new Exception(MSG);
        }
        catch (Exception e)
        {
            caughtMsg = e.msg;
        }
    })).call();
    assert(caughtMsg == MSG);
}


unittest
{
    int x = 0;

    (new Fiber({
        x++;
    })).call();
    assert( x == 1 );
}

nothrow unittest
{
    new Fiber({}).call!(Fiber.Rethrow.no)();
}

unittest
{
    new Fiber({}).call(Fiber.Rethrow.yes);
    new Fiber({}).call(Fiber.Rethrow.no);
}

deprecated unittest
{
    new Fiber({}).call(true);
    new Fiber({}).call(false);
}

version (Win32) {
    // broken on win32 under windows server 2012: bug 13821
} else unittest {
    enum MSG = "Test message.";

    try
    {
        (new Fiber({
            throw new Exception( MSG );
        })).call();
        assert( false, "Expected rethrown exception." );
    }
    catch ( Throwable t )
    {
        assert( t.msg == MSG );
    }
}

// Test exception chaining when switching contexts in finally blocks.
unittest
{
    static void throwAndYield(string msg) {
      try {
        throw new Exception(msg);
      } finally {
        Fiber.yield();
      }
    }

    static void fiber(string name) {
      try {
        try {
          throwAndYield(name ~ ".1");
        } finally {
          throwAndYield(name ~ ".2");
        }
      } catch (Exception e) {
        assert(e.msg == name ~ ".1");
        assert(e.next);
        assert(e.next.msg == name ~ ".2");
        assert(!e.next.next);
      }
    }

    auto first = new Fiber(() => fiber("first"));
    auto second = new Fiber(() => fiber("second"));
    first.call();
    second.call();
    first.call();
    second.call();
    first.call();
    second.call();
    assert(first.state == Fiber.State.TERM);
    assert(second.state == Fiber.State.TERM);
}

// Test Fiber resetting
unittest
{
    static string method;

    static void foo()
    {
        method = "foo";
    }

    void bar()
    {
        method = "bar";
    }

    static void expect(Fiber fib, string s)
    {
        assert(fib.state == Fiber.State.HOLD);
        fib.call();
        assert(fib.state == Fiber.State.TERM);
        assert(method == s); method = null;
    }
    auto fib = new Fiber(&foo);
    expect(fib, "foo");

    fib.reset();
    expect(fib, "foo");

    fib.reset(&foo);
    expect(fib, "foo");

    fib.reset(&bar);
    expect(fib, "bar");

    fib.reset(function void(){method = "function";});
    expect(fib, "function");

    fib.reset(delegate void(){method = "delegate";});
    expect(fib, "delegate");
}

// Test unsafe reset in hold state
unittest
{
    auto fib = new Fiber(function {ubyte[2048] buf = void; Fiber.yield();}, 4096);
    foreach (_; 0 .. 10)
    {
        fib.call();
        assert(fib.state == Fiber.State.HOLD);
        fib.reset();
    }
}

// stress testing GC stack scanning
unittest
{
    import core.memory;

    static void unreferencedThreadObject()
    {
        static void sleep() { Thread.sleep(dur!"msecs"(100)); }
        auto thread = new Thread(&sleep).start();
    }
    unreferencedThreadObject();
    GC.collect();

    static class Foo
    {
        this(int value)
        {
            _value = value;
        }

        int bar()
        {
            return _value;
        }

        int _value;
    }

    static void collect()
    {
        auto foo = new Foo(2);
        assert(foo.bar() == 2);
        GC.collect();
        Fiber.yield();
        GC.collect();
        assert(foo.bar() == 2);
    }

    auto fiber = new Fiber(&collect);

    fiber.call();
    GC.collect();
    fiber.call();

    // thread reference
    auto foo = new Foo(2);

    void collect2()
    {
        assert(foo.bar() == 2);
        GC.collect();
        Fiber.yield();
        GC.collect();
        assert(foo.bar() == 2);
    }

    fiber = new Fiber(&collect2);

    fiber.call();
    GC.collect();
    fiber.call();

    static void recurse(size_t cnt)
    {
        --cnt;
        Fiber.yield();
        if (cnt)
        {
            auto fib = new Fiber(() { recurse(cnt); });
            fib.call();
            GC.collect();
            fib.call();
        }
    }
    fiber = new Fiber(() { recurse(20); });
    fiber.call();
}


version (AsmX86_64_Windows)
{
    // Test Windows x64 calling convention
    unittest
    {
        void testNonvolatileRegister(alias REG)()
        {
            auto zeroRegister = new Fiber(() {
                mixin("asm pure nothrow @nogc { naked; xor "~REG~", "~REG~"; ret; }");
            });
            long after;

            mixin("asm pure nothrow @nogc { mov "~REG~", 0xFFFFFFFFFFFFFFFF; }");
            zeroRegister.call();
            mixin("asm pure nothrow @nogc { mov after, "~REG~"; }");

            assert(after == -1);
        }

        void testNonvolatileRegisterSSE(alias REG)()
        {
            auto zeroRegister = new Fiber(() {
                mixin("asm pure nothrow @nogc { naked; xorpd "~REG~", "~REG~"; ret; }");
            });
            long[2] before = [0xFFFFFFFF_FFFFFFFF, 0xFFFFFFFF_FFFFFFFF], after;

            mixin("asm pure nothrow @nogc { movdqu "~REG~", before; }");
            zeroRegister.call();
            mixin("asm pure nothrow @nogc { movdqu after, "~REG~"; }");

            assert(before == after);
        }

        testNonvolatileRegister!("R12")();
        testNonvolatileRegister!("R13")();
        testNonvolatileRegister!("R14")();
        testNonvolatileRegister!("R15")();
        testNonvolatileRegister!("RDI")();
        testNonvolatileRegister!("RSI")();
        testNonvolatileRegister!("RBX")();

        testNonvolatileRegisterSSE!("XMM6")();
        testNonvolatileRegisterSSE!("XMM7")();
        testNonvolatileRegisterSSE!("XMM8")();
        testNonvolatileRegisterSSE!("XMM9")();
        testNonvolatileRegisterSSE!("XMM10")();
        testNonvolatileRegisterSSE!("XMM11")();
        testNonvolatileRegisterSSE!("XMM12")();
        testNonvolatileRegisterSSE!("XMM13")();
        testNonvolatileRegisterSSE!("XMM14")();
        testNonvolatileRegisterSSE!("XMM15")();
    }
}


version (D_InlineAsm_X86_64)
{
    unittest
    {
        void testStackAlignment()
        {
            void* pRSP;
            asm pure nothrow @nogc
            {
                mov pRSP, RSP;
            }
            assert((cast(size_t)pRSP & 0xF) == 0);
        }

        auto fib = new Fiber(&testStackAlignment);
        fib.call();
    }
}

// regression test for Issue 13416
version (FreeBSD) unittest
{
    static void loop()
    {
        pthread_attr_t attr;
        pthread_attr_init(&attr);
        auto thr = pthread_self();
        foreach (i; 0 .. 50)
            pthread_attr_get_np(thr, &attr);
        pthread_attr_destroy(&attr);
    }

    auto thr = new Thread(&loop).start();
    foreach (i; 0 .. 50)
    {
        thread_suspendAll();
        thread_resumeAll();
    }
    thr.join();
}

version (DragonFlyBSD) unittest
{
    static void loop()
    {
        pthread_attr_t attr;
        pthread_attr_init(&attr);
        auto thr = pthread_self();
        foreach (i; 0 .. 50)
            pthread_attr_get_np(thr, &attr);
        pthread_attr_destroy(&attr);
    }

    auto thr = new Thread(&loop).start();
    foreach (i; 0 .. 50)
    {
        thread_suspendAll();
        thread_resumeAll();
    }
    thr.join();
}

unittest
{
    // use >PAGESIZE to avoid stack overflow (e.g. in an syscall)
    auto thr = new Thread(function{}, 4096 + 1).start();
    thr.join();
}

/**
 * Represents the ID of a thread, as returned by $(D Thread.)$(LREF id).
 * The exact type varies from platform to platform.
 */
version (Windows)
    alias ThreadID = uint;
else
version (Posix)
    alias ThreadID = pthread_t;

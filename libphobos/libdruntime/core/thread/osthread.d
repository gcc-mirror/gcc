/**
 * The osthread module provides low-level, OS-dependent code
 * for thread creation and management.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2012.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly, Walter Bright, Alex Rønne Petersen, Martin Nowak
 * Source:    $(DRUNTIMESRC core/thread/osthread.d)
 */

/* NOTE: This file has been patched from the original DMD distribution to
 * work with the GDC compiler.
 */
module core.thread.osthread;

import core.thread.threadbase;
import core.thread.context;
import core.thread.types;
import core.atomic;
import core.memory : GC, pageSize;
import core.time;
import core.exception : onOutOfMemoryError;
import core.internal.traits : externDFunc;


///////////////////////////////////////////////////////////////////////////////
// Platform Detection and Memory Allocation
///////////////////////////////////////////////////////////////////////////////

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Shared)
    version (GNU)
        version = GNUShared;

version (D_InlineAsm_X86)
{
    version (Windows)
        version = AsmX86_Windows;
    else version (Posix)
        version = AsmX86_Posix;
}
else version (D_InlineAsm_X86_64)
{
    version (Windows)
    {
        version = AsmX86_64_Windows;
    }
    else version (Posix)
    {
        version = AsmX86_64_Posix;
    }
}
else version (X86)
{
    version (CET) {} else
    {
        version = AsmExternal;
    }
}
else version (X86_64)
{
    version (CET)   {} else
    version (D_X32) {} else
    {
        version = AsmExternal;
    }
}
else version (PPC)
{
    version (Posix)
    {
        version = AsmExternal;
    }
}
else version (MIPS_O32)
{
    version (Posix)
    {
        version = AsmExternal;
    }
}
else version (AArch64)
{
    version (Posix)
    {
        version = AsmExternal;
    }
}
else version (ARM)
{
    version (Posix)
    {
        version = AsmExternal;
    }
}

version (Posix)
{
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

version (Windows)
{
    import core.stdc.stdint : uintptr_t; // for _beginthreadex decl below
    import core.stdc.stdlib;             // for malloc, atexit
    import core.sys.windows.basetsd /+: HANDLE+/;
    import core.sys.windows.threadaux /+: getThreadStackBottom, impersonate_thread, OpenThreadHandle+/;
    import core.sys.windows.winbase /+: CloseHandle, CREATE_SUSPENDED, DuplicateHandle, GetCurrentThread,
        GetCurrentThreadId, GetCurrentProcess, GetExitCodeThread, GetSystemInfo, GetThreadContext,
        GetThreadPriority, INFINITE, ResumeThread, SetThreadPriority, Sleep,  STILL_ACTIVE,
        SuspendThread, SwitchToThread, SYSTEM_INFO, THREAD_PRIORITY_IDLE, THREAD_PRIORITY_NORMAL,
        THREAD_PRIORITY_TIME_CRITICAL, WAIT_OBJECT_0, WaitForSingleObject+/;
    import core.sys.windows.windef /+: TRUE+/;
    import core.sys.windows.winnt /+: CONTEXT, CONTEXT_CONTROL, CONTEXT_INTEGER+/;

    private extern (Windows) alias btex_fptr = uint function(void*);
    private extern (C) uintptr_t _beginthreadex(void*, uint, btex_fptr, void*, uint, uint*) nothrow @nogc;
}
else version (Posix)
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
}

version (Solaris)
{
    import core.sys.solaris.sys.priocntl;
    import core.sys.solaris.sys.types;
    import core.sys.posix.sys.wait : idtype_t;
}

version (GNU)
{
    import gcc.builtins;
}

/**
 * Hook for whatever EH implementation is used to save/restore some data
 * per stack.
 *
 * Params:
 *     newContext = The return value of the prior call to this function
 *         where the stack was last swapped out, or null when a fiber stack
 *         is switched in for the first time.
 */
private extern(C) void* _d_eh_swapContext(void* newContext) nothrow @nogc;

version (DigitalMars)
{
    version (Windows)
    {
        extern(D) void* swapContext(void* newContext) nothrow @nogc
        {
            return _d_eh_swapContext(newContext);
        }
    }
    else
    {
        extern(C) void* _d_eh_swapContextDwarf(void* newContext) nothrow @nogc;

        extern(D) void* swapContext(void* newContext) nothrow @nogc
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
{
    extern(D) void* swapContext(void* newContext) nothrow @nogc
    {
        return _d_eh_swapContext(newContext);
    }
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
class Thread : ThreadBase
{
    //
    // Standard thread data
    //
    version (Windows)
    {
        private HANDLE          m_hndl;
    }

    version (Posix)
    {
        private shared bool     m_isRunning;
    }

    version (Darwin)
    {
        private mach_port_t     m_tmach;
    }

    version (Solaris)
    {
        private __gshared bool m_isRTClass;
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
    {
        super(fn, sz);
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
    {
        super(dg, sz);
    }

    package this( size_t sz = 0 ) @safe pure nothrow @nogc
    {
        super(sz);
    }

    /**
     * Cleans up any remaining resources used by this object.
     */
    ~this() nothrow @nogc
    {
        if (super.destructBeforeDtor())
            return;

        version (Windows)
        {
            m_addr = m_addr.init;
            CloseHandle( m_hndl );
            m_hndl = m_hndl.init;
        }
        else version (Posix)
        {
            if (m_addr != m_addr.init)
                pthread_detach( m_addr );
            m_addr = m_addr.init;
        }
        version (Darwin)
        {
            m_tmach = m_tmach.init;
        }
    }

    //
    // Thread entry point.  Invokes the function or delegate passed on
    // construction (if any).
    //
    private final void run()
    {
        super.run();
    }

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
        return ThreadBase.getThis().toThread;
    }

    ///////////////////////////////////////////////////////////////////////////
    // Thread Context and GC Scanning Support
    ///////////////////////////////////////////////////////////////////////////


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
        else version (AArch64)
        {
            ulong[33]       m_reg; // x0-x31, pc
        }
        else version (ARM)
        {
            uint[16]        m_reg; // r0-r15
        }
        else version (PPC)
        {
            // Make the assumption that we only care about non-fp and non-vr regs.
            // ??? : it seems plausible that a valid address can be copied into a VR.
            uint[32]        m_reg; // r0-31
        }
        else version (PPC64)
        {
            // As above.
            ulong[32]       m_reg; // r0-31
        }
        else
        {
            static assert(false, "Architecture not supported." );
        }
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
    do
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
            size_t stksz = adjustStackSize( m_sz );

            pthread_attr_t  attr;

            if ( pthread_attr_init( &attr ) )
                onThreadError( "Error initializing thread attributes" );
            if ( stksz && pthread_attr_setstacksize( &attr, stksz ) )
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
            incrementAboutToStart(this);

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
                    version (GNU)
                    {
                        auto libs = externDFunc!("gcc.sections.pinLoadedLibraries",
                                                 void* function() @nogc nothrow)();
                    }
                    else
                    {
                        auto libs = externDFunc!("rt.sections_elf_shared.pinLoadedLibraries",
                                                 void* function() @nogc nothrow)();
                    }

                    auto ps = cast(void**).malloc(2 * size_t.sizeof);
                    if (ps is null) onOutOfMemoryError();
                    ps[0] = cast(void*)this;
                    ps[1] = cast(void*)libs;
                    if ( pthread_create( &m_addr, &attr, &thread_entryPoint, ps ) != 0 )
                    {
                        version (GNU)
                        {
                            externDFunc!("gcc.sections.unpinLoadedLibraries",
                                         void function(void*) @nogc nothrow)(libs);
                        }
                        else
                        {
                            externDFunc!("rt.sections_elf_shared.unpinLoadedLibraries",
                                         void function(void*) @nogc nothrow)(libs);
                        }
                        .free(ps);
                        onThreadError( "Error creating thread" );
                    }
                }
                else
                {
                    if ( pthread_create( &m_addr, &attr, &thread_entryPoint, cast(void*) this ) != 0 )
                        onThreadError( "Error creating thread" );
                }
                if ( pthread_attr_destroy( &attr ) != 0 )
                    onThreadError( "Error destroying thread attributes" );
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
    override final Throwable join( bool rethrow = true )
    {
        version (Windows)
        {
            if ( m_addr != m_addr.init && WaitForSingleObject( m_hndl, INFINITE ) != WAIT_OBJECT_0 )
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
            if ( m_addr != m_addr.init && pthread_join( m_addr, null ) != 0 )
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
        private static shared Priority cache;
        private static int loadGlobal(string which)()
        {
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
                    result.PRIORITY_MIN = -cast(int)(clinfo[0]);
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
    do
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

    /**
     * Tests whether this thread is running.
     *
     * Returns:
     *  true if the thread is running, false if not.
     */
    override final @property bool isRunning() nothrow @nogc
    {
        if (!super.isRunning())
            return false;

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
    do
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
}

private Thread toThread(return scope ThreadBase t) @trusted nothrow @nogc pure
{
    return cast(Thread) cast(void*) t;
}

private extern(D) static void thread_yield() @nogc nothrow
{
    Thread.yield();
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
        function()
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


unittest
{
    // use >pageSize to avoid stack overflow (e.g. in an syscall)
    auto thr = new Thread(function{}, 4096 + 1).start();
    thr.join();
}


unittest
{
    import core.memory : GC;

    auto t1 = new Thread({
        foreach (_; 0 .. 20)
            ThreadBase.getAll;
    }).start;
    auto t2 = new Thread({
        foreach (_; 0 .. 20)
            GC.collect;
    }).start;
    t1.join();
    t2.join();
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
    synchronized (ThreadBase.criticalRegionLock)
        assert(thr.m_isInCriticalRegion);
    semb.notify();

    sema.wait();
    synchronized (ThreadBase.criticalRegionLock)
        assert(!thr.m_isInCriticalRegion);
    semb.notify();

    thr.join();
}

// https://issues.dlang.org/show_bug.cgi?id=22124
unittest
{
    Thread thread = new Thread({});
    auto fun(Thread t, int x)
    {
        t.__ctor({x = 3;});
        return t;
    }
    static assert(!__traits(compiles, () @nogc => fun(thread, 3) ));
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

///////////////////////////////////////////////////////////////////////////////
// GC Support Routines
///////////////////////////////////////////////////////////////////////////////

version (CoreDdoc)
{
    /**
     * Instruct the thread module, when initialized, to use a different set of
     * signals besides SIGRTMIN and SIGRTMIN + 1 for suspension and resumption of threads.
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
        assert(suspendSignalNo != 0);
        assert(resumeSignalNo  != 0);
    }
    out
    {
        assert(suspendSignalNumber != 0);
        assert(resumeSignalNumber  != 0);
    }
    do
    {
        suspendSignalNumber = suspendSignalNo;
        resumeSignalNumber  = resumeSignalNo;
    }
}

version (Posix)
{
    private __gshared int suspendSignalNumber;
    private __gshared int resumeSignalNumber;
}

private extern (D) ThreadBase attachThread(ThreadBase _thisThread) @nogc nothrow
{
    Thread thisThread = _thisThread.toThread();

    StackContext* thisContext = &thisThread.m_main;
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

        atomicStore!(MemoryOrder.raw)(thisThread.toThread.m_isRunning, true);
    }
    thisThread.m_isDaemon = true;
    thisThread.tlsGCdataInit();
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

/**
 * Registers the calling thread for use with the D Runtime.  If this routine
 * is called for a thread which is already registered, no action is performed.
 *
 * NOTE: This routine does not run thread-local static constructors when called.
 *       If full functionality as a D thread is desired, the following function
 *       must be called after thread_attachThis:
 *
 *       extern (C) void rt_moduleTlsCtor();
 *
 * See_Also:
 *     $(REF thread_detachThis, core,thread,threadbase)
 */
extern(C) Thread thread_attachThis()
{
    return thread_attachThis_tpl!Thread();
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

        if (auto t = thread_findByAddr(addr).toThread)
            return t;

        Thread        thisThread  = new Thread();
        StackContext* thisContext = &thisThread.m_main;
        assert( thisContext == thisThread.m_curr );

        thisThread.m_addr  = addr;
        thisContext.bstack = bstack;
        thisContext.tstack = thisContext.bstack;

        thisThread.m_isDaemon = true;

        if ( addr == GetCurrentThreadId() )
        {
            thisThread.m_hndl = GetCurrentThreadHandle();
            thisThread.tlsGCdataInit();
            Thread.setThis( thisThread );
        }
        else
        {
            thisThread.m_hndl = OpenThreadHandle( addr );
            impersonate_thread(addr,
            {
                thisThread.tlsGCdataInit();
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


// Calls the given delegate, passing the current thread's stack pointer to it.
package extern(D) void callWithStackShell(scope callWithStackShellDg fn) nothrow
in (fn)
{
    // The purpose of the 'shell' is to ensure all the registers get
    // put on the stack so they'll be scanned. We only need to push
    // the callee-save registers.
    void *sp = void;
    version (GNU)
    {
        // The generic solution below using a call to __builtin_unwind_init ()
        // followed by an assignment to sp has two issues:
        // 1) On some archs it stores a huge amount of FP and Vector state which
        //    is not the subject of the scan - and, indeed might produce false
        //    hits.
        // 2) Even on archs like X86, where there are no callee-saved FPRs/VRs there
        //    tend to be 'holes' in the frame allocations (to deal with alignment) which
        //    also will  contain random data which could produce false positives.
        // This solution stores only the integer callee-saved registers.
        version (X86)
        {
            void*[3] regs = void;
            asm pure nothrow @nogc
            {
                "movl   %%ebx, %0" : "=m" (regs[0]);
                "movl   %%esi, %0" : "=m" (regs[1]);
                "movl   %%edi, %0" : "=m" (regs[2]);
            }
            sp = cast(void*)&regs[0];
        }
        else version (X86_64)
        {
            void*[5] regs = void;
            asm pure nothrow @nogc
            {
                "movq   %%rbx, %0" : "=m" (regs[0]);
                "movq   %%r12, %0" : "=m" (regs[1]);
                "movq   %%r13, %0" : "=m" (regs[2]);
                "movq   %%r14, %0" : "=m" (regs[3]);
                "movq   %%r15, %0" : "=m" (regs[4]);
            }
            sp = cast(void*)&regs[0];
        }
        else version (PPC)
        {
            void*[19] regs = void;
            version (Darwin)
                enum regname = "r";
            else
                enum regname = "";
            static foreach (i; 0 .. regs.length)
            {{
                enum int j = 13 + i; // source register
                asm pure nothrow @nogc
                {
                    "stw "~regname~j.stringof~", %0" : "=m" (regs[i]);
                }
            }}
            sp = cast(void*)&regs[0];
        }
        else version (PPC64)
        {
            void*[19] regs = void;
            version (Darwin)
                enum regname = "r";
            else
                enum regname = "";
            static foreach (i; 0 .. regs.length)
            {{
                enum int j = 13 + i; // source register
                asm pure nothrow @nogc
                {
                    "std "~regname~j.stringof~", %0" : "=m" (regs[i]);
                }
            }}
            sp = cast(void*)&regs[0];
        }
        else version (AArch64)
        {
            // Callee-save registers, x19-x28 according to AAPCS64, section
            // 5.1.1.  Include x29 fp because it optionally can be a callee
            // saved reg
            size_t[11] regs = void;
            // store the registers in pairs
            asm pure nothrow @nogc
            {
                "stp x19, x20, %0" : "=m" (regs[ 0]), "=m" (regs[1]);
                "stp x21, x22, %0" : "=m" (regs[ 2]), "=m" (regs[3]);
                "stp x23, x24, %0" : "=m" (regs[ 4]), "=m" (regs[5]);
                "stp x25, x26, %0" : "=m" (regs[ 6]), "=m" (regs[7]);
                "stp x27, x28, %0" : "=m" (regs[ 8]), "=m" (regs[9]);
                "str x29, %0"      : "=m" (regs[10]);
                "mov %0, sp"       : "=r" (sp);
            }
        }
        else version (ARM)
        {
            // Callee-save registers, according to AAPCS, section 5.1.1.
            // arm and thumb2 instructions
            size_t[8] regs = void;
            asm pure nothrow @nogc
            {
                "stm %0, {r4-r11}" : : "r" (regs.ptr) : "memory";
                "mov %0, sp"       : "=r" (sp);
            }
        }
        else
        {
            __builtin_unwind_init();
            sp = &sp;
        }
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

version (Windows)
private extern (D) void scanWindowsOnly(scope ScanAllThreadsTypeFn scan, ThreadBase _t) nothrow
{
    auto t = _t.toThread;

    scan( ScanType.stack, t.m_reg.ptr, t.m_reg.ptr + t.m_reg.length );
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
    import core.sys.posix.unistd;

    alias getpid = core.sys.posix.unistd.getpid;
}
else version (Windows)
{
    alias getpid = core.sys.windows.winbase.GetCurrentProcessId;
}

extern (C) @nogc nothrow
{
    version (CRuntime_Glibc)  version = PThread_Getattr_NP;
    version (CRuntime_Bionic) version = PThread_Getattr_NP;
    version (CRuntime_Musl)   version = PThread_Getattr_NP;
    version (CRuntime_UClibc) version = PThread_Getattr_NP;

    version (FreeBSD)         version = PThread_Attr_Get_NP;
    version (NetBSD)          version = PThread_Attr_Get_NP;
    version (DragonFlyBSD)    version = PThread_Attr_Get_NP;

    version (PThread_Getattr_NP)  int pthread_getattr_np(pthread_t thread, pthread_attr_t* attr);
    version (PThread_Attr_Get_NP) int pthread_attr_get_np(pthread_t thread, pthread_attr_t* attr);
    version (Solaris) int thr_stksegment(stack_t* stk);
    version (OpenBSD) int pthread_stackseg_np(pthread_t thread, stack_t* sinfo);
}


private extern(D) void* getStackTop() nothrow @nogc
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


private extern(D) void* getStackBottom() nothrow @nogc
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
                asm pure nothrow @nogc { "movl %%fs:4, %0;" : "=r" (bottom); }
            else version (X86_64)
                asm pure nothrow @nogc { "movq %%gs:8, %0;" : "=r" (bottom); }
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
    else version (PThread_Getattr_NP)
    {
        pthread_attr_t attr;
        void* addr; size_t size;

        pthread_attr_init(&attr);
        pthread_getattr_np(pthread_self(), &attr);
        pthread_attr_getstack(&attr, &addr, &size);
        pthread_attr_destroy(&attr);
        static if (isStackGrowingDown)
            addr += size;
        return addr;
    }
    else version (PThread_Attr_Get_NP)
    {
        pthread_attr_t attr;
        void* addr; size_t size;

        pthread_attr_init(&attr);
        pthread_attr_get_np(pthread_self(), &attr);
        pthread_attr_getstack(&attr, &addr, &size);
        pthread_attr_destroy(&attr);
        static if (isStackGrowingDown)
            addr += size;
        return addr;
    }
    else version (OpenBSD)
    {
        stack_t stk;

        pthread_stackseg_np(pthread_self(), &stk);
        return stk.ss_sp;
    }
    else version (Solaris)
    {
        stack_t stk;

        thr_stksegment(&stk);
        return stk.ss_sp;
    }
    else
        static assert(false, "Platform not supported.");
}

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
private extern (D) bool suspend( Thread t ) nothrow @nogc
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
        else version (AArch64)
        {
            arm_thread_state64_t state = void;
            mach_msg_type_number_t count = ARM_THREAD_STATE64_COUNT;

            if (thread_get_state(t.m_tmach, ARM_THREAD_STATE64, &state, &count) != KERN_SUCCESS)
                onThreadError("Unable to load thread state");
            // TODO: ThreadException here recurses forever!  Does it
            //still using onThreadError?
            //printf("state count %d (expect %d)\n", count ,ARM_THREAD_STATE64_COUNT);
            if (!t.m_lock)
                t.m_curr.tstack = cast(void*) state.sp;

            t.m_reg[0..29] = state.x;  // x0-x28
            t.m_reg[29] = state.fp;    // x29
            t.m_reg[30] = state.lr;    // x30
            t.m_reg[31] = state.sp;    // x31
            t.m_reg[32] = state.pc;
        }
        else version (ARM)
        {
            arm_thread_state32_t state = void;
            mach_msg_type_number_t count = ARM_THREAD_STATE32_COUNT;

            // Thought this would be ARM_THREAD_STATE32, but that fails.
            // Mystery
            if (thread_get_state(t.m_tmach, ARM_THREAD_STATE, &state, &count) != KERN_SUCCESS)
                onThreadError("Unable to load thread state");
            // TODO: in past, ThreadException here recurses forever!  Does it
            //still using onThreadError?
            //printf("state count %d (expect %d)\n", count ,ARM_THREAD_STATE32_COUNT);
            if (!t.m_lock)
                t.m_curr.tstack = cast(void*) state.sp;

            t.m_reg[0..13] = state.r;  // r0 - r13
            t.m_reg[13] = state.sp;
            t.m_reg[14] = state.lr;
            t.m_reg[15] = state.pc;
        }
        else version (PPC)
        {
            ppc_thread_state_t state = void;
            mach_msg_type_number_t count = PPC_THREAD_STATE_COUNT;

            if (thread_get_state(t.m_tmach, PPC_THREAD_STATE, &state, &count) != KERN_SUCCESS)
                onThreadError("Unable to load thread state");
            if (!t.m_lock)
                t.m_curr.tstack = cast(void*) state.r[1];
            t.m_reg[] = state.r[];
        }
        else version (PPC64)
        {
            ppc_thread_state64_t state = void;
            mach_msg_type_number_t count = PPC_THREAD_STATE64_COUNT;

            if (thread_get_state(t.m_tmach, PPC_THREAD_STATE64, &state, &count) != KERN_SUCCESS)
                onThreadError("Unable to load thread state");
            if (!t.m_lock)
                t.m_curr.tstack = cast(void*) state.r[1];
            t.m_reg[] = state.r[];
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
        bool suspendedSelf;
        Thread t = ThreadBase.sm_tbeg.toThread;
        while (t)
        {
            auto tn = t.next.toThread;
            if (suspend(t))
            {
                if (t is ThreadBase.getThis())
                    suspendedSelf = true;
                ++cnt;
            }
            t = tn;
        }

        version (Darwin)
        {}
        else version (Posix)
        {
            // Subtract own thread if we called suspend() on ourselves.
            // For example, suspendedSelf would be false if the current
            // thread ran thread_detachThis().
            assert(cnt >= 1);
            if (suspendedSelf)
                --cnt;
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
private extern (D) void resume(ThreadBase _t) nothrow @nogc
{
    Thread t = _t.toThread;

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
    else
        static assert(false, "Platform not supported.");
}


/**
 * Initializes the thread module.  This function must be called by the
 * garbage collector on startup and before any other thread routines
 * are called.
 */
extern (C) void thread_init() @nogc nothrow
{
    // NOTE: If thread_init itself performs any allocations then the thread
    //       routines reserved for garbage collector use may be called while
    //       thread_init is being processed.  However, since no memory should
    //       exist to be scanned at this point, it is sufficient for these
    //       functions to detect the condition and return immediately.

    initLowlevelThreads();
    Thread.initLocks();

    version (Darwin)
    {
        // thread id different in forked child process
        static extern(C) void initChildAfterFork()
        {
            auto thisThread = Thread.getThis();
            if (!thisThread)
            {
                // It is possible that runtime was not properly initialized in the current process or thread -
                // it may happen after `fork` call when using a dynamically loaded shared library written in D from a multithreaded non-D program.
                // In such case getThis will return null.
                return;
            }
            thisThread.m_addr = pthread_self();
            assert( thisThread.m_addr != thisThread.m_addr.init );
            thisThread.m_tmach = pthread_mach_thread_np( thisThread.m_addr );
            assert( thisThread.m_tmach != thisThread.m_tmach.init );
       }
        pthread_atfork(null, null, &initChildAfterFork);
    }
    else version (Posix)
    {
        version (OpenBSD)
        {
            // OpenBSD does not support SIGRTMIN or SIGRTMAX
            // Use SIGUSR1 for SIGRTMIN, SIGUSR2 for SIGRTMIN + 1
            // And use 32 for SIGRTMAX (32 is the max signal number on OpenBSD)
            enum SIGRTMIN = SIGUSR1;
            enum SIGRTMAX = 32;
        }

        if ( suspendSignalNumber == 0 )
        {
            suspendSignalNumber = SIGRTMIN;
        }

        if ( resumeSignalNumber == 0 )
        {
            resumeSignalNumber = SIGRTMIN + 1;
            assert(resumeSignalNumber <= SIGRTMAX);
        }
        int         status;
        sigaction_t suspend = void;
        sigaction_t resume = void;

        // This is a quick way to zero-initialize the structs without using
        // memset or creating a link dependency on their static initializer.
        (cast(byte*) &suspend)[0 .. sigaction_t.sizeof] = 0;
        (cast(byte*)  &resume)[0 .. sigaction_t.sizeof] = 0;

        // NOTE: SA_RESTART indicates that system calls should restart if they
        //       are interrupted by a signal, but this is not available on all
        //       Posix systems, even those that support multithreading.
        static if ( __traits( compiles, SA_RESTART ) )
            suspend.sa_flags = SA_RESTART;

        suspend.sa_handler = &thread_suspendHandler;
        // NOTE: We want to ignore all signals while in this handler, so fill
        //       sa_mask to indicate this.
        status = sigfillset( &suspend.sa_mask );
        assert( status == 0 );

        // NOTE: Since resumeSignalNumber should only be issued for threads within the
        //       suspend handler, we don't want this signal to trigger a
        //       restart.
        resume.sa_flags   = 0;
        resume.sa_handler = &thread_resumeHandler;
        // NOTE: We want to ignore all signals while in this handler, so fill
        //       sa_mask to indicate this.
        status = sigfillset( &resume.sa_mask );
        assert( status == 0 );

        status = sigaction( suspendSignalNumber, &suspend, null );
        assert( status == 0 );

        status = sigaction( resumeSignalNumber, &resume, null );
        assert( status == 0 );

        status = sem_init( &suspendCount, 0, 0 );
        assert( status == 0 );
    }
    _mainThreadStore[] = __traits(initSymbol, Thread)[];
    Thread.sm_main = attachThread((cast(Thread)_mainThreadStore.ptr).__ctor());
}

private alias MainThreadStore = void[__traits(classInstanceSize, Thread)];
package __gshared align(__traits(classInstanceAlignment, Thread)) MainThreadStore _mainThreadStore;

/**
 * Terminates the thread module. No other thread routine may be called
 * afterwards.
 */
extern (C) void thread_term() @nogc nothrow
{
    thread_term_tpl!(Thread)(_mainThreadStore);
}


///////////////////////////////////////////////////////////////////////////////
// Thread Entry Point and Signal Handlers
///////////////////////////////////////////////////////////////////////////////


version (Windows)
{
    private
    {
        //
        // Entry point for Windows threads
        //
        extern (Windows) uint thread_entryPoint( void* arg ) nothrow
        {
            Thread  obj = cast(Thread) arg;
            assert( obj );

            obj.initDataStorage();

            Thread.setThis(obj);
            Thread.add(obj);
            scope (exit)
            {
                Thread.remove(obj);
                obj.destroyDataStorage();
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
                obj.m_unhandled = Throwable.chainTogether(obj.m_unhandled, t);
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
            version (GNUShared)
            {
                externDFunc!("gcc.sections.inheritLoadedLibraries",
                             void function(void*) @nogc nothrow)(loadedLibraries);
            }
            else version (Shared)
            {
                externDFunc!("rt.sections_elf_shared.inheritLoadedLibraries",
                             void function(void*) @nogc nothrow)(loadedLibraries);
            }

            obj.initDataStorage();

            atomicStore!(MemoryOrder.raw)(obj.m_isRunning, true);
            Thread.setThis(obj); // allocates lazy TLS (see Issue 11981)
            Thread.add(obj);     // can only receive signals from here on
            scope (exit)
            {
                Thread.remove(obj);
                atomicStore!(MemoryOrder.raw)(obj.m_isRunning, false);
                obj.destroyDataStorage();
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
                obj.m_unhandled = Throwable.chainTogether(obj.m_unhandled, t);
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
                version (GNUShared)
                {
                    externDFunc!("gcc.sections.cleanupLoadedLibraries",
                                 void function() @nogc nothrow)();
                }
                else version (Shared)
                {
                    externDFunc!("rt.sections_elf_shared.cleanupLoadedLibraries",
                                 void function() @nogc nothrow)();
                }
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
        do
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

                status = sem_post( &suspendCount );
                assert( status == 0 );

                sigsuspend( &sigres );

                if ( !obj.m_lock )
                {
                    obj.m_curr.tstack = obj.m_curr.bstack;
                }
            }
            callWithStackShell(&op);
        }


        extern (C) void thread_resumeHandler( int sig ) nothrow
        in
        {
            assert( sig == resumeSignalNumber );
        }
        do
        {

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

//
// exposed by compiler runtime
//
extern (C) void  rt_moduleTlsCtor();
extern (C) void  rt_moduleTlsDtor();


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


///////////////////////////////////////////////////////////////////////////////
// lowlovel threading support
///////////////////////////////////////////////////////////////////////////////

private
{
    version (Windows):
    // If the runtime is dynamically loaded as a DLL, there is a problem with
    // threads still running when the DLL is supposed to be unloaded:
    //
    // - with the VC runtime starting with VS2015 (i.e. using the Universal CRT)
    //   a thread created with _beginthreadex increments the DLL reference count
    //   and decrements it when done, so that the DLL is no longer unloaded unless
    //   all the threads have terminated. With the DLL reference count held up
    //   by a thread that is only stopped by a signal from a static destructor or
    //   the termination of the runtime will cause the DLL to never be unloaded.
    //
    // - with the DigitalMars runtime and VC runtime up to VS2013, the thread
    //   continues to run, but crashes once the DLL is unloaded from memory as
    //   the code memory is no longer accessible. Stopping the threads is not possible
    //   from within the runtime termination as it is invoked from
    //   DllMain(DLL_PROCESS_DETACH) holding a lock that prevents threads from
    //   terminating.
    //
    // Solution: start a watchdog thread that keeps the DLL reference count above 0 and
    // checks it periodically. If it is equal to 1 (plus the number of started threads), no
    // external references to the DLL exist anymore, threads can be stopped
    // and runtime termination and DLL unload can be invoked via FreeLibraryAndExitThread.
    // Note: runtime termination is then performed by a different thread than at startup.
    //
    // Note: if the DLL is never unloaded, process termination kills all threads
    // and signals their handles before unconditionally calling DllMain(DLL_PROCESS_DETACH).

    import core.sys.windows.winbase : FreeLibraryAndExitThread, GetModuleHandleExW,
        GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS, GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT;
    import core.sys.windows.windef : HMODULE;
    import core.sys.windows.dll : dll_getRefCount;

    version (CRuntime_Microsoft)
        extern(C) extern __gshared ubyte msvcUsesUCRT; // from rt/msvc.d

    /// set during termination of a DLL on Windows, i.e. while executing DllMain(DLL_PROCESS_DETACH)
    public __gshared bool thread_DLLProcessDetaching;

    __gshared HMODULE ll_dllModule;
    __gshared ThreadID ll_dllMonitorThread;

    int ll_countLowLevelThreadsWithDLLUnloadCallback() nothrow
    {
        lowlevelLock.lock_nothrow();
        scope(exit) lowlevelLock.unlock_nothrow();

        int cnt = 0;
        foreach (i; 0 .. ll_nThreads)
            if (ll_pThreads[i].cbDllUnload)
                cnt++;
        return cnt;
    }

    bool ll_dllHasExternalReferences() nothrow
    {
        version (CRuntime_DigitalMars)
            enum internalReferences = 1; // only the watchdog thread
        else
            int internalReferences =  msvcUsesUCRT ? 1 + ll_countLowLevelThreadsWithDLLUnloadCallback() : 1;

        int refcnt = dll_getRefCount(ll_dllModule);
        return refcnt > internalReferences;
    }

    private void monitorDLLRefCnt() nothrow
    {
        // this thread keeps the DLL alive until all external references are gone
        while (ll_dllHasExternalReferences())
        {
            Thread.sleep(100.msecs);
        }

        // the current thread will be terminated below
        ll_removeThread(GetCurrentThreadId());

        for (;;)
        {
            ThreadID tid;
            void delegate() nothrow cbDllUnload;
            {
                lowlevelLock.lock_nothrow();
                scope(exit) lowlevelLock.unlock_nothrow();

                foreach (i; 0 .. ll_nThreads)
                    if (ll_pThreads[i].cbDllUnload)
                    {
                        cbDllUnload = ll_pThreads[i].cbDllUnload;
                        tid = ll_pThreads[0].tid;
                    }
            }
            if (!cbDllUnload)
                break;
            cbDllUnload();
            assert(!findLowLevelThread(tid));
        }

        FreeLibraryAndExitThread(ll_dllModule, 0);
    }

    int ll_getDLLRefCount() nothrow @nogc
    {
        if (!ll_dllModule &&
            !GetModuleHandleExW(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
                                cast(const(wchar)*) &ll_getDLLRefCount, &ll_dllModule))
            return -1;
        return dll_getRefCount(ll_dllModule);
    }

    bool ll_startDLLUnloadThread() nothrow @nogc
    {
        int refcnt = ll_getDLLRefCount();
        if (refcnt < 0)
            return false; // not a dynamically loaded DLL

        if (ll_dllMonitorThread !is ThreadID.init)
            return true;

        // if a thread is created from a DLL, the MS runtime (starting with VC2015) increments the DLL reference count
        // to avoid the DLL being unloaded while the thread is still running. Mimick this behavior here for all
        // runtimes not doing this
        version (CRuntime_DigitalMars)
            enum needRef = true;
        else
            bool needRef = !msvcUsesUCRT;

        if (needRef)
        {
            HMODULE hmod;
            GetModuleHandleExW(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS, cast(const(wchar)*) &ll_getDLLRefCount, &hmod);
        }

        ll_dllMonitorThread = createLowLevelThread(() { monitorDLLRefCnt(); });
        return ll_dllMonitorThread != ThreadID.init;
    }
}

/**
 * Create a thread not under control of the runtime, i.e. TLS module constructors are
 * not run and the GC does not suspend it during a collection.
 *
 * Params:
 *  dg        = delegate to execute in the created thread.
 *  stacksize = size of the stack of the created thread. The default of 0 will select the
 *              platform-specific default size.
 *  cbDllUnload = Windows only: if running in a dynamically loaded DLL, this delegate will be called
 *              if the DLL is supposed to be unloaded, but the thread is still running.
 *              The thread must be terminated via `joinLowLevelThread` by the callback.
 *
 * Returns: the platform specific thread ID of the new thread. If an error occurs, `ThreadID.init`
 *  is returned.
 */
ThreadID createLowLevelThread(void delegate() nothrow dg, uint stacksize = 0,
                              void delegate() nothrow cbDllUnload = null) nothrow @nogc
{
    void delegate() nothrow* context = cast(void delegate() nothrow*)malloc(dg.sizeof);
    *context = dg;

    ThreadID tid;
    version (Windows)
    {
        // the thread won't start until after the DLL is unloaded
        if (thread_DLLProcessDetaching)
            return ThreadID.init;

        static extern (Windows) uint thread_lowlevelEntry(void* ctx) nothrow
        {
            auto dg = *cast(void delegate() nothrow*)ctx;
            free(ctx);

            dg();
            ll_removeThread(GetCurrentThreadId());
            return 0;
        }

        // see Thread.start() for why thread is created in suspended state
        HANDLE hThread = cast(HANDLE) _beginthreadex(null, stacksize, &thread_lowlevelEntry,
                                                     context, CREATE_SUSPENDED, &tid);
        if (!hThread)
            return ThreadID.init;
    }

    lowlevelLock.lock_nothrow();
    scope(exit) lowlevelLock.unlock_nothrow();

    ll_nThreads++;
    ll_pThreads = cast(ll_ThreadData*)realloc(ll_pThreads, ll_ThreadData.sizeof * ll_nThreads);

    version (Windows)
    {
        ll_pThreads[ll_nThreads - 1].tid = tid;
        ll_pThreads[ll_nThreads - 1].cbDllUnload = cbDllUnload;
        if (ResumeThread(hThread) == -1)
            onThreadError("Error resuming thread");
        CloseHandle(hThread);

        if (cbDllUnload)
            ll_startDLLUnloadThread();
    }
    else version (Posix)
    {
        static extern (C) void* thread_lowlevelEntry(void* ctx) nothrow
        {
            auto dg = *cast(void delegate() nothrow*)ctx;
            free(ctx);

            dg();
            ll_removeThread(pthread_self());
            return null;
        }

        size_t stksz = adjustStackSize(stacksize);

        pthread_attr_t  attr;

        int rc;
        if ((rc = pthread_attr_init(&attr)) != 0)
            return ThreadID.init;
        if (stksz && (rc = pthread_attr_setstacksize(&attr, stksz)) != 0)
            return ThreadID.init;
        if ((rc = pthread_create(&tid, &attr, &thread_lowlevelEntry, context)) != 0)
            return ThreadID.init;
        if ((rc = pthread_attr_destroy(&attr)) != 0)
            return ThreadID.init;

        ll_pThreads[ll_nThreads - 1].tid = tid;
    }
    return tid;
}

/**
 * Wait for a thread created with `createLowLevelThread` to terminate.
 *
 * Note: In a Windows DLL, if this function is called via DllMain with
 *       argument DLL_PROCESS_DETACH, the thread is terminated forcefully
 *       without proper cleanup as a deadlock would happen otherwise.
 *
 * Params:
 *  tid = the thread ID returned by `createLowLevelThread`.
 */
void joinLowLevelThread(ThreadID tid) nothrow @nogc
{
    version (Windows)
    {
        HANDLE handle = OpenThreadHandle(tid);
        if (!handle)
            return;

        if (thread_DLLProcessDetaching)
        {
            // When being called from DllMain/DLL_DETACH_PROCESS, threads cannot stop
            //  due to the loader lock being held by the current thread.
            // On the other hand, the thread must not continue to run as it will crash
            //  if the DLL is unloaded. The best guess is to terminate it immediately.
            TerminateThread(handle, 1);
            WaitForSingleObject(handle, 10); // give it some time to terminate, but don't wait indefinitely
        }
        else
            WaitForSingleObject(handle, INFINITE);
        CloseHandle(handle);
    }
    else version (Posix)
    {
        if (pthread_join(tid, null) != 0)
            onThreadError("Unable to join thread");
    }
}

nothrow @nogc unittest
{
    struct TaskWithContect
    {
        shared int n = 0;
        void run() nothrow
        {
            n.atomicOp!"+="(1);
        }
    }
    TaskWithContect task;

    ThreadID[8] tids;
    for (int i = 0; i < tids.length; i++)
    {
        tids[i] = createLowLevelThread(&task.run);
        assert(tids[i] != ThreadID.init);
    }

    for (int i = 0; i < tids.length; i++)
        joinLowLevelThread(tids[i]);

    assert(task.n == tids.length);
}

version (Posix)
private size_t adjustStackSize(size_t sz) nothrow @nogc
{
    if (sz == 0)
        return 0;

    // stack size must be at least PTHREAD_STACK_MIN for most platforms.
    if (PTHREAD_STACK_MIN > sz)
        sz = PTHREAD_STACK_MIN;

    version (CRuntime_Glibc)
    {
        // On glibc, TLS uses the top of the stack, so add its size to the requested size
        version (GNU)
        {
            sz += externDFunc!("gcc.sections.elf.sizeOfTLS",
                               size_t function() @nogc nothrow)();
        }
        else
        {
            sz += externDFunc!("rt.sections_elf_shared.sizeOfTLS",
                               size_t function() @nogc nothrow)();
        }
    }

    // stack size must be a multiple of pageSize
    sz = ((sz + pageSize - 1) & ~(pageSize - 1));

    return sz;
}

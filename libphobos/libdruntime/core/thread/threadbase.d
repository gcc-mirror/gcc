/**
 * The threadbase module provides OS-independent code
 * for thread storage and management.
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
module core.thread.threadbase;

import core.thread.context;
import core.thread.types;
import core.time;
import core.sync.mutex;
import core.stdc.stdlib : free, realloc;

private
{
    import core.internal.traits : externDFunc;

    // interface to rt.tlsgc
    alias rt_tlsgc_init = externDFunc!("rt.tlsgc.init", void* function() nothrow @nogc);
    alias rt_tlsgc_destroy = externDFunc!("rt.tlsgc.destroy", void function(void*) nothrow @nogc);

    alias ScanDg = void delegate(void* pstart, void* pend) nothrow;
    alias rt_tlsgc_scan =
        externDFunc!("rt.tlsgc.scan", void function(void*, scope ScanDg) nothrow);

    alias rt_tlsgc_processGCMarks =
        externDFunc!("rt.tlsgc.processGCMarks", void function(void*, scope IsMarkedDg) nothrow);
}


///////////////////////////////////////////////////////////////////////////////
// Thread and Fiber Exceptions
///////////////////////////////////////////////////////////////////////////////


/**
 * Base class for thread exceptions.
 */
class ThreadException : Exception
{
    @nogc @safe pure nothrow this(string msg, string file = __FILE__, size_t line = __LINE__, Throwable next = null)
    {
        super(msg, file, line, next);
    }

    @nogc @safe pure nothrow this(string msg, Throwable next, string file = __FILE__, size_t line = __LINE__)
    {
        super(msg, file, line, next);
    }
}


/**
* Base class for thread errors to be used for function inside GC when allocations are unavailable.
*/
class ThreadError : Error
{
    @nogc @safe pure nothrow this(string msg, string file = __FILE__, size_t line = __LINE__, Throwable next = null)
    {
        super(msg, file, line, next);
    }

    @nogc @safe pure nothrow this(string msg, Throwable next, string file = __FILE__, size_t line = __LINE__)
    {
        super(msg, file, line, next);
    }
}

private
{
    // Handling unaligned mutexes are not supported on all platforms, so we must
    // ensure that the address of all shared data are appropriately aligned.
    enum mutexAlign = __traits(classInstanceAlignment, Mutex);
    enum mutexClassInstanceSize = __traits(classInstanceSize, Mutex);

    alias swapContext = externDFunc!("core.thread.osthread.swapContext", void* function(void*) nothrow @nogc);

    alias getStackBottom = externDFunc!("core.thread.osthread.getStackBottom", void* function() nothrow @nogc);
    alias getStackTop = externDFunc!("core.thread.osthread.getStackTop", void* function() nothrow @nogc);
}


///////////////////////////////////////////////////////////////////////////////
// Thread
///////////////////////////////////////////////////////////////////////////////


class ThreadBase
{
    ///////////////////////////////////////////////////////////////////////////
    // Initialization
    ///////////////////////////////////////////////////////////////////////////

    this(void function() fn, size_t sz = 0) @safe pure nothrow @nogc
    in(fn)
    {
        this(sz);
        m_call = fn;
    }

    this(void delegate() dg, size_t sz = 0) @trusted pure nothrow @nogc
    in( cast(const void delegate()) dg)
    {
        this(sz);
        m_call = dg;
    }

    /**
     * Cleans up any remaining resources used by this object.
     */
    package bool destructBeforeDtor() nothrow @nogc
    {
        destroyDataStorageIfAvail();

        bool no_context = m_addr == m_addr.init;
        bool not_registered = !next && !prev && (sm_tbeg !is this);

        return (no_context || not_registered);
    }

    package void tlsGCdataInit() nothrow @nogc
    {
        m_tlsgcdata = rt_tlsgc_init();
    }

    package void initDataStorage() nothrow
    {
        assert(m_curr is &m_main);

        m_main.bstack = getStackBottom();
        m_main.tstack = m_main.bstack;
        tlsGCdataInit();
    }

    package void destroyDataStorage() nothrow @nogc
    {
        rt_tlsgc_destroy(m_tlsgcdata);
        m_tlsgcdata = null;
    }

    package void destroyDataStorageIfAvail() nothrow @nogc
    {
        if (m_tlsgcdata)
            destroyDataStorage();
    }


    ///////////////////////////////////////////////////////////////////////////
    // General Actions
    ///////////////////////////////////////////////////////////////////////////


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
    abstract Throwable join(bool rethrow = true);


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
        synchronized(this)
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
        synchronized(this)
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
    final @property void name(string val) @safe @nogc
    {
        synchronized(this)
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
        synchronized(this)
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
    final @property void isDaemon(bool val) @safe @nogc
    {
        synchronized(this)
        {
            m_isDaemon = val;
        }
    }

    /**
     * Tests whether this thread is the main thread, i.e. the thread
     * that initialized the runtime
     *
     * Returns:
     *  true if the thread is the main thread
     */
    final @property bool isMainThread() nothrow @nogc
    {
        return this is sm_main;
    }

    /**
     * Tests whether this thread is running.
     *
     * Returns:
     *  true if the thread is running, false if not.
     */
    @property bool isRunning() nothrow @nogc
    {
        if (m_addr == m_addr.init)
            return false;

        return true;
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
    static ThreadBase getThis() @safe nothrow @nogc
    {
        // NOTE: This function may not be called until thread_init has
        //       completed.  See thread_suspendAll for more information
        //       on why this might occur.
        version (GNU) pragma(inline, false);
        return sm_this;
    }


    /**
     * Provides a list of all threads currently being tracked by the system.
     * Note that threads in the returned array might no longer run (see
     * $(D ThreadBase.)$(LREF isRunning)).
     *
     * Returns:
     *  An array containing references to all threads currently being
     *  tracked by the system.  The result of deleting any contained
     *  objects is undefined.
     */
    static ThreadBase[] getAll()
    {
        static void resize(ref ThreadBase[] buf, size_t nlen)
        {
            buf.length = nlen;
        }
        return getAllImpl!resize();
    }


    /**
     * Operates on all threads currently being tracked by the system.  The
     * result of deleting any Thread object is undefined.
     * Note that threads passed to the callback might no longer run (see
     * $(D ThreadBase.)$(LREF isRunning)).
     *
     * Params:
     *  dg = The supplied code as a delegate.
     *
     * Returns:
     *  Zero if all elemented are visited, nonzero if not.
     */
    static int opApply(scope int delegate(ref ThreadBase) dg)
    {
        static void resize(ref ThreadBase[] buf, size_t nlen)
        {
            import core.exception: onOutOfMemoryError;

            auto newBuf = cast(ThreadBase*)realloc(buf.ptr, nlen * size_t.sizeof);
            if (newBuf is null) onOutOfMemoryError();
            buf = newBuf[0 .. nlen];
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

    private static ThreadBase[] getAllImpl(alias resize)()
    {
        import core.atomic;

        ThreadBase[] buf;
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
                    for (ThreadBase t = sm_tbeg; t; t = t.next)
                        buf[pos++] = t;
                    return buf;
                }
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // Actions on Calling Thread
    ///////////////////////////////////////////////////////////////////////////

    /**
     * Forces a context switch to occur away from the calling thread.
     */
    private static void yield() @nogc nothrow
    {
        thread_yield();
    }

    ///////////////////////////////////////////////////////////////////////////
    // Stuff That Should Go Away
    ///////////////////////////////////////////////////////////////////////////


    //
    // Initializes a thread object which has no associated executable function.
    // This is used for the main thread initialized in thread_init().
    //
    package this(size_t sz = 0) @safe pure nothrow @nogc
    {
        m_sz = sz;
        m_curr = &m_main;
    }

    //
    // Thread entry point.  Invokes the function or delegate passed on
    // construction (if any).
    //
    package final void run()
    {
        m_call();
    }

package:

    //
    // Local storage
    //
    static ThreadBase       sm_this;


    //
    // Main process thread
    //
    __gshared ThreadBase    sm_main;


    //
    // Standard thread data
    //
    ThreadID            m_addr;
    Callable            m_call;
    string              m_name;
    size_t              m_sz;
    bool                m_isDaemon;
    bool                m_isInCriticalRegion;
    Throwable           m_unhandled;

    ///////////////////////////////////////////////////////////////////////////
    // Storage of Active Thread
    ///////////////////////////////////////////////////////////////////////////


    //
    // Sets a thread-local reference to the current thread object.
    //
    package static void setThis(ThreadBase t) nothrow @nogc
    {
        sm_this = t;
    }

package(core.thread):

    StackContext        m_main;
    StackContext*       m_curr;
    bool                m_lock;
    private void*       m_tlsgcdata;

    ///////////////////////////////////////////////////////////////////////////
    // Thread Context and GC Scanning Support
    ///////////////////////////////////////////////////////////////////////////


    final void pushContext(StackContext* c) nothrow @nogc
    in
    {
        assert(!c.within);
    }
    do
    {
        m_curr.ehContext = swapContext(c.ehContext);
        c.within = m_curr;
        m_curr = c;
    }


    final void popContext() nothrow @nogc
    in
    {
        assert(m_curr && m_curr.within);
    }
    do
    {
        StackContext* c = m_curr;
        m_curr = c.within;
        c.ehContext = swapContext(m_curr.ehContext);
        c.within = null;
    }

    private final StackContext* topContext() nothrow @nogc
    in(m_curr)
    {
        return m_curr;
    }


package(core.thread):
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

    static void initLocks() @nogc nothrow
    {
        import core.lifetime : emplace;
        emplace!Mutex(_slock[]);
        emplace!Mutex(_criticalRegionLock[]);
    }

    static void termLocks() @nogc nothrow
    {
        (cast(Mutex)_slock.ptr).__dtor();
        (cast(Mutex)_criticalRegionLock.ptr).__dtor();
    }

    __gshared StackContext*  sm_cbeg;

    __gshared ThreadBase    sm_tbeg;
    __gshared size_t        sm_tlen;

    // can't use core.internal.util.array in public code
    __gshared ThreadBase* pAboutToStart;
    __gshared size_t      nAboutToStart;

    //
    // Used for ordering threads in the global thread list.
    //
    ThreadBase          prev;
    ThreadBase          next;


    ///////////////////////////////////////////////////////////////////////////
    // Global Context List Operations
    ///////////////////////////////////////////////////////////////////////////


    //
    // Add a context to the global context list.
    //
    static void add(StackContext* c) nothrow @nogc
    in
    {
        assert(c);
        assert(!c.next && !c.prev);
    }
    do
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
    static void remove(StackContext* c) nothrow @nogc
    in
    {
        assert(c);
        assert(c.next || c.prev);
    }
    do
    {
        if (c.prev)
            c.prev.next = c.next;
        if (c.next)
            c.next.prev = c.prev;
        if (sm_cbeg == c)
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
    static void add(ThreadBase t, bool rmAboutToStart = true) nothrow @nogc
    in
    {
        assert(t);
        assert(!t.next && !t.prev);
    }
    do
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
            memmove(pAboutToStart + idx, pAboutToStart + idx + 1, size_t.sizeof * (nAboutToStart - idx - 1));
            pAboutToStart =
                cast(ThreadBase*)realloc(pAboutToStart, size_t.sizeof * --nAboutToStart);
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
    static void remove(ThreadBase t) nothrow @nogc
    in
    {
        assert(t);
    }
    do
    {
        // Thread was already removed earlier, might happen b/c of thread_detachInstance
        if (!t.next && !t.prev && (sm_tbeg !is t))
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
            remove(&t.m_main);

            if (t.prev)
                t.prev.next = t.next;
            if (t.next)
                t.next.prev = t.prev;
            if (sm_tbeg is t)
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


///////////////////////////////////////////////////////////////////////////////
// GC Support Routines
///////////////////////////////////////////////////////////////////////////////

private alias attachThread = externDFunc!("core.thread.osthread.attachThread", ThreadBase function(ThreadBase) @nogc nothrow);

extern (C) void _d_monitordelete_nogc(Object h) @nogc nothrow;

/**
 * Terminates the thread module. No other thread routine may be called
 * afterwards.
 */
package void thread_term_tpl(ThreadT, MainThreadStore)(ref MainThreadStore _mainThreadStore) @nogc nothrow
{
    assert(_mainThreadStore.ptr is cast(void*) ThreadBase.sm_main);

    // destruct manually as object.destroy is not @nogc
    (cast(ThreadT) cast(void*) ThreadBase.sm_main).__dtor();
    _d_monitordelete_nogc(ThreadBase.sm_main);
    _mainThreadStore[] = __traits(initSymbol, ThreadT)[];
    ThreadBase.sm_main = null;

    assert(ThreadBase.sm_tbeg && ThreadBase.sm_tlen == 1);
    assert(!ThreadBase.nAboutToStart);
    if (ThreadBase.pAboutToStart) // in case realloc(p, 0) doesn't return null
    {
        free(ThreadBase.pAboutToStart);
        ThreadBase.pAboutToStart = null;
    }
    ThreadBase.termLocks();
    termLowlevelThreads();
}


/**
 *
 */
extern (C) bool thread_isMainThread() nothrow @nogc
{
    return ThreadBase.getThis() is ThreadBase.sm_main;
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
package ThreadT thread_attachThis_tpl(ThreadT)()
{
    if (auto t = ThreadT.getThis())
        return t;

    return cast(ThreadT) attachThread(new ThreadT());
}


/**
 * Deregisters the calling thread from use with the runtime.  If this routine
 * is called for a thread which is not registered, the result is undefined.
 *
 * NOTE: This routine does not run thread-local static destructors when called.
 *       If full functionality as a D thread is desired, the following function
 *       must be called before thread_detachThis, particularly if the thread is
 *       being detached at some indeterminate time before program termination:
 *
 *       $(D extern(C) void rt_moduleTlsDtor();)
 *
 * See_Also:
 *     $(REF thread_attachThis, core,thread,osthread)
 */
extern (C) void thread_detachThis() nothrow @nogc
{
    if (auto t = ThreadBase.getThis())
        ThreadBase.remove(t);
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
extern (C) void thread_detachByAddr(ThreadID addr)
{
    if (auto t = thread_findByAddr(addr))
        ThreadBase.remove(t);
}


/// ditto
extern (C) void thread_detachInstance(ThreadBase t) nothrow @nogc
{
    ThreadBase.remove(t);
}


/**
 * Search the list of all threads for a thread with the given thread identifier.
 *
 * Params:
 *  addr = The thread identifier to search for.
 * Returns:
 *  The thread object associated with the thread identifier, null if not found.
 */
static ThreadBase thread_findByAddr(ThreadID addr)
{
    ThreadBase.slock.lock_nothrow();
    scope(exit) ThreadBase.slock.unlock_nothrow();

    // also return just spawned thread so that
    // DLL_THREAD_ATTACH knows it's a D thread
    foreach (t; ThreadBase.pAboutToStart[0 .. ThreadBase.nAboutToStart])
        if (t.m_addr == addr)
            return t;

    foreach (t; ThreadBase)
        if (t.m_addr == addr)
            return t;

    return null;
}


/**
 * Sets the current thread to a specific reference. Only to be used
 * when dealing with externally-created threads (in e.g. C code).
 * The primary use of this function is when ThreadBase.getThis() must
 * return a sensible value in, for example, TLS destructors. In
 * other words, don't touch this unless you know what you're doing.
 *
 * Params:
 *  t = A reference to the current thread. May be null.
 */
extern (C) void thread_setThis(ThreadBase t) nothrow @nogc
{
    ThreadBase.setThis(t);
}


/**
 * Joins all non-daemon threads that are currently running.  This is done by
 * performing successive scans through the thread list until a scan consists
 * of only daemon threads.
 */
extern (C) void thread_joinAll()
{
 Lagain:
    ThreadBase.slock.lock_nothrow();
    // wait for just spawned threads
    if (ThreadBase.nAboutToStart)
    {
        ThreadBase.slock.unlock_nothrow();
        ThreadBase.yield();
        goto Lagain;
    }

    // join all non-daemon threads, the main thread is also a daemon
    auto t = ThreadBase.sm_tbeg;
    while (t)
    {
        if (!t.isRunning)
        {
            auto tn = t.next;
            ThreadBase.remove(t);
            t = tn;
        }
        else if (t.isDaemon)
        {
            t = t.next;
        }
        else
        {
            ThreadBase.slock.unlock_nothrow();
            t.join(); // might rethrow
            goto Lagain; // must restart iteration b/c of unlock
        }
    }
    ThreadBase.slock.unlock_nothrow();
}


/**
 * Performs intermediate shutdown of the thread module.
 */
shared static ~this()
{
    // NOTE: The functionality related to garbage collection must be minimally
    //       operable after this dtor completes.  Therefore, only minimal
    //       cleanup may occur.
    auto t = ThreadBase.sm_tbeg;
    while (t)
    {
        auto tn = t.next;
        if (!t.isRunning)
            ThreadBase.remove(t);
        t = tn;
    }
}

// Used for needLock below.
package __gshared bool multiThreadedFlag = false;

// Used for suspendAll/resumeAll below.
package __gshared uint suspendDepth = 0;

private alias resume = externDFunc!("core.thread.osthread.resume", void function(ThreadBase) nothrow @nogc);

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
    assert(suspendDepth > 0);
}
do
{
    // NOTE: See thread_suspendAll for the logic behind this.
    if (!multiThreadedFlag && ThreadBase.sm_tbeg)
    {
        if (--suspendDepth == 0)
            resume(ThreadBase.getThis());
        return;
    }

    scope(exit) ThreadBase.slock.unlock_nothrow();
    {
        if (--suspendDepth > 0)
            return;

        for (ThreadBase t = ThreadBase.sm_tbeg; t; t = t.next)
        {
            // NOTE: We do not need to care about critical regions at all
            //       here. thread_suspendAll takes care of everything.
            resume(t);
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
extern (C) void thread_scanAllType(scope ScanAllThreadsTypeFn scan) nothrow
in
{
    assert(suspendDepth > 0);
}
do
{
    callWithStackShell(sp => scanAllTypeImpl(scan, sp));
}

package alias callWithStackShellDg = void delegate(void* sp) nothrow;
private alias callWithStackShell = externDFunc!("core.thread.osthread.callWithStackShell", void function(scope callWithStackShellDg) nothrow);

private void scanAllTypeImpl(scope ScanAllThreadsTypeFn scan, void* curStackTop) nothrow
{
    ThreadBase  thisThread  = null;
    void*   oldStackTop = null;

    if (ThreadBase.sm_tbeg)
    {
        thisThread  = ThreadBase.getThis();
        if (!thisThread.m_lock)
        {
            oldStackTop = thisThread.m_curr.tstack;
            thisThread.m_curr.tstack = curStackTop;
        }
    }

    scope(exit)
    {
        if (ThreadBase.sm_tbeg)
        {
            if (!thisThread.m_lock)
            {
                thisThread.m_curr.tstack = oldStackTop;
            }
        }
    }

    // NOTE: Synchronizing on ThreadBase.slock is not needed because this
    //       function may only be called after all other threads have
    //       been suspended from within the same lock.
    if (ThreadBase.nAboutToStart)
        scan(ScanType.stack, ThreadBase.pAboutToStart, ThreadBase.pAboutToStart + ThreadBase.nAboutToStart);

    for (StackContext* c = ThreadBase.sm_cbeg; c; c = c.next)
    {
        static if (isStackGrowingDown)
        {
            assert(c.tstack <= c.bstack, "stack bottom can't be less than top");

            // NOTE: We can't index past the bottom of the stack
            //       so don't do the "+1" if isStackGrowingDown.
            if (c.tstack && c.tstack < c.bstack)
                scan(ScanType.stack, c.tstack, c.bstack);
        }
        else
        {
            assert(c.bstack <= c.tstack, "stack top can't be less than bottom");

            if (c.bstack && c.bstack < c.tstack)
                scan(ScanType.stack, c.bstack, c.tstack + 1);
        }
    }

    for (ThreadBase t = ThreadBase.sm_tbeg; t; t = t.next)
    {
        version (Windows)
        {
            // Ideally, we'd pass ScanType.regs or something like that, but this
            // would make portability annoying because it only makes sense on Windows.
            scanWindowsOnly(scan, t);
        }

        if (t.m_tlsgcdata !is null)
            rt_tlsgc_scan(t.m_tlsgcdata, (p1, p2) => scan(ScanType.tls, p1, p2));
    }
}

version (Windows)
{
    // Currently scanWindowsOnly can't be handled properly by externDFunc
    // https://github.com/dlang/druntime/pull/3135#issuecomment-643673218
    pragma(mangle, "_D4core6thread8osthread15scanWindowsOnlyFNbMDFNbEQBvQBt10threadbase8ScanTypePvQcZvCQDdQDbQBi10ThreadBaseZv")
    private extern (D) void scanWindowsOnly(scope ScanAllThreadsTypeFn scan, ThreadBase) nothrow;
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
extern (C) void thread_scanAll(scope ScanAllThreadsFn scan) nothrow
{
    thread_scanAllType((type, p1, p2) => scan(p1, p2));
}

private alias thread_yield = externDFunc!("core.thread.osthread.thread_yield", void function() @nogc nothrow);

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
 * $(LINK2 https://github.com/mono/mono/blob/521f4a198e442573c400835ef19bbb36b60b0ebb/mono/metadata/sgen-gc.h#L925, Mono's SGen garbage collector).
 *
 * In:
 *  The calling thread must be attached to the runtime.
 */
extern (C) void thread_enterCriticalRegion() @nogc
in
{
    assert(ThreadBase.getThis());
}
do
{
    synchronized (ThreadBase.criticalRegionLock)
        ThreadBase.getThis().m_isInCriticalRegion = true;
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
    assert(ThreadBase.getThis());
}
do
{
    synchronized (ThreadBase.criticalRegionLock)
        ThreadBase.getThis().m_isInCriticalRegion = false;
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
    assert(ThreadBase.getThis());
}
do
{
    synchronized (ThreadBase.criticalRegionLock)
        return ThreadBase.getThis().m_isInCriticalRegion;
}


/**
* A callback for thread errors in D during collections. Since an allocation is not possible
*  a preallocated ThreadError will be used as the Error instance
*
* Returns:
*  never returns
* Throws:
*  ThreadError.
*/
package void onThreadError(string msg) nothrow @nogc
{
    __gshared ThreadError error = new ThreadError(null);
    error.msg = msg;
    error.next = null;
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


/**
 * Indicates whether an address has been marked by the GC.
 */
enum IsMarked : int
{
         no, /// Address is not marked.
        yes, /// Address is marked.
    unknown, /// Address is not managed by the GC.
}

alias IsMarkedDg = int delegate(void* addr) nothrow; /// The isMarked callback function.

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
extern(C) void thread_processGCMarks(scope IsMarkedDg isMarked) nothrow
{
    for (ThreadBase t = ThreadBase.sm_tbeg; t; t = t.next)
    {
        /* Can be null if collection was triggered between adding a
         * thread and calling rt_tlsgc_init.
         */
        if (t.m_tlsgcdata !is null)
            rt_tlsgc_processGCMarks(t.m_tlsgcdata, isMarked);
    }
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
    assert(ThreadBase.getThis());
}
do
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
in (ThreadBase.getThis())
{
    return ThreadBase.getThis().topContext().bstack;
}


///////////////////////////////////////////////////////////////////////////////
// lowlovel threading support
///////////////////////////////////////////////////////////////////////////////
package
{
    __gshared size_t ll_nThreads;
    __gshared ll_ThreadData* ll_pThreads;

    __gshared align(mutexAlign) void[mutexClassInstanceSize] ll_lock;

    @property Mutex lowlevelLock() nothrow @nogc
    {
        return cast(Mutex)ll_lock.ptr;
    }

    void initLowlevelThreads() @nogc nothrow
    {
        import core.lifetime : emplace;
        emplace(lowlevelLock());
    }

    void termLowlevelThreads() @nogc nothrow
    {
        lowlevelLock.__dtor();
    }

    void ll_removeThread(ThreadID tid) nothrow @nogc
    {
        lowlevelLock.lock_nothrow();
        scope(exit) lowlevelLock.unlock_nothrow();

        foreach (i; 0 .. ll_nThreads)
        {
            if (tid is ll_pThreads[i].tid)
            {
                import core.stdc.string : memmove;
                memmove(ll_pThreads + i, ll_pThreads + i + 1, ll_ThreadData.sizeof * (ll_nThreads - i - 1));
                --ll_nThreads;
                // no need to minimize, next add will do
                break;
            }
        }
    }
}

/**
 * Check whether a thread was created by `createLowLevelThread`.
 *
 * Params:
 *  tid = the platform specific thread ID.
 *
 * Returns: `true` if the thread was created by `createLowLevelThread` and is still running.
 */
bool findLowLevelThread(ThreadID tid) nothrow @nogc
{
    lowlevelLock.lock_nothrow();
    scope(exit) lowlevelLock.unlock_nothrow();

    foreach (i; 0 .. ll_nThreads)
        if (tid is ll_pThreads[i].tid)
            return true;
    return false;
}

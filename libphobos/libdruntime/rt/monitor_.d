/**
 * Contains the implementation for object monitors.
 *
 * Copyright: Copyright Digital Mars 2000 - 2015.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright, Sean Kelly, Martin Nowak
 */

/* NOTE: This file has been patched from the original DMD distribution to
 * work with the GDC compiler.
 */
module rt.monitor_;

import core.atomic, core.stdc.stdlib, core.stdc.string;

// NOTE: The dtor callback feature is only supported for monitors that are not
//       supplied by the user.  The assumption is that any object with a user-
//       supplied monitor may have special storage or lifetime requirements and
//       that as a result, storing references to local objects within Monitor
//       may not be safe or desirable.  Thus, devt is only valid if impl is
//       null.

extern (C) void _d_setSameMutex(shared Object ownee, shared Object owner) nothrow
in
{
    assert(ownee.__monitor is null);
}
body
{
    auto m = ensureMonitor(cast(Object) owner);
    if (m.impl is null)
    {
        atomicOp!("+=")(m.refs, cast(size_t) 1);
    }
    // Assume the monitor is garbage collected and simply copy the reference.
    ownee.__monitor = owner.__monitor;
}

extern (C) void _d_monitordelete(Object h, bool det)
{
    auto m = getMonitor(h);
    if (m is null)
        return;

    if (m.impl)
    {
        // let the GC collect the monitor
        setMonitor(h, null);
    }
    else if (!atomicOp!("-=")(m.refs, cast(size_t) 1))
    {
        // refcount == 0 means unshared => no synchronization required
        disposeEvent(cast(Monitor*) m, h);
        deleteMonitor(cast(Monitor*) m);
        setMonitor(h, null);
    }
}

// does not call dispose events, for internal use only
extern (C) void _d_monitordelete_nogc(Object h) @nogc
{
    auto m = getMonitor(h);
    if (m is null)
        return;

    if (m.impl)
    {
        // let the GC collect the monitor
        setMonitor(h, null);
    }
    else if (!atomicOp!("-=")(m.refs, cast(size_t) 1))
    {
        // refcount == 0 means unshared => no synchronization required
        deleteMonitor(cast(Monitor*) m);
        setMonitor(h, null);
    }
}

extern (C) void _d_monitorenter(Object h)
in
{
    assert(h !is null, "Synchronized object must not be null.");
}
body
{
    auto m = cast(Monitor*) ensureMonitor(h);
    auto i = m.impl;
    if (i is null)
        lockMutex(&m.mtx);
    else
        i.lock();
}

extern (C) void _d_monitorexit(Object h)
{
    auto m = cast(Monitor*) getMonitor(h);
    auto i = m.impl;
    if (i is null)
        unlockMutex(&m.mtx);
    else
        i.unlock();
}

extern (C) void rt_attachDisposeEvent(Object h, DEvent e)
{
    synchronized (h)
    {
        auto m = cast(Monitor*) getMonitor(h);
        assert(m.impl is null);

        foreach (ref v; m.devt)
        {
            if (v is null || v == e)
            {
                v = e;
                return;
            }
        }

        auto len = m.devt.length + 4; // grow by 4 elements
        auto pos = m.devt.length; // insert position
        auto p = realloc(m.devt.ptr, DEvent.sizeof * len);
        import core.exception : onOutOfMemoryError;

        if (!p)
            onOutOfMemoryError();
        m.devt = (cast(DEvent*) p)[0 .. len];
        m.devt[pos + 1 .. len] = null;
        m.devt[pos] = e;
    }
}

extern (C) void rt_detachDisposeEvent(Object h, DEvent e)
{
    synchronized (h)
    {
        auto m = cast(Monitor*) getMonitor(h);
        assert(m.impl is null);

        foreach (p, v; m.devt)
        {
            if (v == e)
            {
                memmove(&m.devt[p], &m.devt[p + 1], (m.devt.length - p - 1) * DEvent.sizeof);
                m.devt[$ - 1] = null;
                return;
            }
        }
    }
}

nothrow:

extern (C) void _d_monitor_staticctor()
{
    version (Posix)
    {
        pthread_mutexattr_init(&gattr);
        pthread_mutexattr_settype(&gattr, PTHREAD_MUTEX_RECURSIVE);
    }
    initMutex(&gmtx);
}

extern (C) void _d_monitor_staticdtor()
{
    destroyMutex(&gmtx);
    version (Posix)
        pthread_mutexattr_destroy(&gattr);
}

package:

// This is what the monitor reference in Object points to
alias IMonitor = Object.Monitor;
alias DEvent = void delegate(Object);

version (GNU)
{
    import gcc.config;
    static if (GNU_Thread_Model == ThreadModel.Single)
        version = SingleThreaded;
    // Ignore ThreadModel, we don't want posix threads on windows and
    // will always use native threading instead.
}

version (SingleThreaded)
{
    alias Mutex = int;

    void initMutex(Mutex* mtx)
    {
    }

    void destroyMutex(Mutex* mtx)
    {
    }

    void lockMutex(Mutex* mtx)
    {
    }

    void unlockMutex(Mutex* mtx)
    {
    }
}
else version (Windows)
{
    version (CRuntime_DigitalMars)
    {
        pragma(lib, "snn.lib");
    }
    import core.sys.windows.winbase /+: CRITICAL_SECTION, DeleteCriticalSection,
        EnterCriticalSection, InitializeCriticalSection, LeaveCriticalSection+/;

    alias Mutex = CRITICAL_SECTION;

    alias initMutex = InitializeCriticalSection;
    alias destroyMutex = DeleteCriticalSection;
    alias lockMutex = EnterCriticalSection;
    alias unlockMutex = LeaveCriticalSection;
}
else version (Posix)
{
    import core.sys.posix.pthread;

@nogc:
    alias Mutex = pthread_mutex_t;
    __gshared pthread_mutexattr_t gattr;

    void initMutex(pthread_mutex_t* mtx)
    {
        pthread_mutex_init(mtx, &gattr) && assert(0);
    }

    void destroyMutex(pthread_mutex_t* mtx)
    {
        pthread_mutex_destroy(mtx) && assert(0);
    }

    void lockMutex(pthread_mutex_t* mtx)
    {
        pthread_mutex_lock(mtx) && assert(0);
    }

    void unlockMutex(pthread_mutex_t* mtx)
    {
        pthread_mutex_unlock(mtx) && assert(0);
    }
}
else
{
    static assert(0, "Unsupported platform");
}

struct Monitor
{
    IMonitor impl; // for user-level monitors
    DEvent[] devt; // for internal monitors
    size_t refs; // reference count
    Mutex mtx;
}

private:

@property ref shared(Monitor*) monitor(Object h) pure nothrow @nogc
{
    return *cast(shared Monitor**)&h.__monitor;
}

private shared(Monitor)* getMonitor(Object h) pure @nogc
{
    return atomicLoad!(MemoryOrder.acq)(h.monitor);
}

void setMonitor(Object h, shared(Monitor)* m) pure @nogc
{
    atomicStore!(MemoryOrder.rel)(h.monitor, m);
}

__gshared Mutex gmtx;

shared(Monitor)* ensureMonitor(Object h)
{
    if (auto m = getMonitor(h))
        return m;

    auto m = cast(Monitor*) calloc(Monitor.sizeof, 1);
    assert(m);
    initMutex(&m.mtx);

    bool success;
    lockMutex(&gmtx);
    if (getMonitor(h) is null)
    {
        m.refs = 1;
        setMonitor(h, cast(shared) m);
        success = true;
    }
    unlockMutex(&gmtx);

    if (success)
    {
        // Set the finalize bit so that the monitor gets collected (Bugzilla 14573)
        import core.memory : GC;

        if (!(typeid(h).m_flags & TypeInfo_Class.ClassFlags.hasDtor))
            GC.setAttr(cast(void*) h, GC.BlkAttr.FINALIZE);
        return cast(shared(Monitor)*) m;
    }
    else // another thread succeeded instead
    {
        deleteMonitor(m);
        return getMonitor(h);
    }
}

void deleteMonitor(Monitor* m) @nogc
{
    destroyMutex(&m.mtx);
    free(m);
}

void disposeEvent(Monitor* m, Object h)
{
    foreach (v; m.devt)
    {
        if (v)
            v(h);
    }
    if (m.devt.ptr)
        free(m.devt.ptr);
}

// Bugzilla 14573
unittest
{
    import core.memory : GC;

    auto obj = new Object;
    assert(!(GC.getAttr(cast(void*) obj) & GC.BlkAttr.FINALIZE));
    ensureMonitor(obj);
    assert(getMonitor(obj) !is null);
    assert(GC.getAttr(cast(void*) obj) & GC.BlkAttr.FINALIZE);
}

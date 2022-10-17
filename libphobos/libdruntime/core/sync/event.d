/**
 * The event module provides a primitive for lightweight signaling of other threads
 * (emulating Windows events on Posix)
 *
 * Copyright: Copyright (c) 2019 D Language Foundation
 * License: Distributed under the
 *    $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors: Rainer Schuetze
 * Source:    $(DRUNTIMESRC core/sync/event.d)
 */
module core.sync.event;

version (Windows)
{
    import core.sys.windows.basetsd /+: HANDLE +/;
    import core.sys.windows.winerror /+: WAIT_TIMEOUT +/;
    import core.sys.windows.winbase /+: CreateEvent, CloseHandle, SetEvent, ResetEvent,
        WaitForSingleObject, INFINITE, WAIT_OBJECT_0+/;
}
else version (Posix)
{
    import core.sys.posix.pthread;
    import core.sys.posix.sys.types;
    import core.sys.posix.time;
}
else
{
    static assert(false, "Platform not supported");
}

import core.time;
import core.internal.abort : abort;

/**
 * represents an event. Clients of an event are suspended while waiting
 * for the event to be "signaled".
 *
 * Implemented using `pthread_mutex` and `pthread_condition` on Posix and
 * `CreateEvent` and `SetEvent` on Windows.
---
import core.sync.event, core.thread, std.file;

struct ProcessFile
{
    ThreadGroup group;
    Event event;
    void[] buffer;

    void doProcess()
    {
        event.wait();
        // process buffer
    }

    void process(string filename)
    {
        event.initialize(true, false);
        group = new ThreadGroup;
        for (int i = 0; i < 10; ++i)
            group.create(&doProcess);

        buffer = std.file.read(filename);
        event.set();
        group.joinAll();
        event.terminate();
    }
}
---
 */
struct Event
{
nothrow @nogc:
    /**
     * Creates an event object.
     *
     * Params:
     *  manualReset  = the state of the event is not reset automatically after resuming waiting clients
     *  initialState = initial state of the signal
     */
    this(bool manualReset, bool initialState)
    {
        initialize(manualReset, initialState);
    }

    /**
     * Initializes an event object. Does nothing if the event is already initialized.
     *
     * Params:
     *  manualReset  = the state of the event is not reset automatically after resuming waiting clients
     *  initialState = initial state of the signal
     */
    void initialize(bool manualReset, bool initialState)
    {
        version (Windows)
        {
            if (m_event)
                return;
            m_event = CreateEvent(null, manualReset, initialState, null);
            m_event || abort("Error: CreateEvent failed.");
        }
        else version (Posix)
        {
            if (m_initalized)
                return;
            pthread_mutex_init(cast(pthread_mutex_t*) &m_mutex, null) == 0 ||
                abort("Error: pthread_mutex_init failed.");
            static if ( is( typeof( pthread_condattr_setclock ) ) )
            {
                pthread_condattr_t attr = void;
                pthread_condattr_init(&attr) == 0 ||
                    abort("Error: pthread_condattr_init failed.");
                pthread_condattr_setclock(&attr, CLOCK_MONOTONIC) == 0 ||
                    abort("Error: pthread_condattr_setclock failed.");
                pthread_cond_init(&m_cond, &attr) == 0 ||
                    abort("Error: pthread_cond_init failed.");
                pthread_condattr_destroy(&attr) == 0 ||
                    abort("Error: pthread_condattr_destroy failed.");
            }
            else
            {
                pthread_cond_init(&m_cond, null) == 0 ||
                    abort("Error: pthread_cond_init failed.");
            }
            m_state = initialState;
            m_manualReset = manualReset;
            m_initalized = true;
        }
    }

    // copying not allowed, can produce resource leaks
    @disable this(this);
    @disable void opAssign(Event);

    ~this()
    {
        terminate();
    }

    /**
     * deinitialize event. Does nothing if the event is not initialized. There must not be
     * threads currently waiting for the event to be signaled.
    */
    void terminate()
    {
        version (Windows)
        {
            if (m_event)
                CloseHandle(m_event);
            m_event = null;
        }
        else version (Posix)
        {
            if (m_initalized)
            {
                pthread_mutex_destroy(&m_mutex) == 0 ||
                    abort("Error: pthread_mutex_destroy failed.");
                pthread_cond_destroy(&m_cond) == 0 ||
                    abort("Error: pthread_cond_destroy failed.");
                m_initalized = false;
            }
        }
    }


    /// Set the event to "signaled", so that waiting clients are resumed
    void set()
    {
        version (Windows)
        {
            if (m_event)
                SetEvent(m_event);
        }
        else version (Posix)
        {
            if (m_initalized)
            {
                pthread_mutex_lock(&m_mutex);
                m_state = true;
                pthread_cond_broadcast(&m_cond);
                pthread_mutex_unlock(&m_mutex);
            }
        }
    }

    /// Reset the event manually
    void reset()
    {
        version (Windows)
        {
            if (m_event)
                ResetEvent(m_event);
        }
        else version (Posix)
        {
            if (m_initalized)
            {
                pthread_mutex_lock(&m_mutex);
                m_state = false;
                pthread_mutex_unlock(&m_mutex);
            }
        }
    }

    /**
     * Wait for the event to be signaled without timeout.
     *
     * Returns:
     *  `true` if the event is in signaled state, `false` if the event is uninitialized or another error occured
     */
    bool wait()
    {
        version (Windows)
        {
            return m_event && WaitForSingleObject(m_event, INFINITE) == WAIT_OBJECT_0;
        }
        else version (Posix)
        {
            return wait(Duration.max);
        }
    }

    /**
     * Wait for the event to be signaled with timeout.
     *
     * Params:
     *  tmout = the maximum time to wait
     * Returns:
     *  `true` if the event is in signaled state, `false` if the event was nonsignaled for the given time or
     *  the event is uninitialized or another error occured
     */
    bool wait(Duration tmout)
    {
        version (Windows)
        {
            if (!m_event)
                return false;

            auto maxWaitMillis = dur!("msecs")(uint.max - 1);

            while (tmout > maxWaitMillis)
            {
                auto res = WaitForSingleObject(m_event, uint.max - 1);
                if (res != WAIT_TIMEOUT)
                    return res == WAIT_OBJECT_0;
                tmout -= maxWaitMillis;
            }
            auto ms = cast(uint)(tmout.total!"msecs");
            return WaitForSingleObject(m_event, ms) == WAIT_OBJECT_0;
        }
        else version (Posix)
        {
            if (!m_initalized)
                return false;

            pthread_mutex_lock(&m_mutex);

            int result = 0;
            if (!m_state)
            {
                if (tmout == Duration.max)
                {
                    result = pthread_cond_wait(&m_cond, &m_mutex);
                }
                else
                {
                    import core.sync.config;

                    timespec t = void;
                    mktspec(t, tmout);

                    result = pthread_cond_timedwait(&m_cond, &m_mutex, &t);
                }
            }
            if (result == 0 && !m_manualReset)
                m_state = false;

            pthread_mutex_unlock(&m_mutex);

            return result == 0;
        }
    }

private:
    version (Windows)
    {
        HANDLE m_event;
    }
    else version (Posix)
    {
        pthread_mutex_t m_mutex;
        pthread_cond_t m_cond;
        bool m_initalized;
        bool m_state;
        bool m_manualReset;
    }
}

// Test single-thread (non-shared) use.
@nogc nothrow unittest
{
    // auto-reset, initial state false
    Event ev1 = Event(false, false);
    assert(!ev1.wait(1.dur!"msecs"));
    ev1.set();
    assert(ev1.wait());
    assert(!ev1.wait(1.dur!"msecs"));

    // manual-reset, initial state true
    Event ev2 = Event(true, true);
    assert(ev2.wait());
    assert(ev2.wait());
    ev2.reset();
    assert(!ev2.wait(1.dur!"msecs"));
}

unittest
{
    import core.thread, core.atomic;

    scope event      = new Event(true, false);
    int  numThreads = 10;
    shared int numRunning = 0;

    void testFn()
    {
        event.wait(8.dur!"seconds"); // timeout below limit for druntime test_runner
        numRunning.atomicOp!"+="(1);
    }

    auto group = new ThreadGroup;

    for (int i = 0; i < numThreads; ++i)
        group.create(&testFn);

    auto start = MonoTime.currTime;
    assert(numRunning == 0);

    event.set();
    group.joinAll();

    assert(numRunning == numThreads);

    assert(MonoTime.currTime - start < 5.dur!"seconds");
}

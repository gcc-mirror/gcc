/**
 * The condition module provides a primitive for synchronized condition
 * checking.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Sean Kelly
 * Source:    $(DRUNTIMESRC core/sync/_condition.d)
 */

/*          Copyright Sean Kelly 2005 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sync.condition;


public import core.sync.exception;
public import core.sync.mutex;
public import core.time;

import core.exception : AssertError, staticError;


version (Windows)
{
    import core.sync.semaphore;
    import core.sys.windows.basetsd /+: HANDLE+/;
    import core.sys.windows.winbase /+: CloseHandle, CreateSemaphoreA, CRITICAL_SECTION,
        DeleteCriticalSection, EnterCriticalSection, INFINITE, InitializeCriticalSection,
        LeaveCriticalSection, ReleaseSemaphore, WAIT_OBJECT_0, WaitForSingleObject+/;
    import core.sys.windows.windef /+: BOOL, DWORD+/;
    import core.sys.windows.winerror /+: WAIT_TIMEOUT+/;
}
else version (Posix)
{
    import core.sync.config;
    import core.stdc.errno;
    import core.sys.posix.pthread;
    import core.sys.posix.time;
}
else
{
    static assert(false, "Platform not supported");
}


////////////////////////////////////////////////////////////////////////////////
// Condition
//
// void wait();
// void notify();
// void notifyAll();
////////////////////////////////////////////////////////////////////////////////

/**
 * This class represents a condition variable as conceived by C.A.R. Hoare.  As
 * per Mesa type monitors however, "signal" has been replaced with "notify" to
 * indicate that control is not transferred to the waiter when a notification
 * is sent.
 */
class Condition
{
    ////////////////////////////////////////////////////////////////////////////
    // Initialization
    ////////////////////////////////////////////////////////////////////////////

    /**
     * Initializes a condition object which is associated with the supplied
     * mutex object.
     *
     * Params:
     *  m = The mutex with which this condition will be associated.
     *
     * Throws:
     *  SyncError on error.
     */
    this( Mutex m ) nothrow @safe @nogc
    {
        this(m, true);
    }

    /// ditto
    this( shared Mutex m ) shared nothrow @safe @nogc
    {
        this(m, true);
    }

    //
    private this(this Q, M)( M m, bool _unused_ ) nothrow @trusted @nogc
        if ((is(Q == Condition) && is(M == Mutex)) ||
            (is(Q == shared Condition) && is(M == shared Mutex)))
    {
        version (Windows)
        {
            static if (is(Q == Condition))
            {
                alias HANDLE_TYPE = void*;
            }
            else
            {
                alias HANDLE_TYPE = shared(void*);
            }
            m_blockLock = cast(HANDLE_TYPE) CreateSemaphoreA( null, 1, 1, null );
            if ( m_blockLock == m_blockLock.init )
                throw staticError!AssertError("Unable to initialize condition", __FILE__, __LINE__);
            scope(failure) CloseHandle( cast(void*) m_blockLock );

            m_blockQueue = cast(HANDLE_TYPE) CreateSemaphoreA( null, 0, int.max, null );
            if ( m_blockQueue == m_blockQueue.init )
                throw staticError!AssertError("Unable to initialize condition", __FILE__, __LINE__);
            scope(failure) CloseHandle( cast(void*) m_blockQueue );

            InitializeCriticalSection( cast(RTL_CRITICAL_SECTION*) &m_unblockLock );
            m_assocMutex = m;
        }
        else version (Posix)
        {
            m_assocMutex = m;
            static if ( is( typeof( pthread_condattr_setclock ) ) )
            {
                () @trusted
                {
                    pthread_condattr_t attr = void;
                    int rc  = pthread_condattr_init( &attr );
                    if ( rc )
                        throw staticError!AssertError("Unable to initialize condition", __FILE__, __LINE__);
                    rc = pthread_condattr_setclock( &attr, CLOCK_MONOTONIC );
                    if ( rc )
                        throw staticError!AssertError("Unable to initialize condition", __FILE__, __LINE__);
                    rc = pthread_cond_init( cast(pthread_cond_t*) &m_hndl, &attr );
                    if ( rc )
                        throw staticError!AssertError("Unable to initialize condition", __FILE__, __LINE__);
                    rc = pthread_condattr_destroy( &attr );
                    if ( rc )
                        throw staticError!AssertError("Unable to initialize condition", __FILE__, __LINE__);
                } ();
            }
            else
            {
                int rc = pthread_cond_init( cast(pthread_cond_t*) &m_hndl, null );
                if ( rc )
                    throw staticError!AssertError("Unable to initialize condition", __FILE__, __LINE__);
            }
        }
    }

    ~this() @nogc
    {
        version (Windows)
        {
            BOOL rc = CloseHandle( m_blockLock );
            assert( rc, "Unable to destroy condition" );
            rc = CloseHandle( m_blockQueue );
            assert( rc, "Unable to destroy condition" );
            DeleteCriticalSection( &m_unblockLock );
        }
        else version (Posix)
        {
            int rc = pthread_cond_destroy( &m_hndl );
            assert( !rc, "Unable to destroy condition" );
        }
    }


    ////////////////////////////////////////////////////////////////////////////
    // General Properties
    ////////////////////////////////////////////////////////////////////////////


    /**
     * Gets the mutex associated with this condition.
     *
     * Returns:
     *  The mutex associated with this condition.
     */
    @property Mutex mutex()
    {
        return m_assocMutex;
    }

    /// ditto
    @property shared(Mutex) mutex() shared
    {
        return m_assocMutex;
    }

    // undocumented function for internal use
    final @property Mutex mutex_nothrow() pure nothrow @safe @nogc
    {
        return m_assocMutex;
    }

    // ditto
    final @property shared(Mutex) mutex_nothrow() shared pure nothrow @safe @nogc
    {
        return m_assocMutex;
    }

    ////////////////////////////////////////////////////////////////////////////
    // General Actions
    ////////////////////////////////////////////////////////////////////////////


    /**
     * Wait until notified.
     *
     * Throws:
     *  SyncError on error.
     */
    void wait()
    {
        wait!(typeof(this))(true);
    }

    /// ditto
    void wait() shared
    {
        wait!(typeof(this))(true);
    }

    /// ditto
    void wait(this Q)( bool _unused_ )
        if (is(Q == Condition) || is(Q == shared Condition))
    {
        version (Windows)
        {
            timedWait( INFINITE );
        }
        else version (Posix)
        {
            int rc = pthread_cond_wait( cast(pthread_cond_t*) &m_hndl, (cast(Mutex) m_assocMutex).handleAddr() );
            if ( rc )
                throw staticError!AssertError("Unable to wait for condition", __FILE__, __LINE__);
        }
    }

    /**
     * Suspends the calling thread until a notification occurs or until the
     * supplied time period has elapsed.
     *
     * Params:
     *  val = The time to wait.
     *
     * In:
     *  val must be non-negative.
     *
     * Throws:
     *  SyncError on error.
     *
     * Returns:
     *  true if notified before the timeout and false if not.
     */
    bool wait( Duration val )
    {
        return wait!(typeof(this))(val, true);
    }

    /// ditto
    bool wait( Duration val ) shared
    {
        return wait!(typeof(this))(val, true);
    }

    /// ditto
    bool wait(this Q)( Duration val, bool _unused_ )
        if (is(Q == Condition) || is(Q == shared Condition))
    in
    {
        assert( !val.isNegative );
    }
    do
    {
        version (Windows)
        {
            auto maxWaitMillis = dur!("msecs")( uint.max - 1 );

            while ( val > maxWaitMillis )
            {
                if ( timedWait( cast(uint)
                               maxWaitMillis.total!"msecs" ) )
                    return true;
                val -= maxWaitMillis;
            }
            return timedWait( cast(uint) val.total!"msecs" );
        }
        else version (Posix)
        {
            timespec t = void;
            mktspec( t, val );

            int rc = pthread_cond_timedwait( cast(pthread_cond_t*) &m_hndl,
                                             (cast(Mutex) m_assocMutex).handleAddr(),
                                             &t );
            if ( !rc )
                return true;
            if ( rc == ETIMEDOUT )
                return false;
            throw staticError!AssertError("Unable to wait for condition", __FILE__, __LINE__);
        }
    }

    /**
     * Notifies one waiter.
     *
     * Throws:
     *  SyncError on error.
     */
    void notify()
    {
        notify!(typeof(this))(true);
    }

    /// ditto
    void notify() shared
    {
        notify!(typeof(this))(true);
    }

    /// ditto
    void notify(this Q)( bool _unused_ )
        if (is(Q == Condition) || is(Q == shared Condition))
    {
        version (Windows)
        {
            notify_( false );
        }
        else version (Posix)
        {
            // Since OS X 10.7 (Lion), pthread_cond_signal returns EAGAIN after retrying 8192 times,
            // so need to retrying while it returns EAGAIN.
            //
            // 10.7.0 (Lion):          http://www.opensource.apple.com/source/Libc/Libc-763.11/pthreads/pthread_cond.c
            // 10.8.0 (Mountain Lion): http://www.opensource.apple.com/source/Libc/Libc-825.24/pthreads/pthread_cond.c
            // 10.10.0 (Yosemite):     http://www.opensource.apple.com/source/libpthread/libpthread-105.1.4/src/pthread_cond.c
            // 10.11.0 (El Capitan):   http://www.opensource.apple.com/source/libpthread/libpthread-137.1.1/src/pthread_cond.c
            // 10.12.0 (Sierra):       http://www.opensource.apple.com/source/libpthread/libpthread-218.1.3/src/pthread_cond.c
            // 10.13.0 (High Sierra):  http://www.opensource.apple.com/source/libpthread/libpthread-301.1.6/src/pthread_cond.c
            // 10.14.0 (Mojave):       http://www.opensource.apple.com/source/libpthread/libpthread-330.201.1/src/pthread_cond.c
            // 10.14.1 (Mojave):       http://www.opensource.apple.com/source/libpthread/libpthread-330.220.2/src/pthread_cond.c

            int rc;
            do {
                rc = pthread_cond_signal( cast(pthread_cond_t*) &m_hndl );
            } while ( rc == EAGAIN );
            if ( rc )
                throw staticError!AssertError("Unable to notify condition", __FILE__, __LINE__);
        }
    }

    /**
     * Notifies all waiters.
     *
     * Throws:
     *  SyncError on error.
     */
    void notifyAll()
    {
        notifyAll!(typeof(this))(true);
    }

    /// ditto
    void notifyAll() shared
    {
        notifyAll!(typeof(this))(true);
    }

    /// ditto
    void notifyAll(this Q)( bool _unused_ )
        if (is(Q == Condition) || is(Q == shared Condition))
    {
        version (Windows)
        {
            notify_( true );
        }
        else version (Posix)
        {
            // Since OS X 10.7 (Lion), pthread_cond_broadcast returns EAGAIN after retrying 8192 times,
            // so need to retrying while it returns EAGAIN.
            //
            // 10.7.0 (Lion):          http://www.opensource.apple.com/source/Libc/Libc-763.11/pthreads/pthread_cond.c
            // 10.8.0 (Mountain Lion): http://www.opensource.apple.com/source/Libc/Libc-825.24/pthreads/pthread_cond.c
            // 10.10.0 (Yosemite):     http://www.opensource.apple.com/source/libpthread/libpthread-105.1.4/src/pthread_cond.c
            // 10.11.0 (El Capitan):   http://www.opensource.apple.com/source/libpthread/libpthread-137.1.1/src/pthread_cond.c
            // 10.12.0 (Sierra):       http://www.opensource.apple.com/source/libpthread/libpthread-218.1.3/src/pthread_cond.c
            // 10.13.0 (High Sierra):  http://www.opensource.apple.com/source/libpthread/libpthread-301.1.6/src/pthread_cond.c
            // 10.14.0 (Mojave):       http://www.opensource.apple.com/source/libpthread/libpthread-330.201.1/src/pthread_cond.c
            // 10.14.1 (Mojave):       http://www.opensource.apple.com/source/libpthread/libpthread-330.220.2/src/pthread_cond.c

            int rc;
            do {
                rc = pthread_cond_broadcast( cast(pthread_cond_t*) &m_hndl );
            } while ( rc == EAGAIN );
            if ( rc )
                throw staticError!AssertError("Unable to notify condition", __FILE__, __LINE__);
        }
    }

private:
    version (Windows)
    {
        bool timedWait(this Q)( DWORD timeout )
            if (is(Q == Condition) || is(Q == shared Condition))
        {
            static if (is(Q == Condition))
            {
                auto op(string o, T, V1)(ref T val, V1 mod)
                {
                    return mixin("val " ~ o ~ "mod");
                }
            }
            else
            {
                auto op(string o, T, V1)(ref shared T val, V1 mod)
                {
                    import core.atomic: atomicOp;
                    return atomicOp!o(val, mod);
                }
            }

            int   numSignalsLeft;
            int   numWaitersGone;
            DWORD rc;

            rc = WaitForSingleObject( cast(HANDLE) m_blockLock, INFINITE );
            assert( rc == WAIT_OBJECT_0 );

            op!"+="(m_numWaitersBlocked, 1);

            rc = ReleaseSemaphore( cast(HANDLE) m_blockLock, 1, null );
            assert( rc );

            m_assocMutex.unlock();
            scope(failure) m_assocMutex.lock();

            rc = WaitForSingleObject( cast(HANDLE) m_blockQueue, timeout );
            assert( rc == WAIT_OBJECT_0 || rc == WAIT_TIMEOUT );
            bool timedOut = (rc == WAIT_TIMEOUT);

            EnterCriticalSection( &m_unblockLock );
            scope(failure) LeaveCriticalSection( &m_unblockLock );

            if ( (numSignalsLeft = m_numWaitersToUnblock) != 0 )
            {
                if ( timedOut )
                {
                    // timeout (or canceled)
                    if ( m_numWaitersBlocked != 0 )
                    {
                        op!"-="(m_numWaitersBlocked, 1);
                        // do not unblock next waiter below (already unblocked)
                        numSignalsLeft = 0;
                    }
                    else
                    {
                        // spurious wakeup pending!!
                        m_numWaitersGone = 1;
                    }
                }
                if ( op!"-="(m_numWaitersToUnblock, 1) == 0 )
                {
                    if ( m_numWaitersBlocked != 0 )
                    {
                        // open the gate
                        rc = ReleaseSemaphore( cast(HANDLE) m_blockLock, 1, null );
                        assert( rc );
                        // do not open the gate below again
                        numSignalsLeft = 0;
                    }
                    else if ( (numWaitersGone = m_numWaitersGone) != 0 )
                    {
                        m_numWaitersGone = 0;
                    }
                }
            }
            else if ( op!"+="(m_numWaitersGone, 1) == int.max / 2 )
            {
                // timeout/canceled or spurious event :-)
                rc = WaitForSingleObject( cast(HANDLE) m_blockLock, INFINITE );
                assert( rc == WAIT_OBJECT_0 );
                // something is going on here - test of timeouts?
                op!"-="(m_numWaitersBlocked, m_numWaitersGone);
                rc = ReleaseSemaphore( cast(HANDLE) m_blockLock, 1, null );
                assert( rc == WAIT_OBJECT_0 );
                m_numWaitersGone = 0;
            }

            LeaveCriticalSection( &m_unblockLock );

            if ( numSignalsLeft == 1 )
            {
                // better now than spurious later (same as ResetEvent)
                for ( ; numWaitersGone > 0; --numWaitersGone )
                {
                    rc = WaitForSingleObject( cast(HANDLE) m_blockQueue, INFINITE );
                    assert( rc == WAIT_OBJECT_0 );
                }
                // open the gate
                rc = ReleaseSemaphore( cast(HANDLE) m_blockLock, 1, null );
                assert( rc );
            }
            else if ( numSignalsLeft != 0 )
            {
                // unblock next waiter
                rc = ReleaseSemaphore( cast(HANDLE) m_blockQueue, 1, null );
                assert( rc );
            }
            m_assocMutex.lock();
            return !timedOut;
        }


        void notify_(this Q)( bool all )
            if (is(Q == Condition) || is(Q == shared Condition))
        {
            static if (is(Q == Condition))
            {
                auto op(string o, T, V1)(ref T val, V1 mod)
                {
                    return mixin("val " ~ o ~ "mod");
                }
            }
            else
            {
                auto op(string o, T, V1)(ref shared T val, V1 mod)
                {
                    import core.atomic: atomicOp;
                    return atomicOp!o(val, mod);
                }
            }

            DWORD rc;

            EnterCriticalSection( &m_unblockLock );
            scope(failure) LeaveCriticalSection( &m_unblockLock );

            if ( m_numWaitersToUnblock != 0 )
            {
                if ( m_numWaitersBlocked == 0 )
                {
                    LeaveCriticalSection( &m_unblockLock );
                    return;
                }
                if ( all )
                {
                    op!"+="(m_numWaitersToUnblock, m_numWaitersBlocked);
                    m_numWaitersBlocked = 0;
                }
                else
                {
                    op!"+="(m_numWaitersToUnblock, 1);
                    op!"-="(m_numWaitersBlocked, 1);
                }
                LeaveCriticalSection( &m_unblockLock );
            }
            else if ( m_numWaitersBlocked > m_numWaitersGone )
            {
                rc = WaitForSingleObject( cast(HANDLE) m_blockLock, INFINITE );
                assert( rc == WAIT_OBJECT_0 );
                if ( 0 != m_numWaitersGone )
                {
                    op!"-="(m_numWaitersBlocked, m_numWaitersGone);
                    m_numWaitersGone = 0;
                }
                if ( all )
                {
                    m_numWaitersToUnblock = m_numWaitersBlocked;
                    m_numWaitersBlocked = 0;
                }
                else
                {
                    m_numWaitersToUnblock = 1;
                    op!"-="(m_numWaitersBlocked, 1);
                }
                LeaveCriticalSection( &m_unblockLock );
                rc = ReleaseSemaphore( cast(HANDLE) m_blockQueue, 1, null );
                assert( rc );
            }
            else
            {
                LeaveCriticalSection( &m_unblockLock );
            }
        }


        // NOTE: This implementation uses Algorithm 8c as described here:
        //       http://groups.google.com/group/comp.programming.threads/
        //              browse_frm/thread/1692bdec8040ba40/e7a5f9d40e86503a
        HANDLE              m_blockLock;    // auto-reset event (now semaphore)
        HANDLE              m_blockQueue;   // auto-reset event (now semaphore)
        Mutex               m_assocMutex;   // external mutex/CS
        CRITICAL_SECTION    m_unblockLock;  // internal mutex/CS
        int                 m_numWaitersGone        = 0;
        int                 m_numWaitersBlocked     = 0;
        int                 m_numWaitersToUnblock   = 0;
    }
    else version (Posix)
    {
        Mutex               m_assocMutex;
        pthread_cond_t      m_hndl;
    }
}


////////////////////////////////////////////////////////////////////////////////
// Unit Tests
////////////////////////////////////////////////////////////////////////////////

unittest
{
    import core.thread;
    import core.sync.mutex;
    import core.sync.semaphore;


    void testNotify()
    {
        auto mutex      = new Mutex;
        auto condReady  = new Condition( mutex );
        auto semDone    = new Semaphore;
        auto synLoop    = new Object;
        int  numWaiters = 10;
        int  numTries   = 10;
        int  numReady   = 0;
        int  numTotal   = 0;
        int  numDone    = 0;
        int  numPost    = 0;

        void waiter()
        {
            for ( int i = 0; i < numTries; ++i )
            {
                synchronized( mutex )
                {
                    while ( numReady < 1 )
                    {
                        condReady.wait();
                    }
                    --numReady;
                    ++numTotal;
                }

                synchronized( synLoop )
                {
                    ++numDone;
                }
                semDone.wait();
            }
        }

        auto group = new ThreadGroup;

        for ( int i = 0; i < numWaiters; ++i )
            group.create( &waiter );

        for ( int i = 0; i < numTries; ++i )
        {
            for ( int j = 0; j < numWaiters; ++j )
            {
                synchronized( mutex )
                {
                    ++numReady;
                    condReady.notify();
                }
            }
            while ( true )
            {
                synchronized( synLoop )
                {
                    if ( numDone >= numWaiters )
                        break;
                }
                Thread.yield();
            }
            for ( int j = 0; j < numWaiters; ++j )
            {
                semDone.notify();
            }
        }

        group.joinAll();
        assert( numTotal == numWaiters * numTries );
    }


    void testNotifyAll()
    {
        auto mutex      = new Mutex;
        auto condReady  = new Condition( mutex );
        int  numWaiters = 10;
        int  numReady   = 0;
        int  numDone    = 0;
        bool alert      = false;

        void waiter()
        {
            synchronized( mutex )
            {
                ++numReady;
                while ( !alert )
                    condReady.wait();
                ++numDone;
            }
        }

        auto group = new ThreadGroup;

        for ( int i = 0; i < numWaiters; ++i )
            group.create( &waiter );

        while ( true )
        {
            synchronized( mutex )
            {
                if ( numReady >= numWaiters )
                {
                    alert = true;
                    condReady.notifyAll();
                    break;
                }
            }
            Thread.yield();
        }
        group.joinAll();
        assert( numReady == numWaiters && numDone == numWaiters );
    }


    void testWaitTimeout()
    {
        auto mutex      = new Mutex;
        auto condReady  = new Condition( mutex );
        bool waiting    = false;
        bool alertedOne = true;
        bool alertedTwo = true;

        void waiter()
        {
            synchronized( mutex )
            {
                waiting    = true;
                // we never want to miss the notification (30s)
                alertedOne = condReady.wait( dur!"seconds"(30) );
                // but we don't want to wait long for the timeout (10ms)
                alertedTwo = condReady.wait( dur!"msecs"(10) );
            }
        }

        auto thread = new Thread( &waiter );
        thread.start();

        while ( true )
        {
            synchronized( mutex )
            {
                if ( waiting )
                {
                    condReady.notify();
                    break;
                }
            }
            Thread.yield();
        }
        thread.join();
        assert( waiting );
        assert( alertedOne );
        assert( !alertedTwo );
    }

    testNotify();
    testNotifyAll();
    testWaitTimeout();
}

unittest
{
    import core.thread;
    import core.sync.mutex;
    import core.sync.semaphore;


    void testNotify()
    {
        auto mutex      = new shared Mutex;
        auto condReady  = new shared Condition( mutex );
        auto semDone    = new Semaphore;
        auto synLoop    = new Object;
        int  numWaiters = 10;
        int  numTries   = 10;
        int  numReady   = 0;
        int  numTotal   = 0;
        int  numDone    = 0;
        int  numPost    = 0;

        void waiter()
        {
            for ( int i = 0; i < numTries; ++i )
            {
                synchronized( mutex )
                {
                    while ( numReady < 1 )
                    {
                        condReady.wait();
                    }
                    --numReady;
                    ++numTotal;
                }

                synchronized( synLoop )
                {
                    ++numDone;
                }
                semDone.wait();
            }
        }

        auto group = new ThreadGroup;

        for ( int i = 0; i < numWaiters; ++i )
            group.create( &waiter );

        for ( int i = 0; i < numTries; ++i )
        {
            for ( int j = 0; j < numWaiters; ++j )
            {
                synchronized( mutex )
                {
                    ++numReady;
                    condReady.notify();
                }
            }
            while ( true )
            {
                synchronized( synLoop )
                {
                    if ( numDone >= numWaiters )
                        break;
                }
                Thread.yield();
            }
            for ( int j = 0; j < numWaiters; ++j )
            {
                semDone.notify();
            }
        }

        group.joinAll();
        assert( numTotal == numWaiters * numTries );
    }


    void testNotifyAll()
    {
        auto mutex      = new shared Mutex;
        auto condReady  = new shared Condition( mutex );
        int  numWaiters = 10;
        int  numReady   = 0;
        int  numDone    = 0;
        bool alert      = false;

        void waiter()
        {
            synchronized( mutex )
            {
                ++numReady;
                while ( !alert )
                    condReady.wait();
                ++numDone;
            }
        }

        auto group = new ThreadGroup;

        for ( int i = 0; i < numWaiters; ++i )
            group.create( &waiter );

        while ( true )
        {
            synchronized( mutex )
            {
                if ( numReady >= numWaiters )
                {
                    alert = true;
                    condReady.notifyAll();
                    break;
                }
            }
            Thread.yield();
        }
        group.joinAll();
        assert( numReady == numWaiters && numDone == numWaiters );
    }


    void testWaitTimeout()
    {
        auto mutex      = new shared Mutex;
        auto condReady  = new shared Condition( mutex );
        bool waiting    = false;
        bool alertedOne = true;
        bool alertedTwo = true;

        void waiter()
        {
            synchronized( mutex )
            {
                waiting    = true;
                // we never want to miss the notification (30s)
                alertedOne = condReady.wait( dur!"seconds"(30) );
                // but we don't want to wait long for the timeout (10ms)
                alertedTwo = condReady.wait( dur!"msecs"(10) );
            }
        }

        auto thread = new Thread( &waiter );
        thread.start();

        while ( true )
        {
            synchronized( mutex )
            {
                if ( waiting )
                {
                    condReady.notify();
                    break;
                }
            }
            Thread.yield();
        }
        thread.join();
        assert( waiting );
        assert( alertedOne );
        assert( !alertedTwo );
    }

    testNotify();
    testNotifyAll();
    testWaitTimeout();
}

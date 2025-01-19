/**
 * The semaphore module provides a general use semaphore for synchronization.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Sean Kelly
 * Source:    $(DRUNTIMESRC core/sync/_semaphore.d)
 */

/*          Copyright Sean Kelly 2005 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sync.semaphore;


public import core.sync.exception;
public import core.time;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Windows)
{
    import core.sys.windows.basetsd /+: HANDLE+/;
    import core.sys.windows.winbase /+: CloseHandle, CreateSemaphoreA, INFINITE,
        ReleaseSemaphore, WAIT_OBJECT_0, WaitForSingleObject+/;
    import core.sys.windows.windef /+: BOOL, DWORD+/;
    import core.sys.windows.winerror /+: WAIT_TIMEOUT+/;
}
else version (Darwin)
{
    import core.stdc.errno : EINTR, errno;
    import core.sync.config;
    import core.sys.darwin.mach.kern_return : KERN_ABORTED, KERN_OPERATION_TIMED_OUT;
    import core.sys.darwin.mach.semaphore : mach_task_self, mach_timespec_t, semaphore_create, semaphore_destroy,
        semaphore_signal, semaphore_t, semaphore_timedwait, semaphore_wait, SYNC_POLICY_FIFO;
}
else version (Posix)
{
    import core.stdc.errno : EAGAIN, EINTR, errno, ETIMEDOUT;
    import core.sync.config;
    import core.sys.posix.semaphore : sem_destroy, sem_init, sem_post, sem_t, sem_timedwait, sem_trywait, sem_wait;
    import core.sys.posix.time : clock_gettime, CLOCK_REALTIME, timespec;
}
else
{
    static assert(false, "Platform not supported");
}


////////////////////////////////////////////////////////////////////////////////
// Semaphore
//
// void wait();
// void notify();
// bool tryWait();
////////////////////////////////////////////////////////////////////////////////


/**
 * This class represents a general counting semaphore as concieved by Edsger
 * Dijkstra.  As per Mesa type monitors however, "signal" has been replaced
 * with "notify" to indicate that control is not transferred to the waiter when
 * a notification is sent.
 */
class Semaphore
{
    ////////////////////////////////////////////////////////////////////////////
    // Initialization
    ////////////////////////////////////////////////////////////////////////////


    /**
     * Initializes a semaphore object with the specified initial count.
     *
     * Params:
     *  count = The initial count for the semaphore.
     *
     * Throws:
     *  SyncError on error.
     */
    this( uint count = 0 )
    {
        version (Windows)
        {
            m_hndl = CreateSemaphoreA( null, count, int.max, null );
            if ( m_hndl == m_hndl.init )
                throw new SyncError( "Unable to create semaphore" );
        }
        else version (Darwin)
        {
            auto rc = semaphore_create( mach_task_self(), &m_hndl, SYNC_POLICY_FIFO, count );
            if ( rc )
                throw new SyncError( "Unable to create semaphore" );
        }
        else version (Posix)
        {
            int rc = sem_init( &m_hndl, 0, count );
            if ( rc )
                throw new SyncError( "Unable to create semaphore" );
        }
    }


    ~this()
    {
        version (Windows)
        {
            BOOL rc = CloseHandle( m_hndl );
            assert( rc, "Unable to destroy semaphore" );
        }
        else version (Darwin)
        {
            auto rc = semaphore_destroy( mach_task_self(), m_hndl );
            assert( !rc, "Unable to destroy semaphore" );
        }
        else version (Posix)
        {
            int rc = sem_destroy( &m_hndl );
            assert( !rc, "Unable to destroy semaphore" );
        }
    }


    ////////////////////////////////////////////////////////////////////////////
    // General Actions
    ////////////////////////////////////////////////////////////////////////////


    /**
     * Wait until the current count is above zero, then atomically decrement
     * the count by one and return.
     *
     * Throws:
     *  SyncError on error.
     */
    void wait()
    {
        version (Windows)
        {
            DWORD rc = WaitForSingleObject( m_hndl, INFINITE );
            if ( rc != WAIT_OBJECT_0 )
                throw new SyncError( "Unable to wait for semaphore" );
        }
        else version (Darwin)
        {
            while ( true )
            {
                auto rc = semaphore_wait( m_hndl );
                if ( !rc )
                    return;
                if ( rc == KERN_ABORTED && errno == EINTR )
                    continue;
                throw new SyncError( "Unable to wait for semaphore" );
            }
        }
        else version (Posix)
        {
            while ( true )
            {
                if ( !sem_wait( &m_hndl ) )
                    return;
                if ( errno != EINTR )
                    throw new SyncError( "Unable to wait for semaphore" );
            }
        }
    }


    /**
     * Suspends the calling thread until the current count moves above zero or
     * until the supplied time period has elapsed.  If the count moves above
     * zero in this interval, then atomically decrement the count by one and
     * return true.  Otherwise, return false.
     *
     * Params:
     *  period = The time to wait.
     *
     * In:
     *  period must be non-negative.
     *
     * Throws:
     *  SyncError on error.
     *
     * Returns:
     *  true if notified before the timeout and false if not.
     */
    bool wait( Duration period )
    in
    {
        assert( !period.isNegative );
    }
    do
    {
        version (Windows)
        {
            auto maxWaitMillis = dur!("msecs")( uint.max - 1 );

            while ( period > maxWaitMillis )
            {
                auto rc = WaitForSingleObject( m_hndl, cast(uint)
                                                       maxWaitMillis.total!"msecs" );
                switch ( rc )
                {
                case WAIT_OBJECT_0:
                    return true;
                case WAIT_TIMEOUT:
                    period -= maxWaitMillis;
                    continue;
                default:
                    throw new SyncError( "Unable to wait for semaphore" );
                }
            }
            switch ( WaitForSingleObject( m_hndl, cast(uint) period.total!"msecs" ) )
            {
            case WAIT_OBJECT_0:
                return true;
            case WAIT_TIMEOUT:
                return false;
            default:
                throw new SyncError( "Unable to wait for semaphore" );
            }
        }
        else version (Darwin)
        {
            mach_timespec_t t = void;
            (cast(byte*) &t)[0 .. t.sizeof] = 0;

            if ( period.total!"seconds" > t.tv_sec.max )
            {
                t.tv_sec  = t.tv_sec.max;
                t.tv_nsec = cast(typeof(t.tv_nsec)) period.split!("seconds", "nsecs")().nsecs;
            }
            else
                period.split!("seconds", "nsecs")(t.tv_sec, t.tv_nsec);
            while ( true )
            {
                auto rc = semaphore_timedwait( m_hndl, t );
                if ( !rc )
                    return true;
                if ( rc == KERN_OPERATION_TIMED_OUT )
                    return false;
                if ( rc != KERN_ABORTED || errno != EINTR )
                    throw new SyncError( "Unable to wait for semaphore" );
            }
        }
        else version (Posix)
        {
            timespec t = void;
            clock_gettime( CLOCK_REALTIME, &t );
            mvtspec( t, period );

            while ( true )
            {
                if ( !sem_timedwait( &m_hndl, &t ) )
                    return true;
                if ( errno == ETIMEDOUT )
                    return false;
                if ( errno != EINTR )
                    throw new SyncError( "Unable to wait for semaphore" );
            }
        }
    }


    /**
     * Atomically increment the current count by one.  This will notify one
     * waiter, if there are any in the queue.
     *
     * Throws:
     *  SyncError on error.
     */
    void notify()
    {
        version (Windows)
        {
            if ( !ReleaseSemaphore( m_hndl, 1, null ) )
                throw new SyncError( "Unable to notify semaphore" );
        }
        else version (Darwin)
        {
            auto rc = semaphore_signal( m_hndl );
            if ( rc )
                throw new SyncError( "Unable to notify semaphore" );
        }
        else version (Posix)
        {
            int rc = sem_post( &m_hndl );
            if ( rc )
                throw new SyncError( "Unable to notify semaphore" );
        }
    }


    /**
     * If the current count is equal to zero, return.  Otherwise, atomically
     * decrement the count by one and return true.
     *
     * Throws:
     *  SyncError on error.
     *
     * Returns:
     *  true if the count was above zero and false if not.
     */
    bool tryWait()
    {
        version (Windows)
        {
            switch ( WaitForSingleObject( m_hndl, 0 ) )
            {
            case WAIT_OBJECT_0:
                return true;
            case WAIT_TIMEOUT:
                return false;
            default:
                throw new SyncError( "Unable to wait for semaphore" );
            }
        }
        else version (Darwin)
        {
            return wait( dur!"hnsecs"(0) );
        }
        else version (Posix)
        {
            while ( true )
            {
                if ( !sem_trywait( &m_hndl ) )
                    return true;
                if ( errno == EAGAIN )
                    return false;
                if ( errno != EINTR )
                    throw new SyncError( "Unable to wait for semaphore" );
            }
        }
    }


protected:

    /// Aliases the operating-system-specific semaphore type.
    version (Windows)        alias Handle = HANDLE;
    /// ditto
    else version (Darwin)    alias Handle = semaphore_t;
    /// ditto
    else version (Posix)     alias Handle = sem_t;

    /// Handle to the system-specific semaphore.
    Handle m_hndl;
}


////////////////////////////////////////////////////////////////////////////////
// Unit Tests
////////////////////////////////////////////////////////////////////////////////

unittest
{
    import core.atomic;
    import core.thread;

    void testWait()
    {
        auto semaphore = new Semaphore;
        shared bool stopConsumption = false;
        immutable numToProduce = 20;
        immutable numConsumers = 10;
        shared size_t numConsumed;
        shared size_t numComplete;

        void consumer()
        {
            while (true)
            {
                semaphore.wait();

                if (atomicLoad(stopConsumption))
                    break;
                atomicOp!"+="(numConsumed, 1);
            }
            atomicOp!"+="(numComplete, 1);
        }

        void producer()
        {
            assert(!semaphore.tryWait());

            foreach (_; 0 .. numToProduce)
                semaphore.notify();

            // wait until all items are consumed
            while (atomicLoad(numConsumed) != numToProduce)
                Thread.yield();

            // mark consumption as finished
            atomicStore(stopConsumption, true);

            // wake all consumers
            foreach (_; 0 .. numConsumers)
                semaphore.notify();

            // wait until all consumers completed
            while (atomicLoad(numComplete) != numConsumers)
                Thread.yield();

            assert(!semaphore.tryWait());
            semaphore.notify();
            assert(semaphore.tryWait());
            assert(!semaphore.tryWait());
        }

        auto group = new ThreadGroup;

        for ( int i = 0; i < numConsumers; ++i )
            group.create(&consumer);
        group.create(&producer);
        group.joinAll();
    }


    void testWaitTimeout()
    {
        auto sem = new Semaphore;
        shared bool semReady;
        bool alertedOne, alertedTwo;

        void waiter()
        {
            while (!atomicLoad(semReady))
                Thread.yield();
            alertedOne = sem.wait(dur!"msecs"(1));
            alertedTwo = sem.wait(dur!"msecs"(1));
            assert(alertedOne && !alertedTwo);
        }

        auto thread = new Thread(&waiter);
        thread.start();

        sem.notify();
        atomicStore(semReady, true);
        thread.join();
        assert(alertedOne && !alertedTwo);
    }

    testWait();
    testWaitTimeout();
}

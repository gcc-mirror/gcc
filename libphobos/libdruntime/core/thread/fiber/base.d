/**
 * Base fiber module provides OS-indepedent part of lightweight threads aka fibers.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2012.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly, Walter Bright, Alex Rønne Petersen, Martin Nowak
 * Source:    $(DRUNTIMESRC core/thread/fiber/base.d)
 */

/* NOTE: This file has been patched from the original DMD distribution to
 * work with the GDC compiler.
 */
module core.thread.fiber.base;

package:
version (GNU)
    import gcc.config;

import core.thread.fiber;
import core.thread.threadbase;
import core.thread.threadgroup;
import core.thread.types;
import core.thread.context;

import core.memory : pageSize;

package
{
    import core.atomic : atomicStore, cas, MemoryOrder;
    import core.exception : onOutOfMemoryError;
    import core.stdc.stdlib : abort;

    extern (C) void fiber_entryPoint() nothrow
    {
        FiberBase obj = FiberBase.getThis();
        assert( obj );

        assert( ThreadBase.getThis().m_curr is obj.m_ctxt );
        atomicStore!(MemoryOrder.raw)(*cast(shared)&ThreadBase.getThis().m_lock, false);
        obj.m_ctxt.tstack = obj.m_ctxt.bstack;
        obj.m_state = FiberBase.State.EXEC;

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
 * Authors: Based on a design by Mikola Lysenko.
 */
class FiberBase
{
    /**
     * Initializes a fiber object which is associated with a static
     * D function.
     *
     * Params:
     *  fn = The fiber function.
     *  sz = The stack size for this fiber.
     *  guardPageSize = size of the guard page to trap fiber's stack
     *                  overflows. Beware that using this will increase
     *                  the number of mmaped regions on platforms using mmap
     *                  so an OS-imposed limit may be hit.
     *
     * In:
     *  fn must not be null.
     */
    this( void function() fn, size_t sz, size_t guardPageSize ) nothrow
    in
    {
        assert( fn );
    }
    do
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
     *                  overflows. Beware that using this will increase
     *                  the number of mmaped regions on platforms using mmap
     *                  so an OS-imposed limit may be hit.
     *
     * In:
     *  dg must not be null.
     */
    this( void delegate() dg, size_t sz, size_t guardPageSize ) nothrow
    {
        allocStack( sz, guardPageSize );
        reset( cast(void delegate() const) dg );
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

    private void callImpl() nothrow @nogc
    in
    {
        assert( m_state == State.HOLD );
    }
    do
    {
        FiberBase cur = getThis();

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
    do
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
        m_call  = fn;
    }

    /// ditto
    final void reset( void delegate() dg ) nothrow @nogc
    {
        reset();
        m_call  = dg;
    }

    ///////////////////////////////////////////////////////////////////////////
    // General Properties
    ///////////////////////////////////////////////////////////////////////////


    /// A fiber may occupy one of three states: HOLD, EXEC, and TERM.
    enum State
    {
        /** The HOLD state applies to any fiber that is suspended and ready to
        be called. */
        HOLD,
        /** The EXEC state will be set for any fiber that is currently
        executing. */
        EXEC,
        /** The TERM state is set when a fiber terminates. Once a fiber
        terminates, it must be reset before it may be called again. */
        TERM
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
        FiberBase cur = getThis();
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
    do
    {
        FiberBase cur = getThis();
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
    static FiberBase getThis() @safe nothrow @nogc
    {
        version (GNU) pragma(inline, false);
        return sm_this;
    }


private:

    //
    // Fiber entry point.  Invokes the function or delegate passed on
    // construction (if any).
    //
    final void run()
    {
        m_call();
    }

    //
    // Standard fiber data
    //
    Callable            m_call;
    bool                m_isRunning;
    Throwable           m_unhandled;
    State               m_state;


protected:
    ///////////////////////////////////////////////////////////////////////////
    // Stack Management
    ///////////////////////////////////////////////////////////////////////////


    //
    // Allocate a new stack for this fiber.
    //
    abstract void allocStack( size_t sz, size_t guardPageSize ) nothrow;


    //
    // Free this fiber's stack.
    //
    abstract void freeStack() nothrow @nogc;


    //
    // Initialize the allocated stack.
    // Look above the definition of 'class Fiber' for some information about the implementation of this routine
    //
    abstract void initStack() nothrow @nogc;


    StackContext*   m_ctxt;
    size_t          m_size;
    void*           m_pmem;

    static if ( __traits( compiles, ucontext_t ) )
    {
        // NOTE: The static ucontext instance is used to represent the context
        //       of the executing thread.
        static ucontext_t       sm_utxt = void;
        ucontext_t              m_utxt  = void;
        package ucontext_t*     m_ucur  = null;
    }
    else static if (GNU_Enable_CET)
    {
        // When libphobos was built with --enable-cet, these fields need to
        // always be present in the Fiber class layout.
        import core.sys.posix.ucontext;
        static ucontext_t       sm_utxt = void;
        ucontext_t              m_utxt  = void;
        package ucontext_t*     m_ucur  = null;
    }


private:
    ///////////////////////////////////////////////////////////////////////////
    // Storage of Active Fiber
    ///////////////////////////////////////////////////////////////////////////


    //
    // Sets a thread-local reference to the current fiber object.
    //
    static void setThis( FiberBase f ) nothrow @nogc
    {
        sm_this = f;
    }

    static FiberBase sm_this;


private:
    ///////////////////////////////////////////////////////////////////////////
    // Context Switching
    ///////////////////////////////////////////////////////////////////////////


    //
    // Switches into the stack held by this fiber.
    //
    final void switchIn() nothrow @nogc
    {
        ThreadBase  tobj = ThreadBase.getThis();
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
        ThreadBase  tobj = ThreadBase.getThis();
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
        tobj = ThreadBase.getThis();
        atomicStore!(MemoryOrder.raw)(*cast(shared)&tobj.m_lock, false);
        tobj.m_curr.tstack = tobj.m_curr.bstack;
    }
}

///
unittest {
    int counter;

    class DerivedFiber : Fiber
    {
        this()
        {
            super( &run );
        }

    private :
        void run()
        {
            counter += 2;
        }
    }

    void fiberFunc()
    {
        counter += 4;
        Fiber.yield();
        counter += 8;
    }

    // create instances of each type
    Fiber derived = new DerivedFiber();
    Fiber composed = new Fiber( &fiberFunc );

    assert( counter == 0 );

    derived.call();
    assert( counter == 2, "Derived fiber increment." );

    composed.call();
    assert( counter == 6, "First composed fiber increment." );

    counter += 16;
    assert( counter == 22, "Calling context increment." );

    composed.call();
    assert( counter == 30, "Second composed fiber increment." );

    // since each fiber has run to completion, each should have state TERM
    assert( derived.state == Fiber.State.TERM );
    assert( composed.state == Fiber.State.TERM );
}

version (unittest)
{
    import core.thread.fiber: Fiber;
}

version (CoreUnittest)
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
version (OSX)
{
    version (X86)    version = UnsafeFiberMigration;
    version (X86_64) version = UnsafeFiberMigration;
    version (AArch64) version = UnsafeFiberMigration;
}

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
unittest
{
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

unittest
{
    enum MSG = "Test message.";

    try
    {
        (new Fiber(function() {
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
    import core.thread.osthread : Thread;
    import core.time : dur;

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

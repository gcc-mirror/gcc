/**
$(D std._parallelism) implements high-level primitives for SMP _parallelism.
These include parallel foreach, parallel reduce, parallel eager map, pipelining
and future/promise _parallelism.  $(D std._parallelism) is recommended when the
same operation is to be executed in parallel on different data, or when a
function is to be executed in a background thread and its result returned to a
well-defined main thread.  For communication between arbitrary threads, see
$(D std.concurrency).

$(D std._parallelism) is based on the concept of a $(D Task).  A $(D Task) is an
object that represents the fundamental unit of work in this library and may be
executed in parallel with any other $(D Task).  Using $(D Task)
directly allows programming with a future/promise paradigm.  All other
supported _parallelism paradigms (parallel foreach, map, reduce, pipelining)
represent an additional level of abstraction over $(D Task).  They
automatically create one or more $(D Task) objects, or closely related types
that are conceptually identical but not part of the public API.

After creation, a $(D Task) may be executed in a new thread, or submitted
to a $(D TaskPool) for execution.  A $(D TaskPool) encapsulates a task queue
and its worker threads.  Its purpose is to efficiently map a large
number of $(D Task)s onto a smaller number of threads.  A task queue is a
FIFO queue of $(D Task) objects that have been submitted to the
$(D TaskPool) and are awaiting execution.  A worker thread is a thread that
is associated with exactly one task queue.  It executes the $(D Task) at the
front of its queue when the queue has work available, or sleeps when
no work is available.  Each task queue is associated with zero or
more worker threads.  If the result of a $(D Task) is needed before execution
by a worker thread has begun, the $(D Task) can be removed from the task queue
and executed immediately in the thread where the result is needed.

Warning:  Unless marked as $(D @trusted) or $(D @safe), artifacts in
          this module allow implicit data sharing between threads and cannot
          guarantee that client code is free from low level data races.

Source:    $(PHOBOSSRC std/_parallelism.d)
Author:  David Simcha
Copyright:  Copyright (c) 2009-2011, David Simcha.
License:    $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0)
*/
module std.parallelism;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

///
@system unittest
{
    import std.algorithm.iteration : map;
    import std.math : approxEqual;
    import std.parallelism : taskPool;
    import std.range : iota;

    // Parallel reduce can be combined with
    // std.algorithm.iteration.map to interesting effect.
    // The following example (thanks to Russel Winder)
    // calculates pi by quadrature  using
    // std.algorithm.map and TaskPool.reduce.
    // getTerm is evaluated in parallel as needed by
    // TaskPool.reduce.
    //
    // Timings on an Intel i5-3450 quad core machine
    // for n = 1_000_000_000:
    //
    // TaskPool.reduce:       1.067 s
    // std.algorithm.reduce:  4.011 s

    enum n = 1_000_000;
    enum delta = 1.0 / n;

    alias getTerm = (int i)
    {
        immutable x = ( i - 0.5 ) * delta;
        return delta / ( 1.0 + x * x ) ;
    };

    immutable pi = 4.0 * taskPool.reduce!"a + b"(n.iota.map!getTerm);

    assert(pi.approxEqual(3.1415926));
}

import core.atomic;
import core.memory;
import core.sync.condition;
import core.thread;

import std.functional;
import std.meta;
import std.range.primitives;
import std.traits;

/*
(For now public undocumented with reserved name.)

A lazily initialized global constant. The underlying value is a shared global
statically initialized to `outOfBandValue` which must not be a legit value of
the constant. Upon the first call the situation is detected and the global is
initialized by calling `initializer`. The initializer is assumed to be pure
(even if not marked as such), i.e. return the same value upon repeated calls.
For that reason, no special precautions are taken so `initializer` may be called
more than one time leading to benign races on the cached value.

In the quiescent state the cost of the function is an atomic load from a global.

Params:
    T = The type of the pseudo-constant (may be qualified)
    outOfBandValue = A value that cannot be valid, it is used for initialization
    initializer = The function performing initialization; must be `nothrow`

Returns:
    The lazily initialized value
*/
@property pure
T __lazilyInitializedConstant(T, alias outOfBandValue, alias initializer)()
if (is(Unqual!T : T)
    && is(typeof(initializer()) : T)
    && is(typeof(outOfBandValue) : T))
{
    static T impl() nothrow
    {
        // Thread-local cache
        static Unqual!T tls = outOfBandValue;
        auto local = tls;
        // Shortest path, no atomic operations
        if (local != outOfBandValue) return local;
        // Process-level cache
        static shared Unqual!T result = outOfBandValue;
        // Initialize both process-level cache and tls
        local = atomicLoad(result);
        if (local == outOfBandValue)
        {
            local = initializer();
            atomicStore(result, local);
        }
        tls = local;
        return local;
    }

    import std.traits : SetFunctionAttributes;
    alias Fun = SetFunctionAttributes!(typeof(&impl), "D",
        functionAttributes!(typeof(&impl)) | FunctionAttribute.pure_);
    auto purified = (() @trusted => cast(Fun) &impl)();
    return purified();
}

// Returns the size of a cache line.
alias cacheLineSize =
    __lazilyInitializedConstant!(immutable(size_t), size_t.max, cacheLineSizeImpl);

private size_t cacheLineSizeImpl() @nogc nothrow @trusted
{
    size_t result = 0;
    import core.cpuid : datacache;
    foreach (ref const cachelevel; datacache)
    {
        if (cachelevel.lineSize > result && cachelevel.lineSize < uint.max)
        {
            result = cachelevel.lineSize;
        }
    }
    return result;
}

@nogc @safe nothrow unittest
{
    assert(cacheLineSize == cacheLineSizeImpl);
}

/* Atomics code.  These forward to core.atomic, but are written like this
   for two reasons:

   1.  They used to actually contain ASM code and I don' want to have to change
       to directly calling core.atomic in a zillion different places.

   2.  core.atomic has some misc. issues that make my use cases difficult
       without wrapping it.  If I didn't wrap it, casts would be required
       basically everywhere.
*/
private void atomicSetUbyte(T)(ref T stuff, T newVal)
if (__traits(isIntegral, T) && is(T : ubyte))
{
    //core.atomic.cas(cast(shared) &stuff, stuff, newVal);
    atomicStore(*(cast(shared) &stuff), newVal);
}

private ubyte atomicReadUbyte(T)(ref T val)
if (__traits(isIntegral, T) && is(T : ubyte))
{
    return atomicLoad(*(cast(shared) &val));
}

// This gets rid of the need for a lot of annoying casts in other parts of the
// code, when enums are involved.
private bool atomicCasUbyte(T)(ref T stuff, T testVal, T newVal)
if (__traits(isIntegral, T) && is(T : ubyte))
{
    return core.atomic.cas(cast(shared) &stuff, testVal, newVal);
}

/*--------------------- Generic helper functions, etc.------------------------*/
private template MapType(R, functions...)
{
    static assert(functions.length);

    ElementType!R e = void;
    alias MapType =
        typeof(adjoin!(staticMap!(unaryFun, functions))(e));
}

private template ReduceType(alias fun, R, E)
{
    alias ReduceType = typeof(binaryFun!fun(E.init, ElementType!R.init));
}

private template noUnsharedAliasing(T)
{
    enum bool noUnsharedAliasing = !hasUnsharedAliasing!T;
}

// This template tests whether a function may be executed in parallel from
// @safe code via Task.executeInNewThread().  There is an additional
// requirement for executing it via a TaskPool.  (See isSafeReturn).
private template isSafeTask(F)
{
    enum bool isSafeTask =
        (functionAttributes!F & (FunctionAttribute.safe | FunctionAttribute.trusted)) != 0 &&
        (functionAttributes!F & FunctionAttribute.ref_) == 0 &&
        (isFunctionPointer!F || !hasUnsharedAliasing!F) &&
        allSatisfy!(noUnsharedAliasing, Parameters!F);
}

@safe unittest
{
    alias F1 = void function() @safe;
    alias F2 = void function();
    alias F3 = void function(uint, string) @trusted;
    alias F4 = void function(uint, char[]);

    static assert( isSafeTask!F1);
    static assert(!isSafeTask!F2);
    static assert( isSafeTask!F3);
    static assert(!isSafeTask!F4);

    alias F5 = uint[] function(uint, string) pure @trusted;
    static assert( isSafeTask!F5);
}

// This function decides whether Tasks that meet all of the other requirements
// for being executed from @safe code can be executed on a TaskPool.
// When executing via TaskPool, it's theoretically possible
// to return a value that is also pointed to by a worker thread's thread local
// storage.  When executing from executeInNewThread(), the thread that executed
// the Task is terminated by the time the return value is visible in the calling
// thread, so this is a non-issue.  It's also a non-issue for pure functions
// since they can't read global state.
private template isSafeReturn(T)
{
    static if (!hasUnsharedAliasing!(T.ReturnType))
    {
        enum isSafeReturn = true;
    }
    else static if (T.isPure)
    {
        enum isSafeReturn = true;
    }
    else
    {
        enum isSafeReturn = false;
    }
}

private template randAssignable(R)
{
    enum randAssignable = isRandomAccessRange!R && hasAssignableElements!R;
}

private enum TaskStatus : ubyte
{
    notStarted,
    inProgress,
    done
}

private template AliasReturn(alias fun, T...)
{
    alias AliasReturn = typeof({ T args; return fun(args); });
}

// Should be private, but std.algorithm.reduce is used in the zero-thread case
// and won't work w/ private.
template reduceAdjoin(functions...)
{
    static if (functions.length == 1)
    {
        alias reduceAdjoin = binaryFun!(functions[0]);
    }
    else
    {
        T reduceAdjoin(T, U)(T lhs, U rhs)
        {
            alias funs = staticMap!(binaryFun, functions);

            foreach (i, Unused; typeof(lhs.expand))
            {
                lhs.expand[i] = funs[i](lhs.expand[i], rhs);
            }

            return lhs;
        }
    }
}

private template reduceFinish(functions...)
{
    static if (functions.length == 1)
    {
        alias reduceFinish = binaryFun!(functions[0]);
    }
    else
    {
        T reduceFinish(T)(T lhs, T rhs)
        {
            alias funs = staticMap!(binaryFun, functions);

            foreach (i, Unused; typeof(lhs.expand))
            {
                lhs.expand[i] = funs[i](lhs.expand[i], rhs.expand[i]);
            }

            return lhs;
        }
    }
}

private template isRoundRobin(R : RoundRobinBuffer!(C1, C2), C1, C2)
{
    enum isRoundRobin = true;
}

private template isRoundRobin(T)
{
    enum isRoundRobin = false;
}

@safe unittest
{
    static assert( isRoundRobin!(RoundRobinBuffer!(void delegate(char[]), bool delegate())));
    static assert(!isRoundRobin!(uint));
}

// This is the base "class" for all of the other tasks.  Using C-style
// polymorphism to allow more direct control over memory allocation, etc.
private struct AbstractTask
{
    AbstractTask* prev;
    AbstractTask* next;

    // Pointer to a function that executes this task.
    void function(void*) runTask;

    Throwable exception;
    ubyte taskStatus = TaskStatus.notStarted;

    bool done() @property
    {
        if (atomicReadUbyte(taskStatus) == TaskStatus.done)
        {
            if (exception)
            {
                throw exception;
            }

            return true;
        }

        return false;
    }

    void job()
    {
        runTask(&this);
    }
}

/**
$(D Task) represents the fundamental unit of work.  A $(D Task) may be
executed in parallel with any other $(D Task).  Using this struct directly
allows future/promise _parallelism.  In this paradigm, a function (or delegate
or other callable) is executed in a thread other than the one it was called
from.  The calling thread does not block while the function is being executed.
A call to $(D workForce), $(D yieldForce), or $(D spinForce) is used to
ensure that the $(D Task) has finished executing and to obtain the return
value, if any.  These functions and $(D done) also act as full memory barriers,
meaning that any memory writes made in the thread that executed the $(D Task)
are guaranteed to be visible in the calling thread after one of these functions
returns.

The $(REF task, std,parallelism) and $(REF scopedTask, std,parallelism) functions can
be used to create an instance of this struct.  See $(D task) for usage examples.

Function results are returned from $(D yieldForce), $(D spinForce) and
$(D workForce) by ref.  If $(D fun) returns by ref, the reference will point
to the returned reference of $(D fun).  Otherwise it will point to a
field in this struct.

Copying of this struct is disabled, since it would provide no useful semantics.
If you want to pass this struct around, you should do so by reference or
pointer.

Bugs:  Changes to $(D ref) and $(D out) arguments are not propagated to the
       call site, only to $(D args) in this struct.
*/
struct Task(alias fun, Args...)
{
    AbstractTask base = {runTask : &impl};
    alias base this;

    private @property AbstractTask* basePtr()
    {
        return &base;
    }

    private static void impl(void* myTask)
    {
        import std.algorithm.internal : addressOf;

        Task* myCastedTask = cast(typeof(this)*) myTask;
        static if (is(ReturnType == void))
        {
            fun(myCastedTask._args);
        }
        else static if (is(typeof(addressOf(fun(myCastedTask._args)))))
        {
            myCastedTask.returnVal = addressOf(fun(myCastedTask._args));
        }
        else
        {
            myCastedTask.returnVal = fun(myCastedTask._args);
        }
    }

    private TaskPool pool;
    private bool isScoped;  // True if created with scopedTask.

    Args _args;

    /**
    The arguments the function was called with.  Changes to $(D out) and
    $(D ref) arguments will be visible here.
    */
    static if (__traits(isSame, fun, run))
    {
        alias args = _args[1..$];
    }
    else
    {
        alias args = _args;
    }


    // The purpose of this code is to decide whether functions whose
    // return values have unshared aliasing can be executed via
    // TaskPool from @safe code.  See isSafeReturn.
    static if (__traits(isSame, fun, run))
    {
        static if (isFunctionPointer!(_args[0]))
        {
            private enum bool isPure =
            functionAttributes!(Args[0]) & FunctionAttribute.pure_;
        }
        else
        {
            // BUG:  Should check this for delegates too, but std.traits
            //       apparently doesn't allow this.  isPure is irrelevant
            //       for delegates, at least for now since shared delegates
            //       don't work.
            private enum bool isPure = false;
        }

    }
    else
    {
        // We already know that we can't execute aliases in @safe code, so
        // just put a dummy value here.
        private enum bool isPure = false;
    }


    /**
    The return type of the function called by this $(D Task).  This can be
    $(D void).
    */
    alias ReturnType = typeof(fun(_args));

    static if (!is(ReturnType == void))
    {
        static if (is(typeof(&fun(_args))))
        {
            // Ref return.
            ReturnType* returnVal;

            ref ReturnType fixRef(ReturnType* val)
            {
                return *val;
            }

        }
        else
        {
            ReturnType returnVal;

            ref ReturnType fixRef(ref ReturnType val)
            {
                return val;
            }
        }
    }

    private void enforcePool()
    {
        import std.exception : enforce;
        enforce(this.pool !is null, "Job not submitted yet.");
    }

    static if (Args.length > 0)
    {
        private this(Args args)
        {
            _args = args;
        }
    }

    // Work around DMD bug 6588, allow immutable elements.
    static if (allSatisfy!(isAssignable, Args))
    {
        typeof(this) opAssign(typeof(this) rhs)
        {
            foreach (i, Type; typeof(this.tupleof))
            {
                this.tupleof[i] = rhs.tupleof[i];
            }
            return this;
        }
    }
    else
    {
        @disable typeof(this) opAssign(typeof(this) rhs)
        {
            assert(0);
        }
    }

    /**
    If the $(D Task) isn't started yet, execute it in the current thread.
    If it's done, return its return value, if any.  If it's in progress,
    busy spin until it's done, then return the return value.  If it threw
    an exception, rethrow that exception.

    This function should be used when you expect the result of the
    $(D Task) to be available on a timescale shorter than that of an OS
    context switch.
     */
    @property ref ReturnType spinForce() @trusted
    {
        enforcePool();

        this.pool.tryDeleteExecute(basePtr);

        while (atomicReadUbyte(this.taskStatus) != TaskStatus.done) {}

        if (exception)
        {
            throw exception;
        }

        static if (!is(ReturnType == void))
        {
            return fixRef(this.returnVal);
        }
    }

    /**
    If the $(D Task) isn't started yet, execute it in the current thread.
    If it's done, return its return value, if any.  If it's in progress,
    wait on a condition variable.  If it threw an exception, rethrow that
    exception.

    This function should be used for expensive functions, as waiting on a
    condition variable introduces latency, but avoids wasted CPU cycles.
     */
    @property ref ReturnType yieldForce() @trusted
    {
        enforcePool();
        this.pool.tryDeleteExecute(basePtr);

        if (done)
        {
            static if (is(ReturnType == void))
            {
                return;
            }
            else
            {
                return fixRef(this.returnVal);
            }
        }

        pool.waiterLock();
        scope(exit) pool.waiterUnlock();

        while (atomicReadUbyte(this.taskStatus) != TaskStatus.done)
        {
            pool.waitUntilCompletion();
        }

        if (exception)
        {
            throw exception; // nocoverage
        }

        static if (!is(ReturnType == void))
        {
            return fixRef(this.returnVal);
        }
    }

    /**
    If this $(D Task) was not started yet, execute it in the current
    thread.  If it is finished, return its result.  If it is in progress,
    execute any other $(D Task) from the $(D TaskPool) instance that
    this $(D Task) was submitted to until this one
    is finished.  If it threw an exception, rethrow that exception.
    If no other tasks are available or this $(D Task) was executed using
    $(D executeInNewThread), wait on a condition variable.
     */
    @property ref ReturnType workForce() @trusted
    {
        enforcePool();
        this.pool.tryDeleteExecute(basePtr);

        while (true)
        {
            if (done)    // done() implicitly checks for exceptions.
            {
                static if (is(ReturnType == void))
                {
                    return;
                }
                else
                {
                    return fixRef(this.returnVal);
                }
            }

            AbstractTask* job;
            {
                // Locking explicitly and calling popNoSync() because
                // pop() waits on a condition variable if there are no Tasks
                // in the queue.

                pool.queueLock();
                scope(exit) pool.queueUnlock();
                job = pool.popNoSync();
            }


            if (job !is null)
            {

                version (verboseUnittest)
                {
                    stderr.writeln("Doing workForce work.");
                }

                pool.doJob(job);

                if (done)
                {
                    static if (is(ReturnType == void))
                    {
                        return;
                    }
                    else
                    {
                        return fixRef(this.returnVal);
                    }
                }
            }
            else
            {
                version (verboseUnittest)
                {
                    stderr.writeln("Yield from workForce.");
                }

                return yieldForce;
            }
        }
    }

    /**
    Returns $(D true) if the $(D Task) is finished executing.

    Throws:  Rethrows any exception thrown during the execution of the
             $(D Task).
    */
    @property bool done() @trusted
    {
        // Explicitly forwarded for documentation purposes.
        return base.done;
    }

    /**
    Create a new thread for executing this $(D Task), execute it in the
    newly created thread, then terminate the thread.  This can be used for
    future/promise parallelism.  An explicit priority may be given
    to the $(D Task).  If one is provided, its value is forwarded to
    $(D core.thread.Thread.priority). See $(REF task, std,parallelism) for
    usage example.
    */
    void executeInNewThread() @trusted
    {
        pool = new TaskPool(basePtr);
    }

    /// Ditto
    void executeInNewThread(int priority) @trusted
    {
        pool = new TaskPool(basePtr, priority);
    }

    @safe ~this()
    {
        if (isScoped && pool !is null && taskStatus != TaskStatus.done)
        {
            yieldForce;
        }
    }

    // When this is uncommented, it somehow gets called on returning from
    // scopedTask even though the struct shouldn't be getting copied.
    //@disable this(this) {}
}

// Calls $(D fpOrDelegate) with $(D args).  This is an
// adapter that makes $(D Task) work with delegates, function pointers and
// functors instead of just aliases.
ReturnType!F run(F, Args...)(F fpOrDelegate, ref Args args)
{
    return fpOrDelegate(args);
}

/**
Creates a $(D Task) on the GC heap that calls an alias.  This may be executed
via $(D Task.executeInNewThread) or by submitting to a
$(REF TaskPool, std,parallelism).  A globally accessible instance of
$(D TaskPool) is provided by $(REF taskPool, std,parallelism).

Returns:  A pointer to the $(D Task).

Example:
---
// Read two files into memory at the same time.
import std.file;

void main()
{
    // Create and execute a Task for reading
    // foo.txt.
    auto file1Task = task!read("foo.txt");
    file1Task.executeInNewThread();

    // Read bar.txt in parallel.
    auto file2Data = read("bar.txt");

    // Get the results of reading foo.txt.
    auto file1Data = file1Task.yieldForce;
}
---

---
// Sorts an array using a parallel quick sort algorithm.
// The first partition is done serially.  Both recursion
// branches are then executed in parallel.
//
// Timings for sorting an array of 1,000,000 doubles on
// an Athlon 64 X2 dual core machine:
//
// This implementation:               176 milliseconds.
// Equivalent serial implementation:  280 milliseconds
void parallelSort(T)(T[] data)
{
    // Sort small subarrays serially.
    if (data.length < 100)
    {
         std.algorithm.sort(data);
         return;
    }

    // Partition the array.
    swap(data[$ / 2], data[$ - 1]);
    auto pivot = data[$ - 1];
    bool lessThanPivot(T elem) { return elem < pivot; }

    auto greaterEqual = partition!lessThanPivot(data[0..$ - 1]);
    swap(data[$ - greaterEqual.length - 1], data[$ - 1]);

    auto less = data[0..$ - greaterEqual.length - 1];
    greaterEqual = data[$ - greaterEqual.length..$];

    // Execute both recursion branches in parallel.
    auto recurseTask = task!parallelSort(greaterEqual);
    taskPool.put(recurseTask);
    parallelSort(less);
    recurseTask.yieldForce;
}
---
*/
auto task(alias fun, Args...)(Args args)
{
    return new Task!(fun, Args)(args);
}

/**
Creates a $(D Task) on the GC heap that calls a function pointer, delegate, or
class/struct with overloaded opCall.

Example:
---
// Read two files in at the same time again,
// but this time use a function pointer instead
// of an alias to represent std.file.read.
import std.file;

void main()
{
    // Create and execute a Task for reading
    // foo.txt.
    auto file1Task = task(&read, "foo.txt");
    file1Task.executeInNewThread();

    // Read bar.txt in parallel.
    auto file2Data = read("bar.txt");

    // Get the results of reading foo.txt.
    auto file1Data = file1Task.yieldForce;
}
---

Notes: This function takes a non-scope delegate, meaning it can be
       used with closures.  If you can't allocate a closure due to objects
       on the stack that have scoped destruction, see $(D scopedTask), which
       takes a scope delegate.
 */
auto task(F, Args...)(F delegateOrFp, Args args)
if (is(typeof(delegateOrFp(args))) && !isSafeTask!F)
{
    return new Task!(run, F, Args)(delegateOrFp, args);
}

/**
Version of $(D task) usable from $(D @safe) code.  Usage mechanics are
identical to the non-@safe case, but safety introduces some restrictions:

1.  $(D fun) must be @safe or @trusted.

2.  $(D F) must not have any unshared aliasing as defined by
    $(REF hasUnsharedAliasing, std,traits).  This means it
    may not be an unshared delegate or a non-shared class or struct
    with overloaded $(D opCall).  This also precludes accepting template
    alias parameters.

3.  $(D Args) must not have unshared aliasing.

4.  $(D fun) must not return by reference.

5.  The return type must not have unshared aliasing unless $(D fun) is
    $(D pure) or the $(D Task) is executed via $(D executeInNewThread) instead
    of using a $(D TaskPool).

*/
@trusted auto task(F, Args...)(F fun, Args args)
if (is(typeof(fun(args))) && isSafeTask!F)
{
    return new Task!(run, F, Args)(fun, args);
}

/**
These functions allow the creation of $(D Task) objects on the stack rather
than the GC heap.  The lifetime of a $(D Task) created by $(D scopedTask)
cannot exceed the lifetime of the scope it was created in.

$(D scopedTask) might be preferred over $(D task):

1.  When a $(D Task) that calls a delegate is being created and a closure
    cannot be allocated due to objects on the stack that have scoped
    destruction.  The delegate overload of $(D scopedTask) takes a $(D scope)
    delegate.

2.  As a micro-optimization, to avoid the heap allocation associated with
    $(D task) or with the creation of a closure.

Usage is otherwise identical to $(D task).

Notes:  $(D Task) objects created using $(D scopedTask) will automatically
call $(D Task.yieldForce) in their destructor if necessary to ensure
the $(D Task) is complete before the stack frame they reside on is destroyed.
*/
auto scopedTask(alias fun, Args...)(Args args)
{
    auto ret = Task!(fun, Args)(args);
    ret.isScoped = true;
    return ret;
}

/// Ditto
auto scopedTask(F, Args...)(scope F delegateOrFp, Args args)
if (is(typeof(delegateOrFp(args))) && !isSafeTask!F)
{
    auto ret = Task!(run, F, Args)(delegateOrFp, args);
    ret.isScoped = true;
    return ret;
}

/// Ditto
@trusted auto scopedTask(F, Args...)(F fun, Args args)
if (is(typeof(fun(args))) && isSafeTask!F)
{
    auto ret = Task!(run, F, Args)(fun, args);
    ret.isScoped = true;
    return ret;
}

/**
The total number of CPU cores available on the current machine, as reported by
the operating system.
*/
alias totalCPUs =
    __lazilyInitializedConstant!(immutable(uint), uint.max, totalCPUsImpl);

uint totalCPUsImpl() @nogc nothrow @trusted
{
    version (Windows)
    {
        // BUGS:  Only works on Windows 2000 and above.
        import core.sys.windows.winbase : SYSTEM_INFO, GetSystemInfo;
        import std.algorithm.comparison : max;
        SYSTEM_INFO si;
        GetSystemInfo(&si);
        return max(1, cast(uint) si.dwNumberOfProcessors);
    }
    else version (linux)
    {
        import core.sys.linux.sched : CPU_COUNT, cpu_set_t, sched_getaffinity;
        import core.sys.posix.unistd : _SC_NPROCESSORS_ONLN, sysconf;

        cpu_set_t set = void;
        if (sched_getaffinity(0, cpu_set_t.sizeof, &set) == 0)
        {
            int count = CPU_COUNT(&set);
            if (count > 0)
                return cast(uint) count;
        }
        return cast(uint) sysconf(_SC_NPROCESSORS_ONLN);
    }
    else version (Darwin)
    {
        import core.sys.darwin.sys.sysctl : sysctlbyname;
        uint result;
        size_t len = result.sizeof;
        sysctlbyname("hw.physicalcpu", &result, &len, null, 0);
        return result;
    }
    else version (DragonFlyBSD)
    {
        import core.sys.dragonflybsd.sys.sysctl : sysctlbyname;
        uint result;
        size_t len = result.sizeof;
        sysctlbyname("hw.ncpu", &result, &len, null, 0);
        return result;
    }
    else version (FreeBSD)
    {
        import core.sys.freebsd.sys.sysctl : sysctlbyname;
        uint result;
        size_t len = result.sizeof;
        sysctlbyname("hw.ncpu", &result, &len, null, 0);
        return result;
    }
    else version (NetBSD)
    {
        import core.sys.netbsd.sys.sysctl : sysctlbyname;
        uint result;
        size_t len = result.sizeof;
        sysctlbyname("hw.ncpu", &result, &len, null, 0);
        return result;
    }
    else version (Solaris)
    {
        import core.sys.posix.unistd : _SC_NPROCESSORS_ONLN, sysconf;
        return cast(uint) sysconf(_SC_NPROCESSORS_ONLN);
    }
    else version (OpenBSD)
    {
        import core.sys.posix.unistd : _SC_NPROCESSORS_ONLN, sysconf;
        return cast(uint) sysconf(_SC_NPROCESSORS_ONLN);
    }
    else
    {
        static assert(0, "Don't know how to get N CPUs on this OS.");
    }
}

/*
This class serves two purposes:

1.  It distinguishes std.parallelism threads from other threads so that
    the std.parallelism daemon threads can be terminated.

2.  It adds a reference to the pool that the thread is a member of,
    which is also necessary to allow the daemon threads to be properly
    terminated.
*/
private final class ParallelismThread : Thread
{
    this(void delegate() dg)
    {
        super(dg);
    }

    TaskPool pool;
}

// Kill daemon threads.
shared static ~this()
{
    foreach (ref thread; Thread)
    {
        auto pthread = cast(ParallelismThread) thread;
        if (pthread is null) continue;
        auto pool = pthread.pool;
        if (!pool.isDaemon) continue;
        pool.stop();
        pthread.join();
    }
}

/**
This class encapsulates a task queue and a set of worker threads.  Its purpose
is to efficiently map a large number of $(D Task)s onto a smaller number of
threads.  A task queue is a FIFO queue of $(D Task) objects that have been
submitted to the $(D TaskPool) and are awaiting execution.  A worker thread is a
thread that executes the $(D Task) at the front of the queue when one is
available and sleeps when the queue is empty.

This class should usually be used via the global instantiation
available via the $(REF taskPool, std,parallelism) property.
Occasionally it is useful to explicitly instantiate a $(D TaskPool):

1.  When you want $(D TaskPool) instances with multiple priorities, for example
    a low priority pool and a high priority pool.

2.  When the threads in the global task pool are waiting on a synchronization
    primitive (for example a mutex), and you want to parallelize the code that
    needs to run before these threads can be resumed.
 */
final class TaskPool
{
private:

    // A pool can either be a regular pool or a single-task pool.  A
    // single-task pool is a dummy pool that's fired up for
    // Task.executeInNewThread().
    bool isSingleTask;

    ParallelismThread[] pool;
    Thread singleTaskThread;

    AbstractTask* head;
    AbstractTask* tail;
    PoolState status = PoolState.running;
    Condition workerCondition;
    Condition waiterCondition;
    Mutex queueMutex;
    Mutex waiterMutex;  // For waiterCondition

    // The instanceStartIndex of the next instance that will be created.
    __gshared static size_t nextInstanceIndex = 1;

    // The index of the current thread.
    static size_t threadIndex;

    // The index of the first thread in this instance.
    immutable size_t instanceStartIndex;

    // The index that the next thread to be initialized in this pool will have.
    size_t nextThreadIndex;

    enum PoolState : ubyte
    {
        running,
        finishing,
        stopNow
    }

    void doJob(AbstractTask* job)
    {
        assert(job.taskStatus == TaskStatus.inProgress);
        assert(job.next is null);
        assert(job.prev is null);

        scope(exit)
        {
            if (!isSingleTask)
            {
                waiterLock();
                scope(exit) waiterUnlock();
                notifyWaiters();
            }
        }

        try
        {
            job.job();
        }
        catch (Throwable e)
        {
            job.exception = e;
        }

        atomicSetUbyte(job.taskStatus, TaskStatus.done);
    }

    // This function is used for dummy pools created by Task.executeInNewThread().
    void doSingleTask()
    {
        // No synchronization.  Pool is guaranteed to only have one thread,
        // and the queue is submitted to before this thread is created.
        assert(head);
        auto t = head;
        t.next = t.prev = head = null;
        doJob(t);
    }

    // This function performs initialization for each thread that affects
    // thread local storage and therefore must be done from within the
    // worker thread.  It then calls executeWorkLoop().
    void startWorkLoop()
    {
        // Initialize thread index.
        {
            queueLock();
            scope(exit) queueUnlock();
            threadIndex = nextThreadIndex;
            nextThreadIndex++;
        }

        executeWorkLoop();
    }

    // This is the main work loop that worker threads spend their time in
    // until they terminate.  It's also entered by non-worker threads when
    // finish() is called with the blocking variable set to true.
    void executeWorkLoop()
    {
        while (atomicReadUbyte(status) != PoolState.stopNow)
        {
            AbstractTask* task = pop();
            if (task is null)
            {
                if (atomicReadUbyte(status) == PoolState.finishing)
                {
                    atomicSetUbyte(status, PoolState.stopNow);
                    return;
                }
            }
            else
            {
                doJob(task);
            }
        }
    }

    // Pop a task off the queue.
    AbstractTask* pop()
    {
        queueLock();
        scope(exit) queueUnlock();
        auto ret = popNoSync();
        while (ret is null && status == PoolState.running)
        {
            wait();
            ret = popNoSync();
        }
        return ret;
    }

    AbstractTask* popNoSync()
    out(returned)
    {
        /* If task.prev and task.next aren't null, then another thread
         * can try to delete this task from the pool after it's
         * alreadly been deleted/popped.
         */
        if (returned !is null)
        {
            assert(returned.next is null);
            assert(returned.prev is null);
        }
    }
    body
    {
        if (isSingleTask) return null;

        AbstractTask* returned = head;
        if (head !is null)
        {
            head = head.next;
            returned.prev = null;
            returned.next = null;
            returned.taskStatus = TaskStatus.inProgress;
        }
        if (head !is null)
        {
            head.prev = null;
        }

        return returned;
    }

    // Push a task onto the queue.
    void abstractPut(AbstractTask* task)
    {
        queueLock();
        scope(exit) queueUnlock();
        abstractPutNoSync(task);
    }

    void abstractPutNoSync(AbstractTask* task)
    in
    {
        assert(task);
    }
    out
    {
        import std.conv : text;

        assert(tail.prev !is tail);
        assert(tail.next is null, text(tail.prev, '\t', tail.next));
        if (tail.prev !is null)
        {
            assert(tail.prev.next is tail, text(tail.prev, '\t', tail.next));
        }
    }
    body
    {
        // Not using enforce() to save on function call overhead since this
        // is a performance critical function.
        if (status != PoolState.running)
        {
            throw new Error(
                "Cannot submit a new task to a pool after calling " ~
                "finish() or stop()."
            );
        }

        task.next = null;
        if (head is null)   //Queue is empty.
        {
            head = task;
            tail = task;
            tail.prev = null;
        }
        else
        {
            assert(tail);
            task.prev = tail;
            tail.next = task;
            tail = task;
        }
        notify();
    }

    void abstractPutGroupNoSync(AbstractTask* h, AbstractTask* t)
    {
        if (status != PoolState.running)
        {
            throw new Error(
                "Cannot submit a new task to a pool after calling " ~
                "finish() or stop()."
            );
        }

        if (head is null)
        {
            head = h;
            tail = t;
        }
        else
        {
            h.prev = tail;
            tail.next = h;
            tail = t;
        }

        notifyAll();
    }

    void tryDeleteExecute(AbstractTask* toExecute)
    {
        if (isSingleTask) return;

        if ( !deleteItem(toExecute) )
        {
            return;
        }

        try
        {
            toExecute.job();
        }
        catch (Exception e)
        {
            toExecute.exception = e;
        }

        atomicSetUbyte(toExecute.taskStatus, TaskStatus.done);
    }

    bool deleteItem(AbstractTask* item)
    {
        queueLock();
        scope(exit) queueUnlock();
        return deleteItemNoSync(item);
    }

    bool deleteItemNoSync(AbstractTask* item)
    {
        if (item.taskStatus != TaskStatus.notStarted)
        {
            return false;
        }
        item.taskStatus = TaskStatus.inProgress;

        if (item is head)
        {
            // Make sure head gets set properly.
            popNoSync();
            return true;
        }
        if (item is tail)
        {
            tail = tail.prev;
            if (tail !is null)
            {
                tail.next = null;
            }
            item.next = null;
            item.prev = null;
            return true;
        }
        if (item.next !is null)
        {
            assert(item.next.prev is item);  // Check queue consistency.
            item.next.prev = item.prev;
        }
        if (item.prev !is null)
        {
            assert(item.prev.next is item);  // Check queue consistency.
            item.prev.next = item.next;
        }
        item.next = null;
        item.prev = null;
        return true;
    }

    void queueLock()
    {
        assert(queueMutex);
        if (!isSingleTask) queueMutex.lock();
    }

    void queueUnlock()
    {
        assert(queueMutex);
        if (!isSingleTask) queueMutex.unlock();
    }

    void waiterLock()
    {
        if (!isSingleTask) waiterMutex.lock();
    }

    void waiterUnlock()
    {
        if (!isSingleTask) waiterMutex.unlock();
    }

    void wait()
    {
        if (!isSingleTask) workerCondition.wait();
    }

    void notify()
    {
        if (!isSingleTask) workerCondition.notify();
    }

    void notifyAll()
    {
        if (!isSingleTask) workerCondition.notifyAll();
    }

    void waitUntilCompletion()
    {
        if (isSingleTask)
        {
            singleTaskThread.join();
        }
        else
        {
            waiterCondition.wait();
        }
    }

    void notifyWaiters()
    {
        if (!isSingleTask) waiterCondition.notifyAll();
    }

    // Private constructor for creating dummy pools that only have one thread,
    // only execute one Task, and then terminate.  This is used for
    // Task.executeInNewThread().
    this(AbstractTask* task, int priority = int.max)
    {
        assert(task);

        // Dummy value, not used.
        instanceStartIndex = 0;

        this.isSingleTask = true;
        task.taskStatus = TaskStatus.inProgress;
        this.head = task;
        singleTaskThread = new Thread(&doSingleTask);
        singleTaskThread.start();

        // Disabled until writing code to support
        // running thread with specified priority
        // See https://d.puremagic.com/issues/show_bug.cgi?id=8960

        /*if (priority != int.max)
        {
            singleTaskThread.priority = priority;
        }*/
    }

public:
    // This is used in parallel_algorithm but is too unstable to document
    // as public API.
    size_t defaultWorkUnitSize(size_t rangeLen) const @safe pure nothrow
    {
        import std.algorithm.comparison : max;

        if (this.size == 0)
        {
            return rangeLen;
        }

        immutable size_t eightSize = 4 * (this.size + 1);
        auto ret = (rangeLen / eightSize) + ((rangeLen % eightSize == 0) ? 0 : 1);
        return max(ret, 1);
    }

    /**
    Default constructor that initializes a $(D TaskPool) with
    $(D totalCPUs) - 1 worker threads.  The minus 1 is included because the
    main thread will also be available to do work.

    Note:  On single-core machines, the primitives provided by $(D TaskPool)
           operate transparently in single-threaded mode.
     */
    this() @trusted
    {
        this(totalCPUs - 1);
    }

    /**
    Allows for custom number of worker threads.
    */
    this(size_t nWorkers) @trusted
    {
        synchronized(typeid(TaskPool))
        {
            instanceStartIndex = nextInstanceIndex;

            // The first worker thread to be initialized will have this index,
            // and will increment it.  The second worker to be initialized will
            // have this index plus 1.
            nextThreadIndex = instanceStartIndex;
            nextInstanceIndex += nWorkers;
        }

        queueMutex = new Mutex(this);
        waiterMutex = new Mutex();
        workerCondition = new Condition(queueMutex);
        waiterCondition = new Condition(waiterMutex);

        pool = new ParallelismThread[nWorkers];
        foreach (ref poolThread; pool)
        {
            poolThread = new ParallelismThread(&startWorkLoop);
            poolThread.pool = this;
            poolThread.start();
        }
    }

    /**
    Implements a parallel foreach loop over a range.  This works by implicitly
    creating and submitting one $(D Task) to the $(D TaskPool) for each worker
    thread.  A work unit is a set of consecutive elements of $(D range) to
    be processed by a worker thread between communication with any other
    thread.  The number of elements processed per work unit is controlled by the
    $(D workUnitSize) parameter.  Smaller work units provide better load
    balancing, but larger work units avoid the overhead of communicating
    with other threads frequently to fetch the next work unit.  Large work
    units also avoid false sharing in cases where the range is being modified.
    The less time a single iteration of the loop takes, the larger
    $(D workUnitSize) should be.  For very expensive loop bodies,
    $(D workUnitSize) should  be 1.  An overload that chooses a default work
    unit size is also available.

    Example:
    ---
    // Find the logarithm of every number from 1 to
    // 10_000_000 in parallel.
    auto logs = new double[10_000_000];

    // Parallel foreach works with or without an index
    // variable.  It can be iterate by ref if range.front
    // returns by ref.

    // Iterate over logs using work units of size 100.
    foreach (i, ref elem; taskPool.parallel(logs, 100))
    {
        elem = log(i + 1.0);
    }

    // Same thing, but use the default work unit size.
    //
    // Timings on an Athlon 64 X2 dual core machine:
    //
    // Parallel foreach:  388 milliseconds
    // Regular foreach:   619 milliseconds
    foreach (i, ref elem; taskPool.parallel(logs))
    {
        elem = log(i + 1.0);
    }
    ---

    Notes:

    The memory usage of this implementation is guaranteed to be constant
    in $(D range.length).

    Breaking from a parallel foreach loop via a break, labeled break,
    labeled continue, return or goto statement throws a
    $(D ParallelForeachError).

    In the case of non-random access ranges, parallel foreach buffers lazily
    to an array of size $(D workUnitSize) before executing the parallel portion
    of the loop.  The exception is that, if a parallel foreach is executed
    over a range returned by $(D asyncBuf) or $(D map), the copying is elided
    and the buffers are simply swapped.  In this case $(D workUnitSize) is
    ignored and the work unit size is set to the  buffer size of $(D range).

    A memory barrier is guaranteed to be executed on exit from the loop,
    so that results produced by all threads are visible in the calling thread.

    $(B Exception Handling):

    When at least one exception is thrown from inside a parallel foreach loop,
    the submission of additional $(D Task) objects is terminated as soon as
    possible, in a non-deterministic manner.  All executing or
    enqueued work units are allowed to complete.  Then, all exceptions that
    were thrown by any work unit are chained using $(D Throwable.next) and
    rethrown.  The order of the exception chaining is non-deterministic.
    */
    ParallelForeach!R parallel(R)(R range, size_t workUnitSize)
    {
        import std.exception : enforce;
        enforce(workUnitSize > 0, "workUnitSize must be > 0.");
        alias RetType = ParallelForeach!R;
        return RetType(this, range, workUnitSize);
    }


    /// Ditto
    ParallelForeach!R parallel(R)(R range)
    {
        static if (hasLength!R)
        {
            // Default work unit size is such that we would use 4x as many
            // slots as are in this thread pool.
            size_t workUnitSize = defaultWorkUnitSize(range.length);
            return parallel(range, workUnitSize);
        }
        else
        {
            // Just use a really, really dumb guess if the user is too lazy to
            // specify.
            return parallel(range, 512);
        }
    }

    ///
    template amap(functions...)
    {
        /**
        Eager parallel map.  The eagerness of this function means it has less
        overhead than the lazily evaluated $(D TaskPool.map) and should be
        preferred where the memory requirements of eagerness are acceptable.
        $(D functions) are the functions to be evaluated, passed as template
        alias parameters in a style similar to
        $(REF map, std,algorithm,iteration).
        The first argument must be a random access range. For performance
        reasons, amap will assume the range elements have not yet been
        initialized. Elements will be overwritten without calling a destructor
        nor doing an assignment. As such, the range must not contain meaningful
        data$(DDOC_COMMENT not a section): either un-initialized objects, or
        objects in their $(D .init) state.

        ---
        auto numbers = iota(100_000_000.0);

        // Find the square roots of numbers.
        //
        // Timings on an Athlon 64 X2 dual core machine:
        //
        // Parallel eager map:                   0.802 s
        // Equivalent serial implementation:     1.768 s
        auto squareRoots = taskPool.amap!sqrt(numbers);
        ---

        Immediately after the range argument, an optional work unit size argument
        may be provided.  Work units as used by $(D amap) are identical to those
        defined for parallel foreach.  If no work unit size is provided, the
        default work unit size is used.

        ---
        // Same thing, but make work unit size 100.
        auto squareRoots = taskPool.amap!sqrt(numbers, 100);
        ---

        An output range for returning the results may be provided as the last
        argument.  If one is not provided, an array of the proper type will be
        allocated on the garbage collected heap.  If one is provided, it must be a
        random access range with assignable elements, must have reference
        semantics with respect to assignment to its elements, and must have the
        same length as the input range.  Writing to adjacent elements from
        different threads must be safe.

        ---
        // Same thing, but explicitly allocate an array
        // to return the results in.  The element type
        // of the array may be either the exact type
        // returned by functions or an implicit conversion
        // target.
        auto squareRoots = new float[numbers.length];
        taskPool.amap!sqrt(numbers, squareRoots);

        // Multiple functions, explicit output range, and
        // explicit work unit size.
        auto results = new Tuple!(float, real)[numbers.length];
        taskPool.amap!(sqrt, log)(numbers, 100, results);
        ---

        Note:

        A memory barrier is guaranteed to be executed after all results are written
        but before returning so that results produced by all threads are visible
        in the calling thread.

        Tips:

        To perform the mapping operation in place, provide the same range for the
        input and output range.

        To parallelize the copying of a range with expensive to evaluate elements
        to an array, pass an identity function (a function that just returns
        whatever argument is provided to it) to $(D amap).

        $(B Exception Handling):

        When at least one exception is thrown from inside the map functions,
        the submission of additional $(D Task) objects is terminated as soon as
        possible, in a non-deterministic manner.  All currently executing or
        enqueued work units are allowed to complete.  Then, all exceptions that
        were thrown from any work unit are chained using $(D Throwable.next) and
        rethrown.  The order of the exception chaining is non-deterministic.
        */
        auto amap(Args...)(Args args)
        if (isRandomAccessRange!(Args[0]))
        {
            import std.conv : emplaceRef;

            alias fun = adjoin!(staticMap!(unaryFun, functions));

            alias range = args[0];
            immutable len = range.length;

            static if (
                Args.length > 1 &&
                randAssignable!(Args[$ - 1]) &&
                is(MapType!(Args[0], functions) : ElementType!(Args[$ - 1]))
                )
            {
                import std.conv : text;
                import std.exception : enforce;

                alias buf = args[$ - 1];
                alias args2 = args[0..$ - 1];
                alias Args2 = Args[0..$ - 1];
                enforce(buf.length == len,
                        text("Can't use a user supplied buffer that's the wrong ",
                             "size.  (Expected  :", len, " Got:  ", buf.length));
            }
            else static if (randAssignable!(Args[$ - 1]) && Args.length > 1)
            {
                static assert(0, "Wrong buffer type.");
            }
            else
            {
                import std.array : uninitializedArray;

                auto buf = uninitializedArray!(MapType!(Args[0], functions)[])(len);
                alias args2 = args;
                alias Args2 = Args;
            }

            if (!len) return buf;

            static if (isIntegral!(Args2[$ - 1]))
            {
                static assert(args2.length == 2);
                auto workUnitSize = cast(size_t) args2[1];
            }
            else
            {
                static assert(args2.length == 1, Args);
                auto workUnitSize = defaultWorkUnitSize(range.length);
            }

            alias R = typeof(range);

            if (workUnitSize > len)
            {
                workUnitSize = len;
            }

            // Handle as a special case:
            if (size == 0)
            {
                size_t index = 0;
                foreach (elem; range)
                {
                    emplaceRef(buf[index++], fun(elem));
                }
                return buf;
            }

            // Effectively -1:  chunkIndex + 1 == 0:
            shared size_t workUnitIndex = size_t.max;
            shared bool shouldContinue = true;

            void doIt()
            {
                import std.algorithm.comparison : min;

                scope(failure)
                {
                    // If an exception is thrown, all threads should bail.
                    atomicStore(shouldContinue, false);
                }

                while (atomicLoad(shouldContinue))
                {
                    immutable myUnitIndex = atomicOp!"+="(workUnitIndex, 1);
                    immutable start = workUnitSize * myUnitIndex;
                    if (start >= len)
                    {
                        atomicStore(shouldContinue, false);
                        break;
                    }

                    immutable end = min(len, start + workUnitSize);

                    static if (hasSlicing!R)
                    {
                        auto subrange = range[start .. end];
                        foreach (i; start .. end)
                        {
                            emplaceRef(buf[i], fun(subrange.front));
                            subrange.popFront();
                        }
                    }
                    else
                    {
                        foreach (i; start .. end)
                        {
                            emplaceRef(buf[i], fun(range[i]));
                        }
                    }
                }
            }

            submitAndExecute(this, &doIt);
            return buf;
        }
    }

    ///
    template map(functions...)
    {
        /**
        A semi-lazy parallel map that can be used for pipelining.  The map
        functions are evaluated for the first $(D bufSize) elements and stored in a
        buffer and made available to $(D popFront).  Meanwhile, in the
        background a second buffer of the same size is filled.  When the first
        buffer is exhausted, it is swapped with the second buffer and filled while
        the values from what was originally the second buffer are read.  This
        implementation allows for elements to be written to the buffer without
        the need for atomic operations or synchronization for each write, and
        enables the mapping function to be evaluated efficiently in parallel.

        $(D map) has more overhead than the simpler procedure used by $(D amap)
        but avoids the need to keep all results in memory simultaneously and works
        with non-random access ranges.

        Params:

        source = The input range to be mapped.  If $(D source) is not random
        access it will be lazily buffered to an array of size $(D bufSize) before
        the map function is evaluated.  (For an exception to this rule, see Notes.)

        bufSize = The size of the buffer to store the evaluated elements.

        workUnitSize = The number of elements to evaluate in a single
        $(D Task).  Must be less than or equal to $(D bufSize), and
        should be a fraction of $(D bufSize) such that all worker threads can be
        used.  If the default of size_t.max is used, workUnitSize will be set to
        the pool-wide default.

        Returns:  An input range representing the results of the map.  This range
                  has a length iff $(D source) has a length.

        Notes:

        If a range returned by $(D map) or $(D asyncBuf) is used as an input to
        $(D map), then as an optimization the copying from the output buffer
        of the first range to the input buffer of the second range is elided, even
        though the ranges returned by $(D map) and $(D asyncBuf) are non-random
        access ranges.  This means that the $(D bufSize) parameter passed to the
        current call to $(D map) will be ignored and the size of the buffer
        will be the buffer size of $(D source).

        Example:
        ---
        // Pipeline reading a file, converting each line
        // to a number, taking the logarithms of the numbers,
        // and performing the additions necessary to find
        // the sum of the logarithms.

        auto lineRange = File("numberList.txt").byLine();
        auto dupedLines = std.algorithm.map!"a.idup"(lineRange);
        auto nums = taskPool.map!(to!double)(dupedLines);
        auto logs = taskPool.map!log10(nums);

        double sum = 0;
        foreach (elem; logs)
        {
            sum += elem;
        }
        ---

        $(B Exception Handling):

        Any exceptions thrown while iterating over $(D source)
        or computing the map function are re-thrown on a call to $(D popFront) or,
        if thrown during construction, are simply allowed to propagate to the
        caller.  In the case of exceptions thrown while computing the map function,
        the exceptions are chained as in $(D TaskPool.amap).
        */
        auto
        map(S)(S source, size_t bufSize = 100, size_t workUnitSize = size_t.max)
        if (isInputRange!S)
        {
            import std.exception : enforce;

            enforce(workUnitSize == size_t.max || workUnitSize <= bufSize,
                    "Work unit size must be smaller than buffer size.");
            alias fun = adjoin!(staticMap!(unaryFun, functions));

            static final class Map
            {
                // This is a class because the task needs to be located on the
                // heap and in the non-random access case source needs to be on
                // the heap, too.

            private:
                enum bufferTrick = is(typeof(source.buf1)) &&
                is(typeof(source.bufPos)) &&
                is(typeof(source.doBufSwap()));

                alias E = MapType!(S, functions);
                E[] buf1, buf2;
                S source;
                TaskPool pool;
                Task!(run, E[] delegate(E[]), E[]) nextBufTask;
                size_t workUnitSize;
                size_t bufPos;
                bool lastTaskWaited;

            static if (isRandomAccessRange!S)
            {
                alias FromType = S;

                void popSource()
                {
                    import std.algorithm.comparison : min;

                    static if (__traits(compiles, source[0 .. source.length]))
                    {
                        source = source[min(buf1.length, source.length)..source.length];
                    }
                    else static if (__traits(compiles, source[0..$]))
                    {
                        source = source[min(buf1.length, source.length)..$];
                    }
                    else
                    {
                        static assert(0, "S must have slicing for Map."
                                      ~ "  " ~ S.stringof ~ " doesn't.");
                    }
                }
            }
            else static if (bufferTrick)
            {
                // Make sure we don't have the buffer recycling overload of
                // asyncBuf.
                static if (
                    is(typeof(source.source)) &&
                    isRoundRobin!(typeof(source.source))
                )
                {
                    static assert(0, "Cannot execute a parallel map on " ~
                                  "the buffer recycling overload of asyncBuf."
                                 );
                }

                alias FromType = typeof(source.buf1);
                FromType from;

                // Just swap our input buffer with source's output buffer.
                // No need to copy element by element.
                FromType dumpToFrom()
                {
                    import std.algorithm.mutation : swap;

                    assert(source.buf1.length <= from.length);
                    from.length = source.buf1.length;
                    swap(source.buf1, from);

                    // Just in case this source has been popped before
                    // being sent to map:
                    from = from[source.bufPos..$];

                    static if (is(typeof(source._length)))
                    {
                        source._length -= (from.length - source.bufPos);
                    }

                    source.doBufSwap();

                    return from;
                }
            }
            else
            {
                alias FromType = ElementType!S[];

                // The temporary array that data is copied to before being
                // mapped.
                FromType from;

                FromType dumpToFrom()
                {
                    assert(from !is null);

                    size_t i;
                    for (; !source.empty && i < from.length; source.popFront())
                    {
                        from[i++] = source.front;
                    }

                    from = from[0 .. i];
                    return from;
                }
            }

            static if (hasLength!S)
            {
                size_t _length;

                public @property size_t length() const @safe pure nothrow
                {
                    return _length;
                }
            }

                this(S source, size_t bufSize, size_t workUnitSize, TaskPool pool)
                {
                    static if (bufferTrick)
                    {
                        bufSize = source.buf1.length;
                    }

                    buf1.length = bufSize;
                    buf2.length = bufSize;

                    static if (!isRandomAccessRange!S)
                    {
                        from.length = bufSize;
                    }

                    this.workUnitSize = (workUnitSize == size_t.max) ?
                            pool.defaultWorkUnitSize(bufSize) : workUnitSize;
                    this.source = source;
                    this.pool = pool;

                    static if (hasLength!S)
                    {
                        _length = source.length;
                    }

                    buf1 = fillBuf(buf1);
                    submitBuf2();
                }

                // The from parameter is a dummy and ignored in the random access
                // case.
                E[] fillBuf(E[] buf)
                {
                    import std.algorithm.comparison : min;

                    static if (isRandomAccessRange!S)
                    {
                        import std.range : take;
                        auto toMap = take(source, buf.length);
                        scope(success) popSource();
                    }
                    else
                    {
                        auto toMap = dumpToFrom();
                    }

                    buf = buf[0 .. min(buf.length, toMap.length)];

                    // Handle as a special case:
                    if (pool.size == 0)
                    {
                        size_t index = 0;
                        foreach (elem; toMap)
                        {
                            buf[index++] = fun(elem);
                        }
                        return buf;
                    }

                    pool.amap!functions(toMap, workUnitSize, buf);

                    return buf;
                }

                void submitBuf2()
                in
                {
                    assert(nextBufTask.prev is null);
                    assert(nextBufTask.next is null);
                } body
                {
                    // Hack to reuse the task object.

                    nextBufTask = typeof(nextBufTask).init;
                    nextBufTask._args[0] = &fillBuf;
                    nextBufTask._args[1] = buf2;
                    pool.put(nextBufTask);
                }

                void doBufSwap()
                {
                    if (lastTaskWaited)
                    {
                        // Then the source is empty.  Signal it here.
                        buf1 = null;
                        buf2 = null;

                        static if (!isRandomAccessRange!S)
                        {
                            from = null;
                        }

                        return;
                    }

                    buf2 = buf1;
                    buf1 = nextBufTask.yieldForce;
                    bufPos = 0;

                    if (source.empty)
                    {
                        lastTaskWaited = true;
                    }
                    else
                    {
                        submitBuf2();
                    }
                }

            public:
                @property auto front()
                {
                    return buf1[bufPos];
                }

                void popFront()
                {
                    static if (hasLength!S)
                    {
                        _length--;
                    }

                    bufPos++;
                    if (bufPos >= buf1.length)
                    {
                        doBufSwap();
                    }
                }

                static if (isInfinite!S)
                {
                    enum bool empty = false;
                }
                else
                {

                    bool empty() const @property
                    {
                        // popFront() sets this when source is empty
                        return buf1.length == 0;
                    }
                }
            }
            return new Map(source, bufSize, workUnitSize, this);
        }
    }

    /**
    Given a $(D source) range that is expensive to iterate over, returns an
    input range that asynchronously buffers the contents of
    $(D source) into a buffer of $(D bufSize) elements in a worker thread,
    while making previously buffered elements from a second buffer, also of size
    $(D bufSize), available via the range interface of the returned
    object.  The returned range has a length iff $(D hasLength!S).
    $(D asyncBuf) is useful, for example, when performing expensive operations
    on the elements of ranges that represent data on a disk or network.

    Example:
    ---
    import std.conv, std.stdio;

    void main()
    {
        // Fetch lines of a file in a background thread
        // while processing previously fetched lines,
        // dealing with byLine's buffer recycling by
        // eagerly duplicating every line.
        auto lines = File("foo.txt").byLine();
        auto duped = std.algorithm.map!"a.idup"(lines);

        // Fetch more lines in the background while we
        // process the lines already read into memory
        // into a matrix of doubles.
        double[][] matrix;
        auto asyncReader = taskPool.asyncBuf(duped);

        foreach (line; asyncReader)
        {
            auto ls = line.split("\t");
            matrix ~= to!(double[])(ls);
        }
    }
    ---

    $(B Exception Handling):

    Any exceptions thrown while iterating over $(D source) are re-thrown on a
    call to $(D popFront) or, if thrown during construction, simply
    allowed to propagate to the caller.
    */
    auto asyncBuf(S)(S source, size_t bufSize = 100) if (isInputRange!S)
    {
        static final class AsyncBuf
        {
            // This is a class because the task and source both need to be on
            // the heap.

            // The element type of S.
            alias E = ElementType!S;  // Needs to be here b/c of forward ref bugs.

        private:
            E[] buf1, buf2;
            S source;
            TaskPool pool;
            Task!(run, E[] delegate(E[]), E[]) nextBufTask;
            size_t bufPos;
            bool lastTaskWaited;

            static if (hasLength!S)
            {
                size_t _length;

                // Available if hasLength!S.
                public @property size_t length() const @safe pure nothrow
                {
                    return _length;
                }
            }

            this(S source, size_t bufSize, TaskPool pool)
            {
                buf1.length = bufSize;
                buf2.length = bufSize;

                this.source = source;
                this.pool = pool;

                static if (hasLength!S)
                {
                    _length = source.length;
                }

                buf1 = fillBuf(buf1);
                submitBuf2();
            }

            E[] fillBuf(E[] buf)
            {
                assert(buf !is null);

                size_t i;
                for (; !source.empty && i < buf.length; source.popFront())
                {
                    buf[i++] = source.front;
                }

                buf = buf[0 .. i];
                return buf;
            }

            void submitBuf2()
            in
            {
                assert(nextBufTask.prev is null);
                assert(nextBufTask.next is null);
            } body
            {
                // Hack to reuse the task object.

                nextBufTask = typeof(nextBufTask).init;
                nextBufTask._args[0] = &fillBuf;
                nextBufTask._args[1] = buf2;
                pool.put(nextBufTask);
            }

            void doBufSwap()
            {
                if (lastTaskWaited)
                {
                    // Then source is empty.  Signal it here.
                    buf1 = null;
                    buf2 = null;
                    return;
                }

                buf2 = buf1;
                buf1 = nextBufTask.yieldForce;
                bufPos = 0;

                if (source.empty)
                {
                    lastTaskWaited = true;
                }
                else
                {
                    submitBuf2();
                }
            }

        public:
            E front() @property
            {
                return buf1[bufPos];
            }

            void popFront()
            {
                static if (hasLength!S)
                {
                    _length--;
                }

                bufPos++;
                if (bufPos >= buf1.length)
                {
                    doBufSwap();
                }
            }

            static if (isInfinite!S)
            {
                enum bool empty = false;
            }

            else
            {
                ///
                bool empty() @property
                {
                    // popFront() sets this when source is empty:
                    return buf1.length == 0;
                }
            }
        }
        return new AsyncBuf(source, bufSize, this);
    }

    /**
    Given a callable object $(D next) that writes to a user-provided buffer and
    a second callable object $(D empty) that determines whether more data is
    available to write via $(D next), returns an input range that
    asynchronously calls $(D next) with a set of size $(D nBuffers) of buffers
    and makes the results available in the order they were obtained via the
    input range interface of the returned object.  Similarly to the
    input range overload of $(D asyncBuf), the first half of the buffers
    are made available via the range interface while the second half are
    filled and vice-versa.

    Params:

    next = A callable object that takes a single argument that must be an array
           with mutable elements.  When called, $(D next) writes data to
           the array provided by the caller.

    empty = A callable object that takes no arguments and returns a type
            implicitly convertible to $(D bool).  This is used to signify
            that no more data is available to be obtained by calling $(D next).

    initialBufSize = The initial size of each buffer.  If $(D next) takes its
                     array by reference, it may resize the buffers.

    nBuffers = The number of buffers to cycle through when calling $(D next).

    Example:
    ---
    // Fetch lines of a file in a background
    // thread while processing previously fetched
    // lines, without duplicating any lines.
    auto file = File("foo.txt");

    void next(ref char[] buf)
    {
        file.readln(buf);
    }

    // Fetch more lines in the background while we
    // process the lines already read into memory
    // into a matrix of doubles.
    double[][] matrix;
    auto asyncReader = taskPool.asyncBuf(&next, &file.eof);

    foreach (line; asyncReader)
    {
        auto ls = line.split("\t");
        matrix ~= to!(double[])(ls);
    }
    ---

    $(B Exception Handling):

    Any exceptions thrown while iterating over $(D range) are re-thrown on a
    call to $(D popFront).

    Warning:

    Using the range returned by this function in a parallel foreach loop
    will not work because buffers may be overwritten while the task that
    processes them is in queue.  This is checked for at compile time
    and will result in a static assertion failure.
    */
    auto asyncBuf(C1, C2)(C1 next, C2 empty, size_t initialBufSize = 0, size_t nBuffers = 100)
    if (is(typeof(C2.init()) : bool) &&
        Parameters!C1.length == 1 &&
        Parameters!C2.length == 0 &&
        isArray!(Parameters!C1[0])
    ) {
        auto roundRobin = RoundRobinBuffer!(C1, C2)(next, empty, initialBufSize, nBuffers);
        return asyncBuf(roundRobin, nBuffers / 2);
    }

    ///
    template reduce(functions...)
    {
        /**
        Parallel reduce on a random access range.  Except as otherwise noted,
        usage is similar to $(REF _reduce, std,algorithm,iteration).  This
        function works by splitting the range to be reduced into work units,
        which are slices to be reduced in parallel.  Once the results from all
        work units are computed, a final serial reduction is performed on these
        results to compute the final answer. Therefore, care must be taken to
        choose the seed value appropriately.

        Because the reduction is being performed in parallel, $(D functions)
        must be associative.  For notational simplicity, let # be an
        infix operator representing $(D functions).  Then, (a # b) # c must equal
        a # (b # c).  Floating point addition is not associative
        even though addition in exact arithmetic is.  Summing floating
        point numbers using this function may give different results than summing
        serially.  However, for many practical purposes floating point addition
        can be treated as associative.

        Note that, since $(D functions) are assumed to be associative,
        additional optimizations are made to the serial portion of the reduction
        algorithm. These take advantage of the instruction level parallelism of
        modern CPUs, in addition to the thread-level parallelism that the rest
        of this module exploits.  This can lead to better than linear speedups
        relative to $(REF _reduce, std,algorithm,iteration), especially for
        fine-grained benchmarks like dot products.

        An explicit seed may be provided as the first argument.  If
        provided, it is used as the seed for all work units and for the final
        reduction of results from all work units.  Therefore, if it is not the
        identity value for the operation being performed, results may differ
        from those generated by $(REF _reduce, std,algorithm,iteration) or
        depending on how many work units are used.  The next argument must be
        the range to be reduced.
        ---
        // Find the sum of squares of a range in parallel, using
        // an explicit seed.
        //
        // Timings on an Athlon 64 X2 dual core machine:
        //
        // Parallel reduce:                     72 milliseconds
        // Using std.algorithm.reduce instead:  181 milliseconds
        auto nums = iota(10_000_000.0f);
        auto sumSquares = taskPool.reduce!"a + b"(
            0.0, std.algorithm.map!"a * a"(nums)
        );
        ---

        If no explicit seed is provided, the first element of each work unit
        is used as a seed.  For the final reduction, the result from the first
        work unit is used as the seed.
        ---
        // Find the sum of a range in parallel, using the first
        // element of each work unit as the seed.
        auto sum = taskPool.reduce!"a + b"(nums);
        ---

        An explicit work unit size may be specified as the last argument.
        Specifying too small a work unit size will effectively serialize the
        reduction, as the final reduction of the result of each work unit will
        dominate computation time.  If $(D TaskPool.size) for this instance
        is zero, this parameter is ignored and one work unit is used.
        ---
        // Use a work unit size of 100.
        auto sum2 = taskPool.reduce!"a + b"(nums, 100);

        // Work unit size of 100 and explicit seed.
        auto sum3 = taskPool.reduce!"a + b"(0.0, nums, 100);
        ---

        Parallel reduce supports multiple functions, like
        $(D std.algorithm.reduce).
        ---
        // Find both the min and max of nums.
        auto minMax = taskPool.reduce!(min, max)(nums);
        assert(minMax[0] == reduce!min(nums));
        assert(minMax[1] == reduce!max(nums));
        ---

        $(B Exception Handling):

        After this function is finished executing, any exceptions thrown
        are chained together via $(D Throwable.next) and rethrown.  The chaining
        order is non-deterministic.
         */
        auto reduce(Args...)(Args args)
        {
            import core.exception : OutOfMemoryError;
            import std.conv : emplaceRef;
            import std.exception : enforce;

            alias fun = reduceAdjoin!functions;
            alias finishFun = reduceFinish!functions;

            static if (isIntegral!(Args[$ - 1]))
            {
                size_t workUnitSize = cast(size_t) args[$ - 1];
                alias args2 = args[0..$ - 1];
                alias Args2 = Args[0..$ - 1];
            }
            else
            {
                alias args2 = args;
                alias Args2 = Args;
            }

            auto makeStartValue(Type)(Type e)
            {
                static if (functions.length == 1)
                {
                    return e;
                }
                else
                {
                    typeof(adjoin!(staticMap!(binaryFun, functions))(e, e)) seed = void;
                    foreach (i, T; seed.Types)
                    {
                        emplaceRef(seed.expand[i], e);
                    }

                    return seed;
                }
            }

            static if (args2.length == 2)
            {
                static assert(isInputRange!(Args2[1]));
                alias range = args2[1];
                alias seed = args2[0];
                enum explicitSeed = true;

                static if (!is(typeof(workUnitSize)))
                {
                    size_t workUnitSize = defaultWorkUnitSize(range.length);
                }
            }
            else
            {
                static assert(args2.length == 1);
                alias range = args2[0];

                static if (!is(typeof(workUnitSize)))
                {
                    size_t workUnitSize = defaultWorkUnitSize(range.length);
                }

                enforce(!range.empty,
                    "Cannot reduce an empty range with first element as start value.");

                auto seed = makeStartValue(range.front);
                enum explicitSeed = false;
                range.popFront();
            }

            alias E = typeof(seed);
            alias R = typeof(range);

            E reduceOnRange(R range, size_t lowerBound, size_t upperBound)
            {
                // This is for exploiting instruction level parallelism by
                // using multiple accumulator variables within each thread,
                // since we're assuming functions are associative anyhow.

                // This is so that loops can be unrolled automatically.
                enum ilpTuple = AliasSeq!(0, 1, 2, 3, 4, 5);
                enum nILP = ilpTuple.length;
                immutable subSize = (upperBound - lowerBound) / nILP;

                if (subSize <= 1)
                {
                    // Handle as a special case.
                    static if (explicitSeed)
                    {
                        E result = seed;
                    }
                    else
                    {
                        E result = makeStartValue(range[lowerBound]);
                        lowerBound++;
                    }

                    foreach (i; lowerBound .. upperBound)
                    {
                        result = fun(result, range[i]);
                    }

                    return result;
                }

                assert(subSize > 1);
                E[nILP] results;
                size_t[nILP] offsets;

                foreach (i; ilpTuple)
                {
                    offsets[i] = lowerBound + subSize * i;

                    static if (explicitSeed)
                    {
                        results[i] = seed;
                    }
                    else
                    {
                        results[i] = makeStartValue(range[offsets[i]]);
                        offsets[i]++;
                    }
                }

                immutable nLoop = subSize - (!explicitSeed);
                foreach (i; 0 .. nLoop)
                {
                    foreach (j; ilpTuple)
                    {
                        results[j] = fun(results[j], range[offsets[j]]);
                        offsets[j]++;
                    }
                }

                // Finish the remainder.
                foreach (i; nILP * subSize + lowerBound .. upperBound)
                {
                    results[$ - 1] = fun(results[$ - 1], range[i]);
                }

                foreach (i; ilpTuple[1..$])
                {
                    results[0] = finishFun(results[0], results[i]);
                }

                return results[0];
            }

            immutable len = range.length;
            if (len == 0)
            {
                return seed;
            }

            if (this.size == 0)
            {
                return finishFun(seed, reduceOnRange(range, 0, len));
            }

            // Unlike the rest of the functions here, I can't use the Task object
            // recycling trick here because this has to work on non-commutative
            // operations.  After all the tasks are done executing, fun() has to
            // be applied on the results of these to get a final result, but
            // it can't be evaluated out of order.

            if (workUnitSize > len)
            {
                workUnitSize = len;
            }

            immutable size_t nWorkUnits = (len / workUnitSize) + ((len % workUnitSize == 0) ? 0 : 1);
            assert(nWorkUnits * workUnitSize >= len);

            alias RTask = Task!(run, typeof(&reduceOnRange), R, size_t, size_t);
            RTask[] tasks;

            // Can't use alloca() due to Bug 3753.  Use a fixed buffer
            // backed by malloc().
            enum maxStack = 2_048;
            byte[maxStack] buf = void;
            immutable size_t nBytesNeeded = nWorkUnits * RTask.sizeof;

            import core.stdc.stdlib : malloc, free;
            if (nBytesNeeded < maxStack)
            {
                tasks = (cast(RTask*) buf.ptr)[0 .. nWorkUnits];
            }
            else
            {
                auto ptr = cast(RTask*) malloc(nBytesNeeded);
                if (!ptr)
                {
                    throw new OutOfMemoryError(
                        "Out of memory in std.parallelism."
                    );
                }

                tasks = ptr[0 .. nWorkUnits];
            }

            scope(exit)
            {
                if (nBytesNeeded > maxStack)
                {
                    free(tasks.ptr);
                }
            }

            foreach (ref t; tasks[])
                emplaceRef(t, RTask());

            // Hack to take the address of a nested function w/o
            // making a closure.
            static auto scopedAddress(D)(scope D del) @system
            {
                auto tmp = del;
                return tmp;
            }

            size_t curPos = 0;
            void useTask(ref RTask task)
            {
                import std.algorithm.comparison : min;

                task.pool = this;
                task._args[0] = scopedAddress(&reduceOnRange);
                task._args[3] = min(len, curPos + workUnitSize);  // upper bound.
                task._args[1] = range;  // range
                task._args[2] = curPos; // lower bound.

                curPos += workUnitSize;
            }

            foreach (ref task; tasks)
            {
                useTask(task);
            }

            foreach (i; 1 .. tasks.length - 1)
            {
                tasks[i].next = tasks[i + 1].basePtr;
                tasks[i + 1].prev = tasks[i].basePtr;
            }

            if (tasks.length > 1)
            {
                queueLock();
                scope(exit) queueUnlock();

                abstractPutGroupNoSync(
                    tasks[1].basePtr,
                    tasks[$ - 1].basePtr
                );
            }

            if (tasks.length > 0)
            {
                try
                {
                    tasks[0].job();
                }
                catch (Throwable e)
                {
                    tasks[0].exception = e;
                }
                tasks[0].taskStatus = TaskStatus.done;

                // Try to execute each of these in the current thread
                foreach (ref task; tasks[1..$])
                {
                    tryDeleteExecute(task.basePtr);
                }
            }

            // Now that we've tried to execute every task, they're all either
            // done or in progress.  Force all of them.
            E result = seed;

            Throwable firstException, lastException;

            foreach (ref task; tasks)
            {
                try
                {
                    task.yieldForce;
                }
                catch (Throwable e)
                {
                    addToChain(e, firstException, lastException);
                    continue;
                }

                if (!firstException) result = finishFun(result, task.returnVal);
            }

            if (firstException) throw firstException;

            return result;
        }
    }

    /**
    Gets the index of the current thread relative to this $(D TaskPool).  Any
    thread not in this pool will receive an index of 0.  The worker threads in
    this pool receive unique indices of 1 through $(D this.size).

    This function is useful for maintaining worker-local resources.

    Example:
    ---
    // Execute a loop that computes the greatest common
    // divisor of every number from 0 through 999 with
    // 42 in parallel.  Write the results out to
    // a set of files, one for each thread.  This allows
    // results to be written out without any synchronization.

    import std.conv, std.range, std.numeric, std.stdio;

    void main()
    {
        auto filesHandles = new File[taskPool.size + 1];
        scope(exit) {
            foreach (ref handle; fileHandles)
            {
                handle.close();
            }
        }

        foreach (i, ref handle; fileHandles)
        {
            handle = File("workerResults" ~ to!string(i) ~ ".txt");
        }

        foreach (num; parallel(iota(1_000)))
        {
            auto outHandle = fileHandles[taskPool.workerIndex];
            outHandle.writeln(num, '\t', gcd(num, 42));
        }
    }
    ---
    */
    size_t workerIndex() @property @safe const nothrow
    {
        immutable rawInd = threadIndex;
        return (rawInd >= instanceStartIndex && rawInd < instanceStartIndex + size) ?
                (rawInd - instanceStartIndex + 1) : 0;
    }

    /**
    Struct for creating worker-local storage.  Worker-local storage is
    thread-local storage that exists only for worker threads in a given
    $(D TaskPool) plus a single thread outside the pool.  It is allocated on the
    garbage collected heap in a way that avoids _false sharing, and doesn't
    necessarily have global scope within any thread.  It can be accessed from
    any worker thread in the $(D TaskPool) that created it, and one thread
    outside this $(D TaskPool).  All threads outside the pool that created a
    given instance of worker-local storage share a single slot.

    Since the underlying data for this struct is heap-allocated, this struct
    has reference semantics when passed between functions.

    The main uses cases for $(D WorkerLocalStorageStorage) are:

    1.  Performing parallel reductions with an imperative, as opposed to
    functional, programming style.  In this case, it's useful to treat
    $(D WorkerLocalStorageStorage) as local to each thread for only the parallel
    portion of an algorithm.

    2.  Recycling temporary buffers across iterations of a parallel foreach loop.

    Example:
    ---
    // Calculate pi as in our synopsis example, but
    // use an imperative instead of a functional style.
    immutable n = 1_000_000_000;
    immutable delta = 1.0L / n;

    auto sums = taskPool.workerLocalStorage(0.0L);
    foreach (i; parallel(iota(n)))
    {
        immutable x = ( i - 0.5L ) * delta;
        immutable toAdd = delta / ( 1.0 + x * x );
        sums.get += toAdd;
    }

    // Add up the results from each worker thread.
    real pi = 0;
    foreach (threadResult; sums.toRange)
    {
        pi += 4.0L * threadResult;
    }
    ---
     */
    static struct WorkerLocalStorage(T)
    {
    private:
        TaskPool pool;
        size_t size;

        size_t elemSize;
        bool* stillThreadLocal;

        static size_t roundToLine(size_t num) pure nothrow
        {
            if (num % cacheLineSize == 0)
            {
                return num;
            }
            else
            {
                return ((num / cacheLineSize) + 1) * cacheLineSize;
            }
        }

        void* data;

        void initialize(TaskPool pool)
        {
            this.pool = pool;
            size = pool.size + 1;
            stillThreadLocal = new bool;
            *stillThreadLocal = true;

            // Determines whether the GC should scan the array.
            auto blkInfo = (typeid(T).flags & 1) ?
                           cast(GC.BlkAttr) 0 :
                           GC.BlkAttr.NO_SCAN;

            immutable nElem = pool.size + 1;
            elemSize = roundToLine(T.sizeof);

            // The + 3 is to pad one full cache line worth of space on either side
            // of the data structure to make sure false sharing with completely
            // unrelated heap data is prevented, and to provide enough padding to
            // make sure that data is cache line-aligned.
            data = GC.malloc(elemSize * (nElem + 3), blkInfo) + elemSize;

            // Cache line align data ptr.
            data = cast(void*) roundToLine(cast(size_t) data);

            foreach (i; 0 .. nElem)
            {
                this.opIndex(i) = T.init;
            }
        }

        ref opIndex(this Qualified)(size_t index)
        {
            import std.conv : text;
            assert(index < size, text(index, '\t', uint.max));
            return *(cast(CopyTypeQualifiers!(Qualified, T)*) (data + elemSize * index));
        }

        void opIndexAssign(T val, size_t index)
        {
            assert(index < size);
            *(cast(T*) (data + elemSize * index)) = val;
        }

    public:
        /**
        Get the current thread's instance.  Returns by ref.
        Note that calling $(D get) from any thread
        outside the $(D TaskPool) that created this instance will return the
        same reference, so an instance of worker-local storage should only be
        accessed from one thread outside the pool that created it.  If this
        rule is violated, undefined behavior will result.

        If assertions are enabled and $(D toRange) has been called, then this
        WorkerLocalStorage instance is no longer worker-local and an assertion
        failure will result when calling this method.  This is not checked
        when assertions are disabled for performance reasons.
         */
        ref get(this Qualified)() @property
        {
            assert(*stillThreadLocal,
                "Cannot call get() on this instance of WorkerLocalStorage " ~
                "because it is no longer worker-local."
            );
            return opIndex(pool.workerIndex);
        }

        /**
        Assign a value to the current thread's instance.  This function has
        the same caveats as its overload.
        */
        void get(T val) @property
        {
            assert(*stillThreadLocal,
                "Cannot call get() on this instance of WorkerLocalStorage " ~
                "because it is no longer worker-local."
            );

            opIndexAssign(val, pool.workerIndex);
        }

        /**
        Returns a range view of the values for all threads, which can be used
        to further process the results of each thread after running the parallel
        part of your algorithm.  Do not use this method in the parallel portion
        of your algorithm.

        Calling this function sets a flag indicating that this struct is no
        longer worker-local, and attempting to use the $(D get) method again
        will result in an assertion failure if assertions are enabled.
         */
        WorkerLocalStorageRange!T toRange() @property
        {
            if (*stillThreadLocal)
            {
                *stillThreadLocal = false;

                // Make absolutely sure results are visible to all threads.
                // This is probably not necessary since some other
                // synchronization primitive will be used to signal that the
                // parallel part of the algorithm is done, but the
                // performance impact should be negligible, so it's better
                // to be safe.
                ubyte barrierDummy;
                atomicSetUbyte(barrierDummy, 1);
            }

            return WorkerLocalStorageRange!T(this);
        }
    }

    /**
    Range primitives for worker-local storage.  The purpose of this is to
    access results produced by each worker thread from a single thread once you
    are no longer using the worker-local storage from multiple threads.
    Do not use this struct in the parallel portion of your algorithm.

    The proper way to instantiate this object is to call
    $(D WorkerLocalStorage.toRange).  Once instantiated, this object behaves
    as a finite random-access range with assignable, lvalue elements and
    a length equal to the number of worker threads in the $(D TaskPool) that
    created it plus 1.
     */
    static struct WorkerLocalStorageRange(T)
    {
    private:
        WorkerLocalStorage!T workerLocalStorage;

        size_t _length;
        size_t beginOffset;

        this(WorkerLocalStorage!T wl)
        {
            this.workerLocalStorage = wl;
            _length = wl.size;
        }

    public:
        ref front(this Qualified)() @property
        {
            return this[0];
        }

        ref back(this Qualified)() @property
        {
            return this[_length - 1];
        }

        void popFront()
        {
            if (_length > 0)
            {
                beginOffset++;
                _length--;
            }
        }

        void popBack()
        {
            if (_length > 0)
            {
                _length--;
            }
        }

        typeof(this) save() @property
        {
            return this;
        }

        ref opIndex(this Qualified)(size_t index)
        {
            assert(index < _length);
            return workerLocalStorage[index + beginOffset];
        }

        void opIndexAssign(T val, size_t index)
        {
            assert(index < _length);
            workerLocalStorage[index] = val;
        }

        typeof(this) opSlice(size_t lower, size_t upper)
        {
            assert(upper <= _length);
            auto newWl = this.workerLocalStorage;
            newWl.data += lower * newWl.elemSize;
            newWl.size = upper - lower;
            return typeof(this)(newWl);
        }

        bool empty() const @property
        {
            return length == 0;
        }

        size_t length() const @property
        {
            return _length;
        }
    }

    /**
    Creates an instance of worker-local storage, initialized with a given
    value.  The value is $(D lazy) so that you can, for example, easily
    create one instance of a class for each worker.  For usage example,
    see the $(D WorkerLocalStorage) struct.
     */
    WorkerLocalStorage!T workerLocalStorage(T)(lazy T initialVal = T.init)
    {
        WorkerLocalStorage!T ret;
        ret.initialize(this);
        foreach (i; 0 .. size + 1)
        {
            ret[i] = initialVal;
        }

        // Memory barrier to make absolutely sure that what we wrote is
        // visible to worker threads.
        ubyte barrierDummy;
        atomicSetUbyte(barrierDummy, 0);

        return ret;
    }

    /**
    Signals to all worker threads to terminate as soon as they are finished
    with their current $(D Task), or immediately if they are not executing a
    $(D Task).  $(D Task)s that were in queue will not be executed unless
    a call to $(D Task.workForce), $(D Task.yieldForce) or $(D Task.spinForce)
    causes them to be executed.

    Use only if you have waited on every $(D Task) and therefore know the
    queue is empty, or if you speculatively executed some tasks and no longer
    need the results.
     */
    void stop() @trusted
    {
        queueLock();
        scope(exit) queueUnlock();
        atomicSetUbyte(status, PoolState.stopNow);
        notifyAll();
    }

    /**
    Signals worker threads to terminate when the queue becomes empty.

    If blocking argument is true, wait for all worker threads to terminate
    before returning.  This option might be used in applications where
    task results are never consumed-- e.g. when $(D TaskPool) is employed as a
    rudimentary scheduler for tasks which communicate by means other than
    return values.

    Warning:  Calling this function with $(D blocking = true) from a worker
              thread that is a member of the same $(D TaskPool) that
              $(D finish) is being called on will result in a deadlock.
     */
    void finish(bool blocking = false) @trusted
    {
        {
            queueLock();
            scope(exit) queueUnlock();
            atomicCasUbyte(status, PoolState.running, PoolState.finishing);
            notifyAll();
        }
        if (blocking)
        {
            // Use this thread as a worker until everything is finished.
            executeWorkLoop();

            foreach (t; pool)
            {
                // Maybe there should be something here to prevent a thread
                // from calling join() on itself if this function is called
                // from a worker thread in the same pool, but:
                //
                // 1.  Using an if statement to skip join() would result in
                //     finish() returning without all tasks being finished.
                //
                // 2.  If an exception were thrown, it would bubble up to the
                //     Task from which finish() was called and likely be
                //     swallowed.
                t.join();
            }
        }
    }

    /// Returns the number of worker threads in the pool.
    @property size_t size() @safe const pure nothrow
    {
        return pool.length;
    }

    /**
    Put a $(D Task) object on the back of the task queue.  The $(D Task)
    object may be passed by pointer or reference.

    Example:
    ---
    import std.file;

    // Create a task.
    auto t = task!read("foo.txt");

    // Add it to the queue to be executed.
    taskPool.put(t);
    ---

    Notes:

    @trusted overloads of this function are called for $(D Task)s if
    $(REF hasUnsharedAliasing, std,traits) is false for the $(D Task)'s
    return type or the function the $(D Task) executes is $(D pure).
    $(D Task) objects that meet all other requirements specified in the
    $(D @trusted) overloads of $(D task) and $(D scopedTask) may be created
    and executed from $(D @safe) code via $(D Task.executeInNewThread) but
    not via $(D TaskPool).

    While this function takes the address of variables that may
    be on the stack, some overloads are marked as @trusted.
    $(D Task) includes a destructor that waits for the task to complete
    before destroying the stack frame it is allocated on.  Therefore,
    it is impossible for the stack frame to be destroyed before the task is
    complete and no longer referenced by a $(D TaskPool).
    */
    void put(alias fun, Args...)(ref Task!(fun, Args) task)
    if (!isSafeReturn!(typeof(task)))
    {
        task.pool = this;
        abstractPut(task.basePtr);
    }

    /// Ditto
    void put(alias fun, Args...)(Task!(fun, Args)* task)
    if (!isSafeReturn!(typeof(*task)))
    {
        import std.exception : enforce;
        enforce(task !is null, "Cannot put a null Task on a TaskPool queue.");
        put(*task);
    }

    @trusted void put(alias fun, Args...)(ref Task!(fun, Args) task)
    if (isSafeReturn!(typeof(task)))
    {
        task.pool = this;
        abstractPut(task.basePtr);
    }

    @trusted void put(alias fun, Args...)(Task!(fun, Args)* task)
    if (isSafeReturn!(typeof(*task)))
    {
        import std.exception : enforce;
        enforce(task !is null, "Cannot put a null Task on a TaskPool queue.");
        put(*task);
    }

    /**
    These properties control whether the worker threads are daemon threads.
    A daemon thread is automatically terminated when all non-daemon threads
    have terminated.  A non-daemon thread will prevent a program from
    terminating as long as it has not terminated.

    If any $(D TaskPool) with non-daemon threads is active, either $(D stop)
    or $(D finish) must be called on it before the program can terminate.

    The worker treads in the $(D TaskPool) instance returned by the
    $(D taskPool) property are daemon by default.  The worker threads of
    manually instantiated task pools are non-daemon by default.

    Note:  For a size zero pool, the getter arbitrarily returns true and the
           setter has no effect.
    */
    bool isDaemon() @property @trusted
    {
        queueLock();
        scope(exit) queueUnlock();
        return (size == 0) ? true : pool[0].isDaemon;
    }

    /// Ditto
    void isDaemon(bool newVal) @property @trusted
    {
        queueLock();
        scope(exit) queueUnlock();
        foreach (thread; pool)
        {
            thread.isDaemon = newVal;
        }
    }

    /**
    These functions allow getting and setting the OS scheduling priority of
    the worker threads in this $(D TaskPool).  They forward to
    $(D core.thread.Thread.priority), so a given priority value here means the
    same thing as an identical priority value in $(D core.thread).

    Note:  For a size zero pool, the getter arbitrarily returns
           $(D core.thread.Thread.PRIORITY_MIN) and the setter has no effect.
    */
    int priority() @property @trusted
    {
        return (size == 0) ? core.thread.Thread.PRIORITY_MIN :
        pool[0].priority;
    }

    /// Ditto
    void priority(int newPriority) @property @trusted
    {
        if (size > 0)
        {
            foreach (t; pool)
            {
                t.priority = newPriority;
            }
        }
    }
}

/**
Returns a lazily initialized global instantiation of $(D TaskPool).
This function can safely be called concurrently from multiple non-worker
threads.  The worker threads in this pool are daemon threads, meaning that it
is not necessary to call $(D TaskPool.stop) or $(D TaskPool.finish) before
terminating the main thread.
*/
@property TaskPool taskPool() @trusted
{
    import std.concurrency : initOnce;
    __gshared TaskPool pool;
    return initOnce!pool({
        auto p = new TaskPool(defaultPoolThreads);
        p.isDaemon = true;
        return p;
    }());
}

private shared uint _defaultPoolThreads = uint.max;

/**
These properties get and set the number of worker threads in the $(D TaskPool)
instance returned by $(D taskPool).  The default value is $(D totalCPUs) - 1.
Calling the setter after the first call to $(D taskPool) does not changes
number of worker threads in the instance returned by $(D taskPool).
*/
@property uint defaultPoolThreads() @trusted
{
    const local = atomicLoad(_defaultPoolThreads);
    return local < uint.max ? local : totalCPUs - 1;
}

/// Ditto
@property void defaultPoolThreads(uint newVal) @trusted
{
    atomicStore(_defaultPoolThreads, newVal);
}

/**
Convenience functions that forwards to $(D taskPool.parallel).  The
purpose of these is to make parallel foreach less verbose and more
readable.

Example:
---
// Find the logarithm of every number from
// 1 to 1_000_000 in parallel, using the
// default TaskPool instance.
auto logs = new double[1_000_000];

foreach (i, ref elem; parallel(logs))
{
    elem = log(i + 1.0);
}
---

*/
ParallelForeach!R parallel(R)(R range)
{
    return taskPool.parallel(range);
}

/// Ditto
ParallelForeach!R parallel(R)(R range, size_t workUnitSize)
{
    return taskPool.parallel(range, workUnitSize);
}

// Thrown when a parallel foreach loop is broken from.
class ParallelForeachError : Error
{
    this()
    {
        super("Cannot break from a parallel foreach loop using break, return, "
              ~ "labeled break/continue or goto statements.");
    }
}

/*------Structs that implement opApply for parallel foreach.------------------*/
private template randLen(R)
{
    enum randLen = isRandomAccessRange!R && hasLength!R;
}

private void submitAndExecute(
    TaskPool pool,
    scope void delegate() doIt
)
{
    import core.exception : OutOfMemoryError;
    immutable nThreads = pool.size + 1;

    alias PTask = typeof(scopedTask(doIt));
    import core.stdc.stdlib : malloc, free;
    import core.stdc.string : memcpy;

    // The logical thing to do would be to just use alloca() here, but that
    // causes problems on Windows for reasons that I don't understand
    // (tentatively a compiler bug) and definitely doesn't work on Posix due
    // to Bug 3753.  Therefore, allocate a fixed buffer and fall back to
    // malloc() if someone's using a ridiculous amount of threads.  Also,
    // the using a byte array instead of a PTask array as the fixed buffer
    // is to prevent d'tors from being called on uninitialized excess PTask
    // instances.
    enum nBuf = 64;
    byte[nBuf * PTask.sizeof] buf = void;
    PTask[] tasks;
    if (nThreads <= nBuf)
    {
        tasks = (cast(PTask*) buf.ptr)[0 .. nThreads];
    }
    else
    {
        auto ptr = cast(PTask*) malloc(nThreads * PTask.sizeof);
        if (!ptr) throw new OutOfMemoryError("Out of memory in std.parallelism.");
        tasks = ptr[0 .. nThreads];
    }

    scope(exit)
    {
        if (nThreads > nBuf)
        {
            free(tasks.ptr);
        }
    }

    foreach (ref t; tasks)
    {
        import core.stdc.string : memcpy;

        // This silly looking code is necessary to prevent d'tors from being
        // called on uninitialized objects.
        auto temp = scopedTask(doIt);
        memcpy(&t, &temp, PTask.sizeof);

        // This has to be done to t after copying, not temp before copying.
        // Otherwise, temp's destructor will sit here and wait for the
        // task to finish.
        t.pool = pool;
    }

    foreach (i; 1 .. tasks.length - 1)
    {
        tasks[i].next = tasks[i + 1].basePtr;
        tasks[i + 1].prev = tasks[i].basePtr;
    }

    if (tasks.length > 1)
    {
        pool.queueLock();
        scope(exit) pool.queueUnlock();

        pool.abstractPutGroupNoSync(
            tasks[1].basePtr,
            tasks[$ - 1].basePtr
        );
    }

    if (tasks.length > 0)
    {
        try
        {
            tasks[0].job();
        }
        catch (Throwable e)
        {
            tasks[0].exception = e; // nocoverage
        }
        tasks[0].taskStatus = TaskStatus.done;

        // Try to execute each of these in the current thread
        foreach (ref task; tasks[1..$])
        {
            pool.tryDeleteExecute(task.basePtr);
        }
    }

    Throwable firstException, lastException;

    foreach (i, ref task; tasks)
    {
        try
        {
            task.yieldForce;
        }
        catch (Throwable e)
        {
            addToChain(e, firstException, lastException);
            continue;
        }
    }

    if (firstException) throw firstException;
}

void foreachErr()
{
    throw new ParallelForeachError();
}

int doSizeZeroCase(R, Delegate)(ref ParallelForeach!R p, Delegate dg)
{
    with(p)
    {
        int res = 0;
        size_t index = 0;

        // The explicit ElementType!R in the foreach loops is necessary for
        // correct behavior when iterating over strings.
        static if (hasLvalueElements!R)
        {
            foreach (ref ElementType!R elem; range)
            {
                static if (Parameters!dg.length == 2)
                {
                    res = dg(index, elem);
                }
                else
                {
                    res = dg(elem);
                }
                if (res) break;
                index++;
            }
        }
        else
        {
            foreach (ElementType!R elem; range)
            {
                static if (Parameters!dg.length == 2)
                {
                    res = dg(index, elem);
                }
                else
                {
                    res = dg(elem);
                }
                if (res) break;
                index++;
            }
        }
        if (res) foreachErr;
        return res;
    }
}

private enum string parallelApplyMixinRandomAccess = q{
    // Handle empty thread pool as special case.
    if (pool.size == 0)
    {
        return doSizeZeroCase(this, dg);
    }

    // Whether iteration is with or without an index variable.
    enum withIndex = Parameters!(typeof(dg)).length == 2;

    shared size_t workUnitIndex = size_t.max;  // Effectively -1:  chunkIndex + 1 == 0
    immutable len = range.length;
    if (!len) return 0;

    shared bool shouldContinue = true;

    void doIt()
    {
        import std.algorithm.comparison : min;

        scope(failure)
        {
            // If an exception is thrown, all threads should bail.
            atomicStore(shouldContinue, false);
        }

        while (atomicLoad(shouldContinue))
        {
            immutable myUnitIndex = atomicOp!"+="(workUnitIndex, 1);
            immutable start = workUnitSize * myUnitIndex;
            if (start >= len)
            {
                atomicStore(shouldContinue, false);
                break;
            }

            immutable end = min(len, start + workUnitSize);

            foreach (i; start .. end)
            {
                static if (withIndex)
                {
                    if (dg(i, range[i])) foreachErr();
                }
                else
                {
                    if (dg(range[i])) foreachErr();
                }
            }
        }
    }

    submitAndExecute(pool, &doIt);

    return 0;
};

enum string parallelApplyMixinInputRange = q{
    // Handle empty thread pool as special case.
    if (pool.size == 0)
    {
        return doSizeZeroCase(this, dg);
    }

    // Whether iteration is with or without an index variable.
    enum withIndex = Parameters!(typeof(dg)).length == 2;

    // This protects the range while copying it.
    auto rangeMutex = new Mutex();

    shared bool shouldContinue = true;

    // The total number of elements that have been popped off range.
    // This is updated only while protected by rangeMutex;
    size_t nPopped = 0;

    static if (
        is(typeof(range.buf1)) &&
        is(typeof(range.bufPos)) &&
        is(typeof(range.doBufSwap()))
    )
    {
        // Make sure we don't have the buffer recycling overload of
        // asyncBuf.
        static if (
            is(typeof(range.source)) &&
            isRoundRobin!(typeof(range.source))
        )
        {
            static assert(0, "Cannot execute a parallel foreach loop on " ~
            "the buffer recycling overload of asyncBuf.");
        }

        enum bool bufferTrick = true;
    }
    else
    {
        enum bool bufferTrick = false;
    }

    void doIt()
    {
        scope(failure)
        {
            // If an exception is thrown, all threads should bail.
            atomicStore(shouldContinue, false);
        }

        static if (hasLvalueElements!R)
        {
            alias Temp = ElementType!R*[];
            Temp temp;

            // Returns:  The previous value of nPopped.
            size_t makeTemp()
            {
                import std.algorithm.internal : addressOf;
                import std.array : uninitializedArray;

                if (temp is null)
                {
                    temp = uninitializedArray!Temp(workUnitSize);
                }

                rangeMutex.lock();
                scope(exit) rangeMutex.unlock();

                size_t i = 0;
                for (; i < workUnitSize && !range.empty; range.popFront(), i++)
                {
                    temp[i] = addressOf(range.front);
                }

                temp = temp[0 .. i];
                auto ret = nPopped;
                nPopped += temp.length;
                return ret;
            }

        }
        else
        {

            alias Temp = ElementType!R[];
            Temp temp;

            // Returns:  The previous value of nPopped.
            static if (!bufferTrick) size_t makeTemp()
            {
                import std.array : uninitializedArray;

                if (temp is null)
                {
                    temp = uninitializedArray!Temp(workUnitSize);
                }

                rangeMutex.lock();
                scope(exit) rangeMutex.unlock();

                size_t i = 0;
                for (; i < workUnitSize && !range.empty; range.popFront(), i++)
                {
                    temp[i] = range.front;
                }

                temp = temp[0 .. i];
                auto ret = nPopped;
                nPopped += temp.length;
                return ret;
            }

            static if (bufferTrick) size_t makeTemp()
            {
                import std.algorithm.mutation : swap;
                rangeMutex.lock();
                scope(exit) rangeMutex.unlock();

                // Elide copying by just swapping buffers.
                temp.length = range.buf1.length;
                swap(range.buf1, temp);

                // This is necessary in case popFront() has been called on
                // range before entering the parallel foreach loop.
                temp = temp[range.bufPos..$];

                static if (is(typeof(range._length)))
                {
                    range._length -= (temp.length - range.bufPos);
                }

                range.doBufSwap();
                auto ret = nPopped;
                nPopped += temp.length;
                return ret;
            }
        }

        while (atomicLoad(shouldContinue))
        {
            auto overallIndex = makeTemp();
            if (temp.empty)
            {
                atomicStore(shouldContinue, false);
                break;
            }

            foreach (i; 0 .. temp.length)
            {
                scope(success) overallIndex++;

                static if (hasLvalueElements!R)
                {
                    static if (withIndex)
                    {
                        if (dg(overallIndex, *temp[i])) foreachErr();
                    }
                    else
                    {
                        if (dg(*temp[i])) foreachErr();
                    }
                }
                else
                {
                    static if (withIndex)
                    {
                        if (dg(overallIndex, temp[i])) foreachErr();
                    }
                    else
                    {
                        if (dg(temp[i])) foreachErr();
                    }
                }
            }
        }
    }

    submitAndExecute(pool, &doIt);

    return 0;
};

// Calls e.next until the end of the chain is found.
private Throwable findLastException(Throwable e) pure nothrow
{
    if (e is null) return null;

    while (e.next)
    {
        e = e.next;
    }

    return e;
}

// Adds e to the exception chain.
private void addToChain(
    Throwable e,
    ref Throwable firstException,
    ref Throwable lastException
) pure nothrow
{
    if (firstException)
    {
        assert(lastException); // nocoverage
        lastException.next = e; // nocoverage
        lastException = findLastException(e); // nocoverage
    }
    else
    {
        firstException = e;
        lastException = findLastException(e);
    }
}

private struct ParallelForeach(R)
{
    TaskPool pool;
    R range;
    size_t workUnitSize;
    alias E = ElementType!R;

    static if (hasLvalueElements!R)
    {
        alias NoIndexDg = int delegate(ref E);
        alias IndexDg = int delegate(size_t, ref E);
    }
    else
    {
        alias NoIndexDg = int delegate(E);
        alias IndexDg = int delegate(size_t, E);
    }

    int opApply(scope NoIndexDg dg)
    {
        static if (randLen!R)
        {
            mixin(parallelApplyMixinRandomAccess);
        }
        else
        {
            mixin(parallelApplyMixinInputRange);
        }
    }

    int opApply(scope IndexDg dg)
    {
        static if (randLen!R)
        {
            mixin(parallelApplyMixinRandomAccess);
        }
        else
        {
            mixin(parallelApplyMixinInputRange);
        }
    }
}

/*
This struct buffers the output of a callable that outputs data into a
user-supplied buffer into a set of buffers of some fixed size.  It allows these
buffers to be accessed with an input range interface.  This is used internally
in the buffer-recycling overload of TaskPool.asyncBuf, which creates an
instance and forwards it to the input range overload of asyncBuf.
*/
private struct RoundRobinBuffer(C1, C2)
{
    // No need for constraints because they're already checked for in asyncBuf.

    alias Array = Parameters!(C1.init)[0];
    alias T = typeof(Array.init[0]);

    T[][] bufs;
    size_t index;
    C1 nextDel;
    C2 emptyDel;
    bool _empty;
    bool primed;

    this(
        C1 nextDel,
        C2 emptyDel,
        size_t initialBufSize,
        size_t nBuffers
    ) {
        this.nextDel = nextDel;
        this.emptyDel = emptyDel;
        bufs.length = nBuffers;

        foreach (ref buf; bufs)
        {
            buf.length = initialBufSize;
        }
    }

    void prime()
    in
    {
        assert(!empty);
    }
    body
    {
        scope(success) primed = true;
        nextDel(bufs[index]);
    }


    T[] front() @property
    in
    {
        assert(!empty);
    }
    body
    {
        if (!primed) prime();
        return bufs[index];
    }

    void popFront()
    {
        if (empty || emptyDel())
        {
            _empty = true;
            return;
        }

        index = (index + 1) % bufs.length;
        primed = false;
    }

    bool empty() @property const @safe pure nothrow
    {
        return _empty;
    }
}

version (unittest)
{
    // This was the only way I could get nested maps to work.
    __gshared TaskPool poolInstance;

    import std.stdio;
}

// These test basic functionality but don't stress test for threading bugs.
// These are the tests that should be run every time Phobos is compiled.
@system unittest
{
    import std.algorithm.comparison : equal, min, max;
    import std.algorithm.iteration : filter, map, reduce;
    import std.array : split;
    import std.conv : text;
    import std.exception : assertThrown;
    import std.math : approxEqual, sqrt, log, abs;
    import std.range : indexed, iota, join;
    import std.typecons : Tuple, tuple;

    poolInstance = new TaskPool(2);
    scope(exit) poolInstance.stop();

    // The only way this can be verified is manually.
    debug(std_parallelism) stderr.writeln("totalCPUs = ", totalCPUs);

    auto oldPriority = poolInstance.priority;
    poolInstance.priority = Thread.PRIORITY_MAX;
    assert(poolInstance.priority == Thread.PRIORITY_MAX);

    poolInstance.priority = Thread.PRIORITY_MIN;
    assert(poolInstance.priority == Thread.PRIORITY_MIN);

    poolInstance.priority = oldPriority;
    assert(poolInstance.priority == oldPriority);

    static void refFun(ref uint num)
    {
        num++;
    }

    uint x;

    // Test task().
    auto t = task!refFun(x);
    poolInstance.put(t);
    t.yieldForce;
    assert(t.args[0] == 1);

    auto t2 = task(&refFun, x);
    poolInstance.put(t2);
    t2.yieldForce;
    assert(t2.args[0] == 1);

    // Test scopedTask().
    auto st = scopedTask!refFun(x);
    poolInstance.put(st);
    st.yieldForce;
    assert(st.args[0] == 1);

    auto st2 = scopedTask(&refFun, x);
    poolInstance.put(st2);
    st2.yieldForce;
    assert(st2.args[0] == 1);

    // Test executeInNewThread().
    auto ct = scopedTask!refFun(x);
    ct.executeInNewThread(Thread.PRIORITY_MAX);
    ct.yieldForce;
    assert(ct.args[0] == 1);

    // Test ref return.
    uint toInc = 0;
    static ref T makeRef(T)(ref T num)
    {
        return num;
    }

    auto t3 = task!makeRef(toInc);
    taskPool.put(t3);
    assert(t3.args[0] == 0);
    t3.spinForce++;
    assert(t3.args[0] == 1);

    static void testSafe() @safe {
        static int bump(int num)
        {
            return num + 1;
        }

        auto safePool = new TaskPool(0);
        auto t = task(&bump, 1);
        taskPool.put(t);
        assert(t.yieldForce == 2);

        auto st = scopedTask(&bump, 1);
        taskPool.put(st);
        assert(st.yieldForce == 2);
        safePool.stop();
    }

    auto arr = [1,2,3,4,5];
    auto nums = new uint[5];
    auto nums2 = new uint[5];

    foreach (i, ref elem; poolInstance.parallel(arr))
    {
        elem++;
        nums[i] = cast(uint) i + 2;
        nums2[i] = elem;
    }

    assert(nums == [2,3,4,5,6], text(nums));
    assert(nums2 == nums, text(nums2));
    assert(arr == nums, text(arr));

    // Test const/immutable arguments.
    static int add(int lhs, int rhs)
    {
        return lhs + rhs;
    }
    immutable addLhs = 1;
    immutable addRhs = 2;
    auto addTask = task(&add, addLhs, addRhs);
    auto addScopedTask = scopedTask(&add, addLhs, addRhs);
    poolInstance.put(addTask);
    poolInstance.put(addScopedTask);
    assert(addTask.yieldForce == 3);
    assert(addScopedTask.yieldForce == 3);

    // Test parallel foreach with non-random access range.
    auto range = filter!"a != 666"([0, 1, 2, 3, 4]);

    foreach (i, elem; poolInstance.parallel(range))
    {
        nums[i] = cast(uint) i;
    }

    assert(nums == [0,1,2,3,4]);

    auto logs = new double[1_000_000];
    foreach (i, ref elem; poolInstance.parallel(logs))
    {
        elem = log(i + 1.0);
    }

    foreach (i, elem; logs)
    {
        assert(approxEqual(elem, cast(double) log(i + 1)));
    }

    assert(poolInstance.amap!"a * a"([1,2,3,4,5]) == [1,4,9,16,25]);
    assert(poolInstance.amap!"a * a"([1,2,3,4,5], new long[5]) == [1,4,9,16,25]);
    assert(poolInstance.amap!("a * a", "-a")([1,2,3]) ==
           [tuple(1, -1), tuple(4, -2), tuple(9, -3)]);

    auto tupleBuf = new Tuple!(int, int)[3];
    poolInstance.amap!("a * a", "-a")([1,2,3], tupleBuf);
    assert(tupleBuf == [tuple(1, -1), tuple(4, -2), tuple(9, -3)]);
    poolInstance.amap!("a * a", "-a")([1,2,3], 5, tupleBuf);
    assert(tupleBuf == [tuple(1, -1), tuple(4, -2), tuple(9, -3)]);

    // Test amap with a non-array buffer.
    auto toIndex = new int[5];
    auto ind = indexed(toIndex, [3, 1, 4, 0, 2]);
    poolInstance.amap!"a * 2"([1, 2, 3, 4, 5], ind);
    assert(equal(ind, [2, 4, 6, 8, 10]));
    assert(equal(toIndex, [8, 4, 10, 2, 6]));
    poolInstance.amap!"a / 2"(ind, ind);
    assert(equal(ind, [1, 2, 3, 4, 5]));
    assert(equal(toIndex, [4, 2, 5, 1, 3]));

    auto buf = new int[5];
    poolInstance.amap!"a * a"([1,2,3,4,5], buf);
    assert(buf == [1,4,9,16,25]);
    poolInstance.amap!"a * a"([1,2,3,4,5], 4, buf);
    assert(buf == [1,4,9,16,25]);

    assert(poolInstance.reduce!"a + b"([1]) == 1);
    assert(poolInstance.reduce!"a + b"([1,2,3,4]) == 10);
    assert(poolInstance.reduce!"a + b"(0.0, [1,2,3,4]) == 10);
    assert(poolInstance.reduce!"a + b"(0.0, [1,2,3,4], 1) == 10);
    assert(poolInstance.reduce!(min, max)([1,2,3,4]) == tuple(1, 4));
    assert(poolInstance.reduce!("a + b", "a * b")(tuple(0, 1), [1,2,3,4]) ==
           tuple(10, 24));

    immutable serialAns = reduce!"a + b"(iota(1000));
    assert(poolInstance.reduce!"a + b"(0, iota(1000)) == serialAns);
    assert(poolInstance.reduce!"a + b"(iota(1000)) == serialAns);

    // Test worker-local storage.
    auto wl = poolInstance.workerLocalStorage(0);
    foreach (i; poolInstance.parallel(iota(1000), 1))
    {
        wl.get = wl.get + i;
    }

    auto wlRange = wl.toRange;
    auto parallelSum = poolInstance.reduce!"a + b"(wlRange);
    assert(parallelSum == 499500);
    assert(wlRange[0 .. 1][0] == wlRange[0]);
    assert(wlRange[1 .. 2][0] == wlRange[1]);

    // Test finish()
    {
        static void slowFun() { Thread.sleep(dur!"msecs"(1)); }

        auto pool1 = new TaskPool();
        auto tSlow = task!slowFun();
        pool1.put(tSlow);
        pool1.finish();
        tSlow.yieldForce;
        // Can't assert that pool1.status == PoolState.stopNow because status
        // doesn't change until after the "done" flag is set and the waiting
        // thread is woken up.

        auto pool2 = new TaskPool();
        auto tSlow2 = task!slowFun();
        pool2.put(tSlow2);
        pool2.finish(true); // blocking
        assert(tSlow2.done);

        // Test fix for Bug 8582 by making pool size zero.
        auto pool3 = new TaskPool(0);
        auto tSlow3 = task!slowFun();
        pool3.put(tSlow3);
        pool3.finish(true); // blocking
        assert(tSlow3.done);

        // This is correct because no thread will terminate unless pool2.status
        // and pool3.status have already been set to stopNow.
        assert(pool2.status == TaskPool.PoolState.stopNow);
        assert(pool3.status == TaskPool.PoolState.stopNow);
    }

    // Test default pool stuff.
    assert(taskPool.size == totalCPUs - 1);

    nums = new uint[1000];
    foreach (i; parallel(iota(1000)))
    {
        nums[i] = cast(uint) i;
    }
    assert(equal(nums, iota(1000)));

    assert(equal(
               poolInstance.map!"a * a"(iota(30_000_001), 10_000),
               map!"a * a"(iota(30_000_001))
           ));

    // The filter is to kill random access and test the non-random access
    // branch.
    assert(equal(
               poolInstance.map!"a * a"(
                   filter!"a == a"(iota(30_000_001)
                                  ), 10_000, 1000),
               map!"a * a"(iota(30_000_001))
           ));

    assert(
        reduce!"a + b"(0UL,
                       poolInstance.map!"a * a"(iota(3_000_001), 10_000)
                      ) ==
        reduce!"a + b"(0UL,
                       map!"a * a"(iota(3_000_001))
                      )
    );

    assert(equal(
               iota(1_000_002),
               poolInstance.asyncBuf(filter!"a == a"(iota(1_000_002)))
           ));

    {
        import std.conv : to;
        import std.file : deleteme;

        string temp_file = deleteme ~ "-tempDelMe.txt";
        auto file = File(temp_file, "wb");
        scope(exit)
        {
            file.close();
            import std.file;
            remove(temp_file);
        }

        auto written = [[1.0, 2, 3], [4.0, 5, 6], [7.0, 8, 9]];
        foreach (row; written)
        {
            file.writeln(join(to!(string[])(row), "\t"));
        }

        file = File(temp_file);

        void next(ref char[] buf)
        {
            file.readln(buf);
            import std.string : chomp;
            buf = chomp(buf);
        }

        double[][] read;
        auto asyncReader = taskPool.asyncBuf(&next, &file.eof);

        foreach (line; asyncReader)
        {
            if (line.length == 0) continue;
            auto ls = line.split("\t");
            read ~= to!(double[])(ls);
        }

        assert(read == written);
        file.close();
    }

    // Test Map/AsyncBuf chaining.

    auto abuf = poolInstance.asyncBuf(iota(-1.0, 3_000_000), 100);
    auto temp = poolInstance.map!sqrt(
                    abuf, 100, 5
                );
    auto lmchain = poolInstance.map!"a * a"(temp, 100, 5);
    lmchain.popFront();

    int ii;
    foreach ( elem; (lmchain))
    {
        if (!approxEqual(elem, ii))
        {
            stderr.writeln(ii, '\t', elem);
        }
        ii++;
    }

    // Test buffer trick in parallel foreach.
    abuf = poolInstance.asyncBuf(iota(-1.0, 1_000_000), 100);
    abuf.popFront();
    auto bufTrickTest = new size_t[abuf.length];
    foreach (i, elem; parallel(abuf))
    {
        bufTrickTest[i] = i;
    }

    assert(equal(iota(1_000_000), bufTrickTest));

    auto myTask = task!(abs)(-1);
    taskPool.put(myTask);
    assert(myTask.spinForce == 1);

    // Test that worker local storage from one pool receives an index of 0
    // when the index is queried w.r.t. another pool.  The only way to do this
    // is non-deterministically.
    foreach (i; parallel(iota(1000), 1))
    {
        assert(poolInstance.workerIndex == 0);
    }

    foreach (i; poolInstance.parallel(iota(1000), 1))
    {
        assert(taskPool.workerIndex == 0);
    }

    // Test exception handling.
    static void parallelForeachThrow()
    {
        foreach (elem; parallel(iota(10)))
        {
            throw new Exception("");
        }
    }

    assertThrown!Exception(parallelForeachThrow());

    static int reduceException(int a, int b)
    {
        throw new Exception("");
    }

    assertThrown!Exception(
        poolInstance.reduce!reduceException(iota(3))
    );

    static int mapException(int a)
    {
        throw new Exception("");
    }

    assertThrown!Exception(
        poolInstance.amap!mapException(iota(3))
    );

    static void mapThrow()
    {
        auto m = poolInstance.map!mapException(iota(3));
        m.popFront();
    }

    assertThrown!Exception(mapThrow());

    struct ThrowingRange
    {
        @property int front()
        {
            return 1;
        }
        void popFront()
        {
            throw new Exception("");
        }
        enum bool empty = false;
    }

    assertThrown!Exception(poolInstance.asyncBuf(ThrowingRange.init));
}

//version = parallelismStressTest;

// These are more like stress tests than real unit tests.  They print out
// tons of stuff and should not be run every time make unittest is run.
version (parallelismStressTest)
{
    @safe unittest
    {
        size_t attempt;
        for (; attempt < 10; attempt++)
            foreach (poolSize; [0, 4])
        {

            poolInstance = new TaskPool(poolSize);

            uint[] numbers = new uint[1_000];

            foreach (i; poolInstance.parallel( iota(0, numbers.length)) )
            {
                numbers[i] = cast(uint) i;
            }

            // Make sure it works.
            foreach (i; 0 .. numbers.length)
            {
                assert(numbers[i] == i);
            }

            stderr.writeln("Done creating nums.");


            auto myNumbers = filter!"a % 7 > 0"( iota(0, 1000));
            foreach (num; poolInstance.parallel(myNumbers))
            {
                assert(num % 7 > 0 && num < 1000);
            }
            stderr.writeln("Done modulus test.");

            uint[] squares = poolInstance.amap!"a * a"(numbers, 100);
            assert(squares.length == numbers.length);
            foreach (i, number; numbers)
            {
                assert(squares[i] == number * number);
            }
            stderr.writeln("Done squares.");

            auto sumFuture = task!( reduce!"a + b" )(numbers);
            poolInstance.put(sumFuture);

            ulong sumSquares = 0;
            foreach (elem; numbers)
            {
                sumSquares += elem * elem;
            }

            uint mySum = sumFuture.spinForce();
            assert(mySum == 999 * 1000 / 2);

            auto mySumParallel = poolInstance.reduce!"a + b"(numbers);
            assert(mySum == mySumParallel);
            stderr.writeln("Done sums.");

            auto myTask = task(
            {
                synchronized writeln("Our lives are parallel...Our lives are parallel.");
            });
            poolInstance.put(myTask);

            auto nestedOuter = "abcd";
            auto nestedInner =  iota(0, 10, 2);

            foreach (i, letter; poolInstance.parallel(nestedOuter, 1))
            {
                foreach (j, number; poolInstance.parallel(nestedInner, 1))
                {
                    synchronized writeln(i, ": ", letter, "  ", j, ": ", number);
                }
            }

            poolInstance.stop();
        }

        assert(attempt == 10);
        writeln("Press enter to go to next round of unittests.");
        readln();
    }

    // These unittests are intended more for actual testing and not so much
    // as examples.
    @safe unittest
    {
        foreach (attempt; 0 .. 10)
        foreach (poolSize; [0, 4])
        {
            poolInstance = new TaskPool(poolSize);

            // Test indexing.
            stderr.writeln("Creator Raw Index:  ", poolInstance.threadIndex);
            assert(poolInstance.workerIndex() == 0);

            // Test worker-local storage.
            auto workerLocalStorage = poolInstance.workerLocalStorage!uint(1);
            foreach (i; poolInstance.parallel(iota(0U, 1_000_000)))
            {
                workerLocalStorage.get++;
            }
            assert(reduce!"a + b"(workerLocalStorage.toRange) ==
            1_000_000 + poolInstance.size + 1);

            // Make sure work is reasonably balanced among threads.  This test is
            // non-deterministic and is more of a sanity check than something that
            // has an absolute pass/fail.
            shared(uint)[void*] nJobsByThread;
            foreach (thread; poolInstance.pool)
            {
                nJobsByThread[cast(void*) thread] = 0;
            }
            nJobsByThread[ cast(void*) Thread.getThis()] = 0;

            foreach (i; poolInstance.parallel( iota(0, 1_000_000), 100 ))
            {
                atomicOp!"+="( nJobsByThread[ cast(void*) Thread.getThis() ], 1);
            }

            stderr.writeln("\nCurrent thread is:  ",
            cast(void*) Thread.getThis());
            stderr.writeln("Workload distribution:  ");
            foreach (k, v; nJobsByThread)
            {
                stderr.writeln(k, '\t', v);
            }

            // Test whether amap can be nested.
            real[][] matrix = new real[][](1000, 1000);
            foreach (i; poolInstance.parallel( iota(0, matrix.length) ))
            {
                foreach (j; poolInstance.parallel( iota(0, matrix[0].length) ))
                {
                    matrix[i][j] = i * j;
                }
            }

            // Get around weird bugs having to do w/ sqrt being an intrinsic:
            static real mySqrt(real num)
            {
                return sqrt(num);
            }

            static real[] parallelSqrt(real[] nums)
            {
                return poolInstance.amap!mySqrt(nums);
            }

            real[][] sqrtMatrix = poolInstance.amap!parallelSqrt(matrix);

            foreach (i, row; sqrtMatrix)
            {
                foreach (j, elem; row)
                {
                    real shouldBe = sqrt( cast(real) i * j);
                    assert(approxEqual(shouldBe, elem));
                    sqrtMatrix[i][j] = shouldBe;
                }
            }

            auto saySuccess = task(
            {
                stderr.writeln(
                    "Success doing matrix stuff that involves nested pool use.");
            });
            poolInstance.put(saySuccess);
            saySuccess.workForce();

            // A more thorough test of amap, reduce:  Find the sum of the square roots of
            // matrix.

            static real parallelSum(real[] input)
            {
                return poolInstance.reduce!"a + b"(input);
            }

            auto sumSqrt = poolInstance.reduce!"a + b"(
                               poolInstance.amap!parallelSum(
                                   sqrtMatrix
                               )
                           );

            assert(approxEqual(sumSqrt, 4.437e8));
            stderr.writeln("Done sum of square roots.");

            // Test whether tasks work with function pointers.
            auto nanTask = task(&isNaN, 1.0L);
            poolInstance.put(nanTask);
            assert(nanTask.spinForce == false);

            if (poolInstance.size > 0)
            {
                // Test work waiting.
                static void uselessFun()
                {
                    foreach (i; 0 .. 1_000_000) {}
                }

                auto uselessTasks = new typeof(task(&uselessFun))[1000];
                foreach (ref uselessTask; uselessTasks)
                {
                    uselessTask = task(&uselessFun);
                }
                foreach (ref uselessTask; uselessTasks)
                {
                    poolInstance.put(uselessTask);
                }
                foreach (ref uselessTask; uselessTasks)
                {
                    uselessTask.workForce();
                }
            }

            // Test the case of non-random access + ref returns.
            int[] nums = [1,2,3,4,5];
            static struct RemoveRandom
            {
                int[] arr;

                ref int front()
                {
                    return arr.front;
                }
                void popFront()
                {
                    arr.popFront();
                }
                bool empty()
                {
                    return arr.empty;
                }
            }

            auto refRange = RemoveRandom(nums);
            foreach (ref elem; poolInstance.parallel(refRange))
            {
                elem++;
            }
            assert(nums == [2,3,4,5,6], text(nums));
            stderr.writeln("Nums:  ", nums);

            poolInstance.stop();
        }
    }
}

version (unittest)
{
    struct __S_12733
    {
        invariant() { assert(checksum == 1_234_567_890); }
        this(ulong u){n = u;}
        void opAssign(__S_12733 s){this.n = s.n;}
        ulong n;
        ulong checksum = 1_234_567_890;
    }

    static auto __genPair_12733(ulong n) { return __S_12733(n); }
}

@system unittest
{
    immutable ulong[] data = [ 2UL^^59-1, 2UL^^59-1, 2UL^^59-1, 112_272_537_195_293UL ];

    auto result = taskPool.amap!__genPair_12733(data);
}

@safe unittest
{
    import std.range : iota;

    // this test was in std.range, but caused cycles.
    assert(__traits(compiles, { foreach (i; iota(0, 100UL).parallel) {} }));
}

@safe unittest
{
    import std.algorithm.iteration : each;

    long[] arr;
    static assert(is(typeof({
        arr.parallel.each!"a++";
    })));
}

// https://issues.dlang.org/show_bug.cgi?id=17539
@system unittest
{
    import std.random : rndGen;
    // ensure compilation
    try foreach (rnd; rndGen.parallel) break;
    catch (ParallelForeachError e) {}
}

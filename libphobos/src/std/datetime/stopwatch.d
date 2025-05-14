// Written in the D programming language

/++
    Module containing some basic benchmarking and timing functionality.

    For convenience, this module publicly imports $(MREF core,time).

$(SCRIPT inhibitQuickIndex = 1;)
$(DIVC quickindex,
$(BOOKTABLE,
$(TR $(TH Category) $(TH Functions))
$(TR $(TD Main functionality) $(TD
    $(LREF StopWatch)
    $(LREF benchmark)
))
$(TR $(TD Flags) $(TD
    $(LREF AutoStart)
))
))

    $(RED Unlike the other modules in std.datetime, this module is not currently
          publicly imported in std.datetime.package, because the old
          versions of this functionality which use
          $(REF TickDuration,core,time) are in std.datetime.package and would
          conflict with the symbols in this module. After the old symbols have
          gone through the deprecation cycle and have been fully removed, then
          this module will be publicly imported in std.datetime.package. The
          old, deprecated symbols has been removed from the documentation in
          December 2019 and currently scheduled to be fully removed from Phobos
          after 2.094.)

    So, for now, when using std.datetime.stopwatch, if other modules from
    std.datetime are needed, then either import them individually rather than
    importing std.datetime, or use selective or static imports to import
    std.datetime.stopwatch. e.g.

    ----------------------------------------------------------------------------
    import std.datetime;
    import std.datetime.stopwatch : benchmark, StopWatch;
    ----------------------------------------------------------------------------

    The compiler will then know to use the symbols from std.datetime.stopwatch
    rather than the deprecated ones from std.datetime.package.

    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTP jmdavisprog.com, Jonathan M Davis) and Kato Shoichi
    Source:    $(PHOBOSSRC std/datetime/stopwatch.d)
+/
module std.datetime.stopwatch;

public import core.time;
import std.typecons : Flag;

/++
    Used by StopWatch to indicate whether it should start immediately upon
    construction.

    If set to `AutoStart.no`, then the StopWatch is not started when it is
    constructed.

    Otherwise, if set to `AutoStart.yes`, then the StopWatch is started when
    it is constructed.
  +/
alias AutoStart = Flag!"autoStart";


/++
    StopWatch is used to measure time just like one would do with a physical
    stopwatch, including stopping, restarting, and/or resetting it.

    $(REF MonoTime,core,time) is used to hold the time, and it uses the system's
    monotonic clock, which is high precision and never counts backwards (unlike
    the wall clock time, which $(I can) count backwards, which is why
    $(REF SysTime,std,datetime,systime) should not be used for timing).

    Note that the precision of StopWatch differs from system to system. It is
    impossible for it to be the same for all systems, since the precision of the
    system clock and other system-dependent and situation-dependent factors
    (such as the overhead of a context switch between threads) varies from
    system to system and can affect StopWatch's accuracy.
  +/
struct StopWatch
{
public:

    /++
        Constructs a StopWatch. Whether it starts immediately depends on the
        $(LREF AutoStart) argument.

        If `StopWatch.init` is used, then the constructed StopWatch isn't
        running (and can't be, since no constructor ran).
      +/
    this(AutoStart autostart) @safe nothrow @nogc
    {
        if (autostart)
            start();
    }

    ///
    @system nothrow @nogc unittest
    {
        import core.thread : Thread;

        {
            auto sw = StopWatch(AutoStart.yes);
            assert(sw.running);
            Thread.sleep(usecs(1));
            assert(sw.peek() > Duration.zero);
        }
        {
            auto sw = StopWatch(AutoStart.no);
            assert(!sw.running);
            Thread.sleep(usecs(1));
            assert(sw.peek() == Duration.zero);
        }
        {
            StopWatch sw;
            assert(!sw.running);
            Thread.sleep(usecs(1));
            assert(sw.peek() == Duration.zero);
        }

        assert(StopWatch.init == StopWatch(AutoStart.no));
        assert(StopWatch.init != StopWatch(AutoStart.yes));
    }


    /++
       Resets the StopWatch.

       The StopWatch can be reset while it's running, and resetting it while
       it's running will not cause it to stop.
      +/
    void reset() @safe nothrow @nogc
    {
        if (_running)
            _timeStarted = MonoTime.currTime;
        _ticksElapsed = 0;
    }

    ///
    @system nothrow @nogc unittest
    {
        import core.thread : Thread;

        auto sw = StopWatch(AutoStart.yes);
        Thread.sleep(usecs(1));
        sw.stop();
        assert(sw.peek() > Duration.zero);
        sw.reset();
        assert(sw.peek() == Duration.zero);
    }

    @system nothrow @nogc unittest
    {
        import core.thread : Thread;

        auto sw = StopWatch(AutoStart.yes);
        Thread.sleep(msecs(1));
        assert(sw.peek() > msecs(1));
        immutable before = MonoTime.currTime;

        // Just in case the system clock is slow enough or the system is fast
        // enough for the call to MonoTime.currTime inside of reset to get
        // the same that we just got by calling MonoTime.currTime.
        Thread.sleep(usecs(1));

        sw.reset();
        assert(sw._timeStarted > before);
        assert(sw._timeStarted <= MonoTime.currTime);
    }


    /++
       Starts the StopWatch.

       start should not be called if the StopWatch is already running.
      +/
    void start() @safe nothrow @nogc
    in { assert(!_running, "start was called when the StopWatch was already running."); }
    do
    {
        _running = true;
        _timeStarted = MonoTime.currTime;
    }

    ///
    @system nothrow @nogc unittest
    {
        import core.thread : Thread;

        StopWatch sw;
        assert(!sw.running);
        assert(sw.peek() == Duration.zero);
        sw.start();
        assert(sw.running);
        Thread.sleep(usecs(1));
        assert(sw.peek() > Duration.zero);
    }


    /++
       Stops the StopWatch.

       stop should not be called if the StopWatch is not running.
      +/
    void stop() @safe nothrow @nogc
    in { assert(_running, "stop was called when the StopWatch was not running."); }
    do
    {
        _running = false;
        _ticksElapsed += MonoTime.currTime.ticks - _timeStarted.ticks;
    }

    ///
    @system nothrow @nogc unittest
    {
        import core.thread : Thread;

        auto sw = StopWatch(AutoStart.yes);
        assert(sw.running);
        Thread.sleep(usecs(1));
        immutable t1 = sw.peek();
        assert(t1 > Duration.zero);

        sw.stop();
        assert(!sw.running);
        immutable t2 = sw.peek();
        assert(t2 >= t1);
        immutable t3 = sw.peek();
        assert(t2 == t3);
    }


    /++
       Peek at the amount of time that the StopWatch has been running.

       This does not include any time during which the StopWatch was stopped but
       does include $(I all) of the time that it was running and not just the
       time since it was started last.

       Calling $(LREF reset) will reset this to `Duration.zero`.
      +/
    Duration peek() @safe const nothrow @nogc
    {
        enum hnsecsPerSecond = convert!("seconds", "hnsecs")(1);
        immutable hnsecsMeasured = convClockFreq(_ticksElapsed, MonoTime.ticksPerSecond, hnsecsPerSecond);
        return _running ? MonoTime.currTime - _timeStarted + hnsecs(hnsecsMeasured)
                        : hnsecs(hnsecsMeasured);
    }

    ///
    @system nothrow @nogc unittest
    {
        import core.thread : Thread;

        auto sw = StopWatch(AutoStart.no);
        assert(sw.peek() == Duration.zero);
        sw.start();

        Thread.sleep(usecs(1));
        assert(sw.peek() >= usecs(1));

        Thread.sleep(usecs(1));
        assert(sw.peek() >= usecs(2));

        sw.stop();
        immutable stopped = sw.peek();
        Thread.sleep(usecs(1));
        assert(sw.peek() == stopped);

        sw.start();
        Thread.sleep(usecs(1));
        assert(sw.peek() > stopped);
    }

    @safe nothrow @nogc unittest
    {
        assert(StopWatch.init.peek() == Duration.zero);
    }


    /++
       Sets the total time which the StopWatch has been running (i.e. what peek
       returns).

       The StopWatch does not have to be stopped for setTimeElapsed to be
       called, nor will calling it cause the StopWatch to stop.
      +/
    void setTimeElapsed(Duration timeElapsed) @safe nothrow @nogc
    {
        enum hnsecsPerSecond = convert!("seconds", "hnsecs")(1);
        _ticksElapsed = convClockFreq(timeElapsed.total!"hnsecs", hnsecsPerSecond, MonoTime.ticksPerSecond);
        _timeStarted = MonoTime.currTime;
    }

    ///
    @system nothrow @nogc unittest
    {
        import core.thread : Thread;

        StopWatch sw;
        sw.setTimeElapsed(hours(1));

        // As discussed in MonoTime's documentation, converting between
        // Duration and ticks is not exact, though it will be close.
        // How exact it is depends on the frequency/resolution of the
        // system's monotonic clock.
        assert(abs(sw.peek() - hours(1)) < usecs(1));

        sw.start();
        Thread.sleep(usecs(1));
        assert(sw.peek() > hours(1) + usecs(1));
    }


    /++
       Returns whether this StopWatch is currently running.
      +/
    @property bool running() @safe const pure nothrow @nogc
    {
        return _running;
    }

    ///
    @safe nothrow @nogc unittest
    {
        StopWatch sw;
        assert(!sw.running);
        sw.start();
        assert(sw.running);
        sw.stop();
        assert(!sw.running);
    }


private:

    // We track the ticks for the elapsed time rather than a Duration so that we
    // don't lose any precision.

    bool _running = false; // Whether the StopWatch is currently running
    MonoTime _timeStarted; // The time the StopWatch started measuring (i.e. when it was started or reset).
    long _ticksElapsed;    // Total time that the StopWatch ran before it was stopped last.
}

/// Measure a time in milliseconds, microseconds, or nanoseconds
@safe nothrow @nogc unittest
{
    auto sw = StopWatch(AutoStart.no);
    sw.start();
    // ... Insert operations to be timed here ...
    sw.stop();

    long msecs = sw.peek.total!"msecs";
    long usecs = sw.peek.total!"usecs";
    long nsecs = sw.peek.total!"nsecs";

    assert(usecs >= msecs * 1000);
    assert(nsecs >= usecs * 1000);
}

///
@system nothrow @nogc unittest
{
    import core.thread : Thread;

    auto sw = StopWatch(AutoStart.yes);

    Duration t1 = sw.peek();
    Thread.sleep(usecs(1));
    Duration t2 = sw.peek();
    assert(t2 > t1);

    Thread.sleep(usecs(1));
    sw.stop();

    Duration t3 = sw.peek();
    assert(t3 > t2);
    Duration t4 = sw.peek();
    assert(t3 == t4);

    sw.start();
    Thread.sleep(usecs(1));

    Duration t5 = sw.peek();
    assert(t5 > t4);

    // If stopping or resetting the StopWatch is not required, then
    // MonoTime can easily be used by itself without StopWatch.
    auto before = MonoTime.currTime;
    // do stuff...
    auto timeElapsed = MonoTime.currTime - before;
}


/++
    Benchmarks code for speed assessment and comparison.

    Params:
        fun = aliases of callable objects (e.g. function names). Each callable
              object should take no arguments.
        n   = The number of times each function is to be executed.

    Returns:
        The amount of time (as a $(REF Duration,core,time)) that it took to call
        each function `n` times. The first value is the length of time that
        it took to call `fun[0]` `n` times. The second value is the length
        of time it took to call `fun[1]` `n` times. Etc.
  +/
Duration[fun.length] benchmark(fun...)(uint n)
{
    Duration[fun.length] result;
    auto sw = StopWatch(AutoStart.yes);

    foreach (i, unused; fun)
    {
        sw.reset();
        foreach (_; 0 .. n)
            fun[i]();
        result[i] = sw.peek();
    }

    return result;
}

///
@safe unittest
{
    import std.conv : to;

    int a;
    void f0() {}
    void f1() { auto b = a; }
    void f2() { auto b = to!string(a); }
    auto r = benchmark!(f0, f1, f2)(10_000);
    Duration f0Result = r[0]; // time f0 took to run 10,000 times
    Duration f1Result = r[1]; // time f1 took to run 10,000 times
    Duration f2Result = r[2]; // time f2 took to run 10,000 times
}

@safe nothrow unittest
{
    import std.conv : to;

    int a;
    void f0() nothrow {}
    void f1() nothrow @trusted {
        // do not allow any optimizer to optimize this function away
        import core.thread : getpid;
        import core.stdc.stdio : printf;
        auto b = getpid.to!string;
        if (getpid == 1) // never happens, but prevents optimization
            printf("%p", &b);
    }

    auto sw = StopWatch(AutoStart.yes);
    auto r = benchmark!(f0, f1)(1000);
    auto total = sw.peek();
    assert(r[0] >= Duration.zero);
    assert(r[1] >= Duration.zero);
    assert(r[0] <= total);
    assert(r[1] <= total);
}

@safe nothrow @nogc unittest
{
    int f0Count;
    int f1Count;
    int f2Count;
    void f0() nothrow @nogc { ++f0Count; }
    void f1() nothrow @nogc { ++f1Count; }
    void f2() nothrow @nogc { ++f2Count; }
    auto r = benchmark!(f0, f1, f2)(552);
    assert(f0Count == 552);
    assert(f1Count == 552);
    assert(f2Count == 552);
}

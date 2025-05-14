@system nothrow @nogc unittest
{
    import std.datetime.stopwatch;

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

@system nothrow @nogc unittest
{
    import std.datetime.stopwatch;

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
    import std.datetime.stopwatch;

        import core.thread : Thread;

        StopWatch sw;
        assert(!sw.running);
        assert(sw.peek() == Duration.zero);
        sw.start();
        assert(sw.running);
        Thread.sleep(usecs(1));
        assert(sw.peek() > Duration.zero);
    
}

@system nothrow @nogc unittest
{
    import std.datetime.stopwatch;

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

@system nothrow @nogc unittest
{
    import std.datetime.stopwatch;

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

@system nothrow @nogc unittest
{
    import std.datetime.stopwatch;

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

@safe nothrow @nogc unittest
{
    import std.datetime.stopwatch;

        StopWatch sw;
        assert(!sw.running);
        sw.start();
        assert(sw.running);
        sw.stop();
        assert(!sw.running);
    
}

@safe nothrow @nogc unittest
{
    import std.datetime.stopwatch;

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

@system nothrow @nogc unittest
{
    import std.datetime.stopwatch;

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

@safe unittest
{
    import std.datetime.stopwatch;

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


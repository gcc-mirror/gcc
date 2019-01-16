// Written in the D programming language

/++
    Module containing Date/Time functionality.

    This module provides:
    $(UL
        $(LI Types to represent points in time:
             $(REF SysTime,std,_datetime,systime),
             $(REF Date,std,_datetime,date),
             $(REF TimeOfDay,std,_datetime,date),
             $(REF DateTime,std,_datetime,date).)
        $(LI Types to represent intervals of time.)
        $(LI Types to represent ranges over intervals of time.)
        $(LI Types to represent time zones (used by
             $(REF SysTime,std,_datetime,systime)).)
        $(LI A platform-independent, high precision stopwatch type:
             $(LREF StopWatch))
        $(LI Benchmarking functions.)
        $(LI Various helper functions.)
    )

    Closely related to std.datetime is <a href="core_time.html">$(D core.time)</a>,
    and some of the time types used in std.datetime come from there - such as
    $(REF Duration, core,time), $(REF TickDuration, core,time), and
    $(REF FracSec, core,time).
    core.time is publically imported into std.datetime, it isn't necessary
    to import it separately.

    Three of the main concepts used in this module are time points, time
    durations, and time intervals.

    A time point is a specific point in time. e.g. January 5th, 2010
    or 5:00.

    A time duration is a length of time with units. e.g. 5 days or 231 seconds.

    A time interval indicates a period of time associated with a fixed point in
    time. It is either two time points associated with each other,
    indicating the time starting at the first point up to, but not including,
    the second point - e.g. [January 5th, 2010 - March 10th, 2010$(RPAREN) - or
    it is a time point and a time duration associated with one another. e.g.
    January 5th, 2010 and 5 days, indicating [January 5th, 2010 -
    January 10th, 2010$(RPAREN).

    Various arithmetic operations are supported between time points and
    durations (e.g. the difference between two time points is a time duration),
    and ranges can be gotten from time intervals, so range-based operations may
    be done on a series of time points.

    The types that the typical user is most likely to be interested in are
    $(REF Date,std,_datetime,date) (if they want dates but don't care about
    time), $(REF DateTime,std,_datetime,date) (if they want dates and times
    but don't care about time zones), $(REF SysTime,std,_datetime,systime) (if
    they want the date and time from the OS and/or do care about time zones),
    and StopWatch (a platform-independent, high precision stop watch).
    $(REF Date,std,_datetime,date) and $(REF DateTime,std,_datetime,date) are
    optimized for calendar-based operations, while
    $(REF SysTime,std,_datetime,systime) is designed for dealing with time from
    the OS. Check out their specific documentation for more details.

    To get the current time, use $(REF Clock.currTime,std,_datetime,systime).
    It will return the current time as a $(REF SysTime,std,_datetime,systime). To
    print it, $(D toString) is sufficient, but if using $(D toISOString),
    $(D toISOExtString), or $(D toSimpleString), use the corresponding
    $(D fromISOString), $(D fromISOExtString), or $(D fromSimpleString) to
    create a $(REF SysTime,std,_datetime,systime) from the string.

--------------------
auto currentTime = Clock.currTime();
auto timeString = currentTime.toISOExtString();
auto restoredTime = SysTime.fromISOExtString(timeString);
--------------------

    Various functions take a string (or strings) to represent a unit of time
    (e.g. $(D convert!("days", "hours")(numDays))). The valid strings to use
    with such functions are $(D "years"), $(D "months"), $(D "weeks"),
    $(D "days"), $(D "hours"), $(D "minutes"), $(D "seconds"),
    $(D "msecs") (milliseconds), $(D "usecs") (microseconds),
    $(D "hnsecs") (hecto-nanoseconds - i.e. 100 ns), or some subset thereof.
    There are a few functions in core.time which take $(D "nsecs"), but because
    nothing in std.datetime has precision greater than hnsecs, and very little
    in core.time does, no functions in std.datetime accept $(D "nsecs").
    To remember which units are abbreviated and which aren't,
    all units seconds and greater use their full names, and all
    sub-second units are abbreviated (since they'd be rather long if they
    weren't).

    Note:
        $(REF DateTimeException,std,_datetime,date) is an alias for
        $(REF TimeException, core,time), so you don't need to worry about
        core.time functions and std.datetime functions throwing different
        exception types (except in the rare case that they throw something other
        than $(REF TimeException, core,time) or
        $(REF DateTimeException,std,_datetime,date)).

    See_Also:
        $(DDLINK intro-to-_datetime, Introduction to std.datetime,
                 Introduction to std&#46;_datetime)<br>
        $(HTTP en.wikipedia.org/wiki/ISO_8601, ISO 8601)<br>
        $(HTTP en.wikipedia.org/wiki/Tz_database,
              Wikipedia entry on TZ Database)<br>
        $(HTTP en.wikipedia.org/wiki/List_of_tz_database_time_zones,
              List of Time Zones)<br>

    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   Jonathan M Davis and Kato Shoichi
    Source:    $(PHOBOSSRC std/_datetime/package.d)
+/
module std.datetime;

public import core.time;
public import std.datetime.date;
public import std.datetime.interval;
public import std.datetime.systime;
public import std.datetime.timezone;

import core.exception : AssertError;
import std.functional : unaryFun;
import std.traits;
import std.typecons : Flag, Yes, No;


// Verify module example.
@safe unittest
{
    auto currentTime = Clock.currTime();
    auto timeString = currentTime.toISOExtString();
    auto restoredTime = SysTime.fromISOExtString(timeString);
}

// Verify Examples for core.time.Duration which couldn't be in core.time.
@safe unittest
{
    assert(std.datetime.Date(2010, 9, 7) + dur!"days"(5) ==
           std.datetime.Date(2010, 9, 12));

    assert(std.datetime.Date(2010, 9, 7) - std.datetime.Date(2010, 10, 3) ==
           dur!"days"(-26));
}

@safe unittest
{
    import std.traits : hasUnsharedAliasing;
    /* Issue 6642 */
    static assert(!hasUnsharedAliasing!Date);
    static assert(!hasUnsharedAliasing!TimeOfDay);
    static assert(!hasUnsharedAliasing!DateTime);
    static assert(!hasUnsharedAliasing!SysTime);
}


//==============================================================================
// Everything after here will be deprecated after we have replacements which
// use MonoTime and Duration.
//==============================================================================


/++
   Used by StopWatch to indicate whether it should start immediately upon
   construction.

   If set to $(D AutoStart.no), then the stopwatch is not started when it is
   constructed.

   Otherwise, if set to $(D AutoStart.yes), then the stopwatch is started when
   it is constructed.
  +/
alias AutoStart = Flag!"autoStart";


/++
    $(RED This will be deprecated in 2.076. Please use
          $(REF StopWatch,std,datetime,stopwatch) instead. It uses
          $(REF Monotime,core,time) and $(REF Duration,core,time) rather
          than $(REF TickDuration,core,time), which will also be deprecated in
          2.076.)

   $(D StopWatch) measures time as precisely as possible.

   This class uses a high-performance counter. On Windows systems, it uses
   $(D QueryPerformanceCounter), and on Posix systems, it uses
   $(D clock_gettime) if available, and $(D gettimeofday) otherwise.

   But the precision of $(D StopWatch) differs from system to system. It is
   impossible to for it to be the same from system to system since the precision
   of the system clock varies from system to system, and other system-dependent
   and situation-dependent stuff (such as the overhead of a context switch
   between threads) can also affect $(D StopWatch)'s accuracy.
  +/
@safe struct StopWatch
{
public:

    /++
       Auto start with constructor.
      +/
    this(AutoStart autostart) @nogc
    {
        if (autostart)
            start();
    }

    @nogc @safe unittest
    {
        auto sw = StopWatch(Yes.autoStart);
        sw.stop();
    }


    ///
    bool opEquals(const StopWatch rhs) const pure nothrow @nogc
    {
        return opEquals(rhs);
    }

    /// ditto
    bool opEquals(const ref StopWatch rhs) const pure nothrow @nogc
    {
        return _timeStart == rhs._timeStart &&
               _timeMeasured == rhs._timeMeasured;
    }


    /++
       Resets the stop watch.
      +/
    void reset() @nogc
    {
        if (_flagStarted)
        {
            // Set current system time if StopWatch is measuring.
            _timeStart = TickDuration.currSystemTick;
        }
        else
        {
            // Set zero if StopWatch is not measuring.
            _timeStart.length = 0;
        }

        _timeMeasured.length = 0;
    }

    ///
    @nogc @safe unittest
    {
        StopWatch sw;
        sw.start();
        sw.stop();
        sw.reset();
        assert(sw.peek().to!("seconds", real)() == 0);
    }


    /++
       Starts the stop watch.
      +/
    void start() @nogc
    {
        assert(!_flagStarted);
        _flagStarted = true;
        _timeStart = TickDuration.currSystemTick;
    }

    @nogc @system unittest
    {
        StopWatch sw;
        sw.start();
        auto t1 = sw.peek();
        bool doublestart = true;
        try
            sw.start();
        catch (AssertError e)
            doublestart = false;
        assert(!doublestart);
        sw.stop();
        assert((t1 - sw.peek()).to!("seconds", real)() <= 0);
    }


    /++
       Stops the stop watch.
      +/
    void stop() @nogc
    {
        assert(_flagStarted);
        _flagStarted = false;
        _timeMeasured += TickDuration.currSystemTick - _timeStart;
    }

    @nogc @system unittest
    {
        StopWatch sw;
        sw.start();
        sw.stop();
        auto t1 = sw.peek();
        bool doublestop = true;
        try
            sw.stop();
        catch (AssertError e)
            doublestop = false;
        assert(!doublestop);
        assert((t1 - sw.peek()).to!("seconds", real)() == 0);
    }


    /++
       Peek at the amount of time which has passed since the stop watch was
       started.
      +/
    TickDuration peek() const @nogc
    {
        if (_flagStarted)
            return TickDuration.currSystemTick - _timeStart + _timeMeasured;

        return _timeMeasured;
    }

    @nogc @safe unittest
    {
        StopWatch sw;
        sw.start();
        auto t1 = sw.peek();
        sw.stop();
        auto t2 = sw.peek();
        auto t3 = sw.peek();
        assert(t1 <= t2);
        assert(t2 == t3);
    }


    /++
       Set the amount of time which has been measured since the stop watch was
       started.
      +/
    void setMeasured(TickDuration d) @nogc
    {
        reset();
        _timeMeasured = d;
    }

    @nogc @safe unittest
    {
        StopWatch sw;
        TickDuration t0;
        t0.length = 100;
        sw.setMeasured(t0);
        auto t1 = sw.peek();
        assert(t0 == t1);
    }


    /++
       Confirm whether this stopwatch is measuring time.
      +/
    bool running() @property const pure nothrow @nogc
    {
        return _flagStarted;
    }

    @nogc @safe unittest
    {
        StopWatch sw1;
        assert(!sw1.running);
        sw1.start();
        assert(sw1.running);
        sw1.stop();
        assert(!sw1.running);
        StopWatch sw2 = Yes.autoStart;
        assert(sw2.running);
        sw2.stop();
        assert(!sw2.running);
        sw2.start();
        assert(sw2.running);
    }




private:

    // true if observing.
    bool _flagStarted = false;

    // TickDuration at the time of StopWatch starting measurement.
    TickDuration _timeStart;

    // Total time that StopWatch ran.
    TickDuration _timeMeasured;
}

///
@safe unittest
{
    void writeln(S...)(S args){}
    static void bar() {}

    StopWatch sw;
    enum n = 100;
    TickDuration[n] times;
    TickDuration last = TickDuration.from!"seconds"(0);
    foreach (i; 0 .. n)
    {
       sw.start(); //start/resume mesuring.
       foreach (unused; 0 .. 1_000_000)
           bar();
       sw.stop();  //stop/pause measuring.
       //Return value of peek() after having stopped are the always same.
       writeln((i + 1) * 1_000_000, " times done, lap time: ",
               sw.peek().msecs, "[ms]");
       times[i] = sw.peek() - last;
       last = sw.peek();
    }
    real sum = 0;
    // To get the number of seconds,
    // use properties of TickDuration.
    // (seconds, msecs, usecs, hnsecs)
    foreach (t; times)
       sum += t.hnsecs;
    writeln("Average time: ", sum/n, " hnsecs");
}


/++
    $(RED This will be deprecated in 2.076. Please use
          $(REF benchmark,std,datetime,stopwatch) instead. It uses
          $(REF Monotime,core,time) and $(REF Duration,core,time) rather
          than $(REF TickDuration,core,time), which will also be deprecated in
          2.076.)

    Benchmarks code for speed assessment and comparison.

    Params:
        fun = aliases of callable objects (e.g. function names). Each should
              take no arguments.
        n   = The number of times each function is to be executed.

    Returns:
        The amount of time (as a $(REF TickDuration, core,time)) that it took to
        call each function $(D n) times. The first value is the length of time
        that it took to call $(D fun[0]) $(D n) times. The second value is the
        length of time it took to call $(D fun[1]) $(D n) times. Etc.

    Note that casting the TickDurations to $(REF Duration, core,time)s will make
    the results easier to deal with (and it may change in the future that
    benchmark will return an array of Durations rather than TickDurations).

    See_Also:
        $(LREF measureTime)
  +/
TickDuration[fun.length] benchmark(fun...)(uint n)
{
    TickDuration[fun.length] result;
    StopWatch sw;
    sw.start();

    foreach (i, unused; fun)
    {
        sw.reset();
        foreach (j; 0 .. n)
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
    void f1() {auto b = a;}
    void f2() {auto b = to!string(a);}
    auto r = benchmark!(f0, f1, f2)(10_000);
    auto f0Result = to!Duration(r[0]); // time f0 took to run 10,000 times
    auto f1Result = to!Duration(r[1]); // time f1 took to run 10,000 times
    auto f2Result = to!Duration(r[2]); // time f2 took to run 10,000 times
}

@safe unittest
{
    int a;
    void f0() {}
    //void f1() {auto b = to!(string)(a);}
    void f2() {auto b = (a);}
    auto r = benchmark!(f0, f2)(100);
}


/++
   Return value of benchmark with two functions comparing.
  +/
@safe struct ComparingBenchmarkResult
{
    /++
       Evaluation value

       This returns the evaluation value of performance as the ratio of
       baseFunc's time over targetFunc's time. If performance is high, this
       returns a high value.
      +/
    @property real point() const pure nothrow
    {
        return _baseTime.length / cast(const real)_targetTime.length;
    }


    /++
       The time required of the base function
      +/
    @property public TickDuration baseTime() const pure nothrow
    {
        return _baseTime;
    }


    /++
       The time required of the target function
      +/
    @property public TickDuration targetTime() const pure nothrow
    {
        return _targetTime;
    }

private:

    this(TickDuration baseTime, TickDuration targetTime) pure nothrow
    {
        _baseTime = baseTime;
        _targetTime = targetTime;
    }

    TickDuration _baseTime;
    TickDuration _targetTime;
}


/++
    $(RED This will be deprecated in 2.076. Please use
          $(REF benchmark,std,datetime,stopwatch) instead. This function has
          not been ported to $(REF Monotime,core,time) and
          $(REF Duration,core,time), because it is a trivial wrapper around
          benchmark.)

   Benchmark with two functions comparing.

   Params:
       baseFunc   = The function to become the base of the speed.
       targetFunc = The function that wants to measure speed.
       times      = The number of times each function is to be executed.
  +/
ComparingBenchmarkResult comparingBenchmark(alias baseFunc,
                                            alias targetFunc,
                                            int times = 0xfff)()
{
    auto t = benchmark!(baseFunc, targetFunc)(times);
    return ComparingBenchmarkResult(t[0], t[1]);
}

///
@safe unittest
{
    void f1x() {}
    void f2x() {}
    @safe void f1o() {}
    @safe void f2o() {}
    auto b1 = comparingBenchmark!(f1o, f2o, 1)(); // OK
    //writeln(b1.point);
}

//Bug# 8450
@system unittest
{
    @safe    void safeFunc() {}
    @trusted void trustFunc() {}
    @system  void sysFunc() {}
    auto safeResult  = comparingBenchmark!((){safeFunc();}, (){safeFunc();})();
    auto trustResult = comparingBenchmark!((){trustFunc();}, (){trustFunc();})();
    auto sysResult   = comparingBenchmark!((){sysFunc();}, (){sysFunc();})();
    auto mixedResult1  = comparingBenchmark!((){safeFunc();}, (){trustFunc();})();
    auto mixedResult2  = comparingBenchmark!((){trustFunc();}, (){sysFunc();})();
    auto mixedResult3  = comparingBenchmark!((){safeFunc();}, (){sysFunc();})();
}


/++
    $(RED This will be deprecated in 2.076. Please use
          $(REF StopWatch,std,datetime,stopwatch) instead. This function has
          not been ported to $(REF Monotime,core,time) and
          $(REF Duration,core,time), because it is a trivial wrapper around
          StopWatch.)

    Function for starting to a stop watch time when the function is called
    and stopping it when its return value goes out of scope and is destroyed.

    When the value that is returned by this function is destroyed,
    $(D func) will run. $(D func) is a unary function that takes a
    $(REF TickDuration, core,time).

    Example:
--------------------
{
    auto mt = measureTime!((TickDuration a)
        { /+ do something when the scope is exited +/ });
    // do something that needs to be timed
}
--------------------

    which is functionally equivalent to

--------------------
{
    auto sw = StopWatch(Yes.autoStart);
    scope(exit)
    {
        TickDuration a = sw.peek();
        /+ do something when the scope is exited +/
    }
    // do something that needs to be timed
}
--------------------

    See_Also:
        $(LREF benchmark)
+/
@safe auto measureTime(alias func)()
if (isSafe!((){StopWatch sw; unaryFun!func(sw.peek());}))
{
    struct Result
    {
        private StopWatch _sw = void;
        this(AutoStart as)
        {
            _sw = StopWatch(as);
        }
        ~this()
        {
            unaryFun!(func)(_sw.peek());
        }
    }
    return Result(Yes.autoStart);
}

auto measureTime(alias func)()
if (!isSafe!((){StopWatch sw; unaryFun!func(sw.peek());}))
{
    struct Result
    {
        private StopWatch _sw = void;
        this(AutoStart as)
        {
            _sw = StopWatch(as);
        }
        ~this()
        {
            unaryFun!(func)(_sw.peek());
        }
    }
    return Result(Yes.autoStart);
}

// Verify Example.
@safe unittest
{
    {
        auto mt = measureTime!((TickDuration a)
            { /+ do something when the scope is exited +/ });
        // do something that needs to be timed
    }

    {
        auto sw = StopWatch(Yes.autoStart);
        scope(exit)
        {
            TickDuration a = sw.peek();
            /+ do something when the scope is exited +/
        }
        // do something that needs to be timed
    }
}

@safe unittest
{
    import std.math : isNaN;

    @safe static void func(TickDuration td)
    {
        assert(!td.to!("seconds", real)().isNaN());
    }

    auto mt = measureTime!(func)();

    /+
    with (measureTime!((a){assert(a.seconds);}))
    {
        // doSomething();
        // @@@BUG@@@ doesn't work yet.
    }
    +/
}

@safe unittest
{
    import std.math : isNaN;

    static void func(TickDuration td)
    {
        assert(!td.to!("seconds", real)().isNaN());
    }

    auto mt = measureTime!(func)();

    /+
    with (measureTime!((a){assert(a.seconds);}))
    {
        // doSomething();
        // @@@BUG@@@ doesn't work yet.
    }
    +/
}

//Bug# 8450
@system unittest
{
    @safe    void safeFunc() {}
    @trusted void trustFunc() {}
    @system  void sysFunc() {}
    auto safeResult  = measureTime!((a){safeFunc();})();
    auto trustResult = measureTime!((a){trustFunc();})();
    auto sysResult   = measureTime!((a){sysFunc();})();
}

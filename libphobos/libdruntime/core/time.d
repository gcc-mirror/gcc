//Written in the D programming language

/++
    Module containing core time functionality, such as $(LREF Duration) (which
    represents a duration of time) or $(LREF MonoTime) (which represents a
    timestamp of the system's monotonic clock).

    Various functions take a string (or strings) to represent a unit of time
    (e.g. $(D convert!("days", "hours")(numDays))). The valid strings to use
    with such functions are "years", "months", "weeks", "days", "hours",
    "minutes", "seconds", "msecs" (milliseconds), "usecs" (microseconds),
    "hnsecs" (hecto-nanoseconds - i.e. 100 ns) or some subset thereof. There
    are a few functions that also allow "nsecs", but very little actually
    has precision greater than hnsecs.

    $(BOOKTABLE Cheat Sheet,
    $(TR $(TH Symbol) $(TH Description))
    $(LEADINGROW Types)
    $(TR $(TDNW $(LREF Duration)) $(TD Represents a duration of time of weeks
    or less (kept internally as hnsecs). (e.g. 22 days or 700 seconds).))
    $(TR $(TDNW $(LREF TickDuration)) $(TD $(RED DEPRECATED) Represents a duration of time in
    system clock ticks, using the highest precision that the system provides.))
    $(TR $(TDNW $(LREF MonoTime)) $(TD Represents a monotonic timestamp in
    system clock ticks, using the highest precision that the system provides.))
    $(LEADINGROW Functions)
    $(TR $(TDNW $(LREF convert)) $(TD Generic way of converting between two time
    units.))
    $(TR $(TDNW $(LREF dur)) $(TD Allows constructing a $(LREF Duration) from
    the given time units with the given length.))
    $(TR $(TDNW $(LREF weeks)$(NBSP)$(LREF days)$(NBSP)$(LREF hours)$(BR)
    $(LREF minutes)$(NBSP)$(LREF seconds)$(NBSP)$(LREF msecs)$(BR)
    $(LREF usecs)$(NBSP)$(LREF hnsecs)$(NBSP)$(LREF nsecs))
    $(TD Convenience aliases for $(LREF dur).))
    $(TR $(TDNW $(LREF abs)) $(TD Returns the absolute value of a duration.))
    )

    $(BOOKTABLE Conversions,
    $(TR $(TH )
     $(TH From $(LREF Duration))
     $(TH From $(LREF TickDuration))
     $(TH From units)
    )
    $(TR $(TD $(B To $(LREF Duration)))
     $(TD -)
     $(TD $(D tickDuration.)$(REF_SHORT to, std,conv)$(D !Duration()))
     $(TD $(D dur!"msecs"(5)) or $(D 5.msecs()))
    )
    $(TR $(TD $(B To $(LREF TickDuration)))
     $(TD $(D duration.)$(REF_SHORT to, std,conv)$(D !TickDuration()))
     $(TD -)
     $(TD $(D TickDuration.from!"msecs"(msecs)))
    )
    $(TR $(TD $(B To units))
     $(TD $(D duration.total!"days"))
     $(TD $(D tickDuration.msecs))
     $(TD $(D convert!("days", "msecs")(msecs)))
    ))

    Copyright: Copyright 2010 - 2012
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTP jmdavisprog.com, Jonathan M Davis) and Kato Shoichi
    Source:    $(DRUNTIMESRC core/_time.d)
    Macros:
    NBSP=&nbsp;
 +/
module core.time;

import core.exception;
import core.internal.string;
import core.stdc.time : time;

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
    import core.sys.windows.winbase /+: QueryPerformanceCounter, QueryPerformanceFrequency+/;
}
else version (Darwin)
{
    import core.sys.posix.sys.time : gettimeofday, timeval;
    import core.sys.posix.time : timespec;
}
else version (Posix)
{
    import core.sys.posix.sys.time : gettimeofday, timeval;
    import core.sys.posix.time : clock_getres, clock_gettime, CLOCK_MONOTONIC, timespec;
}

version (unittest) import core.stdc.stdio : printf;


//This probably should be moved somewhere else in druntime which
//is Darwin-specific.
version (Darwin)
{

import core.sys.darwin.mach.kern_return : kern_return_t;

extern(C) nothrow @nogc
{

struct mach_timebase_info_data_t
{
    uint numer;
    uint denom;
}

alias mach_timebase_info_data_t* mach_timebase_info_t;

kern_return_t mach_timebase_info(mach_timebase_info_t);

ulong mach_absolute_time();

}

}

/++
    What type of clock to use with $(LREF MonoTime) / $(LREF MonoTimeImpl) or
    $(D std.datetime.Clock.currTime). They default to $(D ClockType.normal),
    and most programs do not need to ever deal with the others.

    The other $(D ClockType)s are provided so that other clocks provided by the
    underlying C, system calls can be used with $(LREF MonoTimeImpl) or
    $(D std.datetime.Clock.currTime) without having to use the C API directly.

    In the case of the monotonic time, $(LREF MonoTimeImpl) is templatized on
    $(D ClockType), whereas with $(D std.datetime.Clock.currTime), its a runtime
    argument, since in the case of the monotonic time, the type of the clock
    affects the resolution of a $(LREF MonoTimeImpl) object, whereas with
    $(REF SysTime, std,datetime), its resolution is always hecto-nanoseconds
    regardless of the source of the time.

    $(D ClockType.normal), $(D ClockType.coarse), and $(D ClockType.precise)
    work with both $(D Clock.currTime) and $(LREF MonoTimeImpl).
    $(D ClockType.second) only works with $(D Clock.currTime). The others only
    work with $(LREF MonoTimeImpl).
  +/
version (CoreDdoc) enum ClockType
{
    /++
        Use the normal clock.
      +/
    normal = 0,

    /++
        $(BLUE Linux,OpenBSD-Only)

        Uses $(D CLOCK_BOOTTIME).
      +/
    bootTime = 1,

    /++
        Use the coarse clock, not the normal one (e.g. on Linux, that would be
        $(D CLOCK_REALTIME_COARSE) instead of $(D CLOCK_REALTIME) for
        $(D clock_gettime) if a function is using the realtime clock). It's
        generally faster to get the time with the coarse clock than the normal
        clock, but it's less precise (e.g. 1 msec instead of 1 usec or 1 nsec).
        Howeover, it $(I is) guaranteed to still have sub-second precision
        (just not as high as with $(D ClockType.normal)).

        On systems which do not support a coarser clock,
        $(D MonoTimeImpl!(ClockType.coarse)) will internally use the same clock
        as $(D MonoTime) does, and $(D Clock.currTime!(ClockType.coarse)) will
        use the same clock as $(D Clock.currTime). This is because the coarse
        clock is doing the same thing as the normal clock (just at lower
        precision), whereas some of the other clock types
        (e.g. $(D ClockType.processCPUTime)) mean something fundamentally
        different. So, treating those as $(D ClockType.normal) on systems where
        they weren't natively supported would give misleading results.

        Most programs should not use the coarse clock, exactly because it's
        less precise, and most programs don't need to get the time often
        enough to care, but for those rare programs that need to get the time
        extremely frequently (e.g. hundreds of thousands of times a second) but
        don't care about high precision, the coarse clock might be appropriate.

        Currently, only Linux and FreeBSD/DragonFlyBSD support a coarser clock, and on other
        platforms, it's treated as $(D ClockType.normal).
      +/
    coarse = 2,

    /++
        Uses a more precise clock than the normal one (which is already very
        precise), but it takes longer to get the time. Similarly to
        $(D ClockType.coarse), if it's used on a system that does not support a
        more precise clock than the normal one, it's treated as equivalent to
        $(D ClockType.normal).

        Currently, only FreeBSD/DragonFlyBSD supports a more precise clock, where it uses
        $(D CLOCK_MONOTONIC_PRECISE) for the monotonic time and
        $(D CLOCK_REALTIME_PRECISE) for the wall clock time.
      +/
    precise = 3,

    /++
        $(BLUE Linux,OpenBSD,Solaris-Only)

        Uses $(D CLOCK_PROCESS_CPUTIME_ID).
      +/
    processCPUTime = 4,

    /++
        $(BLUE Linux-Only)

        Uses $(D CLOCK_MONOTONIC_RAW).
      +/
    raw = 5,

    /++
        Uses a clock that has a precision of one second (contrast to the coarse
        clock, which has sub-second precision like the normal clock does).

        FreeBSD/DragonFlyBSD are the only systems which specifically have a clock set up for
        this (it has $(D CLOCK_SECOND) to use with $(D clock_gettime) which
        takes advantage of an in-kernel cached value), but on other systems, the
        fastest function available will be used, and the resulting $(D SysTime)
        will be rounded down to the second if the clock that was used gave the
        time at a more precise resolution. So, it's guaranteed that the time
        will be given at a precision of one second and it's likely the case that
        will be faster than $(D ClockType.normal), since there tend to be
        several options on a system to get the time at low resolutions, and they
        tend to be faster than getting the time at high resolutions.

        So, the primary difference between $(D ClockType.coarse) and
        $(D ClockType.second) is that $(D ClockType.coarse) sacrifices some
        precision in order to get speed but is still fairly precise, whereas
        $(D ClockType.second) tries to be as fast as possible at the expense of
        all sub-second precision.
      +/
    second = 6,

    /++
        $(BLUE Linux,OpenBSD,Solaris-Only)

        Uses $(D CLOCK_THREAD_CPUTIME_ID).
      +/
    threadCPUTime = 7,

    /++
        $(BLUE DragonFlyBSD,FreeBSD,OpenBSD-Only)

        Uses $(D CLOCK_UPTIME).
      +/
    uptime = 8,

    /++
        $(BLUE FreeBSD-Only)

        Uses $(D CLOCK_UPTIME_FAST).
      +/
    uptimeCoarse = 9,

    /++
        $(BLUE FreeBSD-Only)

        Uses $(D CLOCK_UPTIME_PRECISE).
      +/
    uptimePrecise = 10,
}
else version (Windows) enum ClockType
{
    normal = 0,
    coarse = 2,
    precise = 3,
    second = 6,
}
else version (Darwin) enum ClockType
{
    normal = 0,
    coarse = 2,
    precise = 3,
    second = 6,
}
else version (linux) enum ClockType
{
    normal = 0,
    bootTime = 1,
    coarse = 2,
    precise = 3,
    processCPUTime = 4,
    raw = 5,
    second = 6,
    threadCPUTime = 7,
}
else version (FreeBSD) enum ClockType
{
    normal = 0,
    coarse = 2,
    precise = 3,
    second = 6,
    uptime = 8,
    uptimeCoarse = 9,
    uptimePrecise = 10,
}
else version (NetBSD) enum ClockType
{
    normal = 0,
    coarse = 2,
    precise = 3,
    second = 6,
}
else version (OpenBSD) enum ClockType
{
    normal = 0,
    bootTime = 1,
    coarse = 2,
    precise = 3,
    processCPUTime = 4,
    second = 6,
    threadCPUTime = 7,
    uptime = 8,
}
else version (DragonFlyBSD) enum ClockType
{
    normal = 0,
    coarse = 2,
    precise = 3,
    second = 6,
    uptime = 8,
    uptimeCoarse = 9,
    uptimePrecise = 10,
}
else version (Solaris) enum ClockType
{
    normal = 0,
    coarse = 2,
    precise = 3,
    processCPUTime = 4,
    second = 6,
    threadCPUTime = 7,
}
else
{
    // It needs to be decided (and implemented in an appropriate version branch
    // here) which clock types new platforms are going to support. At minimum,
    // the ones _not_ marked with $(D Blue Foo-Only) should be supported.
    static assert(0, "What are the clock types supported by this system?");
}

// private, used to translate clock type to proper argument to clock_xxx
// functions on posix systems
version (CoreDdoc)
    private int _posixClock(ClockType clockType) { return 0; }
else
version (Posix)
{
    private auto _posixClock(ClockType clockType)
    {
        version (linux)
        {
            import core.sys.linux.time;
            with(ClockType) final switch (clockType)
            {
            case bootTime: return CLOCK_BOOTTIME;
            case coarse: return CLOCK_MONOTONIC_COARSE;
            case normal: return CLOCK_MONOTONIC;
            case precise: return CLOCK_MONOTONIC;
            case processCPUTime: return CLOCK_PROCESS_CPUTIME_ID;
            case raw: return CLOCK_MONOTONIC_RAW;
            case threadCPUTime: return CLOCK_THREAD_CPUTIME_ID;
            case second: assert(0);
            }
        }
        else version (FreeBSD)
        {
            import core.sys.freebsd.time;
            with(ClockType) final switch (clockType)
            {
            case coarse: return CLOCK_MONOTONIC_FAST;
            case normal: return CLOCK_MONOTONIC;
            case precise: return CLOCK_MONOTONIC_PRECISE;
            case uptime: return CLOCK_UPTIME;
            case uptimeCoarse: return CLOCK_UPTIME_FAST;
            case uptimePrecise: return CLOCK_UPTIME_PRECISE;
            case second: assert(0);
            }
        }
        else version (NetBSD)
        {
            import core.sys.netbsd.time;
            with(ClockType) final switch (clockType)
            {
            case coarse: return CLOCK_MONOTONIC;
            case normal: return CLOCK_MONOTONIC;
            case precise: return CLOCK_MONOTONIC;
            case second: assert(0);
            }
        }
        else version (OpenBSD)
        {
            import core.sys.openbsd.time;
            with(ClockType) final switch (clockType)
            {
            case bootTime: return CLOCK_BOOTTIME;
            case coarse: return CLOCK_MONOTONIC;
            case normal: return CLOCK_MONOTONIC;
            case precise: return CLOCK_MONOTONIC;
            case processCPUTime: return CLOCK_PROCESS_CPUTIME_ID;
            case threadCPUTime: return CLOCK_THREAD_CPUTIME_ID;
            case uptime: return CLOCK_UPTIME;
            case second: assert(0);
            }
        }
        else version (DragonFlyBSD)
        {
            import core.sys.dragonflybsd.time;
            with(ClockType) final switch (clockType)
            {
            case coarse: return CLOCK_MONOTONIC_FAST;
            case normal: return CLOCK_MONOTONIC;
            case precise: return CLOCK_MONOTONIC_PRECISE;
            case uptime: return CLOCK_UPTIME;
            case uptimeCoarse: return CLOCK_UPTIME_FAST;
            case uptimePrecise: return CLOCK_UPTIME_PRECISE;
            case second: assert(0);
            }
        }
        else version (Solaris)
        {
            import core.sys.solaris.time;
            with(ClockType) final switch (clockType)
            {
            case coarse: return CLOCK_MONOTONIC;
            case normal: return CLOCK_MONOTONIC;
            case precise: return CLOCK_MONOTONIC;
            case processCPUTime: return CLOCK_PROCESS_CPUTIME_ID;
            case threadCPUTime: return CLOCK_THREAD_CPUTIME_ID;
            case second: assert(0);
            }
        }
        else
            // It needs to be decided (and implemented in an appropriate
            // version branch here) which clock types new platforms are going
            // to support. Also, ClockType's documentation should be updated to
            // mention it if a new platform uses anything that's not supported
            // on all platforms..
            assert(0, "What are the monotonic clock types supported by this system?");
    }
}

unittest
{
    // Make sure that the values are the same across platforms.
    static if (is(typeof(ClockType.normal)))         static assert(ClockType.normal == 0);
    static if (is(typeof(ClockType.bootTime)))       static assert(ClockType.bootTime == 1);
    static if (is(typeof(ClockType.coarse)))         static assert(ClockType.coarse == 2);
    static if (is(typeof(ClockType.precise)))        static assert(ClockType.precise == 3);
    static if (is(typeof(ClockType.processCPUTime))) static assert(ClockType.processCPUTime == 4);
    static if (is(typeof(ClockType.raw)))            static assert(ClockType.raw == 5);
    static if (is(typeof(ClockType.second)))         static assert(ClockType.second == 6);
    static if (is(typeof(ClockType.threadCPUTime)))  static assert(ClockType.threadCPUTime == 7);
    static if (is(typeof(ClockType.uptime)))         static assert(ClockType.uptime == 8);
    static if (is(typeof(ClockType.uptimeCoarse)))   static assert(ClockType.uptimeCoarse == 9);
    static if (is(typeof(ClockType.uptimePrecise)))  static assert(ClockType.uptimePrecise == 10);
}


/++
    Represents a duration of time of weeks or less (kept internally as hnsecs).
    (e.g. 22 days or 700 seconds).

    It is used when representing a duration of time - such as how long to
    sleep with $(REF Thread.sleep, core,thread).

    In std.datetime, it is also used as the result of various arithmetic
    operations on time points.

    Use the $(LREF dur) function or one of its non-generic aliases to create
    $(D Duration)s.

    It's not possible to create a Duration of months or years, because the
    variable number of days in a month or year makes it impossible to convert
    between months or years and smaller units without a specific date. So,
    nothing uses $(D Duration)s when dealing with months or years. Rather,
    functions specific to months and years are defined. For instance,
    $(REF Date, std,datetime) has $(D add!"years") and $(D add!"months") for adding
    years and months rather than creating a Duration of years or months and
    adding that to a $(REF Date, std,datetime). But Duration is used when dealing
    with weeks or smaller.

    Examples:
--------------------
import std.datetime;

assert(dur!"days"(12) == dur!"hnsecs"(10_368_000_000_000L));
assert(dur!"hnsecs"(27) == dur!"hnsecs"(27));
assert(std.datetime.Date(2010, 9, 7) + dur!"days"(5) ==
       std.datetime.Date(2010, 9, 12));

assert(days(-12) == dur!"hnsecs"(-10_368_000_000_000L));
assert(hnsecs(-27) == dur!"hnsecs"(-27));
assert(std.datetime.Date(2010, 9, 7) - std.datetime.Date(2010, 10, 3) ==
       days(-26));
--------------------
 +/
struct Duration
{
    /++
        Converts this `Duration` to a `string`.

        The string is meant to be human readable, not machine parseable (e.g.
        whether there is an `'s'` on the end of the unit name usually depends on
        whether it's plural or not, and empty units are not included unless the
        Duration is `zero`). Any code needing a specific string format should
        use `total` or `split` to get the units needed to create the desired
        string format and create the string itself.

        The format returned by toString may or may not change in the future.

        Params:
          sink = A sink object, expected to be a delegate or aggregate
                 implementing `opCall` that accepts a `scope const(char)[]`
                 as argument.
      +/
    void toString (SinkT) (scope SinkT sink) const scope
    {
        static immutable units = [
            "weeks", "days", "hours", "minutes", "seconds", "msecs", "usecs"
        ];

        static void appListSep(SinkT sink, uint pos, bool last)
        {
            if (pos == 0)
                return;
            if (!last)
                sink(", ");
            else
                sink(pos == 1 ? " and " : ", and ");
        }

        static void appUnitVal(string units)(SinkT sink, long val)
        {
            immutable plural = val != 1;
            string unit;
            static if (units == "seconds")
                unit = plural ? "secs" : "sec";
            else static if (units == "msecs")
                unit = "ms";
            else static if (units == "usecs")
                unit = "μs";
            else
                unit = plural ? units : units[0 .. $-1];
            sink(signedToTempString(val));
            sink(" ");
            sink(unit);
        }

        if (_hnsecs == 0)
        {
            sink("0 hnsecs");
            return;
        }

        long hnsecs = _hnsecs;
        uint pos;
        static foreach (unit; units)
        {
            if (auto val = splitUnitsFromHNSecs!unit(hnsecs))
            {
                appListSep(sink, pos++, hnsecs == 0);
                appUnitVal!unit(sink, val);
            }
            if (hnsecs == 0)
                return;
        }
        if (hnsecs != 0)
        {
            appListSep(sink, pos++, true);
            appUnitVal!"hnsecs"(sink, hnsecs);
        }
    }

@safe pure:

public:

    /++
        A $(D Duration) of $(D 0). It's shorter than doing something like
        $(D dur!"seconds"(0)) and more explicit than $(D Duration.init).
      +/
    static @property nothrow @nogc Duration zero() { return Duration(0); }

    /++
        Largest $(D Duration) possible.
      +/
    static @property nothrow @nogc Duration max() { return Duration(long.max); }

    /++
        Most negative $(D Duration) possible.
      +/
    static @property nothrow @nogc Duration min() { return Duration(long.min); }

    version (CoreUnittest) unittest
    {
        assert(zero == dur!"seconds"(0));
        assert(Duration.max == Duration(long.max));
        assert(Duration.min == Duration(long.min));
        assert(Duration.min < Duration.zero);
        assert(Duration.zero < Duration.max);
        assert(Duration.min < Duration.max);
        assert(Duration.min - dur!"hnsecs"(1) == Duration.max);
        assert(Duration.max + dur!"hnsecs"(1) == Duration.min);
    }


    /++
        Compares this $(D Duration) with the given $(D Duration).

        Returns:
            $(TABLE
            $(TR $(TD this &lt; rhs) $(TD &lt; 0))
            $(TR $(TD this == rhs) $(TD 0))
            $(TR $(TD this &gt; rhs) $(TD &gt; 0))
            )
     +/
    int opCmp(Duration rhs) const nothrow @nogc
    {
        return (_hnsecs > rhs._hnsecs) - (_hnsecs < rhs._hnsecs);
    }

    version (CoreUnittest) unittest
    {
        import core.internal.traits : rvalueOf;
        foreach (T; AliasSeq!(Duration, const Duration, immutable Duration))
        {
            foreach (U; AliasSeq!(Duration, const Duration, immutable Duration))
            {
                T t = 42;
                // workaround https://issues.dlang.org/show_bug.cgi?id=18296
                version (D_Coverage)
                    U u = T(t._hnsecs);
                else
                    U u = t;
                assert(t == u);
                assert(rvalueOf(t) == u);
                assert(t == rvalueOf(u));
            }
        }

        foreach (D; AliasSeq!(Duration, const Duration, immutable Duration))
        {
            foreach (E; AliasSeq!(Duration, const Duration, immutable Duration))
            {
                assert((cast(D)Duration(12)).opCmp(cast(E)Duration(12)) == 0);
                assert((cast(D)Duration(-12)).opCmp(cast(E)Duration(-12)) == 0);

                assert((cast(D)Duration(10)).opCmp(cast(E)Duration(12)) < 0);
                assert((cast(D)Duration(-12)).opCmp(cast(E)Duration(12)) < 0);

                assert((cast(D)Duration(12)).opCmp(cast(E)Duration(10)) > 0);
                assert((cast(D)Duration(12)).opCmp(cast(E)Duration(-12)) > 0);

                assert(rvalueOf(cast(D)Duration(12)).opCmp(cast(E)Duration(12)) == 0);
                assert(rvalueOf(cast(D)Duration(-12)).opCmp(cast(E)Duration(-12)) == 0);

                assert(rvalueOf(cast(D)Duration(10)).opCmp(cast(E)Duration(12)) < 0);
                assert(rvalueOf(cast(D)Duration(-12)).opCmp(cast(E)Duration(12)) < 0);

                assert(rvalueOf(cast(D)Duration(12)).opCmp(cast(E)Duration(10)) > 0);
                assert(rvalueOf(cast(D)Duration(12)).opCmp(cast(E)Duration(-12)) > 0);

                assert((cast(D)Duration(12)).opCmp(rvalueOf(cast(E)Duration(12))) == 0);
                assert((cast(D)Duration(-12)).opCmp(rvalueOf(cast(E)Duration(-12))) == 0);

                assert((cast(D)Duration(10)).opCmp(rvalueOf(cast(E)Duration(12))) < 0);
                assert((cast(D)Duration(-12)).opCmp(rvalueOf(cast(E)Duration(12))) < 0);

                assert((cast(D)Duration(12)).opCmp(rvalueOf(cast(E)Duration(10))) > 0);
                assert((cast(D)Duration(12)).opCmp(rvalueOf(cast(E)Duration(-12))) > 0);
            }
        }
    }


    /++
        Adds, subtracts or calculates the modulo of two durations.

        The legal types of arithmetic for $(D Duration) using this operator are

        $(TABLE
        $(TR $(TD Duration) $(TD +) $(TD Duration) $(TD -->) $(TD Duration))
        $(TR $(TD Duration) $(TD -) $(TD Duration) $(TD -->) $(TD Duration))
        $(TR $(TD Duration) $(TD %) $(TD Duration) $(TD -->) $(TD Duration))
        )

        Params:
            rhs = The duration to add to or subtract from this $(D Duration).
      +/
    Duration opBinary(string op)(const Duration rhs) const nothrow @nogc
        if (op == "+" || op == "-" || op == "%")
    {
        return Duration(mixin("_hnsecs " ~ op ~ " rhs._hnsecs"));
    }

    deprecated Duration opBinary(string op)(const TickDuration rhs) const nothrow @nogc
        if (op == "+" || op == "-")
    {
        return Duration(mixin("_hnsecs " ~ op ~ " rhs.hnsecs"));
    }

    version (CoreUnittest) unittest
    {
        alias Types = AliasSeq!(Duration, const Duration, immutable Duration);
        foreach (D; Types)
        {
            foreach (E; Types)
            {
                assert((cast(D)Duration(5)) + (cast(E)Duration(7)) == Duration(12));
                assert((cast(D)Duration(5)) - (cast(E)Duration(7)) == Duration(-2));
                assert((cast(D)Duration(5)) % (cast(E)Duration(7)) == Duration(5));
                assert((cast(D)Duration(7)) + (cast(E)Duration(5)) == Duration(12));
                assert((cast(D)Duration(7)) - (cast(E)Duration(5)) == Duration(2));
                assert((cast(D)Duration(7)) % (cast(E)Duration(5)) == Duration(2));

                assert((cast(D)Duration(5)) + (cast(E)Duration(-7)) == Duration(-2));
                assert((cast(D)Duration(5)) - (cast(E)Duration(-7)) == Duration(12));
                assert((cast(D)Duration(5)) % (cast(E)Duration(-7)) == Duration(5));
                assert((cast(D)Duration(7)) + (cast(E)Duration(-5)) == Duration(2));
                assert((cast(D)Duration(7)) - (cast(E)Duration(-5)) == Duration(12));
                assert((cast(D)Duration(7)) % (cast(E)Duration(-5)) == Duration(2));

                assert((cast(D)Duration(-5)) + (cast(E)Duration(7)) == Duration(2));
                assert((cast(D)Duration(-5)) - (cast(E)Duration(7)) == Duration(-12));
                assert((cast(D)Duration(-5)) % (cast(E)Duration(7)) == Duration(-5));
                assert((cast(D)Duration(-7)) + (cast(E)Duration(5)) == Duration(-2));
                assert((cast(D)Duration(-7)) - (cast(E)Duration(5)) == Duration(-12));
                assert((cast(D)Duration(-7)) % (cast(E)Duration(5)) == Duration(-2));

                assert((cast(D)Duration(-5)) + (cast(E)Duration(-7)) == Duration(-12));
                assert((cast(D)Duration(-5)) - (cast(E)Duration(-7)) == Duration(2));
                assert((cast(D)Duration(-5)) % (cast(E)Duration(7)) == Duration(-5));
                assert((cast(D)Duration(-7)) + (cast(E)Duration(-5)) == Duration(-12));
                assert((cast(D)Duration(-7)) - (cast(E)Duration(-5)) == Duration(-2));
                assert((cast(D)Duration(-7)) % (cast(E)Duration(5)) == Duration(-2));
            }
        }
    }

    version (CoreUnittest) deprecated unittest
    {
        foreach (D; AliasSeq!(Duration, const Duration, immutable Duration))
        {
            foreach (T; AliasSeq!(TickDuration, const TickDuration, immutable TickDuration))
            {
                assertApprox((cast(D)Duration(5)) + cast(T)TickDuration.from!"usecs"(7), Duration(70), Duration(80));
                assertApprox((cast(D)Duration(5)) - cast(T)TickDuration.from!"usecs"(7), Duration(-70), Duration(-60));
                assertApprox((cast(D)Duration(7)) + cast(T)TickDuration.from!"usecs"(5), Duration(52), Duration(62));
                assertApprox((cast(D)Duration(7)) - cast(T)TickDuration.from!"usecs"(5), Duration(-48), Duration(-38));

                assertApprox((cast(D)Duration(5)) + cast(T)TickDuration.from!"usecs"(-7), Duration(-70), Duration(-60));
                assertApprox((cast(D)Duration(5)) - cast(T)TickDuration.from!"usecs"(-7), Duration(70), Duration(80));
                assertApprox((cast(D)Duration(7)) + cast(T)TickDuration.from!"usecs"(-5), Duration(-48), Duration(-38));
                assertApprox((cast(D)Duration(7)) - cast(T)TickDuration.from!"usecs"(-5), Duration(52), Duration(62));

                assertApprox((cast(D)Duration(-5)) + cast(T)TickDuration.from!"usecs"(7), Duration(60), Duration(70));
                assertApprox((cast(D)Duration(-5)) - cast(T)TickDuration.from!"usecs"(7), Duration(-80), Duration(-70));
                assertApprox((cast(D)Duration(-7)) + cast(T)TickDuration.from!"usecs"(5), Duration(38), Duration(48));
                assertApprox((cast(D)Duration(-7)) - cast(T)TickDuration.from!"usecs"(5), Duration(-62), Duration(-52));

                assertApprox((cast(D)Duration(-5)) + cast(T)TickDuration.from!"usecs"(-7), Duration(-80), Duration(-70));
                assertApprox((cast(D)Duration(-5)) - cast(T)TickDuration.from!"usecs"(-7), Duration(60), Duration(70));
                assertApprox((cast(D)Duration(-7)) + cast(T)TickDuration.from!"usecs"(-5), Duration(-62), Duration(-52));
                assertApprox((cast(D)Duration(-7)) - cast(T)TickDuration.from!"usecs"(-5), Duration(38), Duration(48));
            }
        }
    }


    /++
        $(RED TickDuration is Deprecated)

        Adds or subtracts two durations.

        The legal types of arithmetic for $(D Duration) using this operator are

        $(TABLE
        $(TR $(TD TickDuration) $(TD +) $(TD Duration) $(TD -->) $(TD Duration))
        $(TR $(TD TickDuration) $(TD -) $(TD Duration) $(TD -->) $(TD Duration))
        )

        Params:
            lhs = The $(D TickDuration) to add to this $(D Duration) or to
                  subtract this $(D Duration) from.
      +/
    deprecated Duration opBinaryRight(string op, D)(D lhs) const nothrow @nogc
        if ((op == "+" || op == "-") &&
            is(immutable D == immutable TickDuration))
    {
        return Duration(mixin("lhs.hnsecs " ~ op ~ " _hnsecs"));
    }

    version (CoreUnittest) deprecated unittest
    {
        foreach (D; AliasSeq!(Duration, const Duration, immutable Duration))
        {
            foreach (T; AliasSeq!(TickDuration, const TickDuration, immutable TickDuration))
            {
                assertApprox((cast(T)TickDuration.from!"usecs"(7)) + cast(D)Duration(5), Duration(70), Duration(80));
                assertApprox((cast(T)TickDuration.from!"usecs"(7)) - cast(D)Duration(5), Duration(60), Duration(70));
                assertApprox((cast(T)TickDuration.from!"usecs"(5)) + cast(D)Duration(7), Duration(52), Duration(62));
                assertApprox((cast(T)TickDuration.from!"usecs"(5)) - cast(D)Duration(7), Duration(38), Duration(48));

                assertApprox((cast(T)TickDuration.from!"usecs"(-7)) + cast(D)Duration(5), Duration(-70), Duration(-60));
                assertApprox((cast(T)TickDuration.from!"usecs"(-7)) - cast(D)Duration(5), Duration(-80), Duration(-70));
                assertApprox((cast(T)TickDuration.from!"usecs"(-5)) + cast(D)Duration(7), Duration(-48), Duration(-38));
                assertApprox((cast(T)TickDuration.from!"usecs"(-5)) - cast(D)Duration(7), Duration(-62), Duration(-52));

                assertApprox((cast(T)TickDuration.from!"usecs"(7)) + (cast(D)Duration(-5)), Duration(60), Duration(70));
                assertApprox((cast(T)TickDuration.from!"usecs"(7)) - (cast(D)Duration(-5)), Duration(70), Duration(80));
                assertApprox((cast(T)TickDuration.from!"usecs"(5)) + (cast(D)Duration(-7)), Duration(38), Duration(48));
                assertApprox((cast(T)TickDuration.from!"usecs"(5)) - (cast(D)Duration(-7)), Duration(52), Duration(62));

                assertApprox((cast(T)TickDuration.from!"usecs"(-7)) + cast(D)Duration(-5), Duration(-80), Duration(-70));
                assertApprox((cast(T)TickDuration.from!"usecs"(-7)) - cast(D)Duration(-5), Duration(-70), Duration(-60));
                assertApprox((cast(T)TickDuration.from!"usecs"(-5)) + cast(D)Duration(-7), Duration(-62), Duration(-52));
                assertApprox((cast(T)TickDuration.from!"usecs"(-5)) - cast(D)Duration(-7), Duration(-48), Duration(-38));
            }
        }
    }


    /++
        Adds, subtracts or calculates the modulo of two durations as well as
        assigning the result to this $(D Duration).

        The legal types of arithmetic for $(D Duration) using this operator are

        $(TABLE
        $(TR $(TD Duration) $(TD +) $(TD Duration) $(TD -->) $(TD Duration))
        $(TR $(TD Duration) $(TD -) $(TD Duration) $(TD -->) $(TD Duration))
        $(TR $(TD Duration) $(TD %) $(TD Duration) $(TD -->) $(TD Duration))
        )

        Params:
            rhs = The duration to add to or subtract from this $(D Duration).
      +/
    ref Duration opOpAssign(string op)(const Duration rhs) nothrow @nogc
        if (op == "+" || op == "-" || op == "%")
    {
        mixin("_hnsecs " ~ op ~ "= rhs._hnsecs;");
        return this;
    }

    deprecated ref Duration opOpAssign(string op)(const TickDuration rhs) nothrow @nogc
        if (op == "+" || op == "-")
    {
        mixin("_hnsecs " ~ op ~ "= rhs.hnsecs;");
        return this;
    }

    version (CoreUnittest) unittest
    {
        static void test1(string op, E)(Duration actual, in E rhs, Duration expected, size_t line = __LINE__)
        {
            if (mixin("actual " ~ op ~ " rhs") != expected)
                throw new AssertError("op failed", __FILE__, line);

            if (actual != expected)
                throw new AssertError("op assign failed", __FILE__, line);
        }

        foreach (E; AliasSeq!(Duration, const Duration, immutable Duration))
        {
            test1!"+="(Duration(5), (cast(E)Duration(7)), Duration(12));
            test1!"-="(Duration(5), (cast(E)Duration(7)), Duration(-2));
            test1!"%="(Duration(5), (cast(E)Duration(7)), Duration(5));
            test1!"+="(Duration(7), (cast(E)Duration(5)), Duration(12));
            test1!"-="(Duration(7), (cast(E)Duration(5)), Duration(2));
            test1!"%="(Duration(7), (cast(E)Duration(5)), Duration(2));

            test1!"+="(Duration(5), (cast(E)Duration(-7)), Duration(-2));
            test1!"-="(Duration(5), (cast(E)Duration(-7)), Duration(12));
            test1!"%="(Duration(5), (cast(E)Duration(-7)), Duration(5));
            test1!"+="(Duration(7), (cast(E)Duration(-5)), Duration(2));
            test1!"-="(Duration(7), (cast(E)Duration(-5)), Duration(12));
            test1!"%="(Duration(7), (cast(E)Duration(-5)), Duration(2));

            test1!"+="(Duration(-5), (cast(E)Duration(7)), Duration(2));
            test1!"-="(Duration(-5), (cast(E)Duration(7)), Duration(-12));
            test1!"%="(Duration(-5), (cast(E)Duration(7)), Duration(-5));
            test1!"+="(Duration(-7), (cast(E)Duration(5)), Duration(-2));
            test1!"-="(Duration(-7), (cast(E)Duration(5)), Duration(-12));
            test1!"%="(Duration(-7), (cast(E)Duration(5)), Duration(-2));

            test1!"+="(Duration(-5), (cast(E)Duration(-7)), Duration(-12));
            test1!"-="(Duration(-5), (cast(E)Duration(-7)), Duration(2));
            test1!"%="(Duration(-5), (cast(E)Duration(-7)), Duration(-5));
            test1!"+="(Duration(-7), (cast(E)Duration(-5)), Duration(-12));
            test1!"-="(Duration(-7), (cast(E)Duration(-5)), Duration(-2));
            test1!"%="(Duration(-7), (cast(E)Duration(-5)), Duration(-2));
        }

        foreach (D; AliasSeq!(const Duration, immutable Duration))
        {
            foreach (E; AliasSeq!(Duration, const Duration, immutable Duration))
            {
                D lhs = D(120);
                E rhs = E(120);
                static assert(!__traits(compiles, lhs += rhs), D.stringof ~ " " ~ E.stringof);
            }
        }
    }

    version (CoreUnittest) deprecated unittest
    {
        static void test2(string op, E)
                         (Duration actual, in E rhs, Duration lower, Duration upper, size_t line = __LINE__)
        {
            assertApprox(mixin("actual " ~ op ~ " rhs"), lower, upper, "op failed", line);
            assertApprox(actual, lower, upper, "op assign failed", line);
        }

        foreach (T; AliasSeq!(TickDuration, const TickDuration, immutable TickDuration))
        {
            test2!"+="(Duration(5), cast(T)TickDuration.from!"usecs"(7), Duration(70), Duration(80));
            test2!"-="(Duration(5), cast(T)TickDuration.from!"usecs"(7), Duration(-70), Duration(-60));
            test2!"+="(Duration(7), cast(T)TickDuration.from!"usecs"(5), Duration(52), Duration(62));
            test2!"-="(Duration(7), cast(T)TickDuration.from!"usecs"(5), Duration(-48), Duration(-38));

            test2!"+="(Duration(5), cast(T)TickDuration.from!"usecs"(-7), Duration(-70), Duration(-60));
            test2!"-="(Duration(5), cast(T)TickDuration.from!"usecs"(-7), Duration(70), Duration(80));
            test2!"+="(Duration(7), cast(T)TickDuration.from!"usecs"(-5), Duration(-48), Duration(-38));
            test2!"-="(Duration(7), cast(T)TickDuration.from!"usecs"(-5), Duration(52), Duration(62));

            test2!"+="(Duration(-5), cast(T)TickDuration.from!"usecs"(7), Duration(60), Duration(70));
            test2!"-="(Duration(-5), cast(T)TickDuration.from!"usecs"(7), Duration(-80), Duration(-70));
            test2!"+="(Duration(-7), cast(T)TickDuration.from!"usecs"(5), Duration(38), Duration(48));
            test2!"-="(Duration(-7), cast(T)TickDuration.from!"usecs"(5), Duration(-62), Duration(-52));

            test2!"+="(Duration(-5), cast(T)TickDuration.from!"usecs"(-7), Duration(-80), Duration(-70));
            test2!"-="(Duration(-5), cast(T)TickDuration.from!"usecs"(-7), Duration(60), Duration(70));
            test2!"+="(Duration(-7), cast(T)TickDuration.from!"usecs"(-5), Duration(-62), Duration(-52));
            test2!"-="(Duration(-7), cast(T)TickDuration.from!"usecs"(-5), Duration(38), Duration(48));
        }

        foreach (D; AliasSeq!(const Duration, immutable Duration))
        {
            foreach (E; AliasSeq!(TickDuration, const TickDuration, immutable TickDuration))
            {
                D lhs = D(120);
                E rhs = E(120);
                static assert(!__traits(compiles, lhs += rhs), D.stringof ~ " " ~ E.stringof);
            }
        }
    }


    /++
        Multiplies or divides the duration by an integer value.

        The legal types of arithmetic for $(D Duration) using this operator
        overload are

        $(TABLE
        $(TR $(TD Duration) $(TD *) $(TD long) $(TD -->) $(TD Duration))
        $(TR $(TD Duration) $(TD /) $(TD long) $(TD -->) $(TD Duration))
        )

        Params:
            value = The value to multiply this $(D Duration) by.
      +/
    Duration opBinary(string op)(long value) const nothrow @nogc
        if (op == "*" || op == "/")
    {
        mixin("return Duration(_hnsecs " ~ op ~ " value);");
    }

    version (CoreUnittest) unittest
    {
        foreach (D; AliasSeq!(Duration, const Duration, immutable Duration))
        {
            assert((cast(D)Duration(5)) * 7 == Duration(35));
            assert((cast(D)Duration(7)) * 5 == Duration(35));

            assert((cast(D)Duration(5)) * -7 == Duration(-35));
            assert((cast(D)Duration(7)) * -5 == Duration(-35));

            assert((cast(D)Duration(-5)) * 7 == Duration(-35));
            assert((cast(D)Duration(-7)) * 5 == Duration(-35));

            assert((cast(D)Duration(-5)) * -7 == Duration(35));
            assert((cast(D)Duration(-7)) * -5 == Duration(35));

            assert((cast(D)Duration(5)) * 0 == Duration(0));
            assert((cast(D)Duration(-5)) * 0 == Duration(0));
        }
    }

    version (CoreUnittest) unittest
    {
        foreach (D; AliasSeq!(Duration, const Duration, immutable Duration))
        {
            assert((cast(D)Duration(5)) / 7 == Duration(0));
            assert((cast(D)Duration(7)) / 5 == Duration(1));

            assert((cast(D)Duration(5)) / -7 == Duration(0));
            assert((cast(D)Duration(7)) / -5 == Duration(-1));

            assert((cast(D)Duration(-5)) / 7 == Duration(0));
            assert((cast(D)Duration(-7)) / 5 == Duration(-1));

            assert((cast(D)Duration(-5)) / -7 == Duration(0));
            assert((cast(D)Duration(-7)) / -5 == Duration(1));
        }
    }


    /++
        Multiplies/Divides the duration by an integer value as well as
        assigning the result to this $(D Duration).

        The legal types of arithmetic for $(D Duration) using this operator
        overload are

        $(TABLE
        $(TR $(TD Duration) $(TD *) $(TD long) $(TD -->) $(TD Duration))
        $(TR $(TD Duration) $(TD /) $(TD long) $(TD -->) $(TD Duration))
        )

        Params:
            value = The value to multiply/divide this $(D Duration) by.
      +/
    ref Duration opOpAssign(string op)(long value) nothrow @nogc
        if (op == "*" || op == "/")
    {
        mixin("_hnsecs " ~ op ~ "= value;");
        return this;
    }

    version (CoreUnittest) unittest
    {
        static void test(D)(D actual, long value, Duration expected, size_t line = __LINE__)
        {
            if ((actual *= value) != expected)
                throw new AssertError("op failed", __FILE__, line);

            if (actual != expected)
                throw new AssertError("op assign failed", __FILE__, line);
        }

        test(Duration(5), 7, Duration(35));
        test(Duration(7), 5, Duration(35));

        test(Duration(5), -7, Duration(-35));
        test(Duration(7), -5, Duration(-35));

        test(Duration(-5), 7, Duration(-35));
        test(Duration(-7), 5, Duration(-35));

        test(Duration(-5), -7, Duration(35));
        test(Duration(-7), -5, Duration(35));

        test(Duration(5), 0, Duration(0));
        test(Duration(-5), 0, Duration(0));

        const cdur = Duration(12);
        immutable idur = Duration(12);
        static assert(!__traits(compiles, cdur *= 12));
        static assert(!__traits(compiles, idur *= 12));
    }

    version (CoreUnittest) unittest
    {
        static void test(Duration actual, long value, Duration expected, size_t line = __LINE__)
        {
            if ((actual /= value) != expected)
                throw new AssertError("op failed", __FILE__, line);

            if (actual != expected)
                throw new AssertError("op assign failed", __FILE__, line);
        }

        test(Duration(5), 7, Duration(0));
        test(Duration(7), 5, Duration(1));

        test(Duration(5), -7, Duration(0));
        test(Duration(7), -5, Duration(-1));

        test(Duration(-5), 7, Duration(0));
        test(Duration(-7), 5, Duration(-1));

        test(Duration(-5), -7, Duration(0));
        test(Duration(-7), -5, Duration(1));

        const cdur = Duration(12);
        immutable idur = Duration(12);
        static assert(!__traits(compiles, cdur /= 12));
        static assert(!__traits(compiles, idur /= 12));
    }


    /++
        Divides two durations.

        The legal types of arithmetic for $(D Duration) using this operator are

        $(TABLE
        $(TR $(TD Duration) $(TD /) $(TD Duration) $(TD -->) $(TD long))
        )

        Params:
            rhs = The duration to divide this $(D Duration) by.
      +/
    long opBinary(string op)(Duration rhs) const nothrow @nogc
        if (op == "/")
    {
        return _hnsecs / rhs._hnsecs;
    }

    version (CoreUnittest) unittest
    {
        assert(Duration(5) / Duration(7) == 0);
        assert(Duration(7) / Duration(5) == 1);
        assert(Duration(8) / Duration(4) == 2);

        assert(Duration(5) / Duration(-7) == 0);
        assert(Duration(7) / Duration(-5) == -1);
        assert(Duration(8) / Duration(-4) == -2);

        assert(Duration(-5) / Duration(7) == 0);
        assert(Duration(-7) / Duration(5) == -1);
        assert(Duration(-8) / Duration(4) == -2);

        assert(Duration(-5) / Duration(-7) == 0);
        assert(Duration(-7) / Duration(-5) == 1);
        assert(Duration(-8) / Duration(-4) == 2);
    }


    /++
        Multiplies an integral value and a $(D Duration).

        The legal types of arithmetic for $(D Duration) using this operator
        overload are

        $(TABLE
        $(TR $(TD long) $(TD *) $(TD Duration) $(TD -->) $(TD Duration))
        )

        Params:
            value = The number of units to multiply this $(D Duration) by.
      +/
    Duration opBinaryRight(string op)(long value) const nothrow @nogc
        if (op == "*")
    {
        return opBinary!op(value);
    }

    version (CoreUnittest) unittest
    {
        foreach (D; AliasSeq!(Duration, const Duration, immutable Duration))
        {
            assert(5 * cast(D)Duration(7) == Duration(35));
            assert(7 * cast(D)Duration(5) == Duration(35));

            assert(5 * cast(D)Duration(-7) == Duration(-35));
            assert(7 * cast(D)Duration(-5) == Duration(-35));

            assert(-5 * cast(D)Duration(7) == Duration(-35));
            assert(-7 * cast(D)Duration(5) == Duration(-35));

            assert(-5 * cast(D)Duration(-7) == Duration(35));
            assert(-7 * cast(D)Duration(-5) == Duration(35));

            assert(0 * cast(D)Duration(-5) == Duration(0));
            assert(0 * cast(D)Duration(5) == Duration(0));
        }
    }


    /++
        Returns the negation of this $(D Duration).
      +/
    Duration opUnary(string op)() const nothrow @nogc
        if (op == "-")
    {
        return Duration(-_hnsecs);
    }

    version (CoreUnittest) unittest
    {
        foreach (D; AliasSeq!(Duration, const Duration, immutable Duration))
        {
            assert(-(cast(D)Duration(7)) == Duration(-7));
            assert(-(cast(D)Duration(5)) == Duration(-5));
            assert(-(cast(D)Duration(-7)) == Duration(7));
            assert(-(cast(D)Duration(-5)) == Duration(5));
            assert(-(cast(D)Duration(0)) == Duration(0));
        }
    }


    /++
        $(RED TickDuration is Deprecated)

        Returns a $(LREF TickDuration) with the same number of hnsecs as this
        $(D Duration).
        Note that the conventional way to convert between $(D Duration) and
        $(D TickDuration) is using $(REF to, std,conv), e.g.:
        $(D duration.to!TickDuration())
      +/
    deprecated TickDuration opCast(T)() const nothrow @nogc
        if (is(immutable T == immutable TickDuration))
    {
        return TickDuration.from!"hnsecs"(_hnsecs);
    }

    version (CoreUnittest) deprecated unittest
    {
        foreach (D; AliasSeq!(Duration, const Duration, immutable Duration))
        {
            foreach (units; AliasSeq!("seconds", "msecs", "usecs", "hnsecs"))
            {
                enum unitsPerSec = convert!("seconds", units)(1);

                if (TickDuration.ticksPerSec >= unitsPerSec)
                {
                    foreach (T; AliasSeq!(TickDuration, const TickDuration, immutable TickDuration))
                    {
                        auto t = TickDuration.from!units(1);
                        assertApprox(cast(T)cast(D)dur!units(1), t - TickDuration(1), t + TickDuration(1), units);
                        t = TickDuration.from!units(2);
                        assertApprox(cast(T)cast(D)dur!units(2), t - TickDuration(1), t + TickDuration(1), units);
                    }
                }
                else
                {
                    auto t = TickDuration.from!units(1);
                    assert(t.to!(units, long)() == 0, units);
                    t = TickDuration.from!units(1_000_000);
                    assert(t.to!(units, long)() >= 900_000, units);
                    assert(t.to!(units, long)() <= 1_100_000, units);
                }
            }
        }
    }

    /++
        Allow Duration to be used as a boolean.
        Returns: `true` if this duration is non-zero.
      +/
    bool opCast(T : bool)() const nothrow @nogc
    {
        return _hnsecs != 0;
    }

    version (CoreUnittest) unittest
    {
        auto d = 10.minutes;
        assert(d);
        assert(!(d - d));
        assert(d + d);
    }

    //Temporary hack until bug http://d.puremagic.com/issues/show_bug.cgi?id=5747 is fixed.
    Duration opCast(T)() const nothrow @nogc
        if (is(immutable T == immutable Duration))
    {
        return this;
    }


    /++
        Splits out the Duration into the given units.

        split takes the list of time units to split out as template arguments.
        The time unit strings must be given in decreasing order. How it returns
        the values for those units depends on the overload used.

        The overload which accepts function arguments takes integral types in
        the order that the time unit strings were given, and those integers are
        passed by $(D ref). split assigns the values for the units to each
        corresponding integer. Any integral type may be used, but no attempt is
        made to prevent integer overflow, so don't use small integral types in
        circumstances where the values for those units aren't likely to fit in
        an integral type that small.

        The overload with no arguments returns the values for the units in a
        struct with members whose names are the same as the given time unit
        strings. The members are all $(D long)s. This overload will also work
        with no time strings being given, in which case $(I all) of the time
        units from weeks through hnsecs will be provided (but no nsecs, since it
        would always be $(D 0)).

        For both overloads, the entire value of the Duration is split among the
        units (rather than splitting the Duration across all units and then only
        providing the values for the requested units), so if only one unit is
        given, the result is equivalent to $(LREF total).

        $(D "nsecs") is accepted by split, but $(D "years") and $(D "months")
        are not.

        For negative durations, all of the split values will be negative.
      +/
    template split(units...)
        if (allAreAcceptedUnits!("weeks", "days", "hours", "minutes", "seconds",
                                "msecs", "usecs", "hnsecs", "nsecs")([units]) &&
           unitsAreInDescendingOrder([units]))
    {
        /++ Ditto +/
        void split(Args...)(out Args args) const nothrow @nogc
            if (units.length != 0 && args.length == units.length && allAreMutableIntegralTypes!Args)
        {
            long hnsecs = _hnsecs;
            foreach (i, unit; units)
            {
                static if (unit == "nsecs")
                    args[i] = cast(Args[i])convert!("hnsecs", "nsecs")(hnsecs);
                else
                    args[i] = cast(Args[i])splitUnitsFromHNSecs!unit(hnsecs);
            }
        }

        /++ Ditto +/
        auto split() const nothrow @nogc
        {
            static if (units.length == 0)
                return split!("weeks", "days", "hours", "minutes", "seconds", "msecs", "usecs", "hnsecs")();
            else
            {
                static string genMemberDecls()
                {
                    string retval;
                    foreach (unit; units)
                    {
                        retval ~= "long ";
                        retval ~= unit;
                        retval ~= "; ";
                    }
                    return retval;
                }

                static struct SplitUnits
                {
                    mixin(genMemberDecls());
                }

                static string genSplitCall()
                {
                    auto retval = "split(";
                    foreach (i, unit; units)
                    {
                        retval ~= "su.";
                        retval ~= unit;
                        if (i < units.length - 1)
                            retval ~= ", ";
                        else
                            retval ~= ");";
                    }
                    return retval;
                }

                SplitUnits su = void;
                mixin(genSplitCall());
                return su;
            }
        }

        /+
            Whether all of the given arguments are integral types.
          +/
        private template allAreMutableIntegralTypes(Args...)
        {
            static if (Args.length == 0)
                enum allAreMutableIntegralTypes = true;
            else static if (!is(Args[0] == long) &&
                           !is(Args[0] == int) &&
                           !is(Args[0] == short) &&
                           !is(Args[0] == byte) &&
                           !is(Args[0] == ulong) &&
                           !is(Args[0] == uint) &&
                           !is(Args[0] == ushort) &&
                           !is(Args[0] == ubyte))
            {
                enum allAreMutableIntegralTypes = false;
            }
            else
                enum allAreMutableIntegralTypes = allAreMutableIntegralTypes!(Args[1 .. $]);
        }

        version (CoreUnittest) unittest
        {
            foreach (T; AliasSeq!(long, int, short, byte, ulong, uint, ushort, ubyte))
                static assert(allAreMutableIntegralTypes!T);
            foreach (T; AliasSeq!(long, int, short, byte, ulong, uint, ushort, ubyte))
                static assert(!allAreMutableIntegralTypes!(const T));
            foreach (T; AliasSeq!(char, wchar, dchar, float, double, real, string))
                static assert(!allAreMutableIntegralTypes!T);
            static assert(allAreMutableIntegralTypes!(long, int, short, byte));
            static assert(!allAreMutableIntegralTypes!(long, int, short, char, byte));
            static assert(!allAreMutableIntegralTypes!(long, int*, short));
        }
    }

    ///
    unittest
    {
        {
            auto d = dur!"days"(12) + dur!"minutes"(7) + dur!"usecs"(501223);
            long days;
            int seconds;
            short msecs;
            d.split!("days", "seconds", "msecs")(days, seconds, msecs);
            assert(days == 12);
            assert(seconds == 7 * 60);
            assert(msecs == 501);

            auto splitStruct = d.split!("days", "seconds", "msecs")();
            assert(splitStruct.days == 12);
            assert(splitStruct.seconds == 7 * 60);
            assert(splitStruct.msecs == 501);

            auto fullSplitStruct = d.split();
            assert(fullSplitStruct.weeks == 1);
            assert(fullSplitStruct.days == 5);
            assert(fullSplitStruct.hours == 0);
            assert(fullSplitStruct.minutes == 7);
            assert(fullSplitStruct.seconds == 0);
            assert(fullSplitStruct.msecs == 501);
            assert(fullSplitStruct.usecs == 223);
            assert(fullSplitStruct.hnsecs == 0);

            assert(d.split!"minutes"().minutes == d.total!"minutes");
        }

        {
            auto d = dur!"days"(12);
            assert(d.split!"weeks"().weeks == 1);
            assert(d.split!"days"().days == 12);

            assert(d.split().weeks == 1);
            assert(d.split().days == 5);
        }

        {
            auto d = dur!"days"(7) + dur!"hnsecs"(42);
            assert(d.split!("seconds", "nsecs")().nsecs == 4200);
        }

        {
            auto d = dur!"days"(-7) + dur!"hours"(-9);
            auto result = d.split!("days", "hours")();
            assert(result.days == -7);
            assert(result.hours == -9);
        }
    }

    version (CoreUnittest) pure nothrow unittest
    {
        foreach (D; AliasSeq!(const Duration, immutable Duration))
        {
            D d = dur!"weeks"(3) + dur!"days"(5) + dur!"hours"(19) + dur!"minutes"(7) +
                  dur!"seconds"(2) + dur!"hnsecs"(1234567);
            byte weeks;
            ubyte days;
            short hours;
            ushort minutes;
            int seconds;
            uint msecs;
            long usecs;
            ulong hnsecs;
            long nsecs;

            d.split!("weeks", "days", "hours", "minutes", "seconds", "msecs", "usecs", "hnsecs", "nsecs")
                    (weeks, days, hours, minutes, seconds, msecs, usecs, hnsecs, nsecs);
            assert(weeks == 3);
            assert(days == 5);
            assert(hours == 19);
            assert(minutes == 7);
            assert(seconds == 2);
            assert(msecs == 123);
            assert(usecs == 456);
            assert(hnsecs == 7);
            assert(nsecs == 0);

            d.split!("weeks", "days", "hours", "seconds", "usecs")(weeks, days, hours, seconds, usecs);
            assert(weeks == 3);
            assert(days == 5);
            assert(hours == 19);
            assert(seconds == 422);
            assert(usecs == 123456);

            d.split!("days", "minutes", "seconds", "nsecs")(days, minutes, seconds, nsecs);
            assert(days == 26);
            assert(minutes == 1147);
            assert(seconds == 2);
            assert(nsecs == 123456700);

            d.split!("minutes", "msecs", "usecs", "hnsecs")(minutes, msecs, usecs, hnsecs);
            assert(minutes == 38587);
            assert(msecs == 2123);
            assert(usecs == 456);
            assert(hnsecs == 7);

            {
                auto result = d.split!("weeks", "days", "hours", "minutes", "seconds",
                                       "msecs", "usecs", "hnsecs", "nsecs");
                assert(result.weeks == 3);
                assert(result.days == 5);
                assert(result.hours == 19);
                assert(result.minutes == 7);
                assert(result.seconds == 2);
                assert(result.msecs == 123);
                assert(result.usecs == 456);
                assert(result.hnsecs == 7);
                assert(result.nsecs == 0);
            }

            {
                auto result = d.split!("weeks", "days", "hours", "seconds", "usecs");
                assert(result.weeks == 3);
                assert(result.days == 5);
                assert(result.hours == 19);
                assert(result.seconds == 422);
                assert(result.usecs == 123456);
            }

            {
                auto result = d.split!("days", "minutes", "seconds", "nsecs")();
                assert(result.days == 26);
                assert(result.minutes == 1147);
                assert(result.seconds == 2);
                assert(result.nsecs == 123456700);
            }

            {
                auto result = d.split!("minutes", "msecs", "usecs", "hnsecs")();
                assert(result.minutes == 38587);
                assert(result.msecs == 2123);
                assert(result.usecs == 456);
                assert(result.hnsecs == 7);
            }

            {
                auto result = d.split();
                assert(result.weeks == 3);
                assert(result.days == 5);
                assert(result.hours == 19);
                assert(result.minutes == 7);
                assert(result.seconds == 2);
                assert(result.msecs == 123);
                assert(result.usecs == 456);
                assert(result.hnsecs == 7);
                static assert(!is(typeof(result.nsecs)));
            }

            static assert(!is(typeof(d.split("seconds", "hnsecs")(seconds))));
            static assert(!is(typeof(d.split("hnsecs", "seconds", "minutes")(hnsecs, seconds, minutes))));
            static assert(!is(typeof(d.split("hnsecs", "seconds", "msecs")(hnsecs, seconds, msecs))));
            static assert(!is(typeof(d.split("seconds", "hnecs", "msecs")(seconds, hnsecs, msecs))));
            static assert(!is(typeof(d.split("seconds", "msecs", "msecs")(seconds, msecs, msecs))));
            static assert(!is(typeof(d.split("hnsecs", "seconds", "minutes")())));
            static assert(!is(typeof(d.split("hnsecs", "seconds", "msecs")())));
            static assert(!is(typeof(d.split("seconds", "hnecs", "msecs")())));
            static assert(!is(typeof(d.split("seconds", "msecs", "msecs")())));
            alias AliasSeq!("nsecs", "hnsecs", "usecs", "msecs", "seconds",
                              "minutes", "hours", "days", "weeks") timeStrs;
            foreach (i, str; timeStrs[1 .. $])
                static assert(!is(typeof(d.split!(timeStrs[i - 1], str)())));

            D nd = -d;

            {
                auto result = nd.split();
                assert(result.weeks == -3);
                assert(result.days == -5);
                assert(result.hours == -19);
                assert(result.minutes == -7);
                assert(result.seconds == -2);
                assert(result.msecs == -123);
                assert(result.usecs == -456);
                assert(result.hnsecs == -7);
            }

            {
                auto result = nd.split!("weeks", "days", "hours", "minutes", "seconds", "nsecs")();
                assert(result.weeks == -3);
                assert(result.days == -5);
                assert(result.hours == -19);
                assert(result.minutes == -7);
                assert(result.seconds == -2);
                assert(result.nsecs == -123456700);
            }
        }
    }


    /++
        Returns the total number of the given units in this $(D Duration).
        So, unlike $(D split), it does not strip out the larger units.
      +/
    @property long total(string units)() const nothrow @nogc
        if (units == "weeks" ||
           units == "days" ||
           units == "hours" ||
           units == "minutes" ||
           units == "seconds" ||
           units == "msecs" ||
           units == "usecs" ||
           units == "hnsecs" ||
           units == "nsecs")
    {
        return convert!("hnsecs", units)(_hnsecs);
    }

    ///
    unittest
    {
        assert(dur!"weeks"(12).total!"weeks" == 12);
        assert(dur!"weeks"(12).total!"days" == 84);

        assert(dur!"days"(13).total!"weeks" == 1);
        assert(dur!"days"(13).total!"days" == 13);

        assert(dur!"hours"(49).total!"days" == 2);
        assert(dur!"hours"(49).total!"hours" == 49);

        assert(dur!"nsecs"(2007).total!"hnsecs" == 20);
        assert(dur!"nsecs"(2007).total!"nsecs" == 2000);
    }

    version (CoreUnittest) unittest
    {
        foreach (D; AliasSeq!(const Duration, immutable Duration))
        {
            assert((cast(D)dur!"weeks"(12)).total!"weeks" == 12);
            assert((cast(D)dur!"weeks"(12)).total!"days" == 84);

            assert((cast(D)dur!"days"(13)).total!"weeks" == 1);
            assert((cast(D)dur!"days"(13)).total!"days" == 13);

            assert((cast(D)dur!"hours"(49)).total!"days" == 2);
            assert((cast(D)dur!"hours"(49)).total!"hours" == 49);

            assert((cast(D)dur!"nsecs"(2007)).total!"hnsecs" == 20);
            assert((cast(D)dur!"nsecs"(2007)).total!"nsecs" == 2000);
        }
    }

    /// Ditto
    string toString() const scope nothrow
    {
        string result;
        this.toString((in char[] data) { result ~= data; });
        return result;
    }

    ///
    unittest
    {
        assert(Duration.zero.toString() == "0 hnsecs");
        assert(weeks(5).toString() == "5 weeks");
        assert(days(2).toString() == "2 days");
        assert(hours(1).toString() == "1 hour");
        assert(minutes(19).toString() == "19 minutes");
        assert(seconds(42).toString() == "42 secs");
        assert(msecs(42).toString() == "42 ms");
        assert(usecs(27).toString() == "27 μs");
        assert(hnsecs(5).toString() == "5 hnsecs");

        assert(seconds(121).toString() == "2 minutes and 1 sec");
        assert((minutes(5) + seconds(3) + usecs(4)).toString() ==
               "5 minutes, 3 secs, and 4 μs");

        assert(seconds(-42).toString() == "-42 secs");
        assert(usecs(-5239492).toString() == "-5 secs, -239 ms, and -492 μs");
    }

    version (CoreUnittest) unittest
    {
        foreach (D; AliasSeq!(Duration, const Duration, immutable Duration))
        {
            assert((cast(D)Duration(0)).toString() == "0 hnsecs");
            assert((cast(D)Duration(1)).toString() == "1 hnsec");
            assert((cast(D)Duration(7)).toString() == "7 hnsecs");
            assert((cast(D)Duration(10)).toString() == "1 μs");
            assert((cast(D)Duration(20)).toString() == "2 μs");
            assert((cast(D)Duration(10_000)).toString() == "1 ms");
            assert((cast(D)Duration(20_000)).toString() == "2 ms");
            assert((cast(D)Duration(10_000_000)).toString() == "1 sec");
            assert((cast(D)Duration(20_000_000)).toString() == "2 secs");
            assert((cast(D)Duration(600_000_000)).toString() == "1 minute");
            assert((cast(D)Duration(1_200_000_000)).toString() == "2 minutes");
            assert((cast(D)Duration(36_000_000_000)).toString() == "1 hour");
            assert((cast(D)Duration(72_000_000_000)).toString() == "2 hours");
            assert((cast(D)Duration(864_000_000_000)).toString() == "1 day");
            assert((cast(D)Duration(1_728_000_000_000)).toString() == "2 days");
            assert((cast(D)Duration(6_048_000_000_000)).toString() == "1 week");
            assert((cast(D)Duration(12_096_000_000_000)).toString() == "2 weeks");

            assert((cast(D)Duration(12)).toString() == "1 μs and 2 hnsecs");
            assert((cast(D)Duration(120_795)).toString() == "12 ms, 79 μs, and 5 hnsecs");
            assert((cast(D)Duration(12_096_020_900_003)).toString() == "2 weeks, 2 secs, 90 ms, and 3 hnsecs");

            assert((cast(D)Duration(-1)).toString() == "-1 hnsecs");
            assert((cast(D)Duration(-7)).toString() == "-7 hnsecs");
            assert((cast(D)Duration(-10)).toString() == "-1 μs");
            assert((cast(D)Duration(-20)).toString() == "-2 μs");
            assert((cast(D)Duration(-10_000)).toString() == "-1 ms");
            assert((cast(D)Duration(-20_000)).toString() == "-2 ms");
            assert((cast(D)Duration(-10_000_000)).toString() == "-1 secs");
            assert((cast(D)Duration(-20_000_000)).toString() == "-2 secs");
            assert((cast(D)Duration(-600_000_000)).toString() == "-1 minutes");
            assert((cast(D)Duration(-1_200_000_000)).toString() == "-2 minutes");
            assert((cast(D)Duration(-36_000_000_000)).toString() == "-1 hours");
            assert((cast(D)Duration(-72_000_000_000)).toString() == "-2 hours");
            assert((cast(D)Duration(-864_000_000_000)).toString() == "-1 days");
            assert((cast(D)Duration(-1_728_000_000_000)).toString() == "-2 days");
            assert((cast(D)Duration(-6_048_000_000_000)).toString() == "-1 weeks");
            assert((cast(D)Duration(-12_096_000_000_000)).toString() == "-2 weeks");

            assert((cast(D)Duration(-12)).toString() == "-1 μs and -2 hnsecs");
            assert((cast(D)Duration(-120_795)).toString() == "-12 ms, -79 μs, and -5 hnsecs");
            assert((cast(D)Duration(-12_096_020_900_003)).toString() == "-2 weeks, -2 secs, -90 ms, and -3 hnsecs");
        }
    }


    /++
        Returns whether this $(D Duration) is negative.
      +/
    @property bool isNegative() const nothrow @nogc
    {
        return _hnsecs < 0;
    }

    version (CoreUnittest) unittest
    {
        foreach (D; AliasSeq!(Duration, const Duration, immutable Duration))
        {
            assert(!(cast(D)Duration(100)).isNegative);
            assert(!(cast(D)Duration(1)).isNegative);
            assert(!(cast(D)Duration(0)).isNegative);
            assert((cast(D)Duration(-1)).isNegative);
            assert((cast(D)Duration(-100)).isNegative);
        }
    }


private:

    /+
        Params:
            hnsecs = The total number of hecto-nanoseconds in this $(D Duration).
      +/
    this(long hnsecs) nothrow @nogc
    {
        _hnsecs = hnsecs;
    }


    long _hnsecs;
}

///
unittest
{
    import core.time;

    // using the dur template
    auto numDays = dur!"days"(12);

    // using the days function
    numDays = days(12);

    // alternatively using UFCS syntax
    numDays = 12.days;

    auto myTime = 100.msecs + 20_000.usecs + 30_000.hnsecs;
    assert(myTime == 123.msecs);
}

// Ensure `toString` doesn't allocate if the sink doesn't
version (CoreUnittest) @safe pure nothrow @nogc unittest
{
    char[256] buffer; size_t len;
    scope sink = (in char[] data) {
        assert(data.length + len <= buffer.length);
        buffer[len .. len + data.length] = data[];
        len += data.length;
    };
    auto dur = Duration(-12_096_020_900_003);
    dur.toString(sink);
    assert(buffer[0 .. len] == "-2 weeks, -2 secs, -90 ms, and -3 hnsecs");
}

/++
    $(RED TickDuration is DEPRECATED)

    Converts a $(D TickDuration) to the given units as either an integral
    value or a floating point value.

    Params:
        units = The units to convert to. Accepts $(D "seconds") and smaller
                only.
        T     = The type to convert to (either an integral type or a
                floating point type).

        td    = The TickDuration to convert
  +/
deprecated("TickDuration has been deprecated, please use Duration or MonoTime instead")
T to(string units, T, D)(D td) @safe pure nothrow @nogc
    if (is(immutable D == immutable TickDuration) &&
       (units == "seconds" ||
        units == "msecs" ||
        units == "usecs" ||
        units == "hnsecs" ||
        units == "nsecs"))
{
    static if (__traits(isIntegral, T) && T.sizeof >= 4)
    {
        enum unitsPerSec = convert!("seconds", units)(1);

        return cast(T) (td.length / (TickDuration.ticksPerSec / cast(real) unitsPerSec));
    }
    else static if (__traits(isFloating, T))
    {
        static if (units == "seconds")
            return td.length / cast(T)TickDuration.ticksPerSec;
        else
        {
            enum unitsPerSec = convert!("seconds", units)(1);

            return cast(T) (td.length /
                (TickDuration.ticksPerSec / cast(real) unitsPerSec));
        }
    }
    else
        static assert(0, "Incorrect template constraint.");
}

///
deprecated unittest
{
    auto t = TickDuration.from!"seconds"(1000);

    long tl = to!("seconds",long)(t);
    assert(tl == 1000);

    import core.stdc.math : fabs;
    double td = to!("seconds",double)(t);
    assert(fabs(td - 1000) < 0.001);
}

deprecated unittest
{
    void testFun(string U)() {
        auto t1v = 1000;
        auto t2v = 333;

        auto t1 = TickDuration.from!U(t1v);
        auto t2 = TickDuration.from!U(t2v);

        auto _str(F)(F val)
        {
            static if (is(F == int) || is(F == long))
                return signedToTempString(val);
            else
                return unsignedToTempString(val);
        }

        foreach (F; AliasSeq!(int,uint,long,ulong,float,double,real))
        {
            F t1f = to!(U,F)(t1);
            F t2f = to!(U,F)(t2);
            auto t12d = t1 / t2v;
            auto t12m = t1 - t2;
            F t3f = to!(U,F)(t12d);
            F t4f = to!(U,F)(t12m);


            static if (is(F == float) || is(F == double) || is(F == real))
            {
                assert((t1f - cast(F)t1v) <= 3.0,
                    F.stringof ~ " " ~ U ~ " " ~ doubleToString(t1f) ~ " " ~
                    doubleToString(cast(F)t1v)
                );
                assert((t2f - cast(F)t2v) <= 3.0,
                    F.stringof ~ " " ~ U ~ " " ~ doubleToString(t2f) ~ " " ~
                    doubleToString(cast(F)t2v)
                );
                assert(t3f - (cast(F)t1v) / (cast(F)t2v) <= 3.0,
                    F.stringof ~ " " ~ U ~ " " ~ doubleToString(t3f) ~ " " ~
                    doubleToString((cast(F)t1v)/(cast(F)t2v))
                );
                assert(t4f - (cast(F)(t1v - t2v)) <= 3.0,
                    F.stringof ~ " " ~ U ~ " " ~ doubleToString(t4f) ~ " " ~
                    doubleToString(cast(F)(t1v - t2v))
                );
            }
            else
            {
                // even though this should be exact math it is not as internal
                // in "to" floating point is used
                assert(_abs(t1f) - _abs(cast(F)t1v) <= 3,
                    F.stringof ~ " " ~ U ~ " " ~ _str(t1f) ~ " " ~
                    _str(cast(F)t1v)
                );
                assert(_abs(t2f) - _abs(cast(F)t2v) <= 3,
                    F.stringof ~ " " ~ U ~ " " ~ _str(t2f) ~ " " ~
                    _str(cast(F)t2v)
                );
                assert(_abs(t3f) - _abs((cast(F)t1v) / (cast(F)t2v)) <= 3,
                    F.stringof ~ " " ~ U ~ " " ~ _str(t3f) ~ " " ~
                    _str((cast(F)t1v) / (cast(F)t2v))
                );
                assert(_abs(t4f) - _abs((cast(F)t1v) - (cast(F)t2v)) <= 3,
                    F.stringof ~ " " ~ U ~ " " ~ _str(t4f) ~ " " ~
                    _str((cast(F)t1v) - (cast(F)t2v))
                );
            }
        }
    }

    testFun!"seconds"();
    testFun!"msecs"();
    testFun!"usecs"();
}

/++
    These allow you to construct a $(D Duration) from the given time units
    with the given length.

    You can either use the generic function $(D dur) and give it the units as
    a $(D string) or use the named aliases.

    The possible values for units are $(D "weeks"), $(D "days"), $(D "hours"),
    $(D "minutes"), $(D "seconds"), $(D "msecs") (milliseconds), $(D "usecs"),
    (microseconds), $(D "hnsecs") (hecto-nanoseconds, i.e. 100 ns), and
    $(D "nsecs").

    Params:
        units  = The time units of the $(D Duration) (e.g. $(D "days")).
        length = The number of units in the $(D Duration).
  +/
Duration dur(string units)(long length) @safe pure nothrow @nogc
    if (units == "weeks" ||
       units == "days" ||
       units == "hours" ||
       units == "minutes" ||
       units == "seconds" ||
       units == "msecs" ||
       units == "usecs" ||
       units == "hnsecs" ||
       units == "nsecs")
{
    return Duration(convert!(units, "hnsecs")(length));
}

alias weeks   = dur!"weeks";   /// Ditto
alias days    = dur!"days";    /// Ditto
alias hours   = dur!"hours";   /// Ditto
alias minutes = dur!"minutes"; /// Ditto
alias seconds = dur!"seconds"; /// Ditto
alias msecs   = dur!"msecs";   /// Ditto
alias usecs   = dur!"usecs";   /// Ditto
alias hnsecs  = dur!"hnsecs";  /// Ditto
alias nsecs   = dur!"nsecs";   /// Ditto

///
unittest
{
    // Generic
    assert(dur!"weeks"(142).total!"weeks" == 142);
    assert(dur!"days"(142).total!"days" == 142);
    assert(dur!"hours"(142).total!"hours" == 142);
    assert(dur!"minutes"(142).total!"minutes" == 142);
    assert(dur!"seconds"(142).total!"seconds" == 142);
    assert(dur!"msecs"(142).total!"msecs" == 142);
    assert(dur!"usecs"(142).total!"usecs" == 142);
    assert(dur!"hnsecs"(142).total!"hnsecs" == 142);
    assert(dur!"nsecs"(142).total!"nsecs" == 100);

    // Non-generic
    assert(weeks(142).total!"weeks" == 142);
    assert(days(142).total!"days" == 142);
    assert(hours(142).total!"hours" == 142);
    assert(minutes(142).total!"minutes" == 142);
    assert(seconds(142).total!"seconds" == 142);
    assert(msecs(142).total!"msecs" == 142);
    assert(usecs(142).total!"usecs" == 142);
    assert(hnsecs(142).total!"hnsecs" == 142);
    assert(nsecs(142).total!"nsecs" == 100);
}

unittest
{
    foreach (D; AliasSeq!(Duration, const Duration, immutable Duration))
    {
        assert(dur!"weeks"(7).total!"weeks" == 7);
        assert(dur!"days"(7).total!"days" == 7);
        assert(dur!"hours"(7).total!"hours" == 7);
        assert(dur!"minutes"(7).total!"minutes" == 7);
        assert(dur!"seconds"(7).total!"seconds" == 7);
        assert(dur!"msecs"(7).total!"msecs" == 7);
        assert(dur!"usecs"(7).total!"usecs" == 7);
        assert(dur!"hnsecs"(7).total!"hnsecs" == 7);
        assert(dur!"nsecs"(7).total!"nsecs" == 0);

        assert(dur!"weeks"(1007) == weeks(1007));
        assert(dur!"days"(1007) == days(1007));
        assert(dur!"hours"(1007) == hours(1007));
        assert(dur!"minutes"(1007) == minutes(1007));
        assert(dur!"seconds"(1007) == seconds(1007));
        assert(dur!"msecs"(1007) == msecs(1007));
        assert(dur!"usecs"(1007) == usecs(1007));
        assert(dur!"hnsecs"(1007) == hnsecs(1007));
        assert(dur!"nsecs"(10) == nsecs(10));
    }
}

// used in MonoTimeImpl
private string _clockTypeName(ClockType clockType)
{
    final switch (clockType)
    {
        foreach (name; __traits(allMembers, ClockType))
        {
        case __traits(getMember, ClockType, name):
            return name;
        }
    }
    assert(0);
}

// used in MonoTimeImpl
private size_t _clockTypeIdx(ClockType clockType)
{
    final switch (clockType)
    {
        foreach (i, name; __traits(allMembers, ClockType))
        {
        case __traits(getMember, ClockType, name):
            return i;
        }
    }
    assert(0);
}


/++
    alias for $(D MonoTimeImpl) instantiated with $(D ClockType.normal). This is
    what most programs should use. It's also what much of $(D MonoTimeImpl) uses
    in its documentation (particularly in the examples), because that's what's
    going to be used in most code.
  +/
alias MonoTime = MonoTimeImpl!(ClockType.normal);

/++
    Represents a timestamp of the system's monotonic clock.

    A monotonic clock is one which always goes forward and never moves
    backwards, unlike the system's wall clock time (as represented by
    $(REF SysTime, std,datetime)). The system's wall clock time can be adjusted
    by the user or by the system itself via services such as NTP, so it is
    unreliable to use the wall clock time for timing. Timers which use the wall
    clock time could easily end up never going off due to changes made to the
    wall clock time or otherwise waiting for a different period of time than
    that specified by the programmer. However, because the monotonic clock
    always increases at a fixed rate and is not affected by adjustments to the
    wall clock time, it is ideal for use with timers or anything which requires
    high precision timing.

    So, MonoTime should be used for anything involving timers and timing,
    whereas $(REF SysTime, std,datetime) should be used when the wall clock time
    is required.

    The monotonic clock has no relation to wall clock time. Rather, it holds
    its time as the number of ticks of the clock which have occurred since the
    clock started (typically when the system booted up). So, to determine how
    much time has passed between two points in time, one monotonic time is
    subtracted from the other to determine the number of ticks which occurred
    between the two points of time, and those ticks are divided by the number of
    ticks that occur every second (as represented by MonoTime.ticksPerSecond)
    to get a meaningful duration of time. Normally, MonoTime does these
    calculations for the programmer, but the $(D ticks) and $(D ticksPerSecond)
    properties are provided for those who require direct access to the system
    ticks. The normal way that MonoTime would be used is

--------------------
    MonoTime before = MonoTime.currTime;
    // do stuff...
    MonoTime after = MonoTime.currTime;
    Duration timeElapsed = after - before;
--------------------

    $(LREF MonoTime) is an alias to $(D MonoTimeImpl!(ClockType.normal)) and is
    what most programs should use for the monotonic clock, so that's what is
    used in most of $(D MonoTimeImpl)'s documentation. But $(D MonoTimeImpl)
    can be instantiated with other clock types for those rare programs that need
    it.

    See_Also:
        $(LREF ClockType)
  +/
struct MonoTimeImpl(ClockType clockType)
{
    private enum _clockIdx = _clockTypeIdx(clockType);
    private enum _clockName = _clockTypeName(clockType);

@safe:

    version (Windows)
    {
        static if (clockType != ClockType.coarse &&
                  clockType != ClockType.normal &&
                  clockType != ClockType.precise)
        {
            static assert(0, "ClockType." ~ _clockName ~
                             " is not supported by MonoTimeImpl on this system.");
        }
    }
    else version (Darwin)
    {
        static if (clockType != ClockType.coarse &&
                  clockType != ClockType.normal &&
                  clockType != ClockType.precise)
        {
            static assert(0, "ClockType." ~ _clockName ~
                             " is not supported by MonoTimeImpl on this system.");
        }
    }
    else version (Posix)
    {
        enum clockArg = _posixClock(clockType);
    }
    else
        static assert(0, "Unsupported platform");

    // POD value, test mutable/const/immutable conversion
    version (CoreUnittest) unittest
    {
        MonoTimeImpl m;
        const MonoTimeImpl cm = m;
        immutable MonoTimeImpl im = m;
        m = cm;
        m = im;
    }

    /++
        The current time of the system's monotonic clock. This has no relation
        to the wall clock time, as the wall clock time can be adjusted (e.g.
        by NTP), whereas the monotonic clock always moves forward. The source
        of the monotonic time is system-specific.

        On Windows, $(D QueryPerformanceCounter) is used. On Mac OS X,
        $(D mach_absolute_time) is used, while on other POSIX systems,
        $(D clock_gettime) is used.

        $(RED Warning): On some systems, the monotonic clock may stop counting
                        when the computer goes to sleep or hibernates. So, the
                        monotonic clock may indicate less time than has actually
                        passed if that occurs. This is known to happen on
                        Mac OS X. It has not been tested whether it occurs on
                        either Windows or Linux.
      +/
    static @property MonoTimeImpl currTime() @trusted nothrow @nogc
    {
        if (ticksPerSecond == 0)
        {
            import core.internal.abort : abort;
            abort("MonoTimeImpl!(ClockType." ~ _clockName ~
                      ") failed to get the frequency of the system's monotonic clock.");
        }

        version (Windows)
        {
            long ticks = void;
            QueryPerformanceCounter(&ticks);
            return MonoTimeImpl(ticks);
        }
        else version (Darwin)
            return MonoTimeImpl(mach_absolute_time());
        else version (Posix)
        {
            timespec ts = void;
            immutable error = clock_gettime(clockArg, &ts);
            // clockArg is supported and if tv_sec is long or larger
            // overflow won't happen before 292 billion years A.D.
            static if (ts.tv_sec.max < long.max)
            {
                if (error)
                {
                    import core.internal.abort : abort;
                    abort("Call to clock_gettime failed.");
                }
            }
            return MonoTimeImpl(convClockFreq(ts.tv_sec * 1_000_000_000L + ts.tv_nsec,
                                              1_000_000_000L,
                                              ticksPerSecond));
        }
    }


    static @property pure nothrow @nogc
    {
    /++
        A $(D MonoTime) of $(D 0) ticks. It's provided to be consistent with
        $(D Duration.zero), and it's more explicit than $(D MonoTime.init).
      +/
    MonoTimeImpl zero() { return MonoTimeImpl(0); }

    /++
        Largest $(D MonoTime) possible.
      +/
    MonoTimeImpl max() { return MonoTimeImpl(long.max); }

    /++
        Most negative $(D MonoTime) possible.
      +/
    MonoTimeImpl min() { return MonoTimeImpl(long.min); }
    }

    version (CoreUnittest) unittest
    {
        assert(MonoTimeImpl.zero == MonoTimeImpl(0));
        assert(MonoTimeImpl.max == MonoTimeImpl(long.max));
        assert(MonoTimeImpl.min == MonoTimeImpl(long.min));
        assert(MonoTimeImpl.min < MonoTimeImpl.zero);
        assert(MonoTimeImpl.zero < MonoTimeImpl.max);
        assert(MonoTimeImpl.min < MonoTimeImpl.max);
    }


    /++
        Compares this MonoTime with the given MonoTime.

        Returns:
            $(BOOKTABLE,
                $(TR $(TD this &lt; rhs) $(TD &lt; 0))
                $(TR $(TD this == rhs) $(TD 0))
                $(TR $(TD this &gt; rhs) $(TD &gt; 0))
            )
     +/
    int opCmp(MonoTimeImpl rhs) const pure nothrow @nogc
    {
        return (_ticks > rhs._ticks) - (_ticks < rhs._ticks);
    }

    version (CoreUnittest) unittest
    {
        import core.internal.traits : rvalueOf;
        const t = MonoTimeImpl.currTime;
        assert(t == rvalueOf(t));
    }

    version (CoreUnittest) unittest
    {
        import core.internal.traits : rvalueOf;
        const before = MonoTimeImpl.currTime;
        auto after = MonoTimeImpl(before._ticks + 42);
        assert(before < after);
        assert(rvalueOf(before) <= before);
        assert(rvalueOf(after) > before);
        assert(after >= rvalueOf(after));
    }

    version (CoreUnittest) unittest
    {
        const currTime = MonoTimeImpl.currTime;
        assert(MonoTimeImpl(long.max) > MonoTimeImpl(0));
        assert(MonoTimeImpl(0) > MonoTimeImpl(long.min));
        assert(MonoTimeImpl(long.max) > currTime);
        assert(currTime > MonoTimeImpl(0));
        assert(MonoTimeImpl(0) < currTime);
        assert(MonoTimeImpl(0) < MonoTimeImpl(long.max));
        assert(MonoTimeImpl(long.min) < MonoTimeImpl(0));
    }


    /++
        Subtracting two MonoTimes results in a $(LREF Duration) representing
        the amount of time which elapsed between them.

        The primary way that programs should time how long something takes is to
        do
--------------------
MonoTime before = MonoTime.currTime;
// do stuff
MonoTime after = MonoTime.currTime;

// How long it took.
Duration timeElapsed = after - before;
--------------------
        or to use a wrapper (such as a stop watch type) which does that.

        $(RED Warning):
            Because $(LREF Duration) is in hnsecs, whereas MonoTime is in system
            ticks, it's usually the case that this assertion will fail
--------------------
auto before = MonoTime.currTime;
// do stuff
auto after = MonoTime.currTime;
auto timeElapsed = after - before;
assert(before + timeElapsed == after);
--------------------

            This is generally fine, and by its very nature, converting from
            system ticks to any type of seconds (hnsecs, nsecs, etc.) will
            introduce rounding errors, but if code needs to avoid any of the
            small rounding errors introduced by conversion, then it needs to use
            MonoTime's $(D ticks) property and keep all calculations in ticks
            rather than using $(LREF Duration).
      +/
    Duration opBinary(string op)(MonoTimeImpl rhs) const pure nothrow @nogc
        if (op == "-")
    {
        immutable diff = _ticks - rhs._ticks;
        return Duration(convClockFreq(diff , ticksPerSecond, hnsecsPer!"seconds"));
    }

    version (CoreUnittest) unittest
    {
        import core.internal.traits : rvalueOf;
        const t = MonoTimeImpl.currTime;
        assert(t - rvalueOf(t) == Duration.zero);
        static assert(!__traits(compiles, t + t));
    }

    version (CoreUnittest) unittest
    {
        static void test(const scope MonoTimeImpl before, const scope MonoTimeImpl after, const scope Duration min)
        {
            immutable diff = after - before;
            assert(diff >= min);
            auto calcAfter = before + diff;
            assertApprox(calcAfter, calcAfter - Duration(1), calcAfter + Duration(1));
            assert(before - after == -diff);
        }

        const before = MonoTimeImpl.currTime;
        test(before, MonoTimeImpl(before._ticks + 4202), Duration.zero);
        test(before, MonoTimeImpl.currTime, Duration.zero);

        const durLargerUnits = dur!"minutes"(7) + dur!"seconds"(22);
        test(before, before + durLargerUnits + dur!"msecs"(33) + dur!"hnsecs"(571), durLargerUnits);
    }


    /++
        Adding or subtracting a $(LREF Duration) to/from a MonoTime results in
        a MonoTime which is adjusted by that amount.
      +/
    MonoTimeImpl opBinary(string op)(Duration rhs) const pure nothrow @nogc
        if (op == "+" || op == "-")
    {
        immutable rhsConverted = convClockFreq(rhs._hnsecs, hnsecsPer!"seconds", ticksPerSecond);
        mixin("return MonoTimeImpl(_ticks " ~ op ~ " rhsConverted);");
    }

    version (CoreUnittest) unittest
    {
        const t = MonoTimeImpl.currTime;
        assert(t + Duration(0) == t);
        assert(t - Duration(0) == t);
    }

    version (CoreUnittest) unittest
    {
        const t = MonoTimeImpl.currTime;

        // We reassign ticks in order to get the same rounding errors
        // that we should be getting with Duration (e.g. MonoTimeImpl may be
        // at a higher precision than hnsecs, meaning that 7333 would be
        // truncated when converting to hnsecs).
        long ticks = 7333;
        auto hnsecs = convClockFreq(ticks, ticksPerSecond, hnsecsPer!"seconds");
        ticks = convClockFreq(hnsecs, hnsecsPer!"seconds", ticksPerSecond);

        assert(t - Duration(hnsecs) == MonoTimeImpl(t._ticks - ticks));
        assert(t + Duration(hnsecs) == MonoTimeImpl(t._ticks + ticks));
    }


    /++ Ditto +/
    ref MonoTimeImpl opOpAssign(string op)(Duration rhs) pure nothrow @nogc
        if (op == "+" || op == "-")
    {
        immutable rhsConverted = convClockFreq(rhs._hnsecs, hnsecsPer!"seconds", ticksPerSecond);
        mixin("_ticks " ~ op ~ "= rhsConverted;");
        return this;
    }

    version (CoreUnittest) unittest
    {
        auto mt = MonoTimeImpl.currTime;
        const initial = mt;
        mt += Duration(0);
        assert(mt == initial);
        mt -= Duration(0);
        assert(mt == initial);

        // We reassign ticks in order to get the same rounding errors
        // that we should be getting with Duration (e.g. MonoTimeImpl may be
        // at a higher precision than hnsecs, meaning that 7333 would be
        // truncated when converting to hnsecs).
        long ticks = 7333;
        auto hnsecs = convClockFreq(ticks, ticksPerSecond, hnsecsPer!"seconds");
        ticks = convClockFreq(hnsecs, hnsecsPer!"seconds", ticksPerSecond);
        auto before = MonoTimeImpl(initial._ticks - ticks);

        assert((mt -= Duration(hnsecs)) == before);
        assert(mt  == before);
        assert((mt += Duration(hnsecs)) == initial);
        assert(mt  == initial);
    }


    /++
        The number of ticks in the monotonic time.

        Most programs should not use this directly, but it's exposed for those
        few programs that need it.

        The main reasons that a program might need to use ticks directly is if
        the system clock has higher precision than hnsecs, and the program needs
        that higher precision, or if the program needs to avoid the rounding
        errors caused by converting to hnsecs.
      +/
    @property long ticks() const pure nothrow @nogc
    {
        return _ticks;
    }

    version (CoreUnittest) unittest
    {
        const mt = MonoTimeImpl.currTime;
        assert(mt.ticks == mt._ticks);
    }


    /++
        The number of ticks that MonoTime has per second - i.e. the resolution
        or frequency of the system's monotonic clock.

        e.g. if the system clock had a resolution of microseconds, then
        ticksPerSecond would be $(D 1_000_000).
      +/
    static @property long ticksPerSecond() pure nothrow @nogc
    {
        return _ticksPerSecond[_clockIdx];
    }

    version (CoreUnittest) unittest
    {
        assert(MonoTimeImpl.ticksPerSecond == _ticksPerSecond[_clockIdx]);
    }


    ///
    string toString() const pure nothrow
    {
        static if (clockType == ClockType.normal)
            return "MonoTime(" ~ signedToTempString(_ticks) ~ " ticks, " ~ signedToTempString(ticksPerSecond) ~ " ticks per second)";
        else
            return "MonoTimeImpl!(ClockType." ~ _clockName ~ ")(" ~ signedToTempString(_ticks) ~ " ticks, " ~
                   signedToTempString(ticksPerSecond) ~ " ticks per second)";
    }

    version (CoreUnittest) unittest
    {
        import core.internal.util.math : min;

        static void eat(ref string s, const(char)[] exp)
        {
            assert(s[0 .. min($, exp.length)] == exp, s~" != "~exp);
            s = s[exp.length .. $];
        }

        immutable mt = MonoTimeImpl.currTime;
        auto str = mt.toString();
        static if (is(typeof(this) == MonoTime))
            eat(str, "MonoTime(");
        else
            eat(str, "MonoTimeImpl!(ClockType."~_clockName~")(");

        eat(str, signedToTempString(mt._ticks));
        eat(str, " ticks, ");
        eat(str, signedToTempString(ticksPerSecond));
        eat(str, " ticks per second)");
    }

private:

    // static immutable long _ticksPerSecond;

    version (CoreUnittest) unittest
    {
        assert(_ticksPerSecond[_clockIdx]);
    }


    long _ticks;
}

// This is supposed to be a static variable in MonoTimeImpl with the static
// constructor being in there, but https://issues.dlang.org/show_bug.cgi?id=14517
// prevents that from working. However, moving it back to a static ctor will
// reraise issues with other systems using MonoTime, so we should leave this
// here even when that bug is fixed.
private immutable long[__traits(allMembers, ClockType).length] _ticksPerSecond;

// This is called directly from the runtime initilization function (rt_init),
// instead of using a static constructor. Other subsystems inside the runtime
// (namely, the GC) may need time functionality, but cannot wait until the
// static ctors have run. Therefore, we initialize these specially. Because
// it's a normal function, we need to do some dangerous casting PLEASE take
// care when modifying this function, and it should NOT be called except from
// the runtime init.
//
// NOTE: the code below SPECIFICALLY does not assert when it cannot initialize
// the ticks per second array. This allows cases where a clock is never used on
// a system that doesn't support it. See bugzilla issue
// https://issues.dlang.org/show_bug.cgi?id=14863
// The assert will occur when someone attempts to use _ticksPerSecond for that
// value.
extern(C) void _d_initMonoTime() @nogc nothrow
{
    // We need a mutable pointer to the ticksPerSecond array. Although this
    // would appear to break immutability, it is logically the same as a static
    // ctor. So we should ONLY write these values once (we will check for 0
    // values when setting to ensure this is truly only called once).
    auto tps = cast(long[])_ticksPerSecond[];

    // If we try to do anything with ClockType in the documentation build, it'll
    // trigger the static assertions related to ClockType, since the
    // documentation build defines all of the possible ClockTypes, which won't
    // work when they're used in the static ifs, because no system supports them
    // all.
    version (CoreDdoc)
    {}
    else version (Windows)
    {
        long ticksPerSecond;
        if (QueryPerformanceFrequency(&ticksPerSecond) != 0)
        {
            foreach (i, typeStr; __traits(allMembers, ClockType))
            {
                // ensure we are only writing immutable data once
                if (tps[i] != 0)
                    // should only be called once
                    assert(0);
                tps[i] = ticksPerSecond;
            }
        }
    }
    else version (Darwin)
    {
        immutable long ticksPerSecond = machTicksPerSecond();
        foreach (i, typeStr; __traits(allMembers, ClockType))
        {
            // ensure we are only writing immutable data once
            if (tps[i] != 0)
                // should only be called once
                assert(0);
            tps[i] = ticksPerSecond;
        }
    }
    else version (Posix)
    {
        timespec ts;
        foreach (i, typeStr; __traits(allMembers, ClockType))
        {
            static if (typeStr != "second")
            {
                enum clockArg = _posixClock(__traits(getMember, ClockType, typeStr));
                if (clock_getres(clockArg, &ts) == 0)
                {
                    // ensure we are only writing immutable data once
                    if (tps[i] != 0)
                        // should only be called once
                        assert(0);

                    // For some reason, on some systems, clock_getres returns
                    // a resolution which is clearly wrong:
                    //  - it's a millisecond or worse, but the time is updated
                    //    much more frequently than that.
                    //  - it's negative
                    //  - it's zero
                    // In such cases, we'll just use nanosecond resolution.
                    tps[i] = ts.tv_sec != 0 || ts.tv_nsec <= 0 || ts.tv_nsec >= 1000
                        ? 1_000_000_000L : 1_000_000_000L / ts.tv_nsec;
                }
            }
        }
    }
    else
        static assert(0, "Unsupported platform");
}


// Tests for MonoTimeImpl.currTime. It has to be outside, because MonoTimeImpl
// is a template. This unittest block also makes sure that MonoTimeImpl actually
// is instantiated with all of the various ClockTypes so that those types and
// their tests are compiled and run.
unittest
{
    // This test is separate so that it can be tested with MonoTime and not just
    // MonoTimeImpl.
    auto norm1 = MonoTime.currTime;
    auto norm2 = MonoTimeImpl!(ClockType.normal).currTime;
    assert(norm1 <= norm2);

    static bool clockSupported(ClockType c)
    {
        // Skip unsupported clocks on older linux kernels, assume that only
        // CLOCK_MONOTONIC and CLOCK_REALTIME exist, as that is the lowest
        // common denominator supported by all versions of Linux pre-2.6.12.
        version (Linux_Pre_2639)
            return c == ClockType.normal || c == ClockType.precise;
        else
            return c != ClockType.second; // second doesn't work with MonoTimeImpl

    }

    foreach (typeStr; __traits(allMembers, ClockType))
    {
        mixin("alias type = ClockType." ~ typeStr ~ ";");
        static if (clockSupported(type))
        {
            auto v1 = MonoTimeImpl!type.currTime;
            auto v2 = MonoTimeImpl!type.currTime;
            scope(failure)
            {
                printf("%s: v1 %s, v2 %s, tps %s\n",
                       (type.stringof ~ "\0").ptr,
                       numToStringz(v1._ticks),
                       numToStringz(v2._ticks),
                       numToStringz(typeof(v1).ticksPerSecond));
            }
            assert(v1 <= v2);

            foreach (otherStr; __traits(allMembers, ClockType))
            {
                mixin("alias other = ClockType." ~ otherStr ~ ";");
                static if (clockSupported(other))
                {
                    static assert(is(typeof({auto o1 = MonTimeImpl!other.currTime; auto b = v1 <= o1;})) ==
                                  is(type == other));
                }
            }
        }
    }
}


/++
    Converts the given time from one clock frequency/resolution to another.

    See_Also:
        $(LREF ticksToNSecs)
  +/
long convClockFreq(long ticks, long srcTicksPerSecond, long dstTicksPerSecond) @safe pure nothrow @nogc
{
    // This would be more straightforward with floating point arithmetic,
    // but we avoid it here in order to avoid the rounding errors that that
    // introduces. Also, by splitting out the units in this way, we're able
    // to deal with much larger values before running into problems with
    // integer overflow.
    return ticks / srcTicksPerSecond * dstTicksPerSecond +
           ticks % srcTicksPerSecond * dstTicksPerSecond / srcTicksPerSecond;
}

///
unittest
{
    // one tick is one second -> one tick is a hecto-nanosecond
    assert(convClockFreq(45, 1, 10_000_000) == 450_000_000);

    // one tick is one microsecond -> one tick is a millisecond
    assert(convClockFreq(9029, 1_000_000, 1_000) == 9);

    // one tick is 1/3_515_654 of a second -> 1/1_001_010 of a second
    assert(convClockFreq(912_319, 3_515_654, 1_001_010) == 259_764);

    // one tick is 1/MonoTime.ticksPerSecond -> one tick is a nanosecond
    // Equivalent to ticksToNSecs
    auto nsecs = convClockFreq(1982, MonoTime.ticksPerSecond, 1_000_000_000);
}

unittest
{
    assert(convClockFreq(99, 43, 57) == 131);
    assert(convClockFreq(131, 57, 43) == 98);
    assert(convClockFreq(1234567890, 10_000_000, 1_000_000_000) == 123456789000);
    assert(convClockFreq(1234567890, 1_000_000_000, 10_000_000) == 12345678);
    assert(convClockFreq(123456789000, 1_000_000_000, 10_000_000) == 1234567890);
    assert(convClockFreq(12345678, 10_000_000, 1_000_000_000) == 1234567800);
    assert(convClockFreq(13131, 3_515_654, 10_000_000) == 37350);
    assert(convClockFreq(37350, 10_000_000, 3_515_654) == 13130);
    assert(convClockFreq(37350, 3_515_654, 10_000_000) == 106239);
    assert(convClockFreq(106239, 10_000_000, 3_515_654) == 37349);

    // It would be too expensive to cover a large range of possible values for
    // ticks, so we use random values in an attempt to get reasonable coverage.
    import core.stdc.stdlib : rand, srand;
    immutable seed = cast(int)time(null);
    srand(seed);
    scope(failure) printf("seed %d\n", seed);
    enum freq1 = 5_527_551L;
    enum freq2 = 10_000_000L;
    enum freq3 = 1_000_000_000L;
    enum freq4 = 98_123_320L;
    immutable freq5 = MonoTime.ticksPerSecond;

    // This makes it so that freq6 is the first multiple of 10 which is greater
    // than or equal to freq5, which at one point was considered for MonoTime's
    // ticksPerSecond rather than using the system's actual clock frequency, so
    // it seemed like a good test case to have.
    import core.stdc.math : floor, log10, pow;
    immutable numDigitsMinus1 = cast(int)floor(log10(freq5));
    auto freq6 = cast(long)pow(10, numDigitsMinus1);
    if (freq5 > freq6)
        freq6 *= 10;

    foreach (_; 0 .. 10_000)
    {
        long[2] values = [rand(), cast(long)rand() * (rand() % 16)];
        foreach (i; values)
        {
            scope(failure) printf("i %s\n", numToStringz(i));
            assertApprox(convClockFreq(convClockFreq(i, freq1, freq2), freq2, freq1), i - 10, i + 10);
            assertApprox(convClockFreq(convClockFreq(i, freq2, freq1), freq1, freq2), i - 10, i + 10);

            assertApprox(convClockFreq(convClockFreq(i, freq3, freq4), freq4, freq3), i - 100, i + 100);
            assertApprox(convClockFreq(convClockFreq(i, freq4, freq3), freq3, freq4), i - 100, i + 100);

            scope(failure) printf("sys %s mt %s\n", numToStringz(freq5), numToStringz(freq6));
            assertApprox(convClockFreq(convClockFreq(i, freq5, freq6), freq6, freq5), i - 10, i + 10);
            assertApprox(convClockFreq(convClockFreq(i, freq6, freq5), freq5, freq6), i - 10, i + 10);

            // This is here rather than in a unittest block immediately after
            // ticksToNSecs in order to avoid code duplication in the unit tests.
            assert(convClockFreq(i, MonoTime.ticksPerSecond, 1_000_000_000) == ticksToNSecs(i));
        }
    }
}


/++
    Convenience wrapper around $(LREF convClockFreq) which converts ticks at
    a clock frequency of $(D MonoTime.ticksPerSecond) to nanoseconds.

    It's primarily of use when $(D MonoTime.ticksPerSecond) is greater than
    hecto-nanosecond resolution, and an application needs a higher precision
    than hecto-nanoceconds.

    See_Also:
        $(LREF convClockFreq)
  +/
long ticksToNSecs(long ticks) @safe pure nothrow @nogc
{
    return convClockFreq(ticks, MonoTime.ticksPerSecond, 1_000_000_000);
}

///
unittest
{
    auto before = MonoTime.currTime;
    // do stuff
    auto after = MonoTime.currTime;
    auto diffInTicks = after.ticks - before.ticks;
    auto diffInNSecs = ticksToNSecs(diffInTicks);
    assert(diffInNSecs == convClockFreq(diffInTicks, MonoTime.ticksPerSecond, 1_000_000_000));
}


/++
    The reverse of $(LREF ticksToNSecs).
  +/
long nsecsToTicks(long ticks) @safe pure nothrow @nogc
{
    return convClockFreq(ticks, 1_000_000_000, MonoTime.ticksPerSecond);
}

unittest
{
    long ticks = 123409832717333;
    auto nsecs = convClockFreq(ticks, MonoTime.ticksPerSecond, 1_000_000_000);
    ticks = convClockFreq(nsecs, 1_000_000_000, MonoTime.ticksPerSecond);
    assert(nsecsToTicks(nsecs) == ticks);
}



/++
   $(RED Warning: TickDuration is deprecated. Please use
          $(LREF MonoTime) for the cases where a monotonic timestamp is needed
          and $(LREF Duration) when a duration is needed, rather than using
          TickDuration.)

   Represents a duration of time in system clock ticks.

   The system clock ticks are the ticks of the system clock at the highest
   precision that the system provides.
  +/
deprecated("TickDuration has been deprecated, please use Duration or MonoTime instead")
struct TickDuration
{
deprecated:
    private static TickDuration TDRvalueOf(TickDuration td)
    {
        return td;
    }
    /++
       The number of ticks that the system clock has in one second.

       If $(D ticksPerSec) is $(D 0), then then $(D TickDuration) failed to
       get the value of $(D ticksPerSec) on the current system, and
       $(D TickDuration) is not going to work. That would be highly abnormal
       though.
      +/
    static immutable long ticksPerSec;


    /++
        The tick of the system clock (as a $(D TickDuration)) when the
        application started.
      +/
    static immutable TickDuration appOrigin;


    static @property @safe pure nothrow @nogc
    {
    /++
        It's the same as $(D TickDuration(0)), but it's provided to be
        consistent with $(D Duration), which provides a $(D zero) property.
      +/
    TickDuration zero() { return TickDuration(0); }

    /++
        Largest $(D TickDuration) possible.
      +/
    TickDuration max() { return TickDuration(long.max); }

    /++
        Most negative $(D TickDuration) possible.
      +/
    TickDuration min() { return TickDuration(long.min); }
    }

    version (CoreUnittest) unittest
    {
        assert((zero == TickDuration(0)) == true);
        assert((TickDuration.max == TickDuration(long.max)) == true);
        assert((TickDuration.min == TickDuration(long.min)) == true);
        assert((TickDuration.min < TickDuration.zero) == true);
        assert((TickDuration.zero < TickDuration.max) == true);
        assert((TickDuration.min < TickDuration.max) == true);
        assert((TickDuration.min - TickDuration(1) == TickDuration.max) == true);
        assert((TickDuration.max + TickDuration(1) == TickDuration.min) == true);
    }


    static pragma(crt_constructor) void time_initializer()
    {
        version (Windows)
        {
            if (QueryPerformanceFrequency(cast(long*)&ticksPerSec) == 0)
                ticksPerSec = 0;
        }
        else version (Darwin)
        {
            ticksPerSec = machTicksPerSecond();
        }
        else version (Posix)
        {
            static if (is(typeof(clock_gettime)))
            {
                timespec ts;

                if (clock_getres(CLOCK_MONOTONIC, &ts) != 0)
                    ticksPerSec = 0;
                else
                {
                    //For some reason, on some systems, clock_getres returns
                    //a resolution which is clearly wrong (it's a millisecond
                    //or worse, but the time is updated much more frequently
                    //than that). In such cases, we'll just use nanosecond
                    //resolution.
                    ticksPerSec = ts.tv_nsec >= 1000 ? 1_000_000_000
                                                     : 1_000_000_000 / ts.tv_nsec;
                }
            }
            else
                ticksPerSec = 1_000_000;
        }
        else
            static assert(0, "Unsupported platform");

        if (ticksPerSec != 0)
            appOrigin = TickDuration.currSystemTick;
    }

    version (CoreUnittest) unittest
    {
        assert(ticksPerSec);
    }


    /++
       The number of system ticks in this $(D TickDuration).

       You can convert this $(D length) into the number of seconds by dividing
       it by $(D ticksPerSec) (or using one the appropriate property function
       to do it).
      +/
    long length;

    /++
        Returns the total number of seconds in this $(D TickDuration).
      +/
    @property long seconds() @safe const pure nothrow @nogc
    {
        return this.to!("seconds", long)();
    }

    version (CoreUnittest) unittest
    {
        foreach (T; AliasSeq!(TickDuration, const TickDuration, immutable TickDuration))
        {
            assert((cast(T)TickDuration(ticksPerSec)).seconds == 1);
            assert((cast(T)TickDuration(ticksPerSec - 1)).seconds == 0);
            assert((cast(T)TickDuration(ticksPerSec * 2)).seconds == 2);
            assert((cast(T)TickDuration(ticksPerSec * 2 - 1)).seconds == 1);
            assert((cast(T)TickDuration(-1)).seconds == 0);
            assert((cast(T)TickDuration(-ticksPerSec - 1)).seconds == -1);
            assert((cast(T)TickDuration(-ticksPerSec)).seconds == -1);
        }
    }


    /++
        Returns the total number of milliseconds in this $(D TickDuration).
      +/
    @property long msecs() @safe const pure nothrow @nogc
    {
        return this.to!("msecs", long)();
    }


    /++
        Returns the total number of microseconds in this $(D TickDuration).
      +/
    @property long usecs() @safe const pure nothrow @nogc
    {
        return this.to!("usecs", long)();
    }


    /++
        Returns the total number of hecto-nanoseconds in this $(D TickDuration).
      +/
    @property long hnsecs() @safe const pure nothrow @nogc
    {
        return this.to!("hnsecs", long)();
    }


    /++
        Returns the total number of nanoseconds in this $(D TickDuration).
      +/
    @property long nsecs() @safe const pure nothrow @nogc
    {
        return this.to!("nsecs", long)();
    }


    /++
        This allows you to construct a $(D TickDuration) from the given time
        units with the given length.

        Params:
            units  = The time units of the $(D TickDuration) (e.g. $(D "msecs")).
            length = The number of units in the $(D TickDuration).
      +/
    static TickDuration from(string units)(long length) @safe pure nothrow @nogc
        if (units == "seconds" ||
           units == "msecs" ||
           units == "usecs" ||
           units == "hnsecs" ||
           units == "nsecs")
    {
        enum unitsPerSec = convert!("seconds", units)(1);

        return TickDuration(cast(long)(length * (ticksPerSec / cast(real)unitsPerSec)));
    }

    version (CoreUnittest) unittest
    {
        foreach (units; AliasSeq!("seconds", "msecs", "usecs", "nsecs"))
        {
            foreach (T; AliasSeq!(TickDuration, const TickDuration, immutable TickDuration))
            {
                assertApprox((cast(T)TickDuration.from!units(1000)).to!(units, long)(),
                             500, 1500, units);
                assertApprox((cast(T)TickDuration.from!units(1_000_000)).to!(units, long)(),
                             900_000, 1_100_000, units);
                assertApprox((cast(T)TickDuration.from!units(2_000_000)).to!(units, long)(),
                             1_900_000, 2_100_000, units);
            }
        }
    }


    /++
        Returns a $(LREF Duration) with the same number of hnsecs as this
        $(D TickDuration).
        Note that the conventional way to convert between $(D TickDuration)
        and $(D Duration) is using $(REF to, std,conv), e.g.:
        $(D tickDuration.to!Duration())
      +/
    Duration opCast(T)() @safe const pure nothrow @nogc
        if (is(immutable T == immutable Duration))
    {
        return Duration(hnsecs);
    }

    version (CoreUnittest) unittest
    {
        foreach (D; AliasSeq!(Duration, const Duration, immutable Duration))
        {
            foreach (T; AliasSeq!(TickDuration, const TickDuration, immutable TickDuration))
            {
                auto expected = dur!"seconds"(1);
                assert(cast(D)cast(T)TickDuration.from!"seconds"(1) == expected);

                foreach (units; AliasSeq!("msecs", "usecs", "hnsecs"))
                {
                    D actual = cast(D)cast(T)TickDuration.from!units(1_000_000);
                    assertApprox(actual, dur!units(900_000), dur!units(1_100_000));
                }
            }
        }
    }


    //Temporary hack until bug http://d.puremagic.com/issues/show_bug.cgi?id=5747 is fixed.
    TickDuration opCast(T)() @safe const pure nothrow @nogc
        if (is(immutable T == immutable TickDuration))
    {
        return this;
    }


    /++
        Adds or subtracts two $(D TickDuration)s as well as assigning the result
        to this $(D TickDuration).

        The legal types of arithmetic for $(D TickDuration) using this operator
        are

        $(TABLE
        $(TR $(TD TickDuration) $(TD +=) $(TD TickDuration) $(TD -->) $(TD TickDuration))
        $(TR $(TD TickDuration) $(TD -=) $(TD TickDuration) $(TD -->) $(TD TickDuration))
        )

        Params:
            rhs = The $(D TickDuration) to add to or subtract from this
                  $(D $(D TickDuration)).
      +/
    ref TickDuration opOpAssign(string op)(TickDuration rhs) @safe pure nothrow @nogc
        if (op == "+" || op == "-")
    {
        mixin("length " ~ op ~ "= rhs.length;");
        return this;
    }

    version (CoreUnittest) unittest
    {
        foreach (T; AliasSeq!(TickDuration, const TickDuration, immutable TickDuration))
        {
            auto a = TickDuration.currSystemTick;
            auto result = a += cast(T)TickDuration.currSystemTick;
            assert((a == result) == true);
            assert(a.to!("seconds", real)() >= 0);

            auto b = TickDuration.currSystemTick;
            result = b -= cast(T)TickDuration.currSystemTick;
            assert((b == result) == true);
            assert(b.to!("seconds", real)() <= 0);

            foreach (U; AliasSeq!(const TickDuration, immutable TickDuration))
            {
                U u = TickDuration(12);
                static assert(!__traits(compiles, u += cast(T)TickDuration.currSystemTick));
                static assert(!__traits(compiles, u -= cast(T)TickDuration.currSystemTick));
            }
        }
    }


    /++
        Adds or subtracts two $(D TickDuration)s.

        The legal types of arithmetic for $(D TickDuration) using this operator
        are

        $(TABLE
        $(TR $(TD TickDuration) $(TD +) $(TD TickDuration) $(TD -->) $(TD TickDuration))
        $(TR $(TD TickDuration) $(TD -) $(TD TickDuration) $(TD -->) $(TD TickDuration))
        )

        Params:
            rhs = The $(D TickDuration) to add to or subtract from this
                  $(D TickDuration).
      +/
    TickDuration opBinary(string op)(TickDuration rhs) @safe const pure nothrow @nogc
        if (op == "+" || op == "-")
    {
        return TickDuration(mixin("length " ~ op ~ " rhs.length"));
    }

    version (CoreUnittest) unittest
    {
        foreach (T; AliasSeq!(TickDuration, const TickDuration, immutable TickDuration))
        {
            T a = TickDuration.currSystemTick;
            T b = TickDuration.currSystemTick;
            assert((a + b).usecs > 0);
            assert((a - b).seconds <= 0);
        }
    }


    /++
        Returns the negation of this $(D TickDuration).
      +/
    TickDuration opUnary(string op)() @safe const pure nothrow @nogc
        if (op == "-")
    {
        return TickDuration(-length);
    }

    version (CoreUnittest) unittest
    {
        foreach (T; AliasSeq!(TickDuration, const TickDuration, immutable TickDuration))
        {
            assert((-(cast(T)TickDuration(7)) == TickDuration(-7)) == true);
            assert((-(cast(T)TickDuration(5)) == TickDuration(-5)) == true);
            assert((-(cast(T)TickDuration(-7)) == TickDuration(7)) == true);
            assert((-(cast(T)TickDuration(-5)) == TickDuration(5)) == true);
            assert((-(cast(T)TickDuration(0)) == TickDuration(0)) == true);
        }
    }


    /++
       operator overloading "<, >, <=, >="
      +/
    int opCmp(TickDuration rhs) @safe const pure nothrow @nogc
    {
        return (length > rhs.length) - (length < rhs.length);
    }

    version (CoreUnittest) unittest
    {
        import core.internal.traits : rvalueOf;
        foreach (T; AliasSeq!(TickDuration, const TickDuration, immutable TickDuration))
        {
            foreach (U; AliasSeq!(TickDuration, const TickDuration, immutable TickDuration))
            {
                T t = TickDuration.currSystemTick;
                U u = t;
                assert((t == u) == true);
                assert((TDRvalueOf(t) == u) == true);
                assert((t == TDRvalueOf(u)) == true);
            }
        }

        foreach (T; AliasSeq!(TickDuration, const TickDuration, immutable TickDuration))
        {
            foreach (U; AliasSeq!(TickDuration, const TickDuration, immutable TickDuration))
            {
                T t = TickDuration.currSystemTick;
                U u = t + t;
                assert((t < u) == true);
                assert((t <= t) == true);
                assert((u > t) == true);
                assert((u >= u) == true);

                assert((TDRvalueOf(t) < u) == true);
                assert((TDRvalueOf(t) <= t) == true);
                assert((TDRvalueOf(u) > t) == true);
                assert((TDRvalueOf(u) >= u) == true);

                assert((t < TDRvalueOf(u)) == true);
                assert((t <= TDRvalueOf(t)) == true);
                assert((u > TDRvalueOf(t)) == true);
                assert((u >= TDRvalueOf(u)) == true);
            }
        }
    }


    /++
        The legal types of arithmetic for $(D TickDuration) using this operator
        overload are

        $(TABLE
        $(TR $(TD TickDuration) $(TD *) $(TD long) $(TD -->) $(TD TickDuration))
        $(TR $(TD TickDuration) $(TD *) $(TD floating point) $(TD -->) $(TD TickDuration))
        )

        Params:
            value = The value to divide from this duration.
      +/
    void opOpAssign(string op, T)(T value) @safe pure nothrow @nogc
        if (op == "*" &&
           (__traits(isIntegral, T) || __traits(isFloating, T)))
    {
        length = cast(long)(length * value);
    }

    version (CoreUnittest) unittest
    {
        immutable curr = TickDuration.currSystemTick;
        TickDuration t1 = curr;
        immutable t2 = curr + curr;
        t1 *= 2;
        assert((t1 == t2) == true);

        t1 = curr;
        t1 *= 2.0;
        immutable tol = TickDuration(cast(long)(_abs(t1.length) * double.epsilon * 2.0));
        assertApprox(t1, t2 - tol, t2 + tol);

        t1 = curr;
        t1 *= 2.1;
        assert((t1 > t2) == true);

        foreach (T; AliasSeq!(const TickDuration, immutable TickDuration))
        {
            T t = TickDuration.currSystemTick;
            assert(!__traits(compiles, t *= 12));
            assert(!__traits(compiles, t *= 12.0));
        }
    }


    /++
        The legal types of arithmetic for $(D TickDuration) using this operator
        overload are

        $(TABLE
        $(TR $(TD TickDuration) $(TD /) $(TD long) $(TD -->) $(TD TickDuration))
        $(TR $(TD TickDuration) $(TD /) $(TD floating point) $(TD -->) $(TD TickDuration))
        )

        Params:
            value = The value to divide from this $(D TickDuration).

        Throws:
            $(D TimeException) if an attempt to divide by $(D 0) is made.
      +/
    void opOpAssign(string op, T)(T value) @safe pure
        if (op == "/" &&
           (__traits(isIntegral, T) || __traits(isFloating, T)))
    {
        if (value == 0)
            throw new TimeException("Attempted division by 0.");

        length = cast(long)(length / value);
    }

    version (CoreUnittest) unittest
    {
        immutable curr = TickDuration.currSystemTick;
        immutable t1 = curr;
        TickDuration t2 = curr + curr;
        t2 /= 2;
        assert((t1 == t2) == true);

        t2 = curr + curr;
        t2 /= 2.0;
        immutable tol = TickDuration(cast(long)(_abs(t2.length) * double.epsilon / 2.0));
        assertApprox(t1, t2 - tol, t2 + tol);

        t2 = curr + curr;
        t2 /= 2.1;
        assert((t1 > t2) == true);

        _assertThrown!TimeException(t2 /= 0);

        foreach (T; AliasSeq!(const TickDuration, immutable TickDuration))
        {
            T t = TickDuration.currSystemTick;
            assert(!__traits(compiles, t /= 12));
            assert(!__traits(compiles, t /= 12.0));
        }
    }


    /++
        The legal types of arithmetic for $(D TickDuration) using this operator
        overload are

        $(TABLE
        $(TR $(TD TickDuration) $(TD *) $(TD long) $(TD -->) $(TD TickDuration))
        $(TR $(TD TickDuration) $(TD *) $(TD floating point) $(TD -->) $(TD TickDuration))
        )

        Params:
            value = The value to divide from this $(D TickDuration).
      +/
    TickDuration opBinary(string op, T)(T value) @safe const pure nothrow @nogc
        if (op == "*" &&
           (__traits(isIntegral, T) || __traits(isFloating, T)))
    {
        return TickDuration(cast(long)(length * value));
    }

    version (CoreUnittest) unittest
    {
        foreach (T; AliasSeq!(TickDuration, const TickDuration, immutable TickDuration))
        {
            T t1 = TickDuration.currSystemTick;
            T t2 = t1 + t1;
            assert((t1 * 2 == t2) == true);
            immutable tol = TickDuration(cast(long)(_abs(t1.length) * double.epsilon * 2.0));
            assertApprox(t1 * 2.0, t2 - tol, t2 + tol);
            assert((t1 * 2.1 > t2) == true);
        }
    }


    /++
        The legal types of arithmetic for $(D TickDuration) using this operator
        overload are

        $(TABLE
        $(TR $(TD TickDuration) $(TD /) $(TD long) $(TD -->) $(TD TickDuration))
        $(TR $(TD TickDuration) $(TD /) $(TD floating point) $(TD -->) $(TD TickDuration))
        )

        Params:
            value = The value to divide from this $(D TickDuration).

        Throws:
            $(D TimeException) if an attempt to divide by $(D 0) is made.
      +/
    TickDuration opBinary(string op, T)(T value) @safe const pure
        if (op == "/" &&
           (__traits(isIntegral, T) || __traits(isFloating, T)))
    {
        if (value == 0)
            throw new TimeException("Attempted division by 0.");

        return TickDuration(cast(long)(length / value));
    }

    version (CoreUnittest) unittest
    {
        foreach (T; AliasSeq!(TickDuration, const TickDuration, immutable TickDuration))
        {
            T t1 = TickDuration.currSystemTick;
            T t2 = t1 + t1;
            assert((t2 / 2 == t1) == true);
            immutable tol = TickDuration(cast(long)(_abs(t2.length) * double.epsilon / 2.0));
            assertApprox(t2 / 2.0, t1 - tol, t1 + tol);
            assert((t2 / 2.1 < t1) == true);

            _assertThrownDep!TimeException(t2 / 0);
        }
    }


    /++
        Params:
            ticks = The number of ticks in the TickDuration.
      +/
    @safe pure nothrow @nogc this(long ticks)
    {
        this.length = ticks;
    }

    version (CoreUnittest) unittest
    {
        foreach (i; [-42, 0, 42])
            assert(TickDuration(i).length == i);
    }


    /++
        The current system tick. The number of ticks per second varies from
        system to system. $(D currSystemTick) uses a monotonic clock, so it's
        intended for precision timing by comparing relative time values, not for
        getting the current system time.

        On Windows, $(D QueryPerformanceCounter) is used. On Mac OS X,
        $(D mach_absolute_time) is used, while on other Posix systems,
        $(D clock_gettime) is used. If $(D mach_absolute_time) or
        $(D clock_gettime) is unavailable, then Posix systems use
        $(D gettimeofday) (the decision is made when $(D TickDuration) is
        compiled), which unfortunately, is not monotonic, but if
        $(D mach_absolute_time) and $(D clock_gettime) aren't available, then
        $(D gettimeofday) is the best that there is.

        $(RED Warning):
            On some systems, the monotonic clock may stop counting when
            the computer goes to sleep or hibernates. So, the monotonic
            clock could be off if that occurs. This is known to happen
            on Mac OS X. It has not been tested whether it occurs on
            either Windows or on Linux.

        Throws:
            $(D TimeException) if it fails to get the time.
      +/
    static @property TickDuration currSystemTick() @trusted nothrow @nogc
    {
        import core.internal.abort : abort;
        version (Windows)
        {
            ulong ticks = void;
            QueryPerformanceCounter(cast(long*)&ticks);
            return TickDuration(ticks);
        }
        else version (Darwin)
        {
            static if (is(typeof(mach_absolute_time)))
                return TickDuration(cast(long)mach_absolute_time());
            else
            {
                timeval tv = void;
                gettimeofday(&tv, null);
                return TickDuration(tv.tv_sec * TickDuration.ticksPerSec +
                                    tv.tv_usec * TickDuration.ticksPerSec / 1000 / 1000);
            }
        }
        else version (Posix)
        {
            static if (is(typeof(clock_gettime)))
            {
                timespec ts = void;
                immutable error = clock_gettime(CLOCK_MONOTONIC, &ts);
                // CLOCK_MONOTONIC is supported and if tv_sec is long or larger
                // overflow won't happen before 292 billion years A.D.
                static if (ts.tv_sec.max < long.max)
                {
                    if (error)
                    {
                        import core.internal.abort : abort;
                        abort("Call to clock_gettime failed.");
                    }
                }
                return TickDuration(ts.tv_sec * TickDuration.ticksPerSec +
                                    ts.tv_nsec * TickDuration.ticksPerSec / 1000 / 1000 / 1000);
            }
            else
            {
                timeval tv = void;
                gettimeofday(&tv, null);
                return TickDuration(tv.tv_sec * TickDuration.ticksPerSec +
                                    tv.tv_usec * TickDuration.ticksPerSec / 1000 / 1000);
            }
        }
    }

    version (CoreUnittest) @safe nothrow unittest
    {
        assert(TickDuration.currSystemTick.length > 0);
    }
}

/++
    Generic way of converting between two time units. Conversions to smaller
    units use truncating division. Years and months can be converted to each
    other, small units can be converted to each other, but years and months
    cannot be converted to or from smaller units (due to the varying number
    of days in a month or year).

    Params:
        from  = The units of time to convert from.
        to    = The units of time to convert to.
        value = The value to convert.
  +/
long convert(string from, string to)(long value) @safe pure nothrow @nogc
    if (((from == "weeks" ||
         from == "days" ||
         from == "hours" ||
         from == "minutes" ||
         from == "seconds" ||
         from == "msecs" ||
         from == "usecs" ||
         from == "hnsecs" ||
         from == "nsecs") &&
        (to == "weeks" ||
         to == "days" ||
         to == "hours" ||
         to == "minutes" ||
         to == "seconds" ||
         to == "msecs" ||
         to == "usecs" ||
         to == "hnsecs" ||
         to == "nsecs")) ||
       ((from == "years" || from == "months") && (to == "years" || to == "months")))
{
    static if (from == "years")
    {
        static if (to == "years")
            return value;
        else static if (to == "months")
            return value * 12;
        else
            static assert(0, "A generic month or year cannot be converted to or from smaller units.");
    }
    else static if (from == "months")
    {
        static if (to == "years")
            return value / 12;
        else static if (to == "months")
            return value;
        else
            static assert(0, "A generic month or year cannot be converted to or from smaller units.");
    }
    else static if (from == "nsecs" && to == "nsecs")
        return value;
    else static if (from == "nsecs")
        return convert!("hnsecs", to)(value / 100);
    else static if (to == "nsecs")
        return convert!(from, "hnsecs")(value) * 100;
    else
        return (hnsecsPer!from * value) / hnsecsPer!to;
}

///
unittest
{
    assert(convert!("years", "months")(1) == 12);
    assert(convert!("months", "years")(12) == 1);

    assert(convert!("weeks", "days")(1) == 7);
    assert(convert!("hours", "seconds")(1) == 3600);
    assert(convert!("seconds", "days")(1) == 0);
    assert(convert!("seconds", "days")(86_400) == 1);

    assert(convert!("nsecs", "nsecs")(1) == 1);
    assert(convert!("nsecs", "hnsecs")(1) == 0);
    assert(convert!("hnsecs", "nsecs")(1) == 100);
    assert(convert!("nsecs", "seconds")(1) == 0);
    assert(convert!("seconds", "nsecs")(1) == 1_000_000_000);
}

unittest
{
    foreach (units; AliasSeq!("weeks", "days", "hours", "seconds", "msecs", "usecs", "hnsecs", "nsecs"))
    {
        static assert(!__traits(compiles, convert!("years", units)(12)), units);
        static assert(!__traits(compiles, convert!(units, "years")(12)), units);
    }

    foreach (units; AliasSeq!("years", "months", "weeks", "days",
                               "hours", "seconds", "msecs", "usecs", "hnsecs", "nsecs"))
    {
        assert(convert!(units, units)(12) == 12);
    }

    assert(convert!("weeks", "hnsecs")(1) == 6_048_000_000_000L);
    assert(convert!("days", "hnsecs")(1) == 864_000_000_000L);
    assert(convert!("hours", "hnsecs")(1) == 36_000_000_000L);
    assert(convert!("minutes", "hnsecs")(1) == 600_000_000L);
    assert(convert!("seconds", "hnsecs")(1) == 10_000_000L);
    assert(convert!("msecs", "hnsecs")(1) == 10_000);
    assert(convert!("usecs", "hnsecs")(1) == 10);

    assert(convert!("hnsecs", "weeks")(6_048_000_000_000L) == 1);
    assert(convert!("hnsecs", "days")(864_000_000_000L) == 1);
    assert(convert!("hnsecs", "hours")(36_000_000_000L) == 1);
    assert(convert!("hnsecs", "minutes")(600_000_000L) == 1);
    assert(convert!("hnsecs", "seconds")(10_000_000L) == 1);
    assert(convert!("hnsecs", "msecs")(10_000) == 1);
    assert(convert!("hnsecs", "usecs")(10) == 1);

    assert(convert!("weeks", "days")(1) == 7);
    assert(convert!("days", "weeks")(7) == 1);

    assert(convert!("days", "hours")(1) == 24);
    assert(convert!("hours", "days")(24) == 1);

    assert(convert!("hours", "minutes")(1) == 60);
    assert(convert!("minutes", "hours")(60) == 1);

    assert(convert!("minutes", "seconds")(1) == 60);
    assert(convert!("seconds", "minutes")(60) == 1);

    assert(convert!("seconds", "msecs")(1) == 1000);
    assert(convert!("msecs", "seconds")(1000) == 1);

    assert(convert!("msecs", "usecs")(1) == 1000);
    assert(convert!("usecs", "msecs")(1000) == 1);

    assert(convert!("usecs", "hnsecs")(1) == 10);
    assert(convert!("hnsecs", "usecs")(10) == 1);

    assert(convert!("weeks", "nsecs")(1) == 604_800_000_000_000L);
    assert(convert!("days", "nsecs")(1) == 86_400_000_000_000L);
    assert(convert!("hours", "nsecs")(1) == 3_600_000_000_000L);
    assert(convert!("minutes", "nsecs")(1) == 60_000_000_000L);
    assert(convert!("seconds", "nsecs")(1) == 1_000_000_000L);
    assert(convert!("msecs", "nsecs")(1) == 1_000_000);
    assert(convert!("usecs", "nsecs")(1) == 1000);
    assert(convert!("hnsecs", "nsecs")(1) == 100);

    assert(convert!("nsecs", "weeks")(604_800_000_000_000L) == 1);
    assert(convert!("nsecs", "days")(86_400_000_000_000L) == 1);
    assert(convert!("nsecs", "hours")(3_600_000_000_000L) == 1);
    assert(convert!("nsecs", "minutes")(60_000_000_000L) == 1);
    assert(convert!("nsecs", "seconds")(1_000_000_000L) == 1);
    assert(convert!("nsecs", "msecs")(1_000_000) == 1);
    assert(convert!("nsecs", "usecs")(1000) == 1);
    assert(convert!("nsecs", "hnsecs")(100) == 1);
}

/++
    Exception type used by core.time.
  +/
class TimeException : Exception
{
    /++
        Params:
            msg  = The message for the exception.
            file = The file where the exception occurred.
            line = The line number where the exception occurred.
            next = The previous exception in the chain of exceptions, if any.
      +/
    this(string msg, string file = __FILE__, size_t line = __LINE__, Throwable next = null) @safe pure nothrow
    {
        super(msg, file, line, next);
    }

    /++
        Params:
            msg  = The message for the exception.
            next = The previous exception in the chain of exceptions.
            file = The file where the exception occurred.
            line = The line number where the exception occurred.
      +/
    this(string msg, Throwable next, string file = __FILE__, size_t line = __LINE__) @safe pure nothrow
    {
        super(msg, file, line, next);
    }
}

unittest
{
    {
        auto e = new TimeException("hello");
        assert(e.msg == "hello");
        assert(e.file == __FILE__);
        assert(e.line == __LINE__ - 3);
        assert(e.next is null);
    }

    {
        auto next = new Exception("foo");
        auto e = new TimeException("goodbye", next);
        assert(e.msg == "goodbye");
        assert(e.file == __FILE__);
        assert(e.line == __LINE__ - 3);
        assert(e.next is next);
    }
}



/++
    Returns the absolute value of a duration.
  +/
Duration abs(Duration duration) @safe pure nothrow @nogc
{
    return Duration(_abs(duration._hnsecs));
}

/++ Ditto +/
deprecated("TickDuration has been deprecated, please use Duration or MonoTime instead")
TickDuration abs(TickDuration duration) @safe pure nothrow @nogc
{
    return TickDuration(_abs(duration.length));
}

unittest
{
    assert(abs(dur!"msecs"(5)) == dur!"msecs"(5));
    assert(abs(dur!"msecs"(-5)) == dur!"msecs"(5));
}

deprecated unittest
{
    assert((abs(TickDuration(17)) == TickDuration(17)) == true);
    assert((abs(TickDuration(-17)) == TickDuration(17)) == true);
}


//==============================================================================
// Private Section.
//
// Much of this is a copy or simplified copy of what's in std.datetime.
//==============================================================================
private:


/+
    Template to help with converting between time units.
 +/
template hnsecsPer(string units)
    if (units == "weeks" ||
       units == "days" ||
       units == "hours" ||
       units == "minutes" ||
       units == "seconds" ||
       units == "msecs" ||
       units == "usecs" ||
       units == "hnsecs")
{
    static if (units == "hnsecs")
        enum hnsecsPer = 1L;
    else static if (units == "usecs")
        enum hnsecsPer = 10L;
    else static if (units == "msecs")
        enum hnsecsPer = 1000 * hnsecsPer!"usecs";
    else static if (units == "seconds")
        enum hnsecsPer = 1000 * hnsecsPer!"msecs";
    else static if (units == "minutes")
        enum hnsecsPer = 60 * hnsecsPer!"seconds";
    else static if (units == "hours")
        enum hnsecsPer = 60 * hnsecsPer!"minutes";
    else static if (units == "days")
        enum hnsecsPer = 24 * hnsecsPer!"hours";
    else static if (units == "weeks")
        enum hnsecsPer = 7 * hnsecsPer!"days";
}

/+
    Splits out a particular unit from hnsecs and gives you the value for that
    unit and the remaining hnsecs. It really shouldn't be used unless all units
    larger than the given units have already been split out.

    Params:
        units  = The units to split out.
        hnsecs = The current total hnsecs. Upon returning, it is the hnsecs left
                 after splitting out the given units.

    Returns:
        The number of the given units from converting hnsecs to those units.
  +/
long splitUnitsFromHNSecs(string units)(ref long hnsecs) @safe pure nothrow @nogc
    if (units == "weeks" ||
       units == "days" ||
       units == "hours" ||
       units == "minutes" ||
       units == "seconds" ||
       units == "msecs" ||
       units == "usecs" ||
       units == "hnsecs")
{
    immutable value = convert!("hnsecs", units)(hnsecs);
    hnsecs -= convert!(units, "hnsecs")(value);

    return value;
}

unittest
{
    auto hnsecs = 2595000000007L;
    immutable days = splitUnitsFromHNSecs!"days"(hnsecs);
    assert(days == 3);
    assert(hnsecs == 3000000007);

    immutable minutes = splitUnitsFromHNSecs!"minutes"(hnsecs);
    assert(minutes == 5);
    assert(hnsecs == 7);
}

/+
    Whether all of the given strings are among the accepted strings.
  +/
bool allAreAcceptedUnits(acceptedUnits...)(scope string[] units)
{
    foreach (unit; units)
    {
        bool found = false;
        foreach (acceptedUnit; acceptedUnits)
        {
            if (unit == acceptedUnit)
            {
                found = true;
                break;
            }
        }
        if (!found)
            return false;
    }
    return true;
}

unittest
{
    assert(allAreAcceptedUnits!("hours", "seconds")(["seconds", "hours"]));
    assert(!allAreAcceptedUnits!("hours", "seconds")(["minutes", "hours"]));
    assert(!allAreAcceptedUnits!("hours", "seconds")(["seconds", "minutes"]));
    assert(allAreAcceptedUnits!("days", "hours", "minutes", "seconds", "msecs")(["minutes"]));
    assert(!allAreAcceptedUnits!("days", "hours", "minutes", "seconds", "msecs")(["usecs"]));
    assert(!allAreAcceptedUnits!("days", "hours", "minutes", "seconds", "msecs")(["secs"]));
}


/+
    Whether the given time unit strings are arranged in order from largest to
    smallest.
  +/
bool unitsAreInDescendingOrder(scope string[] units)
{
    if (units.length <= 1)
        return true;

    immutable string[] timeStrings = ["nsecs", "hnsecs", "usecs", "msecs", "seconds",
                                      "minutes", "hours", "days", "weeks", "months", "years"];
    size_t currIndex = 42;
    foreach (i, timeStr; timeStrings)
    {
        if (units[0] == timeStr)
        {
            currIndex = i;
            break;
        }
    }
    assert(currIndex != 42);

    foreach (unit; units[1 .. $])
    {
        size_t nextIndex = 42;
        foreach (i, timeStr; timeStrings)
        {
            if (unit == timeStr)
            {
                nextIndex = i;
                break;
            }
        }
        assert(nextIndex != 42);

        if (currIndex <= nextIndex)
            return false;
        currIndex = nextIndex;
    }
    return true;
}

unittest
{
    assert(unitsAreInDescendingOrder(["years", "months", "weeks", "days", "hours", "minutes",
                                     "seconds", "msecs", "usecs", "hnsecs", "nsecs"]));
    assert(unitsAreInDescendingOrder(["weeks", "hours", "msecs"]));
    assert(unitsAreInDescendingOrder(["days", "hours", "minutes"]));
    assert(unitsAreInDescendingOrder(["hnsecs"]));
    assert(!unitsAreInDescendingOrder(["days", "hours", "hours"]));
    assert(!unitsAreInDescendingOrder(["days", "hours", "days"]));
}

version (Darwin)
long machTicksPerSecond() @nogc nothrow
{
    // Be optimistic that ticksPerSecond (1e9*denom/numer) is integral. So far
    // so good on Darwin based platforms OS X, iOS.
    import core.internal.abort : abort;
    mach_timebase_info_data_t info;
    if (mach_timebase_info(&info) != 0)
        abort("Failed in mach_timebase_info().");

    long scaledDenom = 1_000_000_000L * info.denom;
    if (scaledDenom % info.numer != 0)
        abort("Non integral ticksPerSecond from mach_timebase_info.");
    return scaledDenom / info.numer;
}

/+
    Local version of abs, since std.math.abs is in Phobos, not druntime.
  +/
long _abs(long val) @safe pure nothrow @nogc
{
    return val >= 0 ? val : -val;
}

double _abs(double val) @safe pure nothrow @nogc
{
    return val >= 0.0 ? val : -val;
}


version (CoreUnittest)
string doubleToString(double value) @safe pure nothrow
{
    string result;
    if (value < 0 && cast(long)value == 0)
        result = "-0";
    else
        result = signedToTempString(cast(long)value).idup;
    result ~= '.';
    result ~= unsignedToTempString(cast(ulong)(_abs((value - cast(long)value) * 1_000_000) + .5));

    while (result[$-1] == '0')
        result = result[0 .. $-1];
    return result;
}

unittest
{
    auto a = 1.337;
    auto aStr = doubleToString(a);
    assert(aStr == "1.337", aStr);

    a = 0.337;
    aStr = doubleToString(a);
    assert(aStr == "0.337", aStr);

    a = -0.337;
    aStr = doubleToString(a);
    assert(aStr == "-0.337", aStr);
}

version (CoreUnittest) const(char)* numToStringz()(long value) @trusted pure nothrow
{
    return (signedToTempString(value) ~ "\0").ptr;
}


/+
    dmd @@@BUG18223@@@
    A selective import of `AliasSeq` happens to bleed through and causes symbol clashes downstream.
 +/
version (none)
    import core.internal.traits : AliasSeq;
else
    import core.internal.traits;


/+ An adjusted copy of std.exception.assertThrown. +/
version (CoreUnittest) void _assertThrown(T : Throwable = Exception, E)
                                    (lazy E expression,
                                     string msg = null,
                                     string file = __FILE__,
                                     size_t line = __LINE__)
{
    bool thrown = false;

    try
        expression();
    catch (T t)
        thrown = true;

    if (!thrown)
    {
        immutable tail = msg.length == 0 ? "." : ": " ~ msg;

        throw new AssertError("assertThrown() failed: No " ~ T.stringof ~ " was thrown" ~ tail, file, line);
    }
}

unittest
{

    void throwEx(Throwable t)
    {
        throw t;
    }

    void nothrowEx()
    {}

    try
        _assertThrown!Exception(throwEx(new Exception("It's an Exception")));
    catch (AssertError)
        assert(0);

    try
        _assertThrown!Exception(throwEx(new Exception("It's an Exception")), "It's a message");
    catch (AssertError)
        assert(0);

    try
        _assertThrown!AssertError(throwEx(new AssertError("It's an AssertError", __FILE__, __LINE__)));
    catch (AssertError)
        assert(0);

    try
        _assertThrown!AssertError(throwEx(new AssertError("It's an AssertError", __FILE__, __LINE__)), "It's a message");
    catch (AssertError)
        assert(0);


    {
        bool thrown = false;
        try
            _assertThrown!Exception(nothrowEx());
        catch (AssertError)
            thrown = true;

        assert(thrown);
    }

    {
        bool thrown = false;
        try
            _assertThrown!Exception(nothrowEx(), "It's a message");
        catch (AssertError)
            thrown = true;

        assert(thrown);
    }

    {
        bool thrown = false;
        try
            _assertThrown!AssertError(nothrowEx());
        catch (AssertError)
            thrown = true;

        assert(thrown);
    }

    {
        bool thrown = false;
        try
            _assertThrown!AssertError(nothrowEx(), "It's a message");
        catch (AssertError)
            thrown = true;

        assert(thrown);
    }
}

version (CoreUnittest) deprecated void _assertThrownDep(T : Throwable = Exception, E)
                                    (lazy E expression,
                                     string msg = null,
                                     string file = __FILE__,
                                     size_t line = __LINE__)
{
    bool thrown = false;

    try
        expression();
    catch (T t)
        thrown = true;

    if (!thrown)
    {
        immutable tail = msg.length == 0 ? "." : ": " ~ msg;

        throw new AssertError("assertThrown() failed: No " ~ T.stringof ~ " was thrown" ~ tail, file, line);
    }
}



version (CoreUnittest) void assertApprox(D, E)(D actual,
                                          E lower,
                                          E upper,
                                          string msg = "unittest failure",
                                          size_t line = __LINE__)
    if (is(D : const Duration) && is(E : const Duration))
{
    if (actual < lower)
        throw new AssertError(msg ~ ": lower: " ~ actual.toString(), __FILE__, line);
    if (actual > upper)
        throw new AssertError(msg ~ ": upper: " ~ actual.toString(), __FILE__, line);
}

version (CoreUnittest) deprecated void assertApprox(D, E)(D actual,
                                          E lower,
                                          E upper,
                                          string msg = "unittest failure",
                                          size_t line = __LINE__)
    if (is(D : const TickDuration) && is(E : const TickDuration))
{
    if (actual.length < lower.length || actual.length > upper.length)
    {
        throw new AssertError(msg ~ (": [" ~ signedToTempString(lower.length) ~ "] [" ~
                              signedToTempString(actual.length) ~ "] [" ~
                              signedToTempString(upper.length) ~ "]").idup,
                              __FILE__, line);
    }
}

version (CoreUnittest) void assertApprox(MT)(MT actual,
                                        MT lower,
                                        MT upper,
                                        string msg = "unittest failure",
                                        size_t line = __LINE__)
    if (is(MT == MonoTimeImpl!type, ClockType type))
{
    assertApprox(actual._ticks, lower._ticks, upper._ticks, msg, line);
}

version (CoreUnittest) void assertApprox()(long actual,
                                      long lower,
                                      long upper,
                                      string msg = "unittest failure",
                                      size_t line = __LINE__)
{
    if (actual < lower)
        throw new AssertError(msg ~ ": lower: " ~ signedToTempString(actual).idup, __FILE__, line);
    if (actual > upper)
        throw new AssertError(msg ~ ": upper: " ~ signedToTempString(actual).idup, __FILE__, line);
}

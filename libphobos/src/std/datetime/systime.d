// Written in the D programming language

/++

$(DIVC quickindex,
$(BOOKTABLE,
$(TR $(TH Category) $(TH Functions))
$(TR $(TD Types) $(TD
    $(LREF Clock)
    $(LREF SysTime)
    $(LREF DosFileTime)
))
$(TR $(TD Conversion) $(TD
    $(LREF parseRFC822DateTime)
    $(LREF DosFileTimeToSysTime)
    $(LREF FILETIMEToStdTime)
    $(LREF FILETIMEToSysTime)
    $(LREF stdTimeToFILETIME)
    $(LREF stdTimeToUnixTime)
    $(LREF SYSTEMTIMEToSysTime)
    $(LREF SysTimeToDosFileTime)
    $(LREF SysTimeToFILETIME)
    $(LREF SysTimeToSYSTEMTIME)
    $(LREF unixTimeToStdTime)
))
))

    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTP jmdavisprog.com, Jonathan M Davis)
    Source:    $(PHOBOSSRC std/datetime/systime.d)
+/
module std.datetime.systime;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

/// Get the current time as a $(LREF SysTime)
@safe unittest
{
    import std.datetime.timezone : LocalTime;
    SysTime today = Clock.currTime();
    assert(today.timezone is LocalTime());
}

/// Construct a $(LREF SysTime) from a ISO time string
@safe unittest
{
    import std.datetime.date : DateTime;
    import std.datetime.timezone : UTC;

    auto st = SysTime.fromISOExtString("2018-01-01T10:30:00Z");
    assert(st == SysTime(DateTime(2018, 1, 1, 10, 30, 0), UTC()));
}

/// Make a specific point in time in the New York timezone
@safe unittest
{
    import core.time : hours;
    import std.datetime.date : DateTime;
    import std.datetime.timezone : SimpleTimeZone;

    auto ny = SysTime(
        DateTime(2018, 1, 1, 10, 30, 0),
        new immutable SimpleTimeZone(-5.hours, "America/New_York")
    );

    // ISO standard time strings
    assert(ny.toISOString() == "20180101T103000-05:00");
    assert(ny.toISOExtString() == "2018-01-01T10:30:00-05:00");
}

// Note: reconsider using specific imports below after
// https://issues.dlang.org/show_bug.cgi?id=17630 has been fixed
import core.time;// : ClockType, convert, dur, Duration, seconds, TimeException;
import std.datetime.date;// : _monthNames, AllowDayOverflow, CmpTimeUnits, Date,
    //DateTime, DateTimeException, DayOfWeek, enforceValid, getDayOfWeek, maxDay,
    //Month, splitUnitsFromHNSecs, TimeOfDay, validTimeUnits, yearIsLeapYear;
import std.datetime.timezone;// : LocalTime, SimpleTimeZone, TimeZone, UTC;
import std.exception : enforce;
import std.format : format;
import std.range.primitives;
import std.traits : isIntegral, isSigned, isSomeString, isNarrowString;

version (Windows)
{
    import core.stdc.time : time_t;
    import core.sys.windows.winbase;
    import core.sys.windows.winnt;
    import core.sys.windows.winsock2;
}
else version (Posix)
{
    import core.sys.posix.signal : timespec;
    import core.sys.posix.sys.types : time_t;
}

version (StdUnittest)
{
    import core.exception : AssertError;
    import std.exception : assertThrown;
}


@safe unittest
{
    initializeTests();
}

version (unittest) private bool clockSupported(ClockType c)
{
    // Skip unsupported clocks on older linux kernels, assume that only
    // CLOCK_MONOTONIC and CLOCK_REALTIME exist, as that is the lowest
    // common denominator supported by all versions of Linux pre-2.6.12.
    version (Linux_Pre_2639)
        return c == ClockType.normal || c == ClockType.precise;
    else
        return true;
}

/++
    Effectively a namespace to make it clear that the methods it contains are
    getting the time from the system clock. It cannot be instantiated.
 +/
final class Clock
{
public:

    /++
        Returns the current time in the given time zone.

        Params:
            clockType = The $(REF ClockType, core,time) indicates which system
                        clock to use to get the current time. Very few programs
                        need to use anything other than the default.
            tz = The time zone for the SysTime that's returned.

        Throws:
            $(REF DateTimeException,std,datetime,date) if it fails to get the
            time.
      +/
    static SysTime currTime(ClockType clockType = ClockType.normal)(immutable TimeZone tz = LocalTime()) @safe
    {
        return SysTime(currStdTime!clockType, tz);
    }

    @safe unittest
    {
        import std.format : format;
        import core.time;
        assert(currTime().timezone is LocalTime());
        assert(currTime(UTC()).timezone is UTC());

        // core.stdc.time.time does not always use unix time on Windows systems.
        // In particular, dmc does not use unix time. If we can guarantee that
        // the MS runtime uses unix time, then we may be able run this test
        // then, but for now, we're just not going to run this test on Windows.
        version (Posix)
        {
            static import core.stdc.time;
            static import std.math;
            immutable unixTimeD = currTime().toUnixTime();
            immutable unixTimeC = core.stdc.time.time(null);
            assert(std.math.abs(unixTimeC - unixTimeD) <= 2);
        }

        auto norm1 = Clock.currTime;
        auto norm2 = Clock.currTime(UTC());
        assert(norm1 <= norm2, format("%s %s", norm1, norm2));
        assert(abs(norm1 - norm2) <= seconds(2));

        import std.meta : AliasSeq;
        static foreach (ct; AliasSeq!(ClockType.coarse, ClockType.precise, ClockType.second))
        {{
            static if (clockSupported(ct))
            {
                auto value1 = Clock.currTime!ct;
                auto value2 = Clock.currTime!ct(UTC());
                assert(value1 <= value2, format("%s %s (ClockType: %s)", value1, value2, ct));
                assert(abs(value1 - value2) <= seconds(2), format("ClockType.%s", ct));
            }
        }}
    }


    /++
        Returns the number of hnsecs since midnight, January 1st, 1 A.D. for the
        current time.

        Params:
            clockType = The $(REF ClockType, core,time) indicates which system
                        clock to use to get the current time. Very few programs
                        need to use anything other than the default.

        Throws:
            $(REF DateTimeException,std,datetime,date) if it fails to get the
            time.
      +/
    static @property long currStdTime(ClockType clockType = ClockType.normal)() @trusted
    {
        static if (clockType != ClockType.coarse &&
                   clockType != ClockType.normal &&
                   clockType != ClockType.precise &&
                   clockType != ClockType.second)
        {
            static assert(0, format("ClockType.%s is not supported by Clock.currTime or Clock.currStdTime", clockType));
        }

        version (Windows)
        {
            FILETIME fileTime;
            GetSystemTimeAsFileTime(&fileTime);
            immutable result = FILETIMEToStdTime(&fileTime);
            static if (clockType == ClockType.second)
            {
                // Ideally, this would use core.std.time.time, but the C runtime
                // has to be using unix time for that to work, and that's not
                // guaranteed on Windows. Digital Mars does not use unix time.
                // MS may or may not. If it does, then this can be made to use
                // core.stdc.time for MS, but for now, we'll leave it like this.
                return convert!("seconds", "hnsecs")(convert!("hnsecs", "seconds")(result));
            }
            else
                return result;
        }
        else version (Posix)
        {
            static import core.stdc.time;
            enum hnsecsToUnixEpoch = unixTimeToStdTime(0);

            version (Darwin)
            {
                static if (clockType == ClockType.second)
                    return unixTimeToStdTime(core.stdc.time.time(null));
                else
                {
                    import core.sys.posix.sys.time : gettimeofday, timeval;
                    timeval tv = void;
                    // Posix gettimeofday called with a valid timeval address
                    // and a null second parameter doesn't fail.
                    gettimeofday(&tv, null);
                    return convert!("seconds", "hnsecs")(tv.tv_sec) +
                           tv.tv_usec * 10 +
                           hnsecsToUnixEpoch;
                }
            }
            else version (linux)
            {
                static if (clockType == ClockType.second)
                    return unixTimeToStdTime(core.stdc.time.time(null));
                else
                {
                    import core.sys.linux.time : CLOCK_REALTIME_COARSE;
                    import core.sys.posix.time : clock_gettime, CLOCK_REALTIME;
                    static if (clockType == ClockType.coarse)       alias clockArg = CLOCK_REALTIME_COARSE;
                    else static if (clockType == ClockType.normal)  alias clockArg = CLOCK_REALTIME;
                    else static if (clockType == ClockType.precise) alias clockArg = CLOCK_REALTIME;
                    else static assert(0, "Previous static if is wrong.");
                    timespec ts = void;
                    immutable error = clock_gettime(clockArg, &ts);
                    // Posix clock_gettime called with a valid address and valid clock_id is only
                    // permitted to fail if the number of seconds does not fit in time_t. If tv_sec
                    // is long or larger overflow won't happen before 292 billion years A.D.
                    static if (ts.tv_sec.max < long.max)
                    {
                        if (error)
                            throw new TimeException("Call to clock_gettime() failed");
                    }
                    return convert!("seconds", "hnsecs")(ts.tv_sec) +
                           ts.tv_nsec / 100 +
                           hnsecsToUnixEpoch;
                }
            }
            else version (FreeBSD)
            {
                import core.sys.freebsd.time : clock_gettime, CLOCK_REALTIME,
                    CLOCK_REALTIME_FAST, CLOCK_REALTIME_PRECISE, CLOCK_SECOND;
                static if (clockType == ClockType.coarse)       alias clockArg = CLOCK_REALTIME_FAST;
                else static if (clockType == ClockType.normal)  alias clockArg = CLOCK_REALTIME;
                else static if (clockType == ClockType.precise) alias clockArg = CLOCK_REALTIME_PRECISE;
                else static if (clockType == ClockType.second)  alias clockArg = CLOCK_SECOND;
                else static assert(0, "Previous static if is wrong.");
                timespec ts = void;
                immutable error = clock_gettime(clockArg, &ts);
                // Posix clock_gettime called with a valid address and valid clock_id is only
                // permitted to fail if the number of seconds does not fit in time_t. If tv_sec
                // is long or larger overflow won't happen before 292 billion years A.D.
                static if (ts.tv_sec.max < long.max)
                {
                    if (error)
                        throw new TimeException("Call to clock_gettime() failed");
                }
                return convert!("seconds", "hnsecs")(ts.tv_sec) +
                       ts.tv_nsec / 100 +
                       hnsecsToUnixEpoch;
            }
            else version (NetBSD)
            {
                static if (clockType == ClockType.second)
                    return unixTimeToStdTime(core.stdc.time.time(null));
                else
                {
                    import core.sys.netbsd.time : clock_gettime, CLOCK_REALTIME;
                    timespec ts = void;
                    immutable error = clock_gettime(CLOCK_REALTIME, &ts);
                    // Posix clock_gettime called with a valid address and valid clock_id is only
                    // permitted to fail if the number of seconds does not fit in time_t. If tv_sec
                    // is long or larger overflow won't happen before 292 billion years A.D.
                    static if (ts.tv_sec.max < long.max)
                    {
                        if (error)
                            throw new TimeException("Call to clock_gettime() failed");
                    }
                    return convert!("seconds", "hnsecs")(ts.tv_sec) +
                           ts.tv_nsec / 100 +
                           hnsecsToUnixEpoch;
                }
            }
            else version (OpenBSD)
            {
                static if (clockType == ClockType.second)
                    return unixTimeToStdTime(core.stdc.time.time(null));
                else
                {
                    import core.sys.openbsd.time : clock_gettime, CLOCK_REALTIME;
                    static if (clockType == ClockType.coarse)       alias clockArg = CLOCK_REALTIME;
                    else static if (clockType == ClockType.normal)  alias clockArg = CLOCK_REALTIME;
                    else static if (clockType == ClockType.precise) alias clockArg = CLOCK_REALTIME;
                    else static assert(0, "Previous static if is wrong.");
                    timespec ts;
                    if (clock_gettime(clockArg, &ts) != 0)
                        throw new TimeException("Call to clock_gettime() failed");
                    return convert!("seconds", "hnsecs")(ts.tv_sec) +
                           ts.tv_nsec / 100 +
                           hnsecsToUnixEpoch;
                }
            }
            else version (DragonFlyBSD)
            {
                import core.sys.dragonflybsd.time : clock_gettime, CLOCK_REALTIME,
                    CLOCK_REALTIME_FAST, CLOCK_REALTIME_PRECISE, CLOCK_SECOND;
                static if (clockType == ClockType.coarse)       alias clockArg = CLOCK_REALTIME_FAST;
                else static if (clockType == ClockType.normal)  alias clockArg = CLOCK_REALTIME;
                else static if (clockType == ClockType.precise) alias clockArg = CLOCK_REALTIME_PRECISE;
                else static if (clockType == ClockType.second)  alias clockArg = CLOCK_SECOND;
                else static assert(0, "Previous static if is wrong.");
                timespec ts = void;
                immutable error = clock_gettime(clockArg, &ts);
                // Posix clock_gettime called with a valid address and valid clock_id is only
                // permitted to fail if the number of seconds does not fit in time_t. If tv_sec
                // is long or larger overflow won't happen before 292 billion years A.D.
                static if (ts.tv_sec.max < long.max)
                {
                    if (error)
                        throw new TimeException("Call to clock_gettime() failed");
                }
                return convert!("seconds", "hnsecs")(ts.tv_sec) +
                       ts.tv_nsec / 100 +
                       hnsecsToUnixEpoch;
            }
            else version (Solaris)
            {
                static if (clockType == ClockType.second)
                    return unixTimeToStdTime(core.stdc.time.time(null));
                else
                {
                    import core.sys.solaris.time : clock_gettime, CLOCK_REALTIME;
                    static if (clockType == ClockType.coarse)       alias clockArg = CLOCK_REALTIME;
                    else static if (clockType == ClockType.normal)  alias clockArg = CLOCK_REALTIME;
                    else static if (clockType == ClockType.precise) alias clockArg = CLOCK_REALTIME;
                    else static assert(0, "Previous static if is wrong.");
                    timespec ts = void;
                    immutable error = clock_gettime(clockArg, &ts);
                    // Posix clock_gettime called with a valid address and valid clock_id is only
                    // permitted to fail if the number of seconds does not fit in time_t. If tv_sec
                    // is long or larger overflow won't happen before 292 billion years A.D.
                    static if (ts.tv_sec.max < long.max)
                    {
                        if (error)
                            throw new TimeException("Call to clock_gettime() failed");
                    }
                    return convert!("seconds", "hnsecs")(ts.tv_sec) +
                           ts.tv_nsec / 100 +
                           hnsecsToUnixEpoch;
                }
            }
            else version (Hurd)
            {
                static if (clockType == ClockType.second)
                    return unixTimeToStdTime(core.stdc.time.time(null));
                else
                {
                    import core.sys.hurd.time : CLOCK_REALTIME_COARSE;
                    import core.sys.posix.time : clock_gettime, CLOCK_REALTIME;
                    static if (clockType == ClockType.coarse)       alias clockArg = CLOCK_REALTIME_COARSE;
                    else static if (clockType == ClockType.normal)  alias clockArg = CLOCK_REALTIME;
                    else static if (clockType == ClockType.precise) alias clockArg = CLOCK_REALTIME;
                    else static assert(0, "Previous static if is wrong.");
                    timespec ts = void;
                    immutable error = clock_gettime(clockArg, &ts);
                    // Posix clock_gettime called with a valid address and valid clock_id is only
                    // permitted to fail if the number of seconds does not fit in time_t. If tv_sec
                    // is long or larger overflow won't happen before 292 billion years A.D.
                    static if (ts.tv_sec.max < long.max)
                    {
                        if (error)
                            throw new TimeException("Call to clock_gettime() failed");
                    }
                    return convert!("seconds", "hnsecs")(ts.tv_sec) +
                           ts.tv_nsec / 100 +
                           hnsecsToUnixEpoch;
                }
            }
            else static assert(0, "Unsupported OS");
        }
        else static assert(0, "Unsupported OS");
    }

    @safe unittest
    {
        import std.format : format;
        import std.math.algebraic : abs;
        import std.meta : AliasSeq;
        enum limit = convert!("seconds", "hnsecs")(2);

        auto norm1 = Clock.currStdTime;
        auto norm2 = Clock.currStdTime;
        assert(norm1 <= norm2, format("%s %s", norm1, norm2));
        assert(abs(norm1 - norm2) <= limit);

        static foreach (ct; AliasSeq!(ClockType.coarse, ClockType.precise, ClockType.second))
        {{
            static if (clockSupported(ct))
            {
                auto value1 = Clock.currStdTime!ct;
                auto value2 = Clock.currStdTime!ct;
                assert(value1 <= value2, format("%s %s (ClockType: %s)", value1, value2, ct));
                assert(abs(value1 - value2) <= limit);
            }
        }}
    }


private:

    @disable this();
}

/// Get the current time as a $(LREF SysTime)
@safe unittest
{
    import std.datetime.timezone : LocalTime;
    SysTime today = Clock.currTime();
    assert(today.timezone is LocalTime());
}


/++
    `SysTime` is the type used to get the current time from the
    system or doing anything that involves time zones. Unlike
    $(REF DateTime,std,datetime,date), the time zone is an integral part of
    `SysTime` (though for local time applications, time zones can be ignored
    and it will work, since it defaults to using the local time zone). It holds
    its internal time in std time (hnsecs since midnight, January 1st, 1 A.D.
    UTC), so it interfaces well with the system time.

    An $(I hnsec) (hecto-nanosecond) is 100 nanoseconds. There are 10,000,000 hnsecs in a second.

$(PANEL
    Unlike $(REF_SHORT DateTime,std,datetime,date), `SysTime` is not optimized for
    calendar-based operations, and getting individual units from it such as
    years or days is going to involve conversions and be less efficient.

    For calendar-based operations that don't
    care about time zones, then $(REF_SHORT DateTime,std,datetime,date) would be
    the type to use. For system time, use `SysTime`.
)
$(P
    Casting a `SysTime` to one of the following types will perform a conversion:
)
    * $(REF Date,std,datetime,date)
    * $(REF_SHORT DateTime,std,datetime,date)
    * $(REF_SHORT TimeOfDay,std,datetime,date)
$(P
    To convert a
    $(REF_SHORT Date,std,datetime,date) or $(REF_SHORT DateTime,std,datetime,date) to a
    `SysTime`, use `SysTime`'s constructor, and pass in the intended time
    zone with it (or don't pass in a $(REF TimeZone,std,datetime,timezone), and
    the local time zone will be used). Be aware, however, that converting from a
    $(REF_SHORT DateTime,std,datetime,date) to a `SysTime` will not necessarily
    be 100% accurate due to DST (one hour of the year doesn't exist and another
    occurs twice). To not risk any conversion errors, keep times as
    `SysTime`s. Aside from DST though, there shouldn't be any conversion
    problems.
)
$(PANEL
    For using time zones other than local time or UTC, use
    $(REF PosixTimeZone,std,datetime,timezone) on Posix systems (or on Windows,
    if providing the TZ Database files), and use
    $(REF WindowsTimeZone,std,datetime,timezone) on Windows systems. The time in
    `SysTime` is kept internally in hnsecs from midnight, January 1st, 1 A.D.
    UTC. Conversion error cannot happen when changing the time zone of a
    `SysTime`. $(REF LocalTime,std,datetime,timezone) is the
    $(REF_SHORT TimeZone,std,datetime,timezone) class which represents the local time,
    and `UTC` is the $(REF_SHORT TimeZone,std,datetime,timezone) class which
    represents UTC. `SysTime` uses $(REF_SHORT LocalTime,std,datetime,timezone) if
    no $(REF_SHORT TimeZone,std,datetime,timezone) is provided. For more details on
    time zones, see the documentation for $(REF_SHORT TimeZone,std,datetime,timezone),
    $(REF_SHORT PosixTimeZone,std,datetime,timezone), and
    $(REF_SHORT WindowsTimeZone,std,datetime,timezone).
)
$(P
    `SysTime`'s range is from approximately 29,000 B.C. to approximately
    29,000 A.D.
)
See_Also:
    $(RELATIVE_LINK2 .Clock.currTime, `Clock.currTime`) will return the current time as a `SysTime`.
  +/
struct SysTime
{
    import core.stdc.time : tm;
    version (Posix) import core.sys.posix.sys.time : timeval;
    import std.typecons : Rebindable;

public:

    /++
        Params:
            dateTime = The $(REF DateTime,std,datetime,date) to use to set
                       this $(LREF SysTime)'s internal std time. As
                       $(REF DateTime,std,datetime,date) has no concept of
                       time zone, tz is used as its time zone.
            tz       = The $(REF TimeZone,std,datetime,timezone) to use for this
                       $(LREF SysTime). If null,
                       $(REF LocalTime,std,datetime,timezone) will be used. The
                       given $(REF DateTime,std,datetime,date) is assumed to
                       be in the given time zone.
      +/
    this(DateTime dateTime, return scope immutable TimeZone tz = null) return scope @safe nothrow
    {
        try
            this(dateTime, Duration.zero, tz);
        catch (Exception e)
            assert(0, "SysTime's constructor threw when it shouldn't have.");
    }

    @safe unittest
    {
        static void test(DateTime dt, immutable TimeZone tz, long expected)
        {
            auto sysTime = SysTime(dt, tz);
            assert(sysTime._stdTime == expected);
            assert(sysTime._timezone is (tz is null ? LocalTime() : tz), format("Given DateTime: %s", dt));
        }

        test(DateTime.init, UTC(), 0);
        test(DateTime(1, 1, 1, 12, 30, 33), UTC(), 450_330_000_000L);
        test(DateTime(0, 12, 31, 12, 30, 33), UTC(), -413_670_000_000L);
        test(DateTime(1, 1, 1, 0, 0, 0), UTC(), 0);
        test(DateTime(1, 1, 1, 0, 0, 1), UTC(), 10_000_000L);
        test(DateTime(0, 12, 31, 23, 59, 59), UTC(), -10_000_000L);

        test(DateTime(1, 1, 1, 0, 0, 0), new immutable SimpleTimeZone(dur!"minutes"(-60)), 36_000_000_000L);
        test(DateTime(1, 1, 1, 0, 0, 0), new immutable SimpleTimeZone(Duration.zero), 0);
        test(DateTime(1, 1, 1, 0, 0, 0), new immutable SimpleTimeZone(dur!"minutes"(60)), -36_000_000_000L);

        static void testScope(scope ref DateTime dt) @safe
        {
            auto st = SysTime(dt);
        }
    }

    /++
        Params:
            dateTime = The $(REF DateTime,std,datetime,date) to use to set
                       this $(LREF SysTime)'s internal std time. As
                       $(REF DateTime,std,datetime,date) has no concept of
                       time zone, tz is used as its time zone.
            fracSecs = The fractional seconds portion of the time.
            tz       = The $(REF TimeZone,std,datetime,timezone) to use for this
                       $(LREF SysTime). If null,
                       $(REF LocalTime,std,datetime,timezone) will be used. The
                       given $(REF DateTime,std,datetime,date) is assumed to
                       be in the given time zone.

        Throws:
            $(REF DateTimeException,std,datetime,date) if `fracSecs` is negative or if it's
            greater than or equal to one second.
      +/
    this(DateTime dateTime, Duration fracSecs, return scope immutable TimeZone tz = null) return scope @safe
    {
        enforce(fracSecs >= Duration.zero, new DateTimeException("A SysTime cannot have negative fractional seconds."));
        enforce(fracSecs < seconds(1), new DateTimeException("Fractional seconds must be less than one second."));
        auto nonNullTZ = tz is null ? LocalTime() : tz;

        immutable dateDiff = dateTime.date - Date.init;
        immutable todDiff = dateTime.timeOfDay - TimeOfDay.init;

        immutable adjustedTime = dateDiff + todDiff + fracSecs;
        immutable standardTime = nonNullTZ.tzToUTC(adjustedTime.total!"hnsecs");

        this(standardTime, nonNullTZ);
    }

    @safe unittest
    {
        import core.time;
        static void test(DateTime dt, Duration fracSecs, immutable TimeZone tz, long expected)
        {
            auto sysTime = SysTime(dt, fracSecs, tz);
            assert(sysTime._stdTime == expected);
            assert(sysTime._timezone is (tz is null ? LocalTime() : tz),
                   format("Given DateTime: %s, Given Duration: %s", dt, fracSecs));
        }

        test(DateTime.init, Duration.zero, UTC(), 0);
        test(DateTime(1, 1, 1, 12, 30, 33), Duration.zero, UTC(), 450_330_000_000L);
        test(DateTime(0, 12, 31, 12, 30, 33), Duration.zero, UTC(), -413_670_000_000L);
        test(DateTime(1, 1, 1, 0, 0, 0), msecs(1), UTC(), 10_000L);
        test(DateTime(0, 12, 31, 23, 59, 59), msecs(999), UTC(), -10_000L);

        test(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999), UTC(), -1);
        test(DateTime(0, 12, 31, 23, 59, 59), hnsecs(1), UTC(), -9_999_999);
        test(DateTime(0, 12, 31, 23, 59, 59), Duration.zero, UTC(), -10_000_000);

        assertThrown!DateTimeException(SysTime(DateTime.init, hnsecs(-1), UTC()));
        assertThrown!DateTimeException(SysTime(DateTime.init, seconds(1), UTC()));

        static void testScope(scope ref DateTime dt, scope ref Duration d) @safe
        {
            auto st = SysTime(dt, d);
        }
    }

    /++
        Params:
            date = The $(REF Date,std,datetime,date) to use to set this
                   $(LREF SysTime)'s internal std time. As
                   $(REF Date,std,datetime,date) has no concept of time zone, tz
                   is used as its time zone.
            tz   = The $(REF TimeZone,std,datetime,timezone) to use for this
                   $(LREF SysTime). If null,
                   $(REF LocalTime,std,datetime,timezone) will be used. The
                   given $(REF Date,std,datetime,date) is assumed to be in the
                   given time zone.
      +/
    this(Date date, return scope immutable TimeZone tz = null) return scope @safe nothrow
    {
        _timezone = tz is null ? LocalTime() : tz;

        try
        {
            immutable adjustedTime = (date - Date(1, 1, 1)).total!"hnsecs";
            immutable standardTime = _timezone.tzToUTC(adjustedTime);

            this(standardTime, _timezone);
        }
        catch (Exception e)
            assert(0, "Date's constructor through when it shouldn't have.");
    }

    @safe unittest
    {
        static void test(Date d, immutable TimeZone tz, long expected)
        {
            auto sysTime = SysTime(d, tz);
            assert(sysTime._stdTime == expected);
            assert(sysTime._timezone is (tz is null ? LocalTime() : tz), format("Given Date: %s", d));
        }

        test(Date.init, UTC(), 0);
        test(Date(1, 1, 1), UTC(), 0);
        test(Date(1, 1, 2), UTC(), 864000000000);
        test(Date(0, 12, 31), UTC(), -864000000000);

        static void testScope(scope ref Date d) @safe
        {
            auto st = SysTime(d);
        }
    }

    /++
        Note:
            Whereas the other constructors take in the given date/time, assume
            that it's in the given time zone, and convert it to hnsecs in UTC
            since midnight, January 1st, 1 A.D. UTC - i.e. std time - this
            constructor takes a std time, which is specifically already in UTC,
            so no conversion takes place. Of course, the various getter
            properties and functions will use the given time zone's conversion
            function to convert the results to that time zone, but no conversion
            of the arguments to this constructor takes place.

        Params:
            stdTime = The number of hnsecs since midnight, January 1st, 1 A.D.
                      UTC.
            tz      = The $(REF TimeZone,std,datetime,timezone) to use for this
                      $(LREF SysTime). If null,
                      $(REF LocalTime,std,datetime,timezone) will be used.
      +/
    this(long stdTime, return scope immutable TimeZone tz = null) return scope @safe pure nothrow
    {
        _stdTime = stdTime;
        _timezone = tz is null ? LocalTime() : tz;
    }

    @safe unittest
    {
        static void test(long stdTime, immutable TimeZone tz)
        {
            auto sysTime = SysTime(stdTime, tz);
            assert(sysTime._stdTime == stdTime);
            assert(sysTime._timezone is (tz is null ? LocalTime() : tz), format("Given stdTime: %s", stdTime));
        }

        foreach (stdTime; [-1234567890L, -250, 0, 250, 1235657390L])
        {
            foreach (tz; testTZs)
                test(stdTime, tz);
        }
    }


    /++
        Params:
            rhs = The $(LREF SysTime) to assign to this one.

        Returns: The `this` of this `SysTime`.
      +/
    ref SysTime opAssign()(auto ref const(SysTime) rhs) scope return @safe pure nothrow
    {
        _stdTime = rhs._stdTime;
        _timezone = rhs._timezone;
        return this;
    }

    @safe unittest
    {
        SysTime st;
        st = SysTime(DateTime(2012, 12, 21, 1, 2, 3), UTC());
        assert(st == SysTime(DateTime(2012, 12, 21, 1, 2, 3), UTC()));

        const other = SysTime(DateTime(19, 1, 7, 13, 14, 15), LocalTime());
        st = other;
        assert(st == other);

        version (none) // https://issues.dlang.org/show_bug.cgi?id=21175
        static void testScope(scope ref SysTime left, const scope SysTime right) @safe
        {
            left = right;
        }
    }


    /++
        Checks for equality between this $(LREF SysTime) and the given
        $(LREF SysTime).

        Note that the time zone is ignored. Only the internal
        std times (which are in UTC) are compared.
     +/
    bool opEquals()(auto ref const(SysTime) rhs) @safe const pure nothrow scope
    {
        return _stdTime == rhs._stdTime;
    }

    @safe unittest
    {
        import std.range : chain;

        assert(SysTime(DateTime.init, UTC()) == SysTime(0, UTC()));
        assert(SysTime(DateTime.init, UTC()) == SysTime(0));
        assert(SysTime(Date.init, UTC()) == SysTime(0));
        assert(SysTime(0) == SysTime(0));

        static void test(DateTime dt, immutable TimeZone tz1, immutable TimeZone tz2)
        {
            auto st1 = SysTime(dt);
            st1.timezone = tz1;

            auto st2 = SysTime(dt);
            st2.timezone = tz2;

            assert(st1 == st2);
        }

        foreach (tz1; testTZs)
        {
            foreach (tz2; testTZs)
            {
                foreach (dt; chain(testDateTimesBC, testDateTimesAD))
                    test(dt, tz1, tz2);
            }
        }

        auto st = SysTime(DateTime(1999, 7, 6, 12, 33, 30));
        const cst = SysTime(DateTime(1999, 7, 6, 12, 33, 30));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 33, 30));
        assert(st == st);
        assert(st == cst);
        assert(st == ist);
        assert(cst == st);
        assert(cst == cst);
        assert(cst == ist);
        assert(ist == st);
        assert(ist == cst);
        assert(ist == ist);

        static void testScope(scope ref SysTime left, const scope SysTime right) @safe
        {
            assert(left == right);
            assert(right == left);
        }
    }


    /++
        Compares this $(LREF SysTime) with the given $(LREF SysTime).

        Time zone is irrelevant when comparing $(LREF SysTime)s.

        Returns:
            $(BOOKTABLE,
            $(TR $(TD this &lt; rhs) $(TD &lt; 0))
            $(TR $(TD this == rhs) $(TD 0))
            $(TR $(TD this &gt; rhs) $(TD &gt; 0))
            )
     +/
    int opCmp()(auto ref const(SysTime) rhs) @safe const pure nothrow scope
    {
        if (_stdTime < rhs._stdTime)
            return -1;
        if (_stdTime > rhs._stdTime)
            return 1;
        return 0;
    }

    @safe unittest
    {
        import std.algorithm.iteration : map;
        import std.array : array;
        import std.range : chain;

        assert(SysTime(DateTime.init, UTC()).opCmp(SysTime(0, UTC())) == 0);
        assert(SysTime(DateTime.init, UTC()).opCmp(SysTime(0)) == 0);
        assert(SysTime(Date.init, UTC()).opCmp(SysTime(0)) == 0);
        assert(SysTime(0).opCmp(SysTime(0)) == 0);

        static void testEqual(SysTime st, immutable TimeZone tz1, immutable TimeZone tz2)
        {
            auto st1 = st;
            st1.timezone = tz1;

            auto st2 = st;
            st2.timezone = tz2;

            assert(st1.opCmp(st2) == 0);
        }

        auto sts = array(map!SysTime(chain(testDateTimesBC, testDateTimesAD)));

        foreach (st; sts)
        {
            foreach (tz1; testTZs)
            {
                foreach (tz2; testTZs)
                    testEqual(st, tz1, tz2);
            }
        }

        static void testCmp(SysTime st1, immutable TimeZone tz1, SysTime st2, immutable TimeZone tz2)
        {
            st1.timezone = tz1;
            st2.timezone = tz2;
            assert(st1.opCmp(st2) < 0);
            assert(st2.opCmp(st1) > 0);
        }

        foreach (si, st1; sts)
        {
            foreach (st2; sts[si + 1 .. $])
            {
                foreach (tz1; testTZs)
                {
                    foreach (tz2; testTZs)
                        testCmp(st1, tz1, st2, tz2);
                }
            }
        }

        auto st = SysTime(DateTime(1999, 7, 6, 12, 33, 30));
        const cst = SysTime(DateTime(1999, 7, 6, 12, 33, 30));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 33, 30));
        assert(st.opCmp(st) == 0);
        assert(st.opCmp(cst) == 0);
        assert(st.opCmp(ist) == 0);
        assert(cst.opCmp(st) == 0);
        assert(cst.opCmp(cst) == 0);
        assert(cst.opCmp(ist) == 0);
        assert(ist.opCmp(st) == 0);
        assert(ist.opCmp(cst) == 0);
        assert(ist.opCmp(ist) == 0);

        static void testScope(scope ref SysTime left, const scope SysTime right) @safe
        {
            assert(left < right);
            assert(right > left);
        }
    }


    /++
        Returns: A hash of the $(LREF SysTime).
     +/
    size_t toHash() const @nogc pure nothrow @safe scope
    {
        static if (is(size_t == ulong))
            return _stdTime;
        else
        {
            // MurmurHash2
            enum ulong m = 0xc6a4a7935bd1e995UL;
            enum ulong n = m * 16;
            enum uint r = 47;

            ulong k = _stdTime;
            k *= m;
            k ^= k >> r;
            k *= m;

            ulong h = n;
            h ^= k;
            h *= m;

            return cast(size_t) h;
        }
    }

    @safe unittest
    {
        assert(SysTime(0).toHash == SysTime(0).toHash);
        assert(SysTime(DateTime(2000, 1, 1)).toHash == SysTime(DateTime(2000, 1, 1)).toHash);
        assert(SysTime(DateTime(2000, 1, 1)).toHash != SysTime(DateTime(2000, 1, 2)).toHash);

        // test that timezones aren't taken into account
        assert(SysTime(0, LocalTime()).toHash == SysTime(0, LocalTime()).toHash);
        assert(SysTime(0, LocalTime()).toHash == SysTime(0, UTC()).toHash);
        assert(SysTime(DateTime(2000, 1, 1), LocalTime()).toHash == SysTime(DateTime(2000, 1, 1), LocalTime()).toHash);
        immutable zone = new SimpleTimeZone(dur!"minutes"(60));
        assert(SysTime(DateTime(2000, 1, 1, 1), zone).toHash == SysTime(DateTime(2000, 1, 1), UTC()).toHash);
        assert(SysTime(DateTime(2000, 1, 1), zone).toHash != SysTime(DateTime(2000, 1, 1), UTC()).toHash);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.toHash();
        }
    }


    /++
        Year of the Gregorian Calendar. Positive numbers are A.D. Non-positive
        are B.C.
     +/
    @property short year() @safe const nothrow scope
    {
        return (cast(Date) this).year;
    }

    @safe unittest
    {
        import std.range : chain;
        static void test(SysTime sysTime, long expected)
        {
            assert(sysTime.year == expected, format("Value given: %s", sysTime));
        }

        test(SysTime(0, UTC()), 1);
        test(SysTime(1, UTC()), 1);
        test(SysTime(-1, UTC()), 0);

        foreach (year; chain(testYearsBC, testYearsAD))
        {
            foreach (md; testMonthDays)
            {
                foreach (tod; testTODs)
                {
                    auto dt = DateTime(Date(year, md.month, md.day), tod);
                    foreach (tz; testTZs)
                    {
                        foreach (fs; testFracSecs)
                            test(SysTime(dt, fs, tz), year);
                    }
                }
            }
        }

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(cst.year == 1999);
        assert(ist.year == 1999);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.year;
        }
    }

    /++
        Year of the Gregorian Calendar. Positive numbers are A.D. Non-positive
        are B.C.

        Params:
            year = The year to set this $(LREF SysTime)'s year to.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the new year is not
            a leap year and the resulting date would be on February 29th.
     +/
    @property void year(int year) @safe scope
    {
        auto hnsecs = adjTime;
        auto days = splitUnitsFromHNSecs!"days"(hnsecs) + 1;

        if (hnsecs < 0)
        {
            hnsecs += convert!("hours", "hnsecs")(24);
            --days;
        }

        auto date = Date(cast(int) days);
        date.year = year;

        immutable newDaysHNSecs = convert!("days", "hnsecs")(date.dayOfGregorianCal - 1);
        adjTime = newDaysHNSecs + hnsecs;
    }

    ///
    @safe unittest
    {
        import std.datetime.date : DateTime;

        assert(SysTime(DateTime(1999, 7, 6, 9, 7, 5)).year == 1999);
        assert(SysTime(DateTime(2010, 10, 4, 0, 0, 30)).year == 2010);
        assert(SysTime(DateTime(-7, 4, 5, 7, 45, 2)).year == -7);
    }

    @safe unittest
    {
        import std.range : chain;

        static void test(SysTime st, int year, SysTime expected)
        {
            st.year = year;
            assert(st == expected);
        }

        foreach (st; chain(testSysTimesBC, testSysTimesAD))
        {
            auto dt = cast(DateTime) st;

            foreach (year; chain(testYearsBC, testYearsAD))
            {
                auto e = SysTime(DateTime(year, dt.month, dt.day, dt.hour, dt.minute, dt.second),
                                 st.fracSecs,
                                 st.timezone);
                test(st, year, e);
            }
        }

        foreach (fs; testFracSecs)
        {
            foreach (tz; testTZs)
            {
                foreach (tod; testTODs)
                {
                    test(SysTime(DateTime(Date(1999, 2, 28), tod), fs, tz), 2000,
                         SysTime(DateTime(Date(2000, 2, 28), tod), fs, tz));
                    test(SysTime(DateTime(Date(2000, 2, 28), tod), fs, tz), 1999,
                         SysTime(DateTime(Date(1999, 2, 28), tod), fs, tz));
                }

                foreach (tod; testTODsThrown)
                {
                    auto st = SysTime(DateTime(Date(2000, 2, 29), tod), fs, tz);
                    assertThrown!DateTimeException(st.year = 1999);
                }
            }
        }

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        static assert(!__traits(compiles, cst.year = 7));
        static assert(!__traits(compiles, ist.year = 7));

        static void testScope(scope ref SysTime st) @safe
        {
            st.year = 42;
        }
    }

    /++
        Year B.C. of the Gregorian Calendar counting year 0 as 1 B.C.

        Throws:
            $(REF DateTimeException,std,datetime,date) if `isAD` is true.
     +/
    @property ushort yearBC() @safe const scope
    {
        return (cast(Date) this).yearBC;
    }

    ///
    @safe unittest
    {
        import std.datetime.date : DateTime;

        assert(SysTime(DateTime(0, 1, 1, 12, 30, 33)).yearBC == 1);
        assert(SysTime(DateTime(-1, 1, 1, 10, 7, 2)).yearBC == 2);
        assert(SysTime(DateTime(-100, 1, 1, 4, 59, 0)).yearBC == 101);
    }

    @safe unittest
    {
        import std.exception : assertNotThrown;
        foreach (st; testSysTimesBC)
        {
            auto msg = format("SysTime: %s", st);
            assertNotThrown!DateTimeException(st.yearBC, msg);
            assert(st.yearBC == (st.year * -1) + 1, msg);
        }

        foreach (st; [testSysTimesAD[0], testSysTimesAD[$/2], testSysTimesAD[$-1]])
            assertThrown!DateTimeException(st.yearBC, format("SysTime: %s", st));

        auto st = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        st.year = 12;
        assert(st.year == 12);
        static assert(!__traits(compiles, cst.year = 12));
        static assert(!__traits(compiles, ist.year = 12));

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.yearBC;
        }
    }


    /++
        Year B.C. of the Gregorian Calendar counting year 0 as 1 B.C.

        Params:
            year = The year B.C. to set this $(LREF SysTime)'s year to.

        Throws:
            $(REF DateTimeException,std,datetime,date) if a non-positive value
            is given.
     +/
    @property void yearBC(int year) @safe scope
    {
        auto hnsecs = adjTime;
        auto days = splitUnitsFromHNSecs!"days"(hnsecs) + 1;

        if (hnsecs < 0)
        {
            hnsecs += convert!("hours", "hnsecs")(24);
            --days;
        }

        auto date = Date(cast(int) days);
        date.yearBC = year;

        immutable newDaysHNSecs = convert!("days", "hnsecs")(date.dayOfGregorianCal - 1);
        adjTime = newDaysHNSecs + hnsecs;
    }

    @safe unittest
    {
        auto st = SysTime(DateTime(2010, 1, 1, 7, 30, 0));
        st.yearBC = 1;
        assert(st == SysTime(DateTime(0, 1, 1, 7, 30, 0)));

        st.yearBC = 10;
        assert(st == SysTime(DateTime(-9, 1, 1, 7, 30, 0)));
    }

    @safe unittest
    {
        import std.range : chain;
        static void test(SysTime st, int year, SysTime expected)
        {
            st.yearBC = year;
            assert(st == expected, format("SysTime: %s", st));
        }

        foreach (st; chain(testSysTimesBC, testSysTimesAD))
        {
            auto dt = cast(DateTime) st;

            foreach (year; testYearsBC)
            {
                auto e = SysTime(DateTime(year, dt.month, dt.day, dt.hour, dt.minute, dt.second),
                                 st.fracSecs,
                                 st.timezone);
                test(st, (year * -1) + 1, e);
            }
        }

        foreach (st; [testSysTimesBC[0], testSysTimesBC[$ - 1], testSysTimesAD[0], testSysTimesAD[$ - 1]])
        {
            foreach (year; testYearsBC)
                assertThrown!DateTimeException(st.yearBC = year);
        }

        foreach (fs; testFracSecs)
        {
            foreach (tz; testTZs)
            {
                foreach (tod; testTODs)
                {
                    test(SysTime(DateTime(Date(-1999, 2, 28), tod), fs, tz), 2001,
                         SysTime(DateTime(Date(-2000, 2, 28), tod), fs, tz));
                    test(SysTime(DateTime(Date(-2000, 2, 28), tod), fs, tz), 2000,
                         SysTime(DateTime(Date(-1999, 2, 28), tod), fs, tz));
                }

                foreach (tod; testTODsThrown)
                {
                    auto st = SysTime(DateTime(Date(-2000, 2, 29), tod), fs, tz);
                    assertThrown!DateTimeException(st.year = -1999);
                }
            }
        }

        auto st = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        st.yearBC = 12;
        assert(st.yearBC == 12);
        static assert(!__traits(compiles, cst.yearBC = 12));
        static assert(!__traits(compiles, ist.yearBC = 12));

        static void testScope(scope ref SysTime st) @safe
        {
            st.yearBC = 42;
        }
    }


    /++
        Month of a Gregorian Year.
     +/
    @property Month month() @safe const nothrow scope
    {
        return (cast(Date) this).month;
    }

    ///
    @safe unittest
    {
        import std.datetime.date : DateTime;

        assert(SysTime(DateTime(1999, 7, 6, 9, 7, 5)).month == 7);
        assert(SysTime(DateTime(2010, 10, 4, 0, 0, 30)).month == 10);
        assert(SysTime(DateTime(-7, 4, 5, 7, 45, 2)).month == 4);
    }

    @safe unittest
    {
        import std.range : chain;

        static void test(SysTime sysTime, Month expected)
        {
            assert(sysTime.month == expected, format("Value given: %s", sysTime));
        }

        test(SysTime(0, UTC()), Month.jan);
        test(SysTime(1, UTC()), Month.jan);
        test(SysTime(-1, UTC()), Month.dec);

        foreach (year; chain(testYearsBC, testYearsAD))
        {
            foreach (md; testMonthDays)
            {
                foreach (tod; testTODs)
                {
                    auto dt = DateTime(Date(year, md.month, md.day), tod);
                    foreach (fs; testFracSecs)
                    {
                        foreach (tz; testTZs)
                            test(SysTime(dt, fs, tz), md.month);
                    }
                }
            }
        }

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(cst.month == 7);
        assert(ist.month == 7);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.month;
        }
    }


    /++
        Month of a Gregorian Year.

        Params:
            month = The month to set this $(LREF SysTime)'s month to.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given month is
            not a valid month.
     +/
    @property void month(Month month) @safe scope
    {
        auto hnsecs = adjTime;
        auto days = splitUnitsFromHNSecs!"days"(hnsecs) + 1;

        if (hnsecs < 0)
        {
            hnsecs += convert!("hours", "hnsecs")(24);
            --days;
        }

        auto date = Date(cast(int) days);
        date.month = month;

        immutable newDaysHNSecs = convert!("days", "hnsecs")(date.dayOfGregorianCal - 1);
        adjTime = newDaysHNSecs + hnsecs;
    }

    @safe unittest
    {
        import std.algorithm.iteration : filter;
        import std.range : chain;

        static void test(SysTime st, Month month, SysTime expected)
        {
            st.month = cast(Month) month;
            assert(st == expected);
        }

        foreach (st; chain(testSysTimesBC, testSysTimesAD))
        {
            auto dt = cast(DateTime) st;

            foreach (md; testMonthDays)
            {
                if (st.day > maxDay(dt.year, md.month))
                    continue;
                auto e = SysTime(DateTime(dt.year, md.month, dt.day, dt.hour, dt.minute, dt.second),
                                 st.fracSecs,
                                 st.timezone);
                test(st, md.month, e);
            }
        }

        foreach (fs; testFracSecs)
        {
            foreach (tz; testTZs)
            {
                foreach (tod; testTODs)
                {
                    foreach (year; filter!((a){return yearIsLeapYear(a);}) (chain(testYearsBC, testYearsAD)))
                    {
                        test(SysTime(DateTime(Date(year, 1, 29), tod), fs, tz),
                             Month.feb,
                             SysTime(DateTime(Date(year, 2, 29), tod), fs, tz));
                    }

                    foreach (year; chain(testYearsBC, testYearsAD))
                    {
                        test(SysTime(DateTime(Date(year, 1, 28), tod), fs, tz),
                             Month.feb,
                             SysTime(DateTime(Date(year, 2, 28), tod), fs, tz));
                        test(SysTime(DateTime(Date(year, 7, 30), tod), fs, tz),
                             Month.jun,
                             SysTime(DateTime(Date(year, 6, 30), tod), fs, tz));
                    }
                }
            }
        }

        foreach (fs; [testFracSecs[0], testFracSecs[$-1]])
        {
            foreach (tz; testTZs)
            {
                foreach (tod; testTODsThrown)
                {
                    foreach (year; [testYearsBC[$-3], testYearsBC[$-2],
                                    testYearsBC[$-2], testYearsAD[0],
                                    testYearsAD[$-2], testYearsAD[$-1]])
                    {
                        auto day = yearIsLeapYear(year) ? 30 : 29;
                        auto st1 = SysTime(DateTime(Date(year, 1, day), tod), fs, tz);
                        assertThrown!DateTimeException(st1.month = Month.feb);

                        auto st2 = SysTime(DateTime(Date(year, 7, 31), tod), fs, tz);
                        assertThrown!DateTimeException(st2.month = Month.jun);
                    }
                }
            }
        }

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        static assert(!__traits(compiles, cst.month = Month.dec));
        static assert(!__traits(compiles, ist.month = Month.dec));

        static void testScope(scope ref SysTime st) @safe
        {
            st.month = Month.dec;
        }
    }

    /++
        Day of a Gregorian Month.
     +/
    @property ubyte day() @safe const nothrow scope
    {
        return (cast(Date) this).day;
    }

    ///
    @safe unittest
    {
        import std.datetime.date : DateTime;

        assert(SysTime(DateTime(1999, 7, 6, 9, 7, 5)).day == 6);
        assert(SysTime(DateTime(2010, 10, 4, 0, 0, 30)).day == 4);
        assert(SysTime(DateTime(-7, 4, 5, 7, 45, 2)).day == 5);
    }

    @safe unittest
    {
        import std.range : chain;

        static void test(SysTime sysTime, int expected)
        {
            assert(sysTime.day == expected, format("Value given: %s", sysTime));
        }

        test(SysTime(0, UTC()), 1);
        test(SysTime(1, UTC()), 1);
        test(SysTime(-1, UTC()), 31);

        foreach (year; chain(testYearsBC, testYearsAD))
        {
            foreach (md; testMonthDays)
            {
                foreach (tod; testTODs)
                {
                    auto dt = DateTime(Date(year, md.month, md.day), tod);

                    foreach (tz; testTZs)
                    {
                        foreach (fs; testFracSecs)
                            test(SysTime(dt, fs, tz), md.day);
                    }
                }
            }
        }

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
         assert(cst.day == 6);
        assert(ist.day == 6);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.day;
        }
    }


    /++
        Day of a Gregorian Month.

        Params:
            day = The day of the month to set this $(LREF SysTime)'s day to.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given day is not
            a valid day of the current month.
     +/
    @property void day(int day) @safe scope
    {
        auto hnsecs = adjTime;
        auto days = splitUnitsFromHNSecs!"days"(hnsecs) + 1;

        if (hnsecs < 0)
        {
            hnsecs += convert!("hours", "hnsecs")(24);
            --days;
        }

        auto date = Date(cast(int) days);
        date.day = day;

        immutable newDaysHNSecs = convert!("days", "hnsecs")(date.dayOfGregorianCal - 1);
        adjTime = newDaysHNSecs + hnsecs;
    }

    @safe unittest
    {
        import std.range : chain;
        import std.traits : EnumMembers;

        foreach (day; chain(testDays))
        {
            foreach (st; chain(testSysTimesBC, testSysTimesAD))
            {
                auto dt = cast(DateTime) st;

                if (day > maxDay(dt.year, dt.month))
                    continue;
                auto expected = SysTime(DateTime(dt.year, dt.month, day, dt.hour, dt.minute, dt.second),
                                        st.fracSecs,
                                        st.timezone);
                st.day = day;
                assert(st == expected, format("[%s] [%s]", st, expected));
            }
        }

        foreach (tz; testTZs)
        {
            foreach (tod; testTODs)
            {
                foreach (fs; testFracSecs)
                {
                    foreach (year; chain(testYearsBC, testYearsAD))
                    {
                        foreach (month; EnumMembers!Month)
                        {
                            auto st = SysTime(DateTime(Date(year, month, 1), tod), fs, tz);
                            immutable max = maxDay(year, month);
                            auto expected = SysTime(DateTime(Date(year, month, max), tod), fs, tz);

                            st.day = max;
                            assert(st == expected, format("[%s] [%s]", st, expected));
                        }
                    }
                }
            }
        }

        foreach (tz; testTZs)
        {
            foreach (tod; testTODsThrown)
            {
                foreach (fs; [testFracSecs[0], testFracSecs[$-1]])
                {
                    foreach (year; [testYearsBC[$-3], testYearsBC[$-2],
                                    testYearsBC[$-2], testYearsAD[0],
                                    testYearsAD[$-2], testYearsAD[$-1]])
                    {
                        foreach (month; EnumMembers!Month)
                        {
                            auto st = SysTime(DateTime(Date(year, month, 1), tod), fs, tz);
                            immutable max = maxDay(year, month);

                            assertThrown!DateTimeException(st.day = max + 1);
                        }
                    }
                }
            }
        }

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        static assert(!__traits(compiles, cst.day = 27));
        static assert(!__traits(compiles, ist.day = 27));

        static void testScope(scope ref SysTime st) @safe
        {
            st.day = 12;
        }
    }


    /++
        Hours past midnight.
     +/
    @property ubyte hour() @safe const nothrow scope
    {
        auto hnsecs = adjTime;
        auto days = splitUnitsFromHNSecs!"days"(hnsecs) + 1;

        if (hnsecs < 0)
        {
            hnsecs += convert!("hours", "hnsecs")(24);
            --days;
        }

        return cast(ubyte) getUnitsFromHNSecs!"hours"(hnsecs);
    }

    @safe unittest
    {
        import std.range : chain;

        static void test(SysTime sysTime, int expected)
        {
            assert(sysTime.hour == expected, format("Value given: %s", sysTime));
        }

        test(SysTime(0, UTC()), 0);
        test(SysTime(1, UTC()), 0);
        test(SysTime(-1, UTC()), 23);

        foreach (tz; testTZs)
        {
            foreach (year; chain(testYearsBC, testYearsAD))
            {
                foreach (md; testMonthDays)
                {
                    foreach (hour; testHours)
                    {
                        foreach (minute; testMinSecs)
                        {
                            foreach (second; testMinSecs)
                            {
                                auto dt = DateTime(Date(year, md.month, md.day), TimeOfDay(hour, minute, second));
                                foreach (fs; testFracSecs)
                                    test(SysTime(dt, fs, tz), hour);
                            }
                        }
                    }
                }
            }
        }

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(cst.hour == 12);
        assert(ist.hour == 12);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.hour;
        }
    }


    /++
        Hours past midnight.

        Params:
            hour = The hours to set this $(LREF SysTime)'s hour to.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given hour are
            not a valid hour of the day.
     +/
    @property void hour(int hour) @safe scope
    {
        enforceValid!"hours"(hour);

        auto hnsecs = adjTime;
        auto days = splitUnitsFromHNSecs!"days"(hnsecs);
        immutable daysHNSecs = convert!("days", "hnsecs")(days);
        immutable negative = hnsecs < 0;

        if (negative)
            hnsecs += convert!("hours", "hnsecs")(24);

        hnsecs = removeUnitsFromHNSecs!"hours"(hnsecs);
        hnsecs += convert!("hours", "hnsecs")(hour);

        if (negative)
            hnsecs -= convert!("hours", "hnsecs")(24);

        adjTime = daysHNSecs + hnsecs;
    }

    @safe unittest
    {
        import std.range : chain;

        foreach (hour; chain(testHours))
        {
            foreach (st; chain(testSysTimesBC, testSysTimesAD))
            {
                auto dt = cast(DateTime) st;
                auto expected = SysTime(DateTime(dt.year, dt.month, dt.day, hour, dt.minute, dt.second),
                                        st.fracSecs,
                                        st.timezone);
                st.hour = hour;
                assert(st == expected, format("[%s] [%s]", st, expected));
            }
        }

        auto st = testSysTimesAD[0];
        assertThrown!DateTimeException(st.hour = -1);
        assertThrown!DateTimeException(st.hour = 60);

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        static assert(!__traits(compiles, cst.hour = 27));
        static assert(!__traits(compiles, ist.hour = 27));

        static void testScope(scope ref SysTime st) @safe
        {
            st.hour = 12;
        }
    }


    /++
        Minutes past the current hour.
     +/
    @property ubyte minute() @safe const nothrow scope
    {
        auto hnsecs = adjTime;
        auto days = splitUnitsFromHNSecs!"days"(hnsecs) + 1;

        if (hnsecs < 0)
        {
            hnsecs += convert!("hours", "hnsecs")(24);
            --days;
        }

        hnsecs = removeUnitsFromHNSecs!"hours"(hnsecs);

        return cast(ubyte) getUnitsFromHNSecs!"minutes"(hnsecs);
    }

    @safe unittest
    {
        import std.range : chain;

        static void test(SysTime sysTime, int expected)
        {
            assert(sysTime.minute == expected, format("Value given: %s", sysTime));
        }

        test(SysTime(0, UTC()), 0);
        test(SysTime(1, UTC()), 0);
        test(SysTime(-1, UTC()), 59);

        foreach (tz; testTZs)
        {
            foreach (year; chain(testYearsBC, testYearsAD))
            {
                foreach (md; testMonthDays)
                {
                    foreach (hour; testHours)
                    {
                        foreach (minute; testMinSecs)
                        {
                            foreach (second; testMinSecs)
                            {
                                auto dt = DateTime(Date(year, md.month, md.day), TimeOfDay(hour, minute, second));
                                foreach (fs; testFracSecs)
                                    test(SysTime(dt, fs, tz), minute);
                            }
                        }
                    }
                }
            }
        }

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(cst.minute == 30);
        assert(ist.minute == 30);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.minute;
        }
    }


    /++
        Minutes past the current hour.

        Params:
            minute = The minute to set this $(LREF SysTime)'s minute to.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given minute are
            not a valid minute of an hour.
     +/
    @property void minute(int minute) @safe scope
    {
        enforceValid!"minutes"(minute);

        auto hnsecs = adjTime;
        auto days = splitUnitsFromHNSecs!"days"(hnsecs);
        immutable daysHNSecs = convert!("days", "hnsecs")(days);
        immutable negative = hnsecs < 0;

        if (negative)
            hnsecs += convert!("hours", "hnsecs")(24);

        immutable hour = splitUnitsFromHNSecs!"hours"(hnsecs);
        hnsecs = removeUnitsFromHNSecs!"minutes"(hnsecs);

        hnsecs += convert!("hours", "hnsecs")(hour);
        hnsecs += convert!("minutes", "hnsecs")(minute);

        if (negative)
            hnsecs -= convert!("hours", "hnsecs")(24);

        adjTime = daysHNSecs + hnsecs;
    }

    @safe unittest
    {
        import std.range : chain;

        foreach (minute; testMinSecs)
        {
            foreach (st; chain(testSysTimesBC, testSysTimesAD))
            {
                auto dt = cast(DateTime) st;
                auto expected = SysTime(DateTime(dt.year, dt.month, dt.day, dt.hour, minute, dt.second),
                                        st.fracSecs,
                                        st.timezone);
                st.minute = minute;
                assert(st == expected, format("[%s] [%s]", st, expected));
            }
        }

        auto st = testSysTimesAD[0];
        assertThrown!DateTimeException(st.minute = -1);
        assertThrown!DateTimeException(st.minute = 60);

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        static assert(!__traits(compiles, cst.minute = 27));
        static assert(!__traits(compiles, ist.minute = 27));

        static void testScope(scope ref SysTime st) @safe
        {
            st.minute = 12;
        }
    }


    /++
        Seconds past the current minute.
     +/
    @property ubyte second() @safe const nothrow scope
    {
        auto hnsecs = adjTime;
        auto days = splitUnitsFromHNSecs!"days"(hnsecs) + 1;

        if (hnsecs < 0)
        {
            hnsecs += convert!("hours", "hnsecs")(24);
            --days;
        }

        hnsecs = removeUnitsFromHNSecs!"hours"(hnsecs);
        hnsecs = removeUnitsFromHNSecs!"minutes"(hnsecs);

        return cast(ubyte) getUnitsFromHNSecs!"seconds"(hnsecs);
    }

    @safe unittest
    {
        import std.range : chain;

        static void test(SysTime sysTime, int expected)
        {
            assert(sysTime.second == expected, format("Value given: %s", sysTime));
        }

        test(SysTime(0, UTC()), 0);
        test(SysTime(1, UTC()), 0);
        test(SysTime(-1, UTC()), 59);

        foreach (tz; testTZs)
        {
            foreach (year; chain(testYearsBC, testYearsAD))
            {
                foreach (md; testMonthDays)
                {
                    foreach (hour; testHours)
                    {
                        foreach (minute; testMinSecs)
                        {
                            foreach (second; testMinSecs)
                            {
                                auto dt = DateTime(Date(year, md.month, md.day), TimeOfDay(hour, minute, second));
                                foreach (fs; testFracSecs)
                                    test(SysTime(dt, fs, tz), second);
                            }
                        }
                    }
                }
            }
        }

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(cst.second == 33);
        assert(ist.second == 33);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.second;
        }
    }


    /++
        Seconds past the current minute.

        Params:
            second = The second to set this $(LREF SysTime)'s second to.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given second are
            not a valid second of a minute.
     +/
    @property void second(int second) @safe scope
    {
        enforceValid!"seconds"(second);

        auto hnsecs = adjTime;
        auto days = splitUnitsFromHNSecs!"days"(hnsecs);
        immutable daysHNSecs = convert!("days", "hnsecs")(days);
        immutable negative = hnsecs < 0;

        if (negative)
            hnsecs += convert!("hours", "hnsecs")(24);

        immutable hour = splitUnitsFromHNSecs!"hours"(hnsecs);
        immutable minute = splitUnitsFromHNSecs!"minutes"(hnsecs);
        hnsecs = removeUnitsFromHNSecs!"seconds"(hnsecs);

        hnsecs += convert!("hours", "hnsecs")(hour);
        hnsecs += convert!("minutes", "hnsecs")(minute);
        hnsecs += convert!("seconds", "hnsecs")(second);

        if (negative)
            hnsecs -= convert!("hours", "hnsecs")(24);

        adjTime = daysHNSecs + hnsecs;
    }

    @safe unittest
    {
        import std.range : chain;

        foreach (second; testMinSecs)
        {
            foreach (st; chain(testSysTimesBC, testSysTimesAD))
            {
                auto dt = cast(DateTime) st;
                auto expected = SysTime(DateTime(dt.year, dt.month, dt.day, dt.hour, dt.minute, second),
                                        st.fracSecs,
                                        st.timezone);
                st.second = second;
                assert(st == expected, format("[%s] [%s]", st, expected));
            }
        }

        auto st = testSysTimesAD[0];
        assertThrown!DateTimeException(st.second = -1);
        assertThrown!DateTimeException(st.second = 60);

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        static assert(!__traits(compiles, cst.seconds = 27));
        static assert(!__traits(compiles, ist.seconds = 27));

        static void testScope(scope ref SysTime st) @safe
        {
            st.second = 12;
        }
    }


    /++
        Fractional seconds past the second (i.e. the portion of a
        $(LREF SysTime) which is less than a second).
     +/
    @property Duration fracSecs() @safe const nothrow scope
    {
        auto hnsecs = removeUnitsFromHNSecs!"days"(adjTime);

        if (hnsecs < 0)
            hnsecs += convert!("hours", "hnsecs")(24);

        return dur!"hnsecs"(removeUnitsFromHNSecs!"seconds"(hnsecs));
    }

    ///
    @safe unittest
    {
        import core.time : msecs, usecs, hnsecs, nsecs;
        import std.datetime.date : DateTime;

        auto dt = DateTime(1982, 4, 1, 20, 59, 22);
        assert(SysTime(dt, msecs(213)).fracSecs == msecs(213));
        assert(SysTime(dt, usecs(5202)).fracSecs == usecs(5202));
        assert(SysTime(dt, hnsecs(1234567)).fracSecs == hnsecs(1234567));

        // SysTime and Duration both have a precision of hnsecs (100 ns),
        // so nsecs are going to be truncated.
        assert(SysTime(dt, nsecs(123456789)).fracSecs == nsecs(123456700));
    }

    @safe unittest
    {
        import std.range : chain;
        import core.time;

        assert(SysTime(0, UTC()).fracSecs == Duration.zero);
        assert(SysTime(1, UTC()).fracSecs == hnsecs(1));
        assert(SysTime(-1, UTC()).fracSecs == hnsecs(9_999_999));

        foreach (tz; testTZs)
        {
            foreach (year; chain(testYearsBC, testYearsAD))
            {
                foreach (md; testMonthDays)
                {
                    foreach (hour; testHours)
                    {
                        foreach (minute; testMinSecs)
                        {
                            foreach (second; testMinSecs)
                            {
                                auto dt = DateTime(Date(year, md.month, md.day), TimeOfDay(hour, minute, second));
                                foreach (fs; testFracSecs)
                                    assert(SysTime(dt, fs, tz).fracSecs == fs);
                            }
                        }
                    }
                }
            }
        }

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(cst.fracSecs == Duration.zero);
        assert(ist.fracSecs == Duration.zero);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.fracSecs;
        }
    }


    /++
        Fractional seconds past the second (i.e. the portion of a
        $(LREF SysTime) which is less than a second).

        Params:
            fracSecs = The duration to set this $(LREF SysTime)'s fractional
                       seconds to.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given duration
            is negative or if it's greater than or equal to one second.
     +/
    @property void fracSecs(Duration fracSecs) @safe scope
    {
        enforce(fracSecs >= Duration.zero, new DateTimeException("A SysTime cannot have negative fractional seconds."));
        enforce(fracSecs < seconds(1), new DateTimeException("Fractional seconds must be less than one second."));

        auto oldHNSecs = adjTime;
        auto days = splitUnitsFromHNSecs!"days"(oldHNSecs);
        immutable daysHNSecs = convert!("days", "hnsecs")(days);
        immutable negative = oldHNSecs < 0;

        if (negative)
            oldHNSecs += convert!("hours", "hnsecs")(24);

        immutable seconds = splitUnitsFromHNSecs!"seconds"(oldHNSecs);
        immutable secondsHNSecs = convert!("seconds", "hnsecs")(seconds);
        auto newHNSecs = fracSecs.total!"hnsecs" + secondsHNSecs;

        if (negative)
            newHNSecs -= convert!("hours", "hnsecs")(24);

        adjTime = daysHNSecs + newHNSecs;
    }

    ///
    @safe unittest
    {
        import core.time : Duration, msecs, hnsecs, nsecs;
        import std.datetime.date : DateTime;

        auto st = SysTime(DateTime(1982, 4, 1, 20, 59, 22));
        assert(st.fracSecs == Duration.zero);

        st.fracSecs = msecs(213);
        assert(st.fracSecs == msecs(213));

        st.fracSecs = hnsecs(1234567);
        assert(st.fracSecs == hnsecs(1234567));

        // SysTime has a precision of hnsecs (100 ns), so nsecs are
        // going to be truncated.
        st.fracSecs = nsecs(123456789);
        assert(st.fracSecs == hnsecs(1234567));
    }

    @safe unittest
    {
        import std.range : chain;
        import core.time;

        foreach (fracSec; testFracSecs)
        {
            foreach (st; chain(testSysTimesBC, testSysTimesAD))
            {
                auto dt = cast(DateTime) st;
                auto expected = SysTime(dt, fracSec, st.timezone);
                st.fracSecs = fracSec;
                assert(st == expected, format("[%s] [%s]", st, expected));
            }
        }

        auto st = testSysTimesAD[0];
        assertThrown!DateTimeException(st.fracSecs = hnsecs(-1));
        assertThrown!DateTimeException(st.fracSecs = seconds(1));

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        static assert(!__traits(compiles, cst.fracSecs = msecs(7)));
        static assert(!__traits(compiles, ist.fracSecs = msecs(7)));

        static void testScope(scope ref SysTime st) @safe
        {
            st.fracSecs = Duration.zero;
        }
    }


    /++
        The total hnsecs from midnight, January 1st, 1 A.D. UTC. This is the
        internal representation of $(LREF SysTime).
     +/
    @property long stdTime() @safe const pure nothrow scope @nogc
    {
        return _stdTime;
    }

    @safe unittest
    {
        import core.time;
        assert(SysTime(0).stdTime == 0);
        assert(SysTime(1).stdTime == 1);
        assert(SysTime(-1).stdTime == -1);
        assert(SysTime(DateTime(1, 1, 1, 0, 0, 33), hnsecs(502), UTC()).stdTime == 330_000_502L);
        assert(SysTime(DateTime(1970, 1, 1, 0, 0, 0), UTC()).stdTime == 621_355_968_000_000_000L);

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(cst.stdTime > 0);
        assert(ist.stdTime > 0);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.stdTime;
        }
    }


    /++
        The total hnsecs from midnight, January 1st, 1 A.D. UTC. This is the
        internal representation of $(LREF SysTime).

        Params:
            stdTime = The number of hnsecs since January 1st, 1 A.D. UTC.
     +/
    @property void stdTime(long stdTime) @safe pure nothrow scope
    {
        _stdTime = stdTime;
    }

    @safe unittest
    {
        import core.time;
        static void test(long stdTime, SysTime expected, size_t line = __LINE__)
        {
            auto st = SysTime(0, UTC());
            st.stdTime = stdTime;
            assert(st == expected);
        }

        test(0, SysTime(Date(1, 1, 1), UTC()));
        test(1, SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(1), UTC()));
        test(-1, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999), UTC()));
        test(330_000_502L, SysTime(DateTime(1, 1, 1, 0, 0, 33), hnsecs(502), UTC()));
        test(621_355_968_000_000_000L, SysTime(DateTime(1970, 1, 1, 0, 0, 0), UTC()));

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        static assert(!__traits(compiles, cst.stdTime = 27));
        static assert(!__traits(compiles, ist.stdTime = 27));

        static void testScope(scope ref SysTime st) @safe
        {
            st.stdTime = 42;
        }
    }


    /++
        The current time zone of this $(LREF SysTime). Its internal time is
        always kept in UTC, so there are no conversion issues between time zones
        due to DST. Functions which return all or part of the time - such as
        hours - adjust the time to this $(LREF SysTime)'s time zone before
        returning.
      +/
    @property immutable(TimeZone) timezone() @safe const pure nothrow return scope
    {
        return _timezone;
    }

    @safe unittest
    {
        assert(SysTime.init.timezone is InitTimeZone());
        assert(SysTime(DateTime.init, UTC()).timezone is UTC());

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.timezone;
        }
    }


    /++
        The current time zone of this $(LREF SysTime). It's internal time is
        always kept in UTC, so there are no conversion issues between time zones
        due to DST. Functions which return all or part of the time - such as
        hours - adjust the time to this $(LREF SysTime)'s time zone before
        returning.

        Params:
            timezone = The $(REF _TimeZone,std,datetime,_timezone) to set this
                       $(LREF SysTime)'s time zone to.
      +/
    @property void timezone(immutable TimeZone timezone) @safe pure nothrow scope
    {
        if (timezone is null)
            _timezone = LocalTime();
        else
            _timezone = timezone;
    }

    @safe unittest
    {
        SysTime st;
        st.timezone = null;
        assert(st.timezone is LocalTime());
        st.timezone = UTC();
        assert(st.timezone is UTC());

        static void testScope(scope ref SysTime st) @safe
        {
            st.timezone = UTC();
        }
    }


    /++
        Returns whether DST is in effect for this $(LREF SysTime).
      +/
    @property bool dstInEffect() @safe const nothrow return scope
    {
        return _timezone.dstInEffect(_stdTime);
    }

    // This function's full unit testing is done in the time zone classes, but
    // this verifies that SysTime.init works correctly, since historically, it
    // has segfaulted due to a null _timezone.
    @safe unittest
    {
        assert(!SysTime.init.dstInEffect);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.dstInEffect;
        }
    }


    /++
        Returns what the offset from UTC is for this $(LREF SysTime).
        It includes the DST offset in effect at that time (if any).
      +/
    @property Duration utcOffset() @safe const nothrow return scope
    {
        return _timezone.utcOffsetAt(_stdTime);
    }

    // This function's full unit testing is done in the time zone classes, but
    // this verifies that SysTime.init works correctly, since historically, it
    // has segfaulted due to a null _timezone.
    @safe unittest
    {
        assert(SysTime.init.utcOffset == Duration.zero);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.utcOffset;
        }
    }


    /++
        Returns a $(LREF SysTime) with the same std time as this one, but with
        $(REF LocalTime,std,datetime,timezone) as its time zone.
      +/
    SysTime toLocalTime() @safe const pure nothrow scope
    {
        return SysTime(_stdTime, LocalTime());
    }

    @safe unittest
    {
        import core.time;
        {
            auto sysTime = SysTime(DateTime(1982, 1, 4, 8, 59, 7), hnsecs(27));
            assert(sysTime == sysTime.toLocalTime());
            assert(sysTime._stdTime == sysTime.toLocalTime()._stdTime);
            assert(sysTime.toLocalTime().timezone is LocalTime());
            assert(sysTime.toLocalTime().timezone is sysTime.timezone);
            assert(sysTime.toLocalTime().timezone !is UTC());
        }

        {
            auto stz = new immutable SimpleTimeZone(dur!"minutes"(-3 * 60));
            auto sysTime = SysTime(DateTime(1982, 1, 4, 8, 59, 7), hnsecs(27), stz);
            assert(sysTime == sysTime.toLocalTime());
            assert(sysTime._stdTime == sysTime.toLocalTime()._stdTime);
            assert(sysTime.toLocalTime().timezone is LocalTime());
            assert(sysTime.toLocalTime().timezone !is UTC());
            assert(sysTime.toLocalTime().timezone !is stz);
        }

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.toLocalTime();
        }
    }


    /++
        Returns a $(LREF SysTime) with the same std time as this one, but with
        `UTC` as its time zone.
      +/
    SysTime toUTC() @safe const pure nothrow scope
    {
        return SysTime(_stdTime, UTC());
    }

    @safe unittest
    {
        import core.time;
        auto sysTime = SysTime(DateTime(1982, 1, 4, 8, 59, 7), hnsecs(27));
        assert(sysTime == sysTime.toUTC());
        assert(sysTime._stdTime == sysTime.toUTC()._stdTime);
        assert(sysTime.toUTC().timezone is UTC());
        assert(sysTime.toUTC().timezone !is LocalTime());
        assert(sysTime.toUTC().timezone !is sysTime.timezone);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.toUTC();
        }
    }


    /++
        Returns a $(LREF SysTime) with the same std time as this one, but with
        given time zone as its time zone.
      +/
    SysTime toOtherTZ(immutable TimeZone tz) @safe const pure nothrow scope
    {
        if (tz is null)
            return SysTime(_stdTime, LocalTime());
        else
            return SysTime(_stdTime, tz);
    }

    @safe unittest
    {
        import core.time;
        auto stz = new immutable SimpleTimeZone(dur!"minutes"(11 * 60));
        auto sysTime = SysTime(DateTime(1982, 1, 4, 8, 59, 7), hnsecs(27));
        assert(sysTime == sysTime.toOtherTZ(stz));
        assert(sysTime._stdTime == sysTime.toOtherTZ(stz)._stdTime);
        assert(sysTime.toOtherTZ(stz).timezone is stz);
        assert(sysTime.toOtherTZ(stz).timezone !is LocalTime());
        assert(sysTime.toOtherTZ(stz).timezone !is UTC());
        assert(sysTime.toOtherTZ(null).timezone is LocalTime());

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.toOtherTZ(null);
        }
    }


    /++
        Converts this $(LREF SysTime) to unix time (i.e. seconds from midnight,
        January 1st, 1970 in UTC).

        The C standard does not specify the representation of time_t, so it is
        implementation defined. On POSIX systems, unix time is equivalent to
        time_t, but that's not necessarily true on other systems (e.g. it is
        not true for the Digital Mars C runtime). So, be careful when using unix
        time with C functions on non-POSIX systems.

        By default, the return type is time_t (which is normally an alias for
        int on 32-bit systems and long on 64-bit systems), but if a different
        size is required than either int or long can be passed as a template
        argument to get the desired size.

        If the return type is int, and the result can't fit in an int, then the
        closest value that can be held in 32 bits will be used (so `int.max`
        if it goes over and `int.min` if it goes under). However, no attempt
        is made to deal with integer overflow if the return type is long.

        Params:
            T = The return type (int or long). It defaults to time_t, which is
                normally 32 bits on a 32-bit system and 64 bits on a 64-bit
                system.

        Returns:
            A signed integer representing the unix time which is equivalent to
            this SysTime.
      +/
    T toUnixTime(T = time_t)() @safe const pure nothrow scope
    if (is(T == int) || is(T == long))
    {
        return stdTimeToUnixTime!T(_stdTime);
    }

    ///
    @safe unittest
    {
        import core.time : hours;
        import std.datetime.date : DateTime;
        import std.datetime.timezone : SimpleTimeZone, UTC;

        assert(SysTime(DateTime(1970, 1, 1), UTC()).toUnixTime() == 0);

        auto pst = new immutable SimpleTimeZone(hours(-8));
        assert(SysTime(DateTime(1970, 1, 1), pst).toUnixTime() == 28800);

        auto utc = SysTime(DateTime(2007, 12, 22, 8, 14, 45), UTC());
        assert(utc.toUnixTime() == 1_198_311_285);

        auto ca = SysTime(DateTime(2007, 12, 22, 8, 14, 45), pst);
        assert(ca.toUnixTime() == 1_198_340_085);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.toUnixTime();
        }
    }

    @safe unittest
    {
        import std.meta : AliasSeq;
        import core.time;
        assert(SysTime(DateTime(1970, 1, 1), UTC()).toUnixTime() == 0);
        static foreach (units; ["hnsecs", "usecs", "msecs"])
            assert(SysTime(DateTime(1970, 1, 1, 0, 0, 0), dur!units(1), UTC()).toUnixTime() == 0);
        assert(SysTime(DateTime(1970, 1, 1, 0, 0, 1), UTC()).toUnixTime() == 1);
        assert(SysTime(DateTime(1969, 12, 31, 23, 59, 59), hnsecs(9_999_999), UTC()).toUnixTime() == 0);
        assert(SysTime(DateTime(1969, 12, 31, 23, 59, 59), usecs(999_999), UTC()).toUnixTime() == 0);
        assert(SysTime(DateTime(1969, 12, 31, 23, 59, 59), msecs(999), UTC()).toUnixTime() == 0);
        assert(SysTime(DateTime(1969, 12, 31, 23, 59, 59), UTC()).toUnixTime() == -1);
    }


    /++
        Converts from unix time (i.e. seconds from midnight, January 1st, 1970
        in UTC) to a $(LREF SysTime).

        The C standard does not specify the representation of time_t, so it is
        implementation defined. On POSIX systems, unix time is equivalent to
        time_t, but that's not necessarily true on other systems (e.g. it is
        not true for the Digital Mars C runtime). So, be careful when using unix
        time with C functions on non-POSIX systems.

        Params:
            unixTime = Seconds from midnight, January 1st, 1970 in UTC.
            tz = The time zone for the SysTime that's returned.
      +/
    static SysTime fromUnixTime(long unixTime, immutable TimeZone tz = LocalTime()) @safe pure nothrow
    {
        return SysTime(unixTimeToStdTime(unixTime), tz);
    }

    ///
    @safe unittest
    {
        import core.time : hours;
        import std.datetime.date : DateTime;
        import std.datetime.timezone : SimpleTimeZone, UTC;

        assert(SysTime.fromUnixTime(0) ==
               SysTime(DateTime(1970, 1, 1), UTC()));

        auto pst = new immutable SimpleTimeZone(hours(-8));
        assert(SysTime.fromUnixTime(28800) ==
               SysTime(DateTime(1970, 1, 1), pst));

        auto st1 = SysTime.fromUnixTime(1_198_311_285, UTC());
        assert(st1 == SysTime(DateTime(2007, 12, 22, 8, 14, 45), UTC()));
        assert(st1.timezone is UTC());
        assert(st1 == SysTime(DateTime(2007, 12, 22, 0, 14, 45), pst));

        auto st2 = SysTime.fromUnixTime(1_198_311_285, pst);
        assert(st2 == SysTime(DateTime(2007, 12, 22, 8, 14, 45), UTC()));
        assert(st2.timezone is pst);
        assert(st2 == SysTime(DateTime(2007, 12, 22, 0, 14, 45), pst));
    }

    @safe unittest
    {
        import core.time;
        assert(SysTime.fromUnixTime(0) == SysTime(DateTime(1970, 1, 1), UTC()));
        assert(SysTime.fromUnixTime(1) == SysTime(DateTime(1970, 1, 1, 0, 0, 1), UTC()));
        assert(SysTime.fromUnixTime(-1) == SysTime(DateTime(1969, 12, 31, 23, 59, 59), UTC()));

        auto st = SysTime.fromUnixTime(0);
        auto dt = cast(DateTime) st;
        assert(dt <= DateTime(1970, 2, 1) && dt >= DateTime(1969, 12, 31));
        assert(st.timezone is LocalTime());

        auto aest = new immutable SimpleTimeZone(hours(10));
        assert(SysTime.fromUnixTime(-36000) == SysTime(DateTime(1970, 1, 1), aest));
    }


    /++
        Returns a `timeval` which represents this $(LREF SysTime).

        Note that like all conversions in std.datetime, this is a truncating
        conversion.

        If `timeval.tv_sec` is int, and the result can't fit in an int, then
        the closest value that can be held in 32 bits will be used for
        `tv_sec`. (so `int.max` if it goes over and `int.min` if it
        goes under).
      +/
    timeval toTimeVal() @safe const pure nothrow scope
    {
        immutable tv_sec = toUnixTime!(typeof(timeval.tv_sec))();
        immutable fracHNSecs = removeUnitsFromHNSecs!"seconds"(_stdTime - 621_355_968_000_000_000L);
        immutable tv_usec = cast(typeof(timeval.tv_usec))convert!("hnsecs", "usecs")(fracHNSecs);
        return timeval(tv_sec, tv_usec);
    }

    @safe unittest
    {
        import core.time;
        assert(SysTime(DateTime(1970, 1, 1), UTC()).toTimeVal() == timeval(0, 0));
        assert(SysTime(DateTime(1970, 1, 1), hnsecs(9), UTC()).toTimeVal() == timeval(0, 0));
        assert(SysTime(DateTime(1970, 1, 1), hnsecs(10), UTC()).toTimeVal() == timeval(0, 1));
        assert(SysTime(DateTime(1970, 1, 1), usecs(7), UTC()).toTimeVal() == timeval(0, 7));

        assert(SysTime(DateTime(1970, 1, 1, 0, 0, 1), UTC()).toTimeVal() == timeval(1, 0));
        assert(SysTime(DateTime(1970, 1, 1, 0, 0, 1), hnsecs(9), UTC()).toTimeVal() == timeval(1, 0));
        assert(SysTime(DateTime(1970, 1, 1, 0, 0, 1), hnsecs(10), UTC()).toTimeVal() == timeval(1, 1));
        assert(SysTime(DateTime(1970, 1, 1, 0, 0, 1), usecs(7), UTC()).toTimeVal() == timeval(1, 7));

        assert(SysTime(DateTime(1969, 12, 31, 23, 59, 59), hnsecs(9_999_999), UTC()).toTimeVal() == timeval(0, 0));
        assert(SysTime(DateTime(1969, 12, 31, 23, 59, 59), hnsecs(9_999_990), UTC()).toTimeVal() == timeval(0, -1));

        assert(SysTime(DateTime(1969, 12, 31, 23, 59, 59), usecs(999_999), UTC()).toTimeVal() == timeval(0, -1));
        assert(SysTime(DateTime(1969, 12, 31, 23, 59, 59), usecs(999), UTC()).toTimeVal() == timeval(0, -999_001));
        assert(SysTime(DateTime(1969, 12, 31, 23, 59, 59), msecs(999), UTC()).toTimeVal() == timeval(0, -1000));
        assert(SysTime(DateTime(1969, 12, 31, 23, 59, 59), UTC()).toTimeVal() == timeval(-1, 0));
        assert(SysTime(DateTime(1969, 12, 31, 23, 59, 58), usecs(17), UTC()).toTimeVal() == timeval(-1, -999_983));

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.toTimeVal();
        }
    }


    version (StdDdoc)
    {
        version (Windows) private struct timespec {}
        /++
            Returns a `timespec` which represents this $(LREF SysTime).

            $(BLUE This function is Posix-Only.)
          +/
        timespec toTimeSpec() @safe const pure nothrow scope;
    }
    else version (Posix)
    {
        timespec toTimeSpec() @safe const pure nothrow scope
        {
            immutable tv_sec = toUnixTime!(typeof(timespec.tv_sec))();
            immutable fracHNSecs = removeUnitsFromHNSecs!"seconds"(_stdTime - 621_355_968_000_000_000L);
            immutable tv_nsec = cast(typeof(timespec.tv_nsec))convert!("hnsecs", "nsecs")(fracHNSecs);
            return timespec(tv_sec, tv_nsec);
        }

        @safe unittest
        {
            import core.time;
            assert(SysTime(DateTime(1970, 1, 1), UTC()).toTimeSpec() == timespec(0, 0));
            assert(SysTime(DateTime(1970, 1, 1), hnsecs(9), UTC()).toTimeSpec() == timespec(0, 900));
            assert(SysTime(DateTime(1970, 1, 1), hnsecs(10), UTC()).toTimeSpec() == timespec(0, 1000));
            assert(SysTime(DateTime(1970, 1, 1), usecs(7), UTC()).toTimeSpec() == timespec(0, 7000));

            assert(SysTime(DateTime(1970, 1, 1, 0, 0, 1), UTC()).toTimeSpec() == timespec(1, 0));
            assert(SysTime(DateTime(1970, 1, 1, 0, 0, 1), hnsecs(9), UTC()).toTimeSpec() == timespec(1, 900));
            assert(SysTime(DateTime(1970, 1, 1, 0, 0, 1), hnsecs(10), UTC()).toTimeSpec() == timespec(1, 1000));
            assert(SysTime(DateTime(1970, 1, 1, 0, 0, 1), usecs(7), UTC()).toTimeSpec() == timespec(1, 7000));

            assert(SysTime(DateTime(1969, 12, 31, 23, 59, 59), hnsecs(9_999_999), UTC()).toTimeSpec() ==
                   timespec(0, -100));
            assert(SysTime(DateTime(1969, 12, 31, 23, 59, 59), hnsecs(9_999_990), UTC()).toTimeSpec() ==
                   timespec(0, -1000));

            assert(SysTime(DateTime(1969, 12, 31, 23, 59, 59), usecs(999_999), UTC()).toTimeSpec() ==
                   timespec(0, -1_000));
            assert(SysTime(DateTime(1969, 12, 31, 23, 59, 59), usecs(999), UTC()).toTimeSpec() ==
                   timespec(0, -999_001_000));
            assert(SysTime(DateTime(1969, 12, 31, 23, 59, 59), msecs(999), UTC()).toTimeSpec() ==
                   timespec(0, -1_000_000));
            assert(SysTime(DateTime(1969, 12, 31, 23, 59, 59), UTC()).toTimeSpec() ==
                   timespec(-1, 0));
            assert(SysTime(DateTime(1969, 12, 31, 23, 59, 58), usecs(17), UTC()).toTimeSpec() ==
                   timespec(-1, -999_983_000));

            static void testScope(scope ref SysTime st) @safe
            {
                auto result = st.toTimeSpec();
            }
        }
    }

    /++
        Returns a `tm` which represents this $(LREF SysTime).
      +/
    tm toTM() @safe const nothrow scope
    {
        auto dateTime = cast(DateTime) this;
        tm timeInfo;

        timeInfo.tm_sec = dateTime.second;
        timeInfo.tm_min = dateTime.minute;
        timeInfo.tm_hour = dateTime.hour;
        timeInfo.tm_mday = dateTime.day;
        timeInfo.tm_mon = dateTime.month - 1;
        timeInfo.tm_year = dateTime.year - 1900;
        timeInfo.tm_wday = dateTime.dayOfWeek;
        timeInfo.tm_yday = dateTime.dayOfYear - 1;
        timeInfo.tm_isdst = _timezone.dstInEffect(_stdTime);

        version (Posix)
        {
            import std.utf : toUTFz;
            timeInfo.tm_gmtoff = cast(int) convert!("hnsecs", "seconds")(adjTime - _stdTime);
            auto zone = timeInfo.tm_isdst ? _timezone.dstName : _timezone.stdName;
            timeInfo.tm_zone = zone.toUTFz!(char*)();
        }

        return timeInfo;
    }

    @system unittest
    {
        import std.conv : to;
        import core.time;

        version (Posix)
        {
            import std.datetime.timezone : clearTZEnvVar, setTZEnvVar;
            setTZEnvVar("America/Los_Angeles");
            scope(exit) clearTZEnvVar();
        }

        {
            auto timeInfo = SysTime(DateTime(1970, 1, 1)).toTM();

            assert(timeInfo.tm_sec == 0);
            assert(timeInfo.tm_min == 0);
            assert(timeInfo.tm_hour == 0);
            assert(timeInfo.tm_mday == 1);
            assert(timeInfo.tm_mon == 0);
            assert(timeInfo.tm_year == 70);
            assert(timeInfo.tm_wday == 4);
            assert(timeInfo.tm_yday == 0);

            version (Posix)
                assert(timeInfo.tm_isdst == 0);
            else version (Windows)
                assert(timeInfo.tm_isdst == 0 || timeInfo.tm_isdst == 1);

            version (Posix)
            {
                assert(timeInfo.tm_gmtoff == -8 * 60 * 60);
                assert(to!string(timeInfo.tm_zone) == "PST");
            }
        }

        {
            auto timeInfo = SysTime(DateTime(2010, 7, 4, 12, 15, 7), hnsecs(15)).toTM();

            assert(timeInfo.tm_sec == 7);
            assert(timeInfo.tm_min == 15);
            assert(timeInfo.tm_hour == 12);
            assert(timeInfo.tm_mday == 4);
            assert(timeInfo.tm_mon == 6);
            assert(timeInfo.tm_year == 110);
            assert(timeInfo.tm_wday == 0);
            assert(timeInfo.tm_yday == 184);

            version (Posix)
                assert(timeInfo.tm_isdst == 1);
            else version (Windows)
                assert(timeInfo.tm_isdst == 0 || timeInfo.tm_isdst == 1);

            version (Posix)
            {
                assert(timeInfo.tm_gmtoff == -7 * 60 * 60);
                assert(to!string(timeInfo.tm_zone) == "PDT");
            }
        }

        // This is more to verify that SysTime.init.toTM() doesn't segfault and
        // does something sane rather than that the value is anything
        // particularly useful.
        {
            auto timeInfo = SysTime.init.toTM();

            assert(timeInfo.tm_sec == 0);
            assert(timeInfo.tm_min == 0);
            assert(timeInfo.tm_hour == 0);
            assert(timeInfo.tm_mday == 1);
            assert(timeInfo.tm_mon == 0);
            assert(timeInfo.tm_year == -1899);
            assert(timeInfo.tm_wday == 1);
            assert(timeInfo.tm_yday == 0);
            assert(timeInfo.tm_isdst == 0);

            version (Posix)
            {
                assert(timeInfo.tm_gmtoff == 0);
                assert(to!string(timeInfo.tm_zone) == "SysTime.init's timezone");
            }
        }

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.toTM();
        }
    }


    /++
        Adds the given number of years or months to this $(LREF SysTime). A
        negative number will subtract.

        Note that if day overflow is allowed, and the date with the adjusted
        year/month overflows the number of days in the new month, then the month
        will be incremented by one, and the day set to the number of days
        overflowed. (e.g. if the day were 31 and the new month were June, then
        the month would be incremented to July, and the new day would be 1). If
        day overflow is not allowed, then the day will be set to the last valid
        day in the month (e.g. June 31st would become June 30th).

        Params:
            units         = The type of units to add ("years" or "months").
            value         = The number of months or years to add to this
                            $(LREF SysTime).
            allowOverflow = Whether the days should be allowed to overflow,
                            causing the month to increment.
      +/
    ref SysTime add(string units)(long value, AllowDayOverflow allowOverflow = AllowDayOverflow.yes) @safe nothrow scope
    if (units == "years" || units == "months")
    {
        auto hnsecs = adjTime;
        auto days = splitUnitsFromHNSecs!"days"(hnsecs) + 1;

        if (hnsecs < 0)
        {
            hnsecs += convert!("hours", "hnsecs")(24);
            --days;
        }

        auto date = Date(cast(int) days);
        date.add!units(value, allowOverflow);
        days = date.dayOfGregorianCal - 1;

        if (days < 0)
        {
            hnsecs -= convert!("hours", "hnsecs")(24);
            ++days;
        }

        immutable newDaysHNSecs = convert!("days", "hnsecs")(days);

        adjTime = newDaysHNSecs + hnsecs;

        return this;
    }

    @safe unittest
    {
        auto st1 = SysTime(DateTime(2010, 1, 1, 12, 30, 33));
        st1.add!"months"(11);
        assert(st1 == SysTime(DateTime(2010, 12, 1, 12, 30, 33)));

        auto st2 = SysTime(DateTime(2010, 1, 1, 12, 30, 33));
        st2.add!"months"(-11);
        assert(st2 == SysTime(DateTime(2009, 2, 1, 12, 30, 33)));

        auto st3 = SysTime(DateTime(2000, 2, 29, 12, 30, 33));
        st3.add!"years"(1);
        assert(st3 == SysTime(DateTime(2001, 3, 1, 12, 30, 33)));

        auto st4 = SysTime(DateTime(2000, 2, 29, 12, 30, 33));
        st4.add!"years"(1, AllowDayOverflow.no);
        assert(st4 == SysTime(DateTime(2001, 2, 28, 12, 30, 33)));
    }

    // Test add!"years"() with AllowDayOverflow.yes
    @safe unittest
    {
        import core.time;
        // Test A.D.
        {
            auto sysTime = SysTime(Date(1999, 7, 6));
            sysTime.add!"years"(7);
            assert(sysTime == SysTime(Date(2006, 7, 6)));
            sysTime.add!"years"(-9);
            assert(sysTime == SysTime(Date(1997, 7, 6)));
        }

        {
            auto sysTime = SysTime(Date(1999, 2, 28));
            sysTime.add!"years"(1);
            assert(sysTime == SysTime(Date(2000, 2, 28)));
        }

        {
            auto sysTime = SysTime(Date(2000, 2, 29));
            sysTime.add!"years"(-1);
            assert(sysTime == SysTime(Date(1999, 3, 1)));
        }

        {
            auto sysTime = SysTime(DateTime(1999, 7, 6, 12, 7, 3), msecs(234));
            sysTime.add!"years"(7);
            assert(sysTime == SysTime(DateTime(2006, 7, 6, 12, 7, 3), msecs(234)));
            sysTime.add!"years"(-9);
            assert(sysTime == SysTime(DateTime(1997, 7, 6, 12, 7, 3), msecs(234)));
        }

        {
            auto sysTime = SysTime(DateTime(1999, 2, 28, 0, 7, 2), usecs(1207));
            sysTime.add!"years"(1);
            assert(sysTime == SysTime(DateTime(2000, 2, 28, 0, 7, 2), usecs(1207)));
        }

        {
            auto sysTime = SysTime(DateTime(2000, 2, 29, 0, 7, 2), usecs(1207));
            sysTime.add!"years"(-1);
            assert(sysTime == SysTime(DateTime(1999, 3, 1, 0, 7, 2), usecs(1207)));
        }

        // Test B.C.
        {
            auto sysTime = SysTime(Date(-1999, 7, 6));
            sysTime.add!"years"(-7);
            assert(sysTime == SysTime(Date(-2006, 7, 6)));
            sysTime.add!"years"(9);
            assert(sysTime == SysTime(Date(-1997, 7, 6)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 2, 28));
            sysTime.add!"years"(-1);
            assert(sysTime == SysTime(Date(-2000, 2, 28)));
        }

        {
            auto sysTime = SysTime(Date(-2000, 2, 29));
            sysTime.add!"years"(1);
            assert(sysTime == SysTime(Date(-1999, 3, 1)));
        }

        {
            auto sysTime = SysTime(DateTime(-1999, 7, 6, 12, 7, 3), msecs(234));
            sysTime.add!"years"(-7);
            assert(sysTime == SysTime(DateTime(-2006, 7, 6, 12, 7, 3), msecs(234)));
            sysTime.add!"years"(9);
            assert(sysTime == SysTime(DateTime(-1997, 7, 6, 12, 7, 3), msecs(234)));
        }

        {
            auto sysTime = SysTime(DateTime(-1999, 2, 28, 3, 3, 3), hnsecs(3));
            sysTime.add!"years"(-1);
            assert(sysTime == SysTime(DateTime(-2000, 2, 28, 3, 3, 3), hnsecs(3)));
        }

        {
            auto sysTime = SysTime(DateTime(-2000, 2, 29, 3, 3, 3), hnsecs(3));
            sysTime.add!"years"(1);
            assert(sysTime == SysTime(DateTime(-1999, 3, 1, 3, 3, 3), hnsecs(3)));
        }

        // Test Both
        {
            auto sysTime = SysTime(Date(4, 7, 6));
            sysTime.add!"years"(-5);
            assert(sysTime == SysTime(Date(-1, 7, 6)));
            sysTime.add!"years"(5);
            assert(sysTime == SysTime(Date(4, 7, 6)));
        }

        {
            auto sysTime = SysTime(Date(-4, 7, 6));
            sysTime.add!"years"(5);
            assert(sysTime == SysTime(Date(1, 7, 6)));
            sysTime.add!"years"(-5);
            assert(sysTime == SysTime(Date(-4, 7, 6)));
        }

        {
            auto sysTime = SysTime(Date(4, 7, 6));
            sysTime.add!"years"(-8);
            assert(sysTime == SysTime(Date(-4, 7, 6)));
            sysTime.add!"years"(8);
            assert(sysTime == SysTime(Date(4, 7, 6)));
        }

        {
            auto sysTime = SysTime(Date(-4, 7, 6));
            sysTime.add!"years"(8);
            assert(sysTime == SysTime(Date(4, 7, 6)));
            sysTime.add!"years"(-8);
            assert(sysTime == SysTime(Date(-4, 7, 6)));
        }

        {
            auto sysTime = SysTime(Date(-4, 2, 29));
            sysTime.add!"years"(5);
            assert(sysTime == SysTime(Date(1, 3, 1)));
        }

        {
            auto sysTime = SysTime(Date(4, 2, 29));
            sysTime.add!"years"(-5);
            assert(sysTime == SysTime(Date(-1, 3, 1)));
        }

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 0, 0, 0));
            sysTime.add!"years"(-1);
            assert(sysTime == SysTime(DateTime(0, 1, 1, 0, 0, 0)));
            sysTime.add!"years"(1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        }

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999));
            sysTime.add!"years"(-1);
            assert(sysTime == SysTime(DateTime(0, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
            sysTime.add!"years"(1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 1, 1, 0, 0, 0));
            sysTime.add!"years"(1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 0, 0)));
            sysTime.add!"years"(-1);
            assert(sysTime == SysTime(DateTime(0, 1, 1, 0, 0, 0)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 1, 1, 23, 59, 59), hnsecs(9_999_999));
            sysTime.add!"years"(1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
            sysTime.add!"years"(-1);
            assert(sysTime == SysTime(DateTime(0, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
        }

        {
            auto sysTime = SysTime(DateTime(4, 7, 6, 14, 7, 1), usecs(54329));
            sysTime.add!"years"(-5);
            assert(sysTime == SysTime(DateTime(-1, 7, 6, 14, 7, 1), usecs(54329)));
            sysTime.add!"years"(5);
            assert(sysTime == SysTime(DateTime(4, 7, 6, 14, 7, 1), usecs(54329)));
        }

        {
            auto sysTime = SysTime(DateTime(-4, 7, 6, 14, 7, 1), usecs(54329));
            sysTime.add!"years"(5);
            assert(sysTime == SysTime(DateTime(1, 7, 6, 14, 7, 1), usecs(54329)));
            sysTime.add!"years"(-5);
            assert(sysTime == SysTime(DateTime(-4, 7, 6, 14, 7, 1), usecs(54329)));
        }

        {
            auto sysTime = SysTime(DateTime(-4, 2, 29, 5, 5, 5), msecs(555));
            sysTime.add!"years"(5);
            assert(sysTime == SysTime(DateTime(1, 3, 1, 5, 5, 5), msecs(555)));
        }

        {
            auto sysTime = SysTime(DateTime(4, 2, 29, 5, 5, 5), msecs(555));
            sysTime.add!"years"(-5);
            assert(sysTime == SysTime(DateTime(-1, 3, 1, 5, 5, 5), msecs(555)));
        }

        {
            auto sysTime = SysTime(DateTime(4, 2, 29, 5, 5, 5), msecs(555));
            sysTime.add!"years"(-5).add!"years"(7);
            assert(sysTime == SysTime(DateTime(6, 3, 1, 5, 5, 5), msecs(555)));
        }

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        static assert(!__traits(compiles, cst.add!"years"(4)));
        static assert(!__traits(compiles, ist.add!"years"(4)));

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.add!"years"(42);
        }
    }

    // Test add!"years"() with AllowDayOverflow.no
    @safe unittest
    {
        import core.time;
        // Test A.D.
        {
            auto sysTime = SysTime(Date(1999, 7, 6));
            sysTime.add!"years"(7, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(2006, 7, 6)));
            sysTime.add!"years"(-9, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1997, 7, 6)));
        }

        {
            auto sysTime = SysTime(Date(1999, 2, 28));
            sysTime.add!"years"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(2000, 2, 28)));
        }

        {
            auto sysTime = SysTime(Date(2000, 2, 29));
            sysTime.add!"years"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 2, 28)));
        }

        {
            auto sysTime = SysTime(DateTime(1999, 7, 6, 12, 7, 3), msecs(234));
            sysTime.add!"years"(7, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(2006, 7, 6, 12, 7, 3), msecs(234)));
            sysTime.add!"years"(-9, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1997, 7, 6, 12, 7, 3), msecs(234)));
        }

        {
            auto sysTime = SysTime(DateTime(1999, 2, 28, 0, 7, 2), usecs(1207));
            sysTime.add!"years"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(2000, 2, 28, 0, 7, 2), usecs(1207)));
        }

        {
            auto sysTime = SysTime(DateTime(2000, 2, 29, 0, 7, 2), usecs(1207));
            sysTime.add!"years"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1999, 2, 28, 0, 7, 2), usecs(1207)));
        }

        // Test B.C.
        {
            auto sysTime = SysTime(Date(-1999, 7, 6));
            sysTime.add!"years"(-7, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-2006, 7, 6)));
            sysTime.add!"years"(9, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1997, 7, 6)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 2, 28));
            sysTime.add!"years"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-2000, 2, 28)));
        }

        {
            auto sysTime = SysTime(Date(-2000, 2, 29));
            sysTime.add!"years"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1999, 2, 28)));
        }

        {
            auto sysTime = SysTime(DateTime(-1999, 7, 6, 12, 7, 3), msecs(234));
            sysTime.add!"years"(-7, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-2006, 7, 6, 12, 7, 3), msecs(234)));
            sysTime.add!"years"(9, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-1997, 7, 6, 12, 7, 3), msecs(234)));
        }

        {
            auto sysTime = SysTime(DateTime(-1999, 2, 28, 3, 3, 3), hnsecs(3));
            sysTime.add!"years"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-2000, 2, 28, 3, 3, 3), hnsecs(3)));
        }

        {
            auto sysTime = SysTime(DateTime(-2000, 2, 29, 3, 3, 3), hnsecs(3));
            sysTime.add!"years"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-1999, 2, 28, 3, 3, 3), hnsecs(3)));
        }

        // Test Both
        {
            auto sysTime = SysTime(Date(4, 7, 6));
            sysTime.add!"years"(-5, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1, 7, 6)));
            sysTime.add!"years"(5, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(4, 7, 6)));
        }

        {
            auto sysTime = SysTime(Date(-4, 7, 6));
            sysTime.add!"years"(5, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1, 7, 6)));
            sysTime.add!"years"(-5, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-4, 7, 6)));
        }

        {
            auto sysTime = SysTime(Date(4, 7, 6));
            sysTime.add!"years"(-8, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-4, 7, 6)));
            sysTime.add!"years"(8, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(4, 7, 6)));
        }

        {
            auto sysTime = SysTime(Date(-4, 7, 6));
            sysTime.add!"years"(8, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(4, 7, 6)));
            sysTime.add!"years"(-8, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-4, 7, 6)));
        }

        {
            auto sysTime = SysTime(Date(-4, 2, 29));
            sysTime.add!"years"(5, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1, 2, 28)));
        }

        {
            auto sysTime = SysTime(Date(4, 2, 29));
            sysTime.add!"years"(-5, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1, 2, 28)));
        }

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 0, 0, 0));
            sysTime.add!"years"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(0, 1, 1, 0, 0, 0)));
            sysTime.add!"years"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        }

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999));
            sysTime.add!"years"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(0, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
            sysTime.add!"years"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 1, 1, 0, 0, 0));
            sysTime.add!"years"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 0, 0)));
            sysTime.add!"years"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(0, 1, 1, 0, 0, 0)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 1, 1, 23, 59, 59), hnsecs(9_999_999));
            sysTime.add!"years"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
            sysTime.add!"years"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(0, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
        }

        {
            auto sysTime = SysTime(DateTime(4, 7, 6, 14, 7, 1), usecs(54329));
            sysTime.add!"years"(-5);
            assert(sysTime == SysTime(DateTime(-1, 7, 6, 14, 7, 1), usecs(54329)));
            sysTime.add!"years"(5);
            assert(sysTime == SysTime(DateTime(4, 7, 6, 14, 7, 1), usecs(54329)));
        }

        {
            auto sysTime = SysTime(DateTime(4, 7, 6, 14, 7, 1), usecs(54329));
            sysTime.add!"years"(-5, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-1, 7, 6, 14, 7, 1), usecs(54329)));
            sysTime.add!"years"(5, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(4, 7, 6, 14, 7, 1), usecs(54329)));
        }

        {
            auto sysTime = SysTime(DateTime(-4, 7, 6, 14, 7, 1), usecs(54329));
            sysTime.add!"years"(5, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1, 7, 6, 14, 7, 1), usecs(54329)));
            sysTime.add!"years"(-5, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-4, 7, 6, 14, 7, 1), usecs(54329)));
        }

        {
            auto sysTime = SysTime(DateTime(-4, 2, 29, 5, 5, 5), msecs(555));
            sysTime.add!"years"(5, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1, 2, 28, 5, 5, 5), msecs(555)));
        }

        {
            auto sysTime = SysTime(DateTime(4, 2, 29, 5, 5, 5), msecs(555));
            sysTime.add!"years"(-5, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-1, 2, 28, 5, 5, 5), msecs(555)));
        }

        {
            auto sysTime = SysTime(DateTime(4, 2, 29, 5, 5, 5), msecs(555));
            sysTime.add!"years"(-5, AllowDayOverflow.no).add!"years"(7, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(6, 2, 28, 5, 5, 5), msecs(555)));
        }
    }

    // Test add!"months"() with AllowDayOverflow.yes
    @safe unittest
    {
        import core.time;
        // Test A.D.
        {
            auto sysTime = SysTime(Date(1999, 7, 6));
            sysTime.add!"months"(3);
            assert(sysTime == SysTime(Date(1999, 10, 6)));
            sysTime.add!"months"(-4);
            assert(sysTime == SysTime(Date(1999, 6, 6)));
        }

        {
            auto sysTime = SysTime(Date(1999, 7, 6));
            sysTime.add!"months"(6);
            assert(sysTime == SysTime(Date(2000, 1, 6)));
            sysTime.add!"months"(-6);
            assert(sysTime == SysTime(Date(1999, 7, 6)));
        }

        {
            auto sysTime = SysTime(Date(1999, 7, 6));
            sysTime.add!"months"(27);
            assert(sysTime == SysTime(Date(2001, 10, 6)));
            sysTime.add!"months"(-28);
            assert(sysTime == SysTime(Date(1999, 6, 6)));
        }

        {
            auto sysTime = SysTime(Date(1999, 5, 31));
            sysTime.add!"months"(1);
            assert(sysTime == SysTime(Date(1999, 7, 1)));
        }

        {
            auto sysTime = SysTime(Date(1999, 5, 31));
            sysTime.add!"months"(-1);
            assert(sysTime == SysTime(Date(1999, 5, 1)));
        }

        {
            auto sysTime = SysTime(Date(1999, 2, 28));
            sysTime.add!"months"(12);
            assert(sysTime == SysTime(Date(2000, 2, 28)));
        }

        {
            auto sysTime = SysTime(Date(2000, 2, 29));
            sysTime.add!"months"(12);
            assert(sysTime == SysTime(Date(2001, 3, 1)));
        }

        {
            auto sysTime = SysTime(Date(1999, 7, 31));
            sysTime.add!"months"(1);
            assert(sysTime == SysTime(Date(1999, 8, 31)));
            sysTime.add!"months"(1);
            assert(sysTime == SysTime(Date(1999, 10, 1)));
        }

        {
            auto sysTime = SysTime(Date(1998, 8, 31));
            sysTime.add!"months"(13);
            assert(sysTime == SysTime(Date(1999, 10, 1)));
            sysTime.add!"months"(-13);
            assert(sysTime == SysTime(Date(1998, 9, 1)));
        }

        {
            auto sysTime = SysTime(Date(1997, 12, 31));
            sysTime.add!"months"(13);
            assert(sysTime == SysTime(Date(1999, 1, 31)));
            sysTime.add!"months"(-13);
            assert(sysTime == SysTime(Date(1997, 12, 31)));
        }

        {
            auto sysTime = SysTime(Date(1997, 12, 31));
            sysTime.add!"months"(14);
            assert(sysTime == SysTime(Date(1999, 3, 3)));
            sysTime.add!"months"(-14);
            assert(sysTime == SysTime(Date(1998, 1, 3)));
        }

        {
            auto sysTime = SysTime(Date(1998, 12, 31));
            sysTime.add!"months"(14);
            assert(sysTime == SysTime(Date(2000, 3, 2)));
            sysTime.add!"months"(-14);
            assert(sysTime == SysTime(Date(1999, 1, 2)));
        }

        {
            auto sysTime = SysTime(Date(1999, 12, 31));
            sysTime.add!"months"(14);
            assert(sysTime == SysTime(Date(2001, 3, 3)));
            sysTime.add!"months"(-14);
            assert(sysTime == SysTime(Date(2000, 1, 3)));
        }

        {
            auto sysTime = SysTime(DateTime(1999, 7, 6, 12, 2, 7), usecs(5007));
            sysTime.add!"months"(3);
            assert(sysTime == SysTime(DateTime(1999, 10, 6, 12, 2, 7), usecs(5007)));
            sysTime.add!"months"(-4);
            assert(sysTime == SysTime(DateTime(1999, 6, 6, 12, 2, 7), usecs(5007)));
        }

        {
            auto sysTime = SysTime(DateTime(1998, 12, 31, 7, 7, 7), hnsecs(422202));
            sysTime.add!"months"(14);
            assert(sysTime == SysTime(DateTime(2000, 3, 2, 7, 7, 7), hnsecs(422202)));
            sysTime.add!"months"(-14);
            assert(sysTime == SysTime(DateTime(1999, 1, 2, 7, 7, 7), hnsecs(422202)));
        }

        {
            auto sysTime = SysTime(DateTime(1999, 12, 31, 7, 7, 7), hnsecs(422202));
            sysTime.add!"months"(14);
            assert(sysTime == SysTime(DateTime(2001, 3, 3, 7, 7, 7), hnsecs(422202)));
            sysTime.add!"months"(-14);
            assert(sysTime == SysTime(DateTime(2000, 1, 3, 7, 7, 7), hnsecs(422202)));
        }

        // Test B.C.
        {
            auto sysTime = SysTime(Date(-1999, 7, 6));
            sysTime.add!"months"(3);
            assert(sysTime == SysTime(Date(-1999, 10, 6)));
            sysTime.add!"months"(-4);
            assert(sysTime == SysTime(Date(-1999, 6, 6)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 7, 6));
            sysTime.add!"months"(6);
            assert(sysTime == SysTime(Date(-1998, 1, 6)));
            sysTime.add!"months"(-6);
            assert(sysTime == SysTime(Date(-1999, 7, 6)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 7, 6));
            sysTime.add!"months"(-27);
            assert(sysTime == SysTime(Date(-2001, 4, 6)));
            sysTime.add!"months"(28);
            assert(sysTime == SysTime(Date(-1999, 8, 6)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 5, 31));
            sysTime.add!"months"(1);
            assert(sysTime == SysTime(Date(-1999, 7, 1)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 5, 31));
            sysTime.add!"months"(-1);
            assert(sysTime == SysTime(Date(-1999, 5, 1)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 2, 28));
            sysTime.add!"months"(-12);
            assert(sysTime == SysTime(Date(-2000, 2, 28)));
        }

        {
            auto sysTime = SysTime(Date(-2000, 2, 29));
            sysTime.add!"months"(-12);
            assert(sysTime == SysTime(Date(-2001, 3, 1)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 7, 31));
            sysTime.add!"months"(1);
            assert(sysTime == SysTime(Date(-1999, 8, 31)));
            sysTime.add!"months"(1);
            assert(sysTime == SysTime(Date(-1999, 10, 1)));
        }

        {
            auto sysTime = SysTime(Date(-1998, 8, 31));
            sysTime.add!"months"(13);
            assert(sysTime == SysTime(Date(-1997, 10, 1)));
            sysTime.add!"months"(-13);
            assert(sysTime == SysTime(Date(-1998, 9, 1)));
        }

        {
            auto sysTime = SysTime(Date(-1997, 12, 31));
            sysTime.add!"months"(13);
            assert(sysTime == SysTime(Date(-1995, 1, 31)));
            sysTime.add!"months"(-13);
            assert(sysTime == SysTime(Date(-1997, 12, 31)));
        }

        {
            auto sysTime = SysTime(Date(-1997, 12, 31));
            sysTime.add!"months"(14);
            assert(sysTime == SysTime(Date(-1995, 3, 3)));
            sysTime.add!"months"(-14);
            assert(sysTime == SysTime(Date(-1996, 1, 3)));
        }

        {
            auto sysTime = SysTime(Date(-2002, 12, 31));
            sysTime.add!"months"(14);
            assert(sysTime == SysTime(Date(-2000, 3, 2)));
            sysTime.add!"months"(-14);
            assert(sysTime == SysTime(Date(-2001, 1, 2)));
        }

        {
            auto sysTime = SysTime(Date(-2001, 12, 31));
            sysTime.add!"months"(14);
            assert(sysTime == SysTime(Date(-1999, 3, 3)));
            sysTime.add!"months"(-14);
            assert(sysTime == SysTime(Date(-2000, 1, 3)));
        }

        {
            auto sysTime = SysTime(DateTime(-1999, 7, 6, 12, 2, 7), usecs(5007));
            sysTime.add!"months"(3);
            assert(sysTime == SysTime(DateTime(-1999, 10, 6, 12, 2, 7), usecs(5007)));
            sysTime.add!"months"(-4);
            assert(sysTime == SysTime(DateTime(-1999, 6, 6, 12, 2, 7), usecs(5007)));
        }

        {
            auto sysTime = SysTime(DateTime(-2002, 12, 31, 7, 7, 7), hnsecs(422202));
            sysTime.add!"months"(14);
            assert(sysTime == SysTime(DateTime(-2000, 3, 2, 7, 7, 7), hnsecs(422202)));
            sysTime.add!"months"(-14);
            assert(sysTime == SysTime(DateTime(-2001, 1, 2, 7, 7, 7), hnsecs(422202)));
        }

        {
            auto sysTime = SysTime(DateTime(-2001, 12, 31, 7, 7, 7), hnsecs(422202));
            sysTime.add!"months"(14);
            assert(sysTime == SysTime(DateTime(-1999, 3, 3, 7, 7, 7), hnsecs(422202)));
            sysTime.add!"months"(-14);
            assert(sysTime == SysTime(DateTime(-2000, 1, 3, 7, 7, 7), hnsecs(422202)));
        }

        // Test Both
        {
            auto sysTime = SysTime(Date(1, 1, 1));
            sysTime.add!"months"(-1);
            assert(sysTime == SysTime(Date(0, 12, 1)));
            sysTime.add!"months"(1);
            assert(sysTime == SysTime(Date(1, 1, 1)));
        }

        {
            auto sysTime = SysTime(Date(4, 1, 1));
            sysTime.add!"months"(-48);
            assert(sysTime == SysTime(Date(0, 1, 1)));
            sysTime.add!"months"(48);
            assert(sysTime == SysTime(Date(4, 1, 1)));
        }

        {
            auto sysTime = SysTime(Date(4, 3, 31));
            sysTime.add!"months"(-49);
            assert(sysTime == SysTime(Date(0, 3, 2)));
            sysTime.add!"months"(49);
            assert(sysTime == SysTime(Date(4, 4, 2)));
        }

        {
            auto sysTime = SysTime(Date(4, 3, 31));
            sysTime.add!"months"(-85);
            assert(sysTime == SysTime(Date(-3, 3, 3)));
            sysTime.add!"months"(85);
            assert(sysTime == SysTime(Date(4, 4, 3)));
        }

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 0, 0, 0));
            sysTime.add!"months"(-1);
            assert(sysTime == SysTime(DateTime(0, 12, 1, 0, 0, 0)));
            sysTime.add!"months"(1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        }

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999));
            sysTime.add!"months"(-1);
            assert(sysTime == SysTime(DateTime(0, 12, 1, 23, 59, 59), hnsecs(9_999_999)));
            sysTime.add!"months"(1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 12, 1, 0, 0, 0));
            sysTime.add!"months"(1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 0, 0)));
            sysTime.add!"months"(-1);
            assert(sysTime == SysTime(DateTime(0, 12, 1, 0, 0, 0)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 12, 1, 23, 59, 59), hnsecs(9_999_999));
            sysTime.add!"months"(1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
            sysTime.add!"months"(-1);
            assert(sysTime == SysTime(DateTime(0, 12, 1, 23, 59, 59), hnsecs(9_999_999)));
        }

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 0, 7, 9), hnsecs(17));
            sysTime.add!"months"(-1);
            assert(sysTime == SysTime(DateTime(0, 12, 1, 0, 7, 9), hnsecs(17)));
            sysTime.add!"months"(1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 7, 9), hnsecs(17)));
        }

        {
            auto sysTime = SysTime(DateTime(4, 3, 31, 12, 11, 10), msecs(9));
            sysTime.add!"months"(-85);
            assert(sysTime == SysTime(DateTime(-3, 3, 3, 12, 11, 10), msecs(9)));
            sysTime.add!"months"(85);
            assert(sysTime == SysTime(DateTime(4, 4, 3, 12, 11, 10), msecs(9)));
        }

        {
            auto sysTime = SysTime(DateTime(-3, 3, 31, 12, 11, 10), msecs(9));
            sysTime.add!"months"(85);
            assert(sysTime == SysTime(DateTime(4, 5, 1, 12, 11, 10), msecs(9)));
            sysTime.add!"months"(-85);
            assert(sysTime == SysTime(DateTime(-3, 4, 1, 12, 11, 10), msecs(9)));
        }

        {
            auto sysTime = SysTime(DateTime(-3, 3, 31, 12, 11, 10), msecs(9));
            sysTime.add!"months"(85).add!"months"(-83);
            assert(sysTime == SysTime(DateTime(-3, 6, 1, 12, 11, 10), msecs(9)));
        }

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        static assert(!__traits(compiles, cst.add!"months"(4)));
        static assert(!__traits(compiles, ist.add!"months"(4)));

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.add!"months"(42);
        }
    }

    // Test add!"months"() with AllowDayOverflow.no
    @safe unittest
    {
        import core.time;
        // Test A.D.
        {
            auto sysTime = SysTime(Date(1999, 7, 6));
            sysTime.add!"months"(3, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 10, 6)));
            sysTime.add!"months"(-4, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 6, 6)));
        }

        {
            auto sysTime = SysTime(Date(1999, 7, 6));
            sysTime.add!"months"(6, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(2000, 1, 6)));
            sysTime.add!"months"(-6, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 7, 6)));
        }

        {
            auto sysTime = SysTime(Date(1999, 7, 6));
            sysTime.add!"months"(27, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(2001, 10, 6)));
            sysTime.add!"months"(-28, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 6, 6)));
        }

        {
            auto sysTime = SysTime(Date(1999, 5, 31));
            sysTime.add!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 6, 30)));
        }

        {
            auto sysTime = SysTime(Date(1999, 5, 31));
            sysTime.add!"months"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 4, 30)));
        }

        {
            auto sysTime = SysTime(Date(1999, 2, 28));
            sysTime.add!"months"(12, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(2000, 2, 28)));
        }

        {
            auto sysTime = SysTime(Date(2000, 2, 29));
            sysTime.add!"months"(12, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(2001, 2, 28)));
        }

        {
            auto sysTime = SysTime(Date(1999, 7, 31));
            sysTime.add!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 8, 31)));
            sysTime.add!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 9, 30)));
        }

        {
            auto sysTime = SysTime(Date(1998, 8, 31));
            sysTime.add!"months"(13, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 9, 30)));
            sysTime.add!"months"(-13, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1998, 8, 30)));
        }

        {
            auto sysTime = SysTime(Date(1997, 12, 31));
            sysTime.add!"months"(13, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 1, 31)));
            sysTime.add!"months"(-13, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1997, 12, 31)));
        }

        {
            auto sysTime = SysTime(Date(1997, 12, 31));
            sysTime.add!"months"(14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 2, 28)));
            sysTime.add!"months"(-14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1997, 12, 28)));
        }

        {
            auto sysTime = SysTime(Date(1998, 12, 31));
            sysTime.add!"months"(14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(2000, 2, 29)));
            sysTime.add!"months"(-14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1998, 12, 29)));
        }

        {
            auto sysTime = SysTime(Date(1999, 12, 31));
            sysTime.add!"months"(14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(2001, 2, 28)));
            sysTime.add!"months"(-14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 12, 28)));
        }

        {
            auto sysTime = SysTime(DateTime(1999, 7, 6, 12, 2, 7), usecs(5007));
            sysTime.add!"months"(3, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1999, 10, 6, 12, 2, 7), usecs(5007)));
            sysTime.add!"months"(-4, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1999, 6, 6, 12, 2, 7), usecs(5007)));
        }

        {
            auto sysTime = SysTime(DateTime(1998, 12, 31, 7, 7, 7), hnsecs(422202));
            sysTime.add!"months"(14, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(2000, 2, 29, 7, 7, 7), hnsecs(422202)));
            sysTime.add!"months"(-14, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1998, 12, 29, 7, 7, 7), hnsecs(422202)));
        }

        {
            auto sysTime = SysTime(DateTime(1999, 12, 31, 7, 7, 7), hnsecs(422202));
            sysTime.add!"months"(14, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(2001, 2, 28, 7, 7, 7), hnsecs(422202)));
            sysTime.add!"months"(-14, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1999, 12, 28, 7, 7, 7), hnsecs(422202)));
        }

        // Test B.C.
        {
            auto sysTime = SysTime(Date(-1999, 7, 6));
            sysTime.add!"months"(3, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1999, 10, 6)));
            sysTime.add!"months"(-4, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1999, 6, 6)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 7, 6));
            sysTime.add!"months"(6, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1998, 1, 6)));
            sysTime.add!"months"(-6, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1999, 7, 6)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 7, 6));
            sysTime.add!"months"(-27, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-2001, 4, 6)));
            sysTime.add!"months"(28, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1999, 8, 6)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 5, 31));
            sysTime.add!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1999, 6, 30)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 5, 31));
            sysTime.add!"months"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1999, 4, 30)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 2, 28));
            sysTime.add!"months"(-12, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-2000, 2, 28)));
        }

        {
            auto sysTime = SysTime(Date(-2000, 2, 29));
            sysTime.add!"months"(-12, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-2001, 2, 28)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 7, 31));
            sysTime.add!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1999, 8, 31)));
            sysTime.add!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1999, 9, 30)));
        }

        {
            auto sysTime = SysTime(Date(-1998, 8, 31));
            sysTime.add!"months"(13, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1997, 9, 30)));
            sysTime.add!"months"(-13, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1998, 8, 30)));
        }

        {
            auto sysTime = SysTime(Date(-1997, 12, 31));
            sysTime.add!"months"(13, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1995, 1, 31)));
            sysTime.add!"months"(-13, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1997, 12, 31)));
        }

        {
            auto sysTime = SysTime(Date(-1997, 12, 31));
            sysTime.add!"months"(14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1995, 2, 28)));
            sysTime.add!"months"(-14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1997, 12, 28)));
        }

        {
            auto sysTime = SysTime(Date(-2002, 12, 31));
            sysTime.add!"months"(14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-2000, 2, 29)));
            sysTime.add!"months"(-14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-2002, 12, 29)));
        }

        {
            auto sysTime = SysTime(Date(-2001, 12, 31));
            sysTime.add!"months"(14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1999, 2, 28)));
            sysTime.add!"months"(-14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-2001, 12, 28)));
        }

        {
            auto sysTime = SysTime(DateTime(-1999, 7, 6, 12, 2, 7), usecs(5007));
            sysTime.add!"months"(3, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-1999, 10, 6, 12, 2, 7), usecs(5007)));
            sysTime.add!"months"(-4, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-1999, 6, 6, 12, 2, 7), usecs(5007)));
        }

        {
            auto sysTime = SysTime(DateTime(-2002, 12, 31, 7, 7, 7), hnsecs(422202));
            sysTime.add!"months"(14, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-2000, 2, 29, 7, 7, 7), hnsecs(422202)));
            sysTime.add!"months"(-14, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-2002, 12, 29, 7, 7, 7), hnsecs(422202)));
        }

        {
            auto sysTime = SysTime(DateTime(-2001, 12, 31, 7, 7, 7), hnsecs(422202));
            sysTime.add!"months"(14, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-1999, 2, 28, 7, 7, 7), hnsecs(422202)));
            sysTime.add!"months"(-14, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-2001, 12, 28, 7, 7, 7), hnsecs(422202)));
        }

        // Test Both
        {
            auto sysTime = SysTime(Date(1, 1, 1));
            sysTime.add!"months"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(0, 12, 1)));
            sysTime.add!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1, 1, 1)));
        }

        {
            auto sysTime = SysTime(Date(4, 1, 1));
            sysTime.add!"months"(-48, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(0, 1, 1)));
            sysTime.add!"months"(48, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(4, 1, 1)));
        }

        {
            auto sysTime = SysTime(Date(4, 3, 31));
            sysTime.add!"months"(-49, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(0, 2, 29)));
            sysTime.add!"months"(49, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(4, 3, 29)));
        }

        {
            auto sysTime = SysTime(Date(4, 3, 31));
            sysTime.add!"months"(-85, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-3, 2, 28)));
            sysTime.add!"months"(85, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(4, 3, 28)));
        }

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 0, 0, 0));
            sysTime.add!"months"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(0, 12, 1, 0, 0, 0)));
            sysTime.add!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        }

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999));
            sysTime.add!"months"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(0, 12, 1, 23, 59, 59), hnsecs(9_999_999)));
            sysTime.add!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 12, 1, 0, 0, 0));
            sysTime.add!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 0, 0)));
            sysTime.add!"months"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(0, 12, 1, 0, 0, 0)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 12, 1, 23, 59, 59), hnsecs(9_999_999));
            sysTime.add!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
            sysTime.add!"months"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(0, 12, 1, 23, 59, 59), hnsecs(9_999_999)));
        }

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 0, 7, 9), hnsecs(17));
            sysTime.add!"months"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(0, 12, 1, 0, 7, 9), hnsecs(17)));
            sysTime.add!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 7, 9), hnsecs(17)));
        }

        {
            auto sysTime = SysTime(DateTime(4, 3, 31, 12, 11, 10), msecs(9));
            sysTime.add!"months"(-85, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-3, 2, 28, 12, 11, 10), msecs(9)));
            sysTime.add!"months"(85, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(4, 3, 28, 12, 11, 10), msecs(9)));
        }

        {
            auto sysTime = SysTime(DateTime(-3, 3, 31, 12, 11, 10), msecs(9));
            sysTime.add!"months"(85, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(4, 4, 30, 12, 11, 10), msecs(9)));
            sysTime.add!"months"(-85, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-3, 3, 30, 12, 11, 10), msecs(9)));
        }

        {
            auto sysTime = SysTime(DateTime(-3, 3, 31, 12, 11, 10), msecs(9));
            sysTime.add!"months"(85, AllowDayOverflow.no).add!"months"(-83, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-3, 5, 30, 12, 11, 10), msecs(9)));
        }
    }


    /++
        Adds the given number of years or months to this $(LREF SysTime). A
        negative number will subtract.

        The difference between rolling and adding is that rolling does not
        affect larger units. Rolling a $(LREF SysTime) 12 months
        gets the exact same $(LREF SysTime). However, the days can still be
        affected due to the differing number of days in each month.

        Because there are no units larger than years, there is no difference
        between adding and rolling years.

        Params:
            units         = The type of units to add ("years" or "months").
            value         = The number of months or years to add to this
                            $(LREF SysTime).
            allowOverflow = Whether the days should be allowed to overflow,
                            causing the month to increment.
      +/
    ref SysTime roll(string units)
                    (long value, AllowDayOverflow allowOverflow = AllowDayOverflow.yes) @safe nothrow scope
    if (units == "years")
    {
        return add!"years"(value, allowOverflow);
    }

    ///
    @safe unittest
    {
        import std.datetime.date : AllowDayOverflow, DateTime;

        auto st1 = SysTime(DateTime(2010, 1, 1, 12, 33, 33));
        st1.roll!"months"(1);
        assert(st1 == SysTime(DateTime(2010, 2, 1, 12, 33, 33)));

        auto st2 = SysTime(DateTime(2010, 1, 1, 12, 33, 33));
        st2.roll!"months"(-1);
        assert(st2 == SysTime(DateTime(2010, 12, 1, 12, 33, 33)));

        auto st3 = SysTime(DateTime(1999, 1, 29, 12, 33, 33));
        st3.roll!"months"(1);
        assert(st3 == SysTime(DateTime(1999, 3, 1, 12, 33, 33)));

        auto st4 = SysTime(DateTime(1999, 1, 29, 12, 33, 33));
        st4.roll!"months"(1, AllowDayOverflow.no);
        assert(st4 == SysTime(DateTime(1999, 2, 28, 12, 33, 33)));

        auto st5 = SysTime(DateTime(2000, 2, 29, 12, 30, 33));
        st5.roll!"years"(1);
        assert(st5 == SysTime(DateTime(2001, 3, 1, 12, 30, 33)));

        auto st6 = SysTime(DateTime(2000, 2, 29, 12, 30, 33));
        st6.roll!"years"(1, AllowDayOverflow.no);
        assert(st6 == SysTime(DateTime(2001, 2, 28, 12, 30, 33)));
    }

    @safe unittest
    {
        auto st = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        st.roll!"years"(4);
        static assert(!__traits(compiles, cst.roll!"years"(4)));
        static assert(!__traits(compiles, ist.roll!"years"(4)));

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.roll!"years"(42);
        }
    }


    // Shares documentation with "years" overload.
    ref SysTime roll(string units)
                    (long value, AllowDayOverflow allowOverflow = AllowDayOverflow.yes) @safe nothrow scope
    if (units == "months")
    {
        auto hnsecs = adjTime;
        auto days = splitUnitsFromHNSecs!"days"(hnsecs) + 1;

        if (hnsecs < 0)
        {
            hnsecs += convert!("hours", "hnsecs")(24);
            --days;
        }

        auto date = Date(cast(int) days);
        date.roll!"months"(value, allowOverflow);
        days = date.dayOfGregorianCal - 1;

        if (days < 0)
        {
            hnsecs -= convert!("hours", "hnsecs")(24);
            ++days;
        }

        immutable newDaysHNSecs = convert!("days", "hnsecs")(days);
        adjTime = newDaysHNSecs + hnsecs;
        return this;
    }

    // Test roll!"months"() with AllowDayOverflow.yes
    @safe unittest
    {
        import core.time;
        // Test A.D.
        {
            auto sysTime = SysTime(Date(1999, 7, 6));
            sysTime.roll!"months"(3);
            assert(sysTime == SysTime(Date(1999, 10, 6)));
            sysTime.roll!"months"(-4);
            assert(sysTime == SysTime(Date(1999, 6, 6)));
        }

        {
            auto sysTime = SysTime(Date(1999, 7, 6));
            sysTime.roll!"months"(6);
            assert(sysTime == SysTime(Date(1999, 1, 6)));
            sysTime.roll!"months"(-6);
            assert(sysTime == SysTime(Date(1999, 7, 6)));
        }

        {
            auto sysTime = SysTime(Date(1999, 7, 6));
            sysTime.roll!"months"(27);
            assert(sysTime == SysTime(Date(1999, 10, 6)));
            sysTime.roll!"months"(-28);
            assert(sysTime == SysTime(Date(1999, 6, 6)));
        }

        {
            auto sysTime = SysTime(Date(1999, 5, 31));
            sysTime.roll!"months"(1);
            assert(sysTime == SysTime(Date(1999, 7, 1)));
        }

        {
            auto sysTime = SysTime(Date(1999, 5, 31));
            sysTime.roll!"months"(-1);
            assert(sysTime == SysTime(Date(1999, 5, 1)));
        }

        {
            auto sysTime = SysTime(Date(1999, 2, 28));
            sysTime.roll!"months"(12);
            assert(sysTime == SysTime(Date(1999, 2, 28)));
        }

        {
            auto sysTime = SysTime(Date(2000, 2, 29));
            sysTime.roll!"months"(12);
            assert(sysTime == SysTime(Date(2000, 2, 29)));
        }

        {
            auto sysTime = SysTime(Date(1999, 7, 31));
            sysTime.roll!"months"(1);
            assert(sysTime == SysTime(Date(1999, 8, 31)));
            sysTime.roll!"months"(1);
            assert(sysTime == SysTime(Date(1999, 10, 1)));
        }

        {
            auto sysTime = SysTime(Date(1998, 8, 31));
            sysTime.roll!"months"(13);
            assert(sysTime == SysTime(Date(1998, 10, 1)));
            sysTime.roll!"months"(-13);
            assert(sysTime == SysTime(Date(1998, 9, 1)));
        }

        {
            auto sysTime = SysTime(Date(1997, 12, 31));
            sysTime.roll!"months"(13);
            assert(sysTime == SysTime(Date(1997, 1, 31)));
            sysTime.roll!"months"(-13);
            assert(sysTime == SysTime(Date(1997, 12, 31)));
        }

        {
            auto sysTime = SysTime(Date(1997, 12, 31));
            sysTime.roll!"months"(14);
            assert(sysTime == SysTime(Date(1997, 3, 3)));
            sysTime.roll!"months"(-14);
            assert(sysTime == SysTime(Date(1997, 1, 3)));
        }

        {
            auto sysTime = SysTime(Date(1998, 12, 31));
            sysTime.roll!"months"(14);
            assert(sysTime == SysTime(Date(1998, 3, 3)));
            sysTime.roll!"months"(-14);
            assert(sysTime == SysTime(Date(1998, 1, 3)));
        }

        {
            auto sysTime = SysTime(Date(1999, 12, 31));
            sysTime.roll!"months"(14);
            assert(sysTime == SysTime(Date(1999, 3, 3)));
            sysTime.roll!"months"(-14);
            assert(sysTime == SysTime(Date(1999, 1, 3)));
        }

        {
            auto sysTime = SysTime(DateTime(1999, 7, 6, 12, 2, 7), usecs(5007));
            sysTime.roll!"months"(3);
            assert(sysTime == SysTime(DateTime(1999, 10, 6, 12, 2, 7), usecs(5007)));
            sysTime.roll!"months"(-4);
            assert(sysTime == SysTime(DateTime(1999, 6, 6, 12, 2, 7), usecs(5007)));
        }

        {
            auto sysTime = SysTime(DateTime(1998, 12, 31, 7, 7, 7), hnsecs(422202));
            sysTime.roll!"months"(14);
            assert(sysTime == SysTime(DateTime(1998, 3, 3, 7, 7, 7), hnsecs(422202)));
            sysTime.roll!"months"(-14);
            assert(sysTime == SysTime(DateTime(1998, 1, 3, 7, 7, 7), hnsecs(422202)));
        }

        {
            auto sysTime = SysTime(DateTime(1999, 12, 31, 7, 7, 7), hnsecs(422202));
            sysTime.roll!"months"(14);
            assert(sysTime == SysTime(DateTime(1999, 3, 3, 7, 7, 7), hnsecs(422202)));
            sysTime.roll!"months"(-14);
            assert(sysTime == SysTime(DateTime(1999, 1, 3, 7, 7, 7), hnsecs(422202)));
        }

        // Test B.C.
        {
            auto sysTime = SysTime(Date(-1999, 7, 6));
            sysTime.roll!"months"(3);
            assert(sysTime == SysTime(Date(-1999, 10, 6)));
            sysTime.roll!"months"(-4);
            assert(sysTime == SysTime(Date(-1999, 6, 6)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 7, 6));
            sysTime.roll!"months"(6);
            assert(sysTime == SysTime(Date(-1999, 1, 6)));
            sysTime.roll!"months"(-6);
            assert(sysTime == SysTime(Date(-1999, 7, 6)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 7, 6));
            sysTime.roll!"months"(-27);
            assert(sysTime == SysTime(Date(-1999, 4, 6)));
            sysTime.roll!"months"(28);
            assert(sysTime == SysTime(Date(-1999, 8, 6)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 5, 31));
            sysTime.roll!"months"(1);
            assert(sysTime == SysTime(Date(-1999, 7, 1)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 5, 31));
            sysTime.roll!"months"(-1);
            assert(sysTime == SysTime(Date(-1999, 5, 1)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 2, 28));
            sysTime.roll!"months"(-12);
            assert(sysTime == SysTime(Date(-1999, 2, 28)));
        }

        {
            auto sysTime = SysTime(Date(-2000, 2, 29));
            sysTime.roll!"months"(-12);
            assert(sysTime == SysTime(Date(-2000, 2, 29)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 7, 31));
            sysTime.roll!"months"(1);
            assert(sysTime == SysTime(Date(-1999, 8, 31)));
            sysTime.roll!"months"(1);
            assert(sysTime == SysTime(Date(-1999, 10, 1)));
        }

        {
            auto sysTime = SysTime(Date(-1998, 8, 31));
            sysTime.roll!"months"(13);
            assert(sysTime == SysTime(Date(-1998, 10, 1)));
            sysTime.roll!"months"(-13);
            assert(sysTime == SysTime(Date(-1998, 9, 1)));
        }

        {
            auto sysTime = SysTime(Date(-1997, 12, 31));
            sysTime.roll!"months"(13);
            assert(sysTime == SysTime(Date(-1997, 1, 31)));
            sysTime.roll!"months"(-13);
            assert(sysTime == SysTime(Date(-1997, 12, 31)));
        }

        {
            auto sysTime = SysTime(Date(-1997, 12, 31));
            sysTime.roll!"months"(14);
            assert(sysTime == SysTime(Date(-1997, 3, 3)));
            sysTime.roll!"months"(-14);
            assert(sysTime == SysTime(Date(-1997, 1, 3)));
        }

        {
            auto sysTime = SysTime(Date(-2002, 12, 31));
            sysTime.roll!"months"(14);
            assert(sysTime == SysTime(Date(-2002, 3, 3)));
            sysTime.roll!"months"(-14);
            assert(sysTime == SysTime(Date(-2002, 1, 3)));
        }

        {
            auto sysTime = SysTime(Date(-2001, 12, 31));
            sysTime.roll!"months"(14);
            assert(sysTime == SysTime(Date(-2001, 3, 3)));
            sysTime.roll!"months"(-14);
            assert(sysTime == SysTime(Date(-2001, 1, 3)));
        }

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 0, 0, 0));
            sysTime.roll!"months"(-1);
            assert(sysTime == SysTime(DateTime(1, 12, 1, 0, 0, 0)));
            sysTime.roll!"months"(1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        }

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999));
            sysTime.roll!"months"(-1);
            assert(sysTime == SysTime(DateTime(1, 12, 1, 23, 59, 59), hnsecs(9_999_999)));
            sysTime.roll!"months"(1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 12, 1, 0, 0, 0));
            sysTime.roll!"months"(1);
            assert(sysTime == SysTime(DateTime(0, 1, 1, 0, 0, 0)));
            sysTime.roll!"months"(-1);
            assert(sysTime == SysTime(DateTime(0, 12, 1, 0, 0, 0)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 12, 1, 23, 59, 59), hnsecs(9_999_999));
            sysTime.roll!"months"(1);
            assert(sysTime == SysTime(DateTime(0, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
            sysTime.roll!"months"(-1);
            assert(sysTime == SysTime(DateTime(0, 12, 1, 23, 59, 59), hnsecs(9_999_999)));
        }

        {
            auto sysTime = SysTime(DateTime(-1999, 7, 6, 12, 2, 7), hnsecs(5007));
            sysTime.roll!"months"(3);
            assert(sysTime == SysTime(DateTime(-1999, 10, 6, 12, 2, 7), hnsecs(5007)));
            sysTime.roll!"months"(-4);
            assert(sysTime == SysTime(DateTime(-1999, 6, 6, 12, 2, 7), hnsecs(5007)));
        }

        {
            auto sysTime = SysTime(DateTime(-2002, 12, 31, 7, 7, 7), hnsecs(422202));
            sysTime.roll!"months"(14);
            assert(sysTime == SysTime(DateTime(-2002, 3, 3, 7, 7, 7), hnsecs(422202)));
            sysTime.roll!"months"(-14);
            assert(sysTime == SysTime(DateTime(-2002, 1, 3, 7, 7, 7), hnsecs(422202)));
        }

        {
            auto sysTime = SysTime(DateTime(-2001, 12, 31, 7, 7, 7), hnsecs(422202));
            sysTime.roll!"months"(14);
            assert(sysTime == SysTime(DateTime(-2001, 3, 3, 7, 7, 7), hnsecs(422202)));
            sysTime.roll!"months"(-14);
            assert(sysTime == SysTime(DateTime(-2001, 1, 3, 7, 7, 7), hnsecs(422202)));
        }

        // Test Both
        {
            auto sysTime = SysTime(Date(1, 1, 1));
            sysTime.roll!"months"(-1);
            assert(sysTime == SysTime(Date(1, 12, 1)));
            sysTime.roll!"months"(1);
            assert(sysTime == SysTime(Date(1, 1, 1)));
        }

        {
            auto sysTime = SysTime(Date(4, 1, 1));
            sysTime.roll!"months"(-48);
            assert(sysTime == SysTime(Date(4, 1, 1)));
            sysTime.roll!"months"(48);
            assert(sysTime == SysTime(Date(4, 1, 1)));
        }

        {
            auto sysTime = SysTime(Date(4, 3, 31));
            sysTime.roll!"months"(-49);
            assert(sysTime == SysTime(Date(4, 3, 2)));
            sysTime.roll!"months"(49);
            assert(sysTime == SysTime(Date(4, 4, 2)));
        }

        {
            auto sysTime = SysTime(Date(4, 3, 31));
            sysTime.roll!"months"(-85);
            assert(sysTime == SysTime(Date(4, 3, 2)));
            sysTime.roll!"months"(85);
            assert(sysTime == SysTime(Date(4, 4, 2)));
        }

        {
            auto sysTime = SysTime(Date(-1, 1, 1));
            sysTime.roll!"months"(-1);
            assert(sysTime == SysTime(Date(-1, 12, 1)));
            sysTime.roll!"months"(1);
            assert(sysTime == SysTime(Date(-1, 1, 1)));
        }

        {
            auto sysTime = SysTime(Date(-4, 1, 1));
            sysTime.roll!"months"(-48);
            assert(sysTime == SysTime(Date(-4, 1, 1)));
            sysTime.roll!"months"(48);
            assert(sysTime == SysTime(Date(-4, 1, 1)));
        }

        {
            auto sysTime = SysTime(Date(-4, 3, 31));
            sysTime.roll!"months"(-49);
            assert(sysTime == SysTime(Date(-4, 3, 2)));
            sysTime.roll!"months"(49);
            assert(sysTime == SysTime(Date(-4, 4, 2)));
        }

        {
            auto sysTime = SysTime(Date(-4, 3, 31));
            sysTime.roll!"months"(-85);
            assert(sysTime == SysTime(Date(-4, 3, 2)));
            sysTime.roll!"months"(85);
            assert(sysTime == SysTime(Date(-4, 4, 2)));
        }

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 0, 7, 9), hnsecs(17));
            sysTime.roll!"months"(-1);
            assert(sysTime == SysTime(DateTime(1, 12, 1, 0, 7, 9), hnsecs(17)));
            sysTime.roll!"months"(1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 7, 9), hnsecs(17)));
        }

        {
            auto sysTime = SysTime(DateTime(4, 3, 31, 12, 11, 10), msecs(9));
            sysTime.roll!"months"(-85);
            assert(sysTime == SysTime(DateTime(4, 3, 2, 12, 11, 10), msecs(9)));
            sysTime.roll!"months"(85);
            assert(sysTime == SysTime(DateTime(4, 4, 2, 12, 11, 10), msecs(9)));
        }

        {
            auto sysTime = SysTime(DateTime(-3, 3, 31, 12, 11, 10), msecs(9));
            sysTime.roll!"months"(85);
            assert(sysTime == SysTime(DateTime(-3, 5, 1, 12, 11, 10), msecs(9)));
            sysTime.roll!"months"(-85);
            assert(sysTime == SysTime(DateTime(-3, 4, 1, 12, 11, 10), msecs(9)));
        }

        {
            auto sysTime = SysTime(DateTime(-3, 3, 31, 12, 11, 10), msecs(9));
            sysTime.roll!"months"(85).roll!"months"(-83);
            assert(sysTime == SysTime(DateTime(-3, 6, 1, 12, 11, 10), msecs(9)));
        }

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        static assert(!__traits(compiles, cst.roll!"months"(4)));
        static assert(!__traits(compiles, ist.roll!"months"(4)));

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.roll!"months"(42);
        }
    }

    // Test roll!"months"() with AllowDayOverflow.no
    @safe unittest
    {
        import core.time;
        // Test A.D.
        {
            auto sysTime = SysTime(Date(1999, 7, 6));
            sysTime.roll!"months"(3, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 10, 6)));
            sysTime.roll!"months"(-4, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 6, 6)));
        }

        {
            auto sysTime = SysTime(Date(1999, 7, 6));
            sysTime.roll!"months"(6, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 1, 6)));
            sysTime.roll!"months"(-6, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 7, 6)));
        }

        {
            auto sysTime = SysTime(Date(1999, 7, 6));
            sysTime.roll!"months"(27, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 10, 6)));
            sysTime.roll!"months"(-28, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 6, 6)));
        }

        {
            auto sysTime = SysTime(Date(1999, 5, 31));
            sysTime.roll!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 6, 30)));
        }

        {
            auto sysTime = SysTime(Date(1999, 5, 31));
            sysTime.roll!"months"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 4, 30)));
        }

        {
            auto sysTime = SysTime(Date(1999, 2, 28));
            sysTime.roll!"months"(12, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 2, 28)));
        }

        {
            auto sysTime = SysTime(Date(2000, 2, 29));
            sysTime.roll!"months"(12, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(2000, 2, 29)));
        }

        {
            auto sysTime = SysTime(Date(1999, 7, 31));
            sysTime.roll!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 8, 31)));
            sysTime.roll!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 9, 30)));
        }

        {
            auto sysTime = SysTime(Date(1998, 8, 31));
            sysTime.roll!"months"(13, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1998, 9, 30)));
            sysTime.roll!"months"(-13, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1998, 8, 30)));
        }

        {
            auto sysTime = SysTime(Date(1997, 12, 31));
            sysTime.roll!"months"(13, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1997, 1, 31)));
            sysTime.roll!"months"(-13, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1997, 12, 31)));
        }

        {
            auto sysTime = SysTime(Date(1997, 12, 31));
            sysTime.roll!"months"(14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1997, 2, 28)));
            sysTime.roll!"months"(-14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1997, 12, 28)));
        }

        {
            auto sysTime = SysTime(Date(1998, 12, 31));
            sysTime.roll!"months"(14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1998, 2, 28)));
            sysTime.roll!"months"(-14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1998, 12, 28)));
        }

        {
            auto sysTime = SysTime(Date(1999, 12, 31));
            sysTime.roll!"months"(14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 2, 28)));
            sysTime.roll!"months"(-14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1999, 12, 28)));
        }

        {
            auto sysTime = SysTime(DateTime(1999, 7, 6, 12, 2, 7), usecs(5007));
            sysTime.roll!"months"(3, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1999, 10, 6, 12, 2, 7), usecs(5007)));
            sysTime.roll!"months"(-4, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1999, 6, 6, 12, 2, 7), usecs(5007)));
        }

        {
            auto sysTime = SysTime(DateTime(1998, 12, 31, 7, 7, 7), hnsecs(422202));
            sysTime.roll!"months"(14, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1998, 2, 28, 7, 7, 7), hnsecs(422202)));
            sysTime.roll!"months"(-14, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1998, 12, 28, 7, 7, 7), hnsecs(422202)));
        }

        {
            auto sysTime = SysTime(DateTime(1999, 12, 31, 7, 7, 7), hnsecs(422202));
            sysTime.roll!"months"(14, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1999, 2, 28, 7, 7, 7), hnsecs(422202)));
            sysTime.roll!"months"(-14, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1999, 12, 28, 7, 7, 7), hnsecs(422202)));
        }

        // Test B.C.
        {
            auto sysTime = SysTime(Date(-1999, 7, 6));
            sysTime.roll!"months"(3, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1999, 10, 6)));
            sysTime.roll!"months"(-4, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1999, 6, 6)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 7, 6));
            sysTime.roll!"months"(6, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1999, 1, 6)));
            sysTime.roll!"months"(-6, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1999, 7, 6)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 7, 6));
            sysTime.roll!"months"(-27, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1999, 4, 6)));
            sysTime.roll!"months"(28, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1999, 8, 6)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 5, 31));
            sysTime.roll!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1999, 6, 30)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 5, 31));
            sysTime.roll!"months"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1999, 4, 30)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 2, 28));
            sysTime.roll!"months"(-12, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1999, 2, 28)));
        }

        {
            auto sysTime = SysTime(Date(-2000, 2, 29));
            sysTime.roll!"months"(-12, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-2000, 2, 29)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 7, 31));
            sysTime.roll!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1999, 8, 31)));
            sysTime.roll!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1999, 9, 30)));
        }

        {
            auto sysTime = SysTime(Date(-1998, 8, 31));
            sysTime.roll!"months"(13, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1998, 9, 30)));
            sysTime.roll!"months"(-13, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1998, 8, 30)));
        }

        {
            auto sysTime = SysTime(Date(-1997, 12, 31));
            sysTime.roll!"months"(13, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1997, 1, 31)));
            sysTime.roll!"months"(-13, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1997, 12, 31)));
        }

        {
            auto sysTime = SysTime(Date(-1997, 12, 31));
            sysTime.roll!"months"(14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1997, 2, 28)));
            sysTime.roll!"months"(-14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1997, 12, 28)));
        }

        {
            auto sysTime = SysTime(Date(-2002, 12, 31));
            sysTime.roll!"months"(14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-2002, 2, 28)));
            sysTime.roll!"months"(-14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-2002, 12, 28)));
        }

        {
            auto sysTime = SysTime(Date(-2001, 12, 31));
            sysTime.roll!"months"(14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-2001, 2, 28)));
            sysTime.roll!"months"(-14, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-2001, 12, 28)));
        }

        {
            auto sysTime = SysTime(DateTime(-1999, 7, 6, 12, 2, 7), usecs(5007));
            sysTime.roll!"months"(3, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-1999, 10, 6, 12, 2, 7), usecs(5007)));
            sysTime.roll!"months"(-4, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-1999, 6, 6, 12, 2, 7), usecs(5007)));
        }

        {
            auto sysTime = SysTime(DateTime(-2002, 12, 31, 7, 7, 7), hnsecs(422202));
            sysTime.roll!"months"(14, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-2002, 2, 28, 7, 7, 7), hnsecs(422202)));
            sysTime.roll!"months"(-14, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-2002, 12, 28, 7, 7, 7), hnsecs(422202)));
        }

        {
            auto sysTime = SysTime(DateTime(-2001, 12, 31, 7, 7, 7), hnsecs(422202));
            sysTime.roll!"months"(14, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-2001, 2, 28, 7, 7, 7), hnsecs(422202)));
            sysTime.roll!"months"(-14, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-2001, 12, 28, 7, 7, 7), hnsecs(422202)));
        }

        // Test Both
        {
            auto sysTime = SysTime(Date(1, 1, 1));
            sysTime.roll!"months"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1, 12, 1)));
            sysTime.roll!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(1, 1, 1)));
        }

        {
            auto sysTime = SysTime(Date(4, 1, 1));
            sysTime.roll!"months"(-48, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(4, 1, 1)));
            sysTime.roll!"months"(48, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(4, 1, 1)));
        }

        {
            auto sysTime = SysTime(Date(4, 3, 31));
            sysTime.roll!"months"(-49, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(4, 2, 29)));
            sysTime.roll!"months"(49, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(4, 3, 29)));
        }

        {
            auto sysTime = SysTime(Date(4, 3, 31));
            sysTime.roll!"months"(-85, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(4, 2, 29)));
            sysTime.roll!"months"(85, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(4, 3, 29)));
        }

        {
            auto sysTime = SysTime(Date(-1, 1, 1));
            sysTime.roll!"months"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1, 12, 1)));
            sysTime.roll!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-1, 1, 1)));
        }

        {
            auto sysTime = SysTime(Date(-4, 1, 1));
            sysTime.roll!"months"(-48, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-4, 1, 1)));
            sysTime.roll!"months"(48, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-4, 1, 1)));
        }

        {
            auto sysTime = SysTime(Date(-4, 3, 31));
            sysTime.roll!"months"(-49, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-4, 2, 29)));
            sysTime.roll!"months"(49, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-4, 3, 29)));
        }

        {
            auto sysTime = SysTime(Date(-4, 3, 31));
            sysTime.roll!"months"(-85, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-4, 2, 29)));
            sysTime.roll!"months"(85, AllowDayOverflow.no);
            assert(sysTime == SysTime(Date(-4, 3, 29)));
        }

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 0, 0, 0));
            sysTime.roll!"months"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1, 12, 1, 0, 0, 0)));
            sysTime.roll!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        }

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999));
            sysTime.roll!"months"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1, 12, 1, 23, 59, 59), hnsecs(9_999_999)));
            sysTime.roll!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 12, 1, 0, 0, 0));
            sysTime.roll!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(0, 1, 1, 0, 0, 0)));
            sysTime.roll!"months"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(0, 12, 1, 0, 0, 0)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 12, 1, 23, 59, 59), hnsecs(9_999_999));
            sysTime.roll!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(0, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
            sysTime.roll!"months"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(0, 12, 1, 23, 59, 59), hnsecs(9_999_999)));
        }

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 0, 7, 9), hnsecs(17));
            sysTime.roll!"months"(-1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1, 12, 1, 0, 7, 9), hnsecs(17)));
            sysTime.roll!"months"(1, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 7, 9), hnsecs(17)));
        }

        {
            auto sysTime = SysTime(DateTime(4, 3, 31, 12, 11, 10), msecs(9));
            sysTime.roll!"months"(-85, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(4, 2, 29, 12, 11, 10), msecs(9)));
            sysTime.roll!"months"(85, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(4, 3, 29, 12, 11, 10), msecs(9)));
        }

        {
            auto sysTime = SysTime(DateTime(-3, 3, 31, 12, 11, 10), msecs(9));
            sysTime.roll!"months"(85, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-3, 4, 30, 12, 11, 10), msecs(9)));
            sysTime.roll!"months"(-85, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-3, 3, 30, 12, 11, 10), msecs(9)));
        }

        {
            auto sysTime = SysTime(DateTime(-3, 3, 31, 12, 11, 10), msecs(9));
            sysTime.roll!"months"(85, AllowDayOverflow.no).roll!"months"(-83, AllowDayOverflow.no);
            assert(sysTime == SysTime(DateTime(-3, 5, 30, 12, 11, 10), msecs(9)));
        }
    }


    /++
        Adds the given number of units to this $(LREF SysTime). A negative number
        will subtract.

        The difference between rolling and adding is that rolling does not
        affect larger units. For instance, rolling a $(LREF SysTime) one
        year's worth of days gets the exact same $(LREF SysTime).

        Accepted units are `"days"`, `"minutes"`, `"hours"`,
        `"minutes"`, `"seconds"`, `"msecs"`, `"usecs"`, and
        `"hnsecs"`.

        Note that when rolling msecs, usecs or hnsecs, they all add up to a
        second. So, for example, rolling 1000 msecs is exactly the same as
        rolling 100,000 usecs.

        Params:
            units = The units to add.
            value = The number of $(D_PARAM units) to add to this
                    $(LREF SysTime).
      +/
    ref SysTime roll(string units)(long value) @safe nothrow scope
    if (units == "days")
    {
        auto hnsecs = adjTime;
        auto gdays = splitUnitsFromHNSecs!"days"(hnsecs) + 1;

        if (hnsecs < 0)
        {
            hnsecs += convert!("hours", "hnsecs")(24);
            --gdays;
        }

        auto date = Date(cast(int) gdays);
        date.roll!"days"(value);
        gdays = date.dayOfGregorianCal - 1;

        if (gdays < 0)
        {
            hnsecs -= convert!("hours", "hnsecs")(24);
            ++gdays;
        }

        immutable newDaysHNSecs = convert!("days", "hnsecs")(gdays);
        adjTime = newDaysHNSecs + hnsecs;
        return  this;
    }

    ///
    @safe unittest
    {
        import core.time : msecs, hnsecs;
        import std.datetime.date : DateTime;

        auto st1 = SysTime(DateTime(2010, 1, 1, 11, 23, 12));
        st1.roll!"days"(1);
        assert(st1 == SysTime(DateTime(2010, 1, 2, 11, 23, 12)));
        st1.roll!"days"(365);
        assert(st1 == SysTime(DateTime(2010, 1, 26, 11, 23, 12)));
        st1.roll!"days"(-32);
        assert(st1 == SysTime(DateTime(2010, 1, 25, 11, 23, 12)));

        auto st2 = SysTime(DateTime(2010, 7, 4, 12, 0, 0));
        st2.roll!"hours"(1);
        assert(st2 == SysTime(DateTime(2010, 7, 4, 13, 0, 0)));

        auto st3 = SysTime(DateTime(2010, 2, 12, 12, 0, 0));
        st3.roll!"hours"(-1);
        assert(st3 == SysTime(DateTime(2010, 2, 12, 11, 0, 0)));

        auto st4 = SysTime(DateTime(2009, 12, 31, 0, 0, 0));
        st4.roll!"minutes"(1);
        assert(st4 == SysTime(DateTime(2009, 12, 31, 0, 1, 0)));

        auto st5 = SysTime(DateTime(2010, 1, 1, 0, 0, 0));
        st5.roll!"minutes"(-1);
        assert(st5 == SysTime(DateTime(2010, 1, 1, 0, 59, 0)));

        auto st6 = SysTime(DateTime(2009, 12, 31, 0, 0, 0));
        st6.roll!"seconds"(1);
        assert(st6 == SysTime(DateTime(2009, 12, 31, 0, 0, 1)));

        auto st7 = SysTime(DateTime(2010, 1, 1, 0, 0, 0));
        st7.roll!"seconds"(-1);
        assert(st7 == SysTime(DateTime(2010, 1, 1, 0, 0, 59)));

        auto dt = DateTime(2010, 1, 1, 0, 0, 0);
        auto st8 = SysTime(dt);
        st8.roll!"msecs"(1);
        assert(st8 == SysTime(dt, msecs(1)));

        auto st9 = SysTime(dt);
        st9.roll!"msecs"(-1);
        assert(st9 == SysTime(dt, msecs(999)));

        auto st10 = SysTime(dt);
        st10.roll!"hnsecs"(1);
        assert(st10 == SysTime(dt, hnsecs(1)));

        auto st11 = SysTime(dt);
        st11.roll!"hnsecs"(-1);
        assert(st11 == SysTime(dt, hnsecs(9_999_999)));
    }

    @safe unittest
    {
        import core.time;
        // Test A.D.
        {
            auto sysTime = SysTime(Date(1999, 2, 28));
            sysTime.roll!"days"(1);
            assert(sysTime == SysTime(Date(1999, 2, 1)));
            sysTime.roll!"days"(-1);
            assert(sysTime == SysTime(Date(1999, 2, 28)));
        }

        {
            auto sysTime = SysTime(Date(2000, 2, 28));
            sysTime.roll!"days"(1);
            assert(sysTime == SysTime(Date(2000, 2, 29)));
            sysTime.roll!"days"(1);
            assert(sysTime == SysTime(Date(2000, 2, 1)));
            sysTime.roll!"days"(-1);
            assert(sysTime == SysTime(Date(2000, 2, 29)));
        }

        {
            auto sysTime = SysTime(Date(1999, 6, 30));
            sysTime.roll!"days"(1);
            assert(sysTime == SysTime(Date(1999, 6, 1)));
            sysTime.roll!"days"(-1);
            assert(sysTime == SysTime(Date(1999, 6, 30)));
        }

        {
            auto sysTime = SysTime(Date(1999, 7, 31));
            sysTime.roll!"days"(1);
            assert(sysTime == SysTime(Date(1999, 7, 1)));
            sysTime.roll!"days"(-1);
            assert(sysTime == SysTime(Date(1999, 7, 31)));
        }

        {
            auto sysTime = SysTime(Date(1999, 1, 1));
            sysTime.roll!"days"(-1);
            assert(sysTime == SysTime(Date(1999, 1, 31)));
            sysTime.roll!"days"(1);
            assert(sysTime == SysTime(Date(1999, 1, 1)));
        }

        {
            auto sysTime = SysTime(Date(1999, 7, 6));
            sysTime.roll!"days"(9);
            assert(sysTime == SysTime(Date(1999, 7, 15)));
            sysTime.roll!"days"(-11);
            assert(sysTime == SysTime(Date(1999, 7, 4)));
            sysTime.roll!"days"(30);
            assert(sysTime == SysTime(Date(1999, 7, 3)));
            sysTime.roll!"days"(-3);
            assert(sysTime == SysTime(Date(1999, 7, 31)));
        }

        {
            auto sysTime = SysTime(Date(1999, 7, 6));
            sysTime.roll!"days"(365);
            assert(sysTime == SysTime(Date(1999, 7, 30)));
            sysTime.roll!"days"(-365);
            assert(sysTime == SysTime(Date(1999, 7, 6)));
            sysTime.roll!"days"(366);
            assert(sysTime == SysTime(Date(1999, 7, 31)));
            sysTime.roll!"days"(730);
            assert(sysTime == SysTime(Date(1999, 7, 17)));
            sysTime.roll!"days"(-1096);
            assert(sysTime == SysTime(Date(1999, 7, 6)));
        }

        {
            auto sysTime = SysTime(Date(1999, 2, 6));
            sysTime.roll!"days"(365);
            assert(sysTime == SysTime(Date(1999, 2, 7)));
            sysTime.roll!"days"(-365);
            assert(sysTime == SysTime(Date(1999, 2, 6)));
            sysTime.roll!"days"(366);
            assert(sysTime == SysTime(Date(1999, 2, 8)));
            sysTime.roll!"days"(730);
            assert(sysTime == SysTime(Date(1999, 2, 10)));
            sysTime.roll!"days"(-1096);
            assert(sysTime == SysTime(Date(1999, 2, 6)));
        }

        {
            auto sysTime = SysTime(DateTime(1999, 2, 28, 7, 9, 2), usecs(234578));
            sysTime.roll!"days"(1);
            assert(sysTime == SysTime(DateTime(1999, 2, 1, 7, 9, 2), usecs(234578)));
            sysTime.roll!"days"(-1);
            assert(sysTime == SysTime(DateTime(1999, 2, 28, 7, 9, 2), usecs(234578)));
        }

        {
            auto sysTime = SysTime(DateTime(1999, 7, 6, 7, 9, 2), usecs(234578));
            sysTime.roll!"days"(9);
            assert(sysTime == SysTime(DateTime(1999, 7, 15, 7, 9, 2), usecs(234578)));
            sysTime.roll!"days"(-11);
            assert(sysTime == SysTime(DateTime(1999, 7, 4, 7, 9, 2), usecs(234578)));
            sysTime.roll!"days"(30);
            assert(sysTime == SysTime(DateTime(1999, 7, 3, 7, 9, 2), usecs(234578)));
            sysTime.roll!"days"(-3);
            assert(sysTime == SysTime(DateTime(1999, 7, 31, 7, 9, 2), usecs(234578)));
        }

        // Test B.C.
        {
            auto sysTime = SysTime(Date(-1999, 2, 28));
            sysTime.roll!"days"(1);
            assert(sysTime == SysTime(Date(-1999, 2, 1)));
            sysTime.roll!"days"(-1);
            assert(sysTime == SysTime(Date(-1999, 2, 28)));
        }

        {
            auto sysTime = SysTime(Date(-2000, 2, 28));
            sysTime.roll!"days"(1);
            assert(sysTime == SysTime(Date(-2000, 2, 29)));
            sysTime.roll!"days"(1);
            assert(sysTime == SysTime(Date(-2000, 2, 1)));
            sysTime.roll!"days"(-1);
            assert(sysTime == SysTime(Date(-2000, 2, 29)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 6, 30));
            sysTime.roll!"days"(1);
            assert(sysTime == SysTime(Date(-1999, 6, 1)));
            sysTime.roll!"days"(-1);
            assert(sysTime == SysTime(Date(-1999, 6, 30)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 7, 31));
            sysTime.roll!"days"(1);
            assert(sysTime == SysTime(Date(-1999, 7, 1)));
            sysTime.roll!"days"(-1);
            assert(sysTime == SysTime(Date(-1999, 7, 31)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 1, 1));
            sysTime.roll!"days"(-1);
            assert(sysTime == SysTime(Date(-1999, 1, 31)));
            sysTime.roll!"days"(1);
            assert(sysTime == SysTime(Date(-1999, 1, 1)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 7, 6));
            sysTime.roll!"days"(9);
            assert(sysTime == SysTime(Date(-1999, 7, 15)));
            sysTime.roll!"days"(-11);
            assert(sysTime == SysTime(Date(-1999, 7, 4)));
            sysTime.roll!"days"(30);
            assert(sysTime == SysTime(Date(-1999, 7, 3)));
            sysTime.roll!"days"(-3);
            assert(sysTime == SysTime(Date(-1999, 7, 31)));
        }

        {
            auto sysTime = SysTime(Date(-1999, 7, 6));
            sysTime.roll!"days"(365);
            assert(sysTime == SysTime(Date(-1999, 7, 30)));
            sysTime.roll!"days"(-365);
            assert(sysTime == SysTime(Date(-1999, 7, 6)));
            sysTime.roll!"days"(366);
            assert(sysTime == SysTime(Date(-1999, 7, 31)));
            sysTime.roll!"days"(730);
            assert(sysTime == SysTime(Date(-1999, 7, 17)));
            sysTime.roll!"days"(-1096);
            assert(sysTime == SysTime(Date(-1999, 7, 6)));
        }

        {
            auto sysTime = SysTime(DateTime(-1999, 2, 28, 7, 9, 2), usecs(234578));
            sysTime.roll!"days"(1);
            assert(sysTime == SysTime(DateTime(-1999, 2, 1, 7, 9, 2), usecs(234578)));
            sysTime.roll!"days"(-1);
            assert(sysTime == SysTime(DateTime(-1999, 2, 28, 7, 9, 2), usecs(234578)));
        }

        {
            auto sysTime = SysTime(DateTime(-1999, 7, 6, 7, 9, 2), usecs(234578));
            sysTime.roll!"days"(9);
            assert(sysTime == SysTime(DateTime(-1999, 7, 15, 7, 9, 2), usecs(234578)));
            sysTime.roll!"days"(-11);
            assert(sysTime == SysTime(DateTime(-1999, 7, 4, 7, 9, 2), usecs(234578)));
            sysTime.roll!"days"(30);
            assert(sysTime == SysTime(DateTime(-1999, 7, 3, 7, 9, 2), usecs(234578)));
            sysTime.roll!"days"(-3);
        }

        // Test Both
        {
            auto sysTime = SysTime(Date(1, 7, 6));
            sysTime.roll!"days"(-365);
            assert(sysTime == SysTime(Date(1, 7, 13)));
            sysTime.roll!"days"(365);
            assert(sysTime == SysTime(Date(1, 7, 6)));
            sysTime.roll!"days"(-731);
            assert(sysTime == SysTime(Date(1, 7, 19)));
            sysTime.roll!"days"(730);
            assert(sysTime == SysTime(Date(1, 7, 5)));
        }

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 0, 0, 0));
            sysTime.roll!"days"(-1);
            assert(sysTime == SysTime(DateTime(1, 1, 31, 0, 0, 0)));
            sysTime.roll!"days"(1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        }

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999));
            sysTime.roll!"days"(-1);
            assert(sysTime == SysTime(DateTime(1, 1, 31, 23, 59, 59), hnsecs(9_999_999)));
            sysTime.roll!"days"(1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 12, 31, 0, 0, 0));
            sysTime.roll!"days"(1);
            assert(sysTime == SysTime(DateTime(0, 12, 1, 0, 0, 0)));
            sysTime.roll!"days"(-1);
            assert(sysTime == SysTime(DateTime(0, 12, 31, 0, 0, 0)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999));
            sysTime.roll!"days"(1);
            assert(sysTime == SysTime(DateTime(0, 12, 1, 23, 59, 59), hnsecs(9_999_999)));
            sysTime.roll!"days"(-1);
            assert(sysTime == SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999)));
        }

        {
            auto sysTime = SysTime(DateTime(1, 7, 6, 13, 13, 9), msecs(22));
            sysTime.roll!"days"(-365);
            assert(sysTime == SysTime(DateTime(1, 7, 13, 13, 13, 9), msecs(22)));
            sysTime.roll!"days"(365);
            assert(sysTime == SysTime(DateTime(1, 7, 6, 13, 13, 9), msecs(22)));
            sysTime.roll!"days"(-731);
            assert(sysTime == SysTime(DateTime(1, 7, 19, 13, 13, 9), msecs(22)));
            sysTime.roll!"days"(730);
            assert(sysTime == SysTime(DateTime(1, 7, 5, 13, 13, 9), msecs(22)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 7, 6, 13, 13, 9), msecs(22));
            sysTime.roll!"days"(-365);
            assert(sysTime == SysTime(DateTime(0, 7, 13, 13, 13, 9), msecs(22)));
            sysTime.roll!"days"(365);
            assert(sysTime == SysTime(DateTime(0, 7, 6, 13, 13, 9), msecs(22)));
            sysTime.roll!"days"(-731);
            assert(sysTime == SysTime(DateTime(0, 7, 19, 13, 13, 9), msecs(22)));
            sysTime.roll!"days"(730);
            assert(sysTime == SysTime(DateTime(0, 7, 5, 13, 13, 9), msecs(22)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 7, 6, 13, 13, 9), msecs(22));
            sysTime.roll!"days"(-365).roll!"days"(362).roll!"days"(-12).roll!"days"(730);
            assert(sysTime == SysTime(DateTime(0, 7, 8, 13, 13, 9), msecs(22)));
        }

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        static assert(!__traits(compiles, cst.roll!"days"(4)));
        static assert(!__traits(compiles, ist.roll!"days"(4)));

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.roll!"days"(42);
        }
    }


    // Shares documentation with "days" version.
    ref SysTime roll(string units)(long value) @safe nothrow scope
    if (units == "hours" || units == "minutes" || units == "seconds")
    {
        try
        {
            auto hnsecs = adjTime;
            auto days = splitUnitsFromHNSecs!"days"(hnsecs) + 1;

            if (hnsecs < 0)
            {
                hnsecs += convert!("hours", "hnsecs")(24);
                --days;
            }

            immutable hour = splitUnitsFromHNSecs!"hours"(hnsecs);
            immutable minute = splitUnitsFromHNSecs!"minutes"(hnsecs);
            immutable second = splitUnitsFromHNSecs!"seconds"(hnsecs);

            auto dateTime = DateTime(Date(cast(int) days), TimeOfDay(cast(int) hour,
                                          cast(int) minute, cast(int) second));
            dateTime.roll!units(value);
            --days;

            hnsecs += convert!("hours", "hnsecs")(dateTime.hour);
            hnsecs += convert!("minutes", "hnsecs")(dateTime.minute);
            hnsecs += convert!("seconds", "hnsecs")(dateTime.second);

            if (days < 0)
            {
                hnsecs -= convert!("hours", "hnsecs")(24);
                ++days;
            }

            immutable newDaysHNSecs = convert!("days", "hnsecs")(days);
            adjTime = newDaysHNSecs + hnsecs;
            return this;
        }
        catch (Exception e)
            assert(0, "Either DateTime's constructor or TimeOfDay's constructor threw.");
    }

    // Test roll!"hours"().
    @safe unittest
    {
        import core.time;
        static void testST(SysTime orig, int hours, SysTime expected, size_t line = __LINE__) @safe
        {
            orig.roll!"hours"(hours);
            if (orig != expected)
                throw new AssertError(format("Failed. actual [%s] != expected [%s]", orig, expected), __FILE__, line);
        }

        // Test A.D.
        immutable d = msecs(45);
        auto beforeAD = SysTime(DateTime(1999, 7, 6, 12, 30, 33), d);
        testST(beforeAD, 0, SysTime(DateTime(1999, 7, 6, 12, 30, 33), d));
        testST(beforeAD, 1, SysTime(DateTime(1999, 7, 6, 13, 30, 33), d));
        testST(beforeAD, 2, SysTime(DateTime(1999, 7, 6, 14, 30, 33), d));
        testST(beforeAD, 3, SysTime(DateTime(1999, 7, 6, 15, 30, 33), d));
        testST(beforeAD, 4, SysTime(DateTime(1999, 7, 6, 16, 30, 33), d));
        testST(beforeAD, 5, SysTime(DateTime(1999, 7, 6, 17, 30, 33), d));
        testST(beforeAD, 6, SysTime(DateTime(1999, 7, 6, 18, 30, 33), d));
        testST(beforeAD, 7, SysTime(DateTime(1999, 7, 6, 19, 30, 33), d));
        testST(beforeAD, 8, SysTime(DateTime(1999, 7, 6, 20, 30, 33), d));
        testST(beforeAD, 9, SysTime(DateTime(1999, 7, 6, 21, 30, 33), d));
        testST(beforeAD, 10, SysTime(DateTime(1999, 7, 6, 22, 30, 33), d));
        testST(beforeAD, 11, SysTime(DateTime(1999, 7, 6, 23, 30, 33), d));
        testST(beforeAD, 12, SysTime(DateTime(1999, 7, 6, 0, 30, 33), d));
        testST(beforeAD, 13, SysTime(DateTime(1999, 7, 6, 1, 30, 33), d));
        testST(beforeAD, 14, SysTime(DateTime(1999, 7, 6, 2, 30, 33), d));
        testST(beforeAD, 15, SysTime(DateTime(1999, 7, 6, 3, 30, 33), d));
        testST(beforeAD, 16, SysTime(DateTime(1999, 7, 6, 4, 30, 33), d));
        testST(beforeAD, 17, SysTime(DateTime(1999, 7, 6, 5, 30, 33), d));
        testST(beforeAD, 18, SysTime(DateTime(1999, 7, 6, 6, 30, 33), d));
        testST(beforeAD, 19, SysTime(DateTime(1999, 7, 6, 7, 30, 33), d));
        testST(beforeAD, 20, SysTime(DateTime(1999, 7, 6, 8, 30, 33), d));
        testST(beforeAD, 21, SysTime(DateTime(1999, 7, 6, 9, 30, 33), d));
        testST(beforeAD, 22, SysTime(DateTime(1999, 7, 6, 10, 30, 33), d));
        testST(beforeAD, 23, SysTime(DateTime(1999, 7, 6, 11, 30, 33), d));
        testST(beforeAD, 24, SysTime(DateTime(1999, 7, 6, 12, 30, 33), d));
        testST(beforeAD, 25, SysTime(DateTime(1999, 7, 6, 13, 30, 33), d));
        testST(beforeAD, 50, SysTime(DateTime(1999, 7, 6, 14, 30, 33), d));
        testST(beforeAD, 10_000, SysTime(DateTime(1999, 7, 6, 4, 30, 33), d));

        testST(beforeAD, -1, SysTime(DateTime(1999, 7, 6, 11, 30, 33), d));
        testST(beforeAD, -2, SysTime(DateTime(1999, 7, 6, 10, 30, 33), d));
        testST(beforeAD, -3, SysTime(DateTime(1999, 7, 6, 9, 30, 33), d));
        testST(beforeAD, -4, SysTime(DateTime(1999, 7, 6, 8, 30, 33), d));
        testST(beforeAD, -5, SysTime(DateTime(1999, 7, 6, 7, 30, 33), d));
        testST(beforeAD, -6, SysTime(DateTime(1999, 7, 6, 6, 30, 33), d));
        testST(beforeAD, -7, SysTime(DateTime(1999, 7, 6, 5, 30, 33), d));
        testST(beforeAD, -8, SysTime(DateTime(1999, 7, 6, 4, 30, 33), d));
        testST(beforeAD, -9, SysTime(DateTime(1999, 7, 6, 3, 30, 33), d));
        testST(beforeAD, -10, SysTime(DateTime(1999, 7, 6, 2, 30, 33), d));
        testST(beforeAD, -11, SysTime(DateTime(1999, 7, 6, 1, 30, 33), d));
        testST(beforeAD, -12, SysTime(DateTime(1999, 7, 6, 0, 30, 33), d));
        testST(beforeAD, -13, SysTime(DateTime(1999, 7, 6, 23, 30, 33), d));
        testST(beforeAD, -14, SysTime(DateTime(1999, 7, 6, 22, 30, 33), d));
        testST(beforeAD, -15, SysTime(DateTime(1999, 7, 6, 21, 30, 33), d));
        testST(beforeAD, -16, SysTime(DateTime(1999, 7, 6, 20, 30, 33), d));
        testST(beforeAD, -17, SysTime(DateTime(1999, 7, 6, 19, 30, 33), d));
        testST(beforeAD, -18, SysTime(DateTime(1999, 7, 6, 18, 30, 33), d));
        testST(beforeAD, -19, SysTime(DateTime(1999, 7, 6, 17, 30, 33), d));
        testST(beforeAD, -20, SysTime(DateTime(1999, 7, 6, 16, 30, 33), d));
        testST(beforeAD, -21, SysTime(DateTime(1999, 7, 6, 15, 30, 33), d));
        testST(beforeAD, -22, SysTime(DateTime(1999, 7, 6, 14, 30, 33), d));
        testST(beforeAD, -23, SysTime(DateTime(1999, 7, 6, 13, 30, 33), d));
        testST(beforeAD, -24, SysTime(DateTime(1999, 7, 6, 12, 30, 33), d));
        testST(beforeAD, -25, SysTime(DateTime(1999, 7, 6, 11, 30, 33), d));
        testST(beforeAD, -50, SysTime(DateTime(1999, 7, 6, 10, 30, 33), d));
        testST(beforeAD, -10_000, SysTime(DateTime(1999, 7, 6, 20, 30, 33), d));

        testST(SysTime(DateTime(1999, 7, 6, 0, 30, 33), d), 1, SysTime(DateTime(1999, 7, 6, 1, 30, 33), d));
        testST(SysTime(DateTime(1999, 7, 6, 0, 30, 33), d), 0, SysTime(DateTime(1999, 7, 6, 0, 30, 33), d));
        testST(SysTime(DateTime(1999, 7, 6, 0, 30, 33), d), -1, SysTime(DateTime(1999, 7, 6, 23, 30, 33), d));

        testST(SysTime(DateTime(1999, 7, 6, 23, 30, 33), d), 1, SysTime(DateTime(1999, 7, 6, 0, 30, 33), d));
        testST(SysTime(DateTime(1999, 7, 6, 23, 30, 33), d), 0, SysTime(DateTime(1999, 7, 6, 23, 30, 33), d));
        testST(SysTime(DateTime(1999, 7, 6, 23, 30, 33), d), -1, SysTime(DateTime(1999, 7, 6, 22, 30, 33), d));

        testST(SysTime(DateTime(1999, 7, 31, 23, 30, 33), d), 1, SysTime(DateTime(1999, 7, 31, 0, 30, 33), d));
        testST(SysTime(DateTime(1999, 8, 1, 0, 30, 33), d), -1, SysTime(DateTime(1999, 8, 1, 23, 30, 33), d));

        testST(SysTime(DateTime(1999, 12, 31, 23, 30, 33), d), 1, SysTime(DateTime(1999, 12, 31, 0, 30, 33), d));
        testST(SysTime(DateTime(2000, 1, 1, 0, 30, 33), d), -1, SysTime(DateTime(2000, 1, 1, 23, 30, 33), d));

        testST(SysTime(DateTime(1999, 2, 28, 23, 30, 33), d), 25, SysTime(DateTime(1999, 2, 28, 0, 30, 33), d));
        testST(SysTime(DateTime(1999, 3, 2, 0, 30, 33), d), -25, SysTime(DateTime(1999, 3, 2, 23, 30, 33), d));

        testST(SysTime(DateTime(2000, 2, 28, 23, 30, 33), d), 25, SysTime(DateTime(2000, 2, 28, 0, 30, 33), d));
        testST(SysTime(DateTime(2000, 3, 1, 0, 30, 33), d), -25, SysTime(DateTime(2000, 3, 1, 23, 30, 33), d));

        // Test B.C.
        auto beforeBC = SysTime(DateTime(-1999, 7, 6, 12, 30, 33), d);
        testST(beforeBC, 0, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), d));
        testST(beforeBC, 1, SysTime(DateTime(-1999, 7, 6, 13, 30, 33), d));
        testST(beforeBC, 2, SysTime(DateTime(-1999, 7, 6, 14, 30, 33), d));
        testST(beforeBC, 3, SysTime(DateTime(-1999, 7, 6, 15, 30, 33), d));
        testST(beforeBC, 4, SysTime(DateTime(-1999, 7, 6, 16, 30, 33), d));
        testST(beforeBC, 5, SysTime(DateTime(-1999, 7, 6, 17, 30, 33), d));
        testST(beforeBC, 6, SysTime(DateTime(-1999, 7, 6, 18, 30, 33), d));
        testST(beforeBC, 7, SysTime(DateTime(-1999, 7, 6, 19, 30, 33), d));
        testST(beforeBC, 8, SysTime(DateTime(-1999, 7, 6, 20, 30, 33), d));
        testST(beforeBC, 9, SysTime(DateTime(-1999, 7, 6, 21, 30, 33), d));
        testST(beforeBC, 10, SysTime(DateTime(-1999, 7, 6, 22, 30, 33), d));
        testST(beforeBC, 11, SysTime(DateTime(-1999, 7, 6, 23, 30, 33), d));
        testST(beforeBC, 12, SysTime(DateTime(-1999, 7, 6, 0, 30, 33), d));
        testST(beforeBC, 13, SysTime(DateTime(-1999, 7, 6, 1, 30, 33), d));
        testST(beforeBC, 14, SysTime(DateTime(-1999, 7, 6, 2, 30, 33), d));
        testST(beforeBC, 15, SysTime(DateTime(-1999, 7, 6, 3, 30, 33), d));
        testST(beforeBC, 16, SysTime(DateTime(-1999, 7, 6, 4, 30, 33), d));
        testST(beforeBC, 17, SysTime(DateTime(-1999, 7, 6, 5, 30, 33), d));
        testST(beforeBC, 18, SysTime(DateTime(-1999, 7, 6, 6, 30, 33), d));
        testST(beforeBC, 19, SysTime(DateTime(-1999, 7, 6, 7, 30, 33), d));
        testST(beforeBC, 20, SysTime(DateTime(-1999, 7, 6, 8, 30, 33), d));
        testST(beforeBC, 21, SysTime(DateTime(-1999, 7, 6, 9, 30, 33), d));
        testST(beforeBC, 22, SysTime(DateTime(-1999, 7, 6, 10, 30, 33), d));
        testST(beforeBC, 23, SysTime(DateTime(-1999, 7, 6, 11, 30, 33), d));
        testST(beforeBC, 24, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), d));
        testST(beforeBC, 25, SysTime(DateTime(-1999, 7, 6, 13, 30, 33), d));
        testST(beforeBC, 50, SysTime(DateTime(-1999, 7, 6, 14, 30, 33), d));
        testST(beforeBC, 10_000, SysTime(DateTime(-1999, 7, 6, 4, 30, 33), d));

        testST(beforeBC, -1, SysTime(DateTime(-1999, 7, 6, 11, 30, 33), d));
        testST(beforeBC, -2, SysTime(DateTime(-1999, 7, 6, 10, 30, 33), d));
        testST(beforeBC, -3, SysTime(DateTime(-1999, 7, 6, 9, 30, 33), d));
        testST(beforeBC, -4, SysTime(DateTime(-1999, 7, 6, 8, 30, 33), d));
        testST(beforeBC, -5, SysTime(DateTime(-1999, 7, 6, 7, 30, 33), d));
        testST(beforeBC, -6, SysTime(DateTime(-1999, 7, 6, 6, 30, 33), d));
        testST(beforeBC, -7, SysTime(DateTime(-1999, 7, 6, 5, 30, 33), d));
        testST(beforeBC, -8, SysTime(DateTime(-1999, 7, 6, 4, 30, 33), d));
        testST(beforeBC, -9, SysTime(DateTime(-1999, 7, 6, 3, 30, 33), d));
        testST(beforeBC, -10, SysTime(DateTime(-1999, 7, 6, 2, 30, 33), d));
        testST(beforeBC, -11, SysTime(DateTime(-1999, 7, 6, 1, 30, 33), d));
        testST(beforeBC, -12, SysTime(DateTime(-1999, 7, 6, 0, 30, 33), d));
        testST(beforeBC, -13, SysTime(DateTime(-1999, 7, 6, 23, 30, 33), d));
        testST(beforeBC, -14, SysTime(DateTime(-1999, 7, 6, 22, 30, 33), d));
        testST(beforeBC, -15, SysTime(DateTime(-1999, 7, 6, 21, 30, 33), d));
        testST(beforeBC, -16, SysTime(DateTime(-1999, 7, 6, 20, 30, 33), d));
        testST(beforeBC, -17, SysTime(DateTime(-1999, 7, 6, 19, 30, 33), d));
        testST(beforeBC, -18, SysTime(DateTime(-1999, 7, 6, 18, 30, 33), d));
        testST(beforeBC, -19, SysTime(DateTime(-1999, 7, 6, 17, 30, 33), d));
        testST(beforeBC, -20, SysTime(DateTime(-1999, 7, 6, 16, 30, 33), d));
        testST(beforeBC, -21, SysTime(DateTime(-1999, 7, 6, 15, 30, 33), d));
        testST(beforeBC, -22, SysTime(DateTime(-1999, 7, 6, 14, 30, 33), d));
        testST(beforeBC, -23, SysTime(DateTime(-1999, 7, 6, 13, 30, 33), d));
        testST(beforeBC, -24, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), d));
        testST(beforeBC, -25, SysTime(DateTime(-1999, 7, 6, 11, 30, 33), d));
        testST(beforeBC, -50, SysTime(DateTime(-1999, 7, 6, 10, 30, 33), d));
        testST(beforeBC, -10_000, SysTime(DateTime(-1999, 7, 6, 20, 30, 33), d));

        testST(SysTime(DateTime(-1999, 7, 6, 0, 30, 33), d), 1, SysTime(DateTime(-1999, 7, 6, 1, 30, 33), d));
        testST(SysTime(DateTime(-1999, 7, 6, 0, 30, 33), d), 0, SysTime(DateTime(-1999, 7, 6, 0, 30, 33), d));
        testST(SysTime(DateTime(-1999, 7, 6, 0, 30, 33), d), -1, SysTime(DateTime(-1999, 7, 6, 23, 30, 33), d));

        testST(SysTime(DateTime(-1999, 7, 6, 23, 30, 33), d), 1, SysTime(DateTime(-1999, 7, 6, 0, 30, 33), d));
        testST(SysTime(DateTime(-1999, 7, 6, 23, 30, 33), d), 0, SysTime(DateTime(-1999, 7, 6, 23, 30, 33), d));
        testST(SysTime(DateTime(-1999, 7, 6, 23, 30, 33), d), -1, SysTime(DateTime(-1999, 7, 6, 22, 30, 33), d));

        testST(SysTime(DateTime(-1999, 7, 31, 23, 30, 33), d), 1, SysTime(DateTime(-1999, 7, 31, 0, 30, 33), d));
        testST(SysTime(DateTime(-1999, 8, 1, 0, 30, 33), d), -1, SysTime(DateTime(-1999, 8, 1, 23, 30, 33), d));

        testST(SysTime(DateTime(-2001, 12, 31, 23, 30, 33), d), 1, SysTime(DateTime(-2001, 12, 31, 0, 30, 33), d));
        testST(SysTime(DateTime(-2000, 1, 1, 0, 30, 33), d), -1, SysTime(DateTime(-2000, 1, 1, 23, 30, 33), d));

        testST(SysTime(DateTime(-2001, 2, 28, 23, 30, 33), d), 25, SysTime(DateTime(-2001, 2, 28, 0, 30, 33), d));
        testST(SysTime(DateTime(-2001, 3, 2, 0, 30, 33), d), -25, SysTime(DateTime(-2001, 3, 2, 23, 30, 33), d));

        testST(SysTime(DateTime(-2000, 2, 28, 23, 30, 33), d), 25, SysTime(DateTime(-2000, 2, 28, 0, 30, 33), d));
        testST(SysTime(DateTime(-2000, 3, 1, 0, 30, 33), d), -25, SysTime(DateTime(-2000, 3, 1, 23, 30, 33), d));

        // Test Both
        testST(SysTime(DateTime(-1, 1, 1, 11, 30, 33), d), 17_546, SysTime(DateTime(-1, 1, 1, 13, 30, 33), d));
        testST(SysTime(DateTime(1, 1, 1, 13, 30, 33), d), -17_546, SysTime(DateTime(1, 1, 1, 11, 30, 33), d));

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 0, 0, 0));
            sysTime.roll!"hours"(-1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 23, 0, 0)));
            sysTime.roll!"hours"(1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        }

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 0, 59, 59), hnsecs(9_999_999));
            sysTime.roll!"hours"(-1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
            sysTime.roll!"hours"(1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 59, 59), hnsecs(9_999_999)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 12, 31, 23, 0, 0));
            sysTime.roll!"hours"(1);
            assert(sysTime == SysTime(DateTime(0, 12, 31, 0, 0, 0)));
            sysTime.roll!"hours"(-1);
            assert(sysTime == SysTime(DateTime(0, 12, 31, 23, 0, 0)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999));
            sysTime.roll!"hours"(1);
            assert(sysTime == SysTime(DateTime(0, 12, 31, 0, 59, 59), hnsecs(9_999_999)));
            sysTime.roll!"hours"(-1);
            assert(sysTime == SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999));
            sysTime.roll!"hours"(1).roll!"hours"(-67);
            assert(sysTime == SysTime(DateTime(0, 12, 31, 5, 59, 59), hnsecs(9_999_999)));
        }

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        static assert(!__traits(compiles, cst.roll!"hours"(4)));
        static assert(!__traits(compiles, ist.roll!"hours"(4)));

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.roll!"hours"(42);
        }
    }

    // Test roll!"minutes"().
    @safe unittest
    {
        import core.time;
        static void testST(SysTime orig, int minutes, SysTime expected, size_t line = __LINE__) @safe
        {
            orig.roll!"minutes"(minutes);
            if (orig != expected)
                throw new AssertError(format("Failed. actual [%s] != expected [%s]", orig, expected), __FILE__, line);
        }

        // Test A.D.
        immutable d = usecs(7203);
        auto beforeAD = SysTime(DateTime(1999, 7, 6, 12, 30, 33), d);
        testST(beforeAD, 0, SysTime(DateTime(1999, 7, 6, 12, 30, 33), d));
        testST(beforeAD, 1, SysTime(DateTime(1999, 7, 6, 12, 31, 33), d));
        testST(beforeAD, 2, SysTime(DateTime(1999, 7, 6, 12, 32, 33), d));
        testST(beforeAD, 3, SysTime(DateTime(1999, 7, 6, 12, 33, 33), d));
        testST(beforeAD, 4, SysTime(DateTime(1999, 7, 6, 12, 34, 33), d));
        testST(beforeAD, 5, SysTime(DateTime(1999, 7, 6, 12, 35, 33), d));
        testST(beforeAD, 10, SysTime(DateTime(1999, 7, 6, 12, 40, 33), d));
        testST(beforeAD, 15, SysTime(DateTime(1999, 7, 6, 12, 45, 33), d));
        testST(beforeAD, 29, SysTime(DateTime(1999, 7, 6, 12, 59, 33), d));
        testST(beforeAD, 30, SysTime(DateTime(1999, 7, 6, 12, 0, 33), d));
        testST(beforeAD, 45, SysTime(DateTime(1999, 7, 6, 12, 15, 33), d));
        testST(beforeAD, 60, SysTime(DateTime(1999, 7, 6, 12, 30, 33), d));
        testST(beforeAD, 75, SysTime(DateTime(1999, 7, 6, 12, 45, 33), d));
        testST(beforeAD, 90, SysTime(DateTime(1999, 7, 6, 12, 0, 33), d));
        testST(beforeAD, 100, SysTime(DateTime(1999, 7, 6, 12, 10, 33), d));

        testST(beforeAD, 689, SysTime(DateTime(1999, 7, 6, 12, 59, 33), d));
        testST(beforeAD, 690, SysTime(DateTime(1999, 7, 6, 12, 0, 33), d));
        testST(beforeAD, 691, SysTime(DateTime(1999, 7, 6, 12, 1, 33), d));
        testST(beforeAD, 960, SysTime(DateTime(1999, 7, 6, 12, 30, 33), d));
        testST(beforeAD, 1439, SysTime(DateTime(1999, 7, 6, 12, 29, 33), d));
        testST(beforeAD, 1440, SysTime(DateTime(1999, 7, 6, 12, 30, 33), d));
        testST(beforeAD, 1441, SysTime(DateTime(1999, 7, 6, 12, 31, 33), d));
        testST(beforeAD, 2880, SysTime(DateTime(1999, 7, 6, 12, 30, 33), d));

        testST(beforeAD, -1, SysTime(DateTime(1999, 7, 6, 12, 29, 33), d));
        testST(beforeAD, -2, SysTime(DateTime(1999, 7, 6, 12, 28, 33), d));
        testST(beforeAD, -3, SysTime(DateTime(1999, 7, 6, 12, 27, 33), d));
        testST(beforeAD, -4, SysTime(DateTime(1999, 7, 6, 12, 26, 33), d));
        testST(beforeAD, -5, SysTime(DateTime(1999, 7, 6, 12, 25, 33), d));
        testST(beforeAD, -10, SysTime(DateTime(1999, 7, 6, 12, 20, 33), d));
        testST(beforeAD, -15, SysTime(DateTime(1999, 7, 6, 12, 15, 33), d));
        testST(beforeAD, -29, SysTime(DateTime(1999, 7, 6, 12, 1, 33), d));
        testST(beforeAD, -30, SysTime(DateTime(1999, 7, 6, 12, 0, 33), d));
        testST(beforeAD, -45, SysTime(DateTime(1999, 7, 6, 12, 45, 33), d));
        testST(beforeAD, -60, SysTime(DateTime(1999, 7, 6, 12, 30, 33), d));
        testST(beforeAD, -75, SysTime(DateTime(1999, 7, 6, 12, 15, 33), d));
        testST(beforeAD, -90, SysTime(DateTime(1999, 7, 6, 12, 0, 33), d));
        testST(beforeAD, -100, SysTime(DateTime(1999, 7, 6, 12, 50, 33), d));

        testST(beforeAD, -749, SysTime(DateTime(1999, 7, 6, 12, 1, 33), d));
        testST(beforeAD, -750, SysTime(DateTime(1999, 7, 6, 12, 0, 33), d));
        testST(beforeAD, -751, SysTime(DateTime(1999, 7, 6, 12, 59, 33), d));
        testST(beforeAD, -960, SysTime(DateTime(1999, 7, 6, 12, 30, 33), d));
        testST(beforeAD, -1439, SysTime(DateTime(1999, 7, 6, 12, 31, 33), d));
        testST(beforeAD, -1440, SysTime(DateTime(1999, 7, 6, 12, 30, 33), d));
        testST(beforeAD, -1441, SysTime(DateTime(1999, 7, 6, 12, 29, 33), d));
        testST(beforeAD, -2880, SysTime(DateTime(1999, 7, 6, 12, 30, 33), d));

        testST(SysTime(DateTime(1999, 7, 6, 12, 0, 33), d), 1, SysTime(DateTime(1999, 7, 6, 12, 1, 33), d));
        testST(SysTime(DateTime(1999, 7, 6, 12, 0, 33), d), 0, SysTime(DateTime(1999, 7, 6, 12, 0, 33), d));
        testST(SysTime(DateTime(1999, 7, 6, 12, 0, 33), d), -1, SysTime(DateTime(1999, 7, 6, 12, 59, 33), d));

        testST(SysTime(DateTime(1999, 7, 6, 11, 59, 33), d), 1, SysTime(DateTime(1999, 7, 6, 11, 0, 33), d));
        testST(SysTime(DateTime(1999, 7, 6, 11, 59, 33), d), 0, SysTime(DateTime(1999, 7, 6, 11, 59, 33), d));
        testST(SysTime(DateTime(1999, 7, 6, 11, 59, 33), d), -1, SysTime(DateTime(1999, 7, 6, 11, 58, 33), d));

        testST(SysTime(DateTime(1999, 7, 6, 0, 0, 33), d), 1, SysTime(DateTime(1999, 7, 6, 0, 1, 33), d));
        testST(SysTime(DateTime(1999, 7, 6, 0, 0, 33), d), 0, SysTime(DateTime(1999, 7, 6, 0, 0, 33), d));
        testST(SysTime(DateTime(1999, 7, 6, 0, 0, 33), d), -1, SysTime(DateTime(1999, 7, 6, 0, 59, 33), d));

        testST(SysTime(DateTime(1999, 7, 5, 23, 59, 33), d), 1, SysTime(DateTime(1999, 7, 5, 23, 0, 33), d));
        testST(SysTime(DateTime(1999, 7, 5, 23, 59, 33), d), 0, SysTime(DateTime(1999, 7, 5, 23, 59, 33), d));
        testST(SysTime(DateTime(1999, 7, 5, 23, 59, 33), d), -1, SysTime(DateTime(1999, 7, 5, 23, 58, 33), d));

        testST(SysTime(DateTime(1998, 12, 31, 23, 59, 33), d), 1, SysTime(DateTime(1998, 12, 31, 23, 0, 33), d));
        testST(SysTime(DateTime(1998, 12, 31, 23, 59, 33), d), 0, SysTime(DateTime(1998, 12, 31, 23, 59, 33), d));
        testST(SysTime(DateTime(1998, 12, 31, 23, 59, 33), d), -1, SysTime(DateTime(1998, 12, 31, 23, 58, 33), d));

        // Test B.C.
        auto beforeBC = SysTime(DateTime(-1999, 7, 6, 12, 30, 33), d);
        testST(beforeBC, 0, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), d));
        testST(beforeBC, 1, SysTime(DateTime(-1999, 7, 6, 12, 31, 33), d));
        testST(beforeBC, 2, SysTime(DateTime(-1999, 7, 6, 12, 32, 33), d));
        testST(beforeBC, 3, SysTime(DateTime(-1999, 7, 6, 12, 33, 33), d));
        testST(beforeBC, 4, SysTime(DateTime(-1999, 7, 6, 12, 34, 33), d));
        testST(beforeBC, 5, SysTime(DateTime(-1999, 7, 6, 12, 35, 33), d));
        testST(beforeBC, 10, SysTime(DateTime(-1999, 7, 6, 12, 40, 33), d));
        testST(beforeBC, 15, SysTime(DateTime(-1999, 7, 6, 12, 45, 33), d));
        testST(beforeBC, 29, SysTime(DateTime(-1999, 7, 6, 12, 59, 33), d));
        testST(beforeBC, 30, SysTime(DateTime(-1999, 7, 6, 12, 0, 33), d));
        testST(beforeBC, 45, SysTime(DateTime(-1999, 7, 6, 12, 15, 33), d));
        testST(beforeBC, 60, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), d));
        testST(beforeBC, 75, SysTime(DateTime(-1999, 7, 6, 12, 45, 33), d));
        testST(beforeBC, 90, SysTime(DateTime(-1999, 7, 6, 12, 0, 33), d));
        testST(beforeBC, 100, SysTime(DateTime(-1999, 7, 6, 12, 10, 33), d));

        testST(beforeBC, 689, SysTime(DateTime(-1999, 7, 6, 12, 59, 33), d));
        testST(beforeBC, 690, SysTime(DateTime(-1999, 7, 6, 12, 0, 33), d));
        testST(beforeBC, 691, SysTime(DateTime(-1999, 7, 6, 12, 1, 33), d));
        testST(beforeBC, 960, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), d));
        testST(beforeBC, 1439, SysTime(DateTime(-1999, 7, 6, 12, 29, 33), d));
        testST(beforeBC, 1440, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), d));
        testST(beforeBC, 1441, SysTime(DateTime(-1999, 7, 6, 12, 31, 33), d));
        testST(beforeBC, 2880, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), d));

        testST(beforeBC, -1, SysTime(DateTime(-1999, 7, 6, 12, 29, 33), d));
        testST(beforeBC, -2, SysTime(DateTime(-1999, 7, 6, 12, 28, 33), d));
        testST(beforeBC, -3, SysTime(DateTime(-1999, 7, 6, 12, 27, 33), d));
        testST(beforeBC, -4, SysTime(DateTime(-1999, 7, 6, 12, 26, 33), d));
        testST(beforeBC, -5, SysTime(DateTime(-1999, 7, 6, 12, 25, 33), d));
        testST(beforeBC, -10, SysTime(DateTime(-1999, 7, 6, 12, 20, 33), d));
        testST(beforeBC, -15, SysTime(DateTime(-1999, 7, 6, 12, 15, 33), d));
        testST(beforeBC, -29, SysTime(DateTime(-1999, 7, 6, 12, 1, 33), d));
        testST(beforeBC, -30, SysTime(DateTime(-1999, 7, 6, 12, 0, 33), d));
        testST(beforeBC, -45, SysTime(DateTime(-1999, 7, 6, 12, 45, 33), d));
        testST(beforeBC, -60, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), d));
        testST(beforeBC, -75, SysTime(DateTime(-1999, 7, 6, 12, 15, 33), d));
        testST(beforeBC, -90, SysTime(DateTime(-1999, 7, 6, 12, 0, 33), d));
        testST(beforeBC, -100, SysTime(DateTime(-1999, 7, 6, 12, 50, 33), d));

        testST(beforeBC, -749, SysTime(DateTime(-1999, 7, 6, 12, 1, 33), d));
        testST(beforeBC, -750, SysTime(DateTime(-1999, 7, 6, 12, 0, 33), d));
        testST(beforeBC, -751, SysTime(DateTime(-1999, 7, 6, 12, 59, 33), d));
        testST(beforeBC, -960, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), d));
        testST(beforeBC, -1439, SysTime(DateTime(-1999, 7, 6, 12, 31, 33), d));
        testST(beforeBC, -1440, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), d));
        testST(beforeBC, -1441, SysTime(DateTime(-1999, 7, 6, 12, 29, 33), d));
        testST(beforeBC, -2880, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), d));

        testST(SysTime(DateTime(-1999, 7, 6, 12, 0, 33), d), 1, SysTime(DateTime(-1999, 7, 6, 12, 1, 33), d));
        testST(SysTime(DateTime(-1999, 7, 6, 12, 0, 33), d), 0, SysTime(DateTime(-1999, 7, 6, 12, 0, 33), d));
        testST(SysTime(DateTime(-1999, 7, 6, 12, 0, 33), d), -1, SysTime(DateTime(-1999, 7, 6, 12, 59, 33), d));

        testST(SysTime(DateTime(-1999, 7, 6, 11, 59, 33), d), 1, SysTime(DateTime(-1999, 7, 6, 11, 0, 33), d));
        testST(SysTime(DateTime(-1999, 7, 6, 11, 59, 33), d), 0, SysTime(DateTime(-1999, 7, 6, 11, 59, 33), d));
        testST(SysTime(DateTime(-1999, 7, 6, 11, 59, 33), d), -1, SysTime(DateTime(-1999, 7, 6, 11, 58, 33), d));

        testST(SysTime(DateTime(-1999, 7, 6, 0, 0, 33), d), 1, SysTime(DateTime(-1999, 7, 6, 0, 1, 33), d));
        testST(SysTime(DateTime(-1999, 7, 6, 0, 0, 33), d), 0, SysTime(DateTime(-1999, 7, 6, 0, 0, 33), d));
        testST(SysTime(DateTime(-1999, 7, 6, 0, 0, 33), d), -1, SysTime(DateTime(-1999, 7, 6, 0, 59, 33), d));

        testST(SysTime(DateTime(-1999, 7, 5, 23, 59, 33), d), 1, SysTime(DateTime(-1999, 7, 5, 23, 0, 33), d));
        testST(SysTime(DateTime(-1999, 7, 5, 23, 59, 33), d), 0, SysTime(DateTime(-1999, 7, 5, 23, 59, 33), d));
        testST(SysTime(DateTime(-1999, 7, 5, 23, 59, 33), d), -1, SysTime(DateTime(-1999, 7, 5, 23, 58, 33), d));

        testST(SysTime(DateTime(-2000, 12, 31, 23, 59, 33), d), 1, SysTime(DateTime(-2000, 12, 31, 23, 0, 33), d));
        testST(SysTime(DateTime(-2000, 12, 31, 23, 59, 33), d), 0, SysTime(DateTime(-2000, 12, 31, 23, 59, 33), d));
        testST(SysTime(DateTime(-2000, 12, 31, 23, 59, 33), d), -1, SysTime(DateTime(-2000, 12, 31, 23, 58, 33), d));

        // Test Both
        testST(SysTime(DateTime(1, 1, 1, 0, 0, 0)), -1, SysTime(DateTime(1, 1, 1, 0, 59, 0)));
        testST(SysTime(DateTime(0, 12, 31, 23, 59, 0)), 1, SysTime(DateTime(0, 12, 31, 23, 0, 0)));

        testST(SysTime(DateTime(0, 1, 1, 0, 0, 0)), -1, SysTime(DateTime(0, 1, 1, 0, 59, 0)));
        testST(SysTime(DateTime(-1, 12, 31, 23, 59, 0)), 1, SysTime(DateTime(-1, 12, 31, 23, 0, 0)));

        testST(SysTime(DateTime(-1, 1, 1, 11, 30, 33), d), 1_052_760, SysTime(DateTime(-1, 1, 1, 11, 30, 33), d));
        testST(SysTime(DateTime(1, 1, 1, 13, 30, 33), d), -1_052_760, SysTime(DateTime(1, 1, 1, 13, 30, 33), d));

        testST(SysTime(DateTime(-1, 1, 1, 11, 30, 33), d), 1_052_782, SysTime(DateTime(-1, 1, 1, 11, 52, 33), d));
        testST(SysTime(DateTime(1, 1, 1, 13, 52, 33), d), -1_052_782, SysTime(DateTime(1, 1, 1, 13, 30, 33), d));

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 0, 0, 0));
            sysTime.roll!"minutes"(-1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 59, 0)));
            sysTime.roll!"minutes"(1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        }

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 0, 0, 59), hnsecs(9_999_999));
            sysTime.roll!"minutes"(-1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 59, 59), hnsecs(9_999_999)));
            sysTime.roll!"minutes"(1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 0, 59), hnsecs(9_999_999)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 12, 31, 23, 59, 0));
            sysTime.roll!"minutes"(1);
            assert(sysTime == SysTime(DateTime(0, 12, 31, 23, 0, 0)));
            sysTime.roll!"minutes"(-1);
            assert(sysTime == SysTime(DateTime(0, 12, 31, 23, 59, 0)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999));
            sysTime.roll!"minutes"(1);
            assert(sysTime == SysTime(DateTime(0, 12, 31, 23, 0, 59), hnsecs(9_999_999)));
            sysTime.roll!"minutes"(-1);
            assert(sysTime == SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999));
            sysTime.roll!"minutes"(1).roll!"minutes"(-79);
            assert(sysTime == SysTime(DateTime(0, 12, 31, 23, 41, 59), hnsecs(9_999_999)));
        }

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        static assert(!__traits(compiles, cst.roll!"minutes"(4)));
        static assert(!__traits(compiles, ist.roll!"minutes"(4)));

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.roll!"minutes"(42);
        }
    }

    // Test roll!"seconds"().
    @safe unittest
    {
        import core.time;
        static void testST(SysTime orig, int seconds, SysTime expected, size_t line = __LINE__) @safe
        {
            orig.roll!"seconds"(seconds);
            if (orig != expected)
                throw new AssertError(format("Failed. actual [%s] != expected [%s]", orig, expected), __FILE__, line);
        }

        // Test A.D.
        immutable d = msecs(274);
        auto beforeAD = SysTime(DateTime(1999, 7, 6, 12, 30, 33), d);
        testST(beforeAD, 0, SysTime(DateTime(1999, 7, 6, 12, 30, 33), d));
        testST(beforeAD, 1, SysTime(DateTime(1999, 7, 6, 12, 30, 34), d));
        testST(beforeAD, 2, SysTime(DateTime(1999, 7, 6, 12, 30, 35), d));
        testST(beforeAD, 3, SysTime(DateTime(1999, 7, 6, 12, 30, 36), d));
        testST(beforeAD, 4, SysTime(DateTime(1999, 7, 6, 12, 30, 37), d));
        testST(beforeAD, 5, SysTime(DateTime(1999, 7, 6, 12, 30, 38), d));
        testST(beforeAD, 10, SysTime(DateTime(1999, 7, 6, 12, 30, 43), d));
        testST(beforeAD, 15, SysTime(DateTime(1999, 7, 6, 12, 30, 48), d));
        testST(beforeAD, 26, SysTime(DateTime(1999, 7, 6, 12, 30, 59), d));
        testST(beforeAD, 27, SysTime(DateTime(1999, 7, 6, 12, 30, 0), d));
        testST(beforeAD, 30, SysTime(DateTime(1999, 7, 6, 12, 30, 3), d));
        testST(beforeAD, 59, SysTime(DateTime(1999, 7, 6, 12, 30, 32), d));
        testST(beforeAD, 60, SysTime(DateTime(1999, 7, 6, 12, 30, 33), d));
        testST(beforeAD, 61, SysTime(DateTime(1999, 7, 6, 12, 30, 34), d));

        testST(beforeAD, 1766, SysTime(DateTime(1999, 7, 6, 12, 30, 59), d));
        testST(beforeAD, 1767, SysTime(DateTime(1999, 7, 6, 12, 30, 0), d));
        testST(beforeAD, 1768, SysTime(DateTime(1999, 7, 6, 12, 30, 1), d));
        testST(beforeAD, 2007, SysTime(DateTime(1999, 7, 6, 12, 30, 0), d));
        testST(beforeAD, 3599, SysTime(DateTime(1999, 7, 6, 12, 30, 32), d));
        testST(beforeAD, 3600, SysTime(DateTime(1999, 7, 6, 12, 30, 33), d));
        testST(beforeAD, 3601, SysTime(DateTime(1999, 7, 6, 12, 30, 34), d));
        testST(beforeAD, 7200, SysTime(DateTime(1999, 7, 6, 12, 30, 33), d));

        testST(beforeAD, -1, SysTime(DateTime(1999, 7, 6, 12, 30, 32), d));
        testST(beforeAD, -2, SysTime(DateTime(1999, 7, 6, 12, 30, 31), d));
        testST(beforeAD, -3, SysTime(DateTime(1999, 7, 6, 12, 30, 30), d));
        testST(beforeAD, -4, SysTime(DateTime(1999, 7, 6, 12, 30, 29), d));
        testST(beforeAD, -5, SysTime(DateTime(1999, 7, 6, 12, 30, 28), d));
        testST(beforeAD, -10, SysTime(DateTime(1999, 7, 6, 12, 30, 23), d));
        testST(beforeAD, -15, SysTime(DateTime(1999, 7, 6, 12, 30, 18), d));
        testST(beforeAD, -33, SysTime(DateTime(1999, 7, 6, 12, 30, 0), d));
        testST(beforeAD, -34, SysTime(DateTime(1999, 7, 6, 12, 30, 59), d));
        testST(beforeAD, -35, SysTime(DateTime(1999, 7, 6, 12, 30, 58), d));
        testST(beforeAD, -59, SysTime(DateTime(1999, 7, 6, 12, 30, 34), d));
        testST(beforeAD, -60, SysTime(DateTime(1999, 7, 6, 12, 30, 33), d));
        testST(beforeAD, -61, SysTime(DateTime(1999, 7, 6, 12, 30, 32), d));

        testST(SysTime(DateTime(1999, 7, 6, 12, 30, 0), d), 1, SysTime(DateTime(1999, 7, 6, 12, 30, 1), d));
        testST(SysTime(DateTime(1999, 7, 6, 12, 30, 0), d), 0, SysTime(DateTime(1999, 7, 6, 12, 30, 0), d));
        testST(SysTime(DateTime(1999, 7, 6, 12, 30, 0), d), -1, SysTime(DateTime(1999, 7, 6, 12, 30, 59), d));

        testST(SysTime(DateTime(1999, 7, 6, 12, 0, 0), d), 1, SysTime(DateTime(1999, 7, 6, 12, 0, 1), d));
        testST(SysTime(DateTime(1999, 7, 6, 12, 0, 0), d), 0, SysTime(DateTime(1999, 7, 6, 12, 0, 0), d));
        testST(SysTime(DateTime(1999, 7, 6, 12, 0, 0), d), -1, SysTime(DateTime(1999, 7, 6, 12, 0, 59), d));

        testST(SysTime(DateTime(1999, 7, 6, 0, 0, 0), d), 1, SysTime(DateTime(1999, 7, 6, 0, 0, 1), d));
        testST(SysTime(DateTime(1999, 7, 6, 0, 0, 0), d), 0, SysTime(DateTime(1999, 7, 6, 0, 0, 0), d));
        testST(SysTime(DateTime(1999, 7, 6, 0, 0, 0), d), -1, SysTime(DateTime(1999, 7, 6, 0, 0, 59), d));

        testST(SysTime(DateTime(1999, 7, 5, 23, 59, 59), d), 1, SysTime(DateTime(1999, 7, 5, 23, 59, 0), d));
        testST(SysTime(DateTime(1999, 7, 5, 23, 59, 59), d), 0, SysTime(DateTime(1999, 7, 5, 23, 59, 59), d));
        testST(SysTime(DateTime(1999, 7, 5, 23, 59, 59), d), -1, SysTime(DateTime(1999, 7, 5, 23, 59, 58), d));

        testST(SysTime(DateTime(1998, 12, 31, 23, 59, 59), d), 1, SysTime(DateTime(1998, 12, 31, 23, 59, 0), d));
        testST(SysTime(DateTime(1998, 12, 31, 23, 59, 59), d), 0, SysTime(DateTime(1998, 12, 31, 23, 59, 59), d));
        testST(SysTime(DateTime(1998, 12, 31, 23, 59, 59), d), -1, SysTime(DateTime(1998, 12, 31, 23, 59, 58), d));

        // Test B.C.
        auto beforeBC = SysTime(DateTime(-1999, 7, 6, 12, 30, 33), d);
        testST(beforeBC, 0, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), d));
        testST(beforeBC, 1, SysTime(DateTime(-1999, 7, 6, 12, 30, 34), d));
        testST(beforeBC, 2, SysTime(DateTime(-1999, 7, 6, 12, 30, 35), d));
        testST(beforeBC, 3, SysTime(DateTime(-1999, 7, 6, 12, 30, 36), d));
        testST(beforeBC, 4, SysTime(DateTime(-1999, 7, 6, 12, 30, 37), d));
        testST(beforeBC, 5, SysTime(DateTime(-1999, 7, 6, 12, 30, 38), d));
        testST(beforeBC, 10, SysTime(DateTime(-1999, 7, 6, 12, 30, 43), d));
        testST(beforeBC, 15, SysTime(DateTime(-1999, 7, 6, 12, 30, 48), d));
        testST(beforeBC, 26, SysTime(DateTime(-1999, 7, 6, 12, 30, 59), d));
        testST(beforeBC, 27, SysTime(DateTime(-1999, 7, 6, 12, 30, 0), d));
        testST(beforeBC, 30, SysTime(DateTime(-1999, 7, 6, 12, 30, 3), d));
        testST(beforeBC, 59, SysTime(DateTime(-1999, 7, 6, 12, 30, 32), d));
        testST(beforeBC, 60, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), d));
        testST(beforeBC, 61, SysTime(DateTime(-1999, 7, 6, 12, 30, 34), d));

        testST(beforeBC, 1766, SysTime(DateTime(-1999, 7, 6, 12, 30, 59), d));
        testST(beforeBC, 1767, SysTime(DateTime(-1999, 7, 6, 12, 30, 0), d));
        testST(beforeBC, 1768, SysTime(DateTime(-1999, 7, 6, 12, 30, 1), d));
        testST(beforeBC, 2007, SysTime(DateTime(-1999, 7, 6, 12, 30, 0), d));
        testST(beforeBC, 3599, SysTime(DateTime(-1999, 7, 6, 12, 30, 32), d));
        testST(beforeBC, 3600, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), d));
        testST(beforeBC, 3601, SysTime(DateTime(-1999, 7, 6, 12, 30, 34), d));
        testST(beforeBC, 7200, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), d));

        testST(beforeBC, -1, SysTime(DateTime(-1999, 7, 6, 12, 30, 32), d));
        testST(beforeBC, -2, SysTime(DateTime(-1999, 7, 6, 12, 30, 31), d));
        testST(beforeBC, -3, SysTime(DateTime(-1999, 7, 6, 12, 30, 30), d));
        testST(beforeBC, -4, SysTime(DateTime(-1999, 7, 6, 12, 30, 29), d));
        testST(beforeBC, -5, SysTime(DateTime(-1999, 7, 6, 12, 30, 28), d));
        testST(beforeBC, -10, SysTime(DateTime(-1999, 7, 6, 12, 30, 23), d));
        testST(beforeBC, -15, SysTime(DateTime(-1999, 7, 6, 12, 30, 18), d));
        testST(beforeBC, -33, SysTime(DateTime(-1999, 7, 6, 12, 30, 0), d));
        testST(beforeBC, -34, SysTime(DateTime(-1999, 7, 6, 12, 30, 59), d));
        testST(beforeBC, -35, SysTime(DateTime(-1999, 7, 6, 12, 30, 58), d));
        testST(beforeBC, -59, SysTime(DateTime(-1999, 7, 6, 12, 30, 34), d));
        testST(beforeBC, -60, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), d));
        testST(beforeBC, -61, SysTime(DateTime(-1999, 7, 6, 12, 30, 32), d));

        testST(SysTime(DateTime(-1999, 7, 6, 12, 30, 0), d), 1, SysTime(DateTime(-1999, 7, 6, 12, 30, 1), d));
        testST(SysTime(DateTime(-1999, 7, 6, 12, 30, 0), d), 0, SysTime(DateTime(-1999, 7, 6, 12, 30, 0), d));
        testST(SysTime(DateTime(-1999, 7, 6, 12, 30, 0), d), -1, SysTime(DateTime(-1999, 7, 6, 12, 30, 59), d));

        testST(SysTime(DateTime(-1999, 7, 6, 12, 0, 0), d), 1, SysTime(DateTime(-1999, 7, 6, 12, 0, 1), d));
        testST(SysTime(DateTime(-1999, 7, 6, 12, 0, 0), d), 0, SysTime(DateTime(-1999, 7, 6, 12, 0, 0), d));
        testST(SysTime(DateTime(-1999, 7, 6, 12, 0, 0), d), -1, SysTime(DateTime(-1999, 7, 6, 12, 0, 59), d));

        testST(SysTime(DateTime(-1999, 7, 6, 0, 0, 0), d), 1, SysTime(DateTime(-1999, 7, 6, 0, 0, 1), d));
        testST(SysTime(DateTime(-1999, 7, 6, 0, 0, 0), d), 0, SysTime(DateTime(-1999, 7, 6, 0, 0, 0), d));
        testST(SysTime(DateTime(-1999, 7, 6, 0, 0, 0), d), -1, SysTime(DateTime(-1999, 7, 6, 0, 0, 59), d));

        testST(SysTime(DateTime(-1999, 7, 5, 23, 59, 59), d), 1, SysTime(DateTime(-1999, 7, 5, 23, 59, 0), d));
        testST(SysTime(DateTime(-1999, 7, 5, 23, 59, 59), d), 0, SysTime(DateTime(-1999, 7, 5, 23, 59, 59), d));
        testST(SysTime(DateTime(-1999, 7, 5, 23, 59, 59), d), -1, SysTime(DateTime(-1999, 7, 5, 23, 59, 58), d));

        testST(SysTime(DateTime(-2000, 12, 31, 23, 59, 59), d), 1, SysTime(DateTime(-2000, 12, 31, 23, 59, 0), d));
        testST(SysTime(DateTime(-2000, 12, 31, 23, 59, 59), d), 0, SysTime(DateTime(-2000, 12, 31, 23, 59, 59), d));
        testST(SysTime(DateTime(-2000, 12, 31, 23, 59, 59), d), -1, SysTime(DateTime(-2000, 12, 31, 23, 59, 58), d));

        // Test Both
        testST(SysTime(DateTime(1, 1, 1, 0, 0, 0), d), -1, SysTime(DateTime(1, 1, 1, 0, 0, 59), d));
        testST(SysTime(DateTime(0, 12, 31, 23, 59, 59), d), 1, SysTime(DateTime(0, 12, 31, 23, 59, 0), d));

        testST(SysTime(DateTime(0, 1, 1, 0, 0, 0), d), -1, SysTime(DateTime(0, 1, 1, 0, 0, 59), d));
        testST(SysTime(DateTime(-1, 12, 31, 23, 59, 59), d), 1, SysTime(DateTime(-1, 12, 31, 23, 59, 0), d));

        testST(SysTime(DateTime(-1, 1, 1, 11, 30, 33), d), 63_165_600L, SysTime(DateTime(-1, 1, 1, 11, 30, 33), d));
        testST(SysTime(DateTime(1, 1, 1, 13, 30, 33), d), -63_165_600L, SysTime(DateTime(1, 1, 1, 13, 30, 33), d));

        testST(SysTime(DateTime(-1, 1, 1, 11, 30, 33), d), 63_165_617L, SysTime(DateTime(-1, 1, 1, 11, 30, 50), d));
        testST(SysTime(DateTime(1, 1, 1, 13, 30, 50), d), -63_165_617L, SysTime(DateTime(1, 1, 1, 13, 30, 33), d));

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 0, 0, 0));
            sysTime.roll!"seconds"(-1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 0, 59)));
            sysTime.roll!"seconds"(1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        }

        {
            auto sysTime = SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(9_999_999));
            sysTime.roll!"seconds"(-1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 0, 59), hnsecs(9_999_999)));
            sysTime.roll!"seconds"(1);
            assert(sysTime == SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(9_999_999)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 12, 31, 23, 59, 59));
            sysTime.roll!"seconds"(1);
            assert(sysTime == SysTime(DateTime(0, 12, 31, 23, 59, 0)));
            sysTime.roll!"seconds"(-1);
            assert(sysTime == SysTime(DateTime(0, 12, 31, 23, 59, 59)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999));
            sysTime.roll!"seconds"(1);
            assert(sysTime == SysTime(DateTime(0, 12, 31, 23, 59, 0), hnsecs(9_999_999)));
            sysTime.roll!"seconds"(-1);
            assert(sysTime == SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999)));
        }

        {
            auto sysTime = SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999));
            sysTime.roll!"seconds"(1).roll!"seconds"(-102);
            assert(sysTime == SysTime(DateTime(0, 12, 31, 23, 59, 18), hnsecs(9_999_999)));
        }

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        static assert(!__traits(compiles, cst.roll!"seconds"(4)));
        static assert(!__traits(compiles, ist.roll!"seconds"(4)));

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.roll!"seconds"(42);
        }
    }


    // Shares documentation with "days" version.
    ref SysTime roll(string units)(long value) @safe nothrow scope
    if (units == "msecs" || units == "usecs" || units == "hnsecs")
    {
        auto hnsecs = adjTime;
        immutable days = splitUnitsFromHNSecs!"days"(hnsecs);
        immutable negative = hnsecs < 0;

        if (negative)
            hnsecs += convert!("hours", "hnsecs")(24);

        immutable seconds = splitUnitsFromHNSecs!"seconds"(hnsecs);
        hnsecs += convert!(units, "hnsecs")(value);
        hnsecs %= convert!("seconds", "hnsecs")(1);

        if (hnsecs < 0)
            hnsecs += convert!("seconds", "hnsecs")(1);
        hnsecs += convert!("seconds", "hnsecs")(seconds);

        if (negative)
            hnsecs -= convert!("hours", "hnsecs")(24);

        immutable newDaysHNSecs = convert!("days", "hnsecs")(days);
        adjTime = newDaysHNSecs + hnsecs;
        return this;
    }


    // Test roll!"msecs"().
    @safe unittest
    {
        import core.time;
        static void testST(SysTime orig, int milliseconds, SysTime expected, size_t line = __LINE__) @safe
        {
            orig.roll!"msecs"(milliseconds);
            if (orig != expected)
                throw new AssertError(format("Failed. actual [%s] != expected [%s]", orig, expected), __FILE__, line);
        }

        // Test A.D.
        auto beforeAD = SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(274));
        testST(beforeAD, 0, SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(274)));
        testST(beforeAD, 1, SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(275)));
        testST(beforeAD, 2, SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(276)));
        testST(beforeAD, 10, SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(284)));
        testST(beforeAD, 100, SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(374)));
        testST(beforeAD, 725, SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(999)));
        testST(beforeAD, 726, SysTime(DateTime(1999, 7, 6, 12, 30, 33)));
        testST(beforeAD, 1000, SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(274)));
        testST(beforeAD, 1001, SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(275)));
        testST(beforeAD, 2000, SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(274)));
        testST(beforeAD, 26_725, SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(999)));
        testST(beforeAD, 26_726, SysTime(DateTime(1999, 7, 6, 12, 30, 33)));
        testST(beforeAD, 26_727, SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(1)));
        testST(beforeAD, 1_766_725, SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(999)));
        testST(beforeAD, 1_766_726, SysTime(DateTime(1999, 7, 6, 12, 30, 33)));

        testST(beforeAD, -1, SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(273)));
        testST(beforeAD, -2, SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(272)));
        testST(beforeAD, -10, SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(264)));
        testST(beforeAD, -100, SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(174)));
        testST(beforeAD, -274, SysTime(DateTime(1999, 7, 6, 12, 30, 33)));
        testST(beforeAD, -275, SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(999)));
        testST(beforeAD, -1000, SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(274)));
        testST(beforeAD, -1001, SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(273)));
        testST(beforeAD, -2000, SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(274)));
        testST(beforeAD, -33_274, SysTime(DateTime(1999, 7, 6, 12, 30, 33)));
        testST(beforeAD, -33_275, SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(999)));
        testST(beforeAD, -1_833_274, SysTime(DateTime(1999, 7, 6, 12, 30, 33)));
        testST(beforeAD, -1_833_275, SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(999)));

        // Test B.C.
        auto beforeBC = SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(274));
        testST(beforeBC, 0, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(274)));
        testST(beforeBC, 1, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(275)));
        testST(beforeBC, 2, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(276)));
        testST(beforeBC, 10, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(284)));
        testST(beforeBC, 100, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(374)));
        testST(beforeBC, 725, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(999)));
        testST(beforeBC, 726, SysTime(DateTime(-1999, 7, 6, 12, 30, 33)));
        testST(beforeBC, 1000, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(274)));
        testST(beforeBC, 1001, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(275)));
        testST(beforeBC, 2000, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(274)));
        testST(beforeBC, 26_725, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(999)));
        testST(beforeBC, 26_726, SysTime(DateTime(-1999, 7, 6, 12, 30, 33)));
        testST(beforeBC, 26_727, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(1)));
        testST(beforeBC, 1_766_725, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(999)));
        testST(beforeBC, 1_766_726, SysTime(DateTime(-1999, 7, 6, 12, 30, 33)));

        testST(beforeBC, -1, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(273)));
        testST(beforeBC, -2, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(272)));
        testST(beforeBC, -10, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(264)));
        testST(beforeBC, -100, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(174)));
        testST(beforeBC, -274, SysTime(DateTime(-1999, 7, 6, 12, 30, 33)));
        testST(beforeBC, -275, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(999)));
        testST(beforeBC, -1000, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(274)));
        testST(beforeBC, -1001, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(273)));
        testST(beforeBC, -2000, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(274)));
        testST(beforeBC, -33_274, SysTime(DateTime(-1999, 7, 6, 12, 30, 33)));
        testST(beforeBC, -33_275, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(999)));
        testST(beforeBC, -1_833_274, SysTime(DateTime(-1999, 7, 6, 12, 30, 33)));
        testST(beforeBC, -1_833_275, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), msecs(999)));

        // Test Both
        auto beforeBoth1 = SysTime(DateTime(1, 1, 1, 0, 0, 0));
        testST(beforeBoth1, 1, SysTime(DateTime(1, 1, 1, 0, 0, 0), msecs(1)));
        testST(beforeBoth1, 0, SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        testST(beforeBoth1, -1, SysTime(DateTime(1, 1, 1, 0, 0, 0), msecs(999)));
        testST(beforeBoth1, -2, SysTime(DateTime(1, 1, 1, 0, 0, 0), msecs(998)));
        testST(beforeBoth1, -1000, SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        testST(beforeBoth1, -2000, SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        testST(beforeBoth1, -2555, SysTime(DateTime(1, 1, 1, 0, 0, 0), msecs(445)));

        auto beforeBoth2 = SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999));
        testST(beforeBoth2, -1, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_989_999)));
        testST(beforeBoth2, 0, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999)));
        testST(beforeBoth2, 1, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9999)));
        testST(beforeBoth2, 2, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(19_999)));
        testST(beforeBoth2, 1000, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999)));
        testST(beforeBoth2, 2000, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999)));
        testST(beforeBoth2, 2555, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(5_549_999)));

        {
            auto st = SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999));
            st.roll!"msecs"(1202).roll!"msecs"(-703);
            assert(st == SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(4_989_999)));
        }

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        static assert(!__traits(compiles, cst.roll!"msecs"(4)));
        static assert(!__traits(compiles, ist.roll!"msecs"(4)));

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.roll!"msecs"(42);
        }
    }

    // Test roll!"usecs"().
    @safe unittest
    {
        import core.time;
        static void testST(SysTime orig, long microseconds, SysTime expected, size_t line = __LINE__) @safe
        {
            orig.roll!"usecs"(microseconds);
            if (orig != expected)
                throw new AssertError(format("Failed. actual [%s] != expected [%s]", orig, expected), __FILE__, line);
        }

        // Test A.D.
        auto beforeAD = SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(274));
        testST(beforeAD, 0, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(274)));
        testST(beforeAD, 1, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(275)));
        testST(beforeAD, 2, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(276)));
        testST(beforeAD, 10, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(284)));
        testST(beforeAD, 100, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(374)));
        testST(beforeAD, 725, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(999)));
        testST(beforeAD, 726, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(1000)));
        testST(beforeAD, 1000, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(1274)));
        testST(beforeAD, 1001, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(1275)));
        testST(beforeAD, 2000, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(2274)));
        testST(beforeAD, 26_725, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(26_999)));
        testST(beforeAD, 26_726, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(27_000)));
        testST(beforeAD, 26_727, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(27_001)));
        testST(beforeAD, 1_766_725, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(766_999)));
        testST(beforeAD, 1_766_726, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(767_000)));
        testST(beforeAD, 1_000_000, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(274)));
        testST(beforeAD, 60_000_000L, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(274)));
        testST(beforeAD, 3_600_000_000L, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(274)));

        testST(beforeAD, -1, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(273)));
        testST(beforeAD, -2, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(272)));
        testST(beforeAD, -10, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(264)));
        testST(beforeAD, -100, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(174)));
        testST(beforeAD, -274, SysTime(DateTime(1999, 7, 6, 12, 30, 33)));
        testST(beforeAD, -275, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(999_999)));
        testST(beforeAD, -1000, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(999_274)));
        testST(beforeAD, -1001, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(999_273)));
        testST(beforeAD, -2000, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(998_274)));
        testST(beforeAD, -33_274, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(967_000)));
        testST(beforeAD, -33_275, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(966_999)));
        testST(beforeAD, -1_833_274, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(167_000)));
        testST(beforeAD, -1_833_275, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(166_999)));
        testST(beforeAD, -1_000_000, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(274)));
        testST(beforeAD, -60_000_000L, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(274)));
        testST(beforeAD, -3_600_000_000L, SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(274)));

        // Test B.C.
        auto beforeBC = SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(274));
        testST(beforeBC, 0, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(274)));
        testST(beforeBC, 1, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(275)));
        testST(beforeBC, 2, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(276)));
        testST(beforeBC, 10, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(284)));
        testST(beforeBC, 100, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(374)));
        testST(beforeBC, 725, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(999)));
        testST(beforeBC, 726, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(1000)));
        testST(beforeBC, 1000, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(1274)));
        testST(beforeBC, 1001, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(1275)));
        testST(beforeBC, 2000, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(2274)));
        testST(beforeBC, 26_725, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(26_999)));
        testST(beforeBC, 26_726, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(27_000)));
        testST(beforeBC, 26_727, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(27_001)));
        testST(beforeBC, 1_766_725, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(766_999)));
        testST(beforeBC, 1_766_726, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(767_000)));
        testST(beforeBC, 1_000_000, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(274)));
        testST(beforeBC, 60_000_000L, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(274)));
        testST(beforeBC, 3_600_000_000L, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(274)));

        testST(beforeBC, -1, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(273)));
        testST(beforeBC, -2, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(272)));
        testST(beforeBC, -10, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(264)));
        testST(beforeBC, -100, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(174)));
        testST(beforeBC, -274, SysTime(DateTime(-1999, 7, 6, 12, 30, 33)));
        testST(beforeBC, -275, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(999_999)));
        testST(beforeBC, -1000, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(999_274)));
        testST(beforeBC, -1001, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(999_273)));
        testST(beforeBC, -2000, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(998_274)));
        testST(beforeBC, -33_274, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(967_000)));
        testST(beforeBC, -33_275, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(966_999)));
        testST(beforeBC, -1_833_274, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(167_000)));
        testST(beforeBC, -1_833_275, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(166_999)));
        testST(beforeBC, -1_000_000, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(274)));
        testST(beforeBC, -60_000_000L, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(274)));
        testST(beforeBC, -3_600_000_000L, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), usecs(274)));

        // Test Both
        auto beforeBoth1 = SysTime(DateTime(1, 1, 1, 0, 0, 0));
        testST(beforeBoth1, 1, SysTime(DateTime(1, 1, 1, 0, 0, 0), usecs(1)));
        testST(beforeBoth1, 0, SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        testST(beforeBoth1, -1, SysTime(DateTime(1, 1, 1, 0, 0, 0), usecs(999_999)));
        testST(beforeBoth1, -2, SysTime(DateTime(1, 1, 1, 0, 0, 0), usecs(999_998)));
        testST(beforeBoth1, -1000, SysTime(DateTime(1, 1, 1, 0, 0, 0), usecs(999_000)));
        testST(beforeBoth1, -2000, SysTime(DateTime(1, 1, 1, 0, 0, 0), usecs(998_000)));
        testST(beforeBoth1, -2555, SysTime(DateTime(1, 1, 1, 0, 0, 0), usecs(997_445)));
        testST(beforeBoth1, -1_000_000, SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        testST(beforeBoth1, -2_000_000, SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        testST(beforeBoth1, -2_333_333, SysTime(DateTime(1, 1, 1, 0, 0, 0), usecs(666_667)));

        auto beforeBoth2 = SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999));
        testST(beforeBoth2, -1, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_989)));
        testST(beforeBoth2, 0, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999)));
        testST(beforeBoth2, 1, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9)));
        testST(beforeBoth2, 2, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(19)));
        testST(beforeBoth2, 1000, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9999)));
        testST(beforeBoth2, 2000, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(19_999)));
        testST(beforeBoth2, 2555, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(25_549)));
        testST(beforeBoth2, 1_000_000, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999)));
        testST(beforeBoth2, 2_000_000, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999)));
        testST(beforeBoth2, 2_333_333, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(3_333_329)));

        {
            auto st = SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999));
            st.roll!"usecs"(9_020_027);
            assert(st == SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(200_269)));
        }

        {
            auto st = SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999));
            st.roll!"usecs"(9_020_027).roll!"usecs"(-70_034);
            assert(st == SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_499_929)));
        }

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        static assert(!__traits(compiles, cst.roll!"usecs"(4)));
        static assert(!__traits(compiles, ist.roll!"usecs"(4)));

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.roll!"usecs"(42);
        }
    }

    // Test roll!"hnsecs"().
    @safe unittest
    {
        import core.time;
        static void testST(SysTime orig, long hnsecs, SysTime expected, size_t line = __LINE__) @safe
        {
            orig.roll!"hnsecs"(hnsecs);
            if (orig != expected)
                throw new AssertError(format("Failed. actual [%s] != expected [%s]", orig, expected), __FILE__, line);
        }

        // Test A.D.
        auto dtAD = DateTime(1999, 7, 6, 12, 30, 33);
        auto beforeAD = SysTime(dtAD, hnsecs(274));
        testST(beforeAD, 0, SysTime(dtAD, hnsecs(274)));
        testST(beforeAD, 1, SysTime(dtAD, hnsecs(275)));
        testST(beforeAD, 2, SysTime(dtAD, hnsecs(276)));
        testST(beforeAD, 10, SysTime(dtAD, hnsecs(284)));
        testST(beforeAD, 100, SysTime(dtAD, hnsecs(374)));
        testST(beforeAD, 725, SysTime(dtAD, hnsecs(999)));
        testST(beforeAD, 726, SysTime(dtAD, hnsecs(1000)));
        testST(beforeAD, 1000, SysTime(dtAD, hnsecs(1274)));
        testST(beforeAD, 1001, SysTime(dtAD, hnsecs(1275)));
        testST(beforeAD, 2000, SysTime(dtAD, hnsecs(2274)));
        testST(beforeAD, 26_725, SysTime(dtAD, hnsecs(26_999)));
        testST(beforeAD, 26_726, SysTime(dtAD, hnsecs(27_000)));
        testST(beforeAD, 26_727, SysTime(dtAD, hnsecs(27_001)));
        testST(beforeAD, 1_766_725, SysTime(dtAD, hnsecs(1_766_999)));
        testST(beforeAD, 1_766_726, SysTime(dtAD, hnsecs(1_767_000)));
        testST(beforeAD, 1_000_000, SysTime(dtAD, hnsecs(1_000_274)));
        testST(beforeAD, 60_000_000L, SysTime(dtAD, hnsecs(274)));
        testST(beforeAD, 3_600_000_000L, SysTime(dtAD, hnsecs(274)));
        testST(beforeAD, 600_000_000L, SysTime(dtAD, hnsecs(274)));
        testST(beforeAD, 36_000_000_000L, SysTime(dtAD, hnsecs(274)));

        testST(beforeAD, -1, SysTime(dtAD, hnsecs(273)));
        testST(beforeAD, -2, SysTime(dtAD, hnsecs(272)));
        testST(beforeAD, -10, SysTime(dtAD, hnsecs(264)));
        testST(beforeAD, -100, SysTime(dtAD, hnsecs(174)));
        testST(beforeAD, -274, SysTime(dtAD));
        testST(beforeAD, -275, SysTime(dtAD, hnsecs(9_999_999)));
        testST(beforeAD, -1000, SysTime(dtAD, hnsecs(9_999_274)));
        testST(beforeAD, -1001, SysTime(dtAD, hnsecs(9_999_273)));
        testST(beforeAD, -2000, SysTime(dtAD, hnsecs(9_998_274)));
        testST(beforeAD, -33_274, SysTime(dtAD, hnsecs(9_967_000)));
        testST(beforeAD, -33_275, SysTime(dtAD, hnsecs(9_966_999)));
        testST(beforeAD, -1_833_274, SysTime(dtAD, hnsecs(8_167_000)));
        testST(beforeAD, -1_833_275, SysTime(dtAD, hnsecs(8_166_999)));
        testST(beforeAD, -1_000_000, SysTime(dtAD, hnsecs(9_000_274)));
        testST(beforeAD, -60_000_000L, SysTime(dtAD, hnsecs(274)));
        testST(beforeAD, -3_600_000_000L, SysTime(dtAD, hnsecs(274)));
        testST(beforeAD, -600_000_000L, SysTime(dtAD, hnsecs(274)));
        testST(beforeAD, -36_000_000_000L, SysTime(dtAD, hnsecs(274)));

        // Test B.C.
        auto dtBC = DateTime(-1999, 7, 6, 12, 30, 33);
        auto beforeBC = SysTime(dtBC, hnsecs(274));
        testST(beforeBC, 0, SysTime(dtBC, hnsecs(274)));
        testST(beforeBC, 1, SysTime(dtBC, hnsecs(275)));
        testST(beforeBC, 2, SysTime(dtBC, hnsecs(276)));
        testST(beforeBC, 10, SysTime(dtBC, hnsecs(284)));
        testST(beforeBC, 100, SysTime(dtBC, hnsecs(374)));
        testST(beforeBC, 725, SysTime(dtBC, hnsecs(999)));
        testST(beforeBC, 726, SysTime(dtBC, hnsecs(1000)));
        testST(beforeBC, 1000, SysTime(dtBC, hnsecs(1274)));
        testST(beforeBC, 1001, SysTime(dtBC, hnsecs(1275)));
        testST(beforeBC, 2000, SysTime(dtBC, hnsecs(2274)));
        testST(beforeBC, 26_725, SysTime(dtBC, hnsecs(26_999)));
        testST(beforeBC, 26_726, SysTime(dtBC, hnsecs(27_000)));
        testST(beforeBC, 26_727, SysTime(dtBC, hnsecs(27_001)));
        testST(beforeBC, 1_766_725, SysTime(dtBC, hnsecs(1_766_999)));
        testST(beforeBC, 1_766_726, SysTime(dtBC, hnsecs(1_767_000)));
        testST(beforeBC, 1_000_000, SysTime(dtBC, hnsecs(1_000_274)));
        testST(beforeBC, 60_000_000L, SysTime(dtBC, hnsecs(274)));
        testST(beforeBC, 3_600_000_000L, SysTime(dtBC, hnsecs(274)));
        testST(beforeBC, 600_000_000L, SysTime(dtBC, hnsecs(274)));
        testST(beforeBC, 36_000_000_000L, SysTime(dtBC, hnsecs(274)));

        testST(beforeBC, -1, SysTime(dtBC, hnsecs(273)));
        testST(beforeBC, -2, SysTime(dtBC, hnsecs(272)));
        testST(beforeBC, -10, SysTime(dtBC, hnsecs(264)));
        testST(beforeBC, -100, SysTime(dtBC, hnsecs(174)));
        testST(beforeBC, -274, SysTime(dtBC));
        testST(beforeBC, -275, SysTime(dtBC, hnsecs(9_999_999)));
        testST(beforeBC, -1000, SysTime(dtBC, hnsecs(9_999_274)));
        testST(beforeBC, -1001, SysTime(dtBC, hnsecs(9_999_273)));
        testST(beforeBC, -2000, SysTime(dtBC, hnsecs(9_998_274)));
        testST(beforeBC, -33_274, SysTime(dtBC, hnsecs(9_967_000)));
        testST(beforeBC, -33_275, SysTime(dtBC, hnsecs(9_966_999)));
        testST(beforeBC, -1_833_274, SysTime(dtBC, hnsecs(8_167_000)));
        testST(beforeBC, -1_833_275, SysTime(dtBC, hnsecs(8_166_999)));
        testST(beforeBC, -1_000_000, SysTime(dtBC, hnsecs(9_000_274)));
        testST(beforeBC, -60_000_000L, SysTime(dtBC, hnsecs(274)));
        testST(beforeBC, -3_600_000_000L, SysTime(dtBC, hnsecs(274)));
        testST(beforeBC, -600_000_000L, SysTime(dtBC, hnsecs(274)));
        testST(beforeBC, -36_000_000_000L, SysTime(dtBC, hnsecs(274)));

        // Test Both
        auto dtBoth1 = DateTime(1, 1, 1, 0, 0, 0);
        auto beforeBoth1 = SysTime(dtBoth1);
        testST(beforeBoth1, 1, SysTime(dtBoth1, hnsecs(1)));
        testST(beforeBoth1, 0, SysTime(dtBoth1));
        testST(beforeBoth1, -1, SysTime(dtBoth1, hnsecs(9_999_999)));
        testST(beforeBoth1, -2, SysTime(dtBoth1, hnsecs(9_999_998)));
        testST(beforeBoth1, -1000, SysTime(dtBoth1, hnsecs(9_999_000)));
        testST(beforeBoth1, -2000, SysTime(dtBoth1, hnsecs(9_998_000)));
        testST(beforeBoth1, -2555, SysTime(dtBoth1, hnsecs(9_997_445)));
        testST(beforeBoth1, -1_000_000, SysTime(dtBoth1, hnsecs(9_000_000)));
        testST(beforeBoth1, -2_000_000, SysTime(dtBoth1, hnsecs(8_000_000)));
        testST(beforeBoth1, -2_333_333, SysTime(dtBoth1, hnsecs(7_666_667)));
        testST(beforeBoth1, -10_000_000, SysTime(dtBoth1));
        testST(beforeBoth1, -20_000_000, SysTime(dtBoth1));
        testST(beforeBoth1, -20_888_888, SysTime(dtBoth1, hnsecs(9_111_112)));

        auto dtBoth2 = DateTime(0, 12, 31, 23, 59, 59);
        auto beforeBoth2 = SysTime(dtBoth2, hnsecs(9_999_999));
        testST(beforeBoth2, -1, SysTime(dtBoth2, hnsecs(9_999_998)));
        testST(beforeBoth2, 0, SysTime(dtBoth2, hnsecs(9_999_999)));
        testST(beforeBoth2, 1, SysTime(dtBoth2));
        testST(beforeBoth2, 2, SysTime(dtBoth2, hnsecs(1)));
        testST(beforeBoth2, 1000, SysTime(dtBoth2, hnsecs(999)));
        testST(beforeBoth2, 2000, SysTime(dtBoth2, hnsecs(1999)));
        testST(beforeBoth2, 2555, SysTime(dtBoth2, hnsecs(2554)));
        testST(beforeBoth2, 1_000_000, SysTime(dtBoth2, hnsecs(999_999)));
        testST(beforeBoth2, 2_000_000, SysTime(dtBoth2, hnsecs(1_999_999)));
        testST(beforeBoth2, 2_333_333, SysTime(dtBoth2, hnsecs(2_333_332)));
        testST(beforeBoth2, 10_000_000, SysTime(dtBoth2, hnsecs(9_999_999)));
        testST(beforeBoth2, 20_000_000, SysTime(dtBoth2, hnsecs(9_999_999)));
        testST(beforeBoth2, 20_888_888, SysTime(dtBoth2, hnsecs(888_887)));

        {
            auto st = SysTime(dtBoth2, hnsecs(9_999_999));
            st.roll!"hnsecs"(70_777_222).roll!"hnsecs"(-222_555_292);
            assert(st == SysTime(dtBoth2, hnsecs(8_221_929)));
        }

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        static assert(!__traits(compiles, cst.roll!"hnsecs"(4)));
        static assert(!__traits(compiles, ist.roll!"hnsecs"(4)));

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.roll!"hnsecs"(42);
        }
    }


    /++
        Gives the result of adding or subtracting a $(REF Duration, core,time)
        from this $(LREF SysTime).

        The legal types of arithmetic for $(LREF SysTime) using this operator
        are

        $(BOOKTABLE,
        $(TR $(TD SysTime) $(TD +) $(TD Duration) $(TD -->) $(TD SysTime))
        $(TR $(TD SysTime) $(TD -) $(TD Duration) $(TD -->) $(TD SysTime))
        )

        Params:
            duration = The $(REF Duration, core,time) to add to or subtract from
                       this $(LREF SysTime).
      +/
    SysTime opBinary(string op)(Duration duration) @safe const pure nothrow return scope
    if (op == "+" || op == "-")
    {
        SysTime retval = SysTime(this._stdTime, this._timezone);
        immutable hnsecs = duration.total!"hnsecs";
        mixin("retval._stdTime " ~ op ~ "= hnsecs;");
        return retval;
    }

    ///
    @safe unittest
    {
        import core.time : hours, seconds;
        import std.datetime.date : DateTime;

        assert(SysTime(DateTime(2015, 12, 31, 23, 59, 59)) + seconds(1) ==
               SysTime(DateTime(2016, 1, 1, 0, 0, 0)));

        assert(SysTime(DateTime(2015, 12, 31, 23, 59, 59)) + hours(1) ==
               SysTime(DateTime(2016, 1, 1, 0, 59, 59)));

        assert(SysTime(DateTime(2016, 1, 1, 0, 0, 0)) - seconds(1) ==
               SysTime(DateTime(2015, 12, 31, 23, 59, 59)));

        assert(SysTime(DateTime(2016, 1, 1, 0, 59, 59)) - hours(1) ==
               SysTime(DateTime(2015, 12, 31, 23, 59, 59)));
    }

    @safe unittest
    {
        import core.time;
        auto st = SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(2_345_678));

        assert(st + dur!"weeks"(7) == SysTime(DateTime(1999, 8, 24, 12, 30, 33), hnsecs(2_345_678)));
        assert(st + dur!"weeks"(-7) == SysTime(DateTime(1999, 5, 18, 12, 30, 33), hnsecs(2_345_678)));
        assert(st + dur!"days"(7) == SysTime(DateTime(1999, 7, 13, 12, 30, 33), hnsecs(2_345_678)));
        assert(st + dur!"days"(-7) == SysTime(DateTime(1999, 6, 29, 12, 30, 33), hnsecs(2_345_678)));
        assert(st + dur!"hours"(7) == SysTime(DateTime(1999, 7, 6, 19, 30, 33), hnsecs(2_345_678)));
        assert(st + dur!"hours"(-7) == SysTime(DateTime(1999, 7, 6, 5, 30, 33), hnsecs(2_345_678)));
        assert(st + dur!"minutes"(7) == SysTime(DateTime(1999, 7, 6, 12, 37, 33), hnsecs(2_345_678)));
        assert(st + dur!"minutes"(-7) == SysTime(DateTime(1999, 7, 6, 12, 23, 33), hnsecs(2_345_678)));
        assert(st + dur!"seconds"(7) == SysTime(DateTime(1999, 7, 6, 12, 30, 40), hnsecs(2_345_678)));
        assert(st + dur!"seconds"(-7) == SysTime(DateTime(1999, 7, 6, 12, 30, 26), hnsecs(2_345_678)));
        assert(st + dur!"msecs"(7) == SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(2_415_678)));
        assert(st + dur!"msecs"(-7) == SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(2_275_678)));
        assert(st + dur!"usecs"(7) == SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(2_345_748)));
        assert(st + dur!"usecs"(-7) == SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(2_345_608)));
        assert(st + dur!"hnsecs"(7) == SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(2_345_685)));
        assert(st + dur!"hnsecs"(-7) == SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(2_345_671)));

        assert(st - dur!"weeks"(-7) == SysTime(DateTime(1999, 8, 24, 12, 30, 33), hnsecs(2_345_678)));
        assert(st - dur!"weeks"(7) == SysTime(DateTime(1999, 5, 18, 12, 30, 33), hnsecs(2_345_678)));
        assert(st - dur!"days"(-7) == SysTime(DateTime(1999, 7, 13, 12, 30, 33), hnsecs(2_345_678)));
        assert(st - dur!"days"(7) == SysTime(DateTime(1999, 6, 29, 12, 30, 33), hnsecs(2_345_678)));
        assert(st - dur!"hours"(-7) == SysTime(DateTime(1999, 7, 6, 19, 30, 33), hnsecs(2_345_678)));
        assert(st - dur!"hours"(7) == SysTime(DateTime(1999, 7, 6, 5, 30, 33), hnsecs(2_345_678)));
        assert(st - dur!"minutes"(-7) == SysTime(DateTime(1999, 7, 6, 12, 37, 33), hnsecs(2_345_678)));
        assert(st - dur!"minutes"(7) == SysTime(DateTime(1999, 7, 6, 12, 23, 33), hnsecs(2_345_678)));
        assert(st - dur!"seconds"(-7) == SysTime(DateTime(1999, 7, 6, 12, 30, 40), hnsecs(2_345_678)));
        assert(st - dur!"seconds"(7) == SysTime(DateTime(1999, 7, 6, 12, 30, 26), hnsecs(2_345_678)));
        assert(st - dur!"msecs"(-7) == SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(2_415_678)));
        assert(st - dur!"msecs"(7) == SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(2_275_678)));
        assert(st - dur!"usecs"(-7) == SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(2_345_748)));
        assert(st - dur!"usecs"(7) == SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(2_345_608)));
        assert(st - dur!"hnsecs"(-7) == SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(2_345_685)));
        assert(st - dur!"hnsecs"(7) == SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(2_345_671)));

        static void testST(SysTime orig, long hnsecs, SysTime expected, size_t line = __LINE__) @safe
        {
            auto result = orig + dur!"hnsecs"(hnsecs);
            if (result != expected)
                throw new AssertError(format("Failed. actual [%s] != expected [%s]", result, expected), __FILE__, line);
        }

        // Test A.D.
        auto beforeAD = SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(274));
        testST(beforeAD, 0, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(274)));
        testST(beforeAD, 1, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(275)));
        testST(beforeAD, 2, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(276)));
        testST(beforeAD, 10, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(284)));
        testST(beforeAD, 100, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(374)));
        testST(beforeAD, 725, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(999)));
        testST(beforeAD, 726, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(1000)));
        testST(beforeAD, 1000, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(1274)));
        testST(beforeAD, 1001, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(1275)));
        testST(beforeAD, 2000, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(2274)));
        testST(beforeAD, 26_725, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(26_999)));
        testST(beforeAD, 26_726, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(27_000)));
        testST(beforeAD, 26_727, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(27_001)));
        testST(beforeAD, 1_766_725, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(1_766_999)));
        testST(beforeAD, 1_766_726, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(1_767_000)));
        testST(beforeAD, 1_000_000, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(1_000_274)));
        testST(beforeAD, 60_000_000L, SysTime(DateTime(1999, 7, 6, 12, 30, 39), hnsecs(274)));
        testST(beforeAD, 3_600_000_000L, SysTime(DateTime(1999, 7, 6, 12, 36, 33), hnsecs(274)));
        testST(beforeAD, 600_000_000L, SysTime(DateTime(1999, 7, 6, 12, 31, 33), hnsecs(274)));
        testST(beforeAD, 36_000_000_000L, SysTime(DateTime(1999, 7, 6, 13, 30, 33), hnsecs(274)));

        testST(beforeAD, -1, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(273)));
        testST(beforeAD, -2, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(272)));
        testST(beforeAD, -10, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(264)));
        testST(beforeAD, -100, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(174)));
        testST(beforeAD, -274, SysTime(DateTime(1999, 7, 6, 12, 30, 33)));
        testST(beforeAD, -275, SysTime(DateTime(1999, 7, 6, 12, 30, 32), hnsecs(9_999_999)));
        testST(beforeAD, -1000, SysTime(DateTime(1999, 7, 6, 12, 30, 32), hnsecs(9_999_274)));
        testST(beforeAD, -1001, SysTime(DateTime(1999, 7, 6, 12, 30, 32), hnsecs(9_999_273)));
        testST(beforeAD, -2000, SysTime(DateTime(1999, 7, 6, 12, 30, 32), hnsecs(9_998_274)));
        testST(beforeAD, -33_274, SysTime(DateTime(1999, 7, 6, 12, 30, 32), hnsecs(9_967_000)));
        testST(beforeAD, -33_275, SysTime(DateTime(1999, 7, 6, 12, 30, 32), hnsecs(9_966_999)));
        testST(beforeAD, -1_833_274, SysTime(DateTime(1999, 7, 6, 12, 30, 32), hnsecs(8_167_000)));
        testST(beforeAD, -1_833_275, SysTime(DateTime(1999, 7, 6, 12, 30, 32), hnsecs(8_166_999)));
        testST(beforeAD, -1_000_000, SysTime(DateTime(1999, 7, 6, 12, 30, 32), hnsecs(9_000_274)));
        testST(beforeAD, -60_000_000L, SysTime(DateTime(1999, 7, 6, 12, 30, 27), hnsecs(274)));
        testST(beforeAD, -3_600_000_000L, SysTime(DateTime(1999, 7, 6, 12, 24, 33), hnsecs(274)));
        testST(beforeAD, -600_000_000L, SysTime(DateTime(1999, 7, 6, 12, 29, 33), hnsecs(274)));
        testST(beforeAD, -36_000_000_000L, SysTime(DateTime(1999, 7, 6, 11, 30, 33), hnsecs(274)));

        // Test B.C.
        auto beforeBC = SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(274));
        testST(beforeBC, 0, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(274)));
        testST(beforeBC, 1, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(275)));
        testST(beforeBC, 2, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(276)));
        testST(beforeBC, 10, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(284)));
        testST(beforeBC, 100, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(374)));
        testST(beforeBC, 725, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(999)));
        testST(beforeBC, 726, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(1000)));
        testST(beforeBC, 1000, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(1274)));
        testST(beforeBC, 1001, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(1275)));
        testST(beforeBC, 2000, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(2274)));
        testST(beforeBC, 26_725, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(26_999)));
        testST(beforeBC, 26_726, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(27_000)));
        testST(beforeBC, 26_727, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(27_001)));
        testST(beforeBC, 1_766_725, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(1_766_999)));
        testST(beforeBC, 1_766_726, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(1_767_000)));
        testST(beforeBC, 1_000_000, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(1_000_274)));
        testST(beforeBC, 60_000_000L, SysTime(DateTime(-1999, 7, 6, 12, 30, 39), hnsecs(274)));
        testST(beforeBC, 3_600_000_000L, SysTime(DateTime(-1999, 7, 6, 12, 36, 33), hnsecs(274)));
        testST(beforeBC, 600_000_000L, SysTime(DateTime(-1999, 7, 6, 12, 31, 33), hnsecs(274)));
        testST(beforeBC, 36_000_000_000L, SysTime(DateTime(-1999, 7, 6, 13, 30, 33), hnsecs(274)));

        testST(beforeBC, -1, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(273)));
        testST(beforeBC, -2, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(272)));
        testST(beforeBC, -10, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(264)));
        testST(beforeBC, -100, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(174)));
        testST(beforeBC, -274, SysTime(DateTime(-1999, 7, 6, 12, 30, 33)));
        testST(beforeBC, -275, SysTime(DateTime(-1999, 7, 6, 12, 30, 32), hnsecs(9_999_999)));
        testST(beforeBC, -1000, SysTime(DateTime(-1999, 7, 6, 12, 30, 32), hnsecs(9_999_274)));
        testST(beforeBC, -1001, SysTime(DateTime(-1999, 7, 6, 12, 30, 32), hnsecs(9_999_273)));
        testST(beforeBC, -2000, SysTime(DateTime(-1999, 7, 6, 12, 30, 32), hnsecs(9_998_274)));
        testST(beforeBC, -33_274, SysTime(DateTime(-1999, 7, 6, 12, 30, 32), hnsecs(9_967_000)));
        testST(beforeBC, -33_275, SysTime(DateTime(-1999, 7, 6, 12, 30, 32), hnsecs(9_966_999)));
        testST(beforeBC, -1_833_274, SysTime(DateTime(-1999, 7, 6, 12, 30, 32), hnsecs(8_167_000)));
        testST(beforeBC, -1_833_275, SysTime(DateTime(-1999, 7, 6, 12, 30, 32), hnsecs(8_166_999)));
        testST(beforeBC, -1_000_000, SysTime(DateTime(-1999, 7, 6, 12, 30, 32), hnsecs(9_000_274)));
        testST(beforeBC, -60_000_000L, SysTime(DateTime(-1999, 7, 6, 12, 30, 27), hnsecs(274)));
        testST(beforeBC, -3_600_000_000L, SysTime(DateTime(-1999, 7, 6, 12, 24, 33), hnsecs(274)));
        testST(beforeBC, -600_000_000L, SysTime(DateTime(-1999, 7, 6, 12, 29, 33), hnsecs(274)));
        testST(beforeBC, -36_000_000_000L, SysTime(DateTime(-1999, 7, 6, 11, 30, 33), hnsecs(274)));

        // Test Both
        auto beforeBoth1 = SysTime(DateTime(1, 1, 1, 0, 0, 0));
        testST(beforeBoth1, 1, SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(1)));
        testST(beforeBoth1, 0, SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        testST(beforeBoth1, -1, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999)));
        testST(beforeBoth1, -2, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_998)));
        testST(beforeBoth1, -1000, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_000)));
        testST(beforeBoth1, -2000, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_998_000)));
        testST(beforeBoth1, -2555, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_997_445)));
        testST(beforeBoth1, -1_000_000, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_000_000)));
        testST(beforeBoth1, -2_000_000, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(8_000_000)));
        testST(beforeBoth1, -2_333_333, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(7_666_667)));
        testST(beforeBoth1, -10_000_000, SysTime(DateTime(0, 12, 31, 23, 59, 59)));
        testST(beforeBoth1, -20_000_000, SysTime(DateTime(0, 12, 31, 23, 59, 58)));
        testST(beforeBoth1, -20_888_888, SysTime(DateTime(0, 12, 31, 23, 59, 57), hnsecs(9_111_112)));

        auto beforeBoth2 = SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999));
        testST(beforeBoth2, -1, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_998)));
        testST(beforeBoth2, 0, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999)));
        testST(beforeBoth2, 1, SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        testST(beforeBoth2, 2, SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(1)));
        testST(beforeBoth2, 1000, SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(999)));
        testST(beforeBoth2, 2000, SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(1999)));
        testST(beforeBoth2, 2555, SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(2554)));
        testST(beforeBoth2, 1_000_000, SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(999_999)));
        testST(beforeBoth2, 2_000_000, SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(1_999_999)));
        testST(beforeBoth2, 2_333_333, SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(2_333_332)));
        testST(beforeBoth2, 10_000_000, SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(9_999_999)));
        testST(beforeBoth2, 20_000_000, SysTime(DateTime(1, 1, 1, 0, 0, 1), hnsecs(9_999_999)));
        testST(beforeBoth2, 20_888_888, SysTime(DateTime(1, 1, 1, 0, 0, 2), hnsecs(888_887)));

        auto duration = dur!"seconds"(12);
        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(cst + duration == SysTime(DateTime(1999, 7, 6, 12, 30, 45)));
        assert(ist + duration == SysTime(DateTime(1999, 7, 6, 12, 30, 45)));
        assert(cst - duration == SysTime(DateTime(1999, 7, 6, 12, 30, 21)));
        assert(ist - duration == SysTime(DateTime(1999, 7, 6, 12, 30, 21)));

        static void testScope(scope ref SysTime st, scope ref Duration d) @safe
        {
            auto result = st + d;
        }
    }


    /++
        Gives the result of adding or subtracting a $(REF Duration, core,time) from
        this $(LREF SysTime), as well as assigning the result to this
        $(LREF SysTime).

        The legal types of arithmetic for $(LREF SysTime) using this operator are

        $(BOOKTABLE,
        $(TR $(TD SysTime) $(TD +) $(TD Duration) $(TD -->) $(TD SysTime))
        $(TR $(TD SysTime) $(TD -) $(TD Duration) $(TD -->) $(TD SysTime))
        )

        Params:
            duration = The $(REF Duration, core,time) to add to or subtract from
                       this $(LREF SysTime).
      +/
    ref SysTime opOpAssign(string op)(Duration duration) @safe pure nothrow scope
    if (op == "+" || op == "-")
    {
        immutable hnsecs = duration.total!"hnsecs";
        mixin("_stdTime " ~ op ~ "= hnsecs;");
        return this;
    }

    @safe unittest
    {
        import core.time;
        auto before = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(before + dur!"weeks"(7) == SysTime(DateTime(1999, 8, 24, 12, 30, 33)));
        assert(before + dur!"weeks"(-7) == SysTime(DateTime(1999, 5, 18, 12, 30, 33)));
        assert(before + dur!"days"(7) == SysTime(DateTime(1999, 7, 13, 12, 30, 33)));
        assert(before + dur!"days"(-7) == SysTime(DateTime(1999, 6, 29, 12, 30, 33)));

        assert(before + dur!"hours"(7) == SysTime(DateTime(1999, 7, 6, 19, 30, 33)));
        assert(before + dur!"hours"(-7) == SysTime(DateTime(1999, 7, 6, 5, 30, 33)));
        assert(before + dur!"minutes"(7) == SysTime(DateTime(1999, 7, 6, 12, 37, 33)));
        assert(before + dur!"minutes"(-7) == SysTime(DateTime(1999, 7, 6, 12, 23, 33)));
        assert(before + dur!"seconds"(7) == SysTime(DateTime(1999, 7, 6, 12, 30, 40)));
        assert(before + dur!"seconds"(-7) == SysTime(DateTime(1999, 7, 6, 12, 30, 26)));
        assert(before + dur!"msecs"(7) == SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(7)));
        assert(before + dur!"msecs"(-7) == SysTime(DateTime(1999, 7, 6, 12, 30, 32), msecs(993)));
        assert(before + dur!"usecs"(7) == SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(7)));
        assert(before + dur!"usecs"(-7) == SysTime(DateTime(1999, 7, 6, 12, 30, 32), usecs(999_993)));
        assert(before + dur!"hnsecs"(7) == SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(7)));
        assert(before + dur!"hnsecs"(-7) == SysTime(DateTime(1999, 7, 6, 12, 30, 32), hnsecs(9_999_993)));

        assert(before - dur!"weeks"(-7) == SysTime(DateTime(1999, 8, 24, 12, 30, 33)));
        assert(before - dur!"weeks"(7) == SysTime(DateTime(1999, 5, 18, 12, 30, 33)));
        assert(before - dur!"days"(-7) == SysTime(DateTime(1999, 7, 13, 12, 30, 33)));
        assert(before - dur!"days"(7) == SysTime(DateTime(1999, 6, 29, 12, 30, 33)));

        assert(before - dur!"hours"(-7) == SysTime(DateTime(1999, 7, 6, 19, 30, 33)));
        assert(before - dur!"hours"(7) == SysTime(DateTime(1999, 7, 6, 5, 30, 33)));
        assert(before - dur!"minutes"(-7) == SysTime(DateTime(1999, 7, 6, 12, 37, 33)));
        assert(before - dur!"minutes"(7) == SysTime(DateTime(1999, 7, 6, 12, 23, 33)));
        assert(before - dur!"seconds"(-7) == SysTime(DateTime(1999, 7, 6, 12, 30, 40)));
        assert(before - dur!"seconds"(7) == SysTime(DateTime(1999, 7, 6, 12, 30, 26)));
        assert(before - dur!"msecs"(-7) == SysTime(DateTime(1999, 7, 6, 12, 30, 33), msecs(7)));
        assert(before - dur!"msecs"(7) == SysTime(DateTime(1999, 7, 6, 12, 30, 32), msecs(993)));
        assert(before - dur!"usecs"(-7) == SysTime(DateTime(1999, 7, 6, 12, 30, 33), usecs(7)));
        assert(before - dur!"usecs"(7) == SysTime(DateTime(1999, 7, 6, 12, 30, 32), usecs(999_993)));
        assert(before - dur!"hnsecs"(-7) == SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(7)));
        assert(before - dur!"hnsecs"(7) == SysTime(DateTime(1999, 7, 6, 12, 30, 32), hnsecs(9_999_993)));

        static void testST(SysTime orig, long hnsecs, SysTime expected, size_t line = __LINE__) @safe
        {
            auto r = orig += dur!"hnsecs"(hnsecs);
            if (orig != expected)
                throw new AssertError(format("Failed 1. actual [%s] != expected [%s]", orig, expected), __FILE__, line);
            if (r != expected)
                throw new AssertError(format("Failed 2. actual [%s] != expected [%s]", r, expected), __FILE__, line);
        }

        // Test A.D.
        auto beforeAD = SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(274));
        testST(beforeAD, 0, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(274)));
        testST(beforeAD, 1, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(275)));
        testST(beforeAD, 2, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(276)));
        testST(beforeAD, 10, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(284)));
        testST(beforeAD, 100, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(374)));
        testST(beforeAD, 725, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(999)));
        testST(beforeAD, 726, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(1000)));
        testST(beforeAD, 1000, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(1274)));
        testST(beforeAD, 1001, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(1275)));
        testST(beforeAD, 2000, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(2274)));
        testST(beforeAD, 26_725, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(26_999)));
        testST(beforeAD, 26_726, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(27_000)));
        testST(beforeAD, 26_727, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(27_001)));
        testST(beforeAD, 1_766_725, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(1_766_999)));
        testST(beforeAD, 1_766_726, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(1_767_000)));
        testST(beforeAD, 1_000_000, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(1_000_274)));
        testST(beforeAD, 60_000_000L, SysTime(DateTime(1999, 7, 6, 12, 30, 39), hnsecs(274)));
        testST(beforeAD, 3_600_000_000L, SysTime(DateTime(1999, 7, 6, 12, 36, 33), hnsecs(274)));
        testST(beforeAD, 600_000_000L, SysTime(DateTime(1999, 7, 6, 12, 31, 33), hnsecs(274)));
        testST(beforeAD, 36_000_000_000L, SysTime(DateTime(1999, 7, 6, 13, 30, 33), hnsecs(274)));

        testST(beforeAD, -1, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(273)));
        testST(beforeAD, -2, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(272)));
        testST(beforeAD, -10, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(264)));
        testST(beforeAD, -100, SysTime(DateTime(1999, 7, 6, 12, 30, 33), hnsecs(174)));
        testST(beforeAD, -274, SysTime(DateTime(1999, 7, 6, 12, 30, 33)));
        testST(beforeAD, -275, SysTime(DateTime(1999, 7, 6, 12, 30, 32), hnsecs(9_999_999)));
        testST(beforeAD, -1000, SysTime(DateTime(1999, 7, 6, 12, 30, 32), hnsecs(9_999_274)));
        testST(beforeAD, -1001, SysTime(DateTime(1999, 7, 6, 12, 30, 32), hnsecs(9_999_273)));
        testST(beforeAD, -2000, SysTime(DateTime(1999, 7, 6, 12, 30, 32), hnsecs(9_998_274)));
        testST(beforeAD, -33_274, SysTime(DateTime(1999, 7, 6, 12, 30, 32), hnsecs(9_967_000)));
        testST(beforeAD, -33_275, SysTime(DateTime(1999, 7, 6, 12, 30, 32), hnsecs(9_966_999)));
        testST(beforeAD, -1_833_274, SysTime(DateTime(1999, 7, 6, 12, 30, 32), hnsecs(8_167_000)));
        testST(beforeAD, -1_833_275, SysTime(DateTime(1999, 7, 6, 12, 30, 32), hnsecs(8_166_999)));
        testST(beforeAD, -1_000_000, SysTime(DateTime(1999, 7, 6, 12, 30, 32), hnsecs(9_000_274)));
        testST(beforeAD, -60_000_000L, SysTime(DateTime(1999, 7, 6, 12, 30, 27), hnsecs(274)));
        testST(beforeAD, -3_600_000_000L, SysTime(DateTime(1999, 7, 6, 12, 24, 33), hnsecs(274)));
        testST(beforeAD, -600_000_000L, SysTime(DateTime(1999, 7, 6, 12, 29, 33), hnsecs(274)));
        testST(beforeAD, -36_000_000_000L, SysTime(DateTime(1999, 7, 6, 11, 30, 33), hnsecs(274)));

        // Test B.C.
        auto beforeBC = SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(274));
        testST(beforeBC, 0, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(274)));
        testST(beforeBC, 1, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(275)));
        testST(beforeBC, 2, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(276)));
        testST(beforeBC, 10, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(284)));
        testST(beforeBC, 100, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(374)));
        testST(beforeBC, 725, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(999)));
        testST(beforeBC, 726, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(1000)));
        testST(beforeBC, 1000, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(1274)));
        testST(beforeBC, 1001, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(1275)));
        testST(beforeBC, 2000, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(2274)));
        testST(beforeBC, 26_725, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(26_999)));
        testST(beforeBC, 26_726, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(27_000)));
        testST(beforeBC, 26_727, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(27_001)));
        testST(beforeBC, 1_766_725, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(1_766_999)));
        testST(beforeBC, 1_766_726, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(1_767_000)));
        testST(beforeBC, 1_000_000, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(1_000_274)));
        testST(beforeBC, 60_000_000L, SysTime(DateTime(-1999, 7, 6, 12, 30, 39), hnsecs(274)));
        testST(beforeBC, 3_600_000_000L, SysTime(DateTime(-1999, 7, 6, 12, 36, 33), hnsecs(274)));
        testST(beforeBC, 600_000_000L, SysTime(DateTime(-1999, 7, 6, 12, 31, 33), hnsecs(274)));
        testST(beforeBC, 36_000_000_000L, SysTime(DateTime(-1999, 7, 6, 13, 30, 33), hnsecs(274)));

        testST(beforeBC, -1, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(273)));
        testST(beforeBC, -2, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(272)));
        testST(beforeBC, -10, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(264)));
        testST(beforeBC, -100, SysTime(DateTime(-1999, 7, 6, 12, 30, 33), hnsecs(174)));
        testST(beforeBC, -274, SysTime(DateTime(-1999, 7, 6, 12, 30, 33)));
        testST(beforeBC, -275, SysTime(DateTime(-1999, 7, 6, 12, 30, 32), hnsecs(9_999_999)));
        testST(beforeBC, -1000, SysTime(DateTime(-1999, 7, 6, 12, 30, 32), hnsecs(9_999_274)));
        testST(beforeBC, -1001, SysTime(DateTime(-1999, 7, 6, 12, 30, 32), hnsecs(9_999_273)));
        testST(beforeBC, -2000, SysTime(DateTime(-1999, 7, 6, 12, 30, 32), hnsecs(9_998_274)));
        testST(beforeBC, -33_274, SysTime(DateTime(-1999, 7, 6, 12, 30, 32), hnsecs(9_967_000)));
        testST(beforeBC, -33_275, SysTime(DateTime(-1999, 7, 6, 12, 30, 32), hnsecs(9_966_999)));
        testST(beforeBC, -1_833_274, SysTime(DateTime(-1999, 7, 6, 12, 30, 32), hnsecs(8_167_000)));
        testST(beforeBC, -1_833_275, SysTime(DateTime(-1999, 7, 6, 12, 30, 32), hnsecs(8_166_999)));
        testST(beforeBC, -1_000_000, SysTime(DateTime(-1999, 7, 6, 12, 30, 32), hnsecs(9_000_274)));
        testST(beforeBC, -60_000_000L, SysTime(DateTime(-1999, 7, 6, 12, 30, 27), hnsecs(274)));
        testST(beforeBC, -3_600_000_000L, SysTime(DateTime(-1999, 7, 6, 12, 24, 33), hnsecs(274)));
        testST(beforeBC, -600_000_000L, SysTime(DateTime(-1999, 7, 6, 12, 29, 33), hnsecs(274)));
        testST(beforeBC, -36_000_000_000L, SysTime(DateTime(-1999, 7, 6, 11, 30, 33), hnsecs(274)));

        // Test Both
        auto beforeBoth1 = SysTime(DateTime(1, 1, 1, 0, 0, 0));
        testST(beforeBoth1, 1, SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(1)));
        testST(beforeBoth1, 0, SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        testST(beforeBoth1, -1, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999)));
        testST(beforeBoth1, -2, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_998)));
        testST(beforeBoth1, -1000, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_000)));
        testST(beforeBoth1, -2000, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_998_000)));
        testST(beforeBoth1, -2555, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_997_445)));
        testST(beforeBoth1, -1_000_000, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_000_000)));
        testST(beforeBoth1, -2_000_000, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(8_000_000)));
        testST(beforeBoth1, -2_333_333, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(7_666_667)));
        testST(beforeBoth1, -10_000_000, SysTime(DateTime(0, 12, 31, 23, 59, 59)));
        testST(beforeBoth1, -20_000_000, SysTime(DateTime(0, 12, 31, 23, 59, 58)));
        testST(beforeBoth1, -20_888_888, SysTime(DateTime(0, 12, 31, 23, 59, 57), hnsecs(9_111_112)));

        auto beforeBoth2 = SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999));
        testST(beforeBoth2, -1, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_998)));
        testST(beforeBoth2, 0, SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999)));
        testST(beforeBoth2, 1, SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        testST(beforeBoth2, 2, SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(1)));
        testST(beforeBoth2, 1000, SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(999)));
        testST(beforeBoth2, 2000, SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(1999)));
        testST(beforeBoth2, 2555, SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(2554)));
        testST(beforeBoth2, 1_000_000, SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(999_999)));
        testST(beforeBoth2, 2_000_000, SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(1_999_999)));
        testST(beforeBoth2, 2_333_333, SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(2_333_332)));
        testST(beforeBoth2, 10_000_000, SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(9_999_999)));
        testST(beforeBoth2, 20_000_000, SysTime(DateTime(1, 1, 1, 0, 0, 1), hnsecs(9_999_999)));
        testST(beforeBoth2, 20_888_888, SysTime(DateTime(1, 1, 1, 0, 0, 2), hnsecs(888_887)));

        {
            auto st = SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999));
            (st += dur!"hnsecs"(52)) += dur!"seconds"(-907);
            assert(st == SysTime(DateTime(0, 12, 31, 23, 44, 53), hnsecs(51)));
        }

        auto duration = dur!"seconds"(12);
        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        static assert(!__traits(compiles, cst += duration));
        static assert(!__traits(compiles, ist += duration));
        static assert(!__traits(compiles, cst -= duration));
        static assert(!__traits(compiles, ist -= duration));

        static void testScope(scope ref SysTime st, scope ref Duration d) @safe
        {
            auto result1 = st += d;
            auto result2 = st -= d;
        }
    }


    /++
        Gives the difference between two $(LREF SysTime)s.

        The legal types of arithmetic for $(LREF SysTime) using this operator
        are

        $(BOOKTABLE,
        $(TR $(TD SysTime) $(TD -) $(TD SysTime) $(TD -->) $(TD duration))
        )
      +/
    Duration opBinary(string op)(SysTime rhs) @safe const pure nothrow scope
    if (op == "-")
    {
        return dur!"hnsecs"(_stdTime - rhs._stdTime);
    }

    @safe unittest
    {
        import core.time;
        assert(SysTime(DateTime(1999, 7, 6, 12, 30, 33)) - SysTime(DateTime(1998, 7, 6, 12, 30, 33)) ==
               dur!"seconds"(31_536_000));
        assert(SysTime(DateTime(1998, 7, 6, 12, 30, 33)) - SysTime(DateTime(1999, 7, 6, 12, 30, 33)) ==
               dur!"seconds"(-31_536_000));

        assert(SysTime(DateTime(1999, 8, 6, 12, 30, 33)) - SysTime(DateTime(1999, 7, 6, 12, 30, 33)) ==
               dur!"seconds"(26_78_400));
        assert(SysTime(DateTime(1999, 7, 6, 12, 30, 33)) - SysTime(DateTime(1999, 8, 6, 12, 30, 33)) ==
               dur!"seconds"(-26_78_400));

        assert(SysTime(DateTime(1999, 7, 6, 12, 30, 33)) - SysTime(DateTime(1999, 7, 5, 12, 30, 33)) ==
               dur!"seconds"(86_400));
        assert(SysTime(DateTime(1999, 7, 5, 12, 30, 33)) - SysTime(DateTime(1999, 7, 6, 12, 30, 33)) ==
               dur!"seconds"(-86_400));

        assert(SysTime(DateTime(1999, 7, 6, 12, 30, 33)) - SysTime(DateTime(1999, 7, 6, 11, 30, 33)) ==
               dur!"seconds"(3600));
        assert(SysTime(DateTime(1999, 7, 6, 11, 30, 33)) - SysTime(DateTime(1999, 7, 6, 12, 30, 33)) ==
               dur!"seconds"(-3600));

        assert(SysTime(DateTime(1999, 7, 6, 12, 31, 33)) - SysTime(DateTime(1999, 7, 6, 12, 30, 33)) ==
               dur!"seconds"(60));
        assert(SysTime(DateTime(1999, 7, 6, 12, 30, 33)) - SysTime(DateTime(1999, 7, 6, 12, 31, 33)) ==
               dur!"seconds"(-60));

        assert(SysTime(DateTime(1999, 7, 6, 12, 30, 34)) - SysTime(DateTime(1999, 7, 6, 12, 30, 33)) ==
               dur!"seconds"(1));
        assert(SysTime(DateTime(1999, 7, 6, 12, 30, 33)) - SysTime(DateTime(1999, 7, 6, 12, 30, 34)) ==
               dur!"seconds"(-1));

        {
            auto dt = DateTime(1999, 7, 6, 12, 30, 33);
            assert(SysTime(dt, msecs(532)) - SysTime(dt) == msecs(532));
            assert(SysTime(dt) - SysTime(dt, msecs(532)) == msecs(-532));

            assert(SysTime(dt, usecs(333_347)) - SysTime(dt) == usecs(333_347));
            assert(SysTime(dt) - SysTime(dt, usecs(333_347)) == usecs(-333_347));

            assert(SysTime(dt, hnsecs(1_234_567)) - SysTime(dt) == hnsecs(1_234_567));
            assert(SysTime(dt) - SysTime(dt, hnsecs(1_234_567)) == hnsecs(-1_234_567));
        }

        assert(SysTime(DateTime(1, 1, 1, 12, 30, 33)) - SysTime(DateTime(1, 1, 1, 0, 0, 0)) == dur!"seconds"(45033));
        assert(SysTime(DateTime(1, 1, 1, 0, 0, 0)) - SysTime(DateTime(1, 1, 1, 12, 30, 33)) == dur!"seconds"(-45033));
        assert(SysTime(DateTime(0, 12, 31, 12, 30, 33)) - SysTime(DateTime(1, 1, 1, 0, 0, 0)) == dur!"seconds"(-41367));
        assert(SysTime(DateTime(1, 1, 1, 0, 0, 0)) - SysTime(DateTime(0, 12, 31, 12, 30, 33)) == dur!"seconds"(41367));

        assert(SysTime(DateTime(1, 1, 1, 0, 0, 0)) - SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999)) ==
               dur!"hnsecs"(1));
        assert(SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999)) - SysTime(DateTime(1, 1, 1, 0, 0, 0)) ==
               dur!"hnsecs"(-1));

        version (Posix)
        {
            import std.datetime.timezone : PosixTimeZone;
            immutable tz = PosixTimeZone.getTimeZone("America/Los_Angeles");
        }
        else version (Windows)
        {
            import std.datetime.timezone : WindowsTimeZone;
            immutable tz = WindowsTimeZone.getTimeZone("Pacific Standard Time");
        }

        {
            auto dt = DateTime(2011, 1, 13, 8, 17, 2);
            auto d = msecs(296);
            assert(SysTime(dt, d, tz) - SysTime(dt, d, tz) == Duration.zero);
            assert(SysTime(dt, d, tz) - SysTime(dt, d, UTC()) == hours(8));
            assert(SysTime(dt, d, UTC()) - SysTime(dt, d, tz) == hours(-8));
        }

        auto st = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(st - st == Duration.zero);
        assert(cst - st == Duration.zero);
        assert(ist - st == Duration.zero);

        assert(st - cst == Duration.zero);
        assert(cst - cst == Duration.zero);
        assert(ist - cst == Duration.zero);

        assert(st - ist == Duration.zero);
        assert(cst - ist == Duration.zero);
        assert(ist - ist == Duration.zero);

        static void testScope(scope ref SysTime left, scope ref SysTime right) @safe
        {
            auto result = left - right;
        }
    }


    /++
        Returns the difference between the two $(LREF SysTime)s in months.

        To get the difference in years, subtract the year property
        of two $(LREF SysTime)s. To get the difference in days or weeks,
        subtract the $(LREF SysTime)s themselves and use the
        $(REF Duration, core,time) that results. Because converting between
        months and smaller units requires a specific date (which
        $(REF Duration, core,time)s don't have), getting the difference in
        months requires some math using both the year and month properties, so
        this is a convenience function for getting the difference in months.

        Note that the number of days in the months or how far into the month
        either date is is irrelevant. It is the difference in the month property
        combined with the difference in years * 12. So, for instance,
        December 31st and January 1st are one month apart just as December 1st
        and January 31st are one month apart.

        Params:
            rhs = The $(LREF SysTime) to subtract from this one.
      +/
    int diffMonths(scope SysTime rhs) @safe const nothrow scope
    {
        return (cast(Date) this).diffMonths(cast(Date) rhs);
    }

    ///
    @safe unittest
    {
        import core.time;
        import std.datetime.date : Date;

        assert(SysTime(Date(1999, 2, 1)).diffMonths(
                   SysTime(Date(1999, 1, 31))) == 1);

        assert(SysTime(Date(1999, 1, 31)).diffMonths(
                   SysTime(Date(1999, 2, 1))) == -1);

        assert(SysTime(Date(1999, 3, 1)).diffMonths(
                   SysTime(Date(1999, 1, 1))) == 2);

        assert(SysTime(Date(1999, 1, 1)).diffMonths(
                   SysTime(Date(1999, 3, 31))) == -2);
    }

    @safe unittest
    {
        import core.time;
        auto st = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(st.diffMonths(st) == 0);
        assert(cst.diffMonths(st) == 0);
        assert(ist.diffMonths(st) == 0);

        assert(st.diffMonths(cst) == 0);
        assert(cst.diffMonths(cst) == 0);
        assert(ist.diffMonths(cst) == 0);

        assert(st.diffMonths(ist) == 0);
        assert(cst.diffMonths(ist) == 0);
        assert(ist.diffMonths(ist) == 0);

        static void testScope(scope ref SysTime left, scope ref SysTime right) @safe
        {
            auto result = left.diffMonths(right);
        }
    }


    /++
        Whether this $(LREF SysTime) is in a leap year.
     +/
    @property bool isLeapYear() @safe const nothrow scope
    {
        return (cast(Date) this).isLeapYear;
    }

    @safe unittest
    {
        import core.time;
        auto st = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(!st.isLeapYear);
        assert(!cst.isLeapYear);
        assert(!ist.isLeapYear);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.isLeapYear;
        }
    }


    /++
        Day of the week this $(LREF SysTime) is on.
      +/
    @property DayOfWeek dayOfWeek() @safe const nothrow scope
    {
        return getDayOfWeek(dayOfGregorianCal);
    }

    @safe unittest
    {
        import core.time;
        auto st = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(st.dayOfWeek == DayOfWeek.tue);
        assert(cst.dayOfWeek == DayOfWeek.tue);
        assert(ist.dayOfWeek == DayOfWeek.tue);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.dayOfWeek;
        }
    }


    /++
        Day of the year this $(LREF SysTime) is on.
      +/
    @property ushort dayOfYear() @safe const nothrow scope
    {
        return (cast(Date) this).dayOfYear;
    }

    ///
    @safe unittest
    {
        import core.time;
        import std.datetime.date : DateTime;

        assert(SysTime(DateTime(1999, 1, 1, 12, 22, 7)).dayOfYear == 1);
        assert(SysTime(DateTime(1999, 12, 31, 7, 2, 59)).dayOfYear == 365);
        assert(SysTime(DateTime(2000, 12, 31, 21, 20, 0)).dayOfYear == 366);
    }

    @safe unittest
    {
        import core.time;
        auto st = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(st.dayOfYear == 187);
        assert(cst.dayOfYear == 187);
        assert(ist.dayOfYear == 187);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.dayOfYear;
        }
    }


    /++
        Day of the year.

        Params:
            day = The day of the year to set which day of the year this
                  $(LREF SysTime) is on.
      +/
    @property void dayOfYear(int day) @safe scope
    {
        immutable hnsecs = adjTime;
        immutable days = convert!("hnsecs", "days")(hnsecs);
        immutable theRest = hnsecs - convert!("days", "hnsecs")(days);

        auto date = Date(cast(int) days);
        date.dayOfYear = day;

        immutable newDaysHNSecs = convert!("days", "hnsecs")(date.dayOfGregorianCal - 1);

        adjTime = newDaysHNSecs + theRest;
    }

    @safe unittest
    {
        import core.time;
        auto st = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        st.dayOfYear = 12;
        assert(st.dayOfYear == 12);
        static assert(!__traits(compiles, cst.dayOfYear = 12));
        static assert(!__traits(compiles, ist.dayOfYear = 12));

        static void testScope(scope ref SysTime st) @safe
        {
            st.dayOfYear = 42;
        }
    }


    /++
        The Xth day of the Gregorian Calendar that this $(LREF SysTime) is on.
     +/
    @property int dayOfGregorianCal() @safe const nothrow scope
    {
        immutable adjustedTime = adjTime;

        // We have to add one because 0 would be midnight, January 1st, 1 A.D.,
        // which would be the 1st day of the Gregorian Calendar, not the 0th. So,
        // simply casting to days is one day off.
        if (adjustedTime > 0)
            return cast(int) getUnitsFromHNSecs!"days"(adjustedTime) + 1;

        long hnsecs = adjustedTime;
        immutable days = cast(int) splitUnitsFromHNSecs!"days"(hnsecs);

        return hnsecs == 0 ? days + 1 : days;
    }

    ///
    @safe unittest
    {
        import core.time;
        import std.datetime.date : DateTime;

        assert(SysTime(DateTime(1, 1, 1, 0, 0, 0)).dayOfGregorianCal == 1);
        assert(SysTime(DateTime(1, 12, 31, 23, 59, 59)).dayOfGregorianCal == 365);
        assert(SysTime(DateTime(2, 1, 1, 2, 2, 2)).dayOfGregorianCal == 366);

        assert(SysTime(DateTime(0, 12, 31, 7, 7, 7)).dayOfGregorianCal == 0);
        assert(SysTime(DateTime(0, 1, 1, 19, 30, 0)).dayOfGregorianCal == -365);
        assert(SysTime(DateTime(-1, 12, 31, 4, 7, 0)).dayOfGregorianCal == -366);

        assert(SysTime(DateTime(2000, 1, 1, 9, 30, 20)).dayOfGregorianCal == 730_120);
        assert(SysTime(DateTime(2010, 12, 31, 15, 45, 50)).dayOfGregorianCal == 734_137);
    }

    @safe unittest
    {
        import core.time;
        // Test A.D.
        assert(SysTime(DateTime(1, 1, 1, 0, 0, 0)).dayOfGregorianCal == 1);
        assert(SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(1)).dayOfGregorianCal == 1);
        assert(SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999)).dayOfGregorianCal == 1);

        assert(SysTime(DateTime(1, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 1);
        assert(SysTime(DateTime(1, 1, 2, 12, 2, 9), msecs(212)).dayOfGregorianCal == 2);
        assert(SysTime(DateTime(1, 2, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 32);
        assert(SysTime(DateTime(2, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 366);
        assert(SysTime(DateTime(3, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 731);
        assert(SysTime(DateTime(4, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 1096);
        assert(SysTime(DateTime(5, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 1462);
        assert(SysTime(DateTime(50, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 17_898);
        assert(SysTime(DateTime(97, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 35_065);
        assert(SysTime(DateTime(100, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 36_160);
        assert(SysTime(DateTime(101, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 36_525);
        assert(SysTime(DateTime(105, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 37_986);
        assert(SysTime(DateTime(200, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 72_684);
        assert(SysTime(DateTime(201, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 73_049);
        assert(SysTime(DateTime(300, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 109_208);
        assert(SysTime(DateTime(301, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 109_573);
        assert(SysTime(DateTime(400, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 145_732);
        assert(SysTime(DateTime(401, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 146_098);
        assert(SysTime(DateTime(500, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 182_257);
        assert(SysTime(DateTime(501, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 182_622);
        assert(SysTime(DateTime(1000, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 364_878);
        assert(SysTime(DateTime(1001, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 365_243);
        assert(SysTime(DateTime(1600, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 584_023);
        assert(SysTime(DateTime(1601, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 584_389);
        assert(SysTime(DateTime(1900, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 693_596);
        assert(SysTime(DateTime(1901, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 693_961);
        assert(SysTime(DateTime(1945, 11, 12, 12, 2, 9), msecs(212)).dayOfGregorianCal == 710_347);
        assert(SysTime(DateTime(1999, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 729_755);
        assert(SysTime(DateTime(2000, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 730_120);
        assert(SysTime(DateTime(2001, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == 730_486);

        assert(SysTime(DateTime(2010, 1, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == 733_773);
        assert(SysTime(DateTime(2010, 1, 31, 23, 59, 59), msecs(999)).dayOfGregorianCal == 733_803);
        assert(SysTime(DateTime(2010, 2, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == 733_804);
        assert(SysTime(DateTime(2010, 2, 28, 23, 59, 59), msecs(999)).dayOfGregorianCal == 733_831);
        assert(SysTime(DateTime(2010, 3, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == 733_832);
        assert(SysTime(DateTime(2010, 3, 31, 23, 59, 59), msecs(999)).dayOfGregorianCal == 733_862);
        assert(SysTime(DateTime(2010, 4, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == 733_863);
        assert(SysTime(DateTime(2010, 4, 30, 23, 59, 59), msecs(999)).dayOfGregorianCal == 733_892);
        assert(SysTime(DateTime(2010, 5, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == 733_893);
        assert(SysTime(DateTime(2010, 5, 31, 23, 59, 59), msecs(999)).dayOfGregorianCal == 733_923);
        assert(SysTime(DateTime(2010, 6, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == 733_924);
        assert(SysTime(DateTime(2010, 6, 30, 23, 59, 59), msecs(999)).dayOfGregorianCal == 733_953);
        assert(SysTime(DateTime(2010, 7, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == 733_954);
        assert(SysTime(DateTime(2010, 7, 31, 23, 59, 59), msecs(999)).dayOfGregorianCal == 733_984);
        assert(SysTime(DateTime(2010, 8, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == 733_985);
        assert(SysTime(DateTime(2010, 8, 31, 23, 59, 59), msecs(999)).dayOfGregorianCal == 734_015);
        assert(SysTime(DateTime(2010, 9, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == 734_016);
        assert(SysTime(DateTime(2010, 9, 30, 23, 59, 59), msecs(999)).dayOfGregorianCal == 734_045);
        assert(SysTime(DateTime(2010, 10, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == 734_046);
        assert(SysTime(DateTime(2010, 10, 31, 23, 59, 59), msecs(999)).dayOfGregorianCal == 734_076);
        assert(SysTime(DateTime(2010, 11, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == 734_077);
        assert(SysTime(DateTime(2010, 11, 30, 23, 59, 59), msecs(999)).dayOfGregorianCal == 734_106);
        assert(SysTime(DateTime(2010, 12, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == 734_107);
        assert(SysTime(DateTime(2010, 12, 31, 23, 59, 59), msecs(999)).dayOfGregorianCal == 734_137);

        assert(SysTime(DateTime(2012, 2, 1, 0, 0, 0)).dayOfGregorianCal == 734_534);
        assert(SysTime(DateTime(2012, 2, 28, 0, 0, 0)).dayOfGregorianCal == 734_561);
        assert(SysTime(DateTime(2012, 2, 29, 0, 0, 0)).dayOfGregorianCal == 734_562);
        assert(SysTime(DateTime(2012, 3, 1, 0, 0, 0)).dayOfGregorianCal == 734_563);

        // Test B.C.
        assert(SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999)).dayOfGregorianCal == 0);
        assert(SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_998)).dayOfGregorianCal == 0);
        assert(SysTime(DateTime(0, 12, 31, 23, 59, 59)).dayOfGregorianCal == 0);
        assert(SysTime(DateTime(0, 12, 31, 0, 0, 0), hnsecs(1)).dayOfGregorianCal == 0);
        assert(SysTime(DateTime(0, 12, 31, 0, 0, 0)).dayOfGregorianCal == 0);

        assert(SysTime(DateTime(-1, 12, 31, 23, 59, 59), hnsecs(9_999_999)).dayOfGregorianCal == -366);
        assert(SysTime(DateTime(-1, 12, 31, 23, 59, 59), hnsecs(9_999_998)).dayOfGregorianCal == -366);
        assert(SysTime(DateTime(-1, 12, 31, 23, 59, 59)).dayOfGregorianCal == -366);
        assert(SysTime(DateTime(-1, 12, 31, 0, 0, 0)).dayOfGregorianCal == -366);

        assert(SysTime(DateTime(0, 12, 31, 12, 2, 9), msecs(212)).dayOfGregorianCal == 0);
        assert(SysTime(DateTime(0, 12, 30, 12, 2, 9), msecs(212)).dayOfGregorianCal == -1);
        assert(SysTime(DateTime(0, 12, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -30);
        assert(SysTime(DateTime(0, 11, 30, 12, 2, 9), msecs(212)).dayOfGregorianCal == -31);

        assert(SysTime(DateTime(-1, 12, 31, 12, 2, 9), msecs(212)).dayOfGregorianCal == -366);
        assert(SysTime(DateTime(-1, 12, 30, 12, 2, 9), msecs(212)).dayOfGregorianCal == -367);
        assert(SysTime(DateTime(-1, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -730);
        assert(SysTime(DateTime(-2, 12, 31, 12, 2, 9), msecs(212)).dayOfGregorianCal == -731);
        assert(SysTime(DateTime(-2, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -1095);
        assert(SysTime(DateTime(-3, 12, 31, 12, 2, 9), msecs(212)).dayOfGregorianCal == -1096);
        assert(SysTime(DateTime(-3, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -1460);
        assert(SysTime(DateTime(-4, 12, 31, 12, 2, 9), msecs(212)).dayOfGregorianCal == -1461);
        assert(SysTime(DateTime(-4, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -1826);
        assert(SysTime(DateTime(-5, 12, 31, 12, 2, 9), msecs(212)).dayOfGregorianCal == -1827);
        assert(SysTime(DateTime(-5, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -2191);
        assert(SysTime(DateTime(-9, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -3652);

        assert(SysTime(DateTime(-49, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -18_262);
        assert(SysTime(DateTime(-50, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -18_627);
        assert(SysTime(DateTime(-97, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -35_794);
        assert(SysTime(DateTime(-99, 12, 31, 12, 2, 9), msecs(212)).dayOfGregorianCal == -36_160);
        assert(SysTime(DateTime(-99, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -36_524);
        assert(SysTime(DateTime(-100, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -36_889);
        assert(SysTime(DateTime(-101, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -37_254);
        assert(SysTime(DateTime(-105, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -38_715);
        assert(SysTime(DateTime(-200, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -73_413);
        assert(SysTime(DateTime(-201, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -73_778);
        assert(SysTime(DateTime(-300, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -109_937);
        assert(SysTime(DateTime(-301, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -110_302);
        assert(SysTime(DateTime(-400, 12, 31, 12, 2, 9), msecs(212)).dayOfGregorianCal == -146_097);
        assert(SysTime(DateTime(-400, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -146_462);
        assert(SysTime(DateTime(-401, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -146_827);
        assert(SysTime(DateTime(-499, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -182_621);
        assert(SysTime(DateTime(-500, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -182_986);
        assert(SysTime(DateTime(-501, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -183_351);
        assert(SysTime(DateTime(-1000, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -365_607);
        assert(SysTime(DateTime(-1001, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -365_972);
        assert(SysTime(DateTime(-1599, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -584_387);
        assert(SysTime(DateTime(-1600, 12, 31, 12, 2, 9), msecs(212)).dayOfGregorianCal == -584_388);
        assert(SysTime(DateTime(-1600, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -584_753);
        assert(SysTime(DateTime(-1601, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -585_118);
        assert(SysTime(DateTime(-1900, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -694_325);
        assert(SysTime(DateTime(-1901, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -694_690);
        assert(SysTime(DateTime(-1999, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -730_484);
        assert(SysTime(DateTime(-2000, 12, 31, 12, 2, 9), msecs(212)).dayOfGregorianCal == -730_485);
        assert(SysTime(DateTime(-2000, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -730_850);
        assert(SysTime(DateTime(-2001, 1, 1, 12, 2, 9), msecs(212)).dayOfGregorianCal == -731_215);

        assert(SysTime(DateTime(-2010, 1, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_502);
        assert(SysTime(DateTime(-2010, 1, 31, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_472);
        assert(SysTime(DateTime(-2010, 2, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_471);
        assert(SysTime(DateTime(-2010, 2, 28, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_444);
        assert(SysTime(DateTime(-2010, 3, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_443);
        assert(SysTime(DateTime(-2010, 3, 31, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_413);
        assert(SysTime(DateTime(-2010, 4, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_412);
        assert(SysTime(DateTime(-2010, 4, 30, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_383);
        assert(SysTime(DateTime(-2010, 5, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_382);
        assert(SysTime(DateTime(-2010, 5, 31, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_352);
        assert(SysTime(DateTime(-2010, 6, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_351);
        assert(SysTime(DateTime(-2010, 6, 30, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_322);
        assert(SysTime(DateTime(-2010, 7, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_321);
        assert(SysTime(DateTime(-2010, 7, 31, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_291);
        assert(SysTime(DateTime(-2010, 8, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_290);
        assert(SysTime(DateTime(-2010, 8, 31, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_260);
        assert(SysTime(DateTime(-2010, 9, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_259);
        assert(SysTime(DateTime(-2010, 9, 30, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_230);
        assert(SysTime(DateTime(-2010, 10, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_229);
        assert(SysTime(DateTime(-2010, 10, 31, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_199);
        assert(SysTime(DateTime(-2010, 11, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_198);
        assert(SysTime(DateTime(-2010, 11, 30, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_169);
        assert(SysTime(DateTime(-2010, 12, 1, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_168);
        assert(SysTime(DateTime(-2010, 12, 31, 23, 59, 59), msecs(999)).dayOfGregorianCal == -734_138);

        assert(SysTime(DateTime(-2012, 2, 1, 0, 0, 0)).dayOfGregorianCal == -735_202);
        assert(SysTime(DateTime(-2012, 2, 28, 0, 0, 0)).dayOfGregorianCal == -735_175);
        assert(SysTime(DateTime(-2012, 2, 29, 0, 0, 0)).dayOfGregorianCal == -735_174);
        assert(SysTime(DateTime(-2012, 3, 1, 0, 0, 0)).dayOfGregorianCal == -735_173);

        // Start of Hebrew Calendar
        assert(SysTime(DateTime(-3760, 9, 7, 0, 0, 0)).dayOfGregorianCal == -1_373_427);

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(cst.dayOfGregorianCal == 729_941);
        assert(ist.dayOfGregorianCal == 729_941);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.dayOfGregorianCal;
        }
    }


    // Test that the logic for the day of the Gregorian Calendar is consistent
    // between Date and SysTime.
    @safe unittest
    {
        import core.time;
        void test(Date date, SysTime st, size_t line = __LINE__)
        {
            if (date.dayOfGregorianCal != st.dayOfGregorianCal)
            {
                throw new AssertError(format("Date [%s] SysTime [%s]", date.dayOfGregorianCal, st.dayOfGregorianCal),
                                      __FILE__, line);
            }
        }

        // Test A.D.
        test(Date(1, 1, 1), SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        test(Date(1, 1, 2), SysTime(DateTime(1, 1, 2, 0, 0, 0), hnsecs(500)));
        test(Date(1, 2, 1), SysTime(DateTime(1, 2, 1, 0, 0, 0), hnsecs(50_000)));
        test(Date(2, 1, 1), SysTime(DateTime(2, 1, 1, 0, 0, 0), hnsecs(9_999_999)));
        test(Date(3, 1, 1), SysTime(DateTime(3, 1, 1, 12, 13, 14)));
        test(Date(4, 1, 1), SysTime(DateTime(4, 1, 1, 12, 13, 14), hnsecs(500)));
        test(Date(5, 1, 1), SysTime(DateTime(5, 1, 1, 12, 13, 14), hnsecs(50_000)));
        test(Date(50, 1, 1), SysTime(DateTime(50, 1, 1, 12, 13, 14), hnsecs(9_999_999)));
        test(Date(97, 1, 1), SysTime(DateTime(97, 1, 1, 23, 59, 59)));
        test(Date(100, 1, 1), SysTime(DateTime(100, 1, 1, 23, 59, 59), hnsecs(500)));
        test(Date(101, 1, 1), SysTime(DateTime(101, 1, 1, 23, 59, 59), hnsecs(50_000)));
        test(Date(105, 1, 1), SysTime(DateTime(105, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
        test(Date(200, 1, 1), SysTime(DateTime(200, 1, 1, 0, 0, 0)));
        test(Date(201, 1, 1), SysTime(DateTime(201, 1, 1, 0, 0, 0), hnsecs(500)));
        test(Date(300, 1, 1), SysTime(DateTime(300, 1, 1, 0, 0, 0), hnsecs(50_000)));
        test(Date(301, 1, 1), SysTime(DateTime(301, 1, 1, 0, 0, 0), hnsecs(9_999_999)));
        test(Date(400, 1, 1), SysTime(DateTime(400, 1, 1, 12, 13, 14)));
        test(Date(401, 1, 1), SysTime(DateTime(401, 1, 1, 12, 13, 14), hnsecs(500)));
        test(Date(500, 1, 1), SysTime(DateTime(500, 1, 1, 12, 13, 14), hnsecs(50_000)));
        test(Date(501, 1, 1), SysTime(DateTime(501, 1, 1, 12, 13, 14), hnsecs(9_999_999)));
        test(Date(1000, 1, 1), SysTime(DateTime(1000, 1, 1, 23, 59, 59)));
        test(Date(1001, 1, 1), SysTime(DateTime(1001, 1, 1, 23, 59, 59), hnsecs(500)));
        test(Date(1600, 1, 1), SysTime(DateTime(1600, 1, 1, 23, 59, 59), hnsecs(50_000)));
        test(Date(1601, 1, 1), SysTime(DateTime(1601, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
        test(Date(1900, 1, 1), SysTime(DateTime(1900, 1, 1, 0, 0, 0)));
        test(Date(1901, 1, 1), SysTime(DateTime(1901, 1, 1, 0, 0, 0), hnsecs(500)));
        test(Date(1945, 11, 12), SysTime(DateTime(1945, 11, 12, 0, 0, 0), hnsecs(50_000)));
        test(Date(1999, 1, 1), SysTime(DateTime(1999, 1, 1, 0, 0, 0), hnsecs(9_999_999)));
        test(Date(1999, 7, 6), SysTime(DateTime(1999, 7, 6, 12, 13, 14)));
        test(Date(2000, 1, 1), SysTime(DateTime(2000, 1, 1, 12, 13, 14), hnsecs(500)));
        test(Date(2001, 1, 1), SysTime(DateTime(2001, 1, 1, 12, 13, 14), hnsecs(50_000)));

        test(Date(2010, 1, 1), SysTime(DateTime(2010, 1, 1, 12, 13, 14), hnsecs(9_999_999)));
        test(Date(2010, 1, 31), SysTime(DateTime(2010, 1, 31, 23, 0, 0)));
        test(Date(2010, 2, 1), SysTime(DateTime(2010, 2, 1, 23, 59, 59), hnsecs(500)));
        test(Date(2010, 2, 28), SysTime(DateTime(2010, 2, 28, 23, 59, 59), hnsecs(50_000)));
        test(Date(2010, 3, 1), SysTime(DateTime(2010, 3, 1, 23, 59, 59), hnsecs(9_999_999)));
        test(Date(2010, 3, 31), SysTime(DateTime(2010, 3, 31, 0, 0, 0)));
        test(Date(2010, 4, 1), SysTime(DateTime(2010, 4, 1, 0, 0, 0), hnsecs(500)));
        test(Date(2010, 4, 30), SysTime(DateTime(2010, 4, 30, 0, 0, 0), hnsecs(50_000)));
        test(Date(2010, 5, 1), SysTime(DateTime(2010, 5, 1, 0, 0, 0), hnsecs(9_999_999)));
        test(Date(2010, 5, 31), SysTime(DateTime(2010, 5, 31, 12, 13, 14)));
        test(Date(2010, 6, 1), SysTime(DateTime(2010, 6, 1, 12, 13, 14), hnsecs(500)));
        test(Date(2010, 6, 30), SysTime(DateTime(2010, 6, 30, 12, 13, 14), hnsecs(50_000)));
        test(Date(2010, 7, 1), SysTime(DateTime(2010, 7, 1, 12, 13, 14), hnsecs(9_999_999)));
        test(Date(2010, 7, 31), SysTime(DateTime(2010, 7, 31, 23, 59, 59)));
        test(Date(2010, 8, 1), SysTime(DateTime(2010, 8, 1, 23, 59, 59), hnsecs(500)));
        test(Date(2010, 8, 31), SysTime(DateTime(2010, 8, 31, 23, 59, 59), hnsecs(50_000)));
        test(Date(2010, 9, 1), SysTime(DateTime(2010, 9, 1, 23, 59, 59), hnsecs(9_999_999)));
        test(Date(2010, 9, 30), SysTime(DateTime(2010, 9, 30, 12, 0, 0)));
        test(Date(2010, 10, 1), SysTime(DateTime(2010, 10, 1, 0, 12, 0), hnsecs(500)));
        test(Date(2010, 10, 31), SysTime(DateTime(2010, 10, 31, 0, 0, 12), hnsecs(50_000)));
        test(Date(2010, 11, 1), SysTime(DateTime(2010, 11, 1, 23, 0, 0), hnsecs(9_999_999)));
        test(Date(2010, 11, 30), SysTime(DateTime(2010, 11, 30, 0, 59, 0)));
        test(Date(2010, 12, 1), SysTime(DateTime(2010, 12, 1, 0, 0, 59), hnsecs(500)));
        test(Date(2010, 12, 31), SysTime(DateTime(2010, 12, 31, 0, 59, 59), hnsecs(50_000)));

        test(Date(2012, 2, 1), SysTime(DateTime(2012, 2, 1, 23, 0, 59), hnsecs(9_999_999)));
        test(Date(2012, 2, 28), SysTime(DateTime(2012, 2, 28, 23, 59, 0)));
        test(Date(2012, 2, 29), SysTime(DateTime(2012, 2, 29, 7, 7, 7), hnsecs(7)));
        test(Date(2012, 3, 1), SysTime(DateTime(2012, 3, 1, 7, 7, 7), hnsecs(7)));

        // Test B.C.
        test(Date(0, 12, 31), SysTime(DateTime(0, 12, 31, 0, 0, 0)));
        test(Date(0, 12, 30), SysTime(DateTime(0, 12, 30, 0, 0, 0), hnsecs(500)));
        test(Date(0, 12, 1), SysTime(DateTime(0, 12, 1, 0, 0, 0), hnsecs(50_000)));
        test(Date(0, 11, 30), SysTime(DateTime(0, 11, 30, 0, 0, 0), hnsecs(9_999_999)));

        test(Date(-1, 12, 31), SysTime(DateTime(-1, 12, 31, 12, 13, 14)));
        test(Date(-1, 12, 30), SysTime(DateTime(-1, 12, 30, 12, 13, 14), hnsecs(500)));
        test(Date(-1, 1, 1), SysTime(DateTime(-1, 1, 1, 12, 13, 14), hnsecs(50_000)));
        test(Date(-2, 12, 31), SysTime(DateTime(-2, 12, 31, 12, 13, 14), hnsecs(9_999_999)));
        test(Date(-2, 1, 1), SysTime(DateTime(-2, 1, 1, 23, 59, 59)));
        test(Date(-3, 12, 31), SysTime(DateTime(-3, 12, 31, 23, 59, 59), hnsecs(500)));
        test(Date(-3, 1, 1), SysTime(DateTime(-3, 1, 1, 23, 59, 59), hnsecs(50_000)));
        test(Date(-4, 12, 31), SysTime(DateTime(-4, 12, 31, 23, 59, 59), hnsecs(9_999_999)));
        test(Date(-4, 1, 1), SysTime(DateTime(-4, 1, 1, 0, 0, 0)));
        test(Date(-5, 12, 31), SysTime(DateTime(-5, 12, 31, 0, 0, 0), hnsecs(500)));
        test(Date(-5, 1, 1), SysTime(DateTime(-5, 1, 1, 0, 0, 0), hnsecs(50_000)));
        test(Date(-9, 1, 1), SysTime(DateTime(-9, 1, 1, 0, 0, 0), hnsecs(9_999_999)));

        test(Date(-49, 1, 1), SysTime(DateTime(-49, 1, 1, 12, 13, 14)));
        test(Date(-50, 1, 1), SysTime(DateTime(-50, 1, 1, 12, 13, 14), hnsecs(500)));
        test(Date(-97, 1, 1), SysTime(DateTime(-97, 1, 1, 12, 13, 14), hnsecs(50_000)));
        test(Date(-99, 12, 31), SysTime(DateTime(-99, 12, 31, 12, 13, 14), hnsecs(9_999_999)));
        test(Date(-99, 1, 1), SysTime(DateTime(-99, 1, 1, 23, 59, 59)));
        test(Date(-100, 1, 1), SysTime(DateTime(-100, 1, 1, 23, 59, 59), hnsecs(500)));
        test(Date(-101, 1, 1), SysTime(DateTime(-101, 1, 1, 23, 59, 59), hnsecs(50_000)));
        test(Date(-105, 1, 1), SysTime(DateTime(-105, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
        test(Date(-200, 1, 1), SysTime(DateTime(-200, 1, 1, 0, 0, 0)));
        test(Date(-201, 1, 1), SysTime(DateTime(-201, 1, 1, 0, 0, 0), hnsecs(500)));
        test(Date(-300, 1, 1), SysTime(DateTime(-300, 1, 1, 0, 0, 0), hnsecs(50_000)));
        test(Date(-301, 1, 1), SysTime(DateTime(-301, 1, 1, 0, 0, 0), hnsecs(9_999_999)));
        test(Date(-400, 12, 31), SysTime(DateTime(-400, 12, 31, 12, 13, 14)));
        test(Date(-400, 1, 1), SysTime(DateTime(-400, 1, 1, 12, 13, 14), hnsecs(500)));
        test(Date(-401, 1, 1), SysTime(DateTime(-401, 1, 1, 12, 13, 14), hnsecs(50_000)));
        test(Date(-499, 1, 1), SysTime(DateTime(-499, 1, 1, 12, 13, 14), hnsecs(9_999_999)));
        test(Date(-500, 1, 1), SysTime(DateTime(-500, 1, 1, 23, 59, 59)));
        test(Date(-501, 1, 1), SysTime(DateTime(-501, 1, 1, 23, 59, 59), hnsecs(500)));
        test(Date(-1000, 1, 1), SysTime(DateTime(-1000, 1, 1, 23, 59, 59), hnsecs(50_000)));
        test(Date(-1001, 1, 1), SysTime(DateTime(-1001, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
        test(Date(-1599, 1, 1), SysTime(DateTime(-1599, 1, 1, 0, 0, 0)));
        test(Date(-1600, 12, 31), SysTime(DateTime(-1600, 12, 31, 0, 0, 0), hnsecs(500)));
        test(Date(-1600, 1, 1), SysTime(DateTime(-1600, 1, 1, 0, 0, 0), hnsecs(50_000)));
        test(Date(-1601, 1, 1), SysTime(DateTime(-1601, 1, 1, 0, 0, 0), hnsecs(9_999_999)));
        test(Date(-1900, 1, 1), SysTime(DateTime(-1900, 1, 1, 12, 13, 14)));
        test(Date(-1901, 1, 1), SysTime(DateTime(-1901, 1, 1, 12, 13, 14), hnsecs(500)));
        test(Date(-1999, 1, 1), SysTime(DateTime(-1999, 1, 1, 12, 13, 14), hnsecs(50_000)));
        test(Date(-1999, 7, 6), SysTime(DateTime(-1999, 7, 6, 12, 13, 14), hnsecs(9_999_999)));
        test(Date(-2000, 12, 31), SysTime(DateTime(-2000, 12, 31, 23, 59, 59)));
        test(Date(-2000, 1, 1), SysTime(DateTime(-2000, 1, 1, 23, 59, 59), hnsecs(500)));
        test(Date(-2001, 1, 1), SysTime(DateTime(-2001, 1, 1, 23, 59, 59), hnsecs(50_000)));

        test(Date(-2010, 1, 1), SysTime(DateTime(-2010, 1, 1, 23, 59, 59), hnsecs(9_999_999)));
        test(Date(-2010, 1, 31), SysTime(DateTime(-2010, 1, 31, 0, 0, 0)));
        test(Date(-2010, 2, 1), SysTime(DateTime(-2010, 2, 1, 0, 0, 0), hnsecs(500)));
        test(Date(-2010, 2, 28), SysTime(DateTime(-2010, 2, 28, 0, 0, 0), hnsecs(50_000)));
        test(Date(-2010, 3, 1), SysTime(DateTime(-2010, 3, 1, 0, 0, 0), hnsecs(9_999_999)));
        test(Date(-2010, 3, 31), SysTime(DateTime(-2010, 3, 31, 12, 13, 14)));
        test(Date(-2010, 4, 1), SysTime(DateTime(-2010, 4, 1, 12, 13, 14), hnsecs(500)));
        test(Date(-2010, 4, 30), SysTime(DateTime(-2010, 4, 30, 12, 13, 14), hnsecs(50_000)));
        test(Date(-2010, 5, 1), SysTime(DateTime(-2010, 5, 1, 12, 13, 14), hnsecs(9_999_999)));
        test(Date(-2010, 5, 31), SysTime(DateTime(-2010, 5, 31, 23, 59, 59)));
        test(Date(-2010, 6, 1), SysTime(DateTime(-2010, 6, 1, 23, 59, 59), hnsecs(500)));
        test(Date(-2010, 6, 30), SysTime(DateTime(-2010, 6, 30, 23, 59, 59), hnsecs(50_000)));
        test(Date(-2010, 7, 1), SysTime(DateTime(-2010, 7, 1, 23, 59, 59), hnsecs(9_999_999)));
        test(Date(-2010, 7, 31), SysTime(DateTime(-2010, 7, 31, 0, 0, 0)));
        test(Date(-2010, 8, 1), SysTime(DateTime(-2010, 8, 1, 0, 0, 0), hnsecs(500)));
        test(Date(-2010, 8, 31), SysTime(DateTime(-2010, 8, 31, 0, 0, 0), hnsecs(50_000)));
        test(Date(-2010, 9, 1), SysTime(DateTime(-2010, 9, 1, 0, 0, 0), hnsecs(9_999_999)));
        test(Date(-2010, 9, 30), SysTime(DateTime(-2010, 9, 30, 12, 0, 0)));
        test(Date(-2010, 10, 1), SysTime(DateTime(-2010, 10, 1, 0, 12, 0), hnsecs(500)));
        test(Date(-2010, 10, 31), SysTime(DateTime(-2010, 10, 31, 0, 0, 12), hnsecs(50_000)));
        test(Date(-2010, 11, 1), SysTime(DateTime(-2010, 11, 1, 23, 0, 0), hnsecs(9_999_999)));
        test(Date(-2010, 11, 30), SysTime(DateTime(-2010, 11, 30, 0, 59, 0)));
        test(Date(-2010, 12, 1), SysTime(DateTime(-2010, 12, 1, 0, 0, 59), hnsecs(500)));
        test(Date(-2010, 12, 31), SysTime(DateTime(-2010, 12, 31, 0, 59, 59), hnsecs(50_000)));

        test(Date(-2012, 2, 1), SysTime(DateTime(-2012, 2, 1, 23, 0, 59), hnsecs(9_999_999)));
        test(Date(-2012, 2, 28), SysTime(DateTime(-2012, 2, 28, 23, 59, 0)));
        test(Date(-2012, 2, 29), SysTime(DateTime(-2012, 2, 29, 7, 7, 7), hnsecs(7)));
        test(Date(-2012, 3, 1), SysTime(DateTime(-2012, 3, 1, 7, 7, 7), hnsecs(7)));

        test(Date(-3760, 9, 7), SysTime(DateTime(-3760, 9, 7, 0, 0, 0)));
    }


    /++
        The Xth day of the Gregorian Calendar that this $(LREF SysTime) is on.
        Setting this property does not affect the time portion of $(LREF SysTime).

        Params:
            days = The day of the Gregorian Calendar to set this $(LREF SysTime)
                   to.
     +/
    @property void dayOfGregorianCal(int days) @safe nothrow scope
    {
        auto hnsecs = adjTime;
        hnsecs = removeUnitsFromHNSecs!"days"(hnsecs);

        if (hnsecs < 0)
            hnsecs += convert!("hours", "hnsecs")(24);

        if (--days < 0)
        {
            hnsecs -= convert!("hours", "hnsecs")(24);
            ++days;
        }

        immutable newDaysHNSecs = convert!("days", "hnsecs")(days);

        adjTime = newDaysHNSecs + hnsecs;
    }

    ///
    @safe unittest
    {
        import core.time;
        import std.datetime.date : DateTime;

        auto st = SysTime(DateTime(0, 1, 1, 12, 0, 0));
        st.dayOfGregorianCal = 1;
        assert(st == SysTime(DateTime(1, 1, 1, 12, 0, 0)));

        st.dayOfGregorianCal = 365;
        assert(st == SysTime(DateTime(1, 12, 31, 12, 0, 0)));

        st.dayOfGregorianCal = 366;
        assert(st == SysTime(DateTime(2, 1, 1, 12, 0, 0)));

        st.dayOfGregorianCal = 0;
        assert(st == SysTime(DateTime(0, 12, 31, 12, 0, 0)));

        st.dayOfGregorianCal = -365;
        assert(st == SysTime(DateTime(-0, 1, 1, 12, 0, 0)));

        st.dayOfGregorianCal = -366;
        assert(st == SysTime(DateTime(-1, 12, 31, 12, 0, 0)));

        st.dayOfGregorianCal = 730_120;
        assert(st == SysTime(DateTime(2000, 1, 1, 12, 0, 0)));

        st.dayOfGregorianCal = 734_137;
        assert(st == SysTime(DateTime(2010, 12, 31, 12, 0, 0)));
    }

    @safe unittest
    {
        import core.time;
        void testST(SysTime orig, int day, SysTime expected, size_t line = __LINE__) @safe
        {
            orig.dayOfGregorianCal = day;
            if (orig != expected)
                throw new AssertError(format("Failed. actual [%s] != expected [%s]", orig, expected), __FILE__, line);
        }

        // Test A.D.
        testST(SysTime(DateTime(1, 1, 1, 0, 0, 0)), 1, SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        testST(SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(1)), 1, SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(1)));
        testST(SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999)), 1,
               SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999)));

        // Test B.C.
        testST(SysTime(DateTime(0, 1, 1, 0, 0, 0)), 0, SysTime(DateTime(0, 12, 31, 0, 0, 0)));
        testST(SysTime(DateTime(0, 1, 1, 23, 59, 59), hnsecs(9_999_999)), 0,
               SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999)));
        testST(SysTime(DateTime(0, 1, 1, 23, 59, 59), hnsecs(1)), 0,
               SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(1)));
        testST(SysTime(DateTime(0, 1, 1, 23, 59, 59)), 0, SysTime(DateTime(0, 12, 31, 23, 59, 59)));

        // Test Both.
        testST(SysTime(DateTime(-512, 7, 20, 0, 0, 0)), 1, SysTime(DateTime(1, 1, 1, 0, 0, 0)));
        testST(SysTime(DateTime(-513, 6, 6, 0, 0, 0), hnsecs(1)), 1, SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(1)));
        testST(SysTime(DateTime(-511, 5, 7, 23, 59, 59), hnsecs(9_999_999)), 1,
               SysTime(DateTime(1, 1, 1, 23, 59, 59), hnsecs(9_999_999)));

        testST(SysTime(DateTime(1607, 4, 8, 0, 0, 0)), 0, SysTime(DateTime(0, 12, 31, 0, 0, 0)));
        testST(SysTime(DateTime(1500, 3, 9, 23, 59, 59), hnsecs(9_999_999)), 0,
               SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999)));
        testST(SysTime(DateTime(999, 2, 10, 23, 59, 59), hnsecs(1)), 0,
               SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(1)));
        testST(SysTime(DateTime(2007, 12, 11, 23, 59, 59)), 0, SysTime(DateTime(0, 12, 31, 23, 59, 59)));


        auto st = SysTime(DateTime(1, 1, 1, 12, 2, 9), msecs(212));

        void testST2(int day, SysTime expected, size_t line = __LINE__) @safe
        {
            st.dayOfGregorianCal = day;
            if (st != expected)
                throw new AssertError(format("Failed. actual [%s] != expected [%s]", st, expected), __FILE__, line);
        }

        // Test A.D.
        testST2(1, SysTime(DateTime(1, 1, 1, 12, 2, 9), msecs(212)));
        testST2(2, SysTime(DateTime(1, 1, 2, 12, 2, 9), msecs(212)));
        testST2(32, SysTime(DateTime(1, 2, 1, 12, 2, 9), msecs(212)));
        testST2(366, SysTime(DateTime(2, 1, 1, 12, 2, 9), msecs(212)));
        testST2(731, SysTime(DateTime(3, 1, 1, 12, 2, 9), msecs(212)));
        testST2(1096, SysTime(DateTime(4, 1, 1, 12, 2, 9), msecs(212)));
        testST2(1462, SysTime(DateTime(5, 1, 1, 12, 2, 9), msecs(212)));
        testST2(17_898, SysTime(DateTime(50, 1, 1, 12, 2, 9), msecs(212)));
        testST2(35_065, SysTime(DateTime(97, 1, 1, 12, 2, 9), msecs(212)));
        testST2(36_160, SysTime(DateTime(100, 1, 1, 12, 2, 9), msecs(212)));
        testST2(36_525, SysTime(DateTime(101, 1, 1, 12, 2, 9), msecs(212)));
        testST2(37_986, SysTime(DateTime(105, 1, 1, 12, 2, 9), msecs(212)));
        testST2(72_684, SysTime(DateTime(200, 1, 1, 12, 2, 9), msecs(212)));
        testST2(73_049, SysTime(DateTime(201, 1, 1, 12, 2, 9), msecs(212)));
        testST2(109_208, SysTime(DateTime(300, 1, 1, 12, 2, 9), msecs(212)));
        testST2(109_573, SysTime(DateTime(301, 1, 1, 12, 2, 9), msecs(212)));
        testST2(145_732, SysTime(DateTime(400, 1, 1, 12, 2, 9), msecs(212)));
        testST2(146_098, SysTime(DateTime(401, 1, 1, 12, 2, 9), msecs(212)));
        testST2(182_257, SysTime(DateTime(500, 1, 1, 12, 2, 9), msecs(212)));
        testST2(182_622, SysTime(DateTime(501, 1, 1, 12, 2, 9), msecs(212)));
        testST2(364_878, SysTime(DateTime(1000, 1, 1, 12, 2, 9), msecs(212)));
        testST2(365_243, SysTime(DateTime(1001, 1, 1, 12, 2, 9), msecs(212)));
        testST2(584_023, SysTime(DateTime(1600, 1, 1, 12, 2, 9), msecs(212)));
        testST2(584_389, SysTime(DateTime(1601, 1, 1, 12, 2, 9), msecs(212)));
        testST2(693_596, SysTime(DateTime(1900, 1, 1, 12, 2, 9), msecs(212)));
        testST2(693_961, SysTime(DateTime(1901, 1, 1, 12, 2, 9), msecs(212)));
        testST2(729_755, SysTime(DateTime(1999, 1, 1, 12, 2, 9), msecs(212)));
        testST2(730_120, SysTime(DateTime(2000, 1, 1, 12, 2, 9), msecs(212)));
        testST2(730_486, SysTime(DateTime(2001, 1, 1, 12, 2, 9), msecs(212)));

        testST2(733_773, SysTime(DateTime(2010, 1, 1, 12, 2, 9), msecs(212)));
        testST2(733_803, SysTime(DateTime(2010, 1, 31, 12, 2, 9), msecs(212)));
        testST2(733_804, SysTime(DateTime(2010, 2, 1, 12, 2, 9), msecs(212)));
        testST2(733_831, SysTime(DateTime(2010, 2, 28, 12, 2, 9), msecs(212)));
        testST2(733_832, SysTime(DateTime(2010, 3, 1, 12, 2, 9), msecs(212)));
        testST2(733_862, SysTime(DateTime(2010, 3, 31, 12, 2, 9), msecs(212)));
        testST2(733_863, SysTime(DateTime(2010, 4, 1, 12, 2, 9), msecs(212)));
        testST2(733_892, SysTime(DateTime(2010, 4, 30, 12, 2, 9), msecs(212)));
        testST2(733_893, SysTime(DateTime(2010, 5, 1, 12, 2, 9), msecs(212)));
        testST2(733_923, SysTime(DateTime(2010, 5, 31, 12, 2, 9), msecs(212)));
        testST2(733_924, SysTime(DateTime(2010, 6, 1, 12, 2, 9), msecs(212)));
        testST2(733_953, SysTime(DateTime(2010, 6, 30, 12, 2, 9), msecs(212)));
        testST2(733_954, SysTime(DateTime(2010, 7, 1, 12, 2, 9), msecs(212)));
        testST2(733_984, SysTime(DateTime(2010, 7, 31, 12, 2, 9), msecs(212)));
        testST2(733_985, SysTime(DateTime(2010, 8, 1, 12, 2, 9), msecs(212)));
        testST2(734_015, SysTime(DateTime(2010, 8, 31, 12, 2, 9), msecs(212)));
        testST2(734_016, SysTime(DateTime(2010, 9, 1, 12, 2, 9), msecs(212)));
        testST2(734_045, SysTime(DateTime(2010, 9, 30, 12, 2, 9), msecs(212)));
        testST2(734_046, SysTime(DateTime(2010, 10, 1, 12, 2, 9), msecs(212)));
        testST2(734_076, SysTime(DateTime(2010, 10, 31, 12, 2, 9), msecs(212)));
        testST2(734_077, SysTime(DateTime(2010, 11, 1, 12, 2, 9), msecs(212)));
        testST2(734_106, SysTime(DateTime(2010, 11, 30, 12, 2, 9), msecs(212)));
        testST2(734_107, SysTime(DateTime(2010, 12, 1, 12, 2, 9), msecs(212)));
        testST2(734_137, SysTime(DateTime(2010, 12, 31, 12, 2, 9), msecs(212)));

        testST2(734_534, SysTime(DateTime(2012, 2, 1, 12, 2, 9), msecs(212)));
        testST2(734_561, SysTime(DateTime(2012, 2, 28, 12, 2, 9), msecs(212)));
        testST2(734_562, SysTime(DateTime(2012, 2, 29, 12, 2, 9), msecs(212)));
        testST2(734_563, SysTime(DateTime(2012, 3, 1, 12, 2, 9), msecs(212)));

        testST2(734_534,  SysTime(DateTime(2012, 2, 1, 12, 2, 9), msecs(212)));

        testST2(734_561, SysTime(DateTime(2012, 2, 28, 12, 2, 9), msecs(212)));
        testST2(734_562, SysTime(DateTime(2012, 2, 29, 12, 2, 9), msecs(212)));
        testST2(734_563, SysTime(DateTime(2012, 3, 1, 12, 2, 9), msecs(212)));

        // Test B.C.
        testST2(0, SysTime(DateTime(0, 12, 31, 12, 2, 9), msecs(212)));
        testST2(-1, SysTime(DateTime(0, 12, 30, 12, 2, 9), msecs(212)));
        testST2(-30, SysTime(DateTime(0, 12, 1, 12, 2, 9), msecs(212)));
        testST2(-31, SysTime(DateTime(0, 11, 30, 12, 2, 9), msecs(212)));

        testST2(-366, SysTime(DateTime(-1, 12, 31, 12, 2, 9), msecs(212)));
        testST2(-367, SysTime(DateTime(-1, 12, 30, 12, 2, 9), msecs(212)));
        testST2(-730, SysTime(DateTime(-1, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-731, SysTime(DateTime(-2, 12, 31, 12, 2, 9), msecs(212)));
        testST2(-1095, SysTime(DateTime(-2, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-1096, SysTime(DateTime(-3, 12, 31, 12, 2, 9), msecs(212)));
        testST2(-1460, SysTime(DateTime(-3, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-1461, SysTime(DateTime(-4, 12, 31, 12, 2, 9), msecs(212)));
        testST2(-1826, SysTime(DateTime(-4, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-1827, SysTime(DateTime(-5, 12, 31, 12, 2, 9), msecs(212)));
        testST2(-2191, SysTime(DateTime(-5, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-3652, SysTime(DateTime(-9, 1, 1, 12, 2, 9), msecs(212)));

        testST2(-18_262, SysTime(DateTime(-49, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-18_627, SysTime(DateTime(-50, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-35_794, SysTime(DateTime(-97, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-36_160, SysTime(DateTime(-99, 12, 31, 12, 2, 9), msecs(212)));
        testST2(-36_524, SysTime(DateTime(-99, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-36_889, SysTime(DateTime(-100, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-37_254, SysTime(DateTime(-101, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-38_715, SysTime(DateTime(-105, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-73_413, SysTime(DateTime(-200, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-73_778, SysTime(DateTime(-201, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-109_937, SysTime(DateTime(-300, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-110_302, SysTime(DateTime(-301, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-146_097, SysTime(DateTime(-400, 12, 31, 12, 2, 9), msecs(212)));
        testST2(-146_462, SysTime(DateTime(-400, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-146_827, SysTime(DateTime(-401, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-182_621, SysTime(DateTime(-499, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-182_986, SysTime(DateTime(-500, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-183_351, SysTime(DateTime(-501, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-365_607, SysTime(DateTime(-1000, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-365_972, SysTime(DateTime(-1001, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-584_387, SysTime(DateTime(-1599, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-584_388, SysTime(DateTime(-1600, 12, 31, 12, 2, 9), msecs(212)));
        testST2(-584_753, SysTime(DateTime(-1600, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-585_118, SysTime(DateTime(-1601, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-694_325, SysTime(DateTime(-1900, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-694_690, SysTime(DateTime(-1901, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-730_484, SysTime(DateTime(-1999, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-730_485, SysTime(DateTime(-2000, 12, 31, 12, 2, 9), msecs(212)));
        testST2(-730_850, SysTime(DateTime(-2000, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-731_215, SysTime(DateTime(-2001, 1, 1, 12, 2, 9), msecs(212)));

        testST2(-734_502, SysTime(DateTime(-2010, 1, 1, 12, 2, 9), msecs(212)));
        testST2(-734_472, SysTime(DateTime(-2010, 1, 31, 12, 2, 9), msecs(212)));
        testST2(-734_471, SysTime(DateTime(-2010, 2, 1, 12, 2, 9), msecs(212)));
        testST2(-734_444, SysTime(DateTime(-2010, 2, 28, 12, 2, 9), msecs(212)));
        testST2(-734_443, SysTime(DateTime(-2010, 3, 1, 12, 2, 9), msecs(212)));
        testST2(-734_413, SysTime(DateTime(-2010, 3, 31, 12, 2, 9), msecs(212)));
        testST2(-734_412, SysTime(DateTime(-2010, 4, 1, 12, 2, 9), msecs(212)));
        testST2(-734_383, SysTime(DateTime(-2010, 4, 30, 12, 2, 9), msecs(212)));
        testST2(-734_382, SysTime(DateTime(-2010, 5, 1, 12, 2, 9), msecs(212)));
        testST2(-734_352, SysTime(DateTime(-2010, 5, 31, 12, 2, 9), msecs(212)));
        testST2(-734_351, SysTime(DateTime(-2010, 6, 1, 12, 2, 9), msecs(212)));
        testST2(-734_322, SysTime(DateTime(-2010, 6, 30, 12, 2, 9), msecs(212)));
        testST2(-734_321, SysTime(DateTime(-2010, 7, 1, 12, 2, 9), msecs(212)));
        testST2(-734_291, SysTime(DateTime(-2010, 7, 31, 12, 2, 9), msecs(212)));
        testST2(-734_290, SysTime(DateTime(-2010, 8, 1, 12, 2, 9), msecs(212)));
        testST2(-734_260, SysTime(DateTime(-2010, 8, 31, 12, 2, 9), msecs(212)));
        testST2(-734_259, SysTime(DateTime(-2010, 9, 1, 12, 2, 9), msecs(212)));
        testST2(-734_230, SysTime(DateTime(-2010, 9, 30, 12, 2, 9), msecs(212)));
        testST2(-734_229, SysTime(DateTime(-2010, 10, 1, 12, 2, 9), msecs(212)));
        testST2(-734_199, SysTime(DateTime(-2010, 10, 31, 12, 2, 9), msecs(212)));
        testST2(-734_198, SysTime(DateTime(-2010, 11, 1, 12, 2, 9), msecs(212)));
        testST2(-734_169, SysTime(DateTime(-2010, 11, 30, 12, 2, 9), msecs(212)));
        testST2(-734_168, SysTime(DateTime(-2010, 12, 1, 12, 2, 9), msecs(212)));
        testST2(-734_138, SysTime(DateTime(-2010, 12, 31, 12, 2, 9), msecs(212)));

        testST2(-735_202, SysTime(DateTime(-2012, 2, 1, 12, 2, 9), msecs(212)));
        testST2(-735_175, SysTime(DateTime(-2012, 2, 28, 12, 2, 9), msecs(212)));
        testST2(-735_174, SysTime(DateTime(-2012, 2, 29, 12, 2, 9), msecs(212)));
        testST2(-735_173, SysTime(DateTime(-2012, 3, 1, 12, 2, 9), msecs(212)));

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        static assert(!__traits(compiles, cst.dayOfGregorianCal = 7));
        static assert(!__traits(compiles, ist.dayOfGregorianCal = 7));

        static void testScope(scope ref SysTime st) @safe
        {
            st.dayOfGregorianCal = 42;
        }
    }


    /++
        The ISO 8601 week of the year that this $(LREF SysTime) is in.

        See_Also:
            $(HTTP en.wikipedia.org/wiki/ISO_week_date, ISO Week Date).
      +/
    @property ubyte isoWeek() @safe const nothrow scope
    {
        return (cast(Date) this).isoWeek;
    }

    ///
    @safe unittest
    {
        import core.time;
        import std.datetime.date : Date;

        auto st = SysTime(Date(1999, 7, 6));
        const cst = SysTime(Date(2010, 5, 1));
        immutable ist = SysTime(Date(2015, 10, 10));

        assert(st.isoWeek == 27);
        assert(cst.isoWeek == 17);
        assert(ist.isoWeek == 41);
    }

    @safe unittest
    {
        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.isoWeek;
        }
    }


    /++
        $(LREF SysTime) for the last day in the month that this Date is in.
        The time portion of endOfMonth is always 23:59:59.9999999.
      +/
    @property SysTime endOfMonth() @safe const nothrow return scope
    {
        immutable hnsecs = adjTime;
        immutable days = getUnitsFromHNSecs!"days"(hnsecs);

        auto date = Date(cast(int) days + 1).endOfMonth;
        auto newDays = date.dayOfGregorianCal - 1;
        long theTimeHNSecs;

        if (newDays < 0)
        {
            theTimeHNSecs = -1;
            ++newDays;
        }
        else
            theTimeHNSecs = convert!("days", "hnsecs")(1) - 1;

        immutable newDaysHNSecs = convert!("days", "hnsecs")(newDays);

        auto retval = SysTime(this._stdTime, this._timezone);
        retval.adjTime = newDaysHNSecs + theTimeHNSecs;

        return retval;
    }

    ///
    @safe unittest
    {
        import core.time : msecs, usecs, hnsecs;
        import std.datetime.date : DateTime;

        assert(SysTime(DateTime(1999, 1, 6, 0, 0, 0)).endOfMonth ==
               SysTime(DateTime(1999, 1, 31, 23, 59, 59), hnsecs(9_999_999)));

        assert(SysTime(DateTime(1999, 2, 7, 19, 30, 0), msecs(24)).endOfMonth ==
               SysTime(DateTime(1999, 2, 28, 23, 59, 59), hnsecs(9_999_999)));

        assert(SysTime(DateTime(2000, 2, 7, 5, 12, 27), usecs(5203)).endOfMonth ==
               SysTime(DateTime(2000, 2, 29, 23, 59, 59), hnsecs(9_999_999)));

        assert(SysTime(DateTime(2000, 6, 4, 12, 22, 9), hnsecs(12345)).endOfMonth ==
               SysTime(DateTime(2000, 6, 30, 23, 59, 59), hnsecs(9_999_999)));
    }

    @safe unittest
    {
        import core.time;
        // Test A.D.
        assert(SysTime(Date(1999, 1, 1)).endOfMonth == SysTime(DateTime(1999, 1, 31, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(1999, 2, 1)).endOfMonth == SysTime(DateTime(1999, 2, 28, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(2000, 2, 1)).endOfMonth == SysTime(DateTime(2000, 2, 29, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(1999, 3, 1)).endOfMonth == SysTime(DateTime(1999, 3, 31, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(1999, 4, 1)).endOfMonth == SysTime(DateTime(1999, 4, 30, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(1999, 5, 1)).endOfMonth == SysTime(DateTime(1999, 5, 31, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(1999, 6, 1)).endOfMonth == SysTime(DateTime(1999, 6, 30, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(1999, 7, 1)).endOfMonth == SysTime(DateTime(1999, 7, 31, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(1999, 8, 1)).endOfMonth == SysTime(DateTime(1999, 8, 31, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(1999, 9, 1)).endOfMonth == SysTime(DateTime(1999, 9, 30, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(1999, 10, 1)).endOfMonth == SysTime(DateTime(1999, 10, 31, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(1999, 11, 1)).endOfMonth == SysTime(DateTime(1999, 11, 30, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(1999, 12, 1)).endOfMonth == SysTime(DateTime(1999, 12, 31, 23, 59, 59), hnsecs(9_999_999)));

        // Test B.C.
        assert(SysTime(Date(-1999, 1, 1)).endOfMonth == SysTime(DateTime(-1999, 1, 31, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(-1999, 2, 1)).endOfMonth == SysTime(DateTime(-1999, 2, 28, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(-2000, 2, 1)).endOfMonth == SysTime(DateTime(-2000, 2, 29, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(-1999, 3, 1)).endOfMonth == SysTime(DateTime(-1999, 3, 31, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(-1999, 4, 1)).endOfMonth == SysTime(DateTime(-1999, 4, 30, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(-1999, 5, 1)).endOfMonth == SysTime(DateTime(-1999, 5, 31, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(-1999, 6, 1)).endOfMonth == SysTime(DateTime(-1999, 6, 30, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(-1999, 7, 1)).endOfMonth == SysTime(DateTime(-1999, 7, 31, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(-1999, 8, 1)).endOfMonth == SysTime(DateTime(-1999, 8, 31, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(-1999, 9, 1)).endOfMonth == SysTime(DateTime(-1999, 9, 30, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(-1999, 10, 1)).endOfMonth ==
               SysTime(DateTime(-1999, 10, 31, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(-1999, 11, 1)).endOfMonth ==
               SysTime(DateTime(-1999, 11, 30, 23, 59, 59), hnsecs(9_999_999)));
        assert(SysTime(Date(-1999, 12, 1)).endOfMonth ==
               SysTime(DateTime(-1999, 12, 31, 23, 59, 59), hnsecs(9_999_999)));

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(cst.endOfMonth == SysTime(DateTime(1999, 7, 31, 23, 59, 59), hnsecs(9_999_999)));
        assert(ist.endOfMonth == SysTime(DateTime(1999, 7, 31, 23, 59, 59), hnsecs(9_999_999)));

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.endOfMonth;
        }
    }


    /++
        The last day in the month that this $(LREF SysTime) is in.
      +/
    @property ubyte daysInMonth() @safe const nothrow scope
    {
        return Date(dayOfGregorianCal).daysInMonth;
    }

    ///
    @safe unittest
    {
        import core.time;
        import std.datetime.date : DateTime;

        assert(SysTime(DateTime(1999, 1, 6, 0, 0, 0)).daysInMonth == 31);
        assert(SysTime(DateTime(1999, 2, 7, 19, 30, 0)).daysInMonth == 28);
        assert(SysTime(DateTime(2000, 2, 7, 5, 12, 27)).daysInMonth == 29);
        assert(SysTime(DateTime(2000, 6, 4, 12, 22, 9)).daysInMonth == 30);
    }

    @safe unittest
    {
        import core.time;
        // Test A.D.
        assert(SysTime(DateTime(1999, 1, 1, 12, 1, 13)).daysInMonth == 31);
        assert(SysTime(DateTime(1999, 2, 1, 17, 13, 12)).daysInMonth == 28);
        assert(SysTime(DateTime(2000, 2, 1, 13, 2, 12)).daysInMonth == 29);
        assert(SysTime(DateTime(1999, 3, 1, 12, 13, 12)).daysInMonth == 31);
        assert(SysTime(DateTime(1999, 4, 1, 12, 6, 13)).daysInMonth == 30);
        assert(SysTime(DateTime(1999, 5, 1, 15, 13, 12)).daysInMonth == 31);
        assert(SysTime(DateTime(1999, 6, 1, 13, 7, 12)).daysInMonth == 30);
        assert(SysTime(DateTime(1999, 7, 1, 12, 13, 17)).daysInMonth == 31);
        assert(SysTime(DateTime(1999, 8, 1, 12, 3, 13)).daysInMonth == 31);
        assert(SysTime(DateTime(1999, 9, 1, 12, 13, 12)).daysInMonth == 30);
        assert(SysTime(DateTime(1999, 10, 1, 13, 19, 12)).daysInMonth == 31);
        assert(SysTime(DateTime(1999, 11, 1, 12, 13, 17)).daysInMonth == 30);
        assert(SysTime(DateTime(1999, 12, 1, 12, 52, 13)).daysInMonth == 31);

        // Test B.C.
        assert(SysTime(DateTime(-1999, 1, 1, 12, 1, 13)).daysInMonth == 31);
        assert(SysTime(DateTime(-1999, 2, 1, 7, 13, 12)).daysInMonth == 28);
        assert(SysTime(DateTime(-2000, 2, 1, 13, 2, 12)).daysInMonth == 29);
        assert(SysTime(DateTime(-1999, 3, 1, 12, 13, 12)).daysInMonth == 31);
        assert(SysTime(DateTime(-1999, 4, 1, 12, 6, 13)).daysInMonth == 30);
        assert(SysTime(DateTime(-1999, 5, 1, 5, 13, 12)).daysInMonth == 31);
        assert(SysTime(DateTime(-1999, 6, 1, 13, 7, 12)).daysInMonth == 30);
        assert(SysTime(DateTime(-1999, 7, 1, 12, 13, 17)).daysInMonth == 31);
        assert(SysTime(DateTime(-1999, 8, 1, 12, 3, 13)).daysInMonth == 31);
        assert(SysTime(DateTime(-1999, 9, 1, 12, 13, 12)).daysInMonth == 30);
        assert(SysTime(DateTime(-1999, 10, 1, 13, 19, 12)).daysInMonth == 31);
        assert(SysTime(DateTime(-1999, 11, 1, 12, 13, 17)).daysInMonth == 30);
        assert(SysTime(DateTime(-1999, 12, 1, 12, 52, 13)).daysInMonth == 31);

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(cst.daysInMonth == 31);
        assert(ist.daysInMonth == 31);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.daysInMonth;
        }
    }


    /++
        Whether the current year is a date in A.D.
      +/
    @property bool isAD() @safe const nothrow scope
    {
        return adjTime >= 0;
    }

    ///
    @safe unittest
    {
        import core.time;
        import std.datetime.date : DateTime;

        assert(SysTime(DateTime(1, 1, 1, 12, 7, 0)).isAD);
        assert(SysTime(DateTime(2010, 12, 31, 0, 0, 0)).isAD);
        assert(!SysTime(DateTime(0, 12, 31, 23, 59, 59)).isAD);
        assert(!SysTime(DateTime(-2010, 1, 1, 2, 2, 2)).isAD);
    }

    @safe unittest
    {
        import core.time;
        assert(SysTime(DateTime(2010, 7, 4, 12, 0, 9)).isAD);
        assert(SysTime(DateTime(1, 1, 1, 0, 0, 0)).isAD);
        assert(!SysTime(DateTime(0, 12, 31, 23, 59, 59)).isAD);
        assert(!SysTime(DateTime(0, 1, 1, 23, 59, 59)).isAD);
        assert(!SysTime(DateTime(-1, 1, 1, 23 ,59 ,59)).isAD);
        assert(!SysTime(DateTime(-2010, 7, 4, 12, 2, 2)).isAD);

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(cst.isAD);
        assert(ist.isAD);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.isAD;
        }
    }


    /++
        The $(HTTP en.wikipedia.org/wiki/Julian_day, Julian day)
        for this $(LREF SysTime) at the given time. For example,
        prior to noon, 1996-03-31 would be the Julian day number 2_450_173, so
        this function returns 2_450_173, while from noon onward, the Julian
        day number would be 2_450_174, so this function returns 2_450_174.
      +/
    @property long julianDay() @safe const nothrow scope
    {
        immutable jd = dayOfGregorianCal + 1_721_425;
        return hour < 12 ? jd - 1 : jd;
    }

    @safe unittest
    {
        import core.time;
        assert(SysTime(DateTime(-4713, 11, 24, 0, 0, 0)).julianDay == -1);
        assert(SysTime(DateTime(-4713, 11, 24, 12, 0, 0)).julianDay == 0);

        assert(SysTime(DateTime(0, 12, 31, 0, 0, 0)).julianDay == 1_721_424);
        assert(SysTime(DateTime(0, 12, 31, 12, 0, 0)).julianDay == 1_721_425);

        assert(SysTime(DateTime(1, 1, 1, 0, 0, 0)).julianDay == 1_721_425);
        assert(SysTime(DateTime(1, 1, 1, 12, 0, 0)).julianDay == 1_721_426);

        assert(SysTime(DateTime(1582, 10, 15, 0, 0, 0)).julianDay == 2_299_160);
        assert(SysTime(DateTime(1582, 10, 15, 12, 0, 0)).julianDay == 2_299_161);

        assert(SysTime(DateTime(1858, 11, 17, 0, 0, 0)).julianDay == 2_400_000);
        assert(SysTime(DateTime(1858, 11, 17, 12, 0, 0)).julianDay == 2_400_001);

        assert(SysTime(DateTime(1982, 1, 4, 0, 0, 0)).julianDay == 2_444_973);
        assert(SysTime(DateTime(1982, 1, 4, 12, 0, 0)).julianDay == 2_444_974);

        assert(SysTime(DateTime(1996, 3, 31, 0, 0, 0)).julianDay == 2_450_173);
        assert(SysTime(DateTime(1996, 3, 31, 12, 0, 0)).julianDay == 2_450_174);

        assert(SysTime(DateTime(2010, 8, 24, 0, 0, 0)).julianDay == 2_455_432);
        assert(SysTime(DateTime(2010, 8, 24, 12, 0, 0)).julianDay == 2_455_433);

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(cst.julianDay == 2_451_366);
        assert(ist.julianDay == 2_451_366);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.julianDay;
        }
    }


    /++
        The modified $(HTTP en.wikipedia.org/wiki/Julian_day, Julian day) for
        any time on this date (since, the modified Julian day changes at
        midnight).
      +/
    @property long modJulianDay() @safe const nothrow scope
    {
        return dayOfGregorianCal + 1_721_425 - 2_400_001;
    }

    @safe unittest
    {
        import core.time;
        assert(SysTime(DateTime(1858, 11, 17, 0, 0, 0)).modJulianDay == 0);
        assert(SysTime(DateTime(1858, 11, 17, 12, 0, 0)).modJulianDay == 0);

        assert(SysTime(DateTime(2010, 8, 24, 0, 0, 0)).modJulianDay == 55_432);
        assert(SysTime(DateTime(2010, 8, 24, 12, 0, 0)).modJulianDay == 55_432);

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(cst.modJulianDay == 51_365);
        assert(ist.modJulianDay == 51_365);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.modJulianDay;
        }
    }


    /++
        Returns a $(REF Date,std,datetime,date) equivalent to this $(LREF SysTime).
      +/
    Date opCast(T)() @safe const nothrow scope
    if (is(immutable T == immutable Date))
    {
        return Date(dayOfGregorianCal);
    }

    @safe unittest
    {
        import core.time;
        assert(cast(Date) SysTime(Date(1999, 7, 6)) == Date(1999, 7, 6));
        assert(cast(Date) SysTime(Date(2000, 12, 31)) == Date(2000, 12, 31));
        assert(cast(Date) SysTime(Date(2001, 1, 1)) == Date(2001, 1, 1));

        assert(cast(Date) SysTime(DateTime(1999, 7, 6, 12, 10, 9)) == Date(1999, 7, 6));
        assert(cast(Date) SysTime(DateTime(2000, 12, 31, 13, 11, 10)) == Date(2000, 12, 31));
        assert(cast(Date) SysTime(DateTime(2001, 1, 1, 14, 12, 11)) == Date(2001, 1, 1));

        assert(cast(Date) SysTime(Date(-1999, 7, 6)) == Date(-1999, 7, 6));
        assert(cast(Date) SysTime(Date(-2000, 12, 31)) == Date(-2000, 12, 31));
        assert(cast(Date) SysTime(Date(-2001, 1, 1)) == Date(-2001, 1, 1));

        assert(cast(Date) SysTime(DateTime(-1999, 7, 6, 12, 10, 9)) == Date(-1999, 7, 6));
        assert(cast(Date) SysTime(DateTime(-2000, 12, 31, 13, 11, 10)) == Date(-2000, 12, 31));
        assert(cast(Date) SysTime(DateTime(-2001, 1, 1, 14, 12, 11)) == Date(-2001, 1, 1));

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(cast(Date) cst != Date.init);
        assert(cast(Date) ist != Date.init);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = cast(Date) st;
        }
    }


    /++
        Returns a $(REF DateTime,std,datetime,date) equivalent to this
        $(LREF SysTime).
      +/
    DateTime opCast(T)() @safe const nothrow scope
    if (is(immutable T == immutable DateTime))
    {
        try
        {
            auto hnsecs = adjTime;
            auto days = splitUnitsFromHNSecs!"days"(hnsecs) + 1;

            if (hnsecs < 0)
            {
                hnsecs += convert!("hours", "hnsecs")(24);
                --days;
            }

            immutable hour = splitUnitsFromHNSecs!"hours"(hnsecs);
            immutable minute = splitUnitsFromHNSecs!"minutes"(hnsecs);
            immutable second = getUnitsFromHNSecs!"seconds"(hnsecs);

            return DateTime(Date(cast(int) days), TimeOfDay(cast(int) hour, cast(int) minute, cast(int) second));
        }
        catch (Exception e)
            assert(0, "Either DateTime's constructor or TimeOfDay's constructor threw.");
    }

    @safe unittest
    {
        import core.time;
        assert(cast(DateTime) SysTime(DateTime(1, 1, 6, 7, 12, 22)) == DateTime(1, 1, 6, 7, 12, 22));
        assert(cast(DateTime) SysTime(DateTime(1, 1, 6, 7, 12, 22), msecs(22)) == DateTime(1, 1, 6, 7, 12, 22));
        assert(cast(DateTime) SysTime(Date(1999, 7, 6)) == DateTime(1999, 7, 6, 0, 0, 0));
        assert(cast(DateTime) SysTime(Date(2000, 12, 31)) == DateTime(2000, 12, 31, 0, 0, 0));
        assert(cast(DateTime) SysTime(Date(2001, 1, 1)) == DateTime(2001, 1, 1, 0, 0, 0));

        assert(cast(DateTime) SysTime(DateTime(1999, 7, 6, 12, 10, 9)) == DateTime(1999, 7, 6, 12, 10, 9));
        assert(cast(DateTime) SysTime(DateTime(2000, 12, 31, 13, 11, 10)) == DateTime(2000, 12, 31, 13, 11, 10));
        assert(cast(DateTime) SysTime(DateTime(2001, 1, 1, 14, 12, 11)) == DateTime(2001, 1, 1, 14, 12, 11));

        assert(cast(DateTime) SysTime(DateTime(-1, 1, 6, 7, 12, 22)) == DateTime(-1, 1, 6, 7, 12, 22));
        assert(cast(DateTime) SysTime(DateTime(-1, 1, 6, 7, 12, 22), msecs(22)) == DateTime(-1, 1, 6, 7, 12, 22));
        assert(cast(DateTime) SysTime(Date(-1999, 7, 6)) == DateTime(-1999, 7, 6, 0, 0, 0));
        assert(cast(DateTime) SysTime(Date(-2000, 12, 31)) == DateTime(-2000, 12, 31, 0, 0, 0));
        assert(cast(DateTime) SysTime(Date(-2001, 1, 1)) == DateTime(-2001, 1, 1, 0, 0, 0));

        assert(cast(DateTime) SysTime(DateTime(-1999, 7, 6, 12, 10, 9)) == DateTime(-1999, 7, 6, 12, 10, 9));
        assert(cast(DateTime) SysTime(DateTime(-2000, 12, 31, 13, 11, 10)) == DateTime(-2000, 12, 31, 13, 11, 10));
        assert(cast(DateTime) SysTime(DateTime(-2001, 1, 1, 14, 12, 11)) == DateTime(-2001, 1, 1, 14, 12, 11));

        assert(cast(DateTime) SysTime(DateTime(2011, 1, 13, 8, 17, 2), msecs(296), LocalTime()) ==
               DateTime(2011, 1, 13, 8, 17, 2));

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(cast(DateTime) cst != DateTime.init);
        assert(cast(DateTime) ist != DateTime.init);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = cast(DateTime) st;
        }
    }


    /++
        Returns a $(REF TimeOfDay,std,datetime,date) equivalent to this
        $(LREF SysTime).
      +/
    TimeOfDay opCast(T)() @safe const nothrow scope
    if (is(immutable T == immutable TimeOfDay))
    {
        try
        {
            auto hnsecs = adjTime;
            hnsecs = removeUnitsFromHNSecs!"days"(hnsecs);

            if (hnsecs < 0)
                hnsecs += convert!("hours", "hnsecs")(24);

            immutable hour = splitUnitsFromHNSecs!"hours"(hnsecs);
            immutable minute = splitUnitsFromHNSecs!"minutes"(hnsecs);
            immutable second = getUnitsFromHNSecs!"seconds"(hnsecs);

            return TimeOfDay(cast(int) hour, cast(int) minute, cast(int) second);
        }
        catch (Exception e)
            assert(0, "TimeOfDay's constructor threw.");
    }

    @safe unittest
    {
        import core.time;
        assert(cast(TimeOfDay) SysTime(Date(1999, 7, 6)) == TimeOfDay(0, 0, 0));
        assert(cast(TimeOfDay) SysTime(Date(2000, 12, 31)) == TimeOfDay(0, 0, 0));
        assert(cast(TimeOfDay) SysTime(Date(2001, 1, 1)) == TimeOfDay(0, 0, 0));

        assert(cast(TimeOfDay) SysTime(DateTime(1999, 7, 6, 12, 10, 9)) == TimeOfDay(12, 10, 9));
        assert(cast(TimeOfDay) SysTime(DateTime(2000, 12, 31, 13, 11, 10)) == TimeOfDay(13, 11, 10));
        assert(cast(TimeOfDay) SysTime(DateTime(2001, 1, 1, 14, 12, 11)) == TimeOfDay(14, 12, 11));

        assert(cast(TimeOfDay) SysTime(Date(-1999, 7, 6)) == TimeOfDay(0, 0, 0));
        assert(cast(TimeOfDay) SysTime(Date(-2000, 12, 31)) == TimeOfDay(0, 0, 0));
        assert(cast(TimeOfDay) SysTime(Date(-2001, 1, 1)) == TimeOfDay(0, 0, 0));

        assert(cast(TimeOfDay) SysTime(DateTime(-1999, 7, 6, 12, 10, 9)) == TimeOfDay(12, 10, 9));
        assert(cast(TimeOfDay) SysTime(DateTime(-2000, 12, 31, 13, 11, 10)) == TimeOfDay(13, 11, 10));
        assert(cast(TimeOfDay) SysTime(DateTime(-2001, 1, 1, 14, 12, 11)) == TimeOfDay(14, 12, 11));

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(cast(TimeOfDay) cst != TimeOfDay.init);
        assert(cast(TimeOfDay) ist != TimeOfDay.init);

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = cast(TimeOfDay) st;
        }
    }


    // Temporary hack until bug https://issues.dlang.org/show_bug.cgi?id=4867 is fixed.
    // This allows assignment from const(SysTime) to SysTime.
    // It may be a good idea to keep it though, since casting from a type to itself
    // should be allowed, and it doesn't work without this opCast() since opCast()
    // has already been defined for other types.
    SysTime opCast(T)() @safe const pure nothrow scope
    if (is(immutable T == immutable SysTime))
    {
        return SysTime(_stdTime, _timezone);
    }

    @safe unittest
    {
        static void testScope(scope ref SysTime st) @safe
        {
            auto result = cast(SysTime) st;
        }
    }


    /++
        Converts this $(LREF SysTime) to a string with the format
        YYYYMMDDTHHMMSS.FFFFFFFTZ (where F is fractional seconds and TZ is time
        zone).

        Note that the number of digits in the fractional seconds varies with the
        number of fractional seconds. It's a maximum of 7 (which would be
        hnsecs), but only has as many as are necessary to hold the correct value
        (so no trailing zeroes), and if there are no fractional seconds, then
        there is no decimal point.

        If this $(LREF SysTime)'s time zone is
        $(REF LocalTime,std,datetime,timezone), then TZ is empty. If its time
        zone is `UTC`, then it is "Z". Otherwise, it is the offset from UTC
        (e.g. +0100 or -0700). Note that the offset from UTC is $(I not) enough
        to uniquely identify the time zone.

        Time zone offsets will be in the form +HHMM or -HHMM.

        $(RED Warning:
            Previously, toISOString did the same as $(LREF toISOExtString) and
            generated +HH:MM or -HH:MM for the time zone when it was not
            $(REF LocalTime,std,datetime,timezone) or
            $(REF UTC,std,datetime,timezone), which is not in conformance with
            ISO 8601 for the non-extended string format. This has now been
            fixed. However, for now, fromISOString will continue to accept the
            extended format for the time zone so that any code which has been
            writing out the result of toISOString to read in later will continue
            to work. The current behavior will be kept until July 2019 at which
            point, fromISOString will be fixed to be standards compliant.)

        Params:
            writer = A `char` accepting
            $(REF_ALTTEXT output range, isOutputRange, std, range, primitives)
        Returns:
            A `string` when not using an output range; `void` otherwise.
      +/
    string toISOString() @safe const nothrow scope
    {
        import std.array : appender;
        auto app = appender!string();
        app.reserve(30);
        try
            toISOString(app);
        catch (Exception e)
            assert(0, "toISOString() threw.");
        return app.data;
    }

    /// ditto
    void toISOString(W)(ref W writer) const scope
    if (isOutputRange!(W, char))
    {
        immutable adjustedTime = adjTime;
        long hnsecs = adjustedTime;

        auto days = splitUnitsFromHNSecs!"days"(hnsecs) + 1;

        if (hnsecs < 0)
        {
            hnsecs += convert!("hours", "hnsecs")(24);
            --days;
        }

        immutable hour = splitUnitsFromHNSecs!"hours"(hnsecs);
        immutable minute = splitUnitsFromHNSecs!"minutes"(hnsecs);
        immutable second = splitUnitsFromHNSecs!"seconds"(hnsecs);

        auto dateTime = DateTime(Date(cast(int) days), TimeOfDay(cast(int) hour,
                                      cast(int) minute, cast(int) second));

        if (_timezone is LocalTime())
        {
            dateTime.toISOString(writer);
            fracSecsToISOString(writer, cast(int) hnsecs);
            return;
        }

        if (_timezone is UTC())
        {
            dateTime.toISOString(writer);
            fracSecsToISOString(writer, cast(int) hnsecs);
            put(writer, 'Z');
            return;
        }

        immutable utcOffset = dur!"hnsecs"(adjustedTime - stdTime);

        dateTime.toISOString(writer);
        fracSecsToISOString(writer, cast(int) hnsecs);
        SimpleTimeZone.toISOExtString(writer, utcOffset);
    }

    ///
    @safe unittest
    {
        import core.time : msecs, hnsecs;
        import std.datetime.date : DateTime;

        assert(SysTime(DateTime(2010, 7, 4, 7, 6, 12)).toISOString() ==
               "20100704T070612");

        assert(SysTime(DateTime(1998, 12, 25, 2, 15, 0), msecs(24)).toISOString() ==
               "19981225T021500.024");

        assert(SysTime(DateTime(0, 1, 5, 23, 9, 59)).toISOString() ==
               "00000105T230959");

        assert(SysTime(DateTime(-4, 1, 5, 0, 0, 2), hnsecs(520_920)).toISOString() ==
               "-00040105T000002.052092");
    }

    @safe unittest
    {
        import core.time;
        // Test A.D.
        assert(SysTime(DateTime.init, UTC()).toISOString() == "00010101T000000Z");
        assert(SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(1), UTC()).toISOString() == "00010101T000000.0000001Z");

        assert(SysTime(DateTime(9, 12, 4, 0, 0, 0)).toISOString() == "00091204T000000");
        assert(SysTime(DateTime(99, 12, 4, 5, 6, 12)).toISOString() == "00991204T050612");
        assert(SysTime(DateTime(999, 12, 4, 13, 44, 59)).toISOString() == "09991204T134459");
        assert(SysTime(DateTime(9999, 7, 4, 23, 59, 59)).toISOString() == "99990704T235959");
        assert(SysTime(DateTime(10000, 10, 20, 1, 1, 1)).toISOString() == "+100001020T010101");

        assert(SysTime(DateTime(9, 12, 4, 0, 0, 0), msecs(42)).toISOString() == "00091204T000000.042");
        assert(SysTime(DateTime(99, 12, 4, 5, 6, 12), msecs(100)).toISOString() == "00991204T050612.1");
        assert(SysTime(DateTime(999, 12, 4, 13, 44, 59), usecs(45020)).toISOString() == "09991204T134459.04502");
        assert(SysTime(DateTime(9999, 7, 4, 23, 59, 59), hnsecs(12)).toISOString() == "99990704T235959.0000012");
        assert(SysTime(DateTime(10000, 10, 20, 1, 1, 1), hnsecs(507890)).toISOString() == "+100001020T010101.050789");

        assert(SysTime(DateTime(2012, 12, 21, 12, 12, 12),
                       new immutable SimpleTimeZone(dur!"minutes"(-360))).toISOString() ==
               "20121221T121212-06:00");

        assert(SysTime(DateTime(2012, 12, 21, 12, 12, 12),
                       new immutable SimpleTimeZone(dur!"minutes"(420))).toISOString() ==
               "20121221T121212+07:00");

        // Test B.C.
        assert(SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999), UTC()).toISOString() ==
               "00001231T235959.9999999Z");
        assert(SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(1), UTC()).toISOString() == "00001231T235959.0000001Z");
        assert(SysTime(DateTime(0, 12, 31, 23, 59, 59), UTC()).toISOString() == "00001231T235959Z");

        assert(SysTime(DateTime(0, 12, 4, 0, 12, 4)).toISOString() == "00001204T001204");
        assert(SysTime(DateTime(-9, 12, 4, 0, 0, 0)).toISOString() == "-00091204T000000");
        assert(SysTime(DateTime(-99, 12, 4, 5, 6, 12)).toISOString() == "-00991204T050612");
        assert(SysTime(DateTime(-999, 12, 4, 13, 44, 59)).toISOString() == "-09991204T134459");
        assert(SysTime(DateTime(-9999, 7, 4, 23, 59, 59)).toISOString() == "-99990704T235959");
        assert(SysTime(DateTime(-10000, 10, 20, 1, 1, 1)).toISOString() == "-100001020T010101");

        assert(SysTime(DateTime(0, 12, 4, 0, 0, 0), msecs(7)).toISOString() == "00001204T000000.007");
        assert(SysTime(DateTime(-9, 12, 4, 0, 0, 0), msecs(42)).toISOString() == "-00091204T000000.042");
        assert(SysTime(DateTime(-99, 12, 4, 5, 6, 12), msecs(100)).toISOString() == "-00991204T050612.1");
        assert(SysTime(DateTime(-999, 12, 4, 13, 44, 59), usecs(45020)).toISOString() == "-09991204T134459.04502");
        assert(SysTime(DateTime(-9999, 7, 4, 23, 59, 59), hnsecs(12)).toISOString() == "-99990704T235959.0000012");
        assert(SysTime(DateTime(-10000, 10, 20, 1, 1, 1), hnsecs(507890)).toISOString() == "-100001020T010101.050789");

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(cst.toISOString() == "19990706T123033");
        assert(ist.toISOString() == "19990706T123033");

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.toISOString();
        }
    }



    /++
        Converts this $(LREF SysTime) to a string with the format
        YYYY-MM-DDTHH:MM:SS.FFFFFFFTZ (where F is fractional seconds and TZ
        is the time zone).

        Default behaviour:
        Note that the number of digits in the fractional seconds varies with the
        number of fractional seconds. It's a maximum of 7 (which would be
        hnsecs), but only has as many as are necessary to hold the correct value
        (so no trailing zeroes), and if there are no fractional seconds, then
        there is no decimal point.

        The optional parameter "prec" allows to change the default behavior by
        specifying the precision of the fractional seconds. The accepted values
        are in the range [-1, 7], where -1 represents the default behavior.

        If this $(LREF SysTime)'s time zone is
        $(REF LocalTime,std,datetime,timezone), then TZ is empty. If its time
        zone is `UTC`, then it is "Z". Otherwise, it is the offset from UTC
        (e.g. +01:00 or -07:00). Note that the offset from UTC is $(I not)
        enough to uniquely identify the time zone.

        Time zone offsets will be in the form +HH:MM or -HH:MM.

        Params:
            writer = A `char` accepting
            $(REF_ALTTEXT output range, isOutputRange, std, range, primitives)
            prec = An `int` representing the desired precision. Acceptable values range from -1 to 7, where -1 represents the default behavior.
        Returns:
            A `string` when not using an output range; `void` otherwise.
      +/
    string toISOExtString(int prec = -1) @safe const nothrow scope
    {
        assert(prec >= -1 && prec <= 7, "Precision must be in the range [-1, 7]");

        import std.array : appender;
        auto app = appender!string();
        app.reserve(35);
        try
            toISOExtString(app, prec);
        catch (Exception e)
            assert(0, "toISOExtString() threw.");
        return app.data;
    }

    /// ditto
    void toISOExtString(W)(ref W writer, int prec = -1) const scope
    if (isOutputRange!(W, char))
    {
        assert(prec >= -1 && prec <= 7, "Precision must be in the range [-1, 7]");

        immutable adjustedTime = adjTime;
        long hnsecs = adjustedTime;

        auto days = splitUnitsFromHNSecs!"days"(hnsecs) + 1;

        if (hnsecs < 0)
        {
            hnsecs += convert!("hours", "hnsecs")(24);
            --days;
        }

        immutable hour = splitUnitsFromHNSecs!"hours"(hnsecs);
        immutable minute = splitUnitsFromHNSecs!"minutes"(hnsecs);
        immutable second = splitUnitsFromHNSecs!"seconds"(hnsecs);

        immutable dateTime = DateTime(Date(cast(int) days), TimeOfDay(cast(int) hour,
                                      cast(int) minute, cast(int) second));

        if (_timezone is LocalTime())
        {
            dateTime.toISOExtString(writer);
            fracSecsToISOString(writer, cast(int) hnsecs, prec);
            return;
        }

        if (_timezone is UTC())
        {
            dateTime.toISOExtString(writer);
            fracSecsToISOString(writer, cast(int) hnsecs, prec);
            put(writer, 'Z');
            return;
        }

        immutable utcOffset = dur!"hnsecs"(adjustedTime - stdTime);

        dateTime.toISOExtString(writer);
        fracSecsToISOString(writer, cast(int) hnsecs, prec);
        SimpleTimeZone.toISOExtString(writer, utcOffset);
    }

    ///
    @safe unittest
    {
        import core.time : msecs, hnsecs;
        import std.datetime.date : DateTime;

        assert(SysTime(DateTime(2010, 7, 4, 7, 6, 12)).toISOExtString() ==
               "2010-07-04T07:06:12");

        assert(SysTime(DateTime(1998, 12, 25, 2, 15, 0), msecs(24)).toISOExtString() ==
               "1998-12-25T02:15:00.024");

        assert(SysTime(DateTime(0, 1, 5, 23, 9, 59)).toISOExtString() ==
               "0000-01-05T23:09:59");

        assert(SysTime(DateTime(-4, 1, 5, 0, 0, 2), hnsecs(520_920)).toISOExtString() ==
               "-0004-01-05T00:00:02.052092");

        assert(SysTime(DateTime(-4, 1, 5, 0, 0, 2), hnsecs(520_920)).toISOExtString(4) ==
               "-0004-01-05T00:00:02.0520");

        assert(SysTime(DateTime(-4, 1, 5, 0, 0, 2), hnsecs(520_920)).toISOExtString(2) ==
               "-0004-01-05T00:00:02.05");

        assert(SysTime(DateTime(-4, 1, 5, 0, 0, 2), hnsecs(520_920)).toISOExtString(7) ==
               "-0004-01-05T00:00:02.0520920");
    }

    @safe unittest
    {
        import core.time;
        // Test A.D.
        assert(SysTime(DateTime.init, UTC()).toISOExtString() == "0001-01-01T00:00:00Z");
        assert(SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(1), UTC()).toISOExtString() ==
               "0001-01-01T00:00:00.0000001Z");

        assert(SysTime(DateTime(9, 12, 4, 0, 0, 0)).toISOExtString() == "0009-12-04T00:00:00");
        assert(SysTime(DateTime(99, 12, 4, 5, 6, 12)).toISOExtString() == "0099-12-04T05:06:12");
        assert(SysTime(DateTime(999, 12, 4, 13, 44, 59)).toISOExtString() == "0999-12-04T13:44:59");
        assert(SysTime(DateTime(9999, 7, 4, 23, 59, 59)).toISOExtString() == "9999-07-04T23:59:59");
        assert(SysTime(DateTime(10000, 10, 20, 1, 1, 1)).toISOExtString() == "+10000-10-20T01:01:01");

        assert(SysTime(DateTime(9, 12, 4, 0, 0, 0), msecs(42)).toISOExtString() == "0009-12-04T00:00:00.042");
        assert(SysTime(DateTime(99, 12, 4, 5, 6, 12), msecs(100)).toISOExtString() == "0099-12-04T05:06:12.1");
        assert(SysTime(DateTime(999, 12, 4, 13, 44, 59), usecs(45020)).toISOExtString() == "0999-12-04T13:44:59.04502");
        assert(SysTime(DateTime(9999, 7, 4, 23, 59, 59), hnsecs(12)).toISOExtString() == "9999-07-04T23:59:59.0000012");
        assert(SysTime(DateTime(10000, 10, 20, 1, 1, 1), hnsecs(507890)).toISOExtString() ==
               "+10000-10-20T01:01:01.050789");

        assert(SysTime(DateTime(2012, 12, 21, 12, 12, 12),
                       new immutable SimpleTimeZone(dur!"minutes"(-360))).toISOExtString() ==
               "2012-12-21T12:12:12-06:00");

        assert(SysTime(DateTime(2012, 12, 21, 12, 12, 12),
                       new immutable SimpleTimeZone(dur!"minutes"(420))).toISOExtString() ==
               "2012-12-21T12:12:12+07:00");

        // Test B.C.
        assert(SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999), UTC()).toISOExtString() ==
               "0000-12-31T23:59:59.9999999Z");
        assert(SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(1), UTC()).toISOExtString() ==
               "0000-12-31T23:59:59.0000001Z");
        assert(SysTime(DateTime(0, 12, 31, 23, 59, 59), UTC()).toISOExtString() == "0000-12-31T23:59:59Z");

        assert(SysTime(DateTime(0, 12, 4, 0, 12, 4)).toISOExtString() == "0000-12-04T00:12:04");
        assert(SysTime(DateTime(-9, 12, 4, 0, 0, 0)).toISOExtString() == "-0009-12-04T00:00:00");
        assert(SysTime(DateTime(-99, 12, 4, 5, 6, 12)).toISOExtString() == "-0099-12-04T05:06:12");
        assert(SysTime(DateTime(-999, 12, 4, 13, 44, 59)).toISOExtString() == "-0999-12-04T13:44:59");
        assert(SysTime(DateTime(-9999, 7, 4, 23, 59, 59)).toISOExtString() == "-9999-07-04T23:59:59");
        assert(SysTime(DateTime(-10000, 10, 20, 1, 1, 1)).toISOExtString() == "-10000-10-20T01:01:01");

        assert(SysTime(DateTime(0, 12, 4, 0, 0, 0), msecs(7)).toISOExtString() == "0000-12-04T00:00:00.007");
        assert(SysTime(DateTime(-9, 12, 4, 0, 0, 0), msecs(42)).toISOExtString() == "-0009-12-04T00:00:00.042");
        assert(SysTime(DateTime(-99, 12, 4, 5, 6, 12), msecs(100)).toISOExtString() == "-0099-12-04T05:06:12.1");
        assert(SysTime(DateTime(-999, 12, 4, 13, 44, 59), usecs(45020)).toISOExtString() ==
               "-0999-12-04T13:44:59.04502");
        assert(SysTime(DateTime(-9999, 7, 4, 23, 59, 59), hnsecs(12)).toISOExtString() ==
               "-9999-07-04T23:59:59.0000012");
        assert(SysTime(DateTime(-10000, 10, 20, 1, 1, 1), hnsecs(507890)).toISOExtString() ==
               "-10000-10-20T01:01:01.050789");

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(cst.toISOExtString() == "1999-07-06T12:30:33");
        assert(ist.toISOExtString() == "1999-07-06T12:30:33");

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.toISOExtString();
        }
    }

    /++
        Converts this $(LREF SysTime) to a string with the format
        YYYY-Mon-DD HH:MM:SS.FFFFFFFTZ (where F is fractional seconds and TZ
        is the time zone).

        Note that the number of digits in the fractional seconds varies with the
        number of fractional seconds. It's a maximum of 7 (which would be
        hnsecs), but only has as many as are necessary to hold the correct value
        (so no trailing zeroes), and if there are no fractional seconds, then
        there is no decimal point.

        If this $(LREF SysTime)'s time zone is
        $(REF LocalTime,std,datetime,timezone), then TZ is empty. If its time
        zone is `UTC`, then it is "Z". Otherwise, it is the offset from UTC
        (e.g. +01:00 or -07:00). Note that the offset from UTC is $(I not)
        enough to uniquely identify the time zone.

        Time zone offsets will be in the form +HH:MM or -HH:MM.

        Params:
            writer = A `char` accepting
            $(REF_ALTTEXT output range, isOutputRange, std, range, primitives)
        Returns:
            A `string` when not using an output range; `void` otherwise.
      +/
    string toSimpleString() @safe const nothrow scope
    {
        import std.array : appender;
        auto app = appender!string();
        app.reserve(35);
        try
            toSimpleString(app);
        catch (Exception e)
            assert(0, "toSimpleString() threw.");
        return app.data;
    }

    /// ditto
    void toSimpleString(W)(ref W writer) const scope
    if (isOutputRange!(W, char))
    {
        immutable adjustedTime = adjTime;
        long hnsecs = adjustedTime;

        auto days = splitUnitsFromHNSecs!"days"(hnsecs) + 1;

        if (hnsecs < 0)
        {
            hnsecs += convert!("hours", "hnsecs")(24);
            --days;
        }

        immutable hour = splitUnitsFromHNSecs!"hours"(hnsecs);
        immutable minute = splitUnitsFromHNSecs!"minutes"(hnsecs);
        immutable second = splitUnitsFromHNSecs!"seconds"(hnsecs);

        immutable dateTime = DateTime(Date(cast(int) days), TimeOfDay(cast(int) hour,
                                      cast(int) minute, cast(int) second));

        if (_timezone is LocalTime())
        {
            dateTime.toSimpleString(writer);
            fracSecsToISOString(writer, cast(int) hnsecs);
            return;
        }

        if (_timezone is UTC())
        {
            dateTime.toSimpleString(writer);
            fracSecsToISOString(writer, cast(int) hnsecs);
            put(writer, 'Z');
            return;
        }

        immutable utcOffset = dur!"hnsecs"(adjustedTime - stdTime);

        dateTime.toSimpleString(writer);
        fracSecsToISOString(writer, cast(int) hnsecs);
        SimpleTimeZone.toISOExtString(writer, utcOffset);
    }

    ///
    @safe unittest
    {
        import core.time : msecs, hnsecs;
        import std.datetime.date : DateTime;

        assert(SysTime(DateTime(2010, 7, 4, 7, 6, 12)).toSimpleString() ==
               "2010-Jul-04 07:06:12");

        assert(SysTime(DateTime(1998, 12, 25, 2, 15, 0), msecs(24)).toSimpleString() ==
               "1998-Dec-25 02:15:00.024");

        assert(SysTime(DateTime(0, 1, 5, 23, 9, 59)).toSimpleString() ==
               "0000-Jan-05 23:09:59");

        assert(SysTime(DateTime(-4, 1, 5, 0, 0, 2), hnsecs(520_920)).toSimpleString() ==
                "-0004-Jan-05 00:00:02.052092");
    }

    @safe unittest
    {
        import core.time;
        // Test A.D.
        assert(SysTime(DateTime.init, UTC()).toString() == "0001-Jan-01 00:00:00Z");
        assert(SysTime(DateTime(1, 1, 1, 0, 0, 0), hnsecs(1), UTC()).toString() == "0001-Jan-01 00:00:00.0000001Z");

        assert(SysTime(DateTime(9, 12, 4, 0, 0, 0)).toSimpleString() == "0009-Dec-04 00:00:00");
        assert(SysTime(DateTime(99, 12, 4, 5, 6, 12)).toSimpleString() == "0099-Dec-04 05:06:12");
        assert(SysTime(DateTime(999, 12, 4, 13, 44, 59)).toSimpleString() == "0999-Dec-04 13:44:59");
        assert(SysTime(DateTime(9999, 7, 4, 23, 59, 59)).toSimpleString() == "9999-Jul-04 23:59:59");
        assert(SysTime(DateTime(10000, 10, 20, 1, 1, 1)).toSimpleString() == "+10000-Oct-20 01:01:01");

        assert(SysTime(DateTime(9, 12, 4, 0, 0, 0), msecs(42)).toSimpleString() == "0009-Dec-04 00:00:00.042");
        assert(SysTime(DateTime(99, 12, 4, 5, 6, 12), msecs(100)).toSimpleString() == "0099-Dec-04 05:06:12.1");
        assert(SysTime(DateTime(999, 12, 4, 13, 44, 59), usecs(45020)).toSimpleString() ==
               "0999-Dec-04 13:44:59.04502");
        assert(SysTime(DateTime(9999, 7, 4, 23, 59, 59), hnsecs(12)).toSimpleString() ==
               "9999-Jul-04 23:59:59.0000012");
        assert(SysTime(DateTime(10000, 10, 20, 1, 1, 1), hnsecs(507890)).toSimpleString() ==
               "+10000-Oct-20 01:01:01.050789");

        assert(SysTime(DateTime(2012, 12, 21, 12, 12, 12),
                       new immutable SimpleTimeZone(dur!"minutes"(-360))).toSimpleString() ==
               "2012-Dec-21 12:12:12-06:00");

        assert(SysTime(DateTime(2012, 12, 21, 12, 12, 12),
                       new immutable SimpleTimeZone(dur!"minutes"(420))).toSimpleString() ==
               "2012-Dec-21 12:12:12+07:00");

        // Test B.C.
        assert(SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(9_999_999), UTC()).toSimpleString() ==
               "0000-Dec-31 23:59:59.9999999Z");
        assert(SysTime(DateTime(0, 12, 31, 23, 59, 59), hnsecs(1), UTC()).toSimpleString() ==
               "0000-Dec-31 23:59:59.0000001Z");
        assert(SysTime(DateTime(0, 12, 31, 23, 59, 59), UTC()).toSimpleString() == "0000-Dec-31 23:59:59Z");

        assert(SysTime(DateTime(0, 12, 4, 0, 12, 4)).toSimpleString() == "0000-Dec-04 00:12:04");
        assert(SysTime(DateTime(-9, 12, 4, 0, 0, 0)).toSimpleString() == "-0009-Dec-04 00:00:00");
        assert(SysTime(DateTime(-99, 12, 4, 5, 6, 12)).toSimpleString() == "-0099-Dec-04 05:06:12");
        assert(SysTime(DateTime(-999, 12, 4, 13, 44, 59)).toSimpleString() == "-0999-Dec-04 13:44:59");
        assert(SysTime(DateTime(-9999, 7, 4, 23, 59, 59)).toSimpleString() == "-9999-Jul-04 23:59:59");
        assert(SysTime(DateTime(-10000, 10, 20, 1, 1, 1)).toSimpleString() == "-10000-Oct-20 01:01:01");

        assert(SysTime(DateTime(0, 12, 4, 0, 0, 0), msecs(7)).toSimpleString() == "0000-Dec-04 00:00:00.007");
        assert(SysTime(DateTime(-9, 12, 4, 0, 0, 0), msecs(42)).toSimpleString() == "-0009-Dec-04 00:00:00.042");
        assert(SysTime(DateTime(-99, 12, 4, 5, 6, 12), msecs(100)).toSimpleString() == "-0099-Dec-04 05:06:12.1");
        assert(SysTime(DateTime(-999, 12, 4, 13, 44, 59), usecs(45020)).toSimpleString() ==
               "-0999-Dec-04 13:44:59.04502");
        assert(SysTime(DateTime(-9999, 7, 4, 23, 59, 59), hnsecs(12)).toSimpleString() ==
               "-9999-Jul-04 23:59:59.0000012");
        assert(SysTime(DateTime(-10000, 10, 20, 1, 1, 1), hnsecs(507890)).toSimpleString() ==
               "-10000-Oct-20 01:01:01.050789");

        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        assert(cst.toSimpleString() == "1999-Jul-06 12:30:33");
        assert(ist.toSimpleString() == "1999-Jul-06 12:30:33");

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.toSimpleString();
        }
    }


    /++
        Converts this $(LREF SysTime) to a string.

        This function exists to make it easy to convert a $(LREF SysTime) to a
        string for code that does not care what the exact format is - just that
        it presents the information in a clear manner. It also makes it easy to
        simply convert a $(LREF SysTime) to a string when using functions such
        as `to!string`, `format`, or `writeln` which use toString to convert
        user-defined types. So, it is unlikely that much code will call
        toString directly.

        The format of the string is purposefully unspecified, and code that
        cares about the format of the string should use `toISOString`,
        `toISOExtString`, `toSimpleString`, or some other custom formatting
        function that explicitly generates the format that the code needs. The
        reason is that the code is then clear about what format it's using,
        making it less error-prone to maintain the code and interact with other
        software that consumes the generated strings. It's for this same reason
        that $(LREF SysTime) has no `fromString` function, whereas it does have
        `fromISOString`, `fromISOExtString`, and `fromSimpleString`.

        The format returned by toString may or may not change in the future.

        Params:
            writer = A `char` accepting
            $(REF_ALTTEXT output range, isOutputRange, std, range, primitives)
        Returns:
            A `string` when not using an output range; `void` otherwise.
      +/
    string toString() @safe const nothrow scope
    {
        return toSimpleString();
    }

    /// ditto
    void toString(W)(ref W writer) const scope
    if (isOutputRange!(W, char))
    {
        toSimpleString(writer);
    }

    @safe unittest
    {
        import core.time;
        auto st = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        const cst = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        immutable ist = SysTime(DateTime(1999, 7, 6, 12, 30, 33));
        static assert(__traits(compiles, st.toString()));
        static assert(__traits(compiles, cst.toString()));
        static assert(__traits(compiles, ist.toString()));

        static void testScope(scope ref SysTime st) @safe
        {
            auto result = st.toString();
        }
    }


    /++
        Creates a $(LREF SysTime) from a string with the format
        YYYYMMDDTHHMMSS.FFFFFFFTZ (where F is fractional seconds and TZ
        is the time zone). Whitespace is stripped from the given string.

        The exact format is exactly as described in $(LREF toISOString) except
        that trailing zeroes are permitted - including having fractional seconds
        with all zeroes. The time zone and fractional seconds are optional,
        however, a decimal point with nothing following it is invalid.
        Also, while $(LREF toISOString) will never generate a string
        with more than 7 digits in the fractional seconds (because that's the
        limit with hecto-nanosecond precision), it will allow more than 7 digits
        in order to read strings from other sources that have higher precision
        (however, any digits beyond 7 will be truncated).

        If there is no time zone in the string, then
        $(REF LocalTime,std,datetime,timezone) is used. If the time zone is "Z",
        then `UTC` is used. Otherwise, a
        $(REF SimpleTimeZone,std,datetime,timezone) which corresponds to the
        given offset from UTC is used. To get the returned $(LREF SysTime) to be
        a particular time zone, pass in that time zone and the $(LREF SysTime)
        to be returned will be converted to that time zone (though it will still
        be read in as whatever time zone is in its string).

        The accepted formats for time zone offsets are +HH, -HH, +HHMM, and
        -HHMM.

        $(RED Warning:
            Previously, $(LREF toISOString) did the same as
            $(LREF toISOExtString) and generated +HH:MM or -HH:MM for the time
            zone when it was not $(REF LocalTime,std,datetime,timezone) or
            $(REF UTC,std,datetime,timezone), which is not in conformance with
            ISO 8601 for the non-extended string format. This has now been
            fixed. However, for now, fromISOString will continue to accept the
            extended format for the time zone so that any code which has been
            writing out the result of toISOString to read in later will continue
            to work. The current behavior will be kept until July 2019 at which
            point, fromISOString will be fixed to be standards compliant.)

        Params:
            isoString = A string formatted in the ISO format for dates and times.
            tz        = The time zone to convert the given time to (no
                        conversion occurs if null).

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given string is
            not in the ISO format or if the resulting $(LREF SysTime) would not
            be valid.
      +/
    static SysTime fromISOString(S)(scope const S isoString, immutable TimeZone tz = null) @safe
    if (isSomeString!S)
    {
        import std.algorithm.searching : startsWith, find;
        import std.conv : to;
        import std.string : strip;
        import std.utf : byCodeUnit;

        auto str = strip(isoString);
        immutable skipFirst = str.startsWith('+', '-');

        auto found = (skipFirst ? str[1..$] : str).byCodeUnit.find('.', 'Z', '+', '-');
        auto dateTimeStr = str[0 .. $ - found[0].length];

        typeof(str.byCodeUnit) foundTZ; // needs to have longer lifetime than zoneStr
        typeof(str) fracSecStr;
        typeof(str) zoneStr;

        if (found[1] != 0)
        {
            if (found[1] == 1)
            {
                foundTZ = found[0].find('Z', '+', '-')[0];

                if (foundTZ.length != 0)
                {
                    static if (isNarrowString!S)
                    {
                        fracSecStr = found[0][0 .. $ - foundTZ.length].source;
                        zoneStr = foundTZ.source;
                    }
                    else
                    {
                        fracSecStr = found[0][0 .. $ - foundTZ.length];
                        zoneStr = foundTZ;
                    }
                }
                else
                {
                    static if (isNarrowString!S)
                        fracSecStr = found[0].source;
                    else
                        fracSecStr = found[0];
                }
            }
            else
            {
                static if (isNarrowString!S)
                    zoneStr = found[0].source;
                else
                    zoneStr = found[0];
            }
        }

        try
        {
            auto dateTime = DateTime.fromISOString(dateTimeStr);
            auto fracSec = fracSecsFromISOString(fracSecStr);

            Rebindable!(immutable TimeZone) parsedZone;

            if (zoneStr.empty)
                parsedZone = LocalTime();
            else if (zoneStr == "Z")
                parsedZone = UTC();
            else
            {
                try
                    parsedZone = SimpleTimeZone.fromISOString(zoneStr);
                catch (DateTimeException dte)
                    parsedZone = SimpleTimeZone.fromISOExtString(zoneStr);
            }

            auto retval = SysTime(dateTime, fracSec, parsedZone);

            if (tz !is null)
                retval.timezone = tz;

            return retval;
        }
        catch (DateTimeException dte)
            throw new DateTimeException(format("Invalid format for SysTime.fromISOString: %s", isoString));
    }

    ///
    @safe unittest
    {
        import core.time : hours, msecs, usecs, hnsecs;
        import std.datetime.date : DateTime;
        import std.datetime.timezone : SimpleTimeZone, UTC;

        assert(SysTime.fromISOString("20100704T070612") ==
               SysTime(DateTime(2010, 7, 4, 7, 6, 12)));

        assert(SysTime.fromISOString("19981225T021500.007") ==
               SysTime(DateTime(1998, 12, 25, 2, 15, 0), msecs(7)));

        assert(SysTime.fromISOString("00000105T230959.00002") ==
               SysTime(DateTime(0, 1, 5, 23, 9, 59), usecs(20)));

        assert(SysTime.fromISOString("20130207T043937.000050392") ==
               SysTime(DateTime(2013, 2, 7, 4, 39, 37), hnsecs(503)));

        assert(SysTime.fromISOString("-00040105T000002") ==
               SysTime(DateTime(-4, 1, 5, 0, 0, 2)));

        assert(SysTime.fromISOString(" 20100704T070612 ") ==
               SysTime(DateTime(2010, 7, 4, 7, 6, 12)));

        assert(SysTime.fromISOString("20100704T070612Z") ==
               SysTime(DateTime(2010, 7, 4, 7, 6, 12), UTC()));

        assert(SysTime.fromISOString("20100704T070612-0800") ==
               SysTime(DateTime(2010, 7, 4, 7, 6, 12),
                       new immutable SimpleTimeZone(hours(-8))));

        assert(SysTime.fromISOString("20100704T070612+0800") ==
               SysTime(DateTime(2010, 7, 4, 7, 6, 12),
                       new immutable SimpleTimeZone(hours(8))));
    }

    @safe unittest
    {
        import core.time;
        foreach (str; ["", "20100704000000", "20100704 000000", "20100704t000000",
                       "20100704T000000.", "20100704T000000.A", "20100704T000000.Z",
                       "20100704T000000.0000000A", "20100704T000000.00000000A",
                       "20100704T000000+", "20100704T000000-", "20100704T000000:",
                       "20100704T000000-:", "20100704T000000+:", "20100704T000000-1:",
                       "20100704T000000+1:", "20100704T000000+1:0",
                       "20100704T000000-12.00", "20100704T000000+12.00",
                       "20100704T000000-8", "20100704T000000+8",
                       "20100704T000000-800", "20100704T000000+800",
                       "20100704T000000-080", "20100704T000000+080",
                       "20100704T000000-2400", "20100704T000000+2400",
                       "20100704T000000-1260", "20100704T000000+1260",
                       "20100704T000000.0-8", "20100704T000000.0+8",
                       "20100704T000000.0-800", "20100704T000000.0+800",
                       "20100704T000000.0-080", "20100704T000000.0+080",
                       "20100704T000000.0-2400", "20100704T000000.0+2400",
                       "20100704T000000.0-1260", "20100704T000000.0+1260",
                       "20100704T000000-8:00", "20100704T000000+8:00",
                       "20100704T000000-08:0", "20100704T000000+08:0",
                       "20100704T000000-24:00", "20100704T000000+24:00",
                       "20100704T000000-12:60", "20100704T000000+12:60",
                       "20100704T000000.0-8:00", "20100704T000000.0+8:00",
                       "20100704T000000.0-08:0", "20100704T000000.0+08:0",
                       "20100704T000000.0-24:00", "20100704T000000.0+24:00",
                       "20100704T000000.0-12:60", "20100704T000000.0+12:60",
                       "2010-07-0400:00:00", "2010-07-04 00:00:00",
                       "2010-07-04t00:00:00", "2010-07-04T00:00:00.",
                       "2010-Jul-0400:00:00", "2010-Jul-04 00:00:00", "2010-Jul-04t00:00:00",
                       "2010-Jul-04T00:00:00", "2010-Jul-04 00:00:00.",
                       "2010-12-22T172201", "2010-Dec-22 17:22:01"])
        {
            assertThrown!DateTimeException(SysTime.fromISOString(str), format("[%s]", str));
        }

        static void test(string str, SysTime st, size_t line = __LINE__)
        {
            if (SysTime.fromISOString(str) != st)
                throw new AssertError("unittest failure", __FILE__, line);
        }

        test("20101222T172201", SysTime(DateTime(2010, 12, 22, 17, 22, 1)));
        test("19990706T123033", SysTime(DateTime(1999, 7, 6, 12, 30, 33)));
        test("-19990706T123033", SysTime(DateTime(-1999, 7, 6, 12, 30, 33)));
        test("+019990706T123033", SysTime(DateTime(1999, 7, 6, 12, 30, 33)));
        test("19990706T123033 ", SysTime(DateTime(1999, 7, 6, 12, 30, 33)));
        test(" 19990706T123033", SysTime(DateTime(1999, 7, 6, 12, 30, 33)));
        test(" 19990706T123033 ", SysTime(DateTime(1999, 7, 6, 12, 30, 33)));

        test("19070707T121212.0", SysTime(DateTime(1907, 7, 7, 12, 12, 12)));
        test("19070707T121212.0000000", SysTime(DateTime(1907, 7, 7, 12, 12, 12)));
        test("19070707T121212.0000001", SysTime(DateTime(1907, 7, 7, 12, 12, 12), hnsecs(1)));
        test("20100704T000000.00000000", SysTime(Date(2010, 7, 4)));
        test("20100704T000000.00000009", SysTime(Date(2010, 7, 4)));
        test("20100704T000000.00000019", SysTime(DateTime(2010, 7, 4), hnsecs(1)));
        test("19070707T121212.000001", SysTime(DateTime(1907, 7, 7, 12, 12, 12), usecs(1)));
        test("19070707T121212.0000010", SysTime(DateTime(1907, 7, 7, 12, 12, 12), usecs(1)));
        test("19070707T121212.001", SysTime(DateTime(1907, 7, 7, 12, 12, 12), msecs(1)));
        test("19070707T121212.0010000", SysTime(DateTime(1907, 7, 7, 12, 12, 12), msecs(1)));

        auto west60 = new immutable SimpleTimeZone(hours(-1));
        auto west90 = new immutable SimpleTimeZone(minutes(-90));
        auto west480 = new immutable SimpleTimeZone(hours(-8));
        auto east60 = new immutable SimpleTimeZone(hours(1));
        auto east90 = new immutable SimpleTimeZone(minutes(90));
        auto east480 = new immutable SimpleTimeZone(hours(8));

        test("20101222T172201Z", SysTime(DateTime(2010, 12, 22, 17, 22, 1), UTC()));
        test("20101222T172201-0100", SysTime(DateTime(2010, 12, 22, 17, 22, 1), west60));
        test("20101222T172201-01", SysTime(DateTime(2010, 12, 22, 17, 22, 1), west60));
        test("20101222T172201-0130", SysTime(DateTime(2010, 12, 22, 17, 22, 1), west90));
        test("20101222T172201-0800", SysTime(DateTime(2010, 12, 22, 17, 22, 1), west480));
        test("20101222T172201+0100", SysTime(DateTime(2010, 12, 22, 17, 22, 1), east60));
        test("20101222T172201+01", SysTime(DateTime(2010, 12, 22, 17, 22, 1), east60));
        test("20101222T172201+0130", SysTime(DateTime(2010, 12, 22, 17, 22, 1), east90));
        test("20101222T172201+0800", SysTime(DateTime(2010, 12, 22, 17, 22, 1), east480));

        test("20101103T065106.57159Z", SysTime(DateTime(2010, 11, 3, 6, 51, 6), hnsecs(5715900), UTC()));
        test("20101222T172201.23412Z", SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(2_341_200), UTC()));
        test("20101222T172201.23112-0100", SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(2_311_200), west60));
        test("20101222T172201.45-01", SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(4_500_000), west60));
        test("20101222T172201.1-0130", SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(1_000_000), west90));
        test("20101222T172201.55-0800", SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(5_500_000), west480));
        test("20101222T172201.1234567+0100", SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(1_234_567), east60));
        test("20101222T172201.0+01", SysTime(DateTime(2010, 12, 22, 17, 22, 1), east60));
        test("20101222T172201.0000000+0130", SysTime(DateTime(2010, 12, 22, 17, 22, 1), east90));
        test("20101222T172201.45+0800", SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(4_500_000), east480));

        // for dstring coverage
        assert(SysTime.fromISOString("20101222T172201.23112-0100"d) == SysTime(
            DateTime(2010, 12, 22, 17, 22, 1), hnsecs(2_311_200), west60));
        assert(SysTime.fromISOString("19070707T121212.0010000"d) == SysTime(
            DateTime(1907, 7, 7, 12, 12, 12), msecs(1)));

        // @@@DEPRECATED_2019-07@@@
        // This isn't deprecated per se, but that text will make it so that it
        // pops up when deprecations are moved along around July 2019. At that
        // time, we will update fromISOString so that it is conformant with ISO
        // 8601, and it will no longer accept ISO extended time zones (it does
        // currently because of https://issues.dlang.org/show_bug.cgi?id=15654
        // toISOString used to incorrectly use the ISO extended time zone format).
        // These tests will then start failing will need to be updated accordingly.
        // Also, the notes about this issue in toISOString and fromISOString's
        // documentation will need to be removed.
        test("20101222T172201-01:00", SysTime(DateTime(2010, 12, 22, 17, 22, 1), west60));
        test("20101222T172201-01:30", SysTime(DateTime(2010, 12, 22, 17, 22, 1), west90));
        test("20101222T172201-08:00", SysTime(DateTime(2010, 12, 22, 17, 22, 1), west480));
        test("20101222T172201+01:00", SysTime(DateTime(2010, 12, 22, 17, 22, 1), east60));
        test("20101222T172201+01:30", SysTime(DateTime(2010, 12, 22, 17, 22, 1), east90));
        test("20101222T172201+08:00", SysTime(DateTime(2010, 12, 22, 17, 22, 1), east480));

        test("20101222T172201.23112-01:00", SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(2_311_200), west60));
        test("20101222T172201.1-01:30", SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(1_000_000), west90));
        test("20101222T172201.55-08:00", SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(5_500_000), west480));
        test("20101222T172201.1234567+01:00", SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(1_234_567), east60));
        test("20101222T172201.0000000+01:30", SysTime(DateTime(2010, 12, 22, 17, 22, 1), east90));
        test("20101222T172201.45+08:00", SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(4_500_000), east480));

        static void testScope(scope ref string str) @safe
        {
            auto result = SysTime.fromISOString(str);
        }
    }

    // https://issues.dlang.org/show_bug.cgi?id=17801
    @safe unittest
    {
        import std.conv : to;
        import std.meta : AliasSeq;
        static foreach (C; AliasSeq!(char, wchar, dchar))
        {
            static foreach (S; AliasSeq!(C[], const(C)[], immutable(C)[]))
            {
                assert(SysTime.fromISOString(to!S("20121221T141516Z")) ==
                       SysTime(DateTime(2012, 12, 21, 14, 15, 16), UTC()));
            }
        }
    }


    /++
        Creates a $(LREF SysTime) from a string with the format
        YYYY-MM-DDTHH:MM:SS.FFFFFFFTZ (where F is fractional seconds and TZ
        is the time zone). Whitespace is stripped from the given string.

        The exact format is exactly as described in $(LREF toISOExtString)
        except that trailing zeroes are permitted - including having fractional
        seconds with all zeroes. The time zone and fractional seconds are
        optional, however, a decimal point with nothing following it is invalid.
        Also, while $(LREF toISOExtString) will never generate a
        string with more than 7 digits in the fractional seconds (because that's
        the limit with hecto-nanosecond precision), it will allow more than 7
        digits in order to read strings from other sources that have higher
        precision (however, any digits beyond 7 will be truncated).

        If there is no time zone in the string, then
        $(REF LocalTime,std,datetime,timezone) is used. If the time zone is "Z",
        then `UTC` is used. Otherwise, a
        $(REF SimpleTimeZone,std,datetime,timezone) which corresponds to the
        given offset from UTC is used. To get the returned $(LREF SysTime) to be
        a particular time zone, pass in that time zone and the $(LREF SysTime)
        to be returned will be converted to that time zone (though it will still
        be read in as whatever time zone is in its string).

        The accepted formats for time zone offsets are +HH, -HH, +HH:MM, and
        -HH:MM.

        Params:
            isoExtString = A string formatted in the ISO Extended format for
                           dates and times.
            tz           = The time zone to convert the given time to (no
                           conversion occurs if null).

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given string is
            not in the ISO format or if the resulting $(LREF SysTime) would not
            be valid.
      +/
    static SysTime fromISOExtString(S)(scope const S isoExtString, immutable TimeZone tz = null) @safe
    if (isSomeString!(S))
    {
        import std.algorithm.searching : countUntil, find;
        import std.conv : to;
        import std.string : strip, indexOf;

        auto str = strip(isoExtString);

        auto tIndex = str.indexOf('T');
        enforce!DateTimeException(tIndex != -1,
                                  format("Invalid format for SysTime.fromISOExtString: %s", isoExtString));

        auto found = str[tIndex + 1 .. $].find('.', 'Z', '+', '-');
        auto dateTimeStr = str[0 .. $ - found[0].length];

        typeof(str) foundTZ;  // needs to have longer lifetime than zoneStr
        typeof(str) fracSecStr;
        typeof(str) zoneStr;

        if (found[1] != 0)
        {
            if (found[1] == 1)
            {
                foundTZ = found[0].find('Z', '+', '-')[0];

                if (foundTZ.length != 0)
                {
                    fracSecStr = found[0][0 .. $ - foundTZ.length];
                    zoneStr = foundTZ;
                }
                else
                    fracSecStr = found[0];
            }
            else
                zoneStr = found[0];
        }

        try
        {
            auto dateTime = DateTime.fromISOExtString(dateTimeStr);
            auto fracSec = fracSecsFromISOString(fracSecStr);
            Rebindable!(immutable TimeZone) parsedZone;

            if (zoneStr.empty)
                parsedZone = LocalTime();
            else if (zoneStr == "Z")
                parsedZone = UTC();
            else
                parsedZone = SimpleTimeZone.fromISOExtString(zoneStr);

            auto retval = SysTime(dateTime, fracSec, parsedZone);

            if (tz !is null)
                retval.timezone = tz;

            return retval;
        }
        catch (DateTimeException dte)
            throw new DateTimeException(format("Invalid format for SysTime.fromISOExtString: %s", isoExtString));
    }

    ///
    @safe unittest
    {
        import core.time : hours, msecs, usecs, hnsecs;
        import std.datetime.date : DateTime;
        import std.datetime.timezone : SimpleTimeZone, UTC;

        assert(SysTime.fromISOExtString("2010-07-04T07:06:12") ==
               SysTime(DateTime(2010, 7, 4, 7, 6, 12)));

        assert(SysTime.fromISOExtString("1998-12-25T02:15:00.007") ==
               SysTime(DateTime(1998, 12, 25, 2, 15, 0), msecs(7)));

        assert(SysTime.fromISOExtString("0000-01-05T23:09:59.00002") ==
               SysTime(DateTime(0, 1, 5, 23, 9, 59), usecs(20)));

        assert(SysTime.fromISOExtString("2013-02-07T04:39:37.000050392") ==
               SysTime(DateTime(2013, 2, 7, 4, 39, 37), hnsecs(503)));

        assert(SysTime.fromISOExtString("-0004-01-05T00:00:02") ==
               SysTime(DateTime(-4, 1, 5, 0, 0, 2)));

        assert(SysTime.fromISOExtString(" 2010-07-04T07:06:12 ") ==
               SysTime(DateTime(2010, 7, 4, 7, 6, 12)));

        assert(SysTime.fromISOExtString("2010-07-04T07:06:12Z") ==
               SysTime(DateTime(2010, 7, 4, 7, 6, 12), UTC()));

        assert(SysTime.fromISOExtString("2010-07-04T07:06:12-08:00") ==
               SysTime(DateTime(2010, 7, 4, 7, 6, 12),
                       new immutable SimpleTimeZone(hours(-8))));
        assert(SysTime.fromISOExtString("2010-07-04T07:06:12+08:00") ==
               SysTime(DateTime(2010, 7, 4, 7, 6, 12),
                       new immutable SimpleTimeZone(hours(8))));
    }

    @safe unittest
    {
        import core.time;
        foreach (str; ["", "20100704000000", "20100704 000000",
                       "20100704t000000", "20100704T000000.", "20100704T000000.0",
                       "2010-07:0400:00:00", "2010-07-04 00:00:00",
                       "2010-07-04 00:00:00", "2010-07-04t00:00:00",
                       "2010-07-04T00:00:00.", "2010-07-04T00:00:00.A", "2010-07-04T00:00:00.Z",
                       "2010-07-04T00:00:00.0000000A", "2010-07-04T00:00:00.00000000A",
                       "2010-07-04T00:00:00+", "2010-07-04T00:00:00-",
                       "2010-07-04T00:00:00:", "2010-07-04T00:00:00-:", "2010-07-04T00:00:00+:",
                       "2010-07-04T00:00:00-1:", "2010-07-04T00:00:00+1:", "2010-07-04T00:00:00+1:0",
                       "2010-07-04T00:00:00-12.00", "2010-07-04T00:00:00+12.00",
                       "2010-07-04T00:00:00-8", "2010-07-04T00:00:00+8",
                       "20100704T000000-800", "20100704T000000+800",
                       "20100704T000000-080", "20100704T000000+080",
                       "20100704T000000-2400", "20100704T000000+2400",
                       "20100704T000000-1260", "20100704T000000+1260",
                       "20100704T000000.0-800", "20100704T000000.0+800",
                       "20100704T000000.0-8", "20100704T000000.0+8",
                       "20100704T000000.0-080", "20100704T000000.0+080",
                       "20100704T000000.0-2400", "20100704T000000.0+2400",
                       "20100704T000000.0-1260", "20100704T000000.0+1260",
                       "2010-07-04T00:00:00-8:00", "2010-07-04T00:00:00+8:00",
                       "2010-07-04T00:00:00-24:00", "2010-07-04T00:00:00+24:00",
                       "2010-07-04T00:00:00-12:60", "2010-07-04T00:00:00+12:60",
                       "2010-07-04T00:00:00.0-8:00", "2010-07-04T00:00:00.0+8:00",
                       "2010-07-04T00:00:00.0-8", "2010-07-04T00:00:00.0+8",
                       "2010-07-04T00:00:00.0-24:00", "2010-07-04T00:00:00.0+24:00",
                       "2010-07-04T00:00:00.0-12:60", "2010-07-04T00:00:00.0+12:60",
                       "2010-Jul-0400:00:00", "2010-Jul-04t00:00:00",
                       "2010-Jul-04 00:00:00.", "2010-Jul-04 00:00:00.0",
                       "20101222T172201", "2010-Dec-22 17:22:01"])
        {
            assertThrown!DateTimeException(SysTime.fromISOExtString(str), format("[%s]", str));
        }

        static void test(string str, SysTime st, size_t line = __LINE__)
        {
            if (SysTime.fromISOExtString(str) != st)
                throw new AssertError("unittest failure", __FILE__, line);
        }

        test("2010-12-22T17:22:01", SysTime(DateTime(2010, 12, 22, 17, 22, 1)));
        test("1999-07-06T12:30:33", SysTime(DateTime(1999, 7, 6, 12, 30, 33)));
        test("-1999-07-06T12:30:33", SysTime(DateTime(-1999, 7, 6, 12, 30, 33)));
        test("+01999-07-06T12:30:33", SysTime(DateTime(1999, 7, 6, 12, 30, 33)));
        test("1999-07-06T12:30:33 ", SysTime(DateTime(1999, 7, 6, 12, 30, 33)));
        test(" 1999-07-06T12:30:33", SysTime(DateTime(1999, 7, 6, 12, 30, 33)));
        test(" 1999-07-06T12:30:33 ", SysTime(DateTime(1999, 7, 6, 12, 30, 33)));

        test("1907-07-07T12:12:12.0", SysTime(DateTime(1907, 7, 7, 12, 12, 12)));
        test("1907-07-07T12:12:12.0000000", SysTime(DateTime(1907, 7, 7, 12, 12, 12)));
        test("1907-07-07T12:12:12.0000001", SysTime(DateTime(1907, 7, 7, 12, 12, 12), hnsecs(1)));
        test("2010-07-04T00:00:00.00000000", SysTime(Date(2010, 7, 4)));
        test("2010-07-04T00:00:00.00000009", SysTime(Date(2010, 7, 4)));
        test("2010-07-04T00:00:00.00000019", SysTime(DateTime(2010, 7, 4), hnsecs(1)));
        test("1907-07-07T12:12:12.000001", SysTime(DateTime(1907, 7, 7, 12, 12, 12), usecs(1)));
        test("1907-07-07T12:12:12.0000010", SysTime(DateTime(1907, 7, 7, 12, 12, 12), usecs(1)));
        test("1907-07-07T12:12:12.001", SysTime(DateTime(1907, 7, 7, 12, 12, 12), msecs(1)));
        test("1907-07-07T12:12:12.0010000", SysTime(DateTime(1907, 7, 7, 12, 12, 12), msecs(1)));

        auto west60 = new immutable SimpleTimeZone(hours(-1));
        auto west90 = new immutable SimpleTimeZone(minutes(-90));
        auto west480 = new immutable SimpleTimeZone(hours(-8));
        auto east60 = new immutable SimpleTimeZone(hours(1));
        auto east90 = new immutable SimpleTimeZone(minutes(90));
        auto east480 = new immutable SimpleTimeZone(hours(8));

        test("2010-12-22T17:22:01Z", SysTime(DateTime(2010, 12, 22, 17, 22, 1), UTC()));
        test("2010-12-22T17:22:01-01:00", SysTime(DateTime(2010, 12, 22, 17, 22, 1), west60));
        test("2010-12-22T17:22:01-01", SysTime(DateTime(2010, 12, 22, 17, 22, 1), west60));
        test("2010-12-22T17:22:01-01:30", SysTime(DateTime(2010, 12, 22, 17, 22, 1), west90));
        test("2010-12-22T17:22:01-08:00", SysTime(DateTime(2010, 12, 22, 17, 22, 1), west480));
        test("2010-12-22T17:22:01+01:00", SysTime(DateTime(2010, 12, 22, 17, 22, 1), east60));
        test("2010-12-22T17:22:01+01", SysTime(DateTime(2010, 12, 22, 17, 22, 1), east60));
        test("2010-12-22T17:22:01+01:30", SysTime(DateTime(2010, 12, 22, 17, 22, 1), east90));
        test("2010-12-22T17:22:01+08:00", SysTime(DateTime(2010, 12, 22, 17, 22, 1), east480));

        test("2010-11-03T06:51:06.57159Z", SysTime(DateTime(2010, 11, 3, 6, 51, 6), hnsecs(5715900), UTC()));
        test("2010-12-22T17:22:01.23412Z", SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(2_341_200), UTC()));
        test("2010-12-22T17:22:01.23112-01:00",
             SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(2_311_200), west60));
        test("2010-12-22T17:22:01.45-01", SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(4_500_000), west60));
        test("2010-12-22T17:22:01.1-01:30", SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(1_000_000), west90));
        test("2010-12-22T17:22:01.55-08:00", SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(5_500_000), west480));
        test("2010-12-22T17:22:01.1234567+01:00",
             SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(1_234_567), east60));
        test("2010-12-22T17:22:01.0+01", SysTime(DateTime(2010, 12, 22, 17, 22, 1), east60));
        test("2010-12-22T17:22:01.0000000+01:30", SysTime(DateTime(2010, 12, 22, 17, 22, 1), east90));
        test("2010-12-22T17:22:01.45+08:00", SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(4_500_000), east480));

        static void testScope(scope ref string str) @safe
        {
            auto result = SysTime.fromISOExtString(str);
        }
    }

    // https://issues.dlang.org/show_bug.cgi?id=17801
    @safe unittest
    {
        import core.time;
        import std.conv : to;
        import std.meta : AliasSeq;
        static foreach (C; AliasSeq!(char, wchar, dchar))
        {
            static foreach (S; AliasSeq!(C[], const(C)[], immutable(C)[]))
            {
                assert(SysTime.fromISOExtString(to!S("2012-12-21T14:15:16Z")) ==
                       SysTime(DateTime(2012, 12, 21, 14, 15, 16), UTC()));
            }
        }
    }


    /++
        Creates a $(LREF SysTime) from a string with the format
        YYYY-Mon-DD HH:MM:SS.FFFFFFFTZ (where F is fractional seconds and TZ
        is the time zone). Whitespace is stripped from the given string.

        The exact format is exactly as described in $(LREF toSimpleString) except
        that trailing zeroes are permitted - including having fractional seconds
        with all zeroes. The time zone and fractional seconds are optional,
        however, a decimal point with nothing following it is invalid.
        Also, while $(LREF toSimpleString) will never generate a
        string with more than 7 digits in the fractional seconds (because that's
        the limit with hecto-nanosecond precision), it will allow more than 7
        digits in order to read strings from other sources that have higher
        precision (however, any digits beyond 7 will be truncated).

        If there is no time zone in the string, then
        $(REF LocalTime,std,datetime,timezone) is used. If the time zone is "Z",
        then `UTC` is used. Otherwise, a
        $(REF SimpleTimeZone,std,datetime,timezone) which corresponds to the
        given offset from UTC is used. To get the returned $(LREF SysTime) to be
        a particular time zone, pass in that time zone and the $(LREF SysTime)
        to be returned will be converted to that time zone (though it will still
        be read in as whatever time zone is in its string).

        The accepted formats for time zone offsets are +HH, -HH, +HH:MM, and
        -HH:MM.

        Params:
            simpleString = A string formatted in the way that
                           `toSimpleString` formats dates and times.
            tz           = The time zone to convert the given time to (no
                           conversion occurs if null).

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given string is
            not in the ISO format or if the resulting $(LREF SysTime) would not
            be valid.
      +/
    static SysTime fromSimpleString(S)(scope const S simpleString, immutable TimeZone tz = null) @safe
    if (isSomeString!(S))
    {
        import std.algorithm.searching : find;
        import std.conv : to;
        import std.string : strip, indexOf;

        auto str = strip(simpleString);

        auto spaceIndex = str.indexOf(' ');
        enforce!DateTimeException(spaceIndex != -1,
                                  format("Invalid format for SysTime.fromSimpleString: %s", simpleString));

        auto found = str[spaceIndex + 1 .. $].find('.', 'Z', '+', '-');
        auto dateTimeStr = str[0 .. $ - found[0].length];

        typeof(str) foundTZ;  // needs to have longer lifetime than zoneStr
        typeof(str) fracSecStr;
        typeof(str) zoneStr;

        if (found[1] != 0)
        {
            if (found[1] == 1)
            {
                foundTZ = found[0].find('Z', '+', '-')[0];

                if (foundTZ.length != 0)
                {
                    fracSecStr = found[0][0 .. $ - foundTZ.length];
                    zoneStr = foundTZ;
                }
                else
                    fracSecStr = found[0];
            }
            else
                zoneStr = found[0];
        }

        try
        {
            auto dateTime = DateTime.fromSimpleString(dateTimeStr);
            auto fracSec = fracSecsFromISOString(fracSecStr);
            Rebindable!(immutable TimeZone) parsedZone;

            if (zoneStr.empty)
                parsedZone = LocalTime();
            else if (zoneStr == "Z")
                parsedZone = UTC();
            else
                parsedZone = SimpleTimeZone.fromISOExtString(zoneStr);

            auto retval = SysTime(dateTime, fracSec, parsedZone);

            if (tz !is null)
                retval.timezone = tz;

            return retval;
        }
        catch (DateTimeException dte)
            throw new DateTimeException(format("Invalid format for SysTime.fromSimpleString: %s", simpleString));
    }

    ///
    @safe unittest
    {
        import core.time : hours, msecs, usecs, hnsecs;
        import std.datetime.date : DateTime;
        import std.datetime.timezone : SimpleTimeZone, UTC;

        assert(SysTime.fromSimpleString("2010-Jul-04 07:06:12") ==
               SysTime(DateTime(2010, 7, 4, 7, 6, 12)));

        assert(SysTime.fromSimpleString("1998-Dec-25 02:15:00.007") ==
               SysTime(DateTime(1998, 12, 25, 2, 15, 0), msecs(7)));

        assert(SysTime.fromSimpleString("0000-Jan-05 23:09:59.00002") ==
               SysTime(DateTime(0, 1, 5, 23, 9, 59), usecs(20)));

        assert(SysTime.fromSimpleString("2013-Feb-07 04:39:37.000050392") ==
               SysTime(DateTime(2013, 2, 7, 4, 39, 37), hnsecs(503)));

        assert(SysTime.fromSimpleString("-0004-Jan-05 00:00:02") ==
               SysTime(DateTime(-4, 1, 5, 0, 0, 2)));

        assert(SysTime.fromSimpleString(" 2010-Jul-04 07:06:12 ") ==
               SysTime(DateTime(2010, 7, 4, 7, 6, 12)));

        assert(SysTime.fromSimpleString("2010-Jul-04 07:06:12Z") ==
               SysTime(DateTime(2010, 7, 4, 7, 6, 12), UTC()));

        assert(SysTime.fromSimpleString("2010-Jul-04 07:06:12-08:00") ==
               SysTime(DateTime(2010, 7, 4, 7, 6, 12),
                       new immutable SimpleTimeZone(hours(-8))));

        assert(SysTime.fromSimpleString("2010-Jul-04 07:06:12+08:00") ==
               SysTime(DateTime(2010, 7, 4, 7, 6, 12),
                       new immutable SimpleTimeZone(hours(8))));
    }

    @safe unittest
    {
        import core.time;
        foreach (str; ["", "20100704000000", "20100704 000000",
                       "20100704t000000", "20100704T000000.", "20100704T000000.0",
                       "2010-07-0400:00:00", "2010-07-04 00:00:00", "2010-07-04t00:00:00",
                       "2010-07-04T00:00:00.", "2010-07-04T00:00:00.0",
                       "2010-Jul-0400:00:00", "2010-Jul-04t00:00:00", "2010-Jul-04T00:00:00",
                       "2010-Jul-04 00:00:00.", "2010-Jul-04 00:00:00.A", "2010-Jul-04 00:00:00.Z",
                       "2010-Jul-04 00:00:00.0000000A", "2010-Jul-04 00:00:00.00000000A",
                       "2010-Jul-04 00:00:00+", "2010-Jul-04 00:00:00-",
                       "2010-Jul-04 00:00:00:", "2010-Jul-04 00:00:00-:",
                       "2010-Jul-04 00:00:00+:", "2010-Jul-04 00:00:00-1:",
                       "2010-Jul-04 00:00:00+1:", "2010-Jul-04 00:00:00+1:0",
                       "2010-Jul-04 00:00:00-12.00", "2010-Jul-04 00:00:00+12.00",
                       "2010-Jul-04 00:00:00-8", "2010-Jul-04 00:00:00+8",
                       "20100704T000000-800", "20100704T000000+800",
                       "20100704T000000-080", "20100704T000000+080",
                       "20100704T000000-2400", "20100704T000000+2400",
                       "20100704T000000-1260", "20100704T000000+1260",
                       "20100704T000000.0-800", "20100704T000000.0+800",
                       "20100704T000000.0-8", "20100704T000000.0+8",
                       "20100704T000000.0-080", "20100704T000000.0+080",
                       "20100704T000000.0-2400", "20100704T000000.0+2400",
                       "20100704T000000.0-1260", "20100704T000000.0+1260",
                       "2010-Jul-04 00:00:00-8:00", "2010-Jul-04 00:00:00+8:00",
                       "2010-Jul-04 00:00:00-08:0", "2010-Jul-04 00:00:00+08:0",
                       "2010-Jul-04 00:00:00-24:00", "2010-Jul-04 00:00:00+24:00",
                       "2010-Jul-04 00:00:00-12:60", "2010-Jul-04 00:00:00+24:60",
                       "2010-Jul-04 00:00:00.0-8:00", "2010-Jul-04 00:00:00+8:00",
                       "2010-Jul-04 00:00:00.0-8", "2010-Jul-04 00:00:00.0+8",
                       "2010-Jul-04 00:00:00.0-08:0", "2010-Jul-04 00:00:00.0+08:0",
                       "2010-Jul-04 00:00:00.0-24:00", "2010-Jul-04 00:00:00.0+24:00",
                       "2010-Jul-04 00:00:00.0-12:60", "2010-Jul-04 00:00:00.0+24:60",
                       "20101222T172201", "2010-12-22T172201"])
        {
            assertThrown!DateTimeException(SysTime.fromSimpleString(str), format("[%s]", str));
        }

        static void test(string str, SysTime st, size_t line = __LINE__)
        {
            if (SysTime.fromSimpleString(str) != st)
                throw new AssertError("unittest failure", __FILE__, line);
        }

        test("2010-Dec-22 17:22:01", SysTime(DateTime(2010, 12, 22, 17, 22, 1)));
        test("1999-Jul-06 12:30:33", SysTime(DateTime(1999, 7, 6, 12, 30, 33)));
        test("-1999-Jul-06 12:30:33", SysTime(DateTime(-1999, 7, 6, 12, 30, 33)));
        test("+01999-Jul-06 12:30:33", SysTime(DateTime(1999, 7, 6, 12, 30, 33)));
        test("1999-Jul-06 12:30:33 ", SysTime(DateTime(1999, 7, 6, 12, 30, 33)));
        test(" 1999-Jul-06 12:30:33", SysTime(DateTime(1999, 7, 6, 12, 30, 33)));
        test(" 1999-Jul-06 12:30:33 ", SysTime(DateTime(1999, 7, 6, 12, 30, 33)));

        test("1907-Jul-07 12:12:12.0", SysTime(DateTime(1907, 7, 7, 12, 12, 12)));
        test("1907-Jul-07 12:12:12.0000000", SysTime(DateTime(1907, 7, 7, 12, 12, 12)));
        test("2010-Jul-04 00:00:00.00000000", SysTime(Date(2010, 7, 4)));
        test("2010-Jul-04 00:00:00.00000009", SysTime(Date(2010, 7, 4)));
        test("2010-Jul-04 00:00:00.00000019", SysTime(DateTime(2010, 7, 4), hnsecs(1)));
        test("1907-Jul-07 12:12:12.0000001", SysTime(DateTime(1907, 7, 7, 12, 12, 12), hnsecs(1)));
        test("1907-Jul-07 12:12:12.000001", SysTime(DateTime(1907, 7, 7, 12, 12, 12), usecs(1)));
        test("1907-Jul-07 12:12:12.0000010", SysTime(DateTime(1907, 7, 7, 12, 12, 12), usecs(1)));
        test("1907-Jul-07 12:12:12.001", SysTime(DateTime(1907, 7, 7, 12, 12, 12), msecs(1)));
        test("1907-Jul-07 12:12:12.0010000", SysTime(DateTime(1907, 7, 7, 12, 12, 12), msecs(1)));

        auto west60 = new immutable SimpleTimeZone(hours(-1));
        auto west90 = new immutable SimpleTimeZone(minutes(-90));
        auto west480 = new immutable SimpleTimeZone(hours(-8));
        auto east60 = new immutable SimpleTimeZone(hours(1));
        auto east90 = new immutable SimpleTimeZone(minutes(90));
        auto east480 = new immutable SimpleTimeZone(hours(8));

        test("2010-Dec-22 17:22:01Z", SysTime(DateTime(2010, 12, 22, 17, 22, 1), UTC()));
        test("2010-Dec-22 17:22:01-01:00", SysTime(DateTime(2010, 12, 22, 17, 22, 1), west60));
        test("2010-Dec-22 17:22:01-01", SysTime(DateTime(2010, 12, 22, 17, 22, 1), west60));
        test("2010-Dec-22 17:22:01-01:30", SysTime(DateTime(2010, 12, 22, 17, 22, 1), west90));
        test("2010-Dec-22 17:22:01-08:00", SysTime(DateTime(2010, 12, 22, 17, 22, 1), west480));
        test("2010-Dec-22 17:22:01+01:00", SysTime(DateTime(2010, 12, 22, 17, 22, 1), east60));
        test("2010-Dec-22 17:22:01+01", SysTime(DateTime(2010, 12, 22, 17, 22, 1), east60));
        test("2010-Dec-22 17:22:01+01:30", SysTime(DateTime(2010, 12, 22, 17, 22, 1), east90));
        test("2010-Dec-22 17:22:01+08:00", SysTime(DateTime(2010, 12, 22, 17, 22, 1), east480));

        test("2010-Nov-03 06:51:06.57159Z", SysTime(DateTime(2010, 11, 3, 6, 51, 6), hnsecs(5715900), UTC()));
        test("2010-Dec-22 17:22:01.23412Z", SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(2_341_200), UTC()));
        test("2010-Dec-22 17:22:01.23112-01:00",
             SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(2_311_200), west60));
        test("2010-Dec-22 17:22:01.45-01", SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(4_500_000), west60));
        test("2010-Dec-22 17:22:01.1-01:30", SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(1_000_000), west90));
        test("2010-Dec-22 17:22:01.55-08:00", SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(5_500_000), west480));
        test("2010-Dec-22 17:22:01.1234567+01:00",
             SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(1_234_567), east60));
        test("2010-Dec-22 17:22:01.0+01", SysTime(DateTime(2010, 12, 22, 17, 22, 1), east60));
        test("2010-Dec-22 17:22:01.0000000+01:30", SysTime(DateTime(2010, 12, 22, 17, 22, 1), east90));
        test("2010-Dec-22 17:22:01.45+08:00", SysTime(DateTime(2010, 12, 22, 17, 22, 1), hnsecs(4_500_000), east480));

        static void testScope(scope ref string str) @safe
        {
            auto result = SysTime.fromSimpleString(str);
        }
    }

    // https://issues.dlang.org/show_bug.cgi?id=17801
    @safe unittest
    {
        import core.time;
        import std.conv : to;
        import std.meta : AliasSeq;
        static foreach (C; AliasSeq!(char, wchar, dchar))
        {
            static foreach (S; AliasSeq!(C[], const(C)[], immutable(C)[]))
            {
                assert(SysTime.fromSimpleString(to!S("2012-Dec-21 14:15:16Z")) ==
                       SysTime(DateTime(2012, 12, 21, 14, 15, 16), UTC()));
            }
        }
    }


    /++
        Returns the $(LREF SysTime) farthest in the past which is representable
        by $(LREF SysTime).

        The $(LREF SysTime) which is returned is in UTC.
      +/
    @property static SysTime min() @safe pure nothrow
    {
        return SysTime(long.min, UTC());
    }

    @safe unittest
    {
        assert(SysTime.min.year < 0);
        assert(SysTime.min < SysTime.max);
    }


    /++
        Returns the $(LREF SysTime) farthest in the future which is representable
        by $(LREF SysTime).

        The $(LREF SysTime) which is returned is in UTC.
      +/
    @property static SysTime max() @safe pure nothrow
    {
        return SysTime(long.max, UTC());
    }

    @safe unittest
    {
        assert(SysTime.max.year > 0);
        assert(SysTime.max > SysTime.min);
    }


private:

    /+
        Returns `stdTime` converted to $(LREF SysTime)'s time zone.
      +/
    @property long adjTime() @safe const nothrow scope
    {
        return _timezone.utcToTZ(_stdTime);
    }


    /+
        Converts the given hnsecs from $(LREF SysTime)'s time zone to std time.
      +/
    @property void adjTime(long adjTime) @safe nothrow scope
    {
        _stdTime = _timezone.tzToUTC(adjTime);
    }


    final class InitTimeZone : TimeZone
    {
    public:

        static immutable(InitTimeZone) opCall() @safe pure nothrow @nogc { return _initTimeZone; }

        @property override bool hasDST() @safe const nothrow @nogc { return false; }

        override bool dstInEffect(long stdTime) @safe const scope nothrow @nogc { return false; }

        override long utcToTZ(long stdTime) @safe const scope nothrow @nogc { return 0; }

        override long tzToUTC(long adjTime) @safe const scope nothrow @nogc { return 0; }

        override Duration utcOffsetAt(long stdTime) @safe const scope nothrow @nogc { return Duration.zero; }

    private:

        this() @safe immutable pure
        {
            super("SysTime.init's timezone", "SysTime.init's timezone", "SysTime.init's timezone");
        }

        static immutable InitTimeZone _initTimeZone = new immutable(InitTimeZone);
    }

    // https://issues.dlang.org/show_bug.cgi?id=17732
    @safe unittest
    {
        assert(SysTime.init.timezone is InitTimeZone());
        assert(SysTime.init.toISOString() == "00010101T000000+00:00");
        assert(SysTime.init.toISOExtString() == "0001-01-01T00:00:00+00:00");
        assert(SysTime.init.toSimpleString() == "0001-Jan-01 00:00:00+00:00");
        assert(SysTime.init.toString() == "0001-Jan-01 00:00:00+00:00");
    }

    // Assigning a value to _timezone in SysTime.init currently doesn't work due
    // to https://issues.dlang.org/show_bug.cgi?id=17740. So, to hack around
    // that problem, these accessors have been added so that we can insert a
    // runtime check for null and then use InitTimeZone for SysTime.init (which
    // which is the only case where _timezone would be null). This thus fixes
    // the problem with segfaulting when using SysTime.init but at the cost of
    // what should be an unnecessary null check. Once 17740 has finally been
    // fixed, _timezoneStorage should be removed, these accessors should be
    // removed, and the _timezone variable declaration should be restored.
    pragma(inline, true) @property _timezone() @safe const pure nothrow @nogc
    {
        return _timezoneStorage is null ? InitTimeZone() : _timezoneStorage;
    }

    pragma(inline, true) @property void _timezone(return scope immutable TimeZone tz) @safe pure nothrow @nogc scope
    {
        _timezoneStorage = tz;
    }


    long  _stdTime;
    Rebindable!(immutable TimeZone) _timezoneStorage;
    //Rebindable!(immutable TimeZone) _timezone = InitTimeZone();
}

///
@safe unittest
{
    import core.time : days, hours, seconds;
    import std.datetime.date : Date, DateTime;
    import std.datetime.timezone : SimpleTimeZone, UTC;

    const dt = DateTime(2018, 1, 1, 10, 30, 0);
    // make a specific point in time in the UTC timezone
    auto st = SysTime(dt, UTC());
    assert(st.year == 2018);
    assert(st.hour == 10);

    // cast to convert
    assert(cast(DateTime) st == dt);
    assert(cast(Date) st == Date(2018, 1, 1));

    // make a specific point in time in the New York timezone
    const ny = SysTime(dt,
        new immutable SimpleTimeZone(-5.hours, "America/New_York")
    );
    assert(ny != st);
    assert(ny.hour == 10);

    // ISO standard time strings
    assert(st.toISOString() == "20180101T103000Z");
    assert(st.toISOExtString() == "2018-01-01T10:30:00Z");

    // add two days and 30 seconds
    st += 2.days + 30.seconds;
    assert(st.toISOExtString() == "2018-01-03T10:30:30Z");
}


/++
    Converts from unix time (which uses midnight, January 1st, 1970 UTC as its
    epoch and seconds as its units) to "std time" (which uses midnight,
    January 1st, 1 A.D. UTC and hnsecs as its units).

    The C standard does not specify the representation of time_t, so it is
    implementation defined. On POSIX systems, unix time is equivalent to
    time_t, but that's not necessarily true on other systems (e.g. it is
    not true for the Digital Mars C runtime). So, be careful when using unix
    time with C functions on non-POSIX systems.

    "std time"'s epoch is based on the Proleptic Gregorian Calendar per ISO
    8601 and is what $(LREF SysTime) uses internally. However, holding the time
    as an integer in hnsecs since that epoch technically isn't actually part of
    the standard, much as it's based on it, so the name "std time" isn't
    particularly good, but there isn't an official name for it. C# uses "ticks"
    for the same thing, but they aren't actually clock ticks, and the term
    "ticks" $(I is) used for actual clock ticks for $(REF MonoTime, core,time),
    so it didn't make sense to use the term ticks here. So, for better or worse,
    std.datetime uses the term "std time" for this.

    Params:
        unixTime = The unix time to convert.

    See_Also:
        SysTime.fromUnixTime
  +/
long unixTimeToStdTime(long unixTime) @safe pure nothrow @nogc
{
    return 621_355_968_000_000_000L + convert!("seconds", "hnsecs")(unixTime);
}

///
@safe unittest
{
    import std.datetime.date : DateTime;
    import std.datetime.timezone : UTC;

    // Midnight, January 1st, 1970
    assert(unixTimeToStdTime(0) == 621_355_968_000_000_000L);
    assert(SysTime(unixTimeToStdTime(0)) ==
           SysTime(DateTime(1970, 1, 1), UTC()));

    assert(unixTimeToStdTime(int.max) == 642_830_804_470_000_000L);
    assert(SysTime(unixTimeToStdTime(int.max)) ==
           SysTime(DateTime(2038, 1, 19, 3, 14, 7), UTC()));

    assert(unixTimeToStdTime(-127_127) == 621_354_696_730_000_000L);
    assert(SysTime(unixTimeToStdTime(-127_127)) ==
           SysTime(DateTime(1969, 12, 30, 12, 41, 13), UTC()));
}

@safe unittest
{
    // Midnight, January 2nd, 1970
    assert(unixTimeToStdTime(86_400) == 621_355_968_000_000_000L + 864_000_000_000L);
    // Midnight, December 31st, 1969
    assert(unixTimeToStdTime(-86_400) == 621_355_968_000_000_000L - 864_000_000_000L);

    assert(unixTimeToStdTime(0) == (Date(1970, 1, 1) - Date(1, 1, 1)).total!"hnsecs");
    assert(unixTimeToStdTime(0) == (DateTime(1970, 1, 1) - DateTime(1, 1, 1)).total!"hnsecs");

    foreach (dt; [DateTime(2010, 11, 1, 19, 5, 22), DateTime(1952, 7, 6, 2, 17, 9)])
        assert(unixTimeToStdTime((dt - DateTime(1970, 1, 1)).total!"seconds") == (dt - DateTime.init).total!"hnsecs");
}


/++
    Converts std time (which uses midnight, January 1st, 1 A.D. UTC as its epoch
    and hnsecs as its units) to unix time (which uses midnight, January 1st,
    1970 UTC as its epoch and seconds as its units).

    The C standard does not specify the representation of time_t, so it is
    implementation defined. On POSIX systems, unix time is equivalent to
    time_t, but that's not necessarily true on other systems (e.g. it is
    not true for the Digital Mars C runtime). So, be careful when using unix
    time with C functions on non-POSIX systems.

    "std time"'s epoch is based on the Proleptic Gregorian Calendar per ISO
    8601 and is what $(LREF SysTime) uses internally. However, holding the time
    as an integer in hnsecs since that epoch technically isn't actually part of
    the standard, much as it's based on it, so the name "std time" isn't
    particularly good, but there isn't an official name for it. C# uses "ticks"
    for the same thing, but they aren't actually clock ticks, and the term
    "ticks" $(I is) used for actual clock ticks for $(REF MonoTime, core,time),
    so it didn't make sense to use the term ticks here. So, for better or worse,
    std.datetime uses the term "std time" for this.

    By default, the return type is time_t (which is normally an alias for
    int on 32-bit systems and long on 64-bit systems), but if a different
    size is required than either int or long can be passed as a template
    argument to get the desired size.

    If the return type is int, and the result can't fit in an int, then the
    closest value that can be held in 32 bits will be used (so `int.max`
    if it goes over and `int.min` if it goes under). However, no attempt
    is made to deal with integer overflow if the return type is long.

    Params:
        T = The return type (int or long). It defaults to time_t, which is
            normally 32 bits on a 32-bit system and 64 bits on a 64-bit
            system.
        stdTime = The std time to convert.

    Returns:
        A signed integer representing the unix time which is equivalent to
        the given std time.

    See_Also:
        SysTime.toUnixTime
  +/
T stdTimeToUnixTime(T = time_t)(long stdTime) @safe pure nothrow
if (is(T == int) || is(T == long))
{
    immutable unixTime = convert!("hnsecs", "seconds")(stdTime - 621_355_968_000_000_000L);

    static assert(is(time_t == int) || is(time_t == long),
                  "Currently, std.datetime only supports systems where time_t is int or long");

    static if (is(T == long))
        return unixTime;
    else static if (is(T == int))
    {
        if (unixTime > int.max)
            return int.max;
        return unixTime < int.min ? int.min : cast(int) unixTime;
    }
    else
        static assert(0, "Bug in template constraint. Only int and long allowed.");
}

///
@safe unittest
{
    // Midnight, January 1st, 1970 UTC
    assert(stdTimeToUnixTime(621_355_968_000_000_000L) == 0);

    // 2038-01-19 03:14:07 UTC
    assert(stdTimeToUnixTime(642_830_804_470_000_000L) == int.max);
}

@safe unittest
{
    enum unixEpochAsStdTime = (Date(1970, 1, 1) - Date.init).total!"hnsecs";

    assert(stdTimeToUnixTime(unixEpochAsStdTime) == 0);  // Midnight, January 1st, 1970
    assert(stdTimeToUnixTime(unixEpochAsStdTime + 864_000_000_000L) == 86_400);  // Midnight, January 2nd, 1970
    assert(stdTimeToUnixTime(unixEpochAsStdTime - 864_000_000_000L) == -86_400);  // Midnight, December 31st, 1969

    assert(stdTimeToUnixTime((Date(1970, 1, 1) - Date(1, 1, 1)).total!"hnsecs") == 0);
    assert(stdTimeToUnixTime((DateTime(1970, 1, 1) - DateTime(1, 1, 1)).total!"hnsecs") == 0);

    foreach (dt; [DateTime(2010, 11, 1, 19, 5, 22), DateTime(1952, 7, 6, 2, 17, 9)])
        assert(stdTimeToUnixTime((dt - DateTime.init).total!"hnsecs") == (dt - DateTime(1970, 1, 1)).total!"seconds");

    enum max = convert!("seconds", "hnsecs")(int.max);
    enum min = convert!("seconds", "hnsecs")(int.min);
    enum one = convert!("seconds", "hnsecs")(1);

    assert(stdTimeToUnixTime!long(unixEpochAsStdTime + max) == int.max);
    assert(stdTimeToUnixTime!int(unixEpochAsStdTime + max) == int.max);

    assert(stdTimeToUnixTime!long(unixEpochAsStdTime + max + one) == int.max + 1L);
    assert(stdTimeToUnixTime!int(unixEpochAsStdTime + max + one) == int.max);
    assert(stdTimeToUnixTime!long(unixEpochAsStdTime + max + 9_999_999) == int.max);
    assert(stdTimeToUnixTime!int(unixEpochAsStdTime + max + 9_999_999) == int.max);

    assert(stdTimeToUnixTime!long(unixEpochAsStdTime + min) == int.min);
    assert(stdTimeToUnixTime!int(unixEpochAsStdTime + min) == int.min);

    assert(stdTimeToUnixTime!long(unixEpochAsStdTime + min - one) == int.min - 1L);
    assert(stdTimeToUnixTime!int(unixEpochAsStdTime + min - one) == int.min);
    assert(stdTimeToUnixTime!long(unixEpochAsStdTime + min - 9_999_999) == int.min);
    assert(stdTimeToUnixTime!int(unixEpochAsStdTime + min - 9_999_999) == int.min);
}


version (StdDdoc)
{
    version (Windows)
    {}
    else
    {
        alias SYSTEMTIME = void*;
        alias FILETIME = void*;
    }

    /++
        $(BLUE This function is Windows-Only.)

        Converts a `SYSTEMTIME` struct to a $(LREF SysTime).

        Params:
            st = The `SYSTEMTIME` struct to convert.
            tz = The time zone that the time in the `SYSTEMTIME` struct is
                 assumed to be (if the `SYSTEMTIME` was supplied by a Windows
                 system call, the `SYSTEMTIME` will either be in local time
                 or UTC, depending on the call).

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given
            `SYSTEMTIME` will not fit in a $(LREF SysTime), which is highly
            unlikely to happen given that `SysTime.max` is in 29,228 A.D. and
            the maximum `SYSTEMTIME` is in 30,827 A.D.
      +/
    SysTime SYSTEMTIMEToSysTime(const scope SYSTEMTIME* st, immutable TimeZone tz = LocalTime()) @safe;


    /++
        $(BLUE This function is Windows-Only.)

        Converts a $(LREF SysTime) to a `SYSTEMTIME` struct.

        The `SYSTEMTIME` which is returned will be set using the given
        $(LREF SysTime)'s time zone, so to get the `SYSTEMTIME` in
        UTC, set the $(LREF SysTime)'s time zone to UTC.

        Params:
            sysTime = The $(LREF SysTime) to convert.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given
            $(LREF SysTime) will not fit in a `SYSTEMTIME`. This will only
            happen if the $(LREF SysTime)'s date is prior to 1601 A.D.
      +/
    SYSTEMTIME SysTimeToSYSTEMTIME(scope SysTime sysTime) @safe;


    /++
        $(BLUE This function is Windows-Only.)

        Converts a `FILETIME` struct to the number of hnsecs since midnight,
        January 1st, 1 A.D.

        Params:
            ft = The `FILETIME` struct to convert.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given
            `FILETIME` cannot be represented as the return value.
      +/
    long FILETIMEToStdTime(scope const FILETIME* ft) @safe;


    /++
        $(BLUE This function is Windows-Only.)

        Converts a `FILETIME` struct to a $(LREF SysTime).

        Params:
            ft = The `FILETIME` struct to convert.
            tz = The time zone that the $(LREF SysTime) will be in
                 (`FILETIME`s are in UTC).

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given
            `FILETIME` will not fit in a $(LREF SysTime).
      +/
    SysTime FILETIMEToSysTime(scope const FILETIME* ft, immutable TimeZone tz = LocalTime()) @safe;


    /++
        $(BLUE This function is Windows-Only.)

        Converts a number of hnsecs since midnight, January 1st, 1 A.D. to a
        `FILETIME` struct.

        Params:
            stdTime = The number of hnsecs since midnight, January 1st, 1 A.D.
                      UTC.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given value will
            not fit in a `FILETIME`.
      +/
    FILETIME stdTimeToFILETIME(long stdTime) @safe;


    /++
        $(BLUE This function is Windows-Only.)

        Converts a $(LREF SysTime) to a `FILETIME` struct.

        `FILETIME`s are always in UTC.

        Params:
            sysTime = The $(LREF SysTime) to convert.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given
            $(LREF SysTime) will not fit in a `FILETIME`.
      +/
    FILETIME SysTimeToFILETIME(scope SysTime sysTime) @safe;
}
else version (Windows)
{
    SysTime SYSTEMTIMEToSysTime(const scope SYSTEMTIME* st, immutable TimeZone tz = LocalTime()) @safe
    {
        const max = SysTime.max;

        static void throwLaterThanMax()
        {
            throw new DateTimeException("The given SYSTEMTIME is for a date greater than SysTime.max.");
        }

        if (st.wYear > max.year)
            throwLaterThanMax();
        else if (st.wYear == max.year)
        {
            if (st.wMonth > max.month)
                throwLaterThanMax();
            else if (st.wMonth == max.month)
            {
                if (st.wDay > max.day)
                    throwLaterThanMax();
                else if (st.wDay == max.day)
                {
                    if (st.wHour > max.hour)
                        throwLaterThanMax();
                    else if (st.wHour == max.hour)
                    {
                        if (st.wMinute > max.minute)
                            throwLaterThanMax();
                        else if (st.wMinute == max.minute)
                        {
                            if (st.wSecond > max.second)
                                throwLaterThanMax();
                            else if (st.wSecond == max.second)
                            {
                                if (st.wMilliseconds > max.fracSecs.total!"msecs")
                                    throwLaterThanMax();
                            }
                        }
                    }
                }
            }
        }

        auto dt = DateTime(st.wYear, st.wMonth, st.wDay, st.wHour, st.wMinute, st.wSecond);

        import core.time : msecs;
        return SysTime(dt, msecs(st.wMilliseconds), tz);
    }

    @system unittest
    {
        auto sysTime = Clock.currTime(UTC());
        SYSTEMTIME st = void;
        GetSystemTime(&st);
        auto converted = SYSTEMTIMEToSysTime(&st, UTC());
        import core.time : abs;
        assert(abs((converted - sysTime)) <= dur!"seconds"(2));

        static void testScope(scope SYSTEMTIME* st) @safe
        {
            auto result = SYSTEMTIMEToSysTime(st);
        }
    }


    SYSTEMTIME SysTimeToSYSTEMTIME(scope SysTime sysTime) @safe
    {
        immutable dt = cast(DateTime) sysTime;

        if (dt.year < 1601)
            throw new DateTimeException("SYSTEMTIME cannot hold dates prior to the year 1601.");

        SYSTEMTIME st;

        st.wYear = dt.year;
        st.wMonth = dt.month;
        st.wDayOfWeek = dt.dayOfWeek;
        st.wDay = dt.day;
        st.wHour = dt.hour;
        st.wMinute = dt.minute;
        st.wSecond = dt.second;
        st.wMilliseconds = cast(ushort) sysTime.fracSecs.total!"msecs";

        return st;
    }

    @system unittest
    {
        SYSTEMTIME st = void;
        GetSystemTime(&st);
        auto sysTime = SYSTEMTIMEToSysTime(&st, UTC());

        SYSTEMTIME result = SysTimeToSYSTEMTIME(sysTime);

        assert(st.wYear == result.wYear);
        assert(st.wMonth == result.wMonth);
        assert(st.wDayOfWeek == result.wDayOfWeek);
        assert(st.wDay == result.wDay);
        assert(st.wHour == result.wHour);
        assert(st.wMinute == result.wMinute);
        assert(st.wSecond == result.wSecond);
        assert(st.wMilliseconds == result.wMilliseconds);

        static void testScope(scope ref SysTime st) @safe
        {
            auto localResult = SysTimeToSYSTEMTIME(st);
        }
    }

    private enum hnsecsFrom1601 = 504_911_232_000_000_000L;

    long FILETIMEToStdTime(scope const FILETIME* ft) @safe
    {
        ULARGE_INTEGER ul;
        ul.HighPart = ft.dwHighDateTime;
        ul.LowPart = ft.dwLowDateTime;
        ulong tempHNSecs = ul.QuadPart;

        if (tempHNSecs > long.max - hnsecsFrom1601)
            throw new DateTimeException("The given FILETIME cannot be represented as a stdTime value.");

        return cast(long) tempHNSecs + hnsecsFrom1601;
    }

    SysTime FILETIMEToSysTime(scope const FILETIME* ft, immutable TimeZone tz = LocalTime()) @safe
    {
        auto sysTime = SysTime(FILETIMEToStdTime(ft), UTC());
        sysTime.timezone = tz;
        return sysTime;
    }

    @system unittest
    {
        auto sysTime = Clock.currTime(UTC());
        SYSTEMTIME st = void;
        GetSystemTime(&st);

        FILETIME ft = void;
        SystemTimeToFileTime(&st, &ft);

        auto converted = FILETIMEToSysTime(&ft);

        import core.time : abs;
        assert(abs((converted - sysTime)) <= dur!"seconds"(2));

        static void testScope(scope FILETIME* ft) @safe
        {
            auto result = FILETIMEToSysTime(ft);
        }
    }


    FILETIME stdTimeToFILETIME(long stdTime) @safe
    {
        if (stdTime < hnsecsFrom1601)
            throw new DateTimeException("The given stdTime value cannot be represented as a FILETIME.");

        ULARGE_INTEGER ul;
        ul.QuadPart = cast(ulong) stdTime - hnsecsFrom1601;

        FILETIME ft;
        ft.dwHighDateTime = ul.HighPart;
        ft.dwLowDateTime = ul.LowPart;

        return ft;
    }

    FILETIME SysTimeToFILETIME(scope SysTime sysTime) @safe
    {
        return stdTimeToFILETIME(sysTime.stdTime);
    }

    @system unittest
    {
        SYSTEMTIME st = void;
        GetSystemTime(&st);

        FILETIME ft = void;
        SystemTimeToFileTime(&st, &ft);
        auto sysTime = FILETIMEToSysTime(&ft, UTC());

        FILETIME result = SysTimeToFILETIME(sysTime);

        assert(ft.dwLowDateTime == result.dwLowDateTime);
        assert(ft.dwHighDateTime == result.dwHighDateTime);

        static void testScope(scope ref SysTime st) @safe
        {
            auto local_result = SysTimeToFILETIME(st);
        }
    }
}


/++
    Type representing the DOS file date/time format.
  +/
alias DosFileTime = uint;

/++
    Converts from DOS file date/time to $(LREF SysTime).

    Params:
        dft = The DOS file time to convert.
        tz  = The time zone which the DOS file time is assumed to be in.

    Throws:
        $(REF DateTimeException,std,datetime,date) if the `DosFileTime` is
        invalid.
  +/
SysTime DosFileTimeToSysTime(DosFileTime dft, immutable TimeZone tz = LocalTime()) @safe
{
    uint dt = cast(uint) dft;

    if (dt == 0)
        throw new DateTimeException("Invalid DosFileTime.");

    int year = ((dt >> 25) & 0x7F) + 1980;
    int month = ((dt >> 21) & 0x0F);       // 1 .. 12
    int dayOfMonth = ((dt >> 16) & 0x1F);  // 1 .. 31
    int hour = (dt >> 11) & 0x1F;          // 0 .. 23
    int minute = (dt >> 5) & 0x3F;         // 0 .. 59
    int second = (dt << 1) & 0x3E;         // 0 .. 58 (in 2 second increments)

    try
        return SysTime(DateTime(year, month, dayOfMonth, hour, minute, second), tz);
    catch (DateTimeException dte)
        throw new DateTimeException("Invalid DosFileTime", __FILE__, __LINE__, dte);
}

///
@safe unittest
{
    import std.datetime.date : DateTime;

    assert(DosFileTimeToSysTime(0b00000000001000010000000000000000) == SysTime(DateTime(1980, 1, 1, 0, 0, 0)));
    assert(DosFileTimeToSysTime(0b11111111100111111011111101111101) == SysTime(DateTime(2107, 12, 31, 23, 59, 58)));
    assert(DosFileTimeToSysTime(0x3E3F8456) == SysTime(DateTime(2011, 1, 31, 16, 34, 44)));
}

@safe unittest
{
    static void testScope(scope ref DosFileTime dft) @safe
    {
        auto result = DosFileTimeToSysTime(dft);
    }
}


/++
    Converts from $(LREF SysTime) to DOS file date/time.

    Params:
        sysTime = The $(LREF SysTime) to convert.

    Throws:
        $(REF DateTimeException,std,datetime,date) if the given
        $(LREF SysTime) cannot be converted to a `DosFileTime`.
  +/
DosFileTime SysTimeToDosFileTime(scope SysTime sysTime) @safe
{
    auto dateTime = cast(DateTime) sysTime;

    if (dateTime.year < 1980)
        throw new DateTimeException("DOS File Times cannot hold dates prior to 1980.");

    if (dateTime.year > 2107)
        throw new DateTimeException("DOS File Times cannot hold dates past 2107.");

    uint retval = 0;
    retval = (dateTime.year - 1980) << 25;
    retval |= (dateTime.month & 0x0F) << 21;
    retval |= (dateTime.day & 0x1F) << 16;
    retval |= (dateTime.hour & 0x1F) << 11;
    retval |= (dateTime.minute & 0x3F) << 5;
    retval |= (dateTime.second >> 1) & 0x1F;

    return cast(DosFileTime) retval;
}

///
@safe unittest
{
    import std.datetime.date : DateTime;

    assert(SysTimeToDosFileTime(SysTime(DateTime(1980, 1, 1, 0, 0, 0))) == 0b00000000001000010000000000000000);
    assert(SysTimeToDosFileTime(SysTime(DateTime(2107, 12, 31, 23, 59, 58))) == 0b11111111100111111011111101111101);
    assert(SysTimeToDosFileTime(SysTime(DateTime(2011, 1, 31, 16, 34, 44))) == 0x3E3F8456);
}

@safe unittest
{
    static void testScope(scope ref SysTime st) @safe
    {
        auto result = SysTimeToDosFileTime(st);
    }
}


/++
    The given array of `char` or random-access range of `char` or
    `ubyte` is expected to be in the format specified in
    $(HTTP tools.ietf.org/html/rfc5322, RFC 5322) section 3.3 with the
    grammar rule $(I date-time). It is the date-time format commonly used in
    internet messages such as e-mail and HTTP. The corresponding
    $(LREF SysTime) will be returned.

    RFC 822 was the original spec (hence the function's name), whereas RFC 5322
    is the current spec.

    The day of the week is ignored beyond verifying that it's a valid day of the
    week, as the day of the week can be inferred from the date. It is not
    checked whether the given day of the week matches the actual day of the week
    of the given date (though it is technically invalid per the spec if the
    day of the week doesn't match the actual day of the week of the given date).

    If the time zone is `"-0000"` (or considered to be equivalent to
    `"-0000"` by section 4.3 of the spec), a
    $(REF SimpleTimeZone,std,datetime,timezone) with a utc offset of `0` is
    used rather than $(REF UTC,std,datetime,timezone), whereas `"+0000"` uses
    $(REF UTC,std,datetime,timezone).

    Note that because $(LREF SysTime) does not currently support having a second
    value of 60 (as is sometimes done for leap seconds), if the date-time value
    does have a value of 60 for the seconds, it is treated as 59.

    The one area in which this function violates RFC 5322 is that it accepts
    `"\n"` in folding whitespace in the place of `"\r\n"`, because the
    HTTP spec requires it.

    Throws:
        $(REF DateTimeException,std,datetime,date) if the given string doesn't
        follow the grammar for a date-time field or if the resulting
        $(LREF SysTime) is invalid.
  +/
SysTime parseRFC822DateTime()(scope const char[] value) @safe
{
    import std.string : representation;
    return parseRFC822DateTime(value.representation);
}

/++ Ditto +/
SysTime parseRFC822DateTime(R)(scope R value)
if (isRandomAccessRange!R && hasSlicing!R && hasLength!R &&
    (is(immutable ElementType!R == immutable char) || is(immutable ElementType!R == immutable ubyte)))
{
    import std.algorithm.searching : find, all;
    import std.ascii : isDigit, isAlpha, isPrintable;
    import std.conv : to;
    import std.functional : not;
    import std.string : capitalize, format;
    import std.traits : EnumMembers, isArray;
    import std.typecons : Rebindable;

    void stripAndCheckLen(R valueBefore, size_t minLen, size_t line = __LINE__)
    {
        value = _stripCFWS(valueBefore);
        if (value.length < minLen)
            throw new DateTimeException("date-time value too short", __FILE__, line);
    }
    stripAndCheckLen(value, "7Dec1200:00A".length);

    static if (isArray!R && (is(ElementEncodingType!R == char) || is(ElementEncodingType!R == ubyte)))
    {
        static string sliceAsString(R str) @trusted
        {
            return cast(string) str;
        }
    }
    else
    {
        char[4] temp;
        char[] sliceAsString(R str) @trusted
        {
            size_t i = 0;
            foreach (c; str)
                temp[i++] = cast(char) c;
            return temp[0 .. str.length];
        }
    }

    // day-of-week
    if (isAlpha(value[0]))
    {
        auto dowStr = sliceAsString(value[0 .. 3]);
        switch (dowStr)
        {
            foreach (dow; EnumMembers!DayOfWeek)
            {
                enum dowC = capitalize(to!string(dow));
                case dowC:
                    goto afterDoW;
            }
            default: throw new DateTimeException(format("Invalid day-of-week: %s", dowStr));
        }
afterDoW: stripAndCheckLen(value[3 .. value.length], ",7Dec1200:00A".length);
        if (value[0] != ',')
            throw new DateTimeException("day-of-week missing comma");
        stripAndCheckLen(value[1 .. value.length], "7Dec1200:00A".length);
    }

    // day
    immutable digits = isDigit(value[1]) ? 2 : 1;
    immutable day = _convDigits!short(value[0 .. digits]);
    if (day == -1)
        throw new DateTimeException("Invalid day");
    stripAndCheckLen(value[digits .. value.length], "Dec1200:00A".length);

    // month
    Month month;
    {
        auto monStr = sliceAsString(value[0 .. 3]);
        switch (monStr)
        {
            foreach (mon; EnumMembers!Month)
            {
                enum monC = capitalize(to!string(mon));
                case monC:
                {
                    month = mon;
                    goto afterMon;
                }
            }
            default: throw new DateTimeException(format("Invalid month: %s", monStr));
        }
afterMon: stripAndCheckLen(value[3 .. value.length], "1200:00A".length);
    }

    // year
    auto found = value[2 .. value.length].find!(not!(isDigit))();
    size_t yearLen = value.length - found.length;
    if (found.length == 0)
        throw new DateTimeException("Invalid year");
    if (found[0] == ':')
        yearLen -= 2;
    auto year = _convDigits!short(value[0 .. yearLen]);
    if (year < 1900)
    {
        if (year == -1)
            throw new DateTimeException("Invalid year");
        if (yearLen < 4)
        {
            if (yearLen == 3)
                year += 1900;
            else if (yearLen == 2)
                year += year < 50 ? 2000 : 1900;
            else
                throw new DateTimeException("Invalid year. Too few digits.");
        }
        else
            throw new DateTimeException("Invalid year. Cannot be earlier than 1900.");
    }
    stripAndCheckLen(value[yearLen .. value.length], "00:00A".length);

    // hour
    immutable hour = _convDigits!short(value[0 .. 2]);
    stripAndCheckLen(value[2 .. value.length], ":00A".length);
    if (value[0] != ':')
        throw new DateTimeException("Invalid hour");
    stripAndCheckLen(value[1 .. value.length], "00A".length);

    // minute
    immutable minute = _convDigits!short(value[0 .. 2]);
    stripAndCheckLen(value[2 .. value.length], "A".length);

    // second
    short second;
    if (value[0] == ':')
    {
        stripAndCheckLen(value[1 .. value.length], "00A".length);
        second = _convDigits!short(value[0 .. 2]);
        // this is just if/until SysTime is sorted out to fully support leap seconds
        if (second == 60)
            second = 59;
        stripAndCheckLen(value[2 .. value.length], "A".length);
    }

    immutable(TimeZone) parseTZ(int sign)
    {
        if (value.length < 5)
            throw new DateTimeException("Invalid timezone");
        immutable zoneHours = _convDigits!short(value[1 .. 3]);
        immutable zoneMinutes = _convDigits!short(value[3 .. 5]);
        if (zoneHours == -1 || zoneMinutes == -1 || zoneMinutes > 59)
            throw new DateTimeException("Invalid timezone");
        value = value[5 .. value.length];
        immutable utcOffset = (dur!"hours"(zoneHours) + dur!"minutes"(zoneMinutes)) * sign;
        if (utcOffset == Duration.zero)
        {
            return sign == 1 ? cast(immutable(TimeZone))UTC()
                             : cast(immutable(TimeZone))new immutable SimpleTimeZone(Duration.zero);
        }
        return new immutable(SimpleTimeZone)(utcOffset);
    }

    // zone
    Rebindable!(immutable TimeZone) tz;
    if (value[0] == '-')
        tz = parseTZ(-1);
    else if (value[0] == '+')
        tz = parseTZ(1);
    else
    {
        // obs-zone
        immutable tzLen = value.length - find(value, ' ', '\t', '(')[0].length;
        switch (sliceAsString(value[0 .. tzLen <= 4 ? tzLen : 4]))
        {
            case "UT": case "GMT": tz = UTC(); break;
            case "EST": tz = new immutable SimpleTimeZone(dur!"hours"(-5)); break;
            case "EDT": tz = new immutable SimpleTimeZone(dur!"hours"(-4)); break;
            case "CST": tz = new immutable SimpleTimeZone(dur!"hours"(-6)); break;
            case "CDT": tz = new immutable SimpleTimeZone(dur!"hours"(-5)); break;
            case "MST": tz = new immutable SimpleTimeZone(dur!"hours"(-7)); break;
            case "MDT": tz = new immutable SimpleTimeZone(dur!"hours"(-6)); break;
            case "PST": tz = new immutable SimpleTimeZone(dur!"hours"(-8)); break;
            case "PDT": tz = new immutable SimpleTimeZone(dur!"hours"(-7)); break;
            case "J": case "j": throw new DateTimeException("Invalid timezone");
            default:
            {
                if (all!(isAlpha)(value[0 .. tzLen]))
                {
                    tz = new immutable SimpleTimeZone(Duration.zero);
                    break;
                }
                throw new DateTimeException("Invalid timezone");
            }
        }
        value = value[tzLen .. value.length];
    }

    // This is kind of arbitrary. Technically, nothing but CFWS is legal past
    // the end of the timezone, but we don't want to be picky about that in a
    // function that's just parsing rather than validating. So, the idea here is
    // that if the next character is printable (and not part of CFWS), then it
    // might be part of the timezone and thus affect what the timezone was
    // supposed to be, so we'll throw, but otherwise, we'll just ignore it.
    if (!value.empty && isPrintable(value[0]) && value[0] != ' ' && value[0] != '(')
        throw new DateTimeException("Invalid timezone");

    try
        return SysTime(DateTime(year, month, day, hour, minute, second), tz);
    catch (DateTimeException dte)
        throw new DateTimeException("date-time format is correct, but the resulting SysTime is invalid.", dte);
}

///
@safe unittest
{
    import core.time : hours;
    import std.datetime.date : DateTime, DateTimeException;
    import std.datetime.timezone : SimpleTimeZone, UTC;
    import std.exception : assertThrown;

    auto tz = new immutable SimpleTimeZone(hours(-8));
    assert(parseRFC822DateTime("Sat, 6 Jan 1990 12:14:19 -0800") ==
           SysTime(DateTime(1990, 1, 6, 12, 14, 19), tz));

    assert(parseRFC822DateTime("9 Jul 2002 13:11 +0000") ==
           SysTime(DateTime(2002, 7, 9, 13, 11, 0), UTC()));

    auto badStr = "29 Feb 2001 12:17:16 +0200";
    assertThrown!DateTimeException(parseRFC822DateTime(badStr));
}

version (StdUnittest) private void testParse822(alias cr)(string str, SysTime expected, size_t line = __LINE__)
{
    import std.format : format;
    auto value = cr(str);
    auto result = parseRFC822DateTime(value);
    if (result != expected)
        throw new AssertError(format("wrong result. expected [%s], actual[%s]", expected, result), __FILE__, line);
}

version (StdUnittest) private void testBadParse822(alias cr)(string str, size_t line = __LINE__)
{
    try
        parseRFC822DateTime(cr(str));
    catch (DateTimeException)
        return;
    throw new AssertError("No DateTimeException was thrown", __FILE__, line);
}

@system unittest
{
    import core.time;
    import std.algorithm.iteration : filter, map;
    import std.algorithm.searching : canFind;
    import std.array : array;
    import std.ascii : letters;
    import std.format : format;
    import std.meta : AliasSeq;
    import std.range : chain, iota, take;
    import std.stdio : writefln, writeln;
    import std.string : representation;

    static struct Rand3Letters
    {
        enum empty = false;
        @property auto front() { return _mon; }
        void popFront()
        {
            import std.exception : assumeUnique;
            import std.random : rndGen;
            _mon = rndGen.map!(a => letters[a % letters.length])().take(3).array().assumeUnique();
        }
        string _mon;
        static auto start() { Rand3Letters retval; retval.popFront(); return retval; }
    }

    static foreach (cr; AliasSeq!(function(string a){return cast(char[]) a;},
                           function(string a){return cast(ubyte[]) a;},
                           function(string a){return a;},
                           function(string a){return map!(b => cast(char) b)(a.representation);}))
    {(){ // workaround slow optimizations for large functions
         // https://issues.dlang.org/show_bug.cgi?id=2396
        scope(failure) writeln(typeof(cr).stringof);
        alias test = testParse822!cr;
        alias testBad = testBadParse822!cr;

        immutable std1 = DateTime(2012, 12, 21, 13, 14, 15);
        immutable std2 = DateTime(2012, 12, 21, 13, 14, 0);
        immutable dst1 = DateTime(1976, 7, 4, 5, 4, 22);
        immutable dst2 = DateTime(1976, 7, 4, 5, 4, 0);

        test("21 Dec 2012 13:14:15 +0000", SysTime(std1, UTC()));
        test("21 Dec 2012 13:14 +0000", SysTime(std2, UTC()));
        test("Fri, 21 Dec 2012 13:14 +0000", SysTime(std2, UTC()));
        test("Fri, 21 Dec 2012 13:14:15 +0000", SysTime(std1, UTC()));

        test("04 Jul 1976 05:04:22 +0000", SysTime(dst1, UTC()));
        test("04 Jul 1976 05:04 +0000", SysTime(dst2, UTC()));
        test("Sun, 04 Jul 1976 05:04 +0000", SysTime(dst2, UTC()));
        test("Sun, 04 Jul 1976 05:04:22 +0000", SysTime(dst1, UTC()));

        test("4 Jul 1976 05:04:22 +0000", SysTime(dst1, UTC()));
        test("4 Jul 1976 05:04 +0000", SysTime(dst2, UTC()));
        test("Sun, 4 Jul 1976 05:04 +0000", SysTime(dst2, UTC()));
        test("Sun, 4 Jul 1976 05:04:22 +0000", SysTime(dst1, UTC()));

        auto badTZ = new immutable SimpleTimeZone(Duration.zero);
        test("21 Dec 2012 13:14:15 -0000", SysTime(std1, badTZ));
        test("21 Dec 2012 13:14 -0000", SysTime(std2, badTZ));
        test("Fri, 21 Dec 2012 13:14 -0000", SysTime(std2, badTZ));
        test("Fri, 21 Dec 2012 13:14:15 -0000", SysTime(std1, badTZ));

        test("04 Jul 1976 05:04:22 -0000", SysTime(dst1, badTZ));
        test("04 Jul 1976 05:04 -0000", SysTime(dst2, badTZ));
        test("Sun, 04 Jul 1976 05:04 -0000", SysTime(dst2, badTZ));
        test("Sun, 04 Jul 1976 05:04:22 -0000", SysTime(dst1, badTZ));

        test("4 Jul 1976 05:04:22 -0000", SysTime(dst1, badTZ));
        test("4 Jul 1976 05:04 -0000", SysTime(dst2, badTZ));
        test("Sun, 4 Jul 1976 05:04 -0000", SysTime(dst2, badTZ));
        test("Sun, 4 Jul 1976 05:04:22 -0000", SysTime(dst1, badTZ));

        auto pst = new immutable SimpleTimeZone(dur!"hours"(-8));
        auto pdt = new immutable SimpleTimeZone(dur!"hours"(-7));
        test("21 Dec 2012 13:14:15 -0800", SysTime(std1, pst));
        test("21 Dec 2012 13:14 -0800", SysTime(std2, pst));
        test("Fri, 21 Dec 2012 13:14 -0800", SysTime(std2, pst));
        test("Fri, 21 Dec 2012 13:14:15 -0800", SysTime(std1, pst));

        test("04 Jul 1976 05:04:22 -0700", SysTime(dst1, pdt));
        test("04 Jul 1976 05:04 -0700", SysTime(dst2, pdt));
        test("Sun, 04 Jul 1976 05:04 -0700", SysTime(dst2, pdt));
        test("Sun, 04 Jul 1976 05:04:22 -0700", SysTime(dst1, pdt));

        test("4 Jul 1976 05:04:22 -0700", SysTime(dst1, pdt));
        test("4 Jul 1976 05:04 -0700", SysTime(dst2, pdt));
        test("Sun, 4 Jul 1976 05:04 -0700", SysTime(dst2, pdt));
        test("Sun, 4 Jul 1976 05:04:22 -0700", SysTime(dst1, pdt));

        auto cet = new immutable SimpleTimeZone(dur!"hours"(1));
        auto cest = new immutable SimpleTimeZone(dur!"hours"(2));
        test("21 Dec 2012 13:14:15 +0100", SysTime(std1, cet));
        test("21 Dec 2012 13:14 +0100", SysTime(std2, cet));
        test("Fri, 21 Dec 2012 13:14 +0100", SysTime(std2, cet));
        test("Fri, 21 Dec 2012 13:14:15 +0100", SysTime(std1, cet));

        test("04 Jul 1976 05:04:22 +0200", SysTime(dst1, cest));
        test("04 Jul 1976 05:04 +0200", SysTime(dst2, cest));
        test("Sun, 04 Jul 1976 05:04 +0200", SysTime(dst2, cest));
        test("Sun, 04 Jul 1976 05:04:22 +0200", SysTime(dst1, cest));

        test("4 Jul 1976 05:04:22 +0200", SysTime(dst1, cest));
        test("4 Jul 1976 05:04 +0200", SysTime(dst2, cest));
        test("Sun, 4 Jul 1976 05:04 +0200", SysTime(dst2, cest));
        test("Sun, 4 Jul 1976 05:04:22 +0200", SysTime(dst1, cest));

        // dst and std times are switched in the Southern Hemisphere which is why the
        // time zone names and DateTime variables don't match.
        auto cstStd = new immutable SimpleTimeZone(dur!"hours"(9) + dur!"minutes"(30));
        auto cstDST = new immutable SimpleTimeZone(dur!"hours"(10) + dur!"minutes"(30));
        test("21 Dec 2012 13:14:15 +1030", SysTime(std1, cstDST));
        test("21 Dec 2012 13:14 +1030", SysTime(std2, cstDST));
        test("Fri, 21 Dec 2012 13:14 +1030", SysTime(std2, cstDST));
        test("Fri, 21 Dec 2012 13:14:15 +1030", SysTime(std1, cstDST));

        test("04 Jul 1976 05:04:22 +0930", SysTime(dst1, cstStd));
        test("04 Jul 1976 05:04 +0930", SysTime(dst2, cstStd));
        test("Sun, 04 Jul 1976 05:04 +0930", SysTime(dst2, cstStd));
        test("Sun, 04 Jul 1976 05:04:22 +0930", SysTime(dst1, cstStd));

        test("4 Jul 1976 05:04:22 +0930", SysTime(dst1, cstStd));
        test("4 Jul 1976 05:04 +0930", SysTime(dst2, cstStd));
        test("Sun, 4 Jul 1976 05:04 +0930", SysTime(dst2, cstStd));
        test("Sun, 4 Jul 1976 05:04:22 +0930", SysTime(dst1, cstStd));

        foreach (int i, mon; _monthNames)
        {
            test(format("17 %s 2012 00:05:02 +0000", mon), SysTime(DateTime(2012, i + 1, 17, 0, 5, 2), UTC()));
            test(format("17 %s 2012 00:05 +0000", mon), SysTime(DateTime(2012, i + 1, 17, 0, 5, 0), UTC()));
        }

        import std.uni : toLower, toUpper;
        foreach (mon; chain(_monthNames[].map!(a => toLower(a))(),
                            _monthNames[].map!(a => toUpper(a))(),
                            ["Jam", "Jen", "Fec", "Fdb", "Mas", "Mbr", "Aps", "Aqr", "Mai", "Miy",
                             "Jum", "Jbn", "Jup", "Jal", "Aur", "Apg", "Sem", "Sap", "Ocm", "Odt",
                             "Nom", "Nav", "Dem", "Dac"],
                            Rand3Letters.start().filter!(a => !_monthNames[].canFind(a)).take(20)))
        {
            scope(failure) writefln("Month: %s", mon);
            testBad(format("17 %s 2012 00:05:02 +0000", mon));
            testBad(format("17 %s 2012 00:05 +0000", mon));
        }

        immutable string[7] daysOfWeekNames = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];

        {
            auto start = SysTime(DateTime(2012, 11, 11, 9, 42, 0), UTC());
            int day = 11;

            foreach (int i, dow; daysOfWeekNames)
            {
                auto curr = start + dur!"days"(i);
                test(format("%s, %s Nov 2012 09:42:00 +0000", dow, day), curr);
                test(format("%s, %s Nov 2012 09:42 +0000", dow, day++), curr);

                // Whether the day of the week matches the date is ignored.
                test(format("%s, 11 Nov 2012 09:42:00 +0000", dow), start);
                test(format("%s, 11 Nov 2012 09:42 +0000", dow), start);
            }
        }

        foreach (dow; chain(daysOfWeekNames[].map!(a => toLower(a))(),
                            daysOfWeekNames[].map!(a => toUpper(a))(),
                            ["Sum", "Spn", "Mom", "Man", "Tuf", "Tae", "Wem", "Wdd", "The", "Tur",
                             "Fro", "Fai", "San", "Sut"],
                            Rand3Letters.start().filter!(a => !daysOfWeekNames[].canFind(a)).take(20)))
        {
            scope(failure) writefln("Day of Week: %s", dow);
            testBad(format("%s, 11 Nov 2012 09:42:00 +0000", dow));
            testBad(format("%s, 11 Nov 2012 09:42 +0000", dow));
        }

        testBad("31 Dec 1899 23:59:59 +0000");
        test("01 Jan 1900 00:00:00 +0000", SysTime(Date(1900, 1, 1), UTC()));
        test("01 Jan 1900 00:00:00 -0000", SysTime(Date(1900, 1, 1),
                                                   new immutable SimpleTimeZone(Duration.zero)));
        test("01 Jan 1900 00:00:00 -0700", SysTime(Date(1900, 1, 1),
                                                   new immutable SimpleTimeZone(dur!"hours"(-7))));

        {
            auto st1 = SysTime(Date(1900, 1, 1), UTC());
            auto st2 = SysTime(Date(1900, 1, 1), new immutable SimpleTimeZone(dur!"hours"(-11)));
            foreach (i; 1900 .. 2102)
            {
                test(format("1 Jan %05d 00:00 +0000", i), st1);
                test(format("1 Jan %05d 00:00 -1100", i), st2);
                st1.add!"years"(1);
                st2.add!"years"(1);
            }
            st1.year = 9998;
            st2.year = 9998;
            foreach (i; 9998 .. 11_002)
            {
                test(format("1 Jan %05d 00:00 +0000", i), st1);
                test(format("1 Jan %05d 00:00 -1100", i), st2);
                st1.add!"years"(1);
                st2.add!"years"(1);
            }
        }

        testBad("12 Feb 1907 23:17:09 0000");
        testBad("12 Feb 1907 23:17:09 +000");
        testBad("12 Feb 1907 23:17:09 -000");
        testBad("12 Feb 1907 23:17:09 +00000");
        testBad("12 Feb 1907 23:17:09 -00000");
        testBad("12 Feb 1907 23:17:09 +A");
        testBad("12 Feb 1907 23:17:09 +PST");
        testBad("12 Feb 1907 23:17:09 -A");
        testBad("12 Feb 1907 23:17:09 -PST");

        // test trailing stuff that gets ignored
        {
            foreach (c; chain(iota(0, 33), ['('], iota(127, ubyte.max + 1)))
            {
                scope(failure) writefln("c: %d", c);
                test(format("21 Dec 2012 13:14:15 +0000%c", cast(char) c), SysTime(std1, UTC()));
                test(format("21 Dec 2012 13:14:15 +0000%c  ", cast(char) c), SysTime(std1, UTC()));
                test(format("21 Dec 2012 13:14:15 +0000%chello", cast(char) c), SysTime(std1, UTC()));
            }
        }

        // test trailing stuff that doesn't get ignored
        {
            foreach (c; chain(iota(33, '('), iota('(' + 1, 127)))
            {
                scope(failure) writefln("c: %d", c);
                testBad(format("21 Dec 2012 13:14:15 +0000%c", cast(char) c));
                testBad(format("21 Dec 2012 13:14:15 +0000%c   ", cast(char) c));
                testBad(format("21 Dec 2012 13:14:15 +0000%chello", cast(char) c));
            }
        }

        testBad("32 Jan 2012 12:13:14 -0800");
        testBad("31 Jan 2012 24:13:14 -0800");
        testBad("31 Jan 2012 12:60:14 -0800");
        testBad("31 Jan 2012 12:13:61 -0800");
        testBad("31 Jan 2012 12:13:14 -0860");
        test("31 Jan 2012 12:13:14 -0859",
             SysTime(DateTime(2012, 1, 31, 12, 13, 14),
                     new immutable SimpleTimeZone(dur!"hours"(-8) + dur!"minutes"(-59))));

        // leap-seconds
        test("21 Dec 2012 15:59:60 -0800", SysTime(DateTime(2012, 12, 21, 15, 59, 59), pst));

        // FWS
        test("Sun,4 Jul 1976 05:04 +0930", SysTime(dst2, cstStd));
        test("Sun,4 Jul 1976 05:04:22 +0930", SysTime(dst1, cstStd));
        test("Sun,4 Jul 1976 05:04 +0930 (foo)", SysTime(dst2, cstStd));
        test("Sun,4 Jul 1976 05:04:22 +0930 (foo)", SysTime(dst1, cstStd));
        test("Sun,4  \r\n  Jul  \r\n  1976  \r\n  05:04  \r\n  +0930  \r\n  (foo)", SysTime(dst2, cstStd));
        test("Sun,4  \r\n  Jul  \r\n  1976  \r\n  05:04:22  \r\n  +0930  \r\n  (foo)", SysTime(dst1, cstStd));

        auto str = "01 Jan 2012 12:13:14 -0800 ";
        test(str, SysTime(DateTime(2012, 1, 1, 12, 13, 14), new immutable SimpleTimeZone(hours(-8))));
        foreach (i; 0 .. str.length)
        {
            auto currStr = str.dup;
            currStr[i] = 'x';
            scope(failure) writefln("failed: %s", currStr);
            testBad(cast(string) currStr);
        }
        foreach (i; 2 .. str.length)
        {
            auto currStr = str[0 .. $ - i];
            scope(failure) writefln("failed: %s", currStr);
            testBad(cast(string) currStr);
            testBad((cast(string) currStr) ~ "                                    ");
        }
    }();}

    static void testScope(scope ref string str) @safe
    {
        auto result = parseRFC822DateTime(str);
    }
}

// Obsolete Format per section 4.3 of RFC 5322.
@system unittest
{
    import std.algorithm.iteration : filter, map;
    import std.ascii : letters;
    import std.exception : collectExceptionMsg;
    import std.format : format;
    import std.meta : AliasSeq;
    import std.range : chain, iota;
    import std.stdio : writefln, writeln;
    import std.string : representation;

    auto std1 = SysTime(DateTime(2012, 12, 21, 13, 14, 15), UTC());
    auto std2 = SysTime(DateTime(2012, 12, 21, 13, 14, 0), UTC());
    auto std3 = SysTime(DateTime(1912, 12, 21, 13, 14, 15), UTC());
    auto std4 = SysTime(DateTime(1912, 12, 21, 13, 14, 0), UTC());
    auto dst1 = SysTime(DateTime(1976, 7, 4, 5, 4, 22), UTC());
    auto dst2 = SysTime(DateTime(1976, 7, 4, 5, 4, 0), UTC());
    auto tooLate1 = SysTime(Date(10_000, 1, 1), UTC());
    auto tooLate2 = SysTime(DateTime(12_007, 12, 31, 12, 22, 19), UTC());

    static foreach (cr; AliasSeq!(function(string a){return cast(char[]) a;},
                           function(string a){return cast(ubyte[]) a;},
                           function(string a){return a;},
                           function(string a){return map!(b => cast(char) b)(a.representation);}))
    {(){ // workaround slow optimizations for large functions
         // https://issues.dlang.org/show_bug.cgi?id=2396
        scope(failure) writeln(typeof(cr).stringof);
        alias test = testParse822!cr;
        {
            auto list = ["", " ", " \r\n\t", "\t\r\n (hello world( frien(dog)) silly \r\n )  \t\t \r\n ()",
                         " \n ", "\t\n\t", " \n\t (foo) \n (bar) \r\n (baz) \n "];

            foreach (i, cfws; list)
            {
                scope(failure) writefln("i: %s", i);

                test(format("%1$s21%1$sDec%1$s2012%1$s13:14:15%1$s+0000%1$s", cfws), std1);
                test(format("%1$s21%1$sDec%1$s2012%1$s13:14%1$s+0000%1$s", cfws), std2);
                test(format("%1$sFri%1$s,%1$s21%1$sDec%1$s2012%1$s13:14%1$s+0000%1$s", cfws), std2);
                test(format("%1$sFri%1$s,%1$s21%1$sDec%1$s2012%1$s13:14:15%1$s+0000%1$s", cfws), std1);

                test(format("%1$s04%1$sJul%1$s1976%1$s05:04:22%1$s+0000%1$s", cfws), dst1);
                test(format("%1$s04%1$sJul%1$s1976%1$s05:04%1$s+0000%1$s", cfws), dst2);
                test(format("%1$sSun%1$s,%1$s04%1$sJul%1$s1976%1$s05:04%1$s+0000%1$s", cfws), dst2);
                test(format("%1$sSun%1$s,%1$s04%1$sJul%1$s1976%1$s05:04:22 +0000%1$s", cfws), dst1);

                test(format("%1$s4%1$sJul%1$s1976%1$s05:04:22%1$s+0000%1$s", cfws), dst1);
                test(format("%1$s4%1$sJul%1$s1976%1$s05:04%1$s+0000%1$s", cfws), dst2);
                test(format("%1$sSun%1$s,%1$s4%1$sJul%1$s1976%1$s05:04%1$s+0000%1$s", cfws), dst2);
                test(format("%1$sSun%1$s,%1$s4%1$sJul%1$s1976%1$s05:04:22%1$s+0000%1$s", cfws), dst1);

                test(format("%1$s21%1$sDec%1$s12%1$s13:14:15%1$s+0000%1$s", cfws), std1);
                test(format("%1$s21%1$sDec%1$s12%1$s13:14%1$s+0000%1$s", cfws), std2);
                test(format("%1$sFri%1$s,%1$s21%1$sDec%1$s12%1$s13:14%1$s+0000%1$s", cfws), std2);
                test(format("%1$sFri%1$s,%1$s21%1$sDec%1$s12%1$s13:14:15%1$s+0000%1$s", cfws), std1);

                test(format("%1$s04%1$sJul%1$s76%1$s05:04:22%1$s+0000%1$s", cfws), dst1);
                test(format("%1$s04%1$sJul%1$s76%1$s05:04%1$s+0000%1$s", cfws), dst2);
                test(format("%1$sSun%1$s,%1$s04%1$sJul%1$s76%1$s05:04%1$s+0000%1$s", cfws), dst2);
                test(format("%1$sSun%1$s,%1$s04%1$sJul%1$s76%1$s05:04:22%1$s+0000%1$s", cfws), dst1);

                test(format("%1$s4%1$sJul%1$s76 05:04:22%1$s+0000%1$s", cfws), dst1);
                test(format("%1$s4%1$sJul%1$s76 05:04%1$s+0000%1$s", cfws), dst2);
                test(format("%1$sSun%1$s,%1$s4%1$sJul%1$s76%1$s05:04%1$s+0000%1$s", cfws), dst2);
                test(format("%1$sSun%1$s,%1$s4%1$sJul%1$s76%1$s05:04:22%1$s+0000%1$s", cfws), dst1);

                test(format("%1$s21%1$sDec%1$s012%1$s13:14:15%1$s+0000%1$s", cfws), std3);
                test(format("%1$s21%1$sDec%1$s012%1$s13:14%1$s+0000%1$s", cfws), std4);
                test(format("%1$sFri%1$s,%1$s21%1$sDec%1$s012%1$s13:14%1$s+0000%1$s", cfws), std4);
                test(format("%1$sFri%1$s,%1$s21%1$sDec%1$s012%1$s13:14:15%1$s+0000%1$s", cfws), std3);

                test(format("%1$s04%1$sJul%1$s076%1$s05:04:22%1$s+0000%1$s", cfws), dst1);
                test(format("%1$s04%1$sJul%1$s076%1$s05:04%1$s+0000%1$s", cfws), dst2);
                test(format("%1$sSun%1$s,%1$s04%1$sJul%1$s076%1$s05:04%1$s+0000%1$s", cfws), dst2);
                test(format("%1$sSun%1$s,%1$s04%1$sJul%1$s076%1$s05:04:22%1$s+0000%1$s", cfws), dst1);

                test(format("%1$s4%1$sJul%1$s076%1$s05:04:22%1$s+0000%1$s", cfws), dst1);
                test(format("%1$s4%1$sJul%1$s076%1$s05:04%1$s+0000%1$s", cfws), dst2);
                test(format("%1$sSun%1$s,%1$s4%1$sJul%1$s076%1$s05:04%1$s+0000%1$s", cfws), dst2);
                test(format("%1$sSun%1$s,%1$s4%1$sJul%1$s076%1$s05:04:22%1$s+0000%1$s", cfws), dst1);

                test(format("%1$s1%1$sJan%1$s10000%1$s00:00:00%1$s+0000%1$s", cfws), tooLate1);
                test(format("%1$s31%1$sDec%1$s12007%1$s12:22:19%1$s+0000%1$s", cfws), tooLate2);
                test(format("%1$sSat%1$s,%1$s1%1$sJan%1$s10000%1$s00:00:00%1$s+0000%1$s", cfws), tooLate1);
                test(format("%1$sSun%1$s,%1$s31%1$sDec%1$s12007%1$s12:22:19%1$s+0000%1$s", cfws), tooLate2);
            }
        }

        // test years of 1, 2, and 3 digits.
        {
            auto st1 = SysTime(Date(2000, 1, 1), UTC());
            auto st2 = SysTime(Date(2000, 1, 1), new immutable SimpleTimeZone(dur!"hours"(-12)));
            foreach (i; 0 .. 50)
            {
                test(format("1 Jan %02d 00:00 GMT", i), st1);
                test(format("1 Jan %02d 00:00 -1200", i), st2);
                st1.add!"years"(1);
                st2.add!"years"(1);
            }
        }

        {
            auto st1 = SysTime(Date(1950, 1, 1), UTC());
            auto st2 = SysTime(Date(1950, 1, 1), new immutable SimpleTimeZone(dur!"hours"(-12)));
            foreach (i; 50 .. 100)
            {
                test(format("1 Jan %02d 00:00 GMT", i), st1);
                test(format("1 Jan %02d 00:00 -1200", i), st2);
                st1.add!"years"(1);
                st2.add!"years"(1);
            }
        }

        {
            auto st1 = SysTime(Date(1900, 1, 1), UTC());
            auto st2 = SysTime(Date(1900, 1, 1), new immutable SimpleTimeZone(dur!"hours"(-11)));
            foreach (i; 0 .. 1000)
            {
                test(format("1 Jan %03d 00:00 GMT", i), st1);
                test(format("1 Jan %03d 00:00 -1100", i), st2);
                st1.add!"years"(1);
                st2.add!"years"(1);
            }
        }

        foreach (i; 0 .. 10)
        {
            auto str1 = cr(format("1 Jan %d 00:00 GMT", i));
            auto str2 = cr(format("1 Jan %d 00:00 -1200", i));
            assertThrown!DateTimeException(parseRFC822DateTime(str1));
            assertThrown!DateTimeException(parseRFC822DateTime(str1));
        }

        // test time zones
        {
            auto dt = DateTime(1982, 5, 3, 12, 22, 4);
            test("Wed, 03 May 1982 12:22:04 UT", SysTime(dt, UTC()));
            test("Wed, 03 May 1982 12:22:04 GMT", SysTime(dt, UTC()));
            test("Wed, 03 May 1982 12:22:04 EST", SysTime(dt, new immutable SimpleTimeZone(dur!"hours"(-5))));
            test("Wed, 03 May 1982 12:22:04 EDT", SysTime(dt, new immutable SimpleTimeZone(dur!"hours"(-4))));
            test("Wed, 03 May 1982 12:22:04 CST", SysTime(dt, new immutable SimpleTimeZone(dur!"hours"(-6))));
            test("Wed, 03 May 1982 12:22:04 CDT", SysTime(dt, new immutable SimpleTimeZone(dur!"hours"(-5))));
            test("Wed, 03 May 1982 12:22:04 MST", SysTime(dt, new immutable SimpleTimeZone(dur!"hours"(-7))));
            test("Wed, 03 May 1982 12:22:04 MDT", SysTime(dt, new immutable SimpleTimeZone(dur!"hours"(-6))));
            test("Wed, 03 May 1982 12:22:04 PST", SysTime(dt, new immutable SimpleTimeZone(dur!"hours"(-8))));
            test("Wed, 03 May 1982 12:22:04 PDT", SysTime(dt, new immutable SimpleTimeZone(dur!"hours"(-7))));

            auto badTZ = new immutable SimpleTimeZone(Duration.zero);
            foreach (dchar c; filter!(a => a != 'j' && a != 'J')(letters))
            {
                scope(failure) writefln("c: %s", c);
                test(format("Wed, 03 May 1982 12:22:04 %s", c), SysTime(dt, badTZ));
                test(format("Wed, 03 May 1982 12:22:04%s", c), SysTime(dt, badTZ));
            }

            foreach (dchar c; ['j', 'J'])
            {
                scope(failure) writefln("c: %s", c);
                assertThrown!DateTimeException(parseRFC822DateTime(cr(format("Wed, 03 May 1982 12:22:04 %s", c))));
                assertThrown!DateTimeException(parseRFC822DateTime(cr(format("Wed, 03 May 1982 12:22:04%s", c))));
            }

            foreach (string s; ["AAA", "GQW", "DDT", "PDA", "GT", "GM"])
            {
                scope(failure) writefln("s: %s", s);
                test(format("Wed, 03 May 1982 12:22:04 %s", s), SysTime(dt, badTZ));
            }

            // test trailing stuff that gets ignored
            {
                foreach (c; chain(iota(0, 33), ['('], iota(127, ubyte.max + 1)))
                {
                    scope(failure) writefln("c: %d", c);
                    test(format("21Dec1213:14:15+0000%c", cast(char) c), std1);
                    test(format("21Dec1213:14:15+0000%c  ", cast(char) c), std1);
                    test(format("21Dec1213:14:15+0000%chello", cast(char) c), std1);
                }
            }

            // test trailing stuff that doesn't get ignored
            {
                foreach (c; chain(iota(33, '('), iota('(' + 1, 127)))
                {
                    scope(failure) writefln("c: %d", c);
                    assertThrown!DateTimeException(
                        parseRFC822DateTime(cr(format("21Dec1213:14:15+0000%c", cast(char) c))));
                    assertThrown!DateTimeException(
                        parseRFC822DateTime(cr(format("21Dec1213:14:15+0000%c  ", cast(char) c))));
                    assertThrown!DateTimeException(
                        parseRFC822DateTime(cr(format("21Dec1213:14:15+0000%chello", cast(char) c))));
                }
            }
        }

        // test that the checks for minimum length work correctly and avoid
        // any RangeErrors.
        test("7Dec1200:00A", SysTime(DateTime(2012, 12, 7, 0, 0, 0),
                                     new immutable SimpleTimeZone(Duration.zero)));
        test("Fri,7Dec1200:00A", SysTime(DateTime(2012, 12, 7, 0, 0, 0),
                                         new immutable SimpleTimeZone(Duration.zero)));
        test("7Dec1200:00:00A", SysTime(DateTime(2012, 12, 7, 0, 0, 0),
                                        new immutable SimpleTimeZone(Duration.zero)));
        test("Fri,7Dec1200:00:00A", SysTime(DateTime(2012, 12, 7, 0, 0, 0),
                                            new immutable SimpleTimeZone(Duration.zero)));

        auto tooShortMsg = collectExceptionMsg!DateTimeException(parseRFC822DateTime(""));
        foreach (str; ["Fri,7Dec1200:00:00", "7Dec1200:00:00"])
        {
            foreach (i; 0 .. str.length)
            {
                auto value = str[0 .. $ - i];
                scope(failure) writeln(value);
                assert(collectExceptionMsg!DateTimeException(parseRFC822DateTime(value)) == tooShortMsg);
            }
        }
    }();}
}


private:

/+
    Returns the given hnsecs as an ISO string of fractional seconds.
  +/
string fracSecsToISOString(int hnsecs, int prec = -1) @safe pure nothrow
{
    import std.array : appender;
    auto w = appender!string();
    try
        fracSecsToISOString(w, hnsecs, prec);
    catch (Exception e)
        assert(0, "fracSecsToISOString() threw.");
    return w.data;
}

void fracSecsToISOString(W)(ref W writer, int hnsecs, int prec = -1)
{
    import std.conv : toChars;
    import std.range : padLeft;

    assert(hnsecs >= 0);

    if (prec == 0)
        return;

    if (hnsecs == 0)
        return;

    put(writer, '.');
    auto chars = hnsecs.toChars.padLeft('0', 7);

    if (prec == -1)
    {
        while (chars.back == '0')
            chars.popBack();
        put(writer, chars);
    }
    else
        put(writer, chars[0 .. prec]);
}

@safe unittest
{
    assert(fracSecsToISOString(0) == "");
    assert(fracSecsToISOString(1) == ".0000001");
    assert(fracSecsToISOString(10) == ".000001");
    assert(fracSecsToISOString(100) == ".00001");
    assert(fracSecsToISOString(1000) == ".0001");
    assert(fracSecsToISOString(10_000) == ".001");
    assert(fracSecsToISOString(100_000) == ".01");
    assert(fracSecsToISOString(1_000_000) == ".1");
    assert(fracSecsToISOString(1_000_001) == ".1000001");
    assert(fracSecsToISOString(1_001_001) == ".1001001");
    assert(fracSecsToISOString(1_071_601) == ".1071601");
    assert(fracSecsToISOString(1_271_641) == ".1271641");
    assert(fracSecsToISOString(9_999_999) == ".9999999");
    assert(fracSecsToISOString(9_999_990) == ".999999");
    assert(fracSecsToISOString(9_999_900) == ".99999");
    assert(fracSecsToISOString(9_999_000) == ".9999");
    assert(fracSecsToISOString(9_990_000) == ".999");
    assert(fracSecsToISOString(9_900_000) == ".99");
    assert(fracSecsToISOString(9_000_000) == ".9");
    assert(fracSecsToISOString(999) == ".0000999");
    assert(fracSecsToISOString(9990) == ".000999");
    assert(fracSecsToISOString(99_900) == ".00999");
    assert(fracSecsToISOString(999_000) == ".0999");
}


/+
    Returns a Duration corresponding to to the given ISO string of
    fractional seconds.
  +/
static Duration fracSecsFromISOString(S)(scope const S isoString) @safe pure
if (isSomeString!S)
{
    import std.algorithm.searching : all;
    import std.ascii : isDigit;
    import std.conv : to;
    import std.format : format;
    import std.string : representation;

    if (isoString.empty)
        return Duration.zero;

    auto str = isoString.representation;

    enforce!DateTimeException(str[0] == '.', format("Invalid format for fracSecsFromISOString: %s", isoString));
    str.popFront();

    enforce!DateTimeException(!str.empty && all!isDigit(str),
                              format("Invalid format for fracSecsFromISOString: %s", isoString));

    dchar[7] fullISOString = void;
    foreach (i, ref dchar c; fullISOString)
    {
        if (i < str.length)
            c = str[i];
        else
            c = '0';
    }

    return hnsecs(to!int(fullISOString[]));
}

@safe unittest
{
    import core.time;
    static void testFSInvalid(string isoString)
    {
        fracSecsFromISOString(isoString);
    }

    assertThrown!DateTimeException(testFSInvalid("."));
    assertThrown!DateTimeException(testFSInvalid("0."));
    assertThrown!DateTimeException(testFSInvalid("0"));
    assertThrown!DateTimeException(testFSInvalid("0000000"));
    assertThrown!DateTimeException(testFSInvalid("T"));
    assertThrown!DateTimeException(testFSInvalid("T."));
    assertThrown!DateTimeException(testFSInvalid(".T"));
    assertThrown!DateTimeException(testFSInvalid(".00000Q0"));
    assertThrown!DateTimeException(testFSInvalid(".000000Q"));
    assertThrown!DateTimeException(testFSInvalid(".0000000Q"));
    assertThrown!DateTimeException(testFSInvalid(".0000000000Q"));

    assert(fracSecsFromISOString("") == Duration.zero);
    assert(fracSecsFromISOString(".0000001") == hnsecs(1));
    assert(fracSecsFromISOString(".000001") == hnsecs(10));
    assert(fracSecsFromISOString(".00001") == hnsecs(100));
    assert(fracSecsFromISOString(".0001") == hnsecs(1000));
    assert(fracSecsFromISOString(".001") == hnsecs(10_000));
    assert(fracSecsFromISOString(".01") == hnsecs(100_000));
    assert(fracSecsFromISOString(".1") == hnsecs(1_000_000));
    assert(fracSecsFromISOString(".1000001") == hnsecs(1_000_001));
    assert(fracSecsFromISOString(".1001001") == hnsecs(1_001_001));
    assert(fracSecsFromISOString(".1071601") == hnsecs(1_071_601));
    assert(fracSecsFromISOString(".1271641") == hnsecs(1_271_641));
    assert(fracSecsFromISOString(".9999999") == hnsecs(9_999_999));
    assert(fracSecsFromISOString(".9999990") == hnsecs(9_999_990));
    assert(fracSecsFromISOString(".999999") == hnsecs(9_999_990));
    assert(fracSecsFromISOString(".9999900") == hnsecs(9_999_900));
    assert(fracSecsFromISOString(".99999") == hnsecs(9_999_900));
    assert(fracSecsFromISOString(".9999000") == hnsecs(9_999_000));
    assert(fracSecsFromISOString(".9999") == hnsecs(9_999_000));
    assert(fracSecsFromISOString(".9990000") == hnsecs(9_990_000));
    assert(fracSecsFromISOString(".999") == hnsecs(9_990_000));
    assert(fracSecsFromISOString(".9900000") == hnsecs(9_900_000));
    assert(fracSecsFromISOString(".9900") == hnsecs(9_900_000));
    assert(fracSecsFromISOString(".99") == hnsecs(9_900_000));
    assert(fracSecsFromISOString(".9000000") == hnsecs(9_000_000));
    assert(fracSecsFromISOString(".9") == hnsecs(9_000_000));
    assert(fracSecsFromISOString(".0000999") == hnsecs(999));
    assert(fracSecsFromISOString(".0009990") == hnsecs(9990));
    assert(fracSecsFromISOString(".000999") == hnsecs(9990));
    assert(fracSecsFromISOString(".0099900") == hnsecs(99_900));
    assert(fracSecsFromISOString(".00999") == hnsecs(99_900));
    assert(fracSecsFromISOString(".0999000") == hnsecs(999_000));
    assert(fracSecsFromISOString(".0999") == hnsecs(999_000));
    assert(fracSecsFromISOString(".00000000") == Duration.zero);
    assert(fracSecsFromISOString(".00000001") == Duration.zero);
    assert(fracSecsFromISOString(".00000009") == Duration.zero);
    assert(fracSecsFromISOString(".1234567890") == hnsecs(1_234_567));
    assert(fracSecsFromISOString(".12345678901234567890") == hnsecs(1_234_567));
}


/+
    This function is used to split out the units without getting the remaining
    hnsecs.

    Params:
        units  = The units to split out.
        hnsecs = The current total hnsecs.

    Returns:
        The split out value.
  +/
long getUnitsFromHNSecs(string units)(long hnsecs) @safe pure nothrow
if (validTimeUnits(units) &&
    CmpTimeUnits!(units, "months") < 0)
{
    return convert!("hnsecs", units)(hnsecs);
}

@safe unittest
{
    auto hnsecs = 2595000000007L;
    immutable days = getUnitsFromHNSecs!"days"(hnsecs);
    assert(days == 3);
    assert(hnsecs == 2595000000007L);
}


/+
    This function is used to split out the units without getting the units but
    just the remaining hnsecs.

    Params:
        units  = The units to split out.
        hnsecs = The current total hnsecs.

    Returns:
        The remaining hnsecs.
  +/
long removeUnitsFromHNSecs(string units)(long hnsecs) @safe pure nothrow
if (validTimeUnits(units) &&
    CmpTimeUnits!(units, "months") < 0)
{
    immutable value = convert!("hnsecs", units)(hnsecs);
    return hnsecs - convert!(units, "hnsecs")(value);
}

@safe unittest
{
    auto hnsecs = 2595000000007L;
    auto returned = removeUnitsFromHNSecs!"days"(hnsecs);
    assert(returned == 3000000007);
    assert(hnsecs == 2595000000007L);
}


/+
    Strips what RFC 5322, section 3.2.2 refers to as CFWS from the left-hand
    side of the given range (it strips comments delimited by $(D '(') and
    `'`') as well as folding whitespace).

    It is assumed that the given range contains the value of a header field and
    no terminating CRLF for the line (though the CRLF for folding whitespace is
    of course expected and stripped) and thus that the only case of CR or LF is
    in folding whitespace.

    If a comment does not terminate correctly (e.g. mismatched parens) or if the
    the FWS is malformed, then the range will be empty when stripCWFS is done.
    However, only minimal validation of the content is done (e.g. quoted pairs
    within a comment aren't validated beyond \$LPAREN or \$RPAREN, because
    they're inside a comment, and thus their value doesn't matter anyway). It's
    only when the content does not conform to the grammar rules for FWS and thus
    literally cannot be parsed that content is considered invalid, and an empty
    range is returned.

    Note that _stripCFWS is eager, not lazy. It does not create a new range.
    Rather, it pops off the CFWS from the range and returns it.
  +/
R _stripCFWS(R)(R range)
if (isRandomAccessRange!R && hasSlicing!R && hasLength!R &&
    (is(immutable ElementType!R == immutable char) || is(immutable ElementType!R == immutable ubyte)))
{
    immutable e = range.length;
    outer: for (size_t i = 0; i < e; )
    {
        switch (range[i])
        {
            case ' ': case '\t':
            {
                ++i;
                break;
            }
            case '\r':
            {
                if (i + 2 < e && range[i + 1] == '\n' && (range[i + 2] == ' ' || range[i + 2] == '\t'))
                {
                    i += 3;
                    break;
                }
                break outer;
            }
            case '\n':
            {
                if (i + 1 < e && (range[i + 1] == ' ' || range[i + 1] == '\t'))
                {
                    i += 2;
                    break;
                }
                break outer;
            }
            case '(':
            {
                ++i;
                size_t commentLevel = 1;
                while (i < e)
                {
                    if (range[i] == '(')
                        ++commentLevel;
                    else if (range[i] == ')')
                    {
                        ++i;
                        if (--commentLevel == 0)
                            continue outer;
                        continue;
                    }
                    else if (range[i] == '\\')
                    {
                        if (++i == e)
                            break outer;
                    }
                    ++i;
                }
                break outer;
            }
            default: return range[i .. e];
        }
    }
    return range[e .. e];
}

@system unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map;
    import std.meta : AliasSeq;
    import std.stdio : writeln;
    import std.string : representation;

    static foreach (cr; AliasSeq!(function(string a){return cast(ubyte[]) a;},
                           function(string a){return map!(b => cast(char) b)(a.representation);}))
    {
        scope(failure) writeln(typeof(cr).stringof);

        assert(_stripCFWS(cr("")).empty);
        assert(_stripCFWS(cr("\r")).empty);
        assert(_stripCFWS(cr("\r\n")).empty);
        assert(_stripCFWS(cr("\r\n ")).empty);
        assert(_stripCFWS(cr(" \t\r\n")).empty);
        assert(equal(_stripCFWS(cr(" \t\r\n hello")), cr("hello")));
        assert(_stripCFWS(cr(" \t\r\nhello")).empty);
        assert(_stripCFWS(cr(" \t\r\n\v")).empty);
        assert(equal(_stripCFWS(cr("\v \t\r\n\v")), cr("\v \t\r\n\v")));
        assert(_stripCFWS(cr("()")).empty);
        assert(_stripCFWS(cr("(hello world)")).empty);
        assert(_stripCFWS(cr("(hello world)(hello world)")).empty);
        assert(_stripCFWS(cr("(hello world\r\n foo\r where's\nwaldo)")).empty);
        assert(_stripCFWS(cr(" \t (hello \tworld\r\n foo\r where's\nwaldo)\t\t ")).empty);
        assert(_stripCFWS(cr("      ")).empty);
        assert(_stripCFWS(cr("\t\t\t")).empty);
        assert(_stripCFWS(cr("\t \r\n\r \n")).empty);
        assert(_stripCFWS(cr("(hello world) (can't find waldo) (he's lost)")).empty);
        assert(_stripCFWS(cr("(hello\\) world) (can't \\(find waldo) (he's \\(\\)lost)")).empty);
        assert(_stripCFWS(cr("(((((")).empty);
        assert(_stripCFWS(cr("(((()))")).empty);
        assert(_stripCFWS(cr("(((())))")).empty);
        assert(equal(_stripCFWS(cr("(((()))))")), cr(")")));
        assert(equal(_stripCFWS(cr(")))))")), cr(")))))")));
        assert(equal(_stripCFWS(cr("()))))")), cr("))))")));
        assert(equal(_stripCFWS(cr(" hello hello ")), cr("hello hello ")));
        assert(equal(_stripCFWS(cr("\thello (world)")), cr("hello (world)")));
        assert(equal(_stripCFWS(cr(" \r\n \\((\\))  foo")), cr("\\((\\))  foo")));
        assert(equal(_stripCFWS(cr(" \r\n (\\((\\)))  foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" \r\n (\\(()))  foo")), cr(")  foo")));
        assert(_stripCFWS(cr(" \r\n (((\\)))  foo")).empty);

        assert(_stripCFWS(cr("(hello)(hello)")).empty);
        assert(_stripCFWS(cr(" \r\n (hello)\r\n (hello)")).empty);
        assert(_stripCFWS(cr(" \r\n (hello) \r\n (hello) \r\n ")).empty);
        assert(_stripCFWS(cr("\t\t\t\t(hello)\t\t\t\t(hello)\t\t\t\t")).empty);
        assert(equal(_stripCFWS(cr(" \r\n (hello)\r\n (hello) \r\n hello")), cr("hello")));
        assert(equal(_stripCFWS(cr(" \r\n (hello) \r\n (hello) \r\n hello")), cr("hello")));
        assert(equal(_stripCFWS(cr("\t\r\n\t(hello)\r\n\t(hello)\t\r\n hello")), cr("hello")));
        assert(equal(_stripCFWS(cr("\t\r\n\t(hello)\t\r\n\t(hello)\t\r\n hello")), cr("hello")));
        assert(equal(_stripCFWS(cr(" \r\n (hello) \r\n \r\n (hello) \r\n hello")), cr("hello")));
        assert(equal(_stripCFWS(cr(" \r\n (hello) \r\n (hello) \r\n \r\n hello")), cr("hello")));
        assert(equal(_stripCFWS(cr(" \r\n \r\n (hello)\t\r\n (hello) \r\n hello")), cr("hello")));
        assert(equal(_stripCFWS(cr(" \r\n\t\r\n\t(hello)\t\r\n (hello) \r\n hello")), cr("hello")));

        assert(equal(_stripCFWS(cr(" (\r\n ( \r\n ) \r\n ) foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" ( \r\n ( \r\n ) \r\n ) foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" (\t\r\n ( \r\n ) \r\n ) foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" (\r\n\t( \r\n ) \r\n ) foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" ( \r\n (\t\r\n ) \r\n ) foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" ( \r\n (\r\n ) \r\n ) foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" ( \r\n (\r\n\t) \r\n ) foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" ( \r\n ( \r\n) \r\n ) foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" ( \r\n ( \r\n )\t\r\n ) foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" ( \r\n ( \r\n )\r\n ) foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" ( \r\n ( \r\n ) \r\n) foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" ( \r\n ( \r\n ) \r\n\t) foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" ( \r\n ( \r\n ) \r\n ) \r\n foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" ( \r\n ( \r\n ) \r\n )\t\r\n foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" ( \r\n ( \r\n ) \r\n )\r\n foo")), cr("foo")));

        assert(equal(_stripCFWS(cr(" ( \r\n \r\n ( \r\n \r\n ) \r\n ) foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" ( \r\n \r\n ( \r\n \r\n ) \r\n ) foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" (\t\r\n \r\n ( \r\n \r\n ) \r\n ) foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" (\r\n \r\n\t( \r\n \r\n ) \r\n ) foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" (\r\n \r\n( \r\n \r\n ) \r\n ) foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" (\r\n \r\n ( \r\n \r\n\t) \r\n ) foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" (\r\n \r\n ( \r\n \r\n )\t\r\n ) foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" (\r\n \r\n ( \r\n \r\n )\r\n ) foo")), cr("foo")));

        assert(equal(_stripCFWS(cr(" ( \r\n bar \r\n ( \r\n bar \r\n ) \r\n ) foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" ( \r\n () \r\n ( \r\n () \r\n ) \r\n ) foo")), cr("foo")));
        assert(equal(_stripCFWS(cr(" ( \r\n \\\\ \r\n ( \r\n \\\\ \r\n ) \r\n ) foo")), cr("foo")));

        assert(_stripCFWS(cr("(hello)(hello)")).empty);
        assert(_stripCFWS(cr(" \n (hello)\n (hello) \n ")).empty);
        assert(_stripCFWS(cr(" \n (hello) \n (hello) \n ")).empty);
        assert(equal(_stripCFWS(cr(" \n (hello)\n (hello) \n hello")), cr("hello")));
        assert(equal(_stripCFWS(cr(" \n (hello) \n (hello) \n hello")), cr("hello")));
        assert(equal(_stripCFWS(cr("\t\n\t(hello)\n\t(hello)\t\n hello")), cr("hello")));
        assert(equal(_stripCFWS(cr("\t\n\t(hello)\t\n\t(hello)\t\n hello")), cr("hello")));
        assert(equal(_stripCFWS(cr(" \n (hello) \n \n (hello) \n hello")), cr("hello")));
        assert(equal(_stripCFWS(cr(" \n (hello) \n (hello) \n \n hello")), cr("hello")));
        assert(equal(_stripCFWS(cr(" \n \n (hello)\t\n (hello) \n hello")), cr("hello")));
        assert(equal(_stripCFWS(cr(" \n\t\n\t(hello)\t\n (hello) \n hello")), cr("hello")));
    }
}

// This is so that we don't have to worry about std.conv.to throwing. It also
// doesn't have to worry about quite as many cases as std.conv.to, since it
// doesn't have to worry about a sign on the value or about whether it fits.
T _convDigits(T, R)(R str)
if (isIntegral!T && isSigned!T) // The constraints on R were already covered by parseRFC822DateTime.
{
    import std.ascii : isDigit;

    assert(!str.empty);
    T num = 0;
    foreach (i; 0 .. str.length)
    {
        if (i != 0)
            num *= 10;
        if (!isDigit(str[i]))
            return -1;
        num += str[i] - '0';
    }
    return num;
}

@safe unittest
{
    import std.conv : to;
    import std.range : chain, iota;
    foreach (i; chain(iota(0, 101), [250, 999, 1000, 1001, 2345, 9999]))
    {
        assert(_convDigits!int(to!string(i)) == i, i.to!string);
    }
    foreach (str; ["-42", "+42", "1a", "1 ", " ", " 42 "])
    {
        assert(_convDigits!int(str) == -1, str);
    }
}


// NOTE: all the non-simple array literals are wrapped in functions, because
// otherwise importing causes re-evaluation of the static initializers using
// CTFE with unittests enabled
version (StdUnittest)
{
private @safe:
    // Variables to help in testing.
    Duration currLocalDiffFromUTC;
    immutable (TimeZone)[] testTZs;

    // All of these helper arrays are sorted in ascending order.
    auto testYearsBC = [-1999, -1200, -600, -4, -1, 0];
    auto testYearsAD = [1, 4, 1000, 1999, 2000, 2012];

    // I'd use a Tuple, but I get forward reference errors if I try.
    struct MonthDay
    {
        Month month;
        short day;

        this(int m, short d)
        {
            month = cast(Month) m;
            day = d;
        }
    }

    MonthDay[] testMonthDays()
    {
       static result = [MonthDay(1, 1),
                                MonthDay(1, 2),
                                MonthDay(3, 17),
                                MonthDay(7, 4),
                                MonthDay(10, 27),
                                MonthDay(12, 30),
                                MonthDay(12, 31)];
       return result;
    }

    auto testDays = [1, 2, 9, 10, 16, 20, 25, 28, 29, 30, 31];

    TimeOfDay[] testTODs()
    {
       static result = [TimeOfDay(0, 0, 0),
                     TimeOfDay(0, 0, 1),
                     TimeOfDay(0, 1, 0),
                     TimeOfDay(1, 0, 0),
                     TimeOfDay(13, 13, 13),
                     TimeOfDay(23, 59, 59)];
       return result;
    }

    auto testHours = [0, 1, 12, 22, 23];
    auto testMinSecs = [0, 1, 30, 58, 59];

    // Throwing exceptions is incredibly expensive, so we want to use a smaller
    // set of values for tests using assertThrown.
    TimeOfDay[] testTODsThrown()
    {
       static result = [TimeOfDay(0, 0, 0),
                           TimeOfDay(13, 13, 13),
                           TimeOfDay(23, 59, 59)];
       return result;
    }

    Date[] testDatesBC;
    Date[] testDatesAD;

    DateTime[] testDateTimesBC;
    DateTime[] testDateTimesAD;

    Duration[] testFracSecs;

    SysTime[] testSysTimesBC;
    SysTime[] testSysTimesAD;

    // I'd use a Tuple, but I get forward reference errors if I try.
    struct GregDay { int day; Date date; }
    GregDay[] testGregDaysBC()
    {
       static result = [GregDay(-1_373_427, Date(-3760, 9, 7)), // Start of the Hebrew Calendar
                           GregDay(-735_233, Date(-2012, 1, 1)),
                           GregDay(-735_202, Date(-2012, 2, 1)),
                           GregDay(-735_175, Date(-2012, 2, 28)),
                           GregDay(-735_174, Date(-2012, 2, 29)),
                           GregDay(-735_173, Date(-2012, 3, 1)),
                           GregDay(-734_502, Date(-2010, 1, 1)),
                           GregDay(-734_472, Date(-2010, 1, 31)),
                           GregDay(-734_471, Date(-2010, 2, 1)),
                           GregDay(-734_444, Date(-2010, 2, 28)),
                           GregDay(-734_443, Date(-2010, 3, 1)),
                           GregDay(-734_413, Date(-2010, 3, 31)),
                           GregDay(-734_412, Date(-2010, 4, 1)),
                           GregDay(-734_383, Date(-2010, 4, 30)),
                           GregDay(-734_382, Date(-2010, 5, 1)),
                           GregDay(-734_352, Date(-2010, 5, 31)),
                           GregDay(-734_351, Date(-2010, 6, 1)),
                           GregDay(-734_322, Date(-2010, 6, 30)),
                           GregDay(-734_321, Date(-2010, 7, 1)),
                           GregDay(-734_291, Date(-2010, 7, 31)),
                           GregDay(-734_290, Date(-2010, 8, 1)),
                           GregDay(-734_260, Date(-2010, 8, 31)),
                           GregDay(-734_259, Date(-2010, 9, 1)),
                           GregDay(-734_230, Date(-2010, 9, 30)),
                           GregDay(-734_229, Date(-2010, 10, 1)),
                           GregDay(-734_199, Date(-2010, 10, 31)),
                           GregDay(-734_198, Date(-2010, 11, 1)),
                           GregDay(-734_169, Date(-2010, 11, 30)),
                           GregDay(-734_168, Date(-2010, 12, 1)),
                           GregDay(-734_139, Date(-2010, 12, 30)),
                           GregDay(-734_138, Date(-2010, 12, 31)),
                           GregDay(-731_215, Date(-2001, 1, 1)),
                           GregDay(-730_850, Date(-2000, 1, 1)),
                           GregDay(-730_849, Date(-2000, 1, 2)),
                           GregDay(-730_486, Date(-2000, 12, 30)),
                           GregDay(-730_485, Date(-2000, 12, 31)),
                           GregDay(-730_484, Date(-1999, 1, 1)),
                           GregDay(-694_690, Date(-1901, 1, 1)),
                           GregDay(-694_325, Date(-1900, 1, 1)),
                           GregDay(-585_118, Date(-1601, 1, 1)),
                           GregDay(-584_753, Date(-1600, 1, 1)),
                           GregDay(-584_388, Date(-1600, 12, 31)),
                           GregDay(-584_387, Date(-1599, 1, 1)),
                           GregDay(-365_972, Date(-1001, 1, 1)),
                           GregDay(-365_607, Date(-1000, 1, 1)),
                           GregDay(-183_351, Date(-501, 1, 1)),
                           GregDay(-182_986, Date(-500, 1, 1)),
                           GregDay(-182_621, Date(-499, 1, 1)),
                           GregDay(-146_827, Date(-401, 1, 1)),
                           GregDay(-146_462, Date(-400, 1, 1)),
                           GregDay(-146_097, Date(-400, 12, 31)),
                           GregDay(-110_302, Date(-301, 1, 1)),
                           GregDay(-109_937, Date(-300, 1, 1)),
                           GregDay(-73_778, Date(-201, 1, 1)),
                           GregDay(-73_413, Date(-200, 1, 1)),
                           GregDay(-38_715, Date(-105, 1, 1)),
                           GregDay(-37_254, Date(-101, 1, 1)),
                           GregDay(-36_889, Date(-100, 1, 1)),
                           GregDay(-36_524, Date(-99, 1, 1)),
                           GregDay(-36_160, Date(-99, 12, 31)),
                           GregDay(-35_794, Date(-97, 1, 1)),
                           GregDay(-18_627, Date(-50, 1, 1)),
                           GregDay(-18_262, Date(-49, 1, 1)),
                           GregDay(-3652, Date(-9, 1, 1)),
                           GregDay(-2191, Date(-5, 1, 1)),
                           GregDay(-1827, Date(-5, 12, 31)),
                           GregDay(-1826, Date(-4, 1, 1)),
                           GregDay(-1825, Date(-4, 1, 2)),
                           GregDay(-1462, Date(-4, 12, 30)),
                           GregDay(-1461, Date(-4, 12, 31)),
                           GregDay(-1460, Date(-3, 1, 1)),
                           GregDay(-1096, Date(-3, 12, 31)),
                           GregDay(-1095, Date(-2, 1, 1)),
                           GregDay(-731, Date(-2, 12, 31)),
                           GregDay(-730, Date(-1, 1, 1)),
                           GregDay(-367, Date(-1, 12, 30)),
                           GregDay(-366, Date(-1, 12, 31)),
                           GregDay(-365, Date(0, 1, 1)),
                           GregDay(-31, Date(0, 11, 30)),
                           GregDay(-30, Date(0, 12, 1)),
                           GregDay(-1, Date(0, 12, 30)),
                           GregDay(0, Date(0, 12, 31))];
       return result;
    }

    GregDay[] testGregDaysAD()
    {
       static result = [GregDay(1, Date(1, 1, 1)),
                           GregDay(2, Date(1, 1, 2)),
                           GregDay(32, Date(1, 2, 1)),
                           GregDay(365, Date(1, 12, 31)),
                           GregDay(366, Date(2, 1, 1)),
                           GregDay(731, Date(3, 1, 1)),
                           GregDay(1096, Date(4, 1, 1)),
                           GregDay(1097, Date(4, 1, 2)),
                           GregDay(1460, Date(4, 12, 30)),
                           GregDay(1461, Date(4, 12, 31)),
                           GregDay(1462, Date(5, 1, 1)),
                           GregDay(17_898, Date(50, 1, 1)),
                           GregDay(35_065, Date(97, 1, 1)),
                           GregDay(36_160, Date(100, 1, 1)),
                           GregDay(36_525, Date(101, 1, 1)),
                           GregDay(37_986, Date(105, 1, 1)),
                           GregDay(72_684, Date(200, 1, 1)),
                           GregDay(73_049, Date(201, 1, 1)),
                           GregDay(109_208, Date(300, 1, 1)),
                           GregDay(109_573, Date(301, 1, 1)),
                           GregDay(145_732, Date(400, 1, 1)),
                           GregDay(146_098, Date(401, 1, 1)),
                           GregDay(182_257, Date(500, 1, 1)),
                           GregDay(182_622, Date(501, 1, 1)),
                           GregDay(364_878, Date(1000, 1, 1)),
                           GregDay(365_243, Date(1001, 1, 1)),
                           GregDay(584_023, Date(1600, 1, 1)),
                           GregDay(584_389, Date(1601, 1, 1)),
                           GregDay(693_596, Date(1900, 1, 1)),
                           GregDay(693_961, Date(1901, 1, 1)),
                           GregDay(729_755, Date(1999, 1, 1)),
                           GregDay(730_120, Date(2000, 1, 1)),
                           GregDay(730_121, Date(2000, 1, 2)),
                           GregDay(730_484, Date(2000, 12, 30)),
                           GregDay(730_485, Date(2000, 12, 31)),
                           GregDay(730_486, Date(2001, 1, 1)),
                           GregDay(733_773, Date(2010, 1, 1)),
                           GregDay(733_774, Date(2010, 1, 2)),
                           GregDay(733_803, Date(2010, 1, 31)),
                           GregDay(733_804, Date(2010, 2, 1)),
                           GregDay(733_831, Date(2010, 2, 28)),
                           GregDay(733_832, Date(2010, 3, 1)),
                           GregDay(733_862, Date(2010, 3, 31)),
                           GregDay(733_863, Date(2010, 4, 1)),
                           GregDay(733_892, Date(2010, 4, 30)),
                           GregDay(733_893, Date(2010, 5, 1)),
                           GregDay(733_923, Date(2010, 5, 31)),
                           GregDay(733_924, Date(2010, 6, 1)),
                           GregDay(733_953, Date(2010, 6, 30)),
                           GregDay(733_954, Date(2010, 7, 1)),
                           GregDay(733_984, Date(2010, 7, 31)),
                           GregDay(733_985, Date(2010, 8, 1)),
                           GregDay(734_015, Date(2010, 8, 31)),
                           GregDay(734_016, Date(2010, 9, 1)),
                           GregDay(734_045, Date(2010, 9, 30)),
                           GregDay(734_046, Date(2010, 10, 1)),
                           GregDay(734_076, Date(2010, 10, 31)),
                           GregDay(734_077, Date(2010, 11, 1)),
                           GregDay(734_106, Date(2010, 11, 30)),
                           GregDay(734_107, Date(2010, 12, 1)),
                           GregDay(734_136, Date(2010, 12, 30)),
                           GregDay(734_137, Date(2010, 12, 31)),
                           GregDay(734_503, Date(2012, 1, 1)),
                           GregDay(734_534, Date(2012, 2, 1)),
                           GregDay(734_561, Date(2012, 2, 28)),
                           GregDay(734_562, Date(2012, 2, 29)),
                           GregDay(734_563, Date(2012, 3, 1)),
                           GregDay(734_858, Date(2012, 12, 21))];
       return result;
    }

    // I'd use a Tuple, but I get forward reference errors if I try.
    struct DayOfYear { int day; MonthDay md; }
    DayOfYear[] testDaysOfYear()
    {
       static result = [DayOfYear(1, MonthDay(1, 1)),
                           DayOfYear(2, MonthDay(1, 2)),
                           DayOfYear(3, MonthDay(1, 3)),
                           DayOfYear(31, MonthDay(1, 31)),
                           DayOfYear(32, MonthDay(2, 1)),
                           DayOfYear(59, MonthDay(2, 28)),
                           DayOfYear(60, MonthDay(3, 1)),
                           DayOfYear(90, MonthDay(3, 31)),
                           DayOfYear(91, MonthDay(4, 1)),
                           DayOfYear(120, MonthDay(4, 30)),
                           DayOfYear(121, MonthDay(5, 1)),
                           DayOfYear(151, MonthDay(5, 31)),
                           DayOfYear(152, MonthDay(6, 1)),
                           DayOfYear(181, MonthDay(6, 30)),
                           DayOfYear(182, MonthDay(7, 1)),
                           DayOfYear(212, MonthDay(7, 31)),
                           DayOfYear(213, MonthDay(8, 1)),
                           DayOfYear(243, MonthDay(8, 31)),
                           DayOfYear(244, MonthDay(9, 1)),
                           DayOfYear(273, MonthDay(9, 30)),
                           DayOfYear(274, MonthDay(10, 1)),
                           DayOfYear(304, MonthDay(10, 31)),
                           DayOfYear(305, MonthDay(11, 1)),
                           DayOfYear(334, MonthDay(11, 30)),
                           DayOfYear(335, MonthDay(12, 1)),
                           DayOfYear(363, MonthDay(12, 29)),
                           DayOfYear(364, MonthDay(12, 30)),
                           DayOfYear(365, MonthDay(12, 31))];
       return result;
    }

    DayOfYear[] testDaysOfLeapYear()
    {
       static result = [DayOfYear(1, MonthDay(1, 1)),
                               DayOfYear(2, MonthDay(1, 2)),
                               DayOfYear(3, MonthDay(1, 3)),
                               DayOfYear(31, MonthDay(1, 31)),
                               DayOfYear(32, MonthDay(2, 1)),
                               DayOfYear(59, MonthDay(2, 28)),
                               DayOfYear(60, MonthDay(2, 29)),
                               DayOfYear(61, MonthDay(3, 1)),
                               DayOfYear(91, MonthDay(3, 31)),
                               DayOfYear(92, MonthDay(4, 1)),
                               DayOfYear(121, MonthDay(4, 30)),
                               DayOfYear(122, MonthDay(5, 1)),
                               DayOfYear(152, MonthDay(5, 31)),
                               DayOfYear(153, MonthDay(6, 1)),
                               DayOfYear(182, MonthDay(6, 30)),
                               DayOfYear(183, MonthDay(7, 1)),
                               DayOfYear(213, MonthDay(7, 31)),
                               DayOfYear(214, MonthDay(8, 1)),
                               DayOfYear(244, MonthDay(8, 31)),
                               DayOfYear(245, MonthDay(9, 1)),
                               DayOfYear(274, MonthDay(9, 30)),
                               DayOfYear(275, MonthDay(10, 1)),
                               DayOfYear(305, MonthDay(10, 31)),
                               DayOfYear(306, MonthDay(11, 1)),
                               DayOfYear(335, MonthDay(11, 30)),
                               DayOfYear(336, MonthDay(12, 1)),
                               DayOfYear(364, MonthDay(12, 29)),
                               DayOfYear(365, MonthDay(12, 30)),
                               DayOfYear(366, MonthDay(12, 31))];
       return result;
    }

    void initializeTests()
    {
        import std.algorithm.sorting : sort;
        import std.typecons : Rebindable;
        immutable lt = LocalTime().utcToTZ(0);
        currLocalDiffFromUTC = dur!"hnsecs"(lt);

        version (Posix)
        {
            import std.datetime.timezone : PosixTimeZone;
            immutable otherTZ = lt < 0 ? PosixTimeZone.getTimeZone("Australia/Sydney")
                                       : PosixTimeZone.getTimeZone("America/Denver");
        }
        else version (Windows)
        {
            import std.datetime.timezone : WindowsTimeZone;
            immutable otherTZ = lt < 0 ? WindowsTimeZone.getTimeZone("AUS Eastern Standard Time")
                                       : WindowsTimeZone.getTimeZone("Mountain Standard Time");
        }

        immutable ot = otherTZ.utcToTZ(0);

        auto diffs = [0L, lt, ot];
        auto diffAA = [0L : Rebindable!(immutable TimeZone)(UTC())];
        diffAA[lt] = Rebindable!(immutable TimeZone)(LocalTime());
        diffAA[ot] = Rebindable!(immutable TimeZone)(otherTZ);

        sort(diffs);
        testTZs = [diffAA[diffs[0]], diffAA[diffs[1]], diffAA[diffs[2]]];

        testFracSecs = [Duration.zero, hnsecs(1), hnsecs(5007), hnsecs(9_999_999)];

        foreach (year; testYearsBC)
        {
            foreach (md; testMonthDays)
                testDatesBC ~= Date(year, md.month, md.day);
        }

        foreach (year; testYearsAD)
        {
            foreach (md; testMonthDays)
                testDatesAD ~= Date(year, md.month, md.day);
        }

        foreach (dt; testDatesBC)
        {
            foreach (tod; testTODs)
                testDateTimesBC ~= DateTime(dt, tod);
        }

        foreach (dt; testDatesAD)
        {
            foreach (tod; testTODs)
                testDateTimesAD ~= DateTime(dt, tod);
        }

        foreach (dt; testDateTimesBC)
        {
            foreach (tz; testTZs)
            {
                foreach (fs; testFracSecs)
                    testSysTimesBC ~= SysTime(dt, fs, tz);
            }
        }

        foreach (dt; testDateTimesAD)
        {
            foreach (tz; testTZs)
            {
                foreach (fs; testFracSecs)
                    testSysTimesAD ~= SysTime(dt, fs, tz);
            }
        }
    }
}

// Written in the D programming language

/++
    $(SCRIPT inhibitQuickIndex = 1;)

    Phobos provides the following functionality for time:

    $(DIVC quickindex,
    $(BOOKTABLE ,
    $(TR $(TH Functionality) $(TH Symbols)
    )
    $(TR
        $(TD Points in Time)
        $(TD
            $(REF_ALTTEXT Clock, Clock, std, datetime, systime)$(NBSP)
            $(REF_ALTTEXT Date, Date, std, datetime, date)$(NBSP)
            $(REF_ALTTEXT TimeOfDay, TimeOfDay, std, datetime, date)$(NBSP)
            $(REF_ALTTEXT DateTime, DateTime, std, datetime, date)$(NBSP)
            $(REF_ALTTEXT SysTime, SysTime, std, datetime, systime)$(NBSP)
        )
    )
    $(TR
        $(TD $(MREF_ALTTEXT Timezones, std, datetime, timezone))
        $(TD
            $(REF_ALTTEXT TimeZone, TimeZone, std, datetime, timezone)$(NBSP)
            $(REF_ALTTEXT UTC, UTC, std, datetime, timezone)$(NBSP)
            $(REF_ALTTEXT LocalTime, LocalTime, std, datetime, timezone)$(NBSP)
            $(REF_ALTTEXT PosixTimeZone, PosixTimeZone, std, datetime, timezone)$(NBSP)
            $(REF_ALTTEXT WindowsTimeZone, WindowsTimeZone, std, datetime, timezone)$(NBSP)
            $(REF_ALTTEXT SimpleTimeZone, SimpleTimeZone, std, datetime, timezone)$(NBSP)
        )
    )
    $(TR
        $(TD Intervals and Ranges of Time)
        $(TD
            $(REF_ALTTEXT Interval, Interval, std, datetime, interval)$(NBSP)
            $(REF_ALTTEXT PosInfInterval, PosInfInterval, std, datetime, interval)$(NBSP)
            $(REF_ALTTEXT NegInfInterval, NegInfInterval, std, datetime, interval)$(NBSP)
        )
    )
    $(TR
        $(TD $(MREF_ALTTEXT Durations of Time, core, time))
        $(TD
            $(REF_ALTTEXT Duration, Duration, core, time)$(NBSP)
            $(REF_ALTTEXT weeks, weeks, core, time)$(NBSP)
            $(REF_ALTTEXT days, days, core, time)$(NBSP)
            $(REF_ALTTEXT hours, hours, core, time)$(NBSP)
            $(REF_ALTTEXT minutes, minutes, core, time)$(NBSP)
            $(REF_ALTTEXT seconds, seconds, core, time)$(NBSP)
            $(REF_ALTTEXT msecs, msecs, core, time)$(NBSP)
            $(REF_ALTTEXT usecs, usecs, core, time)$(NBSP)
            $(REF_ALTTEXT hnsecs, hnsecs, core, time)$(NBSP)
            $(REF_ALTTEXT nsecs, nsecs, core, time)$(NBSP)
        )
    )
    $(TR
        $(TD Time Measurement and Benchmarking)
        $(TD
            $(REF_ALTTEXT MonoTime, MonoTime, core, time)$(NBSP)
            $(REF_ALTTEXT StopWatch, StopWatch, std, datetime, stopwatch)$(NBSP)
            $(REF_ALTTEXT benchmark, benchmark, std, datetime, stopwatch)$(NBSP)
        )
    )
    ))

    This functionality is separated into the following modules:

    $(UL
        $(LI $(MREF std, datetime, date) for points in time without timezones.)
        $(LI $(MREF std, datetime, timezone) for classes which represent timezones.)
        $(LI $(MREF std, datetime, systime) for a point in time with a timezone.)
        $(LI $(MREF std, datetime, interval) for types which represent series of points in time.)
        $(LI $(MREF std, datetime, stopwatch) for measuring time.)
    )

    See_Also:
        $(MREF core, time)$(BR)
        $(DDLINK intro-to-datetime, Introduction to std.datetime,
                 Introduction to std&#46;datetime)<br>
        $(HTTP en.wikipedia.org/wiki/ISO_8601, ISO 8601)<br>
        $(HTTP en.wikipedia.org/wiki/Tz_database,
              Wikipedia entry on TZ Database)<br>
        $(HTTP en.wikipedia.org/wiki/List_of_tz_database_time_zones,
              List of Time Zones)<br>

    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTP jmdavisprog.com, Jonathan M Davis) and Kato Shoichi
    Source:    $(PHOBOSSRC std/datetime/package.d)
+/
module std.datetime;

/// Get the current time from the system clock.
@safe unittest
{
    import std.datetime.systime : SysTime, Clock;

    SysTime currentTime = Clock.currTime();
}

/**
Construct a specific point in time without timezone information
and get its ISO string.
 */
@safe unittest
{
    import std.datetime.date : DateTime;

    auto dt = DateTime(2018, 1, 1, 12, 30, 10);
    assert(dt.toISOString() == "20180101T123010");
    assert(dt.toISOExtString() == "2018-01-01T12:30:10");
}

/**
Construct a specific point in time in the UTC timezone and
add two days.
 */
@safe unittest
{
    import std.datetime.systime : SysTime;
    import std.datetime.timezone : UTC;
    import core.time : days;

    auto st = SysTime(DateTime(2018, 1, 1, 12, 30, 10), UTC());
    assert(st.toISOExtString() == "2018-01-01T12:30:10Z");
    st += 2.days;
    assert(st.toISOExtString() == "2018-01-03T12:30:10Z");
}

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
    /* https://issues.dlang.org/show_bug.cgi?id=6642 */
    static assert(!hasUnsharedAliasing!Date);
    static assert(!hasUnsharedAliasing!TimeOfDay);
    static assert(!hasUnsharedAliasing!DateTime);
    static assert(!hasUnsharedAliasing!SysTime);
}

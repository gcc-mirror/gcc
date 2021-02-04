// Written in the D programming language

/++
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   Jonathan M Davis
    Source:    $(PHOBOSSRC std/datetime/_timezone.d)
+/
module std.datetime.timezone;

import core.time;
import std.datetime.date;
import std.datetime.systime;
import std.exception : enforce;
import std.range.primitives;
import std.traits : isIntegral, isSomeString, Unqual;

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
    import core.stdc.time : time_t;
    import core.sys.windows.windows;
    import core.sys.windows.winsock2;
    import std.windows.registry;

    // Uncomment and run unittests to print missing Windows TZ translations.
    // Please subscribe to Microsoft Daylight Saving Time & Time Zone Blog
    // (https://blogs.technet.microsoft.com/dst2007/) if you feel responsible
    // for updating the translations.
    // version = UpdateWindowsTZTranslations;
}
else version (Posix)
{
    import core.sys.posix.signal : timespec;
    import core.sys.posix.sys.types : time_t;
}

version (unittest) import std.exception : assertThrown;


/++
    Represents a time zone. It is used with $(REF SysTime,std,datetime,systime)
    to indicate the time zone of a $(REF SysTime,std,datetime,systime).
  +/
abstract class TimeZone
{
public:

    /++
        The name of the time zone per the TZ Database. This is the name used to
        get a $(LREF TimeZone) by name with $(D TimeZone.getTimeZone).

        See_Also:
            $(HTTP en.wikipedia.org/wiki/Tz_database, Wikipedia entry on TZ
              Database)<br>
            $(HTTP en.wikipedia.org/wiki/List_of_tz_database_time_zones, List of
              Time Zones)
      +/
    @property string name() @safe const nothrow
    {
        return _name;
    }


    /++
        Typically, the abbreviation (generally 3 or 4 letters) for the time zone
        when DST is $(I not) in effect (e.g. PST). It is not necessarily unique.

        However, on Windows, it may be the unabbreviated name (e.g. Pacific
        Standard Time). Regardless, it is not the same as name.
      +/
    @property string stdName() @safe const nothrow
    {
        return _stdName;
    }


    /++
        Typically, the abbreviation (generally 3 or 4 letters) for the time zone
        when DST $(I is) in effect (e.g. PDT). It is not necessarily unique.

        However, on Windows, it may be the unabbreviated name (e.g. Pacific
        Daylight Time). Regardless, it is not the same as name.
      +/
    @property string dstName() @safe const nothrow
    {
        return _dstName;
    }


    /++
        Whether this time zone has Daylight Savings Time at any point in time.
        Note that for some time zone types it may not have DST for current dates
        but will still return true for $(D hasDST) because the time zone did at
        some point have DST.
      +/
    @property abstract bool hasDST() @safe const nothrow;


    /++
        Takes the number of hnsecs (100 ns) since midnight, January 1st, 1 A.D.
        in UTC time (i.e. std time) and returns whether DST is effect in this
        time zone at the given point in time.

        Params:
            stdTime = The UTC time that needs to be checked for DST in this time
                      zone.
      +/
    abstract bool dstInEffect(long stdTime) @safe const nothrow;


    /++
        Takes the number of hnsecs (100 ns) since midnight, January 1st, 1 A.D.
        in UTC time (i.e. std time) and converts it to this time zone's time.

        Params:
            stdTime = The UTC time that needs to be adjusted to this time zone's
                      time.
      +/
    abstract long utcToTZ(long stdTime) @safe const nothrow;


    /++
        Takes the number of hnsecs (100 ns) since midnight, January 1st, 1 A.D.
        in this time zone's time and converts it to UTC (i.e. std time).

        Params:
            adjTime = The time in this time zone that needs to be adjusted to
                      UTC time.
      +/
    abstract long tzToUTC(long adjTime) @safe const nothrow;


    /++
        Returns what the offset from UTC is at the given std time.
        It includes the DST offset in effect at that time (if any).

        Params:
            stdTime = The UTC time for which to get the offset from UTC for this
                      time zone.
      +/
    Duration utcOffsetAt(long stdTime) @safe const nothrow
    {
        return dur!"hnsecs"(utcToTZ(stdTime) - stdTime);
    }

    // The purpose of this is to handle the case where a Windows time zone is
    // new and exists on an up-to-date Windows box but does not exist on Windows
    // boxes which have not been properly updated. The "date added" is included
    // on the theory that we'll be able to remove them at some point in the
    // the future once enough time has passed, and that way, we know how much
    // time has passed.
    private static string _getOldName(string windowsTZName) @safe pure nothrow
    {
        switch (windowsTZName)
        {
            case "Belarus Standard Time": return "Kaliningrad Standard Time"; // Added 2014-10-08
            case "Russia Time Zone 10": return "Magadan Standard Time"; // Added 2014-10-08
            case "Russia Time Zone 11": return "Magadan Standard Time"; // Added 2014-10-08
            case "Russia Time Zone 3": return "Russian Standard Time"; // Added 2014-10-08
            default: return null;
        }
    }

    // Since reading in the time zone files could be expensive, most unit tests
    // are consolidated into this one unittest block which minimizes how often
    // it reads a time zone file.
    @system unittest
    {
        import core.exception : AssertError;
        import std.conv : to;
        import std.file : exists, isFile;
        import std.format : format;
        import std.path : chainPath;
        import std.stdio : writefln;
        import std.typecons : tuple;

        version (Posix) alias getTimeZone = PosixTimeZone.getTimeZone;
        else version (Windows) alias getTimeZone = WindowsTimeZone.getTimeZone;

        version (Posix) scope(exit) clearTZEnvVar();

        static immutable(TimeZone) testTZ(string tzName,
                                          string stdName,
                                          string dstName,
                                          Duration utcOffset,
                                          Duration dstOffset,
                                          bool north = true)
        {
            scope(failure) writefln("Failed time zone: %s", tzName);

            version (Posix)
            {
                immutable tz = PosixTimeZone.getTimeZone(tzName);
                assert(tz.name == tzName);
            }
            else version (Windows)
            {
                immutable tz = WindowsTimeZone.getTimeZone(tzName);
                assert(tz.name == stdName);
            }

            immutable hasDST = dstOffset != Duration.zero;

            //assert(tz.stdName == stdName);  //Locale-dependent
            //assert(tz.dstName == dstName);  //Locale-dependent
            assert(tz.hasDST == hasDST);

            immutable stdDate = DateTime(2010, north ? 1 : 7, 1, 6, 0, 0);
            immutable dstDate = DateTime(2010, north ? 7 : 1, 1, 6, 0, 0);
            auto std = SysTime(stdDate, tz);
            auto dst = SysTime(dstDate, tz);
            auto stdUTC = SysTime(stdDate - utcOffset, UTC());
            auto dstUTC = SysTime(stdDate - utcOffset + dstOffset, UTC());

            assert(!std.dstInEffect);
            assert(dst.dstInEffect == hasDST);
            assert(tz.utcOffsetAt(std.stdTime) == utcOffset);
            assert(tz.utcOffsetAt(dst.stdTime) == utcOffset + dstOffset);

            assert(cast(DateTime) std == stdDate);
            assert(cast(DateTime) dst == dstDate);
            assert(std == stdUTC);

            version (Posix)
            {
                setTZEnvVar(tzName);

                static void testTM(in SysTime st)
                {
                    import core.stdc.time : localtime, tm;
                    time_t unixTime = st.toUnixTime();
                    tm* osTimeInfo = localtime(&unixTime);
                    tm ourTimeInfo = st.toTM();

                    assert(ourTimeInfo.tm_sec == osTimeInfo.tm_sec);
                    assert(ourTimeInfo.tm_min == osTimeInfo.tm_min);
                    assert(ourTimeInfo.tm_hour == osTimeInfo.tm_hour);
                    assert(ourTimeInfo.tm_mday == osTimeInfo.tm_mday);
                    assert(ourTimeInfo.tm_mon == osTimeInfo.tm_mon);
                    assert(ourTimeInfo.tm_year == osTimeInfo.tm_year);
                    assert(ourTimeInfo.tm_wday == osTimeInfo.tm_wday);
                    assert(ourTimeInfo.tm_yday == osTimeInfo.tm_yday);
                    assert(ourTimeInfo.tm_isdst == osTimeInfo.tm_isdst);
                    assert(ourTimeInfo.tm_gmtoff == osTimeInfo.tm_gmtoff);
                    assert(to!string(ourTimeInfo.tm_zone) == to!string(osTimeInfo.tm_zone));
                }

                testTM(std);
                testTM(dst);

                // Apparently, right/ does not exist on Mac OS X. I don't know
                // whether or not it exists on FreeBSD. It's rather pointless
                // normally, since the Posix standard requires that leap seconds
                // be ignored, so it does make some sense that right/ wouldn't
                // be there, but since PosixTimeZone _does_ use leap seconds if
                // the time zone file does, we'll test that functionality if the
                // appropriate files exist.
                if (chainPath(PosixTimeZone.defaultTZDatabaseDir, "right", tzName).exists)
                {
                    auto leapTZ = PosixTimeZone.getTimeZone("right/" ~ tzName);

                    assert(leapTZ.name == "right/" ~ tzName);
                    //assert(leapTZ.stdName == stdName);  //Locale-dependent
                    //assert(leapTZ.dstName == dstName);  //Locale-dependent
                    assert(leapTZ.hasDST == hasDST);

                    auto leapSTD = SysTime(std.stdTime, leapTZ);
                    auto leapDST = SysTime(dst.stdTime, leapTZ);

                    assert(!leapSTD.dstInEffect);
                    assert(leapDST.dstInEffect == hasDST);

                    assert(leapSTD.stdTime == std.stdTime);
                    assert(leapDST.stdTime == dst.stdTime);

                    // Whenever a leap second is added/removed,
                    // this will have to be adjusted.
                    //enum leapDiff = convert!("seconds", "hnsecs")(25);
                    //assert(leapSTD.adjTime - leapDiff == std.adjTime);
                    //assert(leapDST.adjTime - leapDiff == dst.adjTime);
                }
            }

            return tz;
        }

        auto dstSwitches = [/+America/Los_Angeles+/ tuple(DateTime(2012, 3, 11),  DateTime(2012, 11, 4), 2, 2),
                            /+America/New_York+/    tuple(DateTime(2012, 3, 11),  DateTime(2012, 11, 4), 2, 2),
                            ///+America/Santiago+/    tuple(DateTime(2011, 8, 21),  DateTime(2011, 5, 8), 0, 0),
                            /+Europe/London+/       tuple(DateTime(2012, 3, 25),  DateTime(2012, 10, 28), 1, 2),
                            /+Europe/Paris+/        tuple(DateTime(2012, 3, 25),  DateTime(2012, 10, 28), 2, 3),
                            /+Australia/Adelaide+/  tuple(DateTime(2012, 10, 7),  DateTime(2012, 4, 1), 2, 3)];

        version (Posix)
        {
            version (FreeBSD)            enum utcZone = "Etc/UTC";
            else version (NetBSD)        enum utcZone = "UTC";
            else version (DragonFlyBSD)  enum utcZone = "UTC";
            else version (linux)         enum utcZone = "UTC";
            else version (Darwin)        enum utcZone = "UTC";
            else version (Solaris)       enum utcZone = "UTC";
            else static assert(0, "The location of the UTC timezone file on this Posix platform must be set.");

            auto tzs = [testTZ("America/Los_Angeles", "PST", "PDT", dur!"hours"(-8), dur!"hours"(1)),
                        testTZ("America/New_York", "EST", "EDT", dur!"hours"(-5), dur!"hours"(1)),
                        //testTZ("America/Santiago", "CLT", "CLST", dur!"hours"(-4), dur!"hours"(1), false),
                        testTZ("Europe/London", "GMT", "BST", dur!"hours"(0), dur!"hours"(1)),
                        testTZ("Europe/Paris", "CET", "CEST", dur!"hours"(1), dur!"hours"(1)),
                        // Per www.timeanddate.com, it should be "CST" and "CDT",
                        // but the OS insists that it's "CST" for both. We should
                        // probably figure out how to report an error in the TZ
                        // database and report it.
                        testTZ("Australia/Adelaide", "CST", "CST",
                               dur!"hours"(9) + dur!"minutes"(30), dur!"hours"(1), false)];

            testTZ(utcZone, "UTC", "UTC", dur!"hours"(0), dur!"hours"(0));
            assertThrown!DateTimeException(PosixTimeZone.getTimeZone("hello_world"));
        }
        else version (Windows)
        {
            auto tzs = [testTZ("Pacific Standard Time", "Pacific Standard Time",
                               "Pacific Daylight Time", dur!"hours"(-8), dur!"hours"(1)),
                        testTZ("Eastern Standard Time", "Eastern Standard Time",
                               "Eastern Daylight Time", dur!"hours"(-5), dur!"hours"(1)),
                        //testTZ("Pacific SA Standard Time", "Pacific SA Standard Time",
                               //"Pacific SA Daylight Time", dur!"hours"(-4), dur!"hours"(1), false),
                        testTZ("GMT Standard Time", "GMT Standard Time",
                               "GMT Daylight Time", dur!"hours"(0), dur!"hours"(1)),
                        testTZ("Romance Standard Time", "Romance Standard Time",
                               "Romance Daylight Time", dur!"hours"(1), dur!"hours"(1)),
                        testTZ("Cen. Australia Standard Time", "Cen. Australia Standard Time",
                               "Cen. Australia Daylight Time",
                               dur!"hours"(9) + dur!"minutes"(30), dur!"hours"(1), false)];

            testTZ("Greenwich Standard Time", "Greenwich Standard Time",
                   "Greenwich Daylight Time", dur!"hours"(0), dur!"hours"(0));
            assertThrown!DateTimeException(WindowsTimeZone.getTimeZone("hello_world"));
        }
        else
            assert(0, "OS not supported.");

        foreach (i; 0 .. tzs.length)
        {
            auto tz = tzs[i];
            immutable spring = dstSwitches[i][2];
            immutable fall = dstSwitches[i][3];
            auto stdOffset = SysTime(dstSwitches[i][0] + dur!"days"(-1), tz).utcOffset;
            auto dstOffset = stdOffset + dur!"hours"(1);

            // Verify that creating a SysTime in the given time zone results
            // in a SysTime with the correct std time during and surrounding
            // a DST switch.
            foreach (hour; -12 .. 13)
            {
                auto st = SysTime(dstSwitches[i][0] + dur!"hours"(hour), tz);
                immutable targetHour = hour < 0 ? hour + 24 : hour;

                static void testHour(SysTime st, int hour, string tzName, size_t line = __LINE__)
                {
                    enforce(st.hour == hour,
                            new AssertError(format("[%s] [%s]: [%s] [%s]", st, tzName, st.hour, hour),
                                            __FILE__, line));
                }

                void testOffset1(Duration offset, bool dstInEffect, size_t line = __LINE__)
                {
                    AssertError msg(string tag)
                    {
                        return new AssertError(format("%s [%s] [%s]: [%s] [%s] [%s]",
                                                      tag, st, tz.name, st.utcOffset, stdOffset, dstOffset),
                                               __FILE__, line);
                    }

                    enforce(st.dstInEffect == dstInEffect, msg("1"));
                    enforce(st.utcOffset == offset, msg("2"));
                    enforce((st + dur!"minutes"(1)).utcOffset == offset, msg("3"));
                }

                if (hour == spring)
                {
                    testHour(st, spring + 1, tz.name);
                    testHour(st + dur!"minutes"(1), spring + 1, tz.name);
                }
                else
                {
                    testHour(st, targetHour, tz.name);
                    testHour(st + dur!"minutes"(1), targetHour, tz.name);
                }

                if (hour < spring)
                    testOffset1(stdOffset, false);
                else
                    testOffset1(dstOffset, true);

                st = SysTime(dstSwitches[i][1] + dur!"hours"(hour), tz);
                testHour(st, targetHour, tz.name);

                // Verify that 01:00 is the first 01:00 (or whatever hour before the switch is).
                if (hour == fall - 1)
                    testHour(st + dur!"hours"(1), targetHour, tz.name);

                if (hour < fall)
                    testOffset1(dstOffset, true);
                else
                    testOffset1(stdOffset, false);
            }

            // Verify that converting a time in UTC to a time in another
            // time zone results in the correct time during and surrounding
            // a DST switch.
            bool first = true;
            auto springSwitch = SysTime(dstSwitches[i][0] + dur!"hours"(spring), UTC()) - stdOffset;
            auto fallSwitch = SysTime(dstSwitches[i][1] + dur!"hours"(fall), UTC()) - dstOffset;
            // @@@BUG@@@ 3659 makes this necessary.
            auto fallSwitchMinus1 = fallSwitch - dur!"hours"(1);

            foreach (hour; -24 .. 25)
            {
                auto utc = SysTime(dstSwitches[i][0] + dur!"hours"(hour), UTC());
                auto local = utc.toOtherTZ(tz);

                void testOffset2(Duration offset, size_t line = __LINE__)
                {
                    AssertError msg(string tag)
                    {
                        return new AssertError(format("%s [%s] [%s]: [%s] [%s]", tag, hour, tz.name, utc, local),
                                               __FILE__, line);
                    }

                    enforce((utc + offset).hour == local.hour, msg("1"));
                    enforce((utc + offset + dur!"minutes"(1)).hour == local.hour, msg("2"));
                }

                if (utc < springSwitch)
                    testOffset2(stdOffset);
                else
                    testOffset2(dstOffset);

                utc = SysTime(dstSwitches[i][1] + dur!"hours"(hour), UTC());
                local = utc.toOtherTZ(tz);

                if (utc == fallSwitch || utc == fallSwitchMinus1)
                {
                    if (first)
                    {
                        testOffset2(dstOffset);
                        first = false;
                    }
                    else
                        testOffset2(stdOffset);
                }
                else if (utc > fallSwitch)
                    testOffset2(stdOffset);
                else
                    testOffset2(dstOffset);
            }
        }
    }


protected:

    /++
        Params:
            name    = The name of the time zone.
            stdName = The abbreviation for the time zone during std time.
            dstName = The abbreviation for the time zone during DST.
      +/
    this(string name, string stdName, string dstName) @safe immutable pure
    {
        _name = name;
        _stdName = stdName;
        _dstName = dstName;
    }


private:

    immutable string _name;
    immutable string _stdName;
    immutable string _dstName;
}


/++
    A TimeZone which represents the current local time zone on
    the system running your program.

    This uses the underlying C calls to adjust the time rather than using
    specific D code based off of system settings to calculate the time such as
    $(LREF PosixTimeZone) and $(LREF WindowsTimeZone) do. That also means that
    it will use whatever the current time zone is on the system, even if the
    system's time zone changes while the program is running.
  +/
final class LocalTime : TimeZone
{
public:

    /++
        $(LREF LocalTime) is a singleton class. $(LREF LocalTime) returns its
        only instance.
      +/
    static immutable(LocalTime) opCall() @trusted pure nothrow
    {
        alias FuncType = @safe pure nothrow immutable(LocalTime) function();
        return (cast(FuncType)&singleton)();
    }


    version (StdDdoc)
    {
        /++
            The name of the time zone per the TZ Database. This is the name used
            to get a $(LREF TimeZone) by name with $(D TimeZone.getTimeZone).

            Note that this always returns the empty string. This is because time
            zones cannot be uniquely identified by the attributes given by the
            OS (such as the $(D stdName) and $(D dstName)), and neither Posix
            systems nor Windows systems provide an easy way to get the TZ
            Database name of the local time zone.

            See_Also:
                $(HTTP en.wikipedia.org/wiki/Tz_database, Wikipedia entry on TZ
                  Database)<br>
                $(HTTP en.wikipedia.org/wiki/List_of_tz_database_time_zones, List
                  of Time Zones)
          +/
        @property override string name() @safe const nothrow;
    }


    /++
        Typically, the abbreviation (generally 3 or 4 letters) for the time zone
        when DST is $(I not) in effect (e.g. PST). It is not necessarily unique.

        However, on Windows, it may be the unabbreviated name (e.g. Pacific
        Standard Time). Regardless, it is not the same as name.

        This property is overridden because the local time of the system could
        change while the program is running and we need to determine it
        dynamically rather than it being fixed like it would be with most time
        zones.
      +/
    @property override string stdName() @trusted const nothrow
    {
        version (Posix)
        {
            import core.stdc.time : tzname;
            import std.conv : to;
            try
                return to!string(tzname[0]);
            catch (Exception e)
                assert(0, "to!string(tzname[0]) failed.");
        }
        else version (Windows)
        {
            TIME_ZONE_INFORMATION tzInfo;
            GetTimeZoneInformation(&tzInfo);

            // Cannot use to!string() like this should, probably due to bug
            // http://d.puremagic.com/issues/show_bug.cgi?id=5016
            //return to!string(tzInfo.StandardName);

            wchar[32] str;

            foreach (i, ref wchar c; str)
                c = tzInfo.StandardName[i];

            string retval;

            try
            {
                foreach (dchar c; str)
                {
                    if (c == '\0')
                        break;

                    retval ~= c;
                }

                return retval;
            }
            catch (Exception e)
                assert(0, "GetTimeZoneInformation() returned invalid UTF-16.");
        }
    }

    @safe unittest
    {
        version (FreeBSD)
        {
            // A bug on FreeBSD 9+ makes it so that this test fails.
            // https://bugs.freebsd.org/bugzilla/show_bug.cgi?id=168862
        }
        else version (NetBSD)
        {
            // The same bug on NetBSD 7+
        }
        else
        {
            assert(LocalTime().stdName !is null);

            version (Posix)
            {
                scope(exit) clearTZEnvVar();

                setTZEnvVar("America/Los_Angeles");
                assert(LocalTime().stdName == "PST");

                setTZEnvVar("America/New_York");
                assert(LocalTime().stdName == "EST");
            }
        }
    }


    /++
        Typically, the abbreviation (generally 3 or 4 letters) for the time zone
        when DST $(I is) in effect (e.g. PDT). It is not necessarily unique.

        However, on Windows, it may be the unabbreviated name (e.g. Pacific
        Daylight Time). Regardless, it is not the same as name.

        This property is overridden because the local time of the system could
        change while the program is running and we need to determine it
        dynamically rather than it being fixed like it would be with most time
        zones.
      +/
    @property override string dstName() @trusted const nothrow
    {
        version (Posix)
        {
            import core.stdc.time : tzname;
            import std.conv : to;
            try
                return to!string(tzname[1]);
            catch (Exception e)
                assert(0, "to!string(tzname[1]) failed.");
        }
        else version (Windows)
        {
            TIME_ZONE_INFORMATION tzInfo;
            GetTimeZoneInformation(&tzInfo);

            // Cannot use to!string() like this should, probably due to bug
            // http://d.puremagic.com/issues/show_bug.cgi?id=5016
            //return to!string(tzInfo.DaylightName);

            wchar[32] str;

            foreach (i, ref wchar c; str)
                c = tzInfo.DaylightName[i];

            string retval;

            try
            {
                foreach (dchar c; str)
                {
                    if (c == '\0')
                        break;

                    retval ~= c;
                }

                return retval;
            }
            catch (Exception e)
                assert(0, "GetTimeZoneInformation() returned invalid UTF-16.");
        }
    }

    @safe unittest
    {
        // tzname, called from dstName, isn't set by default for Musl.
        version (CRuntime_Musl)
            assert(LocalTime().dstName is null);
        else
            assert(LocalTime().dstName !is null);

        version (Posix)
        {
            scope(exit) clearTZEnvVar();

            version (FreeBSD)
            {
                // A bug on FreeBSD 9+ makes it so that this test fails.
                // https://bugs.freebsd.org/bugzilla/show_bug.cgi?id=168862
            }
            else version (NetBSD)
            {
                // The same bug on NetBSD 7+
            }
            else
            {
                setTZEnvVar("America/Los_Angeles");
                assert(LocalTime().dstName == "PDT");

                setTZEnvVar("America/New_York");
                assert(LocalTime().dstName == "EDT");
            }
        }
    }


    /++
        Whether this time zone has Daylight Savings Time at any point in time.
        Note that for some time zone types it may not have DST for current
        dates but will still return true for $(D hasDST) because the time zone
        did at some point have DST.
      +/
    @property override bool hasDST() @trusted const nothrow
    {
        version (Posix)
        {
            static if (is(typeof(daylight)))
                return cast(bool)(daylight);
            else
            {
                try
                {
                    auto currYear = (cast(Date) Clock.currTime()).year;
                    auto janOffset = SysTime(Date(currYear, 1, 4), cast(immutable) this).stdTime -
                                     SysTime(Date(currYear, 1, 4), UTC()).stdTime;
                    auto julyOffset = SysTime(Date(currYear, 7, 4), cast(immutable) this).stdTime -
                                      SysTime(Date(currYear, 7, 4), UTC()).stdTime;

                    return janOffset != julyOffset;
                }
                catch (Exception e)
                    assert(0, "Clock.currTime() threw.");
            }
        }
        else version (Windows)
        {
            TIME_ZONE_INFORMATION tzInfo;
            GetTimeZoneInformation(&tzInfo);

            return tzInfo.DaylightDate.wMonth != 0;
        }
    }

    @safe unittest
    {
        LocalTime().hasDST;

        version (Posix)
        {
            scope(exit) clearTZEnvVar();

            setTZEnvVar("America/Los_Angeles");
            assert(LocalTime().hasDST);

            setTZEnvVar("America/New_York");
            assert(LocalTime().hasDST);

            setTZEnvVar("UTC");
            assert(!LocalTime().hasDST);
        }
    }


    /++
        Takes the number of hnsecs (100 ns) since midnight, January 1st, 1 A.D.
        in UTC time (i.e. std time) and returns whether DST is in effect in this
        time zone at the given point in time.

        Params:
            stdTime = The UTC time that needs to be checked for DST in this time
                      zone.
      +/
    override bool dstInEffect(long stdTime) @trusted const nothrow
    {
        import core.stdc.time : localtime, tm;
        time_t unixTime = stdTimeToUnixTime(stdTime);

        version (Posix)
        {
            tm* timeInfo = localtime(&unixTime);

            return cast(bool)(timeInfo.tm_isdst);
        }
        else version (Windows)
        {
            // Apparently Windows isn't smart enough to deal with negative time_t.
            if (unixTime >= 0)
            {
                tm* timeInfo = localtime(&unixTime);

                if (timeInfo)
                    return cast(bool)(timeInfo.tm_isdst);
            }

            TIME_ZONE_INFORMATION tzInfo;
            GetTimeZoneInformation(&tzInfo);

            return WindowsTimeZone._dstInEffect(&tzInfo, stdTime);
        }
    }

    @safe unittest
    {
        auto currTime = Clock.currStdTime;
        LocalTime().dstInEffect(currTime);
    }


    /++
        Returns hnsecs in the local time zone using the standard C function
        calls on Posix systems and the standard Windows system calls on Windows
        systems to adjust the time to the appropriate time zone from std time.

        Params:
            stdTime = The UTC time that needs to be adjusted to this time zone's
                      time.

        See_Also:
            $(D TimeZone.utcToTZ)
      +/
    override long utcToTZ(long stdTime) @trusted const nothrow
    {
        version (Solaris)
            return stdTime + convert!("seconds", "hnsecs")(tm_gmtoff(stdTime));
        else version (Posix)
        {
            import core.stdc.time : localtime, tm;
            time_t unixTime = stdTimeToUnixTime(stdTime);
            tm* timeInfo = localtime(&unixTime);

            return stdTime + convert!("seconds", "hnsecs")(timeInfo.tm_gmtoff);
        }
        else version (Windows)
        {
            TIME_ZONE_INFORMATION tzInfo;
            GetTimeZoneInformation(&tzInfo);

            return WindowsTimeZone._utcToTZ(&tzInfo, stdTime, hasDST);
        }
    }

    @safe unittest
    {
        LocalTime().utcToTZ(0);
    }


    /++
        Returns std time using the standard C function calls on Posix systems
        and the standard Windows system calls on Windows systems to adjust the
        time to UTC from the appropriate time zone.

        See_Also:
            $(D TimeZone.tzToUTC)

        Params:
            adjTime = The time in this time zone that needs to be adjusted to
                      UTC time.
      +/
    override long tzToUTC(long adjTime) @trusted const nothrow
    {
        version (Posix)
        {
            import core.stdc.time : localtime, tm;
            time_t unixTime = stdTimeToUnixTime(adjTime);

            immutable past = unixTime - cast(time_t) convert!("days", "seconds")(1);
            tm* timeInfo = localtime(past < unixTime ? &past : &unixTime);
            immutable pastOffset = timeInfo.tm_gmtoff;

            immutable future = unixTime + cast(time_t) convert!("days", "seconds")(1);
            timeInfo = localtime(future > unixTime ? &future : &unixTime);
            immutable futureOffset = timeInfo.tm_gmtoff;

            if (pastOffset == futureOffset)
                return adjTime - convert!("seconds", "hnsecs")(pastOffset);

            if (pastOffset < futureOffset)
                unixTime -= cast(time_t) convert!("hours", "seconds")(1);

            unixTime -= pastOffset;
            timeInfo = localtime(&unixTime);

            return adjTime - convert!("seconds", "hnsecs")(timeInfo.tm_gmtoff);
        }
        else version (Windows)
        {
            TIME_ZONE_INFORMATION tzInfo;
            GetTimeZoneInformation(&tzInfo);

            return WindowsTimeZone._tzToUTC(&tzInfo, adjTime, hasDST);
        }
    }

    @safe unittest
    {
        import core.exception : AssertError;
        import std.format : format;
        import std.typecons : tuple;

        assert(LocalTime().tzToUTC(LocalTime().utcToTZ(0)) == 0);
        assert(LocalTime().utcToTZ(LocalTime().tzToUTC(0)) == 0);

        assert(LocalTime().tzToUTC(LocalTime().utcToTZ(0)) == 0);
        assert(LocalTime().utcToTZ(LocalTime().tzToUTC(0)) == 0);

        version (Posix)
        {
            scope(exit) clearTZEnvVar();

            auto tzInfos = [tuple("America/Los_Angeles", DateTime(2012, 3, 11), DateTime(2012, 11, 4), 2, 2),
                            tuple("America/New_York",    DateTime(2012, 3, 11), DateTime(2012, 11, 4), 2, 2),
                            //tuple("America/Santiago",    DateTime(2011, 8, 21), DateTime(2011, 5, 8), 0, 0),
                            tuple("Atlantic/Azores",     DateTime(2011, 3, 27), DateTime(2011, 10, 30), 0, 1),
                            tuple("Europe/London",       DateTime(2012, 3, 25), DateTime(2012, 10, 28), 1, 2),
                            tuple("Europe/Paris",        DateTime(2012, 3, 25), DateTime(2012, 10, 28), 2, 3),
                            tuple("Australia/Adelaide",  DateTime(2012, 10, 7), DateTime(2012, 4, 1), 2, 3)];

            foreach (i; 0 .. tzInfos.length)
            {
                auto tzName = tzInfos[i][0];
                setTZEnvVar(tzName);
                immutable spring = tzInfos[i][3];
                immutable fall = tzInfos[i][4];
                auto stdOffset = SysTime(tzInfos[i][1] + dur!"hours"(-12)).utcOffset;
                auto dstOffset = stdOffset + dur!"hours"(1);

                // Verify that creating a SysTime in the given time zone results
                // in a SysTime with the correct std time during and surrounding
                // a DST switch.
                foreach (hour; -12 .. 13)
                {
                    auto st = SysTime(tzInfos[i][1] + dur!"hours"(hour));
                    immutable targetHour = hour < 0 ? hour + 24 : hour;

                    static void testHour(SysTime st, int hour, string tzName, size_t line = __LINE__)
                    {
                        enforce(st.hour == hour,
                                new AssertError(format("[%s] [%s]: [%s] [%s]", st, tzName, st.hour, hour),
                                                __FILE__, line));
                    }

                    void testOffset1(Duration offset, bool dstInEffect, size_t line = __LINE__)
                    {
                        AssertError msg(string tag)
                        {
                            return new AssertError(format("%s [%s] [%s]: [%s] [%s] [%s]",
                                                          tag, st, tzName, st.utcOffset, stdOffset, dstOffset),
                                                   __FILE__, line);
                        }

                        enforce(st.dstInEffect == dstInEffect, msg("1"));
                        enforce(st.utcOffset == offset, msg("2"));
                        enforce((st + dur!"minutes"(1)).utcOffset == offset, msg("3"));
                    }

                    if (hour == spring)
                    {
                        testHour(st, spring + 1, tzName);
                        testHour(st + dur!"minutes"(1), spring + 1, tzName);
                    }
                    else
                    {
                        testHour(st, targetHour, tzName);
                        testHour(st + dur!"minutes"(1), targetHour, tzName);
                    }

                    if (hour < spring)
                        testOffset1(stdOffset, false);
                    else
                        testOffset1(dstOffset, true);

                    st = SysTime(tzInfos[i][2] + dur!"hours"(hour));
                    testHour(st, targetHour, tzName);

                    // Verify that 01:00 is the first 01:00 (or whatever hour before the switch is).
                    if (hour == fall - 1)
                        testHour(st + dur!"hours"(1), targetHour, tzName);

                    if (hour < fall)
                        testOffset1(dstOffset, true);
                    else
                        testOffset1(stdOffset, false);
                }

                // Verify that converting a time in UTC to a time in another
                // time zone results in the correct time during and surrounding
                // a DST switch.
                bool first = true;
                auto springSwitch = SysTime(tzInfos[i][1] + dur!"hours"(spring), UTC()) - stdOffset;
                auto fallSwitch = SysTime(tzInfos[i][2] + dur!"hours"(fall), UTC()) - dstOffset;
                // @@@BUG@@@ 3659 makes this necessary.
                auto fallSwitchMinus1 = fallSwitch - dur!"hours"(1);

                foreach (hour; -24 .. 25)
                {
                    auto utc = SysTime(tzInfos[i][1] + dur!"hours"(hour), UTC());
                    auto local = utc.toLocalTime();

                    void testOffset2(Duration offset, size_t line = __LINE__)
                    {
                        AssertError msg(string tag)
                        {
                            return new AssertError(format("%s [%s] [%s]: [%s] [%s]", tag, hour, tzName, utc, local),
                                                   __FILE__, line);
                        }

                        enforce((utc + offset).hour == local.hour, msg("1"));
                        enforce((utc + offset + dur!"minutes"(1)).hour == local.hour, msg("2"));
                    }

                    if (utc < springSwitch)
                        testOffset2(stdOffset);
                    else
                        testOffset2(dstOffset);

                    utc = SysTime(tzInfos[i][2] + dur!"hours"(hour), UTC());
                    local = utc.toLocalTime();

                    if (utc == fallSwitch || utc == fallSwitchMinus1)
                    {
                        if (first)
                        {
                            testOffset2(dstOffset);
                            first = false;
                        }
                        else
                            testOffset2(stdOffset);
                    }
                    else if (utc > fallSwitch)
                        testOffset2(stdOffset);
                    else
                        testOffset2(dstOffset);
                }
            }
        }
    }


private:

    this() @safe immutable pure
    {
        super("", "", "");
    }


    // This is done so that we can maintain purity in spite of doing an impure
    // operation the first time that LocalTime() is called.
    static immutable(LocalTime) singleton() @trusted
    {
        import core.stdc.time : tzset;
        import std.concurrency : initOnce;
        static instance = new immutable(LocalTime)();
        static shared bool guard;
        initOnce!guard({tzset(); return true;}());
        return instance;
    }


    // The Solaris version of struct tm has no tm_gmtoff field, so do it here
    version (Solaris)
    {
        long tm_gmtoff(long stdTime) @trusted const nothrow
        {
            import core.stdc.time : localtime, gmtime, tm;

            time_t unixTime = stdTimeToUnixTime(stdTime);
            tm* buf = localtime(&unixTime);
            tm timeInfo = *buf;
            buf = gmtime(&unixTime);
            tm timeInfoGmt = *buf;

            return timeInfo.tm_sec - timeInfoGmt.tm_sec +
                   convert!("minutes", "seconds")(timeInfo.tm_min - timeInfoGmt.tm_min) +
                   convert!("hours", "seconds")(timeInfo.tm_hour - timeInfoGmt.tm_hour);
        }
    }
}


/++
    A $(LREF TimeZone) which represents UTC.
  +/
final class UTC : TimeZone
{
public:

    /++
        $(D UTC) is a singleton class. $(D UTC) returns its only instance.
      +/
    static immutable(UTC) opCall() @safe pure nothrow
    {
        return _utc;
    }


    /++
        Always returns false.
      +/
    @property override bool hasDST() @safe const nothrow
    {
        return false;
    }


    /++
        Always returns false.
      +/
    override bool dstInEffect(long stdTime) @safe const nothrow
    {
        return false;
    }


    /++
        Returns the given hnsecs without changing them at all.

        Params:
            stdTime = The UTC time that needs to be adjusted to this time zone's
                      time.

        See_Also:
            $(D TimeZone.utcToTZ)
      +/
    override long utcToTZ(long stdTime) @safe const nothrow
    {
        return stdTime;
    }

    @safe unittest
    {
        assert(UTC().utcToTZ(0) == 0);

        version (Posix)
        {
            scope(exit) clearTZEnvVar();

            setTZEnvVar("UTC");
            auto std = SysTime(Date(2010, 1, 1));
            auto dst = SysTime(Date(2010, 7, 1));
            assert(UTC().utcToTZ(std.stdTime) == std.stdTime);
            assert(UTC().utcToTZ(dst.stdTime) == dst.stdTime);
        }
    }


    /++
        Returns the given hnsecs without changing them at all.

        See_Also:
            $(D TimeZone.tzToUTC)

        Params:
            adjTime = The time in this time zone that needs to be adjusted to
                      UTC time.
      +/
    override long tzToUTC(long adjTime) @safe const nothrow
    {
        return adjTime;
    }

    @safe unittest
    {
        assert(UTC().tzToUTC(0) == 0);

        version (Posix)
        {
            scope(exit) clearTZEnvVar();

            setTZEnvVar("UTC");
            auto std = SysTime(Date(2010, 1, 1));
            auto dst = SysTime(Date(2010, 7, 1));
            assert(UTC().tzToUTC(std.stdTime) == std.stdTime);
            assert(UTC().tzToUTC(dst.stdTime) == dst.stdTime);
        }
    }


    /++
        Returns a $(REF Duration, core,time) of 0.

        Params:
            stdTime = The UTC time for which to get the offset from UTC for this
                      time zone.
      +/
    override Duration utcOffsetAt(long stdTime) @safe const nothrow
    {
        return dur!"hnsecs"(0);
    }


private:

    this() @safe immutable pure
    {
        super("UTC", "UTC", "UTC");
    }


    static immutable UTC _utc = new immutable(UTC)();
}


/++
    Represents a time zone with an offset (in minutes, west is negative) from
    UTC but no DST.

    It's primarily used as the time zone in the result of
    $(REF SysTime,std,datetime,systime)'s $(D fromISOString),
    $(D fromISOExtString), and $(D fromSimpleString).

    $(D name) and $(D dstName) are always the empty string since this time zone
    has no DST, and while it may be meant to represent a time zone which is in
    the TZ Database, obviously it's not likely to be following the exact rules
    of any of the time zones in the TZ Database, so it makes no sense to set it.
  +/
final class SimpleTimeZone : TimeZone
{
public:

    /++
        Always returns false.
      +/
    @property override bool hasDST() @safe const nothrow
    {
        return false;
    }


    /++
        Always returns false.
      +/
    override bool dstInEffect(long stdTime) @safe const nothrow
    {
        return false;
    }


    /++
        Takes the number of hnsecs (100 ns) since midnight, January 1st, 1 A.D.
        in UTC time (i.e. std time) and converts it to this time zone's time.

        Params:
            stdTime = The UTC time that needs to be adjusted to this time zone's
                      time.
      +/
    override long utcToTZ(long stdTime) @safe const nothrow
    {
        return stdTime + _utcOffset.total!"hnsecs";
    }

    @safe unittest
    {
        auto west = new immutable SimpleTimeZone(dur!"hours"(-8));
        auto east = new immutable SimpleTimeZone(dur!"hours"(8));

        assert(west.utcToTZ(0) == -288_000_000_000L);
        assert(east.utcToTZ(0) == 288_000_000_000L);
        assert(west.utcToTZ(54_321_234_567_890L) == 54_033_234_567_890L);

        const cstz = west;
        assert(cstz.utcToTZ(50002) == west.utcToTZ(50002));
    }


    /++
        Takes the number of hnsecs (100 ns) since midnight, January 1st, 1 A.D.
        in this time zone's time and converts it to UTC (i.e. std time).

        Params:
            adjTime = The time in this time zone that needs to be adjusted to
                      UTC time.
      +/
    override long tzToUTC(long adjTime) @safe const nothrow
    {
        return adjTime - _utcOffset.total!"hnsecs";
    }

    @safe unittest
    {
        auto west = new immutable SimpleTimeZone(dur!"hours"(-8));
        auto east = new immutable SimpleTimeZone(dur!"hours"(8));

        assert(west.tzToUTC(-288_000_000_000L) == 0);
        assert(east.tzToUTC(288_000_000_000L) == 0);
        assert(west.tzToUTC(54_033_234_567_890L) == 54_321_234_567_890L);

        const cstz = west;
        assert(cstz.tzToUTC(20005) == west.tzToUTC(20005));
    }


    /++
        Returns utcOffset as a $(REF Duration, core,time).

        Params:
            stdTime = The UTC time for which to get the offset from UTC for this
                      time zone.
      +/
    override Duration utcOffsetAt(long stdTime) @safe const nothrow
    {
        return _utcOffset;
    }


    /++
        Params:
            utcOffset = This time zone's offset from UTC with west of UTC being
                        negative (it is added to UTC to get the adjusted time).
            stdName   = The $(D stdName) for this time zone.
      +/
    this(Duration utcOffset, string stdName = "") @safe immutable pure
    {
        // FIXME This probably needs to be changed to something like (-12 - 13).
        enforce!DateTimeException(abs(utcOffset) < dur!"minutes"(1440),
                                    "Offset from UTC must be within range (-24:00 - 24:00).");
        super("", stdName, "");
        this._utcOffset = utcOffset;
    }

    @safe unittest
    {
        auto stz = new immutable SimpleTimeZone(dur!"hours"(-8), "PST");
        assert(stz.name == "");
        assert(stz.stdName == "PST");
        assert(stz.dstName == "");
        assert(stz.utcOffset == dur!"hours"(-8));
    }


    /++
        The amount of time the offset from UTC is (negative is west of UTC,
        positive is east).
      +/
    @property Duration utcOffset() @safe const pure nothrow
    {
        return _utcOffset;
    }


package:

    /+
        Returns a time zone as a string with an offset from UTC.

        Time zone offsets will be in the form +HHMM or -HHMM.

        Params:
            utcOffset = The number of minutes offset from UTC (negative means
                        west).
      +/
    static string toISOString(Duration utcOffset) @safe pure
    {
        import std.format : format;
        immutable absOffset = abs(utcOffset);
        enforce!DateTimeException(absOffset < dur!"minutes"(1440),
                                  "Offset from UTC must be within range (-24:00 - 24:00).");
        int hours;
        int minutes;
        absOffset.split!("hours", "minutes")(hours, minutes);
        return format(utcOffset < Duration.zero ? "-%02d%02d" : "+%02d%02d", hours, minutes);
    }

    @safe unittest
    {
        static string testSTZInvalid(Duration offset)
        {
            return SimpleTimeZone.toISOString(offset);
        }

        assertThrown!DateTimeException(testSTZInvalid(dur!"minutes"(1440)));
        assertThrown!DateTimeException(testSTZInvalid(dur!"minutes"(-1440)));

        assert(toISOString(dur!"minutes"(0)) == "+0000");
        assert(toISOString(dur!"minutes"(1)) == "+0001");
        assert(toISOString(dur!"minutes"(10)) == "+0010");
        assert(toISOString(dur!"minutes"(59)) == "+0059");
        assert(toISOString(dur!"minutes"(60)) == "+0100");
        assert(toISOString(dur!"minutes"(90)) == "+0130");
        assert(toISOString(dur!"minutes"(120)) == "+0200");
        assert(toISOString(dur!"minutes"(480)) == "+0800");
        assert(toISOString(dur!"minutes"(1439)) == "+2359");

        assert(toISOString(dur!"minutes"(-1)) == "-0001");
        assert(toISOString(dur!"minutes"(-10)) == "-0010");
        assert(toISOString(dur!"minutes"(-59)) == "-0059");
        assert(toISOString(dur!"minutes"(-60)) == "-0100");
        assert(toISOString(dur!"minutes"(-90)) == "-0130");
        assert(toISOString(dur!"minutes"(-120)) == "-0200");
        assert(toISOString(dur!"minutes"(-480)) == "-0800");
        assert(toISOString(dur!"minutes"(-1439)) == "-2359");
    }


    /+
        Returns a time zone as a string with an offset from UTC.

        Time zone offsets will be in the form +HH:MM or -HH:MM.

        Params:
            utcOffset = The number of minutes offset from UTC (negative means
                        west).
      +/
    static string toISOExtString(Duration utcOffset) @safe pure
    {
        import std.format : format;

        immutable absOffset = abs(utcOffset);
        enforce!DateTimeException(absOffset < dur!"minutes"(1440),
                                  "Offset from UTC must be within range (-24:00 - 24:00).");
        int hours;
        int minutes;
        absOffset.split!("hours", "minutes")(hours, minutes);
        return format(utcOffset < Duration.zero ? "-%02d:%02d" : "+%02d:%02d", hours, minutes);
    }

    @safe unittest
    {
        static string testSTZInvalid(Duration offset)
        {
            return SimpleTimeZone.toISOExtString(offset);
        }

        assertThrown!DateTimeException(testSTZInvalid(dur!"minutes"(1440)));
        assertThrown!DateTimeException(testSTZInvalid(dur!"minutes"(-1440)));

        assert(toISOExtString(dur!"minutes"(0)) == "+00:00");
        assert(toISOExtString(dur!"minutes"(1)) == "+00:01");
        assert(toISOExtString(dur!"minutes"(10)) == "+00:10");
        assert(toISOExtString(dur!"minutes"(59)) == "+00:59");
        assert(toISOExtString(dur!"minutes"(60)) == "+01:00");
        assert(toISOExtString(dur!"minutes"(90)) == "+01:30");
        assert(toISOExtString(dur!"minutes"(120)) == "+02:00");
        assert(toISOExtString(dur!"minutes"(480)) == "+08:00");
        assert(toISOExtString(dur!"minutes"(1439)) == "+23:59");

        assert(toISOExtString(dur!"minutes"(-1)) == "-00:01");
        assert(toISOExtString(dur!"minutes"(-10)) == "-00:10");
        assert(toISOExtString(dur!"minutes"(-59)) == "-00:59");
        assert(toISOExtString(dur!"minutes"(-60)) == "-01:00");
        assert(toISOExtString(dur!"minutes"(-90)) == "-01:30");
        assert(toISOExtString(dur!"minutes"(-120)) == "-02:00");
        assert(toISOExtString(dur!"minutes"(-480)) == "-08:00");
        assert(toISOExtString(dur!"minutes"(-1439)) == "-23:59");
    }


    /+
        Takes a time zone as a string with an offset from UTC and returns a
        $(LREF SimpleTimeZone) which matches.

        The accepted formats for time zone offsets are +HH, -HH, +HHMM, and
        -HHMM.

        Params:
            isoString = A string which represents a time zone in the ISO format.
      +/
    static immutable(SimpleTimeZone) fromISOString(S)(S isoString) @safe pure
        if (isSomeString!S)
    {
        import std.algorithm.searching : startsWith, countUntil, all;
        import std.ascii : isDigit;
        import std.conv : to;
        import std.format : format;

        auto dstr = to!dstring(isoString);

        enforce!DateTimeException(dstr.startsWith('-', '+'), "Invalid ISO String");

        auto sign = dstr.startsWith('-') ? -1 : 1;

        dstr.popFront();
        enforce!DateTimeException(all!isDigit(dstr), format("Invalid ISO String: %s", dstr));

        int hours;
        int minutes;

        if (dstr.length == 2)
            hours = to!int(dstr);
        else if (dstr.length == 4)
        {
            hours = to!int(dstr[0 .. 2]);
            minutes = to!int(dstr[2 .. 4]);
        }
        else
            throw new DateTimeException(format("Invalid ISO String: %s", dstr));

        enforce!DateTimeException(hours < 24 && minutes < 60, format("Invalid ISO String: %s", dstr));

        return new immutable SimpleTimeZone(sign * (dur!"hours"(hours) + dur!"minutes"(minutes)));
    }

    @safe unittest
    {
        import core.exception : AssertError;
        import std.format : format;

        foreach (str; ["", "Z", "-", "+", "-:", "+:", "-1:", "+1:", "+1", "-1",
                       "-24:00", "+24:00", "-24", "+24", "-2400", "+2400",
                       "1", "+1", "-1", "+9", "-9",
                       "+1:0", "+01:0", "+1:00", "+01:000", "+01:60",
                       "-1:0", "-01:0", "-1:00", "-01:000", "-01:60",
                       "000", "00000", "0160", "-0160",
                       " +08:00", "+ 08:00", "+08 :00", "+08: 00", "+08:00 ",
                       " -08:00", "- 08:00", "-08 :00", "-08: 00", "-08:00 ",
                       " +0800", "+ 0800", "+08 00", "+08 00", "+0800 ",
                       " -0800", "- 0800", "-08 00", "-08 00", "-0800 ",
                       "+ab:cd", "+abcd", "+0Z:00", "+Z", "+00Z",
                       "-ab:cd", "+abcd", "-0Z:00", "-Z", "-00Z",
                       "01:00", "12:00", "23:59"])
        {
            assertThrown!DateTimeException(SimpleTimeZone.fromISOString(str), format("[%s]", str));
        }

        static void test(string str, Duration utcOffset, size_t line = __LINE__)
        {
            if (SimpleTimeZone.fromISOString(str).utcOffset != (new immutable SimpleTimeZone(utcOffset)).utcOffset)
                throw new AssertError("unittest failure", __FILE__, line);
        }

        test("+0000", Duration.zero);
        test("+0001", minutes(1));
        test("+0010", minutes(10));
        test("+0059", minutes(59));
        test("+0100", hours(1));
        test("+0130", hours(1) + minutes(30));
        test("+0200", hours(2));
        test("+0800", hours(8));
        test("+2359", hours(23) + minutes(59));

        test("-0001", minutes(-1));
        test("-0010", minutes(-10));
        test("-0059", minutes(-59));
        test("-0100", hours(-1));
        test("-0130", hours(-1) - minutes(30));
        test("-0200", hours(-2));
        test("-0800", hours(-8));
        test("-2359", hours(-23) - minutes(59));

        test("+00", Duration.zero);
        test("+01", hours(1));
        test("+02", hours(2));
        test("+12", hours(12));
        test("+23", hours(23));

        test("-00", Duration.zero);
        test("-01", hours(-1));
        test("-02", hours(-2));
        test("-12", hours(-12));
        test("-23", hours(-23));
    }

    @safe unittest
    {
        import core.exception : AssertError;
        import std.format : format;

        static void test(in string isoString, int expectedOffset, size_t line = __LINE__)
        {
            auto stz = SimpleTimeZone.fromISOExtString(isoString);
            if (stz.utcOffset != dur!"minutes"(expectedOffset))
                throw new AssertError(format("unittest failure: wrong offset [%s]", stz.utcOffset), __FILE__, line);

            auto result = SimpleTimeZone.toISOExtString(stz.utcOffset);
            if (result != isoString)
                throw new AssertError(format("unittest failure: [%s] != [%s]", result, isoString), __FILE__, line);
        }

        test("+00:00", 0);
        test("+00:01", 1);
        test("+00:10", 10);
        test("+00:59", 59);
        test("+01:00", 60);
        test("+01:30", 90);
        test("+02:00", 120);
        test("+08:00", 480);
        test("+08:00", 480);
        test("+23:59", 1439);

        test("-00:01", -1);
        test("-00:10", -10);
        test("-00:59", -59);
        test("-01:00", -60);
        test("-01:30", -90);
        test("-02:00", -120);
        test("-08:00", -480);
        test("-08:00", -480);
        test("-23:59", -1439);
    }


    /+
        Takes a time zone as a string with an offset from UTC and returns a
        $(LREF SimpleTimeZone) which matches.

        The accepted formats for time zone offsets are +HH, -HH, +HH:MM, and
        -HH:MM.

        Params:
            isoExtString = A string which represents a time zone in the ISO format.
      +/
    static immutable(SimpleTimeZone) fromISOExtString(S)(S isoExtString) @safe pure
        if (isSomeString!S)
    {
        import std.algorithm.searching : startsWith, countUntil, all;
        import std.ascii : isDigit;
        import std.conv : to;
        import std.format : format;

        auto dstr = to!dstring(isoExtString);

        enforce!DateTimeException(dstr.startsWith('-', '+'), "Invalid ISO String");

        auto sign = dstr.startsWith('-') ? -1 : 1;

        dstr.popFront();
        enforce!DateTimeException(!dstr.empty, "Invalid ISO String");

        immutable colon = dstr.countUntil(':');

        dstring hoursStr;
        dstring minutesStr;

        if (colon != -1)
        {
            hoursStr = dstr[0 .. colon];
            minutesStr = dstr[colon + 1 .. $];
            enforce!DateTimeException(minutesStr.length == 2, format("Invalid ISO String: %s", dstr));
        }
        else
            hoursStr = dstr;

        enforce!DateTimeException(hoursStr.length == 2, format("Invalid ISO String: %s", dstr));
        enforce!DateTimeException(all!isDigit(hoursStr), format("Invalid ISO String: %s", dstr));
        enforce!DateTimeException(all!isDigit(minutesStr), format("Invalid ISO String: %s", dstr));

        immutable hours = to!int(hoursStr);
        immutable minutes = minutesStr.empty ? 0 : to!int(minutesStr);
        enforce!DateTimeException(hours < 24 && minutes < 60, format("Invalid ISO String: %s", dstr));

        return new immutable SimpleTimeZone(sign * (dur!"hours"(hours) + dur!"minutes"(minutes)));
    }

    @safe unittest
    {
        import core.exception : AssertError;
        import std.format : format;

        foreach (str; ["", "Z", "-", "+", "-:", "+:", "-1:", "+1:", "+1", "-1",
                       "-24:00", "+24:00", "-24", "+24", "-2400", "-2400",
                       "1", "+1", "-1", "+9", "-9",
                       "+1:0", "+01:0", "+1:00", "+01:000", "+01:60",
                       "-1:0", "-01:0", "-1:00", "-01:000", "-01:60",
                       "000", "00000", "0160", "-0160",
                       " +08:00", "+ 08:00", "+08 :00", "+08: 00", "+08:00 ",
                       " -08:00", "- 08:00", "-08 :00", "-08: 00", "-08:00 ",
                       " +0800", "+ 0800", "+08 00", "+08 00", "+0800 ",
                       " -0800", "- 0800", "-08 00", "-08 00", "-0800 ",
                       "+ab:cd", "abcd", "+0Z:00", "+Z", "+00Z",
                       "-ab:cd", "abcd", "-0Z:00", "-Z", "-00Z",
                       "0100", "1200", "2359"])
        {
            assertThrown!DateTimeException(SimpleTimeZone.fromISOExtString(str), format("[%s]", str));
        }

        static void test(string str, Duration utcOffset, size_t line = __LINE__)
        {
            if (SimpleTimeZone.fromISOExtString(str).utcOffset != (new immutable SimpleTimeZone(utcOffset)).utcOffset)
                throw new AssertError("unittest failure", __FILE__, line);
        }

        test("+00:00", Duration.zero);
        test("+00:01", minutes(1));
        test("+00:10", minutes(10));
        test("+00:59", minutes(59));
        test("+01:00", hours(1));
        test("+01:30", hours(1) + minutes(30));
        test("+02:00", hours(2));
        test("+08:00", hours(8));
        test("+23:59", hours(23) + minutes(59));

        test("-00:01", minutes(-1));
        test("-00:10", minutes(-10));
        test("-00:59", minutes(-59));
        test("-01:00", hours(-1));
        test("-01:30", hours(-1) - minutes(30));
        test("-02:00", hours(-2));
        test("-08:00", hours(-8));
        test("-23:59", hours(-23) - minutes(59));

        test("+00", Duration.zero);
        test("+01", hours(1));
        test("+02", hours(2));
        test("+12", hours(12));
        test("+23", hours(23));

        test("-00", Duration.zero);
        test("-01", hours(-1));
        test("-02", hours(-2));
        test("-12", hours(-12));
        test("-23", hours(-23));
    }

    @safe unittest
    {
        import core.exception : AssertError;
        import std.format : format;

        static void test(in string isoExtString, int expectedOffset, size_t line = __LINE__)
        {
            auto stz = SimpleTimeZone.fromISOExtString(isoExtString);
            if (stz.utcOffset != dur!"minutes"(expectedOffset))
                throw new AssertError(format("unittest failure: wrong offset [%s]", stz.utcOffset), __FILE__, line);

            auto result = SimpleTimeZone.toISOExtString(stz.utcOffset);
            if (result != isoExtString)
                throw new AssertError(format("unittest failure: [%s] != [%s]", result, isoExtString), __FILE__, line);
        }

        test("+00:00", 0);
        test("+00:01", 1);
        test("+00:10", 10);
        test("+00:59", 59);
        test("+01:00", 60);
        test("+01:30", 90);
        test("+02:00", 120);
        test("+08:00", 480);
        test("+08:00", 480);
        test("+23:59", 1439);

        test("-00:01", -1);
        test("-00:10", -10);
        test("-00:59", -59);
        test("-01:00", -60);
        test("-01:30", -90);
        test("-02:00", -120);
        test("-08:00", -480);
        test("-08:00", -480);
        test("-23:59", -1439);
    }


private:

    immutable Duration _utcOffset;
}


/++
    Represents a time zone from a TZ Database time zone file. Files from the TZ
    Database are how Posix systems hold their time zone information.
    Unfortunately, Windows does not use the TZ Database. To use the TZ Database,
    use $(D PosixTimeZone) (which reads its information from the TZ Database
    files on disk) on Windows by providing the TZ Database files and telling
    $(D PosixTimeZone.getTimeZone) where the directory holding them is.

    To get a $(D PosixTimeZone), either call $(D PosixTimeZone.getTimeZone)
    (which allows specifying the location the time zone files) or call
    $(D TimeZone.getTimeZone) (which will give a $(D PosixTimeZone) on Posix
    systems and a $(LREF WindowsTimeZone) on Windows systems).

    Note:
        Unless your system's local time zone deals with leap seconds (which is
        highly unlikely), then the only way to get a time zone which
        takes leap seconds into account is to use $(D PosixTimeZone) with a
        time zone whose name starts with "right/". Those time zone files do
        include leap seconds, and $(D PosixTimeZone) will take them into account
        (though posix systems which use a "right/" time zone as their local time
        zone will $(I not) take leap seconds into account even though they're
        in the file).

    See_Also:
        $(HTTP www.iana.org/time-zones, Home of the TZ Database files)<br>
        $(HTTP en.wikipedia.org/wiki/Tz_database, Wikipedia entry on TZ Database)<br>
        $(HTTP en.wikipedia.org/wiki/List_of_tz_database_time_zones, List of Time
          Zones)
  +/
final class PosixTimeZone : TimeZone
{
    import std.algorithm.searching : countUntil, canFind, startsWith;
    import std.file : isDir, isFile, exists, dirEntries, SpanMode, DirEntry;
    import std.path : extension;
    import std.stdio : File;
    import std.string : strip, representation;
    import std.traits : isArray, isSomeChar;
public:

    /++
        Whether this time zone has Daylight Savings Time at any point in time.
        Note that for some time zone types it may not have DST for current
        dates but will still return true for $(D hasDST) because the time zone
        did at some point have DST.
      +/
    @property override bool hasDST() @safe const nothrow
    {
        return _hasDST;
    }


    /++
        Takes the number of hnsecs (100 ns) since midnight, January 1st, 1 A.D.
        in UTC time (i.e. std time) and returns whether DST is in effect in this
        time zone at the given point in time.

        Params:
            stdTime = The UTC time that needs to be checked for DST in this time
                      zone.
      +/
    override bool dstInEffect(long stdTime) @safe const nothrow
    {
        assert(!_transitions.empty);

        immutable unixTime = stdTimeToUnixTime(stdTime);
        immutable found = countUntil!"b < a.timeT"(_transitions, unixTime);

        if (found == -1)
            return _transitions.back.ttInfo.isDST;

        immutable transition = found == 0 ? _transitions[0] : _transitions[found - 1];

        return transition.ttInfo.isDST;
    }


    /++
        Takes the number of hnsecs (100 ns) since midnight, January 1st, 1 A.D.
        in UTC time (i.e. std time) and converts it to this time zone's time.

        Params:
            stdTime = The UTC time that needs to be adjusted to this time zone's
                      time.
      +/
    override long utcToTZ(long stdTime) @safe const nothrow
    {
        assert(!_transitions.empty);

        immutable leapSecs = calculateLeapSeconds(stdTime);
        immutable unixTime = stdTimeToUnixTime(stdTime);
        immutable found = countUntil!"b < a.timeT"(_transitions, unixTime);

        if (found == -1)
            return stdTime + convert!("seconds", "hnsecs")(_transitions.back.ttInfo.utcOffset + leapSecs);

        immutable transition = found == 0 ? _transitions[0] : _transitions[found - 1];

        return stdTime + convert!("seconds", "hnsecs")(transition.ttInfo.utcOffset + leapSecs);
    }


    /++
        Takes the number of hnsecs (100 ns) since midnight, January 1st, 1 A.D.
        in this time zone's time and converts it to UTC (i.e. std time).

        Params:
            adjTime = The time in this time zone that needs to be adjusted to
                      UTC time.
      +/
    override long tzToUTC(long adjTime) @safe const nothrow
    {
        assert(!_transitions.empty);

        immutable leapSecs = calculateLeapSeconds(adjTime);
        time_t unixTime = stdTimeToUnixTime(adjTime);
        immutable past = unixTime - convert!("days", "seconds")(1);
        immutable future = unixTime + convert!("days", "seconds")(1);

        immutable pastFound = countUntil!"b < a.timeT"(_transitions, past);

        if (pastFound == -1)
            return adjTime - convert!("seconds", "hnsecs")(_transitions.back.ttInfo.utcOffset + leapSecs);

        immutable futureFound = countUntil!"b < a.timeT"(_transitions[pastFound .. $], future);
        immutable pastTrans = pastFound == 0 ? _transitions[0] : _transitions[pastFound - 1];

        if (futureFound == 0)
            return adjTime - convert!("seconds", "hnsecs")(pastTrans.ttInfo.utcOffset + leapSecs);

        immutable futureTrans = futureFound == -1 ? _transitions.back
                                                  : _transitions[pastFound + futureFound - 1];
        immutable pastOffset = pastTrans.ttInfo.utcOffset;

        if (pastOffset < futureTrans.ttInfo.utcOffset)
            unixTime -= convert!("hours", "seconds")(1);

        immutable found = countUntil!"b < a.timeT"(_transitions[pastFound .. $], unixTime - pastOffset);

        if (found == -1)
            return adjTime - convert!("seconds", "hnsecs")(_transitions.back.ttInfo.utcOffset + leapSecs);

        immutable transition = found == 0 ? pastTrans : _transitions[pastFound + found - 1];

        return adjTime - convert!("seconds", "hnsecs")(transition.ttInfo.utcOffset + leapSecs);
    }


    version (Android)
    {
        // Android concatenates all time zone data into a single file and stores it here.
        enum defaultTZDatabaseDir = "/system/usr/share/zoneinfo/";
    }
    else version (Solaris)
    {
        /++
            The default directory where the TZ Database files are. It's empty
            for Windows, since Windows doesn't have them.
          +/
        enum defaultTZDatabaseDir = "/usr/share/lib/zoneinfo/";
    }
    else version (Posix)
    {
        /++
            The default directory where the TZ Database files are. It's empty
            for Windows, since Windows doesn't have them.
          +/
        enum defaultTZDatabaseDir = "/usr/share/zoneinfo/";
    }
    else version (Windows)
    {
        /++ The default directory where the TZ Database files are. It's empty
            for Windows, since Windows doesn't have them.
          +/
        enum defaultTZDatabaseDir = "";
    }


    /++
        Returns a $(LREF TimeZone) with the give name per the TZ Database. The
        time zone information is fetched from the TZ Database time zone files in
        the given directory.

        See_Also:
            $(HTTP en.wikipedia.org/wiki/Tz_database, Wikipedia entry on TZ
              Database)<br>
            $(HTTP en.wikipedia.org/wiki/List_of_tz_database_time_zones, List of
              Time Zones)

        Params:
            name          = The TZ Database name of the desired time zone
            tzDatabaseDir = The directory where the TZ Database files are
                            located. Because these files are not located on
                            Windows systems, provide them
                            and give their location here to
                            use $(LREF PosixTimeZone)s.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given time zone
            could not be found or $(D FileException) if the TZ Database file
            could not be opened.
      +/
    // TODO make it possible for tzDatabaseDir to be gzipped tar file rather than an uncompressed
    //      directory.
    static immutable(PosixTimeZone) getTimeZone(string name, string tzDatabaseDir = defaultTZDatabaseDir) @trusted
    {
        import std.algorithm.sorting : sort;
        import std.conv : to;
        import std.format : format;
        import std.path : asNormalizedPath, chainPath;
        import std.range : retro;

        name = strip(name);

        enforce(tzDatabaseDir.exists(), new DateTimeException(format("Directory %s does not exist.", tzDatabaseDir)));
        enforce(tzDatabaseDir.isDir, new DateTimeException(format("%s is not a directory.", tzDatabaseDir)));

        version (Android)
        {
            auto tzfileOffset = name in tzdataIndex(tzDatabaseDir);
            enforce(tzfileOffset, new DateTimeException(format("The time zone %s is not listed.", name)));
            string tzFilename = separate_index ? "zoneinfo.dat" : "tzdata";
            const file = asNormalizedPath(chainPath(tzDatabaseDir, tzFilename)).to!string;
        }
        else
            const file = asNormalizedPath(chainPath(tzDatabaseDir, name)).to!string;

        enforce(file.exists(), new DateTimeException(format("File %s does not exist.", file)));
        enforce(file.isFile, new DateTimeException(format("%s is not a file.", file)));

        auto tzFile = File(file);
        version (Android) tzFile.seek(*tzfileOffset);
        immutable gmtZone = name.representation().canFind("GMT");

        try
        {
            _enforceValidTZFile(readVal!(char[])(tzFile, 4) == "TZif");

            immutable char tzFileVersion = readVal!char(tzFile);
            _enforceValidTZFile(tzFileVersion == '\0' || tzFileVersion == '2' || tzFileVersion == '3');

            {
                auto zeroBlock = readVal!(ubyte[])(tzFile, 15);
                bool allZeroes = true;

                foreach (val; zeroBlock)
                {
                    if (val != 0)
                    {
                        allZeroes = false;
                        break;
                    }
                }

                _enforceValidTZFile(allZeroes);
            }


            // The number of UTC/local indicators stored in the file.
            auto tzh_ttisgmtcnt = readVal!int(tzFile);

            // The number of standard/wall indicators stored in the file.
            auto tzh_ttisstdcnt = readVal!int(tzFile);

            // The number of leap seconds for which data is stored in the file.
            auto tzh_leapcnt = readVal!int(tzFile);

            // The number of "transition times" for which data is stored in the file.
            auto tzh_timecnt = readVal!int(tzFile);

            // The number of "local time types" for which data is stored in the file (must not be zero).
            auto tzh_typecnt = readVal!int(tzFile);
            _enforceValidTZFile(tzh_typecnt != 0);

            // The number of characters of "timezone abbreviation strings" stored in the file.
            auto tzh_charcnt = readVal!int(tzFile);

            // time_ts where DST transitions occur.
            auto transitionTimeTs = new long[](tzh_timecnt);
            foreach (ref transition; transitionTimeTs)
                transition = readVal!int(tzFile);

            // Indices into ttinfo structs indicating the changes
            // to be made at the corresponding DST transition.
            auto ttInfoIndices = new ubyte[](tzh_timecnt);
            foreach (ref ttInfoIndex; ttInfoIndices)
                ttInfoIndex = readVal!ubyte(tzFile);

            // ttinfos which give info on DST transitions.
            auto tempTTInfos = new TempTTInfo[](tzh_typecnt);
            foreach (ref ttInfo; tempTTInfos)
                ttInfo = readVal!TempTTInfo(tzFile);

            // The array of time zone abbreviation characters.
            auto tzAbbrevChars = readVal!(char[])(tzFile, tzh_charcnt);

            auto leapSeconds = new LeapSecond[](tzh_leapcnt);
            foreach (ref leapSecond; leapSeconds)
            {
                // The time_t when the leap second occurs.
                auto timeT = readVal!int(tzFile);

                // The total number of leap seconds to be applied after
                // the corresponding leap second.
                auto total = readVal!int(tzFile);

                leapSecond = LeapSecond(timeT, total);
            }

            // Indicate whether each corresponding DST transition were specified
            // in standard time or wall clock time.
            auto transitionIsStd = new bool[](tzh_ttisstdcnt);
            foreach (ref isStd; transitionIsStd)
                isStd = readVal!bool(tzFile);

            // Indicate whether each corresponding DST transition associated with
            // local time types are specified in UTC or local time.
            auto transitionInUTC = new bool[](tzh_ttisgmtcnt);
            foreach (ref inUTC; transitionInUTC)
                inUTC = readVal!bool(tzFile);

            _enforceValidTZFile(!tzFile.eof);

            // If version 2 or 3, the information is duplicated in 64-bit.
            if (tzFileVersion == '2' || tzFileVersion == '3')
            {
                _enforceValidTZFile(readVal!(char[])(tzFile, 4) == "TZif");

                immutable char tzFileVersion2 = readVal!(char)(tzFile);
                _enforceValidTZFile(tzFileVersion2 == '2' || tzFileVersion2 == '3');

                {
                    auto zeroBlock = readVal!(ubyte[])(tzFile, 15);
                    bool allZeroes = true;

                    foreach (val; zeroBlock)
                    {
                        if (val != 0)
                        {
                            allZeroes = false;
                            break;
                        }
                    }

                    _enforceValidTZFile(allZeroes);
                }


                // The number of UTC/local indicators stored in the file.
                tzh_ttisgmtcnt = readVal!int(tzFile);

                // The number of standard/wall indicators stored in the file.
                tzh_ttisstdcnt = readVal!int(tzFile);

                // The number of leap seconds for which data is stored in the file.
                tzh_leapcnt = readVal!int(tzFile);

                // The number of "transition times" for which data is stored in the file.
                tzh_timecnt = readVal!int(tzFile);

                // The number of "local time types" for which data is stored in the file (must not be zero).
                tzh_typecnt = readVal!int(tzFile);
                _enforceValidTZFile(tzh_typecnt != 0);

                // The number of characters of "timezone abbreviation strings" stored in the file.
                tzh_charcnt = readVal!int(tzFile);

                // time_ts where DST transitions occur.
                transitionTimeTs = new long[](tzh_timecnt);
                foreach (ref transition; transitionTimeTs)
                    transition = readVal!long(tzFile);

                // Indices into ttinfo structs indicating the changes
                // to be made at the corresponding DST transition.
                ttInfoIndices = new ubyte[](tzh_timecnt);
                foreach (ref ttInfoIndex; ttInfoIndices)
                    ttInfoIndex = readVal!ubyte(tzFile);

                // ttinfos which give info on DST transitions.
                tempTTInfos = new TempTTInfo[](tzh_typecnt);
                foreach (ref ttInfo; tempTTInfos)
                    ttInfo = readVal!TempTTInfo(tzFile);

                // The array of time zone abbreviation characters.
                tzAbbrevChars = readVal!(char[])(tzFile, tzh_charcnt);

                leapSeconds = new LeapSecond[](tzh_leapcnt);
                foreach (ref leapSecond; leapSeconds)
                {
                    // The time_t when the leap second occurs.
                    auto timeT = readVal!long(tzFile);

                    // The total number of leap seconds to be applied after
                    // the corresponding leap second.
                    auto total = readVal!int(tzFile);

                    leapSecond = LeapSecond(timeT, total);
                }

                // Indicate whether each corresponding DST transition were specified
                // in standard time or wall clock time.
                transitionIsStd = new bool[](tzh_ttisstdcnt);
                foreach (ref isStd; transitionIsStd)
                    isStd = readVal!bool(tzFile);

                // Indicate whether each corresponding DST transition associated with
                // local time types are specified in UTC or local time.
                transitionInUTC = new bool[](tzh_ttisgmtcnt);
                foreach (ref inUTC; transitionInUTC)
                    inUTC = readVal!bool(tzFile);
            }

            _enforceValidTZFile(tzFile.readln().strip().empty);

            cast(void) tzFile.readln();

            version (Android)
            {
                // Android uses a single file for all timezone data, so the file
                // doesn't end here.
            }
            else
            {
                _enforceValidTZFile(tzFile.readln().strip().empty);
                _enforceValidTZFile(tzFile.eof);
            }


            auto transitionTypes = new TransitionType*[](tempTTInfos.length);

            foreach (i, ref ttype; transitionTypes)
            {
                bool isStd = false;

                if (i < transitionIsStd.length && !transitionIsStd.empty)
                    isStd = transitionIsStd[i];

                bool inUTC = false;

                if (i < transitionInUTC.length && !transitionInUTC.empty)
                    inUTC = transitionInUTC[i];

                ttype = new TransitionType(isStd, inUTC);
            }

            auto ttInfos = new immutable(TTInfo)*[](tempTTInfos.length);
            foreach (i, ref ttInfo; ttInfos)
            {
                auto tempTTInfo = tempTTInfos[i];

                if (gmtZone)
                    tempTTInfo.tt_gmtoff = -tempTTInfo.tt_gmtoff;

                auto abbrevChars = tzAbbrevChars[tempTTInfo.tt_abbrind .. $];
                string abbrev = abbrevChars[0 .. abbrevChars.countUntil('\0')].idup;

                ttInfo = new immutable(TTInfo)(tempTTInfos[i], abbrev);
            }

            auto tempTransitions = new TempTransition[](transitionTimeTs.length);
            foreach (i, ref tempTransition; tempTransitions)
            {
                immutable ttiIndex = ttInfoIndices[i];
                auto transitionTimeT = transitionTimeTs[i];
                auto ttype = transitionTypes[ttiIndex];
                auto ttInfo = ttInfos[ttiIndex];

                tempTransition = TempTransition(transitionTimeT, ttInfo, ttype);
            }

            if (tempTransitions.empty)
            {
                _enforceValidTZFile(ttInfos.length == 1 && transitionTypes.length == 1);
                tempTransitions ~= TempTransition(0, ttInfos[0], transitionTypes[0]);
            }

            sort!"a.timeT < b.timeT"(tempTransitions);
            sort!"a.timeT < b.timeT"(leapSeconds);

            auto transitions = new Transition[](tempTransitions.length);
            foreach (i, ref transition; transitions)
            {
                auto tempTransition = tempTransitions[i];
                auto transitionTimeT = tempTransition.timeT;
                auto ttInfo = tempTransition.ttInfo;

                _enforceValidTZFile(i == 0 || transitionTimeT > tempTransitions[i - 1].timeT);

                transition = Transition(transitionTimeT, ttInfo);
            }

            string stdName;
            string dstName;
            bool hasDST = false;

            foreach (transition; retro(transitions))
            {
                auto ttInfo = transition.ttInfo;

                if (ttInfo.isDST)
                {
                    if (dstName.empty)
                        dstName = ttInfo.abbrev;
                    hasDST = true;
                }
                else
                {
                    if (stdName.empty)
                        stdName = ttInfo.abbrev;
                }

                if (!stdName.empty && !dstName.empty)
                    break;
            }

            return new immutable PosixTimeZone(transitions.idup, leapSeconds.idup, name, stdName, dstName, hasDST);
        }
        catch (DateTimeException dte)
            throw dte;
        catch (Exception e)
            throw new DateTimeException("Not a valid TZ data file", __FILE__, __LINE__, e);
    }

    ///
    @safe unittest
    {
        version (Posix)
        {
            auto tz = PosixTimeZone.getTimeZone("America/Los_Angeles");

            assert(tz.name == "America/Los_Angeles");
            assert(tz.stdName == "PST");
            assert(tz.dstName == "PDT");
        }
    }

    /++
        Returns a list of the names of the time zones installed on the system.

        Providing a sub-name narrows down the list of time zones (which
        can number in the thousands). For example,
        passing in "America" as the sub-name returns only the time zones which
        begin with "America".

        Params:
            subName       = The first part of the desired time zones.
            tzDatabaseDir = The directory where the TZ Database files are
                            located.

        Throws:
            $(D FileException) if it fails to read from disk.
      +/
    static string[] getInstalledTZNames(string subName = "", string tzDatabaseDir = defaultTZDatabaseDir) @trusted
    {
        import std.algorithm.sorting : sort;
        import std.array : appender;
        import std.format : format;

        version (Posix)
            subName = strip(subName);
        else version (Windows)
        {
            import std.array : replace;
            import std.path : dirSeparator;
            subName = replace(strip(subName), "/", dirSeparator);
        }

        enforce(tzDatabaseDir.exists(), new DateTimeException(format("Directory %s does not exist.", tzDatabaseDir)));
        enforce(tzDatabaseDir.isDir, new DateTimeException(format("%s is not a directory.", tzDatabaseDir)));

        auto timezones = appender!(string[])();

        version (Android)
        {
            import std.algorithm.iteration : filter;
            import std.algorithm.mutation : copy;
            tzdataIndex(tzDatabaseDir).byKey.filter!(a => a.startsWith(subName)).copy(timezones);
        }
        else
        {
            foreach (DirEntry de; dirEntries(tzDatabaseDir, SpanMode.depth))
            {
                if (de.isFile)
                {
                    auto tzName = de.name[tzDatabaseDir.length .. $];

                    if (!tzName.extension().empty ||
                        !tzName.startsWith(subName) ||
                        tzName == "leapseconds" ||
                        tzName == "+VERSION")
                    {
                        continue;
                    }

                    timezones.put(tzName);
                }
            }
        }

        sort(timezones.data);

        return timezones.data;
    }

    version (Posix) @system unittest
    {
        import std.exception : assertNotThrown;
        import std.stdio : writefln;
        static void testPTZSuccess(string tzName)
        {
            scope(failure) writefln("TZName which threw: %s", tzName);

            PosixTimeZone.getTimeZone(tzName);
        }

        static void testPTZFailure(string tzName)
        {
            scope(success) writefln("TZName which was supposed to throw: %s", tzName);

            PosixTimeZone.getTimeZone(tzName);
        }

        auto tzNames = getInstalledTZNames();

        foreach (tzName; tzNames)
            assertNotThrown!DateTimeException(testPTZSuccess(tzName));

        // No timezone directories on Android, just a single tzdata file
        version (Android)
        {}
        else
        {
            foreach (DirEntry de; dirEntries(defaultTZDatabaseDir, SpanMode.depth))
            {
                if (de.isFile)
                {
                    auto tzName = de.name[defaultTZDatabaseDir.length .. $];

                    if (!canFind(tzNames, tzName))
                        assertThrown!DateTimeException(testPTZFailure(tzName));
                }
            }
        }
    }


private:

    /+
        Holds information on when a time transition occures (usually a
        transition to or from DST) as well as a pointer to the $(D TTInfo) which
        holds information on the utc offset past the transition.
      +/
    struct Transition
    {
        this(long timeT, immutable (TTInfo)* ttInfo) @safe pure
        {
            this.timeT = timeT;
            this.ttInfo = ttInfo;
        }

        long    timeT;
        immutable (TTInfo)* ttInfo;
    }


    /+
        Holds information on when a leap second occurs.
      +/
    struct LeapSecond
    {
        this(long timeT, int total) @safe pure
        {
            this.timeT = timeT;
            this.total = total;
        }

        long timeT;
        int total;
    }

    /+
        Holds information on the utc offset after a transition as well as
        whether DST is in effect after that transition.
      +/
    struct TTInfo
    {
        this(in TempTTInfo tempTTInfo, string abbrev) @safe immutable pure
        {
            utcOffset = tempTTInfo.tt_gmtoff;
            isDST = tempTTInfo.tt_isdst;
            this.abbrev = abbrev;
        }

        immutable int    utcOffset;  // Offset from UTC.
        immutable bool   isDST;      // Whether DST is in effect.
        immutable string abbrev;     // The current abbreviation for the time zone.
    }


    /+
        Struct used to hold information relating to $(D TTInfo) while organizing
        the time zone information prior to putting it in its final form.
      +/
    struct TempTTInfo
    {
        this(int gmtOff, bool isDST, ubyte abbrInd) @safe pure
        {
            tt_gmtoff = gmtOff;
            tt_isdst = isDST;
            tt_abbrind = abbrInd;
        }

        int   tt_gmtoff;
        bool  tt_isdst;
        ubyte tt_abbrind;
    }


    /+
        Struct used to hold information relating to $(D Transition) while
        organizing the time zone information prior to putting it in its final
        form.
      +/
    struct TempTransition
    {
        this(long timeT, immutable (TTInfo)* ttInfo, TransitionType* ttype) @safe pure
        {
            this.timeT = timeT;
            this.ttInfo = ttInfo;
            this.ttype = ttype;
        }

        long                timeT;
        immutable (TTInfo)* ttInfo;
        TransitionType*     ttype;
    }


    /+
        Struct used to hold information relating to $(D Transition) and
        $(D TTInfo) while organizing the time zone information prior to putting
        it in its final form.
      +/
    struct TransitionType
    {
        this(bool isStd, bool inUTC) @safe pure
        {
            this.isStd = isStd;
            this.inUTC = inUTC;
        }

        // Whether the transition is in std time (as opposed to wall clock time).
        bool isStd;

        // Whether the transition is in UTC (as opposed to local time).
        bool inUTC;
    }


    /+
        Reads an int from a TZ file.
      +/
    static T readVal(T)(ref File tzFile) @trusted
        if ((isIntegral!T || isSomeChar!T) || is(Unqual!T == bool))
    {
        import std.bitmanip : bigEndianToNative;
        T[1] buff;

        _enforceValidTZFile(!tzFile.eof);
        tzFile.rawRead(buff);

        return bigEndianToNative!T(cast(ubyte[T.sizeof]) buff);
    }

    /+
        Reads an array of values from a TZ file.
      +/
    static T readVal(T)(ref File tzFile, size_t length) @trusted
        if (isArray!T)
    {
        auto buff = new T(length);

        _enforceValidTZFile(!tzFile.eof);
        tzFile.rawRead(buff);

        return buff;
    }


    /+
        Reads a $(D TempTTInfo) from a TZ file.
      +/
    static T readVal(T)(ref File tzFile) @safe
        if (is(T == TempTTInfo))
    {
        return TempTTInfo(readVal!int(tzFile),
                          readVal!bool(tzFile),
                          readVal!ubyte(tzFile));
    }


    /+
        Throws:
            $(REF DateTimeException,std,datetime,date) if $(D result) is false.
      +/
    static void _enforceValidTZFile(bool result, size_t line = __LINE__) @safe pure
    {
        if (!result)
            throw new DateTimeException("Not a valid tzdata file.", __FILE__, line);
    }


    int calculateLeapSeconds(long stdTime) @safe const pure nothrow
    {
        if (_leapSeconds.empty)
            return 0;

        immutable unixTime = stdTimeToUnixTime(stdTime);

        if (_leapSeconds.front.timeT >= unixTime)
            return 0;

        immutable found = countUntil!"b < a.timeT"(_leapSeconds, unixTime);

        if (found == -1)
            return _leapSeconds.back.total;

        immutable leapSecond = found == 0 ? _leapSeconds[0] : _leapSeconds[found - 1];

        return leapSecond.total;
    }


    this(immutable Transition[] transitions,
         immutable LeapSecond[] leapSeconds,
         string name,
         string stdName,
         string dstName,
         bool hasDST) @safe immutable pure
    {
        if (dstName.empty && !stdName.empty)
            dstName = stdName;
        else if (stdName.empty && !dstName.empty)
            stdName = dstName;

        super(name, stdName, dstName);

        if (!transitions.empty)
        {
            foreach (i, transition; transitions[0 .. $-1])
                _enforceValidTZFile(transition.timeT < transitions[i + 1].timeT);
        }

        foreach (i, leapSecond; leapSeconds)
            _enforceValidTZFile(i == leapSeconds.length - 1 || leapSecond.timeT < leapSeconds[i + 1].timeT);

        _transitions = transitions;
        _leapSeconds = leapSeconds;
        _hasDST = hasDST;
    }

    // Android concatenates the usual timezone directories into a single file,
    // tzdata, along with an index to jump to each timezone's offset.  In older
    // versions of Android, the index was stored in a separate file, zoneinfo.idx,
    // whereas now it's stored at the beginning of tzdata.
    version (Android)
    {
        // Keep track of whether there's a separate index, zoneinfo.idx.  Only
        // check this after calling tzdataIndex, as it's initialized there.
        static shared bool separate_index;

        // Extracts the name of each time zone and the offset where its data is
        // located in the tzdata file from the index and caches it for later.
        static const(uint[string]) tzdataIndex(string tzDir)
        {
            import std.concurrency : initOnce;

            static __gshared uint[string] _tzIndex;

            // _tzIndex is initialized once and then shared across all threads.
            initOnce!_tzIndex(
            {
                import std.conv : to;
                import std.format : format;
                import std.path : asNormalizedPath, chainPath;

                enum indexEntrySize = 52;
                const combinedFile = asNormalizedPath(chainPath(tzDir, "tzdata")).to!string;
                const indexFile = asNormalizedPath(chainPath(tzDir, "zoneinfo.idx")).to!string;
                File tzFile;
                uint indexEntries, dataOffset;
                uint[string] initIndex;

                // Check for the combined file tzdata, which stores the index
                // and the time zone data together.
                if (combinedFile.exists() && combinedFile.isFile)
                {
                    tzFile = File(combinedFile);
                    _enforceValidTZFile(readVal!(char[])(tzFile, 6) == "tzdata");
                    auto tzDataVersion = readVal!(char[])(tzFile, 6);
                    _enforceValidTZFile(tzDataVersion[5] == '\0');

                    uint indexOffset = readVal!uint(tzFile);
                    dataOffset = readVal!uint(tzFile);
                    readVal!uint(tzFile);

                    indexEntries = (dataOffset - indexOffset) / indexEntrySize;
                    separate_index = false;
                }
                else if (indexFile.exists() && indexFile.isFile)
                {
                    tzFile = File(indexFile);
                    indexEntries = to!uint(tzFile.size/indexEntrySize);
                    separate_index = true;
                }
                else
                {
                    throw new DateTimeException(format("Both timezone files %s and %s do not exist.",
                                                       combinedFile, indexFile));
                }

                foreach (_; 0 .. indexEntries)
                {
                    string tzName = to!string(readVal!(char[])(tzFile, 40).ptr);
                    uint tzOffset = readVal!uint(tzFile);
                    readVal!(uint[])(tzFile, 2);
                    initIndex[tzName] = dataOffset + tzOffset;
                }
                initIndex.rehash;
                return initIndex;
            }());
            return _tzIndex;
        }
    }

    // List of times when the utc offset changes.
    immutable Transition[] _transitions;

    // List of leap second occurrences.
    immutable LeapSecond[] _leapSeconds;

    // Whether DST is in effect for this time zone at any point in time.
    immutable bool _hasDST;
}


version (StdDdoc)
{
    /++
        $(BLUE This class is Windows-Only.)

        Represents a time zone from the Windows registry. Unfortunately, Windows
        does not use the TZ Database. To use the TZ Database, use
        $(LREF PosixTimeZone) (which reads its information from the TZ Database
        files on disk) on Windows by providing the TZ Database files and telling
        $(D PosixTimeZone.getTimeZone) where the directory holding them is.

        The TZ Database files and Windows' time zone information frequently
        do not match. Windows has many errors with regards to when DST switches
        occur (especially for historical dates). Also, the TZ Database files
        include far more time zones than Windows does. So, for accurate
        time zone information, use the TZ Database files with
        $(LREF PosixTimeZone) rather than $(D WindowsTimeZone). However, because
        $(D WindowsTimeZone) uses Windows system calls to deal with the time,
        it's far more likely to match the behavior of other Windows programs.
        Be aware of the differences when selecting a method.

        $(D WindowsTimeZone) does not exist on Posix systems.

        To get a $(D WindowsTimeZone), either call
        $(D WindowsTimeZone.getTimeZone) or call $(D TimeZone.getTimeZone)
        (which will give a $(LREF PosixTimeZone) on Posix systems and a
         $(D WindowsTimeZone) on Windows systems).

        See_Also:
            $(HTTP www.iana.org/time-zones, Home of the TZ Database files)
      +/
    final class WindowsTimeZone : TimeZone
    {
    public:

        /++
            Whether this time zone has Daylight Savings Time at any point in
            time. Note that for some time zone types it may not have DST for
            current dates but will still return true for $(D hasDST) because the
            time zone did at some point have DST.
          +/
        @property override bool hasDST() @safe const nothrow;


        /++
            Takes the number of hnsecs (100 ns) since midnight, January 1st,
            1 A.D. in UTC time (i.e. std time) and returns whether DST is in
            effect in this time zone at the given point in time.

            Params:
                stdTime = The UTC time that needs to be checked for DST in this
                          time zone.
          +/
        override bool dstInEffect(long stdTime) @safe const nothrow;


        /++
            Takes the number of hnsecs (100 ns) since midnight, January 1st,
            1 A.D. in UTC time (i.e. std time) and converts it to this time
                zone's time.

            Params:
                stdTime = The UTC time that needs to be adjusted to this time
                          zone's time.
          +/
        override long utcToTZ(long stdTime) @safe const nothrow;


        /++
            Takes the number of hnsecs (100 ns) since midnight, January 1st,
            1 A.D. in this time zone's time and converts it to UTC (i.e. std
            time).

            Params:
                adjTime = The time in this time zone that needs to be adjusted
                          to UTC time.
          +/
        override long tzToUTC(long adjTime) @safe const nothrow;


        /++
            Returns a $(LREF TimeZone) with the given name per the Windows time
            zone names. The time zone information is fetched from the Windows
            registry.

            See_Also:
                $(HTTP en.wikipedia.org/wiki/Tz_database, Wikipedia entry on TZ
                  Database)<br>
                $(HTTP en.wikipedia.org/wiki/List_of_tz_database_time_zones, List
                  of Time Zones)

            Params:
                name = The TZ Database name of the desired time zone.

            Throws:
                $(REF DateTimeException,std,datetime,date) if the given time
                zone could not be found.

            Example:
    --------------------
    auto tz = WindowsTimeZone.getTimeZone("Pacific Standard Time");
    --------------------
          +/
        static immutable(WindowsTimeZone) getTimeZone(string name) @safe;


        /++
            Returns a list of the names of the time zones installed on the
            system. The list returned by WindowsTimeZone contains the Windows
            TZ names, not the TZ Database names. However,
            $(D TimeZone.getinstalledTZNames) will return the TZ Database names
            which are equivalent to the Windows TZ names.
          +/
        static string[] getInstalledTZNames() @safe;

    private:

        version (Windows)
        {}
        else
            alias TIME_ZONE_INFORMATION = void*;

        static bool _dstInEffect(const TIME_ZONE_INFORMATION* tzInfo, long stdTime) nothrow;
        static long _utcToTZ(const TIME_ZONE_INFORMATION* tzInfo, long stdTime, bool hasDST) nothrow;
        static long _tzToUTC(const TIME_ZONE_INFORMATION* tzInfo, long adjTime, bool hasDST) nothrow;

        this() immutable pure
        {
            super("", "", "");
        }
    }

}
else version (Windows)
{
    final class WindowsTimeZone : TimeZone
    {
        import std.algorithm.sorting : sort;
        import std.array : appender;
        import std.conv : to;
        import std.format : format;

    public:

        @property override bool hasDST() @safe const nothrow
        {
            return _tzInfo.DaylightDate.wMonth != 0;
        }


        override bool dstInEffect(long stdTime) @safe const nothrow
        {
            return _dstInEffect(&_tzInfo, stdTime);
        }


        override long utcToTZ(long stdTime) @safe const nothrow
        {
            return _utcToTZ(&_tzInfo, stdTime, hasDST);
        }


        override long tzToUTC(long adjTime) @safe const nothrow
        {
            return _tzToUTC(&_tzInfo, adjTime, hasDST);
        }


        static immutable(WindowsTimeZone) getTimeZone(string name) @trusted
        {
            scope baseKey = Registry.localMachine.getKey(`Software\Microsoft\Windows NT\CurrentVersion\Time Zones`);

            foreach (tzKeyName; baseKey.keyNames)
            {
                if (tzKeyName != name)
                    continue;

                scope tzKey = baseKey.getKey(tzKeyName);

                scope stdVal = tzKey.getValue("Std");
                auto stdName = stdVal.value_SZ;

                scope dstVal = tzKey.getValue("Dlt");
                auto dstName = dstVal.value_SZ;

                scope tziVal = tzKey.getValue("TZI");
                auto binVal = tziVal.value_BINARY;
                assert(binVal.length == REG_TZI_FORMAT.sizeof);
                auto tziFmt = cast(REG_TZI_FORMAT*) binVal.ptr;

                TIME_ZONE_INFORMATION tzInfo;

                auto wstdName = stdName.to!wstring;
                auto wdstName = dstName.to!wstring;
                auto wstdNameLen = wstdName.length > 32 ? 32 : wstdName.length;
                auto wdstNameLen = wdstName.length > 32 ? 32 : wdstName.length;

                tzInfo.Bias = tziFmt.Bias;
                tzInfo.StandardName[0 .. wstdNameLen] = wstdName[0 .. wstdNameLen];
                tzInfo.StandardName[wstdNameLen .. $] = '\0';
                tzInfo.StandardDate = tziFmt.StandardDate;
                tzInfo.StandardBias = tziFmt.StandardBias;
                tzInfo.DaylightName[0 .. wdstNameLen] = wdstName[0 .. wdstNameLen];
                tzInfo.DaylightName[wdstNameLen .. $] = '\0';
                tzInfo.DaylightDate = tziFmt.DaylightDate;
                tzInfo.DaylightBias = tziFmt.DaylightBias;

                return new immutable WindowsTimeZone(name, tzInfo);
            }
            throw new DateTimeException(format("Failed to find time zone: %s", name));
        }

        static string[] getInstalledTZNames() @trusted
        {
            auto timezones = appender!(string[])();

            scope baseKey = Registry.localMachine.getKey(`Software\Microsoft\Windows NT\CurrentVersion\Time Zones`);

            foreach (tzKeyName; baseKey.keyNames)
                timezones.put(tzKeyName);
            sort(timezones.data);

            return timezones.data;
        }

        @safe unittest
        {
            import std.exception : assertNotThrown;
            import std.stdio : writefln;
            static void testWTZSuccess(string tzName)
            {
                scope(failure) writefln("TZName which threw: %s", tzName);

                WindowsTimeZone.getTimeZone(tzName);
            }

            auto tzNames = getInstalledTZNames();

            foreach (tzName; tzNames)
                assertNotThrown!DateTimeException(testWTZSuccess(tzName));
        }


    private:

        static bool _dstInEffect(const TIME_ZONE_INFORMATION* tzInfo, long stdTime) @trusted nothrow
        {
            try
            {
                if (tzInfo.DaylightDate.wMonth == 0)
                    return false;

                auto utcDateTime = cast(DateTime) SysTime(stdTime, UTC());

                //The limits of what SystemTimeToTzSpecificLocalTime will accept.
                if (utcDateTime.year < 1601)
                {
                    if (utcDateTime.month == Month.feb && utcDateTime.day == 29)
                        utcDateTime.day = 28;
                    utcDateTime.year = 1601;
                }
                else if (utcDateTime.year > 30_827)
                {
                    if (utcDateTime.month == Month.feb && utcDateTime.day == 29)
                        utcDateTime.day = 28;
                    utcDateTime.year = 30_827;
                }

                //SystemTimeToTzSpecificLocalTime doesn't act correctly at the
                //beginning or end of the year (bleh). Unless some bizarre time
                //zone changes DST on January 1st or December 31st, this should
                //fix the problem.
                if (utcDateTime.month == Month.jan)
                {
                    if (utcDateTime.day == 1)
                        utcDateTime.day = 2;
                }
                else if (utcDateTime.month == Month.dec && utcDateTime.day == 31)
                    utcDateTime.day = 30;

                SYSTEMTIME utcTime = void;
                SYSTEMTIME otherTime = void;

                utcTime.wYear = utcDateTime.year;
                utcTime.wMonth = utcDateTime.month;
                utcTime.wDay = utcDateTime.day;
                utcTime.wHour = utcDateTime.hour;
                utcTime.wMinute = utcDateTime.minute;
                utcTime.wSecond = utcDateTime.second;
                utcTime.wMilliseconds = 0;

                immutable result = SystemTimeToTzSpecificLocalTime(cast(TIME_ZONE_INFORMATION*) tzInfo,
                                                                   &utcTime,
                                                                   &otherTime);
                assert(result);

                immutable otherDateTime = DateTime(otherTime.wYear,
                                                   otherTime.wMonth,
                                                   otherTime.wDay,
                                                   otherTime.wHour,
                                                   otherTime.wMinute,
                                                   otherTime.wSecond);
                immutable diff = utcDateTime - otherDateTime;
                immutable minutes = diff.total!"minutes" - tzInfo.Bias;

                if (minutes == tzInfo.DaylightBias)
                    return true;

                assert(minutes == tzInfo.StandardBias);

                return false;
            }
            catch (Exception e)
                assert(0, "DateTime's constructor threw.");
        }

        @system unittest
        {
            TIME_ZONE_INFORMATION tzInfo;
            GetTimeZoneInformation(&tzInfo);

            foreach (year; [1600, 1601, 30_827, 30_828])
                WindowsTimeZone._dstInEffect(&tzInfo, SysTime(DateTime(year, 1, 1)).stdTime);
        }


        static long _utcToTZ(const TIME_ZONE_INFORMATION* tzInfo, long stdTime, bool hasDST) @safe nothrow
        {
            if (hasDST && WindowsTimeZone._dstInEffect(tzInfo, stdTime))
                return stdTime - convert!("minutes", "hnsecs")(tzInfo.Bias + tzInfo.DaylightBias);

            return stdTime - convert!("minutes", "hnsecs")(tzInfo.Bias + tzInfo.StandardBias);
        }


        static long _tzToUTC(const TIME_ZONE_INFORMATION* tzInfo, long adjTime, bool hasDST) @trusted nothrow
        {
            if (hasDST)
            {
                try
                {
                    bool dstInEffectForLocalDateTime(DateTime localDateTime)
                    {
                        // The limits of what SystemTimeToTzSpecificLocalTime will accept.
                        if (localDateTime.year < 1601)
                        {
                            if (localDateTime.month == Month.feb && localDateTime.day == 29)
                                localDateTime.day = 28;

                            localDateTime.year = 1601;
                        }
                        else if (localDateTime.year > 30_827)
                        {
                            if (localDateTime.month == Month.feb && localDateTime.day == 29)
                                localDateTime.day = 28;

                            localDateTime.year = 30_827;
                        }

                        // SystemTimeToTzSpecificLocalTime doesn't act correctly at the
                        // beginning or end of the year (bleh). Unless some bizarre time
                        // zone changes DST on January 1st or December 31st, this should
                        // fix the problem.
                        if (localDateTime.month == Month.jan)
                        {
                            if (localDateTime.day == 1)
                                localDateTime.day = 2;
                        }
                        else if (localDateTime.month == Month.dec && localDateTime.day == 31)
                            localDateTime.day = 30;

                        SYSTEMTIME utcTime = void;
                        SYSTEMTIME localTime = void;

                        localTime.wYear = localDateTime.year;
                        localTime.wMonth = localDateTime.month;
                        localTime.wDay = localDateTime.day;
                        localTime.wHour = localDateTime.hour;
                        localTime.wMinute = localDateTime.minute;
                        localTime.wSecond = localDateTime.second;
                        localTime.wMilliseconds = 0;

                        immutable result = TzSpecificLocalTimeToSystemTime(cast(TIME_ZONE_INFORMATION*) tzInfo,
                                                                           &localTime,
                                                                           &utcTime);
                        assert(result);

                        immutable utcDateTime = DateTime(utcTime.wYear,
                                                         utcTime.wMonth,
                                                         utcTime.wDay,
                                                         utcTime.wHour,
                                                         utcTime.wMinute,
                                                         utcTime.wSecond);

                        immutable diff = localDateTime - utcDateTime;
                        immutable minutes = -tzInfo.Bias - diff.total!"minutes";

                        if (minutes == tzInfo.DaylightBias)
                            return true;

                        assert(minutes == tzInfo.StandardBias);

                        return false;
                    }

                    auto localDateTime = cast(DateTime) SysTime(adjTime, UTC());
                    auto localDateTimeBefore = localDateTime - dur!"hours"(1);
                    auto localDateTimeAfter = localDateTime + dur!"hours"(1);

                    auto dstInEffectNow = dstInEffectForLocalDateTime(localDateTime);
                    auto dstInEffectBefore = dstInEffectForLocalDateTime(localDateTimeBefore);
                    auto dstInEffectAfter = dstInEffectForLocalDateTime(localDateTimeAfter);

                    bool isDST;

                    if (dstInEffectBefore && dstInEffectNow && dstInEffectAfter)
                        isDST = true;
                    else if (!dstInEffectBefore && !dstInEffectNow && !dstInEffectAfter)
                        isDST = false;
                    else if (!dstInEffectBefore && dstInEffectAfter)
                        isDST = false;
                    else if (dstInEffectBefore && !dstInEffectAfter)
                        isDST = dstInEffectNow;
                    else
                        assert(0, "Bad Logic.");

                    if (isDST)
                        return adjTime + convert!("minutes", "hnsecs")(tzInfo.Bias + tzInfo.DaylightBias);
                }
                catch (Exception e)
                    assert(0, "SysTime's constructor threw.");
            }

            return adjTime + convert!("minutes", "hnsecs")(tzInfo.Bias + tzInfo.StandardBias);
        }


        this(string name, TIME_ZONE_INFORMATION tzInfo) @trusted immutable pure
        {
            super(name, to!string(tzInfo.StandardName.ptr), to!string(tzInfo.DaylightName.ptr));
            _tzInfo = tzInfo;
        }


        TIME_ZONE_INFORMATION _tzInfo;
    }
}


version (StdDdoc)
{
    /++
        $(BLUE This function is Posix-Only.)

        Sets the local time zone on Posix systems with the TZ
        Database name by setting the TZ environment variable.

        Unfortunately, there is no way to do it on Windows using the TZ
        Database name, so this function only exists on Posix systems.
      +/
    void setTZEnvVar(string tzDatabaseName) @safe nothrow;


    /++
        $(BLUE This function is Posix-Only.)

        Clears the TZ environment variable.
      +/
    void clearTZEnvVar() @safe nothrow;
}
else version (Posix)
{
    void setTZEnvVar(string tzDatabaseName) @trusted nothrow
    {
        import core.stdc.time : tzset;
        import core.sys.posix.stdlib : setenv;
        import std.internal.cstring : tempCString;
        import std.path : asNormalizedPath, chainPath;

        version (Android)
            auto value = asNormalizedPath(tzDatabaseName);
        else
            auto value = asNormalizedPath(chainPath(PosixTimeZone.defaultTZDatabaseDir, tzDatabaseName));
        setenv("TZ", value.tempCString(), 1);
        tzset();
    }


    void clearTZEnvVar() @trusted nothrow
    {
        import core.stdc.time : tzset;
        import core.sys.posix.stdlib : unsetenv;

        unsetenv("TZ");
        tzset();
    }
}


/++
    Provides the conversions between the IANA time zone database time zone names
    (which POSIX systems use) and the time zone names that Windows uses.

    Windows uses a different set of time zone names than the IANA time zone
    database does, and how they correspond to one another changes over time
    (particularly when Microsoft updates Windows).
    $(HTTP unicode.org/cldr/data/common/supplemental/windowsZones.xml, windowsZones.xml)
    provides the current conversions (which may or may not match up with what's
    on a particular Windows box depending on how up-to-date it is), and
    parseTZConversions reads in those conversions from windowsZones.xml so that
    a D program can use those conversions.

    However, it should be noted that the time zone information on Windows is
    frequently less accurate than that in the IANA time zone database, and if
    someone really wants accurate time zone information, they should use the
    IANA time zone database files with $(LREF PosixTimeZone) on Windows rather
    than $(LREF WindowsTimeZone), whereas $(LREF WindowsTimeZone) makes more
    sense when trying to match what Windows will think the time is in a specific
    time zone.

    Also, the IANA time zone database has a lot more time zones than Windows
    does.

    Params:
        windowsZonesXMLText = The text from
        $(HTTP unicode.org/cldr/data/common/supplemental/windowsZones.xml, windowsZones.xml)

    Throws:
        Exception if there is an error while parsing the given XML.

--------------------
    // Parse the conversions from a local file.
    auto text = std.file.readText("path/to/windowsZones.xml");
    auto conversions = parseTZConversions(text);

    // Alternatively, grab the XML file from the web at runtime
    // and parse it so that it's guaranteed to be up-to-date, though
    // that has the downside that the code needs to worry about the
    // site being down or unicode.org changing the URL.
    auto url = "http://unicode.org/cldr/data/common/supplemental/windowsZones.xml";
    auto conversions2 = parseTZConversions(std.net.curl.get(url));
--------------------
  +/
struct TZConversions
{
    /++
        The key is the Windows time zone name, and the value is a list of
        IANA TZ database names which are close (currently only ever one, but
        it allows for multiple in case it's ever necessary).
      +/
    string[][string] toWindows;

    /++
        The key is the IANA time zone database name, and the value is a list of
        Windows time zone names which are close (usually only one, but it could
        be multiple).
      +/
    string[][string] fromWindows;
}

/++ ditto +/
TZConversions parseTZConversions(string windowsZonesXMLText) @safe pure
{
    // This is a bit hacky, since it doesn't properly read XML, but it avoids
    // needing to pull in std.xml (which we're theoretically replacing at some
    // point anyway).
    import std.algorithm.iteration : uniq;
    import std.algorithm.searching : find;
    import std.algorithm.sorting : sort;
    import std.array : array, split;
    import std.string : lineSplitter;

    string[][string] win2Nix;
    string[][string] nix2Win;

    immutable f1 = `<mapZone other="`;
    immutable f2 = `type="`;

    foreach (line; windowsZonesXMLText.lineSplitter())
    {
        // Sample line:
        // <mapZone other="Canada Central Standard Time" territory="CA" type="America/Regina America/Swift_Current"/>

        line = line.find(f1);
        if (line.empty)
            continue;
        line = line[f1.length .. $];
        auto next = line.find('"');
        enforce(!next.empty, "Error parsing. Text does not appear to be from windowsZones.xml");
        auto win = line[0 .. $ - next.length];
        line = next.find(f2);
        enforce(!line.empty, "Error parsing. Text does not appear to be from windowsZones.xml");
        line = line[f2.length .. $];
        next = line.find('"');
        enforce(!next.empty, "Error parsing. Text does not appear to be from windowsZones.xml");
        auto nixes = line[0 .. $ - next.length].split();

        if (auto n = win in win2Nix)
            *n ~= nixes;
        else
            win2Nix[win] = nixes;

        foreach (nix; nixes)
        {
            if (auto w = nix in nix2Win)
                *w ~= win;
            else
                nix2Win[nix] = [win];
        }
    }

    foreach (key, ref value; nix2Win)
        value = value.sort().uniq().array();
    foreach (key, ref value; win2Nix)
        value = value.sort().uniq().array();

    return TZConversions(nix2Win, win2Nix);
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : uniq;
    import std.algorithm.sorting : isSorted;

    // Reduced text from http://unicode.org/cldr/data/common/supplemental/windowsZones.xml
    auto sampleFileText =
`<?xml version="1.0" encoding="UTF-8" ?>
<!DOCTYPE supplementalData SYSTEM "../../common/dtd/ldmlSupplemental.dtd">
<!--
Copyright © 1991-2013 Unicode, Inc.
CLDR data files are interpreted according to the LDML specification (http://unicode.org/reports/tr35/)
For terms of use, see http://www.unicode.org/copyright.html
-->

<supplementalData>
    <version number="$Revision$"/>

    <windowsZones>
        <mapTimezones otherVersion="7df0005" typeVersion="2015g">

            <!-- (UTC-12:00) International Date Line West -->
            <mapZone other="Dateline Standard Time" territory="001" type="Etc/GMT+12"/>
            <mapZone other="Dateline Standard Time" territory="ZZ" type="Etc/GMT+12"/>

            <!-- (UTC-11:00) Coordinated Universal Time-11 -->
            <mapZone other="UTC-11" territory="001" type="Etc/GMT+11"/>
            <mapZone other="UTC-11" territory="AS" type="Pacific/Pago_Pago"/>
            <mapZone other="UTC-11" territory="NU" type="Pacific/Niue"/>
            <mapZone other="UTC-11" territory="UM" type="Pacific/Midway"/>
            <mapZone other="UTC-11" territory="ZZ" type="Etc/GMT+11"/>

            <!-- (UTC-10:00) Hawaii -->
            <mapZone other="Hawaiian Standard Time" territory="001" type="Pacific/Honolulu"/>
            <mapZone other="Hawaiian Standard Time" territory="CK" type="Pacific/Rarotonga"/>
            <mapZone other="Hawaiian Standard Time" territory="PF" type="Pacific/Tahiti"/>
            <mapZone other="Hawaiian Standard Time" territory="UM" type="Pacific/Johnston"/>
            <mapZone other="Hawaiian Standard Time" territory="US" type="Pacific/Honolulu"/>
            <mapZone other="Hawaiian Standard Time" territory="ZZ" type="Etc/GMT+10"/>

            <!-- (UTC-09:00) Alaska -->
            <mapZone other="Alaskan Standard Time" territory="001" type="America/Anchorage"/>
            <mapZone other="Alaskan Standard Time" territory="US" type="America/Anchorage America/Juneau America/Nome America/Sitka America/Yakutat"/>
        </mapTimezones>
    </windowsZones>
</supplementalData>`;

    auto tzConversions = parseTZConversions(sampleFileText);
    assert(tzConversions.toWindows.length == 15);
    assert(tzConversions.toWindows["America/Anchorage"] == ["Alaskan Standard Time"]);
    assert(tzConversions.toWindows["America/Juneau"] == ["Alaskan Standard Time"]);
    assert(tzConversions.toWindows["America/Nome"] == ["Alaskan Standard Time"]);
    assert(tzConversions.toWindows["America/Sitka"] == ["Alaskan Standard Time"]);
    assert(tzConversions.toWindows["America/Yakutat"] == ["Alaskan Standard Time"]);
    assert(tzConversions.toWindows["Etc/GMT+10"] == ["Hawaiian Standard Time"]);
    assert(tzConversions.toWindows["Etc/GMT+11"] == ["UTC-11"]);
    assert(tzConversions.toWindows["Etc/GMT+12"] == ["Dateline Standard Time"]);
    assert(tzConversions.toWindows["Pacific/Honolulu"] == ["Hawaiian Standard Time"]);
    assert(tzConversions.toWindows["Pacific/Johnston"] == ["Hawaiian Standard Time"]);
    assert(tzConversions.toWindows["Pacific/Midway"] == ["UTC-11"]);
    assert(tzConversions.toWindows["Pacific/Niue"] == ["UTC-11"]);
    assert(tzConversions.toWindows["Pacific/Pago_Pago"] == ["UTC-11"]);
    assert(tzConversions.toWindows["Pacific/Rarotonga"] == ["Hawaiian Standard Time"]);
    assert(tzConversions.toWindows["Pacific/Tahiti"] == ["Hawaiian Standard Time"]);

    assert(tzConversions.fromWindows.length == 4);
    assert(tzConversions.fromWindows["Alaskan Standard Time"] ==
           ["America/Anchorage", "America/Juneau", "America/Nome", "America/Sitka", "America/Yakutat"]);
    assert(tzConversions.fromWindows["Dateline Standard Time"] == ["Etc/GMT+12"]);
    assert(tzConversions.fromWindows["Hawaiian Standard Time"] ==
           ["Etc/GMT+10", "Pacific/Honolulu", "Pacific/Johnston", "Pacific/Rarotonga", "Pacific/Tahiti"]);
    assert(tzConversions.fromWindows["UTC-11"] ==
           ["Etc/GMT+11", "Pacific/Midway", "Pacific/Niue", "Pacific/Pago_Pago"]);

    foreach (key, value; tzConversions.fromWindows)
    {
        assert(value.isSorted, key);
        assert(equal(value.uniq(), value), key);
    }
}

@safe unittest
{
    import std.datetime.systime;

    import std.datetime.timezone : LocalTime;
    SysTime today = Clock.currTime();
    assert(today.timezone is LocalTime());
}

@safe unittest
{
    import std.datetime.systime;

    import std.datetime.date : DateTime;
    import std.datetime.timezone : UTC;

    auto st = SysTime.fromISOExtString("2018-01-01T10:30:00Z");
    assert(st == SysTime(DateTime(2018, 1, 1, 10, 30, 0), UTC()));
}

@safe unittest
{
    import std.datetime.systime;

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

@safe unittest
{
    import std.datetime.systime;

    import std.datetime.timezone : LocalTime;
    SysTime today = Clock.currTime();
    assert(today.timezone is LocalTime());
}

@safe unittest
{
    import std.datetime.systime;

        import std.datetime.date : DateTime;

        assert(SysTime(DateTime(1999, 7, 6, 9, 7, 5)).year == 1999);
        assert(SysTime(DateTime(2010, 10, 4, 0, 0, 30)).year == 2010);
        assert(SysTime(DateTime(-7, 4, 5, 7, 45, 2)).year == -7);
    
}

@safe unittest
{
    import std.datetime.systime;

        import std.datetime.date : DateTime;

        assert(SysTime(DateTime(0, 1, 1, 12, 30, 33)).yearBC == 1);
        assert(SysTime(DateTime(-1, 1, 1, 10, 7, 2)).yearBC == 2);
        assert(SysTime(DateTime(-100, 1, 1, 4, 59, 0)).yearBC == 101);
    
}

@safe unittest
{
    import std.datetime.systime;

        import std.datetime.date : DateTime;

        assert(SysTime(DateTime(1999, 7, 6, 9, 7, 5)).month == 7);
        assert(SysTime(DateTime(2010, 10, 4, 0, 0, 30)).month == 10);
        assert(SysTime(DateTime(-7, 4, 5, 7, 45, 2)).month == 4);
    
}

@safe unittest
{
    import std.datetime.systime;

        import std.datetime.date : DateTime;

        assert(SysTime(DateTime(1999, 7, 6, 9, 7, 5)).day == 6);
        assert(SysTime(DateTime(2010, 10, 4, 0, 0, 30)).day == 4);
        assert(SysTime(DateTime(-7, 4, 5, 7, 45, 2)).day == 5);
    
}

@safe unittest
{
    import std.datetime.systime;

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
    import std.datetime.systime;

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
    import std.datetime.systime;

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
    import std.datetime.systime;

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
    import std.datetime.systime;

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
    import std.datetime.systime;

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
    import std.datetime.systime;

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
    import std.datetime.systime;

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
    import std.datetime.systime;

        import core.time;
        import std.datetime.date : DateTime;

        assert(SysTime(DateTime(1999, 1, 1, 12, 22, 7)).dayOfYear == 1);
        assert(SysTime(DateTime(1999, 12, 31, 7, 2, 59)).dayOfYear == 365);
        assert(SysTime(DateTime(2000, 12, 31, 21, 20, 0)).dayOfYear == 366);
    
}

@safe unittest
{
    import std.datetime.systime;

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
    import std.datetime.systime;

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
    import std.datetime.systime;

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
    import std.datetime.systime;

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
    import std.datetime.systime;

        import core.time;
        import std.datetime.date : DateTime;

        assert(SysTime(DateTime(1999, 1, 6, 0, 0, 0)).daysInMonth == 31);
        assert(SysTime(DateTime(1999, 2, 7, 19, 30, 0)).daysInMonth == 28);
        assert(SysTime(DateTime(2000, 2, 7, 5, 12, 27)).daysInMonth == 29);
        assert(SysTime(DateTime(2000, 6, 4, 12, 22, 9)).daysInMonth == 30);
    
}

@safe unittest
{
    import std.datetime.systime;

        import core.time;
        import std.datetime.date : DateTime;

        assert(SysTime(DateTime(1, 1, 1, 12, 7, 0)).isAD);
        assert(SysTime(DateTime(2010, 12, 31, 0, 0, 0)).isAD);
        assert(!SysTime(DateTime(0, 12, 31, 23, 59, 59)).isAD);
        assert(!SysTime(DateTime(-2010, 1, 1, 2, 2, 2)).isAD);
    
}

@safe unittest
{
    import std.datetime.systime;

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
    import std.datetime.systime;

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
    import std.datetime.systime;

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
    import std.datetime.systime;

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
    import std.datetime.systime;

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
    import std.datetime.systime;

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
    import std.datetime.systime;

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

@safe unittest
{
    import std.datetime.systime;

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
    import std.datetime.systime;

    // Midnight, January 1st, 1970 UTC
    assert(stdTimeToUnixTime(621_355_968_000_000_000L) == 0);

    // 2038-01-19 03:14:07 UTC
    assert(stdTimeToUnixTime(642_830_804_470_000_000L) == int.max);
}

@safe unittest
{
    import std.datetime.systime;

    import std.datetime.date : DateTime;

    assert(DosFileTimeToSysTime(0b00000000001000010000000000000000) == SysTime(DateTime(1980, 1, 1, 0, 0, 0)));
    assert(DosFileTimeToSysTime(0b11111111100111111011111101111101) == SysTime(DateTime(2107, 12, 31, 23, 59, 58)));
    assert(DosFileTimeToSysTime(0x3E3F8456) == SysTime(DateTime(2011, 1, 31, 16, 34, 44)));
}

@safe unittest
{
    import std.datetime.systime;

    import std.datetime.date : DateTime;

    assert(SysTimeToDosFileTime(SysTime(DateTime(1980, 1, 1, 0, 0, 0))) == 0b00000000001000010000000000000000);
    assert(SysTimeToDosFileTime(SysTime(DateTime(2107, 12, 31, 23, 59, 58))) == 0b11111111100111111011111101111101);
    assert(SysTimeToDosFileTime(SysTime(DateTime(2011, 1, 31, 16, 34, 44))) == 0x3E3F8456);
}

@safe unittest
{
    import std.datetime.systime;

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


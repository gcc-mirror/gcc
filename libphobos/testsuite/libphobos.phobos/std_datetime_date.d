@safe pure unittest
{
    import std.datetime.date;

    assert(Date(2018, 10, 1).month == Month.oct);
    assert(DateTime(1, 1, 1).month == Month.jan);
}

@safe pure unittest
{
    import std.datetime.date;

    assert(Date(2018, 10, 1).dayOfWeek == DayOfWeek.mon);
    assert(DateTime(5, 5, 5).dayOfWeek == DayOfWeek.thu);
}

@safe unittest
{
    import std.datetime.date;

        assert(DateTime(Date(1999, 7, 6), TimeOfDay(9, 7, 5)).year == 1999);
        assert(DateTime(Date(2010, 10, 4), TimeOfDay(0, 0, 30)).year == 2010);
        assert(DateTime(Date(-7, 4, 5), TimeOfDay(7, 45, 2)).year == -7);
    
}

@safe unittest
{
    import std.datetime.date;

        assert(DateTime(Date(0, 1, 1), TimeOfDay(12, 30, 33)).yearBC == 1);
        assert(DateTime(Date(-1, 1, 1), TimeOfDay(10, 7, 2)).yearBC == 2);
        assert(DateTime(Date(-100, 1, 1), TimeOfDay(4, 59, 0)).yearBC == 101);
    
}

@safe unittest
{
    import std.datetime.date;

        auto dt = DateTime(Date(2010, 1, 1), TimeOfDay(7, 30, 0));
        dt.yearBC = 1;
        assert(dt == DateTime(Date(0, 1, 1), TimeOfDay(7, 30, 0)));

        dt.yearBC = 10;
        assert(dt == DateTime(Date(-9, 1, 1), TimeOfDay(7, 30, 0)));
    
}

@safe unittest
{
    import std.datetime.date;

        assert(DateTime(Date(1999, 7, 6), TimeOfDay(9, 7, 5)).month == 7);
        assert(DateTime(Date(2010, 10, 4), TimeOfDay(0, 0, 30)).month == 10);
        assert(DateTime(Date(-7, 4, 5), TimeOfDay(7, 45, 2)).month == 4);
    
}

@safe unittest
{
    import std.datetime.date;

        assert(DateTime(Date(1999, 7, 6), TimeOfDay(9, 7, 5)).day == 6);
        assert(DateTime(Date(2010, 10, 4), TimeOfDay(0, 0, 30)).day == 4);
        assert(DateTime(Date(-7, 4, 5), TimeOfDay(7, 45, 2)).day == 5);
    
}

@safe unittest
{
    import std.datetime.date;

        auto dt1 = DateTime(2010, 1, 1, 12, 30, 33);
        dt1.add!"months"(11);
        assert(dt1 == DateTime(2010, 12, 1, 12, 30, 33));

        auto dt2 = DateTime(2010, 1, 1, 12, 30, 33);
        dt2.add!"months"(-11);
        assert(dt2 == DateTime(2009, 2, 1, 12, 30, 33));

        auto dt3 = DateTime(2000, 2, 29, 12, 30, 33);
        dt3.add!"years"(1);
        assert(dt3 == DateTime(2001, 3, 1, 12, 30, 33));

        auto dt4 = DateTime(2000, 2, 29, 12, 30, 33);
        dt4.add!"years"(1, AllowDayOverflow.no);
        assert(dt4 == DateTime(2001, 2, 28, 12, 30, 33));
    
}

@safe unittest
{
    import std.datetime.date;

        auto dt1 = DateTime(2010, 1, 1, 12, 33, 33);
        dt1.roll!"months"(1);
        assert(dt1 == DateTime(2010, 2, 1, 12, 33, 33));

        auto dt2 = DateTime(2010, 1, 1, 12, 33, 33);
        dt2.roll!"months"(-1);
        assert(dt2 == DateTime(2010, 12, 1, 12, 33, 33));

        auto dt3 = DateTime(1999, 1, 29, 12, 33, 33);
        dt3.roll!"months"(1);
        assert(dt3 == DateTime(1999, 3, 1, 12, 33, 33));

        auto dt4 = DateTime(1999, 1, 29, 12, 33, 33);
        dt4.roll!"months"(1, AllowDayOverflow.no);
        assert(dt4 == DateTime(1999, 2, 28, 12, 33, 33));

        auto dt5 = DateTime(2000, 2, 29, 12, 30, 33);
        dt5.roll!"years"(1);
        assert(dt5 == DateTime(2001, 3, 1, 12, 30, 33));

        auto dt6 = DateTime(2000, 2, 29, 12, 30, 33);
        dt6.roll!"years"(1, AllowDayOverflow.no);
        assert(dt6 == DateTime(2001, 2, 28, 12, 30, 33));
    
}

@safe unittest
{
    import std.datetime.date;

        auto dt1 = DateTime(2010, 1, 1, 11, 23, 12);
        dt1.roll!"days"(1);
        assert(dt1 == DateTime(2010, 1, 2, 11, 23, 12));
        dt1.roll!"days"(365);
        assert(dt1 == DateTime(2010, 1, 26, 11, 23, 12));
        dt1.roll!"days"(-32);
        assert(dt1 == DateTime(2010, 1, 25, 11, 23, 12));

        auto dt2 = DateTime(2010, 7, 4, 12, 0, 0);
        dt2.roll!"hours"(1);
        assert(dt2 == DateTime(2010, 7, 4, 13, 0, 0));

        auto dt3 = DateTime(2010, 1, 1, 0, 0, 0);
        dt3.roll!"seconds"(-1);
        assert(dt3 == DateTime(2010, 1, 1, 0, 0, 59));
    
}

@safe unittest
{
    import std.datetime.date;

        import core.time : hours, seconds;

        assert(DateTime(2015, 12, 31, 23, 59, 59) + seconds(1) ==
               DateTime(2016, 1, 1, 0, 0, 0));

        assert(DateTime(2015, 12, 31, 23, 59, 59) + hours(1) ==
               DateTime(2016, 1, 1, 0, 59, 59));

        assert(DateTime(2016, 1, 1, 0, 0, 0) - seconds(1) ==
               DateTime(2015, 12, 31, 23, 59, 59));

        assert(DateTime(2016, 1, 1, 0, 59, 59) - hours(1) ==
               DateTime(2015, 12, 31, 23, 59, 59));
    
}

@safe unittest
{
    import std.datetime.date;

        assert(DateTime(1999, 2, 1, 12, 2, 3).diffMonths(
                   DateTime(1999, 1, 31, 23, 59, 59)) == 1);

        assert(DateTime(1999, 1, 31, 0, 0, 0).diffMonths(
                   DateTime(1999, 2, 1, 12, 3, 42)) == -1);

        assert(DateTime(1999, 3, 1, 5, 30, 0).diffMonths(
                   DateTime(1999, 1, 1, 2, 4, 7)) == 2);

        assert(DateTime(1999, 1, 1, 7, 2, 4).diffMonths(
                   DateTime(1999, 3, 31, 0, 30, 58)) == -2);
    
}

@safe unittest
{
    import std.datetime.date;

        assert(DateTime(Date(1999, 1, 1), TimeOfDay(12, 22, 7)).dayOfYear == 1);
        assert(DateTime(Date(1999, 12, 31), TimeOfDay(7, 2, 59)).dayOfYear == 365);
        assert(DateTime(Date(2000, 12, 31), TimeOfDay(21, 20, 0)).dayOfYear == 366);
    
}

@safe unittest
{
    import std.datetime.date;

        assert(DateTime(Date(1, 1, 1), TimeOfDay(0, 0, 0)).dayOfGregorianCal == 1);
        assert(DateTime(Date(1, 12, 31), TimeOfDay(23, 59, 59)).dayOfGregorianCal == 365);
        assert(DateTime(Date(2, 1, 1), TimeOfDay(2, 2, 2)).dayOfGregorianCal == 366);

        assert(DateTime(Date(0, 12, 31), TimeOfDay(7, 7, 7)).dayOfGregorianCal == 0);
        assert(DateTime(Date(0, 1, 1), TimeOfDay(19, 30, 0)).dayOfGregorianCal == -365);
        assert(DateTime(Date(-1, 12, 31), TimeOfDay(4, 7, 0)).dayOfGregorianCal == -366);

        assert(DateTime(Date(2000, 1, 1), TimeOfDay(9, 30, 20)).dayOfGregorianCal == 730_120);
        assert(DateTime(Date(2010, 12, 31), TimeOfDay(15, 45, 50)).dayOfGregorianCal == 734_137);
    
}

@safe unittest
{
    import std.datetime.date;

        auto dt = DateTime(Date.init, TimeOfDay(12, 0, 0));
        dt.dayOfGregorianCal = 1;
        assert(dt == DateTime(Date(1, 1, 1), TimeOfDay(12, 0, 0)));

        dt.dayOfGregorianCal = 365;
        assert(dt == DateTime(Date(1, 12, 31), TimeOfDay(12, 0, 0)));

        dt.dayOfGregorianCal = 366;
        assert(dt == DateTime(Date(2, 1, 1), TimeOfDay(12, 0, 0)));

        dt.dayOfGregorianCal = 0;
        assert(dt == DateTime(Date(0, 12, 31), TimeOfDay(12, 0, 0)));

        dt.dayOfGregorianCal = -365;
        assert(dt == DateTime(Date(-0, 1, 1), TimeOfDay(12, 0, 0)));

        dt.dayOfGregorianCal = -366;
        assert(dt == DateTime(Date(-1, 12, 31), TimeOfDay(12, 0, 0)));

        dt.dayOfGregorianCal = 730_120;
        assert(dt == DateTime(Date(2000, 1, 1), TimeOfDay(12, 0, 0)));

        dt.dayOfGregorianCal = 734_137;
        assert(dt == DateTime(Date(2010, 12, 31), TimeOfDay(12, 0, 0)));
    
}

@safe unittest
{
    import std.datetime.date;

        assert(DateTime(Date(1999, 1, 6), TimeOfDay(0, 0, 0)).endOfMonth ==
               DateTime(Date(1999, 1, 31), TimeOfDay(23, 59, 59)));

        assert(DateTime(Date(1999, 2, 7), TimeOfDay(19, 30, 0)).endOfMonth ==
               DateTime(Date(1999, 2, 28), TimeOfDay(23, 59, 59)));

        assert(DateTime(Date(2000, 2, 7), TimeOfDay(5, 12, 27)).endOfMonth ==
               DateTime(Date(2000, 2, 29), TimeOfDay(23, 59, 59)));

        assert(DateTime(Date(2000, 6, 4), TimeOfDay(12, 22, 9)).endOfMonth ==
               DateTime(Date(2000, 6, 30), TimeOfDay(23, 59, 59)));
    
}

@safe unittest
{
    import std.datetime.date;

        assert(DateTime(Date(1999, 1, 6), TimeOfDay(0, 0, 0)).daysInMonth == 31);
        assert(DateTime(Date(1999, 2, 7), TimeOfDay(19, 30, 0)).daysInMonth == 28);
        assert(DateTime(Date(2000, 2, 7), TimeOfDay(5, 12, 27)).daysInMonth == 29);
        assert(DateTime(Date(2000, 6, 4), TimeOfDay(12, 22, 9)).daysInMonth == 30);
    
}

@safe unittest
{
    import std.datetime.date;

        assert(DateTime(Date(1, 1, 1), TimeOfDay(12, 7, 0)).isAD);
        assert(DateTime(Date(2010, 12, 31), TimeOfDay(0, 0, 0)).isAD);
        assert(!DateTime(Date(0, 12, 31), TimeOfDay(23, 59, 59)).isAD);
        assert(!DateTime(Date(-2010, 1, 1), TimeOfDay(2, 2, 2)).isAD);
    
}

@safe unittest
{
    import std.datetime.date;

        assert(DateTime(Date(2010, 7, 4), TimeOfDay(7, 6, 12)).toISOString() ==
               "20100704T070612");

        assert(DateTime(Date(1998, 12, 25), TimeOfDay(2, 15, 0)).toISOString() ==
               "19981225T021500");

        assert(DateTime(Date(0, 1, 5), TimeOfDay(23, 9, 59)).toISOString() ==
               "00000105T230959");

        assert(DateTime(Date(-4, 1, 5), TimeOfDay(0, 0, 2)).toISOString() ==
               "-00040105T000002");
    
}

@safe unittest
{
    import std.datetime.date;

        assert(DateTime(Date(2010, 7, 4), TimeOfDay(7, 6, 12)).toISOExtString() ==
               "2010-07-04T07:06:12");

        assert(DateTime(Date(1998, 12, 25), TimeOfDay(2, 15, 0)).toISOExtString() ==
               "1998-12-25T02:15:00");

        assert(DateTime(Date(0, 1, 5), TimeOfDay(23, 9, 59)).toISOExtString() ==
               "0000-01-05T23:09:59");

        assert(DateTime(Date(-4, 1, 5), TimeOfDay(0, 0, 2)).toISOExtString() ==
               "-0004-01-05T00:00:02");
    
}

@safe unittest
{
    import std.datetime.date;

        assert(DateTime(Date(2010, 7, 4), TimeOfDay(7, 6, 12)).toSimpleString() ==
               "2010-Jul-04 07:06:12");

        assert(DateTime(Date(1998, 12, 25), TimeOfDay(2, 15, 0)).toSimpleString() ==
               "1998-Dec-25 02:15:00");

        assert(DateTime(Date(0, 1, 5), TimeOfDay(23, 9, 59)).toSimpleString() ==
               "0000-Jan-05 23:09:59");

        assert(DateTime(Date(-4, 1, 5), TimeOfDay(0, 0, 2)).toSimpleString() ==
               "-0004-Jan-05 00:00:02");
    
}

@safe unittest
{
    import std.datetime.date;

        assert(DateTime.fromISOString("20100704T070612") ==
               DateTime(Date(2010, 7, 4), TimeOfDay(7, 6, 12)));

        assert(DateTime.fromISOString("19981225T021500") ==
               DateTime(Date(1998, 12, 25), TimeOfDay(2, 15, 0)));

        assert(DateTime.fromISOString("00000105T230959") ==
               DateTime(Date(0, 1, 5), TimeOfDay(23, 9, 59)));

        assert(DateTime.fromISOString("-00040105T000002") ==
               DateTime(Date(-4, 1, 5), TimeOfDay(0, 0, 2)));

        assert(DateTime.fromISOString(" 20100704T070612 ") ==
               DateTime(Date(2010, 7, 4), TimeOfDay(7, 6, 12)));
    
}

@safe unittest
{
    import std.datetime.date;

        assert(DateTime.fromISOExtString("2010-07-04T07:06:12") ==
               DateTime(Date(2010, 7, 4), TimeOfDay(7, 6, 12)));

        assert(DateTime.fromISOExtString("1998-12-25T02:15:00") ==
               DateTime(Date(1998, 12, 25), TimeOfDay(2, 15, 0)));

        assert(DateTime.fromISOExtString("0000-01-05T23:09:59") ==
               DateTime(Date(0, 1, 5), TimeOfDay(23, 9, 59)));

        assert(DateTime.fromISOExtString("-0004-01-05T00:00:02") ==
               DateTime(Date(-4, 1, 5), TimeOfDay(0, 0, 2)));

        assert(DateTime.fromISOExtString(" 2010-07-04T07:06:12 ") ==
               DateTime(Date(2010, 7, 4), TimeOfDay(7, 6, 12)));
    
}

@safe unittest
{
    import std.datetime.date;

        assert(DateTime.fromSimpleString("2010-Jul-04 07:06:12") ==
               DateTime(Date(2010, 7, 4), TimeOfDay(7, 6, 12)));
        assert(DateTime.fromSimpleString("1998-Dec-25 02:15:00") ==
               DateTime(Date(1998, 12, 25), TimeOfDay(2, 15, 0)));
        assert(DateTime.fromSimpleString("0000-Jan-05 23:09:59") ==
               DateTime(Date(0, 1, 5), TimeOfDay(23, 9, 59)));
        assert(DateTime.fromSimpleString("-0004-Jan-05 00:00:02") ==
               DateTime(Date(-4, 1, 5), TimeOfDay(0, 0, 2)));
        assert(DateTime.fromSimpleString(" 2010-Jul-04 07:06:12 ") ==
               DateTime(Date(2010, 7, 4), TimeOfDay(7, 6, 12)));
    
}

@safe pure unittest
{
    import std.datetime.date;

    import core.time : days, seconds;

    auto dt = DateTime(2000, 6, 1, 10, 30, 0);

    assert(dt.date == Date(2000, 6, 1));
    assert(dt.timeOfDay == TimeOfDay(10, 30, 0));
    assert(dt.dayOfYear == 153);
    assert(dt.dayOfWeek == DayOfWeek.thu);

    dt += 10.days + 100.seconds;
    assert(dt == DateTime(2000, 6, 11, 10, 31, 40));

    assert(dt.toISOExtString() == "2000-06-11T10:31:40");
    assert(dt.toISOString() == "20000611T103140");
    assert(dt.toSimpleString() == "2000-Jun-11 10:31:40");

    assert(DateTime.fromISOExtString("2018-01-01T12:00:00") == DateTime(2018, 1, 1, 12, 0, 0));
    assert(DateTime.fromISOString("20180101T120000") == DateTime(2018, 1, 1, 12, 0, 0));
    assert(DateTime.fromSimpleString("2018-Jan-01 12:00:00") == DateTime(2018, 1, 1, 12, 0, 0));
}

@safe unittest
{
    import std.datetime.date;

        assert(Date(1999, 7, 6).year == 1999);
        assert(Date(2010, 10, 4).year == 2010);
        assert(Date(-7, 4, 5).year == -7);
    
}

@safe unittest
{
    import std.datetime.date;

        assert(Date(1999, 7, 6).year == 1999);
        assert(Date(2010, 10, 4).year == 2010);
        assert(Date(-7, 4, 5).year == -7);
    
}

@safe unittest
{
    import std.datetime.date;

        assert(Date(0, 1, 1).yearBC == 1);
        assert(Date(-1, 1, 1).yearBC == 2);
        assert(Date(-100, 1, 1).yearBC == 101);
    
}

@safe unittest
{
    import std.datetime.date;

        auto date = Date(2010, 1, 1);
        date.yearBC = 1;
        assert(date == Date(0, 1, 1));

        date.yearBC = 10;
        assert(date == Date(-9, 1, 1));
    
}

@safe unittest
{
    import std.datetime.date;

        assert(Date(1999, 7, 6).month == 7);
        assert(Date(2010, 10, 4).month == 10);
        assert(Date(-7, 4, 5).month == 4);
    
}

@safe unittest
{
    import std.datetime.date;

        assert(Date(1999, 7, 6).day == 6);
        assert(Date(2010, 10, 4).day == 4);
        assert(Date(-7, 4, 5).day == 5);
    
}

@safe unittest
{
    import std.datetime.date;

        auto d1 = Date(2010, 1, 1);
        d1.add!"months"(11);
        assert(d1 == Date(2010, 12, 1));

        auto d2 = Date(2010, 1, 1);
        d2.add!"months"(-11);
        assert(d2 == Date(2009, 2, 1));

        auto d3 = Date(2000, 2, 29);
        d3.add!"years"(1);
        assert(d3 == Date(2001, 3, 1));

        auto d4 = Date(2000, 2, 29);
        d4.add!"years"(1, AllowDayOverflow.no);
        assert(d4 == Date(2001, 2, 28));
    
}

@safe unittest
{
    import std.datetime.date;

        auto d1 = Date(2010, 1, 1);
        d1.roll!"months"(1);
        assert(d1 == Date(2010, 2, 1));

        auto d2 = Date(2010, 1, 1);
        d2.roll!"months"(-1);
        assert(d2 == Date(2010, 12, 1));

        auto d3 = Date(1999, 1, 29);
        d3.roll!"months"(1);
        assert(d3 == Date(1999, 3, 1));

        auto d4 = Date(1999, 1, 29);
        d4.roll!"months"(1, AllowDayOverflow.no);
        assert(d4 == Date(1999, 2, 28));

        auto d5 = Date(2000, 2, 29);
        d5.roll!"years"(1);
        assert(d5 == Date(2001, 3, 1));

        auto d6 = Date(2000, 2, 29);
        d6.roll!"years"(1, AllowDayOverflow.no);
        assert(d6 == Date(2001, 2, 28));
    
}

@safe unittest
{
    import std.datetime.date;

        auto d = Date(2010, 1, 1);
        d.roll!"days"(1);
        assert(d == Date(2010, 1, 2));
        d.roll!"days"(365);
        assert(d == Date(2010, 1, 26));
        d.roll!"days"(-32);
        assert(d == Date(2010, 1, 25));
    
}

@safe unittest
{
    import std.datetime.date;

        import core.time : days;

        assert(Date(2015, 12, 31) + days(1) == Date(2016, 1, 1));
        assert(Date(2004, 2, 26) + days(4) == Date(2004, 3, 1));

        assert(Date(2016, 1, 1) - days(1) == Date(2015, 12, 31));
        assert(Date(2004, 3, 1) - days(4) == Date(2004, 2, 26));
    
}

@safe unittest
{
    import std.datetime.date;

        assert(Date(1999, 2, 1).diffMonths(Date(1999, 1, 31)) == 1);
        assert(Date(1999, 1, 31).diffMonths(Date(1999, 2, 1)) == -1);
        assert(Date(1999, 3, 1).diffMonths(Date(1999, 1, 1)) == 2);
        assert(Date(1999, 1, 1).diffMonths(Date(1999, 3, 31)) == -2);
    
}

@safe unittest
{
    import std.datetime.date;

        assert(Date(1999, 1, 1).dayOfYear == 1);
        assert(Date(1999, 12, 31).dayOfYear == 365);
        assert(Date(2000, 12, 31).dayOfYear == 366);
    
}

@safe unittest
{
    import std.datetime.date;

        assert(Date(1, 1, 1).dayOfGregorianCal == 1);
        assert(Date(1, 12, 31).dayOfGregorianCal == 365);
        assert(Date(2, 1, 1).dayOfGregorianCal == 366);

        assert(Date(0, 12, 31).dayOfGregorianCal == 0);
        assert(Date(0, 1, 1).dayOfGregorianCal == -365);
        assert(Date(-1, 12, 31).dayOfGregorianCal == -366);

        assert(Date(2000, 1, 1).dayOfGregorianCal == 730_120);
        assert(Date(2010, 12, 31).dayOfGregorianCal == 734_137);
    
}

@safe unittest
{
    import std.datetime.date;

        auto date = Date.init;
        date.dayOfGregorianCal = 1;
        assert(date == Date(1, 1, 1));

        date.dayOfGregorianCal = 365;
        assert(date == Date(1, 12, 31));

        date.dayOfGregorianCal = 366;
        assert(date == Date(2, 1, 1));

        date.dayOfGregorianCal = 0;
        assert(date == Date(0, 12, 31));

        date.dayOfGregorianCal = -365;
        assert(date == Date(-0, 1, 1));

        date.dayOfGregorianCal = -366;
        assert(date == Date(-1, 12, 31));

        date.dayOfGregorianCal = 730_120;
        assert(date == Date(2000, 1, 1));

        date.dayOfGregorianCal = 734_137;
        assert(date == Date(2010, 12, 31));
    
}

@safe unittest
{
    import std.datetime.date;

        assert(Date(1999, 1, 6).endOfMonth == Date(1999, 1, 31));
        assert(Date(1999, 2, 7).endOfMonth == Date(1999, 2, 28));
        assert(Date(2000, 2, 7).endOfMonth == Date(2000, 2, 29));
        assert(Date(2000, 6, 4).endOfMonth == Date(2000, 6, 30));
    
}

@safe unittest
{
    import std.datetime.date;

        assert(Date(1999, 1, 6).daysInMonth == 31);
        assert(Date(1999, 2, 7).daysInMonth == 28);
        assert(Date(2000, 2, 7).daysInMonth == 29);
        assert(Date(2000, 6, 4).daysInMonth == 30);
    
}

@safe unittest
{
    import std.datetime.date;

        assert(Date(1, 1, 1).isAD);
        assert(Date(2010, 12, 31).isAD);
        assert(!Date(0, 12, 31).isAD);
        assert(!Date(-2010, 1, 1).isAD);
    
}

@safe unittest
{
    import std.datetime.date;

        assert(Date(2010, 7, 4).toISOString() == "20100704");
        assert(Date(1998, 12, 25).toISOString() == "19981225");
        assert(Date(0, 1, 5).toISOString() == "00000105");
        assert(Date(-4, 1, 5).toISOString() == "-00040105");
    
}

@safe unittest
{
    import std.datetime.date;

        assert(Date(2010, 7, 4).toISOExtString() == "2010-07-04");
        assert(Date(1998, 12, 25).toISOExtString() == "1998-12-25");
        assert(Date(0, 1, 5).toISOExtString() == "0000-01-05");
        assert(Date(-4, 1, 5).toISOExtString() == "-0004-01-05");
    
}

@safe unittest
{
    import std.datetime.date;

        assert(Date(2010, 7, 4).toSimpleString() == "2010-Jul-04");
        assert(Date(1998, 12, 25).toSimpleString() == "1998-Dec-25");
        assert(Date(0, 1, 5).toSimpleString() == "0000-Jan-05");
        assert(Date(-4, 1, 5).toSimpleString() == "-0004-Jan-05");
    
}

@safe unittest
{
    import std.datetime.date;

        assert(Date.fromISOString("20100704") == Date(2010, 7, 4));
        assert(Date.fromISOString("19981225") == Date(1998, 12, 25));
        assert(Date.fromISOString("00000105") == Date(0, 1, 5));
        assert(Date.fromISOString("-00040105") == Date(-4, 1, 5));
        assert(Date.fromISOString(" 20100704 ") == Date(2010, 7, 4));
    
}

@safe unittest
{
    import std.datetime.date;

        assert(Date.fromISOExtString("2010-07-04") == Date(2010, 7, 4));
        assert(Date.fromISOExtString("1998-12-25") == Date(1998, 12, 25));
        assert(Date.fromISOExtString("0000-01-05") == Date(0, 1, 5));
        assert(Date.fromISOExtString("-0004-01-05") == Date(-4, 1, 5));
        assert(Date.fromISOExtString(" 2010-07-04 ") == Date(2010, 7, 4));
    
}

@safe unittest
{
    import std.datetime.date;

        assert(Date.fromSimpleString("2010-Jul-04") == Date(2010, 7, 4));
        assert(Date.fromSimpleString("1998-Dec-25") == Date(1998, 12, 25));
        assert(Date.fromSimpleString("0000-Jan-05") == Date(0, 1, 5));
        assert(Date.fromSimpleString("-0004-Jan-05") == Date(-4, 1, 5));
        assert(Date.fromSimpleString(" 2010-Jul-04 ") == Date(2010, 7, 4));
    
}

@safe pure unittest
{
    import std.datetime.date;

    import core.time : days;

    auto d = Date(2000, 6, 1);

    assert(d.dayOfYear == 153);
    assert(d.dayOfWeek == DayOfWeek.thu);

    d += 10.days;
    assert(d == Date(2000, 6, 11));

    assert(d.toISOExtString() == "2000-06-11");
    assert(d.toISOString() == "20000611");
    assert(d.toSimpleString() == "2000-Jun-11");

    assert(Date.fromISOExtString("2018-01-01") == Date(2018, 1, 1));
    assert(Date.fromISOString("20180101") == Date(2018, 1, 1));
    assert(Date.fromSimpleString("2018-Jan-01") == Date(2018, 1, 1));
}

@safe unittest
{
    import std.datetime.date;

        auto tod1 = TimeOfDay(7, 12, 0);
        tod1.roll!"hours"(1);
        assert(tod1 == TimeOfDay(8, 12, 0));

        auto tod2 = TimeOfDay(7, 12, 0);
        tod2.roll!"hours"(-1);
        assert(tod2 == TimeOfDay(6, 12, 0));

        auto tod3 = TimeOfDay(23, 59, 0);
        tod3.roll!"minutes"(1);
        assert(tod3 == TimeOfDay(23, 0, 0));

        auto tod4 = TimeOfDay(0, 0, 0);
        tod4.roll!"minutes"(-1);
        assert(tod4 == TimeOfDay(0, 59, 0));

        auto tod5 = TimeOfDay(23, 59, 59);
        tod5.roll!"seconds"(1);
        assert(tod5 == TimeOfDay(23, 59, 0));

        auto tod6 = TimeOfDay(0, 0, 0);
        tod6.roll!"seconds"(-1);
        assert(tod6 == TimeOfDay(0, 0, 59));
    
}

@safe unittest
{
    import std.datetime.date;

        import core.time : hours, minutes, seconds;

        assert(TimeOfDay(12, 12, 12) + seconds(1) == TimeOfDay(12, 12, 13));
        assert(TimeOfDay(12, 12, 12) + minutes(1) == TimeOfDay(12, 13, 12));
        assert(TimeOfDay(12, 12, 12) + hours(1) == TimeOfDay(13, 12, 12));
        assert(TimeOfDay(23, 59, 59) + seconds(1) == TimeOfDay(0, 0, 0));

        assert(TimeOfDay(12, 12, 12) - seconds(1) == TimeOfDay(12, 12, 11));
        assert(TimeOfDay(12, 12, 12) - minutes(1) == TimeOfDay(12, 11, 12));
        assert(TimeOfDay(12, 12, 12) - hours(1) == TimeOfDay(11, 12, 12));
        assert(TimeOfDay(0, 0, 0) - seconds(1) == TimeOfDay(23, 59, 59));
    
}

@safe unittest
{
    import std.datetime.date;

        assert(TimeOfDay(0, 0, 0).toISOString() == "000000");
        assert(TimeOfDay(12, 30, 33).toISOString() == "123033");
    
}

@safe unittest
{
    import std.datetime.date;

        assert(TimeOfDay(0, 0, 0).toISOExtString() == "00:00:00");
        assert(TimeOfDay(12, 30, 33).toISOExtString() == "12:30:33");
    
}

@safe unittest
{
    import std.datetime.date;

        assert(TimeOfDay.fromISOString("000000") == TimeOfDay(0, 0, 0));
        assert(TimeOfDay.fromISOString("123033") == TimeOfDay(12, 30, 33));
        assert(TimeOfDay.fromISOString(" 123033 ") == TimeOfDay(12, 30, 33));
    
}

@safe unittest
{
    import std.datetime.date;

        assert(TimeOfDay.fromISOExtString("00:00:00") == TimeOfDay(0, 0, 0));
        assert(TimeOfDay.fromISOExtString("12:30:33") == TimeOfDay(12, 30, 33));
        assert(TimeOfDay.fromISOExtString(" 12:30:33 ") == TimeOfDay(12, 30, 33));
    
}

@safe pure unittest
{
    import std.datetime.date;

    import core.time : minutes, seconds;

    auto t = TimeOfDay(12, 30, 0);

    t += 10.minutes + 100.seconds;
    assert(t == TimeOfDay(12, 41, 40));

    assert(t.toISOExtString() == "12:41:40");
    assert(t.toISOString() == "124140");

    assert(TimeOfDay.fromISOExtString("15:00:00") == TimeOfDay(15, 0, 0));
    assert(TimeOfDay.fromISOString("015000") == TimeOfDay(1, 50, 0));
}

@safe unittest
{
    import std.datetime.date;

    assert(valid!"hours"(12));
    assert(!valid!"hours"(32));
    assert(valid!"months"(12));
    assert(!valid!"months"(13));
}

@safe pure nothrow @nogc unittest
{
    import std.datetime.date;

    assert(valid!"days"(2016, 2, 29));
    assert(!valid!"days"(2016, 2, 30));
    assert(valid!"days"(2017, 2, 20));
    assert(!valid!"days"(2017, 2, 29));
}

@safe pure unittest
{
    import std.datetime.date;

    import std.exception : assertThrown, assertNotThrown;

    assertNotThrown(enforceValid!"months"(10));
    assertNotThrown(enforceValid!"seconds"(40));

    assertThrown!DateTimeException(enforceValid!"months"(0));
    assertThrown!DateTimeException(enforceValid!"hours"(24));
    assertThrown!DateTimeException(enforceValid!"minutes"(60));
    assertThrown!DateTimeException(enforceValid!"seconds"(60));
}

@safe pure unittest
{
    import std.datetime.date;

    import std.exception : assertThrown, assertNotThrown;

    assertNotThrown(enforceValid!"days"(2000, Month.jan, 1));
    // leap year
    assertNotThrown(enforceValid!"days"(2000, Month.feb, 29));

    assertThrown!DateTimeException(enforceValid!"days"(2001, Month.feb, 29));
    assertThrown!DateTimeException(enforceValid!"days"(2000, Month.jan, 32));
    assertThrown!DateTimeException(enforceValid!"days"(2000, Month.apr, 31));
}

@safe pure nothrow @nogc unittest
{
    import std.datetime.date;

    assert(daysToDayOfWeek(DayOfWeek.mon, DayOfWeek.mon) == 0);
    assert(daysToDayOfWeek(DayOfWeek.mon, DayOfWeek.sun) == 6);
    assert(daysToDayOfWeek(DayOfWeek.mon, DayOfWeek.wed) == 2);
}

@safe pure unittest
{
    import std.datetime.date;

    assert(monthsToMonth(Month.jan, Month.jan) == 0);
    assert(monthsToMonth(Month.jan, Month.dec) == 11);
    assert(monthsToMonth(Month.jul, Month.oct) == 3);
}

@safe unittest
{
    import std.datetime.date;

    foreach (year; [1, 2, 100, 2001, 2002, 2003, 2005, 2006, 2007, 2009, 2010])
    {
        assert(!yearIsLeapYear(year));
        assert(!yearIsLeapYear(-year));
    }

    foreach (year; [0, 4, 8, 400, 800, 1600, 1996, 2000, 2004, 2008, 2012])
    {
        assert(yearIsLeapYear(year));
        assert(yearIsLeapYear(-year));
    }
}

@safe unittest
{
    import std.datetime.date;

    import core.time : Duration;
    import std.datetime.interval : Interval;
    import std.datetime.systime : SysTime;

    static assert(isTimePoint!Date);
    static assert(isTimePoint!DateTime);
    static assert(isTimePoint!SysTime);
    static assert(isTimePoint!TimeOfDay);

    static assert(!isTimePoint!int);
    static assert(!isTimePoint!Duration);
    static assert(!isTimePoint!(Interval!SysTime));
}

@safe @nogc nothrow unittest
{
    import std.datetime.date;

    assert(validTimeUnits("msecs", "seconds", "minutes"));
    assert(validTimeUnits("days", "weeks", "months"));
    assert(!validTimeUnits("ms", "seconds", "minutes"));
}

@safe pure unittest
{
    import std.datetime.date;

    import std.exception : assertThrown;

    assert(cmpTimeUnits("hours", "hours") == 0);
    assert(cmpTimeUnits("hours", "weeks") < 0);
    assert(cmpTimeUnits("months", "seconds") > 0);

    assertThrown!DateTimeException(cmpTimeUnits("month", "second"));
}

@safe pure unittest
{
    import std.datetime.date;

    static assert(CmpTimeUnits!("years", "weeks") > 0);
    static assert(CmpTimeUnits!("days", "days") == 0);
    static assert(CmpTimeUnits!("seconds", "hours") < 0);
}


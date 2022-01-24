// Written in the D programming language
/++

$(SCRIPT inhibitQuickIndex = 1;)
$(DIVC quickindex,
$(BOOKTABLE,
$(TR $(TH Category) $(TH Functions))
$(TR $(TD Main date types) $(TD
    $(LREF Date)
    $(LREF DateTime)
))
$(TR $(TD Other date types) $(TD
    $(LREF Month)
    $(LREF DayOfWeek)
    $(LREF TimeOfDay)
))
$(TR $(TD Date checking) $(TD
    $(LREF valid)
    $(LREF validTimeUnits)
    $(LREF yearIsLeapYear)
    $(LREF isTimePoint)
    $(LREF enforceValid)
))
$(TR $(TD Date conversion) $(TD
    $(LREF daysToDayOfWeek)
    $(LREF monthsToMonth)
))
$(TR $(TD Time units) $(TD
    $(LREF cmpTimeUnits)
    $(LREF timeStrings)
))
$(TR $(TD Other) $(TD
    $(LREF AllowDayOverflow)
    $(LREF DateTimeException)
))
))

    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTP jmdavisprog.com, Jonathan M Davis)
    Source:    $(PHOBOSSRC std/datetime/date.d)
+/
module std.datetime.date;

import core.time : TimeException;
import std.traits : isSomeString, Unqual;
import std.typecons : Flag;
import std.range.primitives : isOutputRange;

version (StdUnittest) import std.exception : assertThrown;

@safe unittest
{
    initializeTests();
}


/++
    Exception type used by std.datetime. It's an alias to
    $(REF TimeException,core,time). Either can be caught without concern about
    which module it came from.
  +/
alias DateTimeException = TimeException;


/++
    Represents the 12 months of the Gregorian year (January is 1).
  +/
enum Month : ubyte
{
    jan = 1, ///
    feb,     ///
    mar,     ///
    apr,     ///
    may,     ///
    jun,     ///
    jul,     ///
    aug,     ///
    sep,     ///
    oct,     ///
    nov,     ///
    dec      ///
}

///
@safe pure unittest
{
    assert(Date(2018, 10, 1).month == Month.oct);
    assert(DateTime(1, 1, 1).month == Month.jan);
}


/++
    Represents the 7 days of the Gregorian week (Sunday is 0).
  +/
enum DayOfWeek : ubyte
{
    sun = 0, ///
    mon,     ///
    tue,     ///
    wed,     ///
    thu,     ///
    fri,     ///
    sat      ///
}

///
@safe pure unittest
{
    assert(Date(2018, 10, 1).dayOfWeek == DayOfWeek.mon);
    assert(DateTime(5, 5, 5).dayOfWeek == DayOfWeek.thu);
}

/++
    In some date calculations, adding months or years can cause the date to fall
    on a day of the month which is not valid (e.g. February 29th 2001 or
    June 31st 2000). If overflow is allowed (as is the default), then the month
    will be incremented accordingly (so, February 29th 2001 would become
    March 1st 2001, and June 31st 2000 would become July 1st 2000). If overflow
    is not allowed, then the day will be adjusted to the last valid day in that
    month (so, February 29th 2001 would become February 28th 2001 and
    June 31st 2000 would become June 30th 2000).

    AllowDayOverflow only applies to calculations involving months or years.

    If set to `AllowDayOverflow.no`, then day overflow is not allowed.

    Otherwise, if set to `AllowDayOverflow.yes`, then day overflow is
    allowed.
  +/
alias AllowDayOverflow = Flag!"allowDayOverflow";


/++
    Array of the strings representing time units, starting with the smallest
    unit and going to the largest. It does not include `"nsecs"`.

    Includes `"hnsecs"` (hecto-nanoseconds (100 ns)),
    `"usecs"` (microseconds), `"msecs"` (milliseconds), `"seconds"`,
    `"minutes"`, `"hours"`, `"days"`, `"weeks"`, `"months"`, and
    `"years"`
  +/
immutable string[] timeStrings = ["hnsecs", "usecs", "msecs", "seconds", "minutes",
                                  "hours", "days", "weeks", "months", "years"];


/++
    Combines the $(REF Date,std,datetime,date) and
    $(REF TimeOfDay,std,datetime,date) structs to give an object which holds
    both the date and the time. It is optimized for calendar-based operations
    and has no concept of time zone. For an object which is optimized for time
    operations based on the system time, use
    $(REF SysTime,std,datetime,systime). $(REF SysTime,std,datetime,systime) has
    a concept of time zone and has much higher precision (hnsecs). `DateTime`
    is intended primarily for calendar-based uses rather than precise time
    operations.
  +/
struct DateTime
{
public:

    /++
        Params:
            date = The date portion of $(LREF DateTime).
            tod  = The time portion of $(LREF DateTime).
      +/
    this(Date date, TimeOfDay tod = TimeOfDay.init) @safe pure nothrow @nogc
    {
        _date = date;
        _tod = tod;
    }

    @safe unittest
    {
        {
            auto dt = DateTime.init;
            assert(dt._date == Date.init);
            assert(dt._tod == TimeOfDay.init);
        }

        {
            auto dt = DateTime(Date(1999, 7 ,6));
            assert(dt._date == Date(1999, 7, 6));
            assert(dt._tod == TimeOfDay.init);
        }

        {
            auto dt = DateTime(Date(1999, 7 ,6), TimeOfDay(12, 30, 33));
            assert(dt._date == Date(1999, 7, 6));
            assert(dt._tod == TimeOfDay(12, 30, 33));
        }
    }


    /++
        Params:
            year   = The year portion of the date.
            month  = The month portion of the date (January is 1).
            day    = The day portion of the date.
            hour   = The hour portion of the time;
            minute = The minute portion of the time;
            second = The second portion of the time;
      +/
    this(int year, int month, int day, int hour = 0, int minute = 0, int second = 0) @safe pure
    {
        _date = Date(year, month, day);
        _tod = TimeOfDay(hour, minute, second);
    }

    @safe unittest
    {
        {
            auto dt = DateTime(1999, 7 ,6);
            assert(dt._date == Date(1999, 7, 6));
            assert(dt._tod == TimeOfDay.init);
        }

        {
            auto dt = DateTime(1999, 7 ,6, 12, 30, 33);
            assert(dt._date == Date(1999, 7, 6));
            assert(dt._tod == TimeOfDay(12, 30, 33));
        }
    }


    /++
        Compares this $(LREF DateTime) with the given `DateTime.`.

        Returns:
            $(BOOKTABLE,
            $(TR $(TD this &lt; rhs) $(TD &lt; 0))
            $(TR $(TD this == rhs) $(TD 0))
            $(TR $(TD this &gt; rhs) $(TD &gt; 0))
            )
     +/
    int opCmp(DateTime rhs) const @safe pure nothrow @nogc
    {
        immutable dateResult = _date.opCmp(rhs._date);

        if (dateResult != 0)
            return dateResult;

        return _tod.opCmp(rhs._tod);
    }

    @safe unittest
    {
        // Test A.D.
        assert(DateTime(Date.init, TimeOfDay.init).opCmp(DateTime.init) == 0);

        assert(DateTime(Date(1999, 1, 1)).opCmp(DateTime(Date(1999, 1, 1))) == 0);
        assert(DateTime(Date(1, 7, 1)).opCmp(DateTime(Date(1, 7, 1))) == 0);
        assert(DateTime(Date(1, 1, 6)).opCmp(DateTime(Date(1, 1, 6))) == 0);

        assert(DateTime(Date(1999, 7, 1)).opCmp(DateTime(Date(1999, 7, 1))) == 0);
        assert(DateTime(Date(1999, 7, 6)).opCmp(DateTime(Date(1999, 7, 6))) == 0);

        assert(DateTime(Date(1, 7, 6)).opCmp(DateTime(Date(1, 7, 6))) == 0);

        assert(DateTime(Date(1999, 7, 6)).opCmp(DateTime(Date(2000, 7, 6))) < 0);
        assert(DateTime(Date(2000, 7, 6)).opCmp(DateTime(Date(1999, 7, 6))) > 0);
        assert(DateTime(Date(1999, 7, 6)).opCmp(DateTime(Date(1999, 8, 6))) < 0);
        assert(DateTime(Date(1999, 8, 6)).opCmp(DateTime(Date(1999, 7, 6))) > 0);
        assert(DateTime(Date(1999, 7, 6)).opCmp(DateTime(Date(1999, 7, 7))) < 0);
        assert(DateTime(Date(1999, 7, 7)).opCmp(DateTime(Date(1999, 7, 6))) > 0);

        assert(DateTime(Date(1999, 8, 7)).opCmp(DateTime(Date(2000, 7, 6))) < 0);
        assert(DateTime(Date(2000, 8, 6)).opCmp(DateTime(Date(1999, 7, 7))) > 0);
        assert(DateTime(Date(1999, 7, 7)).opCmp(DateTime(Date(2000, 7, 6))) < 0);
        assert(DateTime(Date(2000, 7, 6)).opCmp(DateTime(Date(1999, 7, 7))) > 0);
        assert(DateTime(Date(1999, 7, 7)).opCmp(DateTime(Date(1999, 8, 6))) < 0);
        assert(DateTime(Date(1999, 8, 6)).opCmp(DateTime(Date(1999, 7, 7))) > 0);


        assert(DateTime(Date(1999, 7, 6), TimeOfDay(0, 0, 0)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(0, 0, 0))) == 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 0, 0)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(12, 0, 0))) == 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(0, 30, 0)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(0, 30, 0))) == 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(0, 0, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(0, 0, 33))) == 0);

        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 0)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 0))) == 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33))) == 0);

        assert(DateTime(Date(1999, 7, 6), TimeOfDay(0, 30, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(0, 30, 33))) == 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(0, 0, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(0, 0, 33))) == 0);

        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(13, 30, 33))) < 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(13, 30, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33))) > 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(12, 31, 33))) < 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 31, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33))) > 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 34))) < 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 34)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33))) > 0);

        assert(DateTime(Date(1999, 7, 6), TimeOfDay(13, 30, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 34))) > 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 34)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(13, 30, 33))) < 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(13, 30, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(12, 31, 33))) > 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 31, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(13, 30, 33))) < 0);

        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 31, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 34))) > 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 34)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(12, 31, 33))) < 0);

        assert(DateTime(Date(1999, 7, 6), TimeOfDay(13, 30, 33)).opCmp(
                   DateTime(Date(2000, 7, 6), TimeOfDay(12, 30, 33))) < 0);
        assert(DateTime(Date(2000, 7, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(13, 30, 33))) > 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 31, 33)).opCmp(
                   DateTime(Date(2000, 7, 6), TimeOfDay(12, 30, 33))) < 0);
        assert(DateTime(Date(2000, 7, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(12, 31, 33))) > 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 34)).opCmp(
                   DateTime(Date(2000, 7, 6), TimeOfDay(12, 30, 33))) < 0);
        assert(DateTime(Date(2000, 7, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 34))) > 0);

        assert(DateTime(Date(1999, 7, 6), TimeOfDay(13, 30, 33)).opCmp(
                   DateTime(Date(1999, 8, 6), TimeOfDay(12, 30, 33))) < 0);
        assert(DateTime(Date(1999, 8, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(13, 30, 33))) > 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 31, 33)).opCmp(
                   DateTime(Date(1999, 8, 6), TimeOfDay(12, 30, 33))) < 0);
        assert(DateTime(Date(1999, 8, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(12, 31, 33))) > 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 34)).opCmp(
                   DateTime(Date(1999, 8, 6), TimeOfDay(12, 30, 33))) < 0);
        assert(DateTime(Date(1999, 8, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 34))) > 0);

        assert(DateTime(Date(1999, 7, 6), TimeOfDay(13, 30, 33)).opCmp(
                   DateTime(Date(1999, 7, 7), TimeOfDay(12, 30, 33))) < 0);
        assert(DateTime(Date(1999, 7, 7), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(13, 30, 33))) > 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 31, 33)).opCmp(
                   DateTime(Date(1999, 7, 7), TimeOfDay(12, 31, 33))) < 0);
        assert(DateTime(Date(1999, 7, 7), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33))) > 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 34)).opCmp(
                   DateTime(Date(1999, 7, 7), TimeOfDay(12, 30, 33))) < 0);
        assert(DateTime(Date(1999, 7, 7), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 34))) > 0);

        // Test B.C.
        assert(DateTime(Date(-1, 1, 1), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-1, 1, 1), TimeOfDay(12, 30, 33))) == 0);
        assert(DateTime(Date(-1, 7, 1), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-1, 7, 1), TimeOfDay(12, 30, 33))) == 0);
        assert(DateTime(Date(-1, 1, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-1, 1, 6), TimeOfDay(12, 30, 33))) == 0);

        assert(DateTime(Date(-1999, 7, 1), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-1999, 7, 1), TimeOfDay(12, 30, 33))) == 0);
        assert(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33))) == 0);

        assert(DateTime(Date(-1, 7, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-1, 7, 6), TimeOfDay(12, 30, 33))) == 0);

        assert(DateTime(Date(-2000, 7, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33))) < 0);
        assert(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-2000, 7, 6), TimeOfDay(12, 30, 33))) > 0);
        assert(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-1999, 8, 6), TimeOfDay(12, 30, 33))) < 0);
        assert(DateTime(Date(-1999, 8, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33))) > 0);
        assert(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-1999, 7, 7), TimeOfDay(12, 30, 33))) < 0);
        assert(DateTime(Date(-1999, 7, 7), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33))) > 0);

        assert(DateTime(Date(-2000, 8, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-1999, 7, 7), TimeOfDay(12, 30, 33))) < 0);
        assert(DateTime(Date(-1999, 8, 7), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-2000, 7, 6), TimeOfDay(12, 30, 33))) > 0);
        assert(DateTime(Date(-2000, 7, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-1999, 7, 7), TimeOfDay(12, 30, 33))) < 0);
        assert(DateTime(Date(-1999, 7, 7), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-2000, 7, 6), TimeOfDay(12, 30, 33))) > 0);
        assert(DateTime(Date(-1999, 7, 7), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-1999, 8, 6), TimeOfDay(12, 30, 33))) < 0);
        assert(DateTime(Date(-1999, 8, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-1999, 7, 7), TimeOfDay(12, 30, 33))) > 0);

        // Test Both
        assert(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33))) < 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33))) > 0);

        assert(DateTime(Date(-1999, 8, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33))) < 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-1999, 8, 6), TimeOfDay(12, 30, 33))) > 0);

        assert(DateTime(Date(-1999, 7, 7), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33))) < 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-1999, 7, 7), TimeOfDay(12, 30, 33))) > 0);

        assert(DateTime(Date(-1999, 8, 7), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33))) < 0);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-1999, 8, 7), TimeOfDay(12, 30, 33))) > 0);

        assert(DateTime(Date(-1999, 8, 6), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(1999, 6, 6), TimeOfDay(12, 30, 33))) < 0);
        assert(DateTime(Date(1999, 6, 8), TimeOfDay(12, 30, 33)).opCmp(
                   DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33))) > 0);

        auto dt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 33, 30));
        const cdt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 33, 30));
        immutable idt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 33, 30));
        assert(dt.opCmp(dt) == 0);
        assert(dt.opCmp(cdt) == 0);
        assert(dt.opCmp(idt) == 0);
        assert(cdt.opCmp(dt) == 0);
        assert(cdt.opCmp(cdt) == 0);
        assert(cdt.opCmp(idt) == 0);
        assert(idt.opCmp(dt) == 0);
        assert(idt.opCmp(cdt) == 0);
        assert(idt.opCmp(idt) == 0);
    }


    /++
        The date portion of $(LREF DateTime).
      +/
    @property Date date() const @safe pure nothrow @nogc
    {
        return _date;
    }

    @safe unittest
    {
        {
            auto dt = DateTime.init;
            assert(dt.date == Date.init);
        }

        {
            auto dt = DateTime(Date(1999, 7, 6));
            assert(dt.date == Date(1999, 7, 6));
        }

        const cdt = DateTime(1999, 7, 6);
        immutable idt = DateTime(1999, 7, 6);
        assert(cdt.date == Date(1999, 7, 6));
        assert(idt.date == Date(1999, 7, 6));
    }


    /++
        The date portion of $(LREF DateTime).

        Params:
            date = The Date to set this $(LREF DateTime)'s date portion to.
      +/
    @property void date(Date date) @safe pure nothrow @nogc
    {
        _date = date;
    }

    @safe unittest
    {
        auto dt = DateTime.init;
        dt.date = Date(1999, 7, 6);
        assert(dt._date == Date(1999, 7, 6));
        assert(dt._tod == TimeOfDay.init);

        const cdt = DateTime(1999, 7, 6);
        immutable idt = DateTime(1999, 7, 6);
        static assert(!__traits(compiles, cdt.date = Date(2010, 1, 1)));
        static assert(!__traits(compiles, idt.date = Date(2010, 1, 1)));
    }


    /++
        The time portion of $(LREF DateTime).
      +/
    @property TimeOfDay timeOfDay() const @safe pure nothrow @nogc
    {
        return _tod;
    }

    @safe unittest
    {
        {
            auto dt = DateTime.init;
            assert(dt.timeOfDay == TimeOfDay.init);
        }

        {
            auto dt = DateTime(Date.init, TimeOfDay(12, 30, 33));
            assert(dt.timeOfDay == TimeOfDay(12, 30, 33));
        }

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        assert(cdt.timeOfDay == TimeOfDay(12, 30, 33));
        assert(idt.timeOfDay == TimeOfDay(12, 30, 33));
    }


    /++
        The time portion of $(LREF DateTime).

        Params:
            tod = The $(REF TimeOfDay,std,datetime,date) to set this
                  $(LREF DateTime)'s time portion to.
      +/
    @property void timeOfDay(TimeOfDay tod) @safe pure nothrow @nogc
    {
        _tod = tod;
    }

    @safe unittest
    {
        auto dt = DateTime.init;
        dt.timeOfDay = TimeOfDay(12, 30, 33);
        assert(dt._date == Date.init);
        assert(dt._tod == TimeOfDay(12, 30, 33));

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        static assert(!__traits(compiles, cdt.timeOfDay = TimeOfDay(12, 30, 33)));
        static assert(!__traits(compiles, idt.timeOfDay = TimeOfDay(12, 30, 33)));
    }


    /++
        Year of the Gregorian Calendar. Positive numbers are A.D. Non-positive
        are B.C.
     +/
    @property short year() const @safe pure nothrow @nogc
    {
        return _date.year;
    }

    @safe unittest
    {
        assert(Date.init.year == 1);
        assert(Date(1999, 7, 6).year == 1999);
        assert(Date(-1999, 7, 6).year == -1999);

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        assert(idt.year == 1999);
        assert(idt.year == 1999);
    }


    /++
        Year of the Gregorian Calendar. Positive numbers are A.D. Non-positive
        are B.C.

        Params:
            year = The year to set this $(LREF DateTime)'s year to.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the new year is not
            a leap year and if the resulting date would be on February 29th.
     +/
    @property void year(int year) @safe pure
    {
        _date.year = year;
    }

    ///
    @safe unittest
    {
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(9, 7, 5)).year == 1999);
        assert(DateTime(Date(2010, 10, 4), TimeOfDay(0, 0, 30)).year == 2010);
        assert(DateTime(Date(-7, 4, 5), TimeOfDay(7, 45, 2)).year == -7);
    }

    @safe unittest
    {
        static void testDT(DateTime dt, int year, DateTime expected, size_t line = __LINE__)
        {
            dt.year = year;
            assert(dt == expected);
        }

        testDT(DateTime(Date(1, 1, 1), TimeOfDay(12, 30, 33)),
               1999,
               DateTime(Date(1999, 1, 1), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(1, 1, 1), TimeOfDay(12, 30, 33)),
               0,
               DateTime(Date(0, 1, 1), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(1, 1, 1), TimeOfDay(12, 30, 33)),
               -1999,
               DateTime(Date(-1999, 1, 1), TimeOfDay(12, 30, 33)));

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        static assert(!__traits(compiles, cdt.year = 7));
        static assert(!__traits(compiles, idt.year = 7));
    }


    /++
        Year B.C. of the Gregorian Calendar counting year 0 as 1 B.C.

        Throws:
            $(REF DateTimeException,std,datetime,date) if `isAD` is true.
     +/
    @property short yearBC() const @safe pure
    {
        return _date.yearBC;
    }

    ///
    @safe unittest
    {
        assert(DateTime(Date(0, 1, 1), TimeOfDay(12, 30, 33)).yearBC == 1);
        assert(DateTime(Date(-1, 1, 1), TimeOfDay(10, 7, 2)).yearBC == 2);
        assert(DateTime(Date(-100, 1, 1), TimeOfDay(4, 59, 0)).yearBC == 101);
    }

    @safe unittest
    {
        assertThrown!DateTimeException((DateTime dt){dt.yearBC;}(DateTime(Date(1, 1, 1))));

        auto dt = DateTime(1999, 7, 6, 12, 30, 33);
        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        dt.yearBC = 12;
        assert(dt.yearBC == 12);
        static assert(!__traits(compiles, cdt.yearBC = 12));
        static assert(!__traits(compiles, idt.yearBC = 12));
    }


    /++
        Year B.C. of the Gregorian Calendar counting year 0 as 1 B.C.

        Params:
            year = The year B.C. to set this $(LREF DateTime)'s year to.

        Throws:
            $(REF DateTimeException,std,datetime,date) if a non-positive value
            is given.
     +/
    @property void yearBC(int year) @safe pure
    {
        _date.yearBC = year;
    }

    ///
    @safe unittest
    {
        auto dt = DateTime(Date(2010, 1, 1), TimeOfDay(7, 30, 0));
        dt.yearBC = 1;
        assert(dt == DateTime(Date(0, 1, 1), TimeOfDay(7, 30, 0)));

        dt.yearBC = 10;
        assert(dt == DateTime(Date(-9, 1, 1), TimeOfDay(7, 30, 0)));
    }

    @safe unittest
    {
        assertThrown!DateTimeException((DateTime dt){dt.yearBC = -1;}(DateTime(Date(1, 1, 1))));

        auto dt = DateTime(1999, 7, 6, 12, 30, 33);
        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        dt.yearBC = 12;
        assert(dt.yearBC == 12);
        static assert(!__traits(compiles, cdt.yearBC = 12));
        static assert(!__traits(compiles, idt.yearBC = 12));
    }


    /++
        Month of a Gregorian Year.
     +/
    @property Month month() const @safe pure nothrow @nogc
    {
        return _date.month;
    }

    ///
    @safe unittest
    {
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(9, 7, 5)).month == 7);
        assert(DateTime(Date(2010, 10, 4), TimeOfDay(0, 0, 30)).month == 10);
        assert(DateTime(Date(-7, 4, 5), TimeOfDay(7, 45, 2)).month == 4);
    }

    @safe unittest
    {
        assert(DateTime.init.month == 1);
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)).month == 7);
        assert(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)).month == 7);

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        assert(cdt.month == 7);
        assert(idt.month == 7);
    }


    /++
        Month of a Gregorian Year.

        Params:
            month = The month to set this $(LREF DateTime)'s month to.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given month is
            not a valid month.
     +/
    @property void month(Month month) @safe pure
    {
        _date.month = month;
    }

    @safe unittest
    {
        static void testDT(DateTime dt, Month month, DateTime expected = DateTime.init, size_t line = __LINE__)
        {
            dt.month = month;
            assert(expected != DateTime.init);
            assert(dt == expected);
        }

        assertThrown!DateTimeException(testDT(DateTime(Date(1, 1, 1), TimeOfDay(12, 30, 33)), cast(Month) 0));
        assertThrown!DateTimeException(testDT(DateTime(Date(1, 1, 1), TimeOfDay(12, 30, 33)), cast(Month) 13));

        testDT(DateTime(Date(1, 1, 1), TimeOfDay(12, 30, 33)),
               cast(Month) 7,
               DateTime(Date(1, 7, 1), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(-1, 1, 1), TimeOfDay(12, 30, 33)),
               cast(Month) 7,
               DateTime(Date(-1, 7, 1), TimeOfDay(12, 30, 33)));

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        static assert(!__traits(compiles, cdt.month = 12));
        static assert(!__traits(compiles, idt.month = 12));
    }


    /++
        Day of a Gregorian Month.
     +/
    @property ubyte day() const @safe pure nothrow @nogc
    {
        return _date.day;
    }

    ///
    @safe unittest
    {
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(9, 7, 5)).day == 6);
        assert(DateTime(Date(2010, 10, 4), TimeOfDay(0, 0, 30)).day == 4);
        assert(DateTime(Date(-7, 4, 5), TimeOfDay(7, 45, 2)).day == 5);
    }

    @safe unittest
    {
        import std.format : format;
        import std.range : chain;

        static void test(DateTime dateTime, int expected)
        {
            assert(dateTime.day == expected, format("Value given: %s", dateTime));
        }

        foreach (year; chain(testYearsBC, testYearsAD))
        {
            foreach (md; testMonthDays)
            {
                foreach (tod; testTODs)
                    test(DateTime(Date(year, md.month, md.day), tod), md.day);
            }
        }

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        assert(cdt.day == 6);
        assert(idt.day == 6);
    }


    /++
        Day of a Gregorian Month.

        Params:
            day = The day of the month to set this $(LREF DateTime)'s day to.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given day is not
            a valid day of the current month.
     +/
    @property void day(int day) @safe pure
    {
        _date.day = day;
    }

    @safe unittest
    {
        import std.exception : assertNotThrown;

        static void testDT(DateTime dt, int day)
        {
            dt.day = day;
        }

        // Test A.D.
        assertThrown!DateTimeException(testDT(DateTime(Date(1, 1, 1)), 0));
        assertThrown!DateTimeException(testDT(DateTime(Date(1, 1, 1)), 32));
        assertThrown!DateTimeException(testDT(DateTime(Date(1, 2, 1)), 29));
        assertThrown!DateTimeException(testDT(DateTime(Date(4, 2, 1)), 30));
        assertThrown!DateTimeException(testDT(DateTime(Date(1, 3, 1)), 32));
        assertThrown!DateTimeException(testDT(DateTime(Date(1, 4, 1)), 31));
        assertThrown!DateTimeException(testDT(DateTime(Date(1, 5, 1)), 32));
        assertThrown!DateTimeException(testDT(DateTime(Date(1, 6, 1)), 31));
        assertThrown!DateTimeException(testDT(DateTime(Date(1, 7, 1)), 32));
        assertThrown!DateTimeException(testDT(DateTime(Date(1, 8, 1)), 32));
        assertThrown!DateTimeException(testDT(DateTime(Date(1, 9, 1)), 31));
        assertThrown!DateTimeException(testDT(DateTime(Date(1, 10, 1)), 32));
        assertThrown!DateTimeException(testDT(DateTime(Date(1, 11, 1)), 31));
        assertThrown!DateTimeException(testDT(DateTime(Date(1, 12, 1)), 32));

        assertNotThrown!DateTimeException(testDT(DateTime(Date(1, 1, 1)), 31));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(1, 2, 1)), 28));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(4, 2, 1)), 29));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(1, 3, 1)), 31));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(1, 4, 1)), 30));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(1, 5, 1)), 31));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(1, 6, 1)), 30));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(1, 7, 1)), 31));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(1, 8, 1)), 31));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(1, 9, 1)), 30));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(1, 10, 1)), 31));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(1, 11, 1)), 30));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(1, 12, 1)), 31));

        {
            auto dt = DateTime(Date(1, 1, 1), TimeOfDay(7, 12, 22));
            dt.day = 6;
            assert(dt == DateTime(Date(1, 1, 6), TimeOfDay(7, 12, 22)));
        }

        // Test B.C.
        assertThrown!DateTimeException(testDT(DateTime(Date(-1, 1, 1)), 0));
        assertThrown!DateTimeException(testDT(DateTime(Date(-1, 1, 1)), 32));
        assertThrown!DateTimeException(testDT(DateTime(Date(-1, 2, 1)), 29));
        assertThrown!DateTimeException(testDT(DateTime(Date(0, 2, 1)), 30));
        assertThrown!DateTimeException(testDT(DateTime(Date(-1, 3, 1)), 32));
        assertThrown!DateTimeException(testDT(DateTime(Date(-1, 4, 1)), 31));
        assertThrown!DateTimeException(testDT(DateTime(Date(-1, 5, 1)), 32));
        assertThrown!DateTimeException(testDT(DateTime(Date(-1, 6, 1)), 31));
        assertThrown!DateTimeException(testDT(DateTime(Date(-1, 7, 1)), 32));
        assertThrown!DateTimeException(testDT(DateTime(Date(-1, 8, 1)), 32));
        assertThrown!DateTimeException(testDT(DateTime(Date(-1, 9, 1)), 31));
        assertThrown!DateTimeException(testDT(DateTime(Date(-1, 10, 1)), 32));
        assertThrown!DateTimeException(testDT(DateTime(Date(-1, 11, 1)), 31));
        assertThrown!DateTimeException(testDT(DateTime(Date(-1, 12, 1)), 32));

        assertNotThrown!DateTimeException(testDT(DateTime(Date(-1, 1, 1)), 31));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(-1, 2, 1)), 28));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(0, 2, 1)), 29));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(-1, 3, 1)), 31));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(-1, 4, 1)), 30));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(-1, 5, 1)), 31));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(-1, 6, 1)), 30));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(-1, 7, 1)), 31));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(-1, 8, 1)), 31));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(-1, 9, 1)), 30));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(-1, 10, 1)), 31));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(-1, 11, 1)), 30));
        assertNotThrown!DateTimeException(testDT(DateTime(Date(-1, 12, 1)), 31));

        auto dt = DateTime(Date(-1, 1, 1), TimeOfDay(7, 12, 22));
        dt.day = 6;
        assert(dt == DateTime(Date(-1, 1, 6), TimeOfDay(7, 12, 22)));

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        static assert(!__traits(compiles, cdt.day = 27));
        static assert(!__traits(compiles, idt.day = 27));
    }


    /++
        Hours past midnight.
     +/
    @property ubyte hour() const @safe pure nothrow @nogc
    {
        return _tod.hour;
    }

    @safe unittest
    {
        assert(DateTime.init.hour == 0);
        assert(DateTime(Date.init, TimeOfDay(12, 0, 0)).hour == 12);

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        assert(cdt.hour == 12);
        assert(idt.hour == 12);
    }


    /++
        Hours past midnight.

        Params:
            hour = The hour of the day to set this $(LREF DateTime)'s hour to.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given hour would
            result in an invalid $(LREF DateTime).
     +/
    @property void hour(int hour) @safe pure
    {
        _tod.hour = hour;
    }

    @safe unittest
    {
        assertThrown!DateTimeException((){DateTime(Date(1999, 7, 6), TimeOfDay(0, 0, 0)).hour = 24;}());

        auto dt = DateTime.init;
        dt.hour = 12;
        assert(dt == DateTime(1, 1, 1, 12, 0, 0));

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        static assert(!__traits(compiles, cdt.hour = 27));
        static assert(!__traits(compiles, idt.hour = 27));
    }


    /++
        Minutes past the hour.
     +/
    @property ubyte minute() const @safe pure nothrow @nogc
    {
        return _tod.minute;
    }

    @safe unittest
    {
        assert(DateTime.init.minute == 0);
        assert(DateTime(1, 1, 1, 0, 30, 0).minute == 30);

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        assert(cdt.minute == 30);
        assert(idt.minute == 30);
    }


    /++
        Minutes past the hour.

        Params:
            minute = The minute to set this $(LREF DateTime)'s minute to.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given minute
            would result in an invalid $(LREF DateTime).
     +/
    @property void minute(int minute) @safe pure
    {
        _tod.minute = minute;
    }

    @safe unittest
    {
        assertThrown!DateTimeException((){DateTime.init.minute = 60;}());

        auto dt = DateTime.init;
        dt.minute = 30;
        assert(dt == DateTime(1, 1, 1, 0, 30, 0));

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        static assert(!__traits(compiles, cdt.minute = 27));
        static assert(!__traits(compiles, idt.minute = 27));
    }


    /++
        Seconds past the minute.
     +/
    @property ubyte second() const @safe pure nothrow @nogc
    {
        return _tod.second;
    }

    @safe unittest
    {
        assert(DateTime.init.second == 0);
        assert(DateTime(1, 1, 1, 0, 0, 33).second == 33);

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        assert(cdt.second == 33);
        assert(idt.second == 33);
    }


    /++
        Seconds past the minute.

        Params:
            second = The second to set this $(LREF DateTime)'s second to.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given seconds
            would result in an invalid $(LREF DateTime).
     +/
    @property void second(int second) @safe pure
    {
        _tod.second = second;
    }

    @safe unittest
    {
        assertThrown!DateTimeException((){DateTime.init.second = 60;}());

        auto dt = DateTime.init;
        dt.second = 33;
        assert(dt == DateTime(1, 1, 1, 0, 0, 33));

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        static assert(!__traits(compiles, cdt.second = 27));
        static assert(!__traits(compiles, idt.second = 27));
    }


    /++
        Adds the given number of years or months to this $(LREF DateTime),
        mutating it. A negative number will subtract.

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
                            $(LREF DateTime).
            allowOverflow = Whether the days should be allowed to overflow,
                            causing the month to increment.

        Returns:
            A reference to the `DateTime` (`this`).
      +/
    ref DateTime add(string units)
                    (long value, AllowDayOverflow allowOverflow = AllowDayOverflow.yes) @safe pure nothrow @nogc
        if (units == "years" || units == "months")
    {
        _date.add!units(value, allowOverflow);
        return this;
    }

    ///
    @safe unittest
    {
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
        auto dt = DateTime(2000, 1, 31);
        dt.add!"years"(7).add!"months"(-4);
        assert(dt == DateTime(2006, 10, 1));

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        static assert(!__traits(compiles, cdt.add!"years"(4)));
        static assert(!__traits(compiles, idt.add!"years"(4)));
        static assert(!__traits(compiles, cdt.add!"months"(4)));
        static assert(!__traits(compiles, idt.add!"months"(4)));
    }


    /++
        Adds the given number of years or months to this $(LREF DateTime),
        mutating it. A negative number will subtract.

        The difference between rolling and adding is that rolling does not
        affect larger units. Rolling a $(LREF DateTime) 12 months
        gets the exact same $(LREF DateTime). However, the days can still be
        affected due to the differing number of days in each month.

        Because there are no units larger than years, there is no difference
        between adding and rolling years.

        Params:
            units         = The type of units to add ("years" or "months").
            value         = The number of months or years to add to this
                            $(LREF DateTime).
            allowOverflow = Whether the days should be allowed to overflow,
                            causing the month to increment.

        Returns:
            A reference to the `DateTime` (`this`).
      +/
    ref DateTime roll(string units)
                     (long value, AllowDayOverflow allowOverflow = AllowDayOverflow.yes) @safe pure nothrow @nogc
        if (units == "years" || units == "months")
    {
        _date.roll!units(value, allowOverflow);
        return this;
    }

    ///
    @safe unittest
    {
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
        auto dt = DateTime(2000, 1, 31);
        dt.roll!"years"(7).roll!"months"(-4);
        assert(dt == DateTime(2007, 10, 1));

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        static assert(!__traits(compiles, cdt.roll!"years"(4)));
        static assert(!__traits(compiles, idt.roll!"years"(4)));
        static assert(!__traits(compiles, cdt.roll!"months"(4)));
        static assert(!__traits(compiles, idt.roll!"months"(4)));
    }


    /++
        Adds the given number of units to this $(LREF DateTime), mutating it. A
        negative number will subtract.

        The difference between rolling and adding is that rolling does not
        affect larger units. For instance, rolling a $(LREF DateTime) one
        year's worth of days gets the exact same $(LREF DateTime).

        Accepted units are `"days"`, `"minutes"`, `"hours"`,
        `"minutes"`, and `"seconds"`.

        Params:
            units = The units to add.
            value = The number of $(D_PARAM units) to add to this
                    $(LREF DateTime).

        Returns:
            A reference to the `DateTime` (`this`).
      +/
    ref DateTime roll(string units)(long value) @safe pure nothrow @nogc
        if (units == "days")
    {
        _date.roll!"days"(value);
        return this;
    }

    ///
    @safe unittest
    {
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
        auto dt = DateTime(2000, 1, 31);
        dt.roll!"days"(7).roll!"days"(-4);
        assert(dt == DateTime(2000, 1, 3));

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        static assert(!__traits(compiles, cdt.roll!"days"(4)));
        static assert(!__traits(compiles, idt.roll!"days"(4)));
    }


    /// ditto
    ref DateTime roll(string units)(long value) @safe pure nothrow @nogc
        if (units == "hours" ||
            units == "minutes" ||
            units == "seconds")
    {
        _tod.roll!units(value);
        return this;
    }

    // Test roll!"hours"().
    @safe unittest
    {
        static void testDT(DateTime orig, int hours, DateTime expected, size_t line = __LINE__)
        {
            orig.roll!"hours"(hours);
            assert(orig == expected);
        }

        // Test A.D.
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 0,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 1,
               DateTime(Date(1999, 7, 6), TimeOfDay(13, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 2,
               DateTime(Date(1999, 7, 6), TimeOfDay(14, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 3,
               DateTime(Date(1999, 7, 6), TimeOfDay(15, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 4,
               DateTime(Date(1999, 7, 6), TimeOfDay(16, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 5,
               DateTime(Date(1999, 7, 6), TimeOfDay(17, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 6,
               DateTime(Date(1999, 7, 6), TimeOfDay(18, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 7,
               DateTime(Date(1999, 7, 6), TimeOfDay(19, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 8,
               DateTime(Date(1999, 7, 6), TimeOfDay(20, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 9,
               DateTime(Date(1999, 7, 6), TimeOfDay(21, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 10,
               DateTime(Date(1999, 7, 6), TimeOfDay(22, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 11,
               DateTime(Date(1999, 7, 6), TimeOfDay(23, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 12,
               DateTime(Date(1999, 7, 6), TimeOfDay(0, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 13,
               DateTime(Date(1999, 7, 6), TimeOfDay(1, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 14,
               DateTime(Date(1999, 7, 6), TimeOfDay(2, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 15,
               DateTime(Date(1999, 7, 6), TimeOfDay(3, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 16,
               DateTime(Date(1999, 7, 6), TimeOfDay(4, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 17,
               DateTime(Date(1999, 7, 6), TimeOfDay(5, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 18,
               DateTime(Date(1999, 7, 6), TimeOfDay(6, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 19,
               DateTime(Date(1999, 7, 6), TimeOfDay(7, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 20,
               DateTime(Date(1999, 7, 6), TimeOfDay(8, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 21,
               DateTime(Date(1999, 7, 6), TimeOfDay(9, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 22,
               DateTime(Date(1999, 7, 6), TimeOfDay(10, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 23,
               DateTime(Date(1999, 7, 6), TimeOfDay(11, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 24,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 25,
               DateTime(Date(1999, 7, 6), TimeOfDay(13, 30, 33)));

        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -1,
               DateTime(Date(1999, 7, 6), TimeOfDay(11, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -2,
               DateTime(Date(1999, 7, 6), TimeOfDay(10, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -3,
               DateTime(Date(1999, 7, 6), TimeOfDay(9, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -4,
               DateTime(Date(1999, 7, 6), TimeOfDay(8, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -5,
               DateTime(Date(1999, 7, 6), TimeOfDay(7, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -6,
               DateTime(Date(1999, 7, 6), TimeOfDay(6, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -7,
               DateTime(Date(1999, 7, 6), TimeOfDay(5, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -8,
               DateTime(Date(1999, 7, 6), TimeOfDay(4, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -9,
               DateTime(Date(1999, 7, 6), TimeOfDay(3, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -10,
               DateTime(Date(1999, 7, 6), TimeOfDay(2, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -11,
               DateTime(Date(1999, 7, 6), TimeOfDay(1, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -12,
               DateTime(Date(1999, 7, 6), TimeOfDay(0, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -13,
               DateTime(Date(1999, 7, 6), TimeOfDay(23, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -14,
               DateTime(Date(1999, 7, 6), TimeOfDay(22, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -15,
               DateTime(Date(1999, 7, 6), TimeOfDay(21, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -16,
               DateTime(Date(1999, 7, 6), TimeOfDay(20, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -17,
               DateTime(Date(1999, 7, 6), TimeOfDay(19, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -18,
               DateTime(Date(1999, 7, 6), TimeOfDay(18, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -19,
               DateTime(Date(1999, 7, 6), TimeOfDay(17, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -20,
               DateTime(Date(1999, 7, 6), TimeOfDay(16, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -21,
               DateTime(Date(1999, 7, 6), TimeOfDay(15, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -22,
               DateTime(Date(1999, 7, 6), TimeOfDay(14, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -23,
               DateTime(Date(1999, 7, 6), TimeOfDay(13, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -24,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -25,
               DateTime(Date(1999, 7, 6), TimeOfDay(11, 30, 33)));

        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(0, 30, 33)), 1,
               DateTime(Date(1999, 7, 6), TimeOfDay(1, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(0, 30, 33)), 0,
               DateTime(Date(1999, 7, 6), TimeOfDay(0, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(0, 30, 33)), -1,
               DateTime(Date(1999, 7, 6), TimeOfDay(23, 30, 33)));

        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(23, 30, 33)), 1,
               DateTime(Date(1999, 7, 6), TimeOfDay(0, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(23, 30, 33)), 0,
               DateTime(Date(1999, 7, 6), TimeOfDay(23, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(23, 30, 33)), -1,
               DateTime(Date(1999, 7, 6), TimeOfDay(22, 30, 33)));

        testDT(DateTime(Date(1999, 7, 31), TimeOfDay(23, 30, 33)), 1,
               DateTime(Date(1999, 7, 31), TimeOfDay(0, 30, 33)));
        testDT(DateTime(Date(1999, 8, 1), TimeOfDay(0, 30, 33)), -1,
               DateTime(Date(1999, 8, 1), TimeOfDay(23, 30, 33)));

        testDT(DateTime(Date(1999, 12, 31), TimeOfDay(23, 30, 33)), 1,
               DateTime(Date(1999, 12, 31), TimeOfDay(0, 30, 33)));
        testDT(DateTime(Date(2000, 1, 1), TimeOfDay(0, 30, 33)), -1,
               DateTime(Date(2000, 1, 1), TimeOfDay(23, 30, 33)));

        testDT(DateTime(Date(1999, 2, 28), TimeOfDay(23, 30, 33)), 25,
               DateTime(Date(1999, 2, 28), TimeOfDay(0, 30, 33)));
        testDT(DateTime(Date(1999, 3, 2), TimeOfDay(0, 30, 33)), -25,
               DateTime(Date(1999, 3, 2), TimeOfDay(23, 30, 33)));

        testDT(DateTime(Date(2000, 2, 28), TimeOfDay(23, 30, 33)), 25,
               DateTime(Date(2000, 2, 28), TimeOfDay(0, 30, 33)));
        testDT(DateTime(Date(2000, 3, 1), TimeOfDay(0, 30, 33)), -25,
               DateTime(Date(2000, 3, 1), TimeOfDay(23, 30, 33)));

        // Test B.C.
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 0,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 1,
               DateTime(Date(-1999, 7, 6), TimeOfDay(13, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 2,
               DateTime(Date(-1999, 7, 6), TimeOfDay(14, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 3,
               DateTime(Date(-1999, 7, 6), TimeOfDay(15, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 4,
               DateTime(Date(-1999, 7, 6), TimeOfDay(16, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 5,
               DateTime(Date(-1999, 7, 6), TimeOfDay(17, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 6,
               DateTime(Date(-1999, 7, 6), TimeOfDay(18, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 7,
               DateTime(Date(-1999, 7, 6), TimeOfDay(19, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 8,
               DateTime(Date(-1999, 7, 6), TimeOfDay(20, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 9,
               DateTime(Date(-1999, 7, 6), TimeOfDay(21, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 10,
               DateTime(Date(-1999, 7, 6), TimeOfDay(22, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 11,
               DateTime(Date(-1999, 7, 6), TimeOfDay(23, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 12,
               DateTime(Date(-1999, 7, 6), TimeOfDay(0, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 13,
               DateTime(Date(-1999, 7, 6), TimeOfDay(1, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 14,
               DateTime(Date(-1999, 7, 6), TimeOfDay(2, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 15,
               DateTime(Date(-1999, 7, 6), TimeOfDay(3, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 16,
               DateTime(Date(-1999, 7, 6), TimeOfDay(4, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 17,
               DateTime(Date(-1999, 7, 6), TimeOfDay(5, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 18,
               DateTime(Date(-1999, 7, 6), TimeOfDay(6, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 19,
               DateTime(Date(-1999, 7, 6), TimeOfDay(7, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 20,
               DateTime(Date(-1999, 7, 6), TimeOfDay(8, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 21,
               DateTime(Date(-1999, 7, 6), TimeOfDay(9, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 22,
               DateTime(Date(-1999, 7, 6), TimeOfDay(10, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 23,
               DateTime(Date(-1999, 7, 6), TimeOfDay(11, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 24,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 25,
               DateTime(Date(-1999, 7, 6), TimeOfDay(13, 30, 33)));

        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -1,
               DateTime(Date(-1999, 7, 6), TimeOfDay(11, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -2,
               DateTime(Date(-1999, 7, 6), TimeOfDay(10, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -3,
               DateTime(Date(-1999, 7, 6), TimeOfDay(9, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -4,
               DateTime(Date(-1999, 7, 6), TimeOfDay(8, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -5,
               DateTime(Date(-1999, 7, 6), TimeOfDay(7, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -6,
               DateTime(Date(-1999, 7, 6), TimeOfDay(6, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -7,
               DateTime(Date(-1999, 7, 6), TimeOfDay(5, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -8,
               DateTime(Date(-1999, 7, 6), TimeOfDay(4, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -9,
               DateTime(Date(-1999, 7, 6), TimeOfDay(3, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -10,
               DateTime(Date(-1999, 7, 6), TimeOfDay(2, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -11,
               DateTime(Date(-1999, 7, 6), TimeOfDay(1, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -12,
               DateTime(Date(-1999, 7, 6), TimeOfDay(0, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -13,
               DateTime(Date(-1999, 7, 6), TimeOfDay(23, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -14,
               DateTime(Date(-1999, 7, 6), TimeOfDay(22, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -15,
               DateTime(Date(-1999, 7, 6), TimeOfDay(21, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -16,
               DateTime(Date(-1999, 7, 6), TimeOfDay(20, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -17,
               DateTime(Date(-1999, 7, 6), TimeOfDay(19, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -18,
               DateTime(Date(-1999, 7, 6), TimeOfDay(18, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -19,
               DateTime(Date(-1999, 7, 6), TimeOfDay(17, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -20,
               DateTime(Date(-1999, 7, 6), TimeOfDay(16, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -21,
               DateTime(Date(-1999, 7, 6), TimeOfDay(15, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -22,
               DateTime(Date(-1999, 7, 6), TimeOfDay(14, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -23,
               DateTime(Date(-1999, 7, 6), TimeOfDay(13, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -24,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -25,
               DateTime(Date(-1999, 7, 6), TimeOfDay(11, 30, 33)));

        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(0, 30, 33)), 1,
               DateTime(Date(-1999, 7, 6), TimeOfDay(1, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(0, 30, 33)), 0,
               DateTime(Date(-1999, 7, 6), TimeOfDay(0, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(0, 30, 33)), -1,
               DateTime(Date(-1999, 7, 6), TimeOfDay(23, 30, 33)));

        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(23, 30, 33)), 1,
               DateTime(Date(-1999, 7, 6), TimeOfDay(0, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(23, 30, 33)), 0,
               DateTime(Date(-1999, 7, 6), TimeOfDay(23, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(23, 30, 33)), -1,
               DateTime(Date(-1999, 7, 6), TimeOfDay(22, 30, 33)));

        testDT(DateTime(Date(-1999, 7, 31), TimeOfDay(23, 30, 33)), 1,
               DateTime(Date(-1999, 7, 31), TimeOfDay(0, 30, 33)));
        testDT(DateTime(Date(-1999, 8, 1), TimeOfDay(0, 30, 33)), -1,
               DateTime(Date(-1999, 8, 1), TimeOfDay(23, 30, 33)));

        testDT(DateTime(Date(-2001, 12, 31), TimeOfDay(23, 30, 33)), 1,
               DateTime(Date(-2001, 12, 31), TimeOfDay(0, 30, 33)));
        testDT(DateTime(Date(-2000, 1, 1), TimeOfDay(0, 30, 33)), -1,
               DateTime(Date(-2000, 1, 1), TimeOfDay(23, 30, 33)));

        testDT(DateTime(Date(-2001, 2, 28), TimeOfDay(23, 30, 33)), 25,
               DateTime(Date(-2001, 2, 28), TimeOfDay(0, 30, 33)));
        testDT(DateTime(Date(-2001, 3, 2), TimeOfDay(0, 30, 33)), -25,
               DateTime(Date(-2001, 3, 2), TimeOfDay(23, 30, 33)));

        testDT(DateTime(Date(-2000, 2, 28), TimeOfDay(23, 30, 33)), 25,
               DateTime(Date(-2000, 2, 28), TimeOfDay(0, 30, 33)));
        testDT(DateTime(Date(-2000, 3, 1), TimeOfDay(0, 30, 33)), -25,
               DateTime(Date(-2000, 3, 1), TimeOfDay(23, 30, 33)));

        // Test Both
        testDT(DateTime(Date(-1, 1, 1), TimeOfDay(11, 30, 33)), 17_546,
               DateTime(Date(-1, 1, 1), TimeOfDay(13, 30, 33)));
        testDT(DateTime(Date(1, 1, 1), TimeOfDay(13, 30, 33)), -17_546,
               DateTime(Date(1, 1, 1), TimeOfDay(11, 30, 33)));

        auto dt = DateTime(2000, 1, 31, 9, 7, 6);
        dt.roll!"hours"(27).roll!"hours"(-9);
        assert(dt == DateTime(2000, 1, 31, 3, 7, 6));

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        static assert(!__traits(compiles, cdt.roll!"hours"(4)));
        static assert(!__traits(compiles, idt.roll!"hours"(4)));
    }

    // Test roll!"minutes"().
    @safe unittest
    {
        static void testDT(DateTime orig, int minutes, DateTime expected, size_t line = __LINE__)
        {
            orig.roll!"minutes"(minutes);
            assert(orig == expected);
        }

        // Test A.D.
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 0,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 1,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 31, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 2,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 32, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 3,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 33, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 4,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 34, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 5,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 35, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 10,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 40, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 15,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 45, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 29,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 59, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 30,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 0, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 45,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 15, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 60,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 75,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 45, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 90,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 0, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 100,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 10, 33)));

        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 689,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 59, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 690,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 0, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 691,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 1, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 960,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 1439,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 29, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 1440,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 1441,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 31, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 2880,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));

        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -1,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 29, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -2,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 28, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -3,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 27, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -4,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 26, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -5,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 25, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -10,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 20, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -15,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 15, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -29,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 1, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -30,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 0, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -45,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 45, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -60,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -75,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 15, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -90,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 0, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -100,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 50, 33)));

        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -749,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 1, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -750,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 0, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -751,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 59, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -960,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -1439,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 31, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -1440,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -1441,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 29, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -2880,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));

        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 0, 33)), 1,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 1, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 0, 33)), 0,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 0, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 0, 33)), -1,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 59, 33)));

        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(11, 59, 33)), 1,
               DateTime(Date(1999, 7, 6), TimeOfDay(11, 0, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(11, 59, 33)), 0,
               DateTime(Date(1999, 7, 6), TimeOfDay(11, 59, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(11, 59, 33)), -1,
               DateTime(Date(1999, 7, 6), TimeOfDay(11, 58, 33)));

        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(0, 0, 33)), 1,
               DateTime(Date(1999, 7, 6), TimeOfDay(0, 1, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(0, 0, 33)), 0,
               DateTime(Date(1999, 7, 6), TimeOfDay(0, 0, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(0, 0, 33)), -1,
               DateTime(Date(1999, 7, 6), TimeOfDay(0, 59, 33)));

        testDT(DateTime(Date(1999, 7, 5), TimeOfDay(23, 59, 33)), 1,
               DateTime(Date(1999, 7, 5), TimeOfDay(23, 0, 33)));
        testDT(DateTime(Date(1999, 7, 5), TimeOfDay(23, 59, 33)), 0,
               DateTime(Date(1999, 7, 5), TimeOfDay(23, 59, 33)));
        testDT(DateTime(Date(1999, 7, 5), TimeOfDay(23, 59, 33)), -1,
               DateTime(Date(1999, 7, 5), TimeOfDay(23, 58, 33)));

        testDT(DateTime(Date(1998, 12, 31), TimeOfDay(23, 59, 33)), 1,
               DateTime(Date(1998, 12, 31), TimeOfDay(23, 0, 33)));
        testDT(DateTime(Date(1998, 12, 31), TimeOfDay(23, 59, 33)), 0,
               DateTime(Date(1998, 12, 31), TimeOfDay(23, 59, 33)));
        testDT(DateTime(Date(1998, 12, 31), TimeOfDay(23, 59, 33)), -1,
               DateTime(Date(1998, 12, 31), TimeOfDay(23, 58, 33)));

        // Test B.C.
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 0,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 1,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 31, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 2,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 32, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 3,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 33, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 4,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 34, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 5,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 35, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 10,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 40, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 15,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 45, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 29,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 59, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 30,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 0, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 45,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 15, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 60,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 75,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 45, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 90,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 0, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 100,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 10, 33)));

        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 689,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 59, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 690,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 0, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 691,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 1, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 960,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 1439,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 29, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 1440,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 1441,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 31, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 2880,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)));

        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -1,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 29, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -2,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 28, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -3,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 27, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -4,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 26, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -5,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 25, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -10,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 20, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -15,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 15, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -29,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 1, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -30,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 0, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -45,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 45, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -60,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -75,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 15, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -90,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 0, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -100,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 50, 33)));

        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -749,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 1, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -750,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 0, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -751,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 59, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -960,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -1439,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 31, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -1440,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -1441,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 29, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -2880,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)));

        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 0, 33)), 1,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 1, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 0, 33)), 0,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 0, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 0, 33)), -1,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 59, 33)));

        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(11, 59, 33)), 1,
               DateTime(Date(-1999, 7, 6), TimeOfDay(11, 0, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(11, 59, 33)), 0,
               DateTime(Date(-1999, 7, 6), TimeOfDay(11, 59, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(11, 59, 33)), -1,
               DateTime(Date(-1999, 7, 6), TimeOfDay(11, 58, 33)));

        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(0, 0, 33)), 1,
               DateTime(Date(-1999, 7, 6), TimeOfDay(0, 1, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(0, 0, 33)), 0,
               DateTime(Date(-1999, 7, 6), TimeOfDay(0, 0, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(0, 0, 33)), -1,
               DateTime(Date(-1999, 7, 6), TimeOfDay(0, 59, 33)));

        testDT(DateTime(Date(-1999, 7, 5), TimeOfDay(23, 59, 33)), 1,
               DateTime(Date(-1999, 7, 5), TimeOfDay(23, 0, 33)));
        testDT(DateTime(Date(-1999, 7, 5), TimeOfDay(23, 59, 33)), 0,
               DateTime(Date(-1999, 7, 5), TimeOfDay(23, 59, 33)));
        testDT(DateTime(Date(-1999, 7, 5), TimeOfDay(23, 59, 33)), -1,
               DateTime(Date(-1999, 7, 5), TimeOfDay(23, 58, 33)));

        testDT(DateTime(Date(-2000, 12, 31), TimeOfDay(23, 59, 33)), 1,
               DateTime(Date(-2000, 12, 31), TimeOfDay(23, 0, 33)));
        testDT(DateTime(Date(-2000, 12, 31), TimeOfDay(23, 59, 33)), 0,
               DateTime(Date(-2000, 12, 31), TimeOfDay(23, 59, 33)));
        testDT(DateTime(Date(-2000, 12, 31), TimeOfDay(23, 59, 33)), -1,
               DateTime(Date(-2000, 12, 31), TimeOfDay(23, 58, 33)));

        // Test Both
        testDT(DateTime(Date(1, 1, 1), TimeOfDay(0, 0, 0)), -1,
               DateTime(Date(1, 1, 1), TimeOfDay(0, 59, 0)));
        testDT(DateTime(Date(0, 12, 31), TimeOfDay(23, 59, 0)), 1,
               DateTime(Date(0, 12, 31), TimeOfDay(23, 0, 0)));

        testDT(DateTime(Date(0, 1, 1), TimeOfDay(0, 0, 0)), -1,
               DateTime(Date(0, 1, 1), TimeOfDay(0, 59, 0)));
        testDT(DateTime(Date(-1, 12, 31), TimeOfDay(23, 59, 0)), 1,
               DateTime(Date(-1, 12, 31), TimeOfDay(23, 0, 0)));

        testDT(DateTime(Date(-1, 1, 1), TimeOfDay(11, 30, 33)), 1_052_760,
               DateTime(Date(-1, 1, 1), TimeOfDay(11, 30, 33)));
        testDT(DateTime(Date(1, 1, 1), TimeOfDay(13, 30, 33)), -1_052_760,
               DateTime(Date(1, 1, 1), TimeOfDay(13, 30, 33)));

        testDT(DateTime(Date(-1, 1, 1), TimeOfDay(11, 30, 33)), 1_052_782,
               DateTime(Date(-1, 1, 1), TimeOfDay(11, 52, 33)));
        testDT(DateTime(Date(1, 1, 1), TimeOfDay(13, 52, 33)), -1_052_782,
               DateTime(Date(1, 1, 1), TimeOfDay(13, 30, 33)));

        auto dt = DateTime(2000, 1, 31, 9, 7, 6);
        dt.roll!"minutes"(92).roll!"minutes"(-292);
        assert(dt == DateTime(2000, 1, 31, 9, 47, 6));

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        static assert(!__traits(compiles, cdt.roll!"minutes"(4)));
        static assert(!__traits(compiles, idt.roll!"minutes"(4)));
    }

    // Test roll!"seconds"().
    @safe unittest
    {
        static void testDT(DateTime orig, int seconds, DateTime expected, size_t line = __LINE__)
        {
            orig.roll!"seconds"(seconds);
            assert(orig == expected);
        }

        // Test A.D.
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 0,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 1,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 34)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 2,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 35)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 3,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 36)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 4,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 37)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 5,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 38)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 10,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 43)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 15,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 48)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 26,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 59)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 27,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 0)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 30,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 3)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 59,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 32)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 60,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 61,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 34)));

        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 1766,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 59)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 1767,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 0)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 1768,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 1)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 2007,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 0)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 3599,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 32)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 3600,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 3601,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 34)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), 7200,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));

        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -1,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 32)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -2,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 31)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -3,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 30)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -4,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 29)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -5,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 28)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -10,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 23)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -15,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 18)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -33,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 0)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -34,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 59)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -35,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 58)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -59,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 34)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -60,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)), -61,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 32)));

        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 0)), 1,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 1)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 0)), 0,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 0)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 0)), -1,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 59)));

        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 0, 0)), 1,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 0, 1)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 0, 0)), 0,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 0, 0)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(12, 0, 0)), -1,
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 0, 59)));

        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(0, 0, 0)), 1,
               DateTime(Date(1999, 7, 6), TimeOfDay(0, 0, 1)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(0, 0, 0)), 0,
               DateTime(Date(1999, 7, 6), TimeOfDay(0, 0, 0)));
        testDT(DateTime(Date(1999, 7, 6), TimeOfDay(0, 0, 0)), -1,
               DateTime(Date(1999, 7, 6), TimeOfDay(0, 0, 59)));

        testDT(DateTime(Date(1999, 7, 5), TimeOfDay(23, 59, 59)), 1,
               DateTime(Date(1999, 7, 5), TimeOfDay(23, 59, 0)));
        testDT(DateTime(Date(1999, 7, 5), TimeOfDay(23, 59, 59)), 0,
               DateTime(Date(1999, 7, 5), TimeOfDay(23, 59, 59)));
        testDT(DateTime(Date(1999, 7, 5), TimeOfDay(23, 59, 59)), -1,
               DateTime(Date(1999, 7, 5), TimeOfDay(23, 59, 58)));

        testDT(DateTime(Date(1998, 12, 31), TimeOfDay(23, 59, 59)), 1,
               DateTime(Date(1998, 12, 31), TimeOfDay(23, 59, 0)));
        testDT(DateTime(Date(1998, 12, 31), TimeOfDay(23, 59, 59)), 0,
               DateTime(Date(1998, 12, 31), TimeOfDay(23, 59, 59)));
        testDT(DateTime(Date(1998, 12, 31), TimeOfDay(23, 59, 59)), -1,
               DateTime(Date(1998, 12, 31), TimeOfDay(23, 59, 58)));

        // Test B.C.
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 0,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 1,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 34)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 2,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 35)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 3,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 36)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 4,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 37)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 5,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 38)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 10,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 43)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 15,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 48)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 26,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 59)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 27,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 0)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 30,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 3)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 59,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 32)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 60,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 61,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 34)));

        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 1766,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 59)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 1767,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 0)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 1768,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 1)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 2007,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 0)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 3599,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 32)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 3600,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 3601,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 34)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), 7200,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)));

        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -1,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 32)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -2,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 31)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -3,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 30)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -4,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 29)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -5,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 28)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -10,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 23)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -15,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 18)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -33,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 0)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -34,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 59)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -35,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 58)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -59,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 34)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -60,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)), -61,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 32)));

        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 0)), 1,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 1)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 0)), 0,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 0)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 0)), -1,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 59)));

        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 0, 0)), 1,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 0, 1)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 0, 0)), 0,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 0, 0)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(12, 0, 0)), -1,
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 0, 59)));

        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(0, 0, 0)), 1,
               DateTime(Date(-1999, 7, 6), TimeOfDay(0, 0, 1)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(0, 0, 0)), 0,
               DateTime(Date(-1999, 7, 6), TimeOfDay(0, 0, 0)));
        testDT(DateTime(Date(-1999, 7, 6), TimeOfDay(0, 0, 0)), -1,
               DateTime(Date(-1999, 7, 6), TimeOfDay(0, 0, 59)));

        testDT(DateTime(Date(-1999, 7, 5), TimeOfDay(23, 59, 59)), 1,
               DateTime(Date(-1999, 7, 5), TimeOfDay(23, 59, 0)));
        testDT(DateTime(Date(-1999, 7, 5), TimeOfDay(23, 59, 59)), 0,
               DateTime(Date(-1999, 7, 5), TimeOfDay(23, 59, 59)));
        testDT(DateTime(Date(-1999, 7, 5), TimeOfDay(23, 59, 59)), -1,
               DateTime(Date(-1999, 7, 5), TimeOfDay(23, 59, 58)));

        testDT(DateTime(Date(-2000, 12, 31), TimeOfDay(23, 59, 59)), 1,
               DateTime(Date(-2000, 12, 31), TimeOfDay(23, 59, 0)));
        testDT(DateTime(Date(-2000, 12, 31), TimeOfDay(23, 59, 59)), 0,
               DateTime(Date(-2000, 12, 31), TimeOfDay(23, 59, 59)));
        testDT(DateTime(Date(-2000, 12, 31), TimeOfDay(23, 59, 59)), -1,
               DateTime(Date(-2000, 12, 31), TimeOfDay(23, 59, 58)));

        // Test Both
        testDT(DateTime(Date(1, 1, 1), TimeOfDay(0, 0, 0)), -1,
               DateTime(Date(1, 1, 1), TimeOfDay(0, 0, 59)));
        testDT(DateTime(Date(0, 12, 31), TimeOfDay(23, 59, 59)), 1,
               DateTime(Date(0, 12, 31), TimeOfDay(23, 59, 0)));

        testDT(DateTime(Date(0, 1, 1), TimeOfDay(0, 0, 0)), -1,
               DateTime(Date(0, 1, 1), TimeOfDay(0, 0, 59)));
        testDT(DateTime(Date(-1, 12, 31), TimeOfDay(23, 59, 59)), 1,
               DateTime(Date(-1, 12, 31), TimeOfDay(23, 59, 0)));

        testDT(DateTime(Date(-1, 1, 1), TimeOfDay(11, 30, 33)), 63_165_600L,
               DateTime(Date(-1, 1, 1), TimeOfDay(11, 30, 33)));
        testDT(DateTime(Date(1, 1, 1), TimeOfDay(13, 30, 33)), -63_165_600L,
               DateTime(Date(1, 1, 1), TimeOfDay(13, 30, 33)));

        testDT(DateTime(Date(-1, 1, 1), TimeOfDay(11, 30, 33)), 63_165_617L,
               DateTime(Date(-1, 1, 1), TimeOfDay(11, 30, 50)));
        testDT(DateTime(Date(1, 1, 1), TimeOfDay(13, 30, 50)), -63_165_617L,
               DateTime(Date(1, 1, 1), TimeOfDay(13, 30, 33)));

        auto dt = DateTime(2000, 1, 31, 9, 7, 6);
        dt.roll!"seconds"(92).roll!"seconds"(-292);
        assert(dt == DateTime(2000, 1, 31, 9, 7, 46));

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        static assert(!__traits(compiles, cdt.roll!"seconds"(4)));
        static assert(!__traits(compiles, idt.roll!"seconds"(4)));
    }


    import core.time : Duration;
    /++
        Gives the result of adding or subtracting a $(REF Duration, core,time)
        from this $(LREF DateTime).

        The legal types of arithmetic for $(LREF DateTime) using this operator
        are

        $(BOOKTABLE,
        $(TR $(TD DateTime) $(TD +) $(TD Duration) $(TD -->) $(TD DateTime))
        $(TR $(TD DateTime) $(TD -) $(TD Duration) $(TD -->) $(TD DateTime))
        )

        Params:
            duration = The $(REF Duration, core,time) to add to or subtract from
                       this $(LREF DateTime).
      +/
    DateTime opBinary(string op)(Duration duration) const @safe pure nothrow @nogc
        if (op == "+" || op == "-")
    {
        DateTime retval = this;
        immutable seconds = duration.total!"seconds";
        mixin("return retval._addSeconds(" ~ op ~ "seconds);");
    }

    ///
    @safe unittest
    {
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
        import core.time : dur;

        auto dt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));

        assert(dt + dur!"weeks"(7) == DateTime(Date(1999, 8, 24), TimeOfDay(12, 30, 33)));
        assert(dt + dur!"weeks"(-7) == DateTime(Date(1999, 5, 18), TimeOfDay(12, 30, 33)));
        assert(dt + dur!"days"(7) == DateTime(Date(1999, 7, 13), TimeOfDay(12, 30, 33)));
        assert(dt + dur!"days"(-7) == DateTime(Date(1999, 6, 29), TimeOfDay(12, 30, 33)));

        assert(dt + dur!"hours"(7) == DateTime(Date(1999, 7, 6), TimeOfDay(19, 30, 33)));
        assert(dt + dur!"hours"(-7) == DateTime(Date(1999, 7, 6), TimeOfDay(5, 30, 33)));
        assert(dt + dur!"minutes"(7) == DateTime(Date(1999, 7, 6), TimeOfDay(12, 37, 33)));
        assert(dt + dur!"minutes"(-7) == DateTime(Date(1999, 7, 6), TimeOfDay(12, 23, 33)));
        assert(dt + dur!"seconds"(7) == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 40)));
        assert(dt + dur!"seconds"(-7) == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 26)));
        assert(dt + dur!"msecs"(7_000) == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 40)));
        assert(dt + dur!"msecs"(-7_000) == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 26)));
        assert(dt + dur!"usecs"(7_000_000) == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 40)));
        assert(dt + dur!"usecs"(-7_000_000) == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 26)));
        assert(dt + dur!"hnsecs"(70_000_000) == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 40)));
        assert(dt + dur!"hnsecs"(-70_000_000) == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 26)));

        assert(dt - dur!"weeks"(-7) == DateTime(Date(1999, 8, 24), TimeOfDay(12, 30, 33)));
        assert(dt - dur!"weeks"(7) == DateTime(Date(1999, 5, 18), TimeOfDay(12, 30, 33)));
        assert(dt - dur!"days"(-7) == DateTime(Date(1999, 7, 13), TimeOfDay(12, 30, 33)));
        assert(dt - dur!"days"(7) == DateTime(Date(1999, 6, 29), TimeOfDay(12, 30, 33)));

        assert(dt - dur!"hours"(-7) == DateTime(Date(1999, 7, 6), TimeOfDay(19, 30, 33)));
        assert(dt - dur!"hours"(7) == DateTime(Date(1999, 7, 6), TimeOfDay(5, 30, 33)));
        assert(dt - dur!"minutes"(-7) == DateTime(Date(1999, 7, 6), TimeOfDay(12, 37, 33)));
        assert(dt - dur!"minutes"(7) == DateTime(Date(1999, 7, 6), TimeOfDay(12, 23, 33)));
        assert(dt - dur!"seconds"(-7) == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 40)));
        assert(dt - dur!"seconds"(7) == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 26)));
        assert(dt - dur!"msecs"(-7_000) == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 40)));
        assert(dt - dur!"msecs"(7_000) == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 26)));
        assert(dt - dur!"usecs"(-7_000_000) == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 40)));
        assert(dt - dur!"usecs"(7_000_000) == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 26)));
        assert(dt - dur!"hnsecs"(-70_000_000) == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 40)));
        assert(dt - dur!"hnsecs"(70_000_000) == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 26)));

        auto duration = dur!"seconds"(12);
        const cdt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        immutable idt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        assert(cdt + duration == DateTime(1999, 7, 6, 12, 30, 45));
        assert(idt + duration == DateTime(1999, 7, 6, 12, 30, 45));
        assert(cdt - duration == DateTime(1999, 7, 6, 12, 30, 21));
        assert(idt - duration == DateTime(1999, 7, 6, 12, 30, 21));
    }


    /++
        Gives the result of adding or subtracting a duration from this
        $(LREF DateTime), as well as assigning the result to this
        $(LREF DateTime).

        The legal types of arithmetic for $(LREF DateTime) using this operator
        are

        $(BOOKTABLE,
        $(TR $(TD DateTime) $(TD +) $(TD duration) $(TD -->) $(TD DateTime))
        $(TR $(TD DateTime) $(TD -) $(TD duration) $(TD -->) $(TD DateTime))
        )

        Params:
            duration = The duration to add to or subtract from this
                       $(LREF DateTime).
      +/
    ref DateTime opOpAssign(string op)(Duration duration) @safe pure nothrow @nogc
        if (op == "+" || op == "-")
    {
        import core.time : convert;
        import std.format : format;

        DateTime retval = this;
        immutable hnsecs = duration.total!"hnsecs";

        mixin(format(`return _addSeconds(convert!("hnsecs", "seconds")(%shnsecs));`, op));
    }

    @safe unittest
    {
        import core.time : dur;
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) + dur!"weeks"(7) ==
               DateTime(Date(1999, 8, 24), TimeOfDay(12, 30, 33)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) + dur!"weeks"(-7) ==
               DateTime(Date(1999, 5, 18), TimeOfDay(12, 30, 33)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) + dur!"days"(7) ==
               DateTime(Date(1999, 7, 13), TimeOfDay(12, 30, 33)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) + dur!"days"(-7) ==
               DateTime(Date(1999, 6, 29), TimeOfDay(12, 30, 33)));

        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) + dur!"hours"(7) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(19, 30, 33)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) + dur!"hours"(-7) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(5, 30, 33)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) + dur!"minutes"(7) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 37, 33)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) + dur!"minutes"(-7) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 23, 33)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) + dur!"seconds"(7) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 40)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) + dur!"seconds"(-7) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 26)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) + dur!"msecs"(7_000) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 40)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) + dur!"msecs"(-7_000) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 26)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) + dur!"usecs"(7_000_000) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 40)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) + dur!"usecs"(-7_000_000) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 26)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) + dur!"hnsecs"(70_000_000) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 40)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) + dur!"hnsecs"(-70_000_000) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 26)));

        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) - dur!"weeks"(-7) ==
               DateTime(Date(1999, 8, 24), TimeOfDay(12, 30, 33)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) - dur!"weeks"(7) ==
               DateTime(Date(1999, 5, 18), TimeOfDay(12, 30, 33)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) - dur!"days"(-7) ==
               DateTime(Date(1999, 7, 13), TimeOfDay(12, 30, 33)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) - dur!"days"(7) ==
               DateTime(Date(1999, 6, 29), TimeOfDay(12, 30, 33)));

        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) - dur!"hours"(-7) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(19, 30, 33)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) - dur!"hours"(7) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(5, 30, 33)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) - dur!"minutes"(-7) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 37, 33)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) - dur!"minutes"(7) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 23, 33)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) - dur!"seconds"(-7) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 40)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) - dur!"seconds"(7) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 26)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) - dur!"msecs"(-7_000) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 40)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) - dur!"msecs"(7_000) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 26)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) - dur!"usecs"(-7_000_000) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 40)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) - dur!"usecs"(7_000_000) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 26)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) - dur!"hnsecs"(-70_000_000) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 40)));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) - dur!"hnsecs"(70_000_000) ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 26)));

        auto dt = DateTime(2000, 1, 31, 9, 7, 6);
        (dt += dur!"seconds"(92)) -= dur!"days"(-500);
        assert(dt == DateTime(2001, 6, 14, 9, 8, 38));

        auto duration = dur!"seconds"(12);
        const cdt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        immutable idt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        static assert(!__traits(compiles, cdt += duration));
        static assert(!__traits(compiles, idt += duration));
        static assert(!__traits(compiles, cdt -= duration));
        static assert(!__traits(compiles, idt -= duration));
    }


    /++
        Gives the difference between two $(LREF DateTime)s.

        The legal types of arithmetic for $(LREF DateTime) using this operator are

        $(BOOKTABLE,
        $(TR $(TD DateTime) $(TD -) $(TD DateTime) $(TD -->) $(TD duration))
        )
      +/
    Duration opBinary(string op)(DateTime rhs) const @safe pure nothrow @nogc
        if (op == "-")
    {
        immutable dateResult = _date - rhs.date;
        immutable todResult = _tod - rhs._tod;

        import core.time : dur;
        return dur!"hnsecs"(dateResult.total!"hnsecs" + todResult.total!"hnsecs");
    }

    @safe unittest
    {
        auto dt = DateTime(1999, 7, 6, 12, 30, 33);

        import core.time : dur;
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) - DateTime(Date(1998, 7, 6), TimeOfDay(12, 30, 33)) ==
               dur!"seconds"(31_536_000));
        assert(DateTime(Date(1998, 7, 6), TimeOfDay(12, 30, 33)) - DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) ==
               dur!"seconds"(-31_536_000));

        assert(DateTime(Date(1999, 8, 6), TimeOfDay(12, 30, 33)) - DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) ==
               dur!"seconds"(26_78_400));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) - DateTime(Date(1999, 8, 6), TimeOfDay(12, 30, 33)) ==
               dur!"seconds"(-26_78_400));

        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) - DateTime(Date(1999, 7, 5), TimeOfDay(12, 30, 33)) ==
               dur!"seconds"(86_400));
        assert(DateTime(Date(1999, 7, 5), TimeOfDay(12, 30, 33)) - DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) ==
               dur!"seconds"(-86_400));

        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) - DateTime(Date(1999, 7, 6), TimeOfDay(11, 30, 33)) ==
               dur!"seconds"(3600));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(11, 30, 33)) - DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) ==
               dur!"seconds"(-3600));

        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 31, 33)) - DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) ==
               dur!"seconds"(60));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) - DateTime(Date(1999, 7, 6), TimeOfDay(12, 31, 33)) ==
               dur!"seconds"(-60));

        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 34)) - DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) ==
               dur!"seconds"(1));
        assert(DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)) - DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 34)) ==
               dur!"seconds"(-1));

        assert(DateTime(1, 1, 1, 12, 30, 33) - DateTime(1, 1, 1, 0, 0, 0) == dur!"seconds"(45033));
        assert(DateTime(1, 1, 1, 0, 0, 0) - DateTime(1, 1, 1, 12, 30, 33) == dur!"seconds"(-45033));
        assert(DateTime(0, 12, 31, 12, 30, 33) - DateTime(1, 1, 1, 0, 0, 0) == dur!"seconds"(-41367));
        assert(DateTime(1, 1, 1, 0, 0, 0) - DateTime(0, 12, 31, 12, 30, 33) == dur!"seconds"(41367));

        const cdt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        immutable idt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        assert(dt - dt == Duration.zero);
        assert(cdt - dt == Duration.zero);
        assert(idt - dt == Duration.zero);

        assert(dt - cdt == Duration.zero);
        assert(cdt - cdt == Duration.zero);
        assert(idt - cdt == Duration.zero);

        assert(dt - idt == Duration.zero);
        assert(cdt - idt == Duration.zero);
        assert(idt - idt == Duration.zero);
    }


    /++
        Returns the difference between the two $(LREF DateTime)s in months.

        To get the difference in years, subtract the year property
        of two $(LREF DateTime)s. To get the difference in days or weeks,
        subtract the $(LREF DateTime)s themselves and use the
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
            rhs = The $(LREF DateTime) to subtract from this one.
      +/
    int diffMonths(DateTime rhs) const @safe pure nothrow @nogc
    {
        return _date.diffMonths(rhs._date);
    }

    ///
    @safe unittest
    {
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
        auto dt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        const cdt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        immutable idt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        assert(dt.diffMonths(dt) == 0);
        assert(cdt.diffMonths(dt) == 0);
        assert(idt.diffMonths(dt) == 0);

        assert(dt.diffMonths(cdt) == 0);
        assert(cdt.diffMonths(cdt) == 0);
        assert(idt.diffMonths(cdt) == 0);

        assert(dt.diffMonths(idt) == 0);
        assert(cdt.diffMonths(idt) == 0);
        assert(idt.diffMonths(idt) == 0);
    }


    /++
        Whether this $(LREF DateTime) is in a leap year.
     +/
    @property bool isLeapYear() const @safe pure nothrow @nogc
    {
        return _date.isLeapYear;
    }

    @safe unittest
    {
        auto dt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        const cdt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        immutable idt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        assert(!dt.isLeapYear);
        assert(!cdt.isLeapYear);
        assert(!idt.isLeapYear);
    }


    /++
        Day of the week this $(LREF DateTime) is on.
      +/
    @property DayOfWeek dayOfWeek() const @safe pure nothrow @nogc
    {
        return _date.dayOfWeek;
    }

    @safe unittest
    {
        auto dt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        const cdt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        immutable idt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        assert(dt.dayOfWeek == DayOfWeek.tue);
        assert(cdt.dayOfWeek == DayOfWeek.tue);
        assert(idt.dayOfWeek == DayOfWeek.tue);
    }


    /++
        Day of the year this $(LREF DateTime) is on.
      +/
    @property ushort dayOfYear() const @safe pure nothrow @nogc
    {
        return _date.dayOfYear;
    }

    ///
    @safe unittest
    {
        assert(DateTime(Date(1999, 1, 1), TimeOfDay(12, 22, 7)).dayOfYear == 1);
        assert(DateTime(Date(1999, 12, 31), TimeOfDay(7, 2, 59)).dayOfYear == 365);
        assert(DateTime(Date(2000, 12, 31), TimeOfDay(21, 20, 0)).dayOfYear == 366);
    }

    @safe unittest
    {
        auto dt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        const cdt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        immutable idt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        assert(dt.dayOfYear == 187);
        assert(cdt.dayOfYear == 187);
        assert(idt.dayOfYear == 187);
    }


    /++
        Day of the year.

        Params:
            day = The day of the year to set which day of the year this
                  $(LREF DateTime) is on.
      +/
    @property void dayOfYear(int day) @safe pure
    {
        _date.dayOfYear = day;
    }

    @safe unittest
    {
        auto dt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        const cdt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        immutable idt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        dt.dayOfYear = 12;
        assert(dt.dayOfYear == 12);
        static assert(!__traits(compiles, cdt.dayOfYear = 12));
        static assert(!__traits(compiles, idt.dayOfYear = 12));
    }


    /++
        The Xth day of the Gregorian Calendar that this $(LREF DateTime) is on.
     +/
    @property int dayOfGregorianCal() const @safe pure nothrow @nogc
    {
        return _date.dayOfGregorianCal;
    }

    ///
    @safe unittest
    {
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
        const cdt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        immutable idt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        assert(cdt.dayOfGregorianCal == 729_941);
        assert(idt.dayOfGregorianCal == 729_941);
    }


    /++
        The Xth day of the Gregorian Calendar that this $(LREF DateTime) is on.
        Setting this property does not affect the time portion of
        $(LREF DateTime).

        Params:
            days = The day of the Gregorian Calendar to set this $(LREF DateTime)
                   to.
     +/
    @property void dayOfGregorianCal(int days) @safe pure nothrow @nogc
    {
        _date.dayOfGregorianCal = days;
    }

    ///
    @safe unittest
    {
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
        const cdt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        immutable idt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        static assert(!__traits(compiles, cdt.dayOfGregorianCal = 7));
        static assert(!__traits(compiles, idt.dayOfGregorianCal = 7));
    }


    /++
        The ISO 8601 week of the year that this $(LREF DateTime) is in.

        See_Also:
            $(HTTP en.wikipedia.org/wiki/ISO_week_date, ISO Week Date)
      +/
    @property ubyte isoWeek() const @safe pure nothrow
    {
        return _date.isoWeek;
    }

    @safe unittest
    {
        auto dt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        const cdt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        immutable idt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        assert(dt.isoWeek == 27);
        assert(cdt.isoWeek == 27);
        assert(idt.isoWeek == 27);
    }


    /++
        The year of the ISO 8601 week calendar that this $(LREF DateTime) is in.

        See_Also:
            $(HTTP en.wikipedia.org/wiki/ISO_week_date, ISO Week Date)
      +/
    @property short isoWeekYear() const @safe pure nothrow
    {
        return _date.isoWeekYear;
    }


    /++
        $(LREF DateTime) for the last day in the month that this
        $(LREF DateTime) is in. The time portion of endOfMonth is always
        23:59:59.
      +/
    @property DateTime endOfMonth() const @safe pure nothrow
    {
        try
            return DateTime(_date.endOfMonth, TimeOfDay(23, 59, 59));
        catch (Exception e)
            assert(0, "DateTime constructor threw.");
    }

    ///
    @safe unittest
    {
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
        // Test A.D.
        assert(DateTime(1999, 1, 1, 0, 13, 26).endOfMonth == DateTime(1999, 1, 31, 23, 59, 59));
        assert(DateTime(1999, 2, 1, 1, 14, 27).endOfMonth == DateTime(1999, 2, 28, 23, 59, 59));
        assert(DateTime(2000, 2, 1, 2, 15, 28).endOfMonth == DateTime(2000, 2, 29, 23, 59, 59));
        assert(DateTime(1999, 3, 1, 3, 16, 29).endOfMonth == DateTime(1999, 3, 31, 23, 59, 59));
        assert(DateTime(1999, 4, 1, 4, 17, 30).endOfMonth == DateTime(1999, 4, 30, 23, 59, 59));
        assert(DateTime(1999, 5, 1, 5, 18, 31).endOfMonth == DateTime(1999, 5, 31, 23, 59, 59));
        assert(DateTime(1999, 6, 1, 6, 19, 32).endOfMonth == DateTime(1999, 6, 30, 23, 59, 59));
        assert(DateTime(1999, 7, 1, 7, 20, 33).endOfMonth == DateTime(1999, 7, 31, 23, 59, 59));
        assert(DateTime(1999, 8, 1, 8, 21, 34).endOfMonth == DateTime(1999, 8, 31, 23, 59, 59));
        assert(DateTime(1999, 9, 1, 9, 22, 35).endOfMonth == DateTime(1999, 9, 30, 23, 59, 59));
        assert(DateTime(1999, 10, 1, 10, 23, 36).endOfMonth == DateTime(1999, 10, 31, 23, 59, 59));
        assert(DateTime(1999, 11, 1, 11, 24, 37).endOfMonth == DateTime(1999, 11, 30, 23, 59, 59));
        assert(DateTime(1999, 12, 1, 12, 25, 38).endOfMonth == DateTime(1999, 12, 31, 23, 59, 59));

        // Test B.C.
        assert(DateTime(-1999, 1, 1, 0, 13, 26).endOfMonth == DateTime(-1999, 1, 31, 23, 59, 59));
        assert(DateTime(-1999, 2, 1, 1, 14, 27).endOfMonth == DateTime(-1999, 2, 28, 23, 59, 59));
        assert(DateTime(-2000, 2, 1, 2, 15, 28).endOfMonth == DateTime(-2000, 2, 29, 23, 59, 59));
        assert(DateTime(-1999, 3, 1, 3, 16, 29).endOfMonth == DateTime(-1999, 3, 31, 23, 59, 59));
        assert(DateTime(-1999, 4, 1, 4, 17, 30).endOfMonth == DateTime(-1999, 4, 30, 23, 59, 59));
        assert(DateTime(-1999, 5, 1, 5, 18, 31).endOfMonth == DateTime(-1999, 5, 31, 23, 59, 59));
        assert(DateTime(-1999, 6, 1, 6, 19, 32).endOfMonth == DateTime(-1999, 6, 30, 23, 59, 59));
        assert(DateTime(-1999, 7, 1, 7, 20, 33).endOfMonth == DateTime(-1999, 7, 31, 23, 59, 59));
        assert(DateTime(-1999, 8, 1, 8, 21, 34).endOfMonth == DateTime(-1999, 8, 31, 23, 59, 59));
        assert(DateTime(-1999, 9, 1, 9, 22, 35).endOfMonth == DateTime(-1999, 9, 30, 23, 59, 59));
        assert(DateTime(-1999, 10, 1, 10, 23, 36).endOfMonth == DateTime(-1999, 10, 31, 23, 59, 59));
        assert(DateTime(-1999, 11, 1, 11, 24, 37).endOfMonth == DateTime(-1999, 11, 30, 23, 59, 59));
        assert(DateTime(-1999, 12, 1, 12, 25, 38).endOfMonth == DateTime(-1999, 12, 31, 23, 59, 59));

        const cdt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        immutable idt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        assert(cdt.endOfMonth == DateTime(1999, 7, 31, 23, 59, 59));
        assert(idt.endOfMonth == DateTime(1999, 7, 31, 23, 59, 59));
    }


    /++
        The last day in the month that this $(LREF DateTime) is in.
      +/
    @property ubyte daysInMonth() const @safe pure nothrow @nogc
    {
        return _date.daysInMonth;
    }

    ///
    @safe unittest
    {
        assert(DateTime(Date(1999, 1, 6), TimeOfDay(0, 0, 0)).daysInMonth == 31);
        assert(DateTime(Date(1999, 2, 7), TimeOfDay(19, 30, 0)).daysInMonth == 28);
        assert(DateTime(Date(2000, 2, 7), TimeOfDay(5, 12, 27)).daysInMonth == 29);
        assert(DateTime(Date(2000, 6, 4), TimeOfDay(12, 22, 9)).daysInMonth == 30);
    }

    @safe unittest
    {
        const cdt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        immutable idt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        assert(cdt.daysInMonth == 31);
        assert(idt.daysInMonth == 31);
    }


    /++
        Whether the current year is a date in A.D.
      +/
    @property bool isAD() const @safe pure nothrow @nogc
    {
        return _date.isAD;
    }

    ///
    @safe unittest
    {
        assert(DateTime(Date(1, 1, 1), TimeOfDay(12, 7, 0)).isAD);
        assert(DateTime(Date(2010, 12, 31), TimeOfDay(0, 0, 0)).isAD);
        assert(!DateTime(Date(0, 12, 31), TimeOfDay(23, 59, 59)).isAD);
        assert(!DateTime(Date(-2010, 1, 1), TimeOfDay(2, 2, 2)).isAD);
    }

    @safe unittest
    {
        const cdt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        immutable idt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        assert(cdt.isAD);
        assert(idt.isAD);
    }


    /++
        The $(HTTP en.wikipedia.org/wiki/Julian_day, Julian day) for this
        $(LREF DateTime) at the given time. For example, prior to noon,
        1996-03-31 would be the Julian day number 2_450_173, so this function
        returns 2_450_173, while from noon onward, the julian day number would
        be 2_450_174, so this function returns 2_450_174.
      +/
    @property long julianDay() const @safe pure nothrow @nogc
    {
        if (_tod._hour < 12)
            return _date.julianDay - 1;
        else
            return _date.julianDay;
    }

    @safe unittest
    {
        assert(DateTime(Date(-4713, 11, 24), TimeOfDay(0, 0, 0)).julianDay == -1);
        assert(DateTime(Date(-4713, 11, 24), TimeOfDay(12, 0, 0)).julianDay == 0);

        assert(DateTime(Date(0, 12, 31), TimeOfDay(0, 0, 0)).julianDay == 1_721_424);
        assert(DateTime(Date(0, 12, 31), TimeOfDay(12, 0, 0)).julianDay == 1_721_425);

        assert(DateTime(Date(1, 1, 1), TimeOfDay(0, 0, 0)).julianDay == 1_721_425);
        assert(DateTime(Date(1, 1, 1), TimeOfDay(12, 0, 0)).julianDay == 1_721_426);

        assert(DateTime(Date(1582, 10, 15), TimeOfDay(0, 0, 0)).julianDay == 2_299_160);
        assert(DateTime(Date(1582, 10, 15), TimeOfDay(12, 0, 0)).julianDay == 2_299_161);

        assert(DateTime(Date(1858, 11, 17), TimeOfDay(0, 0, 0)).julianDay == 2_400_000);
        assert(DateTime(Date(1858, 11, 17), TimeOfDay(12, 0, 0)).julianDay == 2_400_001);

        assert(DateTime(Date(1982, 1, 4), TimeOfDay(0, 0, 0)).julianDay == 2_444_973);
        assert(DateTime(Date(1982, 1, 4), TimeOfDay(12, 0, 0)).julianDay == 2_444_974);

        assert(DateTime(Date(1996, 3, 31), TimeOfDay(0, 0, 0)).julianDay == 2_450_173);
        assert(DateTime(Date(1996, 3, 31), TimeOfDay(12, 0, 0)).julianDay == 2_450_174);

        assert(DateTime(Date(2010, 8, 24), TimeOfDay(0, 0, 0)).julianDay == 2_455_432);
        assert(DateTime(Date(2010, 8, 24), TimeOfDay(12, 0, 0)).julianDay == 2_455_433);

        const cdt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        immutable idt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        assert(cdt.julianDay == 2_451_366);
        assert(idt.julianDay == 2_451_366);
    }


    /++
        The modified $(HTTP en.wikipedia.org/wiki/Julian_day, Julian day) for any
        time on this date (since, the modified Julian day changes at midnight).
      +/
    @property long modJulianDay() const @safe pure nothrow @nogc
    {
        return _date.modJulianDay;
    }

    @safe unittest
    {
        assert(DateTime(Date(1858, 11, 17), TimeOfDay(0, 0, 0)).modJulianDay == 0);
        assert(DateTime(Date(1858, 11, 17), TimeOfDay(12, 0, 0)).modJulianDay == 0);

        assert(DateTime(Date(2010, 8, 24), TimeOfDay(0, 0, 0)).modJulianDay == 55_432);
        assert(DateTime(Date(2010, 8, 24), TimeOfDay(12, 0, 0)).modJulianDay == 55_432);

        const cdt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        immutable idt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        assert(cdt.modJulianDay == 51_365);
        assert(idt.modJulianDay == 51_365);
    }


    /++
        Converts this $(LREF DateTime) to a string with the format `YYYYMMDDTHHMMSS`.
        If `writer` is set, the resulting string will be written directly to it.

        Params:
            writer = A `char` accepting
            $(REF_ALTTEXT output range, isOutputRange, std, range, primitives)
        Returns:
            A `string` when not using an output range; `void` otherwise.
      +/
    string toISOString() const @safe pure nothrow
    {
        import std.array : appender;
        auto w = appender!string();
        w.reserve(18);
        try
            toISOString(w);
        catch (Exception e)
            assert(0, "toISOString() threw.");
        return w.data;
    }

    /// ditto
    void toISOString(W)(ref W writer) const
    if (isOutputRange!(W, char))
    {
        import std.format.write : formattedWrite;
        _date.toISOString(writer);
        formattedWrite!("T%02d%02d%02d")(
            writer,
            _tod._hour,
            _tod._minute,
            _tod._second
        );
    }

    ///
    @safe unittest
    {
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
        // Test A.D.
        assert(DateTime(Date(9, 12, 4), TimeOfDay(0, 0, 0)).toISOString() == "00091204T000000");
        assert(DateTime(Date(99, 12, 4), TimeOfDay(5, 6, 12)).toISOString() == "00991204T050612");
        assert(DateTime(Date(999, 12, 4), TimeOfDay(13, 44, 59)).toISOString() == "09991204T134459");
        assert(DateTime(Date(9999, 7, 4), TimeOfDay(23, 59, 59)).toISOString() == "99990704T235959");
        assert(DateTime(Date(10000, 10, 20), TimeOfDay(1, 1, 1)).toISOString() == "+100001020T010101");

        // Test B.C.
        assert(DateTime(Date(0, 12, 4), TimeOfDay(0, 12, 4)).toISOString() == "00001204T001204");
        assert(DateTime(Date(-9, 12, 4), TimeOfDay(0, 0, 0)).toISOString() == "-00091204T000000");
        assert(DateTime(Date(-99, 12, 4), TimeOfDay(5, 6, 12)).toISOString() == "-00991204T050612");
        assert(DateTime(Date(-999, 12, 4), TimeOfDay(13, 44, 59)).toISOString() == "-09991204T134459");
        assert(DateTime(Date(-9999, 7, 4), TimeOfDay(23, 59, 59)).toISOString() == "-99990704T235959");
        assert(DateTime(Date(-10000, 10, 20), TimeOfDay(1, 1, 1)).toISOString() == "-100001020T010101");

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        assert(cdt.toISOString() == "19990706T123033");
        assert(idt.toISOString() == "19990706T123033");
    }


    /++
        Converts this $(LREF DateTime) to a string with the format
        `YYYY-MM-DDTHH:MM:SS`. If `writer` is set, the resulting
        string will be written directly to it.

        Params:
            writer = A `char` accepting
            $(REF_ALTTEXT output range, isOutputRange, std, range, primitives)
        Returns:
            A `string` when not using an output range; `void` otherwise.
      +/
    string toISOExtString() const @safe pure nothrow
    {
        import std.array : appender;
        auto w = appender!string();
        w.reserve(20);
        try
            toISOExtString(w);
        catch (Exception e)
            assert(0, "toISOExtString() threw.");
        return w.data;
    }

    /// ditto
    void toISOExtString(W)(ref W writer) const
    if (isOutputRange!(W, char))
    {
        import std.format.write : formattedWrite;
        _date.toISOExtString(writer);
        formattedWrite!("T%02d:%02d:%02d")(
            writer,
            _tod._hour,
            _tod._minute,
            _tod._second
        );
    }

    ///
    @safe unittest
    {
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
        // Test A.D.
        assert(DateTime(Date(9, 12, 4), TimeOfDay(0, 0, 0)).toISOExtString() == "0009-12-04T00:00:00");
        assert(DateTime(Date(99, 12, 4), TimeOfDay(5, 6, 12)).toISOExtString() == "0099-12-04T05:06:12");
        assert(DateTime(Date(999, 12, 4), TimeOfDay(13, 44, 59)).toISOExtString() == "0999-12-04T13:44:59");
        assert(DateTime(Date(9999, 7, 4), TimeOfDay(23, 59, 59)).toISOExtString() == "9999-07-04T23:59:59");
        assert(DateTime(Date(10000, 10, 20), TimeOfDay(1, 1, 1)).toISOExtString() == "+10000-10-20T01:01:01");

        // Test B.C.
        assert(DateTime(Date(0, 12, 4), TimeOfDay(0, 12, 4)).toISOExtString() == "0000-12-04T00:12:04");
        assert(DateTime(Date(-9, 12, 4), TimeOfDay(0, 0, 0)).toISOExtString() == "-0009-12-04T00:00:00");
        assert(DateTime(Date(-99, 12, 4), TimeOfDay(5, 6, 12)).toISOExtString() == "-0099-12-04T05:06:12");
        assert(DateTime(Date(-999, 12, 4), TimeOfDay(13, 44, 59)).toISOExtString() == "-0999-12-04T13:44:59");
        assert(DateTime(Date(-9999, 7, 4), TimeOfDay(23, 59, 59)).toISOExtString() == "-9999-07-04T23:59:59");
        assert(DateTime(Date(-10000, 10, 20), TimeOfDay(1, 1, 1)).toISOExtString() == "-10000-10-20T01:01:01");

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        assert(cdt.toISOExtString() == "1999-07-06T12:30:33");
        assert(idt.toISOExtString() == "1999-07-06T12:30:33");
    }

    /++
        Converts this $(LREF DateTime) to a string with the format
        `YYYY-Mon-DD HH:MM:SS`. If `writer` is set, the resulting
        string will be written directly to it.

        Params:
            writer = A `char` accepting
            $(REF_ALTTEXT output range, isOutputRange, std, range, primitives)
        Returns:
            A `string` when not using an output range; `void` otherwise.
      +/
    string toSimpleString() const @safe pure nothrow
    {
        import std.array : appender;
        auto w = appender!string();
        w.reserve(22);
        try
            toSimpleString(w);
        catch (Exception e)
            assert(0, "toSimpleString() threw.");
        return w.data;
    }

    /// ditto
    void toSimpleString(W)(ref W writer) const
    if (isOutputRange!(W, char))
    {
        import std.format.write : formattedWrite;
        _date.toSimpleString(writer);
        formattedWrite!(" %02d:%02d:%02d")(
            writer,
            _tod._hour,
            _tod._minute,
            _tod._second
        );
    }

    ///
    @safe unittest
    {
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
        // Test A.D.
        assert(DateTime(Date(9, 12, 4), TimeOfDay(0, 0, 0)).toSimpleString() == "0009-Dec-04 00:00:00");
        assert(DateTime(Date(99, 12, 4), TimeOfDay(5, 6, 12)).toSimpleString() == "0099-Dec-04 05:06:12");
        assert(DateTime(Date(999, 12, 4), TimeOfDay(13, 44, 59)).toSimpleString() == "0999-Dec-04 13:44:59");
        assert(DateTime(Date(9999, 7, 4), TimeOfDay(23, 59, 59)).toSimpleString() == "9999-Jul-04 23:59:59");
        assert(DateTime(Date(10000, 10, 20), TimeOfDay(1, 1, 1)).toSimpleString() == "+10000-Oct-20 01:01:01");

        // Test B.C.
        assert(DateTime(Date(0, 12, 4), TimeOfDay(0, 12, 4)).toSimpleString() == "0000-Dec-04 00:12:04");
        assert(DateTime(Date(-9, 12, 4), TimeOfDay(0, 0, 0)).toSimpleString() == "-0009-Dec-04 00:00:00");
        assert(DateTime(Date(-99, 12, 4), TimeOfDay(5, 6, 12)).toSimpleString() == "-0099-Dec-04 05:06:12");
        assert(DateTime(Date(-999, 12, 4), TimeOfDay(13, 44, 59)).toSimpleString() == "-0999-Dec-04 13:44:59");
        assert(DateTime(Date(-9999, 7, 4), TimeOfDay(23, 59, 59)).toSimpleString() == "-9999-Jul-04 23:59:59");
        assert(DateTime(Date(-10000, 10, 20), TimeOfDay(1, 1, 1)).toSimpleString() == "-10000-Oct-20 01:01:01");

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        assert(cdt.toSimpleString() == "1999-Jul-06 12:30:33");
        assert(idt.toSimpleString() == "1999-Jul-06 12:30:33");
    }


    /++
        Converts this $(LREF DateTime) to a string.

        This function exists to make it easy to convert a $(LREF DateTime) to a
        string for code that does not care what the exact format is - just that
        it presents the information in a clear manner. It also makes it easy to
        simply convert a $(LREF DateTime) to a string when using functions such
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
        that $(LREF DateTime) has no `fromString` function, whereas it does have
        `fromISOString`, `fromISOExtString`, and `fromSimpleString`.

        The format returned by toString may or may not change in the future.
      +/
    string toString() const @safe pure nothrow
    {
        return toSimpleString();
    }

    @safe unittest
    {
        auto dt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        const cdt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        immutable idt = DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33));
        assert(dt.toString());
        assert(cdt.toString());
        assert(idt.toString());
    }

    /// ditto
    void toString(W)(ref W writer) const
    if (isOutputRange!(W, char))
    {
        toSimpleString(writer);
    }

    /++
        Creates a $(LREF DateTime) from a string with the format YYYYMMDDTHHMMSS.
        Whitespace is stripped from the given string.

        Params:
            isoString = A string formatted in the ISO format for dates and times.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given string is
            not in the ISO format or if the resulting $(LREF DateTime) would not
            be valid.
      +/
    static DateTime fromISOString(S)(scope const S isoString) @safe pure
        if (isSomeString!S)
    {
        import std.algorithm.searching : countUntil;
        import std.exception : enforce;
        import std.format : format;
        import std.string : strip;
        import std.utf : byCodeUnit;

        auto str = strip(isoString);

        enforce(str.length >= 15, new DateTimeException(format("Invalid ISO String: %s", isoString)));
        auto t = str.byCodeUnit.countUntil('T');

        enforce(t != -1, new DateTimeException(format("Invalid ISO String: %s", isoString)));

        immutable date = Date.fromISOString(str[0 .. t]);
        immutable tod = TimeOfDay.fromISOString(str[t+1 .. $]);

        return DateTime(date, tod);
    }

    ///
    @safe unittest
    {
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
        assertThrown!DateTimeException(DateTime.fromISOString(""));
        assertThrown!DateTimeException(DateTime.fromISOString("20100704000000"));
        assertThrown!DateTimeException(DateTime.fromISOString("20100704 000000"));
        assertThrown!DateTimeException(DateTime.fromISOString("20100704t000000"));
        assertThrown!DateTimeException(DateTime.fromISOString("20100704T000000."));
        assertThrown!DateTimeException(DateTime.fromISOString("20100704T000000.0"));

        assertThrown!DateTimeException(DateTime.fromISOString("2010-07-0400:00:00"));
        assertThrown!DateTimeException(DateTime.fromISOString("2010-07-04 00:00:00"));
        assertThrown!DateTimeException(DateTime.fromISOString("2010-07-04t00:00:00"));
        assertThrown!DateTimeException(DateTime.fromISOString("2010-07-04T00:00:00."));
        assertThrown!DateTimeException(DateTime.fromISOString("2010-07-04T00:00:00.0"));

        assertThrown!DateTimeException(DateTime.fromISOString("2010-Jul-0400:00:00"));
        assertThrown!DateTimeException(DateTime.fromISOString("2010-Jul-04 00:00:00"));
        assertThrown!DateTimeException(DateTime.fromISOString("2010-Jul-04t00:00:00"));
        assertThrown!DateTimeException(DateTime.fromISOString("2010-Jul-04T00:00:00"));
        assertThrown!DateTimeException(DateTime.fromISOString("2010-Jul-04 00:00:00."));
        assertThrown!DateTimeException(DateTime.fromISOString("2010-Jul-04 00:00:00.0"));

        assertThrown!DateTimeException(DateTime.fromISOString("2010-12-22T172201"));
        assertThrown!DateTimeException(DateTime.fromISOString("2010-Dec-22 17:22:01"));

        assert(DateTime.fromISOString("20101222T172201") == DateTime(Date(2010, 12, 22), TimeOfDay(17, 22, 1)));
        assert(DateTime.fromISOString("19990706T123033") == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        assert(DateTime.fromISOString("-19990706T123033") == DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)));
        assert(DateTime.fromISOString("+019990706T123033") == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        assert(DateTime.fromISOString("19990706T123033 ") == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        assert(DateTime.fromISOString(" 19990706T123033") == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        assert(DateTime.fromISOString(" 19990706T123033 ") == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
    }

    // https://issues.dlang.org/show_bug.cgi?id=17801
    @safe unittest
    {
        import std.conv : to;
        import std.meta : AliasSeq;
        static foreach (C; AliasSeq!(char, wchar, dchar))
        {
            static foreach (S; AliasSeq!(C[], const(C)[], immutable(C)[]))
                assert(DateTime.fromISOString(to!S("20121221T141516")) == DateTime(2012, 12, 21, 14, 15, 16));
        }
    }


    /++
        Creates a $(LREF DateTime) from a string with the format
        YYYY-MM-DDTHH:MM:SS. Whitespace is stripped from the given string.

        Params:
            isoExtString = A string formatted in the ISO Extended format for dates
                           and times.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given string is
            not in the ISO Extended format or if the resulting $(LREF DateTime)
            would not be valid.
      +/
    static DateTime fromISOExtString(S)(scope const S isoExtString) @safe pure
        if (isSomeString!(S))
    {
        import std.algorithm.searching : countUntil;
        import std.exception : enforce;
        import std.format : format;
        import std.string : strip;
        import std.utf : byCodeUnit;

        auto str = strip(isoExtString);

        enforce(str.length >= 15, new DateTimeException(format("Invalid ISO Extended String: %s", isoExtString)));
        auto t = str.byCodeUnit.countUntil('T');

        enforce(t != -1, new DateTimeException(format("Invalid ISO Extended String: %s", isoExtString)));

        immutable date = Date.fromISOExtString(str[0 .. t]);
        immutable tod = TimeOfDay.fromISOExtString(str[t+1 .. $]);

        return DateTime(date, tod);
    }

    ///
    @safe unittest
    {
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
        assertThrown!DateTimeException(DateTime.fromISOExtString(""));
        assertThrown!DateTimeException(DateTime.fromISOExtString("20100704000000"));
        assertThrown!DateTimeException(DateTime.fromISOExtString("20100704 000000"));
        assertThrown!DateTimeException(DateTime.fromISOExtString("20100704t000000"));
        assertThrown!DateTimeException(DateTime.fromISOExtString("20100704T000000."));
        assertThrown!DateTimeException(DateTime.fromISOExtString("20100704T000000.0"));

        assertThrown!DateTimeException(DateTime.fromISOExtString("2010-07:0400:00:00"));
        assertThrown!DateTimeException(DateTime.fromISOExtString("2010-07-04 00:00:00"));
        assertThrown!DateTimeException(DateTime.fromISOExtString("2010-07-04 00:00:00"));
        assertThrown!DateTimeException(DateTime.fromISOExtString("2010-07-04t00:00:00"));
        assertThrown!DateTimeException(DateTime.fromISOExtString("2010-07-04T00:00:00."));
        assertThrown!DateTimeException(DateTime.fromISOExtString("2010-07-04T00:00:00.0"));

        assertThrown!DateTimeException(DateTime.fromISOExtString("2010-Jul-0400:00:00"));
        assertThrown!DateTimeException(DateTime.fromISOExtString("2010-Jul-04t00:00:00"));
        assertThrown!DateTimeException(DateTime.fromISOExtString("2010-Jul-04 00:00:00."));
        assertThrown!DateTimeException(DateTime.fromISOExtString("2010-Jul-04 00:00:00.0"));

        assertThrown!DateTimeException(DateTime.fromISOExtString("20101222T172201"));
        assertThrown!DateTimeException(DateTime.fromISOExtString("2010-Dec-22 17:22:01"));

        assert(DateTime.fromISOExtString("2010-12-22T17:22:01") == DateTime(Date(2010, 12, 22), TimeOfDay(17, 22, 1)));
        assert(DateTime.fromISOExtString("1999-07-06T12:30:33") == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        assert(DateTime.fromISOExtString("-1999-07-06T12:30:33") == DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)));
        assert(DateTime.fromISOExtString("+01999-07-06T12:30:33") == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        assert(DateTime.fromISOExtString("1999-07-06T12:30:33 ") == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        assert(DateTime.fromISOExtString(" 1999-07-06T12:30:33") == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        assert(DateTime.fromISOExtString(" 1999-07-06T12:30:33 ") == DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
    }

    // https://issues.dlang.org/show_bug.cgi?id=17801
    @safe unittest
    {
        import std.conv : to;
        import std.meta : AliasSeq;
        static foreach (C; AliasSeq!(char, wchar, dchar))
        {
            static foreach (S; AliasSeq!(C[], const(C)[], immutable(C)[]))
                assert(DateTime.fromISOExtString(to!S("2012-12-21T14:15:16")) == DateTime(2012, 12, 21, 14, 15, 16));
        }
    }


    /++
        Creates a $(LREF DateTime) from a string with the format
        YYYY-Mon-DD HH:MM:SS. Whitespace is stripped from the given string.

        Params:
            simpleString = A string formatted in the way that toSimpleString
                           formats dates and times.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given string is
            not in the correct format or if the resulting $(LREF DateTime)
            would not be valid.
      +/
    static DateTime fromSimpleString(S)(scope const S simpleString) @safe pure
        if (isSomeString!(S))
    {
        import std.algorithm.searching : countUntil;
        import std.exception : enforce;
        import std.format : format;
        import std.string : strip;
        import std.utf : byCodeUnit;

        auto str = strip(simpleString);

        enforce(str.length >= 15, new DateTimeException(format("Invalid string format: %s", simpleString)));
        auto t = str.byCodeUnit.countUntil(' ');

        enforce(t != -1, new DateTimeException(format("Invalid string format: %s", simpleString)));

        immutable date = Date.fromSimpleString(str[0 .. t]);
        immutable tod = TimeOfDay.fromISOExtString(str[t+1 .. $]);

        return DateTime(date, tod);
    }

    ///
    @safe unittest
    {
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

    @safe unittest
    {
        assertThrown!DateTimeException(DateTime.fromISOString(""));
        assertThrown!DateTimeException(DateTime.fromISOString("20100704000000"));
        assertThrown!DateTimeException(DateTime.fromISOString("20100704 000000"));
        assertThrown!DateTimeException(DateTime.fromISOString("20100704t000000"));
        assertThrown!DateTimeException(DateTime.fromISOString("20100704T000000."));
        assertThrown!DateTimeException(DateTime.fromISOString("20100704T000000.0"));

        assertThrown!DateTimeException(DateTime.fromISOString("2010-07-0400:00:00"));
        assertThrown!DateTimeException(DateTime.fromISOString("2010-07-04 00:00:00"));
        assertThrown!DateTimeException(DateTime.fromISOString("2010-07-04t00:00:00"));
        assertThrown!DateTimeException(DateTime.fromISOString("2010-07-04T00:00:00."));
        assertThrown!DateTimeException(DateTime.fromISOString("2010-07-04T00:00:00.0"));

        assertThrown!DateTimeException(DateTime.fromISOString("2010-Jul-0400:00:00"));
        assertThrown!DateTimeException(DateTime.fromISOString("2010-Jul-04 00:00:00"));
        assertThrown!DateTimeException(DateTime.fromISOString("2010-Jul-04t00:00:00"));
        assertThrown!DateTimeException(DateTime.fromISOString("2010-Jul-04T00:00:00"));
        assertThrown!DateTimeException(DateTime.fromISOString("2010-Jul-04 00:00:00."));
        assertThrown!DateTimeException(DateTime.fromISOString("2010-Jul-04 00:00:00.0"));

        assertThrown!DateTimeException(DateTime.fromSimpleString("20101222T172201"));
        assertThrown!DateTimeException(DateTime.fromSimpleString("2010-12-22T172201"));

        assert(DateTime.fromSimpleString("2010-Dec-22 17:22:01") ==
               DateTime(Date(2010, 12, 22), TimeOfDay(17, 22, 1)));
        assert(DateTime.fromSimpleString("1999-Jul-06 12:30:33") ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        assert(DateTime.fromSimpleString("-1999-Jul-06 12:30:33") ==
               DateTime(Date(-1999, 7, 6), TimeOfDay(12, 30, 33)));
        assert(DateTime.fromSimpleString("+01999-Jul-06 12:30:33") ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        assert(DateTime.fromSimpleString("1999-Jul-06 12:30:33 ") ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        assert(DateTime.fromSimpleString(" 1999-Jul-06 12:30:33") ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
        assert(DateTime.fromSimpleString(" 1999-Jul-06 12:30:33 ") ==
               DateTime(Date(1999, 7, 6), TimeOfDay(12, 30, 33)));
    }

    // https://issues.dlang.org/show_bug.cgi?id=17801
    @safe unittest
    {
        import std.conv : to;
        import std.meta : AliasSeq;
        static foreach (C; AliasSeq!(char, wchar, dchar))
        {
            static foreach (S; AliasSeq!(C[], const(C)[], immutable(C)[]))
                assert(DateTime.fromSimpleString(to!S("2012-Dec-21 14:15:16")) == DateTime(2012, 12, 21, 14, 15, 16));
        }
    }


    /++
        Returns the $(LREF DateTime) farthest in the past which is representable
        by $(LREF DateTime).
      +/
    @property static DateTime min() @safe pure nothrow @nogc
    out(result)
    {
        assert(result._date == Date.min);
        assert(result._tod == TimeOfDay.min);
    }
    do
    {
        auto dt = DateTime.init;
        dt._date._year = short.min;
        dt._date._month = Month.jan;
        dt._date._day = 1;

        return dt;
    }

    @safe unittest
    {
        assert(DateTime.min.year < 0);
        assert(DateTime.min < DateTime.max);
    }


    /++
        Returns the $(LREF DateTime) farthest in the future which is
        representable by $(LREF DateTime).
      +/
    @property static DateTime max() @safe pure nothrow @nogc
    out(result)
    {
        assert(result._date == Date.max);
        assert(result._tod == TimeOfDay.max);
    }
    do
    {
        auto dt = DateTime.init;
        dt._date._year = short.max;
        dt._date._month = Month.dec;
        dt._date._day = 31;
        dt._tod._hour = TimeOfDay.maxHour;
        dt._tod._minute = TimeOfDay.maxMinute;
        dt._tod._second = TimeOfDay.maxSecond;

        return dt;
    }

    @safe unittest
    {
        assert(DateTime.max.year > 0);
        assert(DateTime.max > DateTime.min);
    }


private:

    /+
        Add seconds to the time of day. Negative values will subtract. If the
        number of seconds overflows (or underflows), then the seconds will wrap,
        increasing (or decreasing) the number of minutes accordingly. The
        same goes for any larger units.

        Params:
            seconds = The number of seconds to add to this $(LREF DateTime).
      +/
    ref DateTime _addSeconds(long seconds) return @safe pure nothrow @nogc
    {
        import core.time : convert;
        long hnsecs = convert!("seconds", "hnsecs")(seconds);
        hnsecs += convert!("hours", "hnsecs")(_tod._hour);
        hnsecs += convert!("minutes", "hnsecs")(_tod._minute);
        hnsecs += convert!("seconds", "hnsecs")(_tod._second);

        auto days = splitUnitsFromHNSecs!"days"(hnsecs);

        if (hnsecs < 0)
        {
            hnsecs += convert!("days", "hnsecs")(1);
            --days;
        }

        _date._addDays(days);

        immutable newHours = splitUnitsFromHNSecs!"hours"(hnsecs);
        immutable newMinutes = splitUnitsFromHNSecs!"minutes"(hnsecs);
        immutable newSeconds = splitUnitsFromHNSecs!"seconds"(hnsecs);

        _tod._hour = cast(ubyte) newHours;
        _tod._minute = cast(ubyte) newMinutes;
        _tod._second = cast(ubyte) newSeconds;

        return this;
    }

    @safe unittest
    {
        static void testDT(DateTime orig, int seconds, DateTime expected, size_t line = __LINE__)
        {
            orig._addSeconds(seconds);
            assert(orig == expected);
        }

        // Test A.D.
        testDT(DateTime(1999, 7, 6, 12, 30, 33), 0, DateTime(1999, 7, 6, 12, 30, 33));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), 1, DateTime(1999, 7, 6, 12, 30, 34));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), 2, DateTime(1999, 7, 6, 12, 30, 35));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), 3, DateTime(1999, 7, 6, 12, 30, 36));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), 4, DateTime(1999, 7, 6, 12, 30, 37));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), 5, DateTime(1999, 7, 6, 12, 30, 38));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), 10, DateTime(1999, 7, 6, 12, 30, 43));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), 15, DateTime(1999, 7, 6, 12, 30, 48));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), 26, DateTime(1999, 7, 6, 12, 30, 59));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), 27, DateTime(1999, 7, 6, 12, 31, 0));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), 30, DateTime(1999, 7, 6, 12, 31, 3));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), 59, DateTime(1999, 7, 6, 12, 31, 32));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), 60, DateTime(1999, 7, 6, 12, 31, 33));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), 61, DateTime(1999, 7, 6, 12, 31, 34));

        testDT(DateTime(1999, 7, 6, 12, 30, 33), 1766, DateTime(1999, 7, 6, 12, 59, 59));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), 1767, DateTime(1999, 7, 6, 13, 0, 0));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), 1768, DateTime(1999, 7, 6, 13, 0, 1));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), 2007, DateTime(1999, 7, 6, 13, 4, 0));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), 3599, DateTime(1999, 7, 6, 13, 30, 32));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), 3600, DateTime(1999, 7, 6, 13, 30, 33));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), 3601, DateTime(1999, 7, 6, 13, 30, 34));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), 7200, DateTime(1999, 7, 6, 14, 30, 33));
        testDT(DateTime(1999, 7, 6, 23, 0, 0), 432_123, DateTime(1999, 7, 11, 23, 2, 3));

        testDT(DateTime(1999, 7, 6, 12, 30, 33), -1, DateTime(1999, 7, 6, 12, 30, 32));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), -2, DateTime(1999, 7, 6, 12, 30, 31));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), -3, DateTime(1999, 7, 6, 12, 30, 30));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), -4, DateTime(1999, 7, 6, 12, 30, 29));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), -5, DateTime(1999, 7, 6, 12, 30, 28));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), -10, DateTime(1999, 7, 6, 12, 30, 23));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), -15, DateTime(1999, 7, 6, 12, 30, 18));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), -33, DateTime(1999, 7, 6, 12, 30, 0));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), -34, DateTime(1999, 7, 6, 12, 29, 59));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), -35, DateTime(1999, 7, 6, 12, 29, 58));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), -59, DateTime(1999, 7, 6, 12, 29, 34));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), -60, DateTime(1999, 7, 6, 12, 29, 33));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), -61, DateTime(1999, 7, 6, 12, 29, 32));

        testDT(DateTime(1999, 7, 6, 12, 30, 33), -1833, DateTime(1999, 7, 6, 12, 0, 0));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), -1834, DateTime(1999, 7, 6, 11, 59, 59));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), -3600, DateTime(1999, 7, 6, 11, 30, 33));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), -3601, DateTime(1999, 7, 6, 11, 30, 32));
        testDT(DateTime(1999, 7, 6, 12, 30, 33), -5134, DateTime(1999, 7, 6, 11, 4, 59));
        testDT(DateTime(1999, 7, 6, 23, 0, 0), -432_123, DateTime(1999, 7, 1, 22, 57, 57));

        testDT(DateTime(1999, 7, 6, 12, 30, 0), 1, DateTime(1999, 7, 6, 12, 30, 1));
        testDT(DateTime(1999, 7, 6, 12, 30, 0), 0, DateTime(1999, 7, 6, 12, 30, 0));
        testDT(DateTime(1999, 7, 6, 12, 30, 0), -1, DateTime(1999, 7, 6, 12, 29, 59));

        testDT(DateTime(1999, 7, 6, 12, 0, 0), 1, DateTime(1999, 7, 6, 12, 0, 1));
        testDT(DateTime(1999, 7, 6, 12, 0, 0), 0, DateTime(1999, 7, 6, 12, 0, 0));
        testDT(DateTime(1999, 7, 6, 12, 0, 0), -1, DateTime(1999, 7, 6, 11, 59, 59));

        testDT(DateTime(1999, 7, 6, 0, 0, 0), 1, DateTime(1999, 7, 6, 0, 0, 1));
        testDT(DateTime(1999, 7, 6, 0, 0, 0), 0, DateTime(1999, 7, 6, 0, 0, 0));
        testDT(DateTime(1999, 7, 6, 0, 0, 0), -1, DateTime(1999, 7, 5, 23, 59, 59));

        testDT(DateTime(1999, 7, 5, 23, 59, 59), 1, DateTime(1999, 7, 6, 0, 0, 0));
        testDT(DateTime(1999, 7, 5, 23, 59, 59), 0, DateTime(1999, 7, 5, 23, 59, 59));
        testDT(DateTime(1999, 7, 5, 23, 59, 59), -1, DateTime(1999, 7, 5, 23, 59, 58));

        testDT(DateTime(1998, 12, 31, 23, 59, 59), 1, DateTime(1999, 1, 1, 0, 0, 0));
        testDT(DateTime(1998, 12, 31, 23, 59, 59), 0, DateTime(1998, 12, 31, 23, 59, 59));
        testDT(DateTime(1998, 12, 31, 23, 59, 59), -1, DateTime(1998, 12, 31, 23, 59, 58));

        testDT(DateTime(1998, 1, 1, 0, 0, 0), 1, DateTime(1998, 1, 1, 0, 0, 1));
        testDT(DateTime(1998, 1, 1, 0, 0, 0), 0, DateTime(1998, 1, 1, 0, 0, 0));
        testDT(DateTime(1998, 1, 1, 0, 0, 0), -1, DateTime(1997, 12, 31, 23, 59, 59));

        // Test B.C.
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), 0, DateTime(-1999, 7, 6, 12, 30, 33));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), 1, DateTime(-1999, 7, 6, 12, 30, 34));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), 2, DateTime(-1999, 7, 6, 12, 30, 35));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), 3, DateTime(-1999, 7, 6, 12, 30, 36));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), 4, DateTime(-1999, 7, 6, 12, 30, 37));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), 5, DateTime(-1999, 7, 6, 12, 30, 38));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), 10, DateTime(-1999, 7, 6, 12, 30, 43));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), 15, DateTime(-1999, 7, 6, 12, 30, 48));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), 26, DateTime(-1999, 7, 6, 12, 30, 59));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), 27, DateTime(-1999, 7, 6, 12, 31, 0));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), 30, DateTime(-1999, 7, 6, 12, 31, 3));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), 59, DateTime(-1999, 7, 6, 12, 31, 32));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), 60, DateTime(-1999, 7, 6, 12, 31, 33));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), 61, DateTime(-1999, 7, 6, 12, 31, 34));

        testDT(DateTime(-1999, 7, 6, 12, 30, 33), 1766, DateTime(-1999, 7, 6, 12, 59, 59));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), 1767, DateTime(-1999, 7, 6, 13, 0, 0));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), 1768, DateTime(-1999, 7, 6, 13, 0, 1));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), 2007, DateTime(-1999, 7, 6, 13, 4, 0));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), 3599, DateTime(-1999, 7, 6, 13, 30, 32));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), 3600, DateTime(-1999, 7, 6, 13, 30, 33));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), 3601, DateTime(-1999, 7, 6, 13, 30, 34));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), 7200, DateTime(-1999, 7, 6, 14, 30, 33));
        testDT(DateTime(-1999, 7, 6, 23, 0, 0), 432_123, DateTime(-1999, 7, 11, 23, 2, 3));

        testDT(DateTime(-1999, 7, 6, 12, 30, 33), -1, DateTime(-1999, 7, 6, 12, 30, 32));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), -2, DateTime(-1999, 7, 6, 12, 30, 31));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), -3, DateTime(-1999, 7, 6, 12, 30, 30));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), -4, DateTime(-1999, 7, 6, 12, 30, 29));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), -5, DateTime(-1999, 7, 6, 12, 30, 28));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), -10, DateTime(-1999, 7, 6, 12, 30, 23));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), -15, DateTime(-1999, 7, 6, 12, 30, 18));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), -33, DateTime(-1999, 7, 6, 12, 30, 0));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), -34, DateTime(-1999, 7, 6, 12, 29, 59));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), -35, DateTime(-1999, 7, 6, 12, 29, 58));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), -59, DateTime(-1999, 7, 6, 12, 29, 34));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), -60, DateTime(-1999, 7, 6, 12, 29, 33));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), -61, DateTime(-1999, 7, 6, 12, 29, 32));

        testDT(DateTime(-1999, 7, 6, 12, 30, 33), -1833, DateTime(-1999, 7, 6, 12, 0, 0));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), -1834, DateTime(-1999, 7, 6, 11, 59, 59));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), -3600, DateTime(-1999, 7, 6, 11, 30, 33));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), -3601, DateTime(-1999, 7, 6, 11, 30, 32));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), -5134, DateTime(-1999, 7, 6, 11, 4, 59));
        testDT(DateTime(-1999, 7, 6, 12, 30, 33), -7200, DateTime(-1999, 7, 6, 10, 30, 33));
        testDT(DateTime(-1999, 7, 6, 23, 0, 0), -432_123, DateTime(-1999, 7, 1, 22, 57, 57));

        testDT(DateTime(-1999, 7, 6, 12, 30, 0), 1, DateTime(-1999, 7, 6, 12, 30, 1));
        testDT(DateTime(-1999, 7, 6, 12, 30, 0), 0, DateTime(-1999, 7, 6, 12, 30, 0));
        testDT(DateTime(-1999, 7, 6, 12, 30, 0), -1, DateTime(-1999, 7, 6, 12, 29, 59));

        testDT(DateTime(-1999, 7, 6, 12, 0, 0), 1, DateTime(-1999, 7, 6, 12, 0, 1));
        testDT(DateTime(-1999, 7, 6, 12, 0, 0), 0, DateTime(-1999, 7, 6, 12, 0, 0));
        testDT(DateTime(-1999, 7, 6, 12, 0, 0), -1, DateTime(-1999, 7, 6, 11, 59, 59));

        testDT(DateTime(-1999, 7, 6, 0, 0, 0), 1, DateTime(-1999, 7, 6, 0, 0, 1));
        testDT(DateTime(-1999, 7, 6, 0, 0, 0), 0, DateTime(-1999, 7, 6, 0, 0, 0));
        testDT(DateTime(-1999, 7, 6, 0, 0, 0), -1, DateTime(-1999, 7, 5, 23, 59, 59));

        testDT(DateTime(-1999, 7, 5, 23, 59, 59), 1, DateTime(-1999, 7, 6, 0, 0, 0));
        testDT(DateTime(-1999, 7, 5, 23, 59, 59), 0, DateTime(-1999, 7, 5, 23, 59, 59));
        testDT(DateTime(-1999, 7, 5, 23, 59, 59), -1, DateTime(-1999, 7, 5, 23, 59, 58));

        testDT(DateTime(-2000, 12, 31, 23, 59, 59), 1, DateTime(-1999, 1, 1, 0, 0, 0));
        testDT(DateTime(-2000, 12, 31, 23, 59, 59), 0, DateTime(-2000, 12, 31, 23, 59, 59));
        testDT(DateTime(-2000, 12, 31, 23, 59, 59), -1, DateTime(-2000, 12, 31, 23, 59, 58));

        testDT(DateTime(-2000, 1, 1, 0, 0, 0), 1, DateTime(-2000, 1, 1, 0, 0, 1));
        testDT(DateTime(-2000, 1, 1, 0, 0, 0), 0, DateTime(-2000, 1, 1, 0, 0, 0));
        testDT(DateTime(-2000, 1, 1, 0, 0, 0), -1, DateTime(-2001, 12, 31, 23, 59, 59));

        // Test Both
        testDT(DateTime(1, 1, 1, 0, 0, 0), -1, DateTime(0, 12, 31, 23, 59, 59));
        testDT(DateTime(0, 12, 31, 23, 59, 59), 1, DateTime(1, 1, 1, 0, 0, 0));

        testDT(DateTime(0, 1, 1, 0, 0, 0), -1, DateTime(-1, 12, 31, 23, 59, 59));
        testDT(DateTime(-1, 12, 31, 23, 59, 59), 1, DateTime(0, 1, 1, 0, 0, 0));

        testDT(DateTime(-1, 1, 1, 11, 30, 33), 63_165_600L, DateTime(1, 1, 1, 13, 30, 33));
        testDT(DateTime(1, 1, 1, 13, 30, 33), -63_165_600L, DateTime(-1, 1, 1, 11, 30, 33));

        testDT(DateTime(-1, 1, 1, 11, 30, 33), 63_165_617L, DateTime(1, 1, 1, 13, 30, 50));
        testDT(DateTime(1, 1, 1, 13, 30, 50), -63_165_617L, DateTime(-1, 1, 1, 11, 30, 33));

        const cdt = DateTime(1999, 7, 6, 12, 30, 33);
        immutable idt = DateTime(1999, 7, 6, 12, 30, 33);
        static assert(!__traits(compiles, cdt._addSeconds(4)));
        static assert(!__traits(compiles, idt._addSeconds(4)));
    }


    Date      _date;
    TimeOfDay _tod;
}

///
@safe pure unittest
{
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

/++
    Represents a date in the
    $(HTTP en.wikipedia.org/wiki/Proleptic_Gregorian_calendar, Proleptic
    Gregorian Calendar) ranging from 32,768 B.C. to 32,767 A.D. Positive years
    are A.D. Non-positive years are B.C.

    Year, month, and day are kept separately internally so that `Date` is
    optimized for calendar-based operations.

    `Date` uses the Proleptic Gregorian Calendar, so it assumes the Gregorian
    leap year calculations for its entire length. As per
    $(HTTP en.wikipedia.org/wiki/ISO_8601, ISO 8601), it treats 1 B.C. as
    year 0, i.e. 1 B.C. is 0, 2 B.C. is -1, etc. Use $(LREF yearBC) to use B.C.
    as a positive integer with 1 B.C. being the year prior to 1 A.D.

    Year 0 is a leap year.
 +/
struct Date
{
public:

    /++
        Throws:
            $(REF DateTimeException,std,datetime,date) if the resulting
            $(LREF Date) would not be valid.

        Params:
            year  = Year of the Gregorian Calendar. Positive values are A.D.
                    Non-positive values are B.C. with year 0 being the year
                    prior to 1 A.D.
            month = Month of the year (January is 1).
            day   = Day of the month.
     +/
    this(int year, int month, int day) @safe pure
    {
        enforceValid!"months"(cast(Month) month);
        enforceValid!"days"(year, cast(Month) month, day);

        _year  = cast(short) year;
        _month = cast(Month) month;
        _day   = cast(ubyte) day;
    }

    @safe unittest
    {
        import std.exception : assertNotThrown;
        assert(Date(1, 1, 1) == Date.init);

        static void testDate(Date date, int year, int month, int day)
        {
            assert(date._year == year);
            assert(date._month == month);
            assert(date._day == day);
        }

        testDate(Date(1999, 1 , 1), 1999, Month.jan, 1);
        testDate(Date(1999, 7 , 1), 1999, Month.jul, 1);
        testDate(Date(1999, 7 , 6), 1999, Month.jul, 6);

        // Test A.D.
        assertThrown!DateTimeException(Date(1, 0, 1));
        assertThrown!DateTimeException(Date(1, 1, 0));
        assertThrown!DateTimeException(Date(1999, 13, 1));
        assertThrown!DateTimeException(Date(1999, 1, 32));
        assertThrown!DateTimeException(Date(1999, 2, 29));
        assertThrown!DateTimeException(Date(2000, 2, 30));
        assertThrown!DateTimeException(Date(1999, 3, 32));
        assertThrown!DateTimeException(Date(1999, 4, 31));
        assertThrown!DateTimeException(Date(1999, 5, 32));
        assertThrown!DateTimeException(Date(1999, 6, 31));
        assertThrown!DateTimeException(Date(1999, 7, 32));
        assertThrown!DateTimeException(Date(1999, 8, 32));
        assertThrown!DateTimeException(Date(1999, 9, 31));
        assertThrown!DateTimeException(Date(1999, 10, 32));
        assertThrown!DateTimeException(Date(1999, 11, 31));
        assertThrown!DateTimeException(Date(1999, 12, 32));

        assertNotThrown!DateTimeException(Date(1999, 1, 31));
        assertNotThrown!DateTimeException(Date(1999, 2, 28));
        assertNotThrown!DateTimeException(Date(2000, 2, 29));
        assertNotThrown!DateTimeException(Date(1999, 3, 31));
        assertNotThrown!DateTimeException(Date(1999, 4, 30));
        assertNotThrown!DateTimeException(Date(1999, 5, 31));
        assertNotThrown!DateTimeException(Date(1999, 6, 30));
        assertNotThrown!DateTimeException(Date(1999, 7, 31));
        assertNotThrown!DateTimeException(Date(1999, 8, 31));
        assertNotThrown!DateTimeException(Date(1999, 9, 30));
        assertNotThrown!DateTimeException(Date(1999, 10, 31));
        assertNotThrown!DateTimeException(Date(1999, 11, 30));
        assertNotThrown!DateTimeException(Date(1999, 12, 31));

        // Test B.C.
        assertNotThrown!DateTimeException(Date(0, 1, 1));
        assertNotThrown!DateTimeException(Date(-1, 1, 1));
        assertNotThrown!DateTimeException(Date(-1, 12, 31));
        assertNotThrown!DateTimeException(Date(-1, 2, 28));
        assertNotThrown!DateTimeException(Date(-4, 2, 29));

        assertThrown!DateTimeException(Date(-1, 2, 29));
        assertThrown!DateTimeException(Date(-2, 2, 29));
        assertThrown!DateTimeException(Date(-3, 2, 29));
    }


    /++
        Params:
            day = The Xth day of the Gregorian Calendar that the constructed
                  $(LREF Date) will be for.
     +/
    this(int day) @safe pure nothrow @nogc
    {
        if (day > 0)
        {
            int years = (day / daysIn400Years) * 400 + 1;
            day %= daysIn400Years;

            {
                immutable tempYears = day / daysIn100Years;

                if (tempYears == 4)
                {
                    years += 300;
                    day -= daysIn100Years * 3;
                }
                else
                {
                    years += tempYears * 100;
                    day %= daysIn100Years;
                }
            }

            years += (day / daysIn4Years) * 4;
            day %= daysIn4Years;

            {
                immutable tempYears = day / daysInYear;

                if (tempYears == 4)
                {
                    years += 3;
                    day -= daysInYear * 3;
                }
                else
                {
                    years += tempYears;
                    day %= daysInYear;
                }
            }

            if (day == 0)
            {
                _year = cast(short)(years - 1);
                _month = Month.dec;
                _day = 31;
            }
            else
            {
                _year = cast(short) years;

                setDayOfYear(day);
            }
        }
        else if (day <= 0 && -day < daysInLeapYear)
        {
            _year = 0;

            setDayOfYear(daysInLeapYear + day);
        }
        else
        {
            day += daysInLeapYear - 1;
            int years = (day / daysIn400Years) * 400 - 1;
            day %= daysIn400Years;

            {
                immutable tempYears = day / daysIn100Years;

                if (tempYears == -4)
                {
                    years -= 300;
                    day += daysIn100Years * 3;
                }
                else
                {
                    years += tempYears * 100;
                    day %= daysIn100Years;
                }
            }

            years += (day / daysIn4Years) * 4;
            day %= daysIn4Years;

            {
                immutable tempYears = day / daysInYear;

                if (tempYears == -4)
                {
                    years -= 3;
                    day += daysInYear * 3;
                }
                else
                {
                    years += tempYears;
                    day %= daysInYear;
                }
            }

            if (day == 0)
            {
                _year = cast(short)(years + 1);
                _month = Month.jan;
                _day = 1;
            }
            else
            {
                _year = cast(short) years;
                immutable newDoY = (yearIsLeapYear(_year) ? daysInLeapYear : daysInYear) + day + 1;

                setDayOfYear(newDoY);
            }
        }
    }

    @safe unittest
    {
        import std.range : chain;

        // Test A.D.
        foreach (gd; chain(testGregDaysBC, testGregDaysAD))
            assert(Date(gd.day) == gd.date);
    }


    /++
        Compares this $(LREF Date) with the given $(LREF Date).

        Returns:
            $(BOOKTABLE,
            $(TR $(TD this &lt; rhs) $(TD &lt; 0))
            $(TR $(TD this == rhs) $(TD 0))
            $(TR $(TD this &gt; rhs) $(TD &gt; 0))
            )
     +/
    int opCmp(Date rhs) const @safe pure nothrow @nogc
    {
        if (_year < rhs._year)
            return -1;
        if (_year > rhs._year)
            return 1;

        if (_month < rhs._month)
            return -1;
        if (_month > rhs._month)
            return 1;

        if (_day < rhs._day)
            return -1;
        if (_day > rhs._day)
            return 1;

        return 0;
    }

    @safe unittest
    {
        // Test A.D.
        assert(Date(1, 1, 1).opCmp(Date.init) == 0);

        assert(Date(1999, 1, 1).opCmp(Date(1999, 1, 1)) == 0);
        assert(Date(1, 7, 1).opCmp(Date(1, 7, 1)) == 0);
        assert(Date(1, 1, 6).opCmp(Date(1, 1, 6)) == 0);

        assert(Date(1999, 7, 1).opCmp(Date(1999, 7, 1)) == 0);
        assert(Date(1999, 7, 6).opCmp(Date(1999, 7, 6)) == 0);

        assert(Date(1, 7, 6).opCmp(Date(1, 7, 6)) == 0);

        assert(Date(1999, 7, 6).opCmp(Date(2000, 7, 6)) < 0);
        assert(Date(2000, 7, 6).opCmp(Date(1999, 7, 6)) > 0);
        assert(Date(1999, 7, 6).opCmp(Date(1999, 8, 6)) < 0);
        assert(Date(1999, 8, 6).opCmp(Date(1999, 7, 6)) > 0);
        assert(Date(1999, 7, 6).opCmp(Date(1999, 7, 7)) < 0);
        assert(Date(1999, 7, 7).opCmp(Date(1999, 7, 6)) > 0);

        assert(Date(1999, 8, 7).opCmp(Date(2000, 7, 6)) < 0);
        assert(Date(2000, 8, 6).opCmp(Date(1999, 7, 7)) > 0);
        assert(Date(1999, 7, 7).opCmp(Date(2000, 7, 6)) < 0);
        assert(Date(2000, 7, 6).opCmp(Date(1999, 7, 7)) > 0);
        assert(Date(1999, 7, 7).opCmp(Date(1999, 8, 6)) < 0);
        assert(Date(1999, 8, 6).opCmp(Date(1999, 7, 7)) > 0);

        // Test B.C.
        assert(Date(0, 1, 1).opCmp(Date(0, 1, 1)) == 0);
        assert(Date(-1, 1, 1).opCmp(Date(-1, 1, 1)) == 0);
        assert(Date(-1, 7, 1).opCmp(Date(-1, 7, 1)) == 0);
        assert(Date(-1, 1, 6).opCmp(Date(-1, 1, 6)) == 0);

        assert(Date(-1999, 7, 1).opCmp(Date(-1999, 7, 1)) == 0);
        assert(Date(-1999, 7, 6).opCmp(Date(-1999, 7, 6)) == 0);

        assert(Date(-1, 7, 6).opCmp(Date(-1, 7, 6)) == 0);

        assert(Date(-2000, 7, 6).opCmp(Date(-1999, 7, 6)) < 0);
        assert(Date(-1999, 7, 6).opCmp(Date(-2000, 7, 6)) > 0);
        assert(Date(-1999, 7, 6).opCmp(Date(-1999, 8, 6)) < 0);
        assert(Date(-1999, 8, 6).opCmp(Date(-1999, 7, 6)) > 0);
        assert(Date(-1999, 7, 6).opCmp(Date(-1999, 7, 7)) < 0);
        assert(Date(-1999, 7, 7).opCmp(Date(-1999, 7, 6)) > 0);

        assert(Date(-2000, 8, 6).opCmp(Date(-1999, 7, 7)) < 0);
        assert(Date(-1999, 8, 7).opCmp(Date(-2000, 7, 6)) > 0);
        assert(Date(-2000, 7, 6).opCmp(Date(-1999, 7, 7)) < 0);
        assert(Date(-1999, 7, 7).opCmp(Date(-2000, 7, 6)) > 0);
        assert(Date(-1999, 7, 7).opCmp(Date(-1999, 8, 6)) < 0);
        assert(Date(-1999, 8, 6).opCmp(Date(-1999, 7, 7)) > 0);

        // Test Both
        assert(Date(-1999, 7, 6).opCmp(Date(1999, 7, 6)) < 0);
        assert(Date(1999, 7, 6).opCmp(Date(-1999, 7, 6)) > 0);

        assert(Date(-1999, 8, 6).opCmp(Date(1999, 7, 6)) < 0);
        assert(Date(1999, 7, 6).opCmp(Date(-1999, 8, 6)) > 0);

        assert(Date(-1999, 7, 7).opCmp(Date(1999, 7, 6)) < 0);
        assert(Date(1999, 7, 6).opCmp(Date(-1999, 7, 7)) > 0);

        assert(Date(-1999, 8, 7).opCmp(Date(1999, 7, 6)) < 0);
        assert(Date(1999, 7, 6).opCmp(Date(-1999, 8, 7)) > 0);

        assert(Date(-1999, 8, 6).opCmp(Date(1999, 6, 6)) < 0);
        assert(Date(1999, 6, 8).opCmp(Date(-1999, 7, 6)) > 0);

        auto date = Date(1999, 7, 6);
        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        assert(date.opCmp(date) == 0);
        assert(date.opCmp(cdate) == 0);
        assert(date.opCmp(idate) == 0);
        assert(cdate.opCmp(date) == 0);
        assert(cdate.opCmp(cdate) == 0);
        assert(cdate.opCmp(idate) == 0);
        assert(idate.opCmp(date) == 0);
        assert(idate.opCmp(cdate) == 0);
        assert(idate.opCmp(idate) == 0);
    }


    /++
        Year of the Gregorian Calendar. Positive numbers are A.D. Non-positive
        are B.C.
     +/
    @property short year() const @safe pure nothrow @nogc
    {
        return _year;
    }

    ///
    @safe unittest
    {
        assert(Date(1999, 7, 6).year == 1999);
        assert(Date(2010, 10, 4).year == 2010);
        assert(Date(-7, 4, 5).year == -7);
    }

    @safe unittest
    {
        assert(Date.init.year == 1);
        assert(Date(1999, 7, 6).year == 1999);
        assert(Date(-1999, 7, 6).year == -1999);

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        assert(cdate.year == 1999);
        assert(idate.year == 1999);
    }

    /++
        Year of the Gregorian Calendar. Positive numbers are A.D. Non-positive
        are B.C.

        Params:
            year = The year to set this Date's year to.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the new year is not
            a leap year and the resulting date would be on February 29th.
     +/
    @property void year(int year) @safe pure
    {
        enforceValid!"days"(year, _month, _day);
        _year = cast(short) year;
    }

    ///
    @safe unittest
    {
        assert(Date(1999, 7, 6).year == 1999);
        assert(Date(2010, 10, 4).year == 2010);
        assert(Date(-7, 4, 5).year == -7);
    }

    @safe unittest
    {
        static void testDateInvalid(Date date, int year)
        {
            date.year = year;
        }

        static void testDate(Date date, int year, Date expected)
        {
            date.year = year;
            assert(date == expected);
        }

        assertThrown!DateTimeException(testDateInvalid(Date(4, 2, 29), 1));

        testDate(Date(1, 1, 1), 1999, Date(1999, 1, 1));
        testDate(Date(1, 1, 1), 0, Date(0, 1, 1));
        testDate(Date(1, 1, 1), -1999, Date(-1999, 1, 1));

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        static assert(!__traits(compiles, cdate.year = 1999));
        static assert(!__traits(compiles, idate.year = 1999));
    }


    /++
        Year B.C. of the Gregorian Calendar counting year 0 as 1 B.C.

        Throws:
            $(REF DateTimeException,std,datetime,date) if `isAD` is true.
     +/
    @property ushort yearBC() const @safe pure
    {
        import std.format : format;

        if (isAD)
            throw new DateTimeException(format("Year %s is A.D.", _year));
        return cast(ushort)((_year * -1) + 1);
    }

    ///
    @safe unittest
    {
        assert(Date(0, 1, 1).yearBC == 1);
        assert(Date(-1, 1, 1).yearBC == 2);
        assert(Date(-100, 1, 1).yearBC == 101);
    }

    @safe unittest
    {
        assertThrown!DateTimeException((Date date){date.yearBC;}(Date(1, 1, 1)));

        auto date = Date(0, 7, 6);
        const cdate = Date(0, 7, 6);
        immutable idate = Date(0, 7, 6);
        assert(date.yearBC == 1);
        assert(cdate.yearBC == 1);
        assert(idate.yearBC == 1);
    }


    /++
        Year B.C. of the Gregorian Calendar counting year 0 as 1 B.C.

        Params:
            year = The year B.C. to set this $(LREF Date)'s year to.

        Throws:
            $(REF DateTimeException,std,datetime,date) if a non-positive value
            is given.
     +/
    @property void yearBC(int year) @safe pure
    {
        if (year <= 0)
            throw new DateTimeException("The given year is not a year B.C.");
        _year = cast(short)((year - 1) * -1);
    }

    ///
    @safe unittest
    {
        auto date = Date(2010, 1, 1);
        date.yearBC = 1;
        assert(date == Date(0, 1, 1));

        date.yearBC = 10;
        assert(date == Date(-9, 1, 1));
    }

    @safe unittest
    {
        assertThrown!DateTimeException((Date date){date.yearBC = -1;}(Date(1, 1, 1)));

        auto date = Date(0, 7, 6);
        const cdate = Date(0, 7, 6);
        immutable idate = Date(0, 7, 6);
        date.yearBC = 7;
        assert(date.yearBC == 7);
        static assert(!__traits(compiles, cdate.yearBC = 7));
        static assert(!__traits(compiles, idate.yearBC = 7));
    }


    /++
        Month of a Gregorian Year.
     +/
    @property Month month() const @safe pure nothrow @nogc
    {
        return _month;
    }

    ///
    @safe unittest
    {
        assert(Date(1999, 7, 6).month == 7);
        assert(Date(2010, 10, 4).month == 10);
        assert(Date(-7, 4, 5).month == 4);
    }

    @safe unittest
    {
        assert(Date.init.month == 1);
        assert(Date(1999, 7, 6).month == 7);
        assert(Date(-1999, 7, 6).month == 7);

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        assert(cdate.month == 7);
        assert(idate.month == 7);
    }

    /++
        Month of a Gregorian Year.

        Params:
            month = The month to set this $(LREF Date)'s month to.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given month is
            not a valid month or if the current day would not be valid in the
            given month.
     +/
    @property void month(Month month) @safe pure
    {
        enforceValid!"months"(month);
        enforceValid!"days"(_year, month, _day);
        _month = cast(Month) month;
    }

    @safe unittest
    {
        static void testDate(Date date, Month month, Date expected = Date.init)
        {
            date.month = month;
            assert(expected != Date.init);
            assert(date == expected);
        }

        assertThrown!DateTimeException(testDate(Date(1, 1, 1), cast(Month) 0));
        assertThrown!DateTimeException(testDate(Date(1, 1, 1), cast(Month) 13));
        assertThrown!DateTimeException(testDate(Date(1, 1, 29), cast(Month) 2));
        assertThrown!DateTimeException(testDate(Date(0, 1, 30), cast(Month) 2));

        testDate(Date(1, 1, 1), cast(Month) 7, Date(1, 7, 1));
        testDate(Date(-1, 1, 1), cast(Month) 7, Date(-1, 7, 1));

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        static assert(!__traits(compiles, cdate.month = 7));
        static assert(!__traits(compiles, idate.month = 7));
    }


    /++
        Day of a Gregorian Month.
     +/
    @property ubyte day() const @safe pure nothrow @nogc
    {
        return _day;
    }

    ///
    @safe unittest
    {
        assert(Date(1999, 7, 6).day == 6);
        assert(Date(2010, 10, 4).day == 4);
        assert(Date(-7, 4, 5).day == 5);
    }

    @safe unittest
    {
        import std.format : format;
        import std.range : chain;

        static void test(Date date, int expected)
        {
            assert(date.day == expected, format("Value given: %s", date));
        }

        foreach (year; chain(testYearsBC, testYearsAD))
        {
            foreach (md; testMonthDays)
                test(Date(year, md.month, md.day), md.day);
        }

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        assert(cdate.day == 6);
        assert(idate.day == 6);
    }

    /++
        Day of a Gregorian Month.

        Params:
            day = The day of the month to set this $(LREF Date)'s day to.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given day is not
            a valid day of the current month.
     +/
    @property void day(int day) @safe pure
    {
        enforceValid!"days"(_year, _month, day);
        _day = cast(ubyte) day;
    }

    @safe unittest
    {
        import std.exception : assertNotThrown;

        static void testDate(Date date, int day)
        {
            date.day = day;
        }

        // Test A.D.
        assertThrown!DateTimeException(testDate(Date(1, 1, 1), 0));
        assertThrown!DateTimeException(testDate(Date(1, 1, 1), 32));
        assertThrown!DateTimeException(testDate(Date(1, 2, 1), 29));
        assertThrown!DateTimeException(testDate(Date(4, 2, 1), 30));
        assertThrown!DateTimeException(testDate(Date(1, 3, 1), 32));
        assertThrown!DateTimeException(testDate(Date(1, 4, 1), 31));
        assertThrown!DateTimeException(testDate(Date(1, 5, 1), 32));
        assertThrown!DateTimeException(testDate(Date(1, 6, 1), 31));
        assertThrown!DateTimeException(testDate(Date(1, 7, 1), 32));
        assertThrown!DateTimeException(testDate(Date(1, 8, 1), 32));
        assertThrown!DateTimeException(testDate(Date(1, 9, 1), 31));
        assertThrown!DateTimeException(testDate(Date(1, 10, 1), 32));
        assertThrown!DateTimeException(testDate(Date(1, 11, 1), 31));
        assertThrown!DateTimeException(testDate(Date(1, 12, 1), 32));

        assertNotThrown!DateTimeException(testDate(Date(1, 1, 1), 31));
        assertNotThrown!DateTimeException(testDate(Date(1, 2, 1), 28));
        assertNotThrown!DateTimeException(testDate(Date(4, 2, 1), 29));
        assertNotThrown!DateTimeException(testDate(Date(1, 3, 1), 31));
        assertNotThrown!DateTimeException(testDate(Date(1, 4, 1), 30));
        assertNotThrown!DateTimeException(testDate(Date(1, 5, 1), 31));
        assertNotThrown!DateTimeException(testDate(Date(1, 6, 1), 30));
        assertNotThrown!DateTimeException(testDate(Date(1, 7, 1), 31));
        assertNotThrown!DateTimeException(testDate(Date(1, 8, 1), 31));
        assertNotThrown!DateTimeException(testDate(Date(1, 9, 1), 30));
        assertNotThrown!DateTimeException(testDate(Date(1, 10, 1), 31));
        assertNotThrown!DateTimeException(testDate(Date(1, 11, 1), 30));
        assertNotThrown!DateTimeException(testDate(Date(1, 12, 1), 31));

        {
            auto date = Date(1, 1, 1);
            date.day = 6;
            assert(date == Date(1, 1, 6));
        }

        // Test B.C.
        assertThrown!DateTimeException(testDate(Date(-1, 1, 1), 0));
        assertThrown!DateTimeException(testDate(Date(-1, 1, 1), 32));
        assertThrown!DateTimeException(testDate(Date(-1, 2, 1), 29));
        assertThrown!DateTimeException(testDate(Date(0, 2, 1), 30));
        assertThrown!DateTimeException(testDate(Date(-1, 3, 1), 32));
        assertThrown!DateTimeException(testDate(Date(-1, 4, 1), 31));
        assertThrown!DateTimeException(testDate(Date(-1, 5, 1), 32));
        assertThrown!DateTimeException(testDate(Date(-1, 6, 1), 31));
        assertThrown!DateTimeException(testDate(Date(-1, 7, 1), 32));
        assertThrown!DateTimeException(testDate(Date(-1, 8, 1), 32));
        assertThrown!DateTimeException(testDate(Date(-1, 9, 1), 31));
        assertThrown!DateTimeException(testDate(Date(-1, 10, 1), 32));
        assertThrown!DateTimeException(testDate(Date(-1, 11, 1), 31));
        assertThrown!DateTimeException(testDate(Date(-1, 12, 1), 32));

        assertNotThrown!DateTimeException(testDate(Date(-1, 1, 1), 31));
        assertNotThrown!DateTimeException(testDate(Date(-1, 2, 1), 28));
        assertNotThrown!DateTimeException(testDate(Date(0, 2, 1), 29));
        assertNotThrown!DateTimeException(testDate(Date(-1, 3, 1), 31));
        assertNotThrown!DateTimeException(testDate(Date(-1, 4, 1), 30));
        assertNotThrown!DateTimeException(testDate(Date(-1, 5, 1), 31));
        assertNotThrown!DateTimeException(testDate(Date(-1, 6, 1), 30));
        assertNotThrown!DateTimeException(testDate(Date(-1, 7, 1), 31));
        assertNotThrown!DateTimeException(testDate(Date(-1, 8, 1), 31));
        assertNotThrown!DateTimeException(testDate(Date(-1, 9, 1), 30));
        assertNotThrown!DateTimeException(testDate(Date(-1, 10, 1), 31));
        assertNotThrown!DateTimeException(testDate(Date(-1, 11, 1), 30));
        assertNotThrown!DateTimeException(testDate(Date(-1, 12, 1), 31));

        {
            auto date = Date(-1, 1, 1);
            date.day = 6;
            assert(date == Date(-1, 1, 6));
        }

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        static assert(!__traits(compiles, cdate.day = 6));
        static assert(!__traits(compiles, idate.day = 6));
    }


    /++
        Adds the given number of years or months to this $(LREF Date), mutating
        it. A negative number will subtract.

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
                            $(LREF Date).
            allowOverflow = Whether the day should be allowed to overflow,
                            causing the month to increment.

        Returns:
            A reference to the `Date` (`this`).
      +/
    @safe pure nothrow @nogc
    ref Date add(string units)(long value, AllowDayOverflow allowOverflow = AllowDayOverflow.yes)
        if (units == "years")
    {
        _year += value;

        if (_month == Month.feb && _day == 29 && !yearIsLeapYear(_year))
        {
            if (allowOverflow == AllowDayOverflow.yes)
            {
                _month = Month.mar;
                _day = 1;
            }
            else
                _day = 28;
        }

        return this;
    }

    ///
    @safe unittest
    {
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

    // Test add!"years"() with AllowDayOverflow.yes
    @safe unittest
    {
        // Test A.D.
        {
            auto date = Date(1999, 7, 6);
            date.add!"years"(7);
            assert(date == Date(2006, 7, 6));
            date.add!"years"(-9);
            assert(date == Date(1997, 7, 6));
        }

        {
            auto date = Date(1999, 2, 28);
            date.add!"years"(1);
            assert(date == Date(2000, 2, 28));
        }

        {
            auto date = Date(2000, 2, 29);
            date.add!"years"(-1);
            assert(date == Date(1999, 3, 1));
        }

        // Test B.C.
        {
            auto date = Date(-1999, 7, 6);
            date.add!"years"(-7);
            assert(date == Date(-2006, 7, 6));
            date.add!"years"(9);
            assert(date == Date(-1997, 7, 6));
        }

        {
            auto date = Date(-1999, 2, 28);
            date.add!"years"(-1);
            assert(date == Date(-2000, 2, 28));
        }

        {
            auto date = Date(-2000, 2, 29);
            date.add!"years"(1);
            assert(date == Date(-1999, 3, 1));
        }

        // Test Both
        {
            auto date = Date(4, 7, 6);
            date.add!"years"(-5);
            assert(date == Date(-1, 7, 6));
            date.add!"years"(5);
            assert(date == Date(4, 7, 6));
        }

        {
            auto date = Date(-4, 7, 6);
            date.add!"years"(5);
            assert(date == Date(1, 7, 6));
            date.add!"years"(-5);
            assert(date == Date(-4, 7, 6));
        }

        {
            auto date = Date(4, 7, 6);
            date.add!"years"(-8);
            assert(date == Date(-4, 7, 6));
            date.add!"years"(8);
            assert(date == Date(4, 7, 6));
        }

        {
            auto date = Date(-4, 7, 6);
            date.add!"years"(8);
            assert(date == Date(4, 7, 6));
            date.add!"years"(-8);
            assert(date == Date(-4, 7, 6));
        }

        {
            auto date = Date(-4, 2, 29);
            date.add!"years"(5);
            assert(date == Date(1, 3, 1));
        }

        {
            auto date = Date(4, 2, 29);
            date.add!"years"(-5);
            assert(date == Date(-1, 3, 1));
        }

        {
            auto date = Date(4, 2, 29);
            date.add!"years"(-5).add!"years"(7);
            assert(date == Date(6, 3, 1));
        }

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        static assert(!__traits(compiles, cdate.add!"years"(7)));
        static assert(!__traits(compiles, idate.add!"years"(7)));
    }

    // Test add!"years"() with AllowDayOverflow.no
    @safe unittest
    {
        // Test A.D.
        {
            auto date = Date(1999, 7, 6);
            date.add!"years"(7, AllowDayOverflow.no);
            assert(date == Date(2006, 7, 6));
            date.add!"years"(-9, AllowDayOverflow.no);
            assert(date == Date(1997, 7, 6));
        }

        {
            auto date = Date(1999, 2, 28);
            date.add!"years"(1, AllowDayOverflow.no);
            assert(date == Date(2000, 2, 28));
        }

        {
            auto date = Date(2000, 2, 29);
            date.add!"years"(-1, AllowDayOverflow.no);
            assert(date == Date(1999, 2, 28));
        }

        // Test B.C.
        {
            auto date = Date(-1999, 7, 6);
            date.add!"years"(-7, AllowDayOverflow.no);
            assert(date == Date(-2006, 7, 6));
            date.add!"years"(9, AllowDayOverflow.no);
            assert(date == Date(-1997, 7, 6));
        }

        {
            auto date = Date(-1999, 2, 28);
            date.add!"years"(-1, AllowDayOverflow.no);
            assert(date == Date(-2000, 2, 28));
        }

        {
            auto date = Date(-2000, 2, 29);
            date.add!"years"(1, AllowDayOverflow.no);
            assert(date == Date(-1999, 2, 28));
        }

        // Test Both
        {
            auto date = Date(4, 7, 6);
            date.add!"years"(-5, AllowDayOverflow.no);
            assert(date == Date(-1, 7, 6));
            date.add!"years"(5, AllowDayOverflow.no);
            assert(date == Date(4, 7, 6));
        }

        {
            auto date = Date(-4, 7, 6);
            date.add!"years"(5, AllowDayOverflow.no);
            assert(date == Date(1, 7, 6));
            date.add!"years"(-5, AllowDayOverflow.no);
            assert(date == Date(-4, 7, 6));
        }

        {
            auto date = Date(4, 7, 6);
            date.add!"years"(-8, AllowDayOverflow.no);
            assert(date == Date(-4, 7, 6));
            date.add!"years"(8, AllowDayOverflow.no);
            assert(date == Date(4, 7, 6));
        }

        {
            auto date = Date(-4, 7, 6);
            date.add!"years"(8, AllowDayOverflow.no);
            assert(date == Date(4, 7, 6));
            date.add!"years"(-8, AllowDayOverflow.no);
            assert(date == Date(-4, 7, 6));
        }

        {
            auto date = Date(-4, 2, 29);
            date.add!"years"(5, AllowDayOverflow.no);
            assert(date == Date(1, 2, 28));
        }

        {
            auto date = Date(4, 2, 29);
            date.add!"years"(-5, AllowDayOverflow.no);
            assert(date == Date(-1, 2, 28));
        }

        {
            auto date = Date(4, 2, 29);
            date.add!"years"(-5, AllowDayOverflow.no).add!"years"(7, AllowDayOverflow.no);
            assert(date == Date(6, 2, 28));
        }
    }


    // Shares documentation with "years" version.
    @safe pure nothrow @nogc
    ref Date add(string units)(long months, AllowDayOverflow allowOverflow = AllowDayOverflow.yes)
        if (units == "months")
    {
        auto years = months / 12;
        months %= 12;
        auto newMonth = _month + months;

        if (months < 0)
        {
            if (newMonth < 1)
            {
                newMonth += 12;
                --years;
            }
        }
        else if (newMonth > 12)
        {
            newMonth -= 12;
            ++years;
        }

        _year += years;
        _month = cast(Month) newMonth;

        immutable currMaxDay = maxDay(_year, _month);
        immutable overflow = _day - currMaxDay;

        if (overflow > 0)
        {
            if (allowOverflow == AllowDayOverflow.yes)
            {
                ++_month;
                _day = cast(ubyte) overflow;
            }
            else
                _day = cast(ubyte) currMaxDay;
        }

        return this;
    }

    // Test add!"months"() with AllowDayOverflow.yes
    @safe unittest
    {
        // Test A.D.
        {
            auto date = Date(1999, 7, 6);
            date.add!"months"(3);
            assert(date == Date(1999, 10, 6));
            date.add!"months"(-4);
            assert(date == Date(1999, 6, 6));
        }

        {
            auto date = Date(1999, 7, 6);
            date.add!"months"(6);
            assert(date == Date(2000, 1, 6));
            date.add!"months"(-6);
            assert(date == Date(1999, 7, 6));
        }

        {
            auto date = Date(1999, 7, 6);
            date.add!"months"(27);
            assert(date == Date(2001, 10, 6));
            date.add!"months"(-28);
            assert(date == Date(1999, 6, 6));
        }

        {
            auto date = Date(1999, 5, 31);
            date.add!"months"(1);
            assert(date == Date(1999, 7, 1));
        }

        {
            auto date = Date(1999, 5, 31);
            date.add!"months"(-1);
            assert(date == Date(1999, 5, 1));
        }

        {
            auto date = Date(1999, 2, 28);
            date.add!"months"(12);
            assert(date == Date(2000, 2, 28));
        }

        {
            auto date = Date(2000, 2, 29);
            date.add!"months"(12);
            assert(date == Date(2001, 3, 1));
        }

        {
            auto date = Date(1999, 7, 31);
            date.add!"months"(1);
            assert(date == Date(1999, 8, 31));
            date.add!"months"(1);
            assert(date == Date(1999, 10, 1));
        }

        {
            auto date = Date(1998, 8, 31);
            date.add!"months"(13);
            assert(date == Date(1999, 10, 1));
            date.add!"months"(-13);
            assert(date == Date(1998, 9, 1));
        }

        {
            auto date = Date(1997, 12, 31);
            date.add!"months"(13);
            assert(date == Date(1999, 1, 31));
            date.add!"months"(-13);
            assert(date == Date(1997, 12, 31));
        }

        {
            auto date = Date(1997, 12, 31);
            date.add!"months"(14);
            assert(date == Date(1999, 3, 3));
            date.add!"months"(-14);
            assert(date == Date(1998, 1, 3));
        }

        {
            auto date = Date(1998, 12, 31);
            date.add!"months"(14);
            assert(date == Date(2000, 3, 2));
            date.add!"months"(-14);
            assert(date == Date(1999, 1, 2));
        }

        {
            auto date = Date(1999, 12, 31);
            date.add!"months"(14);
            assert(date == Date(2001, 3, 3));
            date.add!"months"(-14);
            assert(date == Date(2000, 1, 3));
        }

        // Test B.C.
        {
            auto date = Date(-1999, 7, 6);
            date.add!"months"(3);
            assert(date == Date(-1999, 10, 6));
            date.add!"months"(-4);
            assert(date == Date(-1999, 6, 6));
        }

        {
            auto date = Date(-1999, 7, 6);
            date.add!"months"(6);
            assert(date == Date(-1998, 1, 6));
            date.add!"months"(-6);
            assert(date == Date(-1999, 7, 6));
        }

        {
            auto date = Date(-1999, 7, 6);
            date.add!"months"(-27);
            assert(date == Date(-2001, 4, 6));
            date.add!"months"(28);
            assert(date == Date(-1999, 8, 6));
        }

        {
            auto date = Date(-1999, 5, 31);
            date.add!"months"(1);
            assert(date == Date(-1999, 7, 1));
        }

        {
            auto date = Date(-1999, 5, 31);
            date.add!"months"(-1);
            assert(date == Date(-1999, 5, 1));
        }

        {
            auto date = Date(-1999, 2, 28);
            date.add!"months"(-12);
            assert(date == Date(-2000, 2, 28));
        }

        {
            auto date = Date(-2000, 2, 29);
            date.add!"months"(-12);
            assert(date == Date(-2001, 3, 1));
        }

        {
            auto date = Date(-1999, 7, 31);
            date.add!"months"(1);
            assert(date == Date(-1999, 8, 31));
            date.add!"months"(1);
            assert(date == Date(-1999, 10, 1));
        }

        {
            auto date = Date(-1998, 8, 31);
            date.add!"months"(13);
            assert(date == Date(-1997, 10, 1));
            date.add!"months"(-13);
            assert(date == Date(-1998, 9, 1));
        }

        {
            auto date = Date(-1997, 12, 31);
            date.add!"months"(13);
            assert(date == Date(-1995, 1, 31));
            date.add!"months"(-13);
            assert(date == Date(-1997, 12, 31));
        }

        {
            auto date = Date(-1997, 12, 31);
            date.add!"months"(14);
            assert(date == Date(-1995, 3, 3));
            date.add!"months"(-14);
            assert(date == Date(-1996, 1, 3));
        }

        {
            auto date = Date(-2002, 12, 31);
            date.add!"months"(14);
            assert(date == Date(-2000, 3, 2));
            date.add!"months"(-14);
            assert(date == Date(-2001, 1, 2));
        }

        {
            auto date = Date(-2001, 12, 31);
            date.add!"months"(14);
            assert(date == Date(-1999, 3, 3));
            date.add!"months"(-14);
            assert(date == Date(-2000, 1, 3));
        }

        // Test Both
        {
            auto date = Date(1, 1, 1);
            date.add!"months"(-1);
            assert(date == Date(0, 12, 1));
            date.add!"months"(1);
            assert(date == Date(1, 1, 1));
        }

        {
            auto date = Date(4, 1, 1);
            date.add!"months"(-48);
            assert(date == Date(0, 1, 1));
            date.add!"months"(48);
            assert(date == Date(4, 1, 1));
        }

        {
            auto date = Date(4, 3, 31);
            date.add!"months"(-49);
            assert(date == Date(0, 3, 2));
            date.add!"months"(49);
            assert(date == Date(4, 4, 2));
        }

        {
            auto date = Date(4, 3, 31);
            date.add!"months"(-85);
            assert(date == Date(-3, 3, 3));
            date.add!"months"(85);
            assert(date == Date(4, 4, 3));
        }

        {
            auto date = Date(-3, 3, 31);
            date.add!"months"(85).add!"months"(-83);
            assert(date == Date(-3, 6, 1));
        }

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        static assert(!__traits(compiles, cdate.add!"months"(3)));
        static assert(!__traits(compiles, idate.add!"months"(3)));
    }

    // Test add!"months"() with AllowDayOverflow.no
    @safe unittest
    {
        // Test A.D.
        {
            auto date = Date(1999, 7, 6);
            date.add!"months"(3, AllowDayOverflow.no);
            assert(date == Date(1999, 10, 6));
            date.add!"months"(-4, AllowDayOverflow.no);
            assert(date == Date(1999, 6, 6));
        }

        {
            auto date = Date(1999, 7, 6);
            date.add!"months"(6, AllowDayOverflow.no);
            assert(date == Date(2000, 1, 6));
            date.add!"months"(-6, AllowDayOverflow.no);
            assert(date == Date(1999, 7, 6));
        }

        {
            auto date = Date(1999, 7, 6);
            date.add!"months"(27, AllowDayOverflow.no);
            assert(date == Date(2001, 10, 6));
            date.add!"months"(-28, AllowDayOverflow.no);
            assert(date == Date(1999, 6, 6));
        }

        {
            auto date = Date(1999, 5, 31);
            date.add!"months"(1, AllowDayOverflow.no);
            assert(date == Date(1999, 6, 30));
        }

        {
            auto date = Date(1999, 5, 31);
            date.add!"months"(-1, AllowDayOverflow.no);
            assert(date == Date(1999, 4, 30));
        }

        {
            auto date = Date(1999, 2, 28);
            date.add!"months"(12, AllowDayOverflow.no);
            assert(date == Date(2000, 2, 28));
        }

        {
            auto date = Date(2000, 2, 29);
            date.add!"months"(12, AllowDayOverflow.no);
            assert(date == Date(2001, 2, 28));
        }

        {
            auto date = Date(1999, 7, 31);
            date.add!"months"(1, AllowDayOverflow.no);
            assert(date == Date(1999, 8, 31));
            date.add!"months"(1, AllowDayOverflow.no);
            assert(date == Date(1999, 9, 30));
        }

        {
            auto date = Date(1998, 8, 31);
            date.add!"months"(13, AllowDayOverflow.no);
            assert(date == Date(1999, 9, 30));
            date.add!"months"(-13, AllowDayOverflow.no);
            assert(date == Date(1998, 8, 30));
        }

        {
            auto date = Date(1997, 12, 31);
            date.add!"months"(13, AllowDayOverflow.no);
            assert(date == Date(1999, 1, 31));
            date.add!"months"(-13, AllowDayOverflow.no);
            assert(date == Date(1997, 12, 31));
        }

        {
            auto date = Date(1997, 12, 31);
            date.add!"months"(14, AllowDayOverflow.no);
            assert(date == Date(1999, 2, 28));
            date.add!"months"(-14, AllowDayOverflow.no);
            assert(date == Date(1997, 12, 28));
        }

        {
            auto date = Date(1998, 12, 31);
            date.add!"months"(14, AllowDayOverflow.no);
            assert(date == Date(2000, 2, 29));
            date.add!"months"(-14, AllowDayOverflow.no);
            assert(date == Date(1998, 12, 29));
        }

        {
            auto date = Date(1999, 12, 31);
            date.add!"months"(14, AllowDayOverflow.no);
            assert(date == Date(2001, 2, 28));
            date.add!"months"(-14, AllowDayOverflow.no);
            assert(date == Date(1999, 12, 28));
        }

        // Test B.C.
        {
            auto date = Date(-1999, 7, 6);
            date.add!"months"(3, AllowDayOverflow.no);
            assert(date == Date(-1999, 10, 6));
            date.add!"months"(-4, AllowDayOverflow.no);
            assert(date == Date(-1999, 6, 6));
        }

        {
            auto date = Date(-1999, 7, 6);
            date.add!"months"(6, AllowDayOverflow.no);
            assert(date == Date(-1998, 1, 6));
            date.add!"months"(-6, AllowDayOverflow.no);
            assert(date == Date(-1999, 7, 6));
        }

        {
            auto date = Date(-1999, 7, 6);
            date.add!"months"(-27, AllowDayOverflow.no);
            assert(date == Date(-2001, 4, 6));
            date.add!"months"(28, AllowDayOverflow.no);
            assert(date == Date(-1999, 8, 6));
        }

        {
            auto date = Date(-1999, 5, 31);
            date.add!"months"(1, AllowDayOverflow.no);
            assert(date == Date(-1999, 6, 30));
        }

        {
            auto date = Date(-1999, 5, 31);
            date.add!"months"(-1, AllowDayOverflow.no);
            assert(date == Date(-1999, 4, 30));
        }

        {
            auto date = Date(-1999, 2, 28);
            date.add!"months"(-12, AllowDayOverflow.no);
            assert(date == Date(-2000, 2, 28));
        }

        {
            auto date = Date(-2000, 2, 29);
            date.add!"months"(-12, AllowDayOverflow.no);
            assert(date == Date(-2001, 2, 28));
        }

        {
            auto date = Date(-1999, 7, 31);
            date.add!"months"(1, AllowDayOverflow.no);
            assert(date == Date(-1999, 8, 31));
            date.add!"months"(1, AllowDayOverflow.no);
            assert(date == Date(-1999, 9, 30));
        }

        {
            auto date = Date(-1998, 8, 31);
            date.add!"months"(13, AllowDayOverflow.no);
            assert(date == Date(-1997, 9, 30));
            date.add!"months"(-13, AllowDayOverflow.no);
            assert(date == Date(-1998, 8, 30));
        }

        {
            auto date = Date(-1997, 12, 31);
            date.add!"months"(13, AllowDayOverflow.no);
            assert(date == Date(-1995, 1, 31));
            date.add!"months"(-13, AllowDayOverflow.no);
            assert(date == Date(-1997, 12, 31));
        }

        {
            auto date = Date(-1997, 12, 31);
            date.add!"months"(14, AllowDayOverflow.no);
            assert(date == Date(-1995, 2, 28));
            date.add!"months"(-14, AllowDayOverflow.no);
            assert(date == Date(-1997, 12, 28));
        }

        {
            auto date = Date(-2002, 12, 31);
            date.add!"months"(14, AllowDayOverflow.no);
            assert(date == Date(-2000, 2, 29));
            date.add!"months"(-14, AllowDayOverflow.no);
            assert(date == Date(-2002, 12, 29));
        }

        {
            auto date = Date(-2001, 12, 31);
            date.add!"months"(14, AllowDayOverflow.no);
            assert(date == Date(-1999, 2, 28));
            date.add!"months"(-14, AllowDayOverflow.no);
            assert(date == Date(-2001, 12, 28));
        }

        // Test Both
        {
            auto date = Date(1, 1, 1);
            date.add!"months"(-1, AllowDayOverflow.no);
            assert(date == Date(0, 12, 1));
            date.add!"months"(1, AllowDayOverflow.no);
            assert(date == Date(1, 1, 1));
        }

        {
            auto date = Date(4, 1, 1);
            date.add!"months"(-48, AllowDayOverflow.no);
            assert(date == Date(0, 1, 1));
            date.add!"months"(48, AllowDayOverflow.no);
            assert(date == Date(4, 1, 1));
        }

        {
            auto date = Date(4, 3, 31);
            date.add!"months"(-49, AllowDayOverflow.no);
            assert(date == Date(0, 2, 29));
            date.add!"months"(49, AllowDayOverflow.no);
            assert(date == Date(4, 3, 29));
        }

        {
            auto date = Date(4, 3, 31);
            date.add!"months"(-85, AllowDayOverflow.no);
            assert(date == Date(-3, 2, 28));
            date.add!"months"(85, AllowDayOverflow.no);
            assert(date == Date(4, 3, 28));
        }

        {
            auto date = Date(-3, 3, 31);
            date.add!"months"(85, AllowDayOverflow.no).add!"months"(-83, AllowDayOverflow.no);
            assert(date == Date(-3, 5, 30));
        }
    }


    /++
        Adds the given number of years or months to this $(LREF Date), mutating
        it. A negative number will subtract.

        The difference between rolling and adding is that rolling does not
        affect larger units. Rolling a $(LREF Date) 12 months gets
        the exact same $(LREF Date). However, the days can still be affected due
        to the differing number of days in each month.

        Because there are no units larger than years, there is no difference
        between adding and rolling years.

        Params:
            units         = The type of units to add ("years" or "months").
            value         = The number of months or years to add to this
                            $(LREF Date).
            allowOverflow = Whether the day should be allowed to overflow,
                            causing the month to increment.

        Returns:
            A reference to the `Date` (`this`).
      +/
    @safe pure nothrow @nogc
    ref Date roll(string units)(long value, AllowDayOverflow allowOverflow = AllowDayOverflow.yes)
        if (units == "years")
    {
        return add!"years"(value, allowOverflow);
    }

    ///
    @safe unittest
    {
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
        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        static assert(!__traits(compiles, cdate.roll!"years"(3)));
        static assert(!__traits(compiles, idate.rolYears(3)));
    }


    // Shares documentation with "years" version.
    @safe pure nothrow @nogc
    ref Date roll(string units)(long months, AllowDayOverflow allowOverflow = AllowDayOverflow.yes)
        if (units == "months")
    {
        months %= 12;
        auto newMonth = _month + months;

        if (months < 0)
        {
            if (newMonth < 1)
                newMonth += 12;
        }
        else
        {
            if (newMonth > 12)
                newMonth -= 12;
        }

        _month = cast(Month) newMonth;

        immutable currMaxDay = maxDay(_year, _month);
        immutable overflow = _day - currMaxDay;

        if (overflow > 0)
        {
            if (allowOverflow == AllowDayOverflow.yes)
            {
                ++_month;
                _day = cast(ubyte) overflow;
            }
            else
                _day = cast(ubyte) currMaxDay;
        }

        return this;
    }

    // Test roll!"months"() with AllowDayOverflow.yes
    @safe unittest
    {
        // Test A.D.
        {
            auto date = Date(1999, 7, 6);
            date.roll!"months"(3);
            assert(date == Date(1999, 10, 6));
            date.roll!"months"(-4);
            assert(date == Date(1999, 6, 6));
        }

        {
            auto date = Date(1999, 7, 6);
            date.roll!"months"(6);
            assert(date == Date(1999, 1, 6));
            date.roll!"months"(-6);
            assert(date == Date(1999, 7, 6));
        }

        {
            auto date = Date(1999, 7, 6);
            date.roll!"months"(27);
            assert(date == Date(1999, 10, 6));
            date.roll!"months"(-28);
            assert(date == Date(1999, 6, 6));
        }

        {
            auto date = Date(1999, 5, 31);
            date.roll!"months"(1);
            assert(date == Date(1999, 7, 1));
        }

        {
            auto date = Date(1999, 5, 31);
            date.roll!"months"(-1);
            assert(date == Date(1999, 5, 1));
        }

        {
            auto date = Date(1999, 2, 28);
            date.roll!"months"(12);
            assert(date == Date(1999, 2, 28));
        }

        {
            auto date = Date(2000, 2, 29);
            date.roll!"months"(12);
            assert(date == Date(2000, 2, 29));
        }

        {
            auto date = Date(1999, 7, 31);
            date.roll!"months"(1);
            assert(date == Date(1999, 8, 31));
            date.roll!"months"(1);
            assert(date == Date(1999, 10, 1));
        }

        {
            auto date = Date(1998, 8, 31);
            date.roll!"months"(13);
            assert(date == Date(1998, 10, 1));
            date.roll!"months"(-13);
            assert(date == Date(1998, 9, 1));
        }

        {
            auto date = Date(1997, 12, 31);
            date.roll!"months"(13);
            assert(date == Date(1997, 1, 31));
            date.roll!"months"(-13);
            assert(date == Date(1997, 12, 31));
        }

        {
            auto date = Date(1997, 12, 31);
            date.roll!"months"(14);
            assert(date == Date(1997, 3, 3));
            date.roll!"months"(-14);
            assert(date == Date(1997, 1, 3));
        }

        {
            auto date = Date(1998, 12, 31);
            date.roll!"months"(14);
            assert(date == Date(1998, 3, 3));
            date.roll!"months"(-14);
            assert(date == Date(1998, 1, 3));
        }

        {
            auto date = Date(1999, 12, 31);
            date.roll!"months"(14);
            assert(date == Date(1999, 3, 3));
            date.roll!"months"(-14);
            assert(date == Date(1999, 1, 3));
        }

        // Test B.C.
        {
            auto date = Date(-1999, 7, 6);
            date.roll!"months"(3);
            assert(date == Date(-1999, 10, 6));
            date.roll!"months"(-4);
            assert(date == Date(-1999, 6, 6));
        }

        {
            auto date = Date(-1999, 7, 6);
            date.roll!"months"(6);
            assert(date == Date(-1999, 1, 6));
            date.roll!"months"(-6);
            assert(date == Date(-1999, 7, 6));
        }

        {
            auto date = Date(-1999, 7, 6);
            date.roll!"months"(-27);
            assert(date == Date(-1999, 4, 6));
            date.roll!"months"(28);
            assert(date == Date(-1999, 8, 6));
        }

        {
            auto date = Date(-1999, 5, 31);
            date.roll!"months"(1);
            assert(date == Date(-1999, 7, 1));
        }

        {
            auto date = Date(-1999, 5, 31);
            date.roll!"months"(-1);
            assert(date == Date(-1999, 5, 1));
        }

        {
            auto date = Date(-1999, 2, 28);
            date.roll!"months"(-12);
            assert(date == Date(-1999, 2, 28));
        }

        {
            auto date = Date(-2000, 2, 29);
            date.roll!"months"(-12);
            assert(date == Date(-2000, 2, 29));
        }

        {
            auto date = Date(-1999, 7, 31);
            date.roll!"months"(1);
            assert(date == Date(-1999, 8, 31));
            date.roll!"months"(1);
            assert(date == Date(-1999, 10, 1));
        }

        {
            auto date = Date(-1998, 8, 31);
            date.roll!"months"(13);
            assert(date == Date(-1998, 10, 1));
            date.roll!"months"(-13);
            assert(date == Date(-1998, 9, 1));
        }

        {
            auto date = Date(-1997, 12, 31);
            date.roll!"months"(13);
            assert(date == Date(-1997, 1, 31));
            date.roll!"months"(-13);
            assert(date == Date(-1997, 12, 31));
        }

        {
            auto date = Date(-1997, 12, 31);
            date.roll!"months"(14);
            assert(date == Date(-1997, 3, 3));
            date.roll!"months"(-14);
            assert(date == Date(-1997, 1, 3));
        }

        {
            auto date = Date(-2002, 12, 31);
            date.roll!"months"(14);
            assert(date == Date(-2002, 3, 3));
            date.roll!"months"(-14);
            assert(date == Date(-2002, 1, 3));
        }

        {
            auto date = Date(-2001, 12, 31);
            date.roll!"months"(14);
            assert(date == Date(-2001, 3, 3));
            date.roll!"months"(-14);
            assert(date == Date(-2001, 1, 3));
        }

        // Test Both
        {
            auto date = Date(1, 1, 1);
            date.roll!"months"(-1);
            assert(date == Date(1, 12, 1));
            date.roll!"months"(1);
            assert(date == Date(1, 1, 1));
        }

        {
            auto date = Date(4, 1, 1);
            date.roll!"months"(-48);
            assert(date == Date(4, 1, 1));
            date.roll!"months"(48);
            assert(date == Date(4, 1, 1));
        }

        {
            auto date = Date(4, 3, 31);
            date.roll!"months"(-49);
            assert(date == Date(4, 3, 2));
            date.roll!"months"(49);
            assert(date == Date(4, 4, 2));
        }

        {
            auto date = Date(4, 3, 31);
            date.roll!"months"(-85);
            assert(date == Date(4, 3, 2));
            date.roll!"months"(85);
            assert(date == Date(4, 4, 2));
        }

        {
            auto date = Date(-1, 1, 1);
            date.roll!"months"(-1);
            assert(date == Date(-1, 12, 1));
            date.roll!"months"(1);
            assert(date == Date(-1, 1, 1));
        }

        {
            auto date = Date(-4, 1, 1);
            date.roll!"months"(-48);
            assert(date == Date(-4, 1, 1));
            date.roll!"months"(48);
            assert(date == Date(-4, 1, 1));
        }

        {
            auto date = Date(-4, 3, 31);
            date.roll!"months"(-49);
            assert(date == Date(-4, 3, 2));
            date.roll!"months"(49);
            assert(date == Date(-4, 4, 2));
        }

        {
            auto date = Date(-4, 3, 31);
            date.roll!"months"(-85);
            assert(date == Date(-4, 3, 2));
            date.roll!"months"(85);
            assert(date == Date(-4, 4, 2));
        }

        {
            auto date = Date(-3, 3, 31);
            date.roll!"months"(85).roll!"months"(-83);
            assert(date == Date(-3, 6, 1));
        }

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        static assert(!__traits(compiles, cdate.roll!"months"(3)));
        static assert(!__traits(compiles, idate.roll!"months"(3)));
    }

    // Test roll!"months"() with AllowDayOverflow.no
    @safe unittest
    {
        // Test A.D.
        {
            auto date = Date(1999, 7, 6);
            date.roll!"months"(3, AllowDayOverflow.no);
            assert(date == Date(1999, 10, 6));
            date.roll!"months"(-4, AllowDayOverflow.no);
            assert(date == Date(1999, 6, 6));
        }

        {
            auto date = Date(1999, 7, 6);
            date.roll!"months"(6, AllowDayOverflow.no);
            assert(date == Date(1999, 1, 6));
            date.roll!"months"(-6, AllowDayOverflow.no);
            assert(date == Date(1999, 7, 6));
        }

        {
            auto date = Date(1999, 7, 6);
            date.roll!"months"(27, AllowDayOverflow.no);
            assert(date == Date(1999, 10, 6));
            date.roll!"months"(-28, AllowDayOverflow.no);
            assert(date == Date(1999, 6, 6));
        }

        {
            auto date = Date(1999, 5, 31);
            date.roll!"months"(1, AllowDayOverflow.no);
            assert(date == Date(1999, 6, 30));
        }

        {
            auto date = Date(1999, 5, 31);
            date.roll!"months"(-1, AllowDayOverflow.no);
            assert(date == Date(1999, 4, 30));
        }

        {
            auto date = Date(1999, 2, 28);
            date.roll!"months"(12, AllowDayOverflow.no);
            assert(date == Date(1999, 2, 28));
        }

        {
            auto date = Date(2000, 2, 29);
            date.roll!"months"(12, AllowDayOverflow.no);
            assert(date == Date(2000, 2, 29));
        }

        {
            auto date = Date(1999, 7, 31);
            date.roll!"months"(1, AllowDayOverflow.no);
            assert(date == Date(1999, 8, 31));
            date.roll!"months"(1, AllowDayOverflow.no);
            assert(date == Date(1999, 9, 30));
        }

        {
            auto date = Date(1998, 8, 31);
            date.roll!"months"(13, AllowDayOverflow.no);
            assert(date == Date(1998, 9, 30));
            date.roll!"months"(-13, AllowDayOverflow.no);
            assert(date == Date(1998, 8, 30));
        }

        {
            auto date = Date(1997, 12, 31);
            date.roll!"months"(13, AllowDayOverflow.no);
            assert(date == Date(1997, 1, 31));
            date.roll!"months"(-13, AllowDayOverflow.no);
            assert(date == Date(1997, 12, 31));
        }

        {
            auto date = Date(1997, 12, 31);
            date.roll!"months"(14, AllowDayOverflow.no);
            assert(date == Date(1997, 2, 28));
            date.roll!"months"(-14, AllowDayOverflow.no);
            assert(date == Date(1997, 12, 28));
        }

        {
            auto date = Date(1998, 12, 31);
            date.roll!"months"(14, AllowDayOverflow.no);
            assert(date == Date(1998, 2, 28));
            date.roll!"months"(-14, AllowDayOverflow.no);
            assert(date == Date(1998, 12, 28));
        }

        {
            auto date = Date(1999, 12, 31);
            date.roll!"months"(14, AllowDayOverflow.no);
            assert(date == Date(1999, 2, 28));
            date.roll!"months"(-14, AllowDayOverflow.no);
            assert(date == Date(1999, 12, 28));
        }

        // Test B.C.
        {
            auto date = Date(-1999, 7, 6);
            date.roll!"months"(3, AllowDayOverflow.no);
            assert(date == Date(-1999, 10, 6));
            date.roll!"months"(-4, AllowDayOverflow.no);
            assert(date == Date(-1999, 6, 6));
        }

        {
            auto date = Date(-1999, 7, 6);
            date.roll!"months"(6, AllowDayOverflow.no);
            assert(date == Date(-1999, 1, 6));
            date.roll!"months"(-6, AllowDayOverflow.no);
            assert(date == Date(-1999, 7, 6));
        }

        {
            auto date = Date(-1999, 7, 6);
            date.roll!"months"(-27, AllowDayOverflow.no);
            assert(date == Date(-1999, 4, 6));
            date.roll!"months"(28, AllowDayOverflow.no);
            assert(date == Date(-1999, 8, 6));
        }

        {
            auto date = Date(-1999, 5, 31);
            date.roll!"months"(1, AllowDayOverflow.no);
            assert(date == Date(-1999, 6, 30));
        }

        {
            auto date = Date(-1999, 5, 31);
            date.roll!"months"(-1, AllowDayOverflow.no);
            assert(date == Date(-1999, 4, 30));
        }

        {
            auto date = Date(-1999, 2, 28);
            date.roll!"months"(-12, AllowDayOverflow.no);
            assert(date == Date(-1999, 2, 28));
        }

        {
            auto date = Date(-2000, 2, 29);
            date.roll!"months"(-12, AllowDayOverflow.no);
            assert(date == Date(-2000, 2, 29));
        }

        {
            auto date = Date(-1999, 7, 31);
            date.roll!"months"(1, AllowDayOverflow.no);
            assert(date == Date(-1999, 8, 31));
            date.roll!"months"(1, AllowDayOverflow.no);
            assert(date == Date(-1999, 9, 30));
        }

        {
            auto date = Date(-1998, 8, 31);
            date.roll!"months"(13, AllowDayOverflow.no);
            assert(date == Date(-1998, 9, 30));
            date.roll!"months"(-13, AllowDayOverflow.no);
            assert(date == Date(-1998, 8, 30));
        }

        {
            auto date = Date(-1997, 12, 31);
            date.roll!"months"(13, AllowDayOverflow.no);
            assert(date == Date(-1997, 1, 31));
            date.roll!"months"(-13, AllowDayOverflow.no);
            assert(date == Date(-1997, 12, 31));
        }

        {
            auto date = Date(-1997, 12, 31);
            date.roll!"months"(14, AllowDayOverflow.no);
            assert(date == Date(-1997, 2, 28));
            date.roll!"months"(-14, AllowDayOverflow.no);
            assert(date == Date(-1997, 12, 28));
        }

        {
            auto date = Date(-2002, 12, 31);
            date.roll!"months"(14, AllowDayOverflow.no);
            assert(date == Date(-2002, 2, 28));
            date.roll!"months"(-14, AllowDayOverflow.no);
            assert(date == Date(-2002, 12, 28));
        }

        {
            auto date = Date(-2001, 12, 31);
            date.roll!"months"(14, AllowDayOverflow.no);
            assert(date == Date(-2001, 2, 28));
            date.roll!"months"(-14, AllowDayOverflow.no);
            assert(date == Date(-2001, 12, 28));
        }

        // Test Both
        {
            auto date = Date(1, 1, 1);
            date.roll!"months"(-1, AllowDayOverflow.no);
            assert(date == Date(1, 12, 1));
            date.roll!"months"(1, AllowDayOverflow.no);
            assert(date == Date(1, 1, 1));
        }

        {
            auto date = Date(4, 1, 1);
            date.roll!"months"(-48, AllowDayOverflow.no);
            assert(date == Date(4, 1, 1));
            date.roll!"months"(48, AllowDayOverflow.no);
            assert(date == Date(4, 1, 1));
        }

        {
            auto date = Date(4, 3, 31);
            date.roll!"months"(-49, AllowDayOverflow.no);
            assert(date == Date(4, 2, 29));
            date.roll!"months"(49, AllowDayOverflow.no);
            assert(date == Date(4, 3, 29));
        }

        {
            auto date = Date(4, 3, 31);
            date.roll!"months"(-85, AllowDayOverflow.no);
            assert(date == Date(4, 2, 29));
            date.roll!"months"(85, AllowDayOverflow.no);
            assert(date == Date(4, 3, 29));
        }

        {
            auto date = Date(-1, 1, 1);
            date.roll!"months"(-1, AllowDayOverflow.no);
            assert(date == Date(-1, 12, 1));
            date.roll!"months"(1, AllowDayOverflow.no);
            assert(date == Date(-1, 1, 1));
        }

        {
            auto date = Date(-4, 1, 1);
            date.roll!"months"(-48, AllowDayOverflow.no);
            assert(date == Date(-4, 1, 1));
            date.roll!"months"(48, AllowDayOverflow.no);
            assert(date == Date(-4, 1, 1));
        }

        {
            auto date = Date(-4, 3, 31);
            date.roll!"months"(-49, AllowDayOverflow.no);
            assert(date == Date(-4, 2, 29));
            date.roll!"months"(49, AllowDayOverflow.no);
            assert(date == Date(-4, 3, 29));
        }

        {
            auto date = Date(-4, 3, 31);
            date.roll!"months"(-85, AllowDayOverflow.no);
            assert(date == Date(-4, 2, 29));
            date.roll!"months"(85, AllowDayOverflow.no);
            assert(date == Date(-4, 3, 29));
        }

        {
            auto date = Date(-3, 3, 31);
            date.roll!"months"(85, AllowDayOverflow.no).roll!"months"(-83, AllowDayOverflow.no);
            assert(date == Date(-3, 5, 30));
        }
    }


    /++
        Adds the given number of units to this $(LREF Date), mutating it. A
        negative number will subtract.

        The difference between rolling and adding is that rolling does not
        affect larger units. For instance, rolling a $(LREF Date) one
        year's worth of days gets the exact same $(LREF Date).

        The only accepted units are `"days"`.

        Params:
            units = The units to add. Must be `"days"`.
            days  = The number of days to add to this $(LREF Date).

        Returns:
            A reference to the `Date` (`this`).
      +/
    ref Date roll(string units)(long days) @safe pure nothrow @nogc
        if (units == "days")
    {
        immutable limit = maxDay(_year, _month);
        days %= limit;
        auto newDay = _day + days;

        if (days < 0)
        {
            if (newDay < 1)
                newDay += limit;
        }
        else if (newDay > limit)
            newDay -= limit;

        _day = cast(ubyte) newDay;
        return this;
    }

    ///
    @safe unittest
    {
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
        // Test A.D.
        {
            auto date = Date(1999, 2, 28);
            date.roll!"days"(1);
            assert(date == Date(1999, 2, 1));
            date.roll!"days"(-1);
            assert(date == Date(1999, 2, 28));
        }

        {
            auto date = Date(2000, 2, 28);
            date.roll!"days"(1);
            assert(date == Date(2000, 2, 29));
            date.roll!"days"(1);
            assert(date == Date(2000, 2, 1));
            date.roll!"days"(-1);
            assert(date == Date(2000, 2, 29));
        }

        {
            auto date = Date(1999, 6, 30);
            date.roll!"days"(1);
            assert(date == Date(1999, 6, 1));
            date.roll!"days"(-1);
            assert(date == Date(1999, 6, 30));
        }

        {
            auto date = Date(1999, 7, 31);
            date.roll!"days"(1);
            assert(date == Date(1999, 7, 1));
            date.roll!"days"(-1);
            assert(date == Date(1999, 7, 31));
        }

        {
            auto date = Date(1999, 1, 1);
            date.roll!"days"(-1);
            assert(date == Date(1999, 1, 31));
            date.roll!"days"(1);
            assert(date == Date(1999, 1, 1));
        }

        {
            auto date = Date(1999, 7, 6);
            date.roll!"days"(9);
            assert(date == Date(1999, 7, 15));
            date.roll!"days"(-11);
            assert(date == Date(1999, 7, 4));
            date.roll!"days"(30);
            assert(date == Date(1999, 7, 3));
            date.roll!"days"(-3);
            assert(date == Date(1999, 7, 31));
        }

        {
            auto date = Date(1999, 7, 6);
            date.roll!"days"(365);
            assert(date == Date(1999, 7, 30));
            date.roll!"days"(-365);
            assert(date == Date(1999, 7, 6));
            date.roll!"days"(366);
            assert(date == Date(1999, 7, 31));
            date.roll!"days"(730);
            assert(date == Date(1999, 7, 17));
            date.roll!"days"(-1096);
            assert(date == Date(1999, 7, 6));
        }

        {
            auto date = Date(1999, 2, 6);
            date.roll!"days"(365);
            assert(date == Date(1999, 2, 7));
            date.roll!"days"(-365);
            assert(date == Date(1999, 2, 6));
            date.roll!"days"(366);
            assert(date == Date(1999, 2, 8));
            date.roll!"days"(730);
            assert(date == Date(1999, 2, 10));
            date.roll!"days"(-1096);
            assert(date == Date(1999, 2, 6));
        }

        // Test B.C.
        {
            auto date = Date(-1999, 2, 28);
            date.roll!"days"(1);
            assert(date == Date(-1999, 2, 1));
            date.roll!"days"(-1);
            assert(date == Date(-1999, 2, 28));
        }

        {
            auto date = Date(-2000, 2, 28);
            date.roll!"days"(1);
            assert(date == Date(-2000, 2, 29));
            date.roll!"days"(1);
            assert(date == Date(-2000, 2, 1));
            date.roll!"days"(-1);
            assert(date == Date(-2000, 2, 29));
        }

        {
            auto date = Date(-1999, 6, 30);
            date.roll!"days"(1);
            assert(date == Date(-1999, 6, 1));
            date.roll!"days"(-1);
            assert(date == Date(-1999, 6, 30));
        }

        {
            auto date = Date(-1999, 7, 31);
            date.roll!"days"(1);
            assert(date == Date(-1999, 7, 1));
            date.roll!"days"(-1);
            assert(date == Date(-1999, 7, 31));
        }

        {
            auto date = Date(-1999, 1, 1);
            date.roll!"days"(-1);
            assert(date == Date(-1999, 1, 31));
            date.roll!"days"(1);
            assert(date == Date(-1999, 1, 1));
        }

        {
            auto date = Date(-1999, 7, 6);
            date.roll!"days"(9);
            assert(date == Date(-1999, 7, 15));
            date.roll!"days"(-11);
            assert(date == Date(-1999, 7, 4));
            date.roll!"days"(30);
            assert(date == Date(-1999, 7, 3));
            date.roll!"days"(-3);
            assert(date == Date(-1999, 7, 31));
        }

        {
            auto date = Date(-1999, 7, 6);
            date.roll!"days"(365);
            assert(date == Date(-1999, 7, 30));
            date.roll!"days"(-365);
            assert(date == Date(-1999, 7, 6));
            date.roll!"days"(366);
            assert(date == Date(-1999, 7, 31));
            date.roll!"days"(730);
            assert(date == Date(-1999, 7, 17));
            date.roll!"days"(-1096);
            assert(date == Date(-1999, 7, 6));
        }

        // Test Both
        {
            auto date = Date(1, 7, 6);
            date.roll!"days"(-365);
            assert(date == Date(1, 7, 13));
            date.roll!"days"(365);
            assert(date == Date(1, 7, 6));
            date.roll!"days"(-731);
            assert(date == Date(1, 7, 19));
            date.roll!"days"(730);
            assert(date == Date(1, 7, 5));
        }

        {
            auto date = Date(0, 7, 6);
            date.roll!"days"(-365);
            assert(date == Date(0, 7, 13));
            date.roll!"days"(365);
            assert(date == Date(0, 7, 6));
            date.roll!"days"(-731);
            assert(date == Date(0, 7, 19));
            date.roll!"days"(730);
            assert(date == Date(0, 7, 5));
        }

        {
            auto date = Date(0, 7, 6);
            date.roll!"days"(-365).roll!"days"(362).roll!"days"(-12).roll!"days"(730);
            assert(date == Date(0, 7, 8));
        }

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        static assert(!__traits(compiles, cdate.roll!"days"(12)));
        static assert(!__traits(compiles, idate.roll!"days"(12)));
    }

    import core.time : Duration;
    /++
        Gives the result of adding or subtracting a $(REF Duration, core,time)
        from

        The legal types of arithmetic for $(LREF Date) using this operator are

        $(BOOKTABLE,
        $(TR $(TD Date) $(TD +) $(TD Duration) $(TD -->) $(TD Date))
        $(TR $(TD Date) $(TD -) $(TD Duration) $(TD -->) $(TD Date))
        )

        Params:
            duration = The $(REF Duration, core,time) to add to or subtract from
                       this $(LREF Date).
      +/
    Date opBinary(string op)(Duration duration) const @safe pure nothrow @nogc
        if (op == "+" || op == "-")
    {
        Date retval = this;
        immutable days = duration.total!"days";
        mixin("return retval._addDays(" ~ op ~ "days);");
    }

    ///
    @safe unittest
    {
        import core.time : days;

        assert(Date(2015, 12, 31) + days(1) == Date(2016, 1, 1));
        assert(Date(2004, 2, 26) + days(4) == Date(2004, 3, 1));

        assert(Date(2016, 1, 1) - days(1) == Date(2015, 12, 31));
        assert(Date(2004, 3, 1) - days(4) == Date(2004, 2, 26));
    }

    @safe unittest
    {
        auto date = Date(1999, 7, 6);

        import core.time : dur;
        assert(date + dur!"weeks"(7) == Date(1999, 8, 24));
        assert(date + dur!"weeks"(-7) == Date(1999, 5, 18));
        assert(date + dur!"days"(7) == Date(1999, 7, 13));
        assert(date + dur!"days"(-7) == Date(1999, 6, 29));

        assert(date + dur!"hours"(24) == Date(1999, 7, 7));
        assert(date + dur!"hours"(-24) == Date(1999, 7, 5));
        assert(date + dur!"minutes"(1440) == Date(1999, 7, 7));
        assert(date + dur!"minutes"(-1440) == Date(1999, 7, 5));
        assert(date + dur!"seconds"(86_400) == Date(1999, 7, 7));
        assert(date + dur!"seconds"(-86_400) == Date(1999, 7, 5));
        assert(date + dur!"msecs"(86_400_000) == Date(1999, 7, 7));
        assert(date + dur!"msecs"(-86_400_000) == Date(1999, 7, 5));
        assert(date + dur!"usecs"(86_400_000_000) == Date(1999, 7, 7));
        assert(date + dur!"usecs"(-86_400_000_000) == Date(1999, 7, 5));
        assert(date + dur!"hnsecs"(864_000_000_000) == Date(1999, 7, 7));
        assert(date + dur!"hnsecs"(-864_000_000_000) == Date(1999, 7, 5));

        assert(date - dur!"weeks"(-7) == Date(1999, 8, 24));
        assert(date - dur!"weeks"(7) == Date(1999, 5, 18));
        assert(date - dur!"days"(-7) == Date(1999, 7, 13));
        assert(date - dur!"days"(7) == Date(1999, 6, 29));

        assert(date - dur!"hours"(-24) == Date(1999, 7, 7));
        assert(date - dur!"hours"(24) == Date(1999, 7, 5));
        assert(date - dur!"minutes"(-1440) == Date(1999, 7, 7));
        assert(date - dur!"minutes"(1440) == Date(1999, 7, 5));
        assert(date - dur!"seconds"(-86_400) == Date(1999, 7, 7));
        assert(date - dur!"seconds"(86_400) == Date(1999, 7, 5));
        assert(date - dur!"msecs"(-86_400_000) == Date(1999, 7, 7));
        assert(date - dur!"msecs"(86_400_000) == Date(1999, 7, 5));
        assert(date - dur!"usecs"(-86_400_000_000) == Date(1999, 7, 7));
        assert(date - dur!"usecs"(86_400_000_000) == Date(1999, 7, 5));
        assert(date - dur!"hnsecs"(-864_000_000_000) == Date(1999, 7, 7));
        assert(date - dur!"hnsecs"(864_000_000_000) == Date(1999, 7, 5));

        auto duration = dur!"days"(12);
        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        assert(date + duration == Date(1999, 7, 18));
        assert(cdate + duration == Date(1999, 7, 18));
        assert(idate + duration == Date(1999, 7, 18));

        assert(date - duration == Date(1999, 6, 24));
        assert(cdate - duration == Date(1999, 6, 24));
        assert(idate - duration == Date(1999, 6, 24));
    }


    /++
        Gives the result of adding or subtracting a $(REF Duration, core,time)
        from this $(LREF Date), as well as assigning the result to this
        $(LREF Date).

        The legal types of arithmetic for $(LREF Date) using this operator are

        $(BOOKTABLE,
        $(TR $(TD Date) $(TD +) $(TD Duration) $(TD -->) $(TD Date))
        $(TR $(TD Date) $(TD -) $(TD Duration) $(TD -->) $(TD Date))
        )

        Params:
            duration = The $(REF Duration, core,time) to add to or subtract from
                       this $(LREF Date).
      +/
    ref Date opOpAssign(string op)(Duration duration) @safe pure nothrow @nogc
        if (op == "+" || op == "-")
    {
        immutable days = duration.total!"days";
        mixin("return _addDays(" ~ op ~ "days);");
    }

    @safe unittest
    {
        import core.time : dur;
        assert(Date(1999, 7, 6) + dur!"weeks"(7) == Date(1999, 8, 24));
        assert(Date(1999, 7, 6) + dur!"weeks"(-7) == Date(1999, 5, 18));
        assert(Date(1999, 7, 6) + dur!"days"(7) == Date(1999, 7, 13));
        assert(Date(1999, 7, 6) + dur!"days"(-7) == Date(1999, 6, 29));

        assert(Date(1999, 7, 6) + dur!"hours"(24) == Date(1999, 7, 7));
        assert(Date(1999, 7, 6) + dur!"hours"(-24) == Date(1999, 7, 5));
        assert(Date(1999, 7, 6) + dur!"minutes"(1440) == Date(1999, 7, 7));
        assert(Date(1999, 7, 6) + dur!"minutes"(-1440) == Date(1999, 7, 5));
        assert(Date(1999, 7, 6) + dur!"seconds"(86_400) == Date(1999, 7, 7));
        assert(Date(1999, 7, 6) + dur!"seconds"(-86_400) == Date(1999, 7, 5));
        assert(Date(1999, 7, 6) + dur!"msecs"(86_400_000) == Date(1999, 7, 7));
        assert(Date(1999, 7, 6) + dur!"msecs"(-86_400_000) == Date(1999, 7, 5));
        assert(Date(1999, 7, 6) + dur!"usecs"(86_400_000_000) == Date(1999, 7, 7));
        assert(Date(1999, 7, 6) + dur!"usecs"(-86_400_000_000) == Date(1999, 7, 5));
        assert(Date(1999, 7, 6) + dur!"hnsecs"(864_000_000_000) == Date(1999, 7, 7));
        assert(Date(1999, 7, 6) + dur!"hnsecs"(-864_000_000_000) == Date(1999, 7, 5));

        assert(Date(1999, 7, 6) - dur!"weeks"(-7) == Date(1999, 8, 24));
        assert(Date(1999, 7, 6) - dur!"weeks"(7) == Date(1999, 5, 18));
        assert(Date(1999, 7, 6) - dur!"days"(-7) == Date(1999, 7, 13));
        assert(Date(1999, 7, 6) - dur!"days"(7) == Date(1999, 6, 29));

        assert(Date(1999, 7, 6) - dur!"hours"(-24) == Date(1999, 7, 7));
        assert(Date(1999, 7, 6) - dur!"hours"(24) == Date(1999, 7, 5));
        assert(Date(1999, 7, 6) - dur!"minutes"(-1440) == Date(1999, 7, 7));
        assert(Date(1999, 7, 6) - dur!"minutes"(1440) == Date(1999, 7, 5));
        assert(Date(1999, 7, 6) - dur!"seconds"(-86_400) == Date(1999, 7, 7));
        assert(Date(1999, 7, 6) - dur!"seconds"(86_400) == Date(1999, 7, 5));
        assert(Date(1999, 7, 6) - dur!"msecs"(-86_400_000) == Date(1999, 7, 7));
        assert(Date(1999, 7, 6) - dur!"msecs"(86_400_000) == Date(1999, 7, 5));
        assert(Date(1999, 7, 6) - dur!"usecs"(-86_400_000_000) == Date(1999, 7, 7));
        assert(Date(1999, 7, 6) - dur!"usecs"(86_400_000_000) == Date(1999, 7, 5));
        assert(Date(1999, 7, 6) - dur!"hnsecs"(-864_000_000_000) == Date(1999, 7, 7));
        assert(Date(1999, 7, 6) - dur!"hnsecs"(864_000_000_000) == Date(1999, 7, 5));

        {
            auto date = Date(0, 1, 31);
            (date += dur!"days"(507)) += dur!"days"(-2);
            assert(date == Date(1, 6, 19));
        }

        auto duration = dur!"days"(12);
        auto date = Date(1999, 7, 6);
        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        date += duration;
        static assert(!__traits(compiles, cdate += duration));
        static assert(!__traits(compiles, idate += duration));

        date -= duration;
        static assert(!__traits(compiles, cdate -= duration));
        static assert(!__traits(compiles, idate -= duration));
    }


    /++
        Gives the difference between two $(LREF Date)s.

        The legal types of arithmetic for $(LREF Date) using this operator are

        $(BOOKTABLE,
        $(TR $(TD Date) $(TD -) $(TD Date) $(TD -->) $(TD duration))
        )
      +/
    Duration opBinary(string op)(Date rhs) const @safe pure nothrow @nogc
        if (op == "-")
    {
        import core.time : dur;
        return dur!"days"(this.dayOfGregorianCal - rhs.dayOfGregorianCal);
    }

    @safe unittest
    {
        auto date = Date(1999, 7, 6);

        import core.time : dur;
        assert(Date(1999, 7, 6) - Date(1998, 7, 6) == dur!"days"(365));
        assert(Date(1998, 7, 6) - Date(1999, 7, 6) == dur!"days"(-365));
        assert(Date(1999, 6, 6) - Date(1999, 5, 6) == dur!"days"(31));
        assert(Date(1999, 5, 6) - Date(1999, 6, 6) == dur!"days"(-31));
        assert(Date(1999, 1, 1) - Date(1998, 12, 31) == dur!"days"(1));
        assert(Date(1998, 12, 31) - Date(1999, 1, 1) == dur!"days"(-1));

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        assert(date - date == Duration.zero);
        assert(cdate - date == Duration.zero);
        assert(idate - date == Duration.zero);

        assert(date - cdate == Duration.zero);
        assert(cdate - cdate == Duration.zero);
        assert(idate - cdate == Duration.zero);

        assert(date - idate == Duration.zero);
        assert(cdate - idate == Duration.zero);
        assert(idate - idate == Duration.zero);
    }


    /++
        Returns the difference between the two $(LREF Date)s in months.

        To get the difference in years, subtract the year property
        of two $(LREF Date)s. To get the difference in days or weeks,
        subtract the $(LREF Date)s themselves and use the
        $(REF Duration, core,time) that results. Because converting between
        months and smaller units requires a specific date (which
        $(REF Duration, core,time)s don't have), getting the difference in
        months requires some math using both the year and month properties, so
        this is a convenience function for getting the difference in months.

        Note that the number of days in the months or how far into the month
        either $(LREF Date) is is irrelevant. It is the difference in the month
        property combined with the difference in years * 12. So, for instance,
        December 31st and January 1st are one month apart just as December 1st
        and January 31st are one month apart.

        Params:
            rhs = The $(LREF Date) to subtract from this one.
      +/
    int diffMonths(Date rhs) const @safe pure nothrow @nogc
    {
        immutable yearDiff = _year - rhs._year;
        immutable monthDiff = _month - rhs._month;

        return yearDiff * 12 + monthDiff;
    }

    ///
    @safe unittest
    {
        assert(Date(1999, 2, 1).diffMonths(Date(1999, 1, 31)) == 1);
        assert(Date(1999, 1, 31).diffMonths(Date(1999, 2, 1)) == -1);
        assert(Date(1999, 3, 1).diffMonths(Date(1999, 1, 1)) == 2);
        assert(Date(1999, 1, 1).diffMonths(Date(1999, 3, 31)) == -2);
    }

    @safe unittest
    {
        auto date = Date(1999, 7, 6);

        // Test A.D.
        assert(date.diffMonths(Date(1998, 6, 5)) == 13);
        assert(date.diffMonths(Date(1998, 7, 5)) == 12);
        assert(date.diffMonths(Date(1998, 8, 5)) == 11);
        assert(date.diffMonths(Date(1998, 9, 5)) == 10);
        assert(date.diffMonths(Date(1998, 10, 5)) == 9);
        assert(date.diffMonths(Date(1998, 11, 5)) == 8);
        assert(date.diffMonths(Date(1998, 12, 5)) == 7);
        assert(date.diffMonths(Date(1999, 1, 5)) == 6);
        assert(date.diffMonths(Date(1999, 2, 6)) == 5);
        assert(date.diffMonths(Date(1999, 3, 6)) == 4);
        assert(date.diffMonths(Date(1999, 4, 6)) == 3);
        assert(date.diffMonths(Date(1999, 5, 6)) == 2);
        assert(date.diffMonths(Date(1999, 6, 6)) == 1);
        assert(date.diffMonths(date) == 0);
        assert(date.diffMonths(Date(1999, 8, 6)) == -1);
        assert(date.diffMonths(Date(1999, 9, 6)) == -2);
        assert(date.diffMonths(Date(1999, 10, 6)) == -3);
        assert(date.diffMonths(Date(1999, 11, 6)) == -4);
        assert(date.diffMonths(Date(1999, 12, 6)) == -5);
        assert(date.diffMonths(Date(2000, 1, 6)) == -6);
        assert(date.diffMonths(Date(2000, 2, 6)) == -7);
        assert(date.diffMonths(Date(2000, 3, 6)) == -8);
        assert(date.diffMonths(Date(2000, 4, 6)) == -9);
        assert(date.diffMonths(Date(2000, 5, 6)) == -10);
        assert(date.diffMonths(Date(2000, 6, 6)) == -11);
        assert(date.diffMonths(Date(2000, 7, 6)) == -12);
        assert(date.diffMonths(Date(2000, 8, 6)) == -13);

        assert(Date(1998, 6, 5).diffMonths(date) == -13);
        assert(Date(1998, 7, 5).diffMonths(date) == -12);
        assert(Date(1998, 8, 5).diffMonths(date) == -11);
        assert(Date(1998, 9, 5).diffMonths(date) == -10);
        assert(Date(1998, 10, 5).diffMonths(date) == -9);
        assert(Date(1998, 11, 5).diffMonths(date) == -8);
        assert(Date(1998, 12, 5).diffMonths(date) == -7);
        assert(Date(1999, 1, 5).diffMonths(date) == -6);
        assert(Date(1999, 2, 6).diffMonths(date) == -5);
        assert(Date(1999, 3, 6).diffMonths(date) == -4);
        assert(Date(1999, 4, 6).diffMonths(date) == -3);
        assert(Date(1999, 5, 6).diffMonths(date) == -2);
        assert(Date(1999, 6, 6).diffMonths(date) == -1);
        assert(Date(1999, 8, 6).diffMonths(date) == 1);
        assert(Date(1999, 9, 6).diffMonths(date) == 2);
        assert(Date(1999, 10, 6).diffMonths(date) == 3);
        assert(Date(1999, 11, 6).diffMonths(date) == 4);
        assert(Date(1999, 12, 6).diffMonths(date) == 5);
        assert(Date(2000, 1, 6).diffMonths(date) == 6);
        assert(Date(2000, 2, 6).diffMonths(date) == 7);
        assert(Date(2000, 3, 6).diffMonths(date) == 8);
        assert(Date(2000, 4, 6).diffMonths(date) == 9);
        assert(Date(2000, 5, 6).diffMonths(date) == 10);
        assert(Date(2000, 6, 6).diffMonths(date) == 11);
        assert(Date(2000, 7, 6).diffMonths(date) == 12);
        assert(Date(2000, 8, 6).diffMonths(date) == 13);

        assert(date.diffMonths(Date(1999, 6, 30)) == 1);
        assert(date.diffMonths(Date(1999, 7, 1)) == 0);
        assert(date.diffMonths(Date(1999, 7, 6)) == 0);
        assert(date.diffMonths(Date(1999, 7, 11)) == 0);
        assert(date.diffMonths(Date(1999, 7, 16)) == 0);
        assert(date.diffMonths(Date(1999, 7, 21)) == 0);
        assert(date.diffMonths(Date(1999, 7, 26)) == 0);
        assert(date.diffMonths(Date(1999, 7, 31)) == 0);
        assert(date.diffMonths(Date(1999, 8, 1)) == -1);

        assert(date.diffMonths(Date(1990, 6, 30)) == 109);
        assert(date.diffMonths(Date(1990, 7, 1)) == 108);
        assert(date.diffMonths(Date(1990, 7, 6)) == 108);
        assert(date.diffMonths(Date(1990, 7, 11)) == 108);
        assert(date.diffMonths(Date(1990, 7, 16)) == 108);
        assert(date.diffMonths(Date(1990, 7, 21)) == 108);
        assert(date.diffMonths(Date(1990, 7, 26)) == 108);
        assert(date.diffMonths(Date(1990, 7, 31)) == 108);
        assert(date.diffMonths(Date(1990, 8, 1)) == 107);

        assert(Date(1999, 6, 30).diffMonths(date) == -1);
        assert(Date(1999, 7, 1).diffMonths(date) == 0);
        assert(Date(1999, 7, 6).diffMonths(date) == 0);
        assert(Date(1999, 7, 11).diffMonths(date) == 0);
        assert(Date(1999, 7, 16).diffMonths(date) == 0);
        assert(Date(1999, 7, 21).diffMonths(date) == 0);
        assert(Date(1999, 7, 26).diffMonths(date) == 0);
        assert(Date(1999, 7, 31).diffMonths(date) == 0);
        assert(Date(1999, 8, 1).diffMonths(date) == 1);

        assert(Date(1990, 6, 30).diffMonths(date) == -109);
        assert(Date(1990, 7, 1).diffMonths(date) == -108);
        assert(Date(1990, 7, 6).diffMonths(date) == -108);
        assert(Date(1990, 7, 11).diffMonths(date) == -108);
        assert(Date(1990, 7, 16).diffMonths(date) == -108);
        assert(Date(1990, 7, 21).diffMonths(date) == -108);
        assert(Date(1990, 7, 26).diffMonths(date) == -108);
        assert(Date(1990, 7, 31).diffMonths(date) == -108);
        assert(Date(1990, 8, 1).diffMonths(date) == -107);

        // Test B.C.
        auto dateBC = Date(-1999, 7, 6);

        assert(dateBC.diffMonths(Date(-2000, 6, 5)) == 13);
        assert(dateBC.diffMonths(Date(-2000, 7, 5)) == 12);
        assert(dateBC.diffMonths(Date(-2000, 8, 5)) == 11);
        assert(dateBC.diffMonths(Date(-2000, 9, 5)) == 10);
        assert(dateBC.diffMonths(Date(-2000, 10, 5)) == 9);
        assert(dateBC.diffMonths(Date(-2000, 11, 5)) == 8);
        assert(dateBC.diffMonths(Date(-2000, 12, 5)) == 7);
        assert(dateBC.diffMonths(Date(-1999, 1, 5)) == 6);
        assert(dateBC.diffMonths(Date(-1999, 2, 6)) == 5);
        assert(dateBC.diffMonths(Date(-1999, 3, 6)) == 4);
        assert(dateBC.diffMonths(Date(-1999, 4, 6)) == 3);
        assert(dateBC.diffMonths(Date(-1999, 5, 6)) == 2);
        assert(dateBC.diffMonths(Date(-1999, 6, 6)) == 1);
        assert(dateBC.diffMonths(dateBC) == 0);
        assert(dateBC.diffMonths(Date(-1999, 8, 6)) == -1);
        assert(dateBC.diffMonths(Date(-1999, 9, 6)) == -2);
        assert(dateBC.diffMonths(Date(-1999, 10, 6)) == -3);
        assert(dateBC.diffMonths(Date(-1999, 11, 6)) == -4);
        assert(dateBC.diffMonths(Date(-1999, 12, 6)) == -5);
        assert(dateBC.diffMonths(Date(-1998, 1, 6)) == -6);
        assert(dateBC.diffMonths(Date(-1998, 2, 6)) == -7);
        assert(dateBC.diffMonths(Date(-1998, 3, 6)) == -8);
        assert(dateBC.diffMonths(Date(-1998, 4, 6)) == -9);
        assert(dateBC.diffMonths(Date(-1998, 5, 6)) == -10);
        assert(dateBC.diffMonths(Date(-1998, 6, 6)) == -11);
        assert(dateBC.diffMonths(Date(-1998, 7, 6)) == -12);
        assert(dateBC.diffMonths(Date(-1998, 8, 6)) == -13);

        assert(Date(-2000, 6, 5).diffMonths(dateBC) == -13);
        assert(Date(-2000, 7, 5).diffMonths(dateBC) == -12);
        assert(Date(-2000, 8, 5).diffMonths(dateBC) == -11);
        assert(Date(-2000, 9, 5).diffMonths(dateBC) == -10);
        assert(Date(-2000, 10, 5).diffMonths(dateBC) == -9);
        assert(Date(-2000, 11, 5).diffMonths(dateBC) == -8);
        assert(Date(-2000, 12, 5).diffMonths(dateBC) == -7);
        assert(Date(-1999, 1, 5).diffMonths(dateBC) == -6);
        assert(Date(-1999, 2, 6).diffMonths(dateBC) == -5);
        assert(Date(-1999, 3, 6).diffMonths(dateBC) == -4);
        assert(Date(-1999, 4, 6).diffMonths(dateBC) == -3);
        assert(Date(-1999, 5, 6).diffMonths(dateBC) == -2);
        assert(Date(-1999, 6, 6).diffMonths(dateBC) == -1);
        assert(Date(-1999, 8, 6).diffMonths(dateBC) == 1);
        assert(Date(-1999, 9, 6).diffMonths(dateBC) == 2);
        assert(Date(-1999, 10, 6).diffMonths(dateBC) == 3);
        assert(Date(-1999, 11, 6).diffMonths(dateBC) == 4);
        assert(Date(-1999, 12, 6).diffMonths(dateBC) == 5);
        assert(Date(-1998, 1, 6).diffMonths(dateBC) == 6);
        assert(Date(-1998, 2, 6).diffMonths(dateBC) == 7);
        assert(Date(-1998, 3, 6).diffMonths(dateBC) == 8);
        assert(Date(-1998, 4, 6).diffMonths(dateBC) == 9);
        assert(Date(-1998, 5, 6).diffMonths(dateBC) == 10);
        assert(Date(-1998, 6, 6).diffMonths(dateBC) == 11);
        assert(Date(-1998, 7, 6).diffMonths(dateBC) == 12);
        assert(Date(-1998, 8, 6).diffMonths(dateBC) == 13);

        assert(dateBC.diffMonths(Date(-1999, 6, 30)) == 1);
        assert(dateBC.diffMonths(Date(-1999, 7, 1)) == 0);
        assert(dateBC.diffMonths(Date(-1999, 7, 6)) == 0);
        assert(dateBC.diffMonths(Date(-1999, 7, 11)) == 0);
        assert(dateBC.diffMonths(Date(-1999, 7, 16)) == 0);
        assert(dateBC.diffMonths(Date(-1999, 7, 21)) == 0);
        assert(dateBC.diffMonths(Date(-1999, 7, 26)) == 0);
        assert(dateBC.diffMonths(Date(-1999, 7, 31)) == 0);
        assert(dateBC.diffMonths(Date(-1999, 8, 1)) == -1);

        assert(dateBC.diffMonths(Date(-2008, 6, 30)) == 109);
        assert(dateBC.diffMonths(Date(-2008, 7, 1)) == 108);
        assert(dateBC.diffMonths(Date(-2008, 7, 6)) == 108);
        assert(dateBC.diffMonths(Date(-2008, 7, 11)) == 108);
        assert(dateBC.diffMonths(Date(-2008, 7, 16)) == 108);
        assert(dateBC.diffMonths(Date(-2008, 7, 21)) == 108);
        assert(dateBC.diffMonths(Date(-2008, 7, 26)) == 108);
        assert(dateBC.diffMonths(Date(-2008, 7, 31)) == 108);
        assert(dateBC.diffMonths(Date(-2008, 8, 1)) == 107);

        assert(Date(-1999, 6, 30).diffMonths(dateBC) == -1);
        assert(Date(-1999, 7, 1).diffMonths(dateBC) == 0);
        assert(Date(-1999, 7, 6).diffMonths(dateBC) == 0);
        assert(Date(-1999, 7, 11).diffMonths(dateBC) == 0);
        assert(Date(-1999, 7, 16).diffMonths(dateBC) == 0);
        assert(Date(-1999, 7, 21).diffMonths(dateBC) == 0);
        assert(Date(-1999, 7, 26).diffMonths(dateBC) == 0);
        assert(Date(-1999, 7, 31).diffMonths(dateBC) == 0);
        assert(Date(-1999, 8, 1).diffMonths(dateBC) == 1);

        assert(Date(-2008, 6, 30).diffMonths(dateBC) == -109);
        assert(Date(-2008, 7, 1).diffMonths(dateBC) == -108);
        assert(Date(-2008, 7, 6).diffMonths(dateBC) == -108);
        assert(Date(-2008, 7, 11).diffMonths(dateBC) == -108);
        assert(Date(-2008, 7, 16).diffMonths(dateBC) == -108);
        assert(Date(-2008, 7, 21).diffMonths(dateBC) == -108);
        assert(Date(-2008, 7, 26).diffMonths(dateBC) == -108);
        assert(Date(-2008, 7, 31).diffMonths(dateBC) == -108);
        assert(Date(-2008, 8, 1).diffMonths(dateBC) == -107);

        // Test Both
        assert(Date(3, 3, 3).diffMonths(Date(-5, 5, 5)) == 94);
        assert(Date(-5, 5, 5).diffMonths(Date(3, 3, 3)) == -94);

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        assert(date.diffMonths(date) == 0);
        assert(cdate.diffMonths(date) == 0);
        assert(idate.diffMonths(date) == 0);

        assert(date.diffMonths(cdate) == 0);
        assert(cdate.diffMonths(cdate) == 0);
        assert(idate.diffMonths(cdate) == 0);

        assert(date.diffMonths(idate) == 0);
        assert(cdate.diffMonths(idate) == 0);
        assert(idate.diffMonths(idate) == 0);
    }


    /++
        Whether this $(LREF Date) is in a leap year.
     +/
    @property bool isLeapYear() const @safe pure nothrow @nogc
    {
        return yearIsLeapYear(_year);
    }

    @safe unittest
    {
        auto date = Date(1999, 7, 6);
        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        static assert(!__traits(compiles, date.isLeapYear = true));
        static assert(!__traits(compiles, cdate.isLeapYear = true));
        static assert(!__traits(compiles, idate.isLeapYear = true));
    }


    /++
        Day of the week this $(LREF Date) is on.
      +/
    @property DayOfWeek dayOfWeek() const @safe pure nothrow @nogc
    {
        return getDayOfWeek(dayOfGregorianCal);
    }

    @safe unittest
    {
        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        assert(cdate.dayOfWeek == DayOfWeek.tue);
        static assert(!__traits(compiles, cdate.dayOfWeek = DayOfWeek.sun));
        assert(idate.dayOfWeek == DayOfWeek.tue);
        static assert(!__traits(compiles, idate.dayOfWeek = DayOfWeek.sun));
    }


    /++
        Day of the year this $(LREF Date) is on.
      +/
    @property ushort dayOfYear() const @safe pure nothrow @nogc
    {
        if (_month >= Month.jan && _month <= Month.dec)
        {
            immutable int[] lastDay = isLeapYear ? lastDayLeap : lastDayNonLeap;
            auto monthIndex = _month - Month.jan;

            return cast(ushort)(lastDay[monthIndex] + _day);
        }
        assert(0, "Invalid month.");
    }

    ///
    @safe unittest
    {
        assert(Date(1999, 1, 1).dayOfYear == 1);
        assert(Date(1999, 12, 31).dayOfYear == 365);
        assert(Date(2000, 12, 31).dayOfYear == 366);
    }

    @safe unittest
    {
        import std.algorithm.iteration : filter;
        import std.range : chain;

        foreach (year; filter!((a){return !yearIsLeapYear(a);})(chain(testYearsBC, testYearsAD)))
        {
            foreach (doy; testDaysOfYear)
                assert(Date(year, doy.md.month, doy.md.day).dayOfYear == doy.day);
        }

        foreach (year; filter!((a){return yearIsLeapYear(a);})(chain(testYearsBC, testYearsAD)))
        {
            foreach (doy; testDaysOfLeapYear)
                assert(Date(year, doy.md.month, doy.md.day).dayOfYear == doy.day);
        }

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        assert(cdate.dayOfYear == 187);
        assert(idate.dayOfYear == 187);
    }

    /++
        Day of the year.

        Params:
            day = The day of the year to set which day of the year this
                  $(LREF Date) is on.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given day is an
            invalid day of the year.
      +/
    @property void dayOfYear(int day) @safe pure
    {
        setDayOfYear!true(day);
    }

    private void setDayOfYear(bool useExceptions = false)(int day)
    {
        immutable int[] lastDay = isLeapYear ? lastDayLeap : lastDayNonLeap;

        bool dayOutOfRange = day <= 0 || day > (isLeapYear ? daysInLeapYear : daysInYear);
        enum errorMsg = "Invalid day of the year.";

        static if (useExceptions)
        {
            if (dayOutOfRange) throw new DateTimeException(errorMsg);
        }
        else
        {
            assert(!dayOutOfRange, errorMsg);
        }

        foreach (i; 1 .. lastDay.length)
        {
            if (day <= lastDay[i])
            {
                _month = cast(Month)(cast(int) Month.jan + i - 1);
                _day = cast(ubyte)(day - lastDay[i - 1]);
                return;
            }
        }
        assert(0, "Invalid day of the year.");
    }

    @safe unittest
    {
        static void test(Date date, int day, MonthDay expected, size_t line = __LINE__)
        {
            date.dayOfYear = day;
            assert(date.month == expected.month);
            assert(date.day == expected.day);
        }

        foreach (doy; testDaysOfYear)
        {
            test(Date(1999, 1, 1), doy.day, doy.md);
            test(Date(-1, 1, 1), doy.day, doy.md);
        }

        foreach (doy; testDaysOfLeapYear)
        {
            test(Date(2000, 1, 1), doy.day, doy.md);
            test(Date(-4, 1, 1), doy.day, doy.md);
        }

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        static assert(!__traits(compiles, cdate.dayOfYear = 187));
        static assert(!__traits(compiles, idate.dayOfYear = 187));
    }


    /++
        The Xth day of the Gregorian Calendar that this $(LREF Date) is on.
     +/
    @property int dayOfGregorianCal() const @safe pure nothrow @nogc
    {
        if (isAD)
        {
            if (_year == 1)
                return dayOfYear;

            int years = _year - 1;
            auto days = (years / 400) * daysIn400Years;
            years %= 400;

            days += (years / 100) * daysIn100Years;
            years %= 100;

            days += (years / 4) * daysIn4Years;
            years %= 4;

            days += years * daysInYear;

            days += dayOfYear;

            return days;
        }
        else if (_year == 0)
            return dayOfYear - daysInLeapYear;
        else
        {
            int years = _year;
            auto days = (years / 400) * daysIn400Years;
            years %= 400;

            days += (years / 100) * daysIn100Years;
            years %= 100;

            days += (years / 4) * daysIn4Years;
            years %= 4;

            if (years < 0)
            {
                days -= daysInLeapYear;
                ++years;

                days += years * daysInYear;

                days -= daysInYear - dayOfYear;
            }
            else
                days -= daysInLeapYear - dayOfYear;

            return days;
        }
    }

    ///
    @safe unittest
    {
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
        import std.range : chain;

        foreach (gd; chain(testGregDaysBC, testGregDaysAD))
            assert(gd.date.dayOfGregorianCal == gd.day);

        auto date = Date(1999, 7, 6);
        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        assert(date.dayOfGregorianCal == 729_941);
        assert(cdate.dayOfGregorianCal == 729_941);
        assert(idate.dayOfGregorianCal == 729_941);
    }

    /++
        The Xth day of the Gregorian Calendar that this $(LREF Date) is on.

        Params:
            day = The day of the Gregorian Calendar to set this $(LREF Date) to.
     +/
    @property void dayOfGregorianCal(int day) @safe pure nothrow @nogc
    {
        this = Date(day);
    }

    ///
    @safe unittest
    {
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
        auto date = Date(1999, 7, 6);
        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        date.dayOfGregorianCal = 187;
        assert(date.dayOfGregorianCal == 187);
        static assert(!__traits(compiles, cdate.dayOfGregorianCal = 187));
        static assert(!__traits(compiles, idate.dayOfGregorianCal = 187));
    }


    /++
        The ISO 8601 week and year of the year that this $(LREF Date) is in.

        Returns:
            An anonymous struct with the members $(D isoWeekYear) for the
            resulting year and $(D isoWeek) for the resulting ISO week.

        See_Also:
            $(HTTP en.wikipedia.org/wiki/ISO_week_date, ISO Week Date)
      +/
    @property auto isoWeekAndYear() const @safe pure nothrow
    {
        struct ISOWeekAndYear { short isoWeekYear; ubyte isoWeek; }

        immutable weekday = dayOfWeek;
        immutable adjustedWeekday = weekday == DayOfWeek.sun ? 7 : weekday;
        immutable week = (dayOfYear - adjustedWeekday + 10) / 7;

        try
        {
            if (week == 53)
            {
                switch (Date(_year + 1, 1, 1).dayOfWeek)
                {
                    case DayOfWeek.mon:
                    case DayOfWeek.tue:
                    case DayOfWeek.wed:
                    case DayOfWeek.thu:
                        return ISOWeekAndYear(cast(short) (_year + 1), 1);
                    case DayOfWeek.fri:
                    case DayOfWeek.sat:
                    case DayOfWeek.sun:
                        return ISOWeekAndYear(_year, 53);
                    default:
                        assert(0, "Invalid ISO Week");
                }
            }
            else if (week > 0)
                return ISOWeekAndYear(_year, cast(ubyte) week);
            else
                return Date(_year - 1, 12, 31).isoWeekAndYear;
        }
        catch (Exception e)
            assert(0, "Date's constructor threw.");
    }

    /++
        The ISO 8601 week of the year that this $(LREF Date) is in.

        See_Also:
            $(HTTP en.wikipedia.org/wiki/ISO_week_date, ISO Week Date)
      +/
    @property ubyte isoWeek() const @safe pure nothrow
    {
        return isoWeekAndYear().isoWeek;
    }

    @safe unittest
    {
        // Test A.D.
        assert(Date(2009, 12, 28).isoWeek == 53);
        assert(Date(2009, 12, 29).isoWeek == 53);
        assert(Date(2009, 12, 30).isoWeek == 53);
        assert(Date(2009, 12, 31).isoWeek == 53);
        assert(Date(2010, 1, 1).isoWeek == 53);
        assert(Date(2010, 1, 2).isoWeek == 53);
        assert(Date(2010, 1, 3).isoWeek == 53);
        assert(Date(2010, 1, 4).isoWeek == 1);
        assert(Date(2010, 1, 5).isoWeek == 1);
        assert(Date(2010, 1, 6).isoWeek == 1);
        assert(Date(2010, 1, 7).isoWeek == 1);
        assert(Date(2010, 1, 8).isoWeek == 1);
        assert(Date(2010, 1, 9).isoWeek == 1);
        assert(Date(2010, 1, 10).isoWeek == 1);
        assert(Date(2010, 1, 11).isoWeek == 2);
        assert(Date(2010, 12, 31).isoWeek == 52);

        assert(Date(2004, 12, 26).isoWeek == 52);
        assert(Date(2004, 12, 27).isoWeek == 53);
        assert(Date(2004, 12, 28).isoWeek == 53);
        assert(Date(2004, 12, 29).isoWeek == 53);
        assert(Date(2004, 12, 30).isoWeek == 53);
        assert(Date(2004, 12, 31).isoWeek == 53);
        assert(Date(2005, 1, 1).isoWeek == 53);
        assert(Date(2005, 1, 2).isoWeek == 53);

        assert(Date(2005, 12, 31).isoWeek == 52);
        assert(Date(2007, 1, 1).isoWeek == 1);

        assert(Date(2007, 12, 30).isoWeek == 52);
        assert(Date(2007, 12, 31).isoWeek == 1);
        assert(Date(2008, 1, 1).isoWeek == 1);

        assert(Date(2008, 12, 28).isoWeek == 52);
        assert(Date(2008, 12, 29).isoWeek == 1);
        assert(Date(2008, 12, 30).isoWeek == 1);
        assert(Date(2008, 12, 31).isoWeek == 1);
        assert(Date(2009, 1, 1).isoWeek == 1);
        assert(Date(2009, 1, 2).isoWeek == 1);
        assert(Date(2009, 1, 3).isoWeek == 1);
        assert(Date(2009, 1, 4).isoWeek == 1);

        // Test B.C.
        // The algorithm should work identically for both A.D. and B.C. since
        // it doesn't really take the year into account, so B.C. testing
        // probably isn't really needed.
        assert(Date(0, 12, 31).isoWeek == 52);
        assert(Date(0, 1, 4).isoWeek == 1);
        assert(Date(0, 1, 1).isoWeek == 52);

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        assert(cdate.isoWeek == 27);
        static assert(!__traits(compiles, cdate.isoWeek = 3));
        assert(idate.isoWeek == 27);
        static assert(!__traits(compiles, idate.isoWeek = 3));
    }

    /++
        The year inside the ISO 8601 week calendar that this $(LREF Date) is in.

        May differ from $(LREF year) between 28 December and 4 January.

        See_Also:
            $(HTTP en.wikipedia.org/wiki/ISO_week_date, ISO Week Date)
      +/
    @property short isoWeekYear() const @safe pure nothrow
    {
        return isoWeekAndYear().isoWeekYear;
    }

    @safe unittest
    {
        // Test A.D.
        assert(Date(2009, 12, 28).isoWeekYear == 2009);
        assert(Date(2009, 12, 29).isoWeekYear == 2009);
        assert(Date(2009, 12, 30).isoWeekYear == 2009);
        assert(Date(2009, 12, 31).isoWeekYear == 2009);
        assert(Date(2010, 1, 1).isoWeekYear == 2009);
        assert(Date(2010, 1, 2).isoWeekYear == 2009);
        assert(Date(2010, 1, 3).isoWeekYear == 2009);
        assert(Date(2010, 1, 4).isoWeekYear == 2010);
        assert(Date(2010, 1, 5).isoWeekYear == 2010);
        assert(Date(2010, 1, 6).isoWeekYear == 2010);
        assert(Date(2010, 1, 7).isoWeekYear == 2010);
        assert(Date(2010, 1, 8).isoWeekYear == 2010);
        assert(Date(2010, 1, 9).isoWeekYear == 2010);
        assert(Date(2010, 1, 10).isoWeekYear == 2010);
        assert(Date(2010, 1, 11).isoWeekYear == 2010);
        assert(Date(2010, 12, 31).isoWeekYear == 2010);

        assert(Date(2004, 12, 26).isoWeekYear == 2004);
        assert(Date(2004, 12, 27).isoWeekYear == 2004);
        assert(Date(2004, 12, 28).isoWeekYear == 2004);
        assert(Date(2004, 12, 29).isoWeekYear == 2004);
        assert(Date(2004, 12, 30).isoWeekYear == 2004);
        assert(Date(2004, 12, 31).isoWeekYear == 2004);
        assert(Date(2005, 1, 1).isoWeekYear == 2004);
        assert(Date(2005, 1, 2).isoWeekYear == 2004);
        assert(Date(2005, 1, 3).isoWeekYear == 2005);

        assert(Date(2005, 12, 31).isoWeekYear == 2005);
        assert(Date(2007, 1, 1).isoWeekYear == 2007);

        assert(Date(2007, 12, 30).isoWeekYear == 2007);
        assert(Date(2007, 12, 31).isoWeekYear == 2008);
        assert(Date(2008, 1, 1).isoWeekYear == 2008);

        assert(Date(2008, 12, 28).isoWeekYear == 2008);
        assert(Date(2008, 12, 29).isoWeekYear == 2009);
        assert(Date(2008, 12, 30).isoWeekYear == 2009);
        assert(Date(2008, 12, 31).isoWeekYear == 2009);
        assert(Date(2009, 1, 1).isoWeekYear == 2009);
        assert(Date(2009, 1, 2).isoWeekYear == 2009);
        assert(Date(2009, 1, 3).isoWeekYear == 2009);
        assert(Date(2009, 1, 4).isoWeekYear == 2009);

        // Test B.C.
        assert(Date(0, 12, 31).isoWeekYear == 0);
        assert(Date(0, 1, 4).isoWeekYear == 0);
        assert(Date(0, 1, 1).isoWeekYear == -1);

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        assert(cdate.isoWeekYear == 1999);
        assert(idate.isoWeekYear == 1999);
    }

    static Date fromISOWeek(short isoWeekYear, ubyte isoWeek, DayOfWeek weekday) @safe pure nothrow @nogc
    {
        immutable adjustedWeekday = weekday == DayOfWeek.sun ? 7 : weekday;
        immutable dayOffset = (isoWeek - 1) * 7 + adjustedWeekday;

        Date date;
        date._year = isoWeekYear;
        date._month = Month.jan;
        date._day = 3;
        immutable startOfYear = date.dayOfWeek;
        return date._addDays(dayOffset - startOfYear);
    }

    @safe unittest
    {
        // Test -30000 days to 30000 days for matching construction <-> deconstruction
        Date date = Date(1, 1, 1);
        date._addDays(-30_000);
        foreach (day; 0 .. 60_000)
        {
            const year = date.isoWeekYear;
            const dow = date.dayOfWeek;
            const isoWeek = date.isoWeek;
            const reversed = Date.fromISOWeek(year, isoWeek, dow);
            assert(reversed == date, date.toISOExtString ~ " != " ~ reversed.toISOExtString);
            date = date._addDays(1);
        }
    }


    /++
        $(LREF Date) for the last day in the month that this $(LREF Date) is in.
      +/
    @property Date endOfMonth() const @safe pure nothrow
    {
        try
            return Date(_year, _month, maxDay(_year, _month));
        catch (Exception e)
            assert(0, "Date's constructor threw.");
    }

    ///
    @safe unittest
    {
        assert(Date(1999, 1, 6).endOfMonth == Date(1999, 1, 31));
        assert(Date(1999, 2, 7).endOfMonth == Date(1999, 2, 28));
        assert(Date(2000, 2, 7).endOfMonth == Date(2000, 2, 29));
        assert(Date(2000, 6, 4).endOfMonth == Date(2000, 6, 30));
    }

    @safe unittest
    {
        // Test A.D.
        assert(Date(1999, 1, 1).endOfMonth == Date(1999, 1, 31));
        assert(Date(1999, 2, 1).endOfMonth == Date(1999, 2, 28));
        assert(Date(2000, 2, 1).endOfMonth == Date(2000, 2, 29));
        assert(Date(1999, 3, 1).endOfMonth == Date(1999, 3, 31));
        assert(Date(1999, 4, 1).endOfMonth == Date(1999, 4, 30));
        assert(Date(1999, 5, 1).endOfMonth == Date(1999, 5, 31));
        assert(Date(1999, 6, 1).endOfMonth == Date(1999, 6, 30));
        assert(Date(1999, 7, 1).endOfMonth == Date(1999, 7, 31));
        assert(Date(1999, 8, 1).endOfMonth == Date(1999, 8, 31));
        assert(Date(1999, 9, 1).endOfMonth == Date(1999, 9, 30));
        assert(Date(1999, 10, 1).endOfMonth == Date(1999, 10, 31));
        assert(Date(1999, 11, 1).endOfMonth == Date(1999, 11, 30));
        assert(Date(1999, 12, 1).endOfMonth == Date(1999, 12, 31));

        // Test B.C.
        assert(Date(-1999, 1, 1).endOfMonth == Date(-1999, 1, 31));
        assert(Date(-1999, 2, 1).endOfMonth == Date(-1999, 2, 28));
        assert(Date(-2000, 2, 1).endOfMonth == Date(-2000, 2, 29));
        assert(Date(-1999, 3, 1).endOfMonth == Date(-1999, 3, 31));
        assert(Date(-1999, 4, 1).endOfMonth == Date(-1999, 4, 30));
        assert(Date(-1999, 5, 1).endOfMonth == Date(-1999, 5, 31));
        assert(Date(-1999, 6, 1).endOfMonth == Date(-1999, 6, 30));
        assert(Date(-1999, 7, 1).endOfMonth == Date(-1999, 7, 31));
        assert(Date(-1999, 8, 1).endOfMonth == Date(-1999, 8, 31));
        assert(Date(-1999, 9, 1).endOfMonth == Date(-1999, 9, 30));
        assert(Date(-1999, 10, 1).endOfMonth == Date(-1999, 10, 31));
        assert(Date(-1999, 11, 1).endOfMonth == Date(-1999, 11, 30));
        assert(Date(-1999, 12, 1).endOfMonth == Date(-1999, 12, 31));

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        static assert(!__traits(compiles, cdate.endOfMonth = Date(1999, 7, 30)));
        static assert(!__traits(compiles, idate.endOfMonth = Date(1999, 7, 30)));
    }


    /++
        The last day in the month that this $(LREF Date) is in.
      +/
    @property ubyte daysInMonth() const @safe pure nothrow @nogc
    {
        return maxDay(_year, _month);
    }

    ///
    @safe unittest
    {
        assert(Date(1999, 1, 6).daysInMonth == 31);
        assert(Date(1999, 2, 7).daysInMonth == 28);
        assert(Date(2000, 2, 7).daysInMonth == 29);
        assert(Date(2000, 6, 4).daysInMonth == 30);
    }

    @safe unittest
    {
        // Test A.D.
        assert(Date(1999, 1, 1).daysInMonth == 31);
        assert(Date(1999, 2, 1).daysInMonth == 28);
        assert(Date(2000, 2, 1).daysInMonth == 29);
        assert(Date(1999, 3, 1).daysInMonth == 31);
        assert(Date(1999, 4, 1).daysInMonth == 30);
        assert(Date(1999, 5, 1).daysInMonth == 31);
        assert(Date(1999, 6, 1).daysInMonth == 30);
        assert(Date(1999, 7, 1).daysInMonth == 31);
        assert(Date(1999, 8, 1).daysInMonth == 31);
        assert(Date(1999, 9, 1).daysInMonth == 30);
        assert(Date(1999, 10, 1).daysInMonth == 31);
        assert(Date(1999, 11, 1).daysInMonth == 30);
        assert(Date(1999, 12, 1).daysInMonth == 31);

        // Test B.C.
        assert(Date(-1999, 1, 1).daysInMonth == 31);
        assert(Date(-1999, 2, 1).daysInMonth == 28);
        assert(Date(-2000, 2, 1).daysInMonth == 29);
        assert(Date(-1999, 3, 1).daysInMonth == 31);
        assert(Date(-1999, 4, 1).daysInMonth == 30);
        assert(Date(-1999, 5, 1).daysInMonth == 31);
        assert(Date(-1999, 6, 1).daysInMonth == 30);
        assert(Date(-1999, 7, 1).daysInMonth == 31);
        assert(Date(-1999, 8, 1).daysInMonth == 31);
        assert(Date(-1999, 9, 1).daysInMonth == 30);
        assert(Date(-1999, 10, 1).daysInMonth == 31);
        assert(Date(-1999, 11, 1).daysInMonth == 30);
        assert(Date(-1999, 12, 1).daysInMonth == 31);

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        static assert(!__traits(compiles, cdate.daysInMonth = 30));
        static assert(!__traits(compiles, idate.daysInMonth = 30));
    }


    /++
        Whether the current year is a date in A.D.
      +/
    @property bool isAD() const @safe pure nothrow @nogc
    {
        return _year > 0;
    }

    ///
    @safe unittest
    {
        assert(Date(1, 1, 1).isAD);
        assert(Date(2010, 12, 31).isAD);
        assert(!Date(0, 12, 31).isAD);
        assert(!Date(-2010, 1, 1).isAD);
    }

    @safe unittest
    {
        assert(Date(2010, 7, 4).isAD);
        assert(Date(1, 1, 1).isAD);
        assert(!Date(0, 1, 1).isAD);
        assert(!Date(-1, 1, 1).isAD);
        assert(!Date(-2010, 7, 4).isAD);

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        assert(cdate.isAD);
        assert(idate.isAD);
    }


    /++
        The $(HTTP en.wikipedia.org/wiki/Julian_day, Julian day) for this
        $(LREF Date) at noon (since the Julian day changes at noon).
      +/
    @property long julianDay() const @safe pure nothrow @nogc
    {
        return dayOfGregorianCal + 1_721_425;
    }

    @safe unittest
    {
        assert(Date(-4713, 11, 24).julianDay == 0);
        assert(Date(0, 12, 31).julianDay == 1_721_425);
        assert(Date(1, 1, 1).julianDay == 1_721_426);
        assert(Date(1582, 10, 15).julianDay == 2_299_161);
        assert(Date(1858, 11, 17).julianDay == 2_400_001);
        assert(Date(1982, 1, 4).julianDay == 2_444_974);
        assert(Date(1996, 3, 31).julianDay == 2_450_174);
        assert(Date(2010, 8, 24).julianDay == 2_455_433);

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        assert(cdate.julianDay == 2_451_366);
        assert(idate.julianDay == 2_451_366);
    }


    /++
        The modified $(HTTP en.wikipedia.org/wiki/Julian_day, Julian day) for
        any time on this date (since, the modified Julian day changes at
        midnight).
      +/
    @property long modJulianDay() const @safe pure nothrow @nogc
    {
        return julianDay - 2_400_001;
    }

    @safe unittest
    {
        assert(Date(1858, 11, 17).modJulianDay == 0);
        assert(Date(2010, 8, 24).modJulianDay == 55_432);

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        assert(cdate.modJulianDay == 51_365);
        assert(idate.modJulianDay == 51_365);
    }


    /++
        Converts this $(LREF Date) to a string with the format `YYYYMMDD`.
        If `writer` is set, the resulting string will be written directly
        to it.

        Params:
            writer = A `char` accepting $(REF_ALTTEXT output range, isOutputRange, std, range, primitives)
        Returns:
            A `string` when not using an output range; `void` otherwise.
      +/
    string toISOString() const @safe pure nothrow
    {
        import std.array : appender;
        auto w = appender!string();
        w.reserve(8);
        try
            toISOString(w);
        catch (Exception e)
            assert(0, "toISOString() threw.");
        return w.data;
    }

    ///
    @safe unittest
    {
        assert(Date(2010, 7, 4).toISOString() == "20100704");
        assert(Date(1998, 12, 25).toISOString() == "19981225");
        assert(Date(0, 1, 5).toISOString() == "00000105");
        assert(Date(-4, 1, 5).toISOString() == "-00040105");
    }

    @safe unittest
    {
        // Test A.D.
        assert(Date(9, 12, 4).toISOString() == "00091204");
        assert(Date(99, 12, 4).toISOString() == "00991204");
        assert(Date(999, 12, 4).toISOString() == "09991204");
        assert(Date(9999, 7, 4).toISOString() == "99990704");
        assert(Date(10000, 10, 20).toISOString() == "+100001020");

        // Test B.C.
        assert(Date(0, 12, 4).toISOString() == "00001204");
        assert(Date(-9, 12, 4).toISOString() == "-00091204");
        assert(Date(-99, 12, 4).toISOString() == "-00991204");
        assert(Date(-999, 12, 4).toISOString() == "-09991204");
        assert(Date(-9999, 7, 4).toISOString() == "-99990704");
        assert(Date(-10000, 10, 20).toISOString() == "-100001020");

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        assert(cdate.toISOString() == "19990706");
        assert(idate.toISOString() == "19990706");
    }

    /// ditto
    void toISOString(W)(ref W writer) const
    if (isOutputRange!(W, char))
    {
        import std.format.write : formattedWrite;
        if (_year >= 0)
        {
            if (_year < 10_000)
                formattedWrite(writer, "%04d%02d%02d", _year, _month, _day);
            else
                formattedWrite(writer, "+%05d%02d%02d", _year, _month, _day);
        }
        else if (_year > -10_000)
            formattedWrite(writer, "%05d%02d%02d", _year, _month, _day);
        else
            formattedWrite(writer, "%06d%02d%02d", _year, _month, _day);
    }

    @safe pure unittest
    {
        import std.array : appender;

        auto w = appender!(char[])();
        Date(2010, 7, 4).toISOString(w);
        assert(w.data == "20100704");
        w.clear();
        Date(1998, 12, 25).toISOString(w);
        assert(w.data == "19981225");
    }

    /++
        Converts this $(LREF Date) to a string with the format `YYYY-MM-DD`.
        If `writer` is set, the resulting string will be written directly
        to it.

        Params:
            writer = A `char` accepting $(REF_ALTTEXT output range, isOutputRange, std, range, primitives)
        Returns:
            A `string` when not using an output range; `void` otherwise.
      +/
    string toISOExtString() const @safe pure nothrow
    {
        import std.array : appender;
        auto w = appender!string();
        w.reserve(10);
        try
            toISOExtString(w);
        catch (Exception e)
            assert(0, "toISOExtString() threw.");
        return w.data;
    }

    ///
    @safe unittest
    {
        assert(Date(2010, 7, 4).toISOExtString() == "2010-07-04");
        assert(Date(1998, 12, 25).toISOExtString() == "1998-12-25");
        assert(Date(0, 1, 5).toISOExtString() == "0000-01-05");
        assert(Date(-4, 1, 5).toISOExtString() == "-0004-01-05");
    }

    @safe unittest
    {
        // Test A.D.
        assert(Date(9, 12, 4).toISOExtString() == "0009-12-04");
        assert(Date(99, 12, 4).toISOExtString() == "0099-12-04");
        assert(Date(999, 12, 4).toISOExtString() == "0999-12-04");
        assert(Date(9999, 7, 4).toISOExtString() == "9999-07-04");
        assert(Date(10000, 10, 20).toISOExtString() == "+10000-10-20");

        // Test B.C.
        assert(Date(0, 12, 4).toISOExtString() == "0000-12-04");
        assert(Date(-9, 12, 4).toISOExtString() == "-0009-12-04");
        assert(Date(-99, 12, 4).toISOExtString() == "-0099-12-04");
        assert(Date(-999, 12, 4).toISOExtString() == "-0999-12-04");
        assert(Date(-9999, 7, 4).toISOExtString() == "-9999-07-04");
        assert(Date(-10000, 10, 20).toISOExtString() == "-10000-10-20");

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        assert(cdate.toISOExtString() == "1999-07-06");
        assert(idate.toISOExtString() == "1999-07-06");
    }

    /// ditto
    void toISOExtString(W)(ref W writer) const
    if (isOutputRange!(W, char))
    {
        import std.format.write : formattedWrite;
        if (_year >= 0)
        {
            if (_year < 10_000)
                formattedWrite(writer, "%04d-%02d-%02d", _year, _month, _day);
            else
                formattedWrite(writer, "+%05d-%02d-%02d", _year, _month, _day);
        }
        else if (_year > -10_000)
            formattedWrite(writer, "%05d-%02d-%02d", _year, _month, _day);
        else
            formattedWrite(writer, "%06d-%02d-%02d", _year, _month, _day);
    }

    @safe pure unittest
    {
        import std.array : appender;

        auto w = appender!(char[])();
        Date(2010, 7, 4).toISOExtString(w);
        assert(w.data == "2010-07-04");
        w.clear();
        Date(-4, 1, 5).toISOExtString(w);
        assert(w.data == "-0004-01-05");
    }

    /++
        Converts this $(LREF Date) to a string with the format `YYYY-Mon-DD`.
        If `writer` is set, the resulting string will be written directly
        to it.

        Params:
            writer = A `char` accepting $(REF_ALTTEXT output range, isOutputRange, std, range, primitives)
        Returns:
            A `string` when not using an output range; `void` otherwise.
      +/
    string toSimpleString() const @safe pure nothrow
    {
        import std.array : appender;
        auto w = appender!string();
        w.reserve(11);
        try
            toSimpleString(w);
        catch (Exception e)
            assert(0, "toSimpleString() threw.");
        return w.data;
    }

    ///
    @safe unittest
    {
        assert(Date(2010, 7, 4).toSimpleString() == "2010-Jul-04");
        assert(Date(1998, 12, 25).toSimpleString() == "1998-Dec-25");
        assert(Date(0, 1, 5).toSimpleString() == "0000-Jan-05");
        assert(Date(-4, 1, 5).toSimpleString() == "-0004-Jan-05");
    }

    @safe unittest
    {
        // Test A.D.
        assert(Date(9, 12, 4).toSimpleString() == "0009-Dec-04");
        assert(Date(99, 12, 4).toSimpleString() == "0099-Dec-04");
        assert(Date(999, 12, 4).toSimpleString() == "0999-Dec-04");
        assert(Date(9999, 7, 4).toSimpleString() == "9999-Jul-04");
        assert(Date(10000, 10, 20).toSimpleString() == "+10000-Oct-20");

        // Test B.C.
        assert(Date(0, 12, 4).toSimpleString() == "0000-Dec-04");
        assert(Date(-9, 12, 4).toSimpleString() == "-0009-Dec-04");
        assert(Date(-99, 12, 4).toSimpleString() == "-0099-Dec-04");
        assert(Date(-999, 12, 4).toSimpleString() == "-0999-Dec-04");
        assert(Date(-9999, 7, 4).toSimpleString() == "-9999-Jul-04");
        assert(Date(-10000, 10, 20).toSimpleString() == "-10000-Oct-20");

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        assert(cdate.toSimpleString() == "1999-Jul-06");
        assert(idate.toSimpleString() == "1999-Jul-06");
    }

    /// ditto
    void toSimpleString(W)(ref W writer) const
    if (isOutputRange!(W, char))
    {
        import std.format.write : formattedWrite;
        if (_year >= 0)
        {
            if (_year < 10_000)
                formattedWrite(writer, "%04d-%s-%02d", _year, monthToString(_month), _day);
            else
                formattedWrite(writer, "+%05d-%s-%02d", _year, monthToString(_month), _day);
        }
        else if (_year > -10_000)
            formattedWrite(writer, "%05d-%s-%02d", _year, monthToString(_month), _day);
        else
            formattedWrite(writer, "%06d-%s-%02d", _year, monthToString(_month), _day);
    }

    @safe pure unittest
    {
        import std.array : appender;

        auto w = appender!(char[])();
        Date(9, 12, 4).toSimpleString(w);
        assert(w.data == "0009-Dec-04");
        w.clear();
        Date(-10000, 10, 20).toSimpleString(w);
        assert(w.data == "-10000-Oct-20");
    }

    /++
        Converts this $(LREF Date) to a string.

        This function exists to make it easy to convert a $(LREF Date) to a
        string for code that does not care what the exact format is - just that
        it presents the information in a clear manner. It also makes it easy to
        simply convert a $(LREF Date) to a string when using functions such as
        `to!string`, `format`, or `writeln` which use toString to convert
        user-defined types. So, it is unlikely that much code will call
        toString directly.

        The format of the string is purposefully unspecified, and code that
        cares about the format of the string should use `toISOString`,
        `toISOExtString`, `toSimpleString`, or some other custom formatting
        function that explicitly generates the format that the code needs. The
        reason is that the code is then clear about what format it's using,
        making it less error-prone to maintain the code and interact with other
        software that consumes the generated strings. It's for this same reason
        $(LREF Date) has no `fromString` function, whereas it does have
        `fromISOString`, `fromISOExtString`, and `fromSimpleString`.

        The format returned by toString may or may not change in the future.
      +/
    string toString() const @safe pure nothrow
    {
        return toSimpleString();
    }

    @safe unittest
    {
        auto date = Date(1999, 7, 6);
        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        assert(date.toString());
        assert(cdate.toString());
        assert(idate.toString());
    }

    /// ditto
    void toString(W)(ref W writer) const
    if (isOutputRange!(W, char))
    {
        toSimpleString(writer);
    }

    /++
        Creates a $(LREF Date) from a string with the format YYYYMMDD. Whitespace
        is stripped from the given string.

        Params:
            isoString = A string formatted in the ISO format for dates.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given string is
            not in the ISO format or if the resulting $(LREF Date) would not be
            valid.
      +/
    static Date fromISOString(S)(scope const S isoString) @safe pure
        if (isSomeString!S)
    {
        import std.algorithm.searching : startsWith;
        import std.conv : to, text, ConvException;
        import std.exception : enforce;
        import std.string : strip;

        auto str = isoString.strip;

        enforce!DateTimeException(str.length >= 8, text("Invalid ISO String: ", isoString));

        int day, month, year;
        auto yearStr = str[0 .. $ - 4];

        try
        {
            // using conversion to uint plus cast because it checks for +/-
            // for us quickly while throwing ConvException
            day = cast(int) to!uint(str[$ - 2 .. $]);
            month = cast(int) to!uint(str[$ - 4 .. $ - 2]);

            if (yearStr.length > 4)
            {
                enforce!DateTimeException(yearStr.startsWith('-', '+'),
                        text("Invalid ISO String: ", isoString));
                year = to!int(yearStr);
            }
            else
            {
                year = cast(int) to!uint(yearStr);
            }
        }
        catch (ConvException)
        {
            throw new DateTimeException(text("Invalid ISO String: ", isoString));
        }

        return Date(year, month, day);
    }

    ///
    @safe unittest
    {
        assert(Date.fromISOString("20100704") == Date(2010, 7, 4));
        assert(Date.fromISOString("19981225") == Date(1998, 12, 25));
        assert(Date.fromISOString("00000105") == Date(0, 1, 5));
        assert(Date.fromISOString("-00040105") == Date(-4, 1, 5));
        assert(Date.fromISOString(" 20100704 ") == Date(2010, 7, 4));
    }

    @safe unittest
    {
        assertThrown!DateTimeException(Date.fromISOString(""));
        assertThrown!DateTimeException(Date.fromISOString("990704"));
        assertThrown!DateTimeException(Date.fromISOString("0100704"));
        assertThrown!DateTimeException(Date.fromISOString("2010070"));
        assertThrown!DateTimeException(Date.fromISOString("2010070 "));
        assertThrown!DateTimeException(Date.fromISOString("120100704"));
        assertThrown!DateTimeException(Date.fromISOString("-0100704"));
        assertThrown!DateTimeException(Date.fromISOString("+0100704"));
        assertThrown!DateTimeException(Date.fromISOString("2010070a"));
        assertThrown!DateTimeException(Date.fromISOString("20100a04"));
        assertThrown!DateTimeException(Date.fromISOString("2010a704"));

        assertThrown!DateTimeException(Date.fromISOString("99-07-04"));
        assertThrown!DateTimeException(Date.fromISOString("010-07-04"));
        assertThrown!DateTimeException(Date.fromISOString("2010-07-0"));
        assertThrown!DateTimeException(Date.fromISOString("2010-07-0 "));
        assertThrown!DateTimeException(Date.fromISOString("12010-07-04"));
        assertThrown!DateTimeException(Date.fromISOString("-010-07-04"));
        assertThrown!DateTimeException(Date.fromISOString("+010-07-04"));
        assertThrown!DateTimeException(Date.fromISOString("2010-07-0a"));
        assertThrown!DateTimeException(Date.fromISOString("2010-0a-04"));
        assertThrown!DateTimeException(Date.fromISOString("2010-a7-04"));
        assertThrown!DateTimeException(Date.fromISOString("2010/07/04"));
        assertThrown!DateTimeException(Date.fromISOString("2010/7/04"));
        assertThrown!DateTimeException(Date.fromISOString("2010/7/4"));
        assertThrown!DateTimeException(Date.fromISOString("2010/07/4"));
        assertThrown!DateTimeException(Date.fromISOString("2010-7-04"));
        assertThrown!DateTimeException(Date.fromISOString("2010-7-4"));
        assertThrown!DateTimeException(Date.fromISOString("2010-07-4"));

        assertThrown!DateTimeException(Date.fromISOString("99Jul04"));
        assertThrown!DateTimeException(Date.fromISOString("010Jul04"));
        assertThrown!DateTimeException(Date.fromISOString("2010Jul0"));
        assertThrown!DateTimeException(Date.fromISOString("2010Jul0 "));
        assertThrown!DateTimeException(Date.fromISOString("12010Jul04"));
        assertThrown!DateTimeException(Date.fromISOString("-010Jul04"));
        assertThrown!DateTimeException(Date.fromISOString("+010Jul04"));
        assertThrown!DateTimeException(Date.fromISOString("2010Jul0a"));
        assertThrown!DateTimeException(Date.fromISOString("2010Jua04"));
        assertThrown!DateTimeException(Date.fromISOString("2010aul04"));

        assertThrown!DateTimeException(Date.fromISOString("99-Jul-04"));
        assertThrown!DateTimeException(Date.fromISOString("010-Jul-04"));
        assertThrown!DateTimeException(Date.fromISOString("2010-Jul-0"));
        assertThrown!DateTimeException(Date.fromISOString("2010-Jul-0 "));
        assertThrown!DateTimeException(Date.fromISOString("12010-Jul-04"));
        assertThrown!DateTimeException(Date.fromISOString("-010-Jul-04"));
        assertThrown!DateTimeException(Date.fromISOString("+010-Jul-04"));
        assertThrown!DateTimeException(Date.fromISOString("2010-Jul-0a"));
        assertThrown!DateTimeException(Date.fromISOString("2010-Jua-04"));
        assertThrown!DateTimeException(Date.fromISOString("2010-Jal-04"));
        assertThrown!DateTimeException(Date.fromISOString("2010-aul-04"));

        assertThrown!DateTimeException(Date.fromISOString("2010-07-04"));
        assertThrown!DateTimeException(Date.fromISOString("2010-Jul-04"));

        assert(Date.fromISOString("19990706") == Date(1999, 7, 6));
        assert(Date.fromISOString("-19990706") == Date(-1999, 7, 6));
        assert(Date.fromISOString("+019990706") == Date(1999, 7, 6));
        assert(Date.fromISOString("19990706 ") == Date(1999, 7, 6));
        assert(Date.fromISOString(" 19990706") == Date(1999, 7, 6));
        assert(Date.fromISOString(" 19990706 ") == Date(1999, 7, 6));
    }

    // https://issues.dlang.org/show_bug.cgi?id=17801
    @safe unittest
    {
        import std.conv : to;
        import std.meta : AliasSeq;
        static foreach (C; AliasSeq!(char, wchar, dchar))
        {
            static foreach (S; AliasSeq!(C[], const(C)[], immutable(C)[]))
                assert(Date.fromISOString(to!S("20121221")) == Date(2012, 12, 21));
        }
    }


    /++
        Creates a $(LREF Date) from a string with the format YYYY-MM-DD.
        Whitespace is stripped from the given string.

        Params:
            isoExtString = A string formatted in the ISO Extended format for
                           dates.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given string is
            not in the ISO Extended format or if the resulting $(LREF Date)
            would not be valid.
      +/
    static Date fromISOExtString(S)(scope const S isoExtString) @safe pure
        if (isSomeString!(S))
    {
        import std.algorithm.searching : startsWith;
        import std.conv : to, ConvException;
        import std.format : format;
        import std.string : strip;

        auto str = strip(isoExtString);
        short year;
        ubyte month, day;

        if (str.length < 10 || str[$-3] != '-' || str[$-6] != '-')
            throw new DateTimeException(format("Invalid ISO Extended String: %s", isoExtString));

        auto yearStr = str[0 .. $-6];
        auto signAtBegining = cast(bool) yearStr.startsWith('-', '+');
        if ((yearStr.length > 4) != signAtBegining)
        {
            throw new DateTimeException(format("Invalid ISO Extended String: %s", isoExtString));
        }

        try
        {
            day = to!ubyte(str[$-2 .. $]);
            month = to!ubyte(str[$-5 .. $-3]);
            year = to!short(yearStr);
        }
        catch (ConvException)
        {
            throw new DateTimeException(format("Invalid ISO Extended String: %s", isoExtString));
        }

        return Date(year, month, day);
    }

    ///
    @safe unittest
    {
        assert(Date.fromISOExtString("2010-07-04") == Date(2010, 7, 4));
        assert(Date.fromISOExtString("1998-12-25") == Date(1998, 12, 25));
        assert(Date.fromISOExtString("0000-01-05") == Date(0, 1, 5));
        assert(Date.fromISOExtString("-0004-01-05") == Date(-4, 1, 5));
        assert(Date.fromISOExtString(" 2010-07-04 ") == Date(2010, 7, 4));
    }

    @safe unittest
    {
        assertThrown!DateTimeException(Date.fromISOExtString(""));
        assertThrown!DateTimeException(Date.fromISOExtString("990704"));
        assertThrown!DateTimeException(Date.fromISOExtString("0100704"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010070"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010070 "));
        assertThrown!DateTimeException(Date.fromISOExtString("120100704"));
        assertThrown!DateTimeException(Date.fromISOExtString("-0100704"));
        assertThrown!DateTimeException(Date.fromISOExtString("+0100704"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010070a"));
        assertThrown!DateTimeException(Date.fromISOExtString("20100a04"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010a704"));

        assertThrown!DateTimeException(Date.fromISOExtString("99-07-04"));
        assertThrown!DateTimeException(Date.fromISOExtString("010-07-04"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010-07-0"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010-07-0 "));
        assertThrown!DateTimeException(Date.fromISOExtString("12010-07-04"));
        assertThrown!DateTimeException(Date.fromISOExtString("-010-07-04"));
        assertThrown!DateTimeException(Date.fromISOExtString("+010-07-04"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010-07-0a"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010-0a-04"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010-a7-04"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010/07/04"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010/7/04"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010/7/4"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010/07/4"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010-7-04"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010-7-4"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010-07-4"));

        assertThrown!DateTimeException(Date.fromISOExtString("99Jul04"));
        assertThrown!DateTimeException(Date.fromISOExtString("010Jul04"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010Jul0"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010-Jul-0 "));
        assertThrown!DateTimeException(Date.fromISOExtString("12010Jul04"));
        assertThrown!DateTimeException(Date.fromISOExtString("-010Jul04"));
        assertThrown!DateTimeException(Date.fromISOExtString("+010Jul04"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010Jul0a"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010Jua04"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010aul04"));

        assertThrown!DateTimeException(Date.fromISOExtString("99-Jul-04"));
        assertThrown!DateTimeException(Date.fromISOExtString("010-Jul-04"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010-Jul-0"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010Jul0 "));
        assertThrown!DateTimeException(Date.fromISOExtString("12010-Jul-04"));
        assertThrown!DateTimeException(Date.fromISOExtString("-010-Jul-04"));
        assertThrown!DateTimeException(Date.fromISOExtString("+010-Jul-04"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010-Jul-0a"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010-Jua-04"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010-Jal-04"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010-aul-04"));

        assertThrown!DateTimeException(Date.fromISOExtString("20100704"));
        assertThrown!DateTimeException(Date.fromISOExtString("2010-Jul-04"));

        assert(Date.fromISOExtString("1999-07-06") == Date(1999, 7, 6));
        assert(Date.fromISOExtString("-1999-07-06") == Date(-1999, 7, 6));
        assert(Date.fromISOExtString("+01999-07-06") == Date(1999, 7, 6));
        assert(Date.fromISOExtString("1999-07-06 ") == Date(1999, 7, 6));
        assert(Date.fromISOExtString(" 1999-07-06") == Date(1999, 7, 6));
        assert(Date.fromISOExtString(" 1999-07-06 ") == Date(1999, 7, 6));
    }

    // https://issues.dlang.org/show_bug.cgi?id=17801
    @safe unittest
    {
        import std.conv : to;
        import std.meta : AliasSeq;
        static foreach (C; AliasSeq!(char, wchar, dchar))
        {
            static foreach (S; AliasSeq!(C[], const(C)[], immutable(C)[]))
                assert(Date.fromISOExtString(to!S("2012-12-21")) == Date(2012, 12, 21));
        }
    }


    /++
        Creates a $(LREF Date) from a string with the format YYYY-Mon-DD.
        Whitespace is stripped from the given string.

        Params:
            simpleString = A string formatted in the way that toSimpleString
                           formats dates.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given string is
            not in the correct format or if the resulting $(LREF Date) would not
            be valid.
      +/
    static Date fromSimpleString(S)(scope const S simpleString) @safe pure
        if (isSomeString!(S))
    {
        import std.algorithm.searching : startsWith;
        import std.conv : to, ConvException;
        import std.format : format;
        import std.string : strip;

        auto str = strip(simpleString);

        if (str.length < 11 || str[$-3] != '-' || str[$-7] != '-')
            throw new DateTimeException(format!"Invalid string format: %s"(simpleString));

        int year;
        uint day;
        auto month = monthFromString(str[$ - 6 .. $ - 3]);
        auto yearStr = str[0 .. $ - 7];
        auto signAtBegining = cast(bool) yearStr.startsWith('-', '+');
        if ((yearStr.length > 4) != signAtBegining)
        {
            throw new DateTimeException(format!"Invalid string format: %s"(simpleString));
        }

        try
        {
            day = to!uint(str[$ - 2 .. $]);
            year = to!int(yearStr);
        }
        catch (ConvException)
        {
            throw new DateTimeException(format!"Invalid string format: %s"(simpleString));
        }

        return Date(year, month, day);
    }

    ///
    @safe unittest
    {
        assert(Date.fromSimpleString("2010-Jul-04") == Date(2010, 7, 4));
        assert(Date.fromSimpleString("1998-Dec-25") == Date(1998, 12, 25));
        assert(Date.fromSimpleString("0000-Jan-05") == Date(0, 1, 5));
        assert(Date.fromSimpleString("-0004-Jan-05") == Date(-4, 1, 5));
        assert(Date.fromSimpleString(" 2010-Jul-04 ") == Date(2010, 7, 4));
    }

    @safe unittest
    {
        assertThrown!DateTimeException(Date.fromSimpleString(""));
        assertThrown!DateTimeException(Date.fromSimpleString("990704"));
        assertThrown!DateTimeException(Date.fromSimpleString("0100704"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010070"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010070 "));
        assertThrown!DateTimeException(Date.fromSimpleString("120100704"));
        assertThrown!DateTimeException(Date.fromSimpleString("-0100704"));
        assertThrown!DateTimeException(Date.fromSimpleString("+0100704"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010070a"));
        assertThrown!DateTimeException(Date.fromSimpleString("20100a04"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010a704"));

        assertThrown!DateTimeException(Date.fromSimpleString("99-07-04"));
        assertThrown!DateTimeException(Date.fromSimpleString("010-07-04"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010-07-0"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010-07-0 "));
        assertThrown!DateTimeException(Date.fromSimpleString("12010-07-04"));
        assertThrown!DateTimeException(Date.fromSimpleString("-010-07-04"));
        assertThrown!DateTimeException(Date.fromSimpleString("+010-07-04"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010-07-0a"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010-0a-04"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010-a7-04"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010/07/04"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010/7/04"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010/7/4"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010/07/4"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010-7-04"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010-7-4"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010-07-4"));

        assertThrown!DateTimeException(Date.fromSimpleString("99Jul04"));
        assertThrown!DateTimeException(Date.fromSimpleString("010Jul04"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010Jul0"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010Jul0 "));
        assertThrown!DateTimeException(Date.fromSimpleString("12010Jul04"));
        assertThrown!DateTimeException(Date.fromSimpleString("-010Jul04"));
        assertThrown!DateTimeException(Date.fromSimpleString("+010Jul04"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010Jul0a"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010Jua04"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010aul04"));

        assertThrown!DateTimeException(Date.fromSimpleString("99-Jul-04"));
        assertThrown!DateTimeException(Date.fromSimpleString("010-Jul-04"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010-Jul-0"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010-Jul-0 "));
        assertThrown!DateTimeException(Date.fromSimpleString("12010-Jul-04"));
        assertThrown!DateTimeException(Date.fromSimpleString("-010-Jul-04"));
        assertThrown!DateTimeException(Date.fromSimpleString("+010-Jul-04"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010-Jul-0a"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010-Jua-04"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010-Jal-04"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010-aul-04"));

        assertThrown!DateTimeException(Date.fromSimpleString("20100704"));
        assertThrown!DateTimeException(Date.fromSimpleString("2010-07-04"));

        assert(Date.fromSimpleString("1999-Jul-06") == Date(1999, 7, 6));
        assert(Date.fromSimpleString("-1999-Jul-06") == Date(-1999, 7, 6));
        assert(Date.fromSimpleString("+01999-Jul-06") == Date(1999, 7, 6));
        assert(Date.fromSimpleString("1999-Jul-06 ") == Date(1999, 7, 6));
        assert(Date.fromSimpleString(" 1999-Jul-06") == Date(1999, 7, 6));
        assert(Date.fromSimpleString(" 1999-Jul-06 ") == Date(1999, 7, 6));
    }

    // https://issues.dlang.org/show_bug.cgi?id=17801
    @safe unittest
    {
        import std.conv : to;
        import std.meta : AliasSeq;
        static foreach (C; AliasSeq!(char, wchar, dchar))
        {
            static foreach (S; AliasSeq!(C[], const(C)[], immutable(C)[]))
                assert(Date.fromSimpleString(to!S("2012-Dec-21")) == Date(2012, 12, 21));
        }
    }


    /++
        Returns the $(LREF Date) farthest in the past which is representable by
        $(LREF Date).
      +/
    @property static Date min() @safe pure nothrow @nogc
    {
        auto date = Date.init;
        date._year = short.min;
        date._month = Month.jan;
        date._day = 1;

        return date;
    }

    @safe unittest
    {
        assert(Date.min.year < 0);
        assert(Date.min < Date.max);
    }


    /++
        Returns the $(LREF Date) farthest in the future which is representable
        by $(LREF Date).
      +/
    @property static Date max() @safe pure nothrow @nogc
    {
        auto date = Date.init;
        date._year = short.max;
        date._month = Month.dec;
        date._day = 31;

        return date;
    }

    @safe unittest
    {
        assert(Date.max.year > 0);
        assert(Date.max > Date.min);
    }


private:

    /+
        Whether the given values form a valid date.

        Params:
            year  = The year to test.
            month = The month of the Gregorian Calendar to test.
            day   = The day of the month to test.
     +/
    static bool _valid(int year, int month, int day) @safe pure nothrow @nogc
    {
        if (!valid!"months"(month))
            return false;
        return valid!"days"(year, month, day);
    }


package:

    /+
        Adds the given number of days to this $(LREF Date). A negative number
        will subtract.

        The month will be adjusted along with the day if the number of days
        added (or subtracted) would overflow (or underflow) the current month.
        The year will be adjusted along with the month if the increase (or
        decrease) to the month would cause it to overflow (or underflow) the
        current year.

        `_addDays(numDays)` is effectively equivalent to
        $(D date.dayOfGregorianCal = date.dayOfGregorianCal + days).

        Params:
            days = The number of days to add to this Date.
      +/
    ref Date _addDays(long days) return @safe pure nothrow @nogc
    {
        dayOfGregorianCal = cast(int)(dayOfGregorianCal + days);
        return this;
    }

    @safe unittest
    {
        // Test A.D.
        {
            auto date = Date(1999, 2, 28);
            date._addDays(1);
            assert(date == Date(1999, 3, 1));
            date._addDays(-1);
            assert(date == Date(1999, 2, 28));
        }

        {
            auto date = Date(2000, 2, 28);
            date._addDays(1);
            assert(date == Date(2000, 2, 29));
            date._addDays(1);
            assert(date == Date(2000, 3, 1));
            date._addDays(-1);
            assert(date == Date(2000, 2, 29));
        }

        {
            auto date = Date(1999, 6, 30);
            date._addDays(1);
            assert(date == Date(1999, 7, 1));
            date._addDays(-1);
            assert(date == Date(1999, 6, 30));
        }

        {
            auto date = Date(1999, 7, 31);
            date._addDays(1);
            assert(date == Date(1999, 8, 1));
            date._addDays(-1);
            assert(date == Date(1999, 7, 31));
        }

        {
            auto date = Date(1999, 1, 1);
            date._addDays(-1);
            assert(date == Date(1998, 12, 31));
            date._addDays(1);
            assert(date == Date(1999, 1, 1));
        }

        {
            auto date = Date(1999, 7, 6);
            date._addDays(9);
            assert(date == Date(1999, 7, 15));
            date._addDays(-11);
            assert(date == Date(1999, 7, 4));
            date._addDays(30);
            assert(date == Date(1999, 8, 3));
            date._addDays(-3);
            assert(date == Date(1999, 7, 31));
        }

        {
            auto date = Date(1999, 7, 6);
            date._addDays(365);
            assert(date == Date(2000, 7, 5));
            date._addDays(-365);
            assert(date == Date(1999, 7, 6));
            date._addDays(366);
            assert(date == Date(2000, 7, 6));
            date._addDays(730);
            assert(date == Date(2002, 7, 6));
            date._addDays(-1096);
            assert(date == Date(1999, 7, 6));
        }

        // Test B.C.
        {
            auto date = Date(-1999, 2, 28);
            date._addDays(1);
            assert(date == Date(-1999, 3, 1));
            date._addDays(-1);
            assert(date == Date(-1999, 2, 28));
        }

        {
            auto date = Date(-2000, 2, 28);
            date._addDays(1);
            assert(date == Date(-2000, 2, 29));
            date._addDays(1);
            assert(date == Date(-2000, 3, 1));
            date._addDays(-1);
            assert(date == Date(-2000, 2, 29));
        }

        {
            auto date = Date(-1999, 6, 30);
            date._addDays(1);
            assert(date == Date(-1999, 7, 1));
            date._addDays(-1);
            assert(date == Date(-1999, 6, 30));
        }

        {
            auto date = Date(-1999, 7, 31);
            date._addDays(1);
            assert(date == Date(-1999, 8, 1));
            date._addDays(-1);
            assert(date == Date(-1999, 7, 31));
        }

        {
            auto date = Date(-1999, 1, 1);
            date._addDays(-1);
            assert(date == Date(-2000, 12, 31));
            date._addDays(1);
            assert(date == Date(-1999, 1, 1));
        }

        {
            auto date = Date(-1999, 7, 6);
            date._addDays(9);
            assert(date == Date(-1999, 7, 15));
            date._addDays(-11);
            assert(date == Date(-1999, 7, 4));
            date._addDays(30);
            assert(date == Date(-1999, 8, 3));
            date._addDays(-3);
        }

        {
            auto date = Date(-1999, 7, 6);
            date._addDays(365);
            assert(date == Date(-1998, 7, 6));
            date._addDays(-365);
            assert(date == Date(-1999, 7, 6));
            date._addDays(366);
            assert(date == Date(-1998, 7, 7));
            date._addDays(730);
            assert(date == Date(-1996, 7, 6));
            date._addDays(-1096);
            assert(date == Date(-1999, 7, 6));
        }

        // Test Both
        {
            auto date = Date(1, 7, 6);
            date._addDays(-365);
            assert(date == Date(0, 7, 6));
            date._addDays(365);
            assert(date == Date(1, 7, 6));
            date._addDays(-731);
            assert(date == Date(-1, 7, 6));
            date._addDays(730);
            assert(date == Date(1, 7, 5));
        }

        const cdate = Date(1999, 7, 6);
        immutable idate = Date(1999, 7, 6);
        static assert(!__traits(compiles, cdate._addDays(12)));
        static assert(!__traits(compiles, idate._addDays(12)));
    }


    @safe pure invariant()
    {
        import std.format : format;
        assert(valid!"months"(_month),
               format("Invariant Failure: year [%s] month [%s] day [%s]", _year, _month, _day));
        assert(valid!"days"(_year, _month, _day),
               format("Invariant Failure: year [%s] month [%s] day [%s]", _year, _month, _day));
    }

    short _year  = 1;
    Month _month = Month.jan;
    ubyte _day   = 1;
}

///
@safe pure unittest
{
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


/++
    Represents a time of day with hours, minutes, and seconds. It uses 24 hour
    time.
+/
struct TimeOfDay
{
public:

    /++
        Params:
            hour   = Hour of the day [0 - 24$(RPAREN).
            minute = Minute of the hour [0 - 60$(RPAREN).
            second = Second of the minute [0 - 60$(RPAREN).

        Throws:
            $(REF DateTimeException,std,datetime,date) if the resulting
            $(LREF TimeOfDay) would be not be valid.
     +/
    this(int hour, int minute, int second = 0) @safe pure
    {
        enforceValid!"hours"(hour);
        enforceValid!"minutes"(minute);
        enforceValid!"seconds"(second);

        _hour   = cast(ubyte) hour;
        _minute = cast(ubyte) minute;
        _second = cast(ubyte) second;
    }

    @safe unittest
    {
        assert(TimeOfDay(0, 0) == TimeOfDay.init);

        {
            auto tod = TimeOfDay(0, 0);
            assert(tod._hour == 0);
            assert(tod._minute == 0);
            assert(tod._second == 0);
        }

        {
            auto tod = TimeOfDay(12, 30, 33);
            assert(tod._hour == 12);
            assert(tod._minute == 30);
            assert(tod._second == 33);
        }

        {
            auto tod = TimeOfDay(23, 59, 59);
            assert(tod._hour == 23);
            assert(tod._minute == 59);
            assert(tod._second == 59);
        }

        assertThrown!DateTimeException(TimeOfDay(24, 0, 0));
        assertThrown!DateTimeException(TimeOfDay(0, 60, 0));
        assertThrown!DateTimeException(TimeOfDay(0, 0, 60));
    }


    /++
        Compares this $(LREF TimeOfDay) with the given $(LREF TimeOfDay).

        Returns:
            $(BOOKTABLE,
            $(TR $(TD this &lt; rhs) $(TD &lt; 0))
            $(TR $(TD this == rhs) $(TD 0))
            $(TR $(TD this &gt; rhs) $(TD &gt; 0))
            )
     +/
    int opCmp(TimeOfDay rhs) const @safe pure nothrow @nogc
    {
        if (_hour < rhs._hour)
            return -1;
        if (_hour > rhs._hour)
            return 1;

        if (_minute < rhs._minute)
            return -1;
        if (_minute > rhs._minute)
            return 1;

        if (_second < rhs._second)
            return -1;
        if (_second > rhs._second)
            return 1;

        return 0;
    }

    @safe unittest
    {
        assert(TimeOfDay(0, 0, 0).opCmp(TimeOfDay.init) == 0);

        assert(TimeOfDay(0, 0, 0).opCmp(TimeOfDay(0, 0, 0)) == 0);
        assert(TimeOfDay(12, 0, 0).opCmp(TimeOfDay(12, 0, 0)) == 0);
        assert(TimeOfDay(0, 30, 0).opCmp(TimeOfDay(0, 30, 0)) == 0);
        assert(TimeOfDay(0, 0, 33).opCmp(TimeOfDay(0, 0, 33)) == 0);

        assert(TimeOfDay(12, 30, 0).opCmp(TimeOfDay(12, 30, 0)) == 0);
        assert(TimeOfDay(12, 30, 33).opCmp(TimeOfDay(12, 30, 33)) == 0);

        assert(TimeOfDay(0, 30, 33).opCmp(TimeOfDay(0, 30, 33)) == 0);
        assert(TimeOfDay(0, 0, 33).opCmp(TimeOfDay(0, 0, 33)) == 0);

        assert(TimeOfDay(12, 30, 33).opCmp(TimeOfDay(13, 30, 33)) < 0);
        assert(TimeOfDay(13, 30, 33).opCmp(TimeOfDay(12, 30, 33)) > 0);
        assert(TimeOfDay(12, 30, 33).opCmp(TimeOfDay(12, 31, 33)) < 0);
        assert(TimeOfDay(12, 31, 33).opCmp(TimeOfDay(12, 30, 33)) > 0);
        assert(TimeOfDay(12, 30, 33).opCmp(TimeOfDay(12, 30, 34)) < 0);
        assert(TimeOfDay(12, 30, 34).opCmp(TimeOfDay(12, 30, 33)) > 0);

        assert(TimeOfDay(13, 30, 33).opCmp(TimeOfDay(12, 30, 34)) > 0);
        assert(TimeOfDay(12, 30, 34).opCmp(TimeOfDay(13, 30, 33)) < 0);
        assert(TimeOfDay(13, 30, 33).opCmp(TimeOfDay(12, 31, 33)) > 0);
        assert(TimeOfDay(12, 31, 33).opCmp(TimeOfDay(13, 30, 33)) < 0);

        assert(TimeOfDay(12, 31, 33).opCmp(TimeOfDay(12, 30, 34)) > 0);
        assert(TimeOfDay(12, 30, 34).opCmp(TimeOfDay(12, 31, 33)) < 0);

        const ctod = TimeOfDay(12, 30, 33);
        immutable itod = TimeOfDay(12, 30, 33);
        assert(ctod.opCmp(itod) == 0);
        assert(itod.opCmp(ctod) == 0);
    }


    /++
        Hours past midnight.
     +/
    @property ubyte hour() const @safe pure nothrow @nogc
    {
        return _hour;
    }

    @safe unittest
    {
        assert(TimeOfDay.init.hour == 0);
        assert(TimeOfDay(12, 0, 0).hour == 12);

        const ctod = TimeOfDay(12, 0, 0);
        immutable itod = TimeOfDay(12, 0, 0);
        assert(ctod.hour == 12);
        assert(itod.hour == 12);
    }


    /++
        Hours past midnight.

        Params:
            hour = The hour of the day to set this $(LREF TimeOfDay)'s hour to.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given hour would
            result in an invalid $(LREF TimeOfDay).
     +/
    @property void hour(int hour) @safe pure
    {
        enforceValid!"hours"(hour);
        _hour = cast(ubyte) hour;
    }

    @safe unittest
    {
        assertThrown!DateTimeException((){TimeOfDay(0, 0, 0).hour = 24;}());

        auto tod = TimeOfDay(0, 0, 0);
        tod.hour = 12;
        assert(tod == TimeOfDay(12, 0, 0));

        const ctod = TimeOfDay(0, 0, 0);
        immutable itod = TimeOfDay(0, 0, 0);
        static assert(!__traits(compiles, ctod.hour = 12));
        static assert(!__traits(compiles, itod.hour = 12));
    }


    /++
        Minutes past the hour.
     +/
    @property ubyte minute() const @safe pure nothrow @nogc
    {
        return _minute;
    }

    @safe unittest
    {
        assert(TimeOfDay.init.minute == 0);
        assert(TimeOfDay(0, 30, 0).minute == 30);

        const ctod = TimeOfDay(0, 30, 0);
        immutable itod = TimeOfDay(0, 30, 0);
        assert(ctod.minute == 30);
        assert(itod.minute == 30);
    }


    /++
        Minutes past the hour.

        Params:
            minute = The minute to set this $(LREF TimeOfDay)'s minute to.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given minute
            would result in an invalid $(LREF TimeOfDay).
     +/
    @property void minute(int minute) @safe pure
    {
        enforceValid!"minutes"(minute);
        _minute = cast(ubyte) minute;
    }

    @safe unittest
    {
        assertThrown!DateTimeException((){TimeOfDay(0, 0, 0).minute = 60;}());

        auto tod = TimeOfDay(0, 0, 0);
        tod.minute = 30;
        assert(tod == TimeOfDay(0, 30, 0));

        const ctod = TimeOfDay(0, 0, 0);
        immutable itod = TimeOfDay(0, 0, 0);
        static assert(!__traits(compiles, ctod.minute = 30));
        static assert(!__traits(compiles, itod.minute = 30));
    }


    /++
        Seconds past the minute.
     +/
    @property ubyte second() const @safe pure nothrow @nogc
    {
        return _second;
    }

    @safe unittest
    {
        assert(TimeOfDay.init.second == 0);
        assert(TimeOfDay(0, 0, 33).second == 33);

        const ctod = TimeOfDay(0, 0, 33);
        immutable itod = TimeOfDay(0, 0, 33);
        assert(ctod.second == 33);
        assert(itod.second == 33);
    }


    /++
        Seconds past the minute.

        Params:
            second = The second to set this $(LREF TimeOfDay)'s second to.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given second
            would result in an invalid $(LREF TimeOfDay).
     +/
    @property void second(int second) @safe pure
    {
        enforceValid!"seconds"(second);
        _second = cast(ubyte) second;
    }

    @safe unittest
    {
        assertThrown!DateTimeException((){TimeOfDay(0, 0, 0).second = 60;}());

        auto tod = TimeOfDay(0, 0, 0);
        tod.second = 33;
        assert(tod == TimeOfDay(0, 0, 33));

        const ctod = TimeOfDay(0, 0, 0);
        immutable itod = TimeOfDay(0, 0, 0);
        static assert(!__traits(compiles, ctod.second = 33));
        static assert(!__traits(compiles, itod.second = 33));
    }


    /++
        Adds the given number of units to this $(LREF TimeOfDay), mutating it. A
        negative number will subtract.

        The difference between rolling and adding is that rolling does not
        affect larger units. For instance, rolling a $(LREF TimeOfDay)
        one hours's worth of minutes gets the exact same
        $(LREF TimeOfDay).

        Accepted units are `"hours"`, `"minutes"`, and `"seconds"`.

        Params:
            units = The units to add.
            value = The number of $(D_PARAM units) to add to this
                    $(LREF TimeOfDay).

        Returns:
            A reference to the `TimeOfDay` (`this`).
      +/
    ref TimeOfDay roll(string units)(long value) @safe pure nothrow @nogc
        if (units == "hours")
    {
        import core.time : dur;
        return this += dur!"hours"(value);
    }

    ///
    @safe unittest
    {
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
        auto tod = TimeOfDay(12, 27, 2);
        tod.roll!"hours"(22).roll!"hours"(-7);
        assert(tod == TimeOfDay(3, 27, 2));

        const ctod = TimeOfDay(0, 0, 0);
        immutable itod = TimeOfDay(0, 0, 0);
        static assert(!__traits(compiles, ctod.roll!"hours"(53)));
        static assert(!__traits(compiles, itod.roll!"hours"(53)));
    }


    /// ditto
    ref TimeOfDay roll(string units)(long value) @safe pure nothrow @nogc
        if (units == "minutes" || units == "seconds")
    {
        import std.format : format;

        enum memberVarStr = units[0 .. $ - 1];
        value %= 60;
        mixin(format("auto newVal = cast(ubyte)(_%s) + value;", memberVarStr));

        if (value < 0)
        {
            if (newVal < 0)
                newVal += 60;
        }
        else if (newVal >= 60)
            newVal -= 60;

        mixin(format("_%s = cast(ubyte) newVal;", memberVarStr));
        return this;
    }

    // Test roll!"minutes"().
    @safe unittest
    {
        static void testTOD(TimeOfDay orig, int minutes, TimeOfDay expected, size_t line = __LINE__)
        {
            orig.roll!"minutes"(minutes);
            assert(orig == expected);
        }

        testTOD(TimeOfDay(12, 30, 33), 0, TimeOfDay(12, 30, 33));
        testTOD(TimeOfDay(12, 30, 33), 1, TimeOfDay(12, 31, 33));
        testTOD(TimeOfDay(12, 30, 33), 2, TimeOfDay(12, 32, 33));
        testTOD(TimeOfDay(12, 30, 33), 3, TimeOfDay(12, 33, 33));
        testTOD(TimeOfDay(12, 30, 33), 4, TimeOfDay(12, 34, 33));
        testTOD(TimeOfDay(12, 30, 33), 5, TimeOfDay(12, 35, 33));
        testTOD(TimeOfDay(12, 30, 33), 10, TimeOfDay(12, 40, 33));
        testTOD(TimeOfDay(12, 30, 33), 15, TimeOfDay(12, 45, 33));
        testTOD(TimeOfDay(12, 30, 33), 29, TimeOfDay(12, 59, 33));
        testTOD(TimeOfDay(12, 30, 33), 30, TimeOfDay(12, 0, 33));
        testTOD(TimeOfDay(12, 30, 33), 45, TimeOfDay(12, 15, 33));
        testTOD(TimeOfDay(12, 30, 33), 60, TimeOfDay(12, 30, 33));
        testTOD(TimeOfDay(12, 30, 33), 75, TimeOfDay(12, 45, 33));
        testTOD(TimeOfDay(12, 30, 33), 90, TimeOfDay(12, 0, 33));
        testTOD(TimeOfDay(12, 30, 33), 100, TimeOfDay(12, 10, 33));

        testTOD(TimeOfDay(12, 30, 33), 689, TimeOfDay(12, 59, 33));
        testTOD(TimeOfDay(12, 30, 33), 690, TimeOfDay(12, 0, 33));
        testTOD(TimeOfDay(12, 30, 33), 691, TimeOfDay(12, 1, 33));
        testTOD(TimeOfDay(12, 30, 33), 960, TimeOfDay(12, 30, 33));
        testTOD(TimeOfDay(12, 30, 33), 1439, TimeOfDay(12, 29, 33));
        testTOD(TimeOfDay(12, 30, 33), 1440, TimeOfDay(12, 30, 33));
        testTOD(TimeOfDay(12, 30, 33), 1441, TimeOfDay(12, 31, 33));
        testTOD(TimeOfDay(12, 30, 33), 2880, TimeOfDay(12, 30, 33));

        testTOD(TimeOfDay(12, 30, 33), -1, TimeOfDay(12, 29, 33));
        testTOD(TimeOfDay(12, 30, 33), -2, TimeOfDay(12, 28, 33));
        testTOD(TimeOfDay(12, 30, 33), -3, TimeOfDay(12, 27, 33));
        testTOD(TimeOfDay(12, 30, 33), -4, TimeOfDay(12, 26, 33));
        testTOD(TimeOfDay(12, 30, 33), -5, TimeOfDay(12, 25, 33));
        testTOD(TimeOfDay(12, 30, 33), -10, TimeOfDay(12, 20, 33));
        testTOD(TimeOfDay(12, 30, 33), -15, TimeOfDay(12, 15, 33));
        testTOD(TimeOfDay(12, 30, 33), -29, TimeOfDay(12, 1, 33));
        testTOD(TimeOfDay(12, 30, 33), -30, TimeOfDay(12, 0, 33));
        testTOD(TimeOfDay(12, 30, 33), -45, TimeOfDay(12, 45, 33));
        testTOD(TimeOfDay(12, 30, 33), -60, TimeOfDay(12, 30, 33));
        testTOD(TimeOfDay(12, 30, 33), -75, TimeOfDay(12, 15, 33));
        testTOD(TimeOfDay(12, 30, 33), -90, TimeOfDay(12, 0, 33));
        testTOD(TimeOfDay(12, 30, 33), -100, TimeOfDay(12, 50, 33));

        testTOD(TimeOfDay(12, 30, 33), -749, TimeOfDay(12, 1, 33));
        testTOD(TimeOfDay(12, 30, 33), -750, TimeOfDay(12, 0, 33));
        testTOD(TimeOfDay(12, 30, 33), -751, TimeOfDay(12, 59, 33));
        testTOD(TimeOfDay(12, 30, 33), -960, TimeOfDay(12, 30, 33));
        testTOD(TimeOfDay(12, 30, 33), -1439, TimeOfDay(12, 31, 33));
        testTOD(TimeOfDay(12, 30, 33), -1440, TimeOfDay(12, 30, 33));
        testTOD(TimeOfDay(12, 30, 33), -1441, TimeOfDay(12, 29, 33));
        testTOD(TimeOfDay(12, 30, 33), -2880, TimeOfDay(12, 30, 33));

        testTOD(TimeOfDay(12, 0, 33), 1, TimeOfDay(12, 1, 33));
        testTOD(TimeOfDay(12, 0, 33), 0, TimeOfDay(12, 0, 33));
        testTOD(TimeOfDay(12, 0, 33), -1, TimeOfDay(12, 59, 33));

        testTOD(TimeOfDay(11, 59, 33), 1, TimeOfDay(11, 0, 33));
        testTOD(TimeOfDay(11, 59, 33), 0, TimeOfDay(11, 59, 33));
        testTOD(TimeOfDay(11, 59, 33), -1, TimeOfDay(11, 58, 33));

        testTOD(TimeOfDay(0, 0, 33), 1, TimeOfDay(0, 1, 33));
        testTOD(TimeOfDay(0, 0, 33), 0, TimeOfDay(0, 0, 33));
        testTOD(TimeOfDay(0, 0, 33), -1, TimeOfDay(0, 59, 33));

        testTOD(TimeOfDay(23, 59, 33), 1, TimeOfDay(23, 0, 33));
        testTOD(TimeOfDay(23, 59, 33), 0, TimeOfDay(23, 59, 33));
        testTOD(TimeOfDay(23, 59, 33), -1, TimeOfDay(23, 58, 33));

        auto tod = TimeOfDay(12, 27, 2);
        tod.roll!"minutes"(97).roll!"minutes"(-102);
        assert(tod == TimeOfDay(12, 22, 2));

        const ctod = TimeOfDay(0, 0, 0);
        immutable itod = TimeOfDay(0, 0, 0);
        static assert(!__traits(compiles, ctod.roll!"minutes"(7)));
        static assert(!__traits(compiles, itod.roll!"minutes"(7)));
    }

    // Test roll!"seconds"().
    @safe unittest
    {
        static void testTOD(TimeOfDay orig, int seconds, TimeOfDay expected, size_t line = __LINE__)
        {
            orig.roll!"seconds"(seconds);
            assert(orig == expected);
        }

        testTOD(TimeOfDay(12, 30, 33), 0, TimeOfDay(12, 30, 33));
        testTOD(TimeOfDay(12, 30, 33), 1, TimeOfDay(12, 30, 34));
        testTOD(TimeOfDay(12, 30, 33), 2, TimeOfDay(12, 30, 35));
        testTOD(TimeOfDay(12, 30, 33), 3, TimeOfDay(12, 30, 36));
        testTOD(TimeOfDay(12, 30, 33), 4, TimeOfDay(12, 30, 37));
        testTOD(TimeOfDay(12, 30, 33), 5, TimeOfDay(12, 30, 38));
        testTOD(TimeOfDay(12, 30, 33), 10, TimeOfDay(12, 30, 43));
        testTOD(TimeOfDay(12, 30, 33), 15, TimeOfDay(12, 30, 48));
        testTOD(TimeOfDay(12, 30, 33), 26, TimeOfDay(12, 30, 59));
        testTOD(TimeOfDay(12, 30, 33), 27, TimeOfDay(12, 30, 0));
        testTOD(TimeOfDay(12, 30, 33), 30, TimeOfDay(12, 30, 3));
        testTOD(TimeOfDay(12, 30, 33), 59, TimeOfDay(12, 30, 32));
        testTOD(TimeOfDay(12, 30, 33), 60, TimeOfDay(12, 30, 33));
        testTOD(TimeOfDay(12, 30, 33), 61, TimeOfDay(12, 30, 34));

        testTOD(TimeOfDay(12, 30, 33), 1766, TimeOfDay(12, 30, 59));
        testTOD(TimeOfDay(12, 30, 33), 1767, TimeOfDay(12, 30, 0));
        testTOD(TimeOfDay(12, 30, 33), 1768, TimeOfDay(12, 30, 1));
        testTOD(TimeOfDay(12, 30, 33), 2007, TimeOfDay(12, 30, 0));
        testTOD(TimeOfDay(12, 30, 33), 3599, TimeOfDay(12, 30, 32));
        testTOD(TimeOfDay(12, 30, 33), 3600, TimeOfDay(12, 30, 33));
        testTOD(TimeOfDay(12, 30, 33), 3601, TimeOfDay(12, 30, 34));
        testTOD(TimeOfDay(12, 30, 33), 7200, TimeOfDay(12, 30, 33));

        testTOD(TimeOfDay(12, 30, 33), -1, TimeOfDay(12, 30, 32));
        testTOD(TimeOfDay(12, 30, 33), -2, TimeOfDay(12, 30, 31));
        testTOD(TimeOfDay(12, 30, 33), -3, TimeOfDay(12, 30, 30));
        testTOD(TimeOfDay(12, 30, 33), -4, TimeOfDay(12, 30, 29));
        testTOD(TimeOfDay(12, 30, 33), -5, TimeOfDay(12, 30, 28));
        testTOD(TimeOfDay(12, 30, 33), -10, TimeOfDay(12, 30, 23));
        testTOD(TimeOfDay(12, 30, 33), -15, TimeOfDay(12, 30, 18));
        testTOD(TimeOfDay(12, 30, 33), -33, TimeOfDay(12, 30, 0));
        testTOD(TimeOfDay(12, 30, 33), -34, TimeOfDay(12, 30, 59));
        testTOD(TimeOfDay(12, 30, 33), -35, TimeOfDay(12, 30, 58));
        testTOD(TimeOfDay(12, 30, 33), -59, TimeOfDay(12, 30, 34));
        testTOD(TimeOfDay(12, 30, 33), -60, TimeOfDay(12, 30, 33));
        testTOD(TimeOfDay(12, 30, 33), -61, TimeOfDay(12, 30, 32));

        testTOD(TimeOfDay(12, 30, 0), 1, TimeOfDay(12, 30, 1));
        testTOD(TimeOfDay(12, 30, 0), 0, TimeOfDay(12, 30, 0));
        testTOD(TimeOfDay(12, 30, 0), -1, TimeOfDay(12, 30, 59));

        testTOD(TimeOfDay(12, 0, 0), 1, TimeOfDay(12, 0, 1));
        testTOD(TimeOfDay(12, 0, 0), 0, TimeOfDay(12, 0, 0));
        testTOD(TimeOfDay(12, 0, 0), -1, TimeOfDay(12, 0, 59));

        testTOD(TimeOfDay(0, 0, 0), 1, TimeOfDay(0, 0, 1));
        testTOD(TimeOfDay(0, 0, 0), 0, TimeOfDay(0, 0, 0));
        testTOD(TimeOfDay(0, 0, 0), -1, TimeOfDay(0, 0, 59));

        testTOD(TimeOfDay(23, 59, 59), 1, TimeOfDay(23, 59, 0));
        testTOD(TimeOfDay(23, 59, 59), 0, TimeOfDay(23, 59, 59));
        testTOD(TimeOfDay(23, 59, 59), -1, TimeOfDay(23, 59, 58));

        auto tod = TimeOfDay(12, 27, 2);
        tod.roll!"seconds"(105).roll!"seconds"(-77);
        assert(tod == TimeOfDay(12, 27, 30));

        const ctod = TimeOfDay(0, 0, 0);
        immutable itod = TimeOfDay(0, 0, 0);
        static assert(!__traits(compiles, ctod.roll!"seconds"(7)));
        static assert(!__traits(compiles, itod.roll!"seconds"(7)));
    }


    import core.time : Duration;
    /++
        Gives the result of adding or subtracting a $(REF Duration, core,time)
        from this $(LREF TimeOfDay).

        The legal types of arithmetic for $(LREF TimeOfDay) using this operator
        are

        $(BOOKTABLE,
        $(TR $(TD TimeOfDay) $(TD +) $(TD Duration) $(TD -->) $(TD TimeOfDay))
        $(TR $(TD TimeOfDay) $(TD -) $(TD Duration) $(TD -->) $(TD TimeOfDay))
        )

        Params:
            duration = The $(REF Duration, core,time) to add to or subtract from
                       this $(LREF TimeOfDay).
      +/
    TimeOfDay opBinary(string op)(Duration duration) const @safe pure nothrow @nogc
        if (op == "+" || op == "-")
    {
        TimeOfDay retval = this;
        immutable seconds = duration.total!"seconds";
        mixin("return retval._addSeconds(" ~ op ~ "seconds);");
    }

    ///
    @safe unittest
    {
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
        auto tod = TimeOfDay(12, 30, 33);

        import core.time : dur;
        assert(tod + dur!"hours"(7) == TimeOfDay(19, 30, 33));
        assert(tod + dur!"hours"(-7) == TimeOfDay(5, 30, 33));
        assert(tod + dur!"minutes"(7) == TimeOfDay(12, 37, 33));
        assert(tod + dur!"minutes"(-7) == TimeOfDay(12, 23, 33));
        assert(tod + dur!"seconds"(7) == TimeOfDay(12, 30, 40));
        assert(tod + dur!"seconds"(-7) == TimeOfDay(12, 30, 26));

        assert(tod + dur!"msecs"(7000) == TimeOfDay(12, 30, 40));
        assert(tod + dur!"msecs"(-7000) == TimeOfDay(12, 30, 26));
        assert(tod + dur!"usecs"(7_000_000) == TimeOfDay(12, 30, 40));
        assert(tod + dur!"usecs"(-7_000_000) == TimeOfDay(12, 30, 26));
        assert(tod + dur!"hnsecs"(70_000_000) == TimeOfDay(12, 30, 40));
        assert(tod + dur!"hnsecs"(-70_000_000) == TimeOfDay(12, 30, 26));

        assert(tod - dur!"hours"(-7) == TimeOfDay(19, 30, 33));
        assert(tod - dur!"hours"(7) == TimeOfDay(5, 30, 33));
        assert(tod - dur!"minutes"(-7) == TimeOfDay(12, 37, 33));
        assert(tod - dur!"minutes"(7) == TimeOfDay(12, 23, 33));
        assert(tod - dur!"seconds"(-7) == TimeOfDay(12, 30, 40));
        assert(tod - dur!"seconds"(7) == TimeOfDay(12, 30, 26));

        assert(tod - dur!"msecs"(-7000) == TimeOfDay(12, 30, 40));
        assert(tod - dur!"msecs"(7000) == TimeOfDay(12, 30, 26));
        assert(tod - dur!"usecs"(-7_000_000) == TimeOfDay(12, 30, 40));
        assert(tod - dur!"usecs"(7_000_000) == TimeOfDay(12, 30, 26));
        assert(tod - dur!"hnsecs"(-70_000_000) == TimeOfDay(12, 30, 40));
        assert(tod - dur!"hnsecs"(70_000_000) == TimeOfDay(12, 30, 26));

        auto duration = dur!"hours"(11);
        const ctod = TimeOfDay(12, 30, 33);
        immutable itod = TimeOfDay(12, 30, 33);
        assert(tod + duration == TimeOfDay(23, 30, 33));
        assert(ctod + duration == TimeOfDay(23, 30, 33));
        assert(itod + duration == TimeOfDay(23, 30, 33));

        assert(tod - duration == TimeOfDay(1, 30, 33));
        assert(ctod - duration == TimeOfDay(1, 30, 33));
        assert(itod - duration == TimeOfDay(1, 30, 33));
    }


    /++
        Gives the result of adding or subtracting a $(REF Duration, core,time)
        from this $(LREF TimeOfDay), as well as assigning the result to this
        $(LREF TimeOfDay).

        The legal types of arithmetic for $(LREF TimeOfDay) using this operator
        are

        $(BOOKTABLE,
        $(TR $(TD TimeOfDay) $(TD +) $(TD Duration) $(TD -->) $(TD TimeOfDay))
        $(TR $(TD TimeOfDay) $(TD -) $(TD Duration) $(TD -->) $(TD TimeOfDay))
        )

        Params:
            duration = The $(REF Duration, core,time) to add to or subtract from
                       this $(LREF TimeOfDay).
      +/
    ref TimeOfDay opOpAssign(string op)(Duration duration) @safe pure nothrow @nogc
        if (op == "+" || op == "-")
    {
        immutable seconds = duration.total!"seconds";
        mixin("return _addSeconds(" ~ op ~ "seconds);");
    }

    @safe unittest
    {
        import core.time : dur;
        auto duration = dur!"hours"(12);

        assert(TimeOfDay(12, 30, 33) + dur!"hours"(7) == TimeOfDay(19, 30, 33));
        assert(TimeOfDay(12, 30, 33) + dur!"hours"(-7) == TimeOfDay(5, 30, 33));
        assert(TimeOfDay(12, 30, 33) + dur!"minutes"(7) == TimeOfDay(12, 37, 33));
        assert(TimeOfDay(12, 30, 33) + dur!"minutes"(-7) == TimeOfDay(12, 23, 33));
        assert(TimeOfDay(12, 30, 33) + dur!"seconds"(7) == TimeOfDay(12, 30, 40));
        assert(TimeOfDay(12, 30, 33) + dur!"seconds"(-7) == TimeOfDay(12, 30, 26));

        assert(TimeOfDay(12, 30, 33) + dur!"msecs"(7000) == TimeOfDay(12, 30, 40));
        assert(TimeOfDay(12, 30, 33) + dur!"msecs"(-7000) == TimeOfDay(12, 30, 26));
        assert(TimeOfDay(12, 30, 33) + dur!"usecs"(7_000_000) == TimeOfDay(12, 30, 40));
        assert(TimeOfDay(12, 30, 33) + dur!"usecs"(-7_000_000) == TimeOfDay(12, 30, 26));
        assert(TimeOfDay(12, 30, 33) + dur!"hnsecs"(70_000_000) == TimeOfDay(12, 30, 40));
        assert(TimeOfDay(12, 30, 33) + dur!"hnsecs"(-70_000_000) == TimeOfDay(12, 30, 26));

        assert(TimeOfDay(12, 30, 33) - dur!"hours"(-7) == TimeOfDay(19, 30, 33));
        assert(TimeOfDay(12, 30, 33) - dur!"hours"(7) == TimeOfDay(5, 30, 33));
        assert(TimeOfDay(12, 30, 33) - dur!"minutes"(-7) == TimeOfDay(12, 37, 33));
        assert(TimeOfDay(12, 30, 33) - dur!"minutes"(7) == TimeOfDay(12, 23, 33));
        assert(TimeOfDay(12, 30, 33) - dur!"seconds"(-7) == TimeOfDay(12, 30, 40));
        assert(TimeOfDay(12, 30, 33) - dur!"seconds"(7) == TimeOfDay(12, 30, 26));

        assert(TimeOfDay(12, 30, 33) - dur!"msecs"(-7000) == TimeOfDay(12, 30, 40));
        assert(TimeOfDay(12, 30, 33) - dur!"msecs"(7000) == TimeOfDay(12, 30, 26));
        assert(TimeOfDay(12, 30, 33) - dur!"usecs"(-7_000_000) == TimeOfDay(12, 30, 40));
        assert(TimeOfDay(12, 30, 33) - dur!"usecs"(7_000_000) == TimeOfDay(12, 30, 26));
        assert(TimeOfDay(12, 30, 33) - dur!"hnsecs"(-70_000_000) == TimeOfDay(12, 30, 40));
        assert(TimeOfDay(12, 30, 33) - dur!"hnsecs"(70_000_000) == TimeOfDay(12, 30, 26));

        auto tod = TimeOfDay(19, 17, 22);
        (tod += dur!"seconds"(9)) += dur!"seconds"(-7292);
        assert(tod == TimeOfDay(17, 15, 59));

        const ctod = TimeOfDay(12, 33, 30);
        immutable itod = TimeOfDay(12, 33, 30);
        static assert(!__traits(compiles, ctod += duration));
        static assert(!__traits(compiles, itod += duration));
        static assert(!__traits(compiles, ctod -= duration));
        static assert(!__traits(compiles, itod -= duration));
    }


    /++
        Gives the difference between two $(LREF TimeOfDay)s.

        The legal types of arithmetic for $(LREF TimeOfDay) using this operator
        are

        $(BOOKTABLE,
        $(TR $(TD TimeOfDay) $(TD -) $(TD TimeOfDay) $(TD -->) $(TD duration))
        )

        Params:
            rhs = The $(LREF TimeOfDay) to subtract from this one.
      +/
    Duration opBinary(string op)(TimeOfDay rhs) const @safe pure nothrow @nogc
        if (op == "-")
    {
        immutable lhsSec = _hour * 3600 + _minute * 60 + _second;
        immutable rhsSec = rhs._hour * 3600 + rhs._minute * 60 + rhs._second;

        import core.time : dur;
        return dur!"seconds"(lhsSec - rhsSec);
    }

    @safe unittest
    {
        auto tod = TimeOfDay(12, 30, 33);

        import core.time : dur;
        assert(TimeOfDay(7, 12, 52) - TimeOfDay(12, 30, 33) == dur!"seconds"(-19_061));
        assert(TimeOfDay(12, 30, 33) - TimeOfDay(7, 12, 52) == dur!"seconds"(19_061));
        assert(TimeOfDay(12, 30, 33) - TimeOfDay(14, 30, 33) == dur!"seconds"(-7200));
        assert(TimeOfDay(14, 30, 33) - TimeOfDay(12, 30, 33) == dur!"seconds"(7200));
        assert(TimeOfDay(12, 30, 33) - TimeOfDay(12, 34, 33) == dur!"seconds"(-240));
        assert(TimeOfDay(12, 34, 33) - TimeOfDay(12, 30, 33) == dur!"seconds"(240));
        assert(TimeOfDay(12, 30, 33) - TimeOfDay(12, 30, 34) == dur!"seconds"(-1));
        assert(TimeOfDay(12, 30, 34) - TimeOfDay(12, 30, 33) == dur!"seconds"(1));

        const ctod = TimeOfDay(12, 30, 33);
        immutable itod = TimeOfDay(12, 30, 33);
        assert(tod - tod == Duration.zero);
        assert(ctod - tod == Duration.zero);
        assert(itod - tod == Duration.zero);

        assert(tod - ctod == Duration.zero);
        assert(ctod - ctod == Duration.zero);
        assert(itod - ctod == Duration.zero);

        assert(tod - itod == Duration.zero);
        assert(ctod - itod == Duration.zero);
        assert(itod - itod == Duration.zero);
    }


    /++
        Converts this $(LREF TimeOfDay) to a string with the format `HHMMSS`.
        If `writer` is set, the resulting string will be written directly to it.

        Params:
            writer = A `char` accepting $(REF_ALTTEXT output range, isOutputRange, std, range, primitives)
        Returns:
            A `string` when not using an output range; `void` otherwise.
      +/
    string toISOString() const @safe pure nothrow
    {
        import std.array : appender;
        auto w = appender!string();
        w.reserve(6);
        try
            toISOString(w);
        catch (Exception e)
            assert(0, "toISOString() threw.");
        return w.data;
    }

    /// ditto
    void toISOString(W)(ref W writer) const
    if (isOutputRange!(W, char))
    {
        import std.format.write : formattedWrite;
        formattedWrite(writer, "%02d%02d%02d", _hour, _minute, _second);
    }

    ///
    @safe unittest
    {
        assert(TimeOfDay(0, 0, 0).toISOString() == "000000");
        assert(TimeOfDay(12, 30, 33).toISOString() == "123033");
    }

    @safe unittest
    {
        auto tod = TimeOfDay(12, 30, 33);
        const ctod = TimeOfDay(12, 30, 33);
        immutable itod = TimeOfDay(12, 30, 33);
        assert(tod.toISOString() == "123033");
        assert(ctod.toISOString() == "123033");
        assert(itod.toISOString() == "123033");
    }


    /++
        Converts this $(LREF TimeOfDay) to a string with the format `HH:MM:SS`.
        If `writer` is set, the resulting string will be written directly to it.

        Params:
            writer = A `char` accepting $(REF_ALTTEXT output range, isOutputRange, std, range, primitives)
        Returns:
            A `string` when not using an output range; `void` otherwise.
      +/
    string toISOExtString() const @safe pure nothrow
    {
        import std.array : appender;
        auto w = appender!string();
        w.reserve(8);
        try
            toISOExtString(w);
        catch (Exception e)
            assert(0, "toISOExtString() threw.");
        return w.data;
    }

    /// ditto
    void toISOExtString(W)(ref W writer) const
    if (isOutputRange!(W, char))
    {
        import std.format.write : formattedWrite;
        formattedWrite(writer, "%02d:%02d:%02d", _hour, _minute, _second);
    }

    ///
    @safe unittest
    {
        assert(TimeOfDay(0, 0, 0).toISOExtString() == "00:00:00");
        assert(TimeOfDay(12, 30, 33).toISOExtString() == "12:30:33");
    }

    @safe unittest
    {
        auto tod = TimeOfDay(12, 30, 33);
        const ctod = TimeOfDay(12, 30, 33);
        immutable itod = TimeOfDay(12, 30, 33);
        assert(tod.toISOExtString() == "12:30:33");
        assert(ctod.toISOExtString() == "12:30:33");
        assert(itod.toISOExtString() == "12:30:33");
    }


    /++
        Converts this TimeOfDay to a string.

        This function exists to make it easy to convert a $(LREF TimeOfDay) to a
        string for code that does not care what the exact format is - just that
        it presents the information in a clear manner. It also makes it easy to
        simply convert a $(LREF TimeOfDay) to a string when using functions such
        as `to!string`, `format`, or `writeln` which use toString to convert
        user-defined types. So, it is unlikely that much code will call
        toString directly.

        The format of the string is purposefully unspecified, and code that
        cares about the format of the string should use `toISOString`,
        `toISOExtString`, or some other custom formatting function that
        explicitly generates the format that the code needs. The reason is that
        the code is then clear about what format it's using, making it less
        error-prone to maintain the code and interact with other software that
        consumes the generated strings. It's for this same reason that
        $(LREF TimeOfDay) has no `fromString` function, whereas it does have
        `fromISOString` and `fromISOExtString`.

        The format returned by toString may or may not change in the future.

        Params:
            writer = A `char` accepting $(REF_ALTTEXT output range, isOutputRange, std, range, primitives)
        Returns:
            A `string` when not using an output range; `void` otherwise.
      +/
    string toString() const @safe pure nothrow
    {
        return toISOExtString();
    }

    /// ditto
    void toString(W)(ref W writer) const
    if (isOutputRange!(W, char))
    {
        toISOExtString(writer);
    }

    @safe unittest
    {
        auto tod = TimeOfDay(12, 30, 33);
        const ctod = TimeOfDay(12, 30, 33);
        immutable itod = TimeOfDay(12, 30, 33);
        assert(tod.toString());
        assert(ctod.toString());
        assert(itod.toString());
    }


    /++
        Creates a $(LREF TimeOfDay) from a string with the format HHMMSS.
        Whitespace is stripped from the given string.

        Params:
            isoString = A string formatted in the ISO format for times.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given string is
            not in the ISO format or if the resulting $(LREF TimeOfDay) would
            not be valid.
      +/
    static TimeOfDay fromISOString(S)(scope const S isoString) @safe pure
        if (isSomeString!S)
    {
        import std.conv : to, text, ConvException;
        import std.exception : enforce;
        import std.string : strip;

        int hours, minutes, seconds;
        auto str = strip(isoString);

        enforce!DateTimeException(str.length == 6, text("Invalid ISO String: ", isoString));

        try
        {
            // cast to int from uint is used because it checks for
            // non digits without extra loops
            hours = cast(int) to!uint(str[0 .. 2]);
            minutes = cast(int) to!uint(str[2 .. 4]);
            seconds = cast(int) to!uint(str[4 .. $]);
        }
        catch (ConvException)
        {
            throw new DateTimeException(text("Invalid ISO String: ", isoString));
        }

        return TimeOfDay(hours, minutes, seconds);
    }

    ///
    @safe unittest
    {
        assert(TimeOfDay.fromISOString("000000") == TimeOfDay(0, 0, 0));
        assert(TimeOfDay.fromISOString("123033") == TimeOfDay(12, 30, 33));
        assert(TimeOfDay.fromISOString(" 123033 ") == TimeOfDay(12, 30, 33));
    }

    @safe unittest
    {
        assertThrown!DateTimeException(TimeOfDay.fromISOString(""));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("0"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("00"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("000"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("0000"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("00000"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("13033"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("1277"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("12707"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("12070"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("12303a"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("1230a3"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("123a33"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("12a033"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("1a0033"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("a20033"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("1200330"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("0120033"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("-120033"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("+120033"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("120033am"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("120033pm"));

        assertThrown!DateTimeException(TimeOfDay.fromISOString("0::"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString(":0:"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("::0"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("0:0:0"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("0:0:00"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("0:00:0"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("00:0:0"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("00:00:0"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("00:0:00"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("13:0:33"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("12:7:7"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("12:7:07"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("12:07:0"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("12:30:3a"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("12:30:a3"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("12:3a:33"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("12:a0:33"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("1a:00:33"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("a2:00:33"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("12:003:30"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("120:03:30"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("012:00:33"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("01:200:33"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("-12:00:33"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("+12:00:33"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("12:00:33am"));
        assertThrown!DateTimeException(TimeOfDay.fromISOString("12:00:33pm"));

        assertThrown!DateTimeException(TimeOfDay.fromISOString("12:00:33"));

        assert(TimeOfDay.fromISOString("011217") == TimeOfDay(1, 12, 17));
        assert(TimeOfDay.fromISOString("001412") == TimeOfDay(0, 14, 12));
        assert(TimeOfDay.fromISOString("000007") == TimeOfDay(0, 0, 7));
        assert(TimeOfDay.fromISOString("011217 ") == TimeOfDay(1, 12, 17));
        assert(TimeOfDay.fromISOString(" 011217") == TimeOfDay(1, 12, 17));
        assert(TimeOfDay.fromISOString(" 011217 ") == TimeOfDay(1, 12, 17));
    }

    // https://issues.dlang.org/show_bug.cgi?id=17801
    @safe unittest
    {
        import std.conv : to;
        import std.meta : AliasSeq;
        static foreach (C; AliasSeq!(char, wchar, dchar))
        {
            static foreach (S; AliasSeq!(C[], const(C)[], immutable(C)[]))
                assert(TimeOfDay.fromISOString(to!S("141516")) == TimeOfDay(14, 15, 16));
        }
    }


    /++
        Creates a $(LREF TimeOfDay) from a string with the format HH:MM:SS.
        Whitespace is stripped from the given string.

        Params:
            isoExtString = A string formatted in the ISO Extended format for
            times.

        Throws:
            $(REF DateTimeException,std,datetime,date) if the given string is
            not in the ISO Extended format or if the resulting $(LREF TimeOfDay)
            would not be valid.
      +/
    static TimeOfDay fromISOExtString(S)(scope const S isoExtString) @safe pure
        if (isSomeString!S)
    {
        import std.conv : ConvException, text, to;
        import std.string : strip;

        auto str = strip(isoExtString);
        int hours, minutes, seconds;

        if (str.length != 8 || str[2] != ':' || str[5] != ':')
            throw new DateTimeException(text("Invalid ISO Extended String: ", isoExtString));

        try
        {
            // cast to int from uint is used because it checks for
            // non digits without extra loops
            hours = cast(int) to!uint(str[0 .. 2]);
            minutes = cast(int) to!uint(str[3 .. 5]);
            seconds = cast(int) to!uint(str[6 .. $]);
        }
        catch (ConvException)
        {
            throw new DateTimeException(text("Invalid ISO Extended String: ", isoExtString));
        }

        return TimeOfDay(hours, minutes, seconds);
    }

    ///
    @safe unittest
    {
        assert(TimeOfDay.fromISOExtString("00:00:00") == TimeOfDay(0, 0, 0));
        assert(TimeOfDay.fromISOExtString("12:30:33") == TimeOfDay(12, 30, 33));
        assert(TimeOfDay.fromISOExtString(" 12:30:33 ") == TimeOfDay(12, 30, 33));
    }

    @safe unittest
    {
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString(""));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("0"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("00"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("000"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("0000"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("00000"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("13033"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("1277"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("12707"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("12070"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("12303a"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("1230a3"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("123a33"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("12a033"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("1a0033"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("a20033"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("1200330"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("0120033"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("-120033"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("+120033"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("120033am"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("120033pm"));

        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("0::"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString(":0:"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("::0"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("0:0:0"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("0:0:00"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("0:00:0"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("00:0:0"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("00:00:0"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("00:0:00"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("13:0:33"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("12:7:7"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("12:7:07"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("12:07:0"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("12:30:3a"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("12:30:a3"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("12:3a:33"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("12:a0:33"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("1a:00:33"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("a2:00:33"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("12:003:30"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("120:03:30"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("012:00:33"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("01:200:33"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("-12:00:33"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("+12:00:33"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("12:00:33am"));
        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("12:00:33pm"));

        assertThrown!DateTimeException(TimeOfDay.fromISOExtString("120033"));

        assert(TimeOfDay.fromISOExtString("01:12:17") == TimeOfDay(1, 12, 17));
        assert(TimeOfDay.fromISOExtString("00:14:12") == TimeOfDay(0, 14, 12));
        assert(TimeOfDay.fromISOExtString("00:00:07") == TimeOfDay(0, 0, 7));
        assert(TimeOfDay.fromISOExtString("01:12:17 ") == TimeOfDay(1, 12, 17));
        assert(TimeOfDay.fromISOExtString(" 01:12:17") == TimeOfDay(1, 12, 17));
        assert(TimeOfDay.fromISOExtString(" 01:12:17 ") == TimeOfDay(1, 12, 17));
    }

    // https://issues.dlang.org/show_bug.cgi?id=17801
    @safe unittest
    {
        import std.conv : to;
        import std.meta : AliasSeq;
        static foreach (C; AliasSeq!(char, wchar, dchar))
        {
            static foreach (S; AliasSeq!(C[], const(C)[], immutable(C)[]))
                assert(TimeOfDay.fromISOExtString(to!S("14:15:16")) == TimeOfDay(14, 15, 16));
        }
    }


    /++
        Returns midnight.
      +/
    @property static TimeOfDay min() @safe pure nothrow @nogc
    {
        return TimeOfDay.init;
    }

    @safe unittest
    {
        assert(TimeOfDay.min.hour == 0);
        assert(TimeOfDay.min.minute == 0);
        assert(TimeOfDay.min.second == 0);
        assert(TimeOfDay.min < TimeOfDay.max);
    }


    /++
        Returns one second short of midnight.
      +/
    @property static TimeOfDay max() @safe pure nothrow @nogc
    {
        auto tod = TimeOfDay.init;
        tod._hour = maxHour;
        tod._minute = maxMinute;
        tod._second = maxSecond;

        return tod;
    }

    @safe unittest
    {
        assert(TimeOfDay.max.hour == 23);
        assert(TimeOfDay.max.minute == 59);
        assert(TimeOfDay.max.second == 59);
        assert(TimeOfDay.max > TimeOfDay.min);
    }


private:

    /+
        Add seconds to the time of day. Negative values will subtract. If the
        number of seconds overflows (or underflows), then the seconds will wrap,
        increasing (or decreasing) the number of minutes accordingly. If the
        number of minutes overflows (or underflows), then the minutes will wrap.
        If the number of minutes overflows(or underflows), then the hour will
        wrap. (e.g. adding 90 seconds to 23:59:00 would result in 00:00:30).

        Params:
            seconds = The number of seconds to add to this TimeOfDay.
      +/
    ref TimeOfDay _addSeconds(long seconds) return @safe pure nothrow @nogc
    {
        import core.time : convert;
        long hnsecs = convert!("seconds", "hnsecs")(seconds);
        hnsecs += convert!("hours", "hnsecs")(_hour);
        hnsecs += convert!("minutes", "hnsecs")(_minute);
        hnsecs += convert!("seconds", "hnsecs")(_second);

        hnsecs %= convert!("days", "hnsecs")(1);

        if (hnsecs < 0)
            hnsecs += convert!("days", "hnsecs")(1);

        immutable newHours = splitUnitsFromHNSecs!"hours"(hnsecs);
        immutable newMinutes = splitUnitsFromHNSecs!"minutes"(hnsecs);
        immutable newSeconds = splitUnitsFromHNSecs!"seconds"(hnsecs);

        _hour = cast(ubyte) newHours;
        _minute = cast(ubyte) newMinutes;
        _second = cast(ubyte) newSeconds;

        return this;
    }

    @safe unittest
    {
        static void testTOD(TimeOfDay orig, int seconds, TimeOfDay expected, size_t line = __LINE__)
        {
            orig._addSeconds(seconds);
            assert(orig == expected);
        }

        testTOD(TimeOfDay(12, 30, 33), 0, TimeOfDay(12, 30, 33));
        testTOD(TimeOfDay(12, 30, 33), 1, TimeOfDay(12, 30, 34));
        testTOD(TimeOfDay(12, 30, 33), 2, TimeOfDay(12, 30, 35));
        testTOD(TimeOfDay(12, 30, 33), 3, TimeOfDay(12, 30, 36));
        testTOD(TimeOfDay(12, 30, 33), 4, TimeOfDay(12, 30, 37));
        testTOD(TimeOfDay(12, 30, 33), 5, TimeOfDay(12, 30, 38));
        testTOD(TimeOfDay(12, 30, 33), 10, TimeOfDay(12, 30, 43));
        testTOD(TimeOfDay(12, 30, 33), 15, TimeOfDay(12, 30, 48));
        testTOD(TimeOfDay(12, 30, 33), 26, TimeOfDay(12, 30, 59));
        testTOD(TimeOfDay(12, 30, 33), 27, TimeOfDay(12, 31, 0));
        testTOD(TimeOfDay(12, 30, 33), 30, TimeOfDay(12, 31, 3));
        testTOD(TimeOfDay(12, 30, 33), 59, TimeOfDay(12, 31, 32));
        testTOD(TimeOfDay(12, 30, 33), 60, TimeOfDay(12, 31, 33));
        testTOD(TimeOfDay(12, 30, 33), 61, TimeOfDay(12, 31, 34));

        testTOD(TimeOfDay(12, 30, 33), 1766, TimeOfDay(12, 59, 59));
        testTOD(TimeOfDay(12, 30, 33), 1767, TimeOfDay(13, 0, 0));
        testTOD(TimeOfDay(12, 30, 33), 1768, TimeOfDay(13, 0, 1));
        testTOD(TimeOfDay(12, 30, 33), 2007, TimeOfDay(13, 4, 0));
        testTOD(TimeOfDay(12, 30, 33), 3599, TimeOfDay(13, 30, 32));
        testTOD(TimeOfDay(12, 30, 33), 3600, TimeOfDay(13, 30, 33));
        testTOD(TimeOfDay(12, 30, 33), 3601, TimeOfDay(13, 30, 34));
        testTOD(TimeOfDay(12, 30, 33), 7200, TimeOfDay(14, 30, 33));

        testTOD(TimeOfDay(12, 30, 33), -1, TimeOfDay(12, 30, 32));
        testTOD(TimeOfDay(12, 30, 33), -2, TimeOfDay(12, 30, 31));
        testTOD(TimeOfDay(12, 30, 33), -3, TimeOfDay(12, 30, 30));
        testTOD(TimeOfDay(12, 30, 33), -4, TimeOfDay(12, 30, 29));
        testTOD(TimeOfDay(12, 30, 33), -5, TimeOfDay(12, 30, 28));
        testTOD(TimeOfDay(12, 30, 33), -10, TimeOfDay(12, 30, 23));
        testTOD(TimeOfDay(12, 30, 33), -15, TimeOfDay(12, 30, 18));
        testTOD(TimeOfDay(12, 30, 33), -33, TimeOfDay(12, 30, 0));
        testTOD(TimeOfDay(12, 30, 33), -34, TimeOfDay(12, 29, 59));
        testTOD(TimeOfDay(12, 30, 33), -35, TimeOfDay(12, 29, 58));
        testTOD(TimeOfDay(12, 30, 33), -59, TimeOfDay(12, 29, 34));
        testTOD(TimeOfDay(12, 30, 33), -60, TimeOfDay(12, 29, 33));
        testTOD(TimeOfDay(12, 30, 33), -61, TimeOfDay(12, 29, 32));

        testTOD(TimeOfDay(12, 30, 33), -1833, TimeOfDay(12, 0, 0));
        testTOD(TimeOfDay(12, 30, 33), -1834, TimeOfDay(11, 59, 59));
        testTOD(TimeOfDay(12, 30, 33), -3600, TimeOfDay(11, 30, 33));
        testTOD(TimeOfDay(12, 30, 33), -3601, TimeOfDay(11, 30, 32));
        testTOD(TimeOfDay(12, 30, 33), -5134, TimeOfDay(11, 4, 59));
        testTOD(TimeOfDay(12, 30, 33), -7200, TimeOfDay(10, 30, 33));

        testTOD(TimeOfDay(12, 30, 0), 1, TimeOfDay(12, 30, 1));
        testTOD(TimeOfDay(12, 30, 0), 0, TimeOfDay(12, 30, 0));
        testTOD(TimeOfDay(12, 30, 0), -1, TimeOfDay(12, 29, 59));

        testTOD(TimeOfDay(12, 0, 0), 1, TimeOfDay(12, 0, 1));
        testTOD(TimeOfDay(12, 0, 0), 0, TimeOfDay(12, 0, 0));
        testTOD(TimeOfDay(12, 0, 0), -1, TimeOfDay(11, 59, 59));

        testTOD(TimeOfDay(0, 0, 0), 1, TimeOfDay(0, 0, 1));
        testTOD(TimeOfDay(0, 0, 0), 0, TimeOfDay(0, 0, 0));
        testTOD(TimeOfDay(0, 0, 0), -1, TimeOfDay(23, 59, 59));

        testTOD(TimeOfDay(23, 59, 59), 1, TimeOfDay(0, 0, 0));
        testTOD(TimeOfDay(23, 59, 59), 0, TimeOfDay(23, 59, 59));
        testTOD(TimeOfDay(23, 59, 59), -1, TimeOfDay(23, 59, 58));

        const ctod = TimeOfDay(0, 0, 0);
        immutable itod = TimeOfDay(0, 0, 0);
        static assert(!__traits(compiles, ctod._addSeconds(7)));
        static assert(!__traits(compiles, itod._addSeconds(7)));
    }


    /+
        Whether the given values form a valid $(LREF TimeOfDay).
     +/
    static bool _valid(int hour, int minute, int second) @safe pure nothrow @nogc
    {
        return valid!"hours"(hour) && valid!"minutes"(minute) && valid!"seconds"(second);
    }


    @safe pure invariant()
    {
        import std.format : format;
        assert(_valid(_hour, _minute, _second),
               format("Invariant Failure: hour [%s] minute [%s] second [%s]", _hour, _minute, _second));
    }


package:

    ubyte _hour;
    ubyte _minute;
    ubyte _second;

    enum ubyte maxHour   = 24 - 1;
    enum ubyte maxMinute = 60 - 1;
    enum ubyte maxSecond = 60 - 1;
}

///
@safe pure unittest
{
    import core.time : minutes, seconds;

    auto t = TimeOfDay(12, 30, 0);

    t += 10.minutes + 100.seconds;
    assert(t == TimeOfDay(12, 41, 40));

    assert(t.toISOExtString() == "12:41:40");
    assert(t.toISOString() == "124140");

    assert(TimeOfDay.fromISOExtString("15:00:00") == TimeOfDay(15, 0, 0));
    assert(TimeOfDay.fromISOString("015000") == TimeOfDay(1, 50, 0));
}

/++
    Returns whether the given value is valid for the given unit type when in a
    time point. Naturally, a duration is not held to a particular range, but
    the values in a time point are (e.g. a month must be in the range of
    1 - 12 inclusive).

    Params:
        units = The units of time to validate.
        value = The number to validate.
  +/
bool valid(string units)(int value) @safe pure nothrow @nogc
if (units == "months" ||
    units == "hours" ||
    units == "minutes" ||
    units == "seconds")
{
    static if (units == "months")
        return value >= Month.jan && value <= Month.dec;
    else static if (units == "hours")
        return value >= 0 && value <= 23;
    else static if (units == "minutes")
        return value >= 0 && value <= 59;
    else static if (units == "seconds")
        return value >= 0 && value <= 59;
}

///
@safe unittest
{
    assert(valid!"hours"(12));
    assert(!valid!"hours"(32));
    assert(valid!"months"(12));
    assert(!valid!"months"(13));
}

/++
    Returns whether the given day is valid for the given year and month.

    Params:
        units = The units of time to validate.
        year  = The year of the day to validate.
        month = The month of the day to validate (January is 1).
        day   = The day to validate.
  +/
bool valid(string units)(int year, int month, int day) @safe pure nothrow @nogc
if (units == "days")
{
    return day > 0 && day <= maxDay(year, month);
}

///
@safe pure nothrow @nogc unittest
{
    assert(valid!"days"(2016, 2, 29));
    assert(!valid!"days"(2016, 2, 30));
    assert(valid!"days"(2017, 2, 20));
    assert(!valid!"days"(2017, 2, 29));
}


/++
    Params:
        units = The units of time to validate.
        value = The number to validate.
        file  = The file that the $(LREF DateTimeException) will list if thrown.
        line  = The line number that the $(LREF DateTimeException) will list if
                thrown.

    Throws:
        $(LREF DateTimeException) if `valid!units(value)` is false.
  +/
void enforceValid(string units)(int value, string file = __FILE__, size_t line = __LINE__) @safe pure
if (units == "months" ||
    units == "hours" ||
    units == "minutes" ||
    units == "seconds")
{
    import std.format : format;

    static if (units == "months")
    {
        if (!valid!units(value))
            throw new DateTimeException(format("%s is not a valid month of the year.", value), file, line);
    }
    else static if (units == "hours")
    {
        if (!valid!units(value))
            throw new DateTimeException(format("%s is not a valid hour of the day.", value), file, line);
    }
    else static if (units == "minutes")
    {
        if (!valid!units(value))
            throw new DateTimeException(format("%s is not a valid minute of an hour.", value), file, line);
    }
    else static if (units == "seconds")
    {
        if (!valid!units(value))
            throw new DateTimeException(format("%s is not a valid second of a minute.", value), file, line);
    }
}

///
@safe pure unittest
{
    import std.exception : assertThrown, assertNotThrown;

    assertNotThrown(enforceValid!"months"(10));
    assertNotThrown(enforceValid!"seconds"(40));

    assertThrown!DateTimeException(enforceValid!"months"(0));
    assertThrown!DateTimeException(enforceValid!"hours"(24));
    assertThrown!DateTimeException(enforceValid!"minutes"(60));
    assertThrown!DateTimeException(enforceValid!"seconds"(60));
}


/++
    Because the validity of the day number depends on both on the year
    and month of which the day is occurring, take all three variables
    to validate the day.

    Params:
        units = The units of time to validate.
        year  = The year of the day to validate.
        month = The month of the day to validate.
        day   = The day to validate.
        file  = The file that the $(LREF DateTimeException) will list if thrown.
        line  = The line number that the $(LREF DateTimeException) will list if
                thrown.

    Throws:
        $(LREF DateTimeException) if $(D valid!"days"(year, month, day)) is false.
  +/
void enforceValid(string units)
                 (int year, Month month, int day, string file = __FILE__, size_t line = __LINE__) @safe pure
if (units == "days")
{
    import std.format : format;
    if (!valid!"days"(year, month, day))
        throw new DateTimeException(format("%s is not a valid day in %s in %s", day, month, year), file, line);
}

///
@safe pure unittest
{
    import std.exception : assertThrown, assertNotThrown;

    assertNotThrown(enforceValid!"days"(2000, Month.jan, 1));
    // leap year
    assertNotThrown(enforceValid!"days"(2000, Month.feb, 29));

    assertThrown!DateTimeException(enforceValid!"days"(2001, Month.feb, 29));
    assertThrown!DateTimeException(enforceValid!"days"(2000, Month.jan, 32));
    assertThrown!DateTimeException(enforceValid!"days"(2000, Month.apr, 31));
}


/++
    Returns the number of days from the current day of the week to the given
    day of the week. If they are the same, then the result is 0.

    Params:
        currDoW = The current day of the week.
        dow     = The day of the week to get the number of days to.
  +/
int daysToDayOfWeek(DayOfWeek currDoW, DayOfWeek dow) @safe pure nothrow @nogc
{
    if (currDoW == dow)
        return 0;
    if (currDoW < dow)
        return dow - currDoW;
    return DayOfWeek.sat - currDoW + dow + 1;
}

///
@safe pure nothrow @nogc unittest
{
    assert(daysToDayOfWeek(DayOfWeek.mon, DayOfWeek.mon) == 0);
    assert(daysToDayOfWeek(DayOfWeek.mon, DayOfWeek.sun) == 6);
    assert(daysToDayOfWeek(DayOfWeek.mon, DayOfWeek.wed) == 2);
}

@safe unittest
{
    assert(daysToDayOfWeek(DayOfWeek.sun, DayOfWeek.sun) == 0);
    assert(daysToDayOfWeek(DayOfWeek.sun, DayOfWeek.mon) == 1);
    assert(daysToDayOfWeek(DayOfWeek.sun, DayOfWeek.tue) == 2);
    assert(daysToDayOfWeek(DayOfWeek.sun, DayOfWeek.wed) == 3);
    assert(daysToDayOfWeek(DayOfWeek.sun, DayOfWeek.thu) == 4);
    assert(daysToDayOfWeek(DayOfWeek.sun, DayOfWeek.fri) == 5);
    assert(daysToDayOfWeek(DayOfWeek.sun, DayOfWeek.sat) == 6);

    assert(daysToDayOfWeek(DayOfWeek.mon, DayOfWeek.sun) == 6);
    assert(daysToDayOfWeek(DayOfWeek.mon, DayOfWeek.mon) == 0);
    assert(daysToDayOfWeek(DayOfWeek.mon, DayOfWeek.tue) == 1);
    assert(daysToDayOfWeek(DayOfWeek.mon, DayOfWeek.wed) == 2);
    assert(daysToDayOfWeek(DayOfWeek.mon, DayOfWeek.thu) == 3);
    assert(daysToDayOfWeek(DayOfWeek.mon, DayOfWeek.fri) == 4);
    assert(daysToDayOfWeek(DayOfWeek.mon, DayOfWeek.sat) == 5);

    assert(daysToDayOfWeek(DayOfWeek.tue, DayOfWeek.sun) == 5);
    assert(daysToDayOfWeek(DayOfWeek.tue, DayOfWeek.mon) == 6);
    assert(daysToDayOfWeek(DayOfWeek.tue, DayOfWeek.tue) == 0);
    assert(daysToDayOfWeek(DayOfWeek.tue, DayOfWeek.wed) == 1);
    assert(daysToDayOfWeek(DayOfWeek.tue, DayOfWeek.thu) == 2);
    assert(daysToDayOfWeek(DayOfWeek.tue, DayOfWeek.fri) == 3);
    assert(daysToDayOfWeek(DayOfWeek.tue, DayOfWeek.sat) == 4);

    assert(daysToDayOfWeek(DayOfWeek.wed, DayOfWeek.sun) == 4);
    assert(daysToDayOfWeek(DayOfWeek.wed, DayOfWeek.mon) == 5);
    assert(daysToDayOfWeek(DayOfWeek.wed, DayOfWeek.tue) == 6);
    assert(daysToDayOfWeek(DayOfWeek.wed, DayOfWeek.wed) == 0);
    assert(daysToDayOfWeek(DayOfWeek.wed, DayOfWeek.thu) == 1);
    assert(daysToDayOfWeek(DayOfWeek.wed, DayOfWeek.fri) == 2);
    assert(daysToDayOfWeek(DayOfWeek.wed, DayOfWeek.sat) == 3);

    assert(daysToDayOfWeek(DayOfWeek.thu, DayOfWeek.sun) == 3);
    assert(daysToDayOfWeek(DayOfWeek.thu, DayOfWeek.mon) == 4);
    assert(daysToDayOfWeek(DayOfWeek.thu, DayOfWeek.tue) == 5);
    assert(daysToDayOfWeek(DayOfWeek.thu, DayOfWeek.wed) == 6);
    assert(daysToDayOfWeek(DayOfWeek.thu, DayOfWeek.thu) == 0);
    assert(daysToDayOfWeek(DayOfWeek.thu, DayOfWeek.fri) == 1);
    assert(daysToDayOfWeek(DayOfWeek.thu, DayOfWeek.sat) == 2);

    assert(daysToDayOfWeek(DayOfWeek.fri, DayOfWeek.sun) == 2);
    assert(daysToDayOfWeek(DayOfWeek.fri, DayOfWeek.mon) == 3);
    assert(daysToDayOfWeek(DayOfWeek.fri, DayOfWeek.tue) == 4);
    assert(daysToDayOfWeek(DayOfWeek.fri, DayOfWeek.wed) == 5);
    assert(daysToDayOfWeek(DayOfWeek.fri, DayOfWeek.thu) == 6);
    assert(daysToDayOfWeek(DayOfWeek.fri, DayOfWeek.fri) == 0);
    assert(daysToDayOfWeek(DayOfWeek.fri, DayOfWeek.sat) == 1);

    assert(daysToDayOfWeek(DayOfWeek.sat, DayOfWeek.sun) == 1);
    assert(daysToDayOfWeek(DayOfWeek.sat, DayOfWeek.mon) == 2);
    assert(daysToDayOfWeek(DayOfWeek.sat, DayOfWeek.tue) == 3);
    assert(daysToDayOfWeek(DayOfWeek.sat, DayOfWeek.wed) == 4);
    assert(daysToDayOfWeek(DayOfWeek.sat, DayOfWeek.thu) == 5);
    assert(daysToDayOfWeek(DayOfWeek.sat, DayOfWeek.fri) == 6);
    assert(daysToDayOfWeek(DayOfWeek.sat, DayOfWeek.sat) == 0);
}


/++
    Returns the number of months from the current months of the year to the
    given month of the year. If they are the same, then the result is 0.

    Params:
        currMonth = The current month of the year.
        month     = The month of the year to get the number of months to.
  +/
int monthsToMonth(int currMonth, int month) @safe pure
{
    enforceValid!"months"(currMonth);
    enforceValid!"months"(month);

    if (currMonth == month)
        return 0;
    if (currMonth < month)
        return month - currMonth;
    return Month.dec - currMonth + month;
}

///
@safe pure unittest
{
    assert(monthsToMonth(Month.jan, Month.jan) == 0);
    assert(monthsToMonth(Month.jan, Month.dec) == 11);
    assert(monthsToMonth(Month.jul, Month.oct) == 3);
}

@safe unittest
{
    assert(monthsToMonth(Month.jan, Month.jan) == 0);
    assert(monthsToMonth(Month.jan, Month.feb) == 1);
    assert(monthsToMonth(Month.jan, Month.mar) == 2);
    assert(monthsToMonth(Month.jan, Month.apr) == 3);
    assert(monthsToMonth(Month.jan, Month.may) == 4);
    assert(monthsToMonth(Month.jan, Month.jun) == 5);
    assert(monthsToMonth(Month.jan, Month.jul) == 6);
    assert(monthsToMonth(Month.jan, Month.aug) == 7);
    assert(monthsToMonth(Month.jan, Month.sep) == 8);
    assert(monthsToMonth(Month.jan, Month.oct) == 9);
    assert(monthsToMonth(Month.jan, Month.nov) == 10);
    assert(monthsToMonth(Month.jan, Month.dec) == 11);

    assert(monthsToMonth(Month.may, Month.jan) == 8);
    assert(monthsToMonth(Month.may, Month.feb) == 9);
    assert(monthsToMonth(Month.may, Month.mar) == 10);
    assert(monthsToMonth(Month.may, Month.apr) == 11);
    assert(monthsToMonth(Month.may, Month.may) == 0);
    assert(monthsToMonth(Month.may, Month.jun) == 1);
    assert(monthsToMonth(Month.may, Month.jul) == 2);
    assert(monthsToMonth(Month.may, Month.aug) == 3);
    assert(monthsToMonth(Month.may, Month.sep) == 4);
    assert(monthsToMonth(Month.may, Month.oct) == 5);
    assert(monthsToMonth(Month.may, Month.nov) == 6);
    assert(monthsToMonth(Month.may, Month.dec) == 7);

    assert(monthsToMonth(Month.oct, Month.jan) == 3);
    assert(monthsToMonth(Month.oct, Month.feb) == 4);
    assert(monthsToMonth(Month.oct, Month.mar) == 5);
    assert(monthsToMonth(Month.oct, Month.apr) == 6);
    assert(monthsToMonth(Month.oct, Month.may) == 7);
    assert(monthsToMonth(Month.oct, Month.jun) == 8);
    assert(monthsToMonth(Month.oct, Month.jul) == 9);
    assert(monthsToMonth(Month.oct, Month.aug) == 10);
    assert(monthsToMonth(Month.oct, Month.sep) == 11);
    assert(monthsToMonth(Month.oct, Month.oct) == 0);
    assert(monthsToMonth(Month.oct, Month.nov) == 1);
    assert(monthsToMonth(Month.oct, Month.dec) == 2);

    assert(monthsToMonth(Month.dec, Month.jan) == 1);
    assert(monthsToMonth(Month.dec, Month.feb) == 2);
    assert(monthsToMonth(Month.dec, Month.mar) == 3);
    assert(monthsToMonth(Month.dec, Month.apr) == 4);
    assert(monthsToMonth(Month.dec, Month.may) == 5);
    assert(monthsToMonth(Month.dec, Month.jun) == 6);
    assert(monthsToMonth(Month.dec, Month.jul) == 7);
    assert(monthsToMonth(Month.dec, Month.aug) == 8);
    assert(monthsToMonth(Month.dec, Month.sep) == 9);
    assert(monthsToMonth(Month.dec, Month.oct) == 10);
    assert(monthsToMonth(Month.dec, Month.nov) == 11);
    assert(monthsToMonth(Month.dec, Month.dec) == 0);
}


/++
    Whether the given Gregorian Year is a leap year.

    Params:
        year = The year to to be tested.
 +/
bool yearIsLeapYear(int year) @safe pure nothrow @nogc
{
    if (year % 400 == 0)
        return true;
    if (year % 100 == 0)
        return false;
    return year % 4 == 0;
}

///
@safe unittest
{
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
    import std.format : format;
    foreach (year; [1, 2, 3, 5, 6, 7, 100, 200, 300, 500, 600, 700, 1998, 1999,
                    2001, 2002, 2003, 2005, 2006, 2007, 2009, 2010, 2011])
    {
        assert(!yearIsLeapYear(year), format("year: %s.", year));
        assert(!yearIsLeapYear(-year), format("year: %s.", year));
    }

    foreach (year; [0, 4, 8, 400, 800, 1600, 1996, 2000, 2004, 2008, 2012])
    {
        assert(yearIsLeapYear(year), format("year: %s.", year));
        assert(yearIsLeapYear(-year), format("year: %s.", year));
    }
}


/++
    Whether the given type defines all of the necessary functions for it to
    function as a time point.

    1. `T` must define a static property named `min` which is the smallest
       value of `T` as `Unqual!T`.

    2. `T` must define a static property named `max` which is the largest
       value of `T` as `Unqual!T`.

    3. `T` must define an `opBinary` for addition and subtraction that
       accepts $(REF Duration, core,time) and returns `Unqual!T`.

    4. `T` must define an `opOpAssign` for addition and subtraction that
       accepts $(REF Duration, core,time) and returns $(D ref Unqual!T).

    5. `T` must define a `opBinary` for subtraction which accepts `T`
       and returns $(REF Duration, core,time).
  +/
template isTimePoint(T)
{
    import core.time : Duration;
    import std.traits : FunctionAttribute, functionAttributes, Unqual;

    enum isTimePoint = hasMin &&
                       hasMax &&
                       hasOverloadedOpBinaryWithDuration &&
                       hasOverloadedOpAssignWithDuration &&
                       hasOverloadedOpBinaryWithSelf &&
                       !is(U == Duration);

private:

    alias U = Unqual!T;

    enum hasMin = __traits(hasMember, T, "min") &&
                  is(typeof(T.min) == U) &&
                  is(typeof({static assert(__traits(isStaticFunction, T.min));}));

    enum hasMax = __traits(hasMember, T, "max") &&
                  is(typeof(T.max) == U) &&
                  is(typeof({static assert(__traits(isStaticFunction, T.max));}));

    enum hasOverloadedOpBinaryWithDuration = is(typeof(T.init + Duration.init) == U) &&
                                             is(typeof(T.init - Duration.init) == U);

    enum hasOverloadedOpAssignWithDuration = is(typeof(U.init += Duration.init) == U) &&
                                             is(typeof(U.init -= Duration.init) == U) &&
                                             is(typeof(
                                             {
                                                 alias add = U.opOpAssign!"+";
                                                 alias sub = U.opOpAssign!"-";
                                                 alias FA = FunctionAttribute;
                                                 static assert((functionAttributes!add & FA.ref_) != 0);
                                                 static assert((functionAttributes!sub & FA.ref_) != 0);
                                             }));

    enum hasOverloadedOpBinaryWithSelf = is(typeof(T.init - T.init) == Duration);
}

///
@safe unittest
{
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

@safe unittest
{
    import core.time;
    import std.datetime.interval;
    import std.datetime.systime;
    import std.meta : AliasSeq;

    static foreach (TP; AliasSeq!(Date, DateTime, SysTime, TimeOfDay))
    {
        static assert(isTimePoint!(const TP), TP.stringof);
        static assert(isTimePoint!(immutable TP), TP.stringof);
    }

    static foreach (T; AliasSeq!(float, string, Duration, Interval!Date, PosInfInterval!Date, NegInfInterval!Date))
        static assert(!isTimePoint!T, T.stringof);
}


/++
    Whether all of the given strings are valid units of time.

    `"nsecs"` is not considered a valid unit of time. Nothing in std.datetime
    can handle precision greater than hnsecs, and the few functions in core.time
    which deal with "nsecs" deal with it explicitly.
  +/
bool validTimeUnits(string[] units...) @safe pure nothrow @nogc
{
    import std.algorithm.searching : canFind;
    foreach (str; units)
    {
        if (!canFind(timeStrings[], str))
            return false;
    }
    return true;
}

///
@safe @nogc nothrow unittest
{
    assert(validTimeUnits("msecs", "seconds", "minutes"));
    assert(validTimeUnits("days", "weeks", "months"));
    assert(!validTimeUnits("ms", "seconds", "minutes"));
}


/++
    Compares two time unit strings. `"years"` are the largest units and
    `"hnsecs"` are the smallest.

    Returns:
        $(BOOKTABLE,
        $(TR $(TD this &lt; rhs) $(TD &lt; 0))
        $(TR $(TD this == rhs) $(TD 0))
        $(TR $(TD this &gt; rhs) $(TD &gt; 0))
        )

    Throws:
        $(LREF DateTimeException) if either of the given strings is not a valid
        time unit string.
 +/
int cmpTimeUnits(string lhs, string rhs) @safe pure
{
    import std.algorithm.searching : countUntil;
    import std.exception : enforce;
    import std.format : format;

    immutable indexOfLHS = countUntil(timeStrings, lhs);
    immutable indexOfRHS = countUntil(timeStrings, rhs);

    enforce!DateTimeException(indexOfLHS != -1, format("%s is not a valid TimeString", lhs));
    enforce!DateTimeException(indexOfRHS != -1, format("%s is not a valid TimeString", rhs));

    if (indexOfLHS < indexOfRHS)
        return -1;
    if (indexOfLHS > indexOfRHS)
        return 1;

    return 0;
}

///
@safe pure unittest
{
    import std.exception : assertThrown;

    assert(cmpTimeUnits("hours", "hours") == 0);
    assert(cmpTimeUnits("hours", "weeks") < 0);
    assert(cmpTimeUnits("months", "seconds") > 0);

    assertThrown!DateTimeException(cmpTimeUnits("month", "second"));
}

@safe unittest
{
    foreach (i, outerUnits; timeStrings)
    {
        assert(cmpTimeUnits(outerUnits, outerUnits) == 0);

        // For some reason, $ won't compile.
        foreach (innerUnits; timeStrings[i + 1 .. timeStrings.length])
            assert(cmpTimeUnits(outerUnits, innerUnits) == -1);
    }

    foreach (i, outerUnits; timeStrings)
    {
        foreach (innerUnits; timeStrings[0 .. i])
            assert(cmpTimeUnits(outerUnits, innerUnits) == 1);
    }
}


/++
    Compares two time unit strings at compile time. `"years"` are the largest
    units and `"hnsecs"` are the smallest.

    This template is used instead of `cmpTimeUnits` because exceptions
    can't be thrown at compile time and `cmpTimeUnits` must enforce that
    the strings it's given are valid time unit strings. This template uses a
    template constraint instead.

    Returns:
        $(BOOKTABLE,
        $(TR $(TD this &lt; rhs) $(TD &lt; 0))
        $(TR $(TD this == rhs) $(TD 0))
        $(TR $(TD this &gt; rhs) $(TD &gt; 0))
        )
 +/
template CmpTimeUnits(string lhs, string rhs)
if (validTimeUnits(lhs, rhs))
{
    enum CmpTimeUnits = cmpTimeUnitsCTFE(lhs, rhs);
}

///
@safe pure unittest
{
    static assert(CmpTimeUnits!("years", "weeks") > 0);
    static assert(CmpTimeUnits!("days", "days") == 0);
    static assert(CmpTimeUnits!("seconds", "hours") < 0);
}

// Helper function for CmpTimeUnits.
private int cmpTimeUnitsCTFE(string lhs, string rhs) @safe pure nothrow @nogc
{
    import std.algorithm.searching : countUntil;
    auto tstrings = timeStrings;
    immutable indexOfLHS = countUntil(tstrings, lhs);
    immutable indexOfRHS = countUntil(tstrings, rhs);

    if (indexOfLHS < indexOfRHS)
        return -1;
    if (indexOfLHS > indexOfRHS)
        return 1;

    return 0;
}

@safe unittest
{
    static foreach (i; 0 .. timeStrings.length)
    {
        static assert(CmpTimeUnits!(timeStrings[i], timeStrings[i]) == 0);

        static foreach (next; timeStrings[i + 1 .. $])
            static assert(CmpTimeUnits!(timeStrings[i], next) == -1);

        static foreach (prev; timeStrings[0 .. i])
            static assert(CmpTimeUnits!(timeStrings[i], prev) == 1);
    }
}


package:


/+
    Array of the short (three letter) names of each month.
  +/
immutable string[12] _monthNames = ["Jan",
                                    "Feb",
                                    "Mar",
                                    "Apr",
                                    "May",
                                    "Jun",
                                    "Jul",
                                    "Aug",
                                    "Sep",
                                    "Oct",
                                    "Nov",
                                    "Dec"];

/+
    The maximum valid Day in the given month in the given year.

    Params:
        year  = The year to get the day for.
        month = The month of the Gregorian Calendar to get the day for.
 +/
ubyte maxDay(int year, int month) @safe pure nothrow @nogc
in
{
    assert(valid!"months"(month));
}
do
{
    switch (month)
    {
        case Month.jan, Month.mar, Month.may, Month.jul, Month.aug, Month.oct, Month.dec:
            return 31;
        case Month.feb:
            return yearIsLeapYear(year) ? 29 : 28;
        case Month.apr, Month.jun, Month.sep, Month.nov:
            return 30;
        default:
            assert(0, "Invalid month.");
    }
}

@safe unittest
{
    // Test A.D.
    assert(maxDay(1999, 1) == 31);
    assert(maxDay(1999, 2) == 28);
    assert(maxDay(1999, 3) == 31);
    assert(maxDay(1999, 4) == 30);
    assert(maxDay(1999, 5) == 31);
    assert(maxDay(1999, 6) == 30);
    assert(maxDay(1999, 7) == 31);
    assert(maxDay(1999, 8) == 31);
    assert(maxDay(1999, 9) == 30);
    assert(maxDay(1999, 10) == 31);
    assert(maxDay(1999, 11) == 30);
    assert(maxDay(1999, 12) == 31);

    assert(maxDay(2000, 1) == 31);
    assert(maxDay(2000, 2) == 29);
    assert(maxDay(2000, 3) == 31);
    assert(maxDay(2000, 4) == 30);
    assert(maxDay(2000, 5) == 31);
    assert(maxDay(2000, 6) == 30);
    assert(maxDay(2000, 7) == 31);
    assert(maxDay(2000, 8) == 31);
    assert(maxDay(2000, 9) == 30);
    assert(maxDay(2000, 10) == 31);
    assert(maxDay(2000, 11) == 30);
    assert(maxDay(2000, 12) == 31);

    // Test B.C.
    assert(maxDay(-1999, 1) == 31);
    assert(maxDay(-1999, 2) == 28);
    assert(maxDay(-1999, 3) == 31);
    assert(maxDay(-1999, 4) == 30);
    assert(maxDay(-1999, 5) == 31);
    assert(maxDay(-1999, 6) == 30);
    assert(maxDay(-1999, 7) == 31);
    assert(maxDay(-1999, 8) == 31);
    assert(maxDay(-1999, 9) == 30);
    assert(maxDay(-1999, 10) == 31);
    assert(maxDay(-1999, 11) == 30);
    assert(maxDay(-1999, 12) == 31);

    assert(maxDay(-2000, 1) == 31);
    assert(maxDay(-2000, 2) == 29);
    assert(maxDay(-2000, 3) == 31);
    assert(maxDay(-2000, 4) == 30);
    assert(maxDay(-2000, 5) == 31);
    assert(maxDay(-2000, 6) == 30);
    assert(maxDay(-2000, 7) == 31);
    assert(maxDay(-2000, 8) == 31);
    assert(maxDay(-2000, 9) == 30);
    assert(maxDay(-2000, 10) == 31);
    assert(maxDay(-2000, 11) == 30);
    assert(maxDay(-2000, 12) == 31);
}

/+
    Splits out a particular unit from hnsecs and gives the value for that
    unit and the remaining hnsecs. It really shouldn't be used unless unless
    all units larger than the given units have already been split out.

    Params:
        units  = The units to split out.
        hnsecs = The current total hnsecs. Upon returning, it is the hnsecs left
                 after splitting out the given units.

    Returns:
        The number of the given units from converting hnsecs to those units.
  +/
long splitUnitsFromHNSecs(string units)(ref long hnsecs) @safe pure nothrow @nogc
if (validTimeUnits(units) && CmpTimeUnits!(units, "months") < 0)
{
    import core.time : convert;
    immutable value = convert!("hnsecs", units)(hnsecs);
    hnsecs -= convert!(units, "hnsecs")(value);
    return value;
}

@safe unittest
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
    Returns the day of the week for the given day of the Gregorian Calendar.

    Params:
        day = The day of the Gregorian Calendar for which to get the day of
              the week.
  +/
DayOfWeek getDayOfWeek(int day) @safe pure nothrow @nogc
{
    // January 1st, 1 A.D. was a Monday
    if (day >= 0)
        return cast(DayOfWeek)(day % 7);
    else
    {
        immutable dow = cast(DayOfWeek)((day % 7) + 7);

        if (dow == 7)
            return DayOfWeek.sun;
        else
            return dow;
    }
}

@safe unittest
{
    import std.datetime.systime : SysTime;

    // Test A.D.
    assert(getDayOfWeek(SysTime(Date(1, 1, 1)).dayOfGregorianCal) == DayOfWeek.mon);
    assert(getDayOfWeek(SysTime(Date(1, 1, 2)).dayOfGregorianCal) == DayOfWeek.tue);
    assert(getDayOfWeek(SysTime(Date(1, 1, 3)).dayOfGregorianCal) == DayOfWeek.wed);
    assert(getDayOfWeek(SysTime(Date(1, 1, 4)).dayOfGregorianCal) == DayOfWeek.thu);
    assert(getDayOfWeek(SysTime(Date(1, 1, 5)).dayOfGregorianCal) == DayOfWeek.fri);
    assert(getDayOfWeek(SysTime(Date(1, 1, 6)).dayOfGregorianCal) == DayOfWeek.sat);
    assert(getDayOfWeek(SysTime(Date(1, 1, 7)).dayOfGregorianCal) == DayOfWeek.sun);
    assert(getDayOfWeek(SysTime(Date(1, 1, 8)).dayOfGregorianCal) == DayOfWeek.mon);
    assert(getDayOfWeek(SysTime(Date(1, 1, 9)).dayOfGregorianCal) == DayOfWeek.tue);
    assert(getDayOfWeek(SysTime(Date(2, 1, 1)).dayOfGregorianCal) == DayOfWeek.tue);
    assert(getDayOfWeek(SysTime(Date(3, 1, 1)).dayOfGregorianCal) == DayOfWeek.wed);
    assert(getDayOfWeek(SysTime(Date(4, 1, 1)).dayOfGregorianCal) == DayOfWeek.thu);
    assert(getDayOfWeek(SysTime(Date(5, 1, 1)).dayOfGregorianCal) == DayOfWeek.sat);
    assert(getDayOfWeek(SysTime(Date(2000, 1, 1)).dayOfGregorianCal) == DayOfWeek.sat);
    assert(getDayOfWeek(SysTime(Date(2010, 8, 22)).dayOfGregorianCal) == DayOfWeek.sun);
    assert(getDayOfWeek(SysTime(Date(2010, 8, 23)).dayOfGregorianCal) == DayOfWeek.mon);
    assert(getDayOfWeek(SysTime(Date(2010, 8, 24)).dayOfGregorianCal) == DayOfWeek.tue);
    assert(getDayOfWeek(SysTime(Date(2010, 8, 25)).dayOfGregorianCal) == DayOfWeek.wed);
    assert(getDayOfWeek(SysTime(Date(2010, 8, 26)).dayOfGregorianCal) == DayOfWeek.thu);
    assert(getDayOfWeek(SysTime(Date(2010, 8, 27)).dayOfGregorianCal) == DayOfWeek.fri);
    assert(getDayOfWeek(SysTime(Date(2010, 8, 28)).dayOfGregorianCal) == DayOfWeek.sat);
    assert(getDayOfWeek(SysTime(Date(2010, 8, 29)).dayOfGregorianCal) == DayOfWeek.sun);

    // Test B.C.
    assert(getDayOfWeek(SysTime(Date(0, 12, 31)).dayOfGregorianCal) == DayOfWeek.sun);
    assert(getDayOfWeek(SysTime(Date(0, 12, 30)).dayOfGregorianCal) == DayOfWeek.sat);
    assert(getDayOfWeek(SysTime(Date(0, 12, 29)).dayOfGregorianCal) == DayOfWeek.fri);
    assert(getDayOfWeek(SysTime(Date(0, 12, 28)).dayOfGregorianCal) == DayOfWeek.thu);
    assert(getDayOfWeek(SysTime(Date(0, 12, 27)).dayOfGregorianCal) == DayOfWeek.wed);
    assert(getDayOfWeek(SysTime(Date(0, 12, 26)).dayOfGregorianCal) == DayOfWeek.tue);
    assert(getDayOfWeek(SysTime(Date(0, 12, 25)).dayOfGregorianCal) == DayOfWeek.mon);
    assert(getDayOfWeek(SysTime(Date(0, 12, 24)).dayOfGregorianCal) == DayOfWeek.sun);
    assert(getDayOfWeek(SysTime(Date(0, 12, 23)).dayOfGregorianCal) == DayOfWeek.sat);
}


private:

enum daysInYear     = 365;  // The number of days in a non-leap year.
enum daysInLeapYear = 366;  // The numbef or days in a leap year.
enum daysIn4Years   = daysInYear * 3 + daysInLeapYear;  // Number of days in 4 years.
enum daysIn100Years = daysIn4Years * 25 - 1;  // The number of days in 100 years.
enum daysIn400Years = daysIn100Years * 4 + 1; // The number of days in 400 years.

/+
    Array of integers representing the last days of each month in a year.
  +/
immutable int[13] lastDayNonLeap = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365];

/+
    Array of integers representing the last days of each month in a leap year.
  +/
immutable int[13] lastDayLeap = [0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366];


/+
    Returns the string representation of the given month.
  +/
string monthToString(Month month) @safe pure
{
    import std.format : format;
    assert(month >= Month.jan && month <= Month.dec, format("Invalid month: %s", month));
    return _monthNames[month - Month.jan];
}

@safe unittest
{
    assert(monthToString(Month.jan) == "Jan");
    assert(monthToString(Month.feb) == "Feb");
    assert(monthToString(Month.mar) == "Mar");
    assert(monthToString(Month.apr) == "Apr");
    assert(monthToString(Month.may) == "May");
    assert(monthToString(Month.jun) == "Jun");
    assert(monthToString(Month.jul) == "Jul");
    assert(monthToString(Month.aug) == "Aug");
    assert(monthToString(Month.sep) == "Sep");
    assert(monthToString(Month.oct) == "Oct");
    assert(monthToString(Month.nov) == "Nov");
    assert(monthToString(Month.dec) == "Dec");
}


/+
    Returns the Month corresponding to the given string.

    Params:
        monthStr = The string representation of the month to get the Month for.

    Throws:
        $(REF DateTimeException,std,datetime,date) if the given month is not a
        valid month string.
  +/
Month monthFromString(T)(T monthStr) @safe pure
if (isSomeString!T)
{
    import std.format : format;
    switch (monthStr)
    {
        case "Jan":
            return Month.jan;
        case "Feb":
            return Month.feb;
        case "Mar":
            return Month.mar;
        case "Apr":
            return Month.apr;
        case "May":
            return Month.may;
        case "Jun":
            return Month.jun;
        case "Jul":
            return Month.jul;
        case "Aug":
            return Month.aug;
        case "Sep":
            return Month.sep;
        case "Oct":
            return Month.oct;
        case "Nov":
            return Month.nov;
        case "Dec":
            return Month.dec;
        default:
            throw new DateTimeException(format!"Invalid month %s"(monthStr));
    }
}

@safe unittest
{
    import std.conv : to;
    import std.traits : EnumMembers;
    foreach (badStr; ["Ja", "Janu", "Januar", "Januarys", "JJanuary", "JANUARY",
                      "JAN", "january", "jaNuary", "jaN", "jaNuaRy", "jAn"])
    {
        assertThrown!DateTimeException(monthFromString(badStr), badStr);
    }

    foreach (month; EnumMembers!Month)
    {
        assert(monthFromString(monthToString(month)) == month, month.to!string);
    }
}


// NOTE: all the non-simple array literals are wrapped in functions, because
// otherwise importing causes re-evaluation of the static initializers using
// CTFE with unittests enabled
version (StdUnittest)
{
private @safe:
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
        static MonthDay[] result = [MonthDay(1, 1),
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
    }
}

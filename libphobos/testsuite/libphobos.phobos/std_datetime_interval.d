@system unittest
{
    import std.datetime.interval;

    import std.datetime.date : Date, DayOfWeek;

    auto interval = Interval!Date(Date(2010, 9, 2), Date(2010, 9, 27));
    auto func = everyDayOfWeek!Date(DayOfWeek.mon);
    auto range = interval.fwdRange(func);

    // A Thursday. Using PopFirst.yes would have made this Date(2010, 9, 6).
    assert(range.front == Date(2010, 9, 2));

    range.popFront();
    assert(range.front == Date(2010, 9, 6));

    range.popFront();
    assert(range.front == Date(2010, 9, 13));

    range.popFront();
    assert(range.front == Date(2010, 9, 20));

    range.popFront();
    assert(range.empty);
}

@system unittest
{
    import std.datetime.interval;

    import std.datetime.date : Date, Month;

    auto interval = Interval!Date(Date(2000, 1, 30), Date(2004, 8, 5));
    auto func = everyMonth!Date(Month.feb);
    auto range = interval.fwdRange(func);

    // Using PopFirst.yes would have made this Date(2010, 2, 29).
    assert(range.front == Date(2000, 1, 30));

    range.popFront();
    assert(range.front == Date(2000, 2, 29));

    range.popFront();
    assert(range.front == Date(2001, 2, 28));

    range.popFront();
    assert(range.front == Date(2002, 2, 28));

    range.popFront();
    assert(range.front == Date(2003, 2, 28));

    range.popFront();
    assert(range.front == Date(2004, 2, 28));

    range.popFront();
    assert(range.empty);
}

@system unittest
{
    import std.datetime.interval;

    import core.time : dur;
    import std.datetime.date : Date;

    auto interval = Interval!Date(Date(2010, 9, 2), Date(2010, 9, 27));
    auto func = everyDuration!Date(dur!"days"(8));
    auto range = interval.fwdRange(func);

    // Using PopFirst.yes would have made this Date(2010, 9, 10).
    assert(range.front == Date(2010, 9, 2));

    range.popFront();
    assert(range.front == Date(2010, 9, 10));

    range.popFront();
    assert(range.front == Date(2010, 9, 18));

    range.popFront();
    assert(range.front == Date(2010, 9, 26));

    range.popFront();
    assert(range.empty);
}

@system unittest
{
    import std.datetime.interval;

    import core.time : dur;
    import std.datetime.date : AllowDayOverflow, Date;

    auto interval = Interval!Date(Date(2010, 9, 2), Date(2025, 9, 27));
    auto func = everyDuration!Date(4, 1, AllowDayOverflow.yes, dur!"days"(2));
    auto range = interval.fwdRange(func);

    // Using PopFirst.yes would have made this Date(2014, 10, 12).
    assert(range.front == Date(2010, 9, 2));

    range.popFront();
    assert(range.front == Date(2014, 10, 4));

    range.popFront();
    assert(range.front == Date(2018, 11, 6));

    range.popFront();
    assert(range.front == Date(2022, 12, 8));

    range.popFront();
    assert(range.empty);
}


@safe unittest
{
    import std.datetime;

    import std.datetime.systime : SysTime, Clock;

    SysTime currentTime = Clock.currTime();
}

@safe unittest
{
    import std.datetime;

    import std.datetime.date : DateTime;

    auto dt = DateTime(2018, 1, 1, 12, 30, 10);
    assert(dt.toISOString() == "20180101T123010");
    assert(dt.toISOExtString() == "2018-01-01T12:30:10");
}

@safe unittest
{
    import std.datetime;

    import std.datetime.systime : SysTime;
    import std.datetime.timezone : UTC;
    import core.time : days;

    auto st = SysTime(DateTime(2018, 1, 1, 12, 30, 10), UTC());
    assert(st.toISOExtString() == "2018-01-01T12:30:10Z");
    st += 2.days;
    assert(st.toISOExtString() == "2018-01-03T12:30:10Z");
}


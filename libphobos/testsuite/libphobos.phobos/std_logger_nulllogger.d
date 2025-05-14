@safe unittest
{
    import std.logger.nulllogger;

    import std.logger.core : LogLevel;
    auto nl1 = new NullLogger(LogLevel.all);
    nl1.info("You will never read this.");
    nl1.fatal("You will never read this, either and it will not throw");
}


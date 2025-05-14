@safe unittest
{
    import std.logger.core;

    auto nl1 = new StdForwardLogger(LogLevel.all);
}

@system unittest
{
    import std.logger.core;

    import std.logger.filelogger : FileLogger;
    import std.file : deleteme, remove;
    Logger l = stdThreadLocalLog;
    stdThreadLocalLog = new FileLogger(deleteme ~ "-someFile.log");
    scope(exit) remove(deleteme ~ "-someFile.log");

    auto tempLog = stdThreadLocalLog;
    stdThreadLocalLog = l;
    destroy(tempLog);
}


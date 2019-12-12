///
module std.experimental.logger.multilogger;

import std.experimental.logger.core;
import std.experimental.logger.filelogger;

/** This Element is stored inside the $(D MultiLogger) and associates a
$(D Logger) to a $(D string).
*/
struct MultiLoggerEntry
{
    string name; /// The name if the $(D Logger)
    Logger logger; /// The stored $(D Logger)
}

/** MultiLogger logs to multiple $(D Logger). The $(D Logger)s are stored in an
$(D Logger[]) in their order of insertion.

Every data logged to this $(D MultiLogger) will be distributed to all the $(D
Logger)s inserted into it. This $(D MultiLogger) implementation can
hold multiple $(D Logger)s with the same name. If the method $(D removeLogger)
is used to remove a $(D Logger) only the first occurrence with that name will
be removed.
*/
class MultiLogger : Logger
{
    /** A constructor for the $(D MultiLogger) Logger.

    Params:
      lv = The $(D LogLevel) for the $(D MultiLogger). By default the
      $(D LogLevel) for $(D MultiLogger) is $(D LogLevel.all).

    Example:
    -------------
    auto l1 = new MultiLogger(LogLevel.trace);
    -------------
    */
    this(const LogLevel lv = LogLevel.all) @safe
    {
        super(lv);
    }

    /** This member holds all $(D Logger)s stored in the $(D MultiLogger).

    When inheriting from $(D MultiLogger) this member can be used to gain
    access to the stored $(D Logger).
    */
    protected MultiLoggerEntry[] logger;

    /** This method inserts a new Logger into the $(D MultiLogger).

    Params:
      name = The name of the $(D Logger) to insert.
      newLogger = The $(D Logger) to insert.
    */
    void insertLogger(string name, Logger newLogger) @safe
    {
        this.logger ~= MultiLoggerEntry(name, newLogger);
    }

    /** This method removes a Logger from the $(D MultiLogger).

    Params:
      toRemove = The name of the $(D Logger) to remove. If the $(D Logger)
        is not found $(D null) will be returned. Only the first occurrence of
        a $(D Logger) with the given name will be removed.

    Returns: The removed $(D Logger).
    */
    Logger removeLogger(in char[] toRemove) @safe
    {
        import std.algorithm.mutation : copy;
        import std.range.primitives : back, popBack;
        for (size_t i = 0; i < this.logger.length; ++i)
        {
            if (this.logger[i].name == toRemove)
            {
                Logger ret = this.logger[i].logger;
                this.logger[i] = this.logger.back;
                this.logger.popBack();

                return ret;
            }
        }

        return null;
    }

    /* The override to pass the payload to all children of the
    $(D MultiLoggerBase).
    */
    override protected void writeLogMsg(ref LogEntry payload) @safe
    {
        foreach (it; this.logger)
        {
            /* We don't perform any checks here to avoid race conditions.
            Instead the child will check on its own if its log level matches
            and assume LogLevel.all for the globalLogLevel (since we already
            know the message passes this test).
            */
            it.logger.forwardMsg(payload);
        }
    }
}

@safe unittest
{
    import std.exception : assertThrown;
    import std.experimental.logger.nulllogger;
    auto a = new MultiLogger;
    auto n0 = new NullLogger();
    auto n1 = new NullLogger();
    a.insertLogger("zero", n0);
    a.insertLogger("one", n1);

    auto n0_1 = a.removeLogger("zero");
    assert(n0_1 is n0);
    auto n = a.removeLogger("zero");
    assert(n is null);

    auto n1_1 = a.removeLogger("one");
    assert(n1_1 is n1);
    n = a.removeLogger("one");
    assert(n is null);
}

@safe unittest
{
    auto a = new MultiLogger;
    auto n0 = new TestLogger;
    auto n1 = new TestLogger;
    a.insertLogger("zero", n0);
    a.insertLogger("one", n1);

    a.log("Hello TestLogger"); int line = __LINE__;
    assert(n0.msg == "Hello TestLogger");
    assert(n0.line == line);
    assert(n1.msg == "Hello TestLogger");
    assert(n1.line == line);
}

// Issue #16
@system unittest
{
    import std.file : deleteme;
    import std.stdio : File;
    import std.string : indexOf;
    string logName = deleteme ~ __FUNCTION__ ~ ".log";
    auto logFileOutput = File(logName, "w");
    scope(exit)
    {
        import std.file : remove;
        logFileOutput.close();
        remove(logName);
    }
    auto traceLog = new FileLogger(logFileOutput, LogLevel.all);
    auto infoLog  = new TestLogger(LogLevel.info);

    auto root = new MultiLogger(LogLevel.all);
    root.insertLogger("fileLogger", traceLog);
    root.insertLogger("stdoutLogger", infoLog);

    string tMsg = "A trace message";
    root.trace(tMsg); int line1 = __LINE__;

    assert(infoLog.line != line1);
    assert(infoLog.msg != tMsg);

    string iMsg = "A info message";
    root.info(iMsg); int line2 = __LINE__;

    assert(infoLog.line == line2);
    assert(infoLog.msg == iMsg, infoLog.msg ~ ":" ~ iMsg);

    logFileOutput.close();
    logFileOutput = File(logName, "r");
    assert(logFileOutput.isOpen);
    assert(!logFileOutput.eof);

    auto line = logFileOutput.readln();
    assert(line.indexOf(tMsg) != -1, line ~ ":" ~ tMsg);
    assert(!logFileOutput.eof);
    line = logFileOutput.readln();
    assert(line.indexOf(iMsg) != -1, line ~ ":" ~ tMsg);
}

@safe unittest
{
    auto dl = cast(FileLogger) sharedLog;
    assert(dl !is null);
    assert(dl.logLevel == LogLevel.all);
    assert(globalLogLevel == LogLevel.all);

    auto tl = cast(StdForwardLogger) stdThreadLocalLog;
    assert(tl !is null);
    stdThreadLocalLog.logLevel = LogLevel.all;
}

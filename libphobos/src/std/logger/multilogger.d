// Written in the D programming language.
/**
Source: $(PHOBOSSRC std/logger/multilogger.d)
*/
module std.logger.multilogger;

import std.logger.core;
import std.logger.filelogger;

/** This Element is stored inside the `MultiLogger` and associates a
`Logger` to a `string`.
*/
struct MultiLoggerEntry
{
    string name; /// The name if the `Logger`
    Logger logger; /// The stored `Logger`
}

/** MultiLogger logs to multiple `Logger`. The `Logger`s are stored in an
`Logger[]` in their order of insertion.

Every data logged to this `MultiLogger` will be distributed to all the $(D
Logger)s inserted into it. This `MultiLogger` implementation can
hold multiple `Logger`s with the same name. If the method `removeLogger`
is used to remove a `Logger` only the first occurrence with that name will
be removed.
*/
class MultiLogger : Logger
{
    /** A constructor for the `MultiLogger` Logger.

    Params:
      lv = The `LogLevel` for the `MultiLogger`. By default the
      `LogLevel` for `MultiLogger` is `LogLevel.all`.

    Example:
    -------------
    auto l1 = new MultiLogger(LogLevel.trace);
    -------------
    */
    this(const LogLevel lv = LogLevel.all) @safe
    {
        super(lv);
    }

    /** This member holds all `Logger`s stored in the `MultiLogger`.

    When inheriting from `MultiLogger` this member can be used to gain
    access to the stored `Logger`.
    */
    protected MultiLoggerEntry[] logger;

    /** This method inserts a new Logger into the `MultiLogger`.

    Params:
      name = The name of the `Logger` to insert.
      newLogger = The `Logger` to insert.
    */
    void insertLogger(string name, Logger newLogger) @safe
    {
        this.logger ~= MultiLoggerEntry(name, newLogger);
    }

    /** This method removes a Logger from the `MultiLogger`.

    Params:
      toRemove = The name of the `Logger` to remove. If the `Logger`
        is not found `null` will be returned. Only the first occurrence of
        a `Logger` with the given name will be removed.

    Returns: The removed `Logger`.
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
    `MultiLoggerBase`.
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
    import std.logger.nulllogger;
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

@system unittest
{
    auto dl = cast(FileLogger) sharedLog;
    assert(dl !is null);
    assert(dl.logLevel == LogLevel.info);
    assert(globalLogLevel == LogLevel.all);

    auto tl = cast(StdForwardLogger) stdThreadLocalLog;
    assert(tl !is null);
    stdThreadLocalLog.logLevel = LogLevel.all;
}

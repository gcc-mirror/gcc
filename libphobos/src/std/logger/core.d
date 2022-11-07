// Written in the D programming language.
/**
Source: $(PHOBOSSRC std/logger/core.d)
*/
module std.logger.core;

import core.atomic : atomicLoad, atomicOp, atomicStore, MemoryOrder;
import core.sync.mutex : Mutex;
import std.datetime.date : DateTime;
import std.datetime.systime : Clock, SysTime;
import std.range.primitives;
import std.traits;

import std.logger.filelogger;

/** This functions is used at runtime to determine if a `LogLevel` is
active. The same previously defined version statements are used to disable
certain levels. Again the version statements are associated with a compile
unit and can therefore not disable logging in other compile units.
pure bool isLoggingEnabled()(LogLevel ll) @safe nothrow @nogc
*/
bool isLoggingEnabled()(LogLevel ll, LogLevel loggerLL,
    LogLevel globalLL, lazy bool condition = true) @safe
{
    return ll >= globalLL
        && ll >= loggerLL
        && ll != LogLevel.off
        && globalLL != LogLevel.off
        && loggerLL != LogLevel.off
        && condition;
}

/* This function formates a `SysTime` into an `OutputRange`.

The `SysTime` is formatted similar to
$(LREF std.datatime.DateTime.toISOExtString) except the fractional second part.
The fractional second part is in milliseconds and is always 3 digits.
*/
void systimeToISOString(OutputRange)(OutputRange o, const ref SysTime time)
if (isOutputRange!(OutputRange,string))
{
    import std.format.write : formattedWrite;

    const auto dt = cast(DateTime) time;
    const auto fsec = time.fracSecs.total!"msecs";

    formattedWrite(o, "%04d-%02d-%02dT%02d:%02d:%02d.%03d",
        dt.year, dt.month, dt.day, dt.hour, dt.minute, dt.second,
        fsec);
}

/** This function logs data.

In order for the data to be processed, the `LogLevel` of the log call must
be greater or equal to the `LogLevel` of the `sharedLog` and the
`defaultLogLevel`; additionally the condition passed must be `true`.

Params:
  ll = The `LogLevel` used by this log call.
  condition = The condition must be `true` for the data to be logged.
  args = The data that should be logged.

Example:
--------------------
log(LogLevel.warning, true, "Hello World", 3.1415);
--------------------
*/
void log(int line = __LINE__, string file = __FILE__,
    string funcName = __FUNCTION__, string prettyFuncName = __PRETTY_FUNCTION__,
    string moduleName = __MODULE__, A...)(const LogLevel ll,
    lazy bool condition, lazy A args)
if (args.length != 1)
{
    stdThreadLocalLog.log!(line, file, funcName, prettyFuncName, moduleName)
        (ll, condition, args);
}

/// Ditto
void log(T, string moduleName = __MODULE__)(const LogLevel ll,
    lazy bool condition, lazy T arg, int line = __LINE__, string file = __FILE__,
    string funcName = __FUNCTION__, string prettyFuncName = __PRETTY_FUNCTION__)
{
    stdThreadLocalLog.log!(T,moduleName)(ll, condition, arg, line, file, funcName,
        prettyFuncName);
}

/** This function logs data.

In order for the data to be processed the `LogLevel` of the log call must
be greater or equal to the `LogLevel` of the `sharedLog`.

Params:
  ll = The `LogLevel` used by this log call.
  args = The data that should be logged.

Example:
--------------------
log(LogLevel.warning, "Hello World", 3.1415);
--------------------
*/
void log(int line = __LINE__, string file = __FILE__,
    string funcName = __FUNCTION__, string prettyFuncName = __PRETTY_FUNCTION__,
    string moduleName = __MODULE__, A...)(const LogLevel ll, lazy A args)
if (args.length > 1 && !is(Unqual!(A[0]) : bool))
{
    stdThreadLocalLog.log!(line, file, funcName, prettyFuncName, moduleName)
        (ll, args);
}

/// Ditto
void log(T, string moduleName = __MODULE__)(const LogLevel ll, lazy T arg,
    int line = __LINE__, string file = __FILE__, string funcName = __FUNCTION__,
    string prettyFuncName = __PRETTY_FUNCTION__)
{
    stdThreadLocalLog.log!T(ll, arg, line, file, funcName, prettyFuncName,
        moduleName);
}

/** This function logs data.

In order for the data to be processed the `LogLevel` of the
`sharedLog` must be greater or equal to the `defaultLogLevel`
add the condition passed must be `true`.

Params:
  condition = The condition must be `true` for the data to be logged.
  args = The data that should be logged.

Example:
--------------------
log(true, "Hello World", 3.1415);
--------------------
*/
void log(int line = __LINE__, string file = __FILE__,
    string funcName = __FUNCTION__, string prettyFuncName = __PRETTY_FUNCTION__,
    string moduleName = __MODULE__, A...)(lazy bool condition, lazy A args)
if (args.length != 1)
{
    stdThreadLocalLog.log!(line, file, funcName, prettyFuncName, moduleName)
        (stdThreadLocalLog.logLevel, condition, args);
}

/// Ditto
void log(T, string moduleName = __MODULE__)(lazy bool condition, lazy T arg,
    int line = __LINE__, string file = __FILE__, string funcName = __FUNCTION__,
    string prettyFuncName = __PRETTY_FUNCTION__)
{
    stdThreadLocalLog.log!(T,moduleName)(stdThreadLocalLog.logLevel,
        condition, arg, line, file, funcName, prettyFuncName);
}

/** This function logs data.

In order for the data to be processed the `LogLevel` of the
`sharedLog` must be greater or equal to the `defaultLogLevel`.

Params:
  args = The data that should be logged.

Example:
--------------------
log("Hello World", 3.1415);
--------------------
*/
void log(int line = __LINE__, string file = __FILE__,
    string funcName = __FUNCTION__, string prettyFuncName = __PRETTY_FUNCTION__,
    string moduleName = __MODULE__, A...)(lazy A args)
if ((args.length > 1 && !is(Unqual!(A[0]) : bool)
    && !is(Unqual!(A[0]) == LogLevel))
    || args.length == 0)
{
    stdThreadLocalLog.log!(line, file, funcName,
       prettyFuncName, moduleName)(stdThreadLocalLog.logLevel, args);
}

void log(T)(lazy T arg, int line = __LINE__, string file = __FILE__,
    string funcName = __FUNCTION__, string prettyFuncName = __PRETTY_FUNCTION__,
    string moduleName = __MODULE__)
{
    stdThreadLocalLog.log!T(stdThreadLocalLog.logLevel, arg, line, file,
        funcName, prettyFuncName, moduleName);
}

/** This function logs data in a `printf`-style manner.

In order for the data to be processed the `LogLevel` of the log call must
be greater or equal to the `LogLevel` of the `sharedLog` and the
`defaultLogLevel` additionally the condition passed must be `true`.

Params:
  ll = The `LogLevel` used by this log call.
  condition = The condition must be `true` for the data to be logged.
  msg = The `printf`-style string.
  args = The data that should be logged.

Example:
--------------------
logf(LogLevel.warning, true, "Hello World %f", 3.1415);
--------------------
*/
void logf(int line = __LINE__, string file = __FILE__,
    string funcName = __FUNCTION__,
    string prettyFuncName = __PRETTY_FUNCTION__,
    string moduleName = __MODULE__, A...)(const LogLevel ll,
    lazy bool condition, lazy string msg, lazy A args)
{
    stdThreadLocalLog.logf!(line, file, funcName, prettyFuncName, moduleName)
        (ll, condition, msg, args);
}

/** This function logs data in a `printf`-style manner.

In order for the data to be processed the `LogLevel` of the log call must
be greater or equal to the `LogLevel` of the `sharedLog` and the
`defaultLogLevel`.

Params:
  ll = The `LogLevel` used by this log call.
  msg = The `printf`-style string.
  args = The data that should be logged.

Example:
--------------------
logf(LogLevel.warning, "Hello World %f", 3.1415);
--------------------
*/
void logf(int line = __LINE__, string file = __FILE__,
    string funcName = __FUNCTION__, string prettyFuncName = __PRETTY_FUNCTION__,
    string moduleName = __MODULE__, A...)(const LogLevel ll, lazy string msg,
        lazy A args)
{
    stdThreadLocalLog.logf!(line, file, funcName, prettyFuncName, moduleName)
        (ll, msg, args);
}

/** This function logs data in a `printf`-style manner.

In order for the data to be processed the `LogLevel` of the log call must
be greater or equal to the `defaultLogLevel` additionally the condition
passed must be `true`.

Params:
  condition = The condition must be `true` for the data to be logged.
  msg = The `printf`-style string.
  args = The data that should be logged.

Example:
--------------------
logf(true, "Hello World %f", 3.1415);
--------------------
*/
void logf(int line = __LINE__, string file = __FILE__,
    string funcName = __FUNCTION__, string prettyFuncName = __PRETTY_FUNCTION__,
    string moduleName = __MODULE__, A...)(lazy bool condition,
        lazy string msg, lazy A args)
{
    stdThreadLocalLog.logf!(line, file, funcName, prettyFuncName, moduleName)
        (stdThreadLocalLog.logLevel, condition, msg, args);
}

/** This function logs data in a `printf`-style manner.

In order for the data to be processed the `LogLevel` of the log call must
be greater or equal to the `defaultLogLevel`.

Params:
  msg = The `printf`-style string.
  args = The data that should be logged.

Example:
--------------------
logf("Hello World %f", 3.1415);
--------------------
*/
void logf(int line = __LINE__, string file = __FILE__,
    string funcName = __FUNCTION__,
    string prettyFuncName = __PRETTY_FUNCTION__,
    string moduleName = __MODULE__, A...)(lazy string msg, lazy A args)
{
    stdThreadLocalLog.logf!(line, file, funcName,prettyFuncName, moduleName)
        (stdThreadLocalLog.logLevel, msg, args);
}

/** This template provides the global log functions with the `LogLevel`
is encoded in the function name.

The aliases following this template create the public names of these log
functions.
*/
template defaultLogFunction(LogLevel ll)
{
    void defaultLogFunction(int line = __LINE__, string file = __FILE__,
        string funcName = __FUNCTION__,
        string prettyFuncName = __PRETTY_FUNCTION__,
        string moduleName = __MODULE__, A...)(lazy A args)
        if ((args.length > 0 && !is(Unqual!(A[0]) : bool)) || args.length == 0)
    {
            stdThreadLocalLog.memLogFunctions!(ll).logImpl!(line, file, funcName,
                prettyFuncName, moduleName)(args);
    }

    void defaultLogFunction(int line = __LINE__, string file = __FILE__,
        string funcName = __FUNCTION__,
        string prettyFuncName = __PRETTY_FUNCTION__,
        string moduleName = __MODULE__, A...)(lazy bool condition, lazy A args)
    {
        stdThreadLocalLog.memLogFunctions!(ll).logImpl!(line, file, funcName,
            prettyFuncName, moduleName)(condition, args);
    }
}

/** This function logs data to the `stdThreadLocalLog`, optionally depending
on a condition.

In order for the resulting log message to be logged the `LogLevel` must
be greater or equal than the `LogLevel` of the `stdThreadLocalLog` and
must be greater or equal than the global `LogLevel`.
Additionally the `LogLevel` must be greater or equal than the `LogLevel`
of the `stdSharedLogger`.
If a condition is given, it must evaluate to `true`.

Params:
  condition = The condition must be `true` for the data to be logged.
  args = The data that should be logged.

Example:
--------------------
trace(1337, "is number");
info(1337, "is number");
error(1337, "is number");
critical(1337, "is number");
fatal(1337, "is number");
trace(true, 1337, "is number");
info(false, 1337, "is number");
error(true, 1337, "is number");
critical(false, 1337, "is number");
fatal(true, 1337, "is number");
--------------------
*/
alias trace = defaultLogFunction!(LogLevel.trace);
/// Ditto
alias info = defaultLogFunction!(LogLevel.info);
/// Ditto
alias warning = defaultLogFunction!(LogLevel.warning);
/// Ditto
alias error = defaultLogFunction!(LogLevel.error);
/// Ditto
alias critical = defaultLogFunction!(LogLevel.critical);
/// Ditto
alias fatal = defaultLogFunction!(LogLevel.fatal);

/** This template provides the global `printf`-style log functions with
the `LogLevel` is encoded in the function name.

The aliases following this template create the public names of the log
functions.
*/
template defaultLogFunctionf(LogLevel ll)
{
    void defaultLogFunctionf(int line = __LINE__, string file = __FILE__,
        string funcName = __FUNCTION__,
        string prettyFuncName = __PRETTY_FUNCTION__,
        string moduleName = __MODULE__, A...)(lazy string msg, lazy A args)
    {
        stdThreadLocalLog.memLogFunctions!(ll).logImplf!(line, file, funcName,
            prettyFuncName, moduleName)(msg, args);
    }

    void defaultLogFunctionf(int line = __LINE__, string file = __FILE__,
        string funcName = __FUNCTION__,
        string prettyFuncName = __PRETTY_FUNCTION__,
        string moduleName = __MODULE__, A...)(lazy bool condition,
            lazy string msg, lazy A args)
    {
        stdThreadLocalLog.memLogFunctions!(ll).logImplf!(line, file, funcName,
            prettyFuncName, moduleName)(condition, msg, args);
    }
}

/** This function logs data to the `sharedLog` in a `printf`-style
manner.

In order for the resulting log message to be logged the `LogLevel` must
be greater or equal than the `LogLevel` of the `sharedLog` and
must be greater or equal than the global `LogLevel`.
Additionally the `LogLevel` must be greater or equal than the `LogLevel`
of the `stdSharedLogger`.

Params:
  msg = The `printf`-style string.
  args = The data that should be logged.

Example:
--------------------
tracef("is number %d", 1);
infof("is number %d", 2);
errorf("is number %d", 3);
criticalf("is number %d", 4);
fatalf("is number %d", 5);
--------------------

The second version of the function logs data to the `sharedLog` in a $(D
printf)-style manner.

In order for the resulting log message to be logged the `LogLevel` must
be greater or equal than the `LogLevel` of the `sharedLog` and
must be greater or equal than the global `LogLevel`.
Additionally the `LogLevel` must be greater or equal than the `LogLevel`
of the `stdSharedLogger`.

Params:
  condition = The condition must be `true` for the data to be logged.
  msg = The `printf`-style string.
  args = The data that should be logged.

Example:
--------------------
tracef(false, "is number %d", 1);
infof(false, "is number %d", 2);
errorf(true, "is number %d", 3);
criticalf(true, "is number %d", 4);
fatalf(someFunct(), "is number %d", 5);
--------------------
*/
alias tracef = defaultLogFunctionf!(LogLevel.trace);
/// Ditto
alias infof = defaultLogFunctionf!(LogLevel.info);
/// Ditto
alias warningf = defaultLogFunctionf!(LogLevel.warning);
/// Ditto
alias errorf = defaultLogFunctionf!(LogLevel.error);
/// Ditto
alias criticalf = defaultLogFunctionf!(LogLevel.critical);
/// Ditto
alias fatalf = defaultLogFunctionf!(LogLevel.fatal);

private struct MsgRange
{
    import std.traits : isSomeString, isSomeChar;

    private Logger log;

    this(Logger log) @safe
    {
        this.log = log;
    }

    void put(T)(T msg) @safe
        if (isSomeString!T)
    {
        log.logMsgPart(msg);
    }

    void put(dchar elem) @safe
    {
        import std.utf : encode;
        char[4] buffer;
        size_t len = encode(buffer, elem);
        log.logMsgPart(buffer[0 .. len]);
    }
}

private void formatString(A...)(MsgRange oRange, A args)
{
    import std.format.write : formattedWrite;

    foreach (arg; args)
    {
        formattedWrite(oRange, "%s", arg);
    }
}

@system unittest
{
    void dummy() @safe
    {
        auto tl = new TestLogger();
        auto dst = MsgRange(tl);
        formatString(dst, "aaa", "bbb");
    }

    dummy();
}

/**
There are eight usable logging level. These level are $(I all), $(I trace),
$(I info), $(I warning), $(I error), $(I critical), $(I fatal), and $(I off).
If a log function with `LogLevel.fatal` is called the shutdown handler of
that logger is called.
*/
enum LogLevel : ubyte
{
    all = 1, /** Lowest possible assignable `LogLevel`. */
    trace = 32, /** `LogLevel` for tracing the execution of the program. */
    info = 64, /** This level is used to display information about the
                program. */
    warning = 96, /** warnings about the program should be displayed with this
                   level. */
    error = 128, /** Information about errors should be logged with this
                   level.*/
    critical = 160, /** Messages that inform about critical errors should be
                    logged with this level. */
    fatal = 192,   /** Log messages that describe fatal errors should use this
                  level. */
    off = ubyte.max /** Highest possible `LogLevel`. */
}

/** This class is the base of every logger. In order to create a new kind of
logger a deriving class needs to implement the `writeLogMsg` method. By
default this is not thread-safe.

It is also possible to `override` the three methods `beginLogMsg`,
`logMsgPart` and `finishLogMsg` together, this option gives more
flexibility.
*/
abstract class Logger
{
    import std.array : appender, Appender;
    import std.concurrency : thisTid, Tid;

    /** LogEntry is a aggregation combining all information associated
    with a log message. This aggregation will be passed to the method
    writeLogMsg.
    */
    protected struct LogEntry
    {
        /// the filename the log function was called from
        string file;
        /// the line number the log function was called from
        int line;
        /// the name of the function the log function was called from
        string funcName;
        /// the pretty formatted name of the function the log function was
        /// called from
        string prettyFuncName;
        /// the name of the module the log message is coming from
        string moduleName;
        /// the `LogLevel` associated with the log message
        LogLevel logLevel;
        /// thread id of the log message
        Tid threadId;
        /// the time the message was logged
        SysTime timestamp;
        /// the message of the log message
        string msg;
        /// A refernce to the `Logger` used to create this `LogEntry`
        Logger logger;
    }

    /**
    Every subclass of `Logger` has to call this constructor from their
    constructor. It sets the `LogLevel`, and creates a fatal handler. The fatal
    handler will throw an `Error` if a log call is made with level
    `LogLevel.fatal`.

    Params:
         lv = `LogLevel` to use for this `Logger` instance.
    */
    this(this This)(LogLevel lv)
    {
        this.logLevel_ = lv;
        this.fatalHandler_ = delegate() {
            throw new Error("A fatal log message was logged");
        };

        this.mutex = new typeof(mutex)();
    }

    /** A custom logger must implement this method in order to work in a
    `MultiLogger` and `ArrayLogger`.

    Params:
      payload = All information associated with call to log function.

    See_Also: beginLogMsg, logMsgPart, finishLogMsg
    */
    abstract protected void writeLogMsg(ref LogEntry payload) @safe;

    /* The default implementation will use an `std.array.appender`
    internally to construct the message string. This means dynamic,
    GC memory allocation. A logger can avoid this allocation by
    reimplementing `beginLogMsg`, `logMsgPart` and `finishLogMsg`.
    `beginLogMsg` is always called first, followed by any number of calls
    to `logMsgPart` and one call to `finishLogMsg`.

    As an example for such a custom `Logger` compare this:
    ----------------
    class CLogger : Logger
    {
        override void beginLogMsg(string file, int line, string funcName,
            string prettyFuncName, string moduleName, LogLevel logLevel,
            Tid threadId, SysTime timestamp)
        {
            ... logic here
        }

        override void logMsgPart(const(char)[] msg)
        {
            ... logic here
        }

        override void finishLogMsg()
        {
            ... logic here
        }

        void writeLogMsg(ref LogEntry payload)
        {
            this.beginLogMsg(payload.file, payload.line, payload.funcName,
                payload.prettyFuncName, payload.moduleName, payload.logLevel,
                payload.threadId, payload.timestamp, payload.logger);

            this.logMsgPart(payload.msg);
            this.finishLogMsg();
        }
    }
    ----------------
    */
    protected void beginLogMsg(string file, int line, string funcName,
        string prettyFuncName, string moduleName, LogLevel logLevel,
        Tid threadId, SysTime timestamp, Logger logger)
        @safe
    {
        msgAppender = appender!string();
        header = LogEntry(file, line, funcName, prettyFuncName,
            moduleName, logLevel, threadId, timestamp, null, logger);
    }

    /** Logs a part of the log message. */
    protected void logMsgPart(scope const(char)[] msg) @safe
    {
       msgAppender.put(msg);
    }

    /** Signals that the message has been written and no more calls to
    `logMsgPart` follow. */
    protected void finishLogMsg() @safe
    {
        header.msg = msgAppender.data;
        this.writeLogMsg(header);
    }

    /** The `LogLevel` determines if the log call are processed or dropped
    by the `Logger`. In order for the log call to be processed the
    `LogLevel` of the log call must be greater or equal to the `LogLevel`
    of the `logger`.

    These two methods set and get the `LogLevel` of the used `Logger`.

    Example:
    -----------
    auto f = new FileLogger(stdout);
    f.logLevel = LogLevel.info;
    assert(f.logLevel == LogLevel.info);
    -----------
    */
    @property final LogLevel logLevel() const pure @safe @nogc
    {
        return trustedLoad(this.logLevel_);
    }

    /// Ditto
    @property final void logLevel(const LogLevel lv) @safe @nogc
    {
        atomicStore(this.logLevel_, lv);
    }

    /** This `delegate` is called in case a log message with
    `LogLevel.fatal` gets logged.

    By default an `Error` will be thrown.
    */
    @property final void delegate() fatalHandler() @safe @nogc
    {
        synchronized (mutex) return this.fatalHandler_;
    }

    /// Ditto
    @property final void fatalHandler(void delegate() @safe fh) @safe @nogc
    {
        synchronized (mutex) this.fatalHandler_ = fh;
    }

    /** This method allows forwarding log entries from one logger to another.

    `forwardMsg` will ensure proper synchronization and then call
    `writeLogMsg`. This is an API for implementing your own loggers and
    should not be called by normal user code. A notable difference from other
    logging functions is that the `globalLogLevel` wont be evaluated again
    since it is assumed that the caller already checked that.
    */
    void forwardMsg(ref LogEntry payload) @trusted
    {
        if (isLoggingEnabled(payload.logLevel, this.logLevel_,
            globalLogLevel))
        {
            this.writeLogMsg(payload);

            if (payload.logLevel == LogLevel.fatal)
                this.fatalHandler_();
        }
    }

    /** This template provides the log functions for the `Logger` `class`
    with the `LogLevel` encoded in the function name.

    For further information see the two functions defined inside of this
    template.

    The aliases following this template create the public names of these log
    functions.
    */
    template memLogFunctions(LogLevel ll)
    {
        /** This function logs data to the used `Logger`.

        In order for the resulting log message to be logged the `LogLevel`
        must be greater or equal than the `LogLevel` of the used `Logger`
        and must be greater or equal than the global `LogLevel`.

        Params:
          args = The data that should be logged.

        Example:
        --------------------
        auto s = new FileLogger(stdout);
        s.trace(1337, "is number");
        s.info(1337, "is number");
        s.error(1337, "is number");
        s.critical(1337, "is number");
        s.fatal(1337, "is number");
        --------------------
        */
        void logImpl(int line = __LINE__, string file = __FILE__,
            string funcName = __FUNCTION__,
            string prettyFuncName = __PRETTY_FUNCTION__,
            string moduleName = __MODULE__, A...)(lazy A args)
            if (args.length == 0 || (args.length > 0 && !is(A[0] : bool)))
        {
            synchronized (mutex)
            {
                if (isLoggingEnabled(ll, this.logLevel_, globalLogLevel))
                {
                    this.beginLogMsg(file, line, funcName, prettyFuncName,
                        moduleName, ll, thisTid, Clock.currTime, this);

                    auto writer = MsgRange(this);
                    formatString(writer, args);

                    this.finishLogMsg();

                    static if (ll == LogLevel.fatal)
                        this.fatalHandler_();
                }
            }
        }

        /** This function logs data to the used `Logger` depending on a
        condition.

        In order for the resulting log message to be logged the `LogLevel` must
        be greater or equal than the `LogLevel` of the used `Logger` and
        must be greater or equal than the global `LogLevel` additionally the
        condition passed must be `true`.

        Params:
          condition = The condition must be `true` for the data to be logged.
          args = The data that should be logged.

        Example:
        --------------------
        auto s = new FileLogger(stdout);
        s.trace(true, 1337, "is number");
        s.info(false, 1337, "is number");
        s.error(true, 1337, "is number");
        s.critical(false, 1337, "is number");
        s.fatal(true, 1337, "is number");
        --------------------
        */
        void logImpl(int line = __LINE__, string file = __FILE__,
            string funcName = __FUNCTION__,
            string prettyFuncName = __PRETTY_FUNCTION__,
            string moduleName = __MODULE__, A...)(lazy bool condition,
                lazy A args)
        {
            synchronized (mutex)
            {
                if (isLoggingEnabled(ll, this.logLevel_, globalLogLevel,
                                     condition))
                {
                    this.beginLogMsg(file, line, funcName, prettyFuncName,
                        moduleName, ll, thisTid, Clock.currTime, this);

                    auto writer = MsgRange(this);
                    formatString(writer, args);

                    this.finishLogMsg();

                    static if (ll == LogLevel.fatal)
                        this.fatalHandler_();
                }
            }
        }

        /** This function logs data to the used `Logger` in a
        `printf`-style manner.

        In order for the resulting log message to be logged the `LogLevel`
        must be greater or equal than the `LogLevel` of the used `Logger`
        and must be greater or equal than the global `LogLevel` additionally
           the passed condition must be `true`.

        Params:
          condition = The condition must be `true` for the data to be logged.
          msg = The `printf`-style string.
          args = The data that should be logged.

        Example:
        --------------------
        auto s = new FileLogger(stderr);
        s.tracef(true, "is number %d", 1);
        s.infof(true, "is number %d", 2);
        s.errorf(false, "is number %d", 3);
        s.criticalf(someFunc(), "is number %d", 4);
        s.fatalf(true, "is number %d", 5);
        --------------------
        */
        void logImplf(int line = __LINE__, string file = __FILE__,
            string funcName = __FUNCTION__,
            string prettyFuncName = __PRETTY_FUNCTION__,
            string moduleName = __MODULE__, A...)(lazy bool condition,
                lazy string msg, lazy A args)
        {
            synchronized (mutex)
            {
                import std.format.write : formattedWrite;

                if (isLoggingEnabled(ll, this.logLevel_, globalLogLevel,
                                     condition))
                {
                    this.beginLogMsg(file, line, funcName, prettyFuncName,
                        moduleName, ll, thisTid, Clock.currTime, this);

                    auto writer = MsgRange(this);
                    formattedWrite(writer, msg, args);

                    this.finishLogMsg();

                    static if (ll == LogLevel.fatal)
                        this.fatalHandler_();
                }
            }
        }

        /** This function logs data to the used `Logger` in a
        `printf`-style manner.

        In order for the resulting log message to be logged the `LogLevel` must
        be greater or equal than the `LogLevel` of the used `Logger` and
        must be greater or equal than the global `LogLevel`.

        Params:
          msg = The `printf`-style string.
          args = The data that should be logged.

        Example:
        --------------------
        auto s = new FileLogger(stderr);
        s.tracef("is number %d", 1);
        s.infof("is number %d", 2);
        s.errorf("is number %d", 3);
        s.criticalf("is number %d", 4);
        s.fatalf("is number %d", 5);
        --------------------
        */
        void logImplf(int line = __LINE__, string file = __FILE__,
            string funcName = __FUNCTION__,
            string prettyFuncName = __PRETTY_FUNCTION__,
            string moduleName = __MODULE__, A...)(lazy string msg, lazy A args)
        {
            synchronized (mutex)
            {
                import std.format.write : formattedWrite;

                if (isLoggingEnabled(ll, this.logLevel_, globalLogLevel))
                {
                    this.beginLogMsg(file, line, funcName, prettyFuncName,
                        moduleName, ll, thisTid, Clock.currTime, this);

                    auto writer = MsgRange(this);
                    formattedWrite(writer, msg, args);

                    this.finishLogMsg();

                    static if (ll == LogLevel.fatal)
                        this.fatalHandler_();
                }
            }
        }
    }

    /// Ditto
    alias trace = memLogFunctions!(LogLevel.trace).logImpl;
    /// Ditto
    alias tracef = memLogFunctions!(LogLevel.trace).logImplf;
    /// Ditto
    alias info = memLogFunctions!(LogLevel.info).logImpl;
    /// Ditto
    alias infof = memLogFunctions!(LogLevel.info).logImplf;
    /// Ditto
    alias warning = memLogFunctions!(LogLevel.warning).logImpl;
    /// Ditto
    alias warningf = memLogFunctions!(LogLevel.warning).logImplf;
    /// Ditto
    alias error = memLogFunctions!(LogLevel.error).logImpl;
    /// Ditto
    alias errorf = memLogFunctions!(LogLevel.error).logImplf;
    /// Ditto
    alias critical = memLogFunctions!(LogLevel.critical).logImpl;
    /// Ditto
    alias criticalf = memLogFunctions!(LogLevel.critical).logImplf;
    /// Ditto
    alias fatal = memLogFunctions!(LogLevel.fatal).logImpl;
    /// Ditto
    alias fatalf = memLogFunctions!(LogLevel.fatal).logImplf;

    /** This method logs data with the `LogLevel` of the used `Logger`.

    This method takes a `bool` as first argument. In order for the
    data to be processed the `bool` must be `true` and the `LogLevel`
    of the Logger must be greater or equal to the global `LogLevel`.

    Params:
      args = The data that should be logged.
      condition = The condition must be `true` for the data to be logged.
      args = The data that is to be logged.

    Returns: The logger used by the logging function as reference.

    Example:
    --------------------
    auto l = new StdioLogger();
    l.log(1337);
    --------------------
    */
    void log(int line = __LINE__, string file = __FILE__,
        string funcName = __FUNCTION__,
        string prettyFuncName = __PRETTY_FUNCTION__,
        string moduleName = __MODULE__, A...)(const LogLevel ll,
        lazy bool condition, lazy A args)
        if (args.length != 1)
    {
        synchronized (mutex)
        {
            if (isLoggingEnabled(ll, this.logLevel_, globalLogLevel, condition))
            {
                this.beginLogMsg(file, line, funcName, prettyFuncName,
                    moduleName, ll, thisTid, Clock.currTime, this);

                auto writer = MsgRange(this);
                formatString(writer, args);

                this.finishLogMsg();

                if (ll == LogLevel.fatal)
                    this.fatalHandler_();
            }
        }
    }

    /// Ditto
    void log(T, string moduleName = __MODULE__)(const LogLevel ll,
        lazy bool condition, lazy T args, int line = __LINE__,
        string file = __FILE__, string funcName = __FUNCTION__,
        string prettyFuncName = __PRETTY_FUNCTION__)
    {
        synchronized (mutex)
        {
            if (isLoggingEnabled(ll, this.logLevel_, globalLogLevel, condition))
            {
                this.beginLogMsg(file, line, funcName, prettyFuncName,
                    moduleName, ll, thisTid, Clock.currTime, this);
                auto writer = MsgRange(this);
                formatString(writer, args);

                this.finishLogMsg();

                if (ll == LogLevel.fatal)
                    this.fatalHandler_();
            }
        }
    }

    /** This function logs data to the used `Logger` with a specific
    `LogLevel`.

    In order for the resulting log message to be logged the `LogLevel`
    must be greater or equal than the `LogLevel` of the used `Logger`
    and must be greater or equal than the global `LogLevel`.

    Params:
      ll = The specific `LogLevel` used for logging the log message.
      args = The data that should be logged.

    Example:
    --------------------
    auto s = new FileLogger(stdout);
    s.log(LogLevel.trace, 1337, "is number");
    s.log(LogLevel.info, 1337, "is number");
    s.log(LogLevel.warning, 1337, "is number");
    s.log(LogLevel.error, 1337, "is number");
    s.log(LogLevel.fatal, 1337, "is number");
    --------------------
    */
    void log(int line = __LINE__, string file = __FILE__,
        string funcName = __FUNCTION__,
        string prettyFuncName = __PRETTY_FUNCTION__,
        string moduleName = __MODULE__, A...)(const LogLevel ll, lazy A args)
        if ((args.length > 1 && !is(Unqual!(A[0]) : bool)) || args.length == 0)
    {
        synchronized (mutex)
        {
            if (isLoggingEnabled(ll, this.logLevel_, globalLogLevel))
            {
                this.beginLogMsg(file, line, funcName, prettyFuncName,
                    moduleName, ll, thisTid, Clock.currTime, this);

                auto writer = MsgRange(this);
                formatString(writer, args);

                this.finishLogMsg();

                if (ll == LogLevel.fatal)
                    this.fatalHandler_();
            }
        }
    }

    /// Ditto
    void log(T)(const LogLevel ll, lazy T args, int line = __LINE__,
        string file = __FILE__, string funcName = __FUNCTION__,
        string prettyFuncName = __PRETTY_FUNCTION__,
        string moduleName = __MODULE__)
    {
        synchronized (mutex)
        {
            if (isLoggingEnabled(ll, this.logLevel_, globalLogLevel))
            {
                this.beginLogMsg(file, line, funcName, prettyFuncName,
                    moduleName, ll, thisTid, Clock.currTime, this);
                auto writer = MsgRange(this);
                formatString(writer, args);

                this.finishLogMsg();

                if (ll == LogLevel.fatal)
                    this.fatalHandler_();
            }
        }
    }

    /** This function logs data to the used `Logger` depending on a
    explicitly passed condition with the `LogLevel` of the used
    `Logger`.

    In order for the resulting log message to be logged the `LogLevel`
    of the used `Logger` must be greater or equal than the global
    `LogLevel` and the condition must be `true`.

    Params:
      condition = The condition must be `true` for the data to be logged.
      args = The data that should be logged.

    Example:
    --------------------
    auto s = new FileLogger(stdout);
    s.log(true, 1337, "is number");
    s.log(true, 1337, "is number");
    s.log(true, 1337, "is number");
    s.log(false, 1337, "is number");
    s.log(false, 1337, "is number");
    --------------------
    */
    void log(int line = __LINE__, string file = __FILE__,
        string funcName = __FUNCTION__,
        string prettyFuncName = __PRETTY_FUNCTION__,
        string moduleName = __MODULE__, A...)(lazy bool condition, lazy A args)
        if (args.length != 1)
    {
        synchronized (mutex)
        {
            if (isLoggingEnabled(this.logLevel_, this.logLevel_,
                globalLogLevel, condition))
            {
                this.beginLogMsg(file, line, funcName, prettyFuncName,
                    moduleName, this.logLevel_, thisTid, Clock.currTime, this);

                auto writer = MsgRange(this);
                formatString(writer, args);

                this.finishLogMsg();

                if (this.logLevel_ == LogLevel.fatal)
                    this.fatalHandler_();
            }
        }
    }

    /// Ditto
    void log(T)(lazy bool condition, lazy T args, int line = __LINE__,
        string file = __FILE__, string funcName = __FUNCTION__,
        string prettyFuncName = __PRETTY_FUNCTION__,
        string moduleName = __MODULE__)
    {
        synchronized (mutex)
        {
            if (isLoggingEnabled(this.logLevel_, this.logLevel_, globalLogLevel,
                condition))
            {
                this.beginLogMsg(file, line, funcName, prettyFuncName,
                    moduleName, this.logLevel_, thisTid, Clock.currTime, this);
                auto writer = MsgRange(this);
                formatString(writer, args);

                this.finishLogMsg();

                if (this.logLevel_ == LogLevel.fatal)
                    this.fatalHandler_();
            }
        }
    }

    /** This function logs data to the used `Logger` with the `LogLevel`
    of the used `Logger`.

    In order for the resulting log message to be logged the `LogLevel`
    of the used `Logger` must be greater or equal than the global
    `LogLevel`.

    Params:
      args = The data that should be logged.

    Example:
    --------------------
    auto s = new FileLogger(stdout);
    s.log(1337, "is number");
    s.log(info, 1337, "is number");
    s.log(1337, "is number");
    s.log(1337, "is number");
    s.log(1337, "is number");
    --------------------
    */
    void log(int line = __LINE__, string file = __FILE__,
        string funcName = __FUNCTION__,
        string prettyFuncName = __PRETTY_FUNCTION__,
        string moduleName = __MODULE__, A...)(lazy A args)
        if ((args.length > 1
                && !is(Unqual!(A[0]) : bool)
                && !is(immutable A[0] == immutable LogLevel))
            || args.length == 0)
    {
        synchronized (mutex)
        {
            if (isLoggingEnabled(this.logLevel_, this.logLevel_,
                globalLogLevel))
            {
                this.beginLogMsg(file, line, funcName, prettyFuncName,
                    moduleName, this.logLevel_, thisTid, Clock.currTime, this);
                auto writer = MsgRange(this);
                formatString(writer, args);

                this.finishLogMsg();

                if (this.logLevel_ == LogLevel.fatal)
                    this.fatalHandler_();
            }
        }
    }

    /// Ditto
    void log(T)(lazy T arg, int line = __LINE__, string file = __FILE__,
        string funcName = __FUNCTION__,
        string prettyFuncName = __PRETTY_FUNCTION__,
        string moduleName = __MODULE__)
    {
        synchronized (mutex)
        {
            if (isLoggingEnabled(this.logLevel_, this.logLevel_, globalLogLevel))
            {
                this.beginLogMsg(file, line, funcName, prettyFuncName,
                    moduleName, this.logLevel_, thisTid, Clock.currTime, this);
                auto writer = MsgRange(this);
                formatString(writer, arg);

                this.finishLogMsg();

                if (this.logLevel_ == LogLevel.fatal)
                    this.fatalHandler_();
            }
        }
    }

    /** This function logs data to the used `Logger` with a specific
    `LogLevel` and depending on a condition in a `printf`-style manner.

    In order for the resulting log message to be logged the `LogLevel`
    must be greater or equal than the `LogLevel` of the used `Logger`
    and must be greater or equal than the global `LogLevel` and the
    condition must be `true`.

    Params:
      ll = The specific `LogLevel` used for logging the log message.
      condition = The condition must be `true` for the data to be logged.
      msg = The format string used for this log call.
      args = The data that should be logged.

    Example:
    --------------------
    auto s = new FileLogger(stdout);
    s.logf(LogLevel.trace, true ,"%d %s", 1337, "is number");
    s.logf(LogLevel.info, true ,"%d %s", 1337, "is number");
    s.logf(LogLevel.warning, true ,"%d %s", 1337, "is number");
    s.logf(LogLevel.error, false ,"%d %s", 1337, "is number");
    s.logf(LogLevel.fatal, true ,"%d %s", 1337, "is number");
    --------------------
    */
    void logf(int line = __LINE__, string file = __FILE__,
        string funcName = __FUNCTION__,
        string prettyFuncName = __PRETTY_FUNCTION__,
        string moduleName = __MODULE__, A...)(const LogLevel ll,
        lazy bool condition, lazy string msg, lazy A args)
    {
        synchronized (mutex)
        {
            import std.format.write : formattedWrite;

            if (isLoggingEnabled(ll, this.logLevel_, globalLogLevel, condition))
            {
                this.beginLogMsg(file, line, funcName, prettyFuncName,
                    moduleName, ll, thisTid, Clock.currTime, this);

                auto writer = MsgRange(this);
                formattedWrite(writer, msg, args);

                this.finishLogMsg();

                if (ll == LogLevel.fatal)
                    this.fatalHandler_();
            }
        }
    }

    /** This function logs data to the used `Logger` with a specific
    `LogLevel` in a `printf`-style manner.

    In order for the resulting log message to be logged the `LogLevel`
    must be greater or equal than the `LogLevel` of the used `Logger`
    and must be greater or equal than the global `LogLevel`.

    Params:
      ll = The specific `LogLevel` used for logging the log message.
      msg = The format string used for this log call.
      args = The data that should be logged.

    Example:
    --------------------
    auto s = new FileLogger(stdout);
    s.logf(LogLevel.trace, "%d %s", 1337, "is number");
    s.logf(LogLevel.info, "%d %s", 1337, "is number");
    s.logf(LogLevel.warning, "%d %s", 1337, "is number");
    s.logf(LogLevel.error, "%d %s", 1337, "is number");
    s.logf(LogLevel.fatal, "%d %s", 1337, "is number");
    --------------------
    */
    void logf(int line = __LINE__, string file = __FILE__,
        string funcName = __FUNCTION__,
        string prettyFuncName = __PRETTY_FUNCTION__,
        string moduleName = __MODULE__, A...)(const LogLevel ll,
            lazy string msg, lazy A args)
    {
        synchronized (mutex)
        {
            import std.format.write : formattedWrite;

            if (isLoggingEnabled(ll, this.logLevel_, globalLogLevel))
            {
                this.beginLogMsg(file, line, funcName, prettyFuncName,
                    moduleName, ll, thisTid, Clock.currTime, this);

                auto writer = MsgRange(this);
                formattedWrite(writer, msg, args);

                this.finishLogMsg();

                if (ll == LogLevel.fatal)
                    this.fatalHandler_();
            }
        }
    }

    /** This function logs data to the used `Logger` depending on a
    condition with the `LogLevel` of the used `Logger` in a
    `printf`-style manner.

    In order for the resulting log message to be logged the `LogLevel`
    of the used `Logger` must be greater or equal than the global
    `LogLevel` and the condition must be `true`.

    Params:
      condition = The condition must be `true` for the data to be logged.
      msg = The format string used for this log call.
      args = The data that should be logged.

    Example:
    --------------------
    auto s = new FileLogger(stdout);
    s.logf(true ,"%d %s", 1337, "is number");
    s.logf(true ,"%d %s", 1337, "is number");
    s.logf(true ,"%d %s", 1337, "is number");
    s.logf(false ,"%d %s", 1337, "is number");
    s.logf(true ,"%d %s", 1337, "is number");
    --------------------
    */
    void logf(int line = __LINE__, string file = __FILE__,
        string funcName = __FUNCTION__,
        string prettyFuncName = __PRETTY_FUNCTION__,
        string moduleName = __MODULE__, A...)(lazy bool condition,
            lazy string msg, lazy A args)
    {
        synchronized (mutex)
        {
            import std.format.write : formattedWrite;

            if (isLoggingEnabled(this.logLevel_, this.logLevel_, globalLogLevel,
                condition))
            {
                this.beginLogMsg(file, line, funcName, prettyFuncName,
                    moduleName, this.logLevel_, thisTid, Clock.currTime, this);

                auto writer = MsgRange(this);
                formattedWrite(writer, msg, args);

                this.finishLogMsg();

                if (this.logLevel_ == LogLevel.fatal)
                    this.fatalHandler_();
            }
        }
    }

    /** This method logs data to the used `Logger` with the `LogLevel`
    of the this `Logger` in a `printf`-style manner.

    In order for the data to be processed the `LogLevel` of the `Logger`
    must be greater or equal to the global `LogLevel`.

    Params:
      msg = The format string used for this log call.
      args = The data that should be logged.

    Example:
    --------------------
    auto s = new FileLogger(stdout);
    s.logf("%d %s", 1337, "is number");
    s.logf("%d %s", 1337, "is number");
    s.logf("%d %s", 1337, "is number");
    s.logf("%d %s", 1337, "is number");
    s.logf("%d %s", 1337, "is number");
    --------------------
    */
    void logf(int line = __LINE__, string file = __FILE__,
        string funcName = __FUNCTION__,
        string prettyFuncName = __PRETTY_FUNCTION__,
        string moduleName = __MODULE__, A...)(lazy string msg, lazy A args)
    {
        synchronized (mutex)
        {
            import std.format.write : formattedWrite;

            if (isLoggingEnabled(this.logLevel_, this.logLevel_,
                globalLogLevel))
            {
                this.beginLogMsg(file, line, funcName, prettyFuncName,
                    moduleName, this.logLevel_, thisTid, Clock.currTime, this);

                auto writer = MsgRange(this);
                formattedWrite(writer, msg, args);

                this.finishLogMsg();

                if (this.logLevel_ == LogLevel.fatal)
                    this.fatalHandler_();
            }
        }
    }

    private void delegate() @safe fatalHandler_;
    private shared LogLevel logLevel_ = LogLevel.info;
    private Mutex mutex;

    protected Appender!string msgAppender;
    protected LogEntry header;
}

// Thread Global

private shared Logger stdSharedDefaultLogger;
private shared Logger stdSharedLogger;
private shared LogLevel stdLoggerGlobalLogLevel = LogLevel.all;

/* This method returns the global default Logger.
 * Marked @trusted because of excessive reliance on __gshared data
 */
private @property shared(Logger) defaultSharedLoggerImpl() @trusted
{
    import core.lifetime : emplace;
    import std.stdio : stderr;

    __gshared align(__traits(classInstanceAlignment, FileLogger))
        void[__traits(classInstanceSize, FileLogger)] _buffer = void;

    import std.concurrency : initOnce;
    initOnce!stdSharedDefaultLogger({
        auto buffer = cast(ubyte[]) _buffer;
        return cast(shared) emplace!(FileLogger)(buffer, stderr, LogLevel.info);
    }());

    return atomicLoad(stdSharedDefaultLogger);
}

/** This property sets and gets the default `Logger`. Unless set to another
logger by the user, the default logger's log level is LogLevel.info.

Example:
-------------
sharedLog = new FileLogger(yourFile);
-------------
The example sets a new `FileLogger` as new `sharedLog`.

If at some point you want to use the original default logger again, you can
use $(D sharedLog = null;). This will put back the original.

Note:
While getting and setting `sharedLog` is thread-safe, it has to be considered
that the returned reference is only a current snapshot and in the following
code, you must make sure no other thread reassigns to it between reading and
writing `sharedLog`.

`sharedLog` is only thread-safe if the used `Logger` is thread-safe.
The default `Logger` is thread-safe.
-------------
if (sharedLog !is myLogger)
    sharedLog = new myLogger;
-------------
*/
@property shared(Logger) sharedLog() @safe
{
    // If we have set up our own logger use that
    if (auto logger = atomicLoad!(MemoryOrder.seq)(stdSharedLogger))
    {
        return atomicLoad(logger);
    }
    else
    {
        // Otherwise resort to the default logger
        return defaultSharedLoggerImpl;
    }
}

/// Ditto
@property void sharedLog(shared(Logger) logger) @safe
{
    atomicStore!(MemoryOrder.seq)(stdSharedLogger, atomicLoad(logger));
}

/** This methods get and set the global `LogLevel`.

Every log message with a `LogLevel` lower as the global `LogLevel`
will be discarded before it reaches `writeLogMessage` method of any
`Logger`.
*/
/* Implementation note:
For any public logging call, the global log level shall only be queried once on
entry. Otherwise when another threads changes the level, we would work with
different levels at different spots in the code.
*/
@property LogLevel globalLogLevel() @safe @nogc
{
    return trustedLoad(stdLoggerGlobalLogLevel);
}

/// Ditto
@property void globalLogLevel(LogLevel ll) @safe
{
    trustedStore(stdLoggerGlobalLogLevel, ll);
}

// Thread Local

/** The `StdForwardLogger` will always forward anything to the sharedLog.

The `StdForwardLogger` will not throw if data is logged with $(D
LogLevel.fatal).
*/
class StdForwardLogger : Logger
{
    /** The default constructor for the `StdForwardLogger`.

    Params:
      lv = The `LogLevel` for the `MultiLogger`. By default the $(D
          LogLevel) is `all`.
    */
    this(const LogLevel lv = LogLevel.all) @safe
    {
        super(lv);
        this.fatalHandler = delegate() {};
    }

    override protected void writeLogMsg(ref LogEntry payload) @trusted
    {
        synchronized (sharedLog.mutex)
        {
            (cast() sharedLog).forwardMsg(payload);
        }
    }
}

///
@safe unittest
{
    auto nl1 = new StdForwardLogger(LogLevel.all);
}

@safe unittest
{
    import core.thread : Thread, msecs;

    static class RaceLogger : Logger
    {
        int value;
        this() @safe shared
        {
            super(LogLevel.init);
        }
        override void writeLogMsg(ref LogEntry payload) @safe
        {
            import core.thread : Thread, msecs;
            if (payload.msg == "foo")
            {
                value = 42;
                () @trusted { Thread.sleep(100.msecs); }();
                assert(value == 42, "Another thread changed the value");
            }
            else
            {
                () @trusted { Thread.sleep(50.msecs); } ();
                value = 13;
            }
        }
    }

    auto oldSharedLog = sharedLog;

    sharedLog = new shared RaceLogger;
    scope(exit)
    {
        sharedLog = oldSharedLog;
    }
    Thread toWaitFor;
    () @trusted { toWaitFor = new Thread(() { log("foo"); }).start(); }();
    log("bar");

    () @trusted
    {
        toWaitFor.join();
    }();
}

/** This `LogLevel` is unqiue to every thread.

The thread local `Logger` will use this `LogLevel` to filter log calls
every same way as presented earlier.
*/
//public LogLevel threadLogLevel = LogLevel.all;
private Logger stdLoggerThreadLogger;
private Logger stdLoggerDefaultThreadLogger;

/* This method returns the thread local default Logger.
*/
private @property Logger stdThreadLocalLogImpl() @trusted
{
    import core.lifetime : emplace;

    static align(__traits(classInstanceAlignment, StdForwardLogger))
        void[__traits(classInstanceSize, StdForwardLogger)] buffer;

    if (stdLoggerDefaultThreadLogger is null)
    {
        stdLoggerDefaultThreadLogger = emplace!StdForwardLogger(buffer, LogLevel.all);
    }
    return stdLoggerDefaultThreadLogger;
}

/** This function returns a thread unique `Logger`, that by default
propagates all data logged to it to the `sharedLog`.

These properties can be used to set and get this `Logger`. Every
modification to this `Logger` will only be visible in the thread the
modification has been done from.

This `Logger` is called by the free standing log functions. This allows to
create thread local redirections and still use the free standing log
functions.
*/
@property Logger stdThreadLocalLog() @safe
{
    // If we have set up our own logger use that
    if (auto logger = stdLoggerThreadLogger)
        return logger;
    else
        // Otherwise resort to the default logger
        return stdThreadLocalLogImpl;
}

/// Ditto
@property void stdThreadLocalLog(Logger logger) @safe
{
    stdLoggerThreadLogger = logger;
}

/// Ditto
@system unittest
{
    import std.logger.filelogger : FileLogger;
    import std.file : deleteme, remove;
    Logger l = stdThreadLocalLog;
    stdThreadLocalLog = new FileLogger(deleteme ~ "-someFile.log");
    scope(exit) remove(deleteme ~ "-someFile.log");

    auto tempLog = stdThreadLocalLog;
    stdThreadLocalLog = l;
    destroy(tempLog);
}

@safe unittest
{
    LogLevel ll = globalLogLevel;
    globalLogLevel = LogLevel.fatal;
    assert(globalLogLevel == LogLevel.fatal);
    globalLogLevel = ll;
}

package class TestLogger : Logger
{
    int line = -1;
    string file = null;
    string func = null;
    string prettyFunc = null;
    string msg = null;
    LogLevel lvl;

    this(const LogLevel lv = LogLevel.all) @safe
    {
        super(lv);
    }

    override protected void writeLogMsg(ref LogEntry payload) @safe
    {
        this.line = payload.line;
        this.file = payload.file;
        this.func = payload.funcName;
        this.prettyFunc = payload.prettyFuncName;
        this.lvl = payload.logLevel;
        this.msg = payload.msg;
    }
}

version (StdUnittest) private void testFuncNames(Logger logger) @safe
{
    string s = "I'm here";
    logger.log(s);
}

@safe unittest
{
    auto tl1 = new TestLogger();
    testFuncNames(tl1);
    assert(tl1.func == "std.logger.core.testFuncNames", tl1.func);
    assert(tl1.prettyFunc ==
        "void std.logger.core.testFuncNames(Logger logger) @safe",
        tl1.prettyFunc);
    assert(tl1.msg == "I'm here", tl1.msg);
}

@safe unittest
{
    auto tl1 = new TestLogger(LogLevel.all);
    tl1.log();
    assert(tl1.line == __LINE__ - 1);
    tl1.log(true);
    assert(tl1.line == __LINE__ - 1);
    tl1.log(false);
    assert(tl1.line == __LINE__ - 3);
    tl1.log(LogLevel.info);
    assert(tl1.line == __LINE__ - 1);
    tl1.log(LogLevel.off);
    assert(tl1.line == __LINE__ - 3);
    tl1.log(LogLevel.info, true);
    assert(tl1.line == __LINE__ - 1);
    tl1.log(LogLevel.info, false);
    assert(tl1.line == __LINE__ - 3);

    auto oldunspecificLogger = sharedLog;
    scope(exit) {
        sharedLog = atomicLoad(oldunspecificLogger);
    }

    () @trusted {
        sharedLog = cast(shared) tl1;
    }();

    log();
    assert(tl1.line == __LINE__ - 1);

    log(LogLevel.info);
    assert(tl1.line == __LINE__ - 1);

    log(true);
    assert(tl1.line == __LINE__ - 1);

    log(LogLevel.warning, true);
    assert(tl1.line == __LINE__ - 1);

    trace();
    assert(tl1.line == __LINE__ - 1);
}

@safe unittest
{
    import std.logger.multilogger : MultiLogger;

    auto tl1 = new TestLogger;
    auto tl2 = new TestLogger;

    auto ml = new MultiLogger();
    ml.insertLogger("one", tl1);
    ml.insertLogger("two", tl2);

    string msg = "Hello Logger World";
    ml.log(msg);
    int lineNumber = __LINE__ - 1;
    assert(tl1.msg == msg);
    assert(tl1.line == lineNumber);
    assert(tl2.msg == msg);
    assert(tl2.line == lineNumber);

    ml.removeLogger("one");
    ml.removeLogger("two");
    auto n = ml.removeLogger("one");
    assert(n is null);
}

@safe unittest
{
    bool errorThrown = false;
    auto tl = new TestLogger;
    auto dele = delegate() {
        errorThrown = true;
    };
    tl.fatalHandler = dele;
    tl.fatal();
    assert(errorThrown);
}

@safe unittest
{
    import std.conv : to;
    import std.exception : assertThrown, assertNotThrown;
    import std.format : format;

    auto l = new TestLogger(LogLevel.all);
    string msg = "Hello Logger World";
    l.log(msg);
    int lineNumber = __LINE__ - 1;
    assert(l.msg == msg);
    assert(l.line == lineNumber);
    assert(l.logLevel == LogLevel.all);

    l.log(true, msg);
    lineNumber = __LINE__ - 1;
    assert(l.msg == msg, l.msg);
    assert(l.line == lineNumber);
    assert(l.logLevel == LogLevel.all);

    l.log(false, msg);
    assert(l.msg == msg);
    assert(l.line == lineNumber, to!string(l.line));
    assert(l.logLevel == LogLevel.all);

    msg = "%s Another message";
    l.logf(msg, "Yet");
    lineNumber = __LINE__ - 1;
    assert(l.msg == msg.format("Yet"));
    assert(l.line == lineNumber);
    assert(l.logLevel == LogLevel.all);

    l.logf(true, msg, "Yet");
    lineNumber = __LINE__ - 1;
    assert(l.msg == msg.format("Yet"));
    assert(l.line == lineNumber);
    assert(l.logLevel == LogLevel.all);

    l.logf(false, msg, "Yet");
    assert(l.msg == msg.format("Yet"));
    assert(l.line == lineNumber);
    assert(l.logLevel == LogLevel.all);

    () @trusted {
        assertThrown!Throwable(l.logf(LogLevel.fatal, msg, "Yet"));
    } ();
    lineNumber = __LINE__ - 2;
    assert(l.msg == msg.format("Yet"));
    assert(l.line == lineNumber);
    assert(l.logLevel == LogLevel.all);

    () @trusted {
        assertThrown!Throwable(l.logf(LogLevel.fatal, true, msg, "Yet"));
    } ();
    lineNumber = __LINE__ - 2;
    assert(l.msg == msg.format("Yet"));
    assert(l.line == lineNumber);
    assert(l.logLevel == LogLevel.all);

    assertNotThrown(l.logf(LogLevel.fatal, false, msg, "Yet"));
    assert(l.msg == msg.format("Yet"));
    assert(l.line == lineNumber);
    assert(l.logLevel == LogLevel.all);

    Logger oldunspecificLogger;
    () @trusted {
        oldunspecificLogger = cast() sharedLog;
    }();

    assert(oldunspecificLogger.logLevel == LogLevel.info,
         to!string(oldunspecificLogger.logLevel));

    assert(l.logLevel == LogLevel.all);

    () @trusted {
        sharedLog = cast(shared) l;
    }();

    assert(globalLogLevel == LogLevel.all,
            to!string(globalLogLevel));

    scope(exit)
    {
        () @trusted {
            sharedLog = atomicLoad(cast(shared) oldunspecificLogger);
        }();
    }

    () @trusted {
        assert((cast() sharedLog).logLevel == LogLevel.all);
    }();

    assert(stdThreadLocalLog.logLevel == LogLevel.all);
    assert(globalLogLevel == LogLevel.all);

    msg = "Another message";
    log(msg);
    lineNumber = __LINE__ - 1;
    assert(l.logLevel == LogLevel.all);
    assert(l.line == lineNumber, to!string(l.line));
    assert(l.msg == msg, l.msg);

    log(true, msg);
    lineNumber = __LINE__ - 1;
    assert(l.msg == msg);
    assert(l.line == lineNumber);
    assert(l.logLevel == LogLevel.all);

    log(false, msg);
    assert(l.msg == msg);
    assert(l.line == lineNumber);
    assert(l.logLevel == LogLevel.all);

    msg = "%s Another message";
    logf(msg, "Yet");
    lineNumber = __LINE__ - 1;
    assert(l.msg == msg.format("Yet"));
    assert(l.line == lineNumber);
    assert(l.logLevel == LogLevel.all);

    logf(true, msg, "Yet");
    lineNumber = __LINE__ - 1;
    assert(l.msg == msg.format("Yet"));
    assert(l.line == lineNumber);
    assert(l.logLevel == LogLevel.all);

    logf(false, msg, "Yet");
    assert(l.msg == msg.format("Yet"));
    assert(l.line == lineNumber);
    assert(l.logLevel == LogLevel.all);

    msg = "%s Another message";
    () @trusted {
        assertThrown!Throwable(logf(LogLevel.fatal, msg, "Yet"));
    } ();
    lineNumber = __LINE__ - 2;
    assert(l.msg == msg.format("Yet"), l.msg);
    assert(l.line == lineNumber);
    assert(l.logLevel == LogLevel.all);

    () @trusted {
        assertThrown!Throwable(logf(LogLevel.fatal, true, msg, "Yet"));
    } ();
    lineNumber = __LINE__ - 2;
    assert(l.msg == msg.format("Yet"));
    assert(l.line == lineNumber);
    assert(l.logLevel == LogLevel.all);

    assertNotThrown(logf(LogLevel.fatal, false, msg, "Yet"));
    assert(l.msg == msg.format("Yet"));
    assert(l.line == lineNumber);
    assert(l.logLevel == LogLevel.all);
}

@system unittest // default logger
{
    import std.file : deleteme, exists, remove;
    import std.stdio : File;
    import std.string : indexOf;

    string filename = deleteme ~ __FUNCTION__ ~ ".tempLogFile";
    FileLogger l = new FileLogger(filename);
    auto oldunspecificLogger = sharedLog;

    sharedLog = cast(shared) l;

    scope(exit)
    {
        remove(filename);
        assert(!exists(filename));
        sharedLog = atomicLoad(oldunspecificLogger);
        globalLogLevel = LogLevel.all;
    }

    string notWritten = "this should not be written to file";
    string written = "this should be written to file";

    globalLogLevel = LogLevel.critical;
    assert(globalLogLevel == LogLevel.critical);

    log(LogLevel.warning, notWritten);
    log(LogLevel.critical, written);

    l.file.flush();
    l.file.close();

    auto file = File(filename, "r");
    assert(!file.eof);

    string readLine = file.readln();
    assert(readLine.indexOf(written) != -1, readLine);
    assert(readLine.indexOf(notWritten) == -1, readLine);
    file.close();
}

@system unittest
{
    import std.file : deleteme, remove;
    import std.stdio : File;
    import std.string : indexOf;

    string filename = deleteme ~ __FUNCTION__ ~ ".tempLogFile";
    auto oldunspecificLogger = sharedLog;

    scope(exit)
    {
        remove(filename);
        sharedLog = atomicLoad(oldunspecificLogger);
        globalLogLevel = LogLevel.all;
    }

    string notWritten = "this should not be written to file";
    string written = "this should be written to file";

    auto l = new FileLogger(filename);
    sharedLog = cast(shared) l;

    () @trusted {
        (cast() sharedLog).logLevel = LogLevel.critical;
    }();

    log(LogLevel.error, false, notWritten);
    log(LogLevel.critical, true, written);
    destroy(l);

    auto file = File(filename, "r");
    auto readLine = file.readln();
    assert(!readLine.empty, readLine);
    assert(readLine.indexOf(written) != -1);
    assert(readLine.indexOf(notWritten) == -1);
    file.close();
}

@safe unittest
{
    import std.conv : to;

    auto tl = new TestLogger(LogLevel.all);
    int l = __LINE__;
    tl.info("a");
    assert(tl.line == l+1);
    assert(tl.msg == "a");
    assert(tl.logLevel == LogLevel.all);
    assert(globalLogLevel == LogLevel.all);
    l = __LINE__;
    tl.trace("b");
    assert(tl.msg == "b", tl.msg);
    assert(tl.line == l+1, to!string(tl.line));
}

// testing possible log conditions
@safe unittest
{
    import std.conv : to;
    import std.format : format;
    import std.string : indexOf;

    auto oldunspecificLogger = sharedLog;

    auto mem = new TestLogger;
    mem.fatalHandler = delegate() {};

    () @trusted {
        sharedLog = cast(shared) mem;
    }();

    scope(exit)
    {
        sharedLog = atomicLoad(oldunspecificLogger);
        globalLogLevel = LogLevel.all;
    }

    int value = 0;
    foreach (gll; [cast(LogLevel) LogLevel.all, LogLevel.trace,
            LogLevel.info, LogLevel.warning, LogLevel.error,
            LogLevel.critical, LogLevel.fatal, LogLevel.off])
    {

        globalLogLevel = gll;

        foreach (ll; [cast(LogLevel) LogLevel.all, LogLevel.trace,
                LogLevel.info, LogLevel.warning, LogLevel.error,
                LogLevel.critical, LogLevel.fatal, LogLevel.off])
        {

            mem.logLevel = ll;

            foreach (cond; [true, false])
            {
                foreach (condValue; [true, false])
                {
                    foreach (memOrG; [true, false])
                    {
                        foreach (prntf; [true, false])
                        {
                            foreach (ll2; [cast(LogLevel) LogLevel.all, LogLevel.trace,
                                    LogLevel.info, LogLevel.warning,
                                    LogLevel.error, LogLevel.critical,
                                    LogLevel.fatal, LogLevel.off])
                            {
                                foreach (singleMulti; 0 .. 2)
                                {
                                    int lineCall;
                                    mem.msg = "-1";
                                    if (memOrG)
                                    {
                                        if (prntf)
                                        {
                                            if (cond)
                                            {
                                                if (singleMulti == 0)
                                                {
                                                    mem.logf(ll2, condValue, "%s",
                                                        value);
                                                    lineCall = __LINE__;
                                                }
                                                else
                                                {
                                                    mem.logf(ll2, condValue,
                                                        "%d %d", value, value);
                                                    lineCall = __LINE__;
                                                }
                                            }
                                            else
                                            {
                                                if (singleMulti == 0)
                                                {
                                                    mem.logf(ll2, "%s", value);
                                                    lineCall = __LINE__;
                                                }
                                                else
                                                {
                                                    mem.logf(ll2, "%d %d",
                                                        value, value);
                                                    lineCall = __LINE__;
                                                }
                                            }
                                        }
                                        else
                                        {
                                            if (cond)
                                            {
                                                if (singleMulti == 0)
                                                {
                                                    mem.log(ll2, condValue,
                                                        to!string(value));
                                                    lineCall = __LINE__;
                                                }
                                                else
                                                {
                                                    mem.log(ll2, condValue,
                                                        to!string(value), value);
                                                    lineCall = __LINE__;
                                                }
                                            }
                                            else
                                            {
                                                if (singleMulti == 0)
                                                {
                                                    mem.log(ll2, to!string(value));
                                                    lineCall = __LINE__;
                                                }
                                                else
                                                {
                                                    mem.log(ll2,
                                                        to!string(value),
                                                        value);
                                                    lineCall = __LINE__;
                                                }
                                            }
                                        }
                                    }
                                    else
                                    {
                                        if (prntf)
                                        {
                                            if (cond)
                                            {
                                                if (singleMulti == 0)
                                                {
                                                    logf(ll2, condValue, "%s",
                                                        value);
                                                    lineCall = __LINE__;
                                                }
                                                else
                                                {
                                                    logf(ll2, condValue,
                                                        "%s %d", value, value);
                                                    lineCall = __LINE__;
                                                }
                                            }
                                            else
                                            {
                                                if (singleMulti == 0)
                                                {
                                                    logf(ll2, "%s", value);
                                                    lineCall = __LINE__;
                                                }
                                                else
                                                {
                                                    logf(ll2, "%s %s", value,
                                                        value);
                                                    lineCall = __LINE__;
                                                }
                                            }
                                        }
                                        else
                                        {
                                            if (cond)
                                            {
                                                if (singleMulti == 0)
                                                {
                                                    log(ll2, condValue,
                                                        to!string(value));
                                                    lineCall = __LINE__;
                                                }
                                                else
                                                {
                                                    log(ll2, condValue, value,
                                                        to!string(value));
                                                    lineCall = __LINE__;
                                                }
                                            }
                                            else
                                            {
                                                if (singleMulti == 0)
                                                {
                                                    log(ll2, to!string(value));
                                                    lineCall = __LINE__;
                                                }
                                                else
                                                {
                                                    log(ll2, value,
                                                        to!string(value));
                                                    lineCall = __LINE__;
                                                }
                                            }
                                        }
                                    }

                                    string valueStr = to!string(value);
                                    ++value;

                                    bool ll2Off = (ll2 != LogLevel.off);
                                    bool gllOff = (gll != LogLevel.off);
                                    bool llOff = (ll != LogLevel.off);
                                    bool condFalse = (cond ? condValue : true);
                                    bool ll2VSgll = (ll2 >= gll);
                                    bool ll2VSll = (ll2 >= ll);

                                    bool shouldLog = ll2Off && gllOff && llOff
                                        && condFalse && ll2VSgll && ll2VSll;

                                    /*
                                    writefln(
                                        "go(%b) ll2o(%b) c(%b) lg(%b) ll(%b) s(%b)"
                                        , gll != LogLevel.off, ll2 != LogLevel.off,
                                        cond ? condValue : true,
                                        ll2 >= gll, ll2 >= ll, shouldLog);
                                    */


                                    if (shouldLog)
                                    {
                                        assert(mem.msg.indexOf(valueStr) != -1,
                                            format(
                                            "lineCall(%d) ll2Off(%u) gll(%u) ll(%u) ll2(%u) " ~
                                            "cond(%b) condValue(%b)" ~
                                            " memOrG(%b) shouldLog(%b) %s == %s" ~
                                            " %b %b %b %b %b",
                                            lineCall, ll2Off, gll, ll, ll2, cond,
                                            condValue, memOrG, shouldLog, mem.msg,
                                            valueStr, gllOff, llOff, condFalse,
                                            ll2VSgll, ll2VSll
                                        ));
                                    }
                                    else
                                    {
                                        assert(mem.msg.indexOf(valueStr),
                                            format(
                                            "lineCall(%d) ll2Off(%u) gll(%u) ll(%u) ll2(%u) " ~
                                            "cond(%b) condValue(%b)" ~
                                            " memOrG(%b) shouldLog(%b) %s == %s" ~
                                            " %b %b %b %b %b",
                                            lineCall, ll2Off, gll, ll, ll2, cond,
                                            condValue, memOrG, shouldLog, mem.msg,
                                            valueStr, gllOff, llOff, condFalse,
                                            ll2VSgll, ll2VSll
                                        ));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

// more testing
@safe unittest
{
    import std.conv : to;
    import std.format : format;
    import std.string : indexOf;

    auto oldunspecificLogger = sharedLog;

    auto mem = new TestLogger;
    mem.fatalHandler = delegate() {};

    () @trusted {
        sharedLog = cast(shared) mem;
    }();

    scope(exit)
    {
        sharedLog = atomicLoad(oldunspecificLogger);
        globalLogLevel = LogLevel.all;
    }

    int value = 0;
    foreach (gll; [cast(LogLevel) LogLevel.all, LogLevel.trace,
            LogLevel.info, LogLevel.warning, LogLevel.error,
            LogLevel.critical, LogLevel.fatal, LogLevel.off])
    {

        globalLogLevel = gll;

        foreach (ll; [cast(LogLevel) LogLevel.all, LogLevel.trace,
                LogLevel.info, LogLevel.warning, LogLevel.error,
                LogLevel.critical, LogLevel.fatal, LogLevel.off])
        {
            mem.logLevel = ll;

            foreach (tll; [cast(LogLevel) LogLevel.all, LogLevel.trace,
                    LogLevel.info, LogLevel.warning, LogLevel.error,
                    LogLevel.critical, LogLevel.fatal, LogLevel.off])
            {
                stdThreadLocalLog.logLevel = tll;

                foreach (cond; [true, false])
                {
                    foreach (condValue; [true, false])
                    {
                        foreach (memOrG; [true, false])
                        {
                            foreach (prntf; [true, false])
                            {
                                foreach (singleMulti; 0 .. 2)
                                {
                                    int lineCall;
                                    mem.msg = "-1";
                                    if (memOrG)
                                    {
                                        if (prntf)
                                        {
                                            if (cond)
                                            {
                                                if (singleMulti == 0)
                                                {
                                                    mem.logf(condValue, "%s",
                                                        value);
                                                    lineCall = __LINE__;
                                                }
                                                else
                                                {
                                                    mem.logf(condValue,
                                                        "%d %d", value, value);
                                                    lineCall = __LINE__;
                                                }
                                            }
                                            else
                                            {
                                                if (singleMulti == 0)
                                                {
                                                    mem.logf("%s", value);
                                                    lineCall = __LINE__;
                                                }
                                                else
                                                {
                                                    mem.logf("%d %d",
                                                        value, value);
                                                    lineCall = __LINE__;
                                                }
                                            }
                                        }
                                        else
                                        {
                                            if (cond)
                                            {
                                                if (singleMulti == 0)
                                                {
                                                    mem.log(condValue,
                                                        to!string(value));
                                                    lineCall = __LINE__;
                                                }
                                                else
                                                {
                                                    mem.log(condValue,
                                                        to!string(value), value);
                                                    lineCall = __LINE__;
                                                }
                                            }
                                            else
                                            {
                                                if (singleMulti == 0)
                                                {
                                                    mem.log(to!string(value));
                                                    lineCall = __LINE__;
                                                }
                                                else
                                                {
                                                    mem.log(to!string(value),
                                                        value);
                                                    lineCall = __LINE__;
                                                }
                                            }
                                        }
                                    }
                                    else
                                    {
                                        if (prntf)
                                        {
                                            if (cond)
                                            {
                                                if (singleMulti == 0)
                                                {
                                                    logf(condValue, "%s", value);
                                                    lineCall = __LINE__;
                                                }
                                                else
                                                {
                                                    logf(condValue, "%s %d", value,
                                                        value);
                                                    lineCall = __LINE__;
                                                }
                                            }
                                            else
                                            {
                                                if (singleMulti == 0)
                                                {
                                                    logf("%s", value);
                                                    lineCall = __LINE__;
                                                }
                                                else
                                                {
                                                    logf("%s %s", value, value);
                                                    lineCall = __LINE__;
                                                }
                                            }
                                        }
                                        else
                                        {
                                            if (cond)
                                            {
                                                if (singleMulti == 0)
                                                {
                                                    log(condValue,
                                                        to!string(value));
                                                    lineCall = __LINE__;
                                                }
                                                else
                                                {
                                                    log(condValue, value,
                                                        to!string(value));
                                                    lineCall = __LINE__;
                                                }
                                            }
                                            else
                                            {
                                                if (singleMulti == 0)
                                                {
                                                    log(to!string(value));
                                                    lineCall = __LINE__;
                                                }
                                                else
                                                {
                                                    log(value, to!string(value));
                                                    lineCall = __LINE__;
                                                }
                                            }
                                        }
                                    }

                                    string valueStr = to!string(value);
                                    ++value;

                                    bool gllOff = (gll != LogLevel.off);
                                    bool llOff = (ll != LogLevel.off);
                                    bool tllOff = (tll != LogLevel.off);
                                    bool llVSgll = (ll >= gll);
                                    bool tllVSll =
                                        (stdThreadLocalLog.logLevel >= ll);
                                    bool condFalse = (cond ? condValue : true);

                                    bool shouldLog = gllOff && llOff
                                        && (memOrG ? true : tllOff)
                                        && (memOrG ?
                                            (ll >= gll) :
                                            (tll >= gll && tll >= ll))
                                        && condFalse;

                                    if (shouldLog)
                                    {
                                        assert(mem.msg.indexOf(valueStr) != -1,
                                            format("\ngll(%s) ll(%s) tll(%s) " ~
                                                "cond(%s) condValue(%s) " ~
                                                "memOrG(%s) prntf(%s) " ~
                                                "singleMulti(%s)",
                                                gll, ll, tll, cond, condValue,
                                                memOrG, prntf, singleMulti)
                                            ~ format(" gllOff(%s) llOff(%s) " ~
                                                "llVSgll(%s) tllVSll(%s) " ~
                                                "tllOff(%s) condFalse(%s) "
                                                ~ "shoudlLog(%s)",
                                                gll != LogLevel.off,
                                                ll != LogLevel.off, llVSgll,
                                                tllVSll, tllOff, condFalse,
                                                shouldLog)
                                            ~ format("msg(%s) line(%s) " ~
                                                "lineCall(%s) valueStr(%s)",
                                                mem.msg, mem.line, lineCall,
                                                valueStr)
                                        );
                                    }
                                    else
                                    {
                                        assert(mem.msg.indexOf(valueStr) == -1,
                                            format("\ngll(%s) ll(%s) tll(%s) " ~
                                                "cond(%s) condValue(%s) " ~
                                                "memOrG(%s) prntf(%s) " ~
                                                "singleMulti(%s)",
                                                gll, ll, tll, cond, condValue,
                                                memOrG, prntf, singleMulti)
                                            ~ format(" gllOff(%s) llOff(%s) " ~
                                                "llVSgll(%s) tllVSll(%s) " ~
                                                "tllOff(%s) condFalse(%s) "
                                                ~ "shoudlLog(%s)",
                                                gll != LogLevel.off,
                                                ll != LogLevel.off, llVSgll,
                                                tllVSll, tllOff, condFalse,
                                                shouldLog)
                                            ~ format("msg(%s) line(%s) " ~
                                                "lineCall(%s) valueStr(%s)",
                                                mem.msg, mem.line, lineCall,
                                                valueStr)
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

// testing more possible log conditions
@safe unittest
{
    bool fatalLog;
    auto mem = new TestLogger;
    mem.fatalHandler = delegate() { fatalLog = true; };
    auto oldunspecificLogger = sharedLog;

    stdThreadLocalLog.logLevel = LogLevel.all;

    () @trusted {
        sharedLog = cast(shared) mem;
    }();

    scope(exit)
    {
        sharedLog = atomicLoad(oldunspecificLogger);
        globalLogLevel = LogLevel.all;
    }

    foreach (gll; [cast(LogLevel) LogLevel.all, LogLevel.trace,
            LogLevel.info, LogLevel.warning, LogLevel.error,
            LogLevel.critical, LogLevel.fatal, LogLevel.off])
    {

        globalLogLevel = gll;

        foreach (ll; [cast(LogLevel) LogLevel.all, LogLevel.trace,
                LogLevel.info, LogLevel.warning, LogLevel.error,
                LogLevel.critical, LogLevel.fatal, LogLevel.off])
        {
            mem.logLevel = ll;

            foreach (tll; [cast(LogLevel) LogLevel.all, LogLevel.trace,
                    LogLevel.info, LogLevel.warning, LogLevel.error,
                    LogLevel.critical, LogLevel.fatal, LogLevel.off])
            {
                stdThreadLocalLog.logLevel = tll;

                foreach (cond; [true, false])
                {
                    assert(globalLogLevel == gll);
                    assert(mem.logLevel == ll);

                    bool gllVSll = LogLevel.trace >= globalLogLevel;
                    bool llVSgll = ll >= globalLogLevel;
                    bool lVSll = LogLevel.trace >= ll;
                    bool gllOff = globalLogLevel != LogLevel.off;
                    bool llOff = mem.logLevel != LogLevel.off;
                    bool tllOff = stdThreadLocalLog.logLevel != LogLevel.off;
                    bool tllVSll = tll >= ll;
                    bool tllVSgll = tll >= gll;
                    bool lVSgll = LogLevel.trace >= tll;

                    bool test = llVSgll && gllVSll && lVSll && gllOff && llOff && cond;
                    bool testG = gllOff && llOff && tllOff && lVSgll && tllVSll && tllVSgll && cond;

                    mem.line = -1;
                    /*
                    writefln("gll(%3u) ll(%3u) cond(%b) test(%b)",
                        gll, ll, cond, test);
                    writefln("%b %b %b %b %b %b test2(%b)", llVSgll, gllVSll, lVSll,
                        gllOff, llOff, cond, test2);
                    */

                    mem.trace(__LINE__); int line = __LINE__;
                    assert(test ? mem.line == line : true); line = -1;

                    trace(__LINE__); line = __LINE__;
                    assert(testG ? mem.line == line : true); line = -1;

                    mem.trace(cond, __LINE__); line = __LINE__;
                    assert(test ? mem.line == line : true); line = -1;

                    trace(cond, __LINE__); line = __LINE__;
                    assert(testG ? mem.line == line : true); line = -1;

                    mem.tracef("%d", __LINE__); line = __LINE__;
                    assert(test ? mem.line == line : true); line = -1;

                    tracef("%d", __LINE__); line = __LINE__;
                    assert(testG ? mem.line == line : true); line = -1;

                    mem.tracef(cond, "%d", __LINE__); line = __LINE__;
                    assert(test ? mem.line == line : true); line = -1;

                    tracef(cond, "%d", __LINE__); line = __LINE__;
                    assert(testG ? mem.line == line : true); line = -1;

                    llVSgll = ll >= globalLogLevel;
                    lVSll = LogLevel.info >= ll;
                    lVSgll = LogLevel.info >= tll;
                    test = llVSgll && gllVSll && lVSll && gllOff && llOff && cond;
                    testG = gllOff && llOff && tllOff && tllVSll && tllVSgll &&
                        lVSgll && cond;

                    mem.info(__LINE__); line = __LINE__;
                    assert(test ? mem.line == line : true); line = -1;

                    info(__LINE__); line = __LINE__;
                    assert(testG ? mem.line == line : true); line = -1;

                    mem.info(cond, __LINE__); line = __LINE__;
                    assert(test ? mem.line == line : true); line = -1;

                    info(cond, __LINE__); line = __LINE__;
                    assert(testG ? mem.line == line : true); line = -1;

                    mem.infof("%d", __LINE__); line = __LINE__;
                    assert(test ? mem.line == line : true); line = -1;

                    infof("%d", __LINE__); line = __LINE__;
                    assert(testG ? mem.line == line : true); line = -1;

                    mem.infof(cond, "%d", __LINE__); line = __LINE__;
                    assert(test ? mem.line == line : true); line = -1;

                    infof(cond, "%d", __LINE__); line = __LINE__;
                    assert(testG ? mem.line == line : true); line = -1;

                    llVSgll = ll >= globalLogLevel;
                    lVSll = LogLevel.warning >= ll;
                    lVSgll = LogLevel.warning >= tll;
                    test = llVSgll && gllVSll && lVSll && gllOff && llOff && cond;
                    testG = gllOff && llOff && tllOff && tllVSll && tllVSgll &&
                        lVSgll && cond;

                    mem.warning(__LINE__); line = __LINE__;
                    assert(test ? mem.line == line : true); line = -1;

                    warning(__LINE__); line = __LINE__;
                    assert(testG ? mem.line == line : true); line = -1;

                    mem.warning(cond, __LINE__); line = __LINE__;
                    assert(test ? mem.line == line : true); line = -1;

                    warning(cond, __LINE__); line = __LINE__;
                    assert(testG ? mem.line == line : true); line = -1;

                    mem.warningf("%d", __LINE__); line = __LINE__;
                    assert(test ? mem.line == line : true); line = -1;

                    warningf("%d", __LINE__); line = __LINE__;
                    assert(testG ? mem.line == line : true); line = -1;

                    mem.warningf(cond, "%d", __LINE__); line = __LINE__;
                    assert(test ? mem.line == line : true); line = -1;

                    warningf(cond, "%d", __LINE__); line = __LINE__;
                    assert(testG ? mem.line == line : true); line = -1;

                    llVSgll = ll >= globalLogLevel;
                    lVSll = LogLevel.critical >= ll;
                    lVSgll = LogLevel.critical >= tll;
                    test = llVSgll && gllVSll && lVSll && gllOff && llOff && cond;
                    testG = gllOff && llOff && tllOff && tllVSll && tllVSgll &&
                        lVSgll && cond;

                    mem.critical(__LINE__); line = __LINE__;
                    assert(test ? mem.line == line : true); line = -1;

                    critical(__LINE__); line = __LINE__;
                    assert(testG ? mem.line == line : true); line = -1;

                    mem.critical(cond, __LINE__); line = __LINE__;
                    assert(test ? mem.line == line : true); line = -1;

                    critical(cond, __LINE__); line = __LINE__;
                    assert(testG ? mem.line == line : true); line = -1;

                    mem.criticalf("%d", __LINE__); line = __LINE__;
                    assert(test ? mem.line == line : true); line = -1;

                    criticalf("%d", __LINE__); line = __LINE__;
                    assert(testG ? mem.line == line : true); line = -1;

                    mem.criticalf(cond, "%d", __LINE__); line = __LINE__;
                    assert(test ? mem.line == line : true); line = -1;

                    criticalf(cond, "%d", __LINE__); line = __LINE__;
                    assert(testG ? mem.line == line : true); line = -1;

                    llVSgll = ll >= globalLogLevel;
                    lVSll = LogLevel.fatal >= ll;
                    lVSgll = LogLevel.fatal >= tll;
                    test = llVSgll && gllVSll && lVSll && gllOff && llOff && cond;
                    testG = gllOff && llOff && tllOff && tllVSll && tllVSgll &&
                        lVSgll && cond;

                    mem.fatal(__LINE__); line = __LINE__;
                    assert(test ? mem.line == line : true); line = -1;
                    assert(test ? fatalLog : true);
                    fatalLog = false;

                    fatal(__LINE__); line = __LINE__;
                    assert(testG ? mem.line == line : true); line = -1;
                    assert(testG ? fatalLog : true);
                    fatalLog = false;

                    mem.fatal(cond, __LINE__); line = __LINE__;
                    assert(test ? mem.line == line : true); line = -1;
                    assert(test ? fatalLog : true);
                    fatalLog = false;

                    fatal(cond, __LINE__); line = __LINE__;
                    assert(testG ? mem.line == line : true); line = -1;
                    assert(testG ? fatalLog : true);
                    fatalLog = false;

                    mem.fatalf("%d", __LINE__); line = __LINE__;
                    assert(test ? mem.line == line : true); line = -1;
                    assert(test ? fatalLog : true);
                    fatalLog = false;

                    fatalf("%d", __LINE__); line = __LINE__;
                    assert(testG ? mem.line == line : true); line = -1;
                    assert(testG ? fatalLog : true);
                    fatalLog = false;

                    mem.fatalf(cond, "%d", __LINE__); line = __LINE__;
                    assert(test ? mem.line == line : true); line = -1;
                    assert(test ? fatalLog : true);
                    fatalLog = false;

                    fatalf(cond, "%d", __LINE__); line = __LINE__;
                    assert(testG ? mem.line == line : true); line = -1;
                    assert(testG ? fatalLog : true);
                    fatalLog = false;
                }
            }
        }
    }
}

// Issue #5
@safe unittest
{
    import std.string : indexOf;

    auto oldunspecificLogger = sharedLog;

    scope(exit)
    {
        sharedLog = atomicLoad(oldunspecificLogger);
        globalLogLevel = LogLevel.all;
    }

    auto tl = new TestLogger(LogLevel.info);

    () @trusted {
        sharedLog = cast(shared) tl;
    }();

    trace("trace");
    assert(tl.msg.indexOf("trace") == -1);
}

// Issue #5
@safe unittest
{
    import std.logger.multilogger : MultiLogger;
    import std.string : indexOf;

    stdThreadLocalLog.logLevel = LogLevel.all;

    auto oldunspecificLogger = sharedLog;

    scope(exit)
    {
        sharedLog = atomicLoad(oldunspecificLogger);
        globalLogLevel = LogLevel.all;
    }

    auto logger = new MultiLogger(LogLevel.error);

    auto tl = new TestLogger(LogLevel.info);
    logger.insertLogger("required", tl);

    () @trusted {
        sharedLog = cast(shared) logger;
    }();

    trace("trace");
    assert(tl.msg.indexOf("trace") == -1);
    info("info");
    assert(tl.msg.indexOf("info") == -1);
    error("error");
    assert(tl.msg.indexOf("error") == 0);
}

@system unittest
{
    import std.exception : assertThrown;
    auto tl = new TestLogger();
    assertThrown!Throwable(tl.fatal("fatal"));
}

// log objects with non-safe toString
@system unittest
{
    struct Test
    {
        string toString() const @system
        {
            return "test";
        }
    }

    auto tl = new TestLogger();
    tl.info(Test.init);
    assert(tl.msg == "test");
}

// Workaround for atomics not allowed in @safe code
private auto trustedLoad(T)(ref shared T value) @trusted
{
    return atomicLoad!(MemoryOrder.acq)(value);
}

// ditto
private void trustedStore(T)(ref shared T dst, ref T src) @trusted
{
    atomicStore!(MemoryOrder.rel)(dst, src);
}

// check that thread-local logging does not propagate
// to shared logger
@system unittest
{
    import core.thread, std.concurrency;

    static shared logged_count = 0;

    class TestLog : Logger
    {
        Tid tid;

        this()
        {
            super (LogLevel.trace);
            this.tid = thisTid;
        }

        override void writeLogMsg(ref LogEntry payload) @trusted
        {
            assert(thisTid == this.tid);
            atomicOp!"+="(logged_count, 1);
        }
    }

    class IgnoredLog : Logger
    {
        this()
        {
            super (LogLevel.trace);
        }

        override void writeLogMsg(ref LogEntry payload) @trusted
        {
            assert(false);
        }
    }

    auto oldSharedLog = sharedLog;
    scope(exit)
    {
        sharedLog = atomicLoad(oldSharedLog);
    }

    () @trusted {
        sharedLog = cast(shared) new IgnoredLog;
    }();

    Thread[] spawned;

    foreach (i; 0 .. 4)
    {
        spawned ~= new Thread({
            stdThreadLocalLog = new TestLog;
            trace("zzzzzzzzzz");
        });
        spawned[$-1].start();
    }

    foreach (t; spawned)
        t.join();

    assert(atomicOp!"=="(logged_count, 4));
}

@safe unittest
{
    auto dl = () @trusted {
        return cast(FileLogger) cast() sharedLog;
    }();
    assert(dl !is null);
    assert(dl.logLevel == LogLevel.info);
    assert(globalLogLevel == LogLevel.all);

    auto tl = cast(StdForwardLogger) stdThreadLocalLog;
    assert(tl !is null);
    stdThreadLocalLog.logLevel = LogLevel.all;
}

// https://issues.dlang.org/show_bug.cgi?id=14940
@safe unittest
{
    import std.typecons : Nullable;

    Nullable!int a = 1;
    auto l = new TestLogger();
    l.infof("log: %s", a);
    assert(l.msg == "log: 1");
}

// Ensure @system toString methods work
@system unittest
{
    enum SystemToStringMsg = "SystemToString";
    static struct SystemToString
    {
        string toString() @system
        {
            return SystemToStringMsg;
        }
    }

    auto tl = new TestLogger();

    SystemToString sts;
    tl.logf("%s", sts);
    assert(tl.msg == SystemToStringMsg);
}

// https://issues.dlang.org/show_bug.cgi?id=17328
@safe unittest
{
    import std.format : format;

    ubyte[] data = [0];
    string s = format("%(%02x%)", data); // format 00
    assert(s == "00");

    auto tl = new TestLogger();

    tl.infof("%(%02x%)", data);    // infof    000

    size_t i;
    string fs = tl.msg;
    for (; i < s.length; ++i)
    {
        assert(s[s.length - 1 - i] == fs[fs.length - 1 - i], fs);
    }
    assert(fs.length == 2);
}

// https://issues.dlang.org/show_bug.cgi?id=15954
@safe unittest
{
    import std.conv : to;
    auto tl = new TestLogger();
    tl.log("123456789".to!wstring);
    assert(tl.msg == "123456789");
}

// https://issues.dlang.org/show_bug.cgi?id=16256
@safe unittest
{
    import std.conv : to;
    auto tl = new TestLogger();
    tl.log("123456789"d);
    assert(tl.msg == "123456789");
}

// https://issues.dlang.org/show_bug.cgi?id=15517
@system unittest
{
    import std.file : exists, remove, tempDir;
    import std.path : buildPath;
    import std.stdio : File;
    import std.string : indexOf;

    string fn = tempDir.buildPath("logfile.log");
    if (exists(fn))
    {
        remove(fn);
    }

    auto oldShared = sharedLog;
    scope(exit)
    {
        sharedLog = atomicLoad(oldShared);
        if (exists(fn))
        {
            remove(fn);
        }
    }

    auto ts = [ "Test log 1", "Test log 2", "Test log 3"];

    auto fl = new FileLogger(fn);

    () @trusted {
        sharedLog = cast(shared) fl;
    }();

    assert(exists(fn));

    foreach (t; ts)
    {
        log(t);
    }

    auto f = File(fn);
    auto l = f.byLine();
    assert(!l.empty);
    size_t idx;
    foreach (it; l)
    {
        assert(it.indexOf(ts[idx]) != -1, it);
        ++idx;
    }

    assert(exists(fn));
    fl.file.close();
}

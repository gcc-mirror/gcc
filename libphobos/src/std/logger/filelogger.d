// Written in the D programming language.
/**
Source: $(PHOBOSSRC std/logger/filelogger.d)
*/
module std.logger.filelogger;

import std.logger.core;
import std.stdio;

import std.typecons : Flag;

/** An option to create $(LREF FileLogger) directory if it is non-existent.
*/
alias CreateFolder = Flag!"CreateFolder";

/** This `Logger` implementation writes log messages to the associated
file. The name of the file has to be passed on construction time. If the file
is already present new log messages will be append at its end.
*/
class FileLogger : Logger
{
    import std.concurrency : Tid;
    import std.datetime.systime : SysTime;
    import std.format.write : formattedWrite;

    /** A constructor for the `FileLogger` Logger.

    Params:
      fn = The filename of the output file of the `FileLogger`. If that
      file can not be opened for writting an exception will be thrown.
      lv = The `LogLevel` for the `FileLogger`. By default the

    Example:
    -------------
    auto l1 = new FileLogger("logFile");
    auto l2 = new FileLogger("logFile", LogLevel.fatal);
    auto l3 = new FileLogger("logFile", LogLevel.fatal, CreateFolder.yes);
    -------------
    */
    this(this This)(const string fn, const LogLevel lv = LogLevel.all)
    {
         this(fn, lv, CreateFolder.yes);
    }

    /** A constructor for the `FileLogger` Logger that takes a reference to
    a `File`.

    The `File` passed must be open for all the log call to the
    `FileLogger`. If the `File` gets closed, using the `FileLogger`
    for logging will result in undefined behaviour.

    Params:
      fn = The file used for logging.
      lv = The `LogLevel` for the `FileLogger`. By default the
      `LogLevel` for `FileLogger` is `LogLevel.all`.
      createFileNameFolder = if yes and fn contains a folder name, this
      folder will be created.

    Example:
    -------------
    auto file = File("logFile.log", "w");
    auto l1 = new FileLogger(file);
    auto l2 = new FileLogger(file, LogLevel.fatal);
    -------------
    */
    this(this This)(const string fn, const LogLevel lv, CreateFolder createFileNameFolder)
    {
        import std.file : exists, mkdirRecurse;
        import std.path : dirName;
        import std.conv : text;

        super(lv);
        this.filename = fn;

        if (createFileNameFolder)
        {
            auto d = dirName(this.filename);
            mkdirRecurse(d);
            assert(exists(d), text("The folder the FileLogger should have",
                                   " created in '", d,"' could not be created."));
        }

        // Cast away `shared` when the constructor is inferred shared.
        () @trusted { (cast() this.file_).open(this.filename, "a"); }();
    }

    /** A constructor for the `FileLogger` Logger that takes a reference to
    a `File`.

    The `File` passed must be open for all the log call to the
    `FileLogger`. If the `File` gets closed, using the `FileLogger`
    for logging will result in undefined behaviour.

    Params:
      file = The file used for logging.
      lv = The `LogLevel` for the `FileLogger`. By default the
      `LogLevel` for `FileLogger` is `LogLevel.all`.

    Example:
    -------------
    auto file = File("logFile.log", "w");
    auto l1 = new FileLogger(file);
    auto l2 = new FileLogger(file, LogLevel.fatal);
    -------------
    */
    this(File file, const LogLevel lv = LogLevel.all) @safe
    {
        super(lv);
        this.file_ = file;
    }

    /** If the `FileLogger` is managing the `File` it logs to, this
    method will return a reference to this File.
    */
    @property File file() @safe
    {
        return this.file_;
    }

    /* This method overrides the base class method in order to log to a file
    without requiring heap allocated memory. Additionally, the `FileLogger`
    local mutex is logged to serialize the log calls.
    */
    override protected void beginLogMsg(string file, int line, string funcName,
        string prettyFuncName, string moduleName, LogLevel logLevel,
        Tid threadId, SysTime timestamp, Logger logger)
        @safe
    {
        import std.string : lastIndexOf;
        ptrdiff_t fnIdx = file.lastIndexOf('/') + 1;
        ptrdiff_t funIdx = funcName.lastIndexOf('.') + 1;

        auto lt = this.file_.lockingTextWriter();
        systimeToISOString(lt, timestamp);
        import std.conv : to;
        formattedWrite(lt, " [%s] %s:%u:%s ", logLevel.to!string,
                file[fnIdx .. $], line, funcName[funIdx .. $]);
    }

    /* This methods overrides the base class method and writes the parts of
    the log call directly to the file.
    */
    override protected void logMsgPart(scope const(char)[] msg)
    {
        formattedWrite(this.file_.lockingTextWriter(), "%s", msg);
    }

    /* This methods overrides the base class method and finalizes the active
    log call. This requires flushing the `File` and releasing the
    `FileLogger` local mutex.
    */
    override protected void finishLogMsg()
    {
        this.file_.lockingTextWriter().put("\n");
        this.file_.flush();
    }

    /* This methods overrides the base class method and delegates the
    `LogEntry` data to the actual implementation.
    */
    override protected void writeLogMsg(ref LogEntry payload)
    {
        this.beginLogMsg(payload.file, payload.line, payload.funcName,
            payload.prettyFuncName, payload.moduleName, payload.logLevel,
            payload.threadId, payload.timestamp, payload.logger);
        this.logMsgPart(payload.msg);
        this.finishLogMsg();
    }

    /** If the `FileLogger` was constructed with a filename, this method
    returns this filename. Otherwise an empty `string` is returned.
    */
    string getFilename()
    {
        return this.filename;
    }

    /** The `File` log messages are written to. */
    protected File file_;

    /** The filename of the `File` log messages are written to. */
    protected string filename;
}

@system unittest
{
    import std.array : empty;
    import std.file : deleteme, remove;
    import std.string : indexOf;

    string filename = deleteme ~ __FUNCTION__ ~ ".tempLogFile";
    auto l = new FileLogger(filename);

    scope(exit)
    {
        remove(filename);
    }

    string notWritten = "this should not be written to file";
    string written = "this should be written to file";

    l.logLevel = LogLevel.critical;
    l.log(LogLevel.warning, notWritten);
    l.log(LogLevel.critical, written);
    destroy(l);

    auto file = File(filename, "r");
    string readLine = file.readln();
    assert(readLine.indexOf(written) != -1, readLine);
    readLine = file.readln();
    assert(readLine.indexOf(notWritten) == -1, readLine);
}

@safe unittest
{
    import std.file : rmdirRecurse, exists, deleteme;
    import std.path : dirName;

    const string tmpFolder = dirName(deleteme);
    const string filepath = tmpFolder ~ "/bug15771/minas/oops/";
    const string filename = filepath ~ "output.txt";
    assert(!exists(filepath));

    auto f = new FileLogger(filename, LogLevel.all, CreateFolder.yes);
    scope(exit) () @trusted { rmdirRecurse(tmpFolder ~ "/bug15771"); }();

    f.log("Hello World!");
    assert(exists(filepath));
    f.file.close();
}

@system unittest
{
    import std.array : empty;
    import std.file : deleteme, remove;
    import std.string : indexOf;

    string filename = deleteme ~ __FUNCTION__ ~ ".tempLogFile";
    auto file = File(filename, "w");
    auto l = new FileLogger(file);

    scope(exit)
    {
        remove(filename);
    }

    string notWritten = "this should not be written to file";
    string written = "this should be written to file";

    l.logLevel = LogLevel.critical;
    l.log(LogLevel.warning, notWritten);
    l.log(LogLevel.critical, written);
    file.close();

    file = File(filename, "r");
    string readLine = file.readln();
    assert(readLine.indexOf(written) != -1, readLine);
    readLine = file.readln();
    assert(readLine.indexOf(notWritten) == -1, readLine);
    file.close();
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

@safe unittest
{
    // we don't need to actually run the code, only make sure
    // it compiles
    static void _() {
        auto l = new shared FileLogger("");
    }
}

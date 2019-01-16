///
module std.experimental.logger.filelogger;

import std.experimental.logger.core;
import std.stdio;

import std.typecons : Flag;

/** An option to create $(LREF FileLogger) directory if it is non-existent.
*/
alias CreateFolder = Flag!"CreateFolder";

/** This $(D Logger) implementation writes log messages to the associated
file. The name of the file has to be passed on construction time. If the file
is already present new log messages will be append at its end.
*/
class FileLogger : Logger
{
    import std.concurrency : Tid;
    import std.datetime.systime : SysTime;
    import std.format : formattedWrite;

    /** A constructor for the $(D FileLogger) Logger.

    Params:
      fn = The filename of the output file of the $(D FileLogger). If that
      file can not be opened for writting an exception will be thrown.
      lv = The $(D LogLevel) for the $(D FileLogger). By default the

    Example:
    -------------
    auto l1 = new FileLogger("logFile");
    auto l2 = new FileLogger("logFile", LogLevel.fatal);
    auto l3 = new FileLogger("logFile", LogLevel.fatal, CreateFolder.yes);
    -------------
    */
    this(in string fn, const LogLevel lv = LogLevel.all) @safe
    {
         this(fn, lv, CreateFolder.yes);
    }

    /** A constructor for the $(D FileLogger) Logger that takes a reference to
    a $(D File).

    The $(D File) passed must be open for all the log call to the
    $(D FileLogger). If the $(D File) gets closed, using the $(D FileLogger)
    for logging will result in undefined behaviour.

    Params:
      fn = The file used for logging.
      lv = The $(D LogLevel) for the $(D FileLogger). By default the
      $(D LogLevel) for $(D FileLogger) is $(D LogLevel.all).
      createFileNameFolder = if yes and fn contains a folder name, this
      folder will be created.

    Example:
    -------------
    auto file = File("logFile.log", "w");
    auto l1 = new FileLogger(file);
    auto l2 = new FileLogger(file, LogLevel.fatal);
    -------------
    */
    this(in string fn, const LogLevel lv, CreateFolder createFileNameFolder) @safe
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

        this.file_.open(this.filename, "a");
    }

    /** A constructor for the $(D FileLogger) Logger that takes a reference to
    a $(D File).

    The $(D File) passed must be open for all the log call to the
    $(D FileLogger). If the $(D File) gets closed, using the $(D FileLogger)
    for logging will result in undefined behaviour.

    Params:
      file = The file used for logging.
      lv = The $(D LogLevel) for the $(D FileLogger). By default the
      $(D LogLevel) for $(D FileLogger) is $(D LogLevel.all).

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

    /** If the $(D FileLogger) is managing the $(D File) it logs to, this
    method will return a reference to this File.
    */
    @property File file() @safe
    {
        return this.file_;
    }

    /* This method overrides the base class method in order to log to a file
    without requiring heap allocated memory. Additionally, the $(D FileLogger)
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
        formattedWrite(lt, ":%s:%s:%u ", file[fnIdx .. $],
            funcName[funIdx .. $], line);
    }

    /* This methods overrides the base class method and writes the parts of
    the log call directly to the file.
    */
    override protected void logMsgPart(const(char)[] msg)
    {
        formattedWrite(this.file_.lockingTextWriter(), "%s", msg);
    }

    /* This methods overrides the base class method and finalizes the active
    log call. This requires flushing the $(D File) and releasing the
    $(D FileLogger) local mutex.
    */
    override protected void finishLogMsg()
    {
        this.file_.lockingTextWriter().put("\n");
        this.file_.flush();
    }

    /* This methods overrides the base class method and delegates the
    $(D LogEntry) data to the actual implementation.
    */
    override protected void writeLogMsg(ref LogEntry payload)
    {
        this.beginLogMsg(payload.file, payload.line, payload.funcName,
            payload.prettyFuncName, payload.moduleName, payload.logLevel,
            payload.threadId, payload.timestamp, payload.logger);
        this.logMsgPart(payload.msg);
        this.finishLogMsg();
    }

    /** If the $(D FileLogger) was constructed with a filename, this method
    returns this filename. Otherwise an empty $(D string) is returned.
    */
    string getFilename()
    {
        return this.filename;
    }

    private File file_;
    private string filename;
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

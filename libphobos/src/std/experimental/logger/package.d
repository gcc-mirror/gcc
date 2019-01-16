/**
Implements logging facilities.

Copyright: Copyright Robert "burner" Schadek 2013 --
License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
Authors: $(HTTP www.svs.informatik.uni-oldenburg.de/60865.html, Robert burner Schadek)

$(H3 Basic Logging)

Message logging is a common approach to expose runtime information of a
program. Logging should be easy, but also flexible and powerful, therefore
$(D D) provides a standard interface for logging.

The easiest way to create a log message is to write:
-------------
import std.experimental.logger;

void main() {
    log("Hello World");
}
-------------
This will print a message to the $(D stderr) device. The message will contain
the filename, the line number, the name of the surrounding function, the time
and the message.

More complex log call can go along the lines like:
-------------
log("Logging to the sharedLog with its default LogLevel");
logf(LogLevel.info, 5 < 6, "%s to the sharedLog with its LogLevel.info", "Logging");
info("Logging to the sharedLog with its info LogLevel");
warning(5 < 6, "Logging to the sharedLog with its LogLevel.warning if 5 is less than 6");
error("Logging to the sharedLog with its error LogLevel");
errorf("Logging %s the sharedLog %s its error LogLevel", "to", "with");
critical("Logging to the"," sharedLog with its error LogLevel");
fatal("Logging to the sharedLog with its fatal LogLevel");

auto fLogger = new FileLogger("NameOfTheLogFile");
fLogger.log("Logging to the fileLogger with its default LogLevel");
fLogger.info("Logging to the fileLogger with its default LogLevel");
fLogger.warning(5 < 6, "Logging to the fileLogger with its LogLevel.warning if 5 is less than 6");
fLogger.warningf(5 < 6, "Logging to the fileLogger with its LogLevel.warning if %s is %s than 6", 5, "less");
fLogger.critical("Logging to the fileLogger with its info LogLevel");
fLogger.log(LogLevel.trace, 5 < 6, "Logging to the fileLogger"," with its default LogLevel if 5 is less than 6");
fLogger.fatal("Logging to the fileLogger with its warning LogLevel");
-------------
Additionally, this example shows how a new $(D FileLogger) is created.
Individual $(D Logger) and the global log functions share commonly named
functions to log data.

The names of the functions are as follows:
$(UL
    $(LI $(D log))
    $(LI $(D trace))
    $(LI $(D info))
    $(LI $(D warning))
    $(LI $(D critical))
    $(LI $(D fatal))
)
The default $(D Logger) will by default log to $(D stderr) and has a default
$(D LogLevel) of $(D LogLevel.all). The default Logger can be accessed by
using the property called $(D sharedLog). This property is a reference to the
current default $(D Logger). This reference can be used to assign a new
default $(D Logger).
-------------
sharedLog = new FileLogger("New_Default_Log_File.log");
-------------

Additional $(D Logger) can be created by creating a new instance of the
required $(D Logger).

$(H3 Logging Fundamentals)
$(H4 LogLevel)
The $(D LogLevel) of a log call can be defined in two ways. The first is by
calling $(D log) and passing the $(D LogLevel) explicitly as the first argument.
The second way of setting the $(D LogLevel) of a
log call, is by calling either $(D trace), $(D info), $(D warning),
$(D critical), or $(D fatal). The log call will then have the respective
$(D LogLevel). If no $(D LogLevel) is defined the log call will use the
current $(D LogLevel) of the used $(D Logger). If data is logged with
$(D LogLevel) $(D fatal) by default an $(D Error) will be thrown.
This behaviour can be modified by using the member $(D fatalHandler) to
assign a custom delegate to handle log call with $(D LogLevel) $(D fatal).

$(H4 Conditional Logging)
Conditional logging can be achieved be passing a $(D bool) as first
argument to a log function. If conditional logging is used the condition must
be $(D true) in order to have the log message logged.

In order to combine an explicit $(D LogLevel) passing with conditional
logging, the $(D LogLevel) has to be passed as first argument followed by the
$(D bool).

$(H4 Filtering Log Messages)
Messages are logged if the $(D LogLevel) of the log message is greater than or
equal to the $(D LogLevel) of the used $(D Logger) and additionally if the
$(D LogLevel) of the log message is greater than or equal to the global $(D LogLevel).
If a condition is passed into the log call, this condition must be true.

The global $(D LogLevel) is accessible by using $(D globalLogLevel).
To assign a $(D LogLevel) of a $(D Logger) use the $(D logLevel) property of
the logger.

$(H4 Printf Style Logging)
If $(D printf)-style logging is needed add a $(B f) to the logging call, such as
$(D myLogger.infof("Hello %s", "world");) or $(D fatalf("errno %d", 1337)).
The additional $(B f) appended to the function name enables $(D printf)-style
logging for all combinations of explicit $(D LogLevel) and conditional
logging functions and methods.

$(H4 Thread Local Redirection)
Calls to the free standing log functions are not directly forwarded to the
global $(D Logger) $(D sharedLog). Actually, a thread local $(D Logger) of
type $(D StdForwardLogger) processes the log call and then, by default, forwards
the created $(D Logger.LogEntry) to the $(D sharedLog) $(D Logger).
The thread local $(D Logger) is accessible by the $(D stdThreadLocalLog)
property. This property allows to assign user defined $(D Logger). The default
$(D LogLevel) of the $(D stdThreadLocalLog) $(D Logger) is $(D LogLevel.all)
and it will therefore forward all messages to the $(D sharedLog) $(D Logger).
The $(D LogLevel) of the $(D stdThreadLocalLog) can be used to filter log
calls before they reach the $(D sharedLog) $(D Logger).

$(H3 User Defined Logger)
To customize the $(D Logger) behavior, create a new $(D class) that inherits from
the abstract $(D Logger) $(D class), and implements the $(D writeLogMsg)
method.
-------------
class MyCustomLogger : Logger
{
    this(LogLevel lv) @safe
    {
        super(lv);
    }

    override void writeLogMsg(ref LogEntry payload)
    {
        // log message in my custom way
    }
}

auto logger = new MyCustomLogger(LogLevel.info);
logger.log("Awesome log message with LogLevel.info");
-------------

To gain more precise control over the logging process, additionally to
overriding the $(D writeLogMsg) method the methods $(D beginLogMsg),
$(D logMsgPart) and $(D finishLogMsg) can be overridden.

$(H3 Compile Time Disabling of $(D Logger))
In order to disable logging at compile time, pass $(D StdLoggerDisableLogging) as a
version argument to the $(D D) compiler when compiling your program code.
This will disable all logging functionality.
Specific $(D LogLevel) can be disabled at compile time as well.
In order to disable logging with the $(D trace) $(D LogLevel) pass
$(D StdLoggerDisableTrace) as a version.
The following table shows which version statement disables which
$(D LogLevel).
$(TABLE
    $(TR $(TD $(D LogLevel.trace) ) $(TD StdLoggerDisableTrace))
    $(TR $(TD $(D LogLevel.info) ) $(TD StdLoggerDisableInfo))
    $(TR $(TD $(D LogLevel.warning) ) $(TD StdLoggerDisableWarning))
    $(TR $(TD $(D LogLevel.error) ) $(TD StdLoggerDisableError))
    $(TR $(TD $(D LogLevel.critical) ) $(TD StdLoggerDisableCritical))
    $(TR $(TD $(D LogLevel.fatal) ) $(TD StdLoggerDisableFatal))
)
Such a version statement will only disable logging in the associated compile
unit.

$(H3 Provided Logger)
By default four $(D Logger) implementations are given. The $(D FileLogger)
logs data to files. It can also be used to log to $(D stdout) and $(D stderr)
as these devices are files as well. A $(D Logger) that logs to $(D stdout) can
therefore be created by $(D new FileLogger(stdout)).
The $(D MultiLogger) is basically an associative array of $(D string)s to
$(D Logger). It propagates log calls to its stored $(D Logger). The
$(D ArrayLogger) contains an array of $(D Logger) and also propagates log
calls to its stored $(D Logger). The $(D NullLogger) does not do anything. It
will never log a message and will never throw on a log call with $(D LogLevel)
$(D error).
*/
module std.experimental.logger;

public import std.experimental.logger.core;
public import std.experimental.logger.filelogger;
public import std.experimental.logger.multilogger;
public import std.experimental.logger.nulllogger;

// Written in the D programming language.

/**
Functions for starting and interacting with other processes, and for
working with the current process' execution environment.

Process_handling:
$(UL $(LI
    $(LREF spawnProcess) spawns a new process, optionally assigning it an
    arbitrary set of standard input, output, and error streams.
    The function returns immediately, leaving the child process to execute
    in parallel with its parent.  All other functions in this module that
    spawn processes are built around `spawnProcess`.)
$(LI
    $(LREF wait) makes the parent process wait for a child process to
    terminate.  In general one should always do this, to avoid
    child processes becoming "zombies" when the parent process exits.
    Scope guards are perfect for this – see the $(LREF spawnProcess)
    documentation for examples.  $(LREF tryWait) is similar to `wait`,
    but does not block if the process has not yet terminated.)
$(LI
    $(LREF pipeProcess) also spawns a child process which runs
    in parallel with its parent.  However, instead of taking
    arbitrary streams, it automatically creates a set of
    pipes that allow the parent to communicate with the child
    through the child's standard input, output, and/or error streams.
    This function corresponds roughly to C's `popen` function.)
$(LI
    $(LREF execute) starts a new process and waits for it
    to complete before returning.  Additionally, it captures
    the process' standard output and error streams and returns
    the output of these as a string.)
$(LI
    $(LREF spawnShell), $(LREF pipeShell) and $(LREF executeShell) work like
    `spawnProcess`, `pipeProcess` and `execute`, respectively,
    except that they take a single command string and run it through
    the current user's default command interpreter.
    `executeShell` corresponds roughly to C's `system` function.)
$(LI
    $(LREF kill) attempts to terminate a running process.)
)

The following table compactly summarises the different process creation
functions and how they relate to each other:
$(BOOKTABLE,
    $(TR $(TH )
         $(TH Runs program directly)
         $(TH Runs shell command))
    $(TR $(TD Low-level process creation)
         $(TD $(LREF spawnProcess))
         $(TD $(LREF spawnShell)))
    $(TR $(TD Automatic input/output redirection using pipes)
         $(TD $(LREF pipeProcess))
         $(TD $(LREF pipeShell)))
    $(TR $(TD Execute and wait for completion, collect output)
         $(TD $(LREF execute))
         $(TD $(LREF executeShell)))
)

Other_functionality:
$(UL
$(LI
    $(LREF pipe) is used to create unidirectional pipes.)
$(LI
    $(LREF environment) is an interface through which the current process'
    environment variables can be read and manipulated.)
$(LI
    $(LREF escapeShellCommand) and $(LREF escapeShellFileName) are useful
    for constructing shell command lines in a portable way.)
)

Authors:
    $(LINK2 https://github.com/kyllingstad, Lars Tandle Kyllingstad),
    $(LINK2 https://github.com/schveiguy, Steven Schveighoffer),
    $(HTTP thecybershadow.net, Vladimir Panteleev)
Copyright:
    Copyright (c) 2013, the authors. All rights reserved.
License:
   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
Source:
    $(PHOBOSSRC std/process.d)
Macros:
    OBJECTREF=$(REF1 $0, object)

Note:
Most of the functionality in this module is not available on iOS, tvOS
and watchOS. The only functions available on those platforms are:
$(LREF environment), $(LREF thisProcessID) and $(LREF thisThreadID).
*/
module std.process;

import core.thread : ThreadID;

version (Posix)
{
    import core.sys.posix.sys.wait;
    import core.sys.posix.unistd;
}
version (Windows)
{
    import core.stdc.stdio;
    import core.sys.windows.winbase;
    import core.sys.windows.winnt;
    import std.utf;
    import std.windows.syserror;
}

import std.internal.cstring;
import std.range;
import std.stdio;

version (OSX)
    version = Darwin;
else version (iOS)
{
    version = Darwin;
    version = iOSDerived;
}
else version (TVOS)
{
    version = Darwin;
    version = iOSDerived;
}
else version (WatchOS)
{
    version = Darwin;
    version = iOSDerived;
}


// Some of the following should be moved to druntime.
private
{
    // Microsoft Visual C Runtime (MSVCRT) declarations.
    version (CRuntime_Microsoft)
    {
        import core.stdc.stdint;
        enum
        {
            STDIN_FILENO  = 0,
            STDOUT_FILENO = 1,
            STDERR_FILENO = 2,
        }
    }

    // POSIX API declarations.
    version (Posix)
    {
        import core.sys.posix.unistd : getEnvironPtr = environ;

        @system unittest
        {
            import core.thread : Thread;
            new Thread({assert(getEnvironPtr !is null);}).start();
        }
    }
} // private

// =============================================================================
// Environment variable manipulation.
// =============================================================================

/**
Manipulates _environment variables using an associative-array-like
interface.

This class contains only static methods, and cannot be instantiated.
See below for examples of use.
*/
abstract final class environment
{
    static import core.sys.posix.stdlib;
    import core.stdc.errno : errno, EINVAL;

static:
    /**
    Retrieves the value of the environment variable with the given `name`.
    ---
    auto path = environment["PATH"];
    ---

    Throws:
    $(OBJECTREF Exception) if the environment variable does not exist,
    or $(REF UTFException, std,utf) if the variable contains invalid UTF-16
    characters (Windows only).

    See_also:
    $(LREF environment.get), which doesn't throw on failure.
    */
    string opIndex(scope const(char)[] name) @safe
    {
        import std.exception : enforce;
        return get(name, null).enforce("Environment variable not found: "~name);
    }

    /**
    Retrieves the value of the environment variable with the given `name`,
    or a default value if the variable doesn't exist.

    Unlike $(LREF environment.opIndex), this function never throws on Posix.
    ---
    auto sh = environment.get("SHELL", "/bin/sh");
    ---
    This function is also useful in checking for the existence of an
    environment variable.
    ---
    auto myVar = environment.get("MYVAR");
    if (myVar is null)
    {
        // Environment variable doesn't exist.
        // Note that we have to use 'is' for the comparison, since
        // myVar == null is also true if the variable exists but is
        // empty.
    }
    ---
    Params:
        name = name of the environment variable to retrieve
        defaultValue = default value to return if the environment variable doesn't exist.

    Returns:
        the value of the environment variable if found, otherwise
        `null` if the environment doesn't exist.

    Throws:
    $(REF UTFException, std,utf) if the variable contains invalid UTF-16
    characters (Windows only).
    */
    string get(scope const(char)[] name, string defaultValue = null) @safe
    {
        string value;
        getImpl(name, (result) { value = result ? cachedToString(result) : defaultValue; });
        return value;
    }

    /**
    Assigns the given `value` to the environment variable with the given
    `name`.
    If `value` is null the variable is removed from environment.

    If the variable does not exist, it will be created. If it already exists,
    it will be overwritten.
    ---
    environment["foo"] = "bar";
    ---

    Throws:
    $(OBJECTREF Exception) if the environment variable could not be added
        (e.g. if the name is invalid).

    Note:
    On some platforms, modifying environment variables may not be allowed in
    multi-threaded programs. See e.g.
    $(LINK2 https://www.gnu.org/software/libc/manual/html_node/Environment-Access.html#Environment-Access, glibc).
    */
    inout(char)[] opIndexAssign(return scope inout char[] value, scope const(char)[] name) @trusted
    {
        version (Posix)
        {
            import std.exception : enforce, errnoEnforce;
            if (value is null)
            {
                remove(name);
                return value;
            }
            if (core.sys.posix.stdlib.setenv(name.tempCString(), value.tempCString(), 1) != -1)
            {
                return value;
            }
            // The default errno error message is very uninformative
            // in the most common case, so we handle it manually.
            enforce(errno != EINVAL,
                "Invalid environment variable name: '"~name~"'");
            errnoEnforce(false,
                "Failed to add environment variable");
            assert(0);
        }
        else version (Windows)
        {
            import std.windows.syserror : wenforce;
            wenforce(
                SetEnvironmentVariableW(name.tempCStringW(), value.tempCStringW()),
            );
            return value;
        }
        else static assert(0);
    }

    /**
    Removes the environment variable with the given `name`.

    If the variable isn't in the environment, this function returns
    successfully without doing anything.

    Note:
    On some platforms, modifying environment variables may not be allowed in
    multi-threaded programs. See e.g.
    $(LINK2 https://www.gnu.org/software/libc/manual/html_node/Environment-Access.html#Environment-Access, glibc).
    */
    void remove(scope const(char)[] name) @trusted nothrow @nogc
    {
        version (Windows)    SetEnvironmentVariableW(name.tempCStringW(), null);
        else version (Posix) core.sys.posix.stdlib.unsetenv(name.tempCString());
        else static assert(0);
    }

    /**
    Identify whether a variable is defined in the environment.

    Because it doesn't return the value, this function is cheaper than `get`.
    However, if you do need the value as well, you should just check the
    return of `get` for `null` instead of using this function first.

    Example:
    -------------
    // good usage
    if ("MY_ENV_FLAG" in environment)
        doSomething();

    // bad usage
    if ("MY_ENV_VAR" in environment)
        doSomething(environment["MY_ENV_VAR"]);

    // do this instead
    if (auto var = environment.get("MY_ENV_VAR"))
        doSomething(var);
    -------------
    */
    bool opBinaryRight(string op : "in")(scope const(char)[] name) @trusted
    {
        if (name is null)
            return false;
        version (Posix)
            return core.sys.posix.stdlib.getenv(name.tempCString()) !is null;
        else version (Windows)
        {
            SetLastError(NO_ERROR);
            if (GetEnvironmentVariableW(name.tempCStringW, null, 0) > 0)
                return true;
            immutable err = GetLastError();
            if (err == NO_ERROR)
                return true; // zero-length environment variable on Wine / XP
            if (err == ERROR_ENVVAR_NOT_FOUND)
                return false;
            // Some other Windows error, throw.
            throw new WindowsException(err);
        }
        else static assert(0);
    }

    /**
    Copies all environment variables into an associative array.

    Windows_specific:
    While Windows environment variable names are case insensitive, D's
    built-in associative arrays are not.  This function will store all
    variable names in uppercase (e.g. `PATH`).

    Throws:
    $(OBJECTREF Exception) if the environment variables could not
        be retrieved (Windows only).
    */
    string[string] toAA() @trusted
    {
        import std.conv : to;
        string[string] aa;
        version (Posix)
        {
            auto environ = getEnvironPtr;
            for (int i=0; environ[i] != null; ++i)
            {
                import std.string : indexOf;

                immutable varDef = to!string(environ[i]);
                immutable eq = indexOf(varDef, '=');
                assert(eq >= 0);

                immutable name = varDef[0 .. eq];
                immutable value = varDef[eq+1 .. $];

                // In POSIX, environment variables may be defined more
                // than once.  This is a security issue, which we avoid
                // by checking whether the key already exists in the array.
                // For more info:
                // http://www.dwheeler.com/secure-programs/Secure-Programs-HOWTO/environment-variables.html
                if (name !in aa)  aa[name] = value;
            }
        }
        else version (Windows)
        {
            import std.exception : enforce;
            import std.uni : toUpper;
            auto envBlock = GetEnvironmentStringsW();
            enforce(envBlock, "Failed to retrieve environment variables.");
            scope(exit) FreeEnvironmentStringsW(envBlock);

            for (int i=0; envBlock[i] != '\0'; ++i)
            {
                auto start = i;
                while (envBlock[i] != '=') ++i;
                immutable name = toUTF8(toUpper(envBlock[start .. i]));

                start = i+1;
                while (envBlock[i] != '\0') ++i;

                // Ignore variables with empty names. These are used internally
                // by Windows to keep track of each drive's individual current
                // directory.
                if (!name.length)
                    continue;

                // Just like in POSIX systems, environment variables may be
                // defined more than once in an environment block on Windows,
                // and it is just as much of a security issue there.  Moreso,
                // in fact, due to the case insensensitivity of variable names,
                // which is not handled correctly by all programs.
                auto val = toUTF8(envBlock[start .. i]);
                if (name !in aa) aa[name] = val is null ? "" : val;
            }
        }
        else static assert(0);
        return aa;
    }

private:
    version (Windows) alias OSChar = WCHAR;
    else version (Posix) alias OSChar = char;

    // Retrieves the environment variable. Calls `sink` with a
    // temporary buffer of OS characters, or `null` if the variable
    // doesn't exist.
    void getImpl(scope const(char)[] name, scope void delegate(const(OSChar)[]) @safe sink) @trusted
    {
        // fix issue https://issues.dlang.org/show_bug.cgi?id=24549
        if (name is null)
            return sink(null);

        version (Windows)
        {
            // first we ask windows how long the environment variable is,
            // then we try to read it in to a buffer of that length. Lots
            // of error conditions because the windows API is nasty.

            import std.conv : to;
            const namezTmp = name.tempCStringW();
            WCHAR[] buf;

            // clear error because GetEnvironmentVariable only says it sets it
            // if the environment variable is missing, not on other errors.
            SetLastError(NO_ERROR);
            // len includes terminating null
            immutable len = GetEnvironmentVariableW(namezTmp, null, 0);
            if (len == 0)
            {
                immutable err = GetLastError();
                if (err == ERROR_ENVVAR_NOT_FOUND)
                    return sink(null);
                if (err != NO_ERROR) // Some other Windows error, throw.
                    throw new WindowsException(err);
            }
            if (len <= 1)
                return sink("");
            buf.length = len;

            while (true)
            {
                // lenRead is either the number of bytes read w/o null - if buf was long enough - or
                // the number of bytes necessary *including* null if buf wasn't long enough
                immutable lenRead = GetEnvironmentVariableW(namezTmp, buf.ptr, to!DWORD(buf.length));
                if (lenRead == 0)
                {
                    immutable err = GetLastError();
                    if (err == NO_ERROR) // sucessfully read a 0-length variable
                        return sink("");
                    if (err == ERROR_ENVVAR_NOT_FOUND) // variable didn't exist
                        return sink(null);
                    // some other windows error
                    throw new WindowsException(err);
                }
                assert(lenRead != buf.length, "impossible according to msft docs");
                if (lenRead < buf.length) // the buffer was long enough
                    return sink(buf[0 .. lenRead]);
                // resize and go around again, because the environment variable grew
                buf.length = lenRead;
            }
        }
        else version (Posix)
        {
            import core.stdc.string : strlen;

            const vz = core.sys.posix.stdlib.getenv(name.tempCString());
            if (vz == null) return sink(null);
            return sink(vz[0 .. strlen(vz)]);
       }
        else static assert(0);
    }

    string cachedToString(C)(scope const(C)[] v) @safe
    {
        import std.algorithm.comparison : equal;

        // Cache the last call's result.
        static string lastResult;
        if (v.empty)
        {
            // Return non-null array for blank result to distinguish from
            // not-present result.
            lastResult = "";
        }
        else if (!v.equal(lastResult))
        {
            import std.conv : to;
            lastResult = v.to!string;
        }
        return lastResult;
    }
}

@safe unittest
{
    import std.exception : assertThrown;
    // New variable
    environment["std_process"] = "foo";
    assert(environment["std_process"] == "foo");
    assert("std_process" in environment);

    // Set variable again (also tests length 1 case)
    environment["std_process"] = "b";
    assert(environment["std_process"] == "b");
    assert("std_process" in environment);

    // Remove variable
    environment.remove("std_process");
    assert("std_process" !in environment);

    // Remove again, should succeed
    environment.remove("std_process");
    assert("std_process" !in environment);

    // Throw on not found.
    assertThrown(environment["std_process"]);

    // get() without default value
    assert(environment.get("std_process") is null);

    // get() with default value
    assert(environment.get("std_process", "baz") == "baz");

    // get() on an empty (but present) value
    environment["std_process"] = "";
    auto res = environment.get("std_process");
    assert(res !is null);
    assert(res == "");
    assert("std_process" in environment);

    // Important to do the following round-trip after the previous test
    // because it tests toAA with an empty var

    // Convert to associative array
    auto aa = environment.toAA();
    assert(aa.length > 0);
    foreach (n, v; aa)
    {
        // Wine has some bugs related to environment variables:
        //  - Wine allows the existence of an env. variable with the name
        //    "\0", but GetEnvironmentVariable refuses to retrieve it.
        //    As of 2.067 we filter these out anyway (see comment in toAA).

        assert(v == environment[n]);
    }

    // ... and back again.
    foreach (n, v; aa)
        environment[n] = v;

    // Complete the roundtrip
    auto aa2 = environment.toAA();
    import std.conv : text;
    assert(aa == aa2, text(aa, " != ", aa2));
    assert("std_process" in environment);

    // Setting null must have the same effect as remove
    environment["std_process"] = null;
    assert("std_process" !in environment);
}

// https://issues.dlang.org/show_bug.cgi?id=24549
@safe unittest
{
    import std.exception : assertThrown;
    assert(environment.get(null) is null);
    assertThrown(environment[null]);
    assert(!(null in environment));
}

// =============================================================================
// Functions and classes for process management.
// =============================================================================

/**
 * Returns the process ID of the current process,
 * which is guaranteed to be unique on the system.
 *
 * Example:
 * ---
 * writefln("Current process ID: %d", thisProcessID);
 * ---
 */
@property int thisProcessID() @trusted nothrow @nogc //TODO: @safe
{
    version (Windows)    return GetCurrentProcessId();
    else version (Posix) return core.sys.posix.unistd.getpid();
}


/**
 * Returns the process ID of the current thread,
 * which is guaranteed to be unique within the current process.
 *
 * Returns:
 * A $(REF ThreadID, core,thread) value for the calling thread.
 *
 * Example:
 * ---
 * writefln("Current thread ID: %s", thisThreadID);
 * ---
 */
@property ThreadID thisThreadID() @trusted nothrow @nogc //TODO: @safe
{
    version (Windows)
        return GetCurrentThreadId();
    else
    version (Posix)
    {
        import core.sys.posix.pthread : pthread_self;
        return pthread_self();
    }
}


@system unittest
{
    int pidA, pidB;
    ThreadID tidA, tidB;
    pidA = thisProcessID;
    tidA = thisThreadID;

    import core.thread;
    auto t = new Thread({
        pidB = thisProcessID;
        tidB = thisThreadID;
    });
    t.start();
    t.join();

    assert(pidA == pidB);
    assert(tidA != tidB);
}


package(std) string uniqueTempPath() @safe
{
    import std.file : tempDir;
    import std.path : buildPath;
    import std.uuid : randomUUID;
    // Path should contain spaces to test escaping whitespace
    return buildPath(tempDir(), "std.process temporary file " ~
        randomUUID().toString());
}


version (iOSDerived) {}
else:

/**
Spawns a new process, optionally assigning it an arbitrary set of standard
input, output, and error streams.

The function returns immediately, leaving the child process to execute
in parallel with its parent.  It is recommended to always call $(LREF wait)
on the returned $(LREF Pid) unless the process was spawned with
`Config.detached` flag, as detailed in the documentation for `wait`.

Command_line:
There are four overloads of this function.  The first two take an array
of strings, `args`, which should contain the program name as the
zeroth element and any command-line arguments in subsequent elements.
The third and fourth versions are included for convenience, and may be
used when there are no command-line arguments.  They take a single string,
`program`, which specifies the program name.

Unless a directory is specified in `args[0]` or `program`,
`spawnProcess` will search for the program in a platform-dependent
manner.  On POSIX systems, it will look for the executable in the
directories listed in the PATH environment variable, in the order
they are listed.  On Windows, it will search for the executable in
the following sequence:
$(OL
    $(LI The directory from which the application loaded.)
    $(LI The current directory for the parent process.)
    $(LI The 32-bit Windows system directory.)
    $(LI The 16-bit Windows system directory.)
    $(LI The Windows directory.)
    $(LI The directories listed in the PATH environment variable.)
)
---
// Run an executable called "prog" located in the current working
// directory:
auto pid = spawnProcess("./prog");
scope(exit) wait(pid);
// We can do something else while the program runs.  The scope guard
// ensures that the process is waited for at the end of the scope.
...

// Run DMD on the file "myprog.d", specifying a few compiler switches:
auto dmdPid = spawnProcess(["dmd", "-O", "-release", "-inline", "myprog.d" ]);
if (wait(dmdPid) != 0)
    writeln("Compilation failed!");
---

Environment_variables:
By default, the child process inherits the environment of the parent
process, along with any additional variables specified in the `env`
parameter.  If the same variable exists in both the parent's environment
and in `env`, the latter takes precedence.

If the $(LREF Config.newEnv) flag is set in `config`, the child
process will $(I not) inherit the parent's environment.  Its entire
environment will then be determined by `env`.
---
wait(spawnProcess("myapp", ["foo" : "bar"], Config.newEnv));
---

Standard_streams:
The optional arguments `stdin`, `stdout` and `stderr` may
be used to assign arbitrary $(REF File, std,stdio) objects as the standard
input, output and error streams, respectively, of the child process.  The
former must be opened for reading, while the latter two must be opened for
writing.  The default is for the child process to inherit the standard
streams of its parent.
---
// Run DMD on the file myprog.d, logging any error messages to a
// file named errors.log.
auto logFile = File("errors.log", "w");
auto pid = spawnProcess(["dmd", "myprog.d"],
                        std.stdio.stdin,
                        std.stdio.stdout,
                        logFile);
if (wait(pid) != 0)
    writeln("Compilation failed. See errors.log for details.");
---

Note that if you pass a `File` object that is $(I not)
one of the standard input/output/error streams of the parent process,
that stream will by default be $(I closed) in the parent process when
this function returns.  See the $(LREF Config) documentation below for
information about how to disable this behaviour.

Beware of buffering issues when passing `File` objects to
`spawnProcess`.  The child process will inherit the low-level raw
read/write offset associated with the underlying file descriptor, but
it will not be aware of any buffered data.  In cases where this matters
(e.g. when a file should be aligned before being passed on to the
child process), it may be a good idea to use unbuffered streams, or at
least ensure all relevant buffers are flushed.

Params:
args    = An array which contains the program name as the zeroth element
          and any command-line arguments in the following elements.
stdin   = The standard input stream of the child process.
          This can be any $(REF File, std,stdio) that is opened for reading.
          By default the child process inherits the parent's input
          stream.
stdout  = The standard output stream of the child process.
          This can be any $(REF File, std,stdio) that is opened for writing.
          By default the child process inherits the parent's output stream.
stderr  = The standard error stream of the child process.
          This can be any $(REF File, std,stdio) that is opened for writing.
          By default the child process inherits the parent's error stream.
env     = Additional environment variables for the child process.
config  = Flags that control process creation. See $(LREF Config)
          for an overview of available flags.
workDir = The working directory for the new process.
          By default the child process inherits the parent's working
          directory.

Returns:
A $(LREF Pid) object that corresponds to the spawned process.

Throws:
$(LREF ProcessException) on failure to start the process.$(BR)
$(REF StdioException, std,stdio) on failure to pass one of the streams
    to the child process (Windows only).$(BR)
$(REF RangeError, core,exception) if `args` is empty.
*/
Pid spawnProcess(scope const(char[])[] args,
                 File stdin = std.stdio.stdin,
                 File stdout = std.stdio.stdout,
                 File stderr = std.stdio.stderr,
                 const string[string] env = null,
                 Config config = Config.none,
                 scope const char[] workDir = null)
    @safe
{
    version (Windows)
    {
        const commandLine = escapeShellArguments(args);
        const program = args.length ? args[0] : null;
        return spawnProcessWin(commandLine, program, stdin, stdout, stderr, env, config, workDir);
    }
    else version (Posix)
    {
        return spawnProcessPosix(args, stdin, stdout, stderr, env, config, workDir);
    }
    else
        static assert(0);
}

/// ditto
Pid spawnProcess(scope const(char[])[] args,
                 const string[string] env,
                 Config config = Config.none,
                 scope const(char)[] workDir = null)
    @trusted // TODO: Should be @safe
{
    return spawnProcess(args,
                        std.stdio.stdin,
                        std.stdio.stdout,
                        std.stdio.stderr,
                        env,
                        config,
                        workDir);
}

/// ditto
Pid spawnProcess(scope const(char)[] program,
                 File stdin = std.stdio.stdin,
                 File stdout = std.stdio.stdout,
                 File stderr = std.stdio.stderr,
                 const string[string] env = null,
                 Config config = Config.none,
                 scope const(char)[] workDir = null)
    @trusted
{
    return spawnProcess((&program)[0 .. 1],
                        stdin, stdout, stderr, env, config, workDir);
}

/// ditto
Pid spawnProcess(scope const(char)[] program,
                 const string[string] env,
                 Config config = Config.none,
                 scope const(char)[] workDir = null)
    @trusted
{
    return spawnProcess((&program)[0 .. 1], env, config, workDir);
}

version (Posix) private enum InternalError : ubyte
{
    noerror,
    exec,
    chdir,
    getrlimit,
    doubleFork,
    malloc,
    preExec,
    closefds_dup2,
}

/*
Implementation of spawnProcess() for POSIX.

envz should be a zero-terminated array of zero-terminated strings
on the form "var=value".
*/
version (Posix)
private Pid spawnProcessPosix(scope const(char[])[] args,
                              File stdin,
                              File stdout,
                              File stderr,
                              scope const string[string] env,
                              Config config,
                              scope const(char)[] workDir)
    @trusted // TODO: Should be @safe
{
    import core.exception : RangeError;
    import std.algorithm.searching : any;
    import std.conv : text;
    import std.path : isDirSeparator;
    import std.string : toStringz;

    if (args.empty) throw new RangeError();
    const(char)[] name = args[0];
    if (!any!isDirSeparator(name))
    {
        name = searchPathFor(name);
        if (name is null)
            throw new ProcessException(text("Executable file not found: ", args[0]));
    }

    // Convert program name and arguments to C-style strings.
    auto argz = new const(char)*[args.length+1];
    argz[0] = toStringz(name);
    foreach (i; 1 .. args.length) argz[i] = toStringz(args[i]);
    argz[$-1] = null;

    // Prepare environment.
    auto envz = createEnv(env, !(config.flags & Config.Flags.newEnv));

    // Open the working directory.
    // We use open in the parent and fchdir in the child
    // so that most errors (directory doesn't exist, not a directory)
    // can be propagated as exceptions before forking.
    int workDirFD = -1;
    scope(exit) if (workDirFD >= 0) close(workDirFD);
    if (workDir.length)
    {
        import core.sys.posix.fcntl : open, O_RDONLY, stat_t, fstat, S_ISDIR;
        workDirFD = open(workDir.tempCString(), O_RDONLY);
        if (workDirFD < 0)
            throw ProcessException.newFromErrno("Failed to open working directory");
        stat_t s;
        if (fstat(workDirFD, &s) < 0)
            throw ProcessException.newFromErrno("Failed to stat working directory");
        if (!S_ISDIR(s.st_mode))
            throw new ProcessException("Not a directory: " ~ cast(string) workDir);
    }

    static int getFD(ref File f) { return core.stdc.stdio.fileno(f.getFP()); }

    // Get the file descriptors of the streams.
    // These could potentially be invalid, but that is OK.  If so, later calls
    // to dup2() and close() will just silently fail without causing any harm.
    auto stdinFD  = getFD(stdin);
    auto stdoutFD = getFD(stdout);
    auto stderrFD = getFD(stderr);

    // We don't have direct access to the errors that may happen in a child process.
    // So we use this pipe to deliver them.
    int[2] forkPipe;
    if (core.sys.posix.unistd.pipe(forkPipe) == 0)
        setCLOEXEC(forkPipe[1], true);
    else
        throw ProcessException.newFromErrno("Could not create pipe to check startup of child");
    scope(exit) close(forkPipe[0]);

    /*
    To create detached process, we use double fork technique
    but we don't have a direct access to the second fork pid from the caller side thus use a pipe.
    We also can't reuse forkPipe for that purpose
    because we can't predict the order in which pid and possible error will be written
    since the first and the second forks will run in parallel.
    */
    int[2] pidPipe;
    if (config.flags & Config.Flags.detached)
    {
        if (core.sys.posix.unistd.pipe(pidPipe) != 0)
            throw ProcessException.newFromErrno("Could not create pipe to get process pid");
        setCLOEXEC(pidPipe[1], true);
    }
    scope(exit) if (config.flags & Config.Flags.detached) close(pidPipe[0]);

    static void abortOnError(int forkPipeOut, InternalError errorType, int error) nothrow
    {
        core.sys.posix.unistd.write(forkPipeOut, &errorType, errorType.sizeof);
        core.sys.posix.unistd.write(forkPipeOut, &error, error.sizeof);
        close(forkPipeOut);
        core.sys.posix.unistd._exit(1);
        assert(0);
    }

    void closePipeWriteEnds()
    {
        close(forkPipe[1]);
        if (config.flags & Config.Flags.detached)
            close(pidPipe[1]);
    }

    auto id = core.sys.posix.unistd.fork();
    if (id < 0)
    {
        closePipeWriteEnds();
        throw ProcessException.newFromErrno("Failed to spawn new process");
    }

    void forkChild() nothrow @nogc
    {
        static import core.sys.posix.stdio;

        // Child process

        // no need for the read end of pipe on child side
        if (config.flags & Config.Flags.detached)
            close(pidPipe[0]);
        close(forkPipe[0]);
        auto forkPipeOut = forkPipe[1];
        immutable pidPipeOut = pidPipe[1];

        // Set the working directory.
        if (workDirFD >= 0)
        {
            if (fchdir(workDirFD) < 0)
            {
                // Fail. It is dangerous to run a program
                // in an unexpected working directory.
                abortOnError(forkPipeOut, InternalError.chdir, .errno);
            }
            close(workDirFD);
        }

        void execProcess()
        {
            // Redirect streams and close the old file descriptors.
            // In the case that stderr is redirected to stdout, we need
            // to backup the file descriptor since stdout may be redirected
            // as well.
            if (stderrFD == STDOUT_FILENO) stderrFD = dup(stderrFD);
            dup2(stdinFD,  STDIN_FILENO);
            dup2(stdoutFD, STDOUT_FILENO);
            dup2(stderrFD, STDERR_FILENO);

            // Ensure that the standard streams aren't closed on execute, and
            // optionally close all other file descriptors.
            setCLOEXEC(STDIN_FILENO, false);
            setCLOEXEC(STDOUT_FILENO, false);
            setCLOEXEC(STDERR_FILENO, false);

            if (!(config.flags & Config.Flags.inheritFDs))
            {
                version (FreeBSD)
                    import core.sys.freebsd.unistd : closefrom;
                else version (OpenBSD)
                    import core.sys.openbsd.unistd : closefrom;

                static if (!__traits(compiles, closefrom))
                {
                    void fallback (int lowfd)
                    {
                        import core.sys.posix.dirent : dirent, opendir, readdir, closedir, DIR;
                        import core.sys.posix.unistd : close;
                        import core.sys.posix.stdlib : atoi, malloc, free;
                        import core.sys.posix.sys.resource : rlimit, getrlimit, RLIMIT_NOFILE;

                        // Get the maximum number of file descriptors that could be open.
                        rlimit r;
                        if (getrlimit(RLIMIT_NOFILE, &r) != 0)
                            abortOnError(forkPipeOut, InternalError.getrlimit, .errno);

                        immutable maxDescriptors = cast(int) r.rlim_cur;

                        // Missing druntime declaration
                        pragma(mangle, "dirfd")
                        extern(C) nothrow @nogc int dirfd(DIR* dir);

                        DIR* dir = null;

                        // We read from /dev/fd or /proc/self/fd only if the limit is high enough
                        if (maxDescriptors > 128*1024)
                        {
                            // Try to open the directory /dev/fd or /proc/self/fd
                            dir = opendir("/dev/fd");
                            if (dir is null) dir = opendir("/proc/self/fd");
                        }

                        // If we have a directory, close all file descriptors except stdin, stdout, and stderr
                        if (dir)
                        {
                            scope(exit) closedir(dir);

                            int opendirfd = dirfd(dir);

                            // Iterate over all file descriptors
                            while (true)
                            {
                                dirent* entry = readdir(dir);

                                if (entry is null) break;
                                if (entry.d_name[0] == '.') continue;

                                int fd = atoi(cast(char*) entry.d_name);

                                // Don't close stdin, stdout, stderr, or the directory file descriptor
                                if (fd < lowfd || fd == opendirfd) continue;

                                close(fd);
                            }
                        }
                        else
                        {
                            // This is going to allocate 8 bytes for each possible file descriptor from lowfd to r.rlim_cur
                            if (maxDescriptors <= 128*1024)
                            {
                                // NOTE: malloc() and getrlimit() are not on the POSIX async
                                // signal safe functions list, but practically this should
                                // not be a problem. Java VM and CPython also use malloc()
                                // in its own implementation via opendir().
                                import core.stdc.stdlib : malloc;
                                import core.sys.posix.poll : pollfd, poll, POLLNVAL;

                                immutable maxToClose = maxDescriptors - lowfd;

                                // Call poll() to see which ones are actually open:
                                auto pfds = cast(pollfd*) malloc(pollfd.sizeof * maxToClose);
                                if (pfds is null)
                                {
                                    abortOnError(forkPipeOut, InternalError.malloc, .errno);
                                }

                                foreach (i; 0 .. maxToClose)
                                {
                                    pfds[i].fd = i + lowfd;
                                    pfds[i].events = 0;
                                    pfds[i].revents = 0;
                                }

                                if (poll(pfds, maxToClose, 0) < 0)
                                    // couldn't use poll, use the slow path.
                                    goto LslowClose;

                                foreach (i; 0 .. maxToClose)
                                {
                                    // POLLNVAL will be set if the file descriptor is invalid.
                                    if (!(pfds[i].revents & POLLNVAL)) close(pfds[i].fd);
                                }
                            }
                            else
                            {
                            LslowClose:
                                // Fall back to closing everything.
                                foreach (i; lowfd .. maxDescriptors)
                                {
                                    close(i);
                                }
                            }
                        }
                    }

                    // closefrom may not be available on the version of glibc we build against.
                    // Until we find a way to perform this check we will try to use dlsym to
                    // check for the function. See: https://github.com/dlang/phobos/pull/9048
                    version (CRuntime_Glibc)
                    {
                        void closefrom (int lowfd) {
                            static bool tryGlibcClosefrom (int lowfd) {
                                import core.sys.posix.dlfcn : dlopen, dlclose, dlsym, dlerror, RTLD_LAZY;

                                void *handle = dlopen("libc.so.6", RTLD_LAZY);
                                if (!handle)
                                    return false;
                                scope(exit) dlclose(handle);

                                // Clear errors
                                dlerror();
                                alias closefromT = extern(C) void function(int) @nogc @system nothrow;
                                auto closefrom = cast(closefromT) dlsym(handle, "closefrom");
                                if (dlerror())
                                    return false;

                                closefrom(lowfd);
                                return true;
                            }

                            if (!tryGlibcClosefrom(lowfd))
                                fallback(lowfd);
                        }
                    }
                    else
                        alias closefrom = fallback;
                }

                // We need to close all open file descriptors excluding std{in,out,err}
                // and forkPipeOut because we still need it.
                // Since the various libc's provide us with `closefrom` move forkPipeOut
                // to position 3, right after STDERR_FILENO, and close all FDs following that.
                if (dup2(forkPipeOut, 3) == -1)
                    abortOnError(forkPipeOut, InternalError.closefds_dup2, .errno);
                forkPipeOut = 3;
                // forkPipeOut needs to be closed after we call `exec`.
                setCLOEXEC(forkPipeOut, true);
                closefrom(forkPipeOut + 1);
            }
            else // This is already done if we don't inherit descriptors.
            {
                // Close the old file descriptors, unless they are
                // either of the standard streams.
                if (stdinFD  > STDERR_FILENO)  close(stdinFD);
                if (stdoutFD > STDERR_FILENO)  close(stdoutFD);
                if (stderrFD > STDERR_FILENO)  close(stderrFD);
            }

            if (config.preExecFunction !is null)
            {
                if (config.preExecFunction() != true)
                {
                    abortOnError(forkPipeOut, InternalError.preExec, .errno);
                }
            }

            if (config.preExecDelegate !is null)
            {
                if (config.preExecDelegate() != true)
                {
                    abortOnError(forkPipeOut, InternalError.preExec, .errno);
                }
            }

            // Execute program.
            core.sys.posix.unistd.execve(argz[0], argz.ptr, envz is null ? getEnvironPtr : envz);

            // If execution fails, exit as quickly as possible.
            abortOnError(forkPipeOut, InternalError.exec, .errno);
        }

        if (config.flags & Config.Flags.detached)
        {
            auto secondFork = core.sys.posix.unistd.fork();
            if (secondFork == 0)
            {
                close(pidPipeOut);
                execProcess();
            }
            else if (secondFork == -1)
            {
                auto secondForkErrno = .errno;
                close(pidPipeOut);
                abortOnError(forkPipeOut, InternalError.doubleFork, secondForkErrno);
            }
            else
            {
                core.sys.posix.unistd.write(pidPipeOut, &secondFork, pid_t.sizeof);
                close(pidPipeOut);
                close(forkPipeOut);
                _exit(0);
            }
        }
        else
        {
            execProcess();
        }
    }

    if (id == 0)
    {
        forkChild();
        assert(0);
    }
    else
    {
        closePipeWriteEnds();
        auto status = InternalError.noerror;
        auto readExecResult = core.sys.posix.unistd.read(forkPipe[0], &status, status.sizeof);
        // Save error number just in case if subsequent "waitpid" fails and overrides errno
        immutable lastError = .errno;

        if (config.flags & Config.Flags.detached)
        {
            // Forked child exits right after creating second fork. So it should be safe to wait here.
            import core.sys.posix.sys.wait : waitpid;
            int waitResult;
            waitpid(id, &waitResult, 0);
        }

        if (readExecResult == -1)
            throw ProcessException.newFromErrno(lastError, "Could not read from pipe to get child status");

        bool owned = true;
        if (status != InternalError.noerror)
        {
            int error;
            readExecResult = read(forkPipe[0], &error, error.sizeof);
            string errorMsg;
            final switch (status)
            {
                case InternalError.chdir:
                    errorMsg = "Failed to set working directory";
                    break;
                case InternalError.getrlimit:
                    errorMsg = "getrlimit failed";
                    break;
                case InternalError.exec:
                    errorMsg = "Failed to execute '" ~ cast(string) name ~ "'";
                    break;
                case InternalError.doubleFork:
                    // Can happen only when starting detached process
                    assert(config.flags & Config.Flags.detached);
                    errorMsg = "Failed to fork twice";
                    break;
                case InternalError.malloc:
                    errorMsg = "Failed to allocate memory";
                    break;
                case InternalError.preExec:
                    errorMsg = "Failed to execute preExecFunction or preExecDelegate";
                    break;
                case InternalError.closefds_dup2:
                    assert(!(config.flags & Config.Flags.inheritFDs));
                    errorMsg = "Failed to close inherited file descriptors";
                    break;
                case InternalError.noerror:
                    assert(false);
            }
            if (readExecResult == error.sizeof)
                throw ProcessException.newFromErrno(error, errorMsg);
            throw new ProcessException(errorMsg);
        }
        else if (config.flags & Config.Flags.detached)
        {
            owned = false;
            if (read(pidPipe[0], &id, id.sizeof) != id.sizeof)
                throw ProcessException.newFromErrno("Could not read from pipe to get detached process id");
        }

        // Parent process:  Close streams and return.
        if (!(config.flags & Config.Flags.retainStdin ) && stdinFD  > STDERR_FILENO
                                            && stdinFD  != getFD(std.stdio.stdin ))
            stdin.close();
        if (!(config.flags & Config.Flags.retainStdout) && stdoutFD > STDERR_FILENO
                                            && stdoutFD != getFD(std.stdio.stdout))
            stdout.close();
        if (!(config.flags & Config.Flags.retainStderr) && stderrFD > STDERR_FILENO
                                            && stderrFD != getFD(std.stdio.stderr))
            stderr.close();
        return new Pid(id, owned);
    }
}

version (Posix)
@system unittest
{
    import std.concurrency : ownerTid, receiveTimeout, send, spawn;
    import std.datetime : seconds;

    sigset_t ss;
    sigemptyset(&ss);
    sigaddset(&ss, SIGINT);
    pthread_sigmask(SIG_BLOCK, &ss, null);

    Config config = {
        preExecFunction: () @trusted @nogc nothrow {
            // Reset signal handlers
            sigset_t ss;
            if (sigfillset(&ss) != 0)
            {
                return false;
            }
            if (sigprocmask(SIG_UNBLOCK, &ss, null) != 0)
            {
                return false;
            }
            return true;
        },
    };

    auto pid = spawnProcess(["sleep", "10000"],
                            std.stdio.stdin,
                            std.stdio.stdout,
                            std.stdio.stderr,
                            null,
                            config,
                            null);
    scope(failure)
    {
        kill(pid, SIGKILL);
        wait(pid);
    }

    // kill the spawned process with SIGINT
    // and send its return code
    spawn((shared Pid pid) {
        auto p = cast() pid;
        kill(p, SIGINT);
        auto code = wait(p);
        assert(code < 0);
        send(ownerTid, code);
    }, cast(shared) pid);

    auto received = receiveTimeout(3.seconds, (int) {});
    assert(received);
}

version (Posix)
@system unittest
{
    __gshared int j;
    foreach (i; 0 .. 3)
    {
        auto config = Config(
            preExecFunction: function() @trusted {
                j = 1;
                return true;
            },
            preExecDelegate: delegate() @trusted {
                // j should now be 1, as preExecFunction is called before
                // preExecDelegate is.
                _Exit(i + j);
                return true;
            },
        );
        auto pid = spawnProcess(["false"], config: config);
        assert(wait(pid) == i + 1);
    }
}

/*
Implementation of spawnProcess() for Windows.

commandLine must contain the entire command line, properly
quoted/escaped as required by CreateProcessW().

envz must be a pointer to a block of UTF-16 characters on the form
"var1=value1\0var2=value2\0...varN=valueN\0\0".
*/
version (Windows)
private Pid spawnProcessWin(scope const(char)[] commandLine,
                            scope const(char)[] program,
                            File stdin,
                            File stdout,
                            File stderr,
                            scope const string[string] env,
                            Config config,
                            scope const(char)[] workDir)
    @trusted
{
    import core.exception : RangeError;
    import std.conv : text;

    if (commandLine.empty) throw new RangeError("Command line is empty");

    // Prepare environment.
    auto envz = createEnv(env, !(config.flags & Config.Flags.newEnv));

    // Startup info for CreateProcessW().
    STARTUPINFO_W startinfo;
    startinfo.cb = startinfo.sizeof;
    static int getFD(ref File f) { return f.isOpen ? f.fileno : -1; }

    // Extract file descriptors and HANDLEs from the streams and make the
    // handles inheritable.
    static void prepareStream(ref File file, DWORD stdHandle, string which,
                              out int fileDescriptor, out HANDLE handle)
    {
        enum _NO_CONSOLE_FILENO = cast(HANDLE)-2;
        fileDescriptor = getFD(file);
        handle = null;
        if (fileDescriptor >= 0)
            handle = file.windowsHandle;
        // Windows GUI applications have a fd but not a valid Windows HANDLE.
        if (handle is null || handle == INVALID_HANDLE_VALUE || handle == _NO_CONSOLE_FILENO)
            handle = GetStdHandle(stdHandle);

        DWORD dwFlags;
        if (GetHandleInformation(handle, &dwFlags))
        {
            if (!(dwFlags & HANDLE_FLAG_INHERIT))
            {
                if (!SetHandleInformation(handle,
                                          HANDLE_FLAG_INHERIT,
                                          HANDLE_FLAG_INHERIT))
                {
                    throw new StdioException(
                        "Failed to make "~which~" stream inheritable by child process ("
                        ~generateSysErrorMsg() ~ ')',
                        0);
                }
            }
        }
    }
    int stdinFD = -1, stdoutFD = -1, stderrFD = -1;
    prepareStream(stdin,  STD_INPUT_HANDLE,  "stdin" , stdinFD,  startinfo.hStdInput );
    prepareStream(stdout, STD_OUTPUT_HANDLE, "stdout", stdoutFD, startinfo.hStdOutput);
    prepareStream(stderr, STD_ERROR_HANDLE,  "stderr", stderrFD, startinfo.hStdError );

    if ((startinfo.hStdInput  != null && startinfo.hStdInput  != INVALID_HANDLE_VALUE)
     || (startinfo.hStdOutput != null && startinfo.hStdOutput != INVALID_HANDLE_VALUE)
     || (startinfo.hStdError  != null && startinfo.hStdError  != INVALID_HANDLE_VALUE))
        startinfo.dwFlags = STARTF_USESTDHANDLES;

    // Create process.
    PROCESS_INFORMATION pi;
    DWORD dwCreationFlags =
        CREATE_UNICODE_ENVIRONMENT |
        ((config.flags & Config.Flags.suppressConsole) ? CREATE_NO_WINDOW : 0);
    // workaround until https://issues.dlang.org/show_bug.cgi?id=14696 is fixed
    auto pworkDir = workDir.tempCStringW();
    if (!CreateProcessW(null, commandLine.tempCStringW().buffPtr,
                        null, null, true, dwCreationFlags,
                        envz, workDir.length ? pworkDir : null, &startinfo, &pi))
        throw ProcessException.newFromLastError("Failed to spawn process \"" ~ cast(string) program ~ '"');

    // figure out if we should close any of the streams
    if (!(config.flags & Config.Flags.retainStdin ) && stdinFD  > STDERR_FILENO
                                        && stdinFD  != getFD(std.stdio.stdin ))
        stdin.close();
    if (!(config.flags & Config.Flags.retainStdout) && stdoutFD > STDERR_FILENO
                                        && stdoutFD != getFD(std.stdio.stdout))
        stdout.close();
    if (!(config.flags & Config.Flags.retainStderr) && stderrFD > STDERR_FILENO
                                        && stderrFD != getFD(std.stdio.stderr))
        stderr.close();

    // close the thread handle in the process info structure
    CloseHandle(pi.hThread);
    if (config.flags & Config.Flags.detached)
    {
        CloseHandle(pi.hProcess);
        return new Pid(pi.dwProcessId);
    }
    return new Pid(pi.dwProcessId, pi.hProcess);
}

// Converts childEnv to a zero-terminated array of zero-terminated strings
// on the form "name=value", optionally adding those of the current process'
// environment strings that are not present in childEnv.  If the parent's
// environment should be inherited without modification, this function
// returns null.
version (Posix)
private const(char*)* createEnv(const string[string] childEnv,
                                bool mergeWithParentEnv)
{
    // Determine the number of strings in the parent's environment.
    int parentEnvLength = 0;
    auto environ = getEnvironPtr;
    if (mergeWithParentEnv)
    {
        if (childEnv.length == 0) return null;
        while (environ[parentEnvLength] != null) ++parentEnvLength;
    }

    // Convert the "new" variables to C-style strings.
    auto envz = new const(char)*[parentEnvLength + childEnv.length + 1];
    int pos = 0;
    foreach (var, val; childEnv)
        envz[pos++] = (var~'='~val~'\0').ptr;

    // Add the parent's environment.
    foreach (environStr; environ[0 .. parentEnvLength])
    {
        int eqPos = 0;
        while (environStr[eqPos] != '=' && environStr[eqPos] != '\0') ++eqPos;
        if (environStr[eqPos] != '=') continue;
        auto var = environStr[0 .. eqPos];
        if (var in childEnv) continue;
        envz[pos++] = environStr;
    }
    envz[pos] = null;
    return envz.ptr;
}

version (Posix) @system unittest
{
    auto e1 = createEnv(null, false);
    assert(e1 != null && *e1 == null);

    auto e2 = createEnv(null, true);
    assert(e2 == null);

    auto e3 = createEnv(["foo" : "bar", "hello" : "world"], false);
    assert(e3 != null && e3[0] != null && e3[1] != null && e3[2] == null);
    assert((e3[0][0 .. 8] == "foo=bar\0" && e3[1][0 .. 12] == "hello=world\0")
         || (e3[0][0 .. 12] == "hello=world\0" && e3[1][0 .. 8] == "foo=bar\0"));
}


// Converts childEnv to a Windows environment block, which is on the form
// "name1=value1\0name2=value2\0...nameN=valueN\0\0", optionally adding
// those of the current process' environment strings that are not present
// in childEnv.  Returns null if the parent's environment should be
// inherited without modification, as this is what is expected by
// CreateProcess().
version (Windows)
private LPVOID createEnv(const string[string] childEnv,
                         bool mergeWithParentEnv)
{
    if (mergeWithParentEnv && childEnv.length == 0) return null;
    import std.array : appender;
    import std.uni : toUpper;
    auto envz = appender!(wchar[])();
    void put(string var, string val)
    {
        envz.put(var);
        envz.put('=');
        envz.put(val);
        envz.put(cast(wchar) '\0');
    }

    // Add the variables in childEnv, removing them from parentEnv
    // if they exist there too.
    auto parentEnv = mergeWithParentEnv ? environment.toAA() : null;
    foreach (k, v; childEnv)
    {
        auto uk = toUpper(k);
        put(uk, v);
        if (uk in parentEnv) parentEnv.remove(uk);
    }

    // Add remaining parent environment variables.
    foreach (k, v; parentEnv) put(k, v);

    // Two final zeros are needed in case there aren't any environment vars,
    // and the last one does no harm when there are.
    envz.put("\0\0"w);
    return envz.data.ptr;
}

version (Windows) @system unittest
{
    assert(createEnv(null, true) == null);
    assert((cast(wchar*) createEnv(null, false))[0 .. 2] == "\0\0"w);
    auto e1 = (cast(wchar*) createEnv(["foo":"bar", "ab":"c"], false))[0 .. 14];
    assert(e1 == "FOO=bar\0AB=c\0\0"w || e1 == "AB=c\0FOO=bar\0\0"w);
}

// Searches the PATH variable for the given executable file,
// (checking that it is in fact executable).
version (Posix)
package(std) string searchPathFor(scope const(char)[] executable)
    @safe
{
    import std.algorithm.iteration : splitter;
    import std.conv : to;
    import std.path : chainPath;

    typeof(return) result;

    environment.getImpl("PATH",
        (scope const(char)[] path)
        {
            if (!path)
                return;

            foreach (dir; splitter(path, ":"))
            {
                auto execPath = chainPath(dir, executable);
                if (isExecutable(execPath))
                {
                    result = execPath.to!(typeof(result));
                    return;
                }
            }
        });

    return result;
}

// Checks whether the file exists and can be executed by the
// current user.
version (Posix)
private bool isExecutable(R)(R path) @trusted nothrow @nogc
if (isSomeFiniteCharInputRange!R)
{
    return (access(path.tempCString(), X_OK) == 0);
}

version (Posix) @safe unittest
{
    import std.algorithm;
    auto lsPath = searchPathFor("ls");
    assert(!lsPath.empty);
    assert(lsPath[0] == '/');
    assert(lsPath.endsWith("ls"));
    auto unlikely = searchPathFor("lkmqwpoialhggyaofijadsohufoiqezm");
    assert(unlikely is null, "Are you kidding me?");
}

// Sets or unsets the FD_CLOEXEC flag on the given file descriptor.
version (Posix)
private void setCLOEXEC(int fd, bool on) nothrow @nogc
{
    import core.sys.posix.fcntl : fcntl, F_GETFD, FD_CLOEXEC, F_SETFD;
    auto flags = fcntl(fd, F_GETFD);
    if (flags >= 0)
    {
        if (on) flags |= FD_CLOEXEC;
        else    flags &= ~(cast(typeof(flags)) FD_CLOEXEC);
        flags = fcntl(fd, F_SETFD, flags);
    }
    assert(flags != -1 || .errno == EBADF);
}

@system unittest // Command line arguments in spawnProcess().
{
    version (Windows) TestScript prog =
       "if not [%~1]==[foo] ( exit 1 )
        if not [%~2]==[bar] ( exit 2 )
        exit 0";
    else version (Posix) TestScript prog =
       `if test "$1" != "foo"; then exit 1; fi
        if test "$2" != "bar"; then exit 2; fi
        exit 0`;
    assert(wait(spawnProcess(prog.path)) == 1);
    assert(wait(spawnProcess([prog.path])) == 1);
    assert(wait(spawnProcess([prog.path, "foo"])) == 2);
    assert(wait(spawnProcess([prog.path, "foo", "baz"])) == 2);
    assert(wait(spawnProcess([prog.path, "foo", "bar"])) == 0);
}

// test that file descriptors are correctly closed / left open.
// ideally this would be done by the child process making libc
// calls, but we make do...
version (Posix) @system unittest
{
    import core.stdc.errno : errno;
    import core.sys.posix.fcntl : open, O_RDONLY;
    import core.sys.posix.unistd : close;
    import std.algorithm.searching : canFind, findSplitBefore;
    import std.array : split;
    import std.conv : to;
    static import std.file;
    import std.functional : reverseArgs;
    import std.path : buildPath;

    auto directory = uniqueTempPath();
    std.file.mkdir(directory);
    scope(exit) std.file.rmdirRecurse(directory);
    auto path = buildPath(directory, "tmp");
    std.file.write(path, null);
    errno = 0;
    auto fd = open(path.tempCString, O_RDONLY);
    if (fd == -1)
    {
        import core.stdc.string : strerror;
        import std.stdio : stderr;
        import std.string : fromStringz;

        // For the CI logs
        stderr.writefln("%s: could not open '%s': %s",
            __FUNCTION__, path, strerror(errno).fromStringz);
        // TODO: should we retry here instead?
        return;
    }
    scope(exit) close(fd);

    // command >&2 (or any other number) checks whethether that number
    // file descriptor is open.
    // Can't use this for arbitrary descriptors as many shells only support
    // single digit fds.
    TestScript testDefaults = `command >&0 && command >&1 && command >&2`;
    assert(execute(testDefaults.path).status == 0);
    assert(execute(testDefaults.path, null, Config.inheritFDs).status == 0);

    // Try a few different methods to check whether there are any
    // incorrectly-open files.
    void testFDs()
    {
        // try /proc/<pid>/fd/ on linux
        version (linux)
        {
            TestScript proc = "ls /proc/$$/fd";
            auto procRes = execute(proc.path, null);
            if (procRes.status == 0)
            {
                auto fdStr = fd.to!string;
                assert(!procRes.output.split.canFind(fdStr));
                assert(execute(proc.path, null, Config.inheritFDs)
                        .output.split.canFind(fdStr));
                return;
            }
        }

        // try fuser (might sometimes need permissions)
        TestScript fuser = "echo $$ && fuser -f " ~ path;
        auto fuserRes = execute(fuser.path, null);
        if (fuserRes.status == 0)
        {
            assert(!reverseArgs!canFind(fuserRes
                        .output.findSplitBefore("\n").expand));
            assert(reverseArgs!canFind(execute(fuser.path, null, Config.inheritFDs)
                        .output.findSplitBefore("\n").expand));
            return;
        }

        // last resort, try lsof (not available on all Posix)
        TestScript lsof = "lsof -p$$";
        auto lsofRes = execute(lsof.path, null);
        if (lsofRes.status == 0)
        {
            assert(!lsofRes.output.canFind(path));
            auto lsofOut = execute(lsof.path, null, Config.inheritFDs).output;
            if (!lsofOut.canFind(path))
            {
                std.stdio.stderr.writeln(__FILE__, ':', __LINE__,
                    ": Warning: unexpected lsof output:", lsofOut);
            }
            return;
        }

        std.stdio.stderr.writeln(__FILE__, ':', __LINE__,
                ": Warning: Couldn't find any way to check open files");
    }
    testFDs();
}

@system unittest // Environment variables in spawnProcess().
{
    // We really should use set /a on Windows, but Wine doesn't support it.
    version (Windows) TestScript envProg =
       `if [%STD_PROCESS_UNITTEST1%] == [1] (
            if [%STD_PROCESS_UNITTEST2%] == [2] (exit 3)
            exit 1
        )
        if [%STD_PROCESS_UNITTEST1%] == [4] (
            if [%STD_PROCESS_UNITTEST2%] == [2] (exit 6)
            exit 4
        )
        if [%STD_PROCESS_UNITTEST2%] == [2] (exit 2)
        exit 0`;
    version (Posix) TestScript envProg =
       `if test "$std_process_unittest1" = ""; then
            std_process_unittest1=0
        fi
        if test "$std_process_unittest2" = ""; then
            std_process_unittest2=0
        fi
        exit $(($std_process_unittest1+$std_process_unittest2))`;

    environment.remove("std_process_unittest1"); // Just in case.
    environment.remove("std_process_unittest2");
    assert(wait(spawnProcess(envProg.path)) == 0);
    assert(wait(spawnProcess(envProg.path, null, Config.newEnv)) == 0);

    environment["std_process_unittest1"] = "1";
    assert(wait(spawnProcess(envProg.path)) == 1);
    assert(wait(spawnProcess(envProg.path, null, Config.newEnv)) == 0);

    auto env = ["std_process_unittest2" : "2"];
    assert(wait(spawnProcess(envProg.path, env)) == 3);
    assert(wait(spawnProcess(envProg.path, env, Config.newEnv)) == 2);

    env["std_process_unittest1"] = "4";
    assert(wait(spawnProcess(envProg.path, env)) == 6);
    assert(wait(spawnProcess(envProg.path, env, Config.newEnv)) == 6);

    environment.remove("std_process_unittest1");
    assert(wait(spawnProcess(envProg.path, env)) == 6);
    assert(wait(spawnProcess(envProg.path, env, Config.newEnv)) == 6);
}

@system unittest // Stream redirection in spawnProcess().
{
    import std.path : buildPath;
    import std.string;
    version (Windows) TestScript prog =
       "set /p INPUT=
        echo %INPUT% output %~1
        echo %INPUT% error %~2 1>&2
        echo done > %3";
    else version (Posix) TestScript prog =
       "read INPUT
        echo $INPUT output $1
        echo $INPUT error $2 >&2
        echo done > \"$3\"";

    // Pipes
    void testPipes(Config config)
    {
        import std.file : tempDir, exists, remove;
        import std.uuid : randomUUID;
        import std.exception : collectException;
        auto pipei = pipe();
        auto pipeo = pipe();
        auto pipee = pipe();
        auto done = buildPath(tempDir(), randomUUID().toString());
        auto pid = spawnProcess([prog.path, "foo", "bar", done],
                                    pipei.readEnd, pipeo.writeEnd, pipee.writeEnd, null, config);
        pipei.writeEnd.writeln("input");
        pipei.writeEnd.flush();
        assert(pipeo.readEnd.readln().chomp() == "input output foo");
        assert(pipee.readEnd.readln().chomp().stripRight() == "input error bar");
        if (config.flags & Config.Flags.detached)
            while (!done.exists) Thread.sleep(10.msecs);
        else
            wait(pid);
        while (remove(done).collectException) Thread.sleep(10.msecs);
    }

    // Files
    void testFiles(Config config)
    {
        import std.ascii : newline;
        import std.file : tempDir, exists, remove, readText, write;
        import std.uuid : randomUUID;
        import std.exception : collectException;
        auto pathi = buildPath(tempDir(), randomUUID().toString());
        auto patho = buildPath(tempDir(), randomUUID().toString());
        auto pathe = buildPath(tempDir(), randomUUID().toString());
        write(pathi, "INPUT" ~ newline);
        auto filei = File(pathi, "r");
        auto fileo = File(patho, "w");
        auto filee = File(pathe, "w");
        auto done = buildPath(tempDir(), randomUUID().toString());
        auto pid = spawnProcess([prog.path, "bar", "baz", done], filei, fileo, filee, null, config);
        if (config.flags & Config.Flags.detached)
            while (!done.exists) Thread.sleep(10.msecs);
        else
            wait(pid);
        assert(readText(patho).chomp() == "INPUT output bar");
        assert(readText(pathe).chomp().stripRight() == "INPUT error baz");
        while (remove(pathi).collectException) Thread.sleep(10.msecs);
        while (remove(patho).collectException) Thread.sleep(10.msecs);
        while (remove(pathe).collectException) Thread.sleep(10.msecs);
        while (remove(done).collectException) Thread.sleep(10.msecs);
    }

    testPipes(Config.none);
    testFiles(Config.none);
    testPipes(Config.detached);
    testFiles(Config.detached);
}

@system unittest // Error handling in spawnProcess()
{
    import std.algorithm.searching : canFind;
    import std.exception : assertThrown, collectExceptionMsg;

    static void testNotFoundException(string program)
    {
        assert(collectExceptionMsg!ProcessException(spawnProcess(program)).canFind(program));
        assert(collectExceptionMsg!ProcessException(spawnProcess(program, null, Config.detached)).canFind(program));
    }
    testNotFoundException("ewrgiuhrifuheiohnmnvqweoijwf");
    testNotFoundException("./rgiuhrifuheiohnmnvqweoijwf");

    // can't execute malformed file with executable permissions
    version (Posix)
    {
        import std.path : buildPath;
        import std.file : remove, write, setAttributes, tempDir;
        import core.sys.posix.sys.stat : S_IRUSR, S_IWUSR, S_IXUSR, S_IRGRP, S_IXGRP, S_IROTH, S_IXOTH;
        import std.conv : to;
        string deleteme = buildPath(tempDir(), "deleteme.std.process.unittest.pid") ~ to!string(thisProcessID);
        write(deleteme, "");
        scope(exit) remove(deleteme);
        setAttributes(deleteme, S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH);
        assertThrown!ProcessException(spawnProcess(deleteme));
        assertThrown!ProcessException(spawnProcess(deleteme, null, Config.detached));
    }
}

@system unittest // Specifying a working directory.
{
    import std.path;
    import std.file;
    TestScript prog = "echo foo>bar";

    auto directory = uniqueTempPath();
    mkdir(directory);
    scope(exit) rmdirRecurse(directory);

    auto pid = spawnProcess([prog.path], null, Config.none, directory);
    wait(pid);
    assert(exists(buildPath(directory, "bar")));
}

@system unittest // Specifying a bad working directory.
{
    import std.exception : assertThrown;
    import std.file;
    TestScript prog = "echo";

    auto directory = uniqueTempPath();
    assertThrown!ProcessException(spawnProcess([prog.path], null, Config.none, directory));
    assertThrown!ProcessException(spawnProcess([prog.path], null, Config.detached, directory));

    std.file.write(directory, "foo");
    scope(exit) remove(directory);
    assertThrown!ProcessException(spawnProcess([prog.path], null, Config.none, directory));
    assertThrown!ProcessException(spawnProcess([prog.path], null, Config.detached, directory));

    // can't run in directory if user does not have search permission on this directory
    version (Posix)
    {
        if (core.sys.posix.unistd.getuid() != 0)
        {
            import core.sys.posix.sys.stat : S_IRUSR;
            auto directoryNoSearch = uniqueTempPath();
            mkdir(directoryNoSearch);
            scope(exit) rmdirRecurse(directoryNoSearch);
            setAttributes(directoryNoSearch, S_IRUSR);
            assertThrown!ProcessException(spawnProcess(prog.path, null, Config.none, directoryNoSearch));
            assertThrown!ProcessException(spawnProcess(prog.path, null, Config.detached, directoryNoSearch));
        }
    }
}

@system unittest // Specifying empty working directory.
{
    TestScript prog = "";

    string directory = "";
    assert(directory.ptr && !directory.length);
    spawnProcess([prog.path], null, Config.none, directory).wait();
}

// Reopening the standard streams (https://issues.dlang.org/show_bug.cgi?id=13258)
@system unittest
{
    import std.string;
    import std.file;
    void fun()
    {
        spawnShell("echo foo").wait();
        spawnShell("echo bar").wait();
    }

    auto tmpFile = uniqueTempPath();
    scope(exit) if (exists(tmpFile)) remove(tmpFile);

    {
        auto oldOut = std.stdio.stdout;
        scope(exit) std.stdio.stdout = oldOut;

        std.stdio.stdout = File(tmpFile, "w");
        fun();
        std.stdio.stdout.close();
    }

    auto lines = readText(tmpFile).splitLines();
    assert(lines == ["foo", "bar"]);
}

// MSVCRT workaround (https://issues.dlang.org/show_bug.cgi?id=14422)
version (Windows)
@system unittest
{
    auto fn = uniqueTempPath();
    scope(exit) if (exists(fn)) remove(fn);
    std.file.write(fn, "AAAAAAAAAA");

    auto f = File(fn, "a");
    spawnProcess(["cmd", "/c", "echo BBBBB"], std.stdio.stdin, f).wait();

    auto data = readText(fn);
    assert(data == "AAAAAAAAAABBBBB\r\n", data);
}

// https://issues.dlang.org/show_bug.cgi?id=20765
// Test that running processes with relative path works in conjunction
// with indicating a workDir.
version (Posix) @system unittest
{
    import std.file : mkdir, write, setAttributes, rmdirRecurse;
    import std.conv : octal;

    auto dir = uniqueTempPath();
    mkdir(dir);
    scope(exit) rmdirRecurse(dir);
    write(dir ~ "/program", "#!/bin/sh\necho Hello");
    setAttributes(dir ~ "/program", octal!700);

    assert(execute(["./program"], null, Config.none, size_t.max, dir).output == "Hello\n");
}

/**
A variation on $(LREF spawnProcess) that runs the given _command through
the current user's preferred _command interpreter (aka. shell).

The string `command` is passed verbatim to the shell, and is therefore
subject to its rules about _command structure, argument/filename quoting
and escaping of special characters.
The path to the shell executable defaults to $(LREF nativeShell).

In all other respects this function works just like `spawnProcess`.
Please refer to the $(LREF spawnProcess) documentation for descriptions
of the other function parameters, the return value and any exceptions
that may be thrown.
---
// Run the command/program "foo" on the file named "my file.txt", and
// redirect its output into foo.log.
auto pid = spawnShell(`foo "my file.txt" > foo.log`);
wait(pid);
---

See_also:
$(LREF escapeShellCommand), which may be helpful in constructing a
properly quoted and escaped shell _command line for the current platform.
*/
Pid spawnShell(scope const(char)[] command,
               File stdin = std.stdio.stdin,
               File stdout = std.stdio.stdout,
               File stderr = std.stdio.stderr,
               scope const string[string] env = null,
               Config config = Config.none,
               scope const(char)[] workDir = null,
               scope string shellPath = nativeShell)
    @trusted // See reason below
{
    version (Windows)
    {
        // CMD does not parse its arguments like other programs.
        // It does not use CommandLineToArgvW.
        // Instead, it treats the first and last quote specially.
        // See CMD.EXE /? for details.
        const commandLine = escapeShellFileName(shellPath)
                            ~ ` ` ~ shellSwitch ~ ` "` ~ command ~ `"`;
        return spawnProcessWin(commandLine, shellPath, stdin, stdout, stderr, env, config, workDir);
    }
    else version (Posix)
    {
        const(char)[][3] args;
        args[0] = shellPath;
        args[1] = shellSwitch;
        args[2] = command;
        /* The passing of args converts the static array, which is initialized with `scope` pointers,
         * to a dynamic array, which is also a scope parameter. So, it is a scope pointer to a
         * scope pointer, which although is safely used here, D doesn't allow transitive scope.
         * See https://github.com/dlang/dmd/pull/10951
         */
        return spawnProcessPosix(args, stdin, stdout, stderr, env, config, workDir);
    }
    else
        static assert(0);
}

/// ditto
Pid spawnShell(scope const(char)[] command,
               scope const string[string] env,
               Config config = Config.none,
               scope const(char)[] workDir = null,
               scope string shellPath = nativeShell)
    @trusted // TODO: Should be @safe
{
    return spawnShell(command,
                      std.stdio.stdin,
                      std.stdio.stdout,
                      std.stdio.stderr,
                      env,
                      config,
                      workDir,
                      shellPath);
}

@system unittest
{
    version (Windows)
        auto cmd = "echo %FOO%";
    else version (Posix)
        auto cmd = "echo $foo";
    import std.file;
    auto tmpFile = uniqueTempPath();
    scope(exit) if (exists(tmpFile)) remove(tmpFile);
    auto redir = "> \""~tmpFile~'"';
    auto env = ["foo" : "bar"];
    assert(wait(spawnShell(cmd~redir, env)) == 0);
    auto f = File(tmpFile, "a");
    version (CRuntime_Microsoft) f.seek(0, SEEK_END); // MSVCRT probably seeks to the end when writing, not before
    assert(wait(spawnShell(cmd, std.stdio.stdin, f, std.stdio.stderr, env)) == 0);
    f.close();
    auto output = std.file.readText(tmpFile);
    assert(output == "bar\nbar\n" || output == "bar\r\nbar\r\n");
}

version (Windows)
@system unittest
{
    import std.string;
    import std.conv : text;
    TestScript prog = "echo %0 %*";
    auto outputFn = uniqueTempPath();
    scope(exit) if (exists(outputFn)) remove(outputFn);
    auto args = [`a b c`, `a\b\c\`, `a"b"c"`];
    auto result = executeShell(
        escapeShellCommand([prog.path] ~ args)
        ~ " > " ~
        escapeShellFileName(outputFn));
    assert(result.status == 0);
    auto args2 = outputFn.readText().strip().parseCommandLine()[1..$];
    assert(args == args2, text(args2));
}


/**
Options that control the behaviour of process creation functions in this
module. Most options only apply to $(LREF spawnProcess) and
$(LREF spawnShell).

Example:
---
auto logFile = File("myapp_error.log", "w");

// Start program, suppressing the console window (Windows only),
// redirect its error stream to logFile, and leave logFile open
// in the parent process as well.
auto pid = spawnProcess("myapp", stdin, stdout, logFile,
                        Config.retainStderr | Config.suppressConsole);
scope(exit)
{
    auto exitCode = wait(pid);
    logFile.writeln("myapp exited with code ", exitCode);
    logFile.close();
}
---
*/
struct Config
{
    /**
       Flag options.
       Use bitwise OR to combine flags.
    **/
    enum Flags
    {
        none = 0,

        /**
        By default, the child process inherits the parent's environment,
        and any environment variables passed to $(LREF spawnProcess) will
        be added to it.  If this flag is set, the only variables in the
        child process' environment will be those given to spawnProcess.
        */
        newEnv = 1,

        /**
        Unless the child process inherits the standard input/output/error
        streams of its parent, one almost always wants the streams closed
        in the parent when $(LREF spawnProcess) returns.  Therefore, by
        default, this is done.  If this is not desirable, pass any of these
        options to spawnProcess.
        */
        retainStdin  = 2,
        retainStdout = 4,                                  /// ditto
        retainStderr = 8,                                  /// ditto

        /**
        On Windows, if the child process is a console application, this
        flag will prevent the creation of a console window.  Otherwise,
        it will be ignored. On POSIX, `suppressConsole` has no effect.
        */
        suppressConsole = 16,

        /**
        On POSIX, open $(LINK2 http://en.wikipedia.org/wiki/File_descriptor,file descriptors)
        are by default inherited by the child process.  As this may lead
        to subtle bugs when pipes or multiple threads are involved,
        $(LREF spawnProcess) ensures that all file descriptors except the
        ones that correspond to standard input/output/error are closed
        in the child process when it starts.  Use `inheritFDs` to prevent
        this.

        On Windows, this option has no effect, and any handles which have been
        explicitly marked as inheritable will always be inherited by the child
        process.
        */
        inheritFDs = 32,

        /**
        Spawn process in detached state. This removes the need in calling
        $(LREF wait) to clean up the process resources.

        Note:
        Calling $(LREF wait) or $(LREF kill) with the resulting `Pid` is invalid.
        */
        detached = 64,

        /**
        By default, the $(LREF execute) and $(LREF executeShell) functions
        will capture child processes' both stdout and stderr. This can be
        undesirable if the standard output is to be processed or otherwise
        used by the invoking program, as `execute`'s result would then
        contain a mix of output and warning/error messages.

        Specify this flag when calling `execute` or `executeShell` to
        cause invoked processes' stderr stream to be sent to $(REF stderr,
        std,stdio), and only capture and return standard output.

        This flag has no effect on $(LREF spawnProcess) or $(LREF spawnShell).
        */
        stderrPassThrough = 128,
    }
    Flags flags; /// ditto

    /**
       For backwards compatibility, and cases when only flags need to
       be specified in the `Config`, these allow building `Config`
       instances using flag names only.
    */
    enum Config none = Config.init;
    enum Config newEnv = Config(Flags.newEnv); /// ditto
    enum Config retainStdin = Config(Flags.retainStdin); /// ditto
    enum Config retainStdout = Config(Flags.retainStdout); /// ditto
    enum Config retainStderr = Config(Flags.retainStderr); /// ditto
    enum Config suppressConsole = Config(Flags.suppressConsole); /// ditto
    enum Config inheritFDs = Config(Flags.inheritFDs); /// ditto
    enum Config detached = Config(Flags.detached); /// ditto
    enum Config stderrPassThrough = Config(Flags.stderrPassThrough); /// ditto
    Config opUnary(string op)()
    if (is(typeof(mixin(op ~ q{flags}))))
    {
        return Config(mixin(op ~ q{flags}));
    } /// ditto
    Config opBinary(string op)(Config other)
    if (is(typeof(mixin(q{flags} ~ op ~ q{other.flags}))))
    {
        return Config(mixin(q{flags} ~ op ~ q{other.flags}));
    } /// ditto
    Config opOpAssign(string op)(Config other)
    if (is(typeof(mixin(q{flags} ~ op ~ q{=other.flags}))))
    {
        return Config(mixin(q{flags} ~ op ~ q{=other.flags}));
    } /// ditto

    version (StdDdoc)
    {
        /**
        A function that is called before `exec` in $(LREF spawnProcess).
        It returns `true` if succeeded and otherwise returns `false`.

        $(RED Warning:
            Please note that the code in this function must only use
            async-signal-safe functions.)

        If $(LREF preExecDelegate) is also set, it is called last.

        On Windows, this member is not available.
        */
        bool function() nothrow @nogc @safe preExecFunction;

        /**
        A delegate that is called before `exec` in $(LREF spawnProcess).
        It returns `true` if succeeded and otherwise returns `false`.

        $(RED Warning:
            Please note that the code in this function must only use
            async-signal-safe functions.)

        If $(LREF preExecFunction) is also set, it is called first.

        On Windows, this member is not available.
        */
        bool delegate() nothrow @nogc @safe preExecDelegate;
    }
    else version (Posix)
    {
        bool function() nothrow @nogc @safe preExecFunction;
        bool delegate() nothrow @nogc @safe preExecDelegate;
    }
}

// https://issues.dlang.org/show_bug.cgi?id=22125
@safe unittest
{
    Config c = Config.retainStdin;
    c |= Config.retainStdout;
    c |= Config.retainStderr;
    c &= ~Config.retainStderr;
    assert(c == (Config.retainStdin | Config.retainStdout));
}

/// A handle that corresponds to a spawned process.
final class Pid
{
    /**
    The process ID number.

    This is a number that uniquely identifies the process on the operating
    system, for at least as long as the process is running.  Once $(LREF wait)
    has been called on the $(LREF Pid), this method will return an
    invalid (negative) process ID.
    */
    @property int processID() const @safe pure nothrow
    {
        return _processID;
    }

    /**
    An operating system handle to the process.

    This handle is used to specify the process in OS-specific APIs.
    On POSIX, this function returns a `core.sys.posix.sys.types.pid_t`
    with the same value as $(LREF Pid.processID), while on Windows it returns
    a `core.sys.windows.windows.HANDLE`.

    Once $(LREF wait) has been called on the $(LREF Pid), this method
    will return an invalid handle.
    */
    // Note: Since HANDLE is a reference, this function cannot be const.
    version (Windows)
    @property HANDLE osHandle() @nogc @safe pure nothrow
    {
        return _handle;
    }
    else version (Posix)
    @property pid_t osHandle() @nogc @safe pure nothrow
    {
        return _processID;
    }

private:
    /*
    Pid.performWait() does the dirty work for wait() and nonBlockingWait().

    If block == true, this function blocks until the process terminates,
    sets _processID to terminated, and returns the exit code or terminating
    signal as described in the wait() documentation.

    If block == false, this function returns immediately, regardless
    of the status of the process.  If the process has terminated, the
    function has the exact same effect as the blocking version.  If not,
    it returns 0 and does not modify _processID.
    */
    version (Posix)
    int performWait(bool block) @trusted
    {
        import std.exception : enforce;
        enforce!ProcessException(owned, "Can't wait on a detached process");
        if (_processID == terminated) return _exitCode;
        int exitCode;
        while (true)
        {
            int status;
            auto check = waitpid(_processID, &status, block ? 0 : WNOHANG);
            if (check == -1)
            {
                if (errno == ECHILD)
                {
                    throw new ProcessException(
                        "Process does not exist or is not a child process.");
                }
                else
                {
                    // waitpid() was interrupted by a signal.  We simply
                    // restart it.
                    assert(errno == EINTR);
                    continue;
                }
            }
            if (!block && check == 0) return 0;
            if (WIFEXITED(status))
            {
                exitCode = WEXITSTATUS(status);
                break;
            }
            else if (WIFSIGNALED(status))
            {
                exitCode = -WTERMSIG(status);
                break;
            }
            // We check again whether the call should be blocking,
            // since we don't care about other status changes besides
            // "exited" and "terminated by signal".
            if (!block) return 0;

            // Process has stopped, but not terminated, so we continue waiting.
        }
        // Mark Pid as terminated, and cache and return exit code.
        _processID = terminated;
        _exitCode = exitCode;
        return exitCode;
    }
    else version (Windows)
    {
        int performWait(const bool block, const DWORD timeout = INFINITE) @trusted
        {
            import std.exception : enforce;
            enforce!ProcessException(owned, "Can't wait on a detached process");
            if (_processID == terminated) return _exitCode;
            assert(_handle != INVALID_HANDLE_VALUE);
            if (block)
            {
                auto result = WaitForSingleObject(_handle, timeout);
                if (result != WAIT_OBJECT_0)
                {
                    // Wait time exceeded `timeout` milliseconds?
                    if (result == WAIT_TIMEOUT && timeout != INFINITE)
                        return 0;

                    throw ProcessException.newFromLastError("Wait failed.");
                }
            }
            if (!GetExitCodeProcess(_handle, cast(LPDWORD)&_exitCode))
                throw ProcessException.newFromLastError();
            if (!block && _exitCode == STILL_ACTIVE) return 0;
            CloseHandle(_handle);
            _handle = INVALID_HANDLE_VALUE;
            _processID = terminated;
            return _exitCode;
        }

        int performWait(Duration timeout) @safe
        {
            import std.exception : enforce;
            const msecs = timeout.total!"msecs";

            // Limit this implementation the maximum wait time offered by
            // WaitForSingleObject. One could theoretically break up larger
            // durations into multiple waits but (DWORD.max - 1).msecs
            // (> 7 weeks, 17 hours) should be enough for the usual case.
            // DWORD.max is reserved for INFINITE
            enforce!ProcessException(msecs < DWORD.max, "Timeout exceeds maximum wait time!");
            return performWait(true, cast(DWORD) msecs);
        }

        ~this()
        {
            if (_handle != INVALID_HANDLE_VALUE)
            {
                CloseHandle(_handle);
                _handle = INVALID_HANDLE_VALUE;
            }
        }
    }

    // Special values for _processID.
    enum invalid = -1, terminated = -2;

    // OS process ID number.  Only nonnegative IDs correspond to
    // running processes.
    int _processID = invalid;

    // Exit code cached by wait().  This is only expected to hold a
    // sensible value if _processID == terminated.
    int _exitCode;

    // Whether the process can be waited for by wait() for or killed by kill().
    // False if process was started as detached. True otherwise.
    bool owned;

    // Pids are only meant to be constructed inside this module, so
    // we make the constructor private.
    version (Windows)
    {
        HANDLE _handle = INVALID_HANDLE_VALUE;
        this(int pid, HANDLE handle) @safe pure nothrow
        {
            _processID = pid;
            _handle = handle;
            this.owned = true;
        }
        this(int pid) @safe pure nothrow
        {
            _processID = pid;
            this.owned = false;
        }
    }
    else
    {
        this(int id, bool owned) @safe pure nothrow
        {
            _processID = id;
            this.owned = owned;
        }
    }
}


/**
Waits for the process associated with `pid` to terminate, and returns
its exit status.

In general one should always _wait for child processes to terminate
before exiting the parent process unless the process was spawned as detached
(that was spawned with `Config.detached` flag).
Otherwise, they may become "$(HTTP en.wikipedia.org/wiki/Zombie_process,zombies)"
– processes that are defunct, yet still occupy a slot in the OS process table.
You should not and must not wait for detached processes, since you don't own them.

If the process has already terminated, this function returns directly.
The exit code is cached, so that if wait() is called multiple times on
the same $(LREF Pid) it will always return the same value.

POSIX_specific:
If the process is terminated by a signal, this function returns a
negative number whose absolute value is the signal number.
Since POSIX restricts normal exit codes to the range 0-255, a
negative return value will always indicate termination by signal.
Signal codes are defined in the `core.sys.posix.signal` module
(which corresponds to the `signal.h` POSIX header).

Throws:
$(LREF ProcessException) on failure or on attempt to wait for detached process.

Example:
See the $(LREF spawnProcess) documentation.

See_also:
$(LREF tryWait), for a non-blocking function.
*/
int wait(Pid pid) @safe
{
    assert(pid !is null, "Called wait on a null Pid.");
    return pid.performWait(true);
}


@system unittest // Pid and wait()
{
    version (Windows)    TestScript prog = "exit %~1";
    else version (Posix) TestScript prog = "exit $1";
    assert(wait(spawnProcess([prog.path, "0"])) == 0);
    assert(wait(spawnProcess([prog.path, "123"])) == 123);
    auto pid = spawnProcess([prog.path, "10"]);
    assert(pid.processID > 0);
    version (Windows)    assert(pid.osHandle != INVALID_HANDLE_VALUE);
    else version (Posix) assert(pid.osHandle == pid.processID);
    assert(wait(pid) == 10);
    assert(wait(pid) == 10); // cached exit code
    assert(pid.processID < 0);
    version (Windows)    assert(pid.osHandle == INVALID_HANDLE_VALUE);
    else version (Posix) assert(pid.osHandle < 0);
}

private import std.typecons : Tuple;

/**
Waits until either the process associated with `pid` terminates or the
elapsed time exceeds the given timeout.

If the process terminates within the given duration it behaves exactly like
`wait`, except that it returns a tuple `(true, exit code)`.

If the process does not terminate within the given duration it will stop
waiting and return `(false, 0).`

The timeout may not exceed `(uint.max - 1).msecs` (~ 7 weeks, 17 hours).

$(BLUE This function is Windows-Only.)

Returns:
An $(D std.typecons.Tuple!(bool, "terminated", int, "status")).

Throws:
$(LREF ProcessException) on failure or on attempt to wait for detached process.

Example:
See the $(LREF spawnProcess) documentation.

See_also:
$(LREF wait), for a blocking function without timeout.
$(LREF tryWait), for a non-blocking function without timeout.
*/
version (StdDdoc)
Tuple!(bool, "terminated", int, "status") waitTimeout(Pid pid, Duration timeout) @safe;

else version (Windows)
Tuple!(bool, "terminated", int, "status") waitTimeout(Pid pid, Duration timeout) @safe
{
    assert(pid !is null, "Called wait on a null Pid.");
    auto code = pid.performWait(timeout);
    return typeof(return)(pid._processID == Pid.terminated, code);
}

version (Windows)
@system unittest // Pid and waitTimeout()
{
    import std.exception : collectException;
    import std.typecons : tuple;

    TestScript prog = ":Loop\r\n" ~ "goto Loop";
    auto pid = spawnProcess(prog.path);

    // Doesn't block longer than one second
    assert(waitTimeout(pid, 1.seconds) == tuple(false, 0));

    kill(pid);
    assert(waitTimeout(pid, 1.seconds) == tuple(true, 1)); // exit 1 because the process is killed

    // Rejects timeouts exceeding the Windows API capabilities
    const dur = DWORD.max.msecs;
    const ex = collectException!ProcessException(waitTimeout(pid, dur));
    assert(ex);
    assert(ex.msg == "Timeout exceeds maximum wait time!");
}

/**
A non-blocking version of $(LREF wait).

If the process associated with `pid` has already terminated,
`tryWait` has the exact same effect as `wait`.
In this case, it returns a tuple where the `terminated` field
is set to `true` and the `status` field has the same
interpretation as the return value of `wait`.

If the process has $(I not) yet terminated, this function differs
from `wait` in that does not wait for this to happen, but instead
returns immediately.  The `terminated` field of the returned
tuple will then be set to `false`, while the `status` field
will always be 0 (zero).  `wait` or `tryWait` should then be
called again on the same `Pid` at some later time; not only to
get the exit code, but also to avoid the process becoming a "zombie"
when it finally terminates.  (See $(LREF wait) for details).

Returns:
An $(D std.typecons.Tuple!(bool, "terminated", int, "status")).

Throws:
$(LREF ProcessException) on failure or on attempt to wait for detached process.

Example:
---
auto pid = spawnProcess("dmd myapp.d");
scope(exit) wait(pid);
...
auto dmd = tryWait(pid);
if (dmd.terminated)
{
    if (dmd.status == 0) writeln("Compilation succeeded!");
    else writeln("Compilation failed");
}
else writeln("Still compiling...");
...
---
Note that in this example, the first `wait` call will have no
effect if the process has already terminated by the time `tryWait`
is called.  In the opposite case, however, the `scope` statement
ensures that we always wait for the process if it hasn't terminated
by the time we reach the end of the scope.
*/
auto tryWait(Pid pid) @safe
{
    import std.typecons : Tuple;
    assert(pid !is null, "Called tryWait on a null Pid.");
    auto code = pid.performWait(false);
    return Tuple!(bool, "terminated", int, "status")(pid._processID == Pid.terminated, code);
}
// unittest: This function is tested together with kill() below.


/**
Attempts to terminate the process associated with `pid`.

The effect of this function, as well as the meaning of `codeOrSignal`,
is highly platform dependent.  Details are given below.  Common to all
platforms is that this function only $(I initiates) termination of the process,
and returns immediately.  It does not wait for the process to end,
nor does it guarantee that the process does in fact get terminated.

Always call $(LREF wait) to wait for a process to complete, even if `kill`
has been called on it.

Windows_specific:
The process will be
$(LINK2 http://msdn.microsoft.com/en-us/library/windows/desktop/ms686714%28v=vs.100%29.aspx,
forcefully and abruptly terminated).  If `codeOrSignal` is specified, it
must be a nonnegative number which will be used as the exit code of the process.
If not, the process wil exit with code 1.  Do not use $(D codeOrSignal = 259),
as this is a special value (aka. $(LINK2 http://msdn.microsoft.com/en-us/library/windows/desktop/ms683189.aspx,STILL_ACTIVE))
used by Windows to signal that a process has in fact $(I not) terminated yet.
---
auto pid = spawnProcess("some_app");
kill(pid, 10);
assert(wait(pid) == 10);
---

POSIX_specific:
A $(LINK2 http://en.wikipedia.org/wiki/Unix_signal,signal) will be sent to
the process, whose value is given by `codeOrSignal`.  Depending on the
signal sent, this may or may not terminate the process.  Symbolic constants
for various $(LINK2 http://en.wikipedia.org/wiki/Unix_signal#POSIX_signals,
POSIX signals) are defined in `core.sys.posix.signal`, which corresponds to the
$(LINK2 http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/signal.h.html,
`signal.h` POSIX header).  If `codeOrSignal` is omitted, the
`SIGTERM` signal will be sent.  (This matches the behaviour of the
$(LINK2 http://pubs.opengroup.org/onlinepubs/9699919799/utilities/kill.html,
`_kill`) shell command.)
---
import core.sys.posix.signal : SIGKILL;
auto pid = spawnProcess("some_app");
kill(pid, SIGKILL);
assert(wait(pid) == -SIGKILL); // Negative return value on POSIX!
---

Throws:
$(LREF ProcessException) on error (e.g. if codeOrSignal is invalid).
    or on attempt to kill detached process.
    Note that failure to terminate the process is considered a "normal"
    outcome, not an error.$(BR)
*/
void kill(Pid pid)
{
    version (Windows) kill(pid, 1);
    else version (Posix)
    {
        import core.sys.posix.signal : SIGTERM;
        kill(pid, SIGTERM);
    }
}

/// ditto
void kill(Pid pid, int codeOrSignal)
{
    import std.exception : enforce;
    enforce!ProcessException(pid.owned, "Can't kill detached process");
    version (Windows)
    {
        if (codeOrSignal < 0) throw new ProcessException("Invalid exit code");
        // On Windows, TerminateProcess() appears to terminate the
        // *current* process if it is passed an invalid handle...
        if (pid.osHandle == INVALID_HANDLE_VALUE)
            throw new ProcessException("Invalid process handle");
        if (!TerminateProcess(pid.osHandle, codeOrSignal))
            throw ProcessException.newFromLastError();
    }
    else version (Posix)
    {
        import core.sys.posix.signal : kill;
        if (pid.osHandle == Pid.invalid)
            throw new ProcessException("Pid is invalid");
        if (pid.osHandle == Pid.terminated)
            throw new ProcessException("Pid is already terminated");
        if (kill(pid.osHandle, codeOrSignal) == -1)
            throw ProcessException.newFromErrno();
    }
}

@system unittest // tryWait() and kill()
{
    import core.thread;
    import std.exception : assertThrown;
    // The test script goes into an infinite loop.
    version (Windows)
    {
        TestScript prog = ":loop
                           goto loop";
    }
    else version (Posix)
    {
        import core.sys.posix.signal : SIGTERM, SIGKILL;
        TestScript prog = "while true; do sleep 1; done";
    }
    auto pid = spawnProcess(prog.path);
    // Android appears to automatically kill sleeping processes very quickly,
    // so shorten the wait before killing here.
    version (Android)
        Thread.sleep(dur!"msecs"(5));
    else
        Thread.sleep(dur!"msecs"(500));
    kill(pid);
    version (Windows)    assert(wait(pid) == 1);
    else version (Posix) assert(wait(pid) == -SIGTERM);

    pid = spawnProcess(prog.path);
    Thread.sleep(dur!"msecs"(500));
    auto s = tryWait(pid);
    assert(!s.terminated && s.status == 0);
    assertThrown!ProcessException(kill(pid, -123)); // Negative code not allowed.
    version (Windows)    kill(pid, 123);
    else version (Posix) kill(pid, SIGKILL);
    do { s = tryWait(pid); } while (!s.terminated);
    version (Windows)    assert(s.status == 123);
    else version (Posix) assert(s.status == -SIGKILL);
    assertThrown!ProcessException(kill(pid)); // Already terminated
}

@system unittest // wait() and kill() detached process
{
    import core.thread;
    import std.exception : assertThrown;
    TestScript prog = "exit 0";
    auto pid = spawnProcess([prog.path], null, Config.detached);
    /*
    This sleep is needed because we can't wait() for detached process to end
    and therefore TestScript destructor may run at the same time as /bin/sh tries to start the script.
    This leads to the annoying message like "/bin/sh: 0: Can't open /tmp/std.process temporary file" to appear when running tests.
    It does not happen in unittests with non-detached processes because we always wait() for them to finish.
    */
    Thread.sleep(500.msecs);
    assert(!pid.owned);
    version (Windows) assert(pid.osHandle == INVALID_HANDLE_VALUE);
    assertThrown!ProcessException(wait(pid));
    assertThrown!ProcessException(kill(pid));
}


/**
Creates a unidirectional _pipe.

Data is written to one end of the _pipe and read from the other.
---
auto p = pipe();
p.writeEnd.writeln("Hello World");
p.writeEnd.flush();
assert(p.readEnd.readln().chomp() == "Hello World");
---
Pipes can, for example, be used for interprocess communication
by spawning a new process and passing one end of the _pipe to
the child, while the parent uses the other end.
(See also $(LREF pipeProcess) and $(LREF pipeShell) for an easier
way of doing this.)
---
// Use cURL to download the dlang.org front page, pipe its
// output to grep to extract a list of links to ZIP files,
// and write the list to the file "D downloads.txt":
auto p = pipe();
auto outFile = File("D downloads.txt", "w");
auto cpid = spawnProcess(["curl", "http://dlang.org/download.html"],
                         std.stdio.stdin, p.writeEnd);
scope(exit) wait(cpid);
auto gpid = spawnProcess(["grep", "-o", `http://\S*\.zip`],
                         p.readEnd, outFile);
scope(exit) wait(gpid);
---

Returns:
A $(LREF Pipe) object that corresponds to the created _pipe.

Throws:
$(REF StdioException, std,stdio) on failure.
*/
version (Posix)
Pipe pipe() @trusted //TODO: @safe
{
    import core.sys.posix.stdio : fdopen;
    int[2] fds;
    if (core.sys.posix.unistd.pipe(fds) != 0)
        throw new StdioException("Unable to create pipe");
    Pipe p;
    auto readFP = fdopen(fds[0], "r");
    if (readFP == null)
        throw new StdioException("Cannot open read end of pipe");
    p._read = File(readFP, null);
    auto writeFP = fdopen(fds[1], "w");
    if (writeFP == null)
        throw new StdioException("Cannot open write end of pipe");
    p._write = File(writeFP, null);
    return p;
}
else version (Windows)
Pipe pipe() @trusted //TODO: @safe
{
    // use CreatePipe to create an anonymous pipe
    HANDLE readHandle;
    HANDLE writeHandle;
    if (!CreatePipe(&readHandle, &writeHandle, null, 0))
    {
        throw new StdioException(
            "Error creating pipe (" ~ generateSysErrorMsg() ~ ')',
            0);
    }

    scope(failure)
    {
        CloseHandle(readHandle);
        CloseHandle(writeHandle);
    }

    try
    {
        Pipe p;
        p._read .windowsHandleOpen(readHandle , "r");
        p._write.windowsHandleOpen(writeHandle, "a");
        return p;
    }
    catch (Exception e)
    {
        throw new StdioException("Error attaching pipe (" ~ e.msg ~ ")",
            0);
    }
}


/// An interface to a pipe created by the $(LREF pipe) function.
struct Pipe
{
    /// The read end of the pipe.
    @property File readEnd() @safe nothrow { return _read; }


    /// The write end of the pipe.
    @property File writeEnd() @safe nothrow { return _write; }


    /**
    Closes both ends of the pipe.

    Normally it is not necessary to do this manually, as $(REF File, std,stdio)
    objects are automatically closed when there are no more references
    to them.

    Note that if either end of the pipe has been passed to a child process,
    it will only be closed in the parent process.  (What happens in the
    child process is platform dependent.)

    Throws:
    $(REF ErrnoException, std,exception) if an error occurs.
    */
    void close() @safe
    {
        _read.close();
        _write.close();
    }

private:
    File _read, _write;
}

@system unittest
{
    import std.string;
    auto p = pipe();
    p.writeEnd.writeln("Hello World");
    p.writeEnd.flush();
    assert(p.readEnd.readln().chomp() == "Hello World");
    p.close();
    assert(!p.readEnd.isOpen);
    assert(!p.writeEnd.isOpen);
}


/**
Starts a new process, creating pipes to redirect its standard
input, output and/or error streams.

`pipeProcess` and `pipeShell` are convenient wrappers around
$(LREF spawnProcess) and $(LREF spawnShell), respectively, and
automate the task of redirecting one or more of the child process'
standard streams through pipes.  Like the functions they wrap,
these functions return immediately, leaving the child process to
execute in parallel with the invoking process.  It is recommended
to always call $(LREF wait) on the returned $(LREF ProcessPipes.pid),
as detailed in the documentation for `wait`.

The `args`/`program`/`command`, `env` and `config`
parameters are forwarded straight to the underlying spawn functions,
and we refer to their documentation for details.

Params:
args      = An array which contains the program name as the zeroth element
            and any command-line arguments in the following elements.
            (See $(LREF spawnProcess) for details.)
program   = The program name, $(I without) command-line arguments.
            (See $(LREF spawnProcess) for details.)
command   = A shell command which is passed verbatim to the command
            interpreter.  (See $(LREF spawnShell) for details.)
redirect  = Flags that determine which streams are redirected, and
            how.  See $(LREF Redirect) for an overview of available
            flags.
env       = Additional environment variables for the child process.
            (See $(LREF spawnProcess) for details.)
config    = Flags that control process creation. See $(LREF Config)
            for an overview of available flags, and note that the
            `retainStd...` flags have no effect in this function.
workDir   = The working directory for the new process.
            By default the child process inherits the parent's working
            directory.
shellPath = The path to the shell to use to run the specified program.
            By default this is $(LREF nativeShell).

Returns:
A $(LREF ProcessPipes) object which contains $(REF File, std,stdio)
handles that communicate with the redirected streams of the child
process, along with a $(LREF Pid) object that corresponds to the
spawned process.

Throws:
$(LREF ProcessException) on failure to start the process.$(BR)
$(REF StdioException, std,stdio) on failure to redirect any of the streams.$(BR)

Example:
---
// my_application writes to stdout and might write to stderr
auto pipes = pipeProcess("my_application", Redirect.stdout | Redirect.stderr);
scope(exit) wait(pipes.pid);

// Store lines of output.
string[] output;
foreach (line; pipes.stdout.byLine) output ~= line.idup;

// Store lines of errors.
string[] errors;
foreach (line; pipes.stderr.byLine) errors ~= line.idup;


// sendmail expects to read from stdin
pipes = pipeProcess(["/usr/bin/sendmail", "-t"], Redirect.stdin);
pipes.stdin.writeln("To: you");
pipes.stdin.writeln("From: me");
pipes.stdin.writeln("Subject: dlang");
pipes.stdin.writeln("");
pipes.stdin.writeln(message);

// a single period tells sendmail we are finished
pipes.stdin.writeln(".");

// but at this point sendmail might not see it, we need to flush
pipes.stdin.flush();

// sendmail happens to exit on ".", but some you have to close the file:
pipes.stdin.close();

// otherwise this wait will wait forever
wait(pipes.pid);

---
*/
ProcessPipes pipeProcess(scope const(char[])[] args,
                         Redirect redirect = Redirect.all,
                         const string[string] env = null,
                         Config config = Config.none,
                         scope const(char)[] workDir = null)
    @safe
{
    return pipeProcessImpl!spawnProcess(args, redirect, env, config, workDir);
}

/// ditto
ProcessPipes pipeProcess(scope const(char)[] program,
                         Redirect redirect = Redirect.all,
                         const string[string] env = null,
                         Config config = Config.none,
                         scope const(char)[] workDir = null)
    @safe
{
    return pipeProcessImpl!spawnProcess(program, redirect, env, config, workDir);
}

/// ditto
ProcessPipes pipeShell(scope const(char)[] command,
                       Redirect redirect = Redirect.all,
                       const string[string] env = null,
                       Config config = Config.none,
                       scope const(char)[] workDir = null,
                       string shellPath = nativeShell)
    @safe
{
    return pipeProcessImpl!spawnShell(command,
                                      redirect,
                                      env,
                                      config,
                                      workDir,
                                      shellPath);
}

// Implementation of the pipeProcess() family of functions.
private ProcessPipes pipeProcessImpl(alias spawnFunc, Cmd, ExtraSpawnFuncArgs...)
                                    (scope Cmd command,
                                     Redirect redirectFlags,
                                     const string[string] env = null,
                                     Config config = Config.none,
                                     scope const(char)[] workDir = null,
                                     ExtraSpawnFuncArgs extraArgs = ExtraSpawnFuncArgs.init)
    @trusted //TODO: @safe
{
    File childStdin, childStdout, childStderr;
    ProcessPipes pipes;
    pipes._redirectFlags = redirectFlags;

    if (redirectFlags & Redirect.stdin)
    {
        auto p = pipe();
        childStdin = p.readEnd;
        pipes._stdin = p.writeEnd;
    }
    else
    {
        childStdin = std.stdio.stdin;
    }

    if (redirectFlags & Redirect.stdout)
    {
        if ((redirectFlags & Redirect.stdoutToStderr) != 0)
            throw new StdioException("Cannot create pipe for stdout AND "
                                     ~"redirect it to stderr", 0);
        auto p = pipe();
        childStdout = p.writeEnd;
        pipes._stdout = p.readEnd;
    }
    else
    {
        childStdout = std.stdio.stdout;
    }

    if (redirectFlags & Redirect.stderr)
    {
        if ((redirectFlags & Redirect.stderrToStdout) != 0)
            throw new StdioException("Cannot create pipe for stderr AND "
                                     ~"redirect it to stdout", 0);
        auto p = pipe();
        childStderr = p.writeEnd;
        pipes._stderr = p.readEnd;
    }
    else
    {
        childStderr = std.stdio.stderr;
    }

    if (redirectFlags & Redirect.stdoutToStderr)
    {
        if (redirectFlags & Redirect.stderrToStdout)
        {
            // We know that neither of the other options have been
            // set, so we assign the std.stdio.std* streams directly.
            childStdout = std.stdio.stderr;
            childStderr = std.stdio.stdout;
        }
        else
        {
            childStdout = childStderr;
        }
    }
    else if (redirectFlags & Redirect.stderrToStdout)
    {
        childStderr = childStdout;
    }

    config.flags &= ~(Config.Flags.retainStdin | Config.Flags.retainStdout | Config.Flags.retainStderr);
    pipes._pid = spawnFunc(command, childStdin, childStdout, childStderr,
                           env, config, workDir, extraArgs);
    return pipes;
}


/**
Flags that can be passed to $(LREF pipeProcess) and $(LREF pipeShell)
to specify which of the child process' standard streams are redirected.
Use bitwise OR to combine flags.
*/
enum Redirect
{
    /// Redirect the standard input, output or error streams, respectively.
    stdin = 1,
    stdout = 2,                             /// ditto
    stderr = 4,                             /// ditto

    /**
    Redirect _all three streams.  This is equivalent to
    $(D Redirect.stdin | Redirect.stdout | Redirect.stderr).
    */
    all = stdin | stdout | stderr,

    /**
    Redirect the standard error stream into the standard output stream.
    This can not be combined with `Redirect.stderr`.
    */
    stderrToStdout = 8,

    /**
    Redirect the standard output stream into the standard error stream.
    This can not be combined with `Redirect.stdout`.
    */
    stdoutToStderr = 16,
}

@system unittest
{
    import std.string;
    version (Windows) TestScript prog =
       "call :sub %~1 %~2 0
        call :sub %~1 %~2 1
        call :sub %~1 %~2 2
        call :sub %~1 %~2 3
        exit 3

        :sub
        set /p INPUT=
        if -%INPUT%-==-stop- ( exit %~3 )
        echo %INPUT% %~1
        echo %INPUT% %~2 1>&2";
    else version (Posix) TestScript prog =
       `for EXITCODE in 0 1 2 3; do
            read INPUT
            if test "$INPUT" = stop; then break; fi
            echo "$INPUT $1"
            echo "$INPUT $2" >&2
        done
        exit $EXITCODE`;
    auto pp = pipeProcess([prog.path, "bar", "baz"]);
    pp.stdin.writeln("foo");
    pp.stdin.flush();
    assert(pp.stdout.readln().chomp() == "foo bar");
    assert(pp.stderr.readln().chomp().stripRight() == "foo baz");
    pp.stdin.writeln("1234567890");
    pp.stdin.flush();
    assert(pp.stdout.readln().chomp() == "1234567890 bar");
    assert(pp.stderr.readln().chomp().stripRight() == "1234567890 baz");
    pp.stdin.writeln("stop");
    pp.stdin.flush();
    assert(wait(pp.pid) == 2);

    pp = pipeProcess([prog.path, "12345", "67890"],
                     Redirect.stdin | Redirect.stdout | Redirect.stderrToStdout);
    pp.stdin.writeln("xyz");
    pp.stdin.flush();
    assert(pp.stdout.readln().chomp() == "xyz 12345");
    assert(pp.stdout.readln().chomp().stripRight() == "xyz 67890");
    pp.stdin.writeln("stop");
    pp.stdin.flush();
    assert(wait(pp.pid) == 1);

    pp = pipeShell(escapeShellCommand(prog.path, "AAAAA", "BBB"),
                   Redirect.stdin | Redirect.stdoutToStderr | Redirect.stderr);
    pp.stdin.writeln("ab");
    pp.stdin.flush();
    assert(pp.stderr.readln().chomp() == "ab AAAAA");
    assert(pp.stderr.readln().chomp().stripRight() == "ab BBB");
    pp.stdin.writeln("stop");
    pp.stdin.flush();
    assert(wait(pp.pid) == 1);
}

@system unittest
{
    import std.exception : assertThrown;
    TestScript prog = "exit 0";
    assertThrown!StdioException(pipeProcess(
        prog.path,
        Redirect.stdout | Redirect.stdoutToStderr));
    assertThrown!StdioException(pipeProcess(
        prog.path,
        Redirect.stderr | Redirect.stderrToStdout));
    auto p = pipeProcess(prog.path, Redirect.stdin);
    assertThrown!Error(p.stdout);
    assertThrown!Error(p.stderr);
    wait(p.pid);
    p = pipeProcess(prog.path, Redirect.stderr);
    assertThrown!Error(p.stdin);
    assertThrown!Error(p.stdout);
    wait(p.pid);
}

/**
Object which contains $(REF File, std,stdio) handles that allow communication
with a child process through its standard streams.
*/
struct ProcessPipes
{
    /// The $(LREF Pid) of the child process.
    @property Pid pid() @safe nothrow
    {
        return _pid;
    }

    /**
    An $(REF File, std,stdio) that allows writing to the child process'
    standard input stream.

    Throws:
    $(OBJECTREF Error) if the child process' standard input stream hasn't
    been redirected.
    */
    @property File stdin() @safe nothrow
    {
        if ((_redirectFlags & Redirect.stdin) == 0)
            throw new Error("Child process' standard input stream hasn't "
                            ~"been redirected.");
        return _stdin;
    }

    /**
    An $(REF File, std,stdio) that allows reading from the child process'
    standard output stream.

    Throws:
    $(OBJECTREF Error) if the child process' standard output stream hasn't
    been redirected.
    */
    @property File stdout() @safe nothrow
    {
        if ((_redirectFlags & Redirect.stdout) == 0)
            throw new Error("Child process' standard output stream hasn't "
                            ~"been redirected.");
        return _stdout;
    }

    /**
    An $(REF File, std,stdio) that allows reading from the child process'
    standard error stream.

    Throws:
    $(OBJECTREF Error) if the child process' standard error stream hasn't
    been redirected.
    */
    @property File stderr() @safe nothrow
    {
        if ((_redirectFlags & Redirect.stderr) == 0)
            throw new Error("Child process' standard error stream hasn't "
                            ~"been redirected.");
        return _stderr;
    }

private:
    Redirect _redirectFlags;
    Pid _pid;
    File _stdin, _stdout, _stderr;
}



/**
Executes the given program or shell command and returns its exit
code and output.

`execute` and `executeShell` start a new process using
$(LREF spawnProcess) and $(LREF spawnShell), respectively, and wait
for the process to complete before returning.  The functions capture
what the child process prints to both its standard output and
standard error streams, and return this together with its exit code.
---
auto dmd = execute(["dmd", "myapp.d"]);
if (dmd.status != 0) writeln("Compilation failed:\n", dmd.output);

auto ls = executeShell("ls -l");
if (ls.status != 0) writeln("Failed to retrieve file listing");
else writeln(ls.output);
---

The `args`/`program`/`command`, `env` and `config`
parameters are forwarded straight to the underlying spawn functions,
and we refer to their documentation for details.

Params:
args      = An array which contains the program name as the zeroth element
            and any command-line arguments in the following elements.
            (See $(LREF spawnProcess) for details.)
program   = The program name, $(I without) command-line arguments.
            (See $(LREF spawnProcess) for details.)
command   = A shell command which is passed verbatim to the command
            interpreter.  (See $(LREF spawnShell) for details.)
env       = Additional environment variables for the child process.
            (See $(LREF spawnProcess) for details.)
config    = Flags that control process creation. See $(LREF Config)
            for an overview of available flags, and note that the
            `retainStd...` flags have no effect in this function.
maxOutput = The maximum number of bytes of output that should be
            captured.
workDir   = The working directory for the new process.
            By default the child process inherits the parent's working
            directory.
shellPath = The path to the shell to use to run the specified program.
            By default this is $(LREF nativeShell).


Returns:
An $(D std.typecons.Tuple!(int, "status", string, "output")).

POSIX_specific:
If the process is terminated by a signal, the `status` field of
the return value will contain a negative number whose absolute
value is the signal number.  (See $(LREF wait) for details.)

Throws:
$(LREF ProcessException) on failure to start the process.$(BR)
$(REF StdioException, std,stdio) on failure to capture output.
*/
auto execute(scope const(char[])[] args,
             const string[string] env = null,
             Config config = Config.none,
             size_t maxOutput = size_t.max,
             scope const(char)[] workDir = null)
    @safe
{
    return executeImpl!pipeProcess(args, env, config, maxOutput, workDir);
}

/// ditto
auto execute(scope const(char)[] program,
             const string[string] env = null,
             Config config = Config.none,
             size_t maxOutput = size_t.max,
             scope const(char)[] workDir = null)
    @safe
{
    return executeImpl!pipeProcess(program, env, config, maxOutput, workDir);
}

/// ditto
auto executeShell(scope const(char)[] command,
                  const string[string] env = null,
                  Config config = Config.none,
                  size_t maxOutput = size_t.max,
                  scope const(char)[] workDir = null,
                  string shellPath = nativeShell)
    @safe
{
    return executeImpl!pipeShell(command,
                                 env,
                                 config,
                                 maxOutput,
                                 workDir,
                                 shellPath);
}

// Does the actual work for execute() and executeShell().
private auto executeImpl(alias pipeFunc, Cmd, ExtraPipeFuncArgs...)(
    Cmd commandLine,
    const string[string] env = null,
    Config config = Config.none,
    size_t maxOutput = size_t.max,
    scope const(char)[] workDir = null,
    ExtraPipeFuncArgs extraArgs = ExtraPipeFuncArgs.init)
    @trusted //TODO: @safe
{
    import std.algorithm.comparison : min;
    import std.array : appender;
    import std.typecons : Tuple;

    auto redirect = (config.flags & Config.Flags.stderrPassThrough)
        ? Redirect.stdout
        : Redirect.stdout | Redirect.stderrToStdout;

    auto p = pipeFunc(commandLine, redirect,
                      env, config, workDir, extraArgs);

    auto a = appender!string;
    enum size_t defaultChunkSize = 4096;
    immutable chunkSize = min(maxOutput, defaultChunkSize);

    // Store up to maxOutput bytes in a.
    foreach (ubyte[] chunk; p.stdout.byChunk(chunkSize))
    {
        immutable size_t remain = maxOutput - a.data.length;

        if (chunk.length < remain) a.put(chunk);
        else
        {
            a.put(chunk[0 .. remain]);
            break;
        }
    }
    // Exhaust the stream, if necessary.
    foreach (ubyte[] chunk; p.stdout.byChunk(defaultChunkSize)) { }

    return Tuple!(int, "status", string, "output")(wait(p.pid), a.data);
}

@system unittest
{
    import std.string;
    // To avoid printing the newline characters, we use the echo|set trick on
    // Windows, and printf on POSIX (neither echo -n nor echo \c are portable).
    version (Windows) TestScript prog =
       "echo|set /p=%~1
        echo|set /p=%~2 1>&2
        exit 123";
    else version (Android) TestScript prog =
       `echo -n $1
        echo -n $2 >&2
        exit 123`;
    else version (Posix) TestScript prog =
       `printf '%s' $1
        printf '%s' $2 >&2
        exit 123`;
    auto r = execute([prog.path, "foo", "bar"]);
    assert(r.status == 123);
    assert(r.output.stripRight() == "foobar");
    auto s = execute([prog.path, "Hello", "World"]);
    assert(s.status == 123);
    assert(s.output.stripRight() == "HelloWorld");
}

@safe unittest
{
    import std.string;
    auto r1 = executeShell("echo foo");
    assert(r1.status == 0);
    assert(r1.output.chomp() == "foo");
    auto r2 = executeShell("echo bar 1>&2");
    assert(r2.status == 0);
    assert(r2.output.chomp().stripRight() == "bar");
    auto r3 = executeShell("exit 123");
    assert(r3.status == 123);
    assert(r3.output.empty);
}

@system unittest
{
    // Temporarily disable output to stderr so as to not spam the build log.
    import std.stdio : stderr;
    import std.typecons : Tuple;
    import std.file : readText, exists, remove;
    import std.traits : ReturnType;

    ReturnType!executeShell r;
    auto tmpname = uniqueTempPath;
    scope(exit) if (exists(tmpname)) remove(tmpname);
    auto t = stderr;
    // Open a new scope to minimize code ran with stderr redirected.
    {
        stderr.open(tmpname, "w");
        scope(exit) stderr = t;
        r = executeShell("echo D rox>&2", null, Config.stderrPassThrough);
    }
    assert(r.status == 0);
    assert(r.output.empty);
    auto witness = readText(tmpname);
    import std.ascii : newline;
    assert(witness == "D rox" ~ newline, "'" ~ witness ~ "'");
}

@safe unittest
{
    import std.typecons : Tuple;
    void foo() //Just test the compilation
    {
        auto ret1 = execute(["dummy", "arg"]);
        auto ret2 = executeShell("dummy arg");
        static assert(is(typeof(ret1) == typeof(ret2)));

        Tuple!(int, string) ret3 = execute(["dummy", "arg"]);
    }
}

/// An exception that signals a problem with starting or waiting for a process.
class ProcessException : Exception
{
    import std.exception : basicExceptionCtors;
    mixin basicExceptionCtors;

    // Creates a new ProcessException based on errno.
    static ProcessException newFromErrno(string customMsg = null,
                                         string file = __FILE__,
                                         size_t line = __LINE__)
    {
        import core.stdc.errno : errno;
        return newFromErrno(errno, customMsg, file, line);
    }

    // ditto, but error number is provided by caller
    static ProcessException newFromErrno(int error,
                                         string customMsg = null,
                                         string file = __FILE__,
                                         size_t line = __LINE__)
    {
        import std.exception : errnoString;
        auto errnoMsg = errnoString(error);
        auto msg = customMsg.empty ? errnoMsg
                                   : customMsg ~ " (" ~ errnoMsg ~ ')';
        return new ProcessException(msg, file, line);
    }

    // Creates a new ProcessException based on GetLastError() (Windows only).
    version (Windows)
    static ProcessException newFromLastError(string customMsg = null,
                                             string file = __FILE__,
                                             size_t line = __LINE__)
    {
        auto lastMsg = generateSysErrorMsg();
        auto msg = customMsg.empty ? lastMsg
                                   : customMsg ~ " (" ~ lastMsg ~ ')';
        return new ProcessException(msg, file, line);
    }
}


/**
Determines the path to the current user's preferred command interpreter.

On Windows, this function returns the contents of the COMSPEC environment
variable, if it exists.  Otherwise, it returns the result of $(LREF nativeShell).

On POSIX, `userShell` returns the contents of the SHELL environment
variable, if it exists and is non-empty.  Otherwise, it returns the result of
$(LREF nativeShell).
*/
@property string userShell() @safe
{
    version (Windows)      return environment.get("COMSPEC", nativeShell);
    else version (Posix)   return environment.get("SHELL", nativeShell);
}

/**
The platform-specific native shell path.

This function returns `"cmd.exe"` on Windows, `"/bin/sh"` on POSIX, and
`"/system/bin/sh"` on Android.
*/
@property string nativeShell() @safe @nogc pure nothrow
{
    version (Windows)      return "cmd.exe";
    else version (Android) return "/system/bin/sh";
    else version (Posix)   return "/bin/sh";
}

// A command-line switch that indicates to the shell that it should
// interpret the following argument as a command to be executed.
version (Posix)   private immutable string shellSwitch = "-c";
version (Windows) private immutable string shellSwitch = "/C";

// Unittest support code:  TestScript takes a string that contains a
// shell script for the current platform, and writes it to a temporary
// file. On Windows the file name gets a .cmd extension, while on
// POSIX its executable permission bit is set.  The file is
// automatically deleted when the object goes out of scope.
version (StdUnittest)
private struct TestScript
{
    this(string code) @system
    {
        // @system due to chmod
        import std.ascii : newline;
        import std.file : write;
        version (Windows)
        {
            auto ext = ".cmd";
            auto firstLine = "@echo off";
        }
        else version (Posix)
        {
            auto ext = "";
            auto firstLine = "#!" ~ nativeShell;
        }
        path = uniqueTempPath()~ext;
        write(path, firstLine ~ newline ~ code ~ newline);
        version (Posix)
        {
            import core.sys.posix.sys.stat : chmod;
            import std.conv : octal;
            chmod(path.tempCString(), octal!777);
        }
    }

    ~this()
    {
        import std.file : remove, exists;
        if (!path.empty && exists(path))
        {
            try { remove(path); }
            catch (Exception e)
            {
                debug std.stdio.stderr.writeln(e.msg);
            }
        }
    }

    string path;
}


// =============================================================================
// Functions for shell command quoting/escaping.
// =============================================================================


/*
    Command line arguments exist in three forms:
    1) string or char* array, as received by main.
       Also used internally on POSIX systems.
    2) Command line string, as used in Windows'
       CreateProcess and CommandLineToArgvW functions.
       A specific quoting and escaping algorithm is used
       to distinguish individual arguments.
    3) Shell command string, as written at a shell prompt
       or passed to cmd /C - this one may contain shell
       control characters, e.g. > or | for redirection /
       piping - thus, yet another layer of escaping is
       used to distinguish them from program arguments.

    Except for escapeWindowsArgument, the intermediary
    format (2) is hidden away from the user in this module.
*/

/**
Escapes an argv-style argument array to be used with $(LREF spawnShell),
$(LREF pipeShell) or $(LREF executeShell).
---
string url = "http://dlang.org/";
executeShell(escapeShellCommand("wget", url, "-O", "dlang-index.html"));
---

Concatenate multiple `escapeShellCommand` and
$(LREF escapeShellFileName) results to use shell redirection or
piping operators.
---
executeShell(
    escapeShellCommand("curl", "http://dlang.org/download.html") ~
    "|" ~
    escapeShellCommand("grep", "-o", `http://\S*\.zip`) ~
    ">" ~
    escapeShellFileName("D download links.txt"));
---

Throws:
$(OBJECTREF Exception) if any part of the command line contains unescapable
characters (NUL on all platforms, as well as CR and LF on Windows).
*/
string escapeShellCommand(scope const(char[])[] args...) @safe pure
{
    if (args.empty)
        return null;
    version (Windows)
    {
        // Do not ^-escape the first argument (the program path),
        // as the shell parses it differently from parameters.
        // ^-escaping a program path that contains spaces will fail.
        string result = escapeShellFileName(args[0]);
        if (args.length > 1)
        {
            result ~= " " ~ escapeShellCommandString(
                escapeShellArguments(args[1..$]));
        }
        return result;
    }
    version (Posix)
    {
        return escapeShellCommandString(escapeShellArguments(args));
    }
}

@safe unittest
{
    // This is a simple unit test without any special requirements,
    // in addition to the unittest_burnin one below which requires
    // special preparation.

    struct TestVector { string[] args; string windows, posix; }
    TestVector[] tests =
    [
        {
            args    : ["foo bar"],
            windows : `"foo bar"`,
            posix   : `'foo bar'`
        },
        {
            args    : ["foo bar", "hello"],
            windows : `"foo bar" hello`,
            posix   : `'foo bar' hello`
        },
        {
            args    : ["foo bar", "hello world"],
            windows : `"foo bar" ^"hello world^"`,
            posix   : `'foo bar' 'hello world'`
        },
        {
            args    : ["foo bar", "hello", "world"],
            windows : `"foo bar" hello world`,
            posix   : `'foo bar' hello world`
        },
        {
            args    : ["foo bar", `'"^\`],
            windows : `"foo bar" ^"'\^"^^\\^"`,
            posix   : `'foo bar' ''\''"^\'`
        },
        {
            args    : ["foo bar", ""],
            windows : `"foo bar" ^"^"`,
            posix   : `'foo bar' ''`
        },
        {
            args    : ["foo bar", "2"],
            windows : `"foo bar" ^"2^"`,
            posix   : `'foo bar' '2'`
        },
    ];

    foreach (test; tests)
    {
        auto actual = escapeShellCommand(test.args);
        version (Windows)
            string expected = test.windows;
        else
            string expected = test.posix;
        assert(actual == expected, "\nExpected: " ~ expected ~ "\nGot: " ~ actual);
    }
}

private string escapeShellCommandString(return scope string command) @safe pure
{
    version (Windows)
        return escapeWindowsShellCommand(command);
    else
        return command;
}

private string escapeWindowsShellCommand(scope const(char)[] command) @safe pure
{
    import std.array : appender;
    auto result = appender!string();
    result.reserve(command.length);

    foreach (c; command)
        switch (c)
        {
            case '\0':
                throw new Exception("Cannot put NUL in command line");
            case '\r':
            case '\n':
                throw new Exception("CR/LF are not escapable");
            case '\x01': .. case '\x09':
            case '\x0B': .. case '\x0C':
            case '\x0E': .. case '\x1F':
            case '"':
            case '^':
            case '&':
            case '<':
            case '>':
            case '|':
                result.put('^');
                goto default;
            default:
                result.put(c);
        }
    return result.data;
}

private string escapeShellArguments(scope const(char[])[] args...)
    @trusted pure nothrow
{
    import std.exception : assumeUnique;
    char[] buf;

    @safe nothrow
    char[] allocator(size_t size)
    {
        if (buf.length == 0)
            return buf = new char[size];
        else
        {
            auto p = buf.length;
            buf.length = buf.length + 1 + size;
            buf[p++] = ' ';
            return buf[p .. p+size];
        }
    }

    foreach (arg; args)
        escapeShellArgument!allocator(arg);
    return assumeUnique(buf);
}

private auto escapeShellArgument(alias allocator)(scope const(char)[] arg) @safe nothrow
{
    // The unittest for this function requires special
    // preparation - see below.

    version (Windows)
        return escapeWindowsArgumentImpl!allocator(arg);
    else
        return escapePosixArgumentImpl!allocator(arg);
}

/**
Quotes a command-line argument in a manner conforming to the behavior of
$(LINK2 http://msdn.microsoft.com/en-us/library/windows/desktop/bb776391(v=vs.85).aspx,
CommandLineToArgvW).
*/
string escapeWindowsArgument(scope const(char)[] arg) @trusted pure nothrow
{
    // Rationale for leaving this function as public:
    // this algorithm of escaping paths is also used in other software,
    // e.g. DMD's response files.
    import std.exception : assumeUnique;
    auto buf = escapeWindowsArgumentImpl!charAllocator(arg);
    return assumeUnique(buf);
}


private char[] charAllocator(size_t size) @safe pure nothrow
{
    return new char[size];
}


private char[] escapeWindowsArgumentImpl(alias allocator)(scope const(char)[] arg)
    @safe nothrow
if (is(typeof(allocator(size_t.init)[0] = char.init)))
{
    // References:
    // * http://msdn.microsoft.com/en-us/library/windows/desktop/bb776391(v=vs.85).aspx
    // * http://blogs.msdn.com/b/oldnewthing/archive/2010/09/17/10063629.aspx

    // Check if the string needs to be escaped,
    // and calculate the total string size.

    // Trailing backslashes must be escaped
    bool escaping = true;
    bool needEscape = false;
    // Result size = input size + 2 for surrounding quotes + 1 for the
    // backslash for each escaped character.
    size_t size = 1 + arg.length + 1;

    foreach_reverse (char c; arg)
    {
        if (c == '"')
        {
            needEscape = true;
            escaping = true;
            size++;
        }
        else
        if (c == '\\')
        {
            if (escaping)
                size++;
        }
        else
        {
            if (c == ' ' || c == '\t')
                needEscape = true;
            escaping = false;
        }
    }

    import std.ascii : isDigit;
    // Empty arguments need to be specified as ""
    if (!arg.length)
        needEscape = true;
    else
    // Arguments ending with digits need to be escaped,
    // to disambiguate with 1>file redirection syntax
    if (isDigit(arg[$-1]))
        needEscape = true;

    if (!needEscape)
        return allocator(arg.length)[] = arg;

    // Construct result string.

    auto buf = allocator(size);
    size_t p = size;
    buf[--p] = '"';
    escaping = true;
    foreach_reverse (char c; arg)
    {
        if (c == '"')
            escaping = true;
        else
        if (c != '\\')
            escaping = false;

        buf[--p] = c;
        if (escaping)
            buf[--p] = '\\';
    }
    buf[--p] = '"';
    assert(p == 0);

    return buf;
}

version (Windows) version (StdUnittest)
{
private:
    import core.stdc.stddef;
    import core.stdc.wchar_ : wcslen;
    import core.sys.windows.shellapi : CommandLineToArgvW;
    import core.sys.windows.winbase;
    import core.sys.windows.winnt;
    import std.array;

    string[] parseCommandLine(string line)
    {
        import std.algorithm.iteration : map;
        import std.array : array;
        import std.conv : to;
        auto lpCommandLine = (to!(WCHAR[])(line) ~ '\0').ptr;
        int numArgs;
        auto args = CommandLineToArgvW(lpCommandLine, &numArgs);
        scope(exit) LocalFree(args);
        return args[0 .. numArgs]
            .map!(arg => to!string(arg[0 .. wcslen(arg)]))
            .array();
    }

    @system unittest
    {
        import std.conv : text;
        string[] testStrings = [
            `Hello`,
            `Hello, world`,
            `Hello, "world"`,
            `C:\`,
            `C:\dmd`,
            `C:\Program Files\`,
        ];

        enum CHARS = `_x\" *&^` ~ "\t"; // _ is placeholder for nothing
        foreach (c1; CHARS)
        foreach (c2; CHARS)
        foreach (c3; CHARS)
        foreach (c4; CHARS)
            testStrings ~= [c1, c2, c3, c4].replace("_", "");

        foreach (s; testStrings)
        {
            auto q = escapeWindowsArgument(s);
            auto args = parseCommandLine("Dummy.exe " ~ q);
            assert(args.length == 2, s ~ " => " ~ q ~ " #" ~ text(args.length-1));
            assert(args[1] == s, s ~ " => " ~ q ~ " => " ~ args[1]);
        }
    }
}

private string escapePosixArgument(scope const(char)[] arg) @trusted pure nothrow
{
    import std.exception : assumeUnique;
    auto buf = escapePosixArgumentImpl!charAllocator(arg);
    return assumeUnique(buf);
}

private char[] escapePosixArgumentImpl(alias allocator)(scope const(char)[] arg)
    @safe nothrow
if (is(typeof(allocator(size_t.init)[0] = char.init)))
{
    bool needQuoting = {
        import std.ascii : isAlphaNum, isDigit;
        import std.algorithm.comparison : among;

        // Empty arguments need to be specified as ''
        if (arg.length == 0)
            return true;
        // Arguments ending with digits need to be escaped,
        // to disambiguate with 1>file redirection syntax
        if (isDigit(arg[$-1]))
            return true;

        // Obtained using:
        // for n in $(seq 1 255) ; do
        //     c=$(printf \\$(printf "%o" $n))
        //     q=$(/bin/printf '%q' "$c")
        //     if [[ "$q" == "$c" ]] ; then printf "%s, " "'$c'" ; fi
        // done
        // printf '\n'
        foreach (char c; arg)
            if (!isAlphaNum(c) && !c.among('%', '+', ',', '-', '.', '/', ':', '@', ']', '_'))
                return true;
        return false;
    }();
    if (!needQuoting)
    {
        auto buf = allocator(arg.length);
        buf[] = arg;
        return buf;
    }

    // '\'' means: close quoted part of argument, append an escaped
    // single quote, and reopen quotes

    // Below code is equivalent to:
    // return `'` ~ std.array.replace(arg, `'`, `'\''`) ~ `'`;

    size_t size = 1 + arg.length + 1;
    foreach (char c; arg)
        if (c == '\'')
            size += 3;

    auto buf = allocator(size);
    size_t p = 0;
    buf[p++] = '\'';
    foreach (char c; arg)
        if (c == '\'')
        {
            buf[p .. p+4] = `'\''`;
            p += 4;
        }
        else
            buf[p++] = c;
    buf[p++] = '\'';
    assert(p == size);

    return buf;
}

/**
Escapes a filename to be used for shell redirection with $(LREF spawnShell),
$(LREF pipeShell) or $(LREF executeShell).
*/
string escapeShellFileName(scope const(char)[] fileName) @trusted pure nothrow
{
    // The unittest for this function requires special
    // preparation - see below.

    version (Windows)
    {
        // If a file starts with &, it can cause cmd.exe to misinterpret
        // the file name as the stream redirection syntax:
        //     command > "&foo.txt"
        // gets interpreted as
        //     command >&foo.txt
        // Prepend .\ to disambiguate.

        if (fileName.length && fileName[0] == '&')
            return cast(string)(`".\` ~ fileName ~ '"');

        return cast(string)('"' ~ fileName ~ '"');
    }
    else
        return escapePosixArgument(fileName);
}

// Loop generating strings with random characters
//version = unittest_burnin;

version (unittest_burnin)
@system unittest
{
    // There are no readily-available commands on all platforms suitable
    // for properly testing command escaping. The behavior of CMD's "echo"
    // built-in differs from the POSIX program, and Windows ports of POSIX
    // environments (Cygwin, msys, gnuwin32) may interfere with their own
    // "echo" ports.

    // To run this unit test, create std_process_unittest_helper.d with the
    // following content and compile it:
    // import std.stdio, std.array; void main(string[] args) { write(args.join("\0")); }
    // Then, test this module with:
    // rdmd --main -unittest -version=unittest_burnin process.d

    import std.file : readText, remove;
    import std.format : format;
    import std.path : absolutePath;
    import std.random : uniform;

    auto helper = absolutePath("std_process_unittest_helper");
    assert(executeShell(helper ~ " hello").output.split("\0")[1..$] == ["hello"], "Helper malfunction");

    void test(string[] s, string fn)
    {
        string e;
        string[] g;

        e = escapeShellCommand(helper ~ s);
        {
            scope(failure) writefln("executeShell() failed.\nExpected:\t%s\nEncoded:\t%s", s, [e]);
            auto result = executeShell(e);
            assert(result.status == 0, "std_process_unittest_helper failed");
            g = result.output.split("\0")[1..$];
        }
        assert(s == g, format("executeShell() test failed.\nExpected:\t%s\nGot:\t\t%s\nEncoded:\t%s", s, g, [e]));

        e = escapeShellCommand(helper ~ s) ~ ">" ~ escapeShellFileName(fn);
        {
            scope(failure) writefln(
                "executeShell() with redirect failed.\nExpected:\t%s\nFilename:\t%s\nEncoded:\t%s", s, [fn], [e]);
            auto result = executeShell(e);
            assert(result.status == 0, "std_process_unittest_helper failed");
            assert(!result.output.length, "No output expected, got:\n" ~ result.output);
            g = readText(fn).split("\0")[1..$];
        }
        remove(fn);
        assert(s == g,
            format("executeShell() with redirect test failed.\nExpected:\t%s\nGot:\t\t%s\nEncoded:\t%s", s, g, [e]));
    }

    while (true)
    {
        string[] args;
        foreach (n; 0 .. uniform(1, 4))
        {
            string arg;
            foreach (l; 0 .. uniform(0, 10))
            {
                dchar c;
                while (true)
                {
                    version (Windows)
                    {
                        // As long as DMD's system() uses CreateProcessA,
                        // we can't reliably pass Unicode
                        c = uniform(0, 128);
                    }
                    else
                        c = uniform!ubyte();

                    if (c == 0)
                        continue; // argv-strings are zero-terminated
                    version (Windows)
                        if (c == '\r' || c == '\n')
                            continue; // newlines are unescapable on Windows
                    break;
                }
                arg ~= c;
            }
            args ~= arg;
        }

        // generate filename
        string fn;
        foreach (l; 0 .. uniform(1, 10))
        {
            dchar c;
            while (true)
            {
                version (Windows)
                    c = uniform(0, 128); // as above
                else
                    c = uniform!ubyte();

                if (c == 0 || c == '/')
                    continue; // NUL and / are the only characters
                              // forbidden in POSIX filenames
                version (Windows)
                    if (c < '\x20' || c == '<' || c == '>' || c == ':' ||
                        c == '"' || c == '\\' || c == '|' || c == '?' || c == '*')
                        continue; // http://msdn.microsoft.com/en-us/library/aa365247(VS.85).aspx
                break;
            }

            fn ~= c;
        }
        fn = fn[0..$/2] ~ "_testfile_" ~ fn[$/2..$];

        test(args, fn);
    }
}

// =============================================================================
// Everything below this line was part of the old std.process, and most of
// it will be deprecated and removed.
// =============================================================================


/*
Copyright: Copyright The D Language Foundation 2007 - 2009.
License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   $(HTTP digitalmars.com, Walter Bright),
           $(HTTP erdani.org, Andrei Alexandrescu),
           $(HTTP thecybershadow.net, Vladimir Panteleev)
Source:    $(PHOBOSSRC std/_process.d)
*/
/*
         Copyright The D Language Foundation 2007 - 2009.
Distributed under the Boost Software License, Version 1.0.
   (See accompanying file LICENSE_1_0.txt or copy at
         http://www.boost.org/LICENSE_1_0.txt)
*/


import core.stdc.errno;
import core.stdc.stdlib;
import core.stdc.string;
import core.thread;

version (Windows)
{
    import std.file, std.format, std.random;
}
version (Posix)
{
    import core.sys.posix.stdlib;
}

private const(char)** toAStringz(in string[] a)
{
    import std.string : toStringz;
    auto p = (new const(char)*[1 + a.length]).ptr;
    foreach (i, string s; a)
        p[i] = toStringz(s);
    p[a.length] = null;
    return p;
}


/* ========================================================== */

//version (Windows)
//{
//    int spawnvp(int mode, string pathname, string[] argv)
//    {
//      char** argv_ = cast(char**) core.stdc.stdlib.malloc((char*).sizeof * (1 + argv.length));
//      scope(exit) core.stdc.stdlib.free(argv_);
//
//      toAStringz(argv, argv_);
//
//      return spawnvp(mode, pathname.tempCString(), argv_);
//    }
//}

// Incorporating idea (for spawnvp() on Posix) from Dave Fladebo

enum { _P_WAIT, _P_NOWAIT, _P_OVERLAY }
version (Windows) extern(C) int spawnvp(int, scope const(char) *, scope const(char*)*);
alias P_WAIT = _P_WAIT;
alias P_NOWAIT = _P_NOWAIT;

/* ========================================================== */

version (StdDdoc)
{
    /**
    Replaces the current process by executing a command, `pathname`, with
    the arguments in `argv`.

    $(BLUE This function is Posix-Only.)

    Typically, the first element of `argv` is
    the command being executed, i.e. $(D argv[0] == pathname). The 'p'
    versions of `exec` search the PATH environment variable for $(D
    pathname). The 'e' versions additionally take the new process'
    environment variables as an array of strings of the form key=value.

    Does not return on success (the current process will have been
    replaced). Returns -1 on failure with no indication of the
    underlying error.

    Windows_specific:
    These functions are only supported on POSIX platforms, as the Windows
    operating systems do not provide the ability to overwrite the current
    process image with another. In single-threaded programs it is possible
    to approximate the effect of `execv*` by using $(LREF spawnProcess)
    and terminating the current process once the child process has returned.
    For example:
    ---
    auto commandLine = [ "program", "arg1", "arg2" ];
    version (Posix)
    {
        execv(commandLine[0], commandLine);
        throw new Exception("Failed to execute program");
    }
    else version (Windows)
    {
        import core.stdc.stdlib : _Exit;
        _Exit(wait(spawnProcess(commandLine)));
    }
    ---
    This is, however, NOT equivalent to POSIX' `execv*`.  For one thing, the
    executed program is started as a separate process, with all this entails.
    Secondly, in a multithreaded program, other threads will continue to do
    work while the current thread is waiting for the child process to complete.

    A better option may sometimes be to terminate the current program immediately
    after spawning the child process.  This is the behaviour exhibited by the
    $(LINK2 http://msdn.microsoft.com/en-us/library/431x4c1w.aspx,`__exec`)
    functions in Microsoft's C runtime library, and it is how D's now-deprecated
    Windows `execv*` functions work. Example:
    ---
    auto commandLine = [ "program", "arg1", "arg2" ];
    version (Posix)
    {
        execv(commandLine[0], commandLine);
        throw new Exception("Failed to execute program");
    }
    else version (Windows)
    {
        spawnProcess(commandLine);
        import core.stdc.stdlib : _exit;
        _exit(0);
    }
    ---
    */
    int execv(in string pathname, in string[] argv);
    ///ditto
    int execve(in string pathname, in string[] argv, in string[] envp);
    /// ditto
    int execvp(in string pathname, in string[] argv);
    /// ditto
    int execvpe(in string pathname, in string[] argv, in string[] envp);
}
else version (Posix)
{
    int execv(in string pathname, in string[] argv)
    {
        return execv_(pathname, argv);
    }
    int execve(in string pathname, in string[] argv, in string[] envp)
    {
        return execve_(pathname, argv, envp);
    }
    int execvp(in string pathname, in string[] argv)
    {
        return execvp_(pathname, argv);
    }
    int execvpe(in string pathname, in string[] argv, in string[] envp)
    {
        return execvpe_(pathname, argv, envp);
    }
}

// Move these C declarations to druntime if we decide to keep the D wrappers
extern(C)
{
    int execv(scope const(char) *, scope const(char *)*);
    int execve(scope const(char)*, scope const(char*)*, scope const(char*)*);
    int execvp(scope const(char)*, scope const(char*)*);
    version (Windows) int execvpe(scope const(char)*, scope const(char*)*, scope const(char*)*);
}

private int execv_(in string pathname, in string[] argv)
{
    return execv(pathname.tempCString(), toAStringz(argv));
}

private int execve_(in string pathname, in string[] argv, in string[] envp)
{
    return execve(pathname.tempCString(), toAStringz(argv), toAStringz(envp));
}

private int execvp_(in string pathname, in string[] argv)
{
    return execvp(pathname.tempCString(), toAStringz(argv));
}

private int execvpe_(in string pathname, in string[] argv, in string[] envp)
{
version (Posix)
{
    import std.array : split;
    import std.conv : to;
    // Is pathname rooted?
    if (pathname[0] == '/')
    {
        // Yes, so just call execve()
        return execve(pathname, argv, envp);
    }
    else
    {
        // No, so must traverse PATHs, looking for first match
        string[]    envPaths    =   split(
            to!string(core.stdc.stdlib.getenv("PATH")), ":");
        int         iRet        =   0;

        // Note: if any call to execve() succeeds, this process will cease
        // execution, so there's no need to check the execve() result through
        // the loop.

        foreach (string pathDir; envPaths)
        {
            string  composite   =  cast(string) (pathDir ~ "/" ~ pathname);

            iRet = execve(composite, argv, envp);
        }
        if (0 != iRet)
        {
            iRet = execve(pathname, argv, envp);
        }

        return iRet;
    }
}
else version (Windows)
{
    return execvpe(pathname.tempCString(), toAStringz(argv), toAStringz(envp));
}
else
{
    static assert(0);
} // version
}

version (StdDdoc)
{
    /****************************************
     * Start up the browser and set it to viewing the page at url.
     */
    void browse(scope const(char)[] url);
}
else
version (Windows)
{
    import core.sys.windows.shellapi, core.sys.windows.winuser;

    pragma(lib,"shell32.lib");

    void browse(scope const(char)[] url) nothrow @nogc @trusted
    {
        ShellExecuteW(null, "open", url.tempCStringW(), null, null, SW_SHOWNORMAL);
    }
}
else version (Posix)
{
    import core.stdc.stdio;
    import core.stdc.string;
    import core.sys.posix.unistd;

    void browse(scope const(char)[] url) nothrow @nogc @safe
    {
        const buffer = url.tempCString(); // Retain buffer until end of scope
        const(char)*[3] args;

        // Trusted because it's called with a zero-terminated literal
        const(char)* browser = (() @trusted => core.stdc.stdlib.getenv("BROWSER"))();
        if (browser)
        {
            // String already zero-terminated
            browser = (() @trusted => strdup(browser))();
            args[0] = browser;
        }
        else
        {
            version (OSX)
            {
                args[0] = "open";
            }
            else
            {
                //args[0] = "x-www-browser";  // doesn't work on some systems
                args[0] = "xdg-open";
            }
        }

        args[1] = buffer;
        args[2] = null;

        auto childpid = core.sys.posix.unistd.fork();
        if (childpid == 0)
        {
            // Trusted because args and all entries are always zero-terminated
            (() @trusted {
                core.sys.posix.unistd.execvp(args[0], &args[0]);
                perror(args[0]);
                core.sys.posix.unistd._exit(1);
            })();
            assert(0, "Child failed to exec");
        }
        if (browser)
            // Trusted because it's allocated via strdup above
            (() @trusted => free(cast(void*) browser))();

        version (StdUnittest)
        {
            // Verify that the test script actually suceeds
            int status;
            const check = (() @trusted => waitpid(childpid, &status, 0))();
            assert(check != -1);
            assert(status == 0);
        }
    }
}
else
    static assert(0, "os not supported");

// Verify attributes are consistent between all implementations
@safe @nogc nothrow unittest
{
    if (false)
        browse("");
}

version (Windows) { /* Doesn't use BROWSER */ }
else
@system unittest
{
    import std.conv : text;
    import std.range : repeat;
    immutable string url = text("http://", repeat('x', 249));

    TestScript prog = `if [ "$1" != "` ~ url ~ `" ]; then exit 1; fi`;
    environment["BROWSER"] = prog.path;
    browse(url);
}

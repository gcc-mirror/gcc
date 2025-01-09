// Written in the D programming language.

/**
$(SCRIPT inhibitQuickIndex = 1;)
$(DIVC quickindex,
$(BOOKTABLE,
$(TR $(TH Category) $(TH Symbols))
$(TR $(TD File handles) $(TD
    $(MYREF __popen)
    $(MYREF File)
    $(MYREF isFileHandle)
    $(MYREF openNetwork)
    $(MYREF stderr)
    $(MYREF stdin)
    $(MYREF stdout)
))
$(TR $(TD Reading) $(TD
    $(MYREF chunks)
    $(MYREF lines)
    $(MYREF readf)
    $(MYREF readln)
))
$(TR $(TD Writing) $(TD
    $(MYREF toFile)
    $(MYREF write)
    $(MYREF writef)
    $(MYREF writefln)
    $(MYREF writeln)
))
$(TR $(TD Misc) $(TD
    $(MYREF KeepTerminator)
    $(MYREF LockType)
    $(MYREF StdioException)
))
))

Standard I/O functions that extend $(LINK2 https://dlang.org/phobos/core_stdc_stdio.html, core.stdc.stdio).  $(B core.stdc.stdio)
is $(D_PARAM public)ally imported when importing $(B std.stdio).

There are three layers of I/O:
$(OL
$(LI The lowest layer is the operating system layer. The two main schemes are Windows and Posix.)
$(LI C's $(TT stdio.h) which unifies the two operating system schemes.)
$(LI $(TT std.stdio), this module, unifies the various $(TT stdio.h) implementations into
a high level package for D programs.)
)

Source: $(PHOBOSSRC std/stdio.d)
Copyright: Copyright The D Language Foundation 2007-.
License:   $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   $(HTTP digitalmars.com, Walter Bright),
           $(HTTP erdani.org, Andrei Alexandrescu),
           Alex Rønne Petersen
Macros:
CSTDIO=$(HTTP cplusplus.com/reference/cstdio/$1/, $1)
 */
module std.stdio;

/*
# Glossary

The three layers have many terms for their data structures and types.
Here we try to bring some sanity to them for the intrepid code spelunker.

## Windows

Handle

        A Windows handle is an opaque object of type HANDLE.
        The `HANDLE` for standard devices can be retrieved with
        Windows `GetStdHandle()`.

## Posix

file descriptor, aka fileno, aka fildes

        An int from 0..`FOPEN_MAX`, which is an index into some internal data
        structure.
        0 is for `stdin`, 1 for `stdout`, 2 for `stderr`.
        Negative values usually indicate an error.

## stdio.h

`FILE`

        A struct that encapsulates the C library's view of the operating system
        files. A `FILE` should only be referred to via a pointer.

`fileno`

        A field of `FILE` which is the Posix file descriptor for Posix systems, and
        and an index into an array of file `HANDLE`s for Windows.
        This array is how Posix behavior is emulated on Windows.
        For Digital Mars C, that array is `__osfhnd[]`, and is initialized
        at program start by the C runtime library.
        In this module, they are typed as `fileno_t`.

`stdin`, `stdout`, `stderr`

        Global pointers to `FILE` representing standard input, output, and error streams.
        Being global means there are synchronization issues when multiple threads
        are doing I/O on the same streams.

## std.stdio

*/

import core.stdc.stddef : wchar_t;
public import core.stdc.stdio;
import std.algorithm.mutation : copy;
import std.meta : allSatisfy;
import std.range : ElementEncodingType, empty, front, isBidirectionalRange,
    isInputRange, isSomeFiniteCharInputRange, put;
import std.traits : isSomeChar, isSomeString, Unqual;
import std.typecons : Flag, No, Yes;

/++
If flag `KeepTerminator` is set to `KeepTerminator.yes`, then the delimiter
is included in the strings returned.
+/
alias KeepTerminator = Flag!"keepTerminator";

version (CRuntime_Microsoft)
{
}
else version (CRuntime_Glibc)
{
}
else version (CRuntime_Bionic)
{
    version = GENERIC_IO;
}
else version (CRuntime_Musl)
{
    version = GENERIC_IO;
}
else version (CRuntime_UClibc)
{
    version = GENERIC_IO;
}
else version (OSX)
{
    version = GENERIC_IO;
    version = Darwin;
}
else version (iOS)
{
    version = GENERIC_IO;
    version = Darwin;
}
else version (TVOS)
{
    version = GENERIC_IO;
    version = Darwin;
}
else version (WatchOS)
{
    version = GENERIC_IO;
    version = Darwin;
}
else version (FreeBSD)
{
    version = GENERIC_IO;
}
else version (NetBSD)
{
    version = GENERIC_IO;
}
else version (OpenBSD)
{
    version = GENERIC_IO;
}
else version (DragonFlyBSD)
{
    version = GENERIC_IO;
}
else version (Solaris)
{
    version = GENERIC_IO;
}
else
{
    static assert(0, "unsupported operating system");
}

// Character type used for operating system filesystem APIs
version (Windows)
{
    private alias FSChar = wchar;
}
else
{
    private alias FSChar = char;
}

private alias fileno_t = int;   // file descriptor, fildes, fileno

version (Windows)
{
    // core.stdc.stdio.fopen expects file names to be
    // encoded in CP_ACP on Windows instead of UTF-8.
    /+ Waiting for druntime pull 299
    +/
    extern (C) nothrow @nogc FILE* _wfopen(scope const wchar* filename, scope const wchar* mode);
    extern (C) nothrow @nogc FILE* _wfreopen(scope const wchar* filename, scope const wchar* mode, FILE* fp);

    import core.sys.windows.basetsd : HANDLE;
}

version (Posix)
{
    static import core.sys.posix.stdio; // getdelim, flockfile
}

version (CRuntime_Microsoft)
{
    private alias _FPUTC = _fputc_nolock;
    private alias _FPUTWC = _fputwc_nolock;
    private alias _FGETC = _fgetc_nolock;
    private alias _FGETWC = _fgetwc_nolock;
    private alias _FLOCK = _lock_file;
    private alias _FUNLOCK = _unlock_file;
}
else version (CRuntime_Glibc)
{
    private alias _FPUTC = fputc_unlocked;
    private alias _FPUTWC = fputwc_unlocked;
    private alias _FGETC = fgetc_unlocked;
    private alias _FGETWC = fgetwc_unlocked;
    private alias _FLOCK = core.sys.posix.stdio.flockfile;
    private alias _FUNLOCK = core.sys.posix.stdio.funlockfile;
}
else version (GENERIC_IO)
{
    nothrow:
    @nogc:

    extern (C) private
    {
        static import core.stdc.wchar_;

        pragma(mangle, fputc.mangleof) int _FPUTC(int c, _iobuf* fp);
        pragma(mangle, core.stdc.wchar_.fputwc.mangleof) int _FPUTWC(wchar_t c, _iobuf* fp);
        pragma(mangle, fgetc.mangleof) int _FGETC(_iobuf* fp);
        pragma(mangle, core.stdc.wchar_.fgetwc.mangleof) int _FGETWC(_iobuf* fp);
    }

    version (Posix)
    {
        private alias _FLOCK = core.sys.posix.stdio.flockfile;
        private alias _FUNLOCK = core.sys.posix.stdio.funlockfile;
    }
    else
    {
        static assert(0, "don't know how to lock files on GENERIC_IO");
    }
}
else
{
    static assert(0, "unsupported C I/O system");
}

private extern (C) @nogc nothrow
{
    pragma(mangle, _FPUTC.mangleof) int trustedFPUTC(int ch, _iobuf* h) @trusted;
    pragma(mangle, _FPUTWC.mangleof) int trustedFPUTWC(wchar_t ch, _iobuf* h) @trusted;
}

//------------------------------------------------------------------------------
private struct ByRecordImpl(Fields...)
{
private:
    import std.typecons : Tuple;

    File file;
    char[] line;
    Tuple!(Fields) current;
    string format;

public:
    this(File f, string format)
    {
        assert(f.isOpen);
        file = f;
        this.format = format;
        popFront(); // prime the range
    }

    /// Range primitive implementations.
    @property bool empty()
    {
        return !file.isOpen;
    }

    /// Ditto
    @property ref Tuple!(Fields) front()
    {
        return current;
    }

    /// Ditto
    void popFront()
    {
        import std.conv : text;
        import std.exception : enforce;
        import std.format.read : formattedRead;
        import std.string : chomp;

        enforce(file.isOpen, "ByRecord: File must be open");
        file.readln(line);
        if (!line.length)
        {
            file.detach();
        }
        else
        {
            line = chomp(line);
            formattedRead(line, format, &current);
            enforce(line.empty, text("Leftover characters in record: `",
                            line, "'"));
        }
    }
}

template byRecord(Fields...)
{
    auto byRecord(File f, string format)
    {
        return typeof(return)(f, format);
    }
}

/**
Encapsulates a `FILE*`. Generally D does not attempt to provide
thin wrappers over equivalent functions in the C standard library, but
manipulating `FILE*` values directly is unsafe and error-prone in
many ways. The `File` type ensures safe manipulation, automatic
file closing, and a lot of convenience.

The underlying `FILE*` handle is maintained in a reference-counted
manner, such that as soon as the last `File` variable bound to a
given `FILE*` goes out of scope, the underlying `FILE*` is
automatically closed.

Example:
----
// test.d
import std.stdio;

void main(string[] args)
{
    auto f = File("test.txt", "w"); // open for writing
    f.write("Hello");
    if (args.length > 1)
    {
        auto g = f; // now g and f write to the same file
                    // internal reference count is 2
        g.write(", ", args[1]);
        // g exits scope, reference count decreases to 1
    }
    f.writeln("!");
    // f exits scope, reference count falls to zero,
    // underlying `FILE*` is closed.
}
----
$(CONSOLE
% rdmd test.d Jimmy
% cat test.txt
Hello, Jimmy!
% __
)
 */
struct File
{
    import core.atomic : atomicOp, atomicStore, atomicLoad;
    import std.range.primitives : ElementEncodingType;
    import std.traits : isScalarType, isArray;
    enum Orientation { unknown, narrow, wide }

    private struct Impl
    {
        FILE * handle = null; // Is null iff this Impl is closed by another File
        shared uint refs = uint.max / 2;
        bool isPopened; // true iff the stream has been created by popen()
        Orientation orientation;
    }
    private Impl* _p;
    private string _name;

    package this(FILE* handle, string name, uint refs = 1, bool isPopened = false) @trusted @nogc nothrow
    {
        import core.stdc.stdlib : malloc;

        assert(!_p);
        _p = cast(Impl*) malloc(Impl.sizeof);
        if (!_p)
        {
            import core.exception : onOutOfMemoryError;
            onOutOfMemoryError();
        }
        initImpl(handle, name, refs, isPopened);
    }

    private void initImpl(FILE* handle, string name, uint refs = 1, bool isPopened = false) @nogc nothrow pure @safe
    {
        assert(_p);
        _p.handle = handle;
        atomicStore(_p.refs, refs);
        _p.isPopened = isPopened;
        _p.orientation = Orientation.unknown;
        _name = name;
    }

/**
Constructor taking the name of the file to open and the open mode.

Copying one `File` object to another results in the two `File`
objects referring to the same underlying file.

The destructor automatically closes the file as soon as no `File`
object refers to it anymore.

Params:
    name = range or string representing the file _name
    stdioOpenmode = range or string represting the open mode
        (with the same semantics as in the C standard library
        $(CSTDIO fopen) function)

Throws: `ErrnoException` if the file could not be opened.
 */
    this(string name, scope const(char)[] stdioOpenmode = "rb") @safe
    {
        import std.conv : text;
        import std.exception : errnoEnforce;

        this(errnoEnforce(_fopen(name, stdioOpenmode),
                        text("Cannot open file `", name, "' in mode `",
                                stdioOpenmode, "'")),
                name);

        // MSVCRT workaround (https://issues.dlang.org/show_bug.cgi?id=14422)
        version (CRuntime_Microsoft)
        {
            setAppendWin(stdioOpenmode);
        }
    }

    /// ditto
    this(R1, R2)(R1 name)
    if (isSomeFiniteCharInputRange!R1)
    {
        import std.conv : to;
        this(name.to!string, "rb");
    }

    /// ditto
    this(R1, R2)(R1 name, R2 mode)
    if (isSomeFiniteCharInputRange!R1 &&
        isSomeFiniteCharInputRange!R2)
    {
        import std.conv : to;
        this(name.to!string, mode.to!string);
    }

    @safe unittest
    {
        static import std.file;
        import std.utf : byChar;
        auto deleteme = testFilename();
        auto f = File(deleteme.byChar, "w".byChar);
        f.close();
        std.file.remove(deleteme);
    }

    ~this() @safe
    {
        detach();
    }

    this(this) @safe pure nothrow @nogc
    {
        if (!_p) return;
        assert(atomicLoad(_p.refs));
        atomicOp!"+="(_p.refs, 1);
    }

/**
Assigns a file to another. The target of the assignment gets detached
from whatever file it was attached to, and attaches itself to the new
file.
 */
    ref File opAssign(File rhs) @safe return
    {
        import std.algorithm.mutation : swap;

        swap(this, rhs);
        return this;
    }

     // https://issues.dlang.org/show_bug.cgi?id=20129
    @safe unittest
    {
        File[int] aa;
        aa.require(0, File.init);
    }

/**
Detaches from the current file (throwing on failure), and then attempts to
_open file `name` with mode `stdioOpenmode`. The mode has the
same semantics as in the C standard library $(CSTDIO fopen) function.

Throws: `ErrnoException` in case of error.
 */
    void open(string name, scope const(char)[] stdioOpenmode = "rb") @trusted
    {
        resetFile(name, stdioOpenmode, false);
    }

    // https://issues.dlang.org/show_bug.cgi?id=20585
    @system unittest
    {
        File f;
        try
            f.open("doesn't exist");
        catch (Exception _e)
        {
        }

        assert(!f.isOpen);

        f.close();  // to check not crash here
    }

    private void resetFile(string name, scope const(char)[] stdioOpenmode, bool isPopened) @trusted
    {
        import core.stdc.stdlib : malloc;
        import std.exception : enforce;
        import std.conv : text;
        import std.exception : errnoEnforce;

        if (_p !is null)
        {
            detach();
        }

        FILE* handle;
        version (Posix)
        {
            if (isPopened)
            {
                errnoEnforce(handle = _popen(name, stdioOpenmode),
                             "Cannot run command `"~name~"'");
            }
            else
            {
                errnoEnforce(handle = _fopen(name, stdioOpenmode),
                             text("Cannot open file `", name, "' in mode `",
                                  stdioOpenmode, "'"));
            }
        }
        else
        {
            assert(isPopened == false);
            errnoEnforce(handle = _fopen(name, stdioOpenmode),
                         text("Cannot open file `", name, "' in mode `",
                              stdioOpenmode, "'"));
        }
        _p = cast(Impl*) enforce(malloc(Impl.sizeof), "Out of memory");
        initImpl(handle, name, 1, isPopened);
        version (CRuntime_Microsoft)
        {
            setAppendWin(stdioOpenmode);
        }
    }

    private void closeHandles() @trusted
    {
        assert(_p);
        import std.exception : errnoEnforce;

        version (Posix)
        {
            import core.sys.posix.stdio : pclose;
            import std.format : format;

            if (_p.isPopened)
            {
                auto res = pclose(_p.handle);
                errnoEnforce(res != -1,
                        "Could not close pipe `"~_name~"'");
                _p.handle = null;
                return;
            }
        }
        if (_p.handle)
        {
            auto handle = _p.handle;
            _p.handle = null;
            // fclose disassociates the FILE* even in case of error (https://issues.dlang.org/show_bug.cgi?id=19751)
            errnoEnforce(.fclose(handle) == 0,
                    "Could not close file `"~_name~"'");
        }
    }

    version (CRuntime_Microsoft)
    {
        private void setAppendWin(scope const(char)[] stdioOpenmode) @safe
        {
            bool append, update;
            foreach (c; stdioOpenmode)
                if (c == 'a')
                    append = true;
                else
                if (c == '+')
                    update = true;
            if (append && !update)
                seek(size);
        }
    }

/**
Reuses the `File` object to either open a different file, or change
the file mode. If `name` is `null`, the mode of the currently open
file is changed; otherwise, a new file is opened, reusing the C
`FILE*`. The function has the same semantics as in the C standard
library $(CSTDIO freopen) function.

Note: Calling `reopen` with a `null` `name` is not implemented
in all C runtimes.

Throws: `ErrnoException` in case of error.
 */
    void reopen(string name, scope const(char)[] stdioOpenmode = "rb") @trusted
    {
        import std.conv : text;
        import std.exception : enforce, errnoEnforce;
        import std.internal.cstring : tempCString;

        enforce(isOpen, "Attempting to reopen() an unopened file");

        auto namez = (name == null ? _name : name).tempCString!FSChar();
        auto modez = stdioOpenmode.tempCString!FSChar();

        FILE* fd = _p.handle;
        version (Windows)
            fd =  _wfreopen(namez, modez, fd);
        else
            fd = freopen(namez, modez, fd);

        errnoEnforce(fd, name
            ? text("Cannot reopen file `", name, "' in mode `", stdioOpenmode, "'")
            : text("Cannot reopen file in mode `", stdioOpenmode, "'"));

        if (name !is null)
            _name = name;
    }

    @safe unittest // Test changing filename
    {
        import std.exception : assertThrown, assertNotThrown;
        static import std.file;

        auto deleteme = testFilename();
        std.file.write(deleteme, "foo");
        scope(exit) std.file.remove(deleteme);
        auto f = File(deleteme);
        assert(f.readln() == "foo");

        auto deleteme2 = testFilename();
        std.file.write(deleteme2, "bar");
        scope(exit) std.file.remove(deleteme2);
        f.reopen(deleteme2);
        assert(f.name == deleteme2);
        assert(f.readln() == "bar");
        f.close();
    }

    version (CRuntime_Microsoft) {} else // Not implemented
    @safe unittest // Test changing mode
    {
        import std.exception : assertThrown, assertNotThrown;
        static import std.file;

        auto deleteme = testFilename();
        std.file.write(deleteme, "foo");
        scope(exit) std.file.remove(deleteme);
        auto f = File(deleteme, "r+");
        assert(f.readln() == "foo");
        f.reopen(null, "w");
        f.write("bar");
        f.seek(0);
        f.reopen(null, "a");
        f.write("baz");
        assert(f.name == deleteme);
        f.close();
        assert(std.file.readText(deleteme) == "barbaz");
    }

/**
Detaches from the current file (throwing on failure), and then runs a command
by calling the C standard library function $(HTTP
pubs.opengroup.org/onlinepubs/7908799/xsh/popen.html, popen).

Throws: `ErrnoException` in case of error.
 */
    version (Posix) void popen(string command, scope const(char)[] stdioOpenmode = "r") @safe
    {
        resetFile(command, stdioOpenmode ,true);
    }

/**
First calls `detach` (throwing on failure), then attempts to
associate the given file descriptor with the `File`, and sets the file's name to `null`.

The mode must be compatible with the mode of the file descriptor.

Throws: `ErrnoException` in case of error.
Params:
    fd = File descriptor to associate with this `File`.
    stdioOpenmode = Mode to associate with this File. The mode has the same
        semantics as in the POSIX library function $(HTTP
        pubs.opengroup.org/onlinepubs/7908799/xsh/fdopen.html, fdopen)
        and must be compatible with `fd`.
 */
    void fdopen(int fd, scope const(char)[] stdioOpenmode = "rb") @safe
    {
        fdopen(fd, stdioOpenmode, null);
    }

    package void fdopen(int fd, scope const(char)[] stdioOpenmode, string name) @trusted
    {
        import std.exception : errnoEnforce;
        import std.internal.cstring : tempCString;

        auto modez = stdioOpenmode.tempCString();
        detach();

        version (CRuntime_Microsoft)
        {
            auto fp = _fdopen(fd, modez);
            errnoEnforce(fp);
        }
        else version (Posix)
        {
            import core.sys.posix.stdio : fdopen;
            auto fp = fdopen(fd, modez);
            errnoEnforce(fp);
        }
        else
            static assert(0, "no fdopen() available");

        this = File(fp, name);
    }

    // Declare a dummy HANDLE to allow generating documentation
    // for Windows-only methods.
    version (StdDdoc) { version (Windows) {} else alias HANDLE = int; }

/**
First calls `detach` (throwing on failure), and then attempts to
associate the given Windows `HANDLE` with the `File`. The mode must
be compatible with the access attributes of the handle. Windows only.

Throws: `ErrnoException` in case of error.
*/
    version (StdDdoc)
    void windowsHandleOpen(HANDLE handle, scope const(char)[] stdioOpenmode);

    version (Windows)
    void windowsHandleOpen(HANDLE handle, scope const(char)[] stdioOpenmode)
    {
        import core.stdc.stdint : intptr_t;
        import std.exception : errnoEnforce;
        import std.format : format;

        // Create file descriptors from the handles
        int mode;
        modeLoop:
        foreach (c; stdioOpenmode)
            switch (c)
            {
                case 'r': mode |= _O_RDONLY; break;
                case '+': mode &=~_O_RDONLY; break;
                case 'a': mode |= _O_APPEND; break;
                case 'b': mode |= _O_BINARY; break;
                case 't': mode |= _O_TEXT;   break;
                case ',': break modeLoop;
                default: break;
            }

        auto fd = _open_osfhandle(cast(intptr_t) handle, mode);

        errnoEnforce(fd >= 0, "Cannot open Windows HANDLE");
        fdopen(fd, stdioOpenmode, "HANDLE(%s)".format(handle));
    }


/** Returns `true` if the file is opened. */
    @property bool isOpen() const @safe pure nothrow
    {
        return _p !is null && _p.handle;
    }

/**
Returns `true` if the file is at end (see $(CSTDIO feof)).

Throws: `Exception` if the file is not opened.
 */
    @property bool eof() const @trusted pure
    {
        import std.exception : enforce;

        enforce(_p && _p.handle, "Calling eof() against an unopened file.");
        return .feof(cast(FILE*) _p.handle) != 0;
    }

/**
 Returns the name last used to initialize this `File`, if any.

 Some functions that create or initialize the `File` set the name field to `null`.
 Examples include $(LREF tmpfile), $(LREF wrapFile), and $(LREF fdopen). See the
 documentation of those functions for details.

 Returns: The name last used to initialize this this file, or `null` otherwise.
 */
    @property string name() const @safe pure nothrow return
    {
        return _name;
    }

/**
If the file is closed or not yet opened, returns `true`. Otherwise, returns
$(CSTDIO ferror) for the file handle.
 */
    @property bool error() const @trusted pure nothrow
    {
        return !isOpen || .ferror(cast(FILE*) _p.handle);
    }

    @safe unittest
    {
        // https://issues.dlang.org/show_bug.cgi?id=12349
        static import std.file;
        auto deleteme = testFilename();
        auto f = File(deleteme, "w");
        scope(exit) std.file.remove(deleteme);

        f.close();
        assert(f.error);
    }

/**
Detaches from the underlying file. If the sole owner, calls `close`.

Throws: `ErrnoException` on failure if closing the file.
  */
    void detach() @trusted
    {
        import core.stdc.stdlib : free;

        if (!_p) return;
        scope(exit) _p = null;

        if (atomicOp!"-="(_p.refs, 1) == 0)
        {
            scope(exit) free(_p);
            closeHandles();
        }
    }

    @safe unittest
    {
        static import std.file;

        auto deleteme = testFilename();
        scope(exit) std.file.remove(deleteme);
        auto f = File(deleteme, "w");
        {
            auto f2 = f;
            f2.detach();
        }
        assert(f._p.refs == 1);
        f.close();
    }

/**
If the file was closed or not yet opened, succeeds vacuously. Otherwise
closes the file (by calling $(CSTDIO fclose)),
throwing on error. Even if an exception is thrown, afterwards the $(D
File) object is empty. This is different from `detach` in that it
always closes the file; consequently, all other `File` objects
referring to the same handle will see a closed file henceforth.

Throws: `ErrnoException` on error.
 */
    void close() @trusted
    {
        import core.stdc.stdlib : free;
        import std.exception : errnoEnforce;

        if (!_p) return; // succeed vacuously
        scope(exit)
        {
            if (atomicOp!"-="(_p.refs, 1) == 0)
                free(_p);
            _p = null; // start a new life
        }
        if (!_p.handle) return; // Impl is closed by another File

        scope(exit) _p.handle = null; // nullify the handle anyway
        closeHandles();
    }

/**
If the file is closed or not yet opened, succeeds vacuously. Otherwise, returns
$(CSTDIO clearerr) for the file handle.
 */
    void clearerr() @safe pure nothrow
    {
        _p is null || _p.handle is null ||
        .clearerr(_p.handle);
    }

/**
Flushes the C `FILE` buffers.

Calls $(CSTDIO fflush) for the file handle.

Throws: `Exception` if the file is not opened or if the call to `fflush` fails.
 */
    void flush() @trusted
    {
        import std.exception : enforce, errnoEnforce;

        enforce(isOpen, "Attempting to flush() in an unopened file");
        errnoEnforce(.fflush(_p.handle) == 0);
    }

    @safe unittest
    {
        // https://issues.dlang.org/show_bug.cgi?id=12349
        import std.exception : assertThrown;
        static import std.file;

        auto deleteme = testFilename();
        auto f = File(deleteme, "w");
        scope(exit) std.file.remove(deleteme);

        f.close();
        assertThrown(f.flush());
    }

/**
Forces any data buffered by the OS to be written to disk.
Call $(LREF flush) before calling this function to flush the C `FILE` buffers first.

This function calls
$(HTTP msdn.microsoft.com/en-us/library/windows/desktop/aa364439%28v=vs.85%29.aspx,
`FlushFileBuffers`) on Windows,
$(HTTP developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man2/fcntl.2.html,
`F_FULLFSYNC fcntl`) on Darwin and
$(HTTP pubs.opengroup.org/onlinepubs/7908799/xsh/fsync.html,
`fsync`) on POSIX for the file handle.

Throws: `Exception` if the file is not opened or if the OS call fails.
 */
    void sync() @trusted
    {
        import std.exception : enforce;

        enforce(isOpen, "Attempting to sync() an unopened file");

        version (Windows)
        {
            import core.sys.windows.winbase : FlushFileBuffers;
            wenforce(FlushFileBuffers(windowsHandle), "FlushFileBuffers failed");
        }
        else version (Darwin)
        {
            import core.sys.darwin.fcntl : fcntl, F_FULLFSYNC;
            import std.exception : errnoEnforce;
            errnoEnforce(fcntl(fileno, F_FULLFSYNC, 0) != -1, "fcntl failed");
        }
        else
        {
            import core.sys.posix.unistd : fsync;
            import std.exception : errnoEnforce;
            errnoEnforce(fsync(fileno) == 0, "fsync failed");
        }
    }

/**
Calls $(CSTDIO fread) for the
file handle. The number of items to read and the size of
each item is inferred from the size and type of the input array, respectively.

Returns: The slice of `buffer` containing the data that was actually read.
This will be shorter than `buffer` if EOF was reached before the buffer
could be filled. If the buffer is empty, it will be returned.

Throws: `ErrnoException` if the file is not opened or the call to `fread` fails.

`rawRead` always reads in binary mode on Windows.
 */
    T[] rawRead(T)(T[] buffer)
    {
        import std.exception : enforce, errnoEnforce;

        if (!buffer.length)
            return buffer;
        enforce(isOpen, "Attempting to read from an unopened file");
        version (Windows)
        {
            immutable fileno_t fd = .fileno(_p.handle);
            immutable mode = ._setmode(fd, _O_BINARY);
            scope(exit) ._setmode(fd, mode);
        }
        immutable freadResult = trustedFread(_p.handle, buffer);
        assert(freadResult <= buffer.length); // fread return guarantee
        if (freadResult != buffer.length) // error or eof
        {
            errnoEnforce(!error);
            return buffer[0 .. freadResult];
        }
        return buffer;
    }

    ///
    @system unittest
    {
        static import std.file;

        auto testFile = std.file.deleteme();
        std.file.write(testFile, "\r\n\n\r\n");
        scope(exit) std.file.remove(testFile);

        auto f = File(testFile, "r");
        auto buf = f.rawRead(new char[5]);
        f.close();
        assert(buf == "\r\n\n\r\n");
    }

    // https://issues.dlang.org/show_bug.cgi?id=24685
    static assert(!__traits(compiles, (File f) @safe { int*[1] bar; f.rawRead(bar[]); }));

    // https://issues.dlang.org/show_bug.cgi?id=21729
    @system unittest
    {
        import std.exception : assertThrown;

        File f;
        ubyte[1] u;
        assertThrown(f.rawRead(u));
    }

    // https://issues.dlang.org/show_bug.cgi?id=21728
    @system unittest
    {
        static if (__traits(compiles, { import std.process : pipe; })) // not available for iOS
        {
            import std.process : pipe;
            import std.exception : assertThrown;

            auto p = pipe();
            p.readEnd.close;
            ubyte[1] u;
            assertThrown(p.readEnd.rawRead(u));
        }
    }

    // https://issues.dlang.org/show_bug.cgi?id=13893
    @system unittest
    {
        import std.exception : assertNotThrown;

        File f;
        ubyte[0] u;
        assertNotThrown(f.rawRead(u));
    }

/**
Calls $(CSTDIO fwrite) for the file
handle. The number of items to write and the size of each
item is inferred from the size and type of the input array, respectively. An
error is thrown if the buffer could not be written in its entirety.

`rawWrite` always writes in binary mode on Windows.

Throws: `ErrnoException` if the file is not opened or if the call to `fwrite` fails.
 */
    void rawWrite(T)(in T[] buffer)
    {
        import std.conv : text;
        import std.exception : errnoEnforce;

        version (Windows)
        {
            immutable fileno_t fd = .fileno(_p.handle);
            immutable oldMode = ._setmode(fd, _O_BINARY);

            if (oldMode != _O_BINARY)
            {
                // need to flush the data that was written with the original mode
                ._setmode(fd, oldMode);
                flush(); // before changing translation mode ._setmode(fd, _O_BINARY);
                ._setmode(fd, _O_BINARY);
            }

            scope (exit)
            {
                if (oldMode != _O_BINARY)
                {
                    flush();
                    ._setmode(fd, oldMode);
                }
            }
        }

        auto result = trustedFwrite(_p.handle, buffer);
        if (result == result.max) result = 0;
        errnoEnforce(result == buffer.length,
                text("Wrote ", result, " instead of ", buffer.length,
                        " objects of type ", T.stringof, " to file `",
                        _name, "'"));
    }

    ///
    @system unittest
    {
        static import std.file;

        auto testFile = std.file.deleteme();
        auto f = File(testFile, "w");
        scope(exit) std.file.remove(testFile);

        f.rawWrite("\r\n\n\r\n");
        f.close();
        assert(std.file.read(testFile) == "\r\n\n\r\n");
    }

/**
Calls $(CSTDIO fseek)
for the file handle to move its position indicator.

Params:
    offset = Binary files: Number of bytes to offset from origin.$(BR)
             Text files: Either zero, or a value returned by $(LREF tell).
    origin = Binary files: Position used as reference for the offset, must be
             one of $(REF_ALTTEXT SEEK_SET, SEEK_SET, core,stdc,stdio),
             $(REF_ALTTEXT SEEK_CUR, SEEK_CUR, core,stdc,stdio) or
             $(REF_ALTTEXT SEEK_END, SEEK_END, core,stdc,stdio).$(BR)
             Text files: Shall necessarily be
             $(REF_ALTTEXT SEEK_SET, SEEK_SET, core,stdc,stdio).

Throws: `Exception` if the file is not opened.
        `ErrnoException` if the call to `fseek` fails.
 */
    void seek(long offset, int origin = SEEK_SET) @trusted
    {
        import std.conv : to, text;
        import std.exception : enforce, errnoEnforce;

        // Some libc sanitize the whence input (e.g. glibc), but some don't,
        // e.g. Microsoft runtime crashes on an invalid origin,
        // and Musl additionally accept SEEK_DATA & SEEK_HOLE (Linux extension).
        // To provide a consistent behavior cross platform, we use the glibc check
        // See also https://issues.dlang.org/show_bug.cgi?id=19797
        enforce(origin == SEEK_SET || origin == SEEK_CUR ||  origin == SEEK_END,
                "Invalid `origin` argument passed to `seek`, must be one of: SEEK_SET, SEEK_CUR, SEEK_END");

        enforce(isOpen, "Attempting to seek() in an unopened file");
        version (Windows)
        {
            version (CRuntime_Microsoft)
            {
                alias fseekFun = _fseeki64;
                alias off_t = long;
            }
            else
            {
                alias fseekFun = fseek;
                alias off_t = int;
            }
        }
        else version (Posix)
        {
            import core.sys.posix.stdio : fseeko, off_t;
            alias fseekFun = fseeko;
        }
        errnoEnforce(fseekFun(_p.handle, to!off_t(offset), origin) == 0,
                "Could not seek in file `"~_name~"'");
    }

    @system unittest
    {
        import std.conv : text;
        static import std.file;
        import std.exception;

        auto deleteme = testFilename();
        auto f = File(deleteme, "w+");
        scope(exit) { f.close(); std.file.remove(deleteme); }
        f.rawWrite("abcdefghijklmnopqrstuvwxyz");
        f.seek(7);
        assert(f.readln() == "hijklmnopqrstuvwxyz");

        version (CRuntime_Bionic)
            auto bigOffset = int.max - 100;
        else
            auto bigOffset = cast(ulong) int.max + 100;
        f.seek(bigOffset);
        assert(f.tell == bigOffset, text(f.tell));
        // Uncomment the tests below only if you want to wait for
        // a long time
        // f.rawWrite("abcdefghijklmnopqrstuvwxyz");
        // f.seek(-3, SEEK_END);
        // assert(f.readln() == "xyz");

        assertThrown(f.seek(0, ushort.max));
    }

/**
Calls $(CSTDIO ftell)
for the managed file handle, which returns the current value of
the position indicator of the file handle.

Throws: `Exception` if the file is not opened.
        `ErrnoException` if the call to `ftell` fails.
 */
    @property ulong tell() const @trusted
    {
        import std.exception : enforce, errnoEnforce;

        enforce(isOpen, "Attempting to tell() in an unopened file");
        version (Windows)
        {
            version (CRuntime_Microsoft)
                immutable result = _ftelli64(cast(FILE*) _p.handle);
            else
                immutable result = ftell(cast(FILE*) _p.handle);
        }
        else version (Posix)
        {
            import core.sys.posix.stdio : ftello;
            immutable result = ftello(cast(FILE*) _p.handle);
        }
        errnoEnforce(result != -1,
                "Query ftell() failed for file `"~_name~"'");
        return result;
    }

    ///
    @system unittest
    {
        import std.conv : text;
        static import std.file;

        auto testFile = std.file.deleteme();
        std.file.write(testFile, "abcdefghijklmnopqrstuvwqxyz");
        scope(exit) { std.file.remove(testFile); }

        auto f = File(testFile);
        auto a = new ubyte[4];
        f.rawRead(a);
        assert(f.tell == 4, text(f.tell));
    }

/**
Calls $(CSTDIO rewind) for the file handle.

Throws: `Exception` if the file is not opened.
 */
    void rewind() @safe
    {
        import std.exception : enforce;

        enforce(isOpen, "Attempting to rewind() an unopened file");
        .rewind(_p.handle);
    }

/**
Calls $(CSTDIO setvbuf) for the file handle.

Throws: `Exception` if the file is not opened.
        `ErrnoException` if the call to `setvbuf` fails.
 */
    void setvbuf(size_t size, int mode = _IOFBF) @trusted
    {
        import std.exception : enforce, errnoEnforce;

        enforce(isOpen, "Attempting to call setvbuf() on an unopened file");
        errnoEnforce(.setvbuf(_p.handle, null, mode, size) == 0,
                "Could not set buffering for file `"~_name~"'");
    }

/**
Calls $(CSTDIO setvbuf) for the file handle.

Throws: `Exception` if the file is not opened.
        `ErrnoException` if the call to `setvbuf` fails.
*/
    void setvbuf(void[] buf, int mode = _IOFBF) @trusted
    {
        import std.exception : enforce, errnoEnforce;

        enforce(isOpen, "Attempting to call setvbuf() on an unopened file");
        errnoEnforce(.setvbuf(_p.handle,
                        cast(char*) buf.ptr, mode, buf.length) == 0,
                "Could not set buffering for file `"~_name~"'");
    }


    version (Windows)
    {
        import core.sys.windows.winbase : OVERLAPPED;
        import core.sys.windows.winnt : BOOL, ULARGE_INTEGER;
        import std.windows.syserror : wenforce;

        private BOOL lockImpl(alias F, Flags...)(ulong start, ulong length,
            Flags flags)
        {
            if (!start && !length)
                length = ulong.max;
            ULARGE_INTEGER liStart = void, liLength = void;
            liStart.QuadPart = start;
            liLength.QuadPart = length;
            OVERLAPPED overlapped;
            overlapped.Offset = liStart.LowPart;
            overlapped.OffsetHigh = liStart.HighPart;
            overlapped.hEvent = null;
            return F(windowsHandle, flags, 0, liLength.LowPart,
                liLength.HighPart, &overlapped);
        }
    }
    version (Posix)
    {
        private int lockImpl(int operation, short l_type,
            ulong start, ulong length)
        {
            import core.sys.posix.fcntl : fcntl, flock, off_t;
            import core.sys.posix.unistd : getpid;
            import std.conv : to;

            flock fl = void;
            fl.l_type   = l_type;
            fl.l_whence = SEEK_SET;
            fl.l_start  = to!off_t(start);
            fl.l_len    = to!off_t(length);
            fl.l_pid    = getpid();
            return fcntl(fileno, operation, &fl);
        }
    }

/**
Locks the specified file segment. If the file segment is already locked
by another process, waits until the existing lock is released.
If both `start` and `length` are zero, the entire file is locked.

Locks created using `lock` and `tryLock` have the following properties:
$(UL
 $(LI All locks are automatically released when the process terminates.)
 $(LI Locks are not inherited by child processes.)
 $(LI Closing a file will release all locks associated with the file. On POSIX,
      even locks acquired via a different `File` will be released as well.)
 $(LI Not all NFS implementations correctly implement file locking.)
)
 */
    void lock(LockType lockType = LockType.readWrite,
        ulong start = 0, ulong length = 0)
    {
        import std.exception : enforce;

        enforce(isOpen, "Attempting to call lock() on an unopened file");
        version (Posix)
        {
            import core.sys.posix.fcntl : F_RDLCK, F_SETLKW, F_WRLCK;
            import std.exception : errnoEnforce;
            immutable short type = lockType == LockType.readWrite
                ? F_WRLCK : F_RDLCK;
            errnoEnforce(lockImpl(F_SETLKW, type, start, length) != -1,
                    "Could not set lock for file `"~_name~"'");
        }
        else
        version (Windows)
        {
            import core.sys.windows.winbase : LockFileEx, LOCKFILE_EXCLUSIVE_LOCK;
            immutable type = lockType == LockType.readWrite ?
                LOCKFILE_EXCLUSIVE_LOCK : 0;
            wenforce(lockImpl!LockFileEx(start, length, type),
                    "Could not set lock for file `"~_name~"'");
        }
        else
            static assert(false);
    }

/**
Attempts to lock the specified file segment.
If both `start` and `length` are zero, the entire file is locked.
Returns: `true` if the lock was successful, and `false` if the
specified file segment was already locked.
 */
    bool tryLock(LockType lockType = LockType.readWrite,
        ulong start = 0, ulong length = 0)
    {
        import std.exception : enforce;

        enforce(isOpen, "Attempting to call tryLock() on an unopened file");
        version (Posix)
        {
            import core.stdc.errno : EACCES, EAGAIN, errno;
            import core.sys.posix.fcntl : F_RDLCK, F_SETLK, F_WRLCK;
            import std.exception : errnoEnforce;
            immutable short type = lockType == LockType.readWrite
                ? F_WRLCK : F_RDLCK;
            immutable res = lockImpl(F_SETLK, type, start, length);
            if (res == -1 && (errno == EACCES || errno == EAGAIN))
                return false;
            errnoEnforce(res != -1, "Could not set lock for file `"~_name~"'");
            return true;
        }
        else
        version (Windows)
        {
            import core.sys.windows.winbase : GetLastError, LockFileEx, LOCKFILE_EXCLUSIVE_LOCK,
                LOCKFILE_FAIL_IMMEDIATELY;
            import core.sys.windows.winerror : ERROR_IO_PENDING, ERROR_LOCK_VIOLATION;
            immutable type = lockType == LockType.readWrite
                ? LOCKFILE_EXCLUSIVE_LOCK : 0;
            immutable res = lockImpl!LockFileEx(start, length,
                type | LOCKFILE_FAIL_IMMEDIATELY);
            if (!res && (GetLastError() == ERROR_IO_PENDING
                || GetLastError() == ERROR_LOCK_VIOLATION))
                return false;
            wenforce(res, "Could not set lock for file `"~_name~"'");
            return true;
        }
        else
            static assert(false);
    }

/**
Removes the lock over the specified file segment.
 */
    void unlock(ulong start = 0, ulong length = 0)
    {
        import std.exception : enforce;

        enforce(isOpen, "Attempting to call unlock() on an unopened file");
        version (Posix)
        {
            import core.sys.posix.fcntl : F_SETLK, F_UNLCK;
            import std.exception : errnoEnforce;
            errnoEnforce(lockImpl(F_SETLK, F_UNLCK, start, length) != -1,
                    "Could not remove lock for file `"~_name~"'");
        }
        else
        version (Windows)
        {
            import core.sys.windows.winbase : UnlockFileEx;
            wenforce(lockImpl!UnlockFileEx(start, length),
                "Could not remove lock for file `"~_name~"'");
        }
        else
            static assert(false);
    }

    version (Windows)
    @system unittest
    {
        static import std.file;
        auto deleteme = testFilename();
        scope(exit) std.file.remove(deleteme);
        auto f = File(deleteme, "wb");
        assert(f.tryLock());
        auto g = File(deleteme, "wb");
        assert(!g.tryLock());
        assert(!g.tryLock(LockType.read));
        f.unlock();
        f.lock(LockType.read);
        assert(!g.tryLock());
        assert(g.tryLock(LockType.read));
        f.unlock();
        g.unlock();
    }

    version (Posix)
    @system unittest
    {
    static if (__traits(compiles, { import std.process : spawnProcess; }))
    {
        static import std.file;
        auto deleteme = testFilename();
        scope(exit) std.file.remove(deleteme);

        // Since locks are per-process, we cannot test lock failures within
        // the same process. fork() is used to create a second process.
        static void runForked(void delegate() code)
        {
            import core.sys.posix.sys.wait : waitpid;
            import core.sys.posix.unistd : fork, _exit;
            int child, status;
            if ((child = fork()) == 0)
            {
                code();
                _exit(0);
            }
            else
            {
                assert(waitpid(child, &status, 0) != -1);
                assert(status == 0, "Fork crashed");
            }
        }

        auto f = File(deleteme, "w+b");

        runForked
        ({
            auto g = File(deleteme, "a+b");
            assert(g.tryLock());
            g.unlock();
            assert(g.tryLock(LockType.read));
        });

        assert(f.tryLock());
        runForked
        ({
            auto g = File(deleteme, "a+b");
            assert(!g.tryLock());
            assert(!g.tryLock(LockType.read));
        });
        f.unlock();

        f.lock(LockType.read);
        runForked
        ({
            auto g = File(deleteme, "a+b");
            assert(!g.tryLock());
            assert(g.tryLock(LockType.read));
            g.unlock();
        });
        f.unlock();
    } // static if
    } // unittest


/**
Writes its arguments in text format to the file.

Throws: `Exception` if the file is not opened.
        `ErrnoException` on an error writing to the file.
*/
    void write(S...)(S args)
    {
        import std.traits : isBoolean, isIntegral, isAggregateType;
        import std.utf : UTFException;
        auto w = lockingTextWriter();
        foreach (arg; args)
        {
            try
            {
                alias A = typeof(arg);
                static if (isAggregateType!A || is(A == enum))
                {
                    import std.format.write : formattedWrite;

                    formattedWrite(w, "%s", arg);
                }
                else static if (isSomeString!A)
                {
                    put(w, arg);
                }
                else static if (isIntegral!A)
                {
                    import std.conv : toTextRange;

                    toTextRange(arg, w);
                }
                else static if (isBoolean!A)
                {
                    put(w, arg ? "true" : "false");
                }
                else static if (isSomeChar!A)
                {
                    put(w, arg);
                }
                else
                {
                    import std.format.write : formattedWrite;

                    // Most general case
                    formattedWrite(w, "%s", arg);
                }
            }
            catch (UTFException e)
            {
                /* Reset the writer so that it doesn't throw another
                UTFException on destruction. */
                w.highSurrogate = '\0';
                throw e;
            }
        }
    }

/**
Writes its arguments in text format to the file, followed by a newline.

Throws: `Exception` if the file is not opened.
        `ErrnoException` on an error writing to the file.
*/
    void writeln(S...)(S args)
    {
        write(args, '\n');
    }

/**
Writes its arguments in text format to the file, according to the
format string fmt.

Params:
fmt = The $(REF_ALTTEXT format string, formattedWrite, std, _format).
When passed as a compile-time argument, the string will be statically checked
against the argument types passed.
args = Items to write.

Throws: `Exception` if the file is not opened.
        `ErrnoException` on an error writing to the file.
*/
    void writef(alias fmt, A...)(A args)
    if (isSomeString!(typeof(fmt)))
    {
        import std.format : checkFormatException;

        alias e = checkFormatException!(fmt, A);
        static assert(!e, e);
        return this.writef(fmt, args);
    }

    /// ditto
    void writef(Char, A...)(in Char[] fmt, A args)
    {
        import std.format.write : formattedWrite;

        formattedWrite(lockingTextWriter(), fmt, args);
    }

    /// Equivalent to `file.writef(fmt, args, '\n')`.
    void writefln(alias fmt, A...)(A args)
    if (isSomeString!(typeof(fmt)))
    {
        import std.format : checkFormatException;

        alias e = checkFormatException!(fmt, A);
        static assert(!e, e);
        return this.writefln(fmt, args);
    }

    /// ditto
    void writefln(Char, A...)(in Char[] fmt, A args)
    {
        import std.format.write : formattedWrite;

        auto w = lockingTextWriter();
        formattedWrite(w, fmt, args);
        w.put('\n');
    }

/**
Read line from the file handle and return it as a specified type.

This version manages its own read buffer, which means one memory allocation per call. If you are not
retaining a reference to the read data, consider the `File.readln(buf)` version, which may offer
better performance as it can reuse its read buffer.

Params:
    S = Template parameter; the type of the allocated buffer, and the type returned. Defaults to `string`.
    terminator = Line terminator (by default, `'\n'`).

Note:
    String terminators are not supported due to ambiguity with readln(buf) below.

Returns:
    The line that was read, including the line terminator character.

Throws:
    `StdioException` on I/O error, or `UnicodeException` on Unicode conversion error.

Example:
---
// Reads `stdin` and writes it to `stdout`.
import std.stdio;

void main()
{
    string line;
    while ((line = stdin.readln()) !is null)
        write(line);
}
---
*/
    S readln(S = string)(dchar terminator = '\n') @safe
    if (isSomeString!S)
    {
        Unqual!(ElementEncodingType!S)[] buf;
        readln(buf, terminator);
        return (() @trusted => cast(S) buf)();
    }

    @safe unittest
    {
        import std.algorithm.comparison : equal;
        static import std.file;
        import std.meta : AliasSeq;

        auto deleteme = testFilename();
        std.file.write(deleteme, "hello\nworld\n");
        scope(exit) std.file.remove(deleteme);
        static foreach (String; AliasSeq!(string, char[], wstring, wchar[], dstring, dchar[]))
        {{
            auto witness = [ "hello\n", "world\n" ];
            auto f = File(deleteme);
            uint i = 0;
            String buf;
            while ((buf = f.readln!String()).length)
            {
                assert(i < witness.length);
                assert(equal(buf, witness[i++]));
            }
            assert(i == witness.length);
        }}
    }

    @safe unittest
    {
        static import std.file;
        import std.typecons : Tuple;

        auto deleteme = testFilename();
        std.file.write(deleteme, "cześć \U0002000D");
        scope(exit) std.file.remove(deleteme);
        uint[] lengths = [12,8,7];
        static foreach (uint i, C; Tuple!(char, wchar, dchar).Types)
        {{
            immutable(C)[] witness = "cześć \U0002000D";
            auto buf = File(deleteme).readln!(immutable(C)[])();
            assert(buf.length == lengths[i]);
            assert(buf == witness);
        }}
    }

/**
Read line from the file handle and write it to `buf[]`, including
terminating character.

This can be faster than $(D line = File.readln()) because you can reuse
the buffer for each call. Note that reusing the buffer means that you
must copy the previous contents if you wish to retain them.

Params:
buf = Buffer used to store the resulting line data. buf is
enlarged if necessary, then set to the slice exactly containing the line.
terminator = Line terminator (by default, `'\n'`). Use
$(REF newline, std,ascii) for portability (unless the file was opened in
text mode).

Returns:
0 for end of file, otherwise number of characters read.
The return value will always be equal to `buf.length`.

Throws: `StdioException` on I/O error, or `UnicodeException` on Unicode
conversion error.

Example:
---
// Read lines from `stdin` into a string
// Ignore lines starting with '#'
// Write the string to `stdout`
import std.stdio;

void main()
{
    string output;
    char[] buf;

    while (stdin.readln(buf))
    {
        if (buf[0] == '#')
            continue;

        output ~= buf;
    }

    write(output);
}
---

This method can be more efficient than the one in the previous example
because `stdin.readln(buf)` reuses (if possible) memory allocated
for `buf`, whereas $(D line = stdin.readln()) makes a new memory allocation
for every line.

For even better performance you can help `readln` by passing in a
large buffer to avoid memory reallocations. This can be done by reusing the
largest buffer returned by `readln`:

Example:
---
// Read lines from `stdin` and count words
import std.array, std.stdio;

void main()
{
    char[] buf;
    size_t words = 0;

    while (!stdin.eof)
    {
        char[] line = buf;
        stdin.readln(line);
        if (line.length > buf.length)
            buf = line;

        words += line.split.length;
    }

    writeln(words);
}
---
This is actually what $(LREF byLine) does internally, so its usage
is recommended if you want to process a complete file.
*/
    size_t readln(C)(ref C[] buf, dchar terminator = '\n') @safe
    if (isSomeChar!C && is(Unqual!C == C) && !is(C == enum))
    {
        import std.exception : enforce;

        static if (is(C == char))
        {
            enforce(_p && _p.handle, "Attempt to read from an unopened file.");
            if (_p.orientation == Orientation.unknown)
            {
                import core.stdc.wchar_ : fwide;
                auto w = fwide(_p.handle, 0);
                if (w < 0) _p.orientation = Orientation.narrow;
                else if (w > 0) _p.orientation = Orientation.wide;
            }
            return readlnImpl(_p.handle, buf, terminator, _p.orientation);
        }
        else
        {
            string s = readln(terminator);
            if (!s.length)
            {
                buf = buf[0 .. 0];
                return 0;
            }

            import std.utf : codeLength;
            buf.length = codeLength!C(s);
            size_t idx;
            foreach (C c; s)
                buf[idx++] = c;

            return buf.length;
        }
    }

    @safe unittest
    {
        static import std.file;
        auto deleteme = testFilename();
        std.file.write(deleteme, "123\n456789");
        scope(exit) std.file.remove(deleteme);

        auto file = File(deleteme);
        char[] buffer = new char[10];
        char[] line = buffer;
        file.readln(line);
        auto beyond = line.length;
        buffer[beyond] = 'a';
        file.readln(line); // should not write buffer beyond line
        assert(buffer[beyond] == 'a');
    }

    // https://issues.dlang.org/show_bug.cgi?id=15293
    @safe unittest
    {
        // @system due to readln
        static import std.file;
        auto deleteme = testFilename();
        std.file.write(deleteme, "a\n\naa");
        scope(exit) std.file.remove(deleteme);

        auto file = File(deleteme);
        char[] buffer;
        char[] line;

        file.readln(buffer, '\n');

        line = buffer;
        file.readln(line, '\n');

        line = buffer;
        file.readln(line, '\n');

        assert(line[0 .. 1].capacity == 0);
    }

/** ditto */
    size_t readln(C, R)(ref C[] buf, R terminator) @safe
    if (isSomeChar!C && is(Unqual!C == C) && !is(C == enum) &&
        isBidirectionalRange!R && is(typeof(terminator.front == dchar.init)))
    {
        import std.algorithm.mutation : swap;
        import std.algorithm.searching : endsWith;
        import std.range.primitives : back;

        auto last = terminator.back;
        C[] buf2;
        swap(buf, buf2);
        for (;;)
        {
            if (!readln(buf2, last) || endsWith(buf2, terminator))
            {
                if (buf.empty)
                {
                    buf = buf2;
                }
                else
                {
                    buf ~= buf2;
                }
                break;
            }
            buf ~= buf2;
        }
        return buf.length;
    }

    @safe unittest
    {
        static import std.file;
        import std.typecons : Tuple;

        auto deleteme = testFilename();
        std.file.write(deleteme, "hello\n\rworld\nhow\n\rare ya");
        scope(exit) std.file.remove(deleteme);
        foreach (C; Tuple!(char, wchar, dchar).Types)
        {
            immutable(C)[][] witness = [ "hello\n\r", "world\nhow\n\r", "are ya" ];
            auto f = File(deleteme);
            uint i = 0;
            C[] buf;
            while (f.readln(buf, "\n\r"))
            {
                assert(i < witness.length);
                assert(buf == witness[i++]);
            }
            assert(buf.length == 0);
        }
    }

    /**
     * Reads formatted _data from the file using $(REF formattedRead, std,_format).
     * Params:
     * format = The $(REF_ALTTEXT format string, formattedWrite, std, _format).
     * When passed as a compile-time argument, the string will be statically checked
     * against the argument types passed.
     * data = Items to be read.
     * Returns:
     *      Same as `formattedRead`: The number of variables filled. If the input range `r` ends early,
     *      this number will be less than the number of variables provided.
     * Example:
----
// test.d
void main()
{
    import std.stdio;
    auto f = File("input");
    foreach (_; 0 .. 3)
    {
        int a;
        f.readf!" %d"(a);
        writeln(++a);
    }
}
----
$(CONSOLE
% echo "1 2 3" > input
% rdmd test.d
2
3
4
)
     */
    uint readf(alias format, Data...)(auto ref Data data)
    if (isSomeString!(typeof(format)))
    {
        import std.format : checkFormatException;

        alias e = checkFormatException!(format, Data);
        static assert(!e, e);
        return this.readf(format, data);
    }

    /// ditto
    uint readf(Data...)(scope const(char)[] format, auto ref Data data)
    {
        import std.format.read : formattedRead;

        assert(isOpen);
        auto input = LockingTextReader(this);
        return formattedRead(input, format, data);
    }

    ///
    @system unittest
    {
        static import std.file;

        auto deleteme = std.file.deleteme();
        std.file.write(deleteme, "hello\nworld\ntrue\nfalse\n");
        scope(exit) std.file.remove(deleteme);
        string s;
        auto f = File(deleteme);
        f.readf!"%s\n"(s);
        assert(s == "hello", "["~s~"]");
        f.readf("%s\n", s);
        assert(s == "world", "["~s~"]");

        bool b1, b2;
        f.readf("%s\n%s\n", b1, b2);
        assert(b1 == true && b2 == false);
    }

    // backwards compatibility with pointers
    @system unittest
    {
        // @system due to readf
        static import std.file;

        auto deleteme = testFilename();
        std.file.write(deleteme, "hello\nworld\ntrue\nfalse\n");
        scope(exit) std.file.remove(deleteme);
        string s;
        auto f = File(deleteme);
        f.readf("%s\n", &s);
        assert(s == "hello", "["~s~"]");
        f.readf("%s\n", &s);
        assert(s == "world", "["~s~"]");

        // https://issues.dlang.org/show_bug.cgi?id=11698
        bool b1, b2;
        f.readf("%s\n%s\n", &b1, &b2);
        assert(b1 == true && b2 == false);
    }

    // backwards compatibility (mixed)
    @system unittest
    {
        // @system due to readf
        static import std.file;

        auto deleteme = testFilename();
        std.file.write(deleteme, "hello\nworld\ntrue\nfalse\n");
        scope(exit) std.file.remove(deleteme);
        string s1, s2;
        auto f = File(deleteme);
        f.readf("%s\n%s\n", s1, &s2);
        assert(s1 == "hello");
        assert(s2 == "world");

        // https://issues.dlang.org/show_bug.cgi?id=11698
        bool b1, b2;
        f.readf("%s\n%s\n", &b1, b2);
        assert(b1 == true && b2 == false);
    }

    // Nice error of std.stdio.readf with newlines
    // https://issues.dlang.org/show_bug.cgi?id=12260
    @system unittest
    {
        static import std.file;

        auto deleteme = testFilename();
        std.file.write(deleteme, "1\n2");
        scope(exit) std.file.remove(deleteme);
        int input;
        auto f = File(deleteme);
        f.readf("%s", &input);

        import std.conv : ConvException;
        import std.exception : collectException;
        assert(collectException!ConvException(f.readf("%s", &input)).msg ==
            "Unexpected '\\n' when converting from type LockingTextReader to type int");
    }

/**
 Returns a temporary file by calling $(CSTDIO tmpfile).
 Note that the created file has no $(LREF name).*/
    static File tmpfile() @safe
    {
        import std.exception : errnoEnforce;

        return File(errnoEnforce(.tmpfile(),
                "Could not create temporary file with tmpfile()"),
            null);
    }

/**
Unsafe function that wraps an existing `FILE*`. The resulting $(D
File) never takes the initiative in closing the file.
Note that the created file has no $(LREF name)*/
    /*private*/ static File wrapFile(FILE* f) @safe
    {
        import std.exception : enforce;

        return File(enforce(f, "Could not wrap null FILE*"),
            null, /*uint.max / 2*/ 9999);
    }

/**
Returns the `FILE*` corresponding to this object.
 */
    FILE* getFP() @safe pure
    {
        import std.exception : enforce;

        enforce(_p && _p.handle,
                "Attempting to call getFP() on an unopened file");
        return _p.handle;
    }

    @system unittest
    {
        static import core.stdc.stdio;
        assert(stdout.getFP() == core.stdc.stdio.stdout);
    }

/**
Returns the file number corresponding to this object.
 */
    @property fileno_t fileno() const @trusted
    {
        import std.exception : enforce;

        enforce(isOpen, "Attempting to call fileno() on an unopened file");
        return .fileno(cast(FILE*) _p.handle);
    }

/**
Returns the underlying operating system `HANDLE` (Windows only).
*/
    version (StdDdoc)
    @property HANDLE windowsHandle();

    version (Windows)
    @property HANDLE windowsHandle()
    {
        return cast(HANDLE)_get_osfhandle(fileno);
    }


// Note: This was documented until 2013/08
/*
Range that reads one line at a time.  Returned by $(LREF byLine).

Allows to directly use range operations on lines of a file.
*/
    private struct ByLineImpl(Char, Terminator)
    {
    private:
        import std.typecons : RefCounted, RefCountedAutoInitialize;

        /* Ref-counting stops the source range's Impl
         * from getting out of sync after the range is copied, e.g.
         * when accessing range.front, then using std.range.take,
         * then accessing range.front again. */
        alias PImpl = RefCounted!(Impl, RefCountedAutoInitialize.no);
        PImpl impl;

        static if (isScalarType!Terminator)
            enum defTerm = '\n';
        else
            enum defTerm = cast(Terminator)"\n";

    public:
        this(File f, KeepTerminator kt = No.keepTerminator,
                Terminator terminator = defTerm)
        {
            impl = PImpl(f, kt, terminator);
        }

        @property bool empty()
        {
            return impl.refCountedPayload.empty;
        }

        @property Char[] front()
        {
            return impl.refCountedPayload.front;
        }

        void popFront()
        {
            impl.refCountedPayload.popFront();
        }

    private:
        struct Impl
        {
        private:
            File file;
            Char[] line;
            Char[] buffer;
            Terminator terminator;
            KeepTerminator keepTerminator;
            bool haveLine;

        public:
            this(File f, KeepTerminator kt, Terminator terminator)
            {
                file = f;
                this.terminator = terminator;
                keepTerminator = kt;
            }

            // Range primitive implementations.
            @property bool empty()
            {
                needLine();
                return line is null;
            }

            @property Char[] front()
            {
                needLine();
                return line;
            }

            void popFront()
            {
                needLine();
                haveLine = false;
            }

        private:
            void needLine()
            {
                if (haveLine)
                    return;
                import std.algorithm.searching : endsWith;
                assert(file.isOpen);
                line = buffer;
                file.readln(line, terminator);
                if (line.length > buffer.length)
                {
                    buffer = line;
                }
                if (line.empty)
                {
                    file.detach();
                    line = null;
                }
                else if (keepTerminator == No.keepTerminator
                        && endsWith(line, terminator))
                {
                    static if (isScalarType!Terminator)
                        enum tlen = 1;
                    else static if (isArray!Terminator)
                    {
                        static assert(
                            is(immutable ElementEncodingType!Terminator == immutable Char));
                        const tlen = terminator.length;
                    }
                    else
                        static assert(false);
                    line = line[0 .. line.length - tlen];
                }
                haveLine = true;
            }
        }
    }

/**
Returns an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
set up to read from the file handle one line at a time.

The element type for the range will be `Char[]`. Range primitives
may throw `StdioException` on I/O error.

Note:
Each `front` will not persist after $(D
popFront) is called, so the caller must copy its contents (e.g. by
calling `to!string`) when retention is needed. If the caller needs
to retain a copy of every line, use the $(LREF byLineCopy) function
instead.

Params:
Char = Character type for each line, defaulting to `char`.
keepTerminator = Use `Yes.keepTerminator` to include the
terminator at the end of each line.
terminator = Line separator (`'\n'` by default). Use
$(REF newline, std,ascii) for portability (unless the file was opened in
text mode).

Example:
----
import std.algorithm, std.stdio, std.string;
// Count words in a file using ranges.
void main()
{
    auto file = File("file.txt"); // Open for reading
    const wordCount = file.byLine()            // Read lines
                          .map!split           // Split into words
                          .map!(a => a.length) // Count words per line
                          .sum();              // Total word count
    writeln(wordCount);
}
----

Example:
----
import std.range, std.stdio;
// Read lines using foreach.
void main()
{
    auto file = File("file.txt"); // Open for reading
    auto range = file.byLine();
    // Print first three lines
    foreach (line; range.take(3))
        writeln(line);
    // Print remaining lines beginning with '#'
    foreach (line; range)
    {
        if (!line.empty && line[0] == '#')
            writeln(line);
    }
}
----
Notice that neither example accesses the line data returned by
`front` after the corresponding `popFront` call is made (because
the contents may well have changed).
----

Windows specific Example:
----
import std.stdio;

version (Windows)
void main()
{

	foreach (line; File("file.txt").byLine(No.keepTerminator, "\r\n"))
	{
		writeln("|"~line~"|");
		if (line == "HelloWorld")
		    writeln("^This Line is here.");
	}

}
*/
    auto byLine(Terminator = char, Char = char)
            (KeepTerminator keepTerminator = No.keepTerminator,
            Terminator terminator = '\n')
    if (isScalarType!Terminator)
    {
        return ByLineImpl!(Char, Terminator)(this, keepTerminator, terminator);
    }

/// ditto
    auto byLine(Terminator, Char = char)
            (KeepTerminator keepTerminator, Terminator terminator)
    if (is(immutable ElementEncodingType!Terminator == immutable Char))
    {
        return ByLineImpl!(Char, Terminator)(this, keepTerminator, terminator);
    }

    @system unittest
    {
        static import std.file;
        auto deleteme = testFilename();
        std.file.write(deleteme, "hi");
        scope(success) std.file.remove(deleteme);

        import std.meta : AliasSeq;
        static foreach (T; AliasSeq!(char, wchar, dchar))
        {{
            auto blc = File(deleteme).byLine!(T, T);
            assert(blc.front == "hi");
            // check front is cached
            assert(blc.front is blc.front);
        }}
    }

    // https://issues.dlang.org/show_bug.cgi?id=19980
    @system unittest
    {
        static import std.file;
        auto deleteme = testFilename();
        std.file.write(deleteme, "Line 1\nLine 2\nLine 3\n");
        scope(success) std.file.remove(deleteme);

        auto f = File(deleteme);
        f.byLine();
        f.byLine();
        assert(f.byLine().front == "Line 1");
    }

    private struct ByLineCopy(Char, Terminator)
    {
    private:
        import std.typecons : borrow, RefCountedAutoInitialize, SafeRefCounted;

        /* Ref-counting stops the source range's ByLineCopyImpl
         * from getting out of sync after the range is copied, e.g.
         * when accessing range.front, then using std.range.take,
         * then accessing range.front again. */
        alias Impl = SafeRefCounted!(ByLineCopyImpl!(Char, Terminator),
            RefCountedAutoInitialize.no);
        Impl impl;

    public:
        this(File f, KeepTerminator kt, Terminator terminator)
        {
            impl = Impl(f, kt, terminator);
        }

        /* Verifiably `@safe` when built with -preview=DIP1000. */
        @property bool empty() @trusted
        {
            // Using `ref` is actually necessary here.
            return impl.borrow!((ref i) => i.empty);
        }

        /* Verifiably `@safe` when built with -preview=DIP1000. */
        @property Char[] front() @trusted
        {
            // Using `ref` is likely optional here.
            return impl.borrow!((ref i) => i.front);
        }

        /* Verifiably `@safe` when built with -preview=DIP1000. */
        void popFront() @trusted
        {
            impl.borrow!((ref i) => i.popFront());
        }
    }

    private struct ByLineCopyImpl(Char, Terminator)
    {
        ByLineImpl!(Unqual!Char, Terminator).Impl impl;
        bool gotFront;
        Char[] line;

    public:
        this(File f, KeepTerminator kt, Terminator terminator)
        {
            impl = ByLineImpl!(Unqual!Char, Terminator).Impl(f, kt, terminator);
        }

        @property bool empty()
        {
            return impl.empty;
        }

        @property front()
        {
            if (!gotFront)
            {
                line = impl.front.dup;
                gotFront = true;
            }
            return line;
        }

        void popFront()
        {
            impl.popFront();
            gotFront = false;
        }
    }

/**
Returns an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
set up to read from the file handle one line
at a time. Each line will be newly allocated. `front` will cache
its value to allow repeated calls without unnecessary allocations.

Note: Due to caching byLineCopy can be more memory-efficient than
`File.byLine.map!idup`.

The element type for the range will be `Char[]`. Range
primitives may throw `StdioException` on I/O error.

Params:
Char = Character type for each line, defaulting to $(D immutable char).
keepTerminator = Use `Yes.keepTerminator` to include the
terminator at the end of each line.
terminator = Line separator (`'\n'` by default). Use
$(REF newline, std,ascii) for portability (unless the file was opened in
text mode).

Example:
----
import std.algorithm, std.array, std.stdio;
// Print sorted lines of a file.
void main()
{
    auto sortedLines = File("file.txt")   // Open for reading
                       .byLineCopy()      // Read persistent lines
                       .array()           // into an array
                       .sort();           // then sort them
    foreach (line; sortedLines)
        writeln(line);
}
----
See_Also:
$(REF readText, std,file)
*/
    auto byLineCopy(Terminator = char, Char = immutable char)
            (KeepTerminator keepTerminator = No.keepTerminator,
            Terminator terminator = '\n')
    if (isScalarType!Terminator)
    {
        return ByLineCopy!(Char, Terminator)(this, keepTerminator, terminator);
    }

/// ditto
    auto byLineCopy(Terminator, Char = immutable char)
            (KeepTerminator keepTerminator, Terminator terminator)
    if (is(immutable ElementEncodingType!Terminator == immutable Char))
    {
        return ByLineCopy!(Char, Terminator)(this, keepTerminator, terminator);
    }

    @safe unittest
    {
        static assert(is(typeof(File("").byLine.front) == char[]));
        static assert(is(typeof(File("").byLineCopy.front) == string));
        static assert(
            is(typeof(File("").byLineCopy!(char, char).front) == char[]));
    }

    @system unittest
    {
        import std.algorithm.comparison : equal;
        static import std.file;

        scope(failure) printf("Failed test at line %d\n", __LINE__);
        auto deleteme = testFilename();
        std.file.write(deleteme, "");
        scope(success) std.file.remove(deleteme);

        // Test empty file
        auto f = File(deleteme);
        foreach (line; f.byLine())
        {
            assert(false);
        }
        f.detach();
        assert(!f.isOpen);

        void test(Terminator)(string txt, in string[] witness,
                KeepTerminator kt, Terminator term, bool popFirstLine = false)
        {
            import std.algorithm.sorting : sort;
            import std.array : array;
            import std.conv : text;
            import std.range.primitives : walkLength;

            uint i;
            std.file.write(deleteme, txt);
            auto f = File(deleteme);
            scope(exit)
            {
                f.close();
                assert(!f.isOpen);
            }
            auto lines = f.byLine(kt, term);
            if (popFirstLine)
            {
                lines.popFront();
                i = 1;
            }
            assert(lines.empty || lines.front is lines.front);
            foreach (line; lines)
            {
                assert(line == witness[i++]);
            }
            assert(i == witness.length, text(i, " != ", witness.length));

            // https://issues.dlang.org/show_bug.cgi?id=11830
            auto walkedLength = File(deleteme).byLine(kt, term).walkLength;
            assert(walkedLength == witness.length, text(walkedLength, " != ", witness.length));

            // test persistent lines
            assert(File(deleteme).byLineCopy(kt, term).array.sort() == witness.dup.sort());
        }

        KeepTerminator kt = No.keepTerminator;
        test("", null, kt, '\n');
        test("\n", [ "" ], kt, '\n');
        test("asd\ndef\nasdf", [ "asd", "def", "asdf" ], kt, '\n');
        test("asd\ndef\nasdf", [ "asd", "def", "asdf" ], kt, '\n', true);
        test("asd\ndef\nasdf\n", [ "asd", "def", "asdf" ], kt, '\n');
        test("foo", [ "foo" ], kt, '\n', true);
        test("bob\r\nmarge\r\nsteve\r\n", ["bob", "marge", "steve"],
            kt, "\r\n");
        test("sue\r", ["sue"], kt, '\r');

        kt = Yes.keepTerminator;
        test("", null, kt, '\n');
        test("\n", [ "\n" ], kt, '\n');
        test("asd\ndef\nasdf", [ "asd\n", "def\n", "asdf" ], kt, '\n');
        test("asd\ndef\nasdf\n", [ "asd\n", "def\n", "asdf\n" ], kt, '\n');
        test("asd\ndef\nasdf\n", [ "asd\n", "def\n", "asdf\n" ], kt, '\n', true);
        test("foo", [ "foo" ], kt, '\n');
        test("bob\r\nmarge\r\nsteve\r\n", ["bob\r\n", "marge\r\n", "steve\r\n"],
            kt, "\r\n");
        test("sue\r", ["sue\r"], kt, '\r');
    }

    @system unittest
    {
        import std.algorithm.comparison : equal;
        import std.range : drop, take;

        version (Win64)
        {
            static import std.file;

            /* the C function tmpfile doesn't seem to work, even when called from C */
            auto deleteme = testFilename();
            auto file = File(deleteme, "w+");
            scope(success) std.file.remove(deleteme);
        }
        else version (CRuntime_Bionic)
        {
            static import std.file;

            /* the C function tmpfile doesn't work when called from a shared
               library apk:
               https://code.google.com/p/android/issues/detail?id=66815 */
            auto deleteme = testFilename();
            auto file = File(deleteme, "w+");
            scope(success) std.file.remove(deleteme);
        }
        else
            auto file = File.tmpfile();
        file.write("1\n2\n3\n");

        // https://issues.dlang.org/show_bug.cgi?id=9599
        file.rewind();
        File.ByLineImpl!(char, char) fbl = file.byLine();
        auto fbl2 = fbl;
        assert(fbl.front == "1");
        assert(fbl.front is fbl2.front);
        assert(fbl.take(1).equal(["1"]));
        assert(fbl.equal(["2", "3"]));
        assert(fbl.empty);
        assert(file.isOpen); // we still have a valid reference

        file.rewind();
        fbl = file.byLine();
        assert(!fbl.drop(2).empty);
        assert(fbl.equal(["3"]));
        assert(fbl.empty);
        assert(file.isOpen);

        file.detach();
        assert(!file.isOpen);
    }

    @safe unittest
    {
        static import std.file;
        auto deleteme = testFilename();
        std.file.write(deleteme, "hi");
        scope(success) std.file.remove(deleteme);

        auto blc = File(deleteme).byLineCopy;
        assert(!blc.empty);
        // check front is cached
        assert(blc.front is blc.front);
    }

    /**
    Creates an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
    set up to parse one line at a time from the file into a tuple.

    Range primitives may throw `StdioException` on I/O error.

    Params:
        format = tuple record $(REF_ALTTEXT _format, formattedRead, std, _format)

    Returns:
        The input range set up to parse one line at a time into a record tuple.

    See_Also:

        It is similar to $(LREF byLine) and uses
        $(REF_ALTTEXT _format, formattedRead, std, _format) under the hood.
    */
    template byRecord(Fields...)
    {
        auto byRecord(string format)
        {
            return ByRecordImpl!(Fields)(this, format);
        }
    }

    ///
    @system unittest
    {
         static import std.file;
         import std.typecons : tuple;

         // prepare test file
         auto testFile = std.file.deleteme();
         scope(failure) printf("Failed test at line %d\n", __LINE__);
         std.file.write(testFile, "1 2\n4 1\n5 100");
         scope(exit) std.file.remove(testFile);

         File f = File(testFile);
         scope(exit) f.close();

         auto expected = [tuple(1, 2), tuple(4, 1), tuple(5, 100)];
         uint i;
         foreach (e; f.byRecord!(int, int)("%s %s"))
         {
             assert(e == expected[i++]);
         }
    }

    // Note: This was documented until 2013/08
    /*
     * Range that reads a chunk at a time.
     */
    private struct ByChunkImpl
    {
    private:
        File    file_;
        ubyte[] chunk_;

        void prime()
        {
            chunk_ = file_.rawRead(chunk_);
            if (chunk_.length == 0)
                file_.detach();
        }

    public:
        this(File file, size_t size)
        {
            this(file, new ubyte[](size));
        }

        this(File file, ubyte[] buffer)
        {
            import std.exception : enforce;
            enforce(buffer.length, "size must be larger than 0");
            file_ = file;
            chunk_ = buffer;
            prime();
        }

        // `ByChunk`'s input range primitive operations.
        @property nothrow
        bool empty() const
        {
            return !file_.isOpen;
        }

        /// Ditto
        @property nothrow
        ubyte[] front()
        {
            version (assert)
            {
                import core.exception : RangeError;
                if (empty)
                    throw new RangeError();
            }
            return chunk_;
        }

        /// Ditto
        void popFront()
        {
            version (assert)
            {
                import core.exception : RangeError;
                if (empty)
                    throw new RangeError();
            }
            prime();
        }
    }

/**
Returns an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
set up to read from the file handle a chunk at a time.

The element type for the range will be `ubyte[]`. Range primitives
may throw `StdioException` on I/O error.

Example:
---------
void main()
{
    // Read standard input 4KB at a time
    foreach (ubyte[] buffer; stdin.byChunk(4096))
    {
        ... use buffer ...
    }
}
---------

The parameter may be a number (as shown in the example above) dictating the
size of each chunk. Alternatively, `byChunk` accepts a
user-provided buffer that it uses directly.

Example:
---------
void main()
{
    // Read standard input 4KB at a time
    foreach (ubyte[] buffer; stdin.byChunk(new ubyte[4096]))
    {
        ... use buffer ...
    }
}
---------

In either case, the content of the buffer is reused across calls. That means
`front` will not persist after `popFront` is called, so if retention is
needed, the caller must copy its contents (e.g. by calling `buffer.dup`).

In the  example above, `buffer.length` is 4096 for all iterations, except
for the last one, in which case `buffer.length` may be less than 4096 (but
always greater than zero).

With the mentioned limitations, `byChunk` works with any algorithm
compatible with input ranges.

Example:
---
// Efficient file copy, 1MB at a time.
import std.algorithm, std.stdio;
void main()
{
    stdin.byChunk(1024 * 1024).copy(stdout.lockingTextWriter());
}
---

$(REF joiner, std,algorithm,iteration) can be used to join chunks together into
a single range lazily.
Example:
---
import std.algorithm, std.stdio;
void main()
{
    //Range of ranges
    static assert(is(typeof(stdin.byChunk(4096).front) == ubyte[]));
    //Range of elements
    static assert(is(typeof(stdin.byChunk(4096).joiner.front) == ubyte));
}
---

Returns: A call to `byChunk` returns a range initialized with the `File`
object and the appropriate buffer.

Throws: If the user-provided size is zero or the user-provided buffer
is empty, throws an `Exception`. In case of an I/O error throws
`StdioException`.
 */
    auto byChunk(size_t chunkSize)
    {
        return ByChunkImpl(this, chunkSize);
    }
/// Ditto
    auto byChunk(ubyte[] buffer)
    {
        return ByChunkImpl(this, buffer);
    }

    @system unittest
    {
        static import std.file;

        scope(failure) printf("Failed test at line %d\n", __LINE__);

        auto deleteme = testFilename();
        std.file.write(deleteme, "asd\ndef\nasdf");

        auto witness = ["asd\n", "def\n", "asdf" ];
        auto f = File(deleteme);
        scope(exit)
        {
            f.close();
            assert(!f.isOpen);
            std.file.remove(deleteme);
        }

        uint i;
        foreach (chunk; f.byChunk(4))
            assert(chunk == cast(ubyte[]) witness[i++]);

        assert(i == witness.length);
    }

    @system unittest
    {
        static import std.file;

        scope(failure) printf("Failed test at line %d\n", __LINE__);

        auto deleteme = testFilename();
        std.file.write(deleteme, "asd\ndef\nasdf");

        auto witness = ["asd\n", "def\n", "asdf" ];
        auto f = File(deleteme);
        scope(exit)
        {
            f.close();
            assert(!f.isOpen);
            std.file.remove(deleteme);
        }

        uint i;
        foreach (chunk; f.byChunk(new ubyte[4]))
            assert(chunk == cast(ubyte[]) witness[i++]);

        assert(i == witness.length);
    }

    // Note: This was documented until 2013/08
/*
`Range` that locks the file and allows fast writing to it.
 */
    struct LockingTextWriter
    {
    private:
        import std.range.primitives : ElementType, isInfinite, isInputRange;
        // Access the FILE* handle through the 'file_' member
        // to keep the object alive through refcounting
        File file_;

        // the unshared version of FILE* handle, extracted from the File object
        @property _iobuf* handle_() @trusted { return cast(_iobuf*) file_._p.handle; }

        // the file's orientation (byte- or wide-oriented)
        int orientation_;

        // Buffers for when we need to transcode.
        wchar highSurrogate = '\0'; // '\0' indicates empty
        void highSurrogateShouldBeEmpty() @safe
        {
            import std.utf : UTFException;
            if (highSurrogate != '\0')
                throw new UTFException("unpaired surrogate UTF-16 value");
        }
        char[4] rbuf8;
        size_t rbuf8Filled = 0;
    public:

        this(ref File f) @trusted
        {
            import std.exception : enforce;

            enforce(f._p && f._p.handle, "Attempting to write to closed File");
            file_ = f;
            FILE* fps = f._p.handle;

            version (CRuntime_Microsoft)
            {
                // Microsoft doesn't implement fwide. Instead, there's the
                // concept of ANSI/UNICODE mode. fputc doesn't work in UNICODE
                // mode; fputwc has to be used. So that essentially means
                // "wide-oriented" for us.
                immutable int mode = _setmode(f.fileno, _O_TEXT);
                    // Set some arbitrary mode to obtain the previous one.
                if (mode != -1) // _setmode() succeeded
                {
                    _setmode(f.fileno, mode); // Restore previous mode.
                    if (mode & (_O_WTEXT | _O_U16TEXT | _O_U8TEXT))
                    {
                        orientation_ = 1; // wide
                    }
                }
            }
            else
            {
                import core.stdc.wchar_ : fwide;
                orientation_ = fwide(fps, 0);
            }

            _FLOCK(fps);
        }

        ~this() @trusted
        {
            if (auto p = file_._p)
            {
                if (p.handle) _FUNLOCK(p.handle);
            }
            file_ = File.init;
                /* Destroy file_ before possibly throwing. Else it wouldn't be
                destroyed, and its reference count would be wrong. */
            highSurrogateShouldBeEmpty();
        }

        this(this) @trusted
        {
            if (auto p = file_._p)
            {
                if (p.handle) _FLOCK(p.handle);
            }
        }

        /// Range primitive implementations.
        void put(A)(scope A writeme)
        if ((isSomeChar!(ElementType!A) ||
            is(ElementType!A : const(ubyte))) &&
            isInputRange!A &&
            !isInfinite!A)
        {
            import std.exception : errnoEnforce;

            alias C = ElementEncodingType!A;
            static assert(!is(C == void));
            static if (isSomeString!A && C.sizeof == 1 || is(A : const(ubyte)[]))
            {
                if (orientation_ <= 0)
                {
                    //file.write(writeme); causes infinite recursion!!!
                    //file.rawWrite(writeme);
                    auto result = trustedFwrite(file_._p.handle, writeme);
                    if (result != writeme.length) errnoEnforce(0);
                    return;
                }
            }

            // put each element in turn.
            foreach (c; writeme)
            {
                put(c);
            }
        }

        /// ditto
        void put(C)(scope C c) @safe
        if (isSomeChar!C || is(C : const(ubyte)))
        {
            import std.utf : decodeFront, encode, stride;

            static if (c.sizeof == 1)
            {
                highSurrogateShouldBeEmpty();
                if (orientation_ <= 0) trustedFPUTC(c, handle_);
                else if (c <= 0x7F) trustedFPUTWC(c, handle_);
                else if (c >= 0b1100_0000) // start byte of multibyte sequence
                {
                    rbuf8[0] = c;
                    rbuf8Filled = 1;
                }
                else // continuation byte of multibyte sequence
                {
                    rbuf8[rbuf8Filled] = c;
                    ++rbuf8Filled;
                    if (stride(rbuf8[]) == rbuf8Filled) // sequence is complete
                    {
                        char[] str = rbuf8[0 .. rbuf8Filled];
                        immutable dchar d = decodeFront(str);
                        wchar_t[4 / wchar_t.sizeof] wbuf;
                        immutable size = encode(wbuf, d);
                        foreach (i; 0 .. size)
                            trustedFPUTWC(wbuf[i], handle_);
                        rbuf8Filled = 0;
                    }
                }
            }
            else static if (c.sizeof == 2)
            {
                import std.utf : decode;

                if (c <= 0x7F)
                {
                    highSurrogateShouldBeEmpty();
                    if (orientation_ <= 0) trustedFPUTC(c, handle_);
                    else trustedFPUTWC(c, handle_);
                }
                else if (0xD800 <= c && c <= 0xDBFF) // high surrogate
                {
                    highSurrogateShouldBeEmpty();
                    highSurrogate = c;
                }
                else // standalone or low surrogate
                {
                    dchar d = c;
                    if (highSurrogate != '\0')
                    {
                        immutable wchar[2] rbuf = [highSurrogate, c];
                        size_t index = 0;
                        d = decode(rbuf[], index);
                        highSurrogate = 0;
                    }
                    if (orientation_ <= 0)
                    {
                        char[4] wbuf;
                        immutable size = encode(wbuf, d);
                        foreach (i; 0 .. size)
                            trustedFPUTC(wbuf[i], handle_);
                    }
                    else
                    {
                        wchar_t[4 / wchar_t.sizeof] wbuf;
                        immutable size = encode(wbuf, d);
                        foreach (i; 0 .. size)
                            trustedFPUTWC(wbuf[i], handle_);
                    }
                    rbuf8Filled = 0;
                }
            }
            else // 32-bit characters
            {
                import std.utf : encode;

                highSurrogateShouldBeEmpty();
                if (orientation_ <= 0)
                {
                    if (c <= 0x7F)
                    {
                        trustedFPUTC(c, handle_);
                    }
                    else
                    {
                        char[4] buf = void;
                        immutable len = encode(buf, c);
                        foreach (i ; 0 .. len)
                            trustedFPUTC(buf[i], handle_);
                    }
                }
                else
                {
                    version (Windows)
                    {
                        import std.utf : isValidDchar;

                        assert(isValidDchar(c));
                        if (c <= 0xFFFF)
                        {
                            trustedFPUTWC(cast(wchar_t) c, handle_);
                        }
                        else
                        {
                            trustedFPUTWC(cast(wchar_t)
                                    ((((c - 0x10000) >> 10) & 0x3FF)
                                            + 0xD800), handle_);
                            trustedFPUTWC(cast(wchar_t)
                                    (((c - 0x10000) & 0x3FF) + 0xDC00),
                                    handle_);
                        }
                    }
                    else version (Posix)
                    {
                        trustedFPUTWC(cast(wchar_t) c, handle_);
                    }
                    else
                    {
                        static assert(0);
                    }
                }
            }
        }
    }

    /**
     * Output range which locks the file when created, and unlocks the file when it goes
     * out of scope.
     *
     * Returns: An $(REF_ALTTEXT output range, isOutputRange, std, range, primitives)
     * which accepts string types, `ubyte[]`, individual character types, and
     * individual `ubyte`s.
     *
     * Note: Writing either arrays of `char`s or `ubyte`s is faster than
     * writing each character individually from a range. For large amounts of data,
     * writing the contents in chunks using an intermediary array can result
     * in a speed increase.
     *
     * Throws: $(REF UTFException, std, utf) if the data given is a `char` range
     * and it contains malformed UTF data.
     *
     * See_Also: $(LREF byChunk) for an example.
     */
    auto lockingTextWriter() @safe
    {
        return LockingTextWriter(this);
    }

    // An output range which optionally locks the file and puts it into
    // binary mode (similar to rawWrite). Because it needs to restore
    // the file mode on destruction, it is RefCounted on Windows.
    struct BinaryWriterImpl(bool locking)
    {
        import std.traits : hasIndirections;
    private:
        // Access the FILE* handle through the 'file_' member
        // to keep the object alive through refcounting
        File file_;
        string name;

        version (Windows)
        {
            fileno_t fd;
            int oldMode;
        }

    public:
        // Don't use this, but `File.lockingBinaryWriter()` instead.
        // Must be public for RefCounted and emplace() in druntime.
        this(scope ref File f)
        {
            import std.exception : enforce;
            file_ = f;
            enforce(f._p && f._p.handle);
            name = f._name;
            FILE* fps = f._p.handle;
            static if (locking)
                _FLOCK(fps);

            version (Windows)
            {
                .fflush(fps); // before changing translation mode
                fd = .fileno(fps);
                oldMode = ._setmode(fd, _O_BINARY);
            }
        }

        ~this()
        {
            if (!file_._p || !file_._p.handle)
                return;

            FILE* fps = file_._p.handle;

            version (Windows)
            {
                .fflush(fps); // before restoring translation mode
                ._setmode(fd, oldMode);
            }

            _FUNLOCK(fps);
        }

        void rawWrite(T)(in T[] buffer)
        {
            import std.conv : text;
            import std.exception : errnoEnforce;

            auto result = trustedFwrite(file_._p.handle, buffer);
            if (result == result.max) result = 0;
            errnoEnforce(result == buffer.length,
                    text("Wrote ", result, " instead of ", buffer.length,
                            " objects of type ", T.stringof, " to file `",
                            name, "'"));
        }

        version (Windows)
        {
            @disable this(this);
        }
        else
        {
            this(this)
            {
                if (auto p = file_._p)
                {
                    if (p.handle) _FLOCK(p.handle);
                }
            }
        }

        void put(T)(auto ref scope const T value)
        if (!hasIndirections!T &&
            !isInputRange!T)
        {
            rawWrite((&value)[0 .. 1]);
        }

        void put(T)(scope const(T)[] array)
        if (!hasIndirections!T &&
            !isInputRange!T)
        {
            rawWrite(array);
        }
    }

/** Returns an output range that locks the file and allows fast writing to it.

Example:
Produce a grayscale image of the $(LINK2 https://en.wikipedia.org/wiki/Mandelbrot_set, Mandelbrot set)
in binary $(LINK2 https://en.wikipedia.org/wiki/Netpbm_format, Netpbm format) to standard output.
---
import std.algorithm, std.complex, std.range, std.stdio;

void main()
{
    enum size = 500;
    writef("P5\n%d %d %d\n", size, size, ubyte.max);

    iota(-1, 3, 2.0/size).map!(y =>
        iota(-1.5, 0.5, 2.0/size).map!(x =>
            cast(ubyte)(1+
                recurrence!((a, n) => x + y * complex(0, 1) + a[n-1]^^2)(complex(0))
                .take(ubyte.max)
                .countUntil!(z => z.re^^2 + z.im^^2 > 4))
        )
    )
    .copy(stdout.lockingBinaryWriter);
}
---
*/
    auto lockingBinaryWriter()
    {
        alias LockingBinaryWriterImpl = BinaryWriterImpl!true;

        version (Windows)
        {
            import std.typecons : RefCounted;
            alias LockingBinaryWriter = RefCounted!LockingBinaryWriterImpl;
        }
        else
            alias LockingBinaryWriter = LockingBinaryWriterImpl;

        return LockingBinaryWriter(this);
    }

    @system unittest
    {
        import std.algorithm.mutation : reverse;
        import std.exception : collectException;
        static import std.file;
        import std.range : only, retro;
        import std.string : format;

        auto deleteme = testFilename();
        scope(exit) collectException(std.file.remove(deleteme));

        {
            auto writer = File(deleteme, "wb").lockingBinaryWriter();
            auto input = File(deleteme, "rb");

            ubyte[1] byteIn = [42];
            writer.rawWrite(byteIn);
            destroy(writer);

            ubyte[1] byteOut = input.rawRead(new ubyte[1]);
            assert(byteIn[0] == byteOut[0]);
        }

        auto output = File(deleteme, "wb");
        auto writer = output.lockingBinaryWriter();
        auto input = File(deleteme, "rb");

        T[] readExact(T)(T[] buf)
        {
            auto result = input.rawRead(buf);
            assert(result.length == buf.length,
                "Read %d out of %d bytes"
                .format(result.length, buf.length));
            return result;
        }

        // test raw values
        ubyte byteIn = 42;
        byteIn.only.copy(writer); output.flush();
        ubyte byteOut = readExact(new ubyte[1])[0];
        assert(byteIn == byteOut);

        // test arrays
        ubyte[] bytesIn = [1, 2, 3, 4, 5];
        bytesIn.copy(writer); output.flush();
        ubyte[] bytesOut = readExact(new ubyte[bytesIn.length]);
        scope(failure) .writeln(bytesOut);
        assert(bytesIn == bytesOut);

        // test ranges of values
        bytesIn.retro.copy(writer); output.flush();
        bytesOut = readExact(bytesOut);
        bytesOut.reverse();
        assert(bytesIn == bytesOut);

        // test string
        "foobar".copy(writer); output.flush();
        char[] charsOut = readExact(new char[6]);
        assert(charsOut == "foobar");

        // test ranges of arrays
        only("foo", "bar").copy(writer); output.flush();
        charsOut = readExact(charsOut);
        assert(charsOut == "foobar");

        // test that we are writing arrays as is,
        // without UTF-8 transcoding
        "foo"d.copy(writer); output.flush();
        dchar[] dcharsOut = readExact(new dchar[3]);
        assert(dcharsOut == "foo");
    }

/** Returns the size of the file in bytes, ulong.max if file is not searchable or throws if the operation fails.
Example:
---
import std.stdio, std.file;

void main()
{
    string deleteme = "delete.me";
    auto file_handle = File(deleteme, "w");
    file_handle.write("abc"); //create temporary file
    scope(exit) deleteme.remove; //remove temporary file at scope exit

    assert(file_handle.size() == 3); //check if file size is 3 bytes
}
---
*/
    @property ulong size() @safe
    {
        import std.exception : collectException;

        ulong pos = void;
        if (collectException(pos = tell)) return ulong.max;
        scope(exit) seek(pos);
        seek(0, SEEK_END);
        return tell;
    }
}

@system unittest
{
    @system struct SystemToString
    {
        string toString()
        {
            return "system";
        }
    }

    @trusted struct TrustedToString
    {
        string toString()
        {
            return "trusted";
        }
    }

    @safe struct SafeToString
    {
        string toString()
        {
            return "safe";
        }
    }

    @system void systemTests()
    {
        //system code can write to files/stdout with anything!
        if (false)
        {
            auto f = File();

            f.write("just a string");
            f.write("string with arg: ", 47);
            f.write(SystemToString());
            f.write(TrustedToString());
            f.write(SafeToString());

            write("just a string");
            write("string with arg: ", 47);
            write(SystemToString());
            write(TrustedToString());
            write(SafeToString());

            f.writeln("just a string");
            f.writeln("string with arg: ", 47);
            f.writeln(SystemToString());
            f.writeln(TrustedToString());
            f.writeln(SafeToString());

            writeln("just a string");
            writeln("string with arg: ", 47);
            writeln(SystemToString());
            writeln(TrustedToString());
            writeln(SafeToString());

            f.writef("string with arg: %s", 47);
            f.writef("%s", SystemToString());
            f.writef("%s", TrustedToString());
            f.writef("%s", SafeToString());

            writef("string with arg: %s", 47);
            writef("%s", SystemToString());
            writef("%s", TrustedToString());
            writef("%s", SafeToString());

            f.writefln("string with arg: %s", 47);
            f.writefln("%s", SystemToString());
            f.writefln("%s", TrustedToString());
            f.writefln("%s", SafeToString());

            writefln("string with arg: %s", 47);
            writefln("%s", SystemToString());
            writefln("%s", TrustedToString());
            writefln("%s", SafeToString());
        }
    }

    @safe void safeTests()
    {
        auto f = File();

        //safe code can write to files only with @safe and @trusted code...
        if (false)
        {
            f.write("just a string");
            f.write("string with arg: ", 47);
            f.write(TrustedToString());
            f.write(SafeToString());

            write("just a string");
            write("string with arg: ", 47);
            write(TrustedToString());
            write(SafeToString());

            f.writeln("just a string");
            f.writeln("string with arg: ", 47);
            f.writeln(TrustedToString());
            f.writeln(SafeToString());

            writeln("just a string");
            writeln("string with arg: ", 47);
            writeln(TrustedToString());
            writeln(SafeToString());

            f.writef("string with arg: %s", 47);
            f.writef("%s", TrustedToString());
            f.writef("%s", SafeToString());

            writef("string with arg: %s", 47);
            writef("%s", TrustedToString());
            writef("%s", SafeToString());

            f.writefln("string with arg: %s", 47);
            f.writefln("%s", TrustedToString());
            f.writefln("%s", SafeToString());

            writefln("string with arg: %s", 47);
            writefln("%s", TrustedToString());
            writefln("%s", SafeToString());
        }

        static assert(!__traits(compiles, f.write(SystemToString().toString())));
        static assert(!__traits(compiles, f.writeln(SystemToString())));
        static assert(!__traits(compiles, f.writef("%s", SystemToString())));
        static assert(!__traits(compiles, f.writefln("%s", SystemToString())));

        static assert(!__traits(compiles, write(SystemToString().toString())));
        static assert(!__traits(compiles, writeln(SystemToString())));
        static assert(!__traits(compiles, writef("%s", SystemToString())));
        static assert(!__traits(compiles, writefln("%s", SystemToString())));
    }

    systemTests();
    safeTests();
}

@safe unittest
{
    import std.exception : collectException;
    static import std.file;

    auto deleteme = testFilename();
    scope(exit) collectException(std.file.remove(deleteme));
    std.file.write(deleteme, "1 2 3");
    auto f = File(deleteme);
    assert(f.size == 5);
    assert(f.tell == 0);
}

@safe unittest
{
    static import std.file;
    import std.range : chain, only, repeat;
    import std.range.primitives : isOutputRange;

    auto deleteme = testFilename();
    scope(exit) std.file.remove(deleteme);

    {
        auto writer = File(deleteme, "w").lockingTextWriter();
        static assert(isOutputRange!(typeof(writer), dchar));
        writer.put("日本語");
        writer.put("日本語"w);
        writer.put("日本語"d);
        writer.put('日');
        writer.put(chain(only('本'), only('語')));
        // https://issues.dlang.org/show_bug.cgi?id=11945
        writer.put(repeat('#', 12));
        // https://issues.dlang.org/show_bug.cgi?id=17229
        writer.put(cast(immutable(ubyte)[])"日本語");
    }
    assert(File(deleteme).readln() == "日本語日本語日本語日本語############日本語");
}

@safe unittest // wchar -> char
{
    static import std.file;
    import std.exception : assertThrown;
    import std.utf : UTFException;

    auto deleteme = testFilename();
    scope(exit) std.file.remove(deleteme);

    {
        auto writer = File(deleteme, "w").lockingTextWriter();
        writer.put("\U0001F608"w);
    }
    assert(std.file.readText!string(deleteme) == "\U0001F608");

    // Test invalid input: unpaired high surrogate
    {
        immutable wchar surr = "\U0001F608"w[0];
        auto f = File(deleteme, "w");
        assertThrown!UTFException(() {
            auto writer = f.lockingTextWriter();
            writer.put('x');
            writer.put(surr);
            assertThrown!UTFException(writer.put(char('y')));
            assertThrown!UTFException(writer.put(wchar('y')));
            assertThrown!UTFException(writer.put(dchar('y')));
            assertThrown!UTFException(writer.put(surr));
            // First `surr` is still unpaired at this point. `writer` gets
            // destroyed now, and the destructor throws a UTFException for
            // the unpaired surrogate.
        } ());
    }
    assert(std.file.readText!string(deleteme) == "x");

    // Test invalid input: unpaired low surrogate
    {
        immutable wchar surr = "\U0001F608"w[1];
        auto writer = File(deleteme, "w").lockingTextWriter();
        assertThrown!UTFException(writer.put(surr));
        writer.put('y');
        assertThrown!UTFException(writer.put(surr));
    }
    assert(std.file.readText!string(deleteme) == "y");
}

@safe unittest // https://issues.dlang.org/show_bug.cgi?id=18801
{
    static import std.file;
    import std.string : stripLeft;

    auto deleteme = testFilename();
    scope(exit) std.file.remove(deleteme);

    {
        auto writer = File(deleteme, "w,ccs=UTF-8").lockingTextWriter();
        writer.put("foo");
    }
    assert(std.file.readText!string(deleteme).stripLeft("\uFEFF") == "foo");

    {
        auto writer = File(deleteme, "a,ccs=UTF-8").lockingTextWriter();
        writer.put("bar");
    }
    assert(std.file.readText!string(deleteme).stripLeft("\uFEFF") == "foobar");
}
@safe unittest // char/wchar -> wchar_t
{
    import core.stdc.locale : LC_CTYPE, setlocale;
    import core.stdc.wchar_ : fwide;
    import core.stdc.string : strlen;
    import std.algorithm.searching : any, endsWith;
    import std.conv : text;
    import std.meta : AliasSeq;
    import std.string : fromStringz, stripLeft;
    static import std.file;
    auto deleteme = testFilename();
    scope(exit) std.file.remove(deleteme);
    const char* oldCt = () @trusted {
        const(char)* p = setlocale(LC_CTYPE, null);
        // Subsequent calls to `setlocale` might invalidate this return value,
        // so duplicate it.
        // See: https://github.com/dlang/phobos/pull/7660
        return p ? p[0 .. strlen(p) + 1].idup.ptr : null;
    }();
    const utf8 = ["en_US.UTF-8", "C.UTF-8", ".65001"].any!((loc) @trusted {
        return setlocale(LC_CTYPE, loc.ptr).fromStringz.endsWith(loc);
    });
    scope(exit) () @trusted { setlocale(LC_CTYPE, oldCt); } ();
    alias strs = AliasSeq!("xä\U0001F607", "yö\U0001F608"w);
    {
        auto f = File(deleteme, "w");
        version (CRuntime_Microsoft)
        {
            () @trusted { _setmode(fileno(f.getFP()), _O_U8TEXT); } ();
        }
        else
        {
            assert(fwide(f.getFP(), 1) == 1);
        }
        auto writer = f.lockingTextWriter();
        assert(writer.orientation_ == 1);
        static foreach (s; strs) writer.put(s);
    }
    assert(std.file.readText!string(deleteme).stripLeft("\uFEFF") ==
        text(strs));
}
@safe unittest // https://issues.dlang.org/show_bug.cgi?id=18789
{
    static import std.file;
    auto deleteme = testFilename();
    scope(exit) std.file.remove(deleteme);
    // converting to char
    {
        auto f = File(deleteme, "w");
        f.writeln("\U0001F608"w); // UTFException
    }
    // converting to wchar_t
    {
        auto f = File(deleteme, "w,ccs=UTF-16LE");
        // from char
        f.writeln("ö"); // writes garbage
        f.writeln("\U0001F608"); // ditto
        // from wchar
        f.writeln("\U0001F608"w); // leads to ErrnoException
    }
}

@safe unittest
{
    import std.exception : collectException;
    auto e = collectException({ File f; f.writeln("Hello!"); }());
    assert(e && e.msg == "Attempting to write to closed File");
}

@safe unittest // https://issues.dlang.org/show_bug.cgi?id=21592
{
    import std.exception : collectException;
    import std.utf : UTFException;
    static import std.file;
    auto deleteme = testFilename();
    scope(exit) std.file.remove(deleteme);
    auto f = File(deleteme, "w");
    auto e = collectException!UTFException(f.writeln(wchar(0xD801)));
    assert(e.next is null);
}

version (StdStressTest)
{
    // https://issues.dlang.org/show_bug.cgi?id=15768
    @system unittest
    {
        import std.parallelism : parallel;
        import std.range : iota;

        auto deleteme = testFilename();
        stderr = File(deleteme, "w");

        foreach (t; 1_000_000.iota.parallel)
        {
            stderr.write("aaa");
        }
    }
}

/// Used to specify the lock type for `File.lock` and `File.tryLock`.
enum LockType
{
    /**
     * Specifies a _read (shared) lock. A _read lock denies all processes
     * write access to the specified region of the file, including the
     * process that first locks the region. All processes can _read the
     * locked region. Multiple simultaneous _read locks are allowed, as
     * long as there are no exclusive locks.
     */
    read,

    /**
     * Specifies a read/write (exclusive) lock. A read/write lock denies all
     * other processes both read and write access to the locked file region.
     * If a segment has an exclusive lock, it may not have any shared locks
     * or other exclusive locks.
     */
    readWrite
}

struct LockingTextReader
{
    private File _f;
    private char _front;
    private bool _hasChar;

    this(File f)
    {
        import std.exception : enforce;
        enforce(f.isOpen, "LockingTextReader: File must be open");
        _f = f;
        _FLOCK(_f._p.handle);
    }

    this(this)
    {
        _FLOCK(_f._p.handle);
    }

    ~this()
    {
        if (_hasChar)
            ungetc(_front, cast(FILE*)_f._p.handle);

        // File locking has its own reference count
        if (_f.isOpen) _FUNLOCK(_f._p.handle);
    }

    void opAssign(LockingTextReader r)
    {
        import std.algorithm.mutation : swap;
        swap(this, r);
    }

    @property bool empty()
    {
        if (!_hasChar)
        {
            if (!_f.isOpen || _f.eof)
                return true;
            immutable int c = _FGETC(cast(_iobuf*) _f._p.handle);
            if (c == EOF)
            {
                .destroy(_f);
                return true;
            }
            _front = cast(char) c;
            _hasChar = true;
        }
        return false;
    }

    @property char front()
    {
        if (!_hasChar)
        {
            version (assert)
            {
                import core.exception : RangeError;
                if (empty)
                    throw new RangeError();
            }
            else
            {
                empty;
            }
        }
        return _front;
    }

    void popFront()
    {
        if (!_hasChar)
            empty;
        _hasChar = false;
    }
}

@system unittest
{
    // @system due to readf
    static import std.file;
    import std.range.primitives : isInputRange;

    static assert(isInputRange!LockingTextReader);
    auto deleteme = testFilename();
    std.file.write(deleteme, "1 2 3");
    scope(exit) std.file.remove(deleteme);
    int x;
    auto f = File(deleteme);
    f.readf("%s ", &x);
    assert(x == 1);
    f.readf("%d ", &x);
    assert(x == 2);
    f.readf("%d ", &x);
    assert(x == 3);
}

// https://issues.dlang.org/show_bug.cgi?id=13686
@system unittest
{
    import std.algorithm.comparison : equal;
    static import std.file;
    import std.utf : byDchar;

    auto deleteme = testFilename();
    std.file.write(deleteme, "Тест");
    scope(exit) std.file.remove(deleteme);

    string s;
    File(deleteme).readf("%s", &s);
    assert(s == "Тест");

    auto ltr = LockingTextReader(File(deleteme)).byDchar;
    assert(equal(ltr, "Тест".byDchar));
}

// https://issues.dlang.org/show_bug.cgi?id=12320
@system unittest
{
    static import std.file;
    auto deleteme = testFilename();
    std.file.write(deleteme, "ab");
    scope(exit) std.file.remove(deleteme);
    auto ltr = LockingTextReader(File(deleteme));
    assert(ltr.front == 'a');
    ltr.popFront();
    assert(ltr.front == 'b');
    ltr.popFront();
    assert(ltr.empty);
}

// https://issues.dlang.org/show_bug.cgi?id=14861
@system unittest
{
    // @system due to readf
    static import std.file;
    auto deleteme = testFilename();
    File fw = File(deleteme, "w");
    for (int i; i != 5000; i++)
        fw.writeln(i, ";", "Иванов;Пётр;Петрович");
    fw.close();
    scope(exit) std.file.remove(deleteme);
    // Test read
    File fr = File(deleteme, "r");
    scope (exit) fr.close();
    int nom; string fam, nam, ot;
    // Error format read
    while (!fr.eof)
        fr.readf("%s;%s;%s;%s\n", &nom, &fam, &nam, &ot);
}

/**
 * Indicates whether `T` is a file handle, i.e. the type
 * is implicitly convertable to $(LREF File) or a pointer to a
 * $(REF FILE, core,stdc,stdio).
 *
 * Returns:
 *      `true` if `T` is a file handle, `false` otherwise.
 */
template isFileHandle(T)
{
    enum isFileHandle = is(T : FILE*) ||
        is(T : File);
}

///
@safe unittest
{
    static assert(isFileHandle!(FILE*));
    static assert(isFileHandle!(File));
}

/**
 * Property used by writeln/etc. so it can infer @safe since stdout is __gshared
 */
private @property File trustedStdout() @trusted
{
    return stdout;
}

/***********************************
Writes its arguments in text format to standard output (without a trailing newline).

Params:
    args = the items to write to `stdout`

Throws: In case of an I/O error, throws an `StdioException`.

Example:
    Reads `stdin` and writes it to `stdout` with an argument
    counter.
---
import std.stdio;

void main()
{
    string line;

    for (size_t count = 0; (line = readln) !is null; count++)
    {
         write("Input ", count, ": ", line, "\n");
    }
}
---
 */
void write(T...)(T args)
if (!is(T[0] : File))
{
    trustedStdout.write(args);
}

@system unittest
{
    static import std.file;

    scope(failure) printf("Failed test at line %d\n", __LINE__);
    void[] buf;
    if (false) write(buf);
    // test write
    auto deleteme = testFilename();
    auto f = File(deleteme, "w");
    f.write("Hello, ",  "world number ", 42, "!");
    f.close();
    scope(exit) { std.file.remove(deleteme); }
    assert(cast(char[]) std.file.read(deleteme) == "Hello, world number 42!");
}

/***********************************
 * Equivalent to `write(args, '\n')`.  Calling `writeln` without
 * arguments is valid and just prints a newline to the standard
 * output.
 *
 * Params:
 *      args = the items to write to `stdout`
 *
 * Throws:
 *      In case of an I/O error, throws an $(LREF StdioException).
 * Example:
 *        Reads `stdin` and writes it to `stdout` with an argument
 *        counter.
---
import std.stdio;

void main()
{
    string line;

    for (size_t count = 0; (line = readln) !is null; count++)
    {
         writeln("Input ", count, ": ", line);
    }
}
---
 */
void writeln(T...)(T args)
{
    static if (T.length == 0)
    {
        import std.exception : enforce;

        enforce(fputc('\n', .trustedStdout._p.handle) != EOF, "fputc failed");
    }
    else static if (T.length == 1 &&
                    is(T[0] : const(char)[]) &&
                    (is(T[0] == U[], U) || __traits(isStaticArray, T[0])))
    {
        // Specialization for strings - a very frequent case
        auto w = .trustedStdout.lockingTextWriter();

        static if (__traits(isStaticArray, T[0]))
        {
            w.put(args[0][]);
        }
        else
        {
            w.put(args[0]);
        }
        w.put('\n');
    }
    else
    {
        // Most general instance
        trustedStdout.write(args, '\n');
    }
}

@safe unittest
{
    // Just make sure the call compiles
    if (false) writeln();

    if (false) writeln("wyda");

    // https://issues.dlang.org/show_bug.cgi?id=8040
    if (false) writeln(null);
    if (false) writeln(">", null, "<");

    // https://issues.dlang.org/show_bug.cgi?id=14041
    if (false)
    {
        char[8] a;
        writeln(a);
        immutable b = a;
        b.writeln;
        const c = a[];
        c.writeln;
    }
}

@system unittest
{
    static import std.file;

    scope(failure) printf("Failed test at line %d\n", __LINE__);

    // test writeln
    auto deleteme = testFilename();
    auto f = File(deleteme, "w");
    scope(exit) { std.file.remove(deleteme); }
    f.writeln("Hello, ",  "world number ", 42, "!");
    f.close();
    version (Windows)
        assert(cast(char[]) std.file.read(deleteme) ==
                "Hello, world number 42!\r\n");
    else
        assert(cast(char[]) std.file.read(deleteme) ==
                "Hello, world number 42!\n");

    // test writeln on stdout
    auto saveStdout = stdout;
    scope(exit) stdout = saveStdout;
    stdout.open(deleteme, "w");
    writeln("Hello, ",  "world number ", 42, "!");
    stdout.close();
    version (Windows)
        assert(cast(char[]) std.file.read(deleteme) ==
                "Hello, world number 42!\r\n");
    else
        assert(cast(char[]) std.file.read(deleteme) ==
                "Hello, world number 42!\n");

    stdout.open(deleteme, "w");
    writeln("Hello!"c);
    writeln("Hello!"w);    // https://issues.dlang.org/show_bug.cgi?id=8386
    writeln("Hello!"d);    // https://issues.dlang.org/show_bug.cgi?id=8386
    writeln("embedded\0null"c); // https://issues.dlang.org/show_bug.cgi?id=8730
    stdout.close();
    version (Windows)
        assert(cast(char[]) std.file.read(deleteme) ==
            "Hello!\r\nHello!\r\nHello!\r\nembedded\0null\r\n");
    else
        assert(cast(char[]) std.file.read(deleteme) ==
            "Hello!\nHello!\nHello!\nembedded\0null\n");
}

@system unittest
{
    static import std.file;

    auto deleteme = testFilename();
    auto f = File(deleteme, "w");
    scope(exit) { std.file.remove(deleteme); }

    enum EI : int    { A, B }
    enum ED : double { A = 0, B } // NOTE: explicit initialization to 0 required during Enum init deprecation cycle
    enum EC : char   { A = 0, B } // NOTE: explicit initialization to 0 required during Enum init deprecation cycle
    enum ES : string { A = "aaa", B = "bbb" }

    f.writeln(EI.A);  // false, but A on 2.058
    f.writeln(EI.B);  // true, but B on 2.058

    f.writeln(ED.A);  // A
    f.writeln(ED.B);  // B

    f.writeln(EC.A);  // A
    f.writeln(EC.B);  // B

    f.writeln(ES.A);  // A
    f.writeln(ES.B);  // B

    f.close();
    version (Windows)
        assert(cast(char[]) std.file.read(deleteme) ==
                "A\r\nB\r\nA\r\nB\r\nA\r\nB\r\nA\r\nB\r\n");
    else
        assert(cast(char[]) std.file.read(deleteme) ==
                "A\nB\nA\nB\nA\nB\nA\nB\n");
}

@system unittest
{
    static auto useInit(T)(T ltw)
    {
        T val;
        val = ltw;
        val = T.init;
        return val;
    }
    useInit(stdout.lockingTextWriter());
}

@system unittest
{
    // https://issues.dlang.org/show_bug.cgi?id=21920
    void function(string) printer = &writeln!string;
    if (false) printer("Hello");
}


/***********************************
Writes formatted data to standard output (without a trailing newline).

Params:
fmt = The $(REF_ALTTEXT format string, formattedWrite, std, _format).
When passed as a compile-time argument, the string will be statically checked
against the argument types passed.
args = Items to write.

Note: In older versions of Phobos, it used to be possible to write:

------
writef(stderr, "%s", "message");
------

to print a message to `stderr`. This syntax is no longer supported, and has
been superceded by:

------
stderr.writef("%s", "message");
------

*/
void writef(alias fmt, A...)(A args)
if (isSomeString!(typeof(fmt)))
{
    import std.format : checkFormatException;

    alias e = checkFormatException!(fmt, A);
    static assert(!e, e);
    return .writef(fmt, args);
}

/// ditto
void writef(Char, A...)(in Char[] fmt, A args)
{
    trustedStdout.writef(fmt, args);
}

@system unittest
{
    static import std.file;

    scope(failure) printf("Failed test at line %d\n", __LINE__);

    // test writef
    auto deleteme = testFilename();
    auto f = File(deleteme, "w");
    scope(exit) { std.file.remove(deleteme); }
    f.writef!"Hello, %s world number %s!"("nice", 42);
    f.close();
    assert(cast(char[]) std.file.read(deleteme) ==  "Hello, nice world number 42!");
    // test write on stdout
    auto saveStdout = stdout;
    scope(exit) stdout = saveStdout;
    stdout.open(deleteme, "w");
    writef!"Hello, %s world number %s!"("nice", 42);
    stdout.close();
    assert(cast(char[]) std.file.read(deleteme) == "Hello, nice world number 42!");
}

/***********************************
 * Equivalent to $(D writef(fmt, args, '\n')).
 */
void writefln(alias fmt, A...)(A args)
if (isSomeString!(typeof(fmt)))
{
    import std.format : checkFormatException;

    alias e = checkFormatException!(fmt, A);
    static assert(!e, e);
    return .writefln(fmt, args);
}

/// ditto
void writefln(Char, A...)(in Char[] fmt, A args)
{
    trustedStdout.writefln(fmt, args);
}

@system unittest
{
    static import std.file;

    scope(failure) printf("Failed test at line %d\n", __LINE__);

    // test File.writefln
    auto deleteme = testFilename();
    auto f = File(deleteme, "w");
    scope(exit) { std.file.remove(deleteme); }
    f.writefln!"Hello, %s world number %s!"("nice", 42);
    f.close();
    version (Windows)
        assert(cast(char[]) std.file.read(deleteme) ==
                "Hello, nice world number 42!\r\n");
    else
        assert(cast(char[]) std.file.read(deleteme) ==
                "Hello, nice world number 42!\n",
                cast(char[]) std.file.read(deleteme));

    // test writefln
    auto saveStdout = stdout;
    scope(exit) stdout = saveStdout;
    stdout.open(deleteme, "w");
    writefln!"Hello, %s world number %s!"("nice", 42);
    stdout.close();
    version (Windows)
        assert(cast(char[]) std.file.read(deleteme) ==
                "Hello, nice world number 42!\r\n");
    else
        assert(cast(char[]) std.file.read(deleteme) ==
                "Hello, nice world number 42!\n");
}

/**
 * Reads formatted data from `stdin` using $(REF formattedRead, std,_format).
 * Params:
 * format = The $(REF_ALTTEXT format string, formattedWrite, std, _format).
 * When passed as a compile-time argument, the string will be statically checked
 * against the argument types passed.
 * args = Items to be read.
 * Returns:
 *      Same as `formattedRead`: The number of variables filled. If the input range `r` ends early,
 *      this number will be less than the number of variables provided.
 * Example:
----
// test.d
void main()
{
    import std.stdio;
    foreach (_; 0 .. 3)
    {
        int a;
        readf!" %d"(a);
        writeln(++a);
    }
}
----
$(CONSOLE
% echo "1 2 3" | rdmd test.d
2
3
4
)
 */
uint readf(alias format, A...)(auto ref A args)
if (isSomeString!(typeof(format)))
{
    import std.format : checkFormatException;

    alias e = checkFormatException!(format, A);
    static assert(!e, e);
    return .readf(format, args);
}

/// ditto
uint readf(A...)(scope const(char)[] format, auto ref A args)
{
    return stdin.readf(format, args);
}

@system unittest
{
    float f;
    if (false) readf("%s", &f);

    char a;
    wchar b;
    dchar c;
    if (false) readf("%s %s %s", a, b, c);
    // backwards compatibility with pointers
    if (false) readf("%s %s %s", a, &b, c);
    if (false) readf("%s %s %s", &a, &b, &c);
}

/**********************************
 * Read line from `stdin`.
 *
 * This version manages its own read buffer, which means one memory allocation per call. If you are not
 * retaining a reference to the read data, consider the `readln(buf)` version, which may offer
 * better performance as it can reuse its read buffer.
 *
 * Returns:
 *        The line that was read, including the line terminator character.
 * Params:
 *        S = Template parameter; the type of the allocated buffer, and the type returned. Defaults to `string`.
 *        terminator = Line terminator (by default, `'\n'`).
 * Note:
 *        String terminators are not supported due to ambiguity with readln(buf) below.
 * Throws:
 *        `StdioException` on I/O error, or `UnicodeException` on Unicode conversion error.
 * Example:
 *        Reads `stdin` and writes it to `stdout`.
---
import std.stdio;

void main()
{
    string line;
    while ((line = readln()) !is null)
        write(line);
}
---
*/
S readln(S = string)(dchar terminator = '\n')
if (isSomeString!S)
{
    return stdin.readln!S(terminator);
}

/**********************************
 * Read line from `stdin` and write it to buf[], including terminating character.
 *
 * This can be faster than $(D line = readln()) because you can reuse
 * the buffer for each call. Note that reusing the buffer means that you
 * must copy the previous contents if you wish to retain them.
 *
 * Returns:
 *        `size_t` 0 for end of file, otherwise number of characters read
 * Params:
 *        buf = Buffer used to store the resulting line data. buf is resized as necessary.
 *        terminator = Line terminator (by default, `'\n'`). Use $(REF newline, std,ascii)
 *        for portability (unless the file was opened in text mode).
 * Throws:
 *        `StdioException` on I/O error, or `UnicodeException` on Unicode conversion error.
 * Example:
 *        Reads `stdin` and writes it to `stdout`.
---
import std.stdio;

void main()
{
    char[] buf;
    while (readln(buf))
        write(buf);
}
---
*/
size_t readln(C)(ref C[] buf, dchar terminator = '\n')
if (isSomeChar!C && is(Unqual!C == C) && !is(C == enum))
{
    return stdin.readln(buf, terminator);
}

/** ditto */
size_t readln(C, R)(ref C[] buf, R terminator)
if (isSomeChar!C && is(Unqual!C == C) && !is(C == enum) &&
    isBidirectionalRange!R && is(typeof(terminator.front == dchar.init)))
{
    return stdin.readln(buf, terminator);
}

@safe unittest
{
    import std.meta : AliasSeq;

    //we can't actually test readln, so at the very least,
    //we test compilability
    void foo()
    {
        readln();
        readln('\t');
        static foreach (String; AliasSeq!(string, char[], wstring, wchar[], dstring, dchar[]))
        {
            readln!String();
            readln!String('\t');
        }
        static foreach (String; AliasSeq!(char[], wchar[], dchar[]))
        {{
            String buf;
            readln(buf);
            readln(buf, '\t');
            readln(buf, "<br />");
        }}
    }
}

/*
 * Convenience function that forwards to `core.sys.posix.stdio.fopen`
 * (to `_wfopen` on Windows)
 * with appropriately-constructed C-style strings.
 */
private FILE* _fopen(R1, R2)(R1 name, R2 mode = "r")
if ((isSomeFiniteCharInputRange!R1 || isSomeString!R1) &&
    (isSomeFiniteCharInputRange!R2 || isSomeString!R2))
{
    import std.internal.cstring : tempCString;

    auto namez = name.tempCString!FSChar();
    auto modez = mode.tempCString!FSChar();

    static _fopenImpl(scope const(FSChar)* namez, scope const(FSChar)* modez) @trusted nothrow @nogc
    {
        version (Windows)
        {
            return _wfopen(namez, modez);
        }
        else version (Posix)
        {
            /*
             * The new opengroup large file support API is transparently
             * included in the normal C bindings. https://www.opengroup.org/platform/lfs.html#1.0
             * if _FILE_OFFSET_BITS in druntime is 64, off_t is 64 bit and
             * the normal functions work fine. If not, then large file support
             * probably isn't available. Do not use the old transitional API
             * (the native extern(C) fopen64, https://unix.org/version2/whatsnew/lfs20mar.html#3.0)
             */
            import core.sys.posix.stdio : fopen;
            return fopen(namez, modez);
        }
        else
        {
            return fopen(namez, modez);
        }
    }
    return _fopenImpl(namez, modez);
}

version (Posix)
{
    /***********************************
     * Convenience function that forwards to `core.sys.posix.stdio.popen`
     * with appropriately-constructed C-style strings.
     */
    FILE* _popen(R1, R2)(R1 name, R2 mode = "r") @trusted nothrow @nogc
    if ((isSomeFiniteCharInputRange!R1 || isSomeString!R1) &&
        (isSomeFiniteCharInputRange!R2 || isSomeString!R2))
    {
        import std.internal.cstring : tempCString;

        auto namez = name.tempCString!FSChar();
        auto modez = mode.tempCString!FSChar();

        static popenImpl(const(FSChar)* namez, const(FSChar)* modez) @trusted nothrow @nogc
        {
            import core.sys.posix.stdio : popen;
            return popen(namez, modez);
        }
        return popenImpl(namez, modez);
    }
}

/*
 * Convenience function that forwards to `core.stdc.stdio.fwrite`
 */
private auto trustedFwrite(T)(FILE* f, const T[] obj) @trusted
{
    return fwrite(obj.ptr, T.sizeof, obj.length, f);
}

/*
 * Convenience function that forwards to `core.stdc.stdio.fread`
 */
private auto trustedFread(T)(FILE* f, T[] obj) @trusted
if (!imported!"std.traits".hasIndirections!T)
{
    return fread(obj.ptr, T.sizeof, obj.length, f);
}

private auto trustedFread(T)(FILE* f, T[] obj) @system
if (imported!"std.traits".hasIndirections!T)
{
    return fread(obj.ptr, T.sizeof, obj.length, f);
}

/**
 * Iterates through the lines of a file by using `foreach`.
 *
 * Example:
 *
---------
void main()
{
  foreach (string line; lines(stdin))
  {
    ... use line ...
  }
}
---------
The line terminator (`'\n'` by default) is part of the string read (it
could be missing in the last line of the file). Several types are
supported for `line`, and the behavior of `lines`
changes accordingly:

$(OL $(LI If `line` has type `string`, $(D
wstring), or `dstring`, a new string of the respective type
is allocated every read.) $(LI If `line` has type $(D
char[]), `wchar[]`, `dchar[]`, the line's content
will be reused (overwritten) across reads.) $(LI If `line`
has type `immutable(ubyte)[]`, the behavior is similar to
case (1), except that no UTF checking is attempted upon input.) $(LI
If `line` has type `ubyte[]`, the behavior is
similar to case (2), except that no UTF checking is attempted upon
input.))

In all cases, a two-symbols versions is also accepted, in which case
the first symbol (of integral type, e.g. `ulong` or $(D
uint)) tracks the zero-based number of the current line.

Example:
----
  foreach (ulong i, string line; lines(stdin))
  {
    ... use line ...
  }
----

 In case of an I/O error, an `StdioException` is thrown.

See_Also:
$(LREF byLine)
 */

struct lines
{
    private File f;
    private dchar terminator = '\n';

    /**
    Constructor.
    Params:
    f = File to read lines from.
    terminator = Line separator (`'\n'` by default).
    */
    this(File f, dchar terminator = '\n') @safe
    {
        this.f = f;
        this.terminator = terminator;
    }

    int opApply(D)(scope D dg)
    {
        import std.traits : Parameters;
        alias Parms = Parameters!(dg);
        static if (isSomeString!(Parms[$ - 1]))
        {
            int result = 0;
            static if (is(Parms[$ - 1] : const(char)[]))
                alias C = char;
            else static if (is(Parms[$ - 1] : const(wchar)[]))
                alias C = wchar;
            else static if (is(Parms[$ - 1] : const(dchar)[]))
                alias C = dchar;
            C[] line;
            static if (Parms.length == 2)
                Parms[0] i = 0;
            for (;;)
            {
                import std.conv : to;

                if (!f.readln(line, terminator)) break;
                auto copy = to!(Parms[$ - 1])(line);
                static if (Parms.length == 2)
                {
                    result = dg(i, copy);
                    ++i;
                }
                else
                {
                    result = dg(copy);
                }
                if (result != 0) break;
            }
            return result;
        }
        else
        {
            // raw read
            return opApplyRaw(dg);
        }
    }
    // no UTF checking
    int opApplyRaw(D)(scope D dg)
    {
        import std.conv : to;
        import std.exception : assumeUnique;
        import std.traits : Parameters;

        alias Parms = Parameters!(dg);
        enum duplicate = is(Parms[$ - 1] : immutable(ubyte)[]);
        int result = 1;
        int c = void;
        _FLOCK(f._p.handle);
        scope(exit) _FUNLOCK(f._p.handle);
        ubyte[] buffer;
        static if (Parms.length == 2)
            Parms[0] line = 0;
        while ((c = _FGETC(cast(_iobuf*) f._p.handle)) != -1)
        {
            buffer ~= to!(ubyte)(c);
            if (c == terminator)
            {
                static if (duplicate)
                    auto arg = assumeUnique(buffer);
                else
                    alias arg = buffer;
                // unlock the file while calling the delegate
                _FUNLOCK(f._p.handle);
                scope(exit) _FLOCK(f._p.handle);
                static if (Parms.length == 1)
                {
                    result = dg(arg);
                }
                else
                {
                    result = dg(line, arg);
                    ++line;
                }
                if (result) break;
                static if (!duplicate)
                    buffer.length = 0;
            }
        }
        // can only reach when _FGETC returned -1
        if (!f.eof) throw new StdioException("Error in reading file"); // error occured
        return result;
    }
}

@safe unittest
{
    /*
        As pointed out in <https://github.com/dlang/phobos/issues/10605>,
        it's a pity that `byLine()` & co. aren't @safe to use yet.

        This is a first attempt at working towards that goal.
        For now, this test doesn't do much; as there isn't much to do safely yet.
     */
    auto deleteMe = testFilename();
    scope(exit) { imported!"std.file".remove(deleteMe); }

    // Setup
    {
        auto f = File(deleteMe, "w");
        scope(exit) { f.close(); }
        foreach (i; 1 .. 11)
            f.writeln(i);
    }

    // Actual tests
    {
        auto f = File(deleteMe, "r");
        scope(exit) { f.close(); }

        auto myLines = lines(f);
        foreach (string line; myLines)
            continue;
    }


    {
        auto f = File(deleteMe, "r");
        scope(exit) { f.close(); }

        auto myByLineCopy = f.byLineCopy;
        foreach (line; myByLineCopy)
            continue;
    }
}

@system unittest
{
    static import std.file;
    import std.meta : AliasSeq;

    scope(failure) printf("Failed test at line %d\n", __LINE__);

    auto deleteme = testFilename();
    scope(exit) { std.file.remove(deleteme); }

    alias TestedWith =
          AliasSeq!(string, wstring, dstring,
                    char[], wchar[], dchar[]);
    foreach (T; TestedWith)
    {
        // test looping with an empty file
        std.file.write(deleteme, "");
        auto f = File(deleteme, "r");
        foreach (T line; lines(f))
        {
            assert(false);
        }
        f.close();

        // test looping with a file with three lines
        std.file.write(deleteme, "Line one\nline two\nline three\n");
        f.open(deleteme, "r");
        uint i = 0;
        foreach (T line; lines(f))
        {
            if (i == 0) assert(line == "Line one\n");
            else if (i == 1) assert(line == "line two\n");
            else if (i == 2) assert(line == "line three\n");
            else assert(false);
            ++i;
        }
        f.close();

        // test looping with a file with three lines, last without a newline
        std.file.write(deleteme, "Line one\nline two\nline three");
        f.open(deleteme, "r");
        i = 0;
        foreach (T line; lines(f))
        {
            if (i == 0) assert(line == "Line one\n");
            else if (i == 1) assert(line == "line two\n");
            else if (i == 2) assert(line == "line three");
            else assert(false);
            ++i;
        }
        f.close();
    }

    // test with ubyte[] inputs
    alias TestedWith2 = AliasSeq!(immutable(ubyte)[], ubyte[]);
    foreach (T; TestedWith2)
    {
        // test looping with an empty file
        std.file.write(deleteme, "");
        auto f = File(deleteme, "r");
        foreach (T line; lines(f))
        {
            assert(false);
        }
        f.close();

        // test looping with a file with three lines
        std.file.write(deleteme, "Line one\nline two\nline three\n");
        f.open(deleteme, "r");
        uint i = 0;
        foreach (T line; lines(f))
        {
            if (i == 0) assert(cast(char[]) line == "Line one\n");
            else if (i == 1) assert(cast(char[]) line == "line two\n",
                T.stringof ~ " " ~ cast(char[]) line);
            else if (i == 2) assert(cast(char[]) line == "line three\n");
            else assert(false);
            ++i;
        }
        f.close();

        // test looping with a file with three lines, last without a newline
        std.file.write(deleteme, "Line one\nline two\nline three");
        f.open(deleteme, "r");
        i = 0;
        foreach (T line; lines(f))
        {
            if (i == 0) assert(cast(char[]) line == "Line one\n");
            else if (i == 1) assert(cast(char[]) line == "line two\n");
            else if (i == 2) assert(cast(char[]) line == "line three");
            else assert(false);
            ++i;
        }
        f.close();

    }

    static foreach (T; AliasSeq!(ubyte[]))
    {
        // test looping with a file with three lines, last without a newline
        // using a counter too this time
        std.file.write(deleteme, "Line one\nline two\nline three");
        auto f = File(deleteme, "r");
        uint i = 0;
        foreach (ulong j, T line; lines(f))
        {
            if (i == 0) assert(cast(char[]) line == "Line one\n");
            else if (i == 1) assert(cast(char[]) line == "line two\n");
            else if (i == 2) assert(cast(char[]) line == "line three");
            else assert(false);
            ++i;
        }
        f.close();
    }
}

/**
Iterates through a file a chunk at a time by using `foreach`.

Example:

---------
void main()
{
    foreach (ubyte[] buffer; chunks(stdin, 4096))
    {
        ... use buffer ...
    }
}
---------

The content of `buffer` is reused across calls. In the
 example above, `buffer.length` is 4096 for all iterations,
 except for the last one, in which case `buffer.length` may
 be less than 4096 (but always greater than zero).

 In case of an I/O error, an `StdioException` is thrown.
*/
auto chunks(File f, size_t size)
{
    return ChunksImpl(f, size);
}
private struct ChunksImpl
{
    private File f;
    private size_t size;
    // private string fileName; // Currently, no use

    this(File f, size_t size)
    in
    {
        assert(size, "size must be larger than 0");
    }
    do
    {
        this.f = f;
        this.size = size;
    }

    int opApply(D)(scope D dg)
    {
        import core.stdc.stdlib : alloca;
        import std.exception : enforce;

        enforce(f.isOpen, "Attempting to read from an unopened file");
        enum maxStackSize = 1024 * 16;
        ubyte[] buffer = void;
        if (size < maxStackSize)
            buffer = (cast(ubyte*) alloca(size))[0 .. size];
        else
            buffer = new ubyte[size];
        size_t r = void;
        int result = 1;
        uint tally = 0;
        while ((r = trustedFread(f._p.handle, buffer)) > 0)
        {
            assert(r <= size);
            if (r != size)
            {
                // error occured
                if (!f.eof) throw new StdioException(null);
                buffer.length = r;
            }
            static if (is(typeof(dg(tally, buffer))))
            {
                if ((result = dg(tally, buffer)) != 0) break;
            }
            else
            {
                if ((result = dg(buffer)) != 0) break;
            }
            ++tally;
        }
        return result;
    }
}

@system unittest
{
    static import std.file;

    scope(failure) printf("Failed test at line %d\n", __LINE__);

    auto deleteme = testFilename();
    scope(exit) { std.file.remove(deleteme); }

    // test looping with an empty file
    std.file.write(deleteme, "");
    auto f = File(deleteme, "r");
    foreach (ubyte[] line; chunks(f, 4))
    {
        assert(false);
    }
    f.close();

    // test looping with a file with three lines
    std.file.write(deleteme, "Line one\nline two\nline three\n");
    f = File(deleteme, "r");
    uint i = 0;
    foreach (ubyte[] line; chunks(f, 3))
    {
        if (i == 0) assert(cast(char[]) line == "Lin");
        else if (i == 1) assert(cast(char[]) line == "e o");
        else if (i == 2) assert(cast(char[]) line == "ne\n");
        else break;
        ++i;
    }
    f.close();
}

// Issue 21730 - null ptr dereferenced in ChunksImpl.opApply (SIGSEGV)
@system unittest
{
    import std.exception : assertThrown;
    static import std.file;

    auto deleteme = testFilename();
    scope(exit) { if (std.file.exists(deleteme)) std.file.remove(deleteme); }

    auto err1 = File(deleteme, "w+x");
    err1.close;
    std.file.remove(deleteme);
    assertThrown(() {foreach (ubyte[] buf; chunks(err1, 4096)) {}}());
}

/**
Writes an array or range to a file.
Shorthand for $(D data.copy(File(fileName, "wb").lockingBinaryWriter)).
Similar to $(REF write, std,file), strings are written as-is,
rather than encoded according to the `File`'s $(HTTP
en.cppreference.com/w/c/io#Narrow_and_wide_orientation,
orientation).
*/
void toFile(T)(T data, string fileName)
if (is(typeof(copy(data, stdout.lockingBinaryWriter))))
{
    copy(data, File(fileName, "wb").lockingBinaryWriter);
}

@system unittest
{
    static import std.file;

    auto deleteme = testFilename();
    scope(exit) { std.file.remove(deleteme); }

    "Test".toFile(deleteme);
    assert(std.file.readText(deleteme) == "Test");
}

/*********************
 * Thrown if I/O errors happen.
 */
class StdioException : Exception
{
    static import core.stdc.errno;
    /// Operating system error code.
    uint errno;

/**
Initialize with a message and an error code.
*/
    this(string message, uint e = core.stdc.errno.errno) @trusted
    {
        import std.exception : errnoString;
        errno = e;
        auto sysmsg = errnoString(errno);
        // If e is 0, we don't use the system error message.  (The message
        // is "Success", which is rather pointless for an exception.)
        super(e == 0 ? message
                     : (message ? message ~ " (" ~ sysmsg ~ ")" : sysmsg));
    }

/** Convenience functions that throw an `StdioException`. */
    static void opCall(string msg) @safe
    {
        throw new StdioException(msg);
    }

/// ditto
    static void opCall() @safe
    {
        throw new StdioException(null, core.stdc.errno.errno);
    }
}

enum StdFileHandle: string
{
    stdin  = "core.stdc.stdio.stdin",
    stdout = "core.stdc.stdio.stdout",
    stderr = "core.stdc.stdio.stderr",
}

// Undocumented but public because the std* handles are aliasing it.
@property ref File makeGlobal(StdFileHandle _iob)()
{
    __gshared File.Impl impl;
    __gshared File result;

    // Use an inline spinlock to make sure the initializer is only run once.
    // We assume there will be at most uint.max / 2 threads trying to initialize
    // `handle` at once and steal the high bit to indicate that the globals have
    // been initialized.
    static shared uint spinlock;
    import core.atomic : atomicLoad, atomicOp, MemoryOrder;
    if (atomicLoad!(MemoryOrder.acq)(spinlock) <= uint.max / 2)
    {
        for (;;)
        {
            if (atomicLoad!(MemoryOrder.acq)(spinlock) > uint.max / 2)
                break;
            if (atomicOp!"+="(spinlock, 1) == 1)
            {
                with (StdFileHandle)
                    assert(_iob == stdin || _iob == stdout || _iob == stderr);
                impl.handle = cast() mixin(_iob);
                result._p = &impl;
                atomicOp!"+="(spinlock, uint.max / 2);
                break;
            }
            atomicOp!"-="(spinlock, 1);
        }
    }
    return result;
}

/** The standard input stream.

    Returns:
        stdin as a $(LREF File).

    Note:
        The returned $(LREF File) wraps $(REF stdin,core,stdc,stdio), and
        is therefore thread global. Reassigning `stdin` to a different
        `File` must be done in a single-threaded or locked context in
        order to avoid race conditions.

        All reading from `stdin` automatically locks the file globally,
        and will cause all other threads calling `read` to wait until
        the lock is released.
*/
alias stdin = makeGlobal!(StdFileHandle.stdin);

///
@safe unittest
{
    // Read stdin, sort lines, write to stdout
    import std.algorithm.mutation : copy;
    import std.algorithm.sorting : sort;
    import std.array : array;
    import std.typecons : Yes;

    void main()
    {
        stdin                       // read from stdin
        .byLineCopy(Yes.keepTerminator) // copying each line
        .array()                    // convert to array of lines
        .sort()                     // sort the lines
        .copy(                      // copy output of .sort to an OutputRange
            stdout.lockingTextWriter()); // the OutputRange
    }
}

/**
    The standard output stream.

    Returns:
        stdout as a $(LREF File).

    Note:
        The returned $(LREF File) wraps $(REF stdout,core,stdc,stdio), and
        is therefore thread global. Reassigning `stdout` to a different
        `File` must be done in a single-threaded or locked context in
        order to avoid race conditions.

        All writing to `stdout` automatically locks the file globally,
        and will cause all other threads calling `write` to wait until
        the lock is released.
*/
alias stdout = makeGlobal!(StdFileHandle.stdout);

///
@safe unittest
{
    void main()
    {
        stdout.writeln("Write a message to stdout.");
    }
}

///
@safe unittest
{
    void main()
    {
        import std.algorithm.iteration : filter, map, sum;
        import std.format : format;
        import std.range : iota, tee;

        int len;
        const r = 6.iota
                  .filter!(a => a % 2) // 1 3 5
                  .map!(a => a * 2) // 2 6 10
                  .tee!(_ => stdout.writefln("len: %d", len++))
                  .sum;

        assert(r == 18);
    }
}

///
@safe unittest
{
    void main()
    {
        import std.algorithm.mutation : copy;
        import std.algorithm.iteration : map;
        import std.format : format;
        import std.range : iota;

        10.iota
        .map!(e => "N: %d".format(e))
        .copy(stdout.lockingTextWriter()); // the OutputRange
    }
}

/**
    The standard error stream.

    Returns:
        stderr as a $(LREF File).

    Note:
        The returned $(LREF File) wraps $(REF stderr,core,stdc,stdio), and
        is therefore thread global. Reassigning `stderr` to a different
        `File` must be done in a single-threaded or locked context in
        order to avoid race conditions.

        All writing to `stderr` automatically locks the file globally,
        and will cause all other threads calling `write` to wait until
        the lock is released.
*/
alias stderr = makeGlobal!(StdFileHandle.stderr);

///
@safe unittest
{
    void main()
    {
        stderr.writeln("Write a message to stderr.");
    }
}

@system unittest
{
    static import std.file;
    import std.typecons : tuple;

    scope(failure) printf("Failed test at line %d\n", __LINE__);
    auto deleteme = testFilename();

    std.file.write(deleteme, "1 2\n4 1\n5 100");
    scope(exit) std.file.remove(deleteme);
    {
        File f = File(deleteme);
        scope(exit) f.close();
        auto t = [ tuple(1, 2), tuple(4, 1), tuple(5, 100) ];
        uint i;
        foreach (e; f.byRecord!(int, int)("%s %s"))
        {
            //writeln(e);
            assert(e == t[i++]);
        }
        assert(i == 3);
    }
}

@safe unittest
{
    // Retain backwards compatibility
    // https://issues.dlang.org/show_bug.cgi?id=17472
    static assert(is(typeof(stdin) == File));
    static assert(is(typeof(stdout) == File));
    static assert(is(typeof(stderr) == File));
}

// roll our own appender, but with "safe" arrays
private struct ReadlnAppender
{
    char[] buf;
    size_t pos;
    bool safeAppend = false;

    void initialize(char[] b) @safe
    {
        buf = b;
        pos = 0;
    }
    @property char[] data() @trusted
    {
        if (safeAppend)
            assumeSafeAppend(buf.ptr[0 .. pos]);
        return buf.ptr[0 .. pos];
    }

    bool reserveWithoutAllocating(size_t n)
    {
        if (buf.length >= pos + n) // buf is already large enough
            return true;

        immutable curCap = buf.capacity;
        if (curCap >= pos + n)
        {
            buf.length = curCap;
            /* Any extra capacity we end up not using can safely be claimed
            by someone else. */
            safeAppend = true;
            return true;
        }

        return false;
    }
    void reserve(size_t n) @trusted
    {
        import core.stdc.string : memcpy;
        if (!reserveWithoutAllocating(n))
        {
            size_t ncap = buf.length * 2 + 128 + n;
            char[] nbuf = new char[ncap];
            memcpy(nbuf.ptr, buf.ptr, pos);
            buf = nbuf;
            // Allocated a new buffer. No one else knows about it.
            safeAppend = true;
        }
    }
    void putchar(char c) @trusted
    {
        reserve(1);
        buf.ptr[pos++] = c;
    }
    void putdchar(dchar dc) @trusted
    {
        import std.utf : encode, UseReplacementDchar;

        char[4] ubuf;
        immutable size = encode!(UseReplacementDchar.yes)(ubuf, dc);
        reserve(size);
        foreach (c; ubuf)
            buf.ptr[pos++] = c;
    }
    void putonly(const char[] b) @trusted
    {
        import core.stdc.string : memcpy;
        assert(pos == 0);   // assume this is the only put call
        if (reserveWithoutAllocating(b.length))
            memcpy(buf.ptr + pos, b.ptr, b.length);
        else
            buf = b.dup;
        pos = b.length;
    }
}

private struct LockedFile
{
    private @system _iobuf* fp;

    this(FILE* fps) @trusted
    {
        _FLOCK(fps);
        // Since fps is now locked, we can cast away shared
        fp = cast(_iobuf*) fps;
    }

    @disable this();
    @disable this(this);
    @disable void opAssign(LockedFile);

    // these use unlocked fgetc calls
    @trusted fgetc() { return _FGETC(fp); }
    @trusted fgetwc() { return _FGETWC(fp); }

    ~this() @trusted
    {
        _FUNLOCK(cast(FILE*) fp);
    }
}

@safe unittest
{
    void f() @safe
    {
        FILE* fps;
        auto lf = LockedFile(fps);
        static assert(!__traits(compiles, lf = LockedFile(fps)));
        version (ShouldFail)
        {
            lf.fps = null; // error with -preview=systemVariables
        }
    }
}

// Private implementation of readln
private size_t readlnImpl(FILE* fps, ref char[] buf, dchar terminator, File.Orientation orientation) @safe
{
    version (CRuntime_Microsoft)
    {
        auto lf = LockedFile(fps);

        ReadlnAppender app;
        app.initialize(buf);

        int c;
        while ((c = lf.fgetc()) != -1)
        {
            app.putchar(cast(char) c);
            if (c == terminator)
            {
                buf = app.data;
                return buf.length;
            }

        }

        if (ferror(fps))
            StdioException();
        buf = app.data;
        return buf.length;
    }
    else static if (__traits(compiles, core.sys.posix.stdio.getdelim))
    {
        if (orientation == File.Orientation.wide)
        {
            import core.stdc.wchar_ : fwide;

            auto lf = LockedFile(fps);
            /* Stream is in wide characters.
             * Read them and convert to chars.
             */
            version (Windows)
            {
                buf.length = 0;
                for (int c = void; (c = lf.fgetwc()) != -1; )
                {
                    if ((c & ~0x7F) == 0)
                    {   buf ~= c;
                        if (c == terminator)
                            break;
                    }
                    else
                    {
                        if (c >= 0xD800 && c <= 0xDBFF)
                        {
                            int c2 = void;
                            if ((c2 = lf.fgetwc()) != -1 ||
                                    c2 < 0xDC00 && c2 > 0xDFFF)
                            {
                                StdioException("unpaired UTF-16 surrogate");
                            }
                            c = ((c - 0xD7C0) << 10) + (c2 - 0xDC00);
                        }
                        import std.utf : encode;
                        encode(buf, c);
                    }
                }
                if (ferror(fps))
                    StdioException();
                return buf.length;
            }
            else version (Posix)
            {
                buf.length = 0;
                for (int c; (c = lf.fgetwc()) != -1; )
                {
                    import std.utf : encode;

                    if ((c & ~0x7F) == 0)
                        buf ~= cast(char) c;
                    else
                        encode(buf, cast(dchar) c);
                    if (c == terminator)
                        break;
                }
                if (ferror(fps))
                    StdioException();
                return buf.length;
            }
            else
            {
                static assert(0);
            }
        }
        return () @trusted {
            import core.stdc.stdlib : free;

            static char *lineptr = null;
            static size_t n = 0;
            scope(exit)
            {
                if (n > 128 * 1024)
                {
                    // Bound memory used by readln
                    free(lineptr);
                    lineptr = null;
                    n = 0;
                }
            }

            const s = core.sys.posix.stdio.getdelim(&lineptr, &n, terminator, fps);
            if (s < 0)
            {
                if (ferror(fps))
                    StdioException();
                buf.length = 0;                // end of file
                return 0;
            }

            const line = lineptr[0 .. s];
            if (s <= buf.length)
            {
                buf = buf[0 .. s];
                buf[] = line;
            }
            else
            {
                buf = line.dup;
            }
            return s;
        }();
    }
    else // version (NO_GETDELIM)
    {
        import core.stdc.wchar_ : fwide;

        auto lf = LockedFile(fps);
        if (orientation == File.Orientation.wide)
        {
            /* Stream is in wide characters.
             * Read them and convert to chars.
             */
            version (Windows)
            {
                buf.length = 0;
                for (int c; (c = lf.fgetwc()) != -1; )
                {
                    if ((c & ~0x7F) == 0)
                    {   buf ~= c;
                        if (c == terminator)
                            break;
                    }
                    else
                    {
                        if (c >= 0xD800 && c <= 0xDBFF)
                        {
                            int c2 = void;
                            if ((c2 = lf.fgetwc()) != -1 ||
                                    c2 < 0xDC00 && c2 > 0xDFFF)
                            {
                                StdioException("unpaired UTF-16 surrogate");
                            }
                            c = ((c - 0xD7C0) << 10) + (c2 - 0xDC00);
                        }
                        import std.utf : encode;
                        encode(buf, c);
                    }
                }
                if (ferror(fps))
                    StdioException();
                return buf.length;
            }
            else version (Posix)
            {
                import std.utf : encode;
                buf.length = 0;
                for (int c; (c = lf.fgetwc()) != -1; )
                {
                    if ((c & ~0x7F) == 0)
                        buf ~= cast(char) c;
                    else
                        encode(buf, cast(dchar) c);
                    if (c == terminator)
                        break;
                }
                if (ferror(fps))
                    StdioException();
                return buf.length;
            }
            else
            {
                static assert(0);
            }
        }

        // Narrow stream
        // First, fill the existing buffer
        for (size_t bufPos = 0; bufPos < buf.length; )
        {
            immutable c = lf.fgetc();
            if (c == -1)
            {
                buf.length = bufPos;
                goto endGame;
            }
            buf[bufPos++] = cast(char) c;
            if (c == terminator)
            {
                // No need to test for errors in file
                buf.length = bufPos;
                return bufPos;
            }
        }
        // Then, append to it
        for (int c; (c = lf.fgetc()) != -1; )
        {
            buf ~= cast(char) c;
            if (c == terminator)
            {
                // No need to test for errors in file
                return buf.length;
            }
        }

    endGame:
        if (ferror(fps))
            StdioException();
        return buf.length;
    }
}

@system unittest
{
    static import std.file;
    auto deleteme = testFilename();
    scope(exit) std.file.remove(deleteme);

    std.file.write(deleteme, "abcd\n0123456789abcde\n1234\n");
    File f = File(deleteme, "rb");

    char[] ln = new char[2];
    f.readln(ln);

    assert(ln == "abcd\n");
    char[] t = ln[0 .. 2];
    t ~= 't';
    assert(t == "abt");
    // https://issues.dlang.org/show_bug.cgi?id=13856: ln stomped to "abtd"
    assert(ln == "abcd\n");

    // it can also stomp the array length
    ln = new char[4];
    f.readln(ln);
    assert(ln == "0123456789abcde\n");

    char[100] buf;
    ln = buf[];
    f.readln(ln);
    assert(ln == "1234\n");
    assert(ln.ptr == buf.ptr); // avoid allocation, buffer is good enough
}

/** Experimental network access via the File interface

        Opens a TCP connection to the given host and port, then returns
        a File struct with read and write access through the same interface
        as any other file (meaning writef and the byLine ranges work!).

        Authors:
                Adam D. Ruppe

        Bugs:
                Only works on Linux
*/
version (linux)
{
    File openNetwork(string host, ushort port)
    {
        import core.stdc.string : memcpy;
        import core.sys.posix.arpa.inet : htons;
        import core.sys.posix.netdb : gethostbyname;
        import core.sys.posix.netinet.in_ : sockaddr_in;
        static import core.sys.posix.unistd;
        static import sock = core.sys.posix.sys.socket;
        import std.conv : to;
        import std.exception : enforce;
        import std.internal.cstring : tempCString;

        auto h = enforce( gethostbyname(host.tempCString()),
            new StdioException("gethostbyname"));

        int s = sock.socket(sock.AF_INET, sock.SOCK_STREAM, 0);
        enforce(s != -1, new StdioException("socket"));

        scope(failure)
        {
            // want to make sure it doesn't dangle if something throws. Upon
            // normal exit, the File struct's reference counting takes care of
            // closing, so we don't need to worry about success
            core.sys.posix.unistd.close(s);
        }

        sockaddr_in addr;

        addr.sin_family = sock.AF_INET;
        addr.sin_port = htons(port);
        memcpy(&addr.sin_addr.s_addr, h.h_addr, h.h_length);

        enforce(sock.connect(s, cast(sock.sockaddr*) &addr, addr.sizeof) != -1,
            new StdioException("Connect failed"));

        File f;
        f.fdopen(s, "w+", host ~ ":" ~ to!string(port));
        return f;
    }
}

version (StdUnittest) private string testFilename(string file = __FILE__, size_t line = __LINE__) @safe
{
    import std.conv : text;
    import std.file : deleteme;
    import std.path : baseName;

    // filename intentionally contains non-ASCII (Russian) characters for
    // https://issues.dlang.org/show_bug.cgi?id=7648
    return text(deleteme, "-детка.", baseName(file), ".", line);
}

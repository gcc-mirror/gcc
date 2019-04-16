// Written in the D programming language.

/**
Standard I/O functions that extend $(B core.stdc.stdio).  $(B core.stdc.stdio)
is $(D_PARAM public)ally imported when importing $(B std.stdio).

Source: $(PHOBOSSRC std/_stdio.d)
Copyright: Copyright Digital Mars 2007-.
License:   $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   $(HTTP digitalmars.com, Walter Bright),
           $(HTTP erdani.org, Andrei Alexandrescu),
           Alex Rønne Petersen
 */
module std.stdio;

import core.stdc.stddef; // wchar_t
public import core.stdc.stdio;
import std.algorithm.mutation; // copy
import std.meta; // allSatisfy
import std.range.primitives; // ElementEncodingType, empty, front,
    // isBidirectionalRange, isInputRange, put
import std.traits; // isSomeChar, isSomeString, Unqual, isPointer
import std.typecons; // Flag

/++
If flag $(D KeepTerminator) is set to $(D KeepTerminator.yes), then the delimiter
is included in the strings returned.
+/
alias KeepTerminator = Flag!"keepTerminator";

version (CRuntime_Microsoft)
{
    version = MICROSOFT_STDIO;
}
else version (CRuntime_DigitalMars)
{
    // Specific to the way Digital Mars C does stdio
    version = DIGITAL_MARS_STDIO;
}

version (CRuntime_Glibc)
{
    // Specific to the way Gnu C does stdio
    version = GCC_IO;
    version = HAS_GETDELIM;
}

version (OSX)
{
    version = GENERIC_IO;
    version = HAS_GETDELIM;
}

version (FreeBSD)
{
    version = GENERIC_IO;
    version = HAS_GETDELIM;
}

version (NetBSD)
{
    version = GENERIC_IO;
    version = HAS_GETDELIM;
}

version (DragonFlyBSD)
{
    version = GENERIC_IO;
    version = HAS_GETDELIM;
}

version (Solaris)
{
    version = GENERIC_IO;
    version = NO_GETDELIM;
}

version (CRuntime_Bionic)
{
    version = GENERIC_IO;
    version = NO_GETDELIM;
}

// Character type used for operating system filesystem APIs
version (Windows)
{
    private alias FSChar = wchar;
}
else version (Posix)
{
    private alias FSChar = char;
}
else
    static assert(0);

version (Windows)
{
    // core.stdc.stdio.fopen expects file names to be
    // encoded in CP_ACP on Windows instead of UTF-8.
    /+ Waiting for druntime pull 299
    +/
    extern (C) nothrow @nogc FILE* _wfopen(in wchar* filename, in wchar* mode);
    extern (C) nothrow @nogc FILE* _wfreopen(in wchar* filename, in wchar* mode, FILE* fp);

    import core.sys.windows.windows : HANDLE;
}

version (DIGITAL_MARS_STDIO)
{
    extern (C)
    {
        /* **
         * Digital Mars under-the-hood C I/O functions.
         * Use _iobuf* for the unshared version of FILE*,
         * usable when the FILE is locked.
         */
      nothrow:
      @nogc:
        int _fputc_nlock(int, _iobuf*);
        int _fputwc_nlock(int, _iobuf*);
        int _fgetc_nlock(_iobuf*);
        int _fgetwc_nlock(_iobuf*);
        int __fp_lock(FILE*);
        void __fp_unlock(FILE*);

        int setmode(int, int);
    }
    alias FPUTC = _fputc_nlock;
    alias FPUTWC = _fputwc_nlock;
    alias FGETC = _fgetc_nlock;
    alias FGETWC = _fgetwc_nlock;

    alias FLOCK = __fp_lock;
    alias FUNLOCK = __fp_unlock;

    alias _setmode = setmode;
    enum _O_BINARY = 0x8000;
    int _fileno(FILE* f) { return f._file; }
    alias fileno = _fileno;
}
else version (MICROSOFT_STDIO)
{
    extern (C)
    {
        /* **
         * Microsoft under-the-hood C I/O functions
         */
      nothrow:
      @nogc:
        int _fputc_nolock(int, _iobuf*);
        int _fputwc_nolock(int, _iobuf*);
        int _fgetc_nolock(_iobuf*);
        int _fgetwc_nolock(_iobuf*);
        void _lock_file(FILE*);
        void _unlock_file(FILE*);
        int _setmode(int, int);
        int _fileno(FILE*);
        FILE* _fdopen(int, const (char)*);
        int _fseeki64(FILE*, long, int);
        long _ftelli64(FILE*);
    }
    alias FPUTC = _fputc_nolock;
    alias FPUTWC = _fputwc_nolock;
    alias FGETC = _fgetc_nolock;
    alias FGETWC = _fgetwc_nolock;

    alias FLOCK = _lock_file;
    alias FUNLOCK = _unlock_file;

    alias setmode = _setmode;
    alias fileno = _fileno;

    enum
    {
        _O_RDONLY = 0x0000,
        _O_APPEND = 0x0004,
        _O_TEXT   = 0x4000,
        _O_BINARY = 0x8000,
    }
}
else version (GCC_IO)
{
    /* **
     * Gnu under-the-hood C I/O functions; see
     * http://gnu.org/software/libc/manual/html_node/I_002fO-on-Streams.html
     */
    extern (C)
    {
      nothrow:
      @nogc:
        int fputc_unlocked(int, _iobuf*);
        int fputwc_unlocked(wchar_t, _iobuf*);
        int fgetc_unlocked(_iobuf*);
        int fgetwc_unlocked(_iobuf*);
        void flockfile(FILE*);
        void funlockfile(FILE*);

        private size_t fwrite_unlocked(const(void)* ptr,
                size_t size, size_t n, _iobuf *stream);
    }

    alias FPUTC = fputc_unlocked;
    alias FPUTWC = fputwc_unlocked;
    alias FGETC = fgetc_unlocked;
    alias FGETWC = fgetwc_unlocked;

    alias FLOCK = flockfile;
    alias FUNLOCK = funlockfile;
}
else version (GENERIC_IO)
{
    nothrow:
    @nogc:

    extern (C)
    {
        void flockfile(FILE*);
        void funlockfile(FILE*);
    }

    int fputc_unlocked(int c, _iobuf* fp) { return fputc(c, cast(shared) fp); }
    int fputwc_unlocked(wchar_t c, _iobuf* fp)
    {
        import core.stdc.wchar_ : fputwc;
        return fputwc(c, cast(shared) fp);
    }
    int fgetc_unlocked(_iobuf* fp) { return fgetc(cast(shared) fp); }
    int fgetwc_unlocked(_iobuf* fp)
    {
        import core.stdc.wchar_ : fgetwc;
        return fgetwc(cast(shared) fp);
    }

    alias FPUTC = fputc_unlocked;
    alias FPUTWC = fputwc_unlocked;
    alias FGETC = fgetc_unlocked;
    alias FGETWC = fgetwc_unlocked;

    alias FLOCK = flockfile;
    alias FUNLOCK = funlockfile;
}
else
{
    static assert(0, "unsupported C I/O system");
}

version (HAS_GETDELIM) extern(C) nothrow @nogc
{
    ptrdiff_t getdelim(char**, size_t*, int, FILE*);
    // getline() always comes together with getdelim()
    ptrdiff_t getline(char**, size_t*, FILE*);
}

//------------------------------------------------------------------------------
struct ByRecord(Fields...)
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
        import std.format : formattedRead;
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
    ByRecord!(Fields) byRecord(File f, string format)
    {
        return typeof(return)(f, format);
    }
}

/**
Encapsulates a $(D FILE*). Generally D does not attempt to provide
thin wrappers over equivalent functions in the C standard library, but
manipulating $(D FILE*) values directly is unsafe and error-prone in
many ways. The $(D File) type ensures safe manipulation, automatic
file closing, and a lot of convenience.

The underlying $(D FILE*) handle is maintained in a reference-counted
manner, such that as soon as the last $(D File) variable bound to a
given $(D FILE*) goes out of scope, the underlying $(D FILE*) is
automatically closed.

Example:
----
// test.d
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
    import std.range.primitives : ElementEncodingType;
    import std.traits : isScalarType, isArray;
    enum Orientation { unknown, narrow, wide }

    private struct Impl
    {
        FILE * handle = null; // Is null iff this Impl is closed by another File
        uint refs = uint.max / 2;
        bool isPopened; // true iff the stream has been created by popen()
        Orientation orientation;
    }
    private Impl* _p;
    private string _name;

    package this(FILE* handle, string name, uint refs = 1, bool isPopened = false) @trusted
    {
        import core.stdc.stdlib : malloc;
        import std.exception : enforce;

        assert(!_p);
        _p = cast(Impl*) enforce(malloc(Impl.sizeof), "Out of memory");
        _p.handle = handle;
        _p.refs = refs;
        _p.isPopened = isPopened;
        _p.orientation = Orientation.unknown;
        _name = name;
    }

/**
Constructor taking the name of the file to open and the open mode.

Copying one $(D File) object to another results in the two $(D File)
objects referring to the same underlying file.

The destructor automatically closes the file as soon as no $(D File)
object refers to it anymore.

Params:
    name = range or string representing the file _name
    stdioOpenmode = range or string represting the open mode
        (with the same semantics as in the C standard library
        $(HTTP cplusplus.com/reference/clibrary/cstdio/fopen.html, fopen)
        function)

Throws: $(D ErrnoException) if the file could not be opened.
 */
    this(string name, in char[] stdioOpenmode = "rb") @safe
    {
        import std.conv : text;
        import std.exception : errnoEnforce;

        this(errnoEnforce(.fopen(name, stdioOpenmode),
                        text("Cannot open file `", name, "' in mode `",
                                stdioOpenmode, "'")),
                name);

        // MSVCRT workaround (issue 14422)
        version (MICROSOFT_STDIO)
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

    /// ditto
    this(R1, R2)(R1 name)
        if (isInputRange!R1 && isSomeChar!(ElementEncodingType!R1))
    {
        import std.conv : to;
        this(name.to!string, "rb");
    }

    /// ditto
    this(R1, R2)(R1 name, R2 mode)
        if (isInputRange!R1 && isSomeChar!(ElementEncodingType!R1) &&
            isInputRange!R2 && isSomeChar!(ElementEncodingType!R2))
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

    this(this) @safe nothrow
    {
        if (!_p) return;
        assert(_p.refs);
        ++_p.refs;
    }

/**
Assigns a file to another. The target of the assignment gets detached
from whatever file it was attached to, and attaches itself to the new
file.
 */
    void opAssign(File rhs) @safe
    {
        import std.algorithm.mutation : swap;

        swap(this, rhs);
    }

/**
First calls $(D detach) (throwing on failure), and then attempts to
_open file $(D name) with mode $(D stdioOpenmode). The mode has the
same semantics as in the C standard library $(HTTP
cplusplus.com/reference/clibrary/cstdio/fopen.html, fopen) function.

Throws: $(D ErrnoException) in case of error.
 */
    void open(string name, in char[] stdioOpenmode = "rb") @safe
    {
        detach();
        this = File(name, stdioOpenmode);
    }

/**
Reuses the `File` object to either open a different file, or change
the file mode. If `name` is `null`, the mode of the currently open
file is changed; otherwise, a new file is opened, reusing the C
`FILE*`. The function has the same semantics as in the C standard
library $(HTTP cplusplus.com/reference/cstdio/freopen/, freopen)
function.

Note: Calling `reopen` with a `null` `name` is not implemented
in all C runtimes.

Throws: $(D ErrnoException) in case of error.
 */
    void reopen(string name, in char[] stdioOpenmode = "rb") @trusted
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

    @system unittest // Test changing filename
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

    version (CRuntime_DigitalMars) {} else // Not implemented
    version (CRuntime_Microsoft) {} else // Not implemented
    @system unittest // Test changing mode
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
First calls $(D detach) (throwing on failure), and then runs a command
by calling the C standard library function $(HTTP
opengroup.org/onlinepubs/007908799/xsh/_popen.html, _popen).

Throws: $(D ErrnoException) in case of error.
 */
    version (Posix) void popen(string command, in char[] stdioOpenmode = "r") @safe
    {
        import std.exception : errnoEnforce;

        detach();
        this = File(errnoEnforce(.popen(command, stdioOpenmode),
                        "Cannot run command `"~command~"'"),
                command, 1, true);
    }

/**
First calls $(D detach) (throwing on failure), and then attempts to
associate the given file descriptor with the $(D File). The mode must
be compatible with the mode of the file descriptor.

Throws: $(D ErrnoException) in case of error.
 */
    void fdopen(int fd, in char[] stdioOpenmode = "rb") @safe
    {
        fdopen(fd, stdioOpenmode, null);
    }

    package void fdopen(int fd, in char[] stdioOpenmode, string name) @trusted
    {
        import std.exception : errnoEnforce;
        import std.internal.cstring : tempCString;

        auto modez = stdioOpenmode.tempCString();
        detach();

        version (DIGITAL_MARS_STDIO)
        {
            // This is a re-implementation of DMC's fdopen, but without the
            // mucking with the file descriptor.  POSIX standard requires the
            // new fdopen'd file to retain the given file descriptor's
            // position.
            import core.stdc.stdio : fopen;
            auto fp = fopen("NUL", modez);
            errnoEnforce(fp, "Cannot open placeholder NUL stream");
            FLOCK(fp);
            auto iob = cast(_iobuf*) fp;
            .close(iob._file);
            iob._file = fd;
            iob._flag &= ~_IOTRAN;
            FUNLOCK(fp);
        }
        else
        {
            version (Windows) // MSVCRT
                auto fp = _fdopen(fd, modez);
            else version (Posix)
            {
                import core.sys.posix.stdio : fdopen;
                auto fp = fdopen(fd, modez);
            }
            errnoEnforce(fp);
        }
        this = File(fp, name);
    }

    // Declare a dummy HANDLE to allow generating documentation
    // for Windows-only methods.
    version (StdDdoc) { version (Windows) {} else alias HANDLE = int; }

/**
First calls $(D detach) (throwing on failure), and then attempts to
associate the given Windows $(D HANDLE) with the $(D File). The mode must
be compatible with the access attributes of the handle. Windows only.

Throws: $(D ErrnoException) in case of error.
*/
    version (StdDdoc)
    void windowsHandleOpen(HANDLE handle, in char[] stdioOpenmode);

    version (Windows)
    void windowsHandleOpen(HANDLE handle, in char[] stdioOpenmode)
    {
        import core.stdc.stdint : intptr_t;
        import std.exception : errnoEnforce;
        import std.format : format;

        // Create file descriptors from the handles
        version (DIGITAL_MARS_STDIO)
            auto fd = _handleToFD(handle, FHND_DEVICE);
        else // MSVCRT
        {
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
        }

        errnoEnforce(fd >= 0, "Cannot open Windows HANDLE");
        fdopen(fd, stdioOpenmode, "HANDLE(%s)".format(handle));
    }


/** Returns $(D true) if the file is opened. */
    @property bool isOpen() const @safe pure nothrow
    {
        return _p !is null && _p.handle;
    }

/**
Returns $(D true) if the file is at end (see $(HTTP
cplusplus.com/reference/clibrary/cstdio/feof.html, feof)).

Throws: $(D Exception) if the file is not opened.
 */
    @property bool eof() const @trusted pure
    {
        import std.exception : enforce;

        enforce(_p && _p.handle, "Calling eof() against an unopened file.");
        return .feof(cast(FILE*) _p.handle) != 0;
    }

/** Returns the name of the last opened file, if any.
If a $(D File) was created with $(LREF tmpfile) and $(LREF wrapFile)
it has no name.*/
    @property string name() const @safe pure nothrow
    {
        return _name;
    }

/**
If the file is not opened, returns $(D true). Otherwise, returns
$(HTTP cplusplus.com/reference/clibrary/cstdio/ferror.html, ferror) for
the file handle.
 */
    @property bool error() const @trusted pure nothrow
    {
        return !isOpen || .ferror(cast(FILE*) _p.handle);
    }

    @safe unittest
    {
        // Issue 12349
        static import std.file;
        auto deleteme = testFilename();
        auto f = File(deleteme, "w");
        scope(exit) std.file.remove(deleteme);

        f.close();
        assert(f.error);
    }

/**
Detaches from the underlying file. If the sole owner, calls $(D close).

Throws: $(D ErrnoException) on failure if closing the file.
  */
    void detach() @safe
    {
        if (!_p) return;
        if (_p.refs == 1)
            close();
        else
        {
            assert(_p.refs);
            --_p.refs;
            _p = null;
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
If the file was unopened, succeeds vacuously. Otherwise closes the
file (by calling $(HTTP
cplusplus.com/reference/clibrary/cstdio/fclose.html, fclose)),
throwing on error. Even if an exception is thrown, afterwards the $(D
File) object is empty. This is different from $(D detach) in that it
always closes the file; consequently, all other $(D File) objects
referring to the same handle will see a closed file henceforth.

Throws: $(D ErrnoException) on error.
 */
    void close() @trusted
    {
        import core.stdc.stdlib : free;
        import std.exception : errnoEnforce;

        if (!_p) return; // succeed vacuously
        scope(exit)
        {
            assert(_p.refs);
            if (!--_p.refs)
                free(_p);
            _p = null; // start a new life
        }
        if (!_p.handle) return; // Impl is closed by another File

        scope(exit) _p.handle = null; // nullify the handle anyway
        version (Posix)
        {
            import core.sys.posix.stdio : pclose;
            import std.format : format;

            if (_p.isPopened)
            {
                auto res = pclose(_p.handle);
                errnoEnforce(res != -1,
                        "Could not close pipe `"~_name~"'");
                errnoEnforce(res == 0, format("Command returned %d", res));
                return;
            }
        }
        errnoEnforce(.fclose(_p.handle) == 0,
                "Could not close file `"~_name~"'");
    }

/**
If the file is not opened, succeeds vacuously. Otherwise, returns
$(HTTP cplusplus.com/reference/clibrary/cstdio/_clearerr.html,
_clearerr) for the file handle.
 */
    void clearerr() @safe pure nothrow
    {
        _p is null || _p.handle is null ||
        .clearerr(_p.handle);
    }

/**
Flushes the C $(D FILE) buffers.

Calls $(HTTP cplusplus.com/reference/clibrary/cstdio/_fflush.html, _fflush)
for the file handle.

Throws: $(D Exception) if the file is not opened or if the call to $(D fflush) fails.
 */
    void flush() @trusted
    {
        import std.exception : enforce, errnoEnforce;

        enforce(isOpen, "Attempting to flush() in an unopened file");
        errnoEnforce(.fflush(_p.handle) == 0);
    }

    @safe unittest
    {
        // Issue 12349
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
Call $(LREF flush) before calling this function to flush the C $(D FILE) buffers first.

This function calls
$(HTTP msdn.microsoft.com/en-us/library/windows/desktop/aa364439%28v=vs.85%29.aspx,
$(D FlushFileBuffers)) on Windows and
$(HTTP pubs.opengroup.org/onlinepubs/7908799/xsh/fsync.html,
$(D fsync)) on POSIX for the file handle.

Throws: $(D Exception) if the file is not opened or if the OS call fails.
 */
    void sync() @trusted
    {
        import std.exception : enforce;

        enforce(isOpen, "Attempting to sync() an unopened file");

        version (Windows)
        {
            import core.sys.windows.windows : FlushFileBuffers;
            wenforce(FlushFileBuffers(windowsHandle), "FlushFileBuffers failed");
        }
        else
        {
            import core.sys.posix.unistd : fsync;
            import std.exception : errnoEnforce;
            errnoEnforce(fsync(fileno) == 0, "fsync failed");
        }
    }

/**
Calls $(HTTP cplusplus.com/reference/clibrary/cstdio/fread.html, fread) for the
file handle. The number of items to read and the size of
each item is inferred from the size and type of the input array, respectively.

Returns: The slice of $(D buffer) containing the data that was actually read.
This will be shorter than $(D buffer) if EOF was reached before the buffer
could be filled.

Throws: $(D Exception) if $(D buffer) is empty.
        $(D ErrnoException) if the file is not opened or the call to $(D fread) fails.

$(D rawRead) always reads in binary mode on Windows.
 */
    T[] rawRead(T)(T[] buffer)
    {
        import std.exception : errnoEnforce;

        if (!buffer.length)
            throw new Exception("rawRead must take a non-empty buffer");
        version (Windows)
        {
            immutable fd = ._fileno(_p.handle);
            immutable mode = ._setmode(fd, _O_BINARY);
            scope(exit) ._setmode(fd, mode);
            version (DIGITAL_MARS_STDIO)
            {
                import core.atomic : atomicOp;

                // @@@BUG@@@ 4243
                immutable info = __fhnd_info[fd];
                atomicOp!"&="(__fhnd_info[fd], ~FHND_TEXT);
                scope(exit) __fhnd_info[fd] = info;
            }
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

        auto testFile = testFilename();
        std.file.write(testFile, "\r\n\n\r\n");
        scope(exit) std.file.remove(testFile);

        auto f = File(testFile, "r");
        auto buf = f.rawRead(new char[5]);
        f.close();
        assert(buf == "\r\n\n\r\n");
    }

/**
Calls $(HTTP cplusplus.com/reference/clibrary/cstdio/fwrite.html, fwrite) for the file
handle. The number of items to write and the size of each
item is inferred from the size and type of the input array, respectively. An
error is thrown if the buffer could not be written in its entirety.

$(D rawWrite) always writes in binary mode on Windows.

Throws: $(D ErrnoException) if the file is not opened or if the call to $(D fwrite) fails.
 */
    void rawWrite(T)(in T[] buffer)
    {
        import std.conv : text;
        import std.exception : errnoEnforce;

        version (Windows)
        {
            flush(); // before changing translation mode
            immutable fd = ._fileno(_p.handle);
            immutable mode = ._setmode(fd, _O_BINARY);
            scope(exit) ._setmode(fd, mode);
            version (DIGITAL_MARS_STDIO)
            {
                import core.atomic : atomicOp;

                // @@@BUG@@@ 4243
                immutable info = __fhnd_info[fd];
                atomicOp!"&="(__fhnd_info[fd], ~FHND_TEXT);
                scope(exit) __fhnd_info[fd] = info;
            }
            scope(exit) flush(); // before restoring translation mode
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

        auto testFile = testFilename();
        auto f = File(testFile, "w");
        scope(exit) std.file.remove(testFile);

        f.rawWrite("\r\n\n\r\n");
        f.close();
        assert(std.file.read(testFile) == "\r\n\n\r\n");
    }

/**
Calls $(HTTP cplusplus.com/reference/clibrary/cstdio/fseek.html, fseek)
for the file handle.

Throws: $(D Exception) if the file is not opened.
        $(D ErrnoException) if the call to $(D fseek) fails.
 */
    void seek(long offset, int origin = SEEK_SET) @trusted
    {
        import std.conv : to, text;
        import std.exception : enforce, errnoEnforce;

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

        auto deleteme = testFilename();
        auto f = File(deleteme, "w+");
        scope(exit) { f.close(); std.file.remove(deleteme); }
        f.rawWrite("abcdefghijklmnopqrstuvwxyz");
        f.seek(7);
        assert(f.readln() == "hijklmnopqrstuvwxyz");

        version (CRuntime_DigitalMars)
            auto bigOffset = int.max - 100;
        else
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
    }

/**
Calls $(HTTP cplusplus.com/reference/clibrary/cstdio/ftell.html, ftell) for the
managed file handle.

Throws: $(D Exception) if the file is not opened.
        $(D ErrnoException) if the call to $(D ftell) fails.
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

        auto testFile = testFilename();
        std.file.write(testFile, "abcdefghijklmnopqrstuvwqxyz");
        scope(exit) { std.file.remove(testFile); }

        auto f = File(testFile);
        auto a = new ubyte[4];
        f.rawRead(a);
        assert(f.tell == 4, text(f.tell));
    }

/**
Calls $(HTTP cplusplus.com/reference/clibrary/cstdio/_rewind.html, _rewind)
for the file handle.

Throws: $(D Exception) if the file is not opened.
 */
    void rewind() @safe
    {
        import std.exception : enforce;

        enforce(isOpen, "Attempting to rewind() an unopened file");
        .rewind(_p.handle);
    }

/**
Calls $(HTTP cplusplus.com/reference/clibrary/cstdio/_setvbuf.html, _setvbuf) for
the file handle.

Throws: $(D Exception) if the file is not opened.
        $(D ErrnoException) if the call to $(D setvbuf) fails.
 */
    void setvbuf(size_t size, int mode = _IOFBF) @trusted
    {
        import std.exception : enforce, errnoEnforce;

        enforce(isOpen, "Attempting to call setvbuf() on an unopened file");
        errnoEnforce(.setvbuf(_p.handle, null, mode, size) == 0,
                "Could not set buffering for file `"~_name~"'");
    }

/**
Calls $(HTTP cplusplus.com/reference/clibrary/cstdio/_setvbuf.html,
_setvbuf) for the file handle.

Throws: $(D Exception) if the file is not opened.
        $(D ErrnoException) if the call to $(D setvbuf) fails.
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
        import core.sys.windows.windows : ULARGE_INTEGER, OVERLAPPED, BOOL;

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

        private static T wenforce(T)(T cond, string str)
        {
            import core.sys.windows.windows : GetLastError;
            import std.windows.syserror : sysErrorString;

            if (cond) return cond;
            throw new Exception(str ~ ": " ~ sysErrorString(GetLastError()));
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
If both $(D start) and $(D length) are zero, the entire file is locked.

Locks created using $(D lock) and $(D tryLock) have the following properties:
$(UL
 $(LI All locks are automatically released when the process terminates.)
 $(LI Locks are not inherited by child processes.)
 $(LI Closing a file will release all locks associated with the file. On POSIX,
      even locks acquired via a different $(D File) will be released as well.)
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
            import core.sys.windows.windows : LockFileEx, LOCKFILE_EXCLUSIVE_LOCK;
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
If both $(D start) and $(D length) are zero, the entire file is locked.
Returns: $(D true) if the lock was successful, and $(D false) if the
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
            import core.sys.windows.windows : GetLastError, LockFileEx, LOCKFILE_EXCLUSIVE_LOCK,
                ERROR_IO_PENDING, ERROR_LOCK_VIOLATION, LOCKFILE_FAIL_IMMEDIATELY;
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
            import core.sys.windows.windows : UnlockFileEx;
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
        static import std.file;
        auto deleteme = testFilename();
        scope(exit) std.file.remove(deleteme);

        // Since locks are per-process, we cannot test lock failures within
        // the same process. fork() is used to create a second process.
        static void runForked(void delegate() code)
        {
            import core.stdc.stdlib : exit;
            import core.sys.posix.sys.wait : wait;
            import core.sys.posix.unistd : fork;
            int child, status;
            if ((child = fork()) == 0)
            {
                code();
                exit(0);
            }
            else
            {
                assert(wait(&status) != -1);
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
    }


/**
Writes its arguments in text format to the file.

Throws: $(D Exception) if the file is not opened.
        $(D ErrnoException) on an error writing to the file.
*/
    void write(S...)(S args)
    {
        import std.traits : isBoolean, isIntegral, isAggregateType;
        auto w = lockingTextWriter();
        foreach (arg; args)
        {
            alias A = typeof(arg);
            static if (isAggregateType!A || is(A == enum))
            {
                import std.format : formattedWrite;

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
                import std.format : formattedWrite;

                // Most general case
                formattedWrite(w, "%s", arg);
            }
        }
    }

/**
Writes its arguments in text format to the file, followed by a newline.

Throws: $(D Exception) if the file is not opened.
        $(D ErrnoException) on an error writing to the file.
*/
    void writeln(S...)(S args)
    {
        write(args, '\n');
    }

/**
Writes its arguments in text format to the file, according to the
format string fmt.

Params:
fmt = The $(LINK2 std_format.html#format-string, format string).
When passed as a compile-time argument, the string will be statically checked
against the argument types passed.
args = Items to write.

Throws: $(D Exception) if the file is not opened.
        $(D ErrnoException) on an error writing to the file.
*/
    void writef(alias fmt, A...)(A args)
    if (isSomeString!(typeof(fmt)))
    {
        import std.format : checkFormatException;

        alias e = checkFormatException!(fmt, A);
        static assert(!e, e.msg);
        return this.writef(fmt, args);
    }

    /// ditto
    void writef(Char, A...)(in Char[] fmt, A args)
    {
        import std.format : formattedWrite;

        formattedWrite(lockingTextWriter(), fmt, args);
    }

    /// Equivalent to `file.writef(fmt, args, '\n')`.
    void writefln(alias fmt, A...)(A args)
    if (isSomeString!(typeof(fmt)))
    {
        import std.format : checkFormatException;

        alias e = checkFormatException!(fmt, A);
        static assert(!e, e.msg);
        return this.writefln(fmt, args);
    }

    /// ditto
    void writefln(Char, A...)(in Char[] fmt, A args)
    {
        import std.format : formattedWrite;

        auto w = lockingTextWriter();
        formattedWrite(w, fmt, args);
        w.put('\n');
    }

/**
Read line from the file handle and return it as a specified type.

This version manages its own read buffer, which means one memory allocation per call. If you are not
retaining a reference to the read data, consider the $(D File.readln(buf)) version, which may offer
better performance as it can reuse its read buffer.

Params:
    S = Template parameter; the type of the allocated buffer, and the type returned. Defaults to $(D string).
    terminator = Line terminator (by default, $(D '\n')).

Note:
    String terminators are not supported due to ambiguity with readln(buf) below.

Returns:
    The line that was read, including the line terminator character.

Throws:
    $(D StdioException) on I/O error, or $(D UnicodeException) on Unicode conversion error.

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
    S readln(S = string)(dchar terminator = '\n')
    if (isSomeString!S)
    {
        Unqual!(ElementEncodingType!S)[] buf;
        readln(buf, terminator);
        return cast(S) buf;
    }

    @system unittest
    {
        import std.algorithm.comparison : equal;
        static import std.file;
        import std.meta : AliasSeq;

        auto deleteme = testFilename();
        std.file.write(deleteme, "hello\nworld\n");
        scope(exit) std.file.remove(deleteme);
        foreach (String; AliasSeq!(string, char[], wstring, wchar[], dstring, dchar[]))
        {
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
        }
    }

    @system unittest
    {
        static import std.file;
        import std.typecons : Tuple;

        auto deleteme = testFilename();
        std.file.write(deleteme, "cześć \U0002000D");
        scope(exit) std.file.remove(deleteme);
        uint[] lengths = [12,8,7];
        foreach (uint i, C; Tuple!(char, wchar, dchar).Types)
        {
            immutable(C)[] witness = "cześć \U0002000D";
            auto buf = File(deleteme).readln!(immutable(C)[])();
            assert(buf.length == lengths[i]);
            assert(buf == witness);
        }
    }

/**
Read line from the file handle and write it to $(D buf[]), including
terminating character.

This can be faster than $(D line = File.readln()) because you can reuse
the buffer for each call. Note that reusing the buffer means that you
must copy the previous contents if you wish to retain them.

Params:
buf = Buffer used to store the resulting line data. buf is
resized as necessary.
terminator = Line terminator (by default, $(D '\n')). Use
$(REF newline, std,ascii) for portability (unless the file was opened in
text mode).

Returns:
0 for end of file, otherwise number of characters read

Throws: $(D StdioException) on I/O error, or $(D UnicodeException) on Unicode
conversion error.

Example:
---
// Read lines from `stdin` into a string
// Ignore lines starting with '#'
// Write the string to `stdout`

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
because $(D stdin.readln(buf)) reuses (if possible) memory allocated
for $(D buf), whereas $(D line = stdin.readln()) makes a new memory allocation
for every line.

For even better performance you can help $(D readln) by passing in a
large buffer to avoid memory reallocations. This can be done by reusing the
largest buffer returned by $(D readln):

Example:
---
// Read lines from `stdin` and count words

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
    size_t readln(C)(ref C[] buf, dchar terminator = '\n')
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
            // TODO: optimize this
            string s = readln(terminator);
            buf.length = 0;
            if (!s.length) return 0;
            foreach (C c; s)
            {
                buf ~= c;
            }
            return buf.length;
        }
    }

    @system unittest
    {
        // @system due to readln
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

    @system unittest // bugzilla 15293
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
    size_t readln(C, R)(ref C[] buf, R terminator)
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

    @system unittest
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
     * format = The $(LINK2 std_format.html#_format-string, _format string).
     * When passed as a compile-time argument, the string will be statically checked
     * against the argument types passed.
     * data = Items to be read.
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
        static assert(!e, e.msg);
        return this.readf(format, data);
    }

    /// ditto
    uint readf(Data...)(in char[] format, auto ref Data data)
    {
        import std.format : formattedRead;

        assert(isOpen);
        auto input = LockingTextReader(this);
        return formattedRead(input, format, data);
    }

    ///
    @system unittest
    {
        static import std.file;

        auto deleteme = testFilename();
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

        // Issue 11698
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

        // Issue 11698
        bool b1, b2;
        f.readf("%s\n%s\n", &b1, b2);
        assert(b1 == true && b2 == false);
    }

    // Issue 12260 - Nice error of std.stdio.readf with newlines
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
 Returns a temporary file by calling
 $(HTTP cplusplus.com/reference/clibrary/cstdio/_tmpfile.html, _tmpfile).
 Note that the created file has no $(LREF name).*/
    static File tmpfile() @safe
    {
        import std.exception : errnoEnforce;

        return File(errnoEnforce(.tmpfile(),
                "Could not create temporary file with tmpfile()"),
            null);
    }

/**
Unsafe function that wraps an existing $(D FILE*). The resulting $(D
File) never takes the initiative in closing the file.
Note that the created file has no $(LREF name)*/
    /*private*/ static File wrapFile(FILE* f) @safe
    {
        import std.exception : enforce;

        return File(enforce(f, "Could not wrap null FILE*"),
            null, /*uint.max / 2*/ 9999);
    }

/**
Returns the $(D FILE*) corresponding to this object.
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
    @property int fileno() const @trusted
    {
        import std.exception : enforce;

        enforce(isOpen, "Attempting to call fileno() on an unopened file");
        return .fileno(cast(FILE*) _p.handle);
    }

/**
Returns the underlying operating system $(D HANDLE) (Windows only).
*/
    version (StdDdoc)
    @property HANDLE windowsHandle();

    version (Windows)
    @property HANDLE windowsHandle()
    {
        version (DIGITAL_MARS_STDIO)
            return _fdToHandle(fileno);
        else
            return cast(HANDLE)_get_osfhandle(fileno);
    }


// Note: This was documented until 2013/08
/*
Range that reads one line at a time.  Returned by $(LREF byLine).

Allows to directly use range operations on lines of a file.
*/
    struct ByLine(Char, Terminator)
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

        public:
            this(File f, KeepTerminator kt, Terminator terminator)
            {
                file = f;
                this.terminator = terminator;
                keepTerminator = kt;
                popFront();
            }

            // Range primitive implementations.
            @property bool empty()
            {
                return line is null;
            }

            @property Char[] front()
            {
                return line;
            }

            void popFront()
            {
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
                            is(Unqual!(ElementEncodingType!Terminator) == Char));
                        const tlen = terminator.length;
                    }
                    else
                        static assert(false);
                    line = line[0 .. line.length - tlen];
                }
            }
        }
    }

/**
Returns an input range set up to read from the file handle one line
at a time.

The element type for the range will be $(D Char[]). Range primitives
may throw $(D StdioException) on I/O error.

Note:
Each $(D front) will not persist after $(D
popFront) is called, so the caller must copy its contents (e.g. by
calling $(D to!string)) when retention is needed. If the caller needs
to retain a copy of every line, use the $(LREF byLineCopy) function
instead.

Params:
Char = Character type for each line, defaulting to $(D char).
keepTerminator = Use $(D Yes.keepTerminator) to include the
terminator at the end of each line.
terminator = Line separator ($(D '\n') by default). Use
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
$(D front) after the corresponding $(D popFront) call is made (because
the contents may well have changed).
*/
    auto byLine(Terminator = char, Char = char)
            (KeepTerminator keepTerminator = No.keepTerminator,
            Terminator terminator = '\n')
    if (isScalarType!Terminator)
    {
        return ByLine!(Char, Terminator)(this, keepTerminator, terminator);
    }

/// ditto
    auto byLine(Terminator, Char = char)
            (KeepTerminator keepTerminator, Terminator terminator)
    if (is(Unqual!(ElementEncodingType!Terminator) == Char))
    {
        return ByLine!(Char, Terminator)(this, keepTerminator, terminator);
    }

    @system unittest
    {
        static import std.file;
        auto deleteme = testFilename();
        std.file.write(deleteme, "hi");
        scope(success) std.file.remove(deleteme);

        import std.meta : AliasSeq;
        foreach (T; AliasSeq!(char, wchar, dchar))
        {
            auto blc = File(deleteme).byLine!(T, T);
            assert(blc.front == "hi");
            // check front is cached
            assert(blc.front is blc.front);
        }
    }

    private struct ByLineCopy(Char, Terminator)
    {
    private:
        import std.typecons : RefCounted, RefCountedAutoInitialize;

        /* Ref-counting stops the source range's ByLineCopyImpl
         * from getting out of sync after the range is copied, e.g.
         * when accessing range.front, then using std.range.take,
         * then accessing range.front again. */
        alias Impl = RefCounted!(ByLineCopyImpl!(Char, Terminator),
            RefCountedAutoInitialize.no);
        Impl impl;

    public:
        this(File f, KeepTerminator kt, Terminator terminator)
        {
            impl = Impl(f, kt, terminator);
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
    }

    private struct ByLineCopyImpl(Char, Terminator)
    {
        ByLine!(Unqual!Char, Terminator).Impl impl;
        bool gotFront;
        Char[] line;

    public:
        this(File f, KeepTerminator kt, Terminator terminator)
        {
            impl = ByLine!(Unqual!Char, Terminator).Impl(f, kt, terminator);
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
Returns an input range set up to read from the file handle one line
at a time. Each line will be newly allocated. $(D front) will cache
its value to allow repeated calls without unnecessary allocations.

Note: Due to caching byLineCopy can be more memory-efficient than
$(D File.byLine.map!idup).

The element type for the range will be $(D Char[]). Range
primitives may throw $(D StdioException) on I/O error.

Params:
Char = Character type for each line, defaulting to $(D immutable char).
keepTerminator = Use $(D Yes.keepTerminator) to include the
terminator at the end of each line.
terminator = Line separator ($(D '\n') by default). Use
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
    if (is(Unqual!(ElementEncodingType!Terminator) == Unqual!Char))
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

            // Issue 11830
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

        // bug 9599
        file.rewind();
        File.ByLine!(char, char) fbl = file.byLine();
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

    @system unittest
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
    Creates an input range set up to parse one line at a time from the file
    into a tuple.

    Range primitives may throw $(D StdioException) on I/O error.

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
        ByRecord!(Fields) byRecord(string format)
        {
            return typeof(return)(this, format);
        }
    }

    ///
    @system unittest
    {
         static import std.file;
         import std.typecons : tuple;

         // prepare test file
         auto testFile = testFilename();
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
    struct ByChunk
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

        // $(D ByChunk)'s input range primitive operations.
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
Returns an input range set up to read from the file handle a chunk at a
time.

The element type for the range will be $(D ubyte[]). Range primitives
may throw $(D StdioException) on I/O error.

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
size of each chunk. Alternatively, $(D byChunk) accepts a
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
$(D front) will not persist after $(D popFront) is called, so if retention is
needed, the caller must copy its contents (e.g. by calling $(D buffer.dup)).

In the  example above, $(D buffer.length) is 4096 for all iterations, except
for the last one, in which case $(D buffer.length) may be less than 4096 (but
always greater than zero).

With the mentioned limitations, $(D byChunk) works with any algorithm
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

Returns: A call to $(D byChunk) returns a range initialized with the $(D File)
object and the appropriate buffer.

Throws: If the user-provided size is zero or the user-provided buffer
is empty, throws an $(D Exception). In case of an I/O error throws
$(D StdioException).
 */
    auto byChunk(size_t chunkSize)
    {
        return ByChunk(this, chunkSize);
    }
/// Ditto
    ByChunk byChunk(ubyte[] buffer)
    {
        return ByChunk(this, buffer);
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
$(D Range) that locks the file and allows fast writing to it.
 */
    struct LockingTextWriter
    {
    private:
        import std.range.primitives : ElementType, isInfinite, isInputRange;
        // the shared file handle
        FILE* fps_;

        // the unshared version of fps
        @property _iobuf* handle_() @trusted { return cast(_iobuf*) fps_; }

        // the file's orientation (byte- or wide-oriented)
        int orientation_;
    public:

        this(ref File f) @trusted
        {
            import core.stdc.wchar_ : fwide;
            import std.exception : enforce;

            enforce(f._p && f._p.handle, "Attempting to write to closed File");
            fps_ = f._p.handle;
            orientation_ = fwide(fps_, 0);
            FLOCK(fps_);
        }

        ~this() @trusted
        {
            if (fps_)
            {
                FUNLOCK(fps_);
                fps_ = null;
            }
        }

        this(this) @trusted
        {
            if (fps_)
            {
                FLOCK(fps_);
            }
        }

        /// Range primitive implementations.
        void put(A)(A writeme)
            if ((isSomeChar!(Unqual!(ElementType!A)) ||
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
                    auto result = trustedFwrite(fps_, writeme);
                    if (result != writeme.length) errnoEnforce(0);
                    return;
                }
            }

            // put each element in turn.
            alias Elem = Unqual!(ElementType!A);
            foreach (Elem c; writeme)
            {
                put(c);
            }
        }

        /// ditto
        void put(C)(C c) @safe if (isSomeChar!C || is(C : const(ubyte)))
        {
            import std.traits : Parameters;
            static auto trustedFPUTC(int ch, _iobuf* h) @trusted
            {
                return FPUTC(ch, h);
            }
            static auto trustedFPUTWC(Parameters!FPUTWC[0] ch, _iobuf* h) @trusted
            {
                return FPUTWC(ch, h);
            }

            static if (c.sizeof == 1)
            {
                // simple char
                if (orientation_ <= 0) trustedFPUTC(c, handle_);
                else trustedFPUTWC(c, handle_);
            }
            else static if (c.sizeof == 2)
            {
                import std.utf : encode, UseReplacementDchar;

                if (orientation_ <= 0)
                {
                    if (c <= 0x7F)
                    {
                        trustedFPUTC(c, handle_);
                    }
                    else
                    {
                        char[4] buf;
                        immutable size = encode!(UseReplacementDchar.yes)(buf, c);
                        foreach (i ; 0 .. size)
                            trustedFPUTC(buf[i], handle_);
                    }
                }
                else
                {
                    trustedFPUTWC(c, handle_);
                }
            }
            else // 32-bit characters
            {
                import std.utf : encode;

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
                            trustedFPUTWC(c, handle_);
                        }
                        else
                        {
                            trustedFPUTWC(cast(wchar)
                                    ((((c - 0x10000) >> 10) & 0x3FF)
                                            + 0xD800), handle_);
                            trustedFPUTWC(cast(wchar)
                                    (((c - 0x10000) & 0x3FF) + 0xDC00),
                                    handle_);
                        }
                    }
                    else version (Posix)
                    {
                        trustedFPUTWC(c, handle_);
                    }
                    else
                    {
                        static assert(0);
                    }
                }
            }
        }
    }

/** Returns an output range that locks the file and allows fast writing to it.

See $(LREF byChunk) for an example.
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
        FILE* fps;
        string name;

        version (Windows)
        {
            int fd, oldMode;
            version (DIGITAL_MARS_STDIO)
                ubyte oldInfo;
        }

    package:
        this(ref File f)
        {
            import std.exception : enforce;

            enforce(f._p && f._p.handle);
            name = f._name;
            fps = f._p.handle;
            static if (locking)
                FLOCK(fps);

            version (Windows)
            {
                .fflush(fps); // before changing translation mode
                fd = ._fileno(fps);
                oldMode = ._setmode(fd, _O_BINARY);
                version (DIGITAL_MARS_STDIO)
                {
                    import core.atomic : atomicOp;

                    // @@@BUG@@@ 4243
                    oldInfo = __fhnd_info[fd];
                    atomicOp!"&="(__fhnd_info[fd], ~FHND_TEXT);
                }
            }
        }

    public:
        ~this()
        {
            if (!fps)
                return;

            version (Windows)
            {
                .fflush(fps); // before restoring translation mode
                version (DIGITAL_MARS_STDIO)
                {
                    // @@@BUG@@@ 4243
                    __fhnd_info[fd] = oldInfo;
                }
                ._setmode(fd, oldMode);
            }

            FUNLOCK(fps);
            fps = null;
        }

        void rawWrite(T)(in T[] buffer)
        {
            import std.conv : text;
            import std.exception : errnoEnforce;

            auto result = trustedFwrite(fps, buffer);
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
                if (fps)
                {
                    FLOCK(fps);
                }
            }
        }

        void put(T)(auto ref in T value)
        if (!hasIndirections!T &&
            !isInputRange!T)
        {
            rawWrite((&value)[0 .. 1]);
        }

        void put(T)(in T[] array)
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
import std.algorithm, std.range, std.stdio;

void main()
{
    enum size = 500;
    writef("P5\n%d %d %d\n", size, size, ubyte.max);

    iota(-1, 3, 2.0/size).map!(y =>
        iota(-1.5, 0.5, 2.0/size).map!(x =>
            cast(ubyte)(1+
                recurrence!((a, n) => x + y*1i + a[n-1]^^2)(0+0i)
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

/// Get the size of the file, ulong.max if file is not searchable, but still throws if an actual error occurs.
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

@system unittest
{
    // @system due to readln
    static import std.file;
    import std.range : chain, only, repeat;
    import std.range.primitives : isOutputRange;

    auto deleteme = testFilename();
    scope(exit) std.file.remove(deleteme);

    {
        File f = File(deleteme, "w");
        auto writer = f.lockingTextWriter();
        static assert(isOutputRange!(typeof(writer), dchar));
        writer.put("日本語");
        writer.put("日本語"w);
        writer.put("日本語"d);
        writer.put('日');
        writer.put(chain(only('本'), only('語')));
        writer.put(repeat('#', 12)); // BUG 11945
        writer.put(cast(immutable(ubyte)[])"日本語"); // Bug 17229
    }
    assert(File(deleteme).readln() == "日本語日本語日本語日本語############日本語");
}

@safe unittest
{
    import std.exception : collectException;
    auto e = collectException({ File f; f.writeln("Hello!"); }());
    assert(e && e.msg == "Attempting to write to closed File");
}

/// Used to specify the lock type for $(D File.lock) and $(D File.tryLock).
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
        FLOCK(_f._p.handle);
    }

    this(this)
    {
        FLOCK(_f._p.handle);
    }

    ~this()
    {
        if (_hasChar)
            ungetc(_front, cast(FILE*)_f._p.handle);

        // File locking has its own reference count
        if (_f.isOpen) FUNLOCK(_f._p.handle);
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
            immutable int c = FGETC(cast(_iobuf*) _f._p.handle);
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
    int x, y;
    auto f = File(deleteme);
    f.readf("%s ", &x);
    assert(x == 1);
    f.readf("%d ", &x);
    assert(x == 2);
    f.readf("%d ", &x);
    assert(x == 3);
}

@system unittest // bugzilla 13686
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

@system unittest // bugzilla 12320
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

@system unittest // bugzilla 14861
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
 * Indicates whether $(D T) is a file handle, i.e. the type
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
For each argument $(D arg) in $(D args), format the argument (using
$(REF to, std,conv)) and write the resulting
string to $(D args[0]). A call without any arguments will fail to
compile.

Params:
    args = the items to write to `stdout`

Throws: In case of an I/O error, throws an $(D StdioException).

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
 *        Reads $(D stdin) and writes it to $(D stdout) with a argument
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
    import std.traits : isAggregateType;
    static if (T.length == 0)
    {
        import std.exception : enforce;

        enforce(fputc('\n', .trustedStdout._p.handle) != EOF, "fputc failed");
    }
    else static if (T.length == 1 &&
                    is(typeof(args[0]) : const(char)[]) &&
                    !is(typeof(args[0]) == enum) &&
                    !is(Unqual!(typeof(args[0])) == typeof(null)) &&
                    !isAggregateType!(typeof(args[0])))
    {
        import std.traits : isStaticArray;

        // Specialization for strings - a very frequent case
        auto w = .trustedStdout.lockingTextWriter();

        static if (isStaticArray!(typeof(args[0])))
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

    // bug 8040
    if (false) writeln(null);
    if (false) writeln(">", null, "<");

    // Bugzilla 14041
    if (false)
    {
        char[8] a;
        writeln(a);
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
    writeln("Hello!"w);    // bug 8386
    writeln("Hello!"d);    // bug 8386
    writeln("embedded\0null"c); // bug 8730
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
    enum ED : double { A, B }
    enum EC : char   { A, B }
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


/***********************************
Writes formatted data to standard output (without a trailing newline).

Params:
fmt = The $(LINK2 std_format.html#format-string, format string).
When passed as a compile-time argument, the string will be statically checked
against the argument types passed.
args = Items to write.

Note: In older versions of Phobos, it used to be possible to write:

------
writef(stderr, "%s", "message");
------

to print a message to $(D stderr). This syntax is no longer supported, and has
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
    static assert(!e, e.msg);
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
    static assert(!e, e.msg);
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
 * Reads formatted data from $(D stdin) using $(REF formattedRead, std,_format).
 * Params:
 * format = The $(LINK2 std_format.html#_format-string, _format string).
 * When passed as a compile-time argument, the string will be statically checked
 * against the argument types passed.
 * args = Items to be read.
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
    static assert(!e, e.msg);
    return .readf(format, args);
}

/// ditto
uint readf(A...)(in char[] format, auto ref A args)
{
    return stdin.readf(format, args);
}

@system unittest
{
    float f;
    if (false) uint x = readf("%s", &f);

    char a;
    wchar b;
    dchar c;
    if (false) readf("%s %s %s", a, b, c);
    // backwards compatibility with pointers
    if (false) readf("%s %s %s", a, &b, c);
    if (false) readf("%s %s %s", &a, &b, &c);
}

/**********************************
 * Read line from $(D stdin).
 *
 * This version manages its own read buffer, which means one memory allocation per call. If you are not
 * retaining a reference to the read data, consider the $(D readln(buf)) version, which may offer
 * better performance as it can reuse its read buffer.
 *
 * Returns:
 *        The line that was read, including the line terminator character.
 * Params:
 *        S = Template parameter; the type of the allocated buffer, and the type returned. Defaults to $(D string).
 *        terminator = Line terminator (by default, $(D '\n')).
 * Note:
 *        String terminators are not supported due to ambiguity with readln(buf) below.
 * Throws:
 *        $(D StdioException) on I/O error, or $(D UnicodeException) on Unicode conversion error.
 * Example:
 *        Reads $(D stdin) and writes it to $(D stdout).
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
 * Read line from $(D stdin) and write it to buf[], including terminating character.
 *
 * This can be faster than $(D line = readln()) because you can reuse
 * the buffer for each call. Note that reusing the buffer means that you
 * must copy the previous contents if you wish to retain them.
 *
 * Returns:
 *        $(D size_t) 0 for end of file, otherwise number of characters read
 * Params:
 *        buf = Buffer used to store the resulting line data. buf is resized as necessary.
 *        terminator = Line terminator (by default, $(D '\n')). Use $(REF newline, std,ascii)
 *        for portability (unless the file was opened in text mode).
 * Throws:
 *        $(D StdioException) on I/O error, or $(D UnicodeException) on Unicode conversion error.
 * Example:
 *        Reads $(D stdin) and writes it to $(D stdout).
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
        foreach (String; AliasSeq!(string, char[], wstring, wchar[], dstring, dchar[]))
        {
            readln!String();
            readln!String('\t');
        }
        foreach (String; AliasSeq!(char[], wchar[], dchar[]))
        {
            String buf;
            readln(buf);
            readln(buf, '\t');
            readln(buf, "<br />");
        }
    }
}

/*
 * Convenience function that forwards to $(D core.sys.posix.stdio.fopen)
 * (to $(D _wfopen) on Windows)
 * with appropriately-constructed C-style strings.
 */
private FILE* fopen(R1, R2)(R1 name, R2 mode = "r")
if ((isInputRange!R1 && isSomeChar!(ElementEncodingType!R1) || isSomeString!R1) &&
    (isInputRange!R2 && isSomeChar!(ElementEncodingType!R2) || isSomeString!R2))
{
    import std.internal.cstring : tempCString;

    auto namez = name.tempCString!FSChar();
    auto modez = mode.tempCString!FSChar();

    static fopenImpl(const(FSChar)* namez, const(FSChar)* modez) @trusted nothrow @nogc
    {
        version (Windows)
        {
            return _wfopen(namez, modez);
        }
        else version (Posix)
        {
            /*
             * The new opengroup large file support API is transparently
             * included in the normal C bindings. http://opengroup.org/platform/lfs.html#1.0
             * if _FILE_OFFSET_BITS in druntime is 64, off_t is 64 bit and
             * the normal functions work fine. If not, then large file support
             * probably isn't available. Do not use the old transitional API
             * (the native extern(C) fopen64, http://www.unix.org/version2/whatsnew/lfs20mar.html#3.0)
             */
            import core.sys.posix.stdio : fopen;
            return fopen(namez, modez);
        }
        else
        {
            return .fopen(namez, modez);
        }
    }
    return fopenImpl(namez, modez);
}

version (Posix)
{
    /***********************************
     * Convenience function that forwards to $(D core.sys.posix.stdio.popen)
     * with appropriately-constructed C-style strings.
     */
    FILE* popen(R1, R2)(R1 name, R2 mode = "r") @trusted nothrow @nogc
    if ((isInputRange!R1 && isSomeChar!(ElementEncodingType!R1) || isSomeString!R1) &&
        (isInputRange!R2 && isSomeChar!(ElementEncodingType!R2) || isSomeString!R2))
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
 * Convenience function that forwards to $(D core.stdc.stdio.fwrite)
 */
private auto trustedFwrite(T)(FILE* f, const T[] obj) @trusted
{
    return fwrite(obj.ptr, T.sizeof, obj.length, f);
}

/*
 * Convenience function that forwards to $(D core.stdc.stdio.fread)
 */
private auto trustedFread(T)(FILE* f, T[] obj) @trusted
{
    return fread(obj.ptr, T.sizeof, obj.length, f);
}

/**
 * Iterates through the lines of a file by using $(D foreach).
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
The line terminator ($(D '\n') by default) is part of the string read (it
could be missing in the last line of the file). Several types are
supported for $(D line), and the behavior of $(D lines)
changes accordingly:

$(OL $(LI If $(D line) has type $(D string), $(D
wstring), or $(D dstring), a new string of the respective type
is allocated every read.) $(LI If $(D line) has type $(D
char[]), $(D wchar[]), $(D dchar[]), the line's content
will be reused (overwritten) across reads.) $(LI If $(D line)
has type $(D immutable(ubyte)[]), the behavior is similar to
case (1), except that no UTF checking is attempted upon input.) $(LI
If $(D line) has type $(D ubyte[]), the behavior is
similar to case (2), except that no UTF checking is attempted upon
input.))

In all cases, a two-symbols versions is also accepted, in which case
the first symbol (of integral type, e.g. $(D ulong) or $(D
uint)) tracks the zero-based number of the current line.

Example:
----
  foreach (ulong i, string line; lines(stdin))
  {
    ... use line ...
  }
----

 In case of an I/O error, an $(D StdioException) is thrown.

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
    terminator = Line separator ($(D '\n') by default).
    */
    this(File f, dchar terminator = '\n')
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
            enum bool duplicate = is(Parms[$ - 1] == string)
                || is(Parms[$ - 1] == wstring) || is(Parms[$ - 1] == dstring);
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
        FLOCK(f._p.handle);
        scope(exit) FUNLOCK(f._p.handle);
        ubyte[] buffer;
        static if (Parms.length == 2)
            Parms[0] line = 0;
        while ((c = FGETC(cast(_iobuf*) f._p.handle)) != -1)
        {
            buffer ~= to!(ubyte)(c);
            if (c == terminator)
            {
                static if (duplicate)
                    auto arg = assumeUnique(buffer);
                else
                    alias arg = buffer;
                // unlock the file while calling the delegate
                FUNLOCK(f._p.handle);
                scope(exit) FLOCK(f._p.handle);
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
        // can only reach when FGETC returned -1
        if (!f.eof) throw new StdioException("Error in reading file"); // error occured
        return result;
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

    foreach (T; AliasSeq!(ubyte[]))
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
Iterates through a file a chunk at a time by using $(D foreach).

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

The content of $(D buffer) is reused across calls. In the
 example above, $(D buffer.length) is 4096 for all iterations,
 except for the last one, in which case $(D buffer.length) may
 be less than 4096 (but always greater than zero).

 In case of an I/O error, an $(D StdioException) is thrown.
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
    body
    {
        this.f = f;
        this.size = size;
    }

    int opApply(D)(scope D dg)
    {
        import core.stdc.stdlib : alloca;
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


/**
Writes an array or range to a file.
Shorthand for $(D data.copy(File(fileName, "wb").lockingBinaryWriter)).
Similar to $(REF write, std,file), strings are written as-is,
rather than encoded according to the $(D File)'s $(HTTP
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

/** Convenience functions that throw an $(D StdioException). */
    static void opCall(string msg)
    {
        throw new StdioException(msg);
    }

/// ditto
    static void opCall()
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
                impl.handle = mixin(_iob);
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
    Bugs:
        Due to $(LINK2 https://issues.dlang.org/show_bug.cgi?id=15768, bug 15768),
        it is thread un-safe to reassign `stdin` to a different `File` instance
        than the default.
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

    void main() {
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
    Bugs:
        Due to $(LINK2 https://issues.dlang.org/show_bug.cgi?id=15768, bug 15768),
        it is thread un-safe to reassign `stdout` to a different `File` instance
        than the default.
*/
alias stdout = makeGlobal!(StdFileHandle.stdout);

/**
    The standard error stream.
    Bugs:
        Due to $(LINK2 https://issues.dlang.org/show_bug.cgi?id=15768, bug 15768),
        it is thread un-safe to reassign `stderr` to a different `File` instance
        than the default.
*/
alias stderr = makeGlobal!(StdFileHandle.stderr);

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

    void initialize(char[] b)
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
    void putonly(char[] b) @trusted
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

// Private implementation of readln
version (DIGITAL_MARS_STDIO)
private size_t readlnImpl(FILE* fps, ref char[] buf, dchar terminator, File.Orientation /*ignored*/)
{
    FLOCK(fps);
    scope(exit) FUNLOCK(fps);

    /* Since fps is now locked, we can create an "unshared" version
     * of fp.
     */
    auto fp = cast(_iobuf*) fps;

    ReadlnAppender app;
    app.initialize(buf);

    if (__fhnd_info[fp._file] & FHND_WCHAR)
    {   /* Stream is in wide characters.
         * Read them and convert to chars.
         */
        static assert(wchar_t.sizeof == 2);
        for (int c = void; (c = FGETWC(fp)) != -1; )
        {
            if ((c & ~0x7F) == 0)
            {
                app.putchar(cast(char) c);
                if (c == terminator)
                    break;
            }
            else
            {
                if (c >= 0xD800 && c <= 0xDBFF)
                {
                    int c2 = void;
                    if ((c2 = FGETWC(fp)) != -1 ||
                            c2 < 0xDC00 && c2 > 0xDFFF)
                    {
                        StdioException("unpaired UTF-16 surrogate");
                    }
                    c = ((c - 0xD7C0) << 10) + (c2 - 0xDC00);
                }
                app.putdchar(cast(dchar) c);
            }
        }
        if (ferror(fps))
            StdioException();
    }

    else if (fp._flag & _IONBF)
    {
        /* Use this for unbuffered I/O, when running
         * across buffer boundaries, or for any but the common
         * cases.
         */
      L1:
        int c;
        while ((c = FGETC(fp)) != -1)
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
    }
    else
    {
        int u = fp._cnt;
        char* p = fp._ptr;
        int i;
        if (fp._flag & _IOTRAN)
        {   /* Translated mode ignores \r and treats ^Z as end-of-file
             */
            char c;
            while (1)
            {
                if (i == u)         // if end of buffer
                    goto L1;        // give up
                c = p[i];
                i++;
                if (c != '\r')
                {
                    if (c == terminator)
                        break;
                    if (c != 0x1A)
                        continue;
                    goto L1;
                }
                else
                {   if (i != u && p[i] == terminator)
                        break;
                    goto L1;
                }
            }
            app.putonly(p[0 .. i]);
            app.buf[i - 1] = cast(char) terminator;
            if (terminator == '\n' && c == '\r')
                i++;
        }
        else
        {
            while (1)
            {
                if (i == u)         // if end of buffer
                    goto L1;        // give up
                auto c = p[i];
                i++;
                if (c == terminator)
                    break;
            }
            app.putonly(p[0 .. i]);
        }
        fp._cnt -= i;
        fp._ptr += i;
    }

    buf = app.data;
    return buf.length;
}

version (MICROSOFT_STDIO)
private size_t readlnImpl(FILE* fps, ref char[] buf, dchar terminator, File.Orientation /*ignored*/)
{
    FLOCK(fps);
    scope(exit) FUNLOCK(fps);

    /* Since fps is now locked, we can create an "unshared" version
     * of fp.
     */
    auto fp = cast(_iobuf*) fps;

    ReadlnAppender app;
    app.initialize(buf);

    int c;
    while ((c = FGETC(fp)) != -1)
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

version (HAS_GETDELIM)
private size_t readlnImpl(FILE* fps, ref char[] buf, dchar terminator, File.Orientation orientation)
{
    import core.stdc.stdlib : free;
    import core.stdc.wchar_ : fwide;

    if (orientation == File.Orientation.wide)
    {
        /* Stream is in wide characters.
         * Read them and convert to chars.
         */
        FLOCK(fps);
        scope(exit) FUNLOCK(fps);
        auto fp = cast(_iobuf*) fps;
        version (Windows)
        {
            buf.length = 0;
            for (int c = void; (c = FGETWC(fp)) != -1; )
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
                        if ((c2 = FGETWC(fp)) != -1 ||
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
            if (ferror(fp))
                StdioException();
            return buf.length;
        }
        else version (Posix)
        {
            buf.length = 0;
            for (int c; (c = FGETWC(fp)) != -1; )
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

    auto s = getdelim(&lineptr, &n, terminator, fps);
    if (s < 0)
    {
        if (ferror(fps))
            StdioException();
        buf.length = 0;                // end of file
        return 0;
    }

    if (s <= buf.length)
    {
        buf = buf[0 .. s];
        buf[] = lineptr[0 .. s];
    }
    else
    {
        buf = lineptr[0 .. s].dup;
    }
    return s;
}

version (NO_GETDELIM)
private size_t readlnImpl(FILE* fps, ref char[] buf, dchar terminator, File.Orientation orientation)
{
    import core.stdc.wchar_ : fwide;

    FLOCK(fps);
    scope(exit) FUNLOCK(fps);
    auto fp = cast(_iobuf*) fps;
    if (orientation == File.Orientation.wide)
    {
        /* Stream is in wide characters.
         * Read them and convert to chars.
         */
        version (Windows)
        {
            buf.length = 0;
            for (int c; (c = FGETWC(fp)) != -1; )
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
                        if ((c2 = FGETWC(fp)) != -1 ||
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
            if (ferror(fp))
                StdioException();
            return buf.length;
        }
        else version (Posix)
        {
            import std.utf : encode;
            buf.length = 0;
            for (int c; (c = FGETWC(fp)) != -1; )
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
        immutable c = FGETC(fp);
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
    for (int c; (c = FGETC(fp)) != -1; )
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

@system unittest
{
    static import std.file;
    auto deleteme = testFilename();
    scope(exit) std.file.remove(deleteme);

    std.file.write(deleteme, "abcd\n0123456789abcde\n1234\n");
    File f = File(deleteme, "rb");

    char[] ln = new char[2];
    char* lnptr = ln.ptr;
    f.readln(ln);

    assert(ln == "abcd\n");
    char[] t = ln[0 .. 2];
    t ~= 't';
    assert(t == "abt");
    assert(ln == "abcd\n");  // bug 13856: ln stomped to "abtd"

    // it can also stomp the array length
    ln = new char[4];
    lnptr = ln.ptr;
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

version (unittest) string testFilename(string file = __FILE__, size_t line = __LINE__) @safe
{
    import std.conv : text;
    import std.file : deleteme;
    import std.path : baseName;

    // filename intentionally contains non-ASCII (Russian) characters for test Issue 7648
    return text(deleteme, "-детка.", baseName(file), ".", line);
}

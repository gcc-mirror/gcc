// Written in the D programming language.

/**
Utilities for manipulating files and scanning directories. Functions
in this module handle files as a unit, e.g., read or write one _file
at a time. For opening files and manipulating them via handles refer
to module $(MREF std, stdio).

$(SCRIPT inhibitQuickIndex = 1;)
$(BOOKTABLE,
$(TR $(TH Category) $(TH Functions))
$(TR $(TD General) $(TD
          $(LREF exists)
          $(LREF isDir)
          $(LREF isFile)
          $(LREF isSymlink)
          $(LREF rename)
          $(LREF thisExePath)
))
$(TR $(TD Directories) $(TD
          $(LREF chdir)
          $(LREF dirEntries)
          $(LREF getcwd)
          $(LREF mkdir)
          $(LREF mkdirRecurse)
          $(LREF rmdir)
          $(LREF rmdirRecurse)
          $(LREF tempDir)
))
$(TR $(TD Files) $(TD
          $(LREF append)
          $(LREF copy)
          $(LREF read)
          $(LREF readText)
          $(LREF remove)
          $(LREF slurp)
          $(LREF write)
))
$(TR $(TD Symlinks) $(TD
          $(LREF symlink)
          $(LREF readLink)
))
$(TR $(TD Attributes) $(TD
          $(LREF attrIsDir)
          $(LREF attrIsFile)
          $(LREF attrIsSymlink)
          $(LREF getAttributes)
          $(LREF getLinkAttributes)
          $(LREF getSize)
          $(LREF setAttributes)
))
$(TR $(TD Timestamp) $(TD
          $(LREF getTimes)
          $(LREF getTimesWin)
          $(LREF setTimes)
          $(LREF timeLastModified)
))
$(TR $(TD Other) $(TD
          $(LREF DirEntry)
          $(LREF FileException)
          $(LREF PreserveAttributes)
          $(LREF SpanMode)
))
)


Copyright: Copyright Digital Mars 2007 - 2011.
See_Also:  The $(HTTP ddili.org/ders/d.en/files.html, official tutorial) for an
introduction to working with files in D, module
$(MREF std, stdio) for opening files and manipulating them via handles,
and module $(MREF std, path) for manipulating path strings.

License:   $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   $(HTTP digitalmars.com, Walter Bright),
           $(HTTP erdani.org, Andrei Alexandrescu),
           Jonathan M Davis
Source:    $(PHOBOSSRC std/_file.d)
 */
module std.file;

import core.stdc.errno, core.stdc.stdlib, core.stdc.string;
import core.time : abs, dur, hnsecs, seconds;

import std.datetime.date : DateTime;
import std.datetime.systime : Clock, SysTime, unixTimeToStdTime;
import std.internal.cstring;
import std.meta;
import std.range.primitives;
import std.traits;
import std.typecons;

version (Windows)
{
    import core.sys.windows.windows, std.windows.syserror;
}
else version (Posix)
{
    import core.sys.posix.dirent, core.sys.posix.fcntl, core.sys.posix.sys.stat,
        core.sys.posix.sys.time, core.sys.posix.unistd, core.sys.posix.utime;
}
else
    static assert(false, "Module " ~ .stringof ~ " not implemented for this OS.");

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

// Purposefully not documented. Use at your own risk
@property string deleteme() @safe
{
    import std.conv : to;
    import std.path : buildPath;
    import std.process : thisProcessID;

    static _deleteme = "deleteme.dmd.unittest.pid";
    static _first = true;

    if (_first)
    {
        _deleteme = buildPath(tempDir(), _deleteme) ~ to!string(thisProcessID);
        _first = false;
    }

    return _deleteme;
}

version (unittest) private struct TestAliasedString
{
    string get() @safe @nogc pure nothrow { return _s; }
    alias get this;
    @disable this(this);
    string _s;
}

version (Android)
{
    package enum system_directory = "/system/etc";
    package enum system_file      = "/system/etc/hosts";
}
else version (Posix)
{
    package enum system_directory = "/usr/include";
    package enum system_file      = "/usr/include/assert.h";
}


/++
    Exception thrown for file I/O errors.
 +/
class FileException : Exception
{
    import std.conv : text, to;

    /++
        OS error code.
     +/
    immutable uint errno;

    private this(in char[] name, in char[] msg, string file, size_t line, uint errno) @safe pure
    {
        if (msg.empty)
            super(name.idup, file, line);
        else
            super(text(name, ": ", msg), file, line);

        this.errno = errno;
    }

    /++
        Constructor which takes an error message.

        Params:
            name = Name of file for which the error occurred.
            msg  = Message describing the error.
            file = The _file where the error occurred.
            line = The _line where the error occurred.
     +/
    this(in char[] name, in char[] msg, string file = __FILE__, size_t line = __LINE__) @safe pure
    {
        this(name, msg, file, line, 0);
    }

    /++
        Constructor which takes the error number ($(LUCKY GetLastError)
        in Windows, $(D_PARAM errno) in Posix).

        Params:
            name  = Name of file for which the error occurred.
            errno = The error number.
            file  = The _file where the error occurred.
                    Defaults to $(D __FILE__).
            line  = The _line where the error occurred.
                    Defaults to $(D __LINE__).
     +/
    version (Windows) this(in char[] name,
                          uint errno = .GetLastError(),
                          string file = __FILE__,
                          size_t line = __LINE__) @safe
    {
        this(name, sysErrorString(errno), file, line, errno);
    }
    else version (Posix) this(in char[] name,
                             uint errno = .errno,
                             string file = __FILE__,
                             size_t line = __LINE__) @trusted
    {
        import std.exception : errnoString;
        this(name, errnoString(errno), file, line, errno);
    }
}

private T cenforce(T)(T condition, lazy const(char)[] name, string file = __FILE__, size_t line = __LINE__)
{
    if (condition)
        return condition;
    version (Windows)
    {
        throw new FileException(name, .GetLastError(), file, line);
    }
    else version (Posix)
    {
        throw new FileException(name, .errno, file, line);
    }
}

version (Windows)
@trusted
private T cenforce(T)(T condition, const(char)[] name, const(FSChar)* namez,
    string file = __FILE__, size_t line = __LINE__)
{
    if (condition)
        return condition;
    if (!name)
    {
        import core.stdc.wchar_ : wcslen;
        import std.conv : to;

        auto len = namez ? wcslen(namez) : 0;
        name = to!string(namez[0 .. len]);
    }
    throw new FileException(name, .GetLastError(), file, line);
}

version (Posix)
@trusted
private T cenforce(T)(T condition, const(char)[] name, const(FSChar)* namez,
    string file = __FILE__, size_t line = __LINE__)
{
    if (condition)
        return condition;
    if (!name)
    {
        import core.stdc.string : strlen;

        auto len = namez ? strlen(namez) : 0;
        name = namez[0 .. len].idup;
    }
    throw new FileException(name, .errno, file, line);
}

@safe unittest
{
    // issue 17102
    try
    {
        cenforce(false, null, null,
                __FILE__, __LINE__);
    }
    catch (FileException) {}
}

/* **********************************
 * Basic File operations.
 */

/********************************************
Read entire contents of file $(D name) and returns it as an untyped
array. If the file size is larger than $(D upTo), only $(D upTo)
bytes are _read.

Params:
    name = string or range of characters representing the file _name
    upTo = if present, the maximum number of bytes to _read

Returns: Untyped array of bytes _read.

Throws: $(LREF FileException) on error.
 */

void[] read(R)(R name, size_t upTo = size_t.max)
if (isInputRange!R && isSomeChar!(ElementEncodingType!R) && !isInfinite!R &&
    !isConvertibleToString!R)
{
    static if (isNarrowString!R && is(Unqual!(ElementEncodingType!R) == char))
        return readImpl(name, name.tempCString!FSChar(), upTo);
    else
        return readImpl(null, name.tempCString!FSChar(), upTo);
}

///
@safe unittest
{
    import std.utf : byChar;
    scope(exit)
    {
        assert(exists(deleteme));
        remove(deleteme);
    }

    write(deleteme, "1234"); // deleteme is the name of a temporary file
    assert(read(deleteme, 2) == "12");
    assert(read(deleteme.byChar) == "1234");
    assert((cast(const(ubyte)[])read(deleteme)).length == 4);
}

/// ditto
void[] read(R)(auto ref R name, size_t upTo = size_t.max)
if (isConvertibleToString!R)
{
    return read!(StringTypeOf!R)(name, upTo);
}

@safe unittest
{
    static assert(__traits(compiles, read(TestAliasedString(null))));
}

version (Posix) private void[] readImpl(const(char)[] name, const(FSChar)* namez, size_t upTo = size_t.max) @trusted
{
    import core.memory : GC;
    import std.algorithm.comparison : min;
    import std.array : uninitializedArray;
    import std.conv : to;

    // A few internal configuration parameters {
    enum size_t
        minInitialAlloc = 1024 * 4,
        maxInitialAlloc = size_t.max / 2,
        sizeIncrement = 1024 * 16,
        maxSlackMemoryAllowed = 1024;
    // }

    immutable fd = core.sys.posix.fcntl.open(namez,
            core.sys.posix.fcntl.O_RDONLY);
    cenforce(fd != -1, name);
    scope(exit) core.sys.posix.unistd.close(fd);

    stat_t statbuf = void;
    cenforce(fstat(fd, &statbuf) == 0, name, namez);

    immutable initialAlloc = min(upTo, to!size_t(statbuf.st_size
        ? min(statbuf.st_size + 1, maxInitialAlloc)
        : minInitialAlloc));
    void[] result = uninitializedArray!(ubyte[])(initialAlloc);
    scope(failure) GC.free(result.ptr);
    size_t size = 0;

    for (;;)
    {
        immutable actual = core.sys.posix.unistd.read(fd, result.ptr + size,
                min(result.length, upTo) - size);
        cenforce(actual != -1, name, namez);
        if (actual == 0) break;
        size += actual;
        if (size >= upTo) break;
        if (size < result.length) continue;
        immutable newAlloc = size + sizeIncrement;
        result = GC.realloc(result.ptr, newAlloc, GC.BlkAttr.NO_SCAN)[0 .. newAlloc];
    }

    return result.length - size >= maxSlackMemoryAllowed
        ? GC.realloc(result.ptr, size, GC.BlkAttr.NO_SCAN)[0 .. size]
        : result[0 .. size];
}


version (Windows) private void[] readImpl(const(char)[] name, const(FSChar)* namez, size_t upTo = size_t.max) @safe
{
    import core.memory : GC;
    import std.algorithm.comparison : min;
    import std.array : uninitializedArray;
    static trustedCreateFileW(const(wchar)* namez, DWORD dwDesiredAccess, DWORD dwShareMode,
                              SECURITY_ATTRIBUTES *lpSecurityAttributes, DWORD dwCreationDisposition,
                              DWORD dwFlagsAndAttributes, HANDLE hTemplateFile) @trusted
    {
        return CreateFileW(namez, dwDesiredAccess, dwShareMode,
                           lpSecurityAttributes, dwCreationDisposition,
                           dwFlagsAndAttributes, hTemplateFile);

    }
    static trustedCloseHandle(HANDLE hObject) @trusted
    {
        return CloseHandle(hObject);
    }
    static trustedGetFileSize(HANDLE hFile, out ulong fileSize) @trusted
    {
        DWORD sizeHigh;
        DWORD sizeLow = GetFileSize(hFile, &sizeHigh);
        const bool result = sizeLow != INVALID_FILE_SIZE;
        if (result)
            fileSize = makeUlong(sizeLow, sizeHigh);
        return result;
    }
    static trustedReadFile(HANDLE hFile, void *lpBuffer, ulong nNumberOfBytesToRead) @trusted
    {
        // Read by chunks of size < 4GB (Windows API limit)
        ulong totalNumRead = 0;
        while (totalNumRead != nNumberOfBytesToRead)
        {
            const uint chunkSize = min(nNumberOfBytesToRead - totalNumRead, 0xffff_0000);
            DWORD numRead = void;
            const result = ReadFile(hFile, lpBuffer + totalNumRead, chunkSize, &numRead, null);
            if (result == 0 || numRead != chunkSize)
                return false;
            totalNumRead += chunkSize;
        }
        return true;
    }

    alias defaults =
        AliasSeq!(GENERIC_READ,
            FILE_SHARE_READ | FILE_SHARE_WRITE, (SECURITY_ATTRIBUTES*).init,
            OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN,
            HANDLE.init);
    auto h = trustedCreateFileW(namez, defaults);

    cenforce(h != INVALID_HANDLE_VALUE, name, namez);
    scope(exit) cenforce(trustedCloseHandle(h), name, namez);
    ulong fileSize = void;
    cenforce(trustedGetFileSize(h, fileSize), name, namez);
    size_t size = min(upTo, fileSize);
    auto buf = uninitializedArray!(ubyte[])(size);

    scope(failure)
    {
        () @trusted { GC.free(buf.ptr); } ();
    }

    if (size)
        cenforce(trustedReadFile(h, &buf[0], size), name, namez);
    return buf[0 .. size];
}

version (linux) @safe unittest
{
    // A file with "zero" length that doesn't have 0 length at all
    auto s = std.file.readText("/proc/sys/kernel/osrelease");
    assert(s.length > 0);
    //writefln("'%s'", s);
}

@safe unittest
{
    scope(exit) if (exists(deleteme)) remove(deleteme);
    import std.stdio;
    auto f = File(deleteme, "w");
    f.write("abcd"); f.flush();
    assert(read(deleteme) == "abcd");
}

/********************************************
Read and validates (using $(REF validate, std,utf)) a text file. $(D S)
can be a type of array of characters of any width and constancy. No
width conversion is performed; if the width of the characters in file
$(D name) is different from the width of elements of $(D S),
validation will fail.

Params:
    name = string or range of characters representing the file _name

Returns: Array of characters read.

Throws: $(D FileException) on file error, $(D UTFException) on UTF
decoding error.
 */

S readText(S = string, R)(R name)
if (isSomeString!S &&
    (isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R) || isSomeString!R) &&
    !isConvertibleToString!R)
{
    import std.utf : validate;
    static auto trustedCast(void[] buf) @trusted { return cast(S) buf; }
    auto result = trustedCast(read(name));
    validate(result);
    return result;
}

///
@safe unittest
{
    import std.exception : enforce;
    write(deleteme, "abc"); // deleteme is the name of a temporary file
    scope(exit) remove(deleteme);
    string content = readText(deleteme);
    enforce(content == "abc");
}

/// ditto
S readText(S = string, R)(auto ref R name)
if (isConvertibleToString!R)
{
    return readText!(S, StringTypeOf!R)(name);
}

@safe unittest
{
    static assert(__traits(compiles, readText(TestAliasedString(null))));
}

/*********************************************
Write $(D buffer) to file $(D name).

Creates the file if it does not already exist.

Params:
    name = string or range of characters representing the file _name
    buffer = data to be written to file

Throws: $(D FileException) on error.

See_also: $(REF toFile, std,stdio)
 */
void write(R)(R name, const void[] buffer)
if ((isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R) || isSomeString!R) &&
    !isConvertibleToString!R)
{
    static if (isNarrowString!R && is(Unqual!(ElementEncodingType!R) == char))
        writeImpl(name, name.tempCString!FSChar(), buffer, false);
    else
        writeImpl(null, name.tempCString!FSChar(), buffer, false);
}

///
@system unittest
{
   scope(exit)
   {
       assert(exists(deleteme));
       remove(deleteme);
   }

   int[] a = [ 0, 1, 1, 2, 3, 5, 8 ];
   write(deleteme, a); // deleteme is the name of a temporary file
   assert(cast(int[]) read(deleteme) == a);
}

/// ditto
void write(R)(auto ref R name, const void[] buffer)
if (isConvertibleToString!R)
{
    write!(StringTypeOf!R)(name, buffer);
}

@safe unittest
{
    static assert(__traits(compiles, write(TestAliasedString(null), null)));
}

/*********************************************
Appends $(D buffer) to file $(D name).

Creates the file if it does not already exist.

Params:
    name = string or range of characters representing the file _name
    buffer = data to be appended to file

Throws: $(D FileException) on error.
 */
void append(R)(R name, const void[] buffer)
if ((isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R) || isSomeString!R) &&
    !isConvertibleToString!R)
{
    static if (isNarrowString!R && is(Unqual!(ElementEncodingType!R) == char))
        writeImpl(name, name.tempCString!FSChar(), buffer, true);
    else
        writeImpl(null, name.tempCString!FSChar(), buffer, true);
}

///
@system unittest
{
   scope(exit)
   {
       assert(exists(deleteme));
       remove(deleteme);
   }

   int[] a = [ 0, 1, 1, 2, 3, 5, 8 ];
   write(deleteme, a); // deleteme is the name of a temporary file
   int[] b = [ 13, 21 ];
   append(deleteme, b);
   assert(cast(int[]) read(deleteme) == a ~ b);
}

/// ditto
void append(R)(auto ref R name, const void[] buffer)
if (isConvertibleToString!R)
{
    append!(StringTypeOf!R)(name, buffer);
}

@safe unittest
{
    static assert(__traits(compiles, append(TestAliasedString("foo"), [0, 1, 2, 3])));
}

// Posix implementation helper for write and append

version (Posix) private void writeImpl(const(char)[] name, const(FSChar)* namez,
        in void[] buffer, bool append) @trusted
{
    import std.conv : octal;

    // append or write
    auto mode = append ? O_CREAT | O_WRONLY | O_APPEND
                       : O_CREAT | O_WRONLY | O_TRUNC;

    immutable fd = core.sys.posix.fcntl.open(namez, mode, octal!666);
    cenforce(fd != -1, name, namez);
    {
        scope(failure) core.sys.posix.unistd.close(fd);

        immutable size = buffer.length;
        size_t sum, cnt = void;
        while (sum != size)
        {
            cnt = (size - sum < 2^^30) ? (size - sum) : 2^^30;
            const numwritten = core.sys.posix.unistd.write(fd, buffer.ptr + sum, cnt);
            if (numwritten != cnt)
                break;
            sum += numwritten;
        }
        cenforce(sum == size, name, namez);
    }
    cenforce(core.sys.posix.unistd.close(fd) == 0, name, namez);
}

// Windows implementation helper for write and append

version (Windows) private void writeImpl(const(char)[] name, const(FSChar)* namez,
        in void[] buffer, bool append) @trusted
{
    HANDLE h;
    if (append)
    {
        alias defaults =
            AliasSeq!(GENERIC_WRITE, 0, null, OPEN_ALWAYS,
                FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN,
                HANDLE.init);

        h = CreateFileW(namez, defaults);
        cenforce(h != INVALID_HANDLE_VALUE, name, namez);
        cenforce(SetFilePointer(h, 0, null, FILE_END) != INVALID_SET_FILE_POINTER,
            name, namez);
    }
    else // write
    {
        alias defaults =
            AliasSeq!(GENERIC_WRITE, 0, null, CREATE_ALWAYS,
                FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN,
                HANDLE.init);

        h = CreateFileW(namez, defaults);
        cenforce(h != INVALID_HANDLE_VALUE, name, namez);
    }
    immutable size = buffer.length;
    size_t sum, cnt = void;
    DWORD numwritten = void;
    while (sum != size)
    {
        cnt = (size - sum < 2^^30) ? (size - sum) : 2^^30;
        WriteFile(h, buffer.ptr + sum, cast(uint) cnt, &numwritten, null);
        if (numwritten != cnt)
            break;
        sum += numwritten;
    }
    cenforce(sum == size && CloseHandle(h), name, namez);
}

/***************************************************
 * Rename file $(D from) _to $(D to).
 * If the target file exists, it is overwritten.
 * Params:
 *    from = string or range of characters representing the existing file name
 *    to = string or range of characters representing the target file name
 * Throws: $(D FileException) on error.
 */
void rename(RF, RT)(RF from, RT to)
if ((isInputRange!RF && !isInfinite!RF && isSomeChar!(ElementEncodingType!RF) || isSomeString!RF)
    && !isConvertibleToString!RF &&
    (isInputRange!RT && !isInfinite!RT && isSomeChar!(ElementEncodingType!RT) || isSomeString!RT)
    && !isConvertibleToString!RT)
{
    // Place outside of @trusted block
    auto fromz = from.tempCString!FSChar();
    auto toz = to.tempCString!FSChar();

    static if (isNarrowString!RF && is(Unqual!(ElementEncodingType!RF) == char))
        alias f = from;
    else
        enum string f = null;

    static if (isNarrowString!RT && is(Unqual!(ElementEncodingType!RT) == char))
        alias t = to;
    else
        enum string t = null;

    renameImpl(f, t, fromz, toz);
}

/// ditto
void rename(RF, RT)(auto ref RF from, auto ref RT to)
if (isConvertibleToString!RF || isConvertibleToString!RT)
{
    import std.meta : staticMap;
    alias Types = staticMap!(convertToString, RF, RT);
    rename!Types(from, to);
}

@safe unittest
{
    static assert(__traits(compiles, rename(TestAliasedString(null), TestAliasedString(null))));
    static assert(__traits(compiles, rename("", TestAliasedString(null))));
    static assert(__traits(compiles, rename(TestAliasedString(null), "")));
    import std.utf : byChar;
    static assert(__traits(compiles, rename(TestAliasedString(null), "".byChar)));
}

private void renameImpl(const(char)[] f, const(char)[] t, const(FSChar)* fromz, const(FSChar)* toz) @trusted
{
    version (Windows)
    {
        import std.exception : enforce;

        const result = MoveFileExW(fromz, toz, MOVEFILE_REPLACE_EXISTING);
        if (!result)
        {
            import core.stdc.wchar_ : wcslen;
            import std.conv : to, text;

            if (!f)
                f = to!(typeof(f))(fromz[0 .. wcslen(fromz)]);

            if (!t)
                t = to!(typeof(t))(toz[0 .. wcslen(toz)]);

            enforce(false,
                new FileException(
                    text("Attempting to rename file ", f, " to ", t)));
        }
    }
    else version (Posix)
    {
        static import core.stdc.stdio;

        cenforce(core.stdc.stdio.rename(fromz, toz) == 0, t, toz);
    }
}

@safe unittest
{
    import std.utf : byWchar;

    auto t1 = deleteme, t2 = deleteme~"2";
    scope(exit) foreach (t; [t1, t2]) if (t.exists) t.remove();
    write(t1, "1");
    rename(t1, t2);
    assert(readText(t2) == "1");
    write(t1, "2");
    rename(t1, t2.byWchar);
    assert(readText(t2) == "2");
}


/***************************************************
Delete file $(D name).

Params:
    name = string or range of characters representing the file _name

Throws: $(D FileException) on error.
 */
void remove(R)(R name)
if (isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R) &&
    !isConvertibleToString!R)
{
    static if (isNarrowString!R && is(Unqual!(ElementEncodingType!R) == char))
        removeImpl(name, name.tempCString!FSChar());
    else
        removeImpl(null, name.tempCString!FSChar());
}

/// ditto
void remove(R)(auto ref R name)
if (isConvertibleToString!R)
{
    remove!(StringTypeOf!R)(name);
}

@safe unittest
{
    static assert(__traits(compiles, remove(TestAliasedString("foo"))));
}

private void removeImpl(const(char)[] name, const(FSChar)* namez) @trusted
{
    version (Windows)
    {
        cenforce(DeleteFileW(namez), name, namez);
    }
    else version (Posix)
    {
        static import core.stdc.stdio;

        if (!name)
        {
            import core.stdc.string : strlen;
            auto len = strlen(namez);
            name = namez[0 .. len];
        }
        cenforce(core.stdc.stdio.remove(namez) == 0,
            "Failed to remove file " ~ name);
    }
}

version (Windows) private WIN32_FILE_ATTRIBUTE_DATA getFileAttributesWin(R)(R name)
if (isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R))
{
    auto namez = name.tempCString!FSChar();

    WIN32_FILE_ATTRIBUTE_DATA fad = void;

    static if (isNarrowString!R && is(Unqual!(ElementEncodingType!R) == char))
    {
        static void getFA(const(char)[] name, const(FSChar)* namez, out WIN32_FILE_ATTRIBUTE_DATA fad) @trusted
        {
            import std.exception : enforce;
            enforce(GetFileAttributesExW(namez, GET_FILEEX_INFO_LEVELS.GetFileExInfoStandard, &fad),
                new FileException(name.idup));
        }
        getFA(name, namez, fad);
    }
    else
    {
        static void getFA(const(FSChar)* namez, out WIN32_FILE_ATTRIBUTE_DATA fad) @trusted
        {
            import core.stdc.wchar_ : wcslen;
            import std.conv : to;
            import std.exception : enforce;

            enforce(GetFileAttributesExW(namez, GET_FILEEX_INFO_LEVELS.GetFileExInfoStandard, &fad),
                new FileException(namez[0 .. wcslen(namez)].to!string));
        }
        getFA(namez, fad);
    }
    return fad;
}

version (Windows) private ulong makeUlong(DWORD dwLow, DWORD dwHigh) @safe pure nothrow @nogc
{
    ULARGE_INTEGER li;
    li.LowPart  = dwLow;
    li.HighPart = dwHigh;
    return li.QuadPart;
}

/***************************************************
Get size of file $(D name) in bytes.

Params:
    name = string or range of characters representing the file _name

Throws: $(D FileException) on error (e.g., file not found).
 */
ulong getSize(R)(R name)
if (isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R) &&
    !isConvertibleToString!R)
{
    version (Windows)
    {
        with (getFileAttributesWin(name))
            return makeUlong(nFileSizeLow, nFileSizeHigh);
    }
    else version (Posix)
    {
        auto namez = name.tempCString();

        static trustedStat(const(FSChar)* namez, out stat_t buf) @trusted
        {
            return stat(namez, &buf);
        }
        static if (isNarrowString!R && is(Unqual!(ElementEncodingType!R) == char))
            alias names = name;
        else
            string names = null;
        stat_t statbuf = void;
        cenforce(trustedStat(namez, statbuf) == 0, names, namez);
        return statbuf.st_size;
    }
}

/// ditto
ulong getSize(R)(auto ref R name)
if (isConvertibleToString!R)
{
    return getSize!(StringTypeOf!R)(name);
}

@safe unittest
{
    static assert(__traits(compiles, getSize(TestAliasedString("foo"))));
}

@safe unittest
{
    // create a file of size 1
    write(deleteme, "a");
    scope(exit) { assert(exists(deleteme)); remove(deleteme); }
    assert(getSize(deleteme) == 1);
    // create a file of size 3
    write(deleteme, "abc");
    import std.utf : byChar;
    assert(getSize(deleteme.byChar) == 3);
}


// Reads a time field from a stat_t with full precision.
version (Posix)
private SysTime statTimeToStdTime(char which)(ref stat_t statbuf)
{
    auto unixTime = mixin(`statbuf.st_` ~ which ~ `time`);
    long stdTime = unixTimeToStdTime(unixTime);

    static if (is(typeof(mixin(`statbuf.st_` ~ which ~ `tim`))))
        stdTime += mixin(`statbuf.st_` ~ which ~ `tim.tv_nsec`) / 100;
    else
    static if (is(typeof(mixin(`statbuf.st_` ~ which ~ `timensec`))))
        stdTime += mixin(`statbuf.st_` ~ which ~ `timensec`) / 100;
    else
    static if (is(typeof(mixin(`statbuf.st_` ~ which ~ `time_nsec`))))
        stdTime += mixin(`statbuf.st_` ~ which ~ `time_nsec`) / 100;
    else
    static if (is(typeof(mixin(`statbuf.__st_` ~ which ~ `timensec`))))
        stdTime += mixin(`statbuf.__st_` ~ which ~ `timensec`) / 100;

    return SysTime(stdTime);
}

/++
    Get the access and modified times of file or folder $(D name).

    Params:
        name             = File/Folder _name to get times for.
        accessTime       = Time the file/folder was last accessed.
        modificationTime = Time the file/folder was last modified.

    Throws:
        $(D FileException) on error.
 +/
void getTimes(R)(R name,
              out SysTime accessTime,
              out SysTime modificationTime)
if (isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R) &&
    !isConvertibleToString!R)
{
    version (Windows)
    {
        import std.datetime.systime : FILETIMEToSysTime;

        with (getFileAttributesWin(name))
        {
            accessTime = FILETIMEToSysTime(&ftLastAccessTime);
            modificationTime = FILETIMEToSysTime(&ftLastWriteTime);
        }
    }
    else version (Posix)
    {
        auto namez = name.tempCString();

        static auto trustedStat(const(FSChar)* namez, ref stat_t buf) @trusted
        {
            return stat(namez, &buf);
        }
        stat_t statbuf = void;

        static if (isNarrowString!R && is(Unqual!(ElementEncodingType!R) == char))
            alias names = name;
        else
            string names = null;
        cenforce(trustedStat(namez, statbuf) == 0, names, namez);

        accessTime = statTimeToStdTime!'a'(statbuf);
        modificationTime = statTimeToStdTime!'m'(statbuf);
    }
}

/// ditto
void getTimes(R)(auto ref R name,
              out SysTime accessTime,
              out SysTime modificationTime)
if (isConvertibleToString!R)
{
    return getTimes!(StringTypeOf!R)(name, accessTime, modificationTime);
}

@safe unittest
{
    SysTime atime, mtime;
    static assert(__traits(compiles, getTimes(TestAliasedString("foo"), atime, mtime)));
}

@system unittest
{
    import std.stdio : writefln;

    auto currTime = Clock.currTime();

    write(deleteme, "a");
    scope(exit) { assert(exists(deleteme)); remove(deleteme); }

    SysTime accessTime1 = void;
    SysTime modificationTime1 = void;

    getTimes(deleteme, accessTime1, modificationTime1);

    enum leeway = dur!"seconds"(5);

    {
        auto diffa = accessTime1 - currTime;
        auto diffm = modificationTime1 - currTime;
        scope(failure) writefln("[%s] [%s] [%s] [%s] [%s]", accessTime1, modificationTime1, currTime, diffa, diffm);

        assert(abs(diffa) <= leeway);
        assert(abs(diffm) <= leeway);
    }

    version (fullFileTests)
    {
        import core.thread;
        enum sleepTime = dur!"seconds"(2);
        Thread.sleep(sleepTime);

        currTime = Clock.currTime();
        write(deleteme, "b");

        SysTime accessTime2 = void;
        SysTime modificationTime2 = void;

        getTimes(deleteme, accessTime2, modificationTime2);

        {
            auto diffa = accessTime2 - currTime;
            auto diffm = modificationTime2 - currTime;
            scope(failure) writefln("[%s] [%s] [%s] [%s] [%s]", accessTime2, modificationTime2, currTime, diffa, diffm);

            //There is no guarantee that the access time will be updated.
            assert(abs(diffa) <= leeway + sleepTime);
            assert(abs(diffm) <= leeway);
        }

        assert(accessTime1 <= accessTime2);
        assert(modificationTime1 <= modificationTime2);
    }
}


version (StdDdoc)
{
    /++
     $(BLUE This function is Windows-Only.)

     Get creation/access/modified times of file $(D name).

     This is the same as $(D getTimes) except that it also gives you the file
     creation time - which isn't possible on Posix systems.

     Params:
     name                 = File _name to get times for.
     fileCreationTime     = Time the file was created.
     fileAccessTime       = Time the file was last accessed.
     fileModificationTime = Time the file was last modified.

     Throws:
     $(D FileException) on error.
     +/
    void getTimesWin(R)(R name,
                        out SysTime fileCreationTime,
                        out SysTime fileAccessTime,
                        out SysTime fileModificationTime)
    if (isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R) &&
        !isConvertibleToString!R);
}
else version (Windows)
{
    void getTimesWin(R)(R name,
                        out SysTime fileCreationTime,
                        out SysTime fileAccessTime,
                        out SysTime fileModificationTime)
    if (isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R) &&
        !isConvertibleToString!R)
    {
        import std.datetime.systime : FILETIMEToSysTime;

        with (getFileAttributesWin(name))
        {
            fileCreationTime = FILETIMEToSysTime(&ftCreationTime);
            fileAccessTime = FILETIMEToSysTime(&ftLastAccessTime);
            fileModificationTime = FILETIMEToSysTime(&ftLastWriteTime);
        }
    }

    void getTimesWin(R)(auto ref R name,
                        out SysTime fileCreationTime,
                        out SysTime fileAccessTime,
                        out SysTime fileModificationTime)
    if (isConvertibleToString!R)
    {
        getTimesWin!(StringTypeOf!R)(name, fileCreationTime, fileAccessTime, fileModificationTime);
    }
}

version (Windows) @system unittest
{
    import std.stdio : writefln;
    auto currTime = Clock.currTime();

    write(deleteme, "a");
    scope(exit) { assert(exists(deleteme)); remove(deleteme); }

    SysTime creationTime1 = void;
    SysTime accessTime1 = void;
    SysTime modificationTime1 = void;

    getTimesWin(deleteme, creationTime1, accessTime1, modificationTime1);

    enum leeway = dur!"seconds"(5);

    {
        auto diffc = creationTime1 - currTime;
        auto diffa = accessTime1 - currTime;
        auto diffm = modificationTime1 - currTime;
        scope(failure)
        {
            writefln("[%s] [%s] [%s] [%s] [%s] [%s] [%s]",
                     creationTime1, accessTime1, modificationTime1, currTime, diffc, diffa, diffm);
        }

        // Deleting and recreating a file doesn't seem to always reset the "file creation time"
        //assert(abs(diffc) <= leeway);
        assert(abs(diffa) <= leeway);
        assert(abs(diffm) <= leeway);
    }

    version (fullFileTests)
    {
        import core.thread;
        Thread.sleep(dur!"seconds"(2));

        currTime = Clock.currTime();
        write(deleteme, "b");

        SysTime creationTime2 = void;
        SysTime accessTime2 = void;
        SysTime modificationTime2 = void;

        getTimesWin(deleteme, creationTime2, accessTime2, modificationTime2);

        {
            auto diffa = accessTime2 - currTime;
            auto diffm = modificationTime2 - currTime;
            scope(failure)
            {
                writefln("[%s] [%s] [%s] [%s] [%s]",
                         accessTime2, modificationTime2, currTime, diffa, diffm);
            }

            assert(abs(diffa) <= leeway);
            assert(abs(diffm) <= leeway);
        }

        assert(creationTime1 == creationTime2);
        assert(accessTime1 <= accessTime2);
        assert(modificationTime1 <= modificationTime2);
    }

    {
        SysTime ctime, atime, mtime;
        static assert(__traits(compiles, getTimesWin(TestAliasedString("foo"), ctime, atime, mtime)));
    }
}


/++
    Set access/modified times of file or folder $(D name).

    Params:
        name             = File/Folder _name to get times for.
        accessTime       = Time the file/folder was last accessed.
        modificationTime = Time the file/folder was last modified.

    Throws:
        $(D FileException) on error.
 +/
void setTimes(R)(R name,
              SysTime accessTime,
              SysTime modificationTime)
if (isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R) &&
    !isConvertibleToString!R)
{
    version (Windows)
    {
        import std.datetime.systime : SysTimeToFILETIME;

        auto namez = name.tempCString!FSChar();
        static auto trustedCreateFileW(const(FSChar)* namez, DWORD dwDesiredAccess, DWORD dwShareMode,
                                       SECURITY_ATTRIBUTES *lpSecurityAttributes, DWORD dwCreationDisposition,
                                       DWORD dwFlagsAndAttributes, HANDLE hTemplateFile) @trusted
        {
            return CreateFileW(namez, dwDesiredAccess, dwShareMode,
                               lpSecurityAttributes, dwCreationDisposition,
                               dwFlagsAndAttributes, hTemplateFile);

        }
        static auto trustedCloseHandle(HANDLE hObject) @trusted
        {
            return CloseHandle(hObject);
        }
        static auto trustedSetFileTime(HANDLE hFile, in FILETIME *lpCreationTime,
                                       in ref FILETIME lpLastAccessTime, in ref FILETIME lpLastWriteTime) @trusted
        {
            return SetFileTime(hFile, lpCreationTime, &lpLastAccessTime, &lpLastWriteTime);
        }

        const ta = SysTimeToFILETIME(accessTime);
        const tm = SysTimeToFILETIME(modificationTime);
        alias defaults =
            AliasSeq!(GENERIC_WRITE,
                      0,
                      null,
                      OPEN_EXISTING,
                      FILE_ATTRIBUTE_NORMAL |
                      FILE_ATTRIBUTE_DIRECTORY |
                      FILE_FLAG_BACKUP_SEMANTICS,
                      HANDLE.init);
        auto h = trustedCreateFileW(namez, defaults);

        static if (isNarrowString!R && is(Unqual!(ElementEncodingType!R) == char))
            alias names = name;
        else
            string names = null;
        cenforce(h != INVALID_HANDLE_VALUE, names, namez);

        scope(exit)
            cenforce(trustedCloseHandle(h), names, namez);

        cenforce(trustedSetFileTime(h, null, ta, tm), names, namez);
    }
    else version (Posix)
    {
        auto namez = name.tempCString!FSChar();
        static if (is(typeof(&utimensat)))
        {
            static auto trustedUtimensat(int fd, const(FSChar)* namez, const ref timespec[2] times, int flags) @trusted
            {
                return utimensat(fd, namez, times, flags);
            }
            timespec[2] t = void;

            t[0] = accessTime.toTimeSpec();
            t[1] = modificationTime.toTimeSpec();

            static if (isNarrowString!R && is(Unqual!(ElementEncodingType!R) == char))
                alias names = name;
            else
                string names = null;
            cenforce(trustedUtimensat(AT_FDCWD, namez, t, 0) == 0, names, namez);
        }
        else
        {
            static auto trustedUtimes(const(FSChar)* namez, const ref timeval[2] times) @trusted
            {
                return utimes(namez, times);
            }
            timeval[2] t = void;

            t[0] = accessTime.toTimeVal();
            t[1] = modificationTime.toTimeVal();

            static if (isNarrowString!R && is(Unqual!(ElementEncodingType!R) == char))
                alias names = name;
            else
                string names = null;
            cenforce(trustedUtimes(namez, t) == 0, names, namez);
        }
    }
}

/// ditto
void setTimes(R)(auto ref R name,
              SysTime accessTime,
              SysTime modificationTime)
if (isConvertibleToString!R)
{
    setTimes!(StringTypeOf!R)(name, accessTime, modificationTime);
}

@safe unittest
{
    if (false) // Test instatiation
        setTimes(TestAliasedString("foo"), SysTime.init, SysTime.init);
}

@system unittest
{
    import std.stdio : File;
    string newdir = deleteme ~ r".dir";
    string dir = newdir ~ r"/a/b/c";
    string file = dir ~ "/file";

    if (!exists(dir)) mkdirRecurse(dir);
    { auto f = File(file, "w"); }

    void testTimes(int hnsecValue)
    {
        foreach (path; [file, dir])  // test file and dir
        {
            SysTime atime = SysTime(DateTime(2010, 10, 4, 0, 0, 30), hnsecs(hnsecValue));
            SysTime mtime = SysTime(DateTime(2011, 10, 4, 0, 0, 30), hnsecs(hnsecValue));
            setTimes(path, atime, mtime);

            SysTime atime_res;
            SysTime mtime_res;
            getTimes(path, atime_res, mtime_res);
            assert(atime == atime_res);
            assert(mtime == mtime_res);
        }
    }

    testTimes(0);
    version (linux)
        testTimes(123_456_7);

    rmdirRecurse(newdir);
}

/++
    Returns the time that the given file was last modified.

    Throws:
        $(D FileException) if the given file does not exist.
+/
SysTime timeLastModified(R)(R name)
if (isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R) &&
    !isConvertibleToString!R)
{
    version (Windows)
    {
        SysTime dummy;
        SysTime ftm;

        getTimesWin(name, dummy, dummy, ftm);

        return ftm;
    }
    else version (Posix)
    {
        auto namez = name.tempCString!FSChar();
        static auto trustedStat(const(FSChar)* namez, ref stat_t buf) @trusted
        {
            return stat(namez, &buf);
        }
        stat_t statbuf = void;

        static if (isNarrowString!R && is(Unqual!(ElementEncodingType!R) == char))
            alias names = name;
        else
            string names = null;
        cenforce(trustedStat(namez, statbuf) == 0, names, namez);

        return statTimeToStdTime!'m'(statbuf);
    }
}

/// ditto
SysTime timeLastModified(R)(auto ref R name)
if (isConvertibleToString!R)
{
    return timeLastModified!(StringTypeOf!R)(name);
}

@safe unittest
{
    static assert(__traits(compiles, timeLastModified(TestAliasedString("foo"))));
}

/++
    Returns the time that the given file was last modified. If the
    file does not exist, returns $(D returnIfMissing).

    A frequent usage pattern occurs in build automation tools such as
    $(HTTP gnu.org/software/make, make) or $(HTTP
    en.wikipedia.org/wiki/Apache_Ant, ant). To check whether file $(D
    target) must be rebuilt from file $(D source) (i.e., $(D target) is
    older than $(D source) or does not exist), use the comparison
    below. The code throws a $(D FileException) if $(D source) does not
    exist (as it should). On the other hand, the $(D SysTime.min) default
    makes a non-existing $(D target) seem infinitely old so the test
    correctly prompts building it.

    Params:
        name            = The _name of the file to get the modification time for.
        returnIfMissing = The time to return if the given file does not exist.

Example:
--------------------
if (timeLastModified(source) >= timeLastModified(target, SysTime.min))
{
    // must (re)build
}
else
{
    // target is up-to-date
}
--------------------
+/
SysTime timeLastModified(R)(R name, SysTime returnIfMissing)
if (isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R))
{
    version (Windows)
    {
        if (!exists(name))
            return returnIfMissing;

        SysTime dummy;
        SysTime ftm;

        getTimesWin(name, dummy, dummy, ftm);

        return ftm;
    }
    else version (Posix)
    {
        auto namez = name.tempCString!FSChar();
        static auto trustedStat(const(FSChar)* namez, ref stat_t buf) @trusted
        {
            return stat(namez, &buf);
        }
        stat_t statbuf = void;

        return trustedStat(namez, statbuf) != 0 ?
               returnIfMissing :
               statTimeToStdTime!'m'(statbuf);
    }
}

@safe unittest
{
    //std.process.system("echo a > deleteme") == 0 || assert(false);
    if (exists(deleteme))
        remove(deleteme);

    write(deleteme, "a\n");

    scope(exit)
    {
        assert(exists(deleteme));
        remove(deleteme);
    }

    // assert(lastModified("deleteme") >
    //         lastModified("this file does not exist", SysTime.min));
    //assert(lastModified("deleteme") > lastModified(__FILE__));
}


// Tests sub-second precision of querying file times.
// Should pass on most modern systems running on modern filesystems.
// Exceptions:
// - FreeBSD, where one would need to first set the
//   vfs.timestamp_precision sysctl to a value greater than zero.
// - OS X, where the native filesystem (HFS+) stores filesystem
//   timestamps with 1-second precision.
//
// Note: on linux systems, although in theory a change to a file date
// can be tracked with precision of 4 msecs, this test waits 20 msecs
// to prevent possible problems relative to the CI services the dlang uses,
// as they may have the HZ setting that controls the software clock set to 100
// (instead of the more common 250).
// see https://man7.org/linux/man-pages/man7/time.7.html
//     https://stackoverflow.com/a/14393315,
//     https://issues.dlang.org/show_bug.cgi?id=21148
version (FreeBSD) {} else
version (DragonFlyBSD) {} else
version (OSX) {} else
@system unittest
{
    import core.thread;

    if (exists(deleteme))
        remove(deleteme);

    SysTime lastTime;
    foreach (n; 0 .. 3)
    {
        write(deleteme, "a");
        auto time = timeLastModified(deleteme);
        remove(deleteme);
        assert(time != lastTime);
        lastTime = time;
        Thread.sleep(20.msecs);
    }
}


/**
 * Determine whether the given file (or directory) _exists.
 * Params:
 *    name = string or range of characters representing the file _name
 * Returns:
 *    true if the file _name specified as input _exists
 */
bool exists(R)(R name)
if (isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R) &&
    !isConvertibleToString!R)
{
    return existsImpl(name.tempCString!FSChar());
}

/// ditto
bool exists(R)(auto ref R name)
if (isConvertibleToString!R)
{
    return exists!(StringTypeOf!R)(name);
}

private bool existsImpl(const(FSChar)* namez) @trusted nothrow @nogc
{
    version (Windows)
    {
        // http://msdn.microsoft.com/library/default.asp?url=/library/en-us/
        // fileio/base/getfileattributes.asp
        return GetFileAttributesW(namez) != 0xFFFFFFFF;
    }
    else version (Posix)
    {
        /*
            The reason why we use stat (and not access) here is
            the quirky behavior of access for SUID programs: if
            we used access, a file may not appear to "exist",
            despite that the program would be able to open it
            just fine. The behavior in question is described as
            follows in the access man page:

            > The check is done using the calling process's real
            > UID and GID, rather than the effective IDs as is
            > done when actually attempting an operation (e.g.,
            > open(2)) on the file. This allows set-user-ID
            > programs to easily determine the invoking user's
            > authority.

            While various operating systems provide eaccess or
            euidaccess functions, these are not part of POSIX -
            so it's safer to use stat instead.
        */

        stat_t statbuf = void;
        return lstat(namez, &statbuf) == 0;
    }
    else
        static assert(0);
}

@safe unittest
{
    assert(exists("."));
    assert(!exists("this file does not exist"));
    write(deleteme, "a\n");
    scope(exit) { assert(exists(deleteme)); remove(deleteme); }
    assert(exists(deleteme));
}

@safe unittest // Bugzilla 16573
{
    enum S : string { foo = "foo" }
    assert(__traits(compiles, S.foo.exists));
}

/++
 Returns the attributes of the given file.

 Note that the file attributes on Windows and Posix systems are
 completely different. On Windows, they're what is returned by
 $(HTTP msdn.microsoft.com/en-us/library/aa364944(v=vs.85).aspx,
 GetFileAttributes), whereas on Posix systems, they're the $(LUCKY
 st_mode) value which is part of the $(D stat struct) gotten by
 calling the $(HTTP en.wikipedia.org/wiki/Stat_%28Unix%29, $(D stat))
 function.

 On Posix systems, if the given file is a symbolic link, then
 attributes are the attributes of the file pointed to by the symbolic
 link.

 Params:
 name = The file to get the attributes of.

 Throws: $(D FileException) on error.
  +/
uint getAttributes(R)(R name)
if (isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R) &&
    !isConvertibleToString!R)
{
    version (Windows)
    {
        auto namez = name.tempCString!FSChar();
        static auto trustedGetFileAttributesW(const(FSChar)* namez) @trusted
        {
            return GetFileAttributesW(namez);
        }
        immutable result = trustedGetFileAttributesW(namez);

        static if (isNarrowString!R && is(Unqual!(ElementEncodingType!R) == char))
            alias names = name;
        else
            string names = null;
        cenforce(result != INVALID_FILE_ATTRIBUTES, names, namez);

        return result;
    }
    else version (Posix)
    {
        auto namez = name.tempCString!FSChar();
        static auto trustedStat(const(FSChar)* namez, ref stat_t buf) @trusted
        {
            return stat(namez, &buf);
        }
        stat_t statbuf = void;

        static if (isNarrowString!R && is(Unqual!(ElementEncodingType!R) == char))
            alias names = name;
        else
            string names = null;
        cenforce(trustedStat(namez, statbuf) == 0, names, namez);

        return statbuf.st_mode;
    }
}

/// ditto
uint getAttributes(R)(auto ref R name)
if (isConvertibleToString!R)
{
    return getAttributes!(StringTypeOf!R)(name);
}

@safe unittest
{
    static assert(__traits(compiles, getAttributes(TestAliasedString(null))));
}

/++
    If the given file is a symbolic link, then this returns the attributes of the
    symbolic link itself rather than file that it points to. If the given file
    is $(I not) a symbolic link, then this function returns the same result
    as getAttributes.

    On Windows, getLinkAttributes is identical to getAttributes. It exists on
    Windows so that you don't have to special-case code for Windows when dealing
    with symbolic links.

    Params:
        name = The file to get the symbolic link attributes of.

    Returns:
        the attributes

    Throws:
        $(D FileException) on error.
 +/
uint getLinkAttributes(R)(R name)
if (isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R) &&
    !isConvertibleToString!R)
{
    version (Windows)
    {
        return getAttributes(name);
    }
    else version (Posix)
    {
        auto namez = name.tempCString!FSChar();
        static auto trustedLstat(const(FSChar)* namez, ref stat_t buf) @trusted
        {
            return lstat(namez, &buf);
        }
        stat_t lstatbuf = void;
        static if (isNarrowString!R && is(Unqual!(ElementEncodingType!R) == char))
            alias names = name;
        else
            string names = null;
        cenforce(trustedLstat(namez, lstatbuf) == 0, names, namez);
        return lstatbuf.st_mode;
    }
}

/// ditto
uint getLinkAttributes(R)(auto ref R name)
if (isConvertibleToString!R)
{
    return getLinkAttributes!(StringTypeOf!R)(name);
}

@safe unittest
{
    static assert(__traits(compiles, getLinkAttributes(TestAliasedString(null))));
}

/++
    Set the _attributes of the given file.

    Params:
        name = the file _name
        attributes = the _attributes to set the file to

    Throws:
        $(D FileException) if the given file does not exist.
 +/
void setAttributes(R)(R name, uint attributes)
if (isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R) &&
    !isConvertibleToString!R)
{
    version (Windows)
    {
        auto namez = name.tempCString!FSChar();
        static auto trustedSetFileAttributesW(const(FSChar)* namez, uint dwFileAttributes) @trusted
        {
            return SetFileAttributesW(namez, dwFileAttributes);
        }
        static if (isNarrowString!R && is(Unqual!(ElementEncodingType!R) == char))
            alias names = name;
        else
            string names = null;
        cenforce(trustedSetFileAttributesW(namez, attributes), names, namez);
    }
    else version (Posix)
    {
        auto namez = name.tempCString!FSChar();
        static auto trustedChmod(const(FSChar)* namez, mode_t mode) @trusted
        {
            return chmod(namez, mode);
        }
        assert(attributes <= mode_t.max);
        static if (isNarrowString!R && is(Unqual!(ElementEncodingType!R) == char))
            alias names = name;
        else
            string names = null;
        cenforce(!trustedChmod(namez, cast(mode_t) attributes), names, namez);
    }
}

/// ditto
void setAttributes(R)(auto ref R name, uint attributes)
if (isConvertibleToString!R)
{
    return setAttributes!(StringTypeOf!R)(name, attributes);
}

@safe unittest
{
    static assert(__traits(compiles, setAttributes(TestAliasedString(null), 0)));
}

/++
    Returns whether the given file is a directory.

    Params:
        name = The path to the file.

    Returns:
        true if name specifies a directory

    Throws:
        $(D FileException) if the given file does not exist.

Example:
--------------------
assert(!"/etc/fonts/fonts.conf".isDir);
assert("/usr/share/include".isDir);
--------------------
  +/
@property bool isDir(R)(R name)
if (isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R) &&
    !isConvertibleToString!R)
{
    version (Windows)
    {
        return (getAttributes(name) & FILE_ATTRIBUTE_DIRECTORY) != 0;
    }
    else version (Posix)
    {
        return (getAttributes(name) & S_IFMT) == S_IFDIR;
    }
}

/// ditto
@property bool isDir(R)(auto ref R name)
if (isConvertibleToString!R)
{
    return name.isDir!(StringTypeOf!R);
}

@safe unittest
{
    static assert(__traits(compiles, TestAliasedString(null).isDir));
}

@safe unittest
{
    version (Windows)
    {
        if ("C:\\Program Files\\".exists)
            assert("C:\\Program Files\\".isDir);

        if ("C:\\Windows\\system.ini".exists)
            assert(!"C:\\Windows\\system.ini".isDir);
    }
    else version (Posix)
    {
        if (system_directory.exists)
            assert(system_directory.isDir);

        if (system_file.exists)
            assert(!system_file.isDir);
    }
}

@system unittest
{
    version (Windows)
        enum dir = "C:\\Program Files\\";
    else version (Posix)
        enum dir = system_directory;

    if (dir.exists)
    {
        DirEntry de = DirEntry(dir);
        assert(de.isDir);
        assert(DirEntry(dir).isDir);
    }
}

/++
    Returns whether the given file _attributes are for a directory.

    Params:
        attributes = The file _attributes.

    Returns:
        true if attributes specifies a directory

Example:
--------------------
assert(!attrIsDir(getAttributes("/etc/fonts/fonts.conf")));
assert(!attrIsDir(getLinkAttributes("/etc/fonts/fonts.conf")));
--------------------
  +/
bool attrIsDir(uint attributes) @safe pure nothrow @nogc
{
    version (Windows)
    {
        return (attributes & FILE_ATTRIBUTE_DIRECTORY) != 0;
    }
    else version (Posix)
    {
        return (attributes & S_IFMT) == S_IFDIR;
    }
}

@safe unittest
{
    version (Windows)
    {
        if ("C:\\Program Files\\".exists)
        {
            assert(attrIsDir(getAttributes("C:\\Program Files\\")));
            assert(attrIsDir(getLinkAttributes("C:\\Program Files\\")));
        }

        if ("C:\\Windows\\system.ini".exists)
        {
            assert(!attrIsDir(getAttributes("C:\\Windows\\system.ini")));
            assert(!attrIsDir(getLinkAttributes("C:\\Windows\\system.ini")));
        }
    }
    else version (Posix)
    {
        if (system_directory.exists)
        {
            assert(attrIsDir(getAttributes(system_directory)));
            assert(attrIsDir(getLinkAttributes(system_directory)));
        }

        if (system_file.exists)
        {
            assert(!attrIsDir(getAttributes(system_file)));
            assert(!attrIsDir(getLinkAttributes(system_file)));
        }
    }
}


/++
    Returns whether the given file (or directory) is a file.

    On Windows, if a file is not a directory, then it's a file. So,
    either $(D isFile) or $(D isDir) will return true for any given file.

    On Posix systems, if $(D isFile) is $(D true), that indicates that the file
    is a regular file (e.g. not a block not device). So, on Posix systems, it's
    possible for both $(D isFile) and $(D isDir) to be $(D false) for a
    particular file (in which case, it's a special file). You can use
    $(D getAttributes) to get the attributes to figure out what type of special
    it is, or you can use $(D DirEntry) to get at its $(D statBuf), which is the
    result from $(D stat). In either case, see the man page for $(D stat) for
    more information.

    Params:
        name = The path to the file.

    Returns:
        true if name specifies a file

    Throws:
        $(D FileException) if the given file does not exist.

Example:
--------------------
assert("/etc/fonts/fonts.conf".isFile);
assert(!"/usr/share/include".isFile);
--------------------
  +/
@property bool isFile(R)(R name)
if (isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R) &&
    !isConvertibleToString!R)
{
    version (Windows)
        return !name.isDir;
    else version (Posix)
        return (getAttributes(name) & S_IFMT) == S_IFREG;
}

/// ditto
@property bool isFile(R)(auto ref R name)
if (isConvertibleToString!R)
{
    return isFile!(StringTypeOf!R)(name);
}

@system unittest // bugzilla 15658
{
    DirEntry e = DirEntry(".");
    static assert(is(typeof(isFile(e))));
}

@safe unittest
{
    static assert(__traits(compiles, TestAliasedString(null).isFile));
}

@safe unittest
{
    version (Windows)
    {
        if ("C:\\Program Files\\".exists)
            assert(!"C:\\Program Files\\".isFile);

        if ("C:\\Windows\\system.ini".exists)
            assert("C:\\Windows\\system.ini".isFile);
    }
    else version (Posix)
    {
        if (system_directory.exists)
            assert(!system_directory.isFile);

        if (system_file.exists)
            assert(system_file.isFile);
    }
}


/++
    Returns whether the given file _attributes are for a file.

    On Windows, if a file is not a directory, it's a file. So, either
    $(D attrIsFile) or $(D attrIsDir) will return $(D true) for the
    _attributes of any given file.

    On Posix systems, if $(D attrIsFile) is $(D true), that indicates that the
    file is a regular file (e.g. not a block not device). So, on Posix systems,
    it's possible for both $(D attrIsFile) and $(D attrIsDir) to be $(D false)
    for a particular file (in which case, it's a special file). If a file is a
    special file, you can use the _attributes to check what type of special file
    it is (see the man page for $(D stat) for more information).

    Params:
        attributes = The file _attributes.

    Returns:
        true if the given file _attributes are for a file

Example:
--------------------
assert(attrIsFile(getAttributes("/etc/fonts/fonts.conf")));
assert(attrIsFile(getLinkAttributes("/etc/fonts/fonts.conf")));
--------------------
  +/
bool attrIsFile(uint attributes) @safe pure nothrow @nogc
{
    version (Windows)
    {
        return (attributes & FILE_ATTRIBUTE_DIRECTORY) == 0;
    }
    else version (Posix)
    {
        return (attributes & S_IFMT) == S_IFREG;
    }
}

@safe unittest
{
    version (Windows)
    {
        if ("C:\\Program Files\\".exists)
        {
            assert(!attrIsFile(getAttributes("C:\\Program Files\\")));
            assert(!attrIsFile(getLinkAttributes("C:\\Program Files\\")));
        }

        if ("C:\\Windows\\system.ini".exists)
        {
            assert(attrIsFile(getAttributes("C:\\Windows\\system.ini")));
            assert(attrIsFile(getLinkAttributes("C:\\Windows\\system.ini")));
        }
    }
    else version (Posix)
    {
        if (system_directory.exists)
        {
            assert(!attrIsFile(getAttributes(system_directory)));
            assert(!attrIsFile(getLinkAttributes(system_directory)));
        }

        if (system_file.exists)
        {
            assert(attrIsFile(getAttributes(system_file)));
            assert(attrIsFile(getLinkAttributes(system_file)));
        }
    }
}


/++
    Returns whether the given file is a symbolic link.

    On Windows, returns $(D true) when the file is either a symbolic link or a
    junction point.

    Params:
        name = The path to the file.

    Returns:
        true if name is a symbolic link

    Throws:
        $(D FileException) if the given file does not exist.
  +/
@property bool isSymlink(R)(R name)
if (isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R) &&
    !isConvertibleToString!R)
{
    version (Windows)
        return (getAttributes(name) & FILE_ATTRIBUTE_REPARSE_POINT) != 0;
    else version (Posix)
        return (getLinkAttributes(name) & S_IFMT) == S_IFLNK;
}

/// ditto
@property bool isSymlink(R)(auto ref R name)
if (isConvertibleToString!R)
{
    return name.isSymlink!(StringTypeOf!R);
}

@safe unittest
{
    static assert(__traits(compiles, TestAliasedString(null).isSymlink));
}

@system unittest
{
    version (Windows)
    {
        if ("C:\\Program Files\\".exists)
            assert(!"C:\\Program Files\\".isSymlink);

        if ("C:\\Users\\".exists && "C:\\Documents and Settings\\".exists)
            assert("C:\\Documents and Settings\\".isSymlink);

        enum fakeSymFile = "C:\\Windows\\system.ini";
        if (fakeSymFile.exists)
        {
            assert(!fakeSymFile.isSymlink);

            assert(!fakeSymFile.isSymlink);
            assert(!attrIsSymlink(getAttributes(fakeSymFile)));
            assert(!attrIsSymlink(getLinkAttributes(fakeSymFile)));

            assert(attrIsFile(getAttributes(fakeSymFile)));
            assert(attrIsFile(getLinkAttributes(fakeSymFile)));
            assert(!attrIsDir(getAttributes(fakeSymFile)));
            assert(!attrIsDir(getLinkAttributes(fakeSymFile)));

            assert(getAttributes(fakeSymFile) == getLinkAttributes(fakeSymFile));
        }
    }
    else version (Posix)
    {
        if (system_directory.exists)
        {
            assert(!system_directory.isSymlink);

            immutable symfile = deleteme ~ "_slink\0";
            scope(exit) if (symfile.exists) symfile.remove();

            core.sys.posix.unistd.symlink(system_directory, symfile.ptr);

            assert(symfile.isSymlink);
            assert(!attrIsSymlink(getAttributes(symfile)));
            assert(attrIsSymlink(getLinkAttributes(symfile)));

            assert(attrIsDir(getAttributes(symfile)));
            assert(!attrIsDir(getLinkAttributes(symfile)));

            assert(!attrIsFile(getAttributes(symfile)));
            assert(!attrIsFile(getLinkAttributes(symfile)));
        }

        if (system_file.exists)
        {
            assert(!system_file.isSymlink);

            immutable symfile = deleteme ~ "_slink\0";
            scope(exit) if (symfile.exists) symfile.remove();

            core.sys.posix.unistd.symlink(system_file, symfile.ptr);

            assert(symfile.isSymlink);
            assert(!attrIsSymlink(getAttributes(symfile)));
            assert(attrIsSymlink(getLinkAttributes(symfile)));

            assert(!attrIsDir(getAttributes(symfile)));
            assert(!attrIsDir(getLinkAttributes(symfile)));

            assert(attrIsFile(getAttributes(symfile)));
            assert(!attrIsFile(getLinkAttributes(symfile)));
        }
    }

    static assert(__traits(compiles, () @safe { return "dummy".isSymlink; }));
}


/++
    Returns whether the given file attributes are for a symbolic link.

    On Windows, return $(D true) when the file is either a symbolic link or a
    junction point.

    Params:
        attributes = The file attributes.

    Returns:
        true if attributes are for a symbolic link

Example:
--------------------
core.sys.posix.unistd.symlink("/etc/fonts/fonts.conf", "/tmp/alink");

assert(!getAttributes("/tmp/alink").isSymlink);
assert(getLinkAttributes("/tmp/alink").isSymlink);
--------------------
  +/
bool attrIsSymlink(uint attributes) @safe pure nothrow @nogc
{
    version (Windows)
        return (attributes & FILE_ATTRIBUTE_REPARSE_POINT) != 0;
    else version (Posix)
        return (attributes & S_IFMT) == S_IFLNK;
}


/****************************************************
 * Change directory to $(D pathname).
 * Throws: $(D FileException) on error.
 */
void chdir(R)(R pathname)
if (isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R) &&
    !isConvertibleToString!R)
{
    // Place outside of @trusted block
    auto pathz = pathname.tempCString!FSChar();

    version (Windows)
    {
        static auto trustedChdir(const(FSChar)* pathz) @trusted
        {
            return SetCurrentDirectoryW(pathz);
        }
    }
    else version (Posix)
    {
        static auto trustedChdir(const(FSChar)* pathz) @trusted
        {
            return core.sys.posix.unistd.chdir(pathz) == 0;
        }
    }
    static if (isNarrowString!R && is(Unqual!(ElementEncodingType!R) == char))
        alias pathStr = pathname;
    else
        string pathStr = null;
    cenforce(trustedChdir(pathz), pathStr, pathz);
}

/// ditto
void chdir(R)(auto ref R pathname)
if (isConvertibleToString!R)
{
    return chdir!(StringTypeOf!R)(pathname);
}

@safe unittest
{
    static assert(__traits(compiles, chdir(TestAliasedString(null))));
}

/****************************************************
Make directory $(D pathname).

Throws: $(D FileException) on Posix or $(D WindowsException) on Windows
        if an error occured.
 */
void mkdir(R)(R pathname)
if (isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R) &&
    !isConvertibleToString!R)
{
    // Place outside of @trusted block
    const pathz = pathname.tempCString!FSChar();

    version (Windows)
    {
        static auto trustedCreateDirectoryW(const(FSChar)* pathz) @trusted
        {
            return CreateDirectoryW(pathz, null);
        }
        static if (isNarrowString!R && is(Unqual!(ElementEncodingType!R) == char))
            alias pathStr = pathname;
        else
            string pathStr = null;
        wenforce(trustedCreateDirectoryW(pathz), pathStr, pathz);
    }
    else version (Posix)
    {
        import std.conv : octal;

        static auto trustedMkdir(const(FSChar)* pathz, mode_t mode) @trusted
        {
            return core.sys.posix.sys.stat.mkdir(pathz, mode);
        }
        static if (isNarrowString!R && is(Unqual!(ElementEncodingType!R) == char))
            alias pathStr = pathname;
        else
            string pathStr = null;
        cenforce(trustedMkdir(pathz, octal!777) == 0, pathStr, pathz);
    }
}

/// ditto
void mkdir(R)(auto ref R pathname)
if (isConvertibleToString!R)
{
    return mkdir!(StringTypeOf!R)(pathname);
}

@safe unittest
{
    import std.file : mkdir;
    static assert(__traits(compiles, mkdir(TestAliasedString(null))));
}

// Same as mkdir but ignores "already exists" errors.
// Returns: "true" if the directory was created,
//   "false" if it already existed.
private bool ensureDirExists()(in char[] pathname)
{
    import std.exception : enforce;
    const pathz = pathname.tempCString!FSChar();

    version (Windows)
    {
        if (() @trusted { return CreateDirectoryW(pathz, null); }())
            return true;
        cenforce(GetLastError() == ERROR_ALREADY_EXISTS, pathname.idup);
    }
    else version (Posix)
    {
        import std.conv : octal;

        if (() @trusted { return core.sys.posix.sys.stat.mkdir(pathz, octal!777); }() == 0)
            return true;
        cenforce(errno == EEXIST || errno == EISDIR, pathname);
    }
    enforce(pathname.isDir, new FileException(pathname.idup));
    return false;
}

/****************************************************
 * Make directory and all parent directories as needed.
 *
 * Does nothing if the directory specified by
 * $(D pathname) already exists.
 *
 * Throws: $(D FileException) on error.
 */

void mkdirRecurse(in char[] pathname) @safe
{
    import std.path : dirName, baseName;

    const left = dirName(pathname);
    if (left.length != pathname.length && !exists(left))
    {
        mkdirRecurse(left);
    }
    if (!baseName(pathname).empty)
    {
        ensureDirExists(pathname);
    }
}

@safe unittest
{
    import std.exception : assertThrown;
    {
        import std.path : buildPath, buildNormalizedPath;

        immutable basepath = deleteme ~ "_dir";
        scope(exit) () @trusted { rmdirRecurse(basepath); }();

        auto path = buildPath(basepath, "a", "..", "b");
        mkdirRecurse(path);
        path = path.buildNormalizedPath;
        assert(path.isDir);

        path = buildPath(basepath, "c");
        write(path, "");
        assertThrown!FileException(mkdirRecurse(path));

        path = buildPath(basepath, "d");
        mkdirRecurse(path);
        mkdirRecurse(path); // should not throw
    }

    version (Windows)
    {
        assertThrown!FileException(mkdirRecurse(`1:\foobar`));
    }

    // bug3570
    {
        immutable basepath = deleteme ~ "_dir";
        version (Windows)
        {
            immutable path = basepath ~ "\\fake\\here\\";
        }
        else version (Posix)
        {
            immutable path = basepath ~ `/fake/here/`;
        }

        mkdirRecurse(path);
        assert(basepath.exists && basepath.isDir);
        scope(exit) () @trusted { rmdirRecurse(basepath); }();
        assert(path.exists && path.isDir);
    }
}

/****************************************************
Remove directory $(D pathname).

Params:
    pathname = Range or string specifying the directory name

Throws: $(D FileException) on error.
 */
void rmdir(R)(R pathname)
if (isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R) &&
    !isConvertibleToString!R)
{
    // Place outside of @trusted block
    auto pathz = pathname.tempCString!FSChar();

    version (Windows)
    {
        static auto trustedRmdir(const(FSChar)* pathz) @trusted
        {
            return RemoveDirectoryW(pathz);
        }
    }
    else version (Posix)
    {
        static auto trustedRmdir(const(FSChar)* pathz) @trusted
        {
            return core.sys.posix.unistd.rmdir(pathz) == 0;
        }
    }
    static if (isNarrowString!R && is(Unqual!(ElementEncodingType!R) == char))
        alias pathStr = pathname;
    else
        string pathStr = null;
    cenforce(trustedRmdir(pathz), pathStr, pathz);
}

/// ditto
void rmdir(R)(auto ref R pathname)
if (isConvertibleToString!R)
{
    rmdir!(StringTypeOf!R)(pathname);
}

@safe unittest
{
    static assert(__traits(compiles, rmdir(TestAliasedString(null))));
}

/++
    $(BLUE This function is Posix-Only.)

    Creates a symbolic _link (_symlink).

    Params:
        original = The file that is being linked. This is the target path that's
            stored in the _symlink. A relative path is relative to the created
            _symlink.
        link = The _symlink to create. A relative path is relative to the
            current working directory.

    Throws:
        $(D FileException) on error (which includes if the _symlink already
        exists).
  +/
version (StdDdoc) void symlink(RO, RL)(RO original, RL link)
if ((isInputRange!RO && !isInfinite!RO && isSomeChar!(ElementEncodingType!RO) ||
    isConvertibleToString!RO) &&
    (isInputRange!RL && !isInfinite!RL && isSomeChar!(ElementEncodingType!RL) ||
    isConvertibleToString!RL));
else version (Posix) void symlink(RO, RL)(RO original, RL link)
if ((isInputRange!RO && !isInfinite!RO && isSomeChar!(ElementEncodingType!RO) ||
    isConvertibleToString!RO) &&
    (isInputRange!RL && !isInfinite!RL && isSomeChar!(ElementEncodingType!RL) ||
    isConvertibleToString!RL))
{
    static if (isConvertibleToString!RO || isConvertibleToString!RL)
    {
        import std.meta : staticMap;
        alias Types = staticMap!(convertToString, RO, RL);
        symlink!Types(original, link);
    }
    else
    {
        import std.conv : text;
        auto oz = original.tempCString();
        auto lz = link.tempCString();
        alias posixSymlink = core.sys.posix.unistd.symlink;
        immutable int result = () @trusted { return posixSymlink(oz, lz); } ();
        cenforce(result == 0, text(link));
    }
}

version (Posix) @safe unittest
{
    if (system_directory.exists)
    {
        immutable symfile = deleteme ~ "_slink\0";
        scope(exit) if (symfile.exists) symfile.remove();

        symlink(system_directory, symfile);

        assert(symfile.exists);
        assert(symfile.isSymlink);
        assert(!attrIsSymlink(getAttributes(symfile)));
        assert(attrIsSymlink(getLinkAttributes(symfile)));

        assert(attrIsDir(getAttributes(symfile)));
        assert(!attrIsDir(getLinkAttributes(symfile)));

        assert(!attrIsFile(getAttributes(symfile)));
        assert(!attrIsFile(getLinkAttributes(symfile)));
    }

    if (system_file.exists)
    {
        assert(!system_file.isSymlink);

        immutable symfile = deleteme ~ "_slink\0";
        scope(exit) if (symfile.exists) symfile.remove();

        symlink(system_file, symfile);

        assert(symfile.exists);
        assert(symfile.isSymlink);
        assert(!attrIsSymlink(getAttributes(symfile)));
        assert(attrIsSymlink(getLinkAttributes(symfile)));

        assert(!attrIsDir(getAttributes(symfile)));
        assert(!attrIsDir(getLinkAttributes(symfile)));

        assert(attrIsFile(getAttributes(symfile)));
        assert(!attrIsFile(getLinkAttributes(symfile)));
    }
}

version (Posix) @safe unittest
{
    static assert(__traits(compiles,
        symlink(TestAliasedString(null), TestAliasedString(null))));
}


/++
    $(BLUE This function is Posix-Only.)

    Returns the path to the file pointed to by a symlink. Note that the
    path could be either relative or absolute depending on the symlink.
    If the path is relative, it's relative to the symlink, not the current
    working directory.

    Throws:
        $(D FileException) on error.
  +/
version (StdDdoc) string readLink(R)(R link)
if (isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R) ||
    isConvertibleToString!R);
else version (Posix) string readLink(R)(R link)
if (isInputRange!R && !isInfinite!R && isSomeChar!(ElementEncodingType!R) ||
    isConvertibleToString!R)
{
    static if (isConvertibleToString!R)
    {
        return readLink!(convertToString!R)(link);
    }
    else
    {
        import std.conv : to;
        import std.exception : assumeUnique;
        alias posixReadlink = core.sys.posix.unistd.readlink;
        enum bufferLen = 2048;
        enum maxCodeUnits = 6;
        char[bufferLen] buffer;
        const linkz = link.tempCString();
        auto size = () @trusted {
            return posixReadlink(linkz, buffer.ptr, buffer.length);
        } ();
        cenforce(size != -1, to!string(link));

        if (size <= bufferLen - maxCodeUnits)
            return to!string(buffer[0 .. size]);

        auto dynamicBuffer = new char[](bufferLen * 3 / 2);

        foreach (i; 0 .. 10)
        {
            size = () @trusted {
                return posixReadlink(linkz, dynamicBuffer.ptr,
                    dynamicBuffer.length);
            } ();
            cenforce(size != -1, to!string(link));

            if (size <= dynamicBuffer.length - maxCodeUnits)
            {
                dynamicBuffer.length = size;
                return () @trusted {
                    return assumeUnique(dynamicBuffer);
                } ();
            }

            dynamicBuffer.length = dynamicBuffer.length * 3 / 2;
        }

        throw new FileException(to!string(link), "Path is too long to read.");
    }
}

version (Posix) @safe unittest
{
    import std.exception : assertThrown;
    import std.string;

    foreach (file; [system_directory, system_file])
    {
        if (file.exists)
        {
            immutable symfile = deleteme ~ "_slink\0";
            scope(exit) if (symfile.exists) symfile.remove();

            symlink(file, symfile);
            assert(readLink(symfile) == file, format("Failed file: %s", file));
        }
    }

    assertThrown!FileException(readLink("/doesnotexist"));
}

version (Posix) @safe unittest
{
    static assert(__traits(compiles, readLink(TestAliasedString("foo"))));
}

version (Posix) @system unittest // input range of dchars
{
    mkdirRecurse(deleteme);
    scope(exit) if (deleteme.exists) rmdirRecurse(deleteme);
    write(deleteme ~ "/f", "");
    import std.range.interfaces : InputRange, inputRangeObject;
    import std.utf : byChar;
    immutable string link = deleteme ~ "/l";
    symlink("f", link);
    InputRange!dchar linkr = inputRangeObject(link);
    alias R = typeof(linkr);
    static assert(isInputRange!R);
    static assert(!isForwardRange!R);
    assert(readLink(linkr) == "f");
}


/****************************************************
 * Get the current working directory.
 * Throws: $(D FileException) on error.
 */
version (Windows) string getcwd()
{
    import std.conv : to;
    /* GetCurrentDirectory's return value:
        1. function succeeds: the number of characters that are written to
    the buffer, not including the terminating null character.
        2. function fails: zero
        3. the buffer (lpBuffer) is not large enough: the required size of
    the buffer, in characters, including the null-terminating character.
    */
    wchar[4096] buffW = void; //enough for most common case
    immutable n = cenforce(GetCurrentDirectoryW(to!DWORD(buffW.length), buffW.ptr),
            "getcwd");
    // we can do it because toUTFX always produces a fresh string
    if (n < buffW.length)
    {
        return buffW[0 .. n].to!string;
    }
    else //staticBuff isn't enough
    {
        auto ptr = cast(wchar*) malloc(wchar.sizeof * n);
        scope(exit) free(ptr);
        immutable n2 = GetCurrentDirectoryW(n, ptr);
        cenforce(n2 && n2 < n, "getcwd");
        return ptr[0 .. n2].to!string;
    }
}
else version (Solaris) string getcwd()
{
    /* BUF_SIZE >= PATH_MAX */
    enum BUF_SIZE = 4096;
    /* The user should be able to specify any size buffer > 0 */
    auto p = cenforce(core.sys.posix.unistd.getcwd(null, BUF_SIZE),
            "cannot get cwd");
    scope(exit) core.stdc.stdlib.free(p);
    return p[0 .. core.stdc.string.strlen(p)].idup;
}
else version (Posix) string getcwd()
{
    auto p = cenforce(core.sys.posix.unistd.getcwd(null, 0),
            "cannot get cwd");
    scope(exit) core.stdc.stdlib.free(p);
    return p[0 .. core.stdc.string.strlen(p)].idup;
}

@system unittest
{
    auto s = getcwd();
    assert(s.length);
}

version (OSX)
    private extern (C) int _NSGetExecutablePath(char* buf, uint* bufsize);
else version (FreeBSD)
    private extern (C) int sysctl (const int* name, uint namelen, void* oldp,
        size_t* oldlenp, const void* newp, size_t newlen);
else version (NetBSD)
    private extern (C) int sysctl (const int* name, uint namelen, void* oldp,
        size_t* oldlenp, const void* newp, size_t newlen);

/**
 * Returns the full path of the current executable.
 *
 * Throws:
 * $(REF1 Exception, object)
 */
@trusted string thisExePath ()
{
    version (OSX)
    {
        import core.sys.posix.stdlib : realpath;
        import std.conv : to;
        import std.exception : errnoEnforce;

        uint size;

        _NSGetExecutablePath(null, &size); // get the length of the path
        auto buffer = new char[size];
        _NSGetExecutablePath(buffer.ptr, &size);

        auto absolutePath = realpath(buffer.ptr, null); // let the function allocate

        scope (exit)
        {
            if (absolutePath)
                free(absolutePath);
        }

        errnoEnforce(absolutePath);
        return to!(string)(absolutePath);
    }
    else version (linux)
    {
        return readLink("/proc/self/exe");
    }
    else version (Windows)
    {
        import std.conv : to;
        import std.exception : enforce;

        wchar[MAX_PATH] buf;
        wchar[] buffer = buf[];

        while (true)
        {
            auto len = GetModuleFileNameW(null, buffer.ptr, cast(DWORD) buffer.length);
            enforce(len, sysErrorString(GetLastError()));
            if (len != buffer.length)
                return to!(string)(buffer[0 .. len]);
            buffer.length *= 2;
        }
    }
    else version (DragonFlyBSD)
    {
        import core.sys.dragonflybsd.sys.sysctl : sysctl, CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME;
        import std.exception : errnoEnforce, assumeUnique;

        int[4] mib = [CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME, -1];
        size_t len;

        auto result = sysctl(mib.ptr, mib.length, null, &len, null, 0); // get the length of the path
        errnoEnforce(result == 0);

        auto buffer = new char[len - 1];
        result = sysctl(mib.ptr, mib.length, buffer.ptr, &len, null, 0);
        errnoEnforce(result == 0);

        return buffer.assumeUnique;
    }
    else version (FreeBSD)
    {
        import core.sys.freebsd.sys.sysctl : sysctl, CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME;
        import std.exception : errnoEnforce, assumeUnique;

        int[4] mib = [CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME, -1];
        size_t len;

        auto result = sysctl(mib.ptr, mib.length, null, &len, null, 0); // get the length of the path
        errnoEnforce(result == 0);

        auto buffer = new char[len - 1];
        result = sysctl(mib.ptr, mib.length, buffer.ptr, &len, null, 0);
        errnoEnforce(result == 0);

        return buffer.assumeUnique;
    }
    else version (NetBSD)
    {
        import core.sys.netbsd.sys.sysctl : sysctl, CTL_KERN, KERN_PROC_ARGS, KERN_PROC_PATHNAME;
        import std.exception : errnoEnforce, assumeUnique;

        int[4] mib = [CTL_KERN, KERN_PROC_ARGS, -1, KERN_PROC_PATHNAME];
        size_t len;

        auto result = sysctl(mib.ptr, mib.length, null, &len, null, 0); // get the length of the path
        errnoEnforce(result == 0);

        auto buffer = new char[len - 1];
        result = sysctl(mib.ptr, mib.length, buffer.ptr, &len, null, 0);
        errnoEnforce(result == 0);

        return buffer.assumeUnique;
    }
    else version (OpenBSD)
    {
        import core.sys.openbsd.sys.sysctl : sysctl, CTL_KERN, KERN_PROC_ARGS, KERN_PROC_ARGV;
        import core.sys.posix.unistd : getpid;
        import std.conv : to;
        import std.exception : enforce, errnoEnforce;
        import std.process : searchPathFor;

        int[4] mib = [CTL_KERN, KERN_PROC_ARGS, getpid(), KERN_PROC_ARGV];
        size_t len;

        auto result = sysctl(mib.ptr, mib.length, null, &len, null, 0);
        errnoEnforce(result == 0);

        auto argv = new char*[len - 1];
        result = sysctl(mib.ptr, mib.length, argv.ptr, &len, null, 0);
        errnoEnforce(result == 0);

        auto argv0 = argv[0];
        if (*argv0 == '/' || *argv0 == '.')
        {
            import core.sys.posix.stdlib : realpath;
            auto absolutePath = realpath(argv0, null);
            scope (exit)
            {
                if (absolutePath)
                    free(absolutePath);
            }
            errnoEnforce(absolutePath);
            return to!(string)(absolutePath);
        }
        else
        {
            auto absolutePath = searchPathFor(to!string(argv0));
            errnoEnforce(absolutePath);
            return absolutePath;
        }
    }
    else version (Solaris)
    {
        import core.sys.posix.unistd : getpid;
        import std.string : format;

        // Only Solaris 10 and later
        return readLink(format("/proc/%d/path/a.out", getpid()));
    }
    else
        static assert(0, "thisExePath is not supported on this platform");
}

@safe unittest
{
    import std.path : isAbsolute;
    auto path = thisExePath();

    assert(path.exists);
    assert(path.isAbsolute);
    assert(path.isFile);
}

version (StdDdoc)
{
    /++
        Info on a file, similar to what you'd get from stat on a Posix system.
      +/
    struct DirEntry
    {
        /++
            Constructs a $(D DirEntry) for the given file (or directory).

            Params:
                path = The file (or directory) to get a DirEntry for.

            Throws:
                $(D FileException) if the file does not exist.
        +/
        this(string path);

        version (Windows)
        {
            private this(string path, in WIN32_FIND_DATAW *fd);
        }
        else version (Posix)
        {
            private this(string path, core.sys.posix.dirent.dirent* fd);
        }

        /++
            Returns the path to the file represented by this $(D DirEntry).

Example:
--------------------
auto de1 = DirEntry("/etc/fonts/fonts.conf");
assert(de1.name == "/etc/fonts/fonts.conf");

auto de2 = DirEntry("/usr/share/include");
assert(de2.name == "/usr/share/include");
--------------------
          +/
        @property string name() const;


        /++
            Returns whether the file represented by this $(D DirEntry) is a
            directory.

Example:
--------------------
auto de1 = DirEntry("/etc/fonts/fonts.conf");
assert(!de1.isDir);

auto de2 = DirEntry("/usr/share/include");
assert(de2.isDir);
--------------------
          +/
        @property bool isDir();


        /++
            Returns whether the file represented by this $(D DirEntry) is a file.

            On Windows, if a file is not a directory, then it's a file. So,
            either $(D isFile) or $(D isDir) will return $(D true).

            On Posix systems, if $(D isFile) is $(D true), that indicates that
            the file is a regular file (e.g. not a block not device). So, on
            Posix systems, it's possible for both $(D isFile) and $(D isDir) to
            be $(D false) for a particular file (in which case, it's a special
            file). You can use $(D attributes) or $(D statBuf) to get more
            information about a special file (see the stat man page for more
            details).

Example:
--------------------
auto de1 = DirEntry("/etc/fonts/fonts.conf");
assert(de1.isFile);

auto de2 = DirEntry("/usr/share/include");
assert(!de2.isFile);
--------------------
          +/
        @property bool isFile();

        /++
            Returns whether the file represented by this $(D DirEntry) is a
            symbolic link.

            On Windows, return $(D true) when the file is either a symbolic
            link or a junction point.
          +/
        @property bool isSymlink();

        /++
            Returns the size of the the file represented by this $(D DirEntry)
            in bytes.
          +/
        @property ulong size();

        /++
            $(BLUE This function is Windows-Only.)

            Returns the creation time of the file represented by this
            $(D DirEntry).
          +/
        @property SysTime timeCreated() const;

        /++
            Returns the time that the file represented by this $(D DirEntry) was
            last accessed.

            Note that many file systems do not update the access time for files
            (generally for performance reasons), so there's a good chance that
            $(D timeLastAccessed) will return the same value as
            $(D timeLastModified).
          +/
        @property SysTime timeLastAccessed();

        /++
            Returns the time that the file represented by this $(D DirEntry) was
            last modified.
          +/
        @property SysTime timeLastModified();

        /++
            Returns the _attributes of the file represented by this $(D DirEntry).

            Note that the file _attributes on Windows and Posix systems are
            completely different. On, Windows, they're what is returned by
            $(D GetFileAttributes)
            $(HTTP msdn.microsoft.com/en-us/library/aa364944(v=vs.85).aspx, GetFileAttributes)
            Whereas, an Posix systems, they're the $(D st_mode) value which is
            part of the $(D stat) struct gotten by calling $(D stat).

            On Posix systems, if the file represented by this $(D DirEntry) is a
            symbolic link, then _attributes are the _attributes of the file
            pointed to by the symbolic link.
          +/
        @property uint attributes();

        /++
            On Posix systems, if the file represented by this $(D DirEntry) is a
            symbolic link, then $(D linkAttributes) are the attributes of the
            symbolic link itself. Otherwise, $(D linkAttributes) is identical to
            $(D attributes).

            On Windows, $(D linkAttributes) is identical to $(D attributes). It
            exists on Windows so that you don't have to special-case code for
            Windows when dealing with symbolic links.
          +/
        @property uint linkAttributes();

        version (Windows)
            alias stat_t = void*;

        /++
            $(BLUE This function is Posix-Only.)

            The $(D stat) struct gotten from calling $(D stat).
          +/
        @property stat_t statBuf();
    }
}
else version (Windows)
{
    struct DirEntry
    {
    public:
        alias name this;

        this(string path)
        {
            import std.datetime.systime : FILETIMEToSysTime;

            if (!path.exists())
                throw new FileException(path, "File does not exist");

            _name = path;

            with (getFileAttributesWin(path))
            {
                _size = makeUlong(nFileSizeLow, nFileSizeHigh);
                _timeCreated = FILETIMEToSysTime(&ftCreationTime);
                _timeLastAccessed = FILETIMEToSysTime(&ftLastAccessTime);
                _timeLastModified = FILETIMEToSysTime(&ftLastWriteTime);
                _attributes = dwFileAttributes;
            }
        }

        private this(string path, in WIN32_FIND_DATAW *fd)
        {
            import core.stdc.wchar_ : wcslen;
            import std.conv : to;
            import std.datetime.systime : FILETIMEToSysTime;
            import std.path : buildPath;

            size_t clength = wcslen(fd.cFileName.ptr);
            _name = buildPath(path, fd.cFileName[0 .. clength].to!string);
            _size = (cast(ulong) fd.nFileSizeHigh << 32) | fd.nFileSizeLow;
            _timeCreated = FILETIMEToSysTime(&fd.ftCreationTime);
            _timeLastAccessed = FILETIMEToSysTime(&fd.ftLastAccessTime);
            _timeLastModified = FILETIMEToSysTime(&fd.ftLastWriteTime);
            _attributes = fd.dwFileAttributes;
        }

        @property string name() const pure nothrow
        {
            return _name;
        }

        @property bool isDir() const pure nothrow
        {
            return (attributes & FILE_ATTRIBUTE_DIRECTORY) != 0;
        }

        @property bool isFile() const pure nothrow
        {
            //Are there no options in Windows other than directory and file?
            //If there are, then this probably isn't the best way to determine
            //whether this DirEntry is a file or not.
            return !isDir;
        }

        @property bool isSymlink() const pure nothrow
        {
            return (attributes & FILE_ATTRIBUTE_REPARSE_POINT) != 0;
        }

        @property ulong size() const pure nothrow
        {
            return _size;
        }

        @property SysTime timeCreated() const pure nothrow
        {
            return cast(SysTime)_timeCreated;
        }

        @property SysTime timeLastAccessed() const pure nothrow
        {
            return cast(SysTime)_timeLastAccessed;
        }

        @property SysTime timeLastModified() const pure nothrow
        {
            return cast(SysTime)_timeLastModified;
        }

        @property uint attributes() const pure nothrow
        {
            return _attributes;
        }

        @property uint linkAttributes() const pure nothrow
        {
            return _attributes;
        }

    private:
        string _name; /// The file or directory represented by this DirEntry.

        SysTime _timeCreated;      /// The time when the file was created.
        SysTime _timeLastAccessed; /// The time when the file was last accessed.
        SysTime _timeLastModified; /// The time when the file was last modified.

        ulong _size;       /// The size of the file in bytes.
        uint  _attributes; /// The file attributes from WIN32_FIND_DATAW.
    }
}
else version (Posix)
{
    struct DirEntry
    {
    public:
        alias name this;

        this(string path)
        {
            if (!path.exists)
                throw new FileException(path, "File does not exist");

            _name = path;

            _didLStat = false;
            _didStat = false;
            _dTypeSet = false;
        }

        private this(string path, core.sys.posix.dirent.dirent* fd)
        {
            import std.path : buildPath;

            static if (is(typeof(fd.d_namlen)))
                immutable len = fd.d_namlen;
            else
                immutable len = (() @trusted => core.stdc.string.strlen(fd.d_name.ptr))();

            _name = buildPath(path, (() @trusted => fd.d_name.ptr[0 .. len])());

            _didLStat = false;
            _didStat = false;

            //fd_d_type doesn't work for all file systems,
            //in which case the result is DT_UNKOWN. But we
            //can determine the correct type from lstat, so
            //we'll only set the dtype here if we could
            //correctly determine it (not lstat in the case
            //of DT_UNKNOWN in case we don't ever actually
            //need the dtype, thus potentially avoiding the
            //cost of calling lstat).
            static if (__traits(compiles, fd.d_type != DT_UNKNOWN))
            {
                if (fd.d_type != DT_UNKNOWN)
                {
                    _dType = fd.d_type;
                    _dTypeSet = true;
                }
                else
                    _dTypeSet = false;
            }
            else
            {
                // e.g. Solaris does not have the d_type member
                _dTypeSet = false;
            }
        }

        @property string name() const pure nothrow
        {
            return _name;
        }

        @property bool isDir()
        {
            _ensureStatOrLStatDone();

            return (_statBuf.st_mode & S_IFMT) == S_IFDIR;
        }

        @property bool isFile()
        {
            _ensureStatOrLStatDone();

            return (_statBuf.st_mode & S_IFMT) == S_IFREG;
        }

        @property bool isSymlink()
        {
            _ensureLStatDone();

            return (_lstatMode & S_IFMT) == S_IFLNK;
        }

        @property ulong size()
        {
            _ensureStatDone();
            return _statBuf.st_size;
        }

        @property SysTime timeStatusChanged()
        {
            _ensureStatDone();

            return statTimeToStdTime!'c'(_statBuf);
        }

        @property SysTime timeLastAccessed()
        {
            _ensureStatDone();

            return statTimeToStdTime!'a'(_statBuf);
        }

        @property SysTime timeLastModified()
        {
            _ensureStatDone();

            return statTimeToStdTime!'m'(_statBuf);
        }

        @property uint attributes()
        {
            _ensureStatDone();

            return _statBuf.st_mode;
        }

        @property uint linkAttributes()
        {
            _ensureLStatDone();

            return _lstatMode;
        }

        @property stat_t statBuf()
        {
            _ensureStatDone();

            return _statBuf;
        }

    private:
        /++
            This is to support lazy evaluation, because doing stat's is
            expensive and not always needed.
         +/
        void _ensureStatDone() @safe
        {
            import std.exception : enforce;

            static auto trustedStat(in char[] path, stat_t* buf) @trusted
            {
                return stat(path.tempCString(), buf);
            }
            if (_didStat)
                return;

            enforce(trustedStat(_name, &_statBuf) == 0,
                    "Failed to stat file `" ~ _name ~ "'");

            _didStat = true;
        }

        /++
            This is to support lazy evaluation, because doing stat's is
            expensive and not always needed.

            Try both stat and lstat for isFile and isDir
            to detect broken symlinks.
         +/
        void _ensureStatOrLStatDone()
        {
            if (_didStat)
                return;

            if ( stat(_name.tempCString(), &_statBuf) != 0 )
            {
                _ensureLStatDone();

                _statBuf = stat_t.init;
                _statBuf.st_mode = S_IFLNK;
            }
            else
            {
                _didStat = true;
            }
        }

        /++
            This is to support lazy evaluation, because doing stat's is
            expensive and not always needed.
         +/
        void _ensureLStatDone()
        {
            import std.exception : enforce;

            if (_didLStat)
                return;

            stat_t statbuf = void;

            enforce(lstat(_name.tempCString(), &statbuf) == 0,
                "Failed to stat file `" ~ _name ~ "'");

            _lstatMode = statbuf.st_mode;

            _dTypeSet = true;
            _didLStat = true;
        }

        string _name; /// The file or directory represented by this DirEntry.

        stat_t _statBuf = void;  /// The result of stat().
        uint  _lstatMode;               /// The stat mode from lstat().
        ubyte _dType;                   /// The type of the file.

        bool _didLStat = false;   /// Whether lstat() has been called for this DirEntry.
        bool _didStat = false;    /// Whether stat() has been called for this DirEntry.
        bool _dTypeSet = false;   /// Whether the dType of the file has been set.
    }
}

@system unittest
{
    version (Windows)
    {
        if ("C:\\Program Files\\".exists)
        {
            auto de = DirEntry("C:\\Program Files\\");
            assert(!de.isFile);
            assert(de.isDir);
            assert(!de.isSymlink);
        }

        if ("C:\\Users\\".exists && "C:\\Documents and Settings\\".exists)
        {
            auto de = DirEntry("C:\\Documents and Settings\\");
            assert(de.isSymlink);
        }

        if ("C:\\Windows\\system.ini".exists)
        {
            auto de = DirEntry("C:\\Windows\\system.ini");
            assert(de.isFile);
            assert(!de.isDir);
            assert(!de.isSymlink);
        }
    }
    else version (Posix)
    {
        import std.exception : assertThrown;

        if (system_directory.exists)
        {
            {
                auto de = DirEntry(system_directory);
                assert(!de.isFile);
                assert(de.isDir);
                assert(!de.isSymlink);
            }

            immutable symfile = deleteme ~ "_slink\0";
            scope(exit) if (symfile.exists) symfile.remove();

            core.sys.posix.unistd.symlink(system_directory, symfile.ptr);

            {
                auto de = DirEntry(symfile);
                assert(!de.isFile);
                assert(de.isDir);
                assert(de.isSymlink);
            }

            symfile.remove();
            core.sys.posix.unistd.symlink((deleteme ~ "_broken_symlink\0").ptr, symfile.ptr);

            {
                //Issue 8298
                DirEntry de = DirEntry(symfile);

                assert(!de.isFile);
                assert(!de.isDir);
                assert(de.isSymlink);
                assertThrown(de.size);
                assertThrown(de.timeStatusChanged);
                assertThrown(de.timeLastAccessed);
                assertThrown(de.timeLastModified);
                assertThrown(de.attributes);
                assertThrown(de.statBuf);
                assert(symfile.exists);
                symfile.remove();
            }
        }

        if (system_file.exists)
        {
            auto de = DirEntry(system_file);
            assert(de.isFile);
            assert(!de.isDir);
            assert(!de.isSymlink);
        }
    }
}

alias PreserveAttributes = Flag!"preserveAttributes";

version (StdDdoc)
{
    /// Defaults to $(D Yes.preserveAttributes) on Windows, and the opposite on all other platforms.
    PreserveAttributes preserveAttributesDefault;
}
else version (Windows)
{
    enum preserveAttributesDefault = Yes.preserveAttributes;
}
else
{
    enum preserveAttributesDefault = No.preserveAttributes;
}

/***************************************************
Copy file $(D from) _to file $(D to). File timestamps are preserved.
File attributes are preserved, if $(D preserve) equals $(D Yes.preserveAttributes).
On Windows only $(D Yes.preserveAttributes) (the default on Windows) is supported.
If the target file exists, it is overwritten.

Params:
    from = string or range of characters representing the existing file name
    to = string or range of characters representing the target file name
    preserve = whether to _preserve the file attributes

Throws: $(D FileException) on error.
 */
void copy(RF, RT)(RF from, RT to, PreserveAttributes preserve = preserveAttributesDefault)
if (isInputRange!RF && !isInfinite!RF && isSomeChar!(ElementEncodingType!RF) && !isConvertibleToString!RF &&
    isInputRange!RT && !isInfinite!RT && isSomeChar!(ElementEncodingType!RT) && !isConvertibleToString!RT)
{
    // Place outside of @trusted block
    auto fromz = from.tempCString!FSChar();
    auto toz = to.tempCString!FSChar();

    static if (isNarrowString!RF && is(Unqual!(ElementEncodingType!RF) == char))
        alias f = from;
    else
        enum string f = null;

    static if (isNarrowString!RT && is(Unqual!(ElementEncodingType!RT) == char))
        alias t = to;
    else
        enum string t = null;

    copyImpl(f, t, fromz, toz, preserve);
}

/// ditto
void copy(RF, RT)(auto ref RF from, auto ref RT to, PreserveAttributes preserve = preserveAttributesDefault)
if (isConvertibleToString!RF || isConvertibleToString!RT)
{
    import std.meta : staticMap;
    alias Types = staticMap!(convertToString, RF, RT);
    copy!Types(from, to, preserve);
}

@safe unittest // issue 15319
{
    assert(__traits(compiles, copy("from.txt", "to.txt")));
}

private void copyImpl(const(char)[] f, const(char)[] t, const(FSChar)* fromz, const(FSChar)* toz,
        PreserveAttributes preserve) @trusted
{
    version (Windows)
    {
        assert(preserve == Yes.preserveAttributes);
        immutable result = CopyFileW(fromz, toz, false);
        if (!result)
        {
            import core.stdc.wchar_ : wcslen;
            import std.conv : to;

            if (!t)
                t = to!(typeof(t))(toz[0 .. wcslen(toz)]);

            throw new FileException(t);
        }
    }
    else version (Posix)
    {
        static import core.stdc.stdio;
        import std.conv : to, octal;

        immutable fdr = core.sys.posix.fcntl.open(fromz, O_RDONLY);
        cenforce(fdr != -1, f, fromz);
        scope(exit) core.sys.posix.unistd.close(fdr);

        stat_t statbufr = void;
        cenforce(fstat(fdr, &statbufr) == 0, f, fromz);
        //cenforce(core.sys.posix.sys.stat.fstat(fdr, &statbufr) == 0, f, fromz);

        immutable fdw = core.sys.posix.fcntl.open(toz,
                O_CREAT | O_WRONLY, octal!666);
        cenforce(fdw != -1, t, toz);
        {
            scope(failure) core.sys.posix.unistd.close(fdw);

            stat_t statbufw = void;
            cenforce(fstat(fdw, &statbufw) == 0, t, toz);
            if (statbufr.st_dev == statbufw.st_dev && statbufr.st_ino == statbufw.st_ino)
                throw new FileException(t, "Source and destination are the same file");
        }

        scope(failure) core.stdc.stdio.remove(toz);
        {
            scope(failure) core.sys.posix.unistd.close(fdw);
            cenforce(ftruncate(fdw, 0) == 0, t, toz);

            auto BUFSIZ = 4096u * 16;
            auto buf = core.stdc.stdlib.malloc(BUFSIZ);
            if (!buf)
            {
                BUFSIZ = 4096;
                buf = core.stdc.stdlib.malloc(BUFSIZ);
                if (!buf)
                {
                    import core.exception : onOutOfMemoryError;
                    onOutOfMemoryError();
                }
            }
            scope(exit) core.stdc.stdlib.free(buf);

            for (auto size = statbufr.st_size; size; )
            {
                immutable toxfer = (size > BUFSIZ) ? BUFSIZ : cast(size_t) size;
                cenforce(
                    core.sys.posix.unistd.read(fdr, buf, toxfer) == toxfer
                    && core.sys.posix.unistd.write(fdw, buf, toxfer) == toxfer,
                    f, fromz);
                assert(size >= toxfer);
                size -= toxfer;
            }
            if (preserve)
                cenforce(fchmod(fdw, to!mode_t(statbufr.st_mode)) == 0, f, fromz);
        }

        cenforce(core.sys.posix.unistd.close(fdw) != -1, f, fromz);

        utimbuf utim = void;
        utim.actime = cast(time_t) statbufr.st_atime;
        utim.modtime = cast(time_t) statbufr.st_mtime;

        cenforce(utime(toz, &utim) != -1, f, fromz);
    }
}

@safe unittest
{
    import std.algorithm, std.file; // issue 14817
    auto t1 = deleteme, t2 = deleteme~"2";
    scope(exit) foreach (t; [t1, t2]) if (t.exists) t.remove();
    write(t1, "11");
    copy(t1, t2);
    assert(readText(t2) == "11");
    write(t1, "2");
    copy(t1, t2);
    assert(readText(t2) == "2");

    import std.utf : byChar;
    copy(t1.byChar, t2.byChar);
    assert(readText(t2.byChar) == "2");
}

@safe version (Posix) @safe unittest //issue 11434
{
    import std.conv : octal;
    auto t1 = deleteme, t2 = deleteme~"2";
    scope(exit) foreach (t; [t1, t2]) if (t.exists) t.remove();
    write(t1, "1");
    setAttributes(t1, octal!767);
    copy(t1, t2, Yes.preserveAttributes);
    assert(readText(t2) == "1");
    assert(getAttributes(t2) == octal!100767);
}

@safe unittest // issue 15865
{
    import std.exception : assertThrown;
    auto t = deleteme;
    write(t, "a");
    scope(exit) t.remove();
    assertThrown!FileException(copy(t, t));
    assert(readText(t) == "a");
}

/++
    Remove directory and all of its content and subdirectories,
    recursively.

    Throws:
        $(D FileException) if there is an error (including if the given
        file is not a directory).
 +/
void rmdirRecurse(in char[] pathname)
{
    //No references to pathname will be kept after rmdirRecurse,
    //so the cast is safe
    rmdirRecurse(DirEntry(cast(string) pathname));
}

/++
    Remove directory and all of its content and subdirectories,
    recursively.

    Throws:
        $(D FileException) if there is an error (including if the given
        file is not a directory).
 +/
void rmdirRecurse(ref DirEntry de)
{
    if (!de.isDir)
        throw new FileException(de.name, "Not a directory");

    if (de.isSymlink)
    {
        version (Windows)
            rmdir(de.name);
        else
            remove(de.name);
    }
    else
    {
        // all children, recursively depth-first
        foreach (DirEntry e; dirEntries(de.name, SpanMode.depth, false))
        {
            attrIsDir(e.linkAttributes) ? rmdir(e.name) : remove(e.name);
        }

        // the dir itself
        rmdir(de.name);
    }
}
///ditto
//Note, without this overload, passing an RValue DirEntry still works, but
//actually fully reconstructs a DirEntry inside the
//"rmdirRecurse(in char[] pathname)" implementation. That is needlessly
//expensive.
//A DirEntry is a bit big (72B), so keeping the "by ref" signature is desirable.
void rmdirRecurse(DirEntry de)
{
    rmdirRecurse(de);
}

version (Windows) @system unittest
{
    import std.exception : enforce;
    auto d = deleteme ~ r".dir\a\b\c\d\e\f\g";
    mkdirRecurse(d);
    rmdirRecurse(deleteme ~ ".dir");
    enforce(!exists(deleteme ~ ".dir"));
}

version (Posix) @system unittest
{
    import std.exception : enforce, collectException;
    import std.process : executeShell;
    collectException(rmdirRecurse(deleteme));
    auto d = deleteme~"/a/b/c/d/e/f/g";
    enforce(collectException(mkdir(d)));
    mkdirRecurse(d);
    core.sys.posix.unistd.symlink((deleteme~"/a/b/c\0").ptr,
            (deleteme~"/link\0").ptr);
    rmdirRecurse(deleteme~"/link");
    enforce(exists(d));
    rmdirRecurse(deleteme);
    enforce(!exists(deleteme));

    d = deleteme~"/a/b/c/d/e/f/g";
    mkdirRecurse(d);
    version (Android) string link_cmd = "ln -s ";
    else string link_cmd = "ln -sf ";
    executeShell(link_cmd~deleteme~"/a/b/c "~deleteme~"/link");
    rmdirRecurse(deleteme);
    enforce(!exists(deleteme));
}

@system unittest
{
    void[] buf;

    buf = new void[10];
    (cast(byte[]) buf)[] = 3;
    string unit_file = deleteme ~ "-unittest_write.tmp";
    if (exists(unit_file)) remove(unit_file);
    write(unit_file, buf);
    void[] buf2 = read(unit_file);
    assert(buf == buf2);

    string unit2_file = deleteme ~ "-unittest_write2.tmp";
    copy(unit_file, unit2_file);
    buf2 = read(unit2_file);
    assert(buf == buf2);

    remove(unit_file);
    assert(!exists(unit_file));
    remove(unit2_file);
    assert(!exists(unit2_file));
}

/**
 * Dictates directory spanning policy for $(D_PARAM dirEntries) (see below).
 */
enum SpanMode
{
    /** Only spans one directory. */
    shallow,
    /** Spans the directory in
     $(HTTPS en.wikipedia.org/wiki/Tree_traversal#Post-order,
     _depth-first $(B post)-order), i.e. the content of any
     subdirectory is spanned before that subdirectory itself. Useful
     e.g. when recursively deleting files.  */
    depth,
    /** Spans the directory in
    $(HTTPS en.wikipedia.org/wiki/Tree_traversal#Pre-order, depth-first
    $(B pre)-order), i.e. the content of any subdirectory is spanned
    right after that subdirectory itself.

    Note that $(D SpanMode.breadth) will not result in all directory
    members occurring before any subdirectory members, i.e. it is not
    _true
    $(HTTPS en.wikipedia.org/wiki/Tree_traversal#Breadth-first_search,
    _breadth-first traversal).
    */
    breadth,
}

private struct DirIteratorImpl
{
    import std.array : Appender, appender;
    SpanMode _mode;
    // Whether we should follow symlinked directories while iterating.
    // It also indicates whether we should avoid functions which call
    // stat (since we should only need lstat in this case and it would
    // be more efficient to not call stat in addition to lstat).
    bool _followSymlink;
    DirEntry _cur;
    Appender!(DirHandle[]) _stack;
    Appender!(DirEntry[]) _stashed; //used in depth first mode
    //stack helpers
    void pushExtra(DirEntry de){ _stashed.put(de); }
    //ditto
    bool hasExtra(){ return !_stashed.data.empty; }
    //ditto
    DirEntry popExtra()
    {
        DirEntry de;
        de = _stashed.data[$-1];
        _stashed.shrinkTo(_stashed.data.length - 1);
        return de;

    }
    version (Windows)
    {
        struct DirHandle
        {
            string dirpath;
            HANDLE h;
        }

        bool stepIn(string directory)
        {
            import std.path : chainPath;

            auto search_pattern = chainPath(directory, "*.*");
            WIN32_FIND_DATAW findinfo;
            HANDLE h = FindFirstFileW(search_pattern.tempCString!FSChar(), &findinfo);
            cenforce(h != INVALID_HANDLE_VALUE, directory);
            _stack.put(DirHandle(directory, h));
            return toNext(false, &findinfo);
        }

        bool next()
        {
            if (_stack.data.empty)
                return false;
            WIN32_FIND_DATAW findinfo;
            return toNext(true, &findinfo);
        }

        bool toNext(bool fetch, WIN32_FIND_DATAW* findinfo)
        {
            import core.stdc.wchar_ : wcscmp;

            if (fetch)
            {
                if (FindNextFileW(_stack.data[$-1].h, findinfo) == FALSE)
                {
                    popDirStack();
                    return false;
                }
            }
            while ( wcscmp(findinfo.cFileName.ptr, ".") == 0
                    || wcscmp(findinfo.cFileName.ptr, "..") == 0)
                if (FindNextFileW(_stack.data[$-1].h, findinfo) == FALSE)
                {
                    popDirStack();
                    return false;
                }
            _cur = DirEntry(_stack.data[$-1].dirpath, findinfo);
            return true;
        }

        void popDirStack()
        {
            assert(!_stack.data.empty);
            FindClose(_stack.data[$-1].h);
            _stack.shrinkTo(_stack.data.length-1);
        }

        void releaseDirStack()
        {
            foreach ( d;  _stack.data)
                FindClose(d.h);
        }

        bool mayStepIn()
        {
            return _followSymlink ? _cur.isDir : _cur.isDir && !_cur.isSymlink;
        }
    }
    else version (Posix)
    {
        struct DirHandle
        {
            string dirpath;
            DIR*   h;
        }

        bool stepIn(string directory)
        {
            auto h = directory.length ? opendir(directory.tempCString()) : opendir(".");
            cenforce(h, directory);
            _stack.put(DirHandle(directory, h));
            return next();
        }

        bool next()
        {
            if (_stack.data.empty)
                return false;
            for (dirent* fdata; (fdata = readdir(_stack.data[$-1].h)) != null; )
            {
                // Skip "." and ".."
                if (core.stdc.string.strcmp(fdata.d_name.ptr, ".")  &&
                   core.stdc.string.strcmp(fdata.d_name.ptr, "..") )
                {
                    _cur = DirEntry(_stack.data[$-1].dirpath, fdata);
                    return true;
                }
            }
            popDirStack();
            return false;
        }

        void popDirStack()
        {
            assert(!_stack.data.empty);
            closedir(_stack.data[$-1].h);
            _stack.shrinkTo(_stack.data.length-1);
        }

        void releaseDirStack()
        {
            foreach ( d;  _stack.data)
                closedir(d.h);
        }

        bool mayStepIn()
        {
            return _followSymlink ? _cur.isDir : attrIsDir(_cur.linkAttributes);
        }
    }

    this(R)(R pathname, SpanMode mode, bool followSymlink)
        if (isInputRange!R && isSomeChar!(ElementEncodingType!R))
    {
        _mode = mode;
        _followSymlink = followSymlink;
        _stack = appender(cast(DirHandle[])[]);
        if (_mode == SpanMode.depth)
            _stashed = appender(cast(DirEntry[])[]);

        static if (isNarrowString!R && is(Unqual!(ElementEncodingType!R) == char))
            alias pathnameStr = pathname;
        else
        {
            import std.array : array;
            string pathnameStr = pathname.array;
        }
        if (stepIn(pathnameStr))
        {
            if (_mode == SpanMode.depth)
                while (mayStepIn())
                {
                    auto thisDir = _cur;
                    if (stepIn(_cur.name))
                    {
                        pushExtra(thisDir);
                    }
                    else
                        break;
                }
        }
    }
    @property bool empty(){ return _stashed.data.empty && _stack.data.empty; }
    @property DirEntry front(){ return _cur; }
    void popFront()
    {
        switch (_mode)
        {
        case SpanMode.depth:
            if (next())
            {
                while (mayStepIn())
                {
                    auto thisDir = _cur;
                    if (stepIn(_cur.name))
                    {
                        pushExtra(thisDir);
                    }
                    else
                        break;
                }
            }
            else if (hasExtra())
                _cur = popExtra();
            break;
        case SpanMode.breadth:
            if (mayStepIn())
            {
                if (!stepIn(_cur.name))
                    while (!empty && !next()){}
            }
            else
                while (!empty && !next()){}
            break;
        default:
            next();
        }
    }

    ~this()
    {
        releaseDirStack();
    }
}

struct DirIterator
{
private:
    RefCounted!(DirIteratorImpl, RefCountedAutoInitialize.no) impl;
    this(string pathname, SpanMode mode, bool followSymlink)
    {
        impl = typeof(impl)(pathname, mode, followSymlink);
    }
public:
    @property bool empty(){ return impl.empty; }
    @property DirEntry front(){ return impl.front; }
    void popFront(){ impl.popFront(); }

}
/++
    Returns an input range of $(D DirEntry) that lazily iterates a given directory,
    also provides two ways of foreach iteration. The iteration variable can be of
    type $(D string) if only the name is needed, or $(D DirEntry)
    if additional details are needed. The span _mode dictates how the
    directory is traversed. The name of each iterated directory entry
    contains the absolute _path.

    Params:
        path = The directory to iterate over.
               If empty, the current directory will be iterated.

        pattern = Optional string with wildcards, such as $(RED
                  "*.d"). When present, it is used to filter the
                  results by their file name. The supported wildcard
                  strings are described under $(REF globMatch,
                  std,_path).

        mode = Whether the directory's sub-directories should be
               iterated in depth-first port-order ($(LREF depth)),
               depth-first pre-order ($(LREF breadth)), or not at all
               ($(LREF shallow)).

        followSymlink = Whether symbolic links which point to directories
                         should be treated as directories and their contents
                         iterated over.

    Throws:
        $(D FileException) if the directory does not exist.

Example:
--------------------
// Iterate a directory in depth
foreach (string name; dirEntries("destroy/me", SpanMode.depth))
{
    remove(name);
}

// Iterate the current directory in breadth
foreach (string name; dirEntries("", SpanMode.breadth))
{
    writeln(name);
}

// Iterate a directory and get detailed info about it
foreach (DirEntry e; dirEntries("dmd-testing", SpanMode.breadth))
{
    writeln(e.name, "\t", e.size);
}

// Iterate over all *.d files in current directory and all its subdirectories
auto dFiles = dirEntries("", SpanMode.depth).filter!(f => f.name.endsWith(".d"));
foreach (d; dFiles)
    writeln(d.name);

// Hook it up with std.parallelism to compile them all in parallel:
foreach (d; parallel(dFiles, 1)) //passes by 1 file to each thread
{
    string cmd = "dmd -c "  ~ d.name;
    writeln(cmd);
    std.process.system(cmd);
}

// Iterate over all D source files in current directory and all its
// subdirectories
auto dFiles = dirEntries("","*.{d,di}",SpanMode.depth);
foreach (d; dFiles)
    writeln(d.name);
--------------------
 +/
auto dirEntries(string path, SpanMode mode, bool followSymlink = true)
{
    return DirIterator(path, mode, followSymlink);
}

/// Duplicate functionality of D1's $(D std.file.listdir()):
@safe unittest
{
    string[] listdir(string pathname)
    {
        import std.algorithm;
        import std.array;
        import std.file;
        import std.path;

        return std.file.dirEntries(pathname, SpanMode.shallow)
            .filter!(a => a.isFile)
            .map!(a => std.path.baseName(a.name))
            .array;
    }

    void main(string[] args)
    {
        import std.stdio;

        string[] files = listdir(args[1]);
        writefln("%s", files);
     }
}

@system unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map;
    import std.algorithm.searching : startsWith;
    import std.array : array;
    import std.conv : to;
    import std.path : buildPath, absolutePath;
    import std.file : dirEntries;
    import std.process : thisProcessID;
    import std.range.primitives : walkLength;

    version (Android)
        string testdir = deleteme; // This has to be an absolute path when
                                   // called from a shared library on Android,
                                   // ie an apk
    else
        string testdir = "deleteme.dmd.unittest.std.file" ~ to!string(thisProcessID); // needs to be relative
    mkdirRecurse(buildPath(testdir, "somedir"));
    scope(exit) rmdirRecurse(testdir);
    write(buildPath(testdir, "somefile"), null);
    write(buildPath(testdir, "somedir", "somedeepfile"), null);

    // testing range interface
    size_t equalEntries(string relpath, SpanMode mode)
    {
        import std.exception : enforce;
        auto len = enforce(walkLength(dirEntries(absolutePath(relpath), mode)));
        assert(walkLength(dirEntries(relpath, mode)) == len);
        assert(equal(
                   map!(a => absolutePath(a.name))(dirEntries(relpath, mode)),
                   map!(a => a.name)(dirEntries(absolutePath(relpath), mode))));
        return len;
    }

    assert(equalEntries(testdir, SpanMode.shallow) == 2);
    assert(equalEntries(testdir, SpanMode.depth) == 3);
    assert(equalEntries(testdir, SpanMode.breadth) == 3);

    // testing opApply
    foreach (string name; dirEntries(testdir, SpanMode.breadth))
    {
        //writeln(name);
        assert(name.startsWith(testdir));
    }
    foreach (DirEntry e; dirEntries(absolutePath(testdir), SpanMode.breadth))
    {
        //writeln(name);
        assert(e.isFile || e.isDir, e.name);
    }

    //issue 7264
    foreach (string name; dirEntries(testdir, "*.d", SpanMode.breadth))
    {

    }
    foreach (entry; dirEntries(testdir, SpanMode.breadth))
    {
        static assert(is(typeof(entry) == DirEntry));
    }
    //issue 7138
    auto a = array(dirEntries(testdir, SpanMode.shallow));

    // issue 11392
    auto dFiles = dirEntries(testdir, SpanMode.shallow);
    foreach (d; dFiles){}

    // issue 15146
    dirEntries("", SpanMode.shallow).walkLength();
}

/// Ditto
auto dirEntries(string path, string pattern, SpanMode mode,
    bool followSymlink = true)
{
    import std.algorithm.iteration : filter;
    import std.path : globMatch, baseName;

    bool f(DirEntry de) { return globMatch(baseName(de.name), pattern); }
    return filter!f(DirIterator(path, mode, followSymlink));
}

@system unittest
{
    import std.stdio : writefln;
    immutable dpath = deleteme ~ "_dir";
    immutable fpath = deleteme ~ "_file";
    immutable sdpath = deleteme ~ "_sdir";
    immutable sfpath = deleteme ~ "_sfile";
    scope(exit)
    {
        if (dpath.exists) rmdirRecurse(dpath);
        if (fpath.exists) remove(fpath);
        if (sdpath.exists) remove(sdpath);
        if (sfpath.exists) remove(sfpath);
    }

    mkdir(dpath);
    write(fpath, "hello world");
    version (Posix)
    {
        core.sys.posix.unistd.symlink((dpath ~ '\0').ptr, (sdpath ~ '\0').ptr);
        core.sys.posix.unistd.symlink((fpath ~ '\0').ptr, (sfpath ~ '\0').ptr);
    }

    static struct Flags { bool dir, file, link; }
    auto tests = [dpath : Flags(true), fpath : Flags(false, true)];
    version (Posix)
    {
        tests[sdpath] = Flags(true, false, true);
        tests[sfpath] = Flags(false, true, true);
    }

    auto past = Clock.currTime() - 2.seconds;
    auto future = past + 4.seconds;

    foreach (path, flags; tests)
    {
        auto de = DirEntry(path);
        assert(de.name == path);
        assert(de.isDir == flags.dir);
        assert(de.isFile == flags.file);
        assert(de.isSymlink == flags.link);

        assert(de.isDir == path.isDir);
        assert(de.isFile == path.isFile);
        assert(de.isSymlink == path.isSymlink);
        assert(de.size == path.getSize());
        assert(de.attributes == getAttributes(path));
        assert(de.linkAttributes == getLinkAttributes(path));

        scope(failure) writefln("[%s] [%s] [%s] [%s]", past, de.timeLastAccessed, de.timeLastModified, future);
        assert(de.timeLastAccessed > past);
        assert(de.timeLastAccessed < future);
        assert(de.timeLastModified > past);
        assert(de.timeLastModified < future);

        assert(attrIsDir(de.attributes) == flags.dir);
        assert(attrIsDir(de.linkAttributes) == (flags.dir && !flags.link));
        assert(attrIsFile(de.attributes) == flags.file);
        assert(attrIsFile(de.linkAttributes) == (flags.file && !flags.link));
        assert(!attrIsSymlink(de.attributes));
        assert(attrIsSymlink(de.linkAttributes) == flags.link);

        version (Windows)
        {
            assert(de.timeCreated > past);
            assert(de.timeCreated < future);
        }
        else version (Posix)
        {
            assert(de.timeStatusChanged > past);
            assert(de.timeStatusChanged < future);
            assert(de.attributes == de.statBuf.st_mode);
        }
    }
}


/**
 * Reads a file line by line and parses the line into a single value or a
 * $(REF Tuple, std,typecons) of values depending on the length of `Types`.
 * The lines are parsed using the specified format string. The format string is
 * passed to $(REF formattedRead, std,_format), and therefore must conform to the
 * _format string specification outlined in $(MREF std, _format).
 *
 * Params:
 *     Types = the types that each of the elements in the line should be returned as
 *     filename = the name of the file to read
 *     format = the _format string to use when reading
 *
 * Returns:
 *     If only one type is passed, then an array of that type. Otherwise, an
 *     array of $(REF Tuple, std,typecons)s.
 *
 * Throws:
 *     `Exception` if the format string is malformed. Also, throws `Exception`
 *     if any of the lines in the file are not fully consumed by the call
 *     to $(REF formattedRead, std,_format). Meaning that no empty lines or lines
 *     with extra characters are allowed.
 */
Select!(Types.length == 1, Types[0][], Tuple!(Types)[])
slurp(Types...)(string filename, in char[] format)
{
    import std.array : appender;
    import std.conv : text;
    import std.exception : enforce;
    import std.format : formattedRead;
    import std.stdio : File;

    auto app = appender!(typeof(return))();
    ElementType!(typeof(return)) toAdd;
    auto f = File(filename);
    scope(exit) f.close();
    foreach (line; f.byLine())
    {
        formattedRead(line, format, &toAdd);
        enforce(line.empty,
                text("Trailing characters at the end of line: `", line,
                        "'"));
        app.put(toAdd);
    }
    return app.data;
}

///
@system unittest
{
    import std.typecons : tuple;

    scope(exit)
    {
        assert(exists(deleteme));
        remove(deleteme);
    }

    write(deleteme, "12 12.25\n345 1.125"); // deleteme is the name of a temporary file

    // Load file; each line is an int followed by comma, whitespace and a
    // double.
    auto a = slurp!(int, double)(deleteme, "%s %s");
    assert(a.length == 2);
    assert(a[0] == tuple(12, 12.25));
    assert(a[1] == tuple(345, 1.125));
}


/**
Returns the path to a directory for temporary files.

On Windows, this function returns the result of calling the Windows API function
$(LINK2 http://msdn.microsoft.com/en-us/library/windows/desktop/aa364992.aspx, $(D GetTempPath)).

On POSIX platforms, it searches through the following list of directories
and returns the first one which is found to exist:
$(OL
    $(LI The directory given by the $(D TMPDIR) environment variable.)
    $(LI The directory given by the $(D TEMP) environment variable.)
    $(LI The directory given by the $(D TMP) environment variable.)
    $(LI $(D /tmp))
    $(LI $(D /var/tmp))
    $(LI $(D /usr/tmp))
)

On all platforms, $(D tempDir) returns $(D ".") on failure, representing
the current working directory.

The return value of the function is cached, so the procedures described
above will only be performed the first time the function is called.  All
subsequent runs will return the same string, regardless of whether
environment variables and directory structures have changed in the
meantime.

The POSIX $(D tempDir) algorithm is inspired by Python's
$(LINK2 http://docs.python.org/library/tempfile.html#tempfile.tempdir, $(D tempfile.tempdir)).
*/
string tempDir() @trusted
{
    static string cache;
    if (cache is null)
    {
        version (Windows)
        {
            import std.conv : to;
            // http://msdn.microsoft.com/en-us/library/windows/desktop/aa364992(v=vs.85).aspx
            wchar[MAX_PATH + 2] buf;
            DWORD len = GetTempPathW(buf.length, buf.ptr);
            if (len) cache = buf[0 .. len].to!string;
        }
        else version (Posix)
        {
            import std.process : environment;
            // This function looks through the list of alternative directories
            // and returns the first one which exists and is a directory.
            static string findExistingDir(T...)(lazy T alternatives)
            {
                foreach (dir; alternatives)
                    if (!dir.empty && exists(dir)) return dir;
                return null;
            }

            cache = findExistingDir(environment.get("TMPDIR"),
                                    environment.get("TEMP"),
                                    environment.get("TMP"),
                                    "/tmp",
                                    "/var/tmp",
                                    "/usr/tmp");
        }
        else static assert(false, "Unsupported platform");

        if (cache is null) cache = getcwd();
    }
    return cache;
}

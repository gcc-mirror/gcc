// Written in the D programming language.

/**
Utilities for manipulating files and scanning directories. Functions
in this module handle files as a unit, e.g., read or write one file
at a time. For opening files and manipulating them via handles refer
to module $(MREF std, stdio).

$(SCRIPT inhibitQuickIndex = 1;)
$(DIVC quickindex,
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
          $(LREF timeLastAccessed)
          $(LREF timeStatusChanged)
))
$(TR $(TD Other) $(TD
          $(LREF DirEntry)
          $(LREF FileException)
          $(LREF PreserveAttributes)
          $(LREF SpanMode)
          $(LREF getAvailableDiskSpace)
))
))


Copyright: Copyright The D Language Foundation 2007 - 2011.
See_Also:  The $(HTTP ddili.org/ders/d.en/files.html, official tutorial) for an
introduction to working with files in D, module
$(MREF std, stdio) for opening files and manipulating them via handles,
and module $(MREF std, path) for manipulating path strings.

License:   $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   $(HTTP digitalmars.com, Walter Bright),
           $(HTTP erdani.org, Andrei Alexandrescu),
           $(HTTP jmdavisprog.com, Jonathan M Davis)
Source:    $(PHOBOSSRC std/file.d)
 */
module std.file;

import core.stdc.errno, core.stdc.stdlib, core.stdc.string;
import core.time : abs, dur, hnsecs, seconds;

import std.datetime.date : DateTime;
import std.datetime.systime : Clock, SysTime, unixTimeToStdTime;
import std.internal.cstring;
import std.meta;
import std.range;
import std.traits;
import std.typecons;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Windows)
{
    import core.sys.windows.winbase, core.sys.windows.winnt, std.windows.syserror;
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
    private alias FSChar = WCHAR;       // WCHAR can be aliased to wchar or wchar_t
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
    import std.conv : text;
    import std.path : buildPath;
    import std.process : thisProcessID;

    enum base = "deleteme.dmd.unittest.pid";
    static string fileName;

    if (!fileName)
        fileName = text(buildPath(tempDir(), base), thisProcessID);
    return fileName;
}

version (StdUnittest) private struct TestAliasedString
{
    string get() @safe @nogc pure nothrow return scope { return _s; }
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

    private this(scope const(char)[] name, scope const(char)[] msg, string file, size_t line, uint errno) @safe pure
    {
        if (msg.empty)
            super(name is null ? "(null)" : name.idup, file, line);
        else
            super(text(name is null ? "(null)" : name, ": ", msg), file, line);

        this.errno = errno;
    }

    /++
        Constructor which takes an error message.

        Params:
            name = Name of file for which the error occurred.
            msg  = Message describing the error.
            file = The file where the error occurred.
            line = The _line where the error occurred.
     +/
    this(scope const(char)[] name, scope const(char)[] msg, string file = __FILE__, size_t line = __LINE__) @safe pure
    {
        this(name, msg, file, line, 0);
    }

    /++
        Constructor which takes the error number ($(LUCKY GetLastError)
        in Windows, $(D_PARAM errno) in POSIX).

        Params:
            name  = Name of file for which the error occurred.
            errno = The error number.
            file  = The file where the error occurred.
                    Defaults to `__FILE__`.
            line  = The _line where the error occurred.
                    Defaults to `__LINE__`.
     +/
    version (Windows) this(scope const(char)[] name,
                          uint errno = .GetLastError(),
                          string file = __FILE__,
                          size_t line = __LINE__) @safe
    {
        this(name, generateSysErrorMsg(errno), file, line, errno);
    }
    else version (Posix) this(scope const(char)[] name,
                             uint errno = .errno,
                             string file = __FILE__,
                             size_t line = __LINE__) @trusted
    {
        import std.exception : errnoString;
        this(name, errnoString(errno), file, line, errno);
    }
}

///
@safe unittest
{
    import std.exception : assertThrown;

    assertThrown!FileException("non.existing.file.".readText);
}

private T cenforce(T)(T condition, lazy scope const(char)[] name, string file = __FILE__, size_t line = __LINE__)
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
private T cenforce(T)(T condition, scope const(char)[] name, scope const(FSChar)* namez,
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
private T cenforce(T)(T condition, scope const(char)[] name, scope const(FSChar)* namez,
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

// https://issues.dlang.org/show_bug.cgi?id=17102
@safe unittest
{
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
Read entire contents of file `name` and returns it as an untyped
array. If the file size is larger than `upTo`, only `upTo`
bytes are _read.

Params:
    name = string or range of characters representing the file _name
    upTo = if present, the maximum number of bytes to _read

Returns: Untyped array of bytes _read.

Throws: $(LREF FileException) on error.
 */

void[] read(R)(R name, size_t upTo = size_t.max)
if (isSomeFiniteCharInputRange!R && !isConvertibleToString!R)
{
    static if (isNarrowString!R && is(immutable ElementEncodingType!R == immutable char))
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

    std.file.write(deleteme, "1234"); // deleteme is the name of a temporary file
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

version (Posix) private void[] readImpl(scope const(char)[] name, scope const(FSChar)* namez,
                                        size_t upTo = size_t.max) @trusted
{
    import core.memory : GC;
    import std.algorithm.comparison : min;
    import std.conv : to;
    import std.checkedint : checked;

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
    void[] result = GC.malloc(initialAlloc, GC.BlkAttr.NO_SCAN)[0 .. initialAlloc];
    scope(failure) GC.free(result.ptr);

    auto size = checked(size_t(0));

    for (;;)
    {
        immutable actual = core.sys.posix.unistd.read(fd, result.ptr + size.get,
                (min(result.length, upTo) - size).get);
        cenforce(actual != -1, name, namez);
        if (actual == 0) break;
        size += actual;
        if (size >= upTo) break;
        if (size < result.length) continue;
        immutable newAlloc = size + sizeIncrement;
        result = GC.realloc(result.ptr, newAlloc.get, GC.BlkAttr.NO_SCAN)[0 .. newAlloc.get];
    }

    return result.length - size >= maxSlackMemoryAllowed
        ? GC.realloc(result.ptr, size.get, GC.BlkAttr.NO_SCAN)[0 .. size.get]
        : result[0 .. size.get];
}

version (Windows)
private extern (Windows) @nogc nothrow
{
    pragma(mangle, CreateFileW.mangleof)
    HANDLE trustedCreateFileW(scope const(wchar)* namez, DWORD dwDesiredAccess,
        DWORD dwShareMode, SECURITY_ATTRIBUTES* lpSecurityAttributes,
        DWORD dwCreationDisposition, DWORD dwFlagsAndAttributes,
        HANDLE hTemplateFile)  @trusted;

    pragma(mangle, CloseHandle.mangleof) BOOL trustedCloseHandle(HANDLE) @trusted;
}

version (Windows) private void[] readImpl(scope const(char)[] name, scope const(FSChar)* namez,
                                          size_t upTo = size_t.max) @trusted
{
    import core.memory : GC;
    import std.algorithm.comparison : min;
    static trustedGetFileSize(HANDLE hFile, out ulong fileSize)
    {
        DWORD sizeHigh;
        DWORD sizeLow = GetFileSize(hFile, &sizeHigh);
        const bool result = sizeLow != INVALID_FILE_SIZE;
        if (result)
            fileSize = makeUlong(sizeLow, sizeHigh);
        return result;
    }
    static trustedReadFile(HANDLE hFile, void *lpBuffer, size_t nNumberOfBytesToRead)
    {
        // Read by chunks of size < 4GB (Windows API limit)
        size_t totalNumRead = 0;
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
    auto buf = () { return GC.malloc(size, GC.BlkAttr.NO_SCAN)[0 .. size]; } ();

    scope(failure)
    {
        () { GC.free(buf.ptr); } ();
    }

    if (size)
        cenforce(trustedReadFile(h, &buf[0], size), name, namez);
    return buf[0 .. size];
}

version (linux) @safe unittest
{
    // A file with "zero" length that doesn't have 0 length at all
    auto s = std.file.readText("/proc/cpuinfo");
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

/++
    Reads and validates (using $(REF validate, std, utf)) a text file. S can be
    an array of any character type. However, no width or endian conversions are
    performed. So, if the width or endianness of the characters in the given
    file differ from the width or endianness of the element type of S, then
    validation will fail.

    Params:
        S = the string type of the file
        name = string or range of characters representing the file _name

    Returns: Array of characters read.

    Throws: $(LREF FileException) if there is an error reading the file,
            $(REF UTFException, std, utf) on UTF decoding error.
+/
S readText(S = string, R)(auto ref R name)
if (isSomeString!S && (isSomeFiniteCharInputRange!R || is(StringTypeOf!R)))
{
    import std.algorithm.searching : startsWith;
    import std.encoding : getBOM, BOM;
    import std.exception : enforce;
    import std.format : format;
    import std.utf : UTFException, validate;

    static if (is(StringTypeOf!R))
        StringTypeOf!R filename = name;
    else
        auto filename = name;

    static auto trustedCast(T)(void[] buf) @trusted { return cast(T) buf; }
    auto data = trustedCast!(ubyte[])(read(filename));

    immutable bomSeq = getBOM(data);
    immutable bom = bomSeq.schema;

    static if (is(immutable ElementEncodingType!S == immutable char))
    {
        with(BOM) switch (bom)
        {
            case utf16be:
            case utf16le: throw new UTFException("UTF-8 requested. BOM is for UTF-16");
            case utf32be:
            case utf32le: throw new UTFException("UTF-8 requested. BOM is for UTF-32");
            default: break;
        }
    }
    else static if (is(immutable ElementEncodingType!S == immutable wchar))
    {
        with(BOM) switch (bom)
        {
            case utf8: throw new UTFException("UTF-16 requested. BOM is for UTF-8");
            case utf16be:
            {
                version (BigEndian)
                    break;
                else
                    throw new UTFException("BOM is for UTF-16 LE on Big Endian machine");
            }
            case utf16le:
            {
                version (BigEndian)
                    throw new UTFException("BOM is for UTF-16 BE on Little Endian machine");
                else
                    break;
            }
            case utf32be:
            case utf32le: throw new UTFException("UTF-8 requested. BOM is for UTF-32");
            default: break;
        }
    }
    else
    {
        with(BOM) switch (bom)
        {
            case utf8: throw new UTFException("UTF-16 requested. BOM is for UTF-8");
            case utf16be:
            case utf16le: throw new UTFException("UTF-8 requested. BOM is for UTF-16");
            case utf32be:
            {
                version (BigEndian)
                    break;
                else
                    throw new UTFException("BOM is for UTF-32 LE on Big Endian machine");
            }
            case utf32le:
            {
                version (BigEndian)
                    throw new UTFException("BOM is for UTF-32 BE on Little Endian machine");
                else
                    break;
            }
            default: break;
        }
    }

    if (data.length % ElementEncodingType!S.sizeof != 0)
        throw new UTFException(format!"The content of %s is not UTF-%s"(filename, ElementEncodingType!S.sizeof * 8));

    auto result = trustedCast!S(data);
    validate(result);
    return result;
}

/// Read file with UTF-8 text.
@safe unittest
{
    write(deleteme, "abc"); // deleteme is the name of a temporary file
    scope(exit) remove(deleteme);
    string content = readText(deleteme);
    assert(content == "abc");
}

// Read file with UTF-8 text but try to read it as UTF-16.
@safe unittest
{
    import std.exception : assertThrown;
    import std.utf : UTFException;

    write(deleteme, "abc");
    scope(exit) remove(deleteme);
    // Throws because the file is not valid UTF-16.
    assertThrown!UTFException(readText!wstring(deleteme));
}

// Read file with UTF-16 text.
@safe unittest
{
    import std.algorithm.searching : skipOver;

    write(deleteme, "\uFEFFabc"w); // With BOM
    scope(exit) remove(deleteme);
    auto content = readText!wstring(deleteme);
    assert(content == "\uFEFFabc"w);
    // Strips BOM if present.
    content.skipOver('\uFEFF');
    assert(content == "abc"w);
}

@safe unittest
{
    static assert(__traits(compiles, readText(TestAliasedString(null))));
}

@safe unittest
{
    import std.array : appender;
    import std.bitmanip : append, Endian;
    import std.exception : assertThrown;
    import std.path : buildPath;
    import std.string : representation;
    import std.utf : UTFException;

    mkdir(deleteme);
    scope(exit) rmdirRecurse(deleteme);

    immutable none8 = buildPath(deleteme, "none8");
    immutable none16 = buildPath(deleteme, "none16");
    immutable utf8 = buildPath(deleteme, "utf8");
    immutable utf16be = buildPath(deleteme, "utf16be");
    immutable utf16le = buildPath(deleteme, "utf16le");
    immutable utf32be = buildPath(deleteme, "utf32be");
    immutable utf32le = buildPath(deleteme, "utf32le");
    immutable utf7 = buildPath(deleteme, "utf7");

    write(none8, "京都市");
    write(none16, "京都市"w);
    write(utf8, (cast(char[])[0xEF, 0xBB, 0xBF]) ~ "京都市");
    {
        auto str = "\uFEFF京都市"w;
        auto arr = appender!(ubyte[])();
        foreach (c; str)
            arr.append(c);
        write(utf16be, arr.data);
    }
    {
        auto str = "\uFEFF京都市"w;
        auto arr = appender!(ubyte[])();
        foreach (c; str)
            arr.append!(ushort, Endian.littleEndian)(c);
        write(utf16le, arr.data);
    }
    {
        auto str = "\U0000FEFF京都市"d;
        auto arr = appender!(ubyte[])();
        foreach (c; str)
            arr.append(c);
        write(utf32be, arr.data);
    }
    {
        auto str = "\U0000FEFF京都市"d;
        auto arr = appender!(ubyte[])();
        foreach (c; str)
            arr.append!(uint, Endian.littleEndian)(c);
        write(utf32le, arr.data);
    }
    write(utf7, (cast(ubyte[])[0x2B, 0x2F, 0x76, 0x38, 0x2D]) ~ "foobar".representation);

    assertThrown!UTFException(readText(none16));
    assert(readText(utf8) == (cast(char[])[0xEF, 0xBB, 0xBF]) ~ "京都市");
    assertThrown!UTFException(readText(utf16be));
    assertThrown!UTFException(readText(utf16le));
    assertThrown!UTFException(readText(utf32be));
    assertThrown!UTFException(readText(utf32le));
    assert(readText(utf7) == (cast(char[])[0x2B, 0x2F, 0x76, 0x38, 0x2D]) ~ "foobar");

    assertThrown!UTFException(readText!wstring(none8));
    assert(readText!wstring(none16) == "京都市"w);
    assertThrown!UTFException(readText!wstring(utf8));
    version (BigEndian)
    {
        assert(readText!wstring(utf16be) == "\uFEFF京都市"w);
        assertThrown!UTFException(readText!wstring(utf16le));
    }
    else
    {
        assertThrown!UTFException(readText!wstring(utf16be));
        assert(readText!wstring(utf16le) == "\uFEFF京都市"w);
    }
    assertThrown!UTFException(readText!wstring(utf32be));
    assertThrown!UTFException(readText!wstring(utf32le));
    assertThrown!UTFException(readText!wstring(utf7));

    assertThrown!UTFException(readText!dstring(utf8));
    assertThrown!UTFException(readText!dstring(utf16be));
    assertThrown!UTFException(readText!dstring(utf16le));
    version (BigEndian)
    {
       assert(readText!dstring(utf32be) == "\U0000FEFF京都市"d);
       assertThrown!UTFException(readText!dstring(utf32le));
    }
    else
    {
       assertThrown!UTFException(readText!dstring(utf32be));
       assert(readText!dstring(utf32le) == "\U0000FEFF京都市"d);
    }
    assertThrown!UTFException(readText!dstring(utf7));
}

/*********************************************
Write `buffer` to file `name`.

Creates the file if it does not already exist.

Params:
    name = string or range of characters representing the file _name
    buffer = data to be written to file

Throws: $(LREF FileException) on error.

See_also: $(REF toFile, std,stdio)
 */
void write(R)(R name, const void[] buffer)
if ((isSomeFiniteCharInputRange!R || isSomeString!R) && !isConvertibleToString!R)
{
    static if (isNarrowString!R && is(immutable ElementEncodingType!R == immutable char))
        writeImpl(name, name.tempCString!FSChar(), buffer, false);
    else
        writeImpl(null, name.tempCString!FSChar(), buffer, false);
}

///
@safe unittest
{
   scope(exit)
   {
       assert(exists(deleteme));
       remove(deleteme);
   }

   int[] a = [ 0, 1, 1, 2, 3, 5, 8 ];
   write(deleteme, a); // deleteme is the name of a temporary file
   const bytes = read(deleteme);
   const fileInts = () @trusted { return cast(int[]) bytes; }();
   assert(fileInts == a);
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
Appends `buffer` to file `name`.

Creates the file if it does not already exist.

Params:
    name = string or range of characters representing the file _name
    buffer = data to be appended to file

Throws: $(LREF FileException) on error.
 */
void append(R)(R name, const void[] buffer)
if ((isSomeFiniteCharInputRange!R || isSomeString!R) && !isConvertibleToString!R)
{
    static if (isNarrowString!R && is(immutable ElementEncodingType!R == immutable char))
        writeImpl(name, name.tempCString!FSChar(), buffer, true);
    else
        writeImpl(null, name.tempCString!FSChar(), buffer, true);
}

///
@safe unittest
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
   const bytes = read(deleteme);
   const fileInts = () @trusted { return cast(int[]) bytes; }();
   assert(fileInts == a ~ b);
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

// POSIX implementation helper for write and append

version (Posix) private void writeImpl(scope const(char)[] name, scope const(FSChar)* namez,
        scope const(void)[] buffer, bool append) @trusted
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

version (Windows) private void writeImpl(scope const(char)[] name, scope const(FSChar)* namez,
        scope const(void)[] buffer, bool append) @trusted
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
 * Rename file `from` _to `to`, moving it between directories if required.
 * If the target file exists, it is overwritten.
 *
 * It is not possible to rename a file across different mount points
 * or drives. On POSIX, the operation is atomic. That means, if `to`
 * already exists there will be no time period during the operation
 * where `to` is missing. See
 * $(HTTP man7.org/linux/man-pages/man2/rename.2.html, manpage for rename)
 * for more details.
 *
 * Params:
 *    from = string or range of characters representing the existing file name
 *    to = string or range of characters representing the target file name
 *
 * Throws: $(LREF FileException) on error.
 */
void rename(RF, RT)(RF from, RT to)
if ((isSomeFiniteCharInputRange!RF || isSomeString!RF) && !isConvertibleToString!RF &&
    (isSomeFiniteCharInputRange!RT || isSomeString!RT) && !isConvertibleToString!RT)
{
    // Place outside of @trusted block
    auto fromz = from.tempCString!FSChar();
    auto toz = to.tempCString!FSChar();

    static if (isNarrowString!RF && is(immutable ElementEncodingType!RF == immutable char))
        alias f = from;
    else
        enum string f = null;

    static if (isNarrowString!RT && is(immutable ElementEncodingType!RT == immutable char))
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

///
@safe unittest
{
    auto t1 = deleteme, t2 = deleteme~"2";
    scope(exit) foreach (t; [t1, t2]) if (t.exists) t.remove();

    t1.write("1");
    t1.rename(t2);
    assert(t2.readText == "1");

    t1.write("2");
    t1.rename(t2);
    assert(t2.readText == "2");
}

private void renameImpl(scope const(char)[] f, scope const(char)[] t,
                        scope const(FSChar)* fromz, scope const(FSChar)* toz) @trusted
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
Delete file `name`.

Params:
    name = string or range of characters representing the file _name

Throws: $(LREF FileException) on error.
 */
void remove(R)(R name)
if (isSomeFiniteCharInputRange!R && !isConvertibleToString!R)
{
    static if (isNarrowString!R && is(immutable ElementEncodingType!R == immutable char))
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

///
@safe unittest
{
    import std.exception : assertThrown;

    deleteme.write("Hello");
    assert(deleteme.readText == "Hello");

    deleteme.remove;
    assertThrown!FileException(deleteme.readText);
}

@safe unittest
{
    static assert(__traits(compiles, remove(TestAliasedString("foo"))));
}

private void removeImpl(scope const(char)[] name, scope const(FSChar)* namez) @trusted
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

            auto len = namez ? strlen(namez) : 0;
            name = namez[0 .. len];
        }
        cenforce(core.stdc.stdio.remove(namez) == 0,
            "Failed to remove file " ~ (name is null ? "(null)" : name));
    }
}

@safe unittest
{
    import std.exception : collectExceptionMsg, assertThrown;

    string filename = null; // e.g. as returned by File.tmpfile.name

    version (linux)
    {
        // exact exception message is OS-dependent
        auto msg = filename.remove.collectExceptionMsg!FileException;
        assert("Failed to remove file (null): Bad address" == msg, msg);
    }
    else version (Windows)
    {
        import std.algorithm.searching : startsWith;

        // don't test exact message on windows, it's language dependent
        auto msg = filename.remove.collectExceptionMsg!FileException;
        assert(msg.startsWith("(null):"), msg);
    }
    else
    {
        assertThrown!FileException(filename.remove);
    }
}

version (Windows) private WIN32_FILE_ATTRIBUTE_DATA getFileAttributesWin(R)(R name)
if (isSomeFiniteCharInputRange!R)
{
    auto namez = name.tempCString!FSChar();

    WIN32_FILE_ATTRIBUTE_DATA fad = void;

    static if (isNarrowString!R && is(immutable ElementEncodingType!R == immutable char))
    {
        static void getFA(scope const(char)[] name, scope const(FSChar)* namez,
                          out WIN32_FILE_ATTRIBUTE_DATA fad) @trusted
        {
            import std.exception : enforce;
            enforce(GetFileAttributesExW(namez, GET_FILEEX_INFO_LEVELS.GetFileExInfoStandard, &fad),
                new FileException(name.idup));
        }
        getFA(name, namez, fad);
    }
    else
    {
        static void getFA(scope const(FSChar)* namez, out WIN32_FILE_ATTRIBUTE_DATA fad) @trusted
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

version (Posix) private extern (C) pragma(mangle, stat.mangleof)
int trustedStat(scope const(FSChar)* namez, ref stat_t buf) @nogc nothrow @trusted;

/**
Get size of file `name` in bytes.

Params:
    name = string or range of characters representing the file _name
Returns:
    The size of file in bytes.
Throws:
    $(LREF FileException) on error (e.g., file not found).
 */
ulong getSize(R)(R name)
if (isSomeFiniteCharInputRange!R && !isConvertibleToString!R)
{
    version (Windows)
    {
        with (getFileAttributesWin(name))
            return makeUlong(nFileSizeLow, nFileSizeHigh);
    }
    else version (Posix)
    {
        auto namez = name.tempCString();

        static if (isNarrowString!R && is(immutable ElementEncodingType!R == immutable char))
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

///
@safe unittest
{
    scope(exit) deleteme.remove;

    // create a file of size 1
    write(deleteme, "a");
    assert(getSize(deleteme) == 1);

    // create a file of size 3
    write(deleteme, "abc");
    assert(getSize(deleteme) == 3);
}

@safe unittest
{
    // create a file of size 1
    write(deleteme, "a");
    scope(exit) deleteme.exists && deleteme.remove;
    assert(getSize(deleteme) == 1);
    // create a file of size 3
    write(deleteme, "abc");
    import std.utf : byChar;
    assert(getSize(deleteme.byChar) == 3);
}

// Reads a time field from a stat_t with full precision.
version (Posix)
private SysTime statTimeToStdTime(char which)(ref const stat_t statbuf)
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
    Get the access and modified times of file or folder `name`.

    Params:
        name             = File/Folder _name to get times for.
        accessTime       = Time the file/folder was last accessed.
        modificationTime = Time the file/folder was last modified.

    Throws:
        $(LREF FileException) on error.
 +/
void getTimes(R)(R name,
              out SysTime accessTime,
              out SysTime modificationTime)
if (isSomeFiniteCharInputRange!R && !isConvertibleToString!R)
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

        stat_t statbuf = void;

        static if (isNarrowString!R && is(immutable ElementEncodingType!R == immutable char))
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

///
@safe unittest
{
    import std.datetime : abs, SysTime;

    scope(exit) deleteme.remove;
    write(deleteme, "a");

    SysTime accessTime, modificationTime;

    getTimes(deleteme, accessTime, modificationTime);

    import std.datetime : Clock, seconds;
    auto currTime = Clock.currTime();
    enum leeway = 5.seconds;

    auto diffAccess = accessTime - currTime;
    auto diffModification = modificationTime - currTime;
    assert(abs(diffAccess) <= leeway);
    assert(abs(diffModification) <= leeway);
}

@safe unittest
{
    SysTime atime, mtime;
    static assert(__traits(compiles, getTimes(TestAliasedString("foo"), atime, mtime)));
}

@safe unittest
{
    import std.stdio : writefln;

    auto currTime = Clock.currTime();

    write(deleteme, "a");
    scope(exit) assert(deleteme.exists), deleteme.remove;

    SysTime accessTime1;
    SysTime modificationTime1;

    getTimes(deleteme, accessTime1, modificationTime1);

    enum leeway = 5.seconds;

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

     Get creation/access/modified times of file `name`.

     This is the same as `getTimes` except that it also gives you the file
     creation time - which isn't possible on POSIX systems.

     Params:
     name                 = File _name to get times for.
     fileCreationTime     = Time the file was created.
     fileAccessTime       = Time the file was last accessed.
     fileModificationTime = Time the file was last modified.

     Throws:
     $(LREF FileException) on error.
     +/
    void getTimesWin(R)(R name,
                        out SysTime fileCreationTime,
                        out SysTime fileAccessTime,
                        out SysTime fileModificationTime)
    if (isSomeFiniteCharInputRange!R || isConvertibleToString!R);
    // above line contains both constraints for docs
    // (so users know how it can be called)
}
else version (Windows)
{
    void getTimesWin(R)(R name,
                        out SysTime fileCreationTime,
                        out SysTime fileAccessTime,
                        out SysTime fileModificationTime)
    if (isSomeFiniteCharInputRange!R && !isConvertibleToString!R)
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

version (Darwin)
private
{
    import core.stdc.config : c_ulong;
    enum ATTR_CMN_MODTIME  = 0x00000400, ATTR_CMN_ACCTIME  = 0x00001000;
    alias attrgroup_t = uint;
    static struct attrlist
    {
        ushort bitmapcount, reserved;
        attrgroup_t commonattr, volattr, dirattr, fileattr, forkattr;
    }
    extern(C) int setattrlist(scope const(char)* path, scope ref attrlist attrs,
        scope void* attrbuf, size_t attrBufSize, c_ulong options) nothrow @nogc @system;
}

/++
    Set access/modified times of file or folder `name`.

    Params:
        name             = File/Folder _name to get times for.
        accessTime       = Time the file/folder was last accessed.
        modificationTime = Time the file/folder was last modified.

    Throws:
        $(LREF FileException) on error.
 +/
void setTimes(R)(R name,
              SysTime accessTime,
              SysTime modificationTime)
if (isSomeFiniteCharInputRange!R && !isConvertibleToString!R)
{
    auto namez = name.tempCString!FSChar();
    static if (isNarrowString!R && is(immutable ElementEncodingType!R == immutable char))
        alias names = name;
    else
        string names = null;
    setTimesImpl(names, namez, accessTime, modificationTime);
}

///
@safe unittest
{
    import std.datetime : DateTime, hnsecs, SysTime;

    scope(exit) deleteme.remove;
    write(deleteme, "a");

    SysTime accessTime = SysTime(DateTime(2010, 10, 4, 0, 0, 30));
    SysTime modificationTime = SysTime(DateTime(2018, 10, 4, 0, 0, 30));
    setTimes(deleteme, accessTime, modificationTime);

    SysTime accessTimeResolved, modificationTimeResolved;
    getTimes(deleteme, accessTimeResolved, modificationTimeResolved);

    assert(accessTime == accessTimeResolved);
    assert(modificationTime == modificationTimeResolved);
}

/// ditto
void setTimes(R)(auto ref R name,
              SysTime accessTime,
              SysTime modificationTime)
if (isConvertibleToString!R)
{
    setTimes!(StringTypeOf!R)(name, accessTime, modificationTime);
}

private void setTimesImpl(scope const(char)[] names, scope const(FSChar)* namez,
    SysTime accessTime, SysTime modificationTime) @trusted
{
    version (Windows)
    {
        import std.datetime.systime : SysTimeToFILETIME;
        const ta = SysTimeToFILETIME(accessTime);
        const tm = SysTimeToFILETIME(modificationTime);
        alias defaults =
            AliasSeq!(FILE_WRITE_ATTRIBUTES,
                      0,
                      null,
                      OPEN_EXISTING,
                      FILE_ATTRIBUTE_NORMAL |
                      FILE_ATTRIBUTE_DIRECTORY |
                      FILE_FLAG_BACKUP_SEMANTICS,
                      HANDLE.init);
        auto h = CreateFileW(namez, defaults);

        cenforce(h != INVALID_HANDLE_VALUE, names, namez);

        scope(exit)
            cenforce(CloseHandle(h), names, namez);

        cenforce(SetFileTime(h, null, &ta, &tm), names, namez);
    }
    else
    {
        static if (is(typeof(&utimensat)))
        {
            timespec[2] t = void;
            t[0] = accessTime.toTimeSpec();
            t[1] = modificationTime.toTimeSpec();
            cenforce(utimensat(AT_FDCWD, namez, t, 0) == 0, names, namez);
        }
        else
        {
            version (Darwin)
            {
                // Set modification & access times with setattrlist to avoid precision loss.
                attrlist attrs = { bitmapcount: 5, reserved: 0,
                        commonattr: ATTR_CMN_MODTIME | ATTR_CMN_ACCTIME,
                        volattr: 0, dirattr: 0, fileattr: 0, forkattr: 0 };
                timespec[2] attrbuf = [modificationTime.toTimeSpec(), accessTime.toTimeSpec()];
                if (0 == setattrlist(namez, attrs, &attrbuf, attrbuf.sizeof, 0))
                    return;
                if (.errno != ENOTSUP)
                    cenforce(false, names, namez);
                // Not all volumes support setattrlist. In such cases
                // fall through to the utimes implementation.
            }
            timeval[2] t = void;
            t[0] = accessTime.toTimeVal();
            t[1] = modificationTime.toTimeVal();
            cenforce(utimes(namez, t) == 0, names, namez);
        }
    }
}

@safe unittest
{
    if (false) // Test instatiation
        setTimes(TestAliasedString("foo"), SysTime.init, SysTime.init);
}

@safe unittest
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

// https://issues.dlang.org/show_bug.cgi?id=23683
@safe unittest
{
    scope(exit) deleteme.remove;
    import std.stdio : File;
    auto f = File(deleteme, "wb");
    SysTime time = SysTime(DateTime(2018, 10, 4, 0, 0, 30));
    setTimes(deleteme, time, time);
}

/++
    Returns the time that the given file was last modified.

    Params:
        name = the name of the file to check
    Returns:
        A $(REF SysTime,std,datetime,systime).
    Throws:
        $(LREF FileException) if the given file does not exist.
+/
SysTime timeLastModified(R)(R name)
if (isSomeFiniteCharInputRange!R && !isConvertibleToString!R)
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
        stat_t statbuf = void;

        static if (isNarrowString!R && is(immutable ElementEncodingType!R == immutable char))
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

///
@safe unittest
{
    import std.datetime : abs, DateTime, hnsecs, SysTime;
    scope(exit) deleteme.remove;

    import std.datetime : Clock, seconds;
    auto currTime = Clock.currTime();
    enum leeway = 5.seconds;
    deleteme.write("bb");
    assert(abs(deleteme.timeLastModified - currTime) <= leeway);
}

@safe unittest
{
    static assert(__traits(compiles, timeLastModified(TestAliasedString("foo"))));
}

/++
    Returns the time that the given file was last modified. If the
    file does not exist, returns `returnIfMissing`.

    A frequent usage pattern occurs in build automation tools such as
    $(HTTP gnu.org/software/make, make) or $(HTTP
    en.wikipedia.org/wiki/Apache_Ant, ant). To check whether file $(D
    target) must be rebuilt from file `source` (i.e., `target` is
    older than `source` or does not exist), use the comparison
    below. The code throws a $(LREF FileException) if `source` does not
    exist (as it should). On the other hand, the `SysTime.min` default
    makes a non-existing `target` seem infinitely old so the test
    correctly prompts building it.

    Params:
        name = The name of the file to get the modification time for.
        returnIfMissing = The time to return if the given file does not exist.
    Returns:
        A $(REF SysTime,std,datetime,systime).

Example:
--------------------
if (source.timeLastModified >= target.timeLastModified(SysTime.min))
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
if (isSomeFiniteCharInputRange!R)
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
        stat_t statbuf = void;

        return trustedStat(namez, statbuf) != 0 ?
               returnIfMissing :
               statTimeToStdTime!'m'(statbuf);
    }
}

///
@safe unittest
{
    import std.datetime : SysTime;

    assert("file.does.not.exist".timeLastModified(SysTime.min) == SysTime.min);

    auto source = deleteme ~ "source";
    auto target = deleteme ~ "target";
    scope(exit) source.remove, target.remove;

    source.write(".");
    assert(target.timeLastModified(SysTime.min) < source.timeLastModified);
    target.write(".");
    assert(target.timeLastModified(SysTime.min) >= source.timeLastModified);
}

version (StdDdoc)
{
    /++
     $(BLUE This function is POSIX-Only.)

     Returns the time that the given file was last modified.
     Params:
        statbuf = stat_t retrieved from file.
     +/
    SysTime timeLastModified()(auto ref stat_t statbuf) pure nothrow {assert(false);}
    /++
     $(BLUE This function is POSIX-Only.)

     Returns the time that the given file was last accessed.
     Params:
        statbuf = stat_t retrieved from file.
     +/
    SysTime timeLastAccessed()(auto ref stat_t statbuf) pure nothrow {assert(false);}
    /++
     $(BLUE This function is POSIX-Only.)

     Returns the time that the given file was last changed.
     Params:
        statbuf = stat_t retrieved from file.
     +/
    SysTime timeStatusChanged()(auto ref stat_t statbuf) pure nothrow {assert(false);}
}
else version (Posix)
{
    SysTime timeLastModified()(auto ref stat_t statbuf) pure nothrow
    {
        return statTimeToStdTime!'m'(statbuf);
    }
    SysTime timeLastAccessed()(auto ref stat_t statbuf) pure nothrow
    {
        return statTimeToStdTime!'a'(statbuf);
    }
    SysTime timeStatusChanged()(auto ref stat_t statbuf) pure nothrow
    {
        return statTimeToStdTime!'c'(statbuf);
    }

    @safe unittest
    {
        stat_t statbuf;
        // check that both lvalues and rvalues work
        timeLastAccessed(statbuf);
        cast(void) timeLastAccessed(stat_t.init);
    }
}

@safe unittest
{
    //std.process.executeShell("echo a > deleteme");
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
@safe unittest
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
        () @trusted { Thread.sleep(20.msecs); }();
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
if (isSomeFiniteCharInputRange!R && !isConvertibleToString!R)
{
    return existsImpl(name.tempCString!FSChar());
}

/// ditto
bool exists(R)(auto ref R name)
if (isConvertibleToString!R)
{
    return exists!(StringTypeOf!R)(name);
}

///
@safe unittest
{
    auto f = deleteme ~ "does.not.exist";
    assert(!f.exists);

    f.write("hello");
    assert(f.exists);

    f.remove;
    assert(!f.exists);
}

private bool existsImpl(scope const(FSChar)* namez) @trusted nothrow @nogc
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

///
@safe unittest
{
    assert(".".exists);
    assert(!"this file does not exist".exists);
    deleteme.write("a\n");
    scope(exit) deleteme.remove;
    assert(deleteme.exists);
}

// https://issues.dlang.org/show_bug.cgi?id=16573
@safe unittest
{
    enum S : string { foo = "foo" }
    assert(__traits(compiles, S.foo.exists));
}

/++
 Returns the attributes of the given file.

 Note that the file attributes on Windows and POSIX systems are
 completely different. On Windows, they're what is returned by
 $(HTTP msdn.microsoft.com/en-us/library/aa364944(v=vs.85).aspx,
 GetFileAttributes), whereas on POSIX systems, they're the
 `st_mode` value which is part of the $(D stat struct) gotten by
 calling the $(HTTP en.wikipedia.org/wiki/Stat_%28Unix%29, `stat`)
 function.

 On POSIX systems, if the given file is a symbolic link, then
 attributes are the attributes of the file pointed to by the symbolic
 link.

 Params:
    name = The file to get the attributes of.
 Returns:
    The attributes of the file as a `uint`.
 Throws: $(LREF FileException) on error.
  +/
uint getAttributes(R)(R name)
if (isSomeFiniteCharInputRange!R && !isConvertibleToString!R)
{
    version (Windows)
    {
        auto namez = name.tempCString!FSChar();
        static auto trustedGetFileAttributesW(scope const(FSChar)* namez) @trusted
        {
            return GetFileAttributesW(namez);
        }
        immutable result = trustedGetFileAttributesW(namez);

        static if (isNarrowString!R && is(immutable ElementEncodingType!R == immutable char))
            alias names = name;
        else
            string names = null;
        cenforce(result != INVALID_FILE_ATTRIBUTES, names, namez);

        return result;
    }
    else version (Posix)
    {
        auto namez = name.tempCString!FSChar();
        stat_t statbuf = void;

        static if (isNarrowString!R && is(immutable ElementEncodingType!R == immutable char))
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

/// getAttributes with a file
@safe unittest
{
    import std.exception : assertThrown;

    auto f = deleteme ~ "file";
    scope(exit) f.remove;

    assert(!f.exists);
    assertThrown!FileException(f.getAttributes);

    f.write(".");
    auto attributes = f.getAttributes;
    assert(!attributes.attrIsDir);
    assert(attributes.attrIsFile);
}

/// getAttributes with a directory
@safe unittest
{
    import std.exception : assertThrown;

    auto dir = deleteme ~ "dir";
    scope(exit) dir.rmdir;

    assert(!dir.exists);
    assertThrown!FileException(dir.getAttributes);

    dir.mkdir;
    auto attributes = dir.getAttributes;
    assert(attributes.attrIsDir);
    assert(!attributes.attrIsFile);
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
        $(LREF FileException) on error.
 +/
uint getLinkAttributes(R)(R name)
if (isSomeFiniteCharInputRange!R && !isConvertibleToString!R)
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
        static if (isNarrowString!R && is(immutable ElementEncodingType!R == immutable char))
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

///
@safe unittest
{
    import std.exception : assertThrown;

    auto source = deleteme ~ "source";
    auto target = deleteme ~ "target";

    assert(!source.exists);
    assertThrown!FileException(source.getLinkAttributes);

    // symlinking isn't available on Windows
    version (Posix)
    {
        scope(exit) source.remove, target.remove;

        target.write("target");
        target.symlink(source);
        assert(source.readText == "target");
        assert(source.isSymlink);
        assert(source.getLinkAttributes.attrIsSymlink);
    }
}

/// if the file is no symlink, getLinkAttributes behaves like getAttributes
@safe unittest
{
    import std.exception : assertThrown;

    auto f = deleteme ~ "file";
    scope(exit) f.remove;

    assert(!f.exists);
    assertThrown!FileException(f.getLinkAttributes);

    f.write(".");
    auto attributes = f.getLinkAttributes;
    assert(!attributes.attrIsDir);
    assert(attributes.attrIsFile);
}

/// if the file is no symlink, getLinkAttributes behaves like getAttributes
@safe unittest
{
    import std.exception : assertThrown;

    auto dir = deleteme ~ "dir";
    scope(exit) dir.rmdir;

    assert(!dir.exists);
    assertThrown!FileException(dir.getLinkAttributes);

    dir.mkdir;
    auto attributes = dir.getLinkAttributes;
    assert(attributes.attrIsDir);
    assert(!attributes.attrIsFile);
}

@safe unittest
{
    static assert(__traits(compiles, getLinkAttributes(TestAliasedString(null))));
}

/++
    Set the _attributes of the given file.

    For example, a programmatic equivalent of Unix's `chmod +x name`
    to make a file executable is
    `name.setAttributes(name.getAttributes | octal!700)`.

    Params:
        name = the file _name
        attributes = the _attributes to set the file to

    Throws:
        $(LREF FileException) if the given file does not exist.
 +/
void setAttributes(R)(R name, uint attributes)
if (isSomeFiniteCharInputRange!R && !isConvertibleToString!R)
{
    version (Windows)
    {
        auto namez = name.tempCString!FSChar();
        static auto trustedSetFileAttributesW(scope const(FSChar)* namez, uint dwFileAttributes) @trusted
        {
            return SetFileAttributesW(namez, dwFileAttributes);
        }
        static if (isNarrowString!R && is(immutable ElementEncodingType!R == immutable char))
            alias names = name;
        else
            string names = null;
        cenforce(trustedSetFileAttributesW(namez, attributes), names, namez);
    }
    else version (Posix)
    {
        auto namez = name.tempCString!FSChar();
        static auto trustedChmod(scope const(FSChar)* namez, mode_t mode) @trusted
        {
            return chmod(namez, mode);
        }
        assert(attributes <= mode_t.max);
        static if (isNarrowString!R && is(immutable ElementEncodingType!R == immutable char))
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

/// setAttributes with a file
@safe unittest
{
    import std.exception : assertThrown;
    import std.conv : octal;

    auto f = deleteme ~ "file";
    version (Posix)
    {
        scope(exit) f.remove;

        assert(!f.exists);
        assertThrown!FileException(f.setAttributes(octal!777));

        f.write(".");
        auto attributes = f.getAttributes;
        assert(!attributes.attrIsDir);
        assert(attributes.attrIsFile);

        f.setAttributes(octal!777);
        attributes = f.getAttributes;

        assert((attributes & 1023) == octal!777);
    }
}

/// setAttributes with a directory
@safe unittest
{
    import std.exception : assertThrown;
    import std.conv : octal;

    auto dir = deleteme ~ "dir";
    version (Posix)
    {
        scope(exit) dir.rmdir;

        assert(!dir.exists);
        assertThrown!FileException(dir.setAttributes(octal!777));

        dir.mkdir;
        auto attributes = dir.getAttributes;
        assert(attributes.attrIsDir);
        assert(!attributes.attrIsFile);

        dir.setAttributes(octal!777);
        attributes = dir.getAttributes;

        assert((attributes & 1023) == octal!777);
    }
}

/++
    Returns whether the given file is a directory.

    Params:
        name = The path to the file.

    Returns:
        true if name specifies a directory

    Throws:
        $(LREF FileException) if the given file does not exist.
  +/
@property bool isDir(R)(R name)
if (isSomeFiniteCharInputRange!R && !isConvertibleToString!R)
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

///

@safe unittest
{
    import std.exception : assertThrown;

    auto dir = deleteme ~ "dir";
    auto f = deleteme ~ "f";
    scope(exit) dir.rmdir, f.remove;

    assert(!dir.exists);
    assertThrown!FileException(dir.isDir);

    dir.mkdir;
    assert(dir.isDir);

    f.write(".");
    assert(!f.isDir);
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

@safe unittest
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

///
@safe unittest
{
    import std.exception : assertThrown;

    auto dir = deleteme ~ "dir";
    auto f = deleteme ~ "f";
    scope(exit) dir.rmdir, f.remove;

    assert(!dir.exists);
    assertThrown!FileException(dir.getAttributes.attrIsDir);

    dir.mkdir;
    assert(dir.isDir);
    assert(dir.getAttributes.attrIsDir);

    f.write(".");
    assert(!f.isDir);
    assert(!f.getAttributes.attrIsDir);
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
    either `isFile` or `isDir` will return true for any given file.

    On POSIX systems, if `isFile` is `true`, that indicates that the file
    is a regular file (e.g. not a block not device). So, on POSIX systems, it's
    possible for both `isFile` and `isDir` to be `false` for a
    particular file (in which case, it's a special file). You can use
    `getAttributes` to get the attributes to figure out what type of special
    it is, or you can use `DirEntry` to get at its `statBuf`, which is the
    result from `stat`. In either case, see the man page for `stat` for
    more information.

    Params:
        name = The path to the file.

    Returns:
        true if name specifies a file

    Throws:
        $(LREF FileException) if the given file does not exist.
+/
@property bool isFile(R)(R name)
if (isSomeFiniteCharInputRange!R && !isConvertibleToString!R)
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

///
@safe unittest
{
    import std.exception : assertThrown;

    auto dir = deleteme ~ "dir";
    auto f = deleteme ~ "f";
    scope(exit) dir.rmdir, f.remove;

    dir.mkdir;
    assert(!dir.isFile);

    assert(!f.exists);
    assertThrown!FileException(f.isFile);

    f.write(".");
    assert(f.isFile);
}

// https://issues.dlang.org/show_bug.cgi?id=15658
@safe unittest
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
    `attrIsFile` or `attrIsDir` will return `true` for the
    _attributes of any given file.

    On POSIX systems, if `attrIsFile` is `true`, that indicates that the
    file is a regular file (e.g. not a block not device). So, on POSIX systems,
    it's possible for both `attrIsFile` and `attrIsDir` to be `false`
    for a particular file (in which case, it's a special file). If a file is a
    special file, you can use the _attributes to check what type of special file
    it is (see the man page for `stat` for more information).

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

///
@safe unittest
{
    import std.exception : assertThrown;

    auto dir = deleteme ~ "dir";
    auto f = deleteme ~ "f";
    scope(exit) dir.rmdir, f.remove;

    dir.mkdir;
    assert(!dir.isFile);
    assert(!dir.getAttributes.attrIsFile);

    assert(!f.exists);
    assertThrown!FileException(f.getAttributes.attrIsFile);

    f.write(".");
    assert(f.isFile);
    assert(f.getAttributes.attrIsFile);
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

    On Windows, returns `true` when the file is either a symbolic link or a
    junction point.

    Params:
        name = The path to the file.

    Returns:
        true if name is a symbolic link

    Throws:
        $(LREF FileException) if the given file does not exist.
  +/
@property bool isSymlink(R)(R name)
if (isSomeFiniteCharInputRange!R && !isConvertibleToString!R)
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

///
@safe unittest
{
    import std.exception : assertThrown;

    auto source = deleteme ~ "source";
    auto target = deleteme ~ "target";

    assert(!source.exists);
    assertThrown!FileException(source.isSymlink);

    // symlinking isn't available on Windows
    version (Posix)
    {
        scope(exit) source.remove, target.remove;

        target.write("target");
        target.symlink(source);
        assert(source.readText == "target");
        assert(source.isSymlink);
        assert(source.getLinkAttributes.attrIsSymlink);
    }
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

    On Windows, return `true` when the file is either a symbolic link or a
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

///
@safe unittest
{
    import std.exception : assertThrown;

    auto source = deleteme ~ "source";
    auto target = deleteme ~ "target";

    assert(!source.exists);
    assertThrown!FileException(source.getLinkAttributes.attrIsSymlink);

    // symlinking isn't available on Windows
    version (Posix)
    {
        scope(exit) source.remove, target.remove;

        target.write("target");
        target.symlink(source);
        assert(source.readText == "target");
        assert(source.isSymlink);
        assert(source.getLinkAttributes.attrIsSymlink);
    }
}

/**
Change directory to `pathname`. Equivalent to `cd` on
Windows and POSIX.

Params:
    pathname = the directory to step into

Throws: $(LREF FileException) on error.
 */
void chdir(R)(R pathname)
if (isSomeFiniteCharInputRange!R && !isConvertibleToString!R)
{
    // Place outside of @trusted block
    auto pathz = pathname.tempCString!FSChar();

    version (Windows)
    {
        static auto trustedChdir(scope const(FSChar)* pathz) @trusted
        {
            return SetCurrentDirectoryW(pathz);
        }
    }
    else version (Posix)
    {
        static auto trustedChdir(scope const(FSChar)* pathz) @trusted
        {
            return core.sys.posix.unistd.chdir(pathz) == 0;
        }
    }
    static if (isNarrowString!R && is(immutable ElementEncodingType!R == immutable char))
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

///
@system unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.sorting : sort;
    import std.array : array;
    import std.path : buildPath;

    auto cwd = getcwd;
    auto dir = deleteme ~ "dir";
    dir.mkdir;
    scope(exit) cwd.chdir, dir.rmdirRecurse;

    dir.buildPath("a").write(".");
    dir.chdir; // step into dir
    "b".write(".");
    assert(dirEntries(".", SpanMode.shallow).array.sort.equal(
        [".".buildPath("a"), ".".buildPath("b")]
    ));
}

@safe unittest
{
    static assert(__traits(compiles, chdir(TestAliasedString(null))));
}

/**
Make a new directory `pathname`.

Params:
    pathname = the path of the directory to make

Throws:
    $(LREF FileException) on POSIX or $(LREF WindowsException) on Windows
    if an error occured.
 */
void mkdir(R)(R pathname)
if (isSomeFiniteCharInputRange!R && !isConvertibleToString!R)
{
    // Place outside of @trusted block
    const pathz = pathname.tempCString!FSChar();

    version (Windows)
    {
        static auto trustedCreateDirectoryW(scope const(FSChar)* pathz) @trusted
        {
            return CreateDirectoryW(pathz, null);
        }
        static if (isNarrowString!R && is(immutable ElementEncodingType!R == immutable char))
            alias pathStr = pathname;
        else
            string pathStr = null;
        wenforce(trustedCreateDirectoryW(pathz), pathStr, pathz);
    }
    else version (Posix)
    {
        import std.conv : octal;

        static auto trustedMkdir(scope const(FSChar)* pathz, mode_t mode) @trusted
        {
            return core.sys.posix.sys.stat.mkdir(pathz, mode);
        }
        static if (isNarrowString!R && is(immutable ElementEncodingType!R == immutable char))
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

///
@safe unittest
{
    import std.file : mkdir;

    auto dir = deleteme ~ "dir";
    scope(exit) dir.rmdir;

    dir.mkdir;
    assert(dir.exists);
}

///
@safe unittest
{
    import std.exception : assertThrown;
    assertThrown("a/b/c/d/e".mkdir);
}

// Same as mkdir but ignores "already exists" errors.
// Returns: "true" if the directory was created,
//   "false" if it already existed.
private bool ensureDirExists()(scope const(char)[] pathname)
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

/**
Make directory and all parent directories as needed.

Does nothing if the directory specified by
`pathname` already exists.

Params:
    pathname = the full path of the directory to create

Throws: $(LREF FileException) on error.
 */
void mkdirRecurse(scope const(char)[] pathname) @safe
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

///
@safe unittest
{
    import std.path : buildPath;

    auto dir = deleteme ~ "dir";
    scope(exit) dir.rmdirRecurse;

    dir.mkdir;
    assert(dir.exists);
    dir.mkdirRecurse; // does nothing

    // creates all parent directories as needed
    auto nested = dir.buildPath("a", "b", "c");
    nested.mkdirRecurse;
    assert(nested.exists);
}

///
@safe unittest
{
    import std.exception : assertThrown;

    scope(exit) deleteme.remove;
    deleteme.write("a");

    // cannot make directory as it's already a file
    assertThrown!FileException(deleteme.mkdirRecurse);
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

    // https://issues.dlang.org/show_bug.cgi?id=3570
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
Remove directory `pathname`.

Params:
    pathname = Range or string specifying the directory name

Throws: $(LREF FileException) on error.
 */
void rmdir(R)(R pathname)
if (isSomeFiniteCharInputRange!R && !isConvertibleToString!R)
{
    // Place outside of @trusted block
    auto pathz = pathname.tempCString!FSChar();

    version (Windows)
    {
        static auto trustedRmdir(scope const(FSChar)* pathz) @trusted
        {
            return RemoveDirectoryW(pathz);
        }
    }
    else version (Posix)
    {
        static auto trustedRmdir(scope const(FSChar)* pathz) @trusted
        {
            return core.sys.posix.unistd.rmdir(pathz) == 0;
        }
    }
    static if (isNarrowString!R && is(immutable ElementEncodingType!R == immutable char))
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

///
@safe unittest
{
    auto dir = deleteme ~ "dir";

    dir.mkdir;
    assert(dir.exists);
    dir.rmdir;
    assert(!dir.exists);
}

/++
    $(BLUE This function is POSIX-Only.)

    Creates a symbolic _link (_symlink).

    Params:
        original = The file that is being linked. This is the target path that's
            stored in the _symlink. A relative path is relative to the created
            _symlink.
        link = The _symlink to create. A relative path is relative to the
            current working directory.

    Throws:
        $(LREF FileException) on error (which includes if the _symlink already
        exists).
  +/
version (StdDdoc) void symlink(RO, RL)(RO original, RL link)
if ((isSomeFiniteCharInputRange!RO || isConvertibleToString!RO) &&
    (isSomeFiniteCharInputRange!RL || isConvertibleToString!RL));
else version (Posix) void symlink(RO, RL)(RO original, RL link)
if ((isSomeFiniteCharInputRange!RO || isConvertibleToString!RO) &&
    (isSomeFiniteCharInputRange!RL || isConvertibleToString!RL))
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
    $(BLUE This function is POSIX-Only.)

    Returns the path to the file pointed to by a symlink. Note that the
    path could be either relative or absolute depending on the symlink.
    If the path is relative, it's relative to the symlink, not the current
    working directory.

    Throws:
        $(LREF FileException) on error.
  +/
version (StdDdoc) string readLink(R)(R link)
if (isSomeFiniteCharInputRange!R || isConvertibleToString!R);
else version (Posix) string readLink(R)(R link)
if (isSomeFiniteCharInputRange!R || isConvertibleToString!R)
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
    InputRange!(ElementType!string) linkr = inputRangeObject(link);
    alias R = typeof(linkr);
    static assert(isInputRange!R);
    static assert(!isForwardRange!R);
    assert(readLink(linkr) == "f");
}


/****************************************************
 * Get the current working directory.
 * Throws: $(LREF FileException) on error.
 */
version (Windows) string getcwd() @trusted
{
    import std.conv : to;
    import std.checkedint : checked;
    /* GetCurrentDirectory's return value:
        1. function succeeds: the number of characters that are written to
    the buffer, not including the terminating null character.
        2. function fails: zero
        3. the buffer (lpBuffer) is not large enough: the required size of
    the buffer, in characters, including the null-terminating character.
    */
    version (StdUnittest)
        enum BUF_SIZE = 10;     // trigger reallocation code
    else
        enum BUF_SIZE = 4096;   // enough for most common case
    wchar[BUF_SIZE] buffW = void;
    immutable n = cenforce(GetCurrentDirectoryW(to!DWORD(buffW.length), buffW.ptr),
            "getcwd");
    // we can do it because toUTFX always produces a fresh string
    if (n < buffW.length)
    {
        return buffW[0 .. n].to!string;
    }
    else //staticBuff isn't enough
    {
        auto cn = checked(n);
        auto ptr = cast(wchar*) malloc((cn * wchar.sizeof).get);
        scope(exit) free(ptr);
        immutable n2 = GetCurrentDirectoryW(cn.get, ptr);
        cenforce(n2 && n2 < cn, "getcwd");
        return ptr[0 .. n2].to!string;
    }
}
else version (Solaris) string getcwd() @trusted
{
    /* BUF_SIZE >= PATH_MAX */
    enum BUF_SIZE = 4096;
    /* The user should be able to specify any size buffer > 0 */
    auto p = cenforce(core.sys.posix.unistd.getcwd(null, BUF_SIZE),
            "cannot get cwd");
    scope(exit) core.stdc.stdlib.free(p);
    return p[0 .. core.stdc.string.strlen(p)].idup;
}
else version (Posix) string getcwd() @trusted
{
    auto p = cenforce(core.sys.posix.unistd.getcwd(null, 0),
            "cannot get cwd");
    scope(exit) core.stdc.stdlib.free(p);
    return p[0 .. core.stdc.string.strlen(p)].idup;
}

///
@safe unittest
{
    auto s = getcwd();
    assert(s.length);
}

/**
 * Returns the full path of the current executable.
 *
 * Returns:
 *     The path of the executable as a `string`.
 *
 * Throws:
 * $(REF1 Exception, object)
 */
@trusted string thisExePath()
{
    version (Darwin)
    {
        import core.sys.darwin.mach.dyld : _NSGetExecutablePath;
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
            wenforce(len);
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
    else version (Hurd)
    {
        return readLink("/proc/self/exe");
    }
    else
        static assert(0, "thisExePath is not supported on this platform");
}

///
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
        Info on a file, similar to what you'd get from stat on a POSIX system.
      +/
    struct DirEntry
    {
        @safe:
        /++
            Constructs a `DirEntry` for the given file (or directory).

            Params:
                path = The file (or directory) to get a DirEntry for.

            Throws:
                $(LREF FileException) if the file does not exist.
        +/
        this(return scope string path);

        version (Windows)
        {
            private this(string path, in WIN32_FIND_DATAW *fd);
        }
        else version (Posix)
        {
            private this(string path, core.sys.posix.dirent.dirent* fd);
        }

        /++
            Returns the path to the file represented by this `DirEntry`.

Example:
--------------------
auto de1 = DirEntry("/etc/fonts/fonts.conf");
assert(de1.name == "/etc/fonts/fonts.conf");

auto de2 = DirEntry("/usr/share/include");
assert(de2.name == "/usr/share/include");
--------------------
          +/
        @property string name() const return scope;


        /++
            Returns whether the file represented by this `DirEntry` is a
            directory.

Example:
--------------------
auto de1 = DirEntry("/etc/fonts/fonts.conf");
assert(!de1.isDir);

auto de2 = DirEntry("/usr/share/include");
assert(de2.isDir);
--------------------
          +/
        @property bool isDir() scope;


        /++
            Returns whether the file represented by this `DirEntry` is a file.

            On Windows, if a file is not a directory, then it's a file. So,
            either `isFile` or `isDir` will return `true`.

            On POSIX systems, if `isFile` is `true`, that indicates that
            the file is a regular file (e.g. not a block not device). So, on
            POSIX systems, it's possible for both `isFile` and `isDir` to
            be `false` for a particular file (in which case, it's a special
            file). You can use `attributes` or `statBuf` to get more
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
        @property bool isFile() scope;

        /++
            Returns whether the file represented by this `DirEntry` is a
            symbolic link.

            On Windows, return `true` when the file is either a symbolic
            link or a junction point.
          +/
        @property bool isSymlink() scope;

        /++
            Returns the size of the file represented by this `DirEntry`
            in bytes.
          +/
        @property ulong size() scope;

        /++
            $(BLUE This function is Windows-Only.)

            Returns the creation time of the file represented by this
            `DirEntry`.
          +/
        @property SysTime timeCreated() const scope;

        /++
            Returns the time that the file represented by this `DirEntry` was
            last accessed.

            Note that many file systems do not update the access time for files
            (generally for performance reasons), so there's a good chance that
            `timeLastAccessed` will return the same value as
            `timeLastModified`.
          +/
        @property SysTime timeLastAccessed() scope;

        /++
            Returns the time that the file represented by this `DirEntry` was
            last modified.
          +/
        @property SysTime timeLastModified() scope;

        /++
            $(BLUE This function is POSIX-Only.)

            Returns the time that the file represented by this `DirEntry` was
            last changed (not only in contents, but also in permissions or ownership).
          +/
        @property SysTime timeStatusChanged() const scope;

        /++
            Returns the _attributes of the file represented by this `DirEntry`.

            Note that the file _attributes on Windows and POSIX systems are
            completely different. On, Windows, they're what is returned by
            `GetFileAttributes`
            $(HTTP msdn.microsoft.com/en-us/library/aa364944(v=vs.85).aspx, GetFileAttributes)
            Whereas, an POSIX systems, they're the `st_mode` value which is
            part of the `stat` struct gotten by calling `stat`.

            On POSIX systems, if the file represented by this `DirEntry` is a
            symbolic link, then _attributes are the _attributes of the file
            pointed to by the symbolic link.
          +/
        @property uint attributes() scope;

        /++
            On POSIX systems, if the file represented by this `DirEntry` is a
            symbolic link, then `linkAttributes` are the attributes of the
            symbolic link itself. Otherwise, `linkAttributes` is identical to
            `attributes`.

            On Windows, `linkAttributes` is identical to `attributes`. It
            exists on Windows so that you don't have to special-case code for
            Windows when dealing with symbolic links.
          +/
        @property uint linkAttributes() scope;

        version (Windows)
            alias stat_t = void*;

        /++
            $(BLUE This function is POSIX-Only.)

            The `stat` struct gotten from calling `stat`.
          +/
        @property stat_t statBuf() scope;
    }
}
else version (Windows)
{
    struct DirEntry
    {
    @safe:
    public:
        alias name this;

        this(return scope string path)
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

        private this(string path, WIN32_FIND_DATAW *fd) @trusted
        {
            import core.stdc.wchar_ : wcslen;
            import std.conv : to;
            import std.datetime.systime : FILETIMEToSysTime;
            import std.path : buildPath;

            fd.cFileName[$ - 1] = 0;

            size_t clength = wcslen(&fd.cFileName[0]);
            _name = buildPath(path, fd.cFileName[0 .. clength].to!string);
            _size = (cast(ulong) fd.nFileSizeHigh << 32) | fd.nFileSizeLow;
            _timeCreated = FILETIMEToSysTime(&fd.ftCreationTime);
            _timeLastAccessed = FILETIMEToSysTime(&fd.ftLastAccessTime);
            _timeLastModified = FILETIMEToSysTime(&fd.ftLastWriteTime);
            _attributes = fd.dwFileAttributes;
        }

        @property string name() const pure nothrow return scope
        {
            return _name;
        }

        @property bool isDir() const pure nothrow scope
        {
            return (attributes & FILE_ATTRIBUTE_DIRECTORY) != 0;
        }

        @property bool isFile() const pure nothrow scope
        {
            //Are there no options in Windows other than directory and file?
            //If there are, then this probably isn't the best way to determine
            //whether this DirEntry is a file or not.
            return !isDir;
        }

        @property bool isSymlink() const pure nothrow scope
        {
            return (attributes & FILE_ATTRIBUTE_REPARSE_POINT) != 0;
        }

        @property ulong size() const pure nothrow scope
        {
            return _size;
        }

        @property SysTime timeCreated() const pure nothrow return scope
        {
            return cast(SysTime)_timeCreated;
        }

        @property SysTime timeLastAccessed() const pure nothrow return scope
        {
            return cast(SysTime)_timeLastAccessed;
        }

        @property SysTime timeLastModified() const pure nothrow return scope
        {
            return cast(SysTime)_timeLastModified;
        }

        @property uint attributes() const pure nothrow scope
        {
            return _attributes;
        }

        @property uint linkAttributes() const pure nothrow scope
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
    @safe:
    public:
        alias name this;

        this(return scope string path)
        {
            if (!path.exists)
                throw new FileException(path, "File does not exist");

            _name = path;

            _didLStat = false;
            _didStat = false;
            _dTypeSet = false;
        }

        private this(string path, core.sys.posix.dirent.dirent* fd) @safe
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

        @property string name() const pure nothrow return scope
        {
            return _name;
        }

        @property bool isDir() scope
        {
            _ensureStatOrLStatDone();

            return (_statBuf.st_mode & S_IFMT) == S_IFDIR;
        }

        @property bool isFile() scope
        {
            _ensureStatOrLStatDone();

            return (_statBuf.st_mode & S_IFMT) == S_IFREG;
        }

        @property bool isSymlink() scope
        {
            _ensureLStatDone();

            return (_lstatMode & S_IFMT) == S_IFLNK;
        }

        @property ulong size() scope
        {
            _ensureStatDone();
            return _statBuf.st_size;
        }

        @property SysTime timeStatusChanged() scope
        {
            _ensureStatDone();

            return statTimeToStdTime!'c'(_statBuf);
        }

        @property SysTime timeLastAccessed() scope
        {
            _ensureStatDone();

            return statTimeToStdTime!'a'(_statBuf);
        }

        @property SysTime timeLastModified() scope
        {
            _ensureStatDone();

            return statTimeToStdTime!'m'(_statBuf);
        }

        @property uint attributes() scope
        {
            _ensureStatDone();

            return _statBuf.st_mode;
        }

        @property uint linkAttributes() scope
        {
            _ensureLStatDone();

            return _lstatMode;
        }

        @property stat_t statBuf() scope
        {
            _ensureStatDone();

            return _statBuf;
        }

    private:
        /++
            This is to support lazy evaluation, because doing stat's is
            expensive and not always needed.
         +/
        void _ensureStatDone() @trusted scope
        {
            import std.exception : enforce;

            if (_didStat)
                return;

            enforce(stat(_name.tempCString(), &_statBuf) == 0,
                    "Failed to stat file `" ~ _name ~ "'");

            _didStat = true;
        }

        /++
            This is to support lazy evaluation, because doing stat's is
            expensive and not always needed.

            Try both stat and lstat for isFile and isDir
            to detect broken symlinks.
         +/
        void _ensureStatOrLStatDone() @trusted scope
        {
            if (_didStat)
                return;

            if (stat(_name.tempCString(), &_statBuf) != 0)
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
        void _ensureLStatDone() @trusted scope
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

        stat_t _statBuf = void;   /// The result of stat().
        uint  _lstatMode;         /// The stat mode from lstat().
        ubyte _dType;             /// The type of the file.

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
                // https://issues.dlang.org/show_bug.cgi?id=8298
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
    /// Defaults to `Yes.preserveAttributes` on Windows, and the opposite on all other platforms.
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
Copy file `from` _to file `to`. File timestamps are preserved.
File attributes are preserved, if `preserve` equals `Yes.preserveAttributes`.
On Windows only `Yes.preserveAttributes` (the default on Windows) is supported.
If the target file exists, it is overwritten.

Params:
    from = string or range of characters representing the existing file name
    to = string or range of characters representing the target file name
    preserve = whether to _preserve the file attributes

Throws: $(LREF FileException) on error.
 */
void copy(RF, RT)(RF from, RT to, PreserveAttributes preserve = preserveAttributesDefault)
if (isSomeFiniteCharInputRange!RF && !isConvertibleToString!RF &&
    isSomeFiniteCharInputRange!RT && !isConvertibleToString!RT)
{
    // Place outside of @trusted block
    auto fromz = from.tempCString!FSChar();
    auto toz = to.tempCString!FSChar();

    static if (isNarrowString!RF && is(immutable ElementEncodingType!RF == immutable char))
        alias f = from;
    else
        enum string f = null;

    static if (isNarrowString!RT && is(immutable ElementEncodingType!RT == immutable char))
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

///
@safe unittest
{
    auto source = deleteme ~ "source";
    auto target = deleteme ~ "target";
    auto targetNonExistent = deleteme ~ "target2";

    scope(exit) source.remove, target.remove, targetNonExistent.remove;

    source.write("source");
    target.write("target");

    assert(target.readText == "target");

    source.copy(target);
    assert(target.readText == "source");

    source.copy(targetNonExistent);
    assert(targetNonExistent.readText == "source");
}

// https://issues.dlang.org/show_bug.cgi?id=15319
@safe unittest
{
    assert(__traits(compiles, copy("from.txt", "to.txt")));
}

private void copyImpl(scope const(char)[] f, scope const(char)[] t,
                      scope const(FSChar)* fromz, scope const(FSChar)* toz,
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
            import std.format : format;

            /++
            Reference resources: https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-copyfilew
            Because OS copyfilew handles both source and destination paths,
            the GetLastError does not accurately locate whether the error is for the source or destination.
            +/
            if (!f)
                f = to!(typeof(f))(fromz[0 .. wcslen(fromz)]);
            if (!t)
                t = to!(typeof(t))(toz[0 .. wcslen(toz)]);

            throw new FileException(format!"Copy from %s to %s"(f, t));
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

        setTimesImpl(t, toz, statbufr.statTimeToStdTime!'a', statbufr.statTimeToStdTime!'m');
    }
}

// https://issues.dlang.org/show_bug.cgi?id=14817
@safe unittest
{
    import std.algorithm, std.file;
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

// https://issues.dlang.org/show_bug.cgi?id=20370
    version (Windows)
        assert(t1.timeLastModified == t2.timeLastModified);
    else static if (is(typeof(&utimensat)) || is(typeof(&setattrlist)))
        assert(t1.timeLastModified == t2.timeLastModified);
    else
        assert(abs(t1.timeLastModified - t2.timeLastModified) < dur!"usecs"(1));
}

// https://issues.dlang.org/show_bug.cgi?id=11434
@safe version (Posix) @safe unittest
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

// https://issues.dlang.org/show_bug.cgi?id=15865
@safe unittest
{
    import std.exception : assertThrown;
    auto t = deleteme;
    write(t, "a");
    scope(exit) t.remove();
    assertThrown!FileException(copy(t, t));
    assert(readText(t) == "a");
}

// https://issues.dlang.org/show_bug.cgi?id=19834
version (Windows) @safe unittest
{
    import std.exception : collectException;
    import std.algorithm.searching : startsWith;
    import std.format : format;

    auto f = deleteme;
    auto t = f ~ "2";
    auto ex = collectException(copy(f, t));
    assert(ex.msg.startsWith(format!"Copy from %s to %s"(f, t)));
}

/++
    Remove directory and all of its content and subdirectories,
    recursively.

    Params:
        pathname = the path of the directory to completely remove
        de = The $(LREF DirEntry) to remove

    Throws:
        $(LREF FileException) if there is an error (including if the given
        file is not a directory).
 +/
void rmdirRecurse(scope const(char)[] pathname) @safe
{
    //No references to pathname will be kept after rmdirRecurse,
    //so the cast is safe
    rmdirRecurse(DirEntry((() @trusted => cast(string) pathname)()));
}

/// ditto
void rmdirRecurse(ref scope DirEntry de) @safe
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
        // dirEntries is @system without DIP1000 because it uses
        // a DirIterator with a SafeRefCounted variable, but here, no
        // references to the payload are escaped to the outside, so this should
        // be @trusted
        () @trusted {
            // all children, recursively depth-first
            foreach (DirEntry e; dirEntries(de.name, SpanMode.depth, false))
            {
                attrIsDir(e.linkAttributes) ? rmdir(e.name) : remove(e.name);
            }
        }();

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
void rmdirRecurse(scope DirEntry de) @safe
{
    rmdirRecurse(de);
}

///
@system unittest
{
    import std.path : buildPath;

    auto dir = deleteme.buildPath("a", "b", "c");

    dir.mkdirRecurse;
    assert(dir.exists);

    deleteme.rmdirRecurse;
    assert(!dir.exists);
    assert(!deleteme.exists);
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
    const linkTarget = deleteme ~ "/link";
    symlink(deleteme ~ "/a/b/c", linkTarget);
    rmdirRecurse(deleteme);
    enforce(!exists(deleteme));
}

@safe unittest
{
    ubyte[] buf = new ubyte[10];
    buf[] = 3;
    string unit_file = deleteme ~ "-unittest_write.tmp";
    if (exists(unit_file)) remove(unit_file);
    write(unit_file, cast(void[]) buf);
    void[] buf2 = read(unit_file);
    assert(cast(void[]) buf == buf2);

    string unit2_file = deleteme ~ "-unittest_write2.tmp";
    copy(unit_file, unit2_file);
    buf2 = read(unit2_file);
    assert(cast(void[]) buf == buf2);

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

    Note that `SpanMode.breadth` will not result in all directory
    members occurring before any subdirectory members, i.e. it is not
    _true
    $(HTTPS en.wikipedia.org/wiki/Tree_traversal#Breadth-first_search,
    _breadth-first traversal).
    */
    breadth,
}

///
@system unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map;
    import std.algorithm.sorting : sort;
    import std.array : array;
    import std.path : buildPath, relativePath;

    auto root = deleteme ~ "root";
    scope(exit) root.rmdirRecurse;
    root.mkdir;

    root.buildPath("animals").mkdir;
    root.buildPath("animals", "cat").mkdir;

    alias removeRoot = (return scope e) => e.relativePath(root);

    assert(root.dirEntries(SpanMode.depth).map!removeRoot.equal(
        [buildPath("animals", "cat"), "animals"]));

    assert(root.dirEntries(SpanMode.breadth).map!removeRoot.equal(
        ["animals", buildPath("animals", "cat")]));

    root.buildPath("plants").mkdir;

    assert(root.dirEntries(SpanMode.shallow).array.sort.map!removeRoot.equal(
        ["animals", "plants"]));
}

private struct DirIteratorImpl
{
  @safe:
    SpanMode _mode;
    // Whether we should follow symlinked directories while iterating.
    // It also indicates whether we should avoid functions which call
    // stat (since we should only need lstat in this case and it would
    // be more efficient to not call stat in addition to lstat).
    bool _followSymlink;
    DirEntry _cur;
    DirHandle[] _stack;
    DirEntry[] _stashed; //used in depth first mode

    //stack helpers
    void pushExtra(DirEntry de)
    {
        _stashed ~= de;
    }

    //ditto
    bool hasExtra()
    {
        return _stashed.length != 0;
    }

    //ditto
    DirEntry popExtra()
    {
        DirEntry de;
        de = _stashed[$-1];
        _stashed.popBack();
        return de;
    }

    version (Windows)
    {
        WIN32_FIND_DATAW _findinfo;
        struct DirHandle
        {
            string dirpath;
            HANDLE h;
        }

        bool stepIn(string directory) @safe
        {
            import std.path : chainPath;
            auto searchPattern = chainPath(directory, "*.*");

            static auto trustedFindFirstFileW(typeof(searchPattern) pattern, scope WIN32_FIND_DATAW* findinfo) @trusted
            {
                return FindFirstFileW(pattern.tempCString!FSChar(), findinfo);
            }

            HANDLE h = trustedFindFirstFileW(searchPattern, &_findinfo);
            cenforce(h != INVALID_HANDLE_VALUE, directory);
            _stack ~= DirHandle(directory, h);
            return toNext(false, &_findinfo);
        }

        bool next()
        {
            if (_stack.length == 0)
                return false;
            return toNext(true, &_findinfo);
        }

        bool toNext(bool fetch, scope WIN32_FIND_DATAW* findinfo) @trusted
        {
            import core.stdc.wchar_ : wcscmp;

            if (fetch)
            {
                if (FindNextFileW(_stack[$-1].h, findinfo) == FALSE)
                {
                    popDirStack();
                    return false;
                }
            }
            while (wcscmp(&findinfo.cFileName[0], ".") == 0 ||
                   wcscmp(&findinfo.cFileName[0], "..") == 0)
                if (FindNextFileW(_stack[$-1].h, findinfo) == FALSE)
                {
                    popDirStack();
                    return false;
                }
            _cur = DirEntry(_stack[$-1].dirpath, findinfo);
            return true;
        }

        void popDirStack() @trusted
        {
            assert(_stack.length != 0);
            FindClose(_stack[$-1].h);
            _stack.popBack();
        }

        void releaseDirStack() @trusted
        {
            foreach (d; _stack)
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
            static auto trustedOpendir(string dir) @trusted
            {
                return opendir(dir.tempCString());
            }

            auto h = directory.length ? trustedOpendir(directory) : trustedOpendir(".");
            cenforce(h, directory);
            _stack ~= (DirHandle(directory, h));
            return next();
        }

        bool next() @trusted
        {
            if (_stack.length == 0)
                return false;

            for (dirent* fdata; (fdata = readdir(_stack[$-1].h)) != null; )
            {
                // Skip "." and ".."
                if (core.stdc.string.strcmp(&fdata.d_name[0], ".") &&
                    core.stdc.string.strcmp(&fdata.d_name[0], ".."))
                {
                    _cur = DirEntry(_stack[$-1].dirpath, fdata);
                    return true;
                }
            }

            popDirStack();
            return false;
        }

        void popDirStack() @trusted
        {
            assert(_stack.length != 0);
            closedir(_stack[$-1].h);
            _stack.popBack();
        }

        void releaseDirStack() @trusted
        {
            foreach (d; _stack)
                closedir(d.h);
        }

        bool mayStepIn()
        {
            return _followSymlink ? _cur.isDir : attrIsDir(_cur.linkAttributes);
        }
    }

    this(R)(R pathname, SpanMode mode, bool followSymlink)
        if (isSomeFiniteCharInputRange!R)
    {
        _mode = mode;
        _followSymlink = followSymlink;

        static if (isNarrowString!R && is(immutable ElementEncodingType!R == immutable char))
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

    @property bool empty()
    {
        return _stashed.length == 0 && _stack.length == 0;
    }

    @property DirEntry front()
    {
        return _cur;
    }

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

// Must be a template, because the destructor is unsafe or safe depending on
// whether `-preview=dip1000` is in use. Otherwise, linking errors would
// result.
struct _DirIterator(bool useDIP1000)
{
    static assert(useDIP1000 == dip1000Enabled,
        "Please don't override useDIP1000 to disagree with compiler switch.");

private:
    SafeRefCounted!(DirIteratorImpl, RefCountedAutoInitialize.no) impl;

    this(string pathname, SpanMode mode, bool followSymlink) @trusted
    {
        impl = typeof(impl)(pathname, mode, followSymlink);
    }
public:
    @property bool empty() @trusted { return impl.empty; }
    @property DirEntry front() @trusted { return impl.front; }
    void popFront() @trusted { impl.popFront(); }
}

// This has the client code to automatically use and link to the correct
// template instance
alias DirIterator = _DirIterator!dip1000Enabled;

/++
    Returns an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
    of `DirEntry` that lazily iterates a given directory,
    also provides two ways of foreach iteration. The iteration variable can be of
    type `string` if only the name is needed, or `DirEntry`
    if additional details are needed. The span _mode dictates how the
    directory is traversed. The name of each iterated directory entry
    contains the absolute or relative _path (depending on _pathname).

    Note: The order of returned directory entries is as it is provided by the
    operating system / filesystem, and may not follow any particular sorting.

    Params:
        useDIP1000 = used to instantiate this function separately for code with
                     and without -preview=dip1000 compiler switch, because it
                     affects the ABI of this function. Set automatically -
                     don't touch.

        path = The directory to iterate over.
               If empty, the current directory will be iterated.

        pattern = Optional string with wildcards, such as $(RED
                  "*.d"). When present, it is used to filter the
                  results by their file name. The supported wildcard
                  strings are described under $(REF globMatch,
                  std,_path).

        mode = Whether the directory's sub-directories should be
               iterated in depth-first post-order ($(LREF depth)),
               depth-first pre-order ($(LREF breadth)), or not at all
               ($(LREF shallow)).

        followSymlink = Whether symbolic links which point to directories
                         should be treated as directories and their contents
                         iterated over.

    Returns:
        An $(REF_ALTTEXT input range, isInputRange,std,range,primitives) of
        $(LREF DirEntry).

    Throws:
        $(UL
        $(LI $(LREF FileException) if the $(B path) directory does not exist or read permission is denied.)
        $(LI $(LREF FileException) if $(B mode) is not `shallow` and a subdirectory cannot be read.)
        )

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
    std.process.executeShell(cmd);
}

// Iterate over all D source files in current directory and all its
// subdirectories
auto dFiles = dirEntries("","*.{d,di}",SpanMode.depth);
foreach (d; dFiles)
    writeln(d.name);
--------------------
To handle subdirectories with denied read permission, use `SpanMode.shallow`:
---
void scan(string path)
{
    foreach (DirEntry entry; dirEntries(path, SpanMode.shallow))
    {
        try
        {
            writeln(entry.name);
            if (entry.isDir)
                scan(entry.name);
        }
        catch (FileException fe) { continue; } // ignore
    }
}

scan("");
---
+/

// For some reason, doing the same alias-to-a-template trick as with DirIterator
// does not work here.
auto dirEntries(bool useDIP1000 = dip1000Enabled)
    (string path, SpanMode mode, bool followSymlink = true)
{
    return _DirIterator!useDIP1000(path, mode, followSymlink);
}

/// Duplicate functionality of D1's `std.file.listdir()`:
@safe unittest
{
    string[] listdir(string pathname)
    {
        import std.algorithm.iteration : map, filter;
        import std.array : array;
        import std.path : baseName;

        return dirEntries(pathname, SpanMode.shallow)
            .filter!(a => a.isFile)
            .map!((return a) => baseName(a.name))
            .array;
    }

    // Can be safe only with -preview=dip1000
    @safe void main(string[] args)
    {
        import std.stdio : writefln;

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
        string testdir = tempDir.buildPath("deleteme.dmd.unittest.std.file" ~ to!string(thisProcessID));
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
                   map!((return a) => absolutePath(a.name))(dirEntries(relpath, mode)),
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

    // https://issues.dlang.org/show_bug.cgi?id=7264
    foreach (string name; dirEntries(testdir, "*.d", SpanMode.breadth))
    {

    }
    foreach (entry; dirEntries(testdir, SpanMode.breadth))
    {
        static assert(is(typeof(entry) == DirEntry));
    }
    // https://issues.dlang.org/show_bug.cgi?id=7138
    auto a = array(dirEntries(testdir, SpanMode.shallow));

    // https://issues.dlang.org/show_bug.cgi?id=11392
    auto dFiles = dirEntries(testdir, SpanMode.shallow);
    foreach (d; dFiles){}

    // https://issues.dlang.org/show_bug.cgi?id=15146
    dirEntries("", SpanMode.shallow).walkLength();
}

/// Ditto
auto dirEntries(bool useDIP1000 = dip1000Enabled)
    (string path, string pattern, SpanMode mode,
    bool followSymlink = true)
{
    import std.algorithm.iteration : filter;
    import std.path : globMatch, baseName;

    bool f(DirEntry de) { return globMatch(baseName(de.name), pattern); }
    return filter!f(_DirIterator!useDIP1000(path, mode, followSymlink));
}

@safe unittest
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
    version (Posix) () @trusted
    {
        core.sys.posix.unistd.symlink((dpath ~ '\0').ptr, (sdpath ~ '\0').ptr);
        core.sys.posix.unistd.symlink((fpath ~ '\0').ptr, (sfpath ~ '\0').ptr);
    } ();

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

// Make sure that dirEntries does not butcher Unicode file names
// https://issues.dlang.org/show_bug.cgi?id=17962
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map;
    import std.algorithm.sorting : sort;
    import std.array : array;
    import std.path : buildPath;
    import std.uni : normalize;

    // The Unicode normalization is required to make the tests pass on Mac OS X.
    auto dir = deleteme ~ normalize("𐐷");
    scope(exit) if (dir.exists) rmdirRecurse(dir);
    mkdir(dir);
    auto files = ["Hello World",
                  "Ma Chérie.jpeg",
                  "さいごの果実.txt"].map!(a => buildPath(dir, normalize(a)))().array();
    sort(files);
    foreach (file; files)
        write(file, "nothing");

    auto result = dirEntries(dir, SpanMode.shallow).map!((return a) => a.name.normalize()).array();
    sort(result);

    assert(equal(files, result));
}

// https://issues.dlang.org/show_bug.cgi?id=21250
@system unittest
{
    import std.exception : assertThrown;
    assertThrown!Exception(dirEntries("237f5babd6de21f40915826699582e36", "*.bin", SpanMode.depth));
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
slurp(Types...)(string filename, scope const(char)[] format)
{
    import std.array : appender;
    import std.conv : text;
    import std.exception : enforce;
    import std.format.read : formattedRead;
    import std.stdio : File;
    import std.string : stripRight;

    auto app = appender!(typeof(return))();
    ElementType!(typeof(return)) toAdd;
    auto f = File(filename);
    scope(exit) f.close();
    foreach (line; f.byLine())
    {
        formattedRead(line, format, &toAdd);
        enforce(line.stripRight("\r").empty,
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

@system unittest
{
    import std.typecons : tuple;

    scope(exit)
    {
        assert(exists(deleteme));
        remove(deleteme);
    }
    write(deleteme, "10\r\n20");
    assert(slurp!(int)(deleteme, "%d") == [10, 20]);
}

/**
Returns the path to a directory for temporary files.
On POSIX platforms, it searches through the following list of directories
and returns the first one which is found to exist:
$(OL
    $(LI The directory given by the `TMPDIR` environment variable.)
    $(LI The directory given by the `TEMP` environment variable.)
    $(LI The directory given by the `TMP` environment variable.)
    $(LI `/tmp/`)
    $(LI `/var/tmp/`)
    $(LI `/usr/tmp/`)
)

On all platforms, `tempDir` returns the current working directory on failure.

The return value of the function is cached, so the procedures described
below will only be performed the first time the function is called.  All
subsequent runs will return the same string, regardless of whether
environment variables and directory structures have changed in the
meantime.

The POSIX `tempDir` algorithm is inspired by Python's
$(LINK2 http://docs.python.org/library/tempfile.html#tempfile.tempdir, `tempfile.tempdir`).

Returns:
    On Windows, this function returns the result of calling the Windows API function
    $(LINK2 http://msdn.microsoft.com/en-us/library/windows/desktop/aa364992.aspx, `GetTempPath`).

    On POSIX platforms, it searches through the following list of directories
    and returns the first one which is found to exist:
    $(OL
        $(LI The directory given by the `TMPDIR` environment variable.)
        $(LI The directory given by the `TEMP` environment variable.)
        $(LI The directory given by the `TMP` environment variable.)
        $(LI `/tmp`)
        $(LI `/var/tmp`)
        $(LI `/usr/tmp`)
    )

    On all platforms, `tempDir` returns `"."` on failure, representing
    the current working directory.
*/
string tempDir() @trusted
{
    // We must check that the end of a path is not a separator, before adding another
    // If we don't we end up with https://issues.dlang.org/show_bug.cgi?id=22738
    static string addSeparator(string input)
    {
        import std.path : dirSeparator;
        import std.algorithm.searching : endsWith;

        // It is very rare a directory path will reach this point with a directory separator at the end
        // However on OSX this can happen, so we must verify lest we break user code i.e. https://github.com/dlang/dub/pull/2208
        if (!input.endsWith(dirSeparator))
            return input ~ dirSeparator;
        else
            return input;
    }

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
                    if (!dir.empty && exists(dir)) return addSeparator(dir);
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

        if (cache is null)
        {
            cache = addSeparator(getcwd());
        }
    }
    return cache;
}

///
@safe unittest
{
    import std.ascii : letters;
    import std.conv : to;
    import std.path : buildPath;
    import std.random : randomSample;
    import std.utf : byCodeUnit;

    // random id with 20 letters
    auto id = letters.byCodeUnit.randomSample(20).to!string;
    auto myFile = tempDir.buildPath(id ~ "my_tmp_file");
    scope(exit) myFile.remove;

    myFile.write("hello");
    assert(myFile.readText == "hello");
}

@safe unittest
{
    import std.algorithm.searching : endsWith;
    import std.path : dirSeparator;
    assert(tempDir.endsWith(dirSeparator));

    // https://issues.dlang.org/show_bug.cgi?id=22738
    assert(!tempDir.endsWith(dirSeparator ~ dirSeparator));
}

/**
Returns the available disk space based on a given path.
On Windows, `path` must be a directory; on POSIX systems, it can be a file or directory.

Params:
    path = on Windows, it must be a directory; on POSIX it can be a file or directory
Returns:
    Available space in bytes

Throws:
    $(LREF FileException) in case of failure
*/
ulong getAvailableDiskSpace(scope const(char)[] path) @safe
{
    version (Windows)
    {
        import core.sys.windows.winbase : GetDiskFreeSpaceExW;
        import core.sys.windows.winnt : ULARGE_INTEGER;
        import std.internal.cstring : tempCStringW;

        ULARGE_INTEGER freeBytesAvailable;
        auto err = () @trusted {
            return GetDiskFreeSpaceExW(path.tempCStringW(), &freeBytesAvailable, null, null);
        } ();
        cenforce(err != 0, "Cannot get available disk space");

        return freeBytesAvailable.QuadPart;
    }
    else version (Posix)
    {
        import std.internal.cstring : tempCString;

        version (FreeBSD)
        {
            import core.sys.freebsd.sys.mount : statfs, statfs_t;

            statfs_t stats;
            auto err = () @trusted {
                return statfs(path.tempCString(), &stats);
            } ();
            cenforce(err == 0, "Cannot get available disk space");

            return stats.f_bavail * stats.f_bsize;
        }
        else
        {
            import core.sys.posix.sys.statvfs : statvfs, statvfs_t;

            statvfs_t stats;
            auto err = () @trusted {
                return statvfs(path.tempCString(), &stats);
            } ();
            cenforce(err == 0, "Cannot get available disk space");

            return stats.f_bavail * stats.f_frsize;
        }
    }
    else static assert(0, "Unsupported platform");
}

///
@safe unittest
{
    import std.exception : assertThrown;

    auto space = getAvailableDiskSpace(".");
    assert(space > 0);

    assertThrown!FileException(getAvailableDiskSpace("ThisFileDoesNotExist123123"));
}

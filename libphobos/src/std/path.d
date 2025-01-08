// Written in the D programming language.

/** This module is used to manipulate path strings.

    All functions, with the exception of $(LREF expandTilde) (and in some
    cases $(LREF absolutePath) and $(LREF relativePath)), are pure
    string manipulation functions; they don't depend on any state outside
    the program, nor do they perform any actual file system actions.
    This has the consequence that the module does not make any distinction
    between a path that points to a directory and a path that points to a
    file, and it does not know whether or not the object pointed to by the
    path actually exists in the file system.
    To differentiate between these cases, use $(REF isDir, std,file) and
    $(REF exists, std,file).

    Note that on Windows, both the backslash ($(D `\`)) and the slash ($(D `/`))
    are in principle valid directory separators.  This module treats them
    both on equal footing, but in cases where a $(I new) separator is
    added, a backslash will be used.  Furthermore, the $(LREF buildNormalizedPath)
    function will replace all slashes with backslashes on that platform.

    In general, the functions in this module assume that the input paths
    are well-formed.  (That is, they should not contain invalid characters,
    they should follow the file system's path format, etc.)  The result
    of calling a function on an ill-formed path is undefined.  When there
    is a chance that a path or a file name is invalid (for instance, when it
    has been input by the user), it may sometimes be desirable to use the
    $(LREF isValidFilename) and $(LREF isValidPath) functions to check
    this.

    Most functions do not perform any memory allocations, and if a string is
    returned, it is usually a slice of an input string.  If a function
    allocates, this is explicitly mentioned in the documentation.

$(SCRIPT inhibitQuickIndex = 1;)
$(DIVC quickindex,
$(BOOKTABLE,
$(TR $(TH Category) $(TH Functions))
$(TR $(TD Normalization) $(TD
          $(LREF absolutePath)
          $(LREF asAbsolutePath)
          $(LREF asNormalizedPath)
          $(LREF asRelativePath)
          $(LREF buildNormalizedPath)
          $(LREF buildPath)
          $(LREF chainPath)
          $(LREF expandTilde)
))
$(TR $(TD Partitioning) $(TD
          $(LREF baseName)
          $(LREF dirName)
          $(LREF dirSeparator)
          $(LREF driveName)
          $(LREF pathSeparator)
          $(LREF pathSplitter)
          $(LREF relativePath)
          $(LREF rootName)
          $(LREF stripDrive)
))
$(TR $(TD Validation) $(TD
          $(LREF isAbsolute)
          $(LREF isDirSeparator)
          $(LREF isRooted)
          $(LREF isValidFilename)
          $(LREF isValidPath)
))
$(TR $(TD Extension) $(TD
          $(LREF defaultExtension)
          $(LREF extension)
          $(LREF setExtension)
          $(LREF stripExtension)
          $(LREF withDefaultExtension)
          $(LREF withExtension)
))
$(TR $(TD Other) $(TD
          $(LREF filenameCharCmp)
          $(LREF filenameCmp)
          $(LREF globMatch)
          $(LREF CaseSensitive)
))
))

    Authors:
        Lars Tandle Kyllingstad,
        $(HTTP digitalmars.com, Walter Bright),
        Grzegorz Adam Hankiewicz,
        Thomas K$(UUML)hne,
        $(HTTP erdani.org, Andrei Alexandrescu)
    Copyright:
        Copyright (c) 2000-2014, the authors. All rights reserved.
    License:
        $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0)
    Source:
        $(PHOBOSSRC std/path.d)
*/
module std.path;


import std.file : getcwd;
static import std.meta;
import std.range;
import std.traits;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (StdUnittest)
{
private:
    struct TestAliasedString
    {
        string get() @safe @nogc pure nothrow return scope { return _s; }
        alias get this;
        @disable this(this);
        string _s;
    }

    bool testAliasedString(alias func, Args...)(scope string s, scope Args args)
    {
        return func(TestAliasedString(s), args) == func(s, args);
    }
}

/** String used to separate directory names in a path.  Under
    POSIX this is a slash, under Windows a backslash.
*/
version (Posix)          enum string dirSeparator = "/";
else version (Windows)   enum string dirSeparator = "\\";
else static assert(0, "unsupported platform");




/** Path separator string.  A colon under POSIX, a semicolon
    under Windows.
*/
version (Posix)          enum string pathSeparator = ":";
else version (Windows)   enum string pathSeparator = ";";
else static assert(0, "unsupported platform");




/** Determines whether the given character is a directory separator.

    On Windows, this includes both $(D `\`) and $(D `/`).
    On POSIX, it's just $(D `/`).
*/
bool isDirSeparator(dchar c)  @safe pure nothrow @nogc
{
    if (c == '/') return true;
    version (Windows) if (c == '\\') return true;
    return false;
}

///
@safe pure nothrow @nogc unittest
{
    version (Windows)
    {
        assert( '/'.isDirSeparator);
        assert( '\\'.isDirSeparator);
    }
    else
    {
        assert( '/'.isDirSeparator);
        assert(!'\\'.isDirSeparator);
    }
}


/*  Determines whether the given character is a drive separator.

    On Windows, this is true if c is the ':' character that separates
    the drive letter from the rest of the path.  On POSIX, this always
    returns false.
*/
private bool isDriveSeparator(dchar c)  @safe pure nothrow @nogc
{
    version (Windows) return c == ':';
    else return false;
}


/*  Combines the isDirSeparator and isDriveSeparator tests. */
version (Windows) private bool isSeparator(dchar c)  @safe pure nothrow @nogc
{
    return isDirSeparator(c) || isDriveSeparator(c);
}
version (Posix) private alias isSeparator = isDirSeparator;


/*  Helper function that determines the position of the last
    drive/directory separator in a string.  Returns -1 if none
    is found.
*/
private ptrdiff_t lastSeparator(R)(R path)
if (isRandomAccessRange!R && isSomeChar!(ElementType!R) ||
    isNarrowString!R)
{
    auto i = (cast(ptrdiff_t) path.length) - 1;
    while (i >= 0 && !isSeparator(path[i])) --i;
    return i;
}


version (Windows)
{
    private bool isUNC(R)(R path)
    if (isRandomAccessRange!R && isSomeChar!(ElementType!R) ||
        isNarrowString!R)
    {
        return path.length >= 3 && isDirSeparator(path[0]) && isDirSeparator(path[1])
            && !isDirSeparator(path[2]);
    }

    private ptrdiff_t uncRootLength(R)(R path)
    if (isRandomAccessRange!R && isSomeChar!(ElementType!R) ||
        isNarrowString!R)
        in { assert(isUNC(path)); }
        do
    {
        ptrdiff_t i = 3;
        while (i < path.length && !isDirSeparator(path[i])) ++i;
        if (i < path.length)
        {
            auto j = i;
            do { ++j; } while (j < path.length && isDirSeparator(path[j]));
            if (j < path.length)
            {
                do { ++j; } while (j < path.length && !isDirSeparator(path[j]));
                i = j;
            }
        }
        return i;
    }

    private bool hasDrive(R)(R path)
    if (isRandomAccessRange!R && isSomeChar!(ElementType!R) ||
        isNarrowString!R)
    {
        return path.length >= 2 && isDriveSeparator(path[1]);
    }

    private bool isDriveRoot(R)(R path)
    if (isRandomAccessRange!R && isSomeChar!(ElementType!R) ||
        isNarrowString!R)
    {
        return path.length >= 3 && isDriveSeparator(path[1])
            && isDirSeparator(path[2]);
    }
}


/*  Helper functions that strip leading/trailing slashes and backslashes
    from a path.
*/
private auto ltrimDirSeparators(R)(R path)
if (isSomeFiniteCharInputRange!R || isNarrowString!R)
{
    static if (isRandomAccessRange!R && hasSlicing!R || isNarrowString!R)
    {
        int i = 0;
        while (i < path.length && isDirSeparator(path[i]))
            ++i;
        return path[i .. path.length];
    }
    else
    {
        while (!path.empty && isDirSeparator(path.front))
            path.popFront();
        return path;
    }
}

@safe unittest
{
    import std.array;
    import std.utf : byDchar;

    assert(ltrimDirSeparators("//abc//").array == "abc//");
    assert(ltrimDirSeparators("//abc//"d).array == "abc//"d);
    assert(ltrimDirSeparators("//abc//".byDchar).array == "abc//"d);
}

private auto rtrimDirSeparators(R)(R path)
if (isBidirectionalRange!R && isSomeChar!(ElementType!R) ||
    isNarrowString!R)
{
    static if (isRandomAccessRange!R && hasSlicing!R && hasLength!R || isNarrowString!R)
    {
        auto i = (cast(ptrdiff_t) path.length) - 1;
        while (i >= 0 && isDirSeparator(path[i]))
            --i;
        return path[0 .. i+1];
    }
    else
    {
        while (!path.empty && isDirSeparator(path.back))
            path.popBack();
        return path;
    }
}

@safe unittest
{
    import std.array;

    assert(rtrimDirSeparators("//abc//").array == "//abc");
    assert(rtrimDirSeparators("//abc//"d).array == "//abc"d);

    assert(rtrimDirSeparators(MockBiRange!char("//abc//")).array == "//abc");
}

private auto trimDirSeparators(R)(R path)
if (isBidirectionalRange!R && isSomeChar!(ElementType!R) ||
    isNarrowString!R)
{
    return ltrimDirSeparators(rtrimDirSeparators(path));
}

@safe unittest
{
    import std.array;

    assert(trimDirSeparators("//abc//").array == "abc");
    assert(trimDirSeparators("//abc//"d).array == "abc"d);

    assert(trimDirSeparators(MockBiRange!char("//abc//")).array == "abc");
}

/** This `enum` is used as a template argument to functions which
    compare file names, and determines whether the comparison is
    case sensitive or not.
*/
enum CaseSensitive : bool
{
    /// File names are case insensitive
    no = false,

    /// File names are case sensitive
    yes = true,

    /** The default (or most common) setting for the current platform.
        That is, `no` on Windows and Mac OS X, and `yes` on all
        POSIX systems except Darwin (Linux, *BSD, etc.).
    */
    osDefault = osDefaultCaseSensitivity
}

///
@safe unittest
{
    assert(baseName!(CaseSensitive.no)("dir/file.EXT", ".ext") == "file");
    assert(baseName!(CaseSensitive.yes)("dir/file.EXT", ".ext") != "file");

    version (Posix)
        assert(relativePath!(CaseSensitive.no)("/FOO/bar", "/foo/baz") == "../bar");
    else
        assert(relativePath!(CaseSensitive.no)(`c:\FOO\bar`, `c:\foo\baz`) == `..\bar`);
}

version (Windows)     private enum osDefaultCaseSensitivity = false;
else version (Darwin) private enum osDefaultCaseSensitivity = false;
else version (Posix)  private enum osDefaultCaseSensitivity = true;
else static assert(0);

/**
    Params:
        cs = Whether or not suffix matching is case-sensitive.
        path = A path name. It can be a string, or any random-access range of
            characters.
        suffix = An optional suffix to be removed from the file name.
    Returns: The name of the file in the path name, without any leading
        directory and with an optional suffix chopped off.

    If `suffix` is specified, it will be compared to `path`
    using `filenameCmp!cs`,
    where `cs` is an optional template parameter determining whether
    the comparison is case sensitive or not.  See the
    $(LREF filenameCmp) documentation for details.

    Note:
    This function $(I only) strips away the specified suffix, which
    doesn't necessarily have to represent an extension.
    To remove the extension from a path, regardless of what the extension
    is, use $(LREF stripExtension).
    To obtain the filename without leading directories and without
    an extension, combine the functions like this:
    ---
    assert(baseName(stripExtension("dir/file.ext")) == "file");
    ---

    Standards:
    This function complies with
    $(LINK2 http://pubs.opengroup.org/onlinepubs/9699919799/utilities/basename.html,
    the POSIX requirements for the 'basename' shell utility)
    (with suitable adaptations for Windows paths).
*/
auto baseName(R)(return scope R path)
if (isRandomAccessRange!R && hasSlicing!R && isSomeChar!(ElementType!R) && !isSomeString!R)
{
    return _baseName(path);
}

/// ditto
auto baseName(C)(return scope C[] path)
if (isSomeChar!C)
{
    return _baseName(path);
}

/// ditto
inout(C)[] baseName(CaseSensitive cs = CaseSensitive.osDefault, C, C1)
    (return scope inout(C)[] path, in C1[] suffix)
    @safe pure //TODO: nothrow (because of filenameCmp())
if (isSomeChar!C && isSomeChar!C1)
{
    auto p = baseName(path);
    if (p.length > suffix.length
        && filenameCmp!cs(cast(const(C)[])p[$-suffix.length .. $], suffix) == 0)
    {
        return p[0 .. $-suffix.length];
    }
    else return p;
}

///
@safe unittest
{
    assert(baseName("dir/file.ext") == "file.ext");
    assert(baseName("dir/file.ext", ".ext") == "file");
    assert(baseName("dir/file.ext", ".xyz") == "file.ext");
    assert(baseName("dir/filename", "name") == "file");
    assert(baseName("dir/subdir/") == "subdir");

    version (Windows)
    {
        assert(baseName(`d:file.ext`) == "file.ext");
        assert(baseName(`d:\dir\file.ext`) == "file.ext");
    }
}

@safe unittest
{
    assert(baseName("").empty);
    assert(baseName("file.ext"w) == "file.ext");
    assert(baseName("file.ext"d, ".ext") == "file");
    assert(baseName("file", "file"w.dup) == "file");
    assert(baseName("dir/file.ext"d.dup) == "file.ext");
    assert(baseName("dir/file.ext", ".ext"d) == "file");
    assert(baseName("dir/file"w, "file"d) == "file");
    assert(baseName("dir///subdir////") == "subdir");
    assert(baseName("dir/subdir.ext/", ".ext") == "subdir");
    assert(baseName("dir/subdir/".dup, "subdir") == "subdir");
    assert(baseName("/"w.dup) == "/");
    assert(baseName("//"d.dup) == "/");
    assert(baseName("///") == "/");

    assert(baseName!(CaseSensitive.yes)("file.ext", ".EXT") == "file.ext");
    assert(baseName!(CaseSensitive.no)("file.ext", ".EXT") == "file");

    {
        auto r = MockRange!(immutable(char))(`dir/file.ext`);
        auto s = r.baseName();
        foreach (i, c; `file`)
            assert(s[i] == c);
    }

    version (Windows)
    {
        assert(baseName(`dir\file.ext`) == `file.ext`);
        assert(baseName(`dir\file.ext`, `.ext`) == `file`);
        assert(baseName(`dir\file`, `file`) == `file`);
        assert(baseName(`d:file.ext`) == `file.ext`);
        assert(baseName(`d:file.ext`, `.ext`) == `file`);
        assert(baseName(`d:file`, `file`) == `file`);
        assert(baseName(`dir\\subdir\\\`) == `subdir`);
        assert(baseName(`dir\subdir.ext\`, `.ext`) == `subdir`);
        assert(baseName(`dir\subdir\`, `subdir`) == `subdir`);
        assert(baseName(`\`) == `\`);
        assert(baseName(`\\`) == `\`);
        assert(baseName(`\\\`) == `\`);
        assert(baseName(`d:\`) == `\`);
        assert(baseName(`d:`).empty);
        assert(baseName(`\\server\share\file`) == `file`);
        assert(baseName(`\\server\share\`) == `\`);
        assert(baseName(`\\server\share`) == `\`);

        auto r = MockRange!(immutable(char))(`\\server\share`);
        auto s = r.baseName();
        foreach (i, c; `\`)
            assert(s[i] == c);
    }

    assert(baseName(stripExtension("dir/file.ext")) == "file");

    static assert(baseName("dir/file.ext") == "file.ext");
    static assert(baseName("dir/file.ext", ".ext") == "file");

    static struct DirEntry { string s; alias s this; }
    assert(baseName(DirEntry("dir/file.ext")) == "file.ext");
}

@safe unittest
{
    assert(testAliasedString!baseName("file"));

    enum S : string { a = "file/path/to/test" }
    assert(S.a.baseName == "test");

    char[S.a.length] sa = S.a[];
    assert(sa.baseName == "test");
}

private R _baseName(R)(return scope R path)
if (isRandomAccessRange!R && hasSlicing!R && isSomeChar!(ElementType!R) || isNarrowString!R)
{
    auto p1 = stripDrive(path);
    if (p1.empty)
    {
        version (Windows) if (isUNC(path))
            return path[0 .. 1];
        static if (isSomeString!R)
            return null;
        else
            return p1; // which is empty
    }

    auto p2 = rtrimDirSeparators(p1);
    if (p2.empty) return p1[0 .. 1];

    return p2[lastSeparator(p2)+1 .. p2.length];
}

/** Returns the parent directory of `path`. On Windows, this
    includes the drive letter if present. If `path` is a relative path and
    the parent directory is the current working directory, returns `"."`.

    Params:
        path = A path name.

    Returns:
        A slice of `path` or `"."`.

    Standards:
    This function complies with
    $(LINK2 http://pubs.opengroup.org/onlinepubs/9699919799/utilities/dirname.html,
    the POSIX requirements for the 'dirname' shell utility)
    (with suitable adaptations for Windows paths).
*/
auto dirName(R)(return scope R path)
if (isRandomAccessRange!R && hasSlicing!R && hasLength!R && isSomeChar!(ElementType!R) && !isSomeString!R)
{
    return _dirName(path);
}

/// ditto
auto dirName(C)(return scope C[] path)
if (isSomeChar!C)
{
    return _dirName(path);
}

///
@safe unittest
{
    assert(dirName("") == ".");
    assert(dirName("file"w) == ".");
    assert(dirName("dir/"d) == ".");
    assert(dirName("dir///") == ".");
    assert(dirName("dir/file"w.dup) == "dir");
    assert(dirName("dir///file"d.dup) == "dir");
    assert(dirName("dir/subdir/") == "dir");
    assert(dirName("/dir/file"w) == "/dir");
    assert(dirName("/file"d) == "/");
    assert(dirName("/") == "/");
    assert(dirName("///") == "/");

    version (Windows)
    {
        assert(dirName(`dir\`) == `.`);
        assert(dirName(`dir\\\`) == `.`);
        assert(dirName(`dir\file`) == `dir`);
        assert(dirName(`dir\\\file`) == `dir`);
        assert(dirName(`dir\subdir\`) == `dir`);
        assert(dirName(`\dir\file`) == `\dir`);
        assert(dirName(`\file`) == `\`);
        assert(dirName(`\`) == `\`);
        assert(dirName(`\\\`) == `\`);
        assert(dirName(`d:`) == `d:`);
        assert(dirName(`d:file`) == `d:`);
        assert(dirName(`d:\`) == `d:\`);
        assert(dirName(`d:\file`) == `d:\`);
        assert(dirName(`d:\dir\file`) == `d:\dir`);
        assert(dirName(`\\server\share\dir\file`) == `\\server\share\dir`);
        assert(dirName(`\\server\share\file`) == `\\server\share`);
        assert(dirName(`\\server\share\`) == `\\server\share`);
        assert(dirName(`\\server\share`) == `\\server\share`);
    }
}

@safe unittest
{
    assert(testAliasedString!dirName("file"));

    enum S : string { a = "file/path/to/test" }
    assert(S.a.dirName == "file/path/to");

    char[S.a.length] sa = S.a[];
    assert(sa.dirName == "file/path/to");
}

@safe unittest
{
    static assert(dirName("dir/file") == "dir");

    import std.array;
    import std.utf : byChar, byWchar, byDchar;

    assert(dirName("".byChar).array == ".");
    assert(dirName("file"w.byWchar).array == "."w);
    assert(dirName("dir/"d.byDchar).array == "."d);
    assert(dirName("dir///".byChar).array == ".");
    assert(dirName("dir/subdir/".byChar).array == "dir");
    assert(dirName("/dir/file"w.byWchar).array == "/dir"w);
    assert(dirName("/file"d.byDchar).array == "/"d);
    assert(dirName("/".byChar).array == "/");
    assert(dirName("///".byChar).array == "/");

    version (Windows)
    {
        assert(dirName(`dir\`.byChar).array == `.`);
        assert(dirName(`dir\\\`.byChar).array == `.`);
        assert(dirName(`dir\file`.byChar).array == `dir`);
        assert(dirName(`dir\\\file`.byChar).array == `dir`);
        assert(dirName(`dir\subdir\`.byChar).array == `dir`);
        assert(dirName(`\dir\file`.byChar).array == `\dir`);
        assert(dirName(`\file`.byChar).array == `\`);
        assert(dirName(`\`.byChar).array == `\`);
        assert(dirName(`\\\`.byChar).array == `\`);
        assert(dirName(`d:`.byChar).array == `d:`);
        assert(dirName(`d:file`.byChar).array == `d:`);
        assert(dirName(`d:\`.byChar).array == `d:\`);
        assert(dirName(`d:\file`.byChar).array == `d:\`);
        assert(dirName(`d:\dir\file`.byChar).array == `d:\dir`);
        assert(dirName(`\\server\share\dir\file`.byChar).array == `\\server\share\dir`);
        assert(dirName(`\\server\share\file`) == `\\server\share`);
        assert(dirName(`\\server\share\`.byChar).array == `\\server\share`);
        assert(dirName(`\\server\share`.byChar).array == `\\server\share`);
    }

    //static assert(dirName("dir/file".byChar).array == "dir");
}

private auto _dirName(R)(return scope R path)
{
    static auto result(bool dot, typeof(path[0 .. 1]) p)
    {
        static if (isSomeString!R)
            return dot ? "." : p;
        else
        {
            import std.range : choose, only;
            return choose(dot, only(cast(ElementEncodingType!R)'.'), p);
        }
    }

    if (path.empty)
        return result(true, path[0 .. 0]);

    auto p = rtrimDirSeparators(path);
    if (p.empty)
        return result(false, path[0 .. 1]);

    version (Windows)
    {
        if (isUNC(p) && uncRootLength(p) == p.length)
            return result(false, p);

        if (p.length == 2 && isDriveSeparator(p[1]) && path.length > 2)
            return result(false, path[0 .. 3]);
    }

    auto i = lastSeparator(p);
    if (i == -1)
        return result(true, p);
    if (i == 0)
        return result(false, p[0 .. 1]);

    version (Windows)
    {
        // If the directory part is either d: or d:\
        // do not chop off the last symbol.
        if (isDriveSeparator(p[i]) || isDriveSeparator(p[i-1]))
            return result(false, p[0 .. i+1]);
    }
    // Remove any remaining trailing (back)slashes.
    return result(false, rtrimDirSeparators(p[0 .. i]));
}

/** Returns the root directory of the specified path, or `null` if the
    path is not rooted.

    Params:
        path = A path name.

    Returns:
        A slice of `path`.
*/
auto rootName(R)(R path)
if (isRandomAccessRange!R && hasSlicing!R && hasLength!R && isSomeChar!(ElementType!R) && !isSomeString!R)
{
    return _rootName(path);
}

/// ditto
auto rootName(C)(C[] path)
if (isSomeChar!C)
{
    return _rootName(path);
}

///
@safe unittest
{
    assert(rootName("") is null);
    assert(rootName("foo") is null);
    assert(rootName("/") == "/");
    assert(rootName("/foo/bar") == "/");

    version (Windows)
    {
        assert(rootName("d:foo") is null);
        assert(rootName(`d:\foo`) == `d:\`);
        assert(rootName(`\\server\share\foo`) == `\\server\share`);
        assert(rootName(`\\server\share`) == `\\server\share`);
    }
}

@safe unittest
{
    assert(testAliasedString!rootName("/foo/bar"));

    enum S : string { a = "/foo/bar" }
    assert(S.a.rootName == "/");

    char[S.a.length] sa = S.a[];
    assert(sa.rootName == "/");
}

@safe unittest
{
    import std.array;
    import std.utf : byChar;

    assert(rootName("".byChar).array == "");
    assert(rootName("foo".byChar).array == "");
    assert(rootName("/".byChar).array == "/");
    assert(rootName("/foo/bar".byChar).array == "/");

    version (Windows)
    {
        assert(rootName("d:foo".byChar).array == "");
        assert(rootName(`d:\foo`.byChar).array == `d:\`);
        assert(rootName(`\\server\share\foo`.byChar).array == `\\server\share`);
        assert(rootName(`\\server\share`.byChar).array == `\\server\share`);
    }
}

private auto _rootName(R)(R path)
{
    if (path.empty)
        goto Lnull;

    version (Posix)
    {
        if (isDirSeparator(path[0])) return path[0 .. 1];
    }
    else version (Windows)
    {
        if (isDirSeparator(path[0]))
        {
            if (isUNC(path)) return path[0 .. uncRootLength(path)];
            else return path[0 .. 1];
        }
        else if (path.length >= 3 && isDriveSeparator(path[1]) &&
            isDirSeparator(path[2]))
        {
            return path[0 .. 3];
        }
    }
    else static assert(0, "unsupported platform");

    assert(!isRooted(path));
Lnull:
    static if (is(StringTypeOf!R))
        return null; // legacy code may rely on null return rather than slice
    else
        return path[0 .. 0];
}

/**
    Get the drive portion of a path.

    Params:
        path = string or range of characters

    Returns:
        A slice of `path` that is the drive, or an empty range if the drive
        is not specified.  In the case of UNC paths, the network share
        is returned.

        Always returns an empty range on POSIX.
*/
auto driveName(R)(R path)
if (isRandomAccessRange!R && hasSlicing!R && hasLength!R && isSomeChar!(ElementType!R) && !isSomeString!R)
{
    return _driveName(path);
}

/// ditto
auto driveName(C)(C[] path)
if (isSomeChar!C)
{
    return _driveName(path);
}

///
@safe unittest
{
    import std.range : empty;
    version (Posix)  assert(driveName("c:/foo").empty);
    version (Windows)
    {
        assert(driveName(`dir\file`).empty);
        assert(driveName(`d:file`) == "d:");
        assert(driveName(`d:\file`) == "d:");
        assert(driveName("d:") == "d:");
        assert(driveName(`\\server\share\file`) == `\\server\share`);
        assert(driveName(`\\server\share\`) == `\\server\share`);
        assert(driveName(`\\server\share`) == `\\server\share`);

        static assert(driveName(`d:\file`) == "d:");
    }
}

@safe unittest
{
    assert(testAliasedString!driveName("d:/file"));

    version (Posix)
        immutable result = "";
    else version (Windows)
        immutable result = "d:";

    enum S : string { a = "d:/file" }
    assert(S.a.driveName == result);

    char[S.a.length] sa = S.a[];
    assert(sa.driveName == result);
}

@safe unittest
{
    import std.array;
    import std.utf : byChar;

    version (Posix)  assert(driveName("c:/foo".byChar).empty);
    version (Windows)
    {
        assert(driveName(`dir\file`.byChar).empty);
        assert(driveName(`d:file`.byChar).array == "d:");
        assert(driveName(`d:\file`.byChar).array == "d:");
        assert(driveName("d:".byChar).array == "d:");
        assert(driveName(`\\server\share\file`.byChar).array == `\\server\share`);
        assert(driveName(`\\server\share\`.byChar).array == `\\server\share`);
        assert(driveName(`\\server\share`.byChar).array == `\\server\share`);

        static assert(driveName(`d:\file`).array == "d:");
    }
}

private auto _driveName(R)(R path)
{
    version (Windows)
    {
        if (hasDrive(path))
            return path[0 .. 2];
        else if (isUNC(path))
            return path[0 .. uncRootLength(path)];
    }
    static if (isSomeString!R)
        return cast(ElementEncodingType!R[]) null; // legacy code may rely on null return rather than slice
    else
        return path[0 .. 0];
}

/** Strips the drive from a Windows path.  On POSIX, the path is returned
    unaltered.

    Params:
        path = A pathname

    Returns: A slice of path without the drive component.
*/
auto stripDrive(R)(R path)
if (isRandomAccessRange!R && hasSlicing!R && isSomeChar!(ElementType!R) && !isSomeString!R)
{
    return _stripDrive(path);
}

/// ditto
auto stripDrive(C)(C[] path)
if (isSomeChar!C)
{
    return _stripDrive(path);
}

///
@safe unittest
{
    version (Windows)
    {
        assert(stripDrive(`d:\dir\file`) == `\dir\file`);
        assert(stripDrive(`\\server\share\dir\file`) == `\dir\file`);
    }
}

@safe unittest
{
    assert(testAliasedString!stripDrive("d:/dir/file"));

    version (Posix)
        immutable result = "d:/dir/file";
    else version (Windows)
        immutable result = "/dir/file";

    enum S : string { a = "d:/dir/file" }
    assert(S.a.stripDrive == result);

    char[S.a.length] sa = S.a[];
    assert(sa.stripDrive == result);
}

@safe unittest
{
    version (Windows)
    {
        assert(stripDrive(`d:\dir\file`) == `\dir\file`);
        assert(stripDrive(`\\server\share\dir\file`) == `\dir\file`);
        static assert(stripDrive(`d:\dir\file`) == `\dir\file`);

        auto r = MockRange!(immutable(char))(`d:\dir\file`);
        auto s = r.stripDrive();
        foreach (i, c; `\dir\file`)
            assert(s[i] == c);
    }
    version (Posix)
    {
        assert(stripDrive(`d:\dir\file`) == `d:\dir\file`);

        auto r = MockRange!(immutable(char))(`d:\dir\file`);
        auto s = r.stripDrive();
        foreach (i, c; `d:\dir\file`)
            assert(s[i] == c);
    }
}

private auto _stripDrive(R)(R path)
{
    version (Windows)
    {
        if (hasDrive!(BaseOf!R)(path))      return path[2 .. path.length];
        else if (isUNC!(BaseOf!R)(path))    return path[uncRootLength!(BaseOf!R)(path) .. path.length];
    }
    return path;
}


/*  Helper function that returns the position of the filename/extension
    separator dot in path.

    Params:
        path = file spec as string or indexable range
    Returns:
        index of extension separator (the dot), or -1 if not found
*/
private ptrdiff_t extSeparatorPos(R)(const R path)
if (isRandomAccessRange!R && hasLength!R && isSomeChar!(ElementType!R) ||
    isNarrowString!R)
{
    for (auto i = path.length; i-- > 0 && !isSeparator(path[i]); )
    {
        if (path[i] == '.' && i > 0 && !isSeparator(path[i-1]))
            return i;
    }
    return -1;
}

@safe unittest
{
    assert(extSeparatorPos("file") == -1);
    assert(extSeparatorPos("file.ext"w) == 4);
    assert(extSeparatorPos("file.ext1.ext2"d) == 9);
    assert(extSeparatorPos(".foo".dup) == -1);
    assert(extSeparatorPos(".foo.ext"w.dup) == 4);
}

@safe unittest
{
    assert(extSeparatorPos("dir/file"d.dup) == -1);
    assert(extSeparatorPos("dir/file.ext") == 8);
    assert(extSeparatorPos("dir/file.ext1.ext2"w) == 13);
    assert(extSeparatorPos("dir/.foo"d) == -1);
    assert(extSeparatorPos("dir/.foo.ext".dup) == 8);

    version (Windows)
    {
        assert(extSeparatorPos("dir\\file") == -1);
        assert(extSeparatorPos("dir\\file.ext") == 8);
        assert(extSeparatorPos("dir\\file.ext1.ext2") == 13);
        assert(extSeparatorPos("dir\\.foo") == -1);
        assert(extSeparatorPos("dir\\.foo.ext") == 8);

        assert(extSeparatorPos("d:file") == -1);
        assert(extSeparatorPos("d:file.ext") == 6);
        assert(extSeparatorPos("d:file.ext1.ext2") == 11);
        assert(extSeparatorPos("d:.foo") == -1);
        assert(extSeparatorPos("d:.foo.ext") == 6);
    }

    static assert(extSeparatorPos("file") == -1);
    static assert(extSeparatorPos("file.ext"w) == 4);
}


/**
    Params: path = A path name.
    Returns: The _extension part of a file name, including the dot.

    If there is no _extension, `null` is returned.
*/
auto extension(R)(R path)
if (isRandomAccessRange!R && hasSlicing!R && isSomeChar!(ElementType!R) ||
    is(StringTypeOf!R))
{
    auto i = extSeparatorPos!(BaseOf!R)(path);
    if (i == -1)
    {
        static if (is(StringTypeOf!R))
            return StringTypeOf!R.init[];   // which is null
        else
            return path[0 .. 0];
    }
    else return path[i .. path.length];
}

///
@safe unittest
{
    import std.range : empty;
    assert(extension("file").empty);
    assert(extension("file.") == ".");
    assert(extension("file.ext"w) == ".ext");
    assert(extension("file.ext1.ext2"d) == ".ext2");
    assert(extension(".foo".dup).empty);
    assert(extension(".foo.ext"w.dup) == ".ext");

    static assert(extension("file").empty);
    static assert(extension("file.ext") == ".ext");
}

@safe unittest
{
    {
        auto r = MockRange!(immutable(char))(`file.ext1.ext2`);
        auto s = r.extension();
        foreach (i, c; `.ext2`)
            assert(s[i] == c);
    }

    static struct DirEntry { string s; alias s this; }
    assert(extension(DirEntry("file")).empty);
}


/** Remove extension from path.

    Params:
        path = string or range to be sliced

    Returns:
        slice of path with the extension (if any) stripped off
*/
auto stripExtension(R)(R path)
if (isRandomAccessRange!R && hasSlicing!R && hasLength!R && isSomeChar!(ElementType!R) && !isSomeString!R)
{
    return _stripExtension(path);
}

/// Ditto
auto stripExtension(C)(C[] path)
if (isSomeChar!C)
{
    return _stripExtension(path);
}

///
@safe unittest
{
    assert(stripExtension("file")           == "file");
    assert(stripExtension("file.ext")       == "file");
    assert(stripExtension("file.ext1.ext2") == "file.ext1");
    assert(stripExtension("file.")          == "file");
    assert(stripExtension(".file")          == ".file");
    assert(stripExtension(".file.ext")      == ".file");
    assert(stripExtension("dir/file.ext")   == "dir/file");
}

@safe unittest
{
    assert(testAliasedString!stripExtension("file"));

    enum S : string { a = "foo.bar" }
    assert(S.a.stripExtension == "foo");

    char[S.a.length] sa = S.a[];
    assert(sa.stripExtension == "foo");
}

@safe unittest
{
    assert(stripExtension("file.ext"w) == "file");
    assert(stripExtension("file.ext1.ext2"d) == "file.ext1");

    import std.array;
    import std.utf : byChar, byWchar, byDchar;

    assert(stripExtension("file".byChar).array == "file");
    assert(stripExtension("file.ext"w.byWchar).array == "file");
    assert(stripExtension("file.ext1.ext2"d.byDchar).array == "file.ext1");
}

private auto _stripExtension(R)(R path)
{
    immutable i = extSeparatorPos(path);
    return i == -1 ? path : path[0 .. i];
}

/** Sets or replaces an extension.

    If the filename already has an extension, it is replaced. If not, the
    extension is simply appended to the filename. Including a leading dot
    in `ext` is optional.

    If the extension is empty, this function is equivalent to
    $(LREF stripExtension).

    This function normally allocates a new string (the possible exception
    being the case when path is immutable and doesn't already have an
    extension).

    Params:
        path = A path name
        ext = The new extension

    Returns: A string containing the path given by `path`, but where
    the extension has been set to `ext`.

    See_Also:
        $(LREF withExtension) which does not allocate and returns a lazy range.
*/
immutable(C1)[] setExtension(C1, C2)(in C1[] path, in C2[] ext)
if (isSomeChar!C1 && !is(C1 == immutable) && is(immutable C1 == immutable C2))
{
    try
    {
        import std.conv : to;
        return withExtension(path, ext).to!(typeof(return));
    }
    catch (Exception e)
    {
        assert(0);
    }
}

///ditto
immutable(C1)[] setExtension(C1, C2)(immutable(C1)[] path, const(C2)[] ext)
if (isSomeChar!C1 && is(immutable C1 == immutable C2))
{
    if (ext.length == 0)
        return stripExtension(path);

    try
    {
        import std.conv : to;
        return withExtension(path, ext).to!(typeof(return));
    }
    catch (Exception e)
    {
        assert(0);
    }
}

///
@safe unittest
{
    assert(setExtension("file", "ext") == "file.ext");
    assert(setExtension("file"w, ".ext"w) == "file.ext");
    assert(setExtension("file."d, "ext"d) == "file.ext");
    assert(setExtension("file.", ".ext") == "file.ext");
    assert(setExtension("file.old"w, "new"w) == "file.new");
    assert(setExtension("file.old"d, ".new"d) == "file.new");
}

@safe unittest
{
    assert(setExtension("file"w.dup, "ext"w) == "file.ext");
    assert(setExtension("file"w.dup, ".ext"w) == "file.ext");
    assert(setExtension("file."w, "ext"w.dup) == "file.ext");
    assert(setExtension("file."w, ".ext"w.dup) == "file.ext");
    assert(setExtension("file.old"d.dup, "new"d) == "file.new");
    assert(setExtension("file.old"d.dup, ".new"d) == "file.new");

    static assert(setExtension("file", "ext") == "file.ext");
    static assert(setExtension("file.old", "new") == "file.new");

    static assert(setExtension("file"w.dup, "ext"w) == "file.ext");
    static assert(setExtension("file.old"d.dup, "new"d) == "file.new");

    // https://issues.dlang.org/show_bug.cgi?id=10601
    assert(setExtension("file", "") == "file");
    assert(setExtension("file.ext", "") == "file");
}

/************
 * Replace existing extension on filespec with new one.
 *
 * Params:
 *      path = string or random access range representing a filespec
 *      ext = the new extension
 * Returns:
 *      Range with `path`'s extension (if any) replaced with `ext`.
 *      The element encoding type of the returned range will be the same as `path`'s.
 * See_Also:
 *      $(LREF setExtension)
 */
auto withExtension(R, C)(R path, C[] ext)
if (isRandomAccessRange!R && hasSlicing!R && hasLength!R && isSomeChar!(ElementType!R) &&
    !isSomeString!R && isSomeChar!C)
{
    return _withExtension(path, ext);
}

/// Ditto
auto withExtension(C1, C2)(C1[] path, C2[] ext)
if (isSomeChar!C1 && isSomeChar!C2)
{
    return _withExtension(path, ext);
}

///
@safe unittest
{
    import std.array;
    assert(withExtension("file", "ext").array == "file.ext");
    assert(withExtension("file"w, ".ext"w).array == "file.ext");
    assert(withExtension("file.ext"w, ".").array == "file.");

    import std.utf : byChar, byWchar;
    assert(withExtension("file".byChar, "ext").array == "file.ext");
    assert(withExtension("file"w.byWchar, ".ext"w).array == "file.ext"w);
    assert(withExtension("file.ext"w.byWchar, ".").array == "file."w);
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    assert(testAliasedString!withExtension("file", "ext"));

    enum S : string { a = "foo.bar" }
    assert(equal(S.a.withExtension(".txt"), "foo.txt"));

    char[S.a.length] sa = S.a[];
    assert(equal(sa.withExtension(".txt"), "foo.txt"));
}

private auto _withExtension(R, C)(R path, C[] ext)
{
    import std.range : only, chain;
    import std.utf : byUTF;

    alias CR = Unqual!(ElementEncodingType!R);
    auto dot = only(CR('.'));
    if (ext.length == 0 || ext[0] == '.')
        dot.popFront();                 // so dot is an empty range, too
    return chain(stripExtension(path).byUTF!CR, dot, ext.byUTF!CR);
}

/** Params:
        path = A path name.
        ext = The default extension to use.

    Returns: The path given by `path`, with the extension given by `ext`
    appended if the path doesn't already have one.

    Including the dot in the extension is optional.

    This function always allocates a new string, except in the case when
    path is immutable and already has an extension.
*/
immutable(C1)[] defaultExtension(C1, C2)(in C1[] path, in C2[] ext)
if (isSomeChar!C1 && is(immutable C1 == immutable C2))
{
    import std.conv : to;
    return withDefaultExtension(path, ext).to!(typeof(return));
}

///
@safe unittest
{
    assert(defaultExtension("file", "ext") == "file.ext");
    assert(defaultExtension("file", ".ext") == "file.ext");
    assert(defaultExtension("file.", "ext")     == "file.");
    assert(defaultExtension("file.old", "new") == "file.old");
    assert(defaultExtension("file.old", ".new") == "file.old");
}

@safe unittest
{
    assert(defaultExtension("file"w.dup, "ext"w) == "file.ext");
    assert(defaultExtension("file.old"d.dup, "new"d) == "file.old");

    static assert(defaultExtension("file", "ext") == "file.ext");
    static assert(defaultExtension("file.old", "new") == "file.old");

    static assert(defaultExtension("file"w.dup, "ext"w) == "file.ext");
    static assert(defaultExtension("file.old"d.dup, "new"d) == "file.old");
}


/********************************
 * Set the extension of `path` to `ext` if `path` doesn't have one.
 *
 * Params:
 *      path = filespec as string or range
 *      ext = extension, may have leading '.'
 * Returns:
 *      range with the result
 */
auto withDefaultExtension(R, C)(R path, C[] ext)
if (isRandomAccessRange!R && hasSlicing!R && hasLength!R && isSomeChar!(ElementType!R) &&
    !isSomeString!R && isSomeChar!C)
{
    return _withDefaultExtension(path, ext);
}

/// Ditto
auto withDefaultExtension(C1, C2)(C1[] path, C2[] ext)
if (isSomeChar!C1 && isSomeChar!C2)
{
    return _withDefaultExtension(path, ext);
}

///
@safe unittest
{
    import std.array;
    assert(withDefaultExtension("file", "ext").array == "file.ext");
    assert(withDefaultExtension("file"w, ".ext").array == "file.ext"w);
    assert(withDefaultExtension("file.", "ext").array == "file.");
    assert(withDefaultExtension("file", "").array == "file.");

    import std.utf : byChar, byWchar;
    assert(withDefaultExtension("file".byChar, "ext").array == "file.ext");
    assert(withDefaultExtension("file"w.byWchar, ".ext").array == "file.ext"w);
    assert(withDefaultExtension("file.".byChar, "ext"d).array == "file.");
    assert(withDefaultExtension("file".byChar, "").array == "file.");
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    assert(testAliasedString!withDefaultExtension("file", "ext"));

    enum S : string { a = "foo" }
    assert(equal(S.a.withDefaultExtension(".txt"), "foo.txt"));

    char[S.a.length] sa = S.a[];
    assert(equal(sa.withDefaultExtension(".txt"), "foo.txt"));
}

private auto _withDefaultExtension(R, C)(R path, C[] ext)
{
    import std.range : only, chain;
    import std.utf : byUTF;

    alias CR = Unqual!(ElementEncodingType!R);
    auto dot = only(CR('.'));
    immutable i = extSeparatorPos(path);
    if (i == -1)
    {
        if (ext.length > 0 && ext[0] == '.')
            ext = ext[1 .. $];              // remove any leading . from ext[]
    }
    else
    {
        // path already has an extension, so make these empty
        ext = ext[0 .. 0];
        dot.popFront();
    }
    return chain(path.byUTF!CR, dot, ext.byUTF!CR);
}

/** Combines one or more path segments.

    This function takes a set of path segments, given as an input
    range of string elements or as a set of string arguments,
    and concatenates them with each other.  Directory separators
    are inserted between segments if necessary.  If any of the
    path segments are absolute (as defined by $(LREF isAbsolute)), the
    preceding segments will be dropped.

    On Windows, if one of the path segments are rooted, but not absolute
    (e.g. $(D `\foo`)), all preceding path segments down to the previous
    root will be dropped.  (See below for an example.)

    This function always allocates memory to hold the resulting path.
    The variadic overload is guaranteed to only perform a single
    allocation, as is the range version if `paths` is a forward
    range.

    Params:
        segments = An $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
        of segments to assemble the path from.
    Returns: The assembled path.
*/
immutable(ElementEncodingType!(ElementType!Range))[] buildPath(Range)(scope Range segments)
if (isInputRange!Range && !isInfinite!Range && isSomeString!(ElementType!Range))
{
    if (segments.empty) return null;

    // If this is a forward range, we can pre-calculate a maximum length.
    static if (isForwardRange!Range)
    {
        auto segments2 = segments.save;
        size_t precalc = 0;
        foreach (segment; segments2) precalc += segment.length + 1;
    }
    // Otherwise, just venture a guess and resize later if necessary.
    else size_t precalc = 255;

    auto buf = new Unqual!(ElementEncodingType!(ElementType!Range))[](precalc);
    size_t pos = 0;
    foreach (segment; segments)
    {
        if (segment.empty) continue;
        static if (!isForwardRange!Range)
        {
            immutable neededLength = pos + segment.length + 1;
            if (buf.length < neededLength)
                buf.length = reserve(buf, neededLength + buf.length/2);
        }
        auto r = chainPath(buf[0 .. pos], segment);
        size_t i;
        foreach (c; r)
        {
            buf[i] = c;
            ++i;
        }
        pos = i;
    }
    static U trustedCast(U, V)(V v) @trusted pure nothrow { return cast(U) v; }
    return trustedCast!(typeof(return))(buf[0 .. pos]);
}

/// ditto
immutable(C)[] buildPath(C)(const(C)[][] paths...)
    @safe pure nothrow
if (isSomeChar!C)
{
    return buildPath!(typeof(paths))(paths);
}

///
@safe unittest
{
    version (Posix)
    {
        assert(buildPath("foo", "bar", "baz") == "foo/bar/baz");
        assert(buildPath("/foo/", "bar/baz")  == "/foo/bar/baz");
        assert(buildPath("/foo", "/bar")      == "/bar");
    }

    version (Windows)
    {
        assert(buildPath("foo", "bar", "baz") == `foo\bar\baz`);
        assert(buildPath(`c:\foo`, `bar\baz`) == `c:\foo\bar\baz`);
        assert(buildPath("foo", `d:\bar`)     == `d:\bar`);
        assert(buildPath("foo", `\bar`)       == `\bar`);
        assert(buildPath(`c:\foo`, `\bar`)    == `c:\bar`);
    }
}

@system unittest // non-documented
{
    import std.range;
    // ir() wraps an array in a plain (i.e. non-forward) input range, so that
    // we can test both code paths
    InputRange!(C[]) ir(C)(C[][] p...) { return inputRangeObject(p.dup); }
    version (Posix)
    {
        assert(buildPath("foo") == "foo");
        assert(buildPath("/foo/") == "/foo/");
        assert(buildPath("foo", "bar") == "foo/bar");
        assert(buildPath("foo", "bar", "baz") == "foo/bar/baz");
        assert(buildPath("foo/".dup, "bar") == "foo/bar");
        assert(buildPath("foo///", "bar".dup) == "foo///bar");
        assert(buildPath("/foo"w, "bar"w) == "/foo/bar");
        assert(buildPath("foo"w.dup, "/bar"w) == "/bar");
        assert(buildPath("foo"w, "bar/"w.dup) == "foo/bar/");
        assert(buildPath("/"d, "foo"d) == "/foo");
        assert(buildPath(""d.dup, "foo"d) == "foo");
        assert(buildPath("foo"d, ""d.dup) == "foo");
        assert(buildPath("foo", "bar".dup, "baz") == "foo/bar/baz");
        assert(buildPath("foo"w, "/bar"w, "baz"w.dup) == "/bar/baz");

        static assert(buildPath("foo", "bar", "baz") == "foo/bar/baz");
        static assert(buildPath("foo", "/bar", "baz") == "/bar/baz");

        // The following are mostly duplicates of the above, except that the
        // range version does not accept mixed constness.
        assert(buildPath(ir("foo")) == "foo");
        assert(buildPath(ir("/foo/")) == "/foo/");
        assert(buildPath(ir("foo", "bar")) == "foo/bar");
        assert(buildPath(ir("foo", "bar", "baz")) == "foo/bar/baz");
        assert(buildPath(ir("foo/".dup, "bar".dup)) == "foo/bar");
        assert(buildPath(ir("foo///".dup, "bar".dup)) == "foo///bar");
        assert(buildPath(ir("/foo"w, "bar"w)) == "/foo/bar");
        assert(buildPath(ir("foo"w.dup, "/bar"w.dup)) == "/bar");
        assert(buildPath(ir("foo"w.dup, "bar/"w.dup)) == "foo/bar/");
        assert(buildPath(ir("/"d, "foo"d)) == "/foo");
        assert(buildPath(ir(""d.dup, "foo"d.dup)) == "foo");
        assert(buildPath(ir("foo"d, ""d)) == "foo");
        assert(buildPath(ir("foo", "bar", "baz")) == "foo/bar/baz");
        assert(buildPath(ir("foo"w.dup, "/bar"w.dup, "baz"w.dup)) == "/bar/baz");
    }
    version (Windows)
    {
        assert(buildPath("foo") == "foo");
        assert(buildPath(`\foo/`) == `\foo/`);
        assert(buildPath("foo", "bar", "baz") == `foo\bar\baz`);
        assert(buildPath("foo", `\bar`) == `\bar`);
        assert(buildPath(`c:\foo`, "bar") == `c:\foo\bar`);
        assert(buildPath("foo"w, `d:\bar`w.dup) ==  `d:\bar`);
        assert(buildPath(`c:\foo\bar`, `\baz`) == `c:\baz`);
        assert(buildPath(`\\foo\bar\baz`d, `foo`d, `\bar`d) == `\\foo\bar\bar`d);

        static assert(buildPath("foo", "bar", "baz") == `foo\bar\baz`);
        static assert(buildPath("foo", `c:\bar`, "baz") == `c:\bar\baz`);

        assert(buildPath(ir("foo")) == "foo");
        assert(buildPath(ir(`\foo/`)) == `\foo/`);
        assert(buildPath(ir("foo", "bar", "baz")) == `foo\bar\baz`);
        assert(buildPath(ir("foo", `\bar`)) == `\bar`);
        assert(buildPath(ir(`c:\foo`, "bar")) == `c:\foo\bar`);
        assert(buildPath(ir("foo"w.dup, `d:\bar`w.dup)) ==  `d:\bar`);
        assert(buildPath(ir(`c:\foo\bar`, `\baz`)) == `c:\baz`);
        assert(buildPath(ir(`\\foo\bar\baz`d, `foo`d, `\bar`d)) == `\\foo\bar\bar`d);
    }

    // Test that allocation works as it should.
    auto manyShort = "aaa".repeat(1000).array();
    auto manyShortCombined = join(manyShort, dirSeparator);
    assert(buildPath(manyShort) == manyShortCombined);
    assert(buildPath(ir(manyShort)) == manyShortCombined);

    auto fewLong = 'b'.repeat(500).array().repeat(10).array();
    auto fewLongCombined = join(fewLong, dirSeparator);
    assert(buildPath(fewLong) == fewLongCombined);
    assert(buildPath(ir(fewLong)) == fewLongCombined);
}

@safe unittest
{
    // Test for https://issues.dlang.org/show_bug.cgi?id=7397
    string[] ary = ["a", "b"];
    version (Posix)
    {
        assert(buildPath(ary) == "a/b");
    }
    else version (Windows)
    {
        assert(buildPath(ary) == `a\b`);
    }
}


/**
 * Concatenate path segments together to form one path.
 *
 * Params:
 *      r1 = first segment
 *      r2 = second segment
 *      ranges = 0 or more segments
 * Returns:
 *      Lazy range which is the concatenation of r1, r2 and ranges with path separators.
 *      The resulting element type is that of r1.
 * See_Also:
 *      $(LREF buildPath)
 */
auto chainPath(R1, R2, Ranges...)(R1 r1, R2 r2, Ranges ranges)
if ((isRandomAccessRange!R1 && hasSlicing!R1 && hasLength!R1 && isSomeChar!(ElementType!R1) ||
    isNarrowString!R1 &&
    !isConvertibleToString!R1) &&
    (isRandomAccessRange!R2 && hasSlicing!R2 && hasLength!R2 && isSomeChar!(ElementType!R2) ||
    isNarrowString!R2 &&
    !isConvertibleToString!R2) &&
    (Ranges.length == 0 || is(typeof(chainPath(r2, ranges))))
    )
{
    static if (Ranges.length)
    {
        return chainPath(chainPath(r1, r2), ranges);
    }
    else
    {
        import std.range : only, chain;
        import std.utf : byUTF;

        alias CR = Unqual!(ElementEncodingType!R1);
        auto sep = only(CR(dirSeparator[0]));
        bool usesep = false;

        auto pos = r1.length;

        if (pos)
        {
            if (isRooted(r2))
            {
                version (Posix)
                {
                    pos = 0;
                }
                else version (Windows)
                {
                    if (isAbsolute(r2))
                        pos = 0;
                    else
                    {
                        pos = rootName(r1).length;
                        if (pos > 0 && isDirSeparator(r1[pos - 1]))
                            --pos;
                    }
                }
                else
                    static assert(0);
            }
            else if (!isDirSeparator(r1[pos - 1]))
                usesep = true;
        }
        if (!usesep)
            sep.popFront();
        // Return r1 ~ '/' ~ r2
        return chain(r1[0 .. pos].byUTF!CR, sep, r2.byUTF!CR);
    }
}

///
@safe unittest
{
    import std.array;
    version (Posix)
    {
        assert(chainPath("foo", "bar", "baz").array == "foo/bar/baz");
        assert(chainPath("/foo/", "bar/baz").array  == "/foo/bar/baz");
        assert(chainPath("/foo", "/bar").array      == "/bar");
    }

    version (Windows)
    {
        assert(chainPath("foo", "bar", "baz").array == `foo\bar\baz`);
        assert(chainPath(`c:\foo`, `bar\baz`).array == `c:\foo\bar\baz`);
        assert(chainPath("foo", `d:\bar`).array     == `d:\bar`);
        assert(chainPath("foo", `\bar`).array       == `\bar`);
        assert(chainPath(`c:\foo`, `\bar`).array    == `c:\bar`);
    }

    import std.utf : byChar;
    version (Posix)
    {
        assert(chainPath("foo", "bar", "baz").array == "foo/bar/baz");
        assert(chainPath("/foo/".byChar, "bar/baz").array  == "/foo/bar/baz");
        assert(chainPath("/foo", "/bar".byChar).array      == "/bar");
    }

    version (Windows)
    {
        assert(chainPath("foo", "bar", "baz").array == `foo\bar\baz`);
        assert(chainPath(`c:\foo`.byChar, `bar\baz`).array == `c:\foo\bar\baz`);
        assert(chainPath("foo", `d:\bar`).array     == `d:\bar`);
        assert(chainPath("foo", `\bar`.byChar).array       == `\bar`);
        assert(chainPath(`c:\foo`, `\bar`w).array    == `c:\bar`);
    }
}

auto chainPath(Ranges...)(auto ref Ranges ranges)
if (Ranges.length >= 2 &&
    std.meta.anySatisfy!(isConvertibleToString, Ranges))
{
    import std.meta : staticMap;
    alias Types = staticMap!(convertToString, Ranges);
    return chainPath!Types(ranges);
}

@safe unittest
{
    assert(chainPath(TestAliasedString(null), TestAliasedString(null), TestAliasedString(null)).empty);
    assert(chainPath(TestAliasedString(null), TestAliasedString(null), "").empty);
    assert(chainPath(TestAliasedString(null), "", TestAliasedString(null)).empty);
    static struct S { string s; }
    static assert(!__traits(compiles, chainPath(TestAliasedString(null), S(""), TestAliasedString(null))));
}

/** Performs the same task as $(LREF buildPath),
    while at the same time resolving current/parent directory
    symbols (`"."` and `".."`) and removing superfluous
    directory separators.
    It will return "." if the path leads to the starting directory.
    On Windows, slashes are replaced with backslashes.

    Using buildNormalizedPath on null paths will always return null.

    Note that this function does not resolve symbolic links.

    This function always allocates memory to hold the resulting path.
    Use $(LREF asNormalizedPath) to not allocate memory.

    Params:
        paths = An array of paths to assemble.

    Returns: The assembled path.
*/
immutable(C)[] buildNormalizedPath(C)(const(C[])[] paths...)
    @safe pure nothrow
if (isSomeChar!C)
{
    import std.array : array;

    const(C)[] chained;
    foreach (path; paths)
    {
        if (chained)
            chained = chainPath(chained, path).array;
        else
            chained = path;
    }
    auto result = asNormalizedPath(chained);
    // .array returns a copy, so it is unique
    return result.array;
}

///
@safe unittest
{
    assert(buildNormalizedPath("foo", "..") == ".");

    version (Posix)
    {
        assert(buildNormalizedPath("/foo/./bar/..//baz/") == "/foo/baz");
        assert(buildNormalizedPath("../foo/.") == "../foo");
        assert(buildNormalizedPath("/foo", "bar/baz/") == "/foo/bar/baz");
        assert(buildNormalizedPath("/foo", "/bar/..", "baz") == "/baz");
        assert(buildNormalizedPath("foo/./bar", "../../", "../baz") == "../baz");
        assert(buildNormalizedPath("/foo/./bar", "../../baz") == "/baz");
    }

    version (Windows)
    {
        assert(buildNormalizedPath(`c:\foo\.\bar/..\\baz\`) == `c:\foo\baz`);
        assert(buildNormalizedPath(`..\foo\.`) == `..\foo`);
        assert(buildNormalizedPath(`c:\foo`, `bar\baz\`) == `c:\foo\bar\baz`);
        assert(buildNormalizedPath(`c:\foo`, `bar/..`) == `c:\foo`);
        assert(buildNormalizedPath(`\\server\share\foo`, `..\bar`) ==
                `\\server\share\bar`);
    }
}

@safe unittest
{
    assert(buildNormalizedPath(".", ".") == ".");
    assert(buildNormalizedPath("foo", "..") == ".");
    assert(buildNormalizedPath("", "") is null);
    assert(buildNormalizedPath("", ".") == ".");
    assert(buildNormalizedPath(".", "") == ".");
    assert(buildNormalizedPath(null, "foo") == "foo");
    assert(buildNormalizedPath("", "foo") == "foo");
    assert(buildNormalizedPath("", "") == "");
    assert(buildNormalizedPath("", null) == "");
    assert(buildNormalizedPath(null, "") == "");
    assert(buildNormalizedPath!(char)(null, null) == "");

    version (Posix)
    {
        assert(buildNormalizedPath("/", "foo", "bar") == "/foo/bar");
        assert(buildNormalizedPath("foo", "bar", "baz") == "foo/bar/baz");
        assert(buildNormalizedPath("foo", "bar/baz") == "foo/bar/baz");
        assert(buildNormalizedPath("foo", "bar//baz///") == "foo/bar/baz");
        assert(buildNormalizedPath("/foo", "bar/baz") == "/foo/bar/baz");
        assert(buildNormalizedPath("/foo", "/bar/baz") == "/bar/baz");
        assert(buildNormalizedPath("/foo/..", "/bar/./baz") == "/bar/baz");
        assert(buildNormalizedPath("/foo/..", "bar/baz") == "/bar/baz");
        assert(buildNormalizedPath("/foo/../../", "bar/baz") == "/bar/baz");
        assert(buildNormalizedPath("/foo/bar", "../baz") == "/foo/baz");
        assert(buildNormalizedPath("/foo/bar", "../../baz") == "/baz");
        assert(buildNormalizedPath("/foo/bar", ".././/baz/..", "wee/") == "/foo/wee");
        assert(buildNormalizedPath("//foo/bar", "baz///wee") == "/foo/bar/baz/wee");
        static assert(buildNormalizedPath("/foo/..", "/bar/./baz") == "/bar/baz");
    }
    else version (Windows)
    {
        assert(buildNormalizedPath(`\`, `foo`, `bar`) == `\foo\bar`);
        assert(buildNormalizedPath(`foo`, `bar`, `baz`) == `foo\bar\baz`);
        assert(buildNormalizedPath(`foo`, `bar\baz`) == `foo\bar\baz`);
        assert(buildNormalizedPath(`foo`, `bar\\baz\\\`) == `foo\bar\baz`);
        assert(buildNormalizedPath(`\foo`, `bar\baz`) == `\foo\bar\baz`);
        assert(buildNormalizedPath(`\foo`, `\bar\baz`) == `\bar\baz`);
        assert(buildNormalizedPath(`\foo\..`, `\bar\.\baz`) == `\bar\baz`);
        assert(buildNormalizedPath(`\foo\..`, `bar\baz`) == `\bar\baz`);
        assert(buildNormalizedPath(`\foo\..\..\`, `bar\baz`) == `\bar\baz`);
        assert(buildNormalizedPath(`\foo\bar`, `..\baz`) == `\foo\baz`);
        assert(buildNormalizedPath(`\foo\bar`, `../../baz`) == `\baz`);
        assert(buildNormalizedPath(`\foo\bar`, `..\.\/baz\..`, `wee\`) == `\foo\wee`);

        assert(buildNormalizedPath(`c:\`, `foo`, `bar`) == `c:\foo\bar`);
        assert(buildNormalizedPath(`c:foo`, `bar`, `baz`) == `c:foo\bar\baz`);
        assert(buildNormalizedPath(`c:foo`, `bar\baz`) == `c:foo\bar\baz`);
        assert(buildNormalizedPath(`c:foo`, `bar\\baz\\\`) == `c:foo\bar\baz`);
        assert(buildNormalizedPath(`c:\foo`, `bar\baz`) == `c:\foo\bar\baz`);
        assert(buildNormalizedPath(`c:\foo`, `\bar\baz`) == `c:\bar\baz`);
        assert(buildNormalizedPath(`c:\foo\..`, `\bar\.\baz`) == `c:\bar\baz`);
        assert(buildNormalizedPath(`c:\foo\..`, `bar\baz`) == `c:\bar\baz`);
        assert(buildNormalizedPath(`c:\foo\..\..\`, `bar\baz`) == `c:\bar\baz`);
        assert(buildNormalizedPath(`c:\foo\bar`, `..\baz`) == `c:\foo\baz`);
        assert(buildNormalizedPath(`c:\foo\bar`, `..\..\baz`) == `c:\baz`);
        assert(buildNormalizedPath(`c:\foo\bar`, `..\.\\baz\..`, `wee\`) == `c:\foo\wee`);

        assert(buildNormalizedPath(`\\server\share`, `foo`, `bar`) == `\\server\share\foo\bar`);
        assert(buildNormalizedPath(`\\server\share\`, `foo`, `bar`) == `\\server\share\foo\bar`);
        assert(buildNormalizedPath(`\\server\share\foo`, `bar\baz`) == `\\server\share\foo\bar\baz`);
        assert(buildNormalizedPath(`\\server\share\foo`, `\bar\baz`) == `\\server\share\bar\baz`);
        assert(buildNormalizedPath(`\\server\share\foo\..`, `\bar\.\baz`) == `\\server\share\bar\baz`);
        assert(buildNormalizedPath(`\\server\share\foo\..`, `bar\baz`) == `\\server\share\bar\baz`);
        assert(buildNormalizedPath(`\\server\share\foo\..\..\`, `bar\baz`) == `\\server\share\bar\baz`);
        assert(buildNormalizedPath(`\\server\share\foo\bar`, `..\baz`) == `\\server\share\foo\baz`);
        assert(buildNormalizedPath(`\\server\share\foo\bar`, `..\..\baz`) == `\\server\share\baz`);
        assert(buildNormalizedPath(`\\server\share\foo\bar`, `..\.\\baz\..`, `wee\`) == `\\server\share\foo\wee`);

        static assert(buildNormalizedPath(`\foo\..\..\`, `bar\baz`) == `\bar\baz`);
    }
    else static assert(0);
}

@safe unittest
{
    // Test for https://issues.dlang.org/show_bug.cgi?id=7397
    string[] ary = ["a", "b"];
    version (Posix)
    {
        assert(buildNormalizedPath(ary) == "a/b");
    }
    else version (Windows)
    {
        assert(buildNormalizedPath(ary) == `a\b`);
    }
}


/** Normalize a path by resolving current/parent directory
    symbols (`"."` and `".."`) and removing superfluous
    directory separators.
    It will return "." if the path leads to the starting directory.
    On Windows, slashes are replaced with backslashes.

    Using asNormalizedPath on empty paths will always return an empty path.

    Does not resolve symbolic links.

    This function always allocates memory to hold the resulting path.
    Use $(LREF buildNormalizedPath) to allocate memory and return a string.

    Params:
        path = string or random access range representing the path to normalize

    Returns:
        normalized path as a forward range
*/

auto asNormalizedPath(R)(return scope R path)
if (isSomeChar!(ElementEncodingType!R) &&
    (isRandomAccessRange!R && hasSlicing!R && hasLength!R || isNarrowString!R) &&
    !isConvertibleToString!R)
{
    alias C = Unqual!(ElementEncodingType!R);
    alias S = typeof(path[0 .. 0]);

    static struct Result
    {
        @property bool empty()
        {
            return c == c.init;
        }

        @property C front()
        {
            return c;
        }

        void popFront()
        {
            C lastc = c;
            c = c.init;
            if (!element.empty)
            {
                c = getElement0();
                return;
            }
          L1:
            while (1)
            {
                if (elements.empty)
                {
                    element = element[0 .. 0];
                    return;
                }
                element = elements.front;
                elements.popFront();
                if (isDot(element) || (rooted && isDotDot(element)))
                    continue;

                if (rooted || !isDotDot(element))
                {
                    int n = 1;
                    auto elements2 = elements.save;
                    while (!elements2.empty)
                    {
                        auto e = elements2.front;
                        elements2.popFront();
                        if (isDot(e))
                            continue;
                        if (isDotDot(e))
                        {
                            --n;
                            if (n == 0)
                            {
                                elements = elements2;
                                element = element[0 .. 0];
                                continue L1;
                            }
                        }
                        else
                            ++n;
                    }
                }
                break;
            }

            static assert(dirSeparator.length == 1);
            if (lastc == dirSeparator[0] || lastc == lastc.init)
                c = getElement0();
            else
                c = dirSeparator[0];
        }

        static if (isForwardRange!R)
        {
            @property auto save()
            {
                auto result = this;
                result.element = element.save;
                result.elements = elements.save;
                return result;
            }
        }

      private:
        this(R path)
        {
            element = rootName(path);
            auto i = element.length;
            while (i < path.length && isDirSeparator(path[i]))
                ++i;
            rooted = i > 0;
            elements = pathSplitter(path[i .. $]);
            popFront();
            if (c == c.init && path.length)
                c = C('.');
        }

        C getElement0()
        {
            static if (isNarrowString!S)  // avoid autodecode
            {
                C c = element[0];
                element = element[1 .. $];
            }
            else
            {
                C c = element.front;
                element.popFront();
            }
            version (Windows)
            {
                if (c == '/')   // can appear in root element
                    c = '\\';   // use native Windows directory separator
            }
            return c;
        }

        // See if elem is "."
        static bool isDot(S elem)
        {
            return elem.length == 1 && elem[0] == '.';
        }

        // See if elem is ".."
        static bool isDotDot(S elem)
        {
            return elem.length == 2 && elem[0] == '.' && elem[1] == '.';
        }

        bool rooted;    // the path starts with a root directory
        C c;
        S element;
        typeof(pathSplitter(path[0 .. 0])) elements;
    }

    return Result(path);
}

///
@safe unittest
{
    import std.array;
    assert(asNormalizedPath("foo/..").array == ".");

    version (Posix)
    {
        assert(asNormalizedPath("/foo/./bar/..//baz/").array == "/foo/baz");
        assert(asNormalizedPath("../foo/.").array == "../foo");
        assert(asNormalizedPath("/foo/bar/baz/").array == "/foo/bar/baz");
        assert(asNormalizedPath("/foo/./bar/../../baz").array == "/baz");
    }

    version (Windows)
    {
        assert(asNormalizedPath(`c:\foo\.\bar/..\\baz\`).array == `c:\foo\baz`);
        assert(asNormalizedPath(`..\foo\.`).array == `..\foo`);
        assert(asNormalizedPath(`c:\foo\bar\baz\`).array == `c:\foo\bar\baz`);
        assert(asNormalizedPath(`c:\foo\bar/..`).array == `c:\foo`);
        assert(asNormalizedPath(`\\server\share\foo\..\bar`).array ==
                `\\server\share\bar`);
    }
}

auto asNormalizedPath(R)(return scope auto ref R path)
if (isConvertibleToString!R)
{
    return asNormalizedPath!(StringTypeOf!R)(path);
}

@safe unittest
{
    assert(testAliasedString!asNormalizedPath(null));
}

@safe unittest
{
    import std.array;
    import std.utf : byChar;

    assert(asNormalizedPath("").array is null);
    assert(asNormalizedPath("foo").array == "foo");
    assert(asNormalizedPath(".").array == ".");
    assert(asNormalizedPath("./.").array == ".");
    assert(asNormalizedPath("foo/..").array == ".");

    auto save = asNormalizedPath("fob").save;
    save.popFront();
    assert(save.front == 'o');

    version (Posix)
    {
        assert(asNormalizedPath("/foo/bar").array == "/foo/bar");
        assert(asNormalizedPath("foo/bar/baz").array == "foo/bar/baz");
        assert(asNormalizedPath("foo/bar/baz").array == "foo/bar/baz");
        assert(asNormalizedPath("foo/bar//baz///").array == "foo/bar/baz");
        assert(asNormalizedPath("/foo/bar/baz").array == "/foo/bar/baz");
        assert(asNormalizedPath("/foo/../bar/baz").array == "/bar/baz");
        assert(asNormalizedPath("/foo/../..//bar/baz").array == "/bar/baz");
        assert(asNormalizedPath("/foo/bar/../baz").array == "/foo/baz");
        assert(asNormalizedPath("/foo/bar/../../baz").array == "/baz");
        assert(asNormalizedPath("/foo/bar/.././/baz/../wee/").array == "/foo/wee");
        assert(asNormalizedPath("//foo/bar/baz///wee").array == "/foo/bar/baz/wee");

        assert(asNormalizedPath("foo//bar").array == "foo/bar");
        assert(asNormalizedPath("foo/bar").array == "foo/bar");

        //Curent dir path
        assert(asNormalizedPath("./").array == ".");
        assert(asNormalizedPath("././").array == ".");
        assert(asNormalizedPath("./foo/..").array == ".");
        assert(asNormalizedPath("foo/..").array == ".");
    }
    else version (Windows)
    {
        assert(asNormalizedPath(`\foo\bar`).array == `\foo\bar`);
        assert(asNormalizedPath(`foo\bar\baz`).array == `foo\bar\baz`);
        assert(asNormalizedPath(`foo\bar\baz`).array == `foo\bar\baz`);
        assert(asNormalizedPath(`foo\bar\\baz\\\`).array == `foo\bar\baz`);
        assert(asNormalizedPath(`\foo\bar\baz`).array == `\foo\bar\baz`);
        assert(asNormalizedPath(`\foo\..\\bar\.\baz`).array == `\bar\baz`);
        assert(asNormalizedPath(`\foo\..\bar\baz`).array == `\bar\baz`);
        assert(asNormalizedPath(`\foo\..\..\\bar\baz`).array == `\bar\baz`);

        assert(asNormalizedPath(`\foo\bar\..\baz`).array == `\foo\baz`);
        assert(asNormalizedPath(`\foo\bar\../../baz`).array == `\baz`);
        assert(asNormalizedPath(`\foo\bar\..\.\/baz\..\wee\`).array == `\foo\wee`);

        assert(asNormalizedPath(`c:\foo\bar`).array == `c:\foo\bar`);
        assert(asNormalizedPath(`c:foo\bar\baz`).array == `c:foo\bar\baz`);
        assert(asNormalizedPath(`c:foo\bar\baz`).array == `c:foo\bar\baz`);
        assert(asNormalizedPath(`c:foo\bar\\baz\\\`).array == `c:foo\bar\baz`);
        assert(asNormalizedPath(`c:\foo\bar\baz`).array == `c:\foo\bar\baz`);

        assert(asNormalizedPath(`c:\foo\..\\bar\.\baz`).array == `c:\bar\baz`);
        assert(asNormalizedPath(`c:\foo\..\bar\baz`).array == `c:\bar\baz`);
        assert(asNormalizedPath(`c:\foo\..\..\\bar\baz`).array == `c:\bar\baz`);
        assert(asNormalizedPath(`c:\foo\bar\..\baz`).array == `c:\foo\baz`);
        assert(asNormalizedPath(`c:\foo\bar\..\..\baz`).array == `c:\baz`);
        assert(asNormalizedPath(`c:\foo\bar\..\.\\baz\..\wee\`).array == `c:\foo\wee`);
        assert(asNormalizedPath(`\\server\share\foo\bar`).array == `\\server\share\foo\bar`);
        assert(asNormalizedPath(`\\server\share\\foo\bar`).array == `\\server\share\foo\bar`);
        assert(asNormalizedPath(`\\server\share\foo\bar\baz`).array == `\\server\share\foo\bar\baz`);
        assert(asNormalizedPath(`\\server\share\foo\..\\bar\.\baz`).array == `\\server\share\bar\baz`);
        assert(asNormalizedPath(`\\server\share\foo\..\bar\baz`).array == `\\server\share\bar\baz`);
        assert(asNormalizedPath(`\\server\share\foo\..\..\\bar\baz`).array == `\\server\share\bar\baz`);
        assert(asNormalizedPath(`\\server\share\foo\bar\..\baz`).array == `\\server\share\foo\baz`);
        assert(asNormalizedPath(`\\server\share\foo\bar\..\..\baz`).array == `\\server\share\baz`);
        assert(asNormalizedPath(`\\server\share\foo\bar\..\.\\baz\..\wee\`).array == `\\server\share\foo\wee`);

        static assert(asNormalizedPath(`\foo\..\..\\bar\baz`).array == `\bar\baz`);

        assert(asNormalizedPath("foo//bar").array == `foo\bar`);

        //Curent dir path
        assert(asNormalizedPath(`.\`).array == ".");
        assert(asNormalizedPath(`.\.\`).array == ".");
        assert(asNormalizedPath(`.\foo\..`).array == ".");
        assert(asNormalizedPath(`foo\..`).array == ".");
    }
    else static assert(0);
}

@safe unittest
{
    import std.array;

    version (Posix)
    {
        // Trivial
        assert(asNormalizedPath("").empty);
        assert(asNormalizedPath("foo/bar").array == "foo/bar");

        // Correct handling of leading slashes
        assert(asNormalizedPath("/").array == "/");
        assert(asNormalizedPath("///").array == "/");
        assert(asNormalizedPath("////").array == "/");
        assert(asNormalizedPath("/foo/bar").array == "/foo/bar");
        assert(asNormalizedPath("//foo/bar").array == "/foo/bar");
        assert(asNormalizedPath("///foo/bar").array == "/foo/bar");
        assert(asNormalizedPath("////foo/bar").array == "/foo/bar");

        // Correct handling of single-dot symbol (current directory)
        assert(asNormalizedPath("/./foo").array == "/foo");
        assert(asNormalizedPath("/foo/./bar").array == "/foo/bar");

        assert(asNormalizedPath("./foo").array == "foo");
        assert(asNormalizedPath("././foo").array == "foo");
        assert(asNormalizedPath("foo/././bar").array == "foo/bar");

        // Correct handling of double-dot symbol (previous directory)
        assert(asNormalizedPath("/foo/../bar").array == "/bar");
        assert(asNormalizedPath("/foo/../../bar").array == "/bar");
        assert(asNormalizedPath("/../foo").array == "/foo");
        assert(asNormalizedPath("/../../foo").array == "/foo");
        assert(asNormalizedPath("/foo/..").array == "/");
        assert(asNormalizedPath("/foo/../..").array == "/");

        assert(asNormalizedPath("foo/../bar").array == "bar");
        assert(asNormalizedPath("foo/../../bar").array == "../bar");
        assert(asNormalizedPath("../foo").array == "../foo");
        assert(asNormalizedPath("../../foo").array == "../../foo");
        assert(asNormalizedPath("../foo/../bar").array == "../bar");
        assert(asNormalizedPath(".././../foo").array == "../../foo");
        assert(asNormalizedPath("foo/bar/..").array == "foo");
        assert(asNormalizedPath("/foo/../..").array == "/");

        // The ultimate path
        assert(asNormalizedPath("/foo/../bar//./../...///baz//").array == "/.../baz");
        static assert(asNormalizedPath("/foo/../bar//./../...///baz//").array == "/.../baz");
    }
    else version (Windows)
    {
        // Trivial
        assert(asNormalizedPath("").empty);
        assert(asNormalizedPath(`foo\bar`).array == `foo\bar`);
        assert(asNormalizedPath("foo/bar").array == `foo\bar`);

        // Correct handling of absolute paths
        assert(asNormalizedPath("/").array == `\`);
        assert(asNormalizedPath(`\`).array == `\`);
        assert(asNormalizedPath(`\\\`).array == `\`);
        assert(asNormalizedPath(`\\\\`).array == `\`);
        assert(asNormalizedPath(`\foo\bar`).array == `\foo\bar`);
        assert(asNormalizedPath(`\\foo`).array == `\\foo`);
        assert(asNormalizedPath(`\\foo\\`).array == `\\foo`);
        assert(asNormalizedPath(`\\foo/bar`).array == `\\foo\bar`);
        assert(asNormalizedPath(`\\\foo\bar`).array == `\foo\bar`);
        assert(asNormalizedPath(`\\\\foo\bar`).array == `\foo\bar`);
        assert(asNormalizedPath(`c:\`).array == `c:\`);
        assert(asNormalizedPath(`c:\foo\bar`).array == `c:\foo\bar`);
        assert(asNormalizedPath(`c:\\foo\bar`).array == `c:\foo\bar`);

        // Correct handling of single-dot symbol (current directory)
        assert(asNormalizedPath(`\./foo`).array == `\foo`);
        assert(asNormalizedPath(`\foo/.\bar`).array == `\foo\bar`);

        assert(asNormalizedPath(`.\foo`).array == `foo`);
        assert(asNormalizedPath(`./.\foo`).array == `foo`);
        assert(asNormalizedPath(`foo\.\./bar`).array == `foo\bar`);

        // Correct handling of double-dot symbol (previous directory)
        assert(asNormalizedPath(`\foo\..\bar`).array == `\bar`);
        assert(asNormalizedPath(`\foo\../..\bar`).array == `\bar`);
        assert(asNormalizedPath(`\..\foo`).array == `\foo`);
        assert(asNormalizedPath(`\..\..\foo`).array == `\foo`);
        assert(asNormalizedPath(`\foo\..`).array == `\`);
        assert(asNormalizedPath(`\foo\../..`).array == `\`);

        assert(asNormalizedPath(`foo\..\bar`).array == `bar`);
        assert(asNormalizedPath(`foo\..\../bar`).array == `..\bar`);

        assert(asNormalizedPath(`..\foo`).array == `..\foo`);
        assert(asNormalizedPath(`..\..\foo`).array == `..\..\foo`);
        assert(asNormalizedPath(`..\foo\..\bar`).array == `..\bar`);
        assert(asNormalizedPath(`..\.\..\foo`).array == `..\..\foo`);
        assert(asNormalizedPath(`foo\bar\..`).array == `foo`);
        assert(asNormalizedPath(`\foo\..\..`).array == `\`);
        assert(asNormalizedPath(`c:\foo\..\..`).array == `c:\`);

        // Correct handling of non-root path with drive specifier
        assert(asNormalizedPath(`c:foo`).array == `c:foo`);
        assert(asNormalizedPath(`c:..\foo\.\..\bar`).array == `c:..\bar`);

        // The ultimate path
        assert(asNormalizedPath(`c:\foo\..\bar\\.\..\...\\\baz\\`).array == `c:\...\baz`);
        static assert(asNormalizedPath(`c:\foo\..\bar\\.\..\...\\\baz\\`).array == `c:\...\baz`);
    }
    else static assert(false);
}

/** Slice up a path into its elements.

    Params:
        path = string or slicable random access range

    Returns:
        bidirectional range of slices of `path`
*/
auto pathSplitter(R)(R path)
if ((isRandomAccessRange!R && hasSlicing!R ||
    isNarrowString!R) &&
    !isConvertibleToString!R)
{
    static struct PathSplitter
    {
        @property bool empty() const { return pe == 0; }

        @property R front()
        {
            assert(!empty);
            return _path[fs .. fe];
        }

        void popFront()
        {
            assert(!empty);
            if (ps == pe)
            {
                if (fs == bs && fe == be)
                {
                    pe = 0;
                }
                else
                {
                    fs = bs;
                    fe = be;
                }
            }
            else
            {
                fs = ps;
                fe = fs;
                while (fe < pe && !isDirSeparator(_path[fe]))
                    ++fe;
                ps = ltrim(fe, pe);
            }
        }

        @property R back()
        {
            assert(!empty);
            return _path[bs .. be];
        }

        void popBack()
        {
            assert(!empty);
            if (ps == pe)
            {
                if (fs == bs && fe == be)
                {
                    pe = 0;
                }
                else
                {
                    bs = fs;
                    be = fe;
                }
            }
            else
            {
                bs = pe;
                be = bs;
                while (bs > ps && !isDirSeparator(_path[bs - 1]))
                    --bs;
                pe = rtrim(ps, bs);
            }
        }
        @property auto save() { return this; }


    private:
        R _path;
        size_t ps, pe;
        size_t fs, fe;
        size_t bs, be;

        this(R p)
        {
            if (p.empty)
            {
                pe = 0;
                return;
            }
            _path = p;

            ps = 0;
            pe = _path.length;

            // If path is rooted, first element is special
            version (Windows)
            {
                if (isUNC(_path))
                {
                    auto i = uncRootLength(_path);
                    fs = 0;
                    fe = i;
                    ps = ltrim(fe, pe);
                }
                else if (isDriveRoot(_path))
                {
                    fs = 0;
                    fe = 3;
                    ps = ltrim(fe, pe);
                }
                else if (_path.length >= 1 && isDirSeparator(_path[0]))
                {
                    fs = 0;
                    fe = 1;
                    ps = ltrim(fe, pe);
                }
                else
                {
                    assert(!isRooted(_path));
                    popFront();
                }
            }
            else version (Posix)
            {
                if (_path.length >= 1 && isDirSeparator(_path[0]))
                {
                    fs = 0;
                    fe = 1;
                    ps = ltrim(fe, pe);
                }
                else
                {
                    popFront();
                }
            }
            else static assert(0);

            if (ps == pe)
            {
                bs = fs;
                be = fe;
            }
            else
            {
                pe = rtrim(ps, pe);
                popBack();
            }
        }

        size_t ltrim(size_t s, size_t e)
        {
            while (s < e && isDirSeparator(_path[s]))
                ++s;
            return s;
        }

        size_t rtrim(size_t s, size_t e)
        {
            while (s < e && isDirSeparator(_path[e - 1]))
                --e;
            return e;
        }
    }

    return PathSplitter(path);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.conv : to;

    assert(equal(pathSplitter("/"), ["/"]));
    assert(equal(pathSplitter("/foo/bar"), ["/", "foo", "bar"]));
    assert(equal(pathSplitter("foo/../bar//./"), ["foo", "..", "bar", "."]));

    version (Posix)
    {
        assert(equal(pathSplitter("//foo/bar"), ["/", "foo", "bar"]));
    }

    version (Windows)
    {
        assert(equal(pathSplitter(`foo\..\bar\/.\`), ["foo", "..", "bar", "."]));
        assert(equal(pathSplitter("c:"), ["c:"]));
        assert(equal(pathSplitter(`c:\foo\bar`), [`c:\`, "foo", "bar"]));
        assert(equal(pathSplitter(`c:foo\bar`), ["c:foo", "bar"]));
    }
}

auto pathSplitter(R)(auto ref R path)
if (isConvertibleToString!R)
{
    return pathSplitter!(StringTypeOf!R)(path);
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    assert(testAliasedString!pathSplitter("/"));
}

@safe unittest
{
    // equal2 verifies that the range is the same both ways, i.e.
    // through front/popFront and back/popBack.
    import std.algorithm;
    import std.range;
    bool equal2(R1, R2)(R1 r1, R2 r2)
    {
        static assert(isBidirectionalRange!R1);
        return equal(r1, r2) && equal(retro(r1), retro(r2));
    }

    assert(pathSplitter("").empty);

    // Root directories
    assert(equal2(pathSplitter("/"), ["/"]));
    assert(equal2(pathSplitter("//"), ["/"]));
    assert(equal2(pathSplitter("///"w), ["/"w]));

    // Absolute paths
    assert(equal2(pathSplitter("/foo/bar".dup), ["/", "foo", "bar"]));

    // General
    assert(equal2(pathSplitter("foo/bar"d.dup), ["foo"d, "bar"d]));
    assert(equal2(pathSplitter("foo//bar"), ["foo", "bar"]));
    assert(equal2(pathSplitter("foo/bar//"w), ["foo"w, "bar"w]));
    assert(equal2(pathSplitter("foo/../bar//./"d), ["foo"d, ".."d, "bar"d, "."d]));

    // save()
    auto ps1 = pathSplitter("foo/bar/baz");
    auto ps2 = ps1.save;
    ps1.popFront();
    assert(equal2(ps1, ["bar", "baz"]));
    assert(equal2(ps2, ["foo", "bar", "baz"]));

    // Platform specific
    version (Posix)
    {
        assert(equal2(pathSplitter("//foo/bar"w.dup), ["/"w, "foo"w, "bar"w]));
    }
    version (Windows)
    {
        assert(equal2(pathSplitter(`\`), [`\`]));
        assert(equal2(pathSplitter(`foo\..\bar\/.\`), ["foo", "..", "bar", "."]));
        assert(equal2(pathSplitter("c:"), ["c:"]));
        assert(equal2(pathSplitter(`c:\foo\bar`), [`c:\`, "foo", "bar"]));
        assert(equal2(pathSplitter(`c:foo\bar`), ["c:foo", "bar"]));
        assert(equal2(pathSplitter(`\\foo\bar`), [`\\foo\bar`]));
        assert(equal2(pathSplitter(`\\foo\bar\\`), [`\\foo\bar`]));
        assert(equal2(pathSplitter(`\\foo\bar\baz`), [`\\foo\bar`, "baz"]));
    }

    import std.exception;
    assertCTFEable!(
    {
        assert(equal(pathSplitter("/foo/bar".dup), ["/", "foo", "bar"]));
    });

    static assert(is(typeof(pathSplitter!(const(char)[])(null).front) == const(char)[]));

    import std.utf : byDchar;
    assert(equal2(pathSplitter("foo/bar"d.byDchar), ["foo"d, "bar"d]));
}




/** Determines whether a path starts at a root directory.

Params:
    path = A path name.
Returns:
    Whether a path starts at a root directory.

    On POSIX, this function returns true if and only if the path starts
    with a slash (/).

    On Windows, this function returns true if the path starts at
    the root directory of the current drive, of some other drive,
    or of a network drive.
*/
bool isRooted(R)(R path)
if (isRandomAccessRange!R && isSomeChar!(ElementType!R) ||
    is(StringTypeOf!R))
{
    if (path.length >= 1 && isDirSeparator(path[0])) return true;
    version (Posix)         return false;
    else version (Windows)  return isAbsolute!(BaseOf!R)(path);
}

///
@safe unittest
{
    version (Posix)
    {
        assert( isRooted("/"));
        assert( isRooted("/foo"));
        assert(!isRooted("foo"));
        assert(!isRooted("../foo"));
    }

    version (Windows)
    {
        assert( isRooted(`\`));
        assert( isRooted(`\foo`));
        assert( isRooted(`d:\foo`));
        assert( isRooted(`\\foo\bar`));
        assert(!isRooted("foo"));
        assert(!isRooted("d:foo"));
    }
}

@safe unittest
{
    assert(isRooted("/"));
    assert(isRooted("/foo"));
    assert(!isRooted("foo"));
    assert(!isRooted("../foo"));

    version (Windows)
    {
    assert(isRooted(`\`));
    assert(isRooted(`\foo`));
    assert(isRooted(`d:\foo`));
    assert(isRooted(`\\foo\bar`));
    assert(!isRooted("foo"));
    assert(!isRooted("d:foo"));
    }

    static assert(isRooted("/foo"));
    static assert(!isRooted("foo"));

    static struct DirEntry { string s; alias s this; }
    assert(!isRooted(DirEntry("foo")));
}

/** Determines whether a path is absolute or not.

    Params: path = A path name.

    Returns: Whether a path is absolute or not.

    Example:
    On POSIX, an absolute path starts at the root directory.
    (In fact, `_isAbsolute` is just an alias for $(LREF isRooted).)
    ---
    version (Posix)
    {
        assert(isAbsolute("/"));
        assert(isAbsolute("/foo"));
        assert(!isAbsolute("foo"));
        assert(!isAbsolute("../foo"));
    }
    ---

    On Windows, an absolute path starts at the root directory of
    a specific drive.  Hence, it must start with $(D `d:\`) or $(D `d:/`),
    where `d` is the drive letter.  Alternatively, it may be a
    network path, i.e. a path starting with a double (back)slash.
    ---
    version (Windows)
    {
        assert(isAbsolute(`d:\`));
        assert(isAbsolute(`d:\foo`));
        assert(isAbsolute(`\\foo\bar`));
        assert(!isAbsolute(`\`));
        assert(!isAbsolute(`\foo`));
        assert(!isAbsolute("d:foo"));
    }
    ---
*/
version (StdDdoc)
{
    bool isAbsolute(R)(R path) pure nothrow @safe
    if (isRandomAccessRange!R && isSomeChar!(ElementType!R) ||
        is(StringTypeOf!R));
}
else version (Windows)
{
    bool isAbsolute(R)(R path)
    if (isRandomAccessRange!R && isSomeChar!(ElementType!R) ||
        is(StringTypeOf!R))
    {
        return isDriveRoot!(BaseOf!R)(path) || isUNC!(BaseOf!R)(path);
    }
}
else version (Posix)
{
    alias isAbsolute = isRooted;
}


@safe unittest
{
    assert(!isAbsolute("foo"));
    assert(!isAbsolute("../foo"w));
    static assert(!isAbsolute("foo"));

    version (Posix)
    {
    assert(isAbsolute("/"d));
    assert(isAbsolute("/foo".dup));
    static assert(isAbsolute("/foo"));
    }

    version (Windows)
    {
    assert(isAbsolute("d:\\"w));
    assert(isAbsolute("d:\\foo"d));
    assert(isAbsolute("\\\\foo\\bar"));
    assert(!isAbsolute("\\"w.dup));
    assert(!isAbsolute("\\foo"d.dup));
    assert(!isAbsolute("d:"));
    assert(!isAbsolute("d:foo"));
    static assert(isAbsolute(`d:\foo`));
    }

    {
        auto r = MockRange!(immutable(char))(`../foo`);
        assert(!r.isAbsolute());
    }

    static struct DirEntry { string s; alias s this; }
    assert(!isAbsolute(DirEntry("foo")));
}




/** Transforms `path` into an absolute path.

    The following algorithm is used:
    $(OL
        $(LI If `path` is empty, return `null`.)
        $(LI If `path` is already absolute, return it.)
        $(LI Otherwise, append `path` to `base` and return
            the result. If `base` is not specified, the current
            working directory is used.)
    )
    The function allocates memory if and only if it gets to the third stage
    of this algorithm.

    Note that `absolutePath` will not normalize `..` segments.
    Use `buildNormalizedPath(absolutePath(path))` if that is desired.

    Params:
        path = the relative path to transform
        base = the base directory of the relative path

    Returns:
        string of transformed path

    Throws:
    `Exception` if the specified _base directory is not absolute.

    See_Also:
        $(LREF asAbsolutePath) which does not allocate
*/
string absolutePath(return scope const string path, lazy string base = getcwd())
    @safe pure
{
    import std.array : array;
    if (path.empty)  return null;
    if (isAbsolute(path))  return path;
    auto baseVar = base;
    if (!isAbsolute(baseVar)) throw new Exception("Base directory must be absolute");
    return chainPath(baseVar, path).array;
}

///
@safe unittest
{
    version (Posix)
    {
        assert(absolutePath("some/file", "/foo/bar")  == "/foo/bar/some/file");
        assert(absolutePath("../file", "/foo/bar")    == "/foo/bar/../file");
        assert(absolutePath("/some/file", "/foo/bar") == "/some/file");
    }

    version (Windows)
    {
        assert(absolutePath(`some\file`, `c:\foo\bar`)    == `c:\foo\bar\some\file`);
        assert(absolutePath(`..\file`, `c:\foo\bar`)      == `c:\foo\bar\..\file`);
        assert(absolutePath(`c:\some\file`, `c:\foo\bar`) == `c:\some\file`);
        assert(absolutePath(`\`, `c:\`)                   == `c:\`);
        assert(absolutePath(`\some\file`, `c:\foo\bar`)   == `c:\some\file`);
    }
}

@safe unittest
{
    version (Posix)
    {
        static assert(absolutePath("some/file", "/foo/bar") == "/foo/bar/some/file");
    }

    version (Windows)
    {
        static assert(absolutePath(`some\file`, `c:\foo\bar`) == `c:\foo\bar\some\file`);
    }

    import std.exception;
    assertThrown(absolutePath("bar", "foo"));
}

// Ensure that we can call absolute path with scope paramaters
@safe unittest
{
    string testAbsPath(scope const string path, scope const string base) {
        return absolutePath(path, base);
    }

    version (Posix)
        assert(testAbsPath("some/file", "/foo/bar")  == "/foo/bar/some/file");
    version (Windows)
        assert(testAbsPath(`some\file`, `c:\foo\bar`)    == `c:\foo\bar\some\file`);
}

/** Transforms `path` into an absolute path.

    The following algorithm is used:
    $(OL
        $(LI If `path` is empty, return `null`.)
        $(LI If `path` is already absolute, return it.)
        $(LI Otherwise, append `path` to the current working directory,
        which allocates memory.)
    )

    Note that `asAbsolutePath` will not normalize `..` segments.
    Use `asNormalizedPath(asAbsolutePath(path))` if that is desired.

    Params:
        path = the relative path to transform

    Returns:
        the transformed path as a lazy range

    See_Also:
        $(LREF absolutePath) which returns an allocated string
*/
auto asAbsolutePath(R)(R path)
if ((isRandomAccessRange!R && isSomeChar!(ElementType!R) ||
    isNarrowString!R) &&
    !isConvertibleToString!R)
{
    import std.file : getcwd;
    string base = null;
    if (!path.empty && !isAbsolute(path))
        base = getcwd();
    return chainPath(base, path);
}

///
@system unittest
{
    import std.array;
    assert(asAbsolutePath(cast(string) null).array == "");
    version (Posix)
    {
        assert(asAbsolutePath("/foo").array == "/foo");
    }
    version (Windows)
    {
        assert(asAbsolutePath("c:/foo").array == "c:/foo");
    }
    asAbsolutePath("foo");
}

auto asAbsolutePath(R)(auto ref R path)
if (isConvertibleToString!R)
{
    return asAbsolutePath!(StringTypeOf!R)(path);
}

@system unittest
{
    assert(testAliasedString!asAbsolutePath(null));
}

/** Translates `path` into a relative path.

    The returned path is relative to `base`, which is by default
    taken to be the current working directory.  If specified,
    `base` must be an absolute path, and it is always assumed
    to refer to a directory.  If `path` and `base` refer to
    the same directory, the function returns $(D `.`).

    The following algorithm is used:
    $(OL
        $(LI If `path` is a relative directory, return it unaltered.)
        $(LI Find a common root between `path` and `base`.
            If there is no common root, return `path` unaltered.)
        $(LI Prepare a string with as many $(D `../`) or $(D `..\`) as
            necessary to reach the common root from base path.)
        $(LI Append the remaining segments of `path` to the string
            and return.)
    )

    In the second step, path components are compared using `filenameCmp!cs`,
    where `cs` is an optional template parameter determining whether
    the comparison is case sensitive or not.  See the
    $(LREF filenameCmp) documentation for details.

    This function allocates memory.

    Params:
        cs = Whether matching path name components against the base path should
            be case-sensitive or not.
        path = A path name.
        base = The base path to construct the relative path from.

    Returns: The relative path.

    See_Also:
        $(LREF asRelativePath) which does not allocate memory

    Throws:
    `Exception` if the specified _base directory is not absolute.
*/
string relativePath(CaseSensitive cs = CaseSensitive.osDefault)
    (string path, lazy string base = getcwd())
{
    if (!isAbsolute(path))
        return path;
    auto baseVar = base;
    if (!isAbsolute(baseVar))
        throw new Exception("Base directory must be absolute");

    import std.conv : to;
    return asRelativePath!cs(path, baseVar).to!string;
}

///
@safe unittest
{
    assert(relativePath("foo") == "foo");

    version (Posix)
    {
        assert(relativePath("foo", "/bar") == "foo");
        assert(relativePath("/foo/bar", "/foo/bar") == ".");
        assert(relativePath("/foo/bar", "/foo/baz") == "../bar");
        assert(relativePath("/foo/bar/baz", "/foo/woo/wee") == "../../bar/baz");
        assert(relativePath("/foo/bar/baz", "/foo/bar") == "baz");
    }
    version (Windows)
    {
        assert(relativePath("foo", `c:\bar`) == "foo");
        assert(relativePath(`c:\foo\bar`, `c:\foo\bar`) == ".");
        assert(relativePath(`c:\foo\bar`, `c:\foo\baz`) == `..\bar`);
        assert(relativePath(`c:\foo\bar\baz`, `c:\foo\woo\wee`) == `..\..\bar\baz`);
        assert(relativePath(`c:\foo\bar\baz`, `c:\foo\bar`) == "baz");
        assert(relativePath(`c:\foo\bar`, `d:\foo`) == `c:\foo\bar`);
    }
}

@safe unittest
{
    import std.exception;
    assert(relativePath("foo") == "foo");
    version (Posix)
    {
        relativePath("/foo");
        assert(relativePath("/foo/bar", "/foo/baz") == "../bar");
        assertThrown(relativePath("/foo", "bar"));
    }
    else version (Windows)
    {
        relativePath(`\foo`);
        assert(relativePath(`c:\foo\bar\baz`, `c:\foo\bar`) == "baz");
        assertThrown(relativePath(`c:\foo`, "bar"));
    }
    else static assert(0);
}

/** Transforms `path` into a path relative to `base`.

    The returned path is relative to `base`, which is usually
    the current working directory.
    `base` must be an absolute path, and it is always assumed
    to refer to a directory.  If `path` and `base` refer to
    the same directory, the function returns `'.'`.

    The following algorithm is used:
    $(OL
        $(LI If `path` is a relative directory, return it unaltered.)
        $(LI Find a common root between `path` and `base`.
            If there is no common root, return `path` unaltered.)
        $(LI Prepare a string with as many `../` or `..\` as
            necessary to reach the common root from base path.)
        $(LI Append the remaining segments of `path` to the string
            and return.)
    )

    In the second step, path components are compared using `filenameCmp!cs`,
    where `cs` is an optional template parameter determining whether
    the comparison is case sensitive or not.  See the
    $(LREF filenameCmp) documentation for details.

    Params:
        path = path to transform
        base = absolute path
        cs = whether filespec comparisons are sensitive or not; defaults to
         `CaseSensitive.osDefault`

    Returns:
        a random access range of the transformed path

    See_Also:
        $(LREF relativePath)
*/
auto asRelativePath(CaseSensitive cs = CaseSensitive.osDefault, R1, R2)
    (R1 path, R2 base)
if ((isNarrowString!R1 ||
    (isRandomAccessRange!R1 && hasSlicing!R1 && isSomeChar!(ElementType!R1)) &&
    !isConvertibleToString!R1) &&
    (isNarrowString!R2 ||
    (isRandomAccessRange!R2 && hasSlicing!R2 && isSomeChar!(ElementType!R2)) &&
    !isConvertibleToString!R2))
{
    bool choosePath = !isAbsolute(path);

    // Find common root with current working directory

    auto basePS = pathSplitter(base);
    auto pathPS = pathSplitter(path);
    choosePath |= filenameCmp!cs(basePS.front, pathPS.front) != 0;

    basePS.popFront();
    pathPS.popFront();

    import std.algorithm.comparison : mismatch;
    import std.algorithm.iteration : joiner;
    import std.array : array;
    import std.range.primitives : walkLength;
    import std.range : repeat, chain, choose;
    import std.utf : byCodeUnit, byChar;

    // Remove matching prefix from basePS and pathPS
    auto tup = mismatch!((a, b) => filenameCmp!cs(a, b) == 0)(basePS, pathPS);
    basePS = tup[0];
    pathPS = tup[1];

    string sep;
    if (basePS.empty && pathPS.empty)
        sep = ".";              // if base == path, this is the return
    else if (!basePS.empty && !pathPS.empty)
        sep = dirSeparator;

    // Append as many "../" as necessary to reach common base from path
    auto r1 = ".."
        .byChar
        .repeat(basePS.walkLength())
        .joiner(dirSeparator.byChar);

    auto r2 = pathPS
        .joiner(dirSeparator.byChar)
        .byChar;

    // Return (r1 ~ sep ~ r2)
    return choose(choosePath, path.byCodeUnit, chain(r1, sep.byChar, r2));
}

///
@safe unittest
{
    import std.array;
    version (Posix)
    {
        assert(asRelativePath("foo", "/bar").array == "foo");
        assert(asRelativePath("/foo/bar", "/foo/bar").array == ".");
        assert(asRelativePath("/foo/bar", "/foo/baz").array == "../bar");
        assert(asRelativePath("/foo/bar/baz", "/foo/woo/wee").array == "../../bar/baz");
        assert(asRelativePath("/foo/bar/baz", "/foo/bar").array == "baz");
    }
    else version (Windows)
    {
        assert(asRelativePath("foo", `c:\bar`).array == "foo");
        assert(asRelativePath(`c:\foo\bar`, `c:\foo\bar`).array == ".");
        assert(asRelativePath(`c:\foo\bar`, `c:\foo\baz`).array == `..\bar`);
        assert(asRelativePath(`c:\foo\bar\baz`, `c:\foo\woo\wee`).array == `..\..\bar\baz`);
        assert(asRelativePath(`c:/foo/bar/baz`, `c:\foo\woo\wee`).array == `..\..\bar\baz`);
        assert(asRelativePath(`c:\foo\bar\baz`, `c:\foo\bar`).array == "baz");
        assert(asRelativePath(`c:\foo\bar`, `d:\foo`).array == `c:\foo\bar`);
        assert(asRelativePath(`\\foo\bar`, `c:\foo`).array == `\\foo\bar`);
    }
    else
        static assert(0);
}

@safe unittest
{
    version (Posix)
    {
        assert(isBidirectionalRange!(typeof(asRelativePath("foo/bar/baz", "/foo/woo/wee"))));
    }

    version (Windows)
    {
        assert(isBidirectionalRange!(typeof(asRelativePath(`c:\foo\bar`, `c:\foo\baz`))));
    }
}

auto asRelativePath(CaseSensitive cs = CaseSensitive.osDefault, R1, R2)
    (auto ref R1 path, auto ref R2 base)
if (isConvertibleToString!R1 || isConvertibleToString!R2)
{
    import std.meta : staticMap;
    alias Types = staticMap!(convertToString, R1, R2);
    return asRelativePath!(cs, Types)(path, base);
}

@safe unittest
{
    import std.array;
    version (Posix)
        assert(asRelativePath(TestAliasedString("foo"), TestAliasedString("/bar")).array == "foo");
    else version (Windows)
        assert(asRelativePath(TestAliasedString("foo"), TestAliasedString(`c:\bar`)).array == "foo");
    assert(asRelativePath(TestAliasedString("foo"), "bar").array == "foo");
    assert(asRelativePath("foo", TestAliasedString("bar")).array == "foo");
    assert(asRelativePath(TestAliasedString("foo"), TestAliasedString("bar")).array == "foo");
    import std.utf : byDchar;
    assert(asRelativePath("foo"d.byDchar, TestAliasedString("bar")).array == "foo");
}

@safe unittest
{
    import std.array, std.utf : bCU=byCodeUnit;
    version (Posix)
    {
        assert(asRelativePath("/foo/bar/baz".bCU, "/foo/bar".bCU).array == "baz");
        assert(asRelativePath("/foo/bar/baz"w.bCU, "/foo/bar"w.bCU).array == "baz"w);
        assert(asRelativePath("/foo/bar/baz"d.bCU, "/foo/bar"d.bCU).array == "baz"d);
    }
    else version (Windows)
    {
        assert(asRelativePath(`\\foo\bar`.bCU, `c:\foo`.bCU).array == `\\foo\bar`);
        assert(asRelativePath(`\\foo\bar`w.bCU, `c:\foo`w.bCU).array == `\\foo\bar`w);
        assert(asRelativePath(`\\foo\bar`d.bCU, `c:\foo`d.bCU).array == `\\foo\bar`d);
    }
}

/** Compares filename characters.

    This function can perform a case-sensitive or a case-insensitive
    comparison.  This is controlled through the `cs` template parameter
    which, if not specified, is given by $(LREF CaseSensitive)`.osDefault`.

    On Windows, the backslash and slash characters ($(D `\`) and $(D `/`))
    are considered equal.

    Params:
        cs = Case-sensitivity of the comparison.
        a = A filename character.
        b = A filename character.

    Returns:
        $(D < 0) if $(D a < b),
        `0` if $(D a == b), and
        $(D > 0) if $(D a > b).
*/
int filenameCharCmp(CaseSensitive cs = CaseSensitive.osDefault)(dchar a, dchar b)
    @safe pure nothrow
{
    if (isDirSeparator(a) && isDirSeparator(b)) return 0;
    static if (!cs)
    {
        import std.uni : toLower;
        a = toLower(a);
        b = toLower(b);
    }
    return cast(int)(a - b);
}

///
@safe unittest
{
    assert(filenameCharCmp('a', 'a') == 0);
    assert(filenameCharCmp('a', 'b') < 0);
    assert(filenameCharCmp('b', 'a') > 0);

    version (linux)
    {
        // Same as calling filenameCharCmp!(CaseSensitive.yes)(a, b)
        assert(filenameCharCmp('A', 'a') < 0);
        assert(filenameCharCmp('a', 'A') > 0);
    }
    version (Windows)
    {
        // Same as calling filenameCharCmp!(CaseSensitive.no)(a, b)
        assert(filenameCharCmp('a', 'A') == 0);
        assert(filenameCharCmp('a', 'B') < 0);
        assert(filenameCharCmp('A', 'b') < 0);
    }
}

@safe unittest
{
    assert(filenameCharCmp!(CaseSensitive.yes)('A', 'a') < 0);
    assert(filenameCharCmp!(CaseSensitive.yes)('a', 'A') > 0);

    assert(filenameCharCmp!(CaseSensitive.no)('a', 'a') == 0);
    assert(filenameCharCmp!(CaseSensitive.no)('a', 'b') < 0);
    assert(filenameCharCmp!(CaseSensitive.no)('b', 'a') > 0);
    assert(filenameCharCmp!(CaseSensitive.no)('A', 'a') == 0);
    assert(filenameCharCmp!(CaseSensitive.no)('a', 'A') == 0);
    assert(filenameCharCmp!(CaseSensitive.no)('a', 'B') < 0);
    assert(filenameCharCmp!(CaseSensitive.no)('B', 'a') > 0);
    assert(filenameCharCmp!(CaseSensitive.no)('A', 'b') < 0);
    assert(filenameCharCmp!(CaseSensitive.no)('b', 'A') > 0);

    version (Posix)   assert(filenameCharCmp('\\', '/') != 0);
    version (Windows) assert(filenameCharCmp('\\', '/') == 0);
}


/** Compares file names and returns

    Individual characters are compared using `filenameCharCmp!cs`,
    where `cs` is an optional template parameter determining whether
    the comparison is case sensitive or not.

    Treatment of invalid UTF encodings is implementation defined.

    Params:
        cs = case sensitivity
        filename1 = range for first file name
        filename2 = range for second file name

    Returns:
        $(D < 0) if $(D filename1 < filename2),
        `0` if $(D filename1 == filename2) and
        $(D > 0) if $(D filename1 > filename2).

    See_Also:
        $(LREF filenameCharCmp)
*/
int filenameCmp(CaseSensitive cs = CaseSensitive.osDefault, Range1, Range2)
    (Range1 filename1, Range2 filename2)
if (isSomeFiniteCharInputRange!Range1 && !isConvertibleToString!Range1 &&
    isSomeFiniteCharInputRange!Range2 && !isConvertibleToString!Range2)
{
    alias C1 = Unqual!(ElementEncodingType!Range1);
    alias C2 = Unqual!(ElementEncodingType!Range2);

    static if (!cs && (C1.sizeof < 4 || C2.sizeof < 4) ||
               C1.sizeof != C2.sizeof)
    {
        // Case insensitive - decode so case is checkable
        // Different char sizes - decode to bring to common type
        import std.utf : byDchar;
        return filenameCmp!cs(filename1.byDchar, filename2.byDchar);
    }
    else static if (isSomeString!Range1 && C1.sizeof < 4 ||
                    isSomeString!Range2 && C2.sizeof < 4)
    {
        // Avoid autodecoding
        import std.utf : byCodeUnit;
        return filenameCmp!cs(filename1.byCodeUnit, filename2.byCodeUnit);
    }
    else
    {
        for (;;)
        {
            if (filename1.empty) return -(cast(int) !filename2.empty);
            if (filename2.empty) return  1;
            const c = filenameCharCmp!cs(filename1.front, filename2.front);
            if (c != 0) return c;
            filename1.popFront();
            filename2.popFront();
        }
    }
}

///
@safe unittest
{
    assert(filenameCmp("abc", "abc") == 0);
    assert(filenameCmp("abc", "abd") < 0);
    assert(filenameCmp("abc", "abb") > 0);
    assert(filenameCmp("abc", "abcd") < 0);
    assert(filenameCmp("abcd", "abc") > 0);

    version (linux)
    {
        // Same as calling filenameCmp!(CaseSensitive.yes)(filename1, filename2)
        assert(filenameCmp("Abc", "abc") < 0);
        assert(filenameCmp("abc", "Abc") > 0);
    }
    version (Windows)
    {
        // Same as calling filenameCmp!(CaseSensitive.no)(filename1, filename2)
        assert(filenameCmp("Abc", "abc") == 0);
        assert(filenameCmp("abc", "Abc") == 0);
        assert(filenameCmp("Abc", "abD") < 0);
        assert(filenameCmp("abc", "AbB") > 0);
    }
}

int filenameCmp(CaseSensitive cs = CaseSensitive.osDefault, Range1, Range2)
    (auto ref Range1 filename1, auto ref Range2 filename2)
if (isConvertibleToString!Range1 || isConvertibleToString!Range2)
{
    import std.meta : staticMap;
    alias Types = staticMap!(convertToString, Range1, Range2);
    return filenameCmp!(cs, Types)(filename1, filename2);
}

@safe unittest
{
    assert(filenameCmp!(CaseSensitive.yes)(TestAliasedString("Abc"), "abc") < 0);
    assert(filenameCmp!(CaseSensitive.yes)("Abc", TestAliasedString("abc")) < 0);
    assert(filenameCmp!(CaseSensitive.yes)(TestAliasedString("Abc"), TestAliasedString("abc")) < 0);
}

@safe unittest
{
    assert(filenameCmp!(CaseSensitive.yes)("Abc", "abc") < 0);
    assert(filenameCmp!(CaseSensitive.yes)("abc", "Abc") > 0);

    assert(filenameCmp!(CaseSensitive.no)("abc", "abc") == 0);
    assert(filenameCmp!(CaseSensitive.no)("abc", "abd") < 0);
    assert(filenameCmp!(CaseSensitive.no)("abc", "abb") > 0);
    assert(filenameCmp!(CaseSensitive.no)("abc", "abcd") < 0);
    assert(filenameCmp!(CaseSensitive.no)("abcd", "abc") > 0);
    assert(filenameCmp!(CaseSensitive.no)("Abc", "abc") == 0);
    assert(filenameCmp!(CaseSensitive.no)("abc", "Abc") == 0);
    assert(filenameCmp!(CaseSensitive.no)("Abc", "abD") < 0);
    assert(filenameCmp!(CaseSensitive.no)("abc", "AbB") > 0);

    version (Posix)   assert(filenameCmp(`abc\def`, `abc/def`) != 0);
    version (Windows) assert(filenameCmp(`abc\def`, `abc/def`) == 0);
}

/** Matches a pattern against a path.

    Some characters of pattern have a special meaning (they are
    $(I meta-characters)) and can't be escaped. These are:

    $(BOOKTABLE,
    $(TR $(TD `*`)
         $(TD Matches 0 or more instances of any character.))
    $(TR $(TD `?`)
         $(TD Matches exactly one instance of any character.))
    $(TR $(TD `[`$(I chars)`]`)
         $(TD Matches one instance of any character that appears
              between the brackets.))
    $(TR $(TD `[!`$(I chars)`]`)
         $(TD Matches one instance of any character that does not
              appear between the brackets after the exclamation mark.))
    $(TR $(TD `{`$(I string1)`,`$(I string2)`,`&hellip;`}`)
         $(TD Matches either of the specified strings.))
    )

    Individual characters are compared using `filenameCharCmp!cs`,
    where `cs` is an optional template parameter determining whether
    the comparison is case sensitive or not.  See the
    $(LREF filenameCharCmp) documentation for details.

    Note that directory
    separators and dots don't stop a meta-character from matching
    further portions of the path.

    Params:
        cs = Whether the matching should be case-sensitive
        path = The path to be matched against
        pattern = The glob pattern

    Returns:
    `true` if pattern matches path, `false` otherwise.

    See_also:
    $(LINK2 http://en.wikipedia.org/wiki/Glob_%28programming%29,Wikipedia: _glob (programming))
 */
bool globMatch(CaseSensitive cs = CaseSensitive.osDefault, C, Range)
    (Range path, const(C)[] pattern)
    @safe pure nothrow
if (isForwardRange!Range && !isInfinite!Range &&
    isSomeChar!(ElementEncodingType!Range) && !isConvertibleToString!Range &&
    isSomeChar!C && is(immutable C == immutable ElementEncodingType!Range))
in
{
    // Verify that pattern[] is valid
    import std.algorithm.searching : balancedParens;
    import std.utf : byUTF;

    assert(balancedParens(pattern.byUTF!C, '[', ']', 0));
    assert(balancedParens(pattern.byUTF!C, '{', '}', 0));
}
do
{
    alias RC = Unqual!(ElementEncodingType!Range);

    static if (RC.sizeof == 1 && isSomeString!Range)
    {
        import std.utf : byChar;
        return globMatch!cs(path.byChar, pattern);
    }
    else static if (RC.sizeof == 2 && isSomeString!Range)
    {
        import std.utf : byWchar;
        return globMatch!cs(path.byWchar, pattern);
    }
    else
    {
        import core.memory : pureMalloc, pureFree;
        C[] pattmp;
        scope(exit) if (pattmp !is null) (() @trusted => pureFree(pattmp.ptr))();

        for (size_t pi = 0; pi < pattern.length; pi++)
        {
            const pc = pattern[pi];
            switch (pc)
            {
                case '*':
                    if (pi + 1 == pattern.length)
                        return true;
                    for (; !path.empty; path.popFront())
                    {
                        auto p = path.save;
                        if (globMatch!(cs, C)(p,
                                        pattern[pi + 1 .. pattern.length]))
                            return true;
                    }
                    return false;

                case '?':
                    if (path.empty)
                        return false;
                    path.popFront();
                    break;

                case '[':
                    if (path.empty)
                        return false;
                    auto nc = path.front;
                    path.popFront();
                    auto not = false;
                    ++pi;
                    if (pattern[pi] == '!')
                    {
                        not = true;
                        ++pi;
                    }
                    auto anymatch = false;
                    while (1)
                    {
                        const pc2 = pattern[pi];
                        if (pc2 == ']')
                            break;
                        if (!anymatch && (filenameCharCmp!cs(nc, pc2) == 0))
                            anymatch = true;
                        ++pi;
                    }
                    if (anymatch == not)
                        return false;
                    break;

                case '{':
                    // find end of {} section
                    auto piRemain = pi;
                    for (; piRemain < pattern.length
                             && pattern[piRemain] != '}'; ++piRemain)
                    {   }

                    if (piRemain < pattern.length)
                        ++piRemain;
                    ++pi;

                    while (pi < pattern.length)
                    {
                        const pi0 = pi;
                        C pc3 = pattern[pi];
                        // find end of current alternative
                        for (; pi < pattern.length && pc3 != '}' && pc3 != ','; ++pi)
                        {
                            pc3 = pattern[pi];
                        }

                        auto p = path.save;
                        if (pi0 == pi)
                        {
                            if (globMatch!(cs, C)(p, pattern[piRemain..$]))
                            {
                                return true;
                            }
                            ++pi;
                        }
                        else
                        {
                            /* Match for:
                             *   pattern[pi0 .. pi-1] ~ pattern[piRemain..$]
                             */
                            if (pattmp is null)
                            {
                                // Allocate this only once per function invocation.
                                pattmp = (() @trusted =>
                                    (cast(C*) pureMalloc(C.sizeof * pattern.length))[0 .. pattern.length])
                                ();
                            }

                            const len1 = pi - 1 - pi0;
                            pattmp[0 .. len1] = pattern[pi0 .. pi - 1];

                            const len2 = pattern.length - piRemain;
                            pattmp[len1 .. len1 + len2] = pattern[piRemain .. $];

                            if (globMatch!(cs, C)(p, pattmp[0 .. len1 + len2]))
                            {
                                return true;
                            }
                        }
                        if (pc3 == '}')
                        {
                            break;
                        }
                    }
                    return false;

                default:
                    if (path.empty)
                        return false;
                    if (filenameCharCmp!cs(pc, path.front) != 0)
                        return false;
                    path.popFront();
                    break;
            }
        }
        return path.empty;
    }
}

///
@safe @nogc unittest
{
    assert(globMatch("foo.bar", "*"));
    assert(globMatch("foo.bar", "*.*"));
    assert(globMatch(`foo/foo\bar`, "f*b*r"));
    assert(globMatch("foo.bar", "f???bar"));
    assert(globMatch("foo.bar", "[fg]???bar"));
    assert(globMatch("foo.bar", "[!gh]*bar"));
    assert(globMatch("bar.fooz", "bar.{foo,bif}z"));
    assert(globMatch("bar.bifz", "bar.{foo,bif}z"));

    version (Windows)
    {
        // Same as calling globMatch!(CaseSensitive.no)(path, pattern)
        assert(globMatch("foo", "Foo"));
        assert(globMatch("Goo.bar", "[fg]???bar"));
    }
    version (linux)
    {
        // Same as calling globMatch!(CaseSensitive.yes)(path, pattern)
        assert(!globMatch("foo", "Foo"));
        assert(!globMatch("Goo.bar", "[fg]???bar"));
    }
}

bool globMatch(CaseSensitive cs = CaseSensitive.osDefault, C, Range)
    (auto ref Range path, const(C)[] pattern)
    @safe pure nothrow
if (isConvertibleToString!Range)
{
    return globMatch!(cs, C, StringTypeOf!Range)(path, pattern);
}

@safe unittest
{
    assert(testAliasedString!globMatch("foo.bar", "*"));
}

@safe unittest
{
    assert(globMatch!(CaseSensitive.no)("foo", "Foo"));
    assert(!globMatch!(CaseSensitive.yes)("foo", "Foo"));

    assert(globMatch("foo", "*"));
    assert(globMatch("foo.bar"w, "*"w));
    assert(globMatch("foo.bar"d, "*.*"d));
    assert(globMatch("foo.bar", "foo*"));
    assert(globMatch("foo.bar"w, "f*bar"w));
    assert(globMatch("foo.bar"d, "f*b*r"d));
    assert(globMatch("foo.bar", "f???bar"));
    assert(globMatch("foo.bar"w, "[fg]???bar"w));
    assert(globMatch("foo.bar"d, "[!gh]*bar"d));

    assert(!globMatch("foo", "bar"));
    assert(!globMatch("foo"w, "*.*"w));
    assert(!globMatch("foo.bar"d, "f*baz"d));
    assert(!globMatch("foo.bar", "f*b*x"));
    assert(!globMatch("foo.bar", "[gh]???bar"));
    assert(!globMatch("foo.bar"w, "[!fg]*bar"w));
    assert(!globMatch("foo.bar"d, "[fg]???baz"d));
    // https://issues.dlang.org/show_bug.cgi?id=6634
    assert(!globMatch("foo.di", "*.d")); // triggered bad assertion

    assert(globMatch("foo.bar", "{foo,bif}.bar"));
    assert(globMatch("bif.bar"w, "{foo,bif}.bar"w));

    assert(globMatch("bar.foo"d, "bar.{foo,bif}"d));
    assert(globMatch("bar.bif", "bar.{foo,bif}"));

    assert(globMatch("bar.fooz"w, "bar.{foo,bif}z"w));
    assert(globMatch("bar.bifz"d, "bar.{foo,bif}z"d));

    assert(globMatch("bar.foo", "bar.{biz,,baz}foo"));
    assert(globMatch("bar.foo"w, "bar.{biz,}foo"w));
    assert(globMatch("bar.foo"d, "bar.{,biz}foo"d));
    assert(globMatch("bar.foo", "bar.{}foo"));

    assert(globMatch("bar.foo"w, "bar.{ar,,fo}o"w));
    assert(globMatch("bar.foo"d, "bar.{,ar,fo}o"d));
    assert(globMatch("bar.o", "bar.{,ar,fo}o"));

    assert(!globMatch("foo", "foo?"));
    assert(!globMatch("foo", "foo[]"));
    assert(!globMatch("foo", "foob"));
    assert(!globMatch("foo", "foo{b}"));


    static assert(globMatch("foo.bar", "[!gh]*bar"));
}




/** Checks that the given file or directory name is valid.

    The maximum length of `filename` is given by the constant
    `core.stdc.stdio.FILENAME_MAX`.  (On Windows, this number is
    defined as the maximum number of UTF-16 code points, and the
    test will therefore only yield strictly correct results when
    `filename` is a string of `wchar`s.)

    On Windows, the following criteria must be satisfied
    ($(LINK2 http://msdn.microsoft.com/en-us/library/aa365247(v=vs.85).aspx,source)):
    $(UL
        $(LI `filename` must not contain any characters whose integer
            representation is in the range 0-31.)
        $(LI `filename` must not contain any of the following $(I reserved
            characters): `<>:"/\|?*`)
        $(LI `filename` may not end with a space ($(D ' ')) or a period
            (`'.'`).)
    )

    On POSIX, `filename` may not contain a forward slash (`'/'`) or
    the null character (`'\0'`).

    Params:
        filename = string to check

    Returns:
        `true` if and only if `filename` is not
        empty, not too long, and does not contain invalid characters.

*/
bool isValidFilename(Range)(Range filename)
if ((isRandomAccessRange!Range && hasLength!Range && hasSlicing!Range && isSomeChar!(ElementEncodingType!Range) ||
    isNarrowString!Range) &&
    !isConvertibleToString!Range)
{
    import core.stdc.stdio : FILENAME_MAX;
    if (filename.length == 0 || filename.length >= FILENAME_MAX) return false;
    foreach (c; filename)
    {
        version (Windows)
        {
            switch (c)
            {
                case 0:
                ..
                case 31:
                case '<':
                case '>':
                case ':':
                case '"':
                case '/':
                case '\\':
                case '|':
                case '?':
                case '*':
                    return false;

                default:
                    break;
            }
        }
        else version (Posix)
        {
            if (c == 0 || c == '/') return false;
        }
        else static assert(0);
    }
    version (Windows)
    {
        auto last = filename[filename.length - 1];
        if (last == '.' || last == ' ') return false;
    }

    // All criteria passed
    return true;
}

///
@safe pure @nogc nothrow
unittest
{
    import std.utf : byCodeUnit;

    assert(isValidFilename("hello.exe".byCodeUnit));
}

bool isValidFilename(Range)(auto ref Range filename)
if (isConvertibleToString!Range)
{
    return isValidFilename!(StringTypeOf!Range)(filename);
}

@safe unittest
{
    assert(testAliasedString!isValidFilename("hello.exe"));
}

@safe pure
unittest
{
    import std.conv;
    auto valid = ["foo"];
    auto invalid = ["", "foo\0bar", "foo/bar"];
    auto pfdep = [`foo\bar`, "*.txt"];
    version (Windows) invalid ~= pfdep;
    else version (Posix) valid ~= pfdep;
    else static assert(0);

    import std.meta : AliasSeq;
    static foreach (T; AliasSeq!(char[], const(char)[], string, wchar[],
        const(wchar)[], wstring, dchar[], const(dchar)[], dstring))
    {
        foreach (fn; valid)
            assert(isValidFilename(to!T(fn)));
        foreach (fn; invalid)
            assert(!isValidFilename(to!T(fn)));
    }

    {
        auto r = MockRange!(immutable(char))(`dir/file.d`);
        assert(!isValidFilename(r));
    }

    static struct DirEntry { string s; alias s this; }
    assert(isValidFilename(DirEntry("file.ext")));

    version (Windows)
    {
        immutable string cases = "<>:\"/\\|?*";
        foreach (i; 0 .. 31 + cases.length)
        {
            char[3] buf;
            buf[0] = 'a';
            buf[1] = i <= 31 ? cast(char) i : cases[i - 32];
            buf[2] = 'b';
            assert(!isValidFilename(buf[]));
        }
    }
}



/** Checks whether `path` is a valid path.

    Generally, this function checks that `path` is not empty, and that
    each component of the path either satisfies $(LREF isValidFilename)
    or is equal to `"."` or `".."`.

    $(B It does $(I not) check whether the path points to an existing file
    or directory; use $(REF exists, std,file) for this purpose.)

    On Windows, some special rules apply:
    $(UL
        $(LI If the second character of `path` is a colon (`':'`),
            the first character is interpreted as a drive letter, and
            must be in the range A-Z (case insensitive).)
        $(LI If `path` is on the form $(D `\\$(I server)\$(I share)\...`)
            (UNC path), $(LREF isValidFilename) is applied to $(I server)
            and $(I share) as well.)
        $(LI If `path` starts with $(D `\\?\`) (long UNC path), the
            only requirement for the rest of the string is that it does
            not contain the null character.)
        $(LI If `path` starts with $(D `\\.\`) (Win32 device namespace)
            this function returns `false`; such paths are beyond the scope
            of this module.)
    )

    Params:
        path = string or Range of characters to check

    Returns:
        true if `path` is a valid path.
*/
bool isValidPath(Range)(Range path)
if ((isRandomAccessRange!Range && hasLength!Range && hasSlicing!Range && isSomeChar!(ElementEncodingType!Range) ||
    isNarrowString!Range) &&
    !isConvertibleToString!Range)
{
    alias C = Unqual!(ElementEncodingType!Range);

    if (path.empty) return false;

    // Check whether component is "." or "..", or whether it satisfies
    // isValidFilename.
    bool isValidComponent(Range component)
    {
        assert(component.length > 0);
        if (component[0] == '.')
        {
            if (component.length == 1) return true;
            else if (component.length == 2 && component[1] == '.') return true;
        }
        return isValidFilename(component);
    }

    if (path.length == 1)
        return isDirSeparator(path[0]) || isValidComponent(path);

    Range remainder;
    version (Windows)
    {
        if (isDirSeparator(path[0]) && isDirSeparator(path[1]))
        {
            // Some kind of UNC path
            if (path.length < 5)
            {
                // All valid UNC paths must have at least 5 characters
                return false;
            }
            else if (path[2] == '?')
            {
                // Long UNC path
                if (!isDirSeparator(path[3])) return false;
                foreach (c; path[4 .. $])
                {
                    if (c == '\0') return false;
                }
                return true;
            }
            else if (path[2] == '.')
            {
                // Win32 device namespace not supported
                return false;
            }
            else
            {
                // Normal UNC path, i.e. \\server\share\...
                size_t i = 2;
                while (i < path.length && !isDirSeparator(path[i])) ++i;
                if (i == path.length || !isValidFilename(path[2 .. i]))
                    return false;
                ++i; // Skip a single dir separator
                size_t j = i;
                while (j < path.length && !isDirSeparator(path[j])) ++j;
                if (!isValidFilename(path[i .. j])) return false;
                remainder = path[j .. $];
            }
        }
        else if (isDriveSeparator(path[1]))
        {
            import std.ascii : isAlpha;
            if (!isAlpha(path[0])) return false;
            remainder = path[2 .. $];
        }
        else
        {
            remainder = path;
        }
    }
    else version (Posix)
    {
        remainder = path;
    }
    else static assert(0);
    remainder = ltrimDirSeparators(remainder);

    // Check that each component satisfies isValidComponent.
    while (!remainder.empty)
    {
        size_t i = 0;
        while (i < remainder.length && !isDirSeparator(remainder[i])) ++i;
        assert(i > 0);
        if (!isValidComponent(remainder[0 .. i])) return false;
        remainder = ltrimDirSeparators(remainder[i .. $]);
    }

    // All criteria passed
    return true;
}

///
@safe pure @nogc nothrow
unittest
{
    assert(isValidPath("/foo/bar"));
    assert(!isValidPath("/foo\0/bar"));
    assert(isValidPath("/"));
    assert(isValidPath("a"));

    version (Windows)
    {
        assert(isValidPath(`c:\`));
        assert(isValidPath(`c:\foo`));
        assert(isValidPath(`c:\foo\.\bar\\\..\`));
        assert(!isValidPath(`!:\foo`));
        assert(!isValidPath(`c::\foo`));
        assert(!isValidPath(`c:\foo?`));
        assert(!isValidPath(`c:\foo.`));

        assert(isValidPath(`\\server\share`));
        assert(isValidPath(`\\server\share\foo`));
        assert(isValidPath(`\\server\share\\foo`));
        assert(!isValidPath(`\\\server\share\foo`));
        assert(!isValidPath(`\\server\\share\foo`));
        assert(!isValidPath(`\\ser*er\share\foo`));
        assert(!isValidPath(`\\server\sha?e\foo`));
        assert(!isValidPath(`\\server\share\|oo`));

        assert(isValidPath(`\\?\<>:"?*|/\..\.`));
        assert(!isValidPath("\\\\?\\foo\0bar"));

        assert(!isValidPath(`\\.\PhysicalDisk1`));
        assert(!isValidPath(`\\`));
    }

    import std.utf : byCodeUnit;
    assert(isValidPath("/foo/bar".byCodeUnit));
}

bool isValidPath(Range)(auto ref Range path)
if (isConvertibleToString!Range)
{
    return isValidPath!(StringTypeOf!Range)(path);
}

@safe unittest
{
    assert(testAliasedString!isValidPath("/foo/bar"));
}

/** Performs tilde expansion in paths on POSIX systems.
    On Windows, this function does nothing.

    There are two ways of using tilde expansion in a path. One
    involves using the tilde alone or followed by a path separator. In
    this case, the tilde will be expanded with the value of the
    environment variable `HOME`.  The second way is putting
    a username after the tilde (i.e. `~john/Mail`). Here,
    the username will be searched for in the user database
    (i.e. `/etc/passwd` on Unix systems) and will expand to
    whatever path is stored there.  The username is considered the
    string after the tilde ending at the first instance of a path
    separator.

    Note that using the `~user` syntax may give different
    values from just `~` if the environment variable doesn't
    match the value stored in the user database.

    When the environment variable version is used, the path won't
    be modified if the environment variable doesn't exist or it
    is empty. When the database version is used, the path won't be
    modified if the user doesn't exist in the database or there is
    not enough memory to perform the query.

    This function performs several memory allocations.

    Params:
        inputPath = The path name to expand.

    Returns:
    `inputPath` with the tilde expanded, or just `inputPath`
    if it could not be expanded.
    For Windows, `expandTilde` merely returns its argument `inputPath`.

    Example:
    -----
    void processFile(string path)
    {
        // Allow calling this function with paths such as ~/foo
        auto fullPath = expandTilde(path);
        ...
    }
    -----
*/
string expandTilde(return scope const string inputPath) @safe nothrow
{
    version (Posix)
    {
        import core.exception : onOutOfMemoryError;
        import core.stdc.errno : errno, EBADF, ENOENT, EPERM, ERANGE, ESRCH;
        import core.stdc.stdlib : malloc, free, realloc;

        /*  Joins a path from a C string to the remainder of path.

            The last path separator from c_path is discarded. The result
            is joined to path[char_pos .. length] if char_pos is smaller
            than length, otherwise path is not appended to c_path.
        */
        static string combineCPathWithDPath(char* c_path, string path, size_t char_pos) @trusted nothrow
        {
            import core.stdc.string : strlen;
            import std.exception : assumeUnique;

            assert(c_path != null);
            assert(path.length > 0);
            assert(char_pos >= 0);

            // Search end of C string
            size_t end = strlen(c_path);

            const cPathEndsWithDirSep = end && isDirSeparator(c_path[end - 1]);

            string cp;
            if (char_pos < path.length)
            {
                // Remove trailing path separator, if any (with special care for root /)
                if (cPathEndsWithDirSep && (end > 1 || isDirSeparator(path[char_pos])))
                    end--;

                // Append something from path
                cp = assumeUnique(c_path[0 .. end] ~ path[char_pos .. $]);
            }
            else
            {
                // Remove trailing path separator, if any (except for root /)
                if (cPathEndsWithDirSep && end > 1)
                    end--;

                // Create our own copy, as lifetime of c_path is undocumented
                cp = c_path[0 .. end].idup;
            }

            return cp;
        }

        // Replaces the tilde from path with the environment variable HOME.
        static string expandFromEnvironment(string path) @safe nothrow
        {
            import core.stdc.stdlib : getenv;

            assert(path.length >= 1);
            assert(path[0] == '~');

            // Get HOME and use that to replace the tilde.
            auto home = () @trusted { return getenv("HOME"); } ();
            if (home == null)
                return path;

            return combineCPathWithDPath(home, path, 1);
        }

        // Replaces the tilde from path with the path from the user database.
        static string expandFromDatabase(string path) @safe nothrow
        {
            // bionic doesn't really support this, as getpwnam_r
            // isn't provided and getpwnam is basically just a stub
            version (CRuntime_Bionic)
            {
                return path;
            }
            else
            {
                import core.sys.posix.pwd : passwd, getpwnam_r;
                import std.string : indexOf;

                assert(path.length > 2 || (path.length == 2 && !isDirSeparator(path[1])));
                assert(path[0] == '~');

                // Extract username, searching for path separator.
                auto last_char = indexOf(path, dirSeparator[0]);

                size_t username_len = (last_char == -1) ? path.length : last_char;
                char[] username = new char[username_len * char.sizeof];

                if (last_char == -1)
                {
                    username[0 .. username_len - 1] = path[1 .. $];
                    last_char = path.length + 1;
                }
                else
                {
                    username[0 .. username_len - 1] = path[1 .. last_char];
                }
                username[username_len - 1] = 0;

                assert(last_char > 1);

                // Reserve C memory for the getpwnam_r() function.
                version (StdUnittest)
                    uint extra_memory_size = 2;
                else
                    uint extra_memory_size = 5 * 1024;
                char[] extra_memory;

                passwd result;
                loop: while (1)
                {
                    extra_memory.length += extra_memory_size;

                    // Obtain info from database.
                    passwd *verify;
                    errno = 0;
                    auto passResult = () @trusted { return getpwnam_r(
                        &username[0],
                        &result,
                        &extra_memory[0],
                        extra_memory.length,
                        &verify
                    ); } ();
                    if (passResult == 0)
                    {
                        // Succeeded if verify points at result
                        if (verify == () @trusted { return &result; } ())
                            // username is found
                            path = combineCPathWithDPath(result.pw_dir, path, last_char);
                        break;
                    }

                    switch (errno)
                    {
                        case ERANGE:
                        // On BSD and OSX, errno can be left at 0 instead of set to ERANGE
                        case 0:
                            break;

                        case ENOENT:
                        case ESRCH:
                        case EBADF:
                        case EPERM:
                            // The given name or uid was not found.
                            break loop;

                        default:
                            onOutOfMemoryError();
                    }

                    // extra_memory isn't large enough
                    import core.checkedint : mulu;
                    bool overflow;
                    extra_memory_size = mulu(extra_memory_size, 2, overflow);
                    if (overflow) assert(0);
                }
                return path;
            }
        }

        // Return early if there is no tilde in path.
        if (inputPath.length < 1 || inputPath[0] != '~')
            return inputPath;

        if (inputPath.length == 1 || isDirSeparator(inputPath[1]))
            return expandFromEnvironment(inputPath);
        else
            return expandFromDatabase(inputPath);
    }
    else version (Windows)
    {
        // Put here real windows implementation.
        return inputPath;
    }
    else
    {
        static assert(0); // Guard. Implement on other platforms.
    }
}

///
@safe unittest
{
    version (Posix)
    {
        import std.process : environment;

        auto oldHome = environment["HOME"];
        scope(exit) environment["HOME"] = oldHome;

        environment["HOME"] = "dmd/test";
        assert(expandTilde("~/") == "dmd/test/");
        assert(expandTilde("~") == "dmd/test");
    }
}

@safe unittest
{
    version (Posix)
    {
        static if (__traits(compiles, { import std.process : executeShell; }))
            import std.process : executeShell;

        import std.process : environment;
        import std.string : strip;

        // Retrieve the current home variable.
        auto oldHome = environment.get("HOME");

        // Testing when there is no environment variable.
        environment.remove("HOME");
        assert(expandTilde("~/") == "~/");
        assert(expandTilde("~") == "~");

        // Testing when an environment variable is set.
        environment["HOME"] = "dmd/test";
        assert(expandTilde("~/") == "dmd/test/");
        assert(expandTilde("~") == "dmd/test");

        // The same, but with a variable ending in a slash.
        environment["HOME"] = "dmd/test/";
        assert(expandTilde("~/") == "dmd/test/");
        assert(expandTilde("~") == "dmd/test");

        // The same, but with a variable set to root.
        environment["HOME"] = "/";
        assert(expandTilde("~/") == "/");
        assert(expandTilde("~") == "/");

        // Recover original HOME variable before continuing.
        if (oldHome !is null) environment["HOME"] = oldHome;
        else environment.remove("HOME");

        static if (is(typeof(executeShell)))
        {
            immutable tildeUser = "~" ~ environment.get("USER");
            immutable path = executeShell("echo " ~ tildeUser).output.strip();
            immutable expTildeUser = expandTilde(tildeUser);
            assert(expTildeUser == path, expTildeUser);
            immutable expTildeUserSlash = expandTilde(tildeUser ~ "/");
            immutable pathSlash = path[$-1] == '/' ? path : path ~ "/";
            assert(expTildeUserSlash == pathSlash, expTildeUserSlash);
        }

        assert(expandTilde("~Idontexist/hey") == "~Idontexist/hey");
    }
}

@safe unittest
{
    version (Posix)
    {
        import std.process : environment;

        string testPath(scope const string source_path) {
            return source_path.expandTilde;
        }

        auto oldHome = environment["HOME"];
        scope(exit) environment["HOME"] = oldHome;

        environment["HOME"] = "dmd/test";
        assert(testPath("~/") == "dmd/test/");
        assert(testPath("~") == "dmd/test");
    }
}


version (StdUnittest)
{
private:
    /* Define a mock RandomAccessRange to use for unittesting.
     */

    struct MockRange(C)
    {
        this(C[] array) { this.array = array; }
      const
      {
        @property size_t length() { return array.length; }
        @property bool empty() { return array.length == 0; }
        @property C front() { return array[0]; }
        @property C back()  { return array[$ - 1]; }
        alias opDollar = length;
        C opIndex(size_t i) { return array[i]; }
      }
        void popFront() { array = array[1 .. $]; }
        void popBack()  { array = array[0 .. $-1]; }
        MockRange!C opSlice( size_t lwr, size_t upr) const
        {
            return MockRange!C(array[lwr .. upr]);
        }
        @property MockRange save() { return this; }
      private:
        C[] array;
    }

    /* Define a mock BidirectionalRange to use for unittesting.
     */

    struct MockBiRange(C)
    {
        this(const(C)[] array) { this.array = array; }
        const
        {
            @property bool empty() { return array.length == 0; }
            @property C front() { return array[0]; }
            @property C back()  { return array[$ - 1]; }
            @property size_t opDollar() { return array.length; }
        }
        void popFront() { array = array[1 .. $]; }
        void popBack()  { array = array[0 .. $-1]; }
        @property MockBiRange save() { return this; }
      private:
        const(C)[] array;
    }

}

@safe unittest
{
    static assert( isRandomAccessRange!(MockRange!(const(char))) );
    static assert( isBidirectionalRange!(MockBiRange!(const(char))) );
}

private template BaseOf(R)
{
    static if (isRandomAccessRange!R && isSomeChar!(ElementType!R))
        alias BaseOf = R;
    else
        alias BaseOf = StringTypeOf!R;
}

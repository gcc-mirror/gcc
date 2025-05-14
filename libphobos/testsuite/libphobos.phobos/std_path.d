@safe pure nothrow @nogc unittest
{
    import std.path;

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

@safe unittest
{
    import std.path;

    assert(baseName!(CaseSensitive.no)("dir/file.EXT", ".ext") == "file");
    assert(baseName!(CaseSensitive.yes)("dir/file.EXT", ".ext") != "file");

    version (Posix)
        assert(relativePath!(CaseSensitive.no)("/FOO/bar", "/foo/baz") == "../bar");
    else
        assert(relativePath!(CaseSensitive.no)(`c:\FOO\bar`, `c:\foo\baz`) == `..\bar`);
}

@safe unittest
{
    import std.path;

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
    import std.path;

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
    import std.path;

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
    import std.path;

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
    import std.path;

    version (Windows)
    {
        assert(stripDrive(`d:\dir\file`) == `\dir\file`);
        assert(stripDrive(`\\server\share\dir\file`) == `\dir\file`);
    }
}

@safe unittest
{
    import std.path;

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
    import std.path;

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
    import std.path;

    assert(setExtension("file", "ext") == "file.ext");
    assert(setExtension("file"w, ".ext"w) == "file.ext");
    assert(setExtension("file."d, "ext"d) == "file.ext");
    assert(setExtension("file.", ".ext") == "file.ext");
    assert(setExtension("file.old"w, "new"w) == "file.new");
    assert(setExtension("file.old"d, ".new"d) == "file.new");
}

@safe unittest
{
    import std.path;

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
    import std.path;

    assert(defaultExtension("file", "ext") == "file.ext");
    assert(defaultExtension("file", ".ext") == "file.ext");
    assert(defaultExtension("file.", "ext")     == "file.");
    assert(defaultExtension("file.old", "new") == "file.old");
    assert(defaultExtension("file.old", ".new") == "file.old");
}

@safe unittest
{
    import std.path;

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
    import std.path;

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

@safe unittest
{
    import std.path;

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

@safe unittest
{
    import std.path;

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
    import std.path;

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

@safe unittest
{
    import std.path;

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

@safe unittest
{
    import std.path;

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
    import std.path;

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

@system unittest
{
    import std.path;

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

@safe unittest
{
    import std.path;

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
    import std.path;

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
    import std.path;

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
    import std.path;

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

@safe @nogc unittest
{
    import std.path;

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

@safe pure @nogc nothrow unittest
{
    import std.path;

    import std.utf : byCodeUnit;

    assert(isValidFilename("hello.exe".byCodeUnit));
}

@safe pure @nogc nothrow unittest
{
    import std.path;

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

@safe unittest
{
    import std.path;

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


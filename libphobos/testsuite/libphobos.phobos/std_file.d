@safe unittest
{
    import std.file;

    import std.exception : assertThrown;

    assertThrown!FileException("non.existing.file.".readText);
}

@safe unittest
{
    import std.file;

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

@safe unittest
{
    import std.file;

    write(deleteme, "abc"); // deleteme is the name of a temporary file
    scope(exit) remove(deleteme);
    string content = readText(deleteme);
    assert(content == "abc");
}

@safe unittest
{
    import std.file;

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

@safe unittest
{
    import std.file;

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

@safe unittest
{
    import std.file;

    auto t1 = deleteme, t2 = deleteme~"2";
    scope(exit) foreach (t; [t1, t2]) if (t.exists) t.remove();

    t1.write("1");
    t1.rename(t2);
    assert(t2.readText == "1");

    t1.write("2");
    t1.rename(t2);
    assert(t2.readText == "2");
}

@safe unittest
{
    import std.file;

    import std.exception : assertThrown;

    deleteme.write("Hello");
    assert(deleteme.readText == "Hello");

    deleteme.remove;
    assertThrown!FileException(deleteme.readText);
}

@safe unittest
{
    import std.file;

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
    import std.file;

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
    import std.file;

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

@safe unittest
{
    import std.file;

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
    import std.file;

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

@safe unittest
{
    import std.file;

    auto f = deleteme ~ "does.not.exist";
    assert(!f.exists);

    f.write("hello");
    assert(f.exists);

    f.remove;
    assert(!f.exists);
}

@safe unittest
{
    import std.file;

    assert(".".exists);
    assert(!"this file does not exist".exists);
    deleteme.write("a\n");
    scope(exit) deleteme.remove;
    assert(deleteme.exists);
}

@safe unittest
{
    import std.file;

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

@safe unittest
{
    import std.file;

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
    import std.file;

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

@safe unittest
{
    import std.file;

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

@safe unittest
{
    import std.file;

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
    import std.file;

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

@safe unittest
{
    import std.file;

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

@safe unittest
{
    import std.file;

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
    import std.file;

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
    import std.file;

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

@safe unittest
{
    import std.file;

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
    import std.file;

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

@safe unittest
{
    import std.file;

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

@system unittest
{
    import std.file;

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
    import std.file;

    import std.file : mkdir;

    auto dir = deleteme ~ "dir";
    scope(exit) dir.rmdir;

    dir.mkdir;
    assert(dir.exists);
}

@safe unittest
{
    import std.file;

    import std.exception : assertThrown;
    assertThrown("a/b/c/d/e".mkdir);
}

@safe unittest
{
    import std.file;

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

@safe unittest
{
    import std.file;

    import std.exception : assertThrown;

    scope(exit) deleteme.remove;
    deleteme.write("a");

    // cannot make directory as it's already a file
    assertThrown!FileException(deleteme.mkdirRecurse);
}

@safe unittest
{
    import std.file;

    auto dir = deleteme ~ "dir";

    dir.mkdir;
    assert(dir.exists);
    dir.rmdir;
    assert(!dir.exists);
}

@safe unittest
{
    import std.file;

    auto s = getcwd();
    assert(s.length);
}

@safe unittest
{
    import std.file;

    import std.path : isAbsolute;
    auto path = thisExePath();

    assert(path.exists);
    assert(path.isAbsolute);
    assert(path.isFile);
}

@safe unittest
{
    import std.file;

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

@system unittest
{
    import std.file;

    import std.path : buildPath;

    auto dir = deleteme.buildPath("a", "b", "c");

    dir.mkdirRecurse;
    assert(dir.exists);

    deleteme.rmdirRecurse;
    assert(!dir.exists);
    assert(!deleteme.exists);
}

@system unittest
{
    import std.file;

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

@safe unittest
{
    import std.file;

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
    import std.file;

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

@safe unittest
{
    import std.file;

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
    import std.file;

    import std.exception : assertThrown;

    auto space = getAvailableDiskSpace(".");
    assert(space > 0);

    assertThrown!FileException(getAvailableDiskSpace("ThisFileDoesNotExist123123"));
}


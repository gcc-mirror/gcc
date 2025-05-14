@system unittest
{
    import std.stdio;

        static import std.file;

        auto testFile = std.file.deleteme();
        std.file.write(testFile, "\r\n\n\r\n");
        scope(exit) std.file.remove(testFile);

        auto f = File(testFile, "r");
        auto buf = f.rawRead(new char[5]);
        f.close();
        assert(buf == "\r\n\n\r\n");
    
}

@system unittest
{
    import std.stdio;

        static import std.file;

        auto testFile = std.file.deleteme();
        auto f = File(testFile, "w");
        scope(exit) std.file.remove(testFile);

        f.rawWrite("\r\n\n\r\n");
        f.close();
        assert(std.file.read(testFile) == "\r\n\n\r\n");
    
}

@system unittest
{
    import std.stdio;

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

@system unittest
{
    import std.stdio;

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

@system unittest
{
    import std.stdio;

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

@safe unittest
{
    import std.stdio;

    static assert(isFileHandle!(FILE*));
    static assert(isFileHandle!(File));
}

@safe unittest
{
    import std.stdio;

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

@safe unittest
{
    import std.stdio;

    void main()
    {
        stdout.writeln("Write a message to stdout.");
    }
}

@safe unittest
{
    import std.stdio;

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

@safe unittest
{
    import std.stdio;

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

@safe unittest
{
    import std.stdio;

    void main()
    {
        stderr.writeln("Write a message to stderr.");
    }
}


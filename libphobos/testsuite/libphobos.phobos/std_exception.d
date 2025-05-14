@system unittest
{
    import std.exception;

    import core.stdc.stdlib : malloc, free;
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map, splitter;
    import std.algorithm.searching : endsWith;
    import std.conv : ConvException, to;
    import std.range : front, retro;

    // use enforce like assert
    int a = 3;
    enforce(a > 2, "a needs to be higher than 2.");

    // enforce can throw a custom exception
    enforce!ConvException(a > 2, "a needs to be higher than 2.");

    // enforce will return it's input
    enum size = 42;
    auto memory = enforce(malloc(size), "malloc failed")[0 .. size];
    scope(exit) free(memory.ptr);

    // collectException can be used to test for exceptions
    Exception e = collectException("abc".to!int);
    assert(e.file.endsWith("conv.d"));

    // and just for the exception message
    string msg = collectExceptionMsg("abc".to!int);
    assert(msg == "Unexpected 'a' when converting from type string to type int");

    // assertThrown can be used to assert that an exception is thrown
    assertThrown!ConvException("abc".to!int);

    // ifThrown can be used to provide a default value if an exception is thrown
    assert("x".to!int().ifThrown(0) == 0);

    // handle is a more advanced version of ifThrown for ranges
    auto r = "12,1337z32,54".splitter(',').map!(a => to!int(a));
    auto h = r.handle!(ConvException, RangePrimitive.front, (e, r) => 0);
    assert(h.equal([12, 0, 54]));
    assertThrown!ConvException(h.retro.equal([54, 0, 12]));

    // basicExceptionCtors avoids the boilerplate when creating custom exceptions
    static class MeaCulpa : Exception
    {
        mixin basicExceptionCtors;
    }
    e = collectException((){throw new MeaCulpa("diagnostic message");}());
    assert(e.msg == "diagnostic message");
    assert(e.file == __FILE__);
    assert(e.line == __LINE__ - 3);

    // assumeWontThrow can be used to cast throwing code into `nothrow`
    void exceptionFreeCode() nothrow
    {
        // auto-decoding only throws if an invalid UTF char is given
        assumeWontThrow("abc".front);
    }

    // assumeUnique can be used to cast mutable instance to an `immutable` one
    // use with care
    char[] str = "  mutable".dup;
    str[0 .. 2] = "im";
    immutable res = assumeUnique(str);
    assert(res == "immutable");
}

@system unittest
{
    import std.exception;

    import core.exception : AssertError;

    import std.string;
    assertNotThrown!StringException(enforce!StringException(true, "Error!"));

    //Exception is the default.
    assertNotThrown(enforce!StringException(true, "Error!"));

    assert(collectExceptionMsg!AssertError(assertNotThrown!StringException(
               enforce!StringException(false, "Error!"))) ==
           `assertNotThrown failed: StringException was thrown: Error!`);
}

@system unittest
{
    import std.exception;

    import core.exception : AssertError;
    import std.string;

    assertThrown!StringException(enforce!StringException(false, "Error!"));

    //Exception is the default.
    assertThrown(enforce!StringException(false, "Error!"));

    assert(collectExceptionMsg!AssertError(assertThrown!StringException(
               enforce!StringException(true, "Error!"))) ==
           `assertThrown failed: No StringException was thrown.`);
}

@system unittest
{
    import std.exception;

    import core.stdc.stdlib : malloc, free;
    import std.conv : ConvException, to;

    // use enforce like assert
    int a = 3;
    enforce(a > 2, "a needs to be higher than 2.");

    // enforce can throw a custom exception
    enforce!ConvException(a > 2, "a needs to be higher than 2.");

    // enforce will return it's input
    enum size = 42;
    auto memory = enforce(malloc(size), "malloc failed")[0 .. size];
    scope(exit) free(memory.ptr);
}

@safe unittest
{
    import std.exception;

    assertNotThrown(enforce(true, new Exception("this should not be thrown")));
    assertThrown(enforce(false, new Exception("this should be thrown")));
}

@safe unittest
{
    import std.exception;

    assert(enforce(123) == 123);

    try
    {
        enforce(false, "error");
        assert(false);
    }
    catch (Exception e)
    {
        assert(e.msg == "error");
        assert(e.file == __FILE__);
        assert(e.line == __LINE__-7);
    }
}

@safe unittest
{
    import std.exception;

    import std.conv : ConvException;
    alias convEnforce = enforce!ConvException;
    assertNotThrown(convEnforce(true));
    assertThrown!ConvException(convEnforce(false, "blah"));
}

@system unittest
{
    import std.exception;

    import core.stdc.stdio : fclose, fgets, fopen;
    import std.file : thisExePath;
    import std.string : toStringz;

    auto f = fopen(thisExePath.toStringz, "r").errnoEnforce;
    scope(exit) fclose(f);
    char[100] buf;
    auto line = fgets(buf.ptr, buf.length, f);
    enforce(line !is null); // expect a non-empty line
}

@system unittest
{
    import std.exception;

    int b;
    int foo() { throw new Exception("blah"); }
    assert(collectException(foo(), b));

    version (D_NoBoundsChecks) {}
    else
    {
        // check for out of bounds error
        int[] a = new int[3];
        import core.exception : RangeError;
        assert(collectException!RangeError(a[4], b));
    }
}

@safe unittest
{
    import std.exception;

    int foo() { throw new Exception("blah"); }
    assert(collectException(foo()).msg == "blah");
}

@safe unittest
{
    import std.exception;

    void throwFunc() { throw new Exception("My Message."); }
    assert(collectExceptionMsg(throwFunc()) == "My Message.");

    void nothrowFunc() {}
    assert(collectExceptionMsg(nothrowFunc()) is null);

    void throwEmptyFunc() { throw new Exception(""); }
    assert(collectExceptionMsg(throwEmptyFunc()) == emptyExceptionMsg);
}

@system unittest
{
    import std.exception;

    int[] arr = new int[1];
    auto arr1 = arr.assumeUnique;
    static assert(is(typeof(arr1) == immutable(int)[]));
    assert(arr == null);
    assert(arr1 == [0]);
}

@system unittest
{
    import std.exception;

    int[string] arr = ["a":1];
    auto arr1 = arr.assumeUnique;
    static assert(is(typeof(arr1) == immutable(int[string])));
    assert(arr == null);
    assert(arr1.keys == ["a"]);
}

@safe unittest
{
    import std.exception;

    import std.math.algebraic : sqrt;

    // This function may throw.
    int squareRoot(int x)
    {
        if (x < 0)
            throw new Exception("Tried to take root of negative number");
        return cast(int) sqrt(cast(double) x);
    }

    // This function never throws.
    int computeLength(int x, int y) nothrow
    {
        // Since x*x + y*y is always positive, we can safely assume squareRoot
        // won't throw, and use it to implement this nothrow function. If it
        // does throw (e.g., if x*x + y*y overflows a 32-bit value), then the
        // program will terminate.
        return assumeWontThrow(squareRoot(x*x + y*y));
    }

    assert(computeLength(3, 4) == 5);
}

@system unittest
{
    import std.exception;

    int  i = 0;
    int* p = null;
    assert(!p.doesPointTo(i));
    p = &i;
    assert( p.doesPointTo(i));
}

@system unittest
{
    import std.exception;

    struct S
    {
        int v;
        int* p;
    }
    int i;
    auto s = S(0, &i);

    // structs and unions "own" their members
    // pointsTo will answer true if one of the members pointsTo.
    assert(!s.doesPointTo(s.v)); //s.v is just v member of s, so not pointed.
    assert( s.p.doesPointTo(i)); //i is pointed by s.p.
    assert( s  .doesPointTo(i)); //which means i is pointed by s itself.

    // Unions will behave exactly the same. Points to will check each "member"
    // individually, even if they share the same memory
}

@system unittest
{
    import std.exception;

    int i;
     // trick the compiler when initializing slice
     // https://issues.dlang.org/show_bug.cgi?id=18637
    int* p = &i;
    int[]  slice = [0, 1, 2, 3, 4];
    int[5] arr   = [0, 1, 2, 3, 4];
    int*[]  slicep = [p];
    int*[1] arrp   = [&i];

    // A slice points to all of its members:
    assert( slice.doesPointTo(slice[3]));
    assert(!slice[0 .. 2].doesPointTo(slice[3])); // Object 3 is outside of the
                                                  // slice [0 .. 2]

    // Note that a slice will not take into account what its members point to.
    assert( slicep[0].doesPointTo(i));
    assert(!slicep   .doesPointTo(i));

    // static arrays are objects that own their members, just like structs:
    assert(!arr.doesPointTo(arr[0])); // arr[0] is just a member of arr, so not
                                      // pointed.
    assert( arrp[0].doesPointTo(i));  // i is pointed by arrp[0].
    assert( arrp   .doesPointTo(i));  // which means i is pointed by arrp
                                      // itself.

    // Notice the difference between static and dynamic arrays:
    assert(!arr  .doesPointTo(arr[0]));
    assert( arr[].doesPointTo(arr[0]));
    assert( arrp  .doesPointTo(i));
    assert(!arrp[].doesPointTo(i));
}

@system unittest
{
    import std.exception;

    class C
    {
        this(int* p){this.p = p;}
        int* p;
    }
    int i;
    C a = new C(&i);
    C b = a;

    // Classes are a bit particular, as they are treated like simple pointers
    // to a class payload.
    assert( a.p.doesPointTo(i)); // a.p points to i.
    assert(!a  .doesPointTo(i)); // Yet a itself does not point i.

    //To check the class payload itself, iterate on its members:
    ()
    {
        import std.traits : Fields;

        foreach (index, _; Fields!C)
            if (doesPointTo(a.tupleof[index], i))
                return;
        assert(0);
    }();

    // To check if a class points a specific payload, a direct memmory check
    // can be done:
    auto aLoc = cast(ubyte[__traits(classInstanceSize, C)]*) a;
    assert(b.doesPointTo(*aLoc)); // b points to where a is pointing
}

@safe unittest
{
    import std.exception;

    import core.stdc.errno : EAGAIN;
    auto ex = new ErrnoException("oh no", EAGAIN);
    assert(ex.errno == EAGAIN);
}

@safe unittest
{
    import std.exception;

    import core.stdc.errno : errno, EAGAIN;

    auto old = errno;
    scope(exit) errno = old;

    // fake that errno got set by the callee
    errno = EAGAIN;
    auto ex = new ErrnoException("oh no");
    assert(ex.errno == EAGAIN);
}

@safe unittest
{
    import std.exception;

    import std.conv : to;
    assert("x".to!int.ifThrown(0) == 0);
}

@safe unittest
{
    import std.exception;

    import std.conv : ConvException, to;
    string s = "true";
    assert(s.to!int.ifThrown(cast(int) s.to!double)
                   .ifThrown(cast(int) s.to!bool) == 1);

    s = "2.0";
    assert(s.to!int.ifThrown(cast(int) s.to!double)
                   .ifThrown(cast(int) s.to!bool) == 2);

    // Respond differently to different types of errors
    alias orFallback = (lazy a)  => a.ifThrown!ConvException("not a number")
                                     .ifThrown!Exception("number too small");

    assert(orFallback(enforce("x".to!int < 1).to!string) == "not a number");
    assert(orFallback(enforce("2".to!int < 1).to!string) == "number too small");
}

@safe unittest
{
    import std.exception;

    // null and new Object have a common type(Object).
    static assert(is(typeof(null.ifThrown(new Object())) == Object));
    static assert(is(typeof((new Object()).ifThrown(null)) == Object));

    // 1 and new Object do not have a common type.
    static assert(!__traits(compiles, 1.ifThrown(new Object())));
    static assert(!__traits(compiles, (new Object()).ifThrown(1)));
}

@system unittest
{
    import std.exception;

    import std.format : format;
    assert("%s".format.ifThrown!Exception(e => typeid(e).name) == "std.format.FormatException");
}

pure @safe unittest
{
    import std.exception;

    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map, splitter;
    import std.conv : to, ConvException;

    auto s = "12,1337z32,54,2,7,9,1z,6,8";

    // The next line composition will throw when iterated
    // as some elements of the input do not convert to integer
    auto r = s.splitter(',').map!(a => to!int(a));

    // Substitute 0 for cases of ConvException
    auto h = r.handle!(ConvException, RangePrimitive.front, (e, r) => 0);
    assert(h.equal([12, 0, 54, 2, 7, 9, 0, 6, 8]));
}

pure @safe unittest
{
    import std.exception;

    import std.algorithm.comparison : equal;
    import std.range : retro;
    import std.utf : UTFException;

    auto str = "hello\xFFworld"; // 0xFF is an invalid UTF-8 code unit

    auto handled = str.handle!(UTFException, RangePrimitive.access,
            (e, r) => ' '); // Replace invalid code points with spaces

    assert(handled.equal("hello world")); // `front` is handled,
    assert(handled.retro.equal("dlrow olleh")); // as well as `back`
}

pure @safe unittest
{
    import std.exception;

    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map, splitter;
    import std.conv : to, ConvException;

    auto s = "12,1337z32,54,2,7,9,1z,6,8";

    // The next line composition will throw when iterated
    // as some elements of the input do not convert to integer
    auto r = s.splitter(',').map!(a => to!int(a));

    // Substitute 0 for cases of ConvException
    auto h = r.handle!(ConvException, RangePrimitive.front, (e, r) => 0);
    assert(h.equal([12, 0, 54, 2, 7, 9, 0, 6, 8]));
}

pure @safe unittest
{
    import std.exception;

    import std.algorithm.comparison : equal;
    import std.range : retro;
    import std.utf : UTFException;

    auto str = "hello\xFFworld"; // 0xFF is an invalid UTF-8 code unit

    auto handled = str.handle!(UTFException, RangePrimitive.access,
            (e, r) => ' '); // Replace invalid code points with spaces

    assert(handled.equal("hello world")); // `front` is handled,
    assert(handled.retro.equal("dlrow olleh")); // as well as `back`
}

@safe unittest
{
    import std.exception;

    class MeaCulpa: Exception
    {
        ///
        mixin basicExceptionCtors;
    }

    try
        throw new MeaCulpa("test");
    catch (MeaCulpa e)
    {
        assert(e.msg == "test");
        assert(e.file == __FILE__);
        assert(e.line == __LINE__ - 5);
    }
}

@safe pure nothrow unittest
{
    import std.exception;

    class TestException : Exception { mixin basicExceptionCtors; }
    auto e = new Exception("msg");
    auto te1 = new TestException("foo");
    auto te2 = new TestException("foo", e);
}


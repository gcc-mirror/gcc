// Written in the D programming language.

/++
    This module defines functions related to exceptions and general error
    handling. It also defines functions intended to aid in unit testing.

$(SCRIPT inhibitQuickIndex = 1;)
$(DIVC quickindex,
$(BOOKTABLE,
$(TR $(TH Category) $(TH Functions))
$(TR $(TD Assumptions) $(TD
        $(LREF assertNotThrown)
        $(LREF assertThrown)
        $(LREF assumeUnique)
        $(LREF assumeWontThrow)
        $(LREF mayPointTo)
))
$(TR $(TD Enforce) $(TD
        $(LREF doesPointTo)
        $(LREF enforce)
        $(LREF errnoEnforce)
))
$(TR $(TD Handlers) $(TD
        $(LREF collectException)
        $(LREF collectExceptionMsg)
        $(LREF ifThrown)
        $(LREF handle)
))
$(TR $(TD Other) $(TD
        $(LREF basicExceptionCtors)
        $(LREF emptyExceptionMsg)
        $(LREF ErrnoException)
        $(LREF RangePrimitive)
))
))

    Copyright: Copyright Andrei Alexandrescu 2008-, Jonathan M Davis 2011-.
    License:   $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0)
    Authors:   $(HTTP erdani.org, Andrei Alexandrescu) and
               $(HTTP jmdavisprog.com, Jonathan M Davis)
    Source:    $(PHOBOSSRC std/exception.d)

 +/
module std.exception;

/// Synopis
@system unittest
{
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

import std.range.primitives;
import std.traits;

/++
    Asserts that the given expression does $(I not) throw the given type
    of `Throwable`. If a `Throwable` of the given type is thrown,
    it is caught and does not escape assertNotThrown. Rather, an
    `AssertError` is thrown. However, any other `Throwable`s will escape.

    Params:
        T          = The `Throwable` to test for.
        expression = The expression to test.
        msg        = Optional message to output on test failure.
                     If msg is empty, and the thrown exception has a
                     non-empty msg field, the exception's msg field
                     will be output on test failure.
        file       = The file where the error occurred.
                     Defaults to `__FILE__`.
        line       = The line where the error occurred.
                     Defaults to `__LINE__`.

    Throws:
        `AssertError` if the given `Throwable` is thrown.

    Returns:
        the result of `expression`.
 +/
auto assertNotThrown(T : Throwable = Exception, E)
                    (lazy E expression,
                     string msg = null,
                     string file = __FILE__,
                     size_t line = __LINE__)
{
    import core.exception : AssertError;
    try
    {
        return expression();
    }
    catch (T t)
    {
        immutable message = msg.length == 0 ? t.msg : msg;
        immutable tail = message.length == 0 ? "." : ": " ~ message;
        throw new AssertError("assertNotThrown failed: " ~ T.stringof ~ " was thrown" ~ tail, file, line, t);
    }
}
///
@system unittest
{
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
    import core.exception : AssertError;
    import std.string;
    assert(collectExceptionMsg!AssertError(assertNotThrown!StringException(
               enforce!StringException(false, ""), "Error!")) ==
           `assertNotThrown failed: StringException was thrown: Error!`);

    assert(collectExceptionMsg!AssertError(assertNotThrown!StringException(
               enforce!StringException(false, ""))) ==
           `assertNotThrown failed: StringException was thrown.`);

    assert(collectExceptionMsg!AssertError(assertNotThrown!StringException(
               enforce!StringException(false, ""), "")) ==
           `assertNotThrown failed: StringException was thrown.`);
}

@system unittest
{
    import core.exception : AssertError;

    static noreturn throwEx(Throwable t) { throw t; }
    bool nothrowEx() { return true; }

    try
    {
        assert(assertNotThrown!Exception(nothrowEx()));
    }
    catch (AssertError) assert(0);

    try
    {
        assert(assertNotThrown!Exception(nothrowEx(), "It's a message"));
    }
    catch (AssertError) assert(0);

    try
    {
        assert(assertNotThrown!AssertError(nothrowEx()));
    }
    catch (AssertError) assert(0);

    try
    {
        assert(assertNotThrown!AssertError(nothrowEx(), "It's a message"));
    }
    catch (AssertError) assert(0);

    {
        bool thrown = false;
        try
        {
            assertNotThrown!Exception(
                throwEx(new Exception("It's an Exception")));
        }
        catch (AssertError) thrown = true;
        assert(thrown);
    }

    {
        bool thrown = false;
        try
        {
            assertNotThrown!Exception(
                throwEx(new Exception("It's an Exception")), "It's a message");
        }
        catch (AssertError) thrown = true;
        assert(thrown);
    }

    {
        bool thrown = false;
        try
        {
            assertNotThrown!AssertError(
                throwEx(new AssertError("It's an AssertError", __FILE__, __LINE__)));
        }
        catch (AssertError) thrown = true;
        assert(thrown);
    }

    {
        bool thrown = false;
        try
        {
            assertNotThrown!AssertError(
                throwEx(new AssertError("It's an AssertError", __FILE__, __LINE__)),
                        "It's a message");
        }
        catch (AssertError) thrown = true;
        assert(thrown);
    }
}

/++
    Asserts that the given expression throws the given type of `Throwable`.
    The `Throwable` is caught and does not escape assertThrown. However,
    any other `Throwable`s $(I will) escape, and if no `Throwable`
    of the given type is thrown, then an `AssertError` is thrown.

    Params:
        T          = The `Throwable` to test for.
        expression = The expression to test.
        msg        = Optional message to output on test failure.
        file       = The file where the error occurred.
                     Defaults to `__FILE__`.
        line       = The line where the error occurred.
                     Defaults to `__LINE__`.

    Throws:
        `AssertError` if the given `Throwable` is not thrown.
  +/
void assertThrown(T : Throwable = Exception, E)
                 (lazy E expression,
                  string msg = null,
                  string file = __FILE__,
                  size_t line = __LINE__)
{
    import core.exception : AssertError;

    try
        expression();
    catch (T)
        return;

    static if (!is(immutable E == immutable noreturn))
        throw new AssertError("assertThrown failed: No " ~ T.stringof ~ " was thrown"
                                 ~ (msg.length == 0 ? "." : ": ") ~ msg,
                          file, line);
}
///
@system unittest
{
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
    import core.exception : AssertError;

    static noreturn throwEx(Throwable t) { throw t; }
    void nothrowEx() { }

    try
    {
        assertThrown!Exception(throwEx(new Exception("It's an Exception")));
    }
    catch (AssertError) assert(0);

    try
    {
        assertThrown!Exception(throwEx(new Exception("It's an Exception")),
                               "It's a message");
    }
    catch (AssertError) assert(0);

    try
    {
        assertThrown!AssertError(throwEx(new AssertError("It's an AssertError",
                                                         __FILE__, __LINE__)));
    }
    catch (AssertError) assert(0);

    try
    {
        assertThrown!AssertError(throwEx(new AssertError("It's an AssertError",
                                                         __FILE__, __LINE__)),
                                 "It's a message");
    }
    catch (AssertError) assert(0);


    {
        bool thrown = false;
        try
            assertThrown!Exception(nothrowEx());
        catch (AssertError)
            thrown = true;

        assert(thrown);
    }

    {
        bool thrown = false;
        try
            assertThrown!Exception(nothrowEx(), "It's a message");
        catch (AssertError)
            thrown = true;

        assert(thrown);
    }

    {
        bool thrown = false;
        try
            assertThrown!AssertError(nothrowEx());
        catch (AssertError)
            thrown = true;

        assert(thrown);
    }

    {
        bool thrown = false;
        try
            assertThrown!AssertError(nothrowEx(), "It's a message");
        catch (AssertError)
            thrown = true;

        assert(thrown);
    }
}


/++
    Enforces that the given value is true.
    If the given value is false, an exception is thrown.
    The
    $(UL
        $(LI `msg` - error message as a `string`)
        $(LI `dg` - custom delegate that return a string and is only called if an exception occurred)
        $(LI `ex` - custom exception to be thrown. It is `lazy` and is only created if an exception occurred)
    )

    Params:
        value = The value to test.
        E = Exception type to throw if the value evaluates to false.
        msg = The error message to put in the exception if it is thrown.
        dg = The delegate to be called if the value evaluates to false.
        ex = The exception to throw if the value evaluates to false.
        file = The source file of the caller.
        line = The line number of the caller.

    Returns: `value`, if `cast(bool) value` is true. Otherwise,
    depending on the chosen overload, `new Exception(msg)`, `dg()` or `ex` is thrown.

    Note:
        `enforce` is used to throw exceptions and is therefore intended to
        aid in error handling. It is $(I not) intended for verifying the logic
        of your program. That is what `assert` is for. Also, do not use
        `enforce` inside of contracts (i.e. inside of `in` and `out`
        blocks and `invariant`s), because contracts are compiled out when
        compiling with $(I -release).

        If a delegate is passed, the safety and purity of this function are inferred
        from `Dg`'s safety and purity.
 +/
template enforce(E : Throwable = Exception)
if (is(typeof(new E("", string.init, size_t.init)) : Throwable) ||
    is(typeof(new E(string.init, size_t.init)) : Throwable))
{
    ///
    T enforce(T)(T value, lazy const(char)[] msg = null,
    string file = __FILE__, size_t line = __LINE__)
    if (is(typeof({ if (!value) {} })))
    {
        if (!value) bailOut!E(file, line, msg);
        return value;
    }
}

/// ditto
T enforce(T, Dg, string file = __FILE__, size_t line = __LINE__)
    (T value, scope Dg dg)
if (isSomeFunction!Dg && is(typeof( dg() )) &&
    is(typeof({ if (!value) {} })))
{
    if (!value) dg();
    return value;
}

/// ditto
T enforce(T)(T value, lazy Throwable ex)
{
    if (!value) throw ex();
    return value;
}

///
@system unittest
{
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

///
@safe unittest
{
    assertNotThrown(enforce(true, new Exception("this should not be thrown")));
    assertThrown(enforce(false, new Exception("this should be thrown")));
}

///
@safe unittest
{
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

/// Alias your own enforce function
@safe unittest
{
    import std.conv : ConvException;
    alias convEnforce = enforce!ConvException;
    assertNotThrown(convEnforce(true));
    assertThrown!ConvException(convEnforce(false, "blah"));
}

private noreturn bailOut(E : Throwable = Exception)(string file, size_t line, scope const(char)[] msg)
{
    static if (is(typeof(new E(string.init, string.init, size_t.init))))
    {
        throw new E(msg ? msg.idup : "Enforcement failed", file, line);
    }
    else static if (is(typeof(new E(string.init, size_t.init))))
    {
        throw new E(file, line);
    }
    else
    {
        static assert(0, "Expected this(string, string, size_t) or this(string, size_t)" ~
            " constructor for " ~ __traits(identifier, E));
    }
}

// https://issues.dlang.org/show_bug.cgi?id=10510
@safe unittest
{
    extern(C) void cFoo() { }
    enforce(false, &cFoo);
}

// purity and safety inference test
@system unittest
{
    static foreach (EncloseSafe; [false, true])
    static foreach (EnclosePure; [false, true])
    {
        static foreach (BodySafe; [false, true])
        static foreach (BodyPure; [false, true])
        {{
            enum code =
                "delegate void() " ~
                (EncloseSafe ? "@safe " : "") ~
                (EnclosePure ? "pure " : "") ~
                "{ enforce(true, { " ~
                        "int n; " ~
                        (BodySafe ? "" : "auto p = &n + 10; "    ) ~    // unsafe code
                        (BodyPure ? "" : "static int g; g = 10; ") ~    // impure code
                    "}); " ~
                "}";
            enum expect =
                (BodySafe || !EncloseSafe) && (!EnclosePure || BodyPure);

            version (none)
            pragma(msg, "safe = ", EncloseSafe?1:0, "/", BodySafe?1:0, ", ",
                        "pure = ", EnclosePure?1:0, "/", BodyPure?1:0, ", ",
                        "expect = ", expect?"OK":"NG", ", ",
                        "code = ", code);

            static assert(__traits(compiles, mixin(code)()) == expect);
        }}
    }
}

// Test for https://issues.dlang.org/show_bug.cgi?id=8637
@system unittest
{
    struct S
    {
        static int g;
        ~this() {}  // impure & unsafe destructor
        bool opCast(T:bool)() {
            int* p = cast(int*) 0;   // unsafe operation
            int n = g;              // impure operation
            return true;
        }
    }
    S s;

    enforce(s);
    enforce(s, {});
    enforce(s, new Exception(""));

    errnoEnforce(s);

    alias E1 = Exception;
    static class E2 : Exception
    {
        this(string fn, size_t ln) { super("", fn, ln); }
    }
    static class E3 : Exception
    {
        this(string msg) { super(msg, __FILE__, __LINE__); }
    }
    enforce!E1(s);
    enforce!E2(s);
}

// https://issues.dlang.org/show_bug.cgi?id=14685
@safe unittest
{
    class E : Exception
    {
        this() { super("Not found"); }
    }
    static assert(!__traits(compiles, { enforce!E(false); }));
}

/++
    Enforces that the given value is true, throwing an `ErrnoException` if it
    is not.

    Params:
        value = The value to test.
        msg = The message to include in the `ErrnoException` if it is thrown.

    Returns: `value`, if `cast(bool) value` is true. Otherwise,
    $(D new ErrnoException(msg)) is thrown.  It is assumed that the last
    operation set `errno` to an error code corresponding with the failed
    condition.
 +/
alias errnoEnforce = enforce!ErrnoException;

///
@system unittest
{
    import core.stdc.stdio : fclose, fgets, fopen;
    import std.file : thisExePath;
    import std.string : toStringz;

    auto f = fopen(thisExePath.toStringz, "r").errnoEnforce;
    scope(exit) fclose(f);
    char[100] buf;
    auto line = fgets(buf.ptr, buf.length, f);
    enforce(line !is null); // expect a non-empty line
}

/++
    Catches and returns the exception thrown from the given expression.
    If no exception is thrown, then null is returned and `result` is
    set to the result of the expression.

    Note that while `collectException` $(I can) be used to collect any
    `Throwable` and not just `Exception`s, it is generally ill-advised to
    catch anything that is neither an `Exception` nor a type derived from
    `Exception`. So, do not use `collectException` to collect
    non-`Exception`s unless you're sure that that's what you really want to
    do.

    Params:
        T          = The type of exception to catch.
        expression = The expression which may throw an exception.
        result     = The result of the expression if no exception is thrown.
+/
T collectException(T = Exception, E)(lazy E expression, ref E result)
{
    try
    {
        result = expression();
    }
    catch (T e)
    {
        return e;
    }
    // Avoid "statement not reachable" warning
    static if (!is(immutable E == immutable noreturn))
        return null;
}
///
@system unittest
{
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

/++
    Catches and returns the exception thrown from the given expression.
    If no exception is thrown, then null is returned. `E` can be
    `void`.

    Note that while `collectException` $(I can) be used to collect any
    `Throwable` and not just `Exception`s, it is generally ill-advised to
    catch anything that is neither an `Exception` nor a type derived from
    `Exception`. So, do not use `collectException` to collect
    non-`Exception`s unless you're sure that that's what you really want to
    do.

    Params:
        T          = The type of exception to catch.
        expression = The expression which may throw an exception.
+/
T collectException(T : Throwable = Exception, E)(lazy E expression)
{
    try
    {
        expression();
    }
    catch (T t)
    {
        return t;
    }
    // Avoid "statement not reachable" warning
    static if (!is(immutable E == immutable noreturn))
        return null;
}

///
@safe unittest
{
    int foo() { throw new Exception("blah"); }
    assert(collectException(foo()).msg == "blah");
}

/++
    Catches the exception thrown from the given expression and returns the
    msg property of that exception. If no exception is thrown, then null is
    returned. `E` can be `void`.

    If an exception is thrown but it has an empty message, then
    `emptyExceptionMsg` is returned.

    Note that while `collectExceptionMsg` $(I can) be used to collect any
    `Throwable` and not just `Exception`s, it is generally ill-advised to
    catch anything that is neither an `Exception` nor a type derived from
    `Exception`. So, do not use `collectExceptionMsg` to collect
    non-`Exception`s unless you're sure that that's what you really want to
    do.

    Params:
        T          = The type of exception to catch.
        expression = The expression which may throw an exception.
+/
string collectExceptionMsg(T = Exception, E)(lazy E expression)
{
    import std.array : empty;
    try
    {
        expression();

        // Avoid "statement not reachable" warning
        static if (!is(immutable E == immutable noreturn))
            return cast(string) null;
    }
    catch (T e)
        return e.msg.empty ? emptyExceptionMsg : e.msg;
}
///
@safe unittest
{
    void throwFunc() { throw new Exception("My Message."); }
    assert(collectExceptionMsg(throwFunc()) == "My Message.");

    void nothrowFunc() {}
    assert(collectExceptionMsg(nothrowFunc()) is null);

    void throwEmptyFunc() { throw new Exception(""); }
    assert(collectExceptionMsg(throwEmptyFunc()) == emptyExceptionMsg);
}

/++
    Value that collectExceptionMsg returns when it catches an exception
    with an empty exception message.
 +/
enum emptyExceptionMsg = "<Empty Exception Message>";

// https://issues.dlang.org/show_bug.cgi?id=22364
@system unittest
{
    static noreturn foo() { throw new Exception(""); }

    const ex = collectException!(Exception, noreturn)(foo());
    assert(ex);

    const msg = collectExceptionMsg!(Exception, noreturn)(foo());
    assert(msg);

    noreturn n;

    // Triggers a backend assertion failure
    // collectException!(Exception, noreturn)(foo(), n);

    static assert(__traits(compiles, collectException!(Exception, noreturn)(foo(), n)));
}

/**
Casts a mutable array to an immutable array in an idiomatic
manner. Technically, `assumeUnique` just inserts a cast,
but its name documents assumptions on the part of the
caller. `assumeUnique(arr)` should only be called when
there are no more active mutable aliases to elements of $(D
arr). To strengthen this assumption, `assumeUnique(arr)`
also clears `arr` before returning. Essentially $(D
assumeUnique(arr)) indicates commitment from the caller that there
is no more mutable access to any of `arr`'s elements
(transitively), and that all future accesses will be done through
the immutable array returned by `assumeUnique`.

Typically, `assumeUnique` is used to return arrays from
functions that have allocated and built them.

Params:
 array = The array to cast to immutable.

Returns: The immutable array.

Example:

$(RUNNABLE_EXAMPLE
----
string letters()
{
  char[] result = new char['z' - 'a' + 1];
  foreach (i, ref e; result)
  {
    e = cast(char)('a' + i);
  }
  return assumeUnique(result);
}
----
)

The use in the example above is correct because `result`
was private to `letters` and is inaccessible in writing
after the function returns. The following example shows an
incorrect use of `assumeUnique`.

Bad:

$(RUNNABLE_EXAMPLE
----
char[] buffer;
string letters(char first, char last)
{
  if (first >= last) return null; // fine
  auto sneaky = buffer;
  sneaky.length = last - first + 1;
  foreach (i, ref e; sneaky)
  {
    e = cast(char)('a' + i);
  }
  return assumeUnique(sneaky); // BAD
}
----
)

The example above wreaks havoc on client code because it is
modifying arrays that callers considered immutable. To obtain an
immutable array from the writable array `buffer`, replace
the last line with:

----
return to!(string)(sneaky); // not that sneaky anymore
----

The call will duplicate the array appropriately.

Note that checking for uniqueness during compilation is
possible in certain cases, especially when a function is
marked as a pure function. The following example does not
need to call `assumeUnique` because the compiler can infer the
uniqueness of the array in the pure function:

$(RUNNABLE_EXAMPLE
----
static string letters() pure
{
  char[] result = new char['z' - 'a' + 1];
  foreach (i, ref e; result)
  {
    e = cast(char)('a' + i);
  }
  return result;
}
----
)

For more on infering uniqueness see the $(B unique) and
$(B lent) keywords in the
$(HTTP www.cs.cmu.edu/~aldrich/papers/aldrich-dissertation.pdf, ArchJava)
language.

The downside of using `assumeUnique`'s
convention-based usage is that at this time there is no
formal checking of the correctness of the assumption;
on the upside, the idiomatic use of `assumeUnique` is
simple and rare enough to be tolerable.
 */
immutable(T)[] assumeUnique(T)(T[] array) pure nothrow
{
    return .assumeUnique(array);    // call ref version
}
/// ditto
immutable(T)[] assumeUnique(T)(ref T[] array) pure nothrow
{
    auto result = cast(immutable(T)[]) array;
    array = null;
    return result;
}
/// ditto
immutable(T[U]) assumeUnique(T, U)(ref T[U] array) pure nothrow
{
    auto result = cast(immutable(T[U])) array;
    array = null;
    return result;
}

///
@system unittest
{
    int[] arr = new int[1];
    auto arr1 = arr.assumeUnique;
    static assert(is(typeof(arr1) == immutable(int)[]));
    assert(arr == null);
    assert(arr1 == [0]);
}

///
@system unittest
{
    int[string] arr = ["a":1];
    auto arr1 = arr.assumeUnique;
    static assert(is(typeof(arr1) == immutable(int[string])));
    assert(arr == null);
    assert(arr1.keys == ["a"]);
}

/**
 * Wraps a possibly-throwing expression in a `nothrow` wrapper so that it
 * can be called by a `nothrow` function.
 *
 * This wrapper function documents commitment on the part of the caller that
 * the appropriate steps have been taken to avoid whatever conditions may
 * trigger an exception during the evaluation of `expr`.  If it turns out
 * that the expression $(I does) throw at runtime, the wrapper will throw an
 * `AssertError`.
 *
 * (Note that `Throwable` objects such as `AssertError` that do not
 * subclass `Exception` may be thrown even from `nothrow` functions,
 * since they are considered to be serious runtime problems that cannot be
 * recovered from.)
 *
 * Params:
 *  expr = The expression asserted not to throw.
 *  msg = The message to include in the `AssertError` if the assumption turns
 *      out to be false.
 *  file = The source file name of the caller.
 *  line = The line number of the caller.
 *
 * Returns:
 *  The value of `expr`, if any.
 */
T assumeWontThrow(T)(lazy T expr,
                     string msg = null,
                     string file = __FILE__,
                     size_t line = __LINE__) nothrow
{
    import core.exception : AssertError;
    try
    {
        return expr;
    }
    catch (Exception e)
    {
        import std.range.primitives : empty;
        immutable tail = msg.empty ? "." : ": " ~ msg;
        throw new AssertError("assumeWontThrow failed: Expression did throw" ~
                              tail, file, line);
    }
}

///
@safe unittest
{
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
    import core.exception : AssertError;

    void alwaysThrows()
    {
        throw new Exception("I threw up");
    }
    void bad() nothrow
    {
        assumeWontThrow(alwaysThrows());
    }
    assertThrown!AssertError(bad());
}

/**
Checks whether a given source object contains pointers or references to a given
target object.

Params:
    source = The source object
    target = The target object

Bugs:
    The function is explicitly annotated `@nogc` because inference could fail,
    see $(LINK2 https://issues.dlang.org/show_bug.cgi?id=17084, issue 17084).

Returns: `true` if `source`'s representation embeds a pointer
that points to `target`'s representation or somewhere inside
it.

If `source` is or contains a dynamic array, then, then these functions will check
if there is overlap between the dynamic array and `target`'s representation.

If `source` is a class, then it will be handled as a pointer.

If `target` is a pointer, a dynamic array or a class, then these functions will only
check if `source` points to `target`, $(I not) what `target` references.

If `source` is or contains a union or `void[n]`, then there may be either false positives or
false negatives:

`doesPointTo` will return `true` if it is absolutely certain
`source` points to `target`. It may produce false negatives, but never
false positives. This function should be prefered when trying to validate
input data.

`mayPointTo` will return `false` if it is absolutely certain
`source` does not point to `target`. It may produce false positives, but never
false negatives. This function should be prefered for defensively choosing a
code path.

Note: Evaluating $(D doesPointTo(x, x)) checks whether `x` has
internal pointers. This should only be done as an assertive test,
as the language is free to assume objects don't have internal pointers
(TDPL 7.1.3.5).
*/
bool doesPointTo(S, T, Tdummy=void)(auto ref const S source, ref const T target) @nogc @trusted pure nothrow
if (__traits(isRef, source) || isDynamicArray!S ||
    is(S == U*, U) || is(S == class))
{
    static if (is(S == U*, U) || is(S == class) || is(S == interface))
    {
        const m = *cast(void**) &source;
        const b = cast(void*) &target;
        const e = b + target.sizeof;
        return b <= m && m < e;
    }
    else static if (is(S == struct) || is(S == union))
    {
        foreach (i, Subobj; typeof(source.tupleof))
            static if (!isUnionAliased!(S, i))
                if (doesPointTo(source.tupleof[i], target)) return true;
        return false;
    }
    else static if (isStaticArray!S)
    {
        static if (!is(S == void[n], size_t n))
        {
            foreach (ref s; source)
                if (doesPointTo(s, target)) return true;
        }
        return false;
    }
    else static if (isDynamicArray!S)
    {
        import std.array : overlap;
        return overlap(cast(void[]) source, cast(void[])(&target)[0 .. 1]).length != 0;
    }
    else
    {
        return false;
    }
}

// for shared objects
/// ditto
bool doesPointTo(S, T)(auto ref const shared S source, ref const shared T target) @trusted pure nothrow
{
    return doesPointTo!(shared S, shared T, void)(source, target);
}

/// ditto
bool mayPointTo(S, T, Tdummy=void)(auto ref const S source, ref const T target) @trusted pure nothrow
if (__traits(isRef, source) || isDynamicArray!S ||
    is(S == U*, U) || is(S == class))
{
    static if (is(S == U*, U) || is(S == class) || is(S == interface))
    {
        const m = *cast(void**) &source;
        const b = cast(void*) &target;
        const e = b + target.sizeof;
        return b <= m && m < e;
    }
    else static if (is(S == struct) || is(S == union))
    {
        foreach (i, Subobj; typeof(source.tupleof))
            if (mayPointTo(source.tupleof[i], target)) return true;
        return false;
    }
    else static if (isStaticArray!S)
    {
        static if (is(S == void[n], size_t n))
        {
            static if (n >= (void[]).sizeof)
            {
                // could contain a slice, which could point at anything.
                // But a void[N] that is all 0 cannot point anywhere
                import std.algorithm.searching : any;
                if (__ctfe || any(cast(ubyte[]) source[]))
                    return true;
            }
            else static if (n >= (void*).sizeof)
            {
                // Reinterpreting cast is impossible during ctfe
                if (__ctfe)
                    return true;

                // Only check for properly aligned pointers
                enum al = (void*).alignof - 1;
                const base = cast(size_t) &source;
                const alBase = (base + al) & ~al;

                if ((n - (alBase - base)) >= (void*).sizeof &&
                    mayPointTo(*(cast(void**) alBase), target))
                    return true;
            }
        }
        else
        {
            foreach (size_t i; 0 .. S.length)
                if (mayPointTo(source[i], target)) return true;
        }

        return false;
    }
    else static if (isDynamicArray!S)
    {
        import std.array : overlap;
        return overlap(cast(void[]) source, cast(void[])(&target)[0 .. 1]).length != 0;
    }
    else
    {
        return false;
    }
}

// for shared objects
/// ditto
bool mayPointTo(S, T)(auto ref const shared S source, ref const shared T target) @trusted pure nothrow
{
    return mayPointTo!(shared S, shared T, void)(source, target);
}

/// Pointers
@system unittest
{
    int  i = 0;
    int* p = null;
    assert(!p.doesPointTo(i));
    p = &i;
    assert( p.doesPointTo(i));
}

/// Structs and Unions
@system unittest
{
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

/// Arrays (dynamic and static)
@system unittest
{
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

/// Classes
@system unittest
{
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


version (StdUnittest)
{
    // https://issues.dlang.org/show_bug.cgi?id=17084
    // the bug doesn't happen if these declarations are in the unittest block
    // (static or not).
    private struct Page17084
    {
        URL17084 url;
        int opCmp(P)(P) { return 0; }
        int opCmp(P)(shared(P)) shared { return 0; }
    }

    private struct URL17084
    {
        int[] queryParams;
        string toString()() const { return ""; }
        alias toString this;
    }
}

// https://issues.dlang.org/show_bug.cgi?id=17084
@system unittest
{
    import std.algorithm.sorting : sort;
    Page17084[] s;
    sort(s);
    shared(Page17084)[] p;
    sort(p);
}

@system unittest
{
    struct S1 { int a; S1 * b; }
    S1 a1;
    S1 * p = &a1;
    assert(doesPointTo(p, a1));

    S1 a2;
    a2.b = &a1;
    assert(doesPointTo(a2, a1));

    struct S3 { int[10] a; }
    S3 a3;
    auto a4 = a3.a[2 .. 3];
    assert(doesPointTo(a4, a3));

    auto a5 = new double[4];
    auto a6 = a5[1 .. 2];
    assert(!doesPointTo(a5, a6));

    auto a7 = new double[3];
    auto a8 = new double[][1];
    a8[0] = a7;
    assert(!doesPointTo(a8[0], a8[0]));

    // don't invoke postblit on subobjects
    {
        static struct NoCopy { this(this) { assert(0); } }
        static struct Holder { NoCopy a, b, c; }
        Holder h;
        cast(void) doesPointTo(h, h);
    }

    shared S3 sh3;
    shared sh3sub = sh3.a[];
    assert(doesPointTo(sh3sub, sh3));

    int[] darr = [1, 2, 3, 4];

    //dynamic arrays don't point to each other, or slices of themselves
    assert(!doesPointTo(darr, darr));
    assert(!doesPointTo(darr[0 .. 1], darr));

    //But they do point their elements
    foreach (i; 0 .. 4)
        assert(doesPointTo(darr, darr[i]));
    assert(doesPointTo(darr[0 .. 3], darr[2]));
    assert(!doesPointTo(darr[0 .. 3], darr[3]));
}

@system unittest
{
    //tests with static arrays
    //Static arrays themselves are just objects, and don't really *point* to anything.
    //They aggregate their contents, much the same way a structure aggregates its attributes.
    //*However* The elements inside the static array may themselves point to stuff.

    //Standard array
    int[2] k;
    assert(!doesPointTo(k, k)); //an array doesn't point to itself
    //Technically, k doesn't point its elements, although it does alias them
    assert(!doesPointTo(k, k[0]));
    assert(!doesPointTo(k, k[1]));
    //But an extracted slice will point to the same array.
    assert(doesPointTo(k[], k));
    assert(doesPointTo(k[], k[1]));

    //An array of pointers
    int*[2] pp;
    int a;
    int b;
    pp[0] = &a;
    assert( doesPointTo(pp, a));  //The array contains a pointer to a
    assert(!doesPointTo(pp, b));  //The array does NOT contain a pointer to b
    assert(!doesPointTo(pp, pp)); //The array does not point itslef

    //A struct containing a static array of pointers
    static struct S
    {
        int*[2] p;
    }
    S s;
    s.p[0] = &a;
    assert( doesPointTo(s, a)); //The struct contains an array that points a
    assert(!doesPointTo(s, b)); //But doesn't point b
    assert(!doesPointTo(s, s)); //The struct doesn't actually point itslef.

    //An array containing structs that have pointers
    static struct SS
    {
        int* p;
    }
    SS[2] ss = [SS(&a), SS(null)];
    assert( doesPointTo(ss, a));  //The array contains a struct that points to a
    assert(!doesPointTo(ss, b));  //The array doesn't contains a struct that points to b
    assert(!doesPointTo(ss, ss)); //The array doesn't point itself.

    // https://issues.dlang.org/show_bug.cgi?id=20426
    align((void*).alignof) void[32] voidArr = void;
    (cast(void*[]) voidArr[])[] = null; // Ensure no false pointers

    // zeroed void ranges can't point at anything
    assert(!mayPointTo(voidArr, a));
    assert(!mayPointTo(voidArr, b));

    *cast(void**) &voidArr[16] = &a; // Pointers should be found

    alias SA = void[size_t.sizeof + 3];
    SA *smallArr1 = cast(SA*)&voidArr;
    SA *smallArr2 = cast(SA*)&(voidArr[16]);

    // But it should only consider properly aligned pointers
    // Write single bytes to avoid issues due to misaligned writes
    void*[1] tmp = [&b];
    (cast(ubyte[]) voidArr[3 .. 3 + (void*).sizeof])[] = cast(ubyte[]) tmp[];


    assert( mayPointTo(*smallArr2, a));
    assert(!mayPointTo(*smallArr1, b));

    assert(!doesPointTo(voidArr, a)); // Value might be a false pointer
    assert(!doesPointTo(voidArr, b));

    SA *smallArr3 = cast(SA *) &voidArr[13]; // Works for weird sizes/alignments
    assert( mayPointTo(*smallArr3, a));
    assert(!mayPointTo(*smallArr3, b));

    assert(!doesPointTo(*smallArr3, a));
    assert(!doesPointTo(*smallArr3, b));

    auto v3 = cast(void[3]*) &voidArr[16]; // Arrays smaller than pointers are ignored
    assert(!mayPointTo(*v3, a));
    assert(!mayPointTo(*v3, b));

    assert(!doesPointTo(*v3, a));
    assert(!doesPointTo(*v3, b));

    assert(mayPointTo(voidArr, a)); // slice-contiaining void[N] might point at anything
    assert(mayPointTo(voidArr, b));

    static assert(() {
        void[16] arr1 = void;
        void[size_t.sizeof] arr2 = void;
        int var;
        return mayPointTo(arr1, var) && !doesPointTo(arr1, var) &&
               mayPointTo(arr2, var) && !doesPointTo(arr2, var);
    }());
}


@system unittest //Unions
{
    int i;
    union U //Named union
    {
        size_t asInt = 0;
        int*   asPointer;
    }
    struct S
    {
        union //Anonymous union
        {
            size_t asInt = 0;
            int*   asPointer;
        }
    }

    U u;
    S s;
    assert(!doesPointTo(u, i));
    assert(!doesPointTo(s, i));
    assert(!mayPointTo(u, i));
    assert(!mayPointTo(s, i));

    u.asPointer = &i;
    s.asPointer = &i;
    assert(!doesPointTo(u, i));
    assert(!doesPointTo(s, i));
    assert( mayPointTo(u, i));
    assert( mayPointTo(s, i));

    u.asInt = cast(size_t)&i;
    s.asInt = cast(size_t)&i;
    assert(!doesPointTo(u, i));
    assert(!doesPointTo(s, i));
    assert( mayPointTo(u, i));
    assert( mayPointTo(s, i));
}

@system unittest //Classes
{
    int i;
    static class A
    {
        int* p;
    }
    A a = new A, b = a;
    assert(!doesPointTo(a, b)); //a does not point to b
    a.p = &i;
    assert(!doesPointTo(a, i)); //a does not point to i
}
@safe unittest //alias this test
{
    static int i;
    static int j;
    struct S
    {
        int* p;
        @property int* foo(){return &i;}
        alias foo this;
    }
    assert(is(S : int*));
    S s = S(&j);
    assert(!doesPointTo(s, i));
    assert( doesPointTo(s, j));
    assert( doesPointTo(cast(int*) s, i));
    assert(!doesPointTo(cast(int*) s, j));
}

/+
Returns true if the field at index `i` in ($D T) shares its address with another field.

Note: This does not merelly check if the field is a member of an union, but also that
it is not a single child.
+/
package enum isUnionAliased(T, size_t i) = isUnionAliasedImpl!T(T.tupleof[i].offsetof);
private bool isUnionAliasedImpl(T)(size_t offset)
{
    int count = 0;
    foreach (i, U; typeof(T.tupleof))
        if (T.tupleof[i].offsetof == offset)
            ++count;
    return count >= 2;
}
//
@safe unittest
{
    static struct S
    {
        int a0; //Not aliased
        union
        {
            int a1; //Not aliased
        }
        union
        {
            int a2; //Aliased
            int a3; //Aliased
        }
        union A4
        {
            int b0; //Not aliased
        }
        A4 a4;
        union A5
        {
            int b0; //Aliased
            int b1; //Aliased
        }
        A5 a5;
    }

    static assert(!isUnionAliased!(S, 0)); //a0;
    static assert(!isUnionAliased!(S, 1)); //a1;
    static assert( isUnionAliased!(S, 2)); //a2;
    static assert( isUnionAliased!(S, 3)); //a3;
    static assert(!isUnionAliased!(S, 4)); //a4;
        static assert(!isUnionAliased!(S.A4, 0)); //a4.b0;
    static assert(!isUnionAliased!(S, 5)); //a5;
        static assert( isUnionAliased!(S.A5, 0)); //a5.b0;
        static assert( isUnionAliased!(S.A5, 1)); //a5.b1;
}

version (CRuntime_Glibc) version = GNU_STRERROR;
version (CRuntime_UClibc) version = GNU_STRERROR;

package string errnoString(int errno) nothrow @trusted
{
    import core.stdc.string : strlen;
    version (GNU_STRERROR)
    {
        import core.stdc.string : strerror_r;
        char[1024] buf = void;
        auto s = strerror_r(errno, buf.ptr, buf.length);
    }
    else version (Posix)
    {
        // XSI-compliant
        import core.stdc.string : strerror_r;
        char[1024] buf = void;
        const(char)* s;
        if (strerror_r(errno, buf.ptr, buf.length) == 0)
            s = buf.ptr;
        else
            return "Unknown error";
    }
    else
    {
        import core.stdc.string : strerror;
        auto s = strerror(errno);
    }
    return s[0 .. s.strlen].idup;
}

/*********************
 * Thrown if errors that set `errno` occur.
 */
class ErrnoException : Exception
{
    /// Operating system error code.
    final @property uint errno() nothrow pure scope @nogc @safe { return _errno; }
    private uint _errno;
    /// Constructor which takes an error message. The current global $(REF errno, core,stdc,errno) value is used as error code.
    this(string msg, string file = null, size_t line = 0) @safe
    {
        import core.stdc.errno : errno;
        this(msg, errno, file, line);
    }
    /// Constructor which takes an error message and error code.
    this(string msg, int errno, string file = null, size_t line = 0) @safe
    {
        _errno = errno;
        super(msg ~ " (" ~ errnoString(errno) ~ ")", file, line);
    }
}

///
@safe unittest
{
    import core.stdc.errno : EAGAIN;
    auto ex = new ErrnoException("oh no", EAGAIN);
    assert(ex.errno == EAGAIN);
}

/// errno is used by default if no explicit error code is provided
@safe unittest
{
    import core.stdc.errno : errno, EAGAIN;

    auto old = errno;
    scope(exit) errno = old;

    // fake that errno got set by the callee
    errno = EAGAIN;
    auto ex = new ErrnoException("oh no");
    assert(ex.errno == EAGAIN);
}

/++
    ML-style functional exception handling. Runs the supplied expression and
    returns its result. If the expression throws a `Throwable`, runs the
    supplied error handler instead and return its result. The error handler's
    type must be the same as the expression's type.

    Params:
        E            = The type of `Throwable`s to catch. Defaults to `Exception`
        T1           = The type of the expression.
        T2           = The return type of the error handler.
        expression   = The expression to run and return its result.
        errorHandler = The handler to run if the expression throwed.

    Returns:
        expression, if it does not throw. Otherwise, returns the result of
        errorHandler.
+/
//lazy version
CommonType!(T1, T2) ifThrown(E : Throwable = Exception, T1, T2)(lazy scope T1 expression, lazy scope T2 errorHandler)
{
    static assert(!is(typeof(return) == void),
        "The error handler's return value("
        ~ T2.stringof ~
        ") does not have a common type with the expression("
        ~ T1.stringof ~
        ")."
    );
    try
    {
        return expression();
    }
    catch (E)
    {
        return errorHandler();
    }
}

///ditto
//delegate version
CommonType!(T1, T2) ifThrown(E : Throwable, T1, T2)(lazy scope T1 expression, scope T2 delegate(E) errorHandler)
{
    static assert(!is(typeof(return) == void),
        "The error handler's return value("
        ~ T2.stringof ~
        ") does not have a common type with the expression("
        ~ T1.stringof ~
        ")."
    );
    try
    {
        return expression();
    }
    catch (E e)
    {
        return errorHandler(e);
    }
}

///ditto
//delegate version, general overload to catch any Exception
CommonType!(T1, T2) ifThrown(T1, T2)(lazy scope T1 expression, scope T2 delegate(Exception) errorHandler)
{
    static assert(!is(typeof(return) == void),
        "The error handler's return value("
        ~ T2.stringof ~
        ") does not have a common type with the expression("
        ~ T1.stringof ~
        ")."
    );
    try
    {
        return expression();
    }
    catch (Exception e)
    {
        return errorHandler(e);
    }
}

/// Revert to a default value upon an error:
@safe unittest
{
    import std.conv : to;
    assert("x".to!int.ifThrown(0) == 0);
}

/**
Chain multiple calls to ifThrown, each capturing errors from the
entire preceding expression.
*/
@safe unittest
{
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

/**
The expression and the errorHandler must have a common type they can both
be implicitly casted to, and that type will be the type of the compound
expression.
*/
@safe unittest
{
    // null and new Object have a common type(Object).
    static assert(is(typeof(null.ifThrown(new Object())) == Object));
    static assert(is(typeof((new Object()).ifThrown(null)) == Object));

    // 1 and new Object do not have a common type.
    static assert(!__traits(compiles, 1.ifThrown(new Object())));
    static assert(!__traits(compiles, (new Object()).ifThrown(1)));
}

/// Use a lambda to get the thrown object.
@system unittest
{
    import std.format : format;
    assert("%s".format.ifThrown!Exception(e => e.classinfo.name) == "std.format.FormatException");
}

//Verify Examples
@system unittest
{
    import std.conv;
    import std.string;
    //Revert to a default value upon an error:
    assert("x".to!int().ifThrown(0) == 0);

    //Chaining multiple calls to ifThrown to attempt multiple things in a row:
    string s="true";
    assert(s.to!int().
            ifThrown(cast(int) s.to!double()).
            ifThrown(cast(int) s.to!bool())
            == 1);

    //Respond differently to different types of errors
    assert(enforce("x".to!int() < 1).to!string()
            .ifThrown!ConvException("not a number")
            .ifThrown!Exception("number too small")
            == "not a number");

    //null and new Object have a common type(Object).
    static assert(is(typeof(null.ifThrown(new Object())) == Object));
    static assert(is(typeof((new Object()).ifThrown(null)) == Object));

    //1 and new Object do not have a common type.
    static assert(!__traits(compiles, 1.ifThrown(new Object())));
    static assert(!__traits(compiles, (new Object()).ifThrown(1)));

    //Use a lambda to get the thrown object.
    assert("%s".format().ifThrown(e => e.classinfo.name) == "std.format.FormatException");
}

@system unittest
{
    import core.exception;
    import std.conv;
    import std.string;
    //Basic behaviour - all versions.
    assert("1".to!int().ifThrown(0) == 1);
    assert("x".to!int().ifThrown(0) == 0);
    assert("1".to!int().ifThrown!ConvException(0) == 1);
    assert("x".to!int().ifThrown!ConvException(0) == 0);
    assert("1".to!int().ifThrown(e=>0) == 1);
    assert("x".to!int().ifThrown(e=>0) == 0);
    static if (__traits(compiles, 0.ifThrown!Exception(e => 0))) //This will only work with a fix that was not yet pulled
    {
        assert("1".to!int().ifThrown!ConvException(e=>0) == 1);
        assert("x".to!int().ifThrown!ConvException(e=>0) == 0);
    }

    //Exceptions other than stated not caught.
    assert("x".to!int().ifThrown!StringException(0).collectException!ConvException() !is null);
    static if (__traits(compiles, 0.ifThrown!Exception(e => 0))) //This will only work with a fix that was not yet pulled
    {
        assert("x".to!int().ifThrown!StringException(e=>0).collectException!ConvException() !is null);
    }

    //Default does not include errors.
    int throwRangeError() { throw new RangeError; }
    assert(throwRangeError().ifThrown(0).collectException!RangeError() !is null);
    assert(throwRangeError().ifThrown(e=>0).collectException!RangeError() !is null);

    //Incompatible types are not accepted.
    static assert(!__traits(compiles, 1.ifThrown(new Object())));
    static assert(!__traits(compiles, (new Object()).ifThrown(1)));
    static assert(!__traits(compiles, 1.ifThrown(e=>new Object())));
    static assert(!__traits(compiles, (new Object()).ifThrown(e=>1)));
}

version (StdUnittest) package
void assertCTFEable(alias dg)()
{
    static assert({ cast(void) dg(); return true; }());
    cast(void) dg();
}

/** This `enum` is used to select the primitives of the range to handle by the
  $(LREF handle) range wrapper. The values of the `enum` can be `OR`'d to
  select multiple primitives to be handled.

  `RangePrimitive.access` is a shortcut for the access primitives; `front`,
  `back` and `opIndex`.

  `RangePrimitive.pop` is a shortcut for the mutating primitives;
  `popFront` and `popBack`.
 */
enum RangePrimitive
{
    front    = 0b00_0000_0001, ///
    back     = 0b00_0000_0010, /// Ditto
    popFront = 0b00_0000_0100, /// Ditto
    popBack  = 0b00_0000_1000, /// Ditto
    empty    = 0b00_0001_0000, /// Ditto
    save     = 0b00_0010_0000, /// Ditto
    length   = 0b00_0100_0000, /// Ditto
    opDollar = 0b00_1000_0000, /// Ditto
    opIndex  = 0b01_0000_0000, /// Ditto
    opSlice  = 0b10_0000_0000, /// Ditto
    access   = front | back | opIndex, /// Ditto
    pop      = popFront | popBack, /// Ditto
}

///
pure @safe unittest
{
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

///
pure @safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : retro;
    import std.utf : UTFException;

    auto str = "hello\xFFworld"; // 0xFF is an invalid UTF-8 code unit

    auto handled = str.handle!(UTFException, RangePrimitive.access,
            (e, r) => ' '); // Replace invalid code points with spaces

    assert(handled.equal("hello world")); // `front` is handled,
    assert(handled.retro.equal("dlrow olleh")); // as well as `back`
}

/** Handle exceptions thrown from range primitives.

Use the $(LREF RangePrimitive) enum to specify which primitives to _handle.
Multiple range primitives can be handled at once by using the `OR` operator
or the pseudo-primitives `RangePrimitive.access` and `RangePrimitive.pop`.
All handled primitives must have return types or values compatible with the
user-supplied handler.

Params:
    E = The type of `Throwable` to _handle.
    primitivesToHandle = Set of range primitives to _handle.
    handler = The callable that is called when a handled primitive throws a
    `Throwable` of type `E`. The handler must accept arguments of
    the form $(D E, ref IRange) and its return value is used as the primitive's
    return value whenever `E` is thrown. For `opIndex`, the handler can
    optionally recieve a third argument; the index that caused the exception.
    input = The range to _handle.

Returns: A wrapper `struct` that preserves the range interface of `input`.

Note:
Infinite ranges with slicing support must return an instance of
$(REF Take, std,range) when sliced with a specific lower and upper
bound (see $(REF hasSlicing, std,range,primitives)); `handle` deals with
this by `take`ing 0 from the return value of the handler function and
returning that when an exception is caught.
*/
auto handle(E : Throwable, RangePrimitive primitivesToHandle, alias handler, Range)(Range input)
if (isInputRange!Range)
{
    static struct Handler
    {
        private Range range;

        static if (isForwardRange!Range)
        {
            @property typeof(this) save()
            {
                static if (primitivesToHandle & RangePrimitive.save)
                {
                    try
                    {
                        return typeof(this)(range.save);
                    }
                    catch (E exception)
                    {
                        return typeof(this)(handler(exception, this.range));
                    }
                }
                else
                    return typeof(this)(range.save);
            }
        }

        static if (isInfinite!Range)
        {
            enum bool empty = false;
        }
        else
        {
            @property bool empty()
            {
                static if (primitivesToHandle & RangePrimitive.empty)
                {
                    try
                    {
                        return this.range.empty;
                    }
                    catch (E exception)
                    {
                        return handler(exception, this.range);
                    }
                }
                else
                    return this.range.empty;
            }
        }

        @property auto ref front()
        {
            static if (primitivesToHandle & RangePrimitive.front)
            {
                try
                {
                    return this.range.front;
                }
                catch (E exception)
                {
                    return handler(exception, this.range);
                }
            }
            else
                return this.range.front;
        }

        void popFront()
        {
            static if (primitivesToHandle & RangePrimitive.popFront)
            {
                try
                {
                    this.range.popFront();
                }
                catch (E exception)
                {
                    handler(exception, this.range);
                }
            }
            else
                this.range.popFront();
        }

        static if (isBidirectionalRange!Range)
        {
            @property auto ref back()
            {
                static if (primitivesToHandle & RangePrimitive.back)
                {
                    try
                    {
                        return this.range.back;
                    }
                    catch (E exception)
                    {
                        return handler(exception, this.range);
                    }
                }
                else
                    return this.range.back;
            }

            void popBack()
            {
                static if (primitivesToHandle & RangePrimitive.popBack)
                {
                    try
                    {
                        this.range.popBack();
                    }
                    catch (E exception)
                    {
                        handler(exception, this.range);
                    }
                }
                else
                    this.range.popBack();
            }
        }

        static if (isRandomAccessRange!Range)
        {
            auto ref opIndex(size_t index)
            {
                static if (primitivesToHandle & RangePrimitive.opIndex)
                {
                    try
                    {
                        return this.range[index];
                    }
                    catch (E exception)
                    {
                        static if (__traits(compiles, handler(exception, this.range, index)))
                            return handler(exception, this.range, index);
                        else
                            return handler(exception, this.range);
                    }
                }
                else
                    return this.range[index];
            }
        }

        static if (hasLength!Range)
        {
            @property auto length()
            {
                static if (primitivesToHandle & RangePrimitive.length)
                {
                    try
                    {
                        return this.range.length;
                    }
                    catch (E exception)
                    {
                        return handler(exception, this.range);
                    }
                }
                else
                    return this.range.length;
            }
        }

        static if (hasSlicing!Range)
        {
            static if (hasLength!Range)
            {
                typeof(this) opSlice(size_t lower, size_t upper)
                {
                    static if (primitivesToHandle & RangePrimitive.opSlice)
                    {
                        try
                        {
                            return typeof(this)(this.range[lower .. upper]);
                        }
                        catch (E exception)
                        {
                            return typeof(this)(handler(exception, this.range));
                        }
                    }
                    else
                        return typeof(this)(this.range[lower .. upper]);
                }
            }
            else static if (is(typeof(Range.init[size_t.init .. $])))
            {
                import std.range : Take, takeExactly;
                static struct DollarToken {}
                enum opDollar = DollarToken.init;

                typeof(this) opSlice(size_t lower, DollarToken)
                {
                    static if (primitivesToHandle & RangePrimitive.opSlice)
                    {
                        try
                        {
                            return typeof(this)(this.range[lower .. $]);
                        }
                        catch (E exception)
                        {
                            return typeof(this)(handler(exception, this.range));
                        }
                    }
                    else
                        return typeof(this)(this.range[lower .. $]);
                }

                Take!Handler opSlice(size_t lower, size_t upper)
                {
                    static if (primitivesToHandle & RangePrimitive.opSlice)
                    {
                        try
                        {
                            return takeExactly(typeof(this)(this.range[lower .. $]), upper - 1);
                        }
                        catch (E exception)
                        {
                            return takeExactly(typeof(this)(handler(exception, this.range)), 0);
                        }
                    }
                    else
                        return takeExactly(typeof(this)(this.range[lower .. $]), upper - 1);
                }
            }
        }
    }

    return Handler(input);
}

///
pure @safe unittest
{
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

///
pure @safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : retro;
    import std.utf : UTFException;

    auto str = "hello\xFFworld"; // 0xFF is an invalid UTF-8 code unit

    auto handled = str.handle!(UTFException, RangePrimitive.access,
            (e, r) => ' '); // Replace invalid code points with spaces

    assert(handled.equal("hello world")); // `front` is handled,
    assert(handled.retro.equal("dlrow olleh")); // as well as `back`
}

pure nothrow @safe unittest
{
    static struct ThrowingRange
    {
        pure @safe:
        @property bool empty()
        {
            throw new Exception("empty has thrown");
        }

        @property int front()
        {
            throw new Exception("front has thrown");
        }

        @property int back()
        {
            throw new Exception("back has thrown");
        }

        void popFront()
        {
            throw new Exception("popFront has thrown");
        }

        void popBack()
        {
            throw new Exception("popBack has thrown");
        }

        int opIndex(size_t)
        {
            throw new Exception("opIndex has thrown");
        }

        ThrowingRange opSlice(size_t, size_t)
        {
            throw new Exception("opSlice has thrown");
        }

        @property size_t length()
        {
            throw new Exception("length has thrown");
        }

        alias opDollar = length;

        @property ThrowingRange save()
        {
            throw new Exception("save has thrown");
        }
    }

    static assert(isInputRange!ThrowingRange);
    static assert(isForwardRange!ThrowingRange);
    static assert(isBidirectionalRange!ThrowingRange);
    static assert(hasSlicing!ThrowingRange);
    static assert(hasLength!ThrowingRange);

    auto f = ThrowingRange();
    auto fb = f.handle!(Exception, RangePrimitive.front | RangePrimitive.back,
            (e, r) => -1)();
    assert(fb.front == -1);
    assert(fb.back == -1);
    assertThrown(fb.popFront());
    assertThrown(fb.popBack());
    assertThrown(fb.empty);
    assertThrown(fb.save);
    assertThrown(fb[0]);

    auto accessRange = f.handle!(Exception, RangePrimitive.access,
            (e, r) => -1);
    assert(accessRange.front == -1);
    assert(accessRange.back == -1);
    assert(accessRange[0] == -1);
    assertThrown(accessRange.popFront());
    assertThrown(accessRange.popBack());

    auto pfb = f.handle!(Exception, RangePrimitive.pop, (e, r) => -1)();

    pfb.popFront(); // this would throw otherwise
    pfb.popBack(); // this would throw otherwise

    auto em = f.handle!(Exception,
            RangePrimitive.empty, (e, r) => false)();

    assert(!em.empty);

    auto arr = f.handle!(Exception,
            RangePrimitive.opIndex, (e, r) => 1337)();

    assert(arr[0] == 1337);

    auto arr2 = f.handle!(Exception,
            RangePrimitive.opIndex, (e, r, i) => i)();

    assert(arr2[0] == 0);
    assert(arr2[1337] == 1337);

    auto save = f.handle!(Exception,
        RangePrimitive.save,
        function(Exception e, ref ThrowingRange r) {
            return ThrowingRange();
        })();

    save.save;

    auto slice = f.handle!(Exception,
        RangePrimitive.opSlice, (e, r) => ThrowingRange())();

    auto sliced = slice[0 .. 1337]; // this would throw otherwise

    static struct Infinite
    {
        import std.range : Take;
        pure @safe:
        enum bool empty = false;
        int front() { assert(false); }
        void popFront() { assert(false); }
        Infinite save() @property { assert(false); }
        static struct DollarToken {}
        enum opDollar = DollarToken.init;
        Take!Infinite opSlice(size_t, size_t) { assert(false); }
        Infinite opSlice(size_t, DollarToken)
        {
            throw new Exception("opSlice has thrown");
        }
    }

    static assert(isInputRange!Infinite);
    static assert(isInfinite!Infinite);
    static assert(hasSlicing!Infinite);

    assertThrown(Infinite()[0 .. $]);

    auto infinite = Infinite.init.handle!(Exception,
        RangePrimitive.opSlice, (e, r) => Infinite())();

    auto infSlice = infinite[0 .. $]; // this would throw otherwise
}


/++
    Convenience mixin for trivially sub-classing exceptions

    Even trivially sub-classing an exception involves writing boilerplate code
    for the constructor to: 1$(RPAREN) correctly pass in the source file and line number
    the exception was thrown from; 2$(RPAREN) be usable with $(LREF enforce) which
    expects exception constructors to take arguments in a fixed order. This
    mixin provides that boilerplate code.

    Note however that you need to mark the $(B mixin) line with at least a
    minimal (i.e. just $(B ///)) DDoc comment if you want the mixed-in
    constructors to be documented in the newly created Exception subclass.

    $(RED Current limitation): Due to
    $(LINK2 https://issues.dlang.org/show_bug.cgi?id=11500, bug #11500),
    currently the constructors specified in this mixin cannot be overloaded with
    any other custom constructors. Thus this mixin can currently only be used
    when no such custom constructors need to be explicitly specified.
 +/
mixin template basicExceptionCtors()
{
    /++
        Params:
            msg  = The message for the exception.
            file = The file where the exception occurred.
            line = The line number where the exception occurred.
            next = The previous exception in the chain of exceptions, if any.
    +/
    this(string msg, string file = __FILE__, size_t line = __LINE__,
         Throwable next = null) @nogc @safe pure nothrow
    {
        super(msg, file, line, next);
    }

    /++
        Params:
            msg  = The message for the exception.
            next = The previous exception in the chain of exceptions.
            file = The file where the exception occurred.
            line = The line number where the exception occurred.
    +/
    this(string msg, Throwable next, string file = __FILE__,
         size_t line = __LINE__) @nogc @safe pure nothrow
    {
        super(msg, file, line, next);
    }
}

///
@safe unittest
{
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
    class TestException : Exception { mixin basicExceptionCtors; }
    auto e = new Exception("msg");
    auto te1 = new TestException("foo");
    auto te2 = new TestException("foo", e);
}

@safe unittest
{
    class TestException : Exception { mixin basicExceptionCtors; }
    auto e = new Exception("!!!");

    auto te1 = new TestException("message", "file", 42, e);
    assert(te1.msg == "message");
    assert(te1.file == "file");
    assert(te1.line == 42);
    assert(te1.next is e);

    auto te2 = new TestException("message", e, "file", 42);
    assert(te2.msg == "message");
    assert(te2.file == "file");
    assert(te2.line == 42);
    assert(te2.next is e);

    auto te3 = new TestException("foo");
    assert(te3.msg == "foo");
    assert(te3.file == __FILE__);
    assert(te3.line == __LINE__ - 3);
    assert(te3.next is null);

    auto te4 = new TestException("foo", e);
    assert(te4.msg == "foo");
    assert(te4.file == __FILE__);
    assert(te4.line == __LINE__ - 3);
    assert(te4.next is e);
}

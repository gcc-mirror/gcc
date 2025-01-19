// Written in the D programming language.

/**
This is a submodule of $(MREF std, format).

It provides two functions for reading formatted input: $(LREF
unformatValue) and $(LREF formattedRead). The former reads a single
value. The latter reads several values at once and matches the
characters found between format specifiers.

Parameters are ignored, except for the ones consisting of a single
$(B '*'). See $(LREF formattedRead) for more information.

A space outside of a format specifier has a special meaning: it
matches any sequence of whitespace characters, not just a single
space.

The following combinations of format characters and types are
available:

$(BOOKTABLE ,
$(TR $(TH) $(TH s) $(TH c) $(TH d, u, b, o, x, X) $(TH e, E, f, g, G) $(TH r) $(TH compound))
$(TR $(TD `bool`) $(TD yes) $(TD $(MDASH)) $(TD yes) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)))
$(TR $(TD `null`) $(TD yes) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)))
$(TR $(TD $(I integer)) $(TD yes) $(TD $(MDASH)) $(TD yes) $(TD $(MDASH)) $(TD yes) $(TD $(MDASH)))
$(TR $(TD $(I floating point)) $(TD yes) $(TD $(MDASH)) $(TD $(MDASH)) $(TD yes) $(TD yes) $(TD $(MDASH)))
$(TR $(TD $(I character)) $(TD yes) $(TD yes) $(TD yes) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)))
$(TR $(TD $(I string)) $(TD yes) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD yes))
$(TR $(TD $(I array)) $(TD yes) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD yes))
$(TR $(TD $(I associative array)) $(TD yes) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD yes))
)

Below are highlighted examples on how these combinations are used
with $(LREF unformatValue), however, they apply for $(LREF
formattedRead) also

Copyright: Copyright The D Language Foundation 2000-2013.

License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Authors: $(HTTP walterbright.com, Walter Bright), $(HTTP erdani.com,
Andrei Alexandrescu), and Kenji Hara

Source: $(PHOBOSSRC std/format/read.d)
 */
module std.format.read;

/// Booleans
@safe pure unittest
{
    import std.format.spec : singleSpec;

    auto str = "false";
    auto spec = singleSpec("%s");
    assert(str.unformatValue!bool(spec) == false);

    str = "1";
    spec = singleSpec("%d");
    assert(str.unformatValue!bool(spec) == true);
}

/// Null values
@safe pure unittest
{
    import std.format.spec : singleSpec;

    auto str = "null";
    auto spec = singleSpec("%s");
    assert(str.unformatValue!(typeof(null))(spec) == null);
}

/// Integrals
@safe pure unittest
{
    import std.format.spec : singleSpec;

    // signed decimal values
    auto str = "123";
    auto spec = singleSpec("%s");
    assert(str.unformatValue!int(spec) == 123);

    // hexadecimal values
    str = "ABC";
    spec = singleSpec("%X");
    assert(str.unformatValue!int(spec) == 2748);

    // octal values
    str = "11610";
    spec = singleSpec("%o");
    assert(str.unformatValue!int(spec) == 5000);

    // raw read, depends on endianess
    str = "\x75\x01";
    spec = singleSpec("%r");
    auto result = str.unformatValue!short(spec);
    assert(result == 373 /* little endian */ || result == 29953 /* big endian */ );
}

/// Floating point numbers
@safe pure unittest
{
    import std.format.spec : singleSpec;
    import std.math.operations : isClose;

    // natural notation
    auto str = "123.456";
    auto spec = singleSpec("%s");
    assert(str.unformatValue!double(spec).isClose(123.456));

    // scientific notation
    str = "1e17";
    spec = singleSpec("%e");
    assert(str.unformatValue!double(spec).isClose(1e17));

    // raw read, depends on endianess
    str = "\x40\x00\x00\xBF";
    spec = singleSpec("%r");
    auto result = str.unformatValue!float(spec);
    assert(isClose(result, -0.5) /* little endian */ || isClose(result, 2.0) /* big endian */ );
}

/// Characters
@safe pure unittest
{
    import std.format.spec : singleSpec;

    // only the first character is read
    auto str = "abc";
    auto spec = singleSpec("%s");
    assert(str.unformatValue!char(spec) == 'a');

    // using a numerical format character treats the read number as unicode code point
    str = "65";
    spec = singleSpec("%d");
    assert(str.unformatValue!char(spec) == 'A');

    str = "41";
    spec = singleSpec("%x");
    assert(str.unformatValue!char(spec) == 'A');

    str = "10003";
    spec = singleSpec("%d");
    assert(str.unformatValue!dchar(spec) == '✓');
}

/// Arrays
@safe pure unittest
{
    import std.format.spec : singleSpec;

    // string value
    string str = "aaa";
    auto spec = singleSpec("%s");
    assert(str.unformatValue!(dchar[])(spec) == "aaa"d);

    // fixed size array with characters
    str = "aaa";
    spec = singleSpec("%s");
    dchar[3] ret = ['a', 'a', 'a'];
    assert(str.unformatValue!(dchar[3])(spec) == ret);

    // dynamic array
    str = "[1, 2, 3, 4]";
    spec = singleSpec("%s");
    assert(str.unformatValue!(int[])(spec) == [1, 2, 3, 4]);

    // fixed size array with integers
    str = "[1, 2, 3, 4]";
    spec = singleSpec("%s");
    int[4] ret2 = [1, 2, 3, 4];
    assert(str.unformatValue!(int[4])(spec) == ret2);

    // compound specifiers can be used for more control
    str = "1,2,3";
    spec = singleSpec("%(%s,%)");
    assert(str.unformatValue!(int[])(spec) == [1, 2, 3]);

    str = "cool";
    spec = singleSpec("%(%c%)");
    assert(str.unformatValue!(char[])(spec) == ['c', 'o', 'o', 'l']);
}

/// Associative arrays
@safe pure unittest
{
    import std.format.spec : singleSpec;

    // as single value
    auto str = `["one": 1, "two": 2]`;
    auto spec = singleSpec("%s");
    assert(str.unformatValue!(int[string])(spec) == ["one": 1, "two": 2]);

    // with compound specifier for more control
    str = "1/1, 2/4, 3/9";
    spec = singleSpec("%(%d/%d%|, %)");
    assert(str.unformatValue!(int[int])(spec) == [1: 1, 2: 4, 3: 9]);
}

import std.format.spec : FormatSpec;
import std.format.internal.read;
import std.meta : allSatisfy;
import std.traits : isSomeString, isType;

/**
Reads an input range according to a format string and stores the read
values into its arguments.

Format specifiers with format character $(B 'd'), $(B 'u') and $(B
'c') can take a $(B '*') parameter for skipping values.

The second version of `formattedRead` takes the format string as
template argument. In this case, it is checked for consistency at
compile-time.

Params:
    r = an $(REF_ALTTEXT input range, isInputRange, std, range, primitives),
        where the formatted input is read from
    fmt = a $(MREF_ALTTEXT format string, std,format)
    args = a variadic list of arguments where the read values are stored
    Range = the type of the input range `r`
    Char = the character type used for `fmt`
    Args = a variadic list of types of the arguments

Returns:
    The number of variables filled. If the input range `r` ends early,
    this number will be less than the number of variables provided.

Throws:
    A $(REF_ALTTEXT FormatException, FormatException, std, format)
    if reading did not succeed.

Note:
    For backward compatibility the arguments `args` can be given as pointers
    to that variable, but it is not recommended to do so, because this
    option might be removed in the future.
 */
uint formattedRead(Range, Char, Args...)(auto ref Range r, const(Char)[] fmt, auto ref Args args)
{
    import std.format : enforceFmt;
    import std.range.primitives : empty;
    import std.traits : isPointer;
    import std.typecons : isTuple;

    auto spec = FormatSpec!Char(fmt);
    static if (!Args.length)
    {
        spec.readUpToNextSpec(r);
        enforceFmt(spec.trailing.empty, "Trailing characters in formattedRead format string");
        return 0;
    }
    else
    {
        enum hasPointer = isPointer!(typeof(args[0]));

        // The function below accounts for '*' == fields meant to be
        // read and skipped
        void skipUnstoredFields()
        {
            for (;;)
            {
                spec.readUpToNextSpec(r);
                if (spec.width != spec.DYNAMIC) break;
                // must skip this field
                skipData(r, spec);
            }
        }

        skipUnstoredFields();
        if (r.empty)
        {
            // Input is empty, nothing to read
            return 0;
        }

        static if (hasPointer)
            alias A = typeof(*args[0]);
        else
            alias A = typeof(args[0]);

        static if (isTuple!A)
        {
            foreach (i, T; A.Types)
            {
                static if (hasPointer)
                    (*args[0])[i] = unformatValue!(T)(r, spec);
                else
                    args[0][i] = unformatValue!(T)(r, spec);
                skipUnstoredFields();
            }
        }
        else
        {
            static if (hasPointer)
                *args[0] = unformatValue!(A)(r, spec);
            else
                args[0] = unformatValue!(A)(r, spec);
        }
        return 1 + formattedRead(r, spec.trailing, args[1 .. $]);
    }
}

/// ditto
uint formattedRead(alias fmt, Range, Args...)(auto ref Range r, auto ref Args args)
if (!isType!fmt && isSomeString!(typeof(fmt)))
{
    import std.format : checkFormatException;
    import std.meta : staticMap;
    import std.typecons : Tuple;


    // formattedRead supports std.typecons.Tuple
    // however, checkFormatException does not
    // this means that all std.typecons.Tuple's types in Args must be unwrapped
    // and passed to checkFormatException
    template Flatten(T)
    {
        static if (is(T : Tuple!Args, Args...))
            alias Flatten = Args;
        else
            alias Flatten = T;
    }

    alias e = checkFormatException!(fmt, staticMap!(Flatten, Args));
    static assert(!e, e);
    return .formattedRead(r, fmt, args);
}

///
@safe pure unittest
{
    string object;
    char cmp;
    int value;

    assert(formattedRead("angle < 36", "%s %c %d", object, cmp, value) == 3);
    assert(object == "angle");
    assert(cmp == '<');
    assert(value == 36);

    // reading may end early:
    assert(formattedRead("length >", "%s %c %d", object, cmp, value) == 2);
    assert(object == "length");
    assert(cmp == '>');
    // value is not changed:
    assert(value == 36);
}

/// The format string can be checked at compile-time:
@safe pure unittest
{
    string a;
    int b;
    double c;

    assert("hello!124:34.5".formattedRead!"%s!%s:%s"(a, b, c) == 3);
    assert(a == "hello");
    assert(b == 124);
    assert(c == 34.5);
}

/// Skipping values
@safe pure unittest
{
    string item;
    double amount;

    assert("orange: (12%) 15.25".formattedRead("%s: (%*d%%) %f", item, amount) == 2);
    assert(item == "orange");
    assert(amount == 15.25);

    // can also be used with tuples
    import std.typecons : Tuple;

    Tuple!(int, float) t;
    char[] line = "1 7643 2.125".dup;
    formattedRead(line, "%s %*u %s", t);
    assert(t[0] == 1 && t[1] == 2.125);
}

// https://issues.dlang.org/show_bug.cgi?id=23600
@safe pure unittest
{
    import std.typecons : Tuple, tuple;

    string h, w;
    Tuple!(int, float) t;

    assert("hello 1 2.34 world".formattedRead!"%s %d %f %s"(h, t, w) == 3);
    assert(h == "hello");
    assert(t == tuple(1, 2.34f));
    assert(w == "world");
}

@safe unittest
{
    import std.math.operations : isClose;
    import std.math.traits : isNaN;
    import std.range.primitives : empty;

    string s = " 1.2 3.4 ";
    double x, y, z;
    assert(formattedRead(s, " %s %s %s ", x, y, z) == 2);
    assert(s.empty);
    assert(isClose(x, 1.2));
    assert(isClose(y, 3.4));
    assert(isNaN(z));
}

// for backwards compatibility
@safe pure unittest
{
    string s = "hello!124:34.5";
    string a;
    int b;
    double c;
    formattedRead(s, "%s!%s:%s", &a, &b, &c);
    assert(a == "hello" && b == 124 && c == 34.5);

    // mix pointers and auto-ref
    s = "world!200:42.25";
    formattedRead(s, "%s!%s:%s", a, &b, &c);
    assert(a == "world" && b == 200 && c == 42.25);

    s = "world1!201:42.5";
    formattedRead(s, "%s!%s:%s", &a, &b, c);
    assert(a == "world1" && b == 201 && c == 42.5);

    s = "world2!202:42.75";
    formattedRead(s, "%s!%s:%s", a, b, &c);
    assert(a == "world2" && b == 202 && c == 42.75);
}

// for backwards compatibility
@safe pure unittest
{
    import std.math.operations : isClose;
    import std.math.traits : isNaN;
    import std.range.primitives : empty;

    string s = " 1.2 3.4 ";
    double x, y, z;
    assert(formattedRead(s, " %s %s %s ", &x, &y, &z) == 2);
    assert(s.empty);
    assert(isClose(x, 1.2));
    assert(isClose(y, 3.4));
    assert(isNaN(z));
}

@safe unittest
{
    string s = "hello!124:34.5";
    string a;
    int b;
    double c;
    formattedRead(s, "%s!%s:%s", &a, &b, &c);
    assert(a == "hello" && b == 124 && c == 34.5);
}

@safe pure unittest
{
    string line;

    bool f1;

    line = "true";
    formattedRead(line, "%s", &f1);
    assert(f1);

    line = "TrUE";
    formattedRead(line, "%s", &f1);
    assert(f1);

    line = "false";
    formattedRead(line, "%s", &f1);
    assert(!f1);

    line = "fALsE";
    formattedRead(line, "%s", &f1);
    assert(!f1);

    line = "1";
    formattedRead(line, "%d", &f1);
    assert(f1);

    line = "-1";
    formattedRead(line, "%d", &f1);
    assert(f1);

    line = "0";
    formattedRead(line, "%d", &f1);
    assert(!f1);

    line = "-0";
    formattedRead(line, "%d", &f1);
    assert(!f1);
}

@safe pure unittest
{
    union B
    {
        char[int.sizeof] untyped;
        int typed;
    }

    B b;
    b.typed = 5;
    char[] input = b.untyped[];
    int witness;
    formattedRead(input, "%r", &witness);
    assert(witness == b.typed);
}

@safe pure unittest
{
    union A
    {
        char[float.sizeof] untyped;
        float typed;
    }

    A a;
    a.typed = 5.5;
    char[] input = a.untyped[];
    float witness;
    formattedRead(input, "%r", &witness);
    assert(witness == a.typed);
}

@safe pure unittest
{
    import std.typecons : Tuple;

    char[] line = "1 2".dup;
    int a, b;
    formattedRead(line, "%s %s", &a, &b);
    assert(a == 1 && b == 2);

    line = "10 2 3".dup;
    formattedRead(line, "%d ", &a);
    assert(a == 10);
    assert(line == "2 3");

    Tuple!(int, float) t;
    line = "1 2.125".dup;
    formattedRead(line, "%d %g", &t);
    assert(t[0] == 1 && t[1] == 2.125);

    line = "1 7643 2.125".dup;
    formattedRead(line, "%s %*u %s", &t);
    assert(t[0] == 1 && t[1] == 2.125);
}

@safe pure unittest
{
    string line;

    char c1, c2;

    line = "abc";
    formattedRead(line, "%s%c", &c1, &c2);
    assert(c1 == 'a' && c2 == 'b');
    assert(line == "c");
}

@safe pure unittest
{
    string line;

    line = "[1,2,3]";
    int[] s1;
    formattedRead(line, "%s", &s1);
    assert(s1 == [1,2,3]);
}

@safe pure unittest
{
    string line;

    line = "[1,2,3]";
    int[] s1;
    formattedRead(line, "[%(%s,%)]", &s1);
    assert(s1 == [1,2,3]);

    line = `["hello", "world"]`;
    string[] s2;
    formattedRead(line, "[%(%s, %)]", &s2);
    assert(s2 == ["hello", "world"]);

    line = "123 456";
    int[] s3;
    formattedRead(line, "%(%s %)", &s3);
    assert(s3 == [123, 456]);

    line = "h,e,l,l,o; w,o,r,l,d";
    string[] s4;
    formattedRead(line, "%(%(%c,%); %)", &s4);
    assert(s4 == ["hello", "world"]);
}

@safe pure unittest
{
    import std.exception : assertThrown;

    string line;

    int[4] sa1;
    line = `[1,2,3,4]`;
    formattedRead(line, "%s", &sa1);
    assert(sa1 == [1,2,3,4]);

    int[4] sa2;
    line = `[1,2,3]`;
    assertThrown(formattedRead(line, "%s", &sa2));

    int[4] sa3;
    line = `[1,2,3,4,5]`;
    assertThrown(formattedRead(line, "%s", &sa3));
}

@safe pure unittest
{
    import std.exception : assertThrown;
    import std.format : FormatException;

    string input;

    int[4] sa1;
    input = `[1,2,3,4]`;
    formattedRead(input, "[%(%s,%)]", &sa1);
    assert(sa1 == [1,2,3,4]);

    int[4] sa2;
    input = `[1,2,3]`;
    assertThrown!FormatException(formattedRead(input, "[%(%s,%)]", &sa2));
}

@safe pure unittest
{
    string line;

    string s1, s2;

    line = "hello, world";
    formattedRead(line, "%s", &s1);
    assert(s1 == "hello, world", s1);

    line = "hello, world;yah";
    formattedRead(line, "%s;%s", &s1, &s2);
    assert(s1 == "hello, world", s1);
    assert(s2 == "yah", s2);

    line = `['h','e','l','l','o']`;
    string s3;
    formattedRead(line, "[%(%s,%)]", &s3);
    assert(s3 == "hello");

    line = `"hello"`;
    string s4;
    formattedRead(line, "\"%(%c%)\"", &s4);
    assert(s4 == "hello");
}

@safe pure unittest
{
    string line;

    string[int] aa1;
    line = `[1:"hello", 2:"world"]`;
    formattedRead(line, "%s", &aa1);
    assert(aa1 == [1:"hello", 2:"world"]);

    int[string] aa2;
    line = `{"hello"=1; "world"=2}`;
    formattedRead(line, "{%(%s=%s; %)}", &aa2);
    assert(aa2 == ["hello":1, "world":2]);

    int[string] aa3;
    line = `{[hello=1]; [world=2]}`;
    formattedRead(line, "{%([%(%c%)=%s]%|; %)}", &aa3);
    assert(aa3 == ["hello":1, "world":2]);
}

// test rvalue using
@safe pure unittest
{
    string[int] aa1;
    formattedRead!("%s")(`[1:"hello", 2:"world"]`, aa1);
    assert(aa1 == [1:"hello", 2:"world"]);

    int[string] aa2;
    formattedRead(`{"hello"=1; "world"=2}`, "{%(%s=%s; %)}", aa2);
    assert(aa2 == ["hello":1, "world":2]);
}

/**
Reads an input range according to a format string and returns a tuple of Args
with the read values.

Format specifiers with format character $(B 'd'), $(B 'u') and $(B
'c') can take a $(B '*') parameter for skipping values.

The second version of `formattedRead` takes the format string as
template argument. In this case, it is checked for consistency at
compile-time.

Params:
    Args = a variadic list of types of the arguments
 */
template formattedRead(Args...)
if (Args.length && allSatisfy!(isType, Args))
{
    import std.typecons : Tuple;

    /**
    Params:
        r = an $(REF_ALTTEXT input range, isInputRange, std, range, primitives),
            where the formatted input is read from
        fmt = a $(MREF_ALTTEXT format string, std,format)
        Range = the type of the input range `r`
        Char = the character type used for `fmt`

    Returns:
        A Tuple!Args with the elements filled.

    Throws:
        A $(REF_ALTTEXT FormatException, FormatException, std, format)
        if reading did not succeed.
    */
    Tuple!Args formattedRead(Range, Char)(auto ref Range r, const(Char)[] fmt)
    {
        import core.lifetime : forward;
        import std.format : enforceFmt;

        Tuple!Args args;
        const numArgsFilled = .formattedRead(forward!r, fmt, args.expand);
        enforceFmt(numArgsFilled == Args.length, "Failed reading into all format arguments");
        return args;
    }
}

///
@safe pure unittest
{
    import std.exception : assertThrown;
    import std.format : FormatException;
    import std.typecons : tuple;

    auto complete = "hello!34.5:124".formattedRead!(string, double, int)("%s!%s:%s");
    assert(complete == tuple("hello", 34.5, 124));

    // reading ends early
    assertThrown!FormatException("hello!34.5:".formattedRead!(string, double, int)("%s!%s:%s"));
}

/// Skipping values
@safe pure unittest
{
    import std.format : FormatException;
    import std.typecons : tuple;

    auto result = "orange: (12%) 15.25".formattedRead!(string, double)("%s: (%*d%%) %f");
    assert(result == tuple("orange", 15.25));
}

/// ditto
template formattedRead(alias fmt, Args...)
if (!isType!fmt && isSomeString!(typeof(fmt)) && Args.length && allSatisfy!(isType, Args))
{
    import std.typecons : Flag, Tuple, Yes;
    Tuple!Args formattedRead(Range)(auto ref Range r)
    {
        import core.lifetime : forward;
        import std.format : enforceFmt;

        Tuple!Args args;
        const numArgsFilled = .formattedRead!fmt(forward!r, args.expand);
        enforceFmt(numArgsFilled == Args.length, "Failed reading into all format arguments");
        return args;
    }
}

/// The format string can be checked at compile-time
@safe pure unittest
{
    import std.exception : assertThrown;
    import std.format : FormatException;
    import std.typecons : tuple;

    auto expected = tuple("hello", 124, 34.5);
    auto result = "hello!124:34.5".formattedRead!("%s!%s:%s", string, int, double);
    assert(result == expected);

    assertThrown!FormatException("hello!34.5:".formattedRead!("%s!%s:%s", string, double, int));
}

/// Compile-time consistency check
@safe pure unittest
{
    import std.format : FormatException;
    import std.typecons : tuple;

    static assert(!__traits(compiles, "orange: (12%) 15.25".formattedRead!("%s: (%*d%%) %f", string, double)));
}

/**
Reads a value from the given _input range and converts it according to a
format specifier.

Params:
    input = the $(REF_ALTTEXT input range, isInputRange, std, range, primitives),
            to read from
    spec = a $(MREF_ALTTEXT format string, std,format)
    T = type to return
    Range = the type of the input range `input`
    Char = the character type used for `spec`

Returns:
    A value from `input` of type `T`.

Throws:
    A $(REF_ALTTEXT FormatException, FormatException, std, format)
    if reading did not succeed.

See_Also:
    $(REF parse, std, conv) and $(REF to, std, conv)
 */
T unformatValue(T, Range, Char)(ref Range input, scope const ref FormatSpec!Char spec)
{
    return unformatValueImpl!T(input, spec);
}

///
@safe pure unittest
{
    import std.format.spec : singleSpec;

    string s = "42";
    auto spec = singleSpec("%s");
    assert(unformatValue!int(s, spec) == 42);
}

// https://issues.dlang.org/show_bug.cgi?id=7241
@safe pure unittest
{
    string input = "a";
    auto spec = FormatSpec!char("%s");
    spec.readUpToNextSpec(input);
    auto result = unformatValue!(dchar[1])(input, spec);
    assert(result[0] == 'a');
}

// https://issues.dlang.org/show_bug.cgi?id=20393
@safe pure unittest
{
    import std.exception : assertThrown;
    string str = "foo 12a-buzz";
    string a, c;
    int b;
    assertThrown(formattedRead(str, "%s %d-%s", &a, &b, &c));
}

// https://issues.dlang.org/show_bug.cgi?id=18051
@safe pure unittest
{
    import std.format : format;

    enum Op { lt, gt, eq }

    auto s = format!"%s"(Op.lt);
    Op op;
    assert(formattedRead!"%s"(s, op) == 1);
    assert(op == Op.lt);
}

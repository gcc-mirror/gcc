// Written in the D programming language.

/**
   This module implements the formatting functionality for strings and
   I/O. It's comparable to C99's $(D vsprintf()) and uses a similar
   _format encoding scheme.

   For an introductory look at $(B std._format)'s capabilities and how to use
   this module see the dedicated
   $(LINK2 http://wiki.dlang.org/Defining_custom_print_format_specifiers, DWiki article).

   This module centers around two functions:

$(BOOKTABLE ,
$(TR $(TH Function Name) $(TH Description)
)
    $(TR $(TD $(LREF formattedRead))
        $(TD Reads values according to the _format string from an InputRange.
    ))
    $(TR $(TD $(LREF formattedWrite))
        $(TD Formats its arguments according to the _format string and puts them
        to an OutputRange.
    ))
)

   Please see the documentation of function $(LREF formattedWrite) for a
   description of the _format string.

   Two functions have been added for convenience:

$(BOOKTABLE ,
$(TR $(TH Function Name) $(TH Description)
)
    $(TR $(TD $(LREF _format))
        $(TD Returns a GC-allocated string with the formatting result.
    ))
    $(TR $(TD $(LREF sformat))
        $(TD Puts the formatting result into a preallocated array.
    ))
)

   These two functions are publicly imported by $(MREF std, string)
   to be easily available.

   The functions $(LREF formatValue) and $(LREF unformatValue) are
   used for the plumbing.
   Copyright: Copyright Digital Mars 2000-2013.

   License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

   Authors: $(HTTP walterbright.com, Walter Bright), $(HTTP erdani.com,
   Andrei Alexandrescu), and Kenji Hara

   Source: $(PHOBOSSRC std/_format.d)
 */
module std.format;

//debug=format;                // uncomment to turn on debugging printf's

import core.vararg;
import std.exception;
import std.meta;
import std.range.primitives;
import std.traits;


/**********************************************************************
 * Signals a mismatch between a format and its corresponding argument.
 */
class FormatException : Exception
{
    @safe pure nothrow
    this()
    {
        super("format error");
    }

    @safe pure nothrow
    this(string msg, string fn = __FILE__, size_t ln = __LINE__, Throwable next = null)
    {
        super(msg, fn, ln, next);
    }
}

private alias enforceFmt = enforceEx!FormatException;


/**********************************************************************
   Interprets variadic argument list $(D args), formats them according
   to $(D fmt), and sends the resulting characters to $(D w). The
   encoding of the output is the same as $(D Char). The type $(D Writer)
   must satisfy $(D $(REF isOutputRange, std,range,primitives)!(Writer, Char)).

   The variadic arguments are normally consumed in order. POSIX-style
   $(HTTP opengroup.org/onlinepubs/009695399/functions/printf.html,
   positional parameter syntax) is also supported. Each argument is
   formatted into a sequence of chars according to the format
   specification, and the characters are passed to $(D w). As many
   arguments as specified in the format string are consumed and
   formatted. If there are fewer arguments than format specifiers, a
   $(D FormatException) is thrown. If there are more remaining arguments
   than needed by the format specification, they are ignored but only
   if at least one argument was formatted.

   The format string supports the formatting of array and nested array elements
   via the grouping format specifiers $(B %&#40;) and $(B %&#41;). Each
   matching pair of $(B %&#40;) and $(B %&#41;) corresponds with a single array
   argument. The enclosed sub-format string is applied to individual array
   elements.  The trailing portion of the sub-format string following the
   conversion specifier for the array element is interpreted as the array
   delimiter, and is therefore omitted following the last array element. The
   $(B %|) specifier may be used to explicitly indicate the start of the
   delimiter, so that the preceding portion of the string will be included
   following the last array element.  (See below for explicit examples.)

   Params:

   w = Output is sent to this writer. Typical output writers include
   $(REF Appender!string, std,array) and $(REF LockingTextWriter, std,stdio).

   fmt = Format string.

   args = Variadic argument list.

   Returns: Formatted number of arguments.

   Throws: Mismatched arguments and formats result in a $(D
   FormatException) being thrown.

   Format_String: <a name="format-string">$(I Format strings)</a>
   consist of characters interspersed with $(I format
   specifications). Characters are simply copied to the output (such
   as putc) after any necessary conversion to the corresponding UTF-8
   sequence.

   The format string has the following grammar:

$(PRE
$(I FormatString):
    $(I FormatStringItem)*
$(I FormatStringItem):
    $(B '%%')
    $(B '%') $(I Position) $(I Flags) $(I Width) $(I Separator) $(I Precision) $(I FormatChar)
    $(B '%$(LPAREN)') $(I FormatString) $(B '%$(RPAREN)')
    $(I OtherCharacterExceptPercent)
$(I Position):
    $(I empty)
    $(I Integer) $(B '$')
$(I Flags):
    $(I empty)
    $(B '-') $(I Flags)
    $(B '+') $(I Flags)
    $(B '#') $(I Flags)
    $(B '0') $(I Flags)
    $(B ' ') $(I Flags)
$(I Width):
    $(I empty)
    $(I Integer)
    $(B '*')
$(I Separator):
    $(I empty)
    $(B ',')
    $(B ',') $(B '?')
    $(B ',') $(B '*') $(B '?')
    $(B ',') $(I Integer) $(B '?')
    $(B ',') $(B '*')
    $(B ',') $(I Integer)
$(I Precision):
    $(I empty)
    $(B '.')
    $(B '.') $(I Integer)
    $(B '.*')
$(I Integer):
    $(I Digit)
    $(I Digit) $(I Integer)
$(I Digit):
    $(B '0')|$(B '1')|$(B '2')|$(B '3')|$(B '4')|$(B '5')|$(B '6')|$(B '7')|$(B '8')|$(B '9')
$(I FormatChar):
    $(B 's')|$(B 'c')|$(B 'b')|$(B 'd')|$(B 'o')|$(B 'x')|$(B 'X')|$(B 'e')|$(B 'E')|$(B 'f')|$(B 'F')|$(B 'g')|$(B 'G')|$(B 'a')|$(B 'A')|$(B '|')
)

    $(BOOKTABLE Flags affect formatting depending on the specifier as
    follows., $(TR $(TH Flag) $(TH Types&nbsp;affected) $(TH Semantics))

    $(TR $(TD $(B '-')) $(TD numeric) $(TD Left justify the result in
        the field.  It overrides any $(B 0) flag.))

    $(TR $(TD $(B '+')) $(TD numeric) $(TD Prefix positive numbers in
    a signed conversion with a $(B +).  It overrides any $(I space)
    flag.))

    $(TR $(TD $(B '#')) $(TD integral ($(B 'o'))) $(TD Add to
    precision as necessary so that the first digit of the octal
    formatting is a '0', even if both the argument and the $(I
    Precision) are zero.))

    $(TR $(TD $(B '#')) $(TD integral ($(B 'x'), $(B 'X'))) $(TD If
       non-zero, prefix result with $(B 0x) ($(B 0X)).))

    $(TR $(TD $(B '#')) $(TD floating) $(TD Always insert the decimal
       point and print trailing zeros.))

    $(TR $(TD $(B '0')) $(TD numeric) $(TD Use leading
    zeros to pad rather than spaces (except for the floating point
    values $(D nan) and $(D infinity)).  Ignore if there's a $(I
    Precision).))

    $(TR $(TD $(B ' ')) $(TD numeric) $(TD Prefix positive
    numbers in a signed conversion with a space.)))

    $(DL
        $(DT $(I Width))
        $(DD
        Specifies the minimum field width.
        If the width is a $(B *), an additional argument of type $(B int),
        preceding the actual argument, is taken as the width.
        If the width is negative, it is as if the $(B -) was given
        as a $(I Flags) character.)

        $(DT $(I Precision))
        $(DD Gives the precision for numeric conversions.
        If the precision is a $(B *), an additional argument of type $(B int),
        preceding the actual argument, is taken as the precision.
        If it is negative, it is as if there was no $(I Precision) specifier.)

        $(DT $(I Separator))
        $(DD Inserts the separator symbols ',' every $(I X) digits, from right
        to left, into numeric values to increase readability.
        The fractional part of floating point values inserts the separator
        from left to right.
        Entering an integer after the ',' allows to specify $(I X).
        If a '*' is placed after the ',' then $(I X) is specified by an
        additional parameter to the format function.
        Adding a '?' after the ',' or $(I X) specifier allows to specify
        the separator character as an additional parameter.
        )

        $(DT $(I FormatChar))
        $(DD
        $(DL
            $(DT $(B 's'))
            $(DD The corresponding argument is formatted in a manner consistent
            with its type:
            $(DL
                $(DT $(B bool))
                $(DD The result is $(D "true") or $(D "false").)
                $(DT integral types)
                $(DD The $(B %d) format is used.)
                $(DT floating point types)
                $(DD The $(B %g) format is used.)
                $(DT string types)
                $(DD The result is the string converted to UTF-8.
                A $(I Precision) specifies the maximum number of characters
                to use in the result.)
                $(DT structs)
                $(DD If the struct defines a $(B toString()) method the result is
                the string returned from this function. Otherwise the result is
                StructName(field<sub>0</sub>, field<sub>1</sub>, ...) where
                field<sub>n</sub> is the nth element formatted with the default
                format.)
                $(DT classes derived from $(B Object))
                $(DD The result is the string returned from the class instance's
                $(B .toString()) method.
                A $(I Precision) specifies the maximum number of characters
                to use in the result.)
                $(DT unions)
                $(DD If the union defines a $(B toString()) method the result is
                the string returned from this function. Otherwise the result is
                the name of the union, without its contents.)
                $(DT non-string static and dynamic arrays)
                $(DD The result is [s<sub>0</sub>, s<sub>1</sub>, ...]
                where s<sub>n</sub> is the nth element
                formatted with the default format.)
                $(DT associative arrays)
                $(DD The result is the equivalent of what the initializer
                would look like for the contents of the associative array,
                e.g.: ["red" : 10, "blue" : 20].)
            ))

            $(DT $(B 'c'))
            $(DD The corresponding argument must be a character type.)

            $(DT $(B 'b','d','o','x','X'))
            $(DD The corresponding argument must be an integral type
            and is formatted as an integer. If the argument is a signed type
            and the $(I FormatChar) is $(B d) it is converted to
            a signed string of characters, otherwise it is treated as
            unsigned. An argument of type $(B bool) is formatted as '1'
            or '0'. The base used is binary for $(B b), octal for $(B o),
            decimal
            for $(B d), and hexadecimal for $(B x) or $(B X).
            $(B x) formats using lower case letters, $(B X) uppercase.
            If there are fewer resulting digits than the $(I Precision),
            leading zeros are used as necessary.
            If the $(I Precision) is 0 and the number is 0, no digits
            result.)

            $(DT $(B 'e','E'))
            $(DD A floating point number is formatted as one digit before
            the decimal point, $(I Precision) digits after, the $(I FormatChar),
            &plusmn;, followed by at least a two digit exponent:
            $(I d.dddddd)e$(I &plusmn;dd).
            If there is no $(I Precision), six
            digits are generated after the decimal point.
            If the $(I Precision) is 0, no decimal point is generated.)

            $(DT $(B 'f','F'))
            $(DD A floating point number is formatted in decimal notation.
            The $(I Precision) specifies the number of digits generated
            after the decimal point. It defaults to six. At least one digit
            is generated before the decimal point. If the $(I Precision)
            is zero, no decimal point is generated.)

            $(DT $(B 'g','G'))
            $(DD A floating point number is formatted in either $(B e) or
            $(B f) format for $(B g); $(B E) or $(B F) format for
            $(B G).
            The $(B f) format is used if the exponent for an $(B e) format
            is greater than -5 and less than the $(I Precision).
            The $(I Precision) specifies the number of significant
            digits, and defaults to six.
            Trailing zeros are elided after the decimal point, if the fractional
            part is zero then no decimal point is generated.)

            $(DT $(B 'a','A'))
            $(DD A floating point number is formatted in hexadecimal
            exponential notation 0x$(I h.hhhhhh)p$(I &plusmn;d).
            There is one hexadecimal digit before the decimal point, and as
            many after as specified by the $(I Precision).
            If the $(I Precision) is zero, no decimal point is generated.
            If there is no $(I Precision), as many hexadecimal digits as
            necessary to exactly represent the mantissa are generated.
            The exponent is written in as few digits as possible,
            but at least one, is in decimal, and represents a power of 2 as in
            $(I h.hhhhhh)*2<sup>$(I &plusmn;d)</sup>.
            The exponent for zero is zero.
            The hexadecimal digits, x and p are in upper case if the
            $(I FormatChar) is upper case.)
        ))
    )

    Floating point NaN's are formatted as $(B nan) if the
    $(I FormatChar) is lower case, or $(B NAN) if upper.
    Floating point infinities are formatted as $(B inf) or
    $(B infinity) if the
    $(I FormatChar) is lower case, or $(B INF) or $(B INFINITY) if upper.

    The positional and non-positional styles can be mixed in the same
    format string. (POSIX leaves this behavior undefined.) The internal
    counter for non-positional parameters tracks the next parameter after
    the largest positional parameter already used.

    Example using array and nested array formatting:
    -------------------------
    import std.stdio;

    void main()
    {
        writefln("My items are %(%s %).", [1,2,3]);
        writefln("My items are %(%s, %).", [1,2,3]);
    }
    -------------------------
    The output is:
$(CONSOLE
My items are 1 2 3.
My items are 1, 2, 3.
)

    The trailing end of the sub-format string following the specifier for each
    item is interpreted as the array delimiter, and is therefore omitted
    following the last array item. The $(B %|) delimiter specifier may be used
    to indicate where the delimiter begins, so that the portion of the format
    string prior to it will be retained in the last array element:
    -------------------------
    import std.stdio;

    void main()
    {
        writefln("My items are %(-%s-%|, %).", [1,2,3]);
    }
    -------------------------
    which gives the output:
$(CONSOLE
My items are -1-, -2-, -3-.
)

    These compound format specifiers may be nested in the case of a nested
    array argument:
    -------------------------
    import std.stdio;
    void main() {
         auto mat = [[1, 2, 3],
                     [4, 5, 6],
                     [7, 8, 9]];

         writefln("%(%(%d %)\n%)", mat);
         writeln();

         writefln("[%(%(%d %)\n %)]", mat);
         writeln();

         writefln("[%([%(%d %)]%|\n %)]", mat);
         writeln();
    }
    -------------------------
    The output is:
$(CONSOLE
1 2 3
4 5 6
7 8 9

[1 2 3
 4 5 6
 7 8 9]

[[1 2 3]
 [4 5 6]
 [7 8 9]]
)

    Inside a compound format specifier, strings and characters are escaped
    automatically. To avoid this behavior, add $(B '-') flag to
    $(D "%$(LPAREN)").
    -------------------------
    import std.stdio;

    void main()
    {
        writefln("My friends are %s.", ["John", "Nancy"]);
        writefln("My friends are %(%s, %).", ["John", "Nancy"]);
        writefln("My friends are %-(%s, %).", ["John", "Nancy"]);
    }
    -------------------------
   which gives the output:
$(CONSOLE
My friends are ["John", "Nancy"].
My friends are "John", "Nancy".
My friends are John, Nancy.
)
 */
uint formattedWrite(alias fmt, Writer, A...)(auto ref Writer w, A args)
if (isSomeString!(typeof(fmt)))
{
    alias e = checkFormatException!(fmt, A);
    static assert(!e, e.msg);
    return .formattedWrite(w, fmt, args);
}

/// The format string can be checked at compile-time (see $(LREF format) for details):
@safe pure unittest
{
    import std.array : appender;
    import std.format : formattedWrite;

    auto writer = appender!string();
    writer.formattedWrite!"%s is the ultimate %s."(42, "answer");
    assert(writer.data == "42 is the ultimate answer.");

    // Clear the writer
    writer = appender!string();
    formattedWrite(writer, "Date: %2$s %1$s", "October", 5);
    assert(writer.data == "Date: 5 October");
}

/// ditto
uint formattedWrite(Writer, Char, A...)(auto ref Writer w, in Char[] fmt, A args)
{
    import std.conv : text;

    auto spec = FormatSpec!Char(fmt);

    // Are we already done with formats? Then just dump each parameter in turn
    uint currentArg = 0;
    while (spec.writeUpToNextSpec(w))
    {
        if (currentArg == A.length && !spec.indexStart)
        {
            // leftover spec?
            enforceFmt(fmt.length == 0,
                text("Orphan format specifier: %", spec.spec));
            break;
        }

        if (spec.width == spec.DYNAMIC)
        {
            auto width = getNthInt!"integer width"(currentArg, args);
            if (width < 0)
            {
                spec.flDash = true;
                width = -width;
            }
            spec.width = width;
            ++currentArg;
        }
        else if (spec.width < 0)
        {
            // means: get width as a positional parameter
            auto index = cast(uint) -spec.width;
            assert(index > 0);
            auto width = getNthInt!"integer width"(index - 1, args);
            if (currentArg < index) currentArg = index;
            if (width < 0)
            {
                spec.flDash = true;
                width = -width;
            }
            spec.width = width;
        }

        if (spec.precision == spec.DYNAMIC)
        {
            auto precision = getNthInt!"integer precision"(currentArg, args);
            if (precision >= 0) spec.precision = precision;
            // else negative precision is same as no precision
            else spec.precision = spec.UNSPECIFIED;
            ++currentArg;
        }
        else if (spec.precision < 0)
        {
            // means: get precision as a positional parameter
            auto index = cast(uint) -spec.precision;
            assert(index > 0);
            auto precision = getNthInt!"integer precision"(index- 1, args);
            if (currentArg < index) currentArg = index;
            if (precision >= 0) spec.precision = precision;
            // else negative precision is same as no precision
            else spec.precision = spec.UNSPECIFIED;
        }

        if (spec.separators == spec.DYNAMIC)
        {
            auto separators = getNthInt!"separator digit width"(currentArg, args);
            spec.separators = separators;
            ++currentArg;
        }

        if (spec.separatorCharPos == spec.DYNAMIC)
        {
            auto separatorChar =
                getNth!("separator character", isSomeChar, dchar)(currentArg, args);
            spec.separatorChar = separatorChar;
            ++currentArg;
        }

        if (currentArg == A.length && !spec.indexStart)
        {
            // leftover spec?
            enforceFmt(fmt.length == 0,
                text("Orphan format specifier: %", spec.spec));
            break;
        }

        // Format an argument
        // This switch uses a static foreach to generate a jump table.
        // Currently `spec.indexStart` use the special value '0' to signal
        // we should use the current argument. An enhancement would be to
        // always store the index.
        size_t index = currentArg;
        if (spec.indexStart != 0)
            index = spec.indexStart - 1;
        else
            ++currentArg;
    SWITCH: switch (index)
        {
            foreach (i, Tunused; A)
            {
            case i:
                formatValue(w, args[i], spec);
                if (currentArg < spec.indexEnd)
                    currentArg = spec.indexEnd;
                // A little know feature of format is to format a range
                // of arguments, e.g. `%1:3$` will format the first 3
                // arguments. Since they have to be consecutive we can
                // just use explicit fallthrough to cover that case.
                if (i + 1 < spec.indexEnd)
                {
                    // You cannot goto case if the next case is the default
                    static if (i + 1 < A.length)
                        goto case;
                    else
                        goto default;
                }
                else
                    break SWITCH;
            }
        default:
            throw new FormatException(
                text("Positional specifier %", spec.indexStart, '$', spec.spec,
                     " index exceeds ", A.length));
        }
    }
    return currentArg;
}

///
@safe unittest
{
    assert(format("%,d", 1000) == "1,000");
    assert(format("%,f", 1234567.891011) == "1,234,567.891,011");
    assert(format("%,?d", '?', 1000) == "1?000");
    assert(format("%,1d", 1000) == "1,0,0,0", format("%,1d", 1000));
    assert(format("%,*d", 4, -12345) == "-1,2345");
    assert(format("%,*?d", 4, '_', -12345) == "-1_2345");
    assert(format("%,6?d", '_', -12345678) == "-12_345678");
    assert(format("%12,3.3f", 1234.5678) == "   1,234.568", "'" ~
            format("%12,3.3f", 1234.5678) ~ "'");
}

@safe pure unittest
{
    import std.array;
    auto w = appender!string();
    formattedWrite(w, "%s %d", "@safe/pure", 42);
    assert(w.data == "@safe/pure 42");
}

/**
Reads characters from input range $(D r), converts them according
to $(D fmt), and writes them to $(D args).

Params:
    r = The range to read from.
    fmt = The format of the data to read.
    args = The drain of the data read.

Returns:

On success, the function returns the number of variables filled. This count
can match the expected number of readings or fewer, even zero, if a
matching failure happens.

Throws:
    An `Exception` if `S.length == 0` and `fmt` has format specifiers.
 */
uint formattedRead(alias fmt, R, S...)(ref R r, auto ref S args)
if (isSomeString!(typeof(fmt)))
{
    alias e = checkFormatException!(fmt, S);
    static assert(!e, e.msg);
    return .formattedRead(r, fmt, args);
}

/// ditto
uint formattedRead(R, Char, S...)(ref R r, const(Char)[] fmt, auto ref S args)
{
    import std.typecons : isTuple;

    auto spec = FormatSpec!Char(fmt);
    static if (!S.length)
    {
        spec.readUpToNextSpec(r);
        enforce(spec.trailing.empty, "Trailing characters in formattedRead format string");
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

/// The format string can be checked at compile-time (see $(LREF format) for details):
@safe pure unittest
{
    string s = "hello!124:34.5";
    string a;
    int b;
    double c;
    s.formattedRead!"%s!%s:%s"(a, b, c);
    assert(a == "hello" && b == 124 && c == 34.5);
}

@safe unittest
{
    import std.math;
    string s = " 1.2 3.4 ";
    double x, y, z;
    assert(formattedRead(s, " %s %s %s ", x, y, z) == 2);
    assert(s.empty);
    assert(approxEqual(x, 1.2));
    assert(approxEqual(y, 3.4));
    assert(isNaN(z));
}

// for backwards compatibility
@system pure unittest
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
@system pure unittest
{
    import std.math;
    string s = " 1.2 3.4 ";
    double x, y, z;
    assert(formattedRead(s, " %s %s %s ", &x, &y, &z) == 2);
    assert(s.empty);
    assert(approxEqual(x, 1.2));
    assert(approxEqual(y, 3.4));
    assert(isNaN(z));
}

@system pure unittest
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

@system pure unittest
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

@system pure unittest
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

@system pure unittest
{
    import std.typecons;
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

@system pure unittest
{
    string line;

    char c1, c2;

    line = "abc";
    formattedRead(line, "%s%c", &c1, &c2);
    assert(c1 == 'a' && c2 == 'b');
    assert(line == "c");
}

@system pure unittest
{
    string line;

    line = "[1,2,3]";
    int[] s1;
    formattedRead(line, "%s", &s1);
    assert(s1 == [1,2,3]);
}

@system pure unittest
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

@system pure unittest
{
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

@system pure unittest
{
    string input;

    int[4] sa1;
    input = `[1,2,3,4]`;
    formattedRead(input, "[%(%s,%)]", &sa1);
    assert(sa1 == [1,2,3,4]);

    int[4] sa2;
    input = `[1,2,3]`;
    assertThrown(formattedRead(input, "[%(%s,%)]", &sa2));
}

@system pure unittest
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

@system pure unittest
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

template FormatSpec(Char)
if (!is(Unqual!Char == Char))
{
    alias FormatSpec = FormatSpec!(Unqual!Char);
}

/**
 * A General handler for $(D printf) style format specifiers. Used for building more
 * specific formatting functions.
 */
struct FormatSpec(Char)
if (is(Unqual!Char == Char))
{
    import std.algorithm.searching : startsWith;
    import std.ascii : isDigit, isPunctuation, isAlpha;
    import std.conv : parse, text, to;

    /**
       Minimum _width, default $(D 0).
     */
    int width = 0;

    /**
       Precision. Its semantics depends on the argument type. For
       floating point numbers, _precision dictates the number of
       decimals printed.
     */
    int precision = UNSPECIFIED;

    /**
       Number of digits printed between _separators.
    */
    int separators = UNSPECIFIED;

    /**
       Set to `DYNAMIC` when the separator character is supplied at runtime.
    */
    int separatorCharPos = UNSPECIFIED;

    /**
       Character to insert between digits.
    */
    dchar separatorChar = ',';

    /**
       Special value for width and precision. $(D DYNAMIC) width or
       precision means that they were specified with $(D '*') in the
       format string and are passed at runtime through the varargs.
     */
    enum int DYNAMIC = int.max;

    /**
       Special value for precision, meaning the format specifier
       contained no explicit precision.
     */
    enum int UNSPECIFIED = DYNAMIC - 1;

    /**
       The actual format specifier, $(D 's') by default.
    */
    char spec = 's';

    /**
       Index of the argument for positional parameters, from $(D 1) to
       $(D ubyte.max). ($(D 0) means not used).
    */
    ubyte indexStart;

    /**
       Index of the last argument for positional parameter range, from
       $(D 1) to $(D ubyte.max). ($(D 0) means not used).
    */
    ubyte indexEnd;

    version (StdDdoc)
    {
        /**
         The format specifier contained a $(D '-') ($(D printf)
         compatibility).
         */
        bool flDash;

        /**
         The format specifier contained a $(D '0') ($(D printf)
         compatibility).
         */
        bool flZero;

        /**
         The format specifier contained a $(D ' ') ($(D printf)
         compatibility).
         */
        bool flSpace;

        /**
         The format specifier contained a $(D '+') ($(D printf)
         compatibility).
         */
        bool flPlus;

        /**
         The format specifier contained a $(D '#') ($(D printf)
         compatibility).
         */
        bool flHash;

        /**
         The format specifier contained a $(D ',')
         */
        bool flSeparator;

        // Fake field to allow compilation
        ubyte allFlags;
    }
    else
    {
        union
        {
            import std.bitmanip : bitfields;
            mixin(bitfields!(
                        bool, "flDash", 1,
                        bool, "flZero", 1,
                        bool, "flSpace", 1,
                        bool, "flPlus", 1,
                        bool, "flHash", 1,
                        bool, "flSeparator", 1,
                        ubyte, "", 2));
            ubyte allFlags;
        }
    }

    /**
       In case of a compound format specifier starting with $(D
       "%$(LPAREN)") and ending with $(D "%$(RPAREN)"), $(D _nested)
       contains the string contained within the two separators.
     */
    const(Char)[] nested;

    /**
       In case of a compound format specifier, $(D _sep) contains the
       string positioning after $(D "%|").
       `sep is null` means no separator else `sep.empty` means 0 length
        separator.
     */
    const(Char)[] sep;

    /**
       $(D _trailing) contains the rest of the format string.
     */
    const(Char)[] trailing;

    /*
       This string is inserted before each sequence (e.g. array)
       formatted (by default $(D "[")).
     */
    enum immutable(Char)[] seqBefore = "[";

    /*
       This string is inserted after each sequence formatted (by
       default $(D "]")).
     */
    enum immutable(Char)[] seqAfter = "]";

    /*
       This string is inserted after each element keys of a sequence (by
       default $(D ":")).
     */
    enum immutable(Char)[] keySeparator = ":";

    /*
       This string is inserted in between elements of a sequence (by
       default $(D ", ")).
     */
    enum immutable(Char)[] seqSeparator = ", ";

    /**
       Construct a new $(D FormatSpec) using the format string $(D fmt), no
       processing is done until needed.
     */
    this(in Char[] fmt) @safe pure
    {
        trailing = fmt;
    }

    bool writeUpToNextSpec(OutputRange)(ref OutputRange writer)
    {
        if (trailing.empty)
            return false;
        for (size_t i = 0; i < trailing.length; ++i)
        {
            if (trailing[i] != '%') continue;
            put(writer, trailing[0 .. i]);
            trailing = trailing[i .. $];
            enforceFmt(trailing.length >= 2, `Unterminated format specifier: "%"`);
            trailing = trailing[1 .. $];

            if (trailing[0] != '%')
            {
                // Spec found. Fill up the spec, and bailout
                fillUp();
                return true;
            }
            // Doubled! Reset and Keep going
            i = 0;
        }
        // no format spec found
        put(writer, trailing);
        trailing = null;
        return false;
    }

    @safe unittest
    {
        import std.array;
        auto w = appender!(char[])();
        auto f = FormatSpec("abc%sdef%sghi");
        f.writeUpToNextSpec(w);
        assert(w.data == "abc", w.data);
        assert(f.trailing == "def%sghi", text(f.trailing));
        f.writeUpToNextSpec(w);
        assert(w.data == "abcdef", w.data);
        assert(f.trailing == "ghi");
        // test with embedded %%s
        f = FormatSpec("ab%%cd%%ef%sg%%h%sij");
        w.clear();
        f.writeUpToNextSpec(w);
        assert(w.data == "ab%cd%ef" && f.trailing == "g%%h%sij", w.data);
        f.writeUpToNextSpec(w);
        assert(w.data == "ab%cd%efg%h" && f.trailing == "ij");
        // bug4775
        f = FormatSpec("%%%s");
        w.clear();
        f.writeUpToNextSpec(w);
        assert(w.data == "%" && f.trailing == "");
        f = FormatSpec("%%%%%s%%");
        w.clear();
        while (f.writeUpToNextSpec(w)) continue;
        assert(w.data == "%%%");

        f = FormatSpec("a%%b%%c%");
        w.clear();
        assertThrown!FormatException(f.writeUpToNextSpec(w));
        assert(w.data == "a%b%c" && f.trailing == "%");
    }

    private void fillUp()
    {
        // Reset content
        if (__ctfe)
        {
            flDash = false;
            flZero = false;
            flSpace = false;
            flPlus = false;
            flHash = false;
            flSeparator = false;
        }
        else
        {
            allFlags = 0;
        }

        width = 0;
        precision = UNSPECIFIED;
        nested = null;
        // Parse the spec (we assume we're past '%' already)
        for (size_t i = 0; i < trailing.length; )
        {
            switch (trailing[i])
            {
            case '(':
                // Embedded format specifier.
                auto j = i + 1;
                // Get the matching balanced paren
                for (uint innerParens;;)
                {
                    enforceFmt(j + 1 < trailing.length,
                        text("Incorrect format specifier: %", trailing[i .. $]));
                    if (trailing[j++] != '%')
                    {
                        // skip, we're waiting for %( and %)
                        continue;
                    }
                    if (trailing[j] == '-') // for %-(
                    {
                        ++j;    // skip
                        enforceFmt(j < trailing.length,
                            text("Incorrect format specifier: %", trailing[i .. $]));
                    }
                    if (trailing[j] == ')')
                    {
                        if (innerParens-- == 0) break;
                    }
                    else if (trailing[j] == '|')
                    {
                        if (innerParens == 0) break;
                    }
                    else if (trailing[j] == '(')
                    {
                        ++innerParens;
                    }
                }
                if (trailing[j] == '|')
                {
                    auto k = j;
                    for (++j;;)
                    {
                        if (trailing[j++] != '%')
                            continue;
                        if (trailing[j] == '%')
                            ++j;
                        else if (trailing[j] == ')')
                            break;
                        else
                            throw new Exception(
                                text("Incorrect format specifier: %",
                                        trailing[j .. $]));
                    }
                    nested = trailing[i + 1 .. k - 1];
                    sep = trailing[k + 1 .. j - 1];
                }
                else
                {
                    nested = trailing[i + 1 .. j - 1];
                    sep = null; // no separator
                }
                //this = FormatSpec(innerTrailingSpec);
                spec = '(';
                // We practically found the format specifier
                trailing = trailing[j + 1 .. $];
                return;
            case '-': flDash = true; ++i; break;
            case '+': flPlus = true; ++i; break;
            case '#': flHash = true; ++i; break;
            case '0': flZero = true; ++i; break;
            case ' ': flSpace = true; ++i; break;
            case '*':
                if (isDigit(trailing[++i]))
                {
                    // a '*' followed by digits and '$' is a
                    // positional format
                    trailing = trailing[1 .. $];
                    width = -parse!(typeof(width))(trailing);
                    i = 0;
                    enforceFmt(trailing[i++] == '$',
                        "$ expected");
                }
                else
                {
                    // read result
                    width = DYNAMIC;
                }
                break;
            case '1': .. case '9':
                auto tmp = trailing[i .. $];
                const widthOrArgIndex = parse!uint(tmp);
                enforceFmt(tmp.length,
                    text("Incorrect format specifier %", trailing[i .. $]));
                i = arrayPtrDiff(tmp, trailing);
                if (tmp.startsWith('$'))
                {
                    // index of the form %n$
                    indexEnd = indexStart = to!ubyte(widthOrArgIndex);
                    ++i;
                }
                else if (tmp.startsWith(':'))
                {
                    // two indexes of the form %m:n$, or one index of the form %m:$
                    indexStart = to!ubyte(widthOrArgIndex);
                    tmp = tmp[1 .. $];
                    if (tmp.startsWith('$'))
                    {
                        indexEnd = indexEnd.max;
                    }
                    else
                    {
                        indexEnd = parse!(typeof(indexEnd))(tmp);
                    }
                    i = arrayPtrDiff(tmp, trailing);
                    enforceFmt(trailing[i++] == '$',
                        "$ expected");
                }
                else
                {
                    // width
                    width = to!int(widthOrArgIndex);
                }
                break;
            case ',':
                // Precision
                ++i;
                flSeparator = true;

                if (trailing[i] == '*')
                {
                    ++i;
                    // read result
                    separators = DYNAMIC;
                }
                else if (isDigit(trailing[i]))
                {
                    auto tmp = trailing[i .. $];
                    separators = parse!int(tmp);
                    i = arrayPtrDiff(tmp, trailing);
                }
                else
                {
                    // "," was specified, but nothing after it
                    separators = 3;
                }

                if (trailing[i] == '?')
                {
                    separatorCharPos = DYNAMIC;
                    ++i;
                }

                break;
            case '.':
                // Precision
                if (trailing[++i] == '*')
                {
                    if (isDigit(trailing[++i]))
                    {
                        // a '.*' followed by digits and '$' is a
                        // positional precision
                        trailing = trailing[i .. $];
                        i = 0;
                        precision = -parse!int(trailing);
                        enforceFmt(trailing[i++] == '$',
                            "$ expected");
                    }
                    else
                    {
                        // read result
                        precision = DYNAMIC;
                    }
                }
                else if (trailing[i] == '-')
                {
                    // negative precision, as good as 0
                    precision = 0;
                    auto tmp = trailing[i .. $];
                    parse!int(tmp); // skip digits
                    i = arrayPtrDiff(tmp, trailing);
                }
                else if (isDigit(trailing[i]))
                {
                    auto tmp = trailing[i .. $];
                    precision = parse!int(tmp);
                    i = arrayPtrDiff(tmp, trailing);
                }
                else
                {
                    // "." was specified, but nothing after it
                    precision = 0;
                }
                break;
            default:
                // this is the format char
                spec = cast(char) trailing[i++];
                trailing = trailing[i .. $];
                return;
            } // end switch
        } // end for
        throw new Exception(text("Incorrect format specifier: ", trailing));
    }

    //--------------------------------------------------------------------------
    private bool readUpToNextSpec(R)(ref R r)
    {
        import std.ascii : isLower, isWhite;
        import std.utf : stride;

        // Reset content
        if (__ctfe)
        {
            flDash = false;
            flZero = false;
            flSpace = false;
            flPlus = false;
            flHash = false;
            flSeparator = false;
        }
        else
        {
            allFlags = 0;
        }
        width = 0;
        precision = UNSPECIFIED;
        nested = null;
        // Parse the spec
        while (trailing.length)
        {
            const c = trailing[0];
            if (c == '%' && trailing.length > 1)
            {
                const c2 = trailing[1];
                if (c2 == '%')
                {
                    assert(!r.empty);
                    // Require a '%'
                    if (r.front != '%') break;
                    trailing = trailing[2 .. $];
                    r.popFront();
                }
                else
                {
                    enforce(isLower(c2) || c2 == '*' ||
                            c2 == '(',
                            text("'%", c2,
                                    "' not supported with formatted read"));
                    trailing = trailing[1 .. $];
                    fillUp();
                    return true;
                }
            }
            else
            {
                if (c == ' ')
                {
                    while (!r.empty && isWhite(r.front)) r.popFront();
                    //r = std.algorithm.find!(not!(isWhite))(r);
                }
                else
                {
                    enforce(!r.empty,
                            text("parseToFormatSpec: Cannot find character '",
                                    c, "' in the input string."));
                    if (r.front != trailing.front) break;
                    r.popFront();
                }
                trailing = trailing[stride(trailing, 0) .. $];
            }
        }
        return false;
    }

    private string getCurFmtStr() const
    {
        import std.array : appender;
        auto w = appender!string();
        auto f = FormatSpec!Char("%s"); // for stringnize

        put(w, '%');
        if (indexStart != 0)
        {
            formatValue(w, indexStart, f);
            put(w, '$');
        }
        if (flDash)  put(w, '-');
        if (flZero)  put(w, '0');
        if (flSpace) put(w, ' ');
        if (flPlus)  put(w, '+');
        if (flHash)  put(w, '#');
        if (flSeparator)  put(w, ',');
        if (width != 0)
            formatValue(w, width, f);
        if (precision != FormatSpec!Char.UNSPECIFIED)
        {
            put(w, '.');
            formatValue(w, precision, f);
        }
        put(w, spec);
        return w.data;
    }

    @safe unittest
    {
        // issue 5237
        import std.array;
        auto w = appender!string();
        auto f = FormatSpec!char("%.16f");
        f.writeUpToNextSpec(w); // dummy eating
        assert(f.spec == 'f');
        auto fmt = f.getCurFmtStr();
        assert(fmt == "%.16f");
    }

    private const(Char)[] headUpToNextSpec()
    {
        import std.array : appender;
        auto w = appender!(typeof(return))();
        auto tr = trailing;

        while (tr.length)
        {
            if (tr[0] == '%')
            {
                if (tr.length > 1 && tr[1] == '%')
                {
                    tr = tr[2 .. $];
                    w.put('%');
                }
                else
                    break;
            }
            else
            {
                w.put(tr.front);
                tr.popFront();
            }
        }
        return w.data;
    }

    string toString()
    {
        return text("address = ", cast(void*) &this,
                "\nwidth = ", width,
                "\nprecision = ", precision,
                "\nspec = ", spec,
                "\nindexStart = ", indexStart,
                "\nindexEnd = ", indexEnd,
                "\nflDash = ", flDash,
                "\nflZero = ", flZero,
                "\nflSpace = ", flSpace,
                "\nflPlus = ", flPlus,
                "\nflHash = ", flHash,
                "\nflSeparator = ", flSeparator,
                "\nnested = ", nested,
                "\ntrailing = ", trailing, "\n");
    }
}

///
@safe pure unittest
{
    import std.array;
    auto a = appender!(string)();
    auto fmt = "Number: %2.4e\nString: %s";
    auto f = FormatSpec!char(fmt);

    f.writeUpToNextSpec(a);

    assert(a.data == "Number: ");
    assert(f.trailing == "\nString: %s");
    assert(f.spec == 'e');
    assert(f.width == 2);
    assert(f.precision == 4);

    f.writeUpToNextSpec(a);

    assert(a.data == "Number: \nString: ");
    assert(f.trailing == "");
    assert(f.spec == 's');
}

// Issue 14059
@safe unittest
{
    import std.array : appender;
    auto a = appender!(string)();

    auto f = FormatSpec!char("%-(%s%"); // %)")
    assertThrown(f.writeUpToNextSpec(a));

    f = FormatSpec!char("%(%-"); // %)")
    assertThrown(f.writeUpToNextSpec(a));
}

@safe unittest
{
    import std.array : appender;
    auto a = appender!(string)();

    auto f = FormatSpec!char("%,d");
    f.writeUpToNextSpec(a);

    assert(f.spec == 'd', format("%s", f.spec));
    assert(f.precision == FormatSpec!char.UNSPECIFIED);
    assert(f.separators == 3);

    f = FormatSpec!char("%5,10f");
    f.writeUpToNextSpec(a);
    assert(f.spec == 'f', format("%s", f.spec));
    assert(f.separators == 10);
    assert(f.width == 5);

    f = FormatSpec!char("%5,10.4f");
    f.writeUpToNextSpec(a);
    assert(f.spec == 'f', format("%s", f.spec));
    assert(f.separators == 10);
    assert(f.width == 5);
    assert(f.precision == 4);
}

/**
Helper function that returns a $(D FormatSpec) for a single specifier given
in $(D fmt).

Params:
    fmt = A format specifier.

Returns:
    A $(D FormatSpec) with the specifier parsed.
Throws:
    An `Exception` when more than one specifier is given or the specifier
    is malformed.
  */
FormatSpec!Char singleSpec(Char)(Char[] fmt)
{
    import std.conv : text;
    enforce(fmt.length >= 2, "fmt must be at least 2 characters long");
    enforce(fmt.front == '%', "fmt must start with a '%' character");

    static struct DummyOutputRange {
        void put(C)(C[] buf) {} // eat elements
    }
    auto a = DummyOutputRange();
    auto spec = FormatSpec!Char(fmt);
    //dummy write
    spec.writeUpToNextSpec(a);

    enforce(spec.trailing.empty,
            text("Trailing characters in fmt string: '", spec.trailing));

    return spec;
}

///
@safe pure unittest
{
    import std.exception : assertThrown;
    auto spec = singleSpec("%2.3e");

    assert(spec.trailing == "");
    assert(spec.spec == 'e');
    assert(spec.width == 2);
    assert(spec.precision == 3);

    assertThrown(singleSpec(""));
    assertThrown(singleSpec("2.3e"));
    assertThrown(singleSpec("%2.3eTest"));
}

/**
$(D bool)s are formatted as "true" or "false" with %s and as "1" or
"0" with integral-specific format specs.

Params:
    w = The $(D OutputRange) to write to.
    obj = The value to write.
    f = The $(D FormatSpec) defining how to write the value.
 */
void formatValue(Writer, T, Char)(auto ref Writer w, T obj, const ref FormatSpec!Char f)
if (is(BooleanTypeOf!T) && !is(T == enum) && !hasToString!(T, Char))
{
    BooleanTypeOf!T val = obj;

    if (f.spec == 's')
    {
        string s = val ? "true" : "false";
        if (!f.flDash)
        {
            // right align
            if (f.width > s.length)
                foreach (i ; 0 .. f.width - s.length) put(w, ' ');
            put(w, s);
        }
        else
        {
            // left align
            put(w, s);
            if (f.width > s.length)
                foreach (i ; 0 .. f.width - s.length) put(w, ' ');
        }
    }
    else
        formatValue(w, cast(int) val, f);
}

///
@safe pure unittest
{
    import std.array : appender;
    auto w = appender!string();
    auto spec = singleSpec("%s");
    formatValue(w, true, spec);

    assert(w.data == "true");
}

@safe pure unittest
{
    assertCTFEable!(
    {
        formatTest( false, "false" );
        formatTest( true,  "true"  );
    });
}
@system unittest
{
    class C1 { bool val; alias val this; this(bool v){ val = v; } }
    class C2 { bool val; alias val this; this(bool v){ val = v; }
               override string toString() const { return "C"; } }
    formatTest( new C1(false), "false" );
    formatTest( new C1(true),  "true" );
    formatTest( new C2(false), "C" );
    formatTest( new C2(true),  "C" );

    struct S1 { bool val; alias val this; }
    struct S2 { bool val; alias val this;
                string toString() const { return "S"; } }
    formatTest( S1(false), "false" );
    formatTest( S1(true),  "true"  );
    formatTest( S2(false), "S" );
    formatTest( S2(true),  "S" );
}

@safe pure unittest
{
    string t1 = format("[%6s] [%6s] [%-6s]", true, false, true);
    assert(t1 == "[  true] [ false] [true  ]");

    string t2 = format("[%3s] [%-2s]", true, false);
    assert(t2 == "[true] [false]");
}

/**
$(D null) literal is formatted as $(D "null").

Params:
    w = The $(D OutputRange) to write to.
    obj = The value to write.
    f = The $(D FormatSpec) defining how to write the value.
 */
void formatValue(Writer, T, Char)(auto ref Writer w, T obj, const ref FormatSpec!Char f)
if (is(Unqual!T == typeof(null)) && !is(T == enum) && !hasToString!(T, Char))
{
    enforceFmt(f.spec == 's',
        "null literal cannot match %" ~ f.spec);

    put(w, "null");
}

///
@safe pure unittest
{
    import std.array : appender;
    auto w = appender!string();
    auto spec = singleSpec("%s");
    formatValue(w, null, spec);

    assert(w.data == "null");
}

@safe pure unittest
{
    assert(collectExceptionMsg!FormatException(format("%p", null)).back == 'p');

    assertCTFEable!(
    {
        formatTest( null, "null" );
    });
}

/**
Integrals are formatted like $(D printf) does.

Params:
    w = The $(D OutputRange) to write to.
    obj = The value to write.
    f = The $(D FormatSpec) defining how to write the value.
 */
void formatValue(Writer, T, Char)(auto ref Writer w, T obj, const ref FormatSpec!Char f)
if (is(IntegralTypeOf!T) && !is(T == enum) && !hasToString!(T, Char))
{
    alias U = IntegralTypeOf!T;
    U val = obj;    // Extracting alias this may be impure/system/may-throw

    if (f.spec == 'r')
    {
        // raw write, skip all else and write the thing
        auto raw = (ref val)@trusted{
            return (cast(const char*) &val)[0 .. val.sizeof];
        }(val);
        if (needToSwapEndianess(f))
        {
            foreach_reverse (c; raw)
                put(w, c);
        }
        else
        {
            foreach (c; raw)
                put(w, c);
        }
        return;
    }

    immutable uint base =
        f.spec == 'x' || f.spec == 'X' ? 16 :
        f.spec == 'o' ? 8 :
        f.spec == 'b' ? 2 :
        f.spec == 's' || f.spec == 'd' || f.spec == 'u' ? 10 :
        0;
    enforceFmt(base > 0,
        "incompatible format character for integral argument: %" ~ f.spec);

    // Forward on to formatIntegral to handle both U and const(U)
    // Saves duplication of code for both versions.
    static if (is(ucent) && (is(U == cent) || is(U == ucent)))
        alias C = U;
    else static if (isSigned!U)
        alias C = long;
    else
        alias C = ulong;
    formatIntegral(w, cast(C) val, f, base, Unsigned!U.max);
}

///
@safe pure unittest
{
    import std.array : appender;
    auto w = appender!string();
    auto spec = singleSpec("%d");
    formatValue(w, 1337, spec);

    assert(w.data == "1337");
}

private void formatIntegral(Writer, T, Char)(ref Writer w, const(T) val, const ref FormatSpec!Char fs,
    uint base, ulong mask)
{
    T arg = val;

    immutable negative = (base == 10 && arg < 0);
    if (negative)
    {
        arg = -arg;
    }

    // All unsigned integral types should fit in ulong.
    static if (is(ucent) && is(typeof(arg) == ucent))
        formatUnsigned(w, (cast(ucent) arg) & mask, fs, base, negative);
    else
        formatUnsigned(w, (cast(ulong) arg) & mask, fs, base, negative);
}

private void formatUnsigned(Writer, T, Char)
(ref Writer w, T arg, const ref FormatSpec!Char fs, uint base, bool negative)
{
    /* Write string:
     *    leftpad prefix1 prefix2 zerofill digits rightpad
     */

    /* Convert arg to digits[].
     * Note that 0 becomes an empty digits[]
     */
    char[64] buffer = void; // 64 bits in base 2 at most
    char[] digits;
    if (arg < base && base <= 10 && arg)
    {
        // Most numbers are a single digit - avoid expensive divide
        buffer[0] = cast(char)(arg + '0');
        digits = buffer[0 .. 1];
    }
    else
    {
        size_t i = buffer.length;
        while (arg)
        {
            --i;
            char c = cast(char) (arg % base);
            arg /= base;
            if (c < 10)
                buffer[i] = cast(char)(c + '0');
            else
                buffer[i] = cast(char)(c + (fs.spec == 'x' ? 'a' - 10 : 'A' - 10));
        }
        digits = buffer[i .. $]; // got the digits without the sign
    }


    immutable precision = (fs.precision == fs.UNSPECIFIED) ? 1 : fs.precision;

    char padChar = 0;
    if (!fs.flDash)
    {
        padChar = (fs.flZero && fs.precision == fs.UNSPECIFIED) ? '0' : ' ';
    }

    // Compute prefix1 and prefix2
    char prefix1 = 0;
    char prefix2 = 0;
    if (base == 10)
    {
        if (negative)
            prefix1 = '-';
        else if (fs.flPlus)
            prefix1 = '+';
        else if (fs.flSpace)
            prefix1 = ' ';
    }
    else if (base == 16 && fs.flHash && digits.length)
    {
        prefix1 = '0';
        prefix2 = fs.spec == 'x' ? 'x' : 'X';
    }
    // adjust precision to print a '0' for octal if alternate format is on
    else if (base == 8 && fs.flHash &&
             (precision <= 1 || precision <= digits.length) && // too low precision
             digits.length > 0)
        prefix1 = '0';

    size_t zerofill = precision > digits.length ? precision - digits.length : 0;
    size_t leftpad = 0;
    size_t rightpad = 0;

    immutable ptrdiff_t spacesToPrint =
        fs.width - (
              (prefix1 != 0)
            + (prefix2 != 0)
            + zerofill
            + digits.length
            + ((fs.flSeparator != 0) * (digits.length / fs.separators))
        );
    if (spacesToPrint > 0) // need to do some padding
    {
        if (padChar == '0')
            zerofill += spacesToPrint;
        else if (padChar)
            leftpad = spacesToPrint;
        else
            rightpad = spacesToPrint;
    }

    // Print
    foreach (i ; 0 .. leftpad)
        put(w, ' ');

    if (prefix1) put(w, prefix1);
    if (prefix2) put(w, prefix2);

    foreach (i ; 0 .. zerofill)
        put(w, '0');

    if (fs.flSeparator)
    {
        for (size_t j = 0; j < digits.length; ++j)
        {
            if (j != 0 && (digits.length - j) % fs.separators == 0)
            {
                put(w, fs.separatorChar);
            }
            put(w, digits[j]);
        }
    }
    else
    {
        put(w, digits);
    }

    foreach (i ; 0 .. rightpad)
        put(w, ' ');
}

@safe pure unittest
{
    assert(collectExceptionMsg!FormatException(format("%c", 5)).back == 'c');

    assertCTFEable!(
    {
        formatTest(9, "9");
        formatTest( 10, "10" );
    });
}

@system unittest
{
    class C1 { long val; alias val this; this(long v){ val = v; } }
    class C2 { long val; alias val this; this(long v){ val = v; }
               override string toString() const { return "C"; } }
    formatTest( new C1(10), "10" );
    formatTest( new C2(10), "C" );

    struct S1 { long val; alias val this; }
    struct S2 { long val; alias val this;
                string toString() const { return "S"; } }
    formatTest( S1(10), "10" );
    formatTest( S2(10), "S" );
}

// bugzilla 9117
@safe unittest
{
    static struct Frop {}

    static struct Foo
    {
        int n = 0;
        alias n this;
        T opCast(T) () if (is(T == Frop))
        {
            return Frop();
        }
        string toString()
        {
            return "Foo";
        }
    }

    static struct Bar
    {
        Foo foo;
        alias foo this;
        string toString()
        {
            return "Bar";
        }
    }

    const(char)[] result;
    void put(const char[] s){ result ~= s; }

    Foo foo;
    formattedWrite(&put, "%s", foo);    // OK
    assert(result == "Foo");

    result = null;

    Bar bar;
    formattedWrite(&put, "%s", bar);    // NG
    assert(result == "Bar");

    result = null;

    int i = 9;
    formattedWrite(&put, "%s", 9);
    assert(result == "9");
}

private enum ctfpMessage = "Cannot format floating point types at compile-time";

/**
Floating-point values are formatted like $(D printf) does.

Params:
    w = The $(D OutputRange) to write to.
    obj = The value to write.
    f = The $(D FormatSpec) defining how to write the value.
 */
void formatValue(Writer, T, Char)(auto ref Writer w, T obj, const ref FormatSpec!Char f)
if (is(FloatingPointTypeOf!T) && !is(T == enum) && !hasToString!(T, Char))
{
    import std.algorithm.comparison : min;
    import std.algorithm.searching : find;
    import std.string : indexOf, indexOfAny, indexOfNeither;

    FormatSpec!Char fs = f; // fs is copy for change its values.
    FloatingPointTypeOf!T val = obj;

    if (fs.spec == 'r')
    {
        // raw write, skip all else and write the thing
        auto raw = (ref val)@trusted{
            return (cast(const char*) &val)[0 .. val.sizeof];
        }(val);
        if (needToSwapEndianess(f))
        {
            foreach_reverse (c; raw)
                put(w, c);
        }
        else
        {
            foreach (c; raw)
                put(w, c);
        }
        return;
    }
    enforceFmt(find("fgFGaAeEs", fs.spec).length,
        "incompatible format character for floating point argument: %" ~ fs.spec);
    enforceFmt(!__ctfe, ctfpMessage);

    version (CRuntime_Microsoft)
    {
        import std.math : isNaN, isInfinity;
        immutable double tval = val; // convert early to get "inf" in case of overflow
        string s;
        if (isNaN(tval))
            s = "nan"; // snprintf writes 1.#QNAN
        else if (isInfinity(tval))
            s = val >= 0 ? "inf" : "-inf"; // snprintf writes 1.#INF

        if (s.length > 0)
        {
          version (none)
          {
            return formatValue(w, s, f);
          }
          else  // FIXME:workaround
          {
            s = s[0 .. f.precision < $ ? f.precision : $];
            if (!f.flDash)
            {
                // right align
                if (f.width > s.length)
                    foreach (j ; 0 .. f.width - s.length) put(w, ' ');
                put(w, s);
            }
            else
            {
                // left align
                put(w, s);
                if (f.width > s.length)
                    foreach (j ; 0 .. f.width - s.length) put(w, ' ');
            }
            return;
          }
        }
    }
    else
        alias tval = val;
    if (fs.spec == 's') fs.spec = 'g';
    char[1 /*%*/ + 5 /*flags*/ + 3 /*width.prec*/ + 2 /*format*/
                     + 1 /*\0*/] sprintfSpec = void;
    sprintfSpec[0] = '%';
    uint i = 1;
    if (fs.flDash) sprintfSpec[i++] = '-';
    if (fs.flPlus) sprintfSpec[i++] = '+';
    if (fs.flZero) sprintfSpec[i++] = '0';
    if (fs.flSpace) sprintfSpec[i++] = ' ';
    if (fs.flHash) sprintfSpec[i++] = '#';
    sprintfSpec[i .. i + 3] = "*.*";
    i += 3;
    if (is(Unqual!(typeof(val)) == real)) sprintfSpec[i++] = 'L';
    sprintfSpec[i++] = fs.spec;
    sprintfSpec[i] = 0;
    //printf("format: '%s'; geeba: %g\n", sprintfSpec.ptr, val);
    char[512] buf = void;

    immutable n = ()@trusted{
        import core.stdc.stdio : snprintf;
        return snprintf(buf.ptr, buf.length,
                        sprintfSpec.ptr,
                        fs.width,
                        // negative precision is same as no precision specified
                        fs.precision == fs.UNSPECIFIED ? -1 : fs.precision,
                        tval);
    }();

    enforceFmt(n >= 0,
        "floating point formatting failure");

    auto len = min(n, buf.length-1);
    ptrdiff_t dot = buf[0 .. len].indexOf('.');
    if (fs.flSeparator && dot != -1)
    {
        ptrdiff_t firstDigit = buf[0 .. len].indexOfAny("0123456789");
        ptrdiff_t ePos = buf[0 .. len].indexOf('e');
        size_t j;

        ptrdiff_t firstLen = dot - firstDigit;

        size_t separatorScoreCnt = firstLen / fs.separators;

        size_t afterDotIdx;
        if (ePos != -1)
        {
            afterDotIdx = ePos;
        }
        else
        {
            afterDotIdx = len;
        }

        if (dot != -1)
        {
            ptrdiff_t mantissaLen = afterDotIdx - (dot + 1);
            separatorScoreCnt += (mantissaLen > 0) ? (mantissaLen - 1) / fs.separators : 0;
        }

        // plus, minus prefix
        ptrdiff_t digitsBegin = buf[0 .. separatorScoreCnt].indexOfNeither(" ");
        if (digitsBegin == -1)
        {
            digitsBegin = separatorScoreCnt;
        }
        put(w, buf[digitsBegin .. firstDigit]);

        // digits until dot with separator
        for (j = 0; j < firstLen; ++j)
        {
            if (j > 0 && (firstLen - j) % fs.separators == 0)
            {
                put(w, fs.separatorChar);
            }
            put(w, buf[j + firstDigit]);
        }
        put(w, '.');

        // digits after dot
        for (j = dot + 1; j < afterDotIdx; ++j)
        {
            auto realJ = (j - (dot + 1));
            if (realJ != 0 && realJ % fs.separators == 0)
            {
                put(w, fs.separatorChar);
            }
            put(w, buf[j]);
        }

        // rest
        if (ePos != -1)
        {
            put(w, buf[afterDotIdx .. len]);
        }
    }
    else
    {
        put(w, buf[0 .. len]);
    }
}

///
@safe unittest
{
    import std.array : appender;
    auto w = appender!string();
    auto spec = singleSpec("%.1f");
    formatValue(w, 1337.7, spec);

    assert(w.data == "1337.7");
}

@safe /*pure*/ unittest     // formatting floating point values is now impure
{
    import std.conv : to;

    assert(collectExceptionMsg!FormatException(format("%d", 5.1)).back == 'd');

    foreach (T; AliasSeq!(float, double, real))
    {
        formatTest( to!(          T)(5.5), "5.5" );
        formatTest( to!(    const T)(5.5), "5.5" );
        formatTest( to!(immutable T)(5.5), "5.5" );

        formatTest( T.nan, "nan" );
    }
}

@system unittest
{
    formatTest( 2.25, "2.25" );

    class C1 { double val; alias val this; this(double v){ val = v; } }
    class C2 { double val; alias val this; this(double v){ val = v; }
               override string toString() const { return "C"; } }
    formatTest( new C1(2.25), "2.25" );
    formatTest( new C2(2.25), "C" );

    struct S1 { double val; alias val this; }
    struct S2 { double val; alias val this;
                string toString() const { return "S"; } }
    formatTest( S1(2.25), "2.25" );
    formatTest( S2(2.25), "S" );
}

/*
Formatting a $(D creal) is deprecated but still kept around for a while.

Params:
    w = The $(D OutputRange) to write to.
    obj = The value to write.
    f = The $(D FormatSpec) defining how to write the value.
 */
void formatValue(Writer, T, Char)(auto ref Writer w, T obj, const ref FormatSpec!Char f)
if (is(Unqual!T : creal) && !is(T == enum) && !hasToString!(T, Char))
{
    immutable creal val = obj;

    formatValue(w, val.re, f);
    if (val.im >= 0)
    {
        put(w, '+');
    }
    formatValue(w, val.im, f);
    put(w, 'i');
}

@safe /*pure*/ unittest     // formatting floating point values is now impure
{
    import std.conv : to;
    foreach (T; AliasSeq!(cfloat, cdouble, creal))
    {
        formatTest( to!(          T)(1 + 1i), "1+1i" );
        formatTest( to!(    const T)(1 + 1i), "1+1i" );
        formatTest( to!(immutable T)(1 + 1i), "1+1i" );
    }
    foreach (T; AliasSeq!(cfloat, cdouble, creal))
    {
        formatTest( to!(          T)(0 - 3i), "0-3i" );
        formatTest( to!(    const T)(0 - 3i), "0-3i" );
        formatTest( to!(immutable T)(0 - 3i), "0-3i" );
    }
}

@system unittest
{
    formatTest( 3+2.25i, "3+2.25i" );

    class C1 { cdouble val; alias val this; this(cdouble v){ val = v; } }
    class C2 { cdouble val; alias val this; this(cdouble v){ val = v; }
               override string toString() const { return "C"; } }
    formatTest( new C1(3+2.25i), "3+2.25i" );
    formatTest( new C2(3+2.25i), "C" );

    struct S1 { cdouble val; alias val this; }
    struct S2 { cdouble val; alias val this;
                string toString() const { return "S"; } }
    formatTest( S1(3+2.25i), "3+2.25i" );
    formatTest( S2(3+2.25i), "S" );
}

/*
   Formatting an $(D ireal) is deprecated but still kept around for a while.

Params:
    w = The $(D OutputRange) to write to.
    obj = The value to write.
    f = The $(D FormatSpec) defining how to write the value.
 */
void formatValue(Writer, T, Char)(auto ref Writer w, T obj, const ref FormatSpec!Char f)
if (is(Unqual!T : ireal) && !is(T == enum) && !hasToString!(T, Char))
{
    immutable ireal val = obj;

    formatValue(w, val.im, f);
    put(w, 'i');
}

@safe /*pure*/ unittest     // formatting floating point values is now impure
{
    import std.conv : to;
    foreach (T; AliasSeq!(ifloat, idouble, ireal))
    {
        formatTest( to!(          T)(1i), "1i" );
        formatTest( to!(    const T)(1i), "1i" );
        formatTest( to!(immutable T)(1i), "1i" );
    }
}

@system unittest
{
    formatTest( 2.25i, "2.25i" );

    class C1 { idouble val; alias val this; this(idouble v){ val = v; } }
    class C2 { idouble val; alias val this; this(idouble v){ val = v; }
               override string toString() const { return "C"; } }
    formatTest( new C1(2.25i), "2.25i" );
    formatTest( new C2(2.25i), "C" );

    struct S1 { idouble val; alias val this; }
    struct S2 { idouble val; alias val this;
                string toString() const { return "S"; } }
    formatTest( S1(2.25i), "2.25i" );
    formatTest( S2(2.25i), "S" );
}

/**
Individual characters ($(D char), $(D wchar), or $(D dchar)) are formatted as
Unicode characters with %s and as integers with integral-specific format
specs.

Params:
    w = The $(D OutputRange) to write to.
    obj = The value to write.
    f = The $(D FormatSpec) defining how to write the value.
 */
void formatValue(Writer, T, Char)(auto ref Writer w, T obj, const ref FormatSpec!Char f)
if (is(CharTypeOf!T) && !is(T == enum) && !hasToString!(T, Char))
{
    CharTypeOf!T val = obj;

    if (f.spec == 's' || f.spec == 'c')
    {
        put(w, val);
    }
    else
    {
        alias U = AliasSeq!(ubyte, ushort, uint)[CharTypeOf!T.sizeof/2];
        formatValue(w, cast(U) val, f);
    }
}

///
@safe pure unittest
{
    import std.array : appender;
    auto w = appender!string();
    auto spec = singleSpec("%c");
    formatValue(w, 'a', spec);

    assert(w.data == "a");
}

@safe pure unittest
{
    assertCTFEable!(
    {
        formatTest( 'c', "c" );
    });
}

@system unittest
{
    class C1 { char val; alias val this; this(char v){ val = v; } }
    class C2 { char val; alias val this; this(char v){ val = v; }
               override string toString() const { return "C"; } }
    formatTest( new C1('c'), "c" );
    formatTest( new C2('c'), "C" );

    struct S1 { char val; alias val this; }
    struct S2 { char val; alias val this;
                string toString() const { return "S"; } }
    formatTest( S1('c'), "c" );
    formatTest( S2('c'), "S" );
}

@safe unittest
{
    //Little Endian
    formatTest( "%-r", cast( char)'c', ['c'         ] );
    formatTest( "%-r", cast(wchar)'c', ['c', 0      ] );
    formatTest( "%-r", cast(dchar)'c', ['c', 0, 0, 0] );
    formatTest( "%-r", '本', ['\x2c', '\x67'] );

    //Big Endian
    formatTest( "%+r", cast( char)'c', [         'c'] );
    formatTest( "%+r", cast(wchar)'c', [0,       'c'] );
    formatTest( "%+r", cast(dchar)'c', [0, 0, 0, 'c'] );
    formatTest( "%+r", '本', ['\x67', '\x2c'] );
}

/**
Strings are formatted like $(D printf) does.

Params:
    w = The $(D OutputRange) to write to.
    obj = The value to write.
    f = The $(D FormatSpec) defining how to write the value.
 */
void formatValue(Writer, T, Char)(auto ref Writer w, T obj, const ref FormatSpec!Char f)
if (is(StringTypeOf!T) && !is(StaticArrayTypeOf!T) && !is(T == enum) && !hasToString!(T, Char))
{
    Unqual!(StringTypeOf!T) val = obj;  // for `alias this`, see bug5371
    formatRange(w, val, f);
}

///
@safe pure unittest
{
    import std.array : appender;
    auto w = appender!string();
    auto spec = singleSpec("%s");
    formatValue(w, "hello", spec);

    assert(w.data == "hello");
}

@safe unittest
{
    formatTest( "abc", "abc" );
}

@system unittest
{
    // Test for bug 5371 for classes
    class C1 { const string var; alias var this; this(string s){ var = s; } }
    class C2 {       string var; alias var this; this(string s){ var = s; } }
    formatTest( new C1("c1"), "c1" );
    formatTest( new C2("c2"), "c2" );

    // Test for bug 5371 for structs
    struct S1 { const string var; alias var this; }
    struct S2 {       string var; alias var this; }
    formatTest( S1("s1"), "s1" );
    formatTest( S2("s2"), "s2" );
}

@system unittest
{
    class  C3 { string val; alias val this; this(string s){ val = s; }
                override string toString() const { return "C"; } }
    formatTest( new C3("c3"), "C" );

    struct S3 { string val; alias val this;
                string toString() const { return "S"; } }
    formatTest( S3("s3"), "S" );
}

@safe pure unittest
{
    //Little Endian
    formatTest( "%-r", "ab"c, ['a'         , 'b'         ] );
    formatTest( "%-r", "ab"w, ['a', 0      , 'b', 0      ] );
    formatTest( "%-r", "ab"d, ['a', 0, 0, 0, 'b', 0, 0, 0] );
    formatTest( "%-r", "日本語"c, ['\xe6', '\x97', '\xa5', '\xe6', '\x9c', '\xac', '\xe8', '\xaa', '\x9e'] );
    formatTest( "%-r", "日本語"w, ['\xe5', '\x65', '\x2c', '\x67', '\x9e', '\x8a']);
    formatTest( "%-r", "日本語"d, ['\xe5', '\x65', '\x00', '\x00', '\x2c', '\x67',
        '\x00', '\x00', '\x9e', '\x8a', '\x00', '\x00'] );

    //Big Endian
    formatTest( "%+r", "ab"c, [         'a',          'b'] );
    formatTest( "%+r", "ab"w, [      0, 'a',       0, 'b'] );
    formatTest( "%+r", "ab"d, [0, 0, 0, 'a', 0, 0, 0, 'b'] );
    formatTest( "%+r", "日本語"c, ['\xe6', '\x97', '\xa5', '\xe6', '\x9c', '\xac', '\xe8', '\xaa', '\x9e'] );
    formatTest( "%+r", "日本語"w, ['\x65', '\xe5', '\x67', '\x2c', '\x8a', '\x9e'] );
    formatTest( "%+r", "日本語"d, ['\x00', '\x00', '\x65', '\xe5', '\x00', '\x00',
        '\x67', '\x2c', '\x00', '\x00', '\x8a', '\x9e'] );
}

/**
Static-size arrays are formatted as dynamic arrays.

Params:
    w = The $(D OutputRange) to write to.
    obj = The value to write.
    f = The $(D FormatSpec) defining how to write the value.
 */
void formatValue(Writer, T, Char)(auto ref Writer w, auto ref T obj, const ref FormatSpec!Char f)
if (is(StaticArrayTypeOf!T) && !is(T == enum) && !hasToString!(T, Char))
{
    formatValue(w, obj[], f);
}

///
@safe pure unittest
{
    import std.array : appender;
    auto w = appender!string();
    auto spec = singleSpec("%s");
    char[2] two = ['a', 'b'];
    formatValue(w, two, spec);

    assert(w.data == "ab");
}

@safe unittest    // Test for issue 8310
{
    import std.array : appender;
    FormatSpec!char f;
    auto w = appender!string();

    char[2] two = ['a', 'b'];
    formatValue(w, two, f);

    char[2] getTwo(){ return two; }
    formatValue(w, getTwo(), f);
}

/**
Dynamic arrays are formatted as input ranges.

Specializations:
    $(UL $(LI $(D void[]) is formatted like $(D ubyte[]).)
        $(LI Const array is converted to input range by removing its qualifier.))

Params:
    w = The $(D OutputRange) to write to.
    obj = The value to write.
    f = The $(D FormatSpec) defining how to write the value.
 */
void formatValue(Writer, T, Char)(auto ref Writer w, T obj, const ref FormatSpec!Char f)
if (is(DynamicArrayTypeOf!T) && !is(StringTypeOf!T) && !is(T == enum) && !hasToString!(T, Char))
{
    static if (is(const(ArrayTypeOf!T) == const(void[])))
    {
        formatValue(w, cast(const ubyte[]) obj, f);
    }
    else static if (!isInputRange!T)
    {
        alias U = Unqual!(ArrayTypeOf!T);
        static assert(isInputRange!U);
        U val = obj;
        formatValue(w, val, f);
    }
    else
    {
        formatRange(w, obj, f);
    }
}

///
@safe pure unittest
{
    import std.array : appender;
    auto w = appender!string();
    auto spec = singleSpec("%s");
    auto two = [1, 2];
    formatValue(w, two, spec);

    assert(w.data == "[1, 2]");
}

// alias this, input range I/F, and toString()
@system unittest
{
    struct S(int flags)
    {
        int[] arr;
      static if (flags & 1)
        alias arr this;

      static if (flags & 2)
      {
        @property bool empty() const { return arr.length == 0; }
        @property int front() const { return arr[0] * 2; }
        void popFront() { arr = arr[1..$]; }
      }

      static if (flags & 4)
        string toString() const { return "S"; }
    }
    formatTest(S!0b000([0, 1, 2]), "S!0([0, 1, 2])");
    formatTest(S!0b001([0, 1, 2]), "[0, 1, 2]");        // Test for bug 7628
    formatTest(S!0b010([0, 1, 2]), "[0, 2, 4]");
    formatTest(S!0b011([0, 1, 2]), "[0, 2, 4]");
    formatTest(S!0b100([0, 1, 2]), "S");
    formatTest(S!0b101([0, 1, 2]), "S");                // Test for bug 7628
    formatTest(S!0b110([0, 1, 2]), "S");
    formatTest(S!0b111([0, 1, 2]), "S");

    class C(uint flags)
    {
        int[] arr;
      static if (flags & 1)
        alias arr this;

        this(int[] a) { arr = a; }

      static if (flags & 2)
      {
        @property bool empty() const { return arr.length == 0; }
        @property int front() const { return arr[0] * 2; }
        void popFront() { arr = arr[1..$]; }
      }

      static if (flags & 4)
        override string toString() const { return "C"; }
    }
    formatTest(new C!0b000([0, 1, 2]), (new C!0b000([])).toString());
    formatTest(new C!0b001([0, 1, 2]), "[0, 1, 2]");    // Test for bug 7628
    formatTest(new C!0b010([0, 1, 2]), "[0, 2, 4]");
    formatTest(new C!0b011([0, 1, 2]), "[0, 2, 4]");
    formatTest(new C!0b100([0, 1, 2]), "C");
    formatTest(new C!0b101([0, 1, 2]), "C");            // Test for bug 7628
    formatTest(new C!0b110([0, 1, 2]), "C");
    formatTest(new C!0b111([0, 1, 2]), "C");
}

@system unittest
{
    // void[]
    void[] val0;
    formatTest( val0, "[]" );

    void[] val = cast(void[]) cast(ubyte[])[1, 2, 3];
    formatTest( val, "[1, 2, 3]" );

    void[0] sval0 = [];
    formatTest( sval0, "[]");

    void[3] sval = cast(void[3]) cast(ubyte[3])[1, 2, 3];
    formatTest( sval, "[1, 2, 3]" );
}

@safe unittest
{
    // const(T[]) -> const(T)[]
    const short[] a = [1, 2, 3];
    formatTest( a, "[1, 2, 3]" );

    struct S { const(int[]) arr; alias arr this; }
    auto s = S([1,2,3]);
    formatTest( s, "[1, 2, 3]" );
}

@safe unittest
{
    // 6640
    struct Range
    {
      @safe:
        string value;
        @property bool empty() const { return !value.length; }
        @property dchar front() const { return value.front; }
        void popFront() { value.popFront(); }

        @property size_t length() const { return value.length; }
    }
    immutable table =
    [
        ["[%s]", "[string]"],
        ["[%10s]", "[    string]"],
        ["[%-10s]", "[string    ]"],
        ["[%(%02x %)]", "[73 74 72 69 6e 67]"],
        ["[%(%c %)]", "[s t r i n g]"],
    ];
    foreach (e; table)
    {
        formatTest(e[0], "string", e[1]);
        formatTest(e[0], Range("string"), e[1]);
    }
}

@system unittest
{
    // string literal from valid UTF sequence is encoding free.
    foreach (StrType; AliasSeq!(string, wstring, dstring))
    {
        // Valid and printable (ASCII)
        formatTest( [cast(StrType)"hello"],
                    `["hello"]` );

        // 1 character escape sequences (' is not escaped in strings)
        formatTest( [cast(StrType)"\"'\0\\\a\b\f\n\r\t\v"],
                    `["\"'\0\\\a\b\f\n\r\t\v"]` );

        // 1 character optional escape sequences
        formatTest( [cast(StrType)"\'\?"],
                    `["'?"]` );

        // Valid and non-printable code point (<= U+FF)
        formatTest( [cast(StrType)"\x10\x1F\x20test"],
                    `["\x10\x1F test"]` );

        // Valid and non-printable code point (<= U+FFFF)
        formatTest( [cast(StrType)"\u200B..\u200F"],
                    `["\u200B..\u200F"]` );

        // Valid and non-printable code point (<= U+10FFFF)
        formatTest( [cast(StrType)"\U000E0020..\U000E007F"],
                    `["\U000E0020..\U000E007F"]` );
    }

    // invalid UTF sequence needs hex-string literal postfix (c/w/d)
    {
        // U+FFFF with UTF-8 (Invalid code point for interchange)
        formatTest( [cast(string)[0xEF, 0xBF, 0xBF]],
                    `[x"EF BF BF"c]` );

        // U+FFFF with UTF-16 (Invalid code point for interchange)
        formatTest( [cast(wstring)[0xFFFF]],
                    `[x"FFFF"w]` );

        // U+FFFF with UTF-32 (Invalid code point for interchange)
        formatTest( [cast(dstring)[0xFFFF]],
                    `[x"FFFF"d]` );
    }
}

@safe unittest
{
    // nested range formatting with array of string
    formatTest( "%({%(%02x %)}%| %)", ["test", "msg"],
                `{74 65 73 74} {6d 73 67}` );
}

@safe unittest
{
    // stop auto escaping inside range formatting
    auto arr = ["hello", "world"];
    formatTest( "%(%s, %)",  arr, `"hello", "world"` );
    formatTest( "%-(%s, %)", arr, `hello, world` );

    auto aa1 = [1:"hello", 2:"world"];
    formatTest( "%(%s:%s, %)",  aa1, [`1:"hello", 2:"world"`, `2:"world", 1:"hello"`] );
    formatTest( "%-(%s:%s, %)", aa1, [`1:hello, 2:world`, `2:world, 1:hello`] );

    auto aa2 = [1:["ab", "cd"], 2:["ef", "gh"]];
    formatTest( "%-(%s:%s, %)",        aa2, [`1:["ab", "cd"], 2:["ef", "gh"]`, `2:["ef", "gh"], 1:["ab", "cd"]`] );
    formatTest( "%-(%s:%(%s%), %)",    aa2, [`1:"ab""cd", 2:"ef""gh"`, `2:"ef""gh", 1:"ab""cd"`] );
    formatTest( "%-(%s:%-(%s%)%|, %)", aa2, [`1:abcd, 2:efgh`, `2:efgh, 1:abcd`] );
}

// input range formatting
private void formatRange(Writer, T, Char)(ref Writer w, ref T val, const ref FormatSpec!Char f)
if (isInputRange!T)
{
    import std.conv : text;

    // Formatting character ranges like string
    if (f.spec == 's')
    {
        alias E = ElementType!T;

        static if (!is(E == enum) && is(CharTypeOf!E))
        {
            static if (is(StringTypeOf!T))
            {
                auto s = val[0 .. f.precision < $ ? f.precision : $];
                if (!f.flDash)
                {
                    // right align
                    if (f.width > s.length)
                        foreach (i ; 0 .. f.width - s.length) put(w, ' ');
                    put(w, s);
                }
                else
                {
                    // left align
                    put(w, s);
                    if (f.width > s.length)
                        foreach (i ; 0 .. f.width - s.length) put(w, ' ');
                }
            }
            else
            {
                if (!f.flDash)
                {
                    static if (hasLength!T)
                    {
                        // right align
                        auto len = val.length;
                    }
                    else static if (isForwardRange!T && !isInfinite!T)
                    {
                        auto len = walkLength(val.save);
                    }
                    else
                    {
                        enforce(f.width == 0, "Cannot right-align a range without length");
                        size_t len = 0;
                    }
                    if (f.precision != f.UNSPECIFIED && len > f.precision)
                        len = f.precision;

                    if (f.width > len)
                        foreach (i ; 0 .. f.width - len)
                            put(w, ' ');
                    if (f.precision == f.UNSPECIFIED)
                        put(w, val);
                    else
                    {
                        size_t printed = 0;
                        for (; !val.empty && printed < f.precision; val.popFront(), ++printed)
                            put(w, val.front);
                    }
                }
                else
                {
                    size_t printed = void;

                    // left align
                    if (f.precision == f.UNSPECIFIED)
                    {
                        static if (hasLength!T)
                        {
                            printed = val.length;
                            put(w, val);
                        }
                        else
                        {
                            printed = 0;
                            for (; !val.empty; val.popFront(), ++printed)
                                put(w, val.front);
                        }
                    }
                    else
                    {
                        printed = 0;
                        for (; !val.empty && printed < f.precision; val.popFront(), ++printed)
                            put(w, val.front);
                    }

                    if (f.width > printed)
                        foreach (i ; 0 .. f.width - printed)
                            put(w, ' ');
                }
            }
        }
        else
        {
            put(w, f.seqBefore);
            if (!val.empty)
            {
                formatElement(w, val.front, f);
                val.popFront();
                for (size_t i; !val.empty; val.popFront(), ++i)
                {
                    put(w, f.seqSeparator);
                    formatElement(w, val.front, f);
                }
            }
            static if (!isInfinite!T) put(w, f.seqAfter);
        }
    }
    else if (f.spec == 'r')
    {
        static if (is(DynamicArrayTypeOf!T))
        {
            alias ARR = DynamicArrayTypeOf!T;
            foreach (e ; cast(ARR) val)
            {
                formatValue(w, e, f);
            }
        }
        else
        {
            for (size_t i; !val.empty; val.popFront(), ++i)
            {
                formatValue(w, val.front, f);
            }
        }
    }
    else if (f.spec == '(')
    {
        if (val.empty)
            return;
        // Nested specifier is to be used
        for (;;)
        {
            auto fmt = FormatSpec!Char(f.nested);
            fmt.writeUpToNextSpec(w);
            if (f.flDash)
                formatValue(w, val.front, fmt);
            else
                formatElement(w, val.front, fmt);
            if (f.sep !is null)
            {
                put(w, fmt.trailing);
                val.popFront();
                if (val.empty)
                    break;
                put(w, f.sep);
            }
            else
            {
                val.popFront();
                if (val.empty)
                    break;
                put(w, fmt.trailing);
            }
        }
    }
    else
        throw new Exception(text("Incorrect format specifier for range: %", f.spec));
}

@safe pure unittest
{
    assert(collectExceptionMsg(format("%d", "hi")).back == 'd');
}

// character formatting with ecaping
private void formatChar(Writer)(ref Writer w, in dchar c, in char quote)
{
    import std.uni : isGraphical;

    string fmt;
    if (isGraphical(c))
    {
        if (c == quote || c == '\\')
            put(w, '\\');
        put(w, c);
        return;
    }
    else if (c <= 0xFF)
    {
        if (c < 0x20)
        {
            foreach (i, k; "\n\r\t\a\b\f\v\0")
            {
                if (c == k)
                {
                    put(w, '\\');
                    put(w, "nrtabfv0"[i]);
                    return;
                }
            }
        }
        fmt = "\\x%02X";
    }
    else if (c <= 0xFFFF)
        fmt = "\\u%04X";
    else
        fmt = "\\U%08X";

    formattedWrite(w, fmt, cast(uint) c);
}

// undocumented because of deprecation
// string elements are formatted like UTF-8 string literals.
void formatElement(Writer, T, Char)(auto ref Writer w, T val, const ref FormatSpec!Char f)
if (is(StringTypeOf!T) && !is(T == enum))
{
    import std.array : appender;
    import std.utf : UTFException;

    StringTypeOf!T str = val;   // bug 8015

    if (f.spec == 's')
    {
        try
        {
            // ignore other specifications and quote
            auto app = appender!(typeof(val[0])[])();
            put(app, '\"');
            for (size_t i = 0; i < str.length; )
            {
                import std.utf : decode;

                auto c = decode(str, i);
                // \uFFFE and \uFFFF are considered valid by isValidDchar,
                // so need checking for interchange.
                if (c == 0xFFFE || c == 0xFFFF)
                    goto LinvalidSeq;
                formatChar(app, c, '"');
            }
            put(app, '\"');
            put(w, app.data);
            return;
        }
        catch (UTFException)
        {
        }

        // If val contains invalid UTF sequence, formatted like HexString literal
    LinvalidSeq:
        static if (is(typeof(str[0]) : const(char)))
        {
            enum postfix = 'c';
            alias IntArr = const(ubyte)[];
        }
        else static if (is(typeof(str[0]) : const(wchar)))
        {
            enum postfix = 'w';
            alias IntArr = const(ushort)[];
        }
        else static if (is(typeof(str[0]) : const(dchar)))
        {
            enum postfix = 'd';
            alias IntArr = const(uint)[];
        }
        formattedWrite(w, "x\"%(%02X %)\"%s", cast(IntArr) str, postfix);
    }
    else
        formatValue(w, str, f);
}

@safe pure unittest
{
    import std.array : appender;
    auto w = appender!string();
    auto spec = singleSpec("%s");
    formatElement(w, "Hello World", spec);

    assert(w.data == "\"Hello World\"");
}

@safe unittest
{
    // Test for bug 8015
    import std.typecons;

    struct MyStruct {
        string str;
        @property string toStr() {
            return str;
        }
        alias toStr this;
    }

    Tuple!(MyStruct) t;
}

// undocumented because of deprecation
// Character elements are formatted like UTF-8 character literals.
void formatElement(Writer, T, Char)(auto ref Writer w, T val, const ref FormatSpec!Char f)
if (is(CharTypeOf!T) && !is(T == enum))
{
    if (f.spec == 's')
    {
        put(w, '\'');
        formatChar(w, val, '\'');
        put(w, '\'');
    }
    else
        formatValue(w, val, f);
}

///
@safe unittest
{
    import std.array : appender;
    auto w = appender!string();
    auto spec = singleSpec("%s");
    formatElement(w, "H", spec);

    assert(w.data == "\"H\"", w.data);
}

// undocumented
// Maybe T is noncopyable struct, so receive it by 'auto ref'.
void formatElement(Writer, T, Char)(auto ref Writer w, auto ref T val, const ref FormatSpec!Char f)
if (!is(StringTypeOf!T) && !is(CharTypeOf!T) || is(T == enum))
{
    formatValue(w, val, f);
}

/**
   Associative arrays are formatted by using $(D ':') and $(D ", ") as
   separators, and enclosed by $(D '[') and $(D ']').

Params:
    w = The $(D OutputRange) to write to.
    obj = The value to write.
    f = The $(D FormatSpec) defining how to write the value.
 */
void formatValue(Writer, T, Char)(auto ref Writer w, T obj, const ref FormatSpec!Char f)
if (is(AssocArrayTypeOf!T) && !is(T == enum) && !hasToString!(T, Char))
{
    AssocArrayTypeOf!T val = obj;

    enforceFmt(f.spec == 's' || f.spec == '(',
        "incompatible format character for associative array argument: %" ~ f.spec);

    enum const(Char)[] defSpec = "%s" ~ f.keySeparator ~ "%s" ~ f.seqSeparator;
    auto fmtSpec = f.spec == '(' ? f.nested : defSpec;

    size_t i = 0;
    immutable end = val.length;

    if (f.spec == 's')
        put(w, f.seqBefore);
    foreach (k, ref v; val)
    {
        auto fmt = FormatSpec!Char(fmtSpec);
        fmt.writeUpToNextSpec(w);
        if (f.flDash)
        {
            formatValue(w, k, fmt);
            fmt.writeUpToNextSpec(w);
            formatValue(w, v, fmt);
        }
        else
        {
            formatElement(w, k, fmt);
            fmt.writeUpToNextSpec(w);
            formatElement(w, v, fmt);
        }
        if (f.sep !is null)
        {
            fmt.writeUpToNextSpec(w);
            if (++i != end)
                put(w, f.sep);
        }
        else
        {
            if (++i != end)
                fmt.writeUpToNextSpec(w);
        }
    }
    if (f.spec == 's')
        put(w, f.seqAfter);
}

///
@safe pure unittest
{
    import std.array : appender;
    auto w = appender!string();
    auto spec = singleSpec("%s");
    auto aa = ["H":"W"];
    formatElement(w, aa, spec);

    assert(w.data == "[\"H\":\"W\"]", w.data);
}

@safe unittest
{
    assert(collectExceptionMsg!FormatException(format("%d", [0:1])).back == 'd');

    int[string] aa0;
    formatTest( aa0, `[]` );

    // elements escaping
    formatTest(  ["aaa":1, "bbb":2],
               [`["aaa":1, "bbb":2]`, `["bbb":2, "aaa":1]`] );
    formatTest(  ['c':"str"],
                `['c':"str"]` );
    formatTest(  ['"':"\"", '\'':"'"],
               [`['"':"\"", '\'':"'"]`, `['\'':"'", '"':"\""]`] );

    // range formatting for AA
    auto aa3 = [1:"hello", 2:"world"];
    // escape
    formatTest( "{%(%s:%s $ %)}", aa3,
               [`{1:"hello" $ 2:"world"}`, `{2:"world" $ 1:"hello"}`]);
    // use range formatting for key and value, and use %|
    formatTest( "{%([%04d->%(%c.%)]%| $ %)}", aa3,
               [`{[0001->h.e.l.l.o] $ [0002->w.o.r.l.d]}`, `{[0002->w.o.r.l.d] $ [0001->h.e.l.l.o]}`] );

    // issue 12135
    formatTest("%(%s:<%s>%|,%)", [1:2], "1:<2>");
    formatTest("%(%s:<%s>%|%)" , [1:2], "1:<2>");
}

@system unittest
{
    class C1 { int[char] val; alias val this; this(int[char] v){ val = v; } }
    class C2 { int[char] val; alias val this; this(int[char] v){ val = v; }
               override string toString() const { return "C"; } }
    formatTest( new C1(['c':1, 'd':2]), [`['c':1, 'd':2]`, `['d':2, 'c':1]`] );
    formatTest( new C2(['c':1, 'd':2]), "C" );

    struct S1 { int[char] val; alias val this; }
    struct S2 { int[char] val; alias val this;
                string toString() const { return "S"; } }
    formatTest( S1(['c':1, 'd':2]), [`['c':1, 'd':2]`, `['d':2, 'c':1]`] );
    formatTest( S2(['c':1, 'd':2]), "S" );
}

@safe unittest  // Issue 8921
{
    enum E : char { A = 'a', B = 'b', C = 'c' }
    E[3] e = [E.A, E.B, E.C];
    formatTest(e, "[A, B, C]");

    E[] e2 = [E.A, E.B, E.C];
    formatTest(e2, "[A, B, C]");
}

template hasToString(T, Char)
{
    static if (isPointer!T && !isAggregateType!T)
    {
        // X* does not have toString, even if X is aggregate type has toString.
        enum hasToString = 0;
    }
    else static if (is(typeof({ T val = void; FormatSpec!Char f; val.toString((const(char)[] s){}, f); })))
    {
        enum hasToString = 4;
    }
    else static if (is(typeof({ T val = void; val.toString((const(char)[] s){}, "%s"); })))
    {
        enum hasToString = 3;
    }
    else static if (is(typeof({ T val = void; val.toString((const(char)[] s){}); })))
    {
        enum hasToString = 2;
    }
    else static if (is(typeof({ T val = void; return val.toString(); }()) S) && isSomeString!S)
    {
        enum hasToString = 1;
    }
    else
    {
        enum hasToString = 0;
    }
}

// object formatting with toString
private void formatObject(Writer, T, Char)(ref Writer w, ref T val, const ref FormatSpec!Char f)
if (hasToString!(T, Char))
{
    static if (is(typeof(val.toString((const(char)[] s){}, f))))
    {
        val.toString((const(char)[] s) { put(w, s); }, f);
    }
    else static if (is(typeof(val.toString((const(char)[] s){}, "%s"))))
    {
        val.toString((const(char)[] s) { put(w, s); }, f.getCurFmtStr());
    }
    else static if (is(typeof(val.toString((const(char)[] s){}))))
    {
        val.toString((const(char)[] s) { put(w, s); });
    }
    else static if (is(typeof(val.toString()) S) && isSomeString!S)
    {
        put(w, val.toString());
    }
    else
        static assert(0);
}

void enforceValidFormatSpec(T, Char)(const ref FormatSpec!Char f)
{
    static if (!isInputRange!T && hasToString!(T, Char) != 4)
    {
        enforceFmt(f.spec == 's',
            "Expected '%s' format specifier for type '" ~ T.stringof ~ "'");
    }
}

@system unittest
{
    static interface IF1 { }
    class CIF1 : IF1 { }
    static struct SF1 { }
    static union UF1 { }
    static class CF1 { }

    static interface IF2 { string toString(); }
    static class CIF2 : IF2 { override string toString() { return ""; } }
    static struct SF2 { string toString() { return ""; } }
    static union UF2 { string toString() { return ""; } }
    static class CF2 { override string toString() { return ""; } }

    static interface IK1 { void toString(scope void delegate(const(char)[]) sink,
                           FormatSpec!char) const; }
    static class CIK1 : IK1 { override void toString(scope void delegate(const(char)[]) sink,
                              FormatSpec!char) const { sink("CIK1"); } }
    static struct KS1 { void toString(scope void delegate(const(char)[]) sink,
                        FormatSpec!char) const { sink("KS1"); } }

    static union KU1 { void toString(scope void delegate(const(char)[]) sink,
                       FormatSpec!char) const { sink("KU1"); } }

    static class KC1 { void toString(scope void delegate(const(char)[]) sink,
                       FormatSpec!char) const { sink("KC1"); } }

    IF1 cif1 = new CIF1;
    assertThrown!FormatException(format("%f", cif1));
    assertThrown!FormatException(format("%f", SF1()));
    assertThrown!FormatException(format("%f", UF1()));
    assertThrown!FormatException(format("%f", new CF1()));

    IF2 cif2 = new CIF2;
    assertThrown!FormatException(format("%f", cif2));
    assertThrown!FormatException(format("%f", SF2()));
    assertThrown!FormatException(format("%f", UF2()));
    assertThrown!FormatException(format("%f", new CF2()));

    IK1 cik1 = new CIK1;
    assert(format("%f", cik1) == "CIK1");
    assert(format("%f", KS1()) == "KS1");
    assert(format("%f", KU1()) == "KU1");
    assert(format("%f", new KC1()) == "KC1");
}

/**
   Aggregates ($(D struct), $(D union), $(D class), and $(D interface)) are
   basically formatted by calling $(D toString).
   $(D toString) should have one of the following signatures:

---
const void toString(scope void delegate(const(char)[]) sink, FormatSpec fmt);
const void toString(scope void delegate(const(char)[]) sink, string fmt);
const void toString(scope void delegate(const(char)[]) sink);
const string toString();
---

   For the class objects which have input range interface,
   $(UL $(LI If the instance $(D toString) has overridden
             $(D Object.toString), it is used.)
        $(LI Otherwise, the objects are formatted as input range.))

   For the struct and union objects which does not have $(D toString),
   $(UL $(LI If they have range interface, formatted as input range.)
        $(LI Otherwise, they are formatted like $(D Type(field1, filed2, ...)).))

   Otherwise, are formatted just as their type name.
 */
void formatValue(Writer, T, Char)(auto ref Writer w, T val, const ref FormatSpec!Char f)
if (is(T == class) && !is(T == enum))
{
    enforceValidFormatSpec!(T, Char)(f);
    // TODO: Change this once toString() works for shared objects.
    static assert(!is(T == shared), "unable to format shared objects");

    if (val is null)
        put(w, "null");
    else
    {
        static if (hasToString!(T, Char) > 1 || (!isInputRange!T && !is(BuiltinTypeOf!T)))
        {
            formatObject!(Writer, T, Char)(w, val, f);
        }
        else
        {
          //string delegate() dg = &val.toString;
            Object o = val;     // workaround
            string delegate() dg = &o.toString;
            if (dg.funcptr != &Object.toString) // toString is overridden
            {
                formatObject(w, val, f);
            }
            else static if (isInputRange!T)
            {
                formatRange(w, val, f);
            }
            else static if (is(BuiltinTypeOf!T X))
            {
                X x = val;
                formatValue(w, x, f);
            }
            else
            {
                formatObject(w, val, f);
            }
        }
    }
}

/++
   $(D formatValue) allows to reuse existing format specifiers:
 +/
@system unittest
{
   import std.format;

   struct Point
   {
       int x, y;

       void toString(scope void delegate(const(char)[]) sink,
                     FormatSpec!char fmt) const
       {
           sink("(");
           sink.formatValue(x, fmt);
           sink(",");
           sink.formatValue(y, fmt);
           sink(")");
       }
   }

   auto p = Point(16,11);
   assert(format("%03d", p) == "(016,011)");
   assert(format("%02x", p) == "(10,0b)");
}

/++
   The following code compares the use of $(D formatValue) and $(D formattedWrite).
 +/
@safe pure unittest
{
   import std.array : appender;
   import std.format;

   auto writer1 = appender!string();
   writer1.formattedWrite("%08b", 42);

   auto writer2 = appender!string();
   auto f = singleSpec("%08b");
   writer2.formatValue(42, f);

   assert(writer1.data == writer2.data && writer1.data == "00101010");
}

@system unittest
{
    import std.array : appender;
    import std.range.interfaces;
    // class range (issue 5154)
    auto c = inputRangeObject([1,2,3,4]);
    formatTest( c, "[1, 2, 3, 4]" );
    assert(c.empty);
    c = null;
    formatTest( c, "null" );
}

@system unittest
{
    // 5354
    // If the class has both range I/F and custom toString, the use of custom
    // toString routine is prioritized.

    // Enable the use of custom toString that gets a sink delegate
    // for class formatting.

    enum inputRangeCode =
    q{
        int[] arr;
        this(int[] a){ arr = a; }
        @property int front() const { return arr[0]; }
        @property bool empty() const { return arr.length == 0; }
        void popFront(){ arr = arr[1..$]; }
    };

    class C1
    {
        mixin(inputRangeCode);
        void toString(scope void delegate(const(char)[]) dg, const ref FormatSpec!char f) const { dg("[012]"); }
    }
    class C2
    {
        mixin(inputRangeCode);
        void toString(scope void delegate(const(char)[]) dg, string f) const { dg("[012]"); }
    }
    class C3
    {
        mixin(inputRangeCode);
        void toString(scope void delegate(const(char)[]) dg) const { dg("[012]"); }
    }
    class C4
    {
        mixin(inputRangeCode);
        override string toString() const { return "[012]"; }
    }
    class C5
    {
        mixin(inputRangeCode);
    }

    formatTest( new C1([0, 1, 2]), "[012]" );
    formatTest( new C2([0, 1, 2]), "[012]" );
    formatTest( new C3([0, 1, 2]), "[012]" );
    formatTest( new C4([0, 1, 2]), "[012]" );
    formatTest( new C5([0, 1, 2]), "[0, 1, 2]" );
}

/// ditto
void formatValue(Writer, T, Char)(auto ref Writer w, T val, const ref FormatSpec!Char f)
if (is(T == interface) && (hasToString!(T, Char) || !is(BuiltinTypeOf!T)) && !is(T == enum))
{
    enforceValidFormatSpec!(T, Char)(f);
    if (val is null)
        put(w, "null");
    else
    {
        static if (hasToString!(T, Char))
        {
            formatObject(w, val, f);
        }
        else static if (isInputRange!T)
        {
            formatRange(w, val, f);
        }
        else
        {
            version (Windows)
            {
                import core.sys.windows.com : IUnknown;
                static if (is(T : IUnknown))
                {
                    formatValue(w, *cast(void**)&val, f);
                }
                else
                {
                    formatValue(w, cast(Object) val, f);
                }
            }
            else
            {
                formatValue(w, cast(Object) val, f);
            }
        }
    }
}

@system unittest
{
    // interface
    import std.range.interfaces;
    InputRange!int i = inputRangeObject([1,2,3,4]);
    formatTest( i, "[1, 2, 3, 4]" );
    assert(i.empty);
    i = null;
    formatTest( i, "null" );

    // interface (downcast to Object)
    interface Whatever {}
    class C : Whatever
    {
        override @property string toString() const { return "ab"; }
    }
    Whatever val = new C;
    formatTest( val, "ab" );

    // Issue 11175
    version (Windows)
    {
        import core.sys.windows.com : IUnknown, IID;
        import core.sys.windows.windows : HRESULT;

        interface IUnknown2 : IUnknown { }

        class D : IUnknown2
        {
            extern(Windows) HRESULT QueryInterface(const(IID)* riid, void** pvObject) { return typeof(return).init; }
            extern(Windows) uint AddRef() { return 0; }
            extern(Windows) uint Release() { return 0; }
        }

        IUnknown2 d = new D;
        string expected = format("%X", cast(void*) d);
        formatTest(d, expected);
    }
}

/// ditto
// Maybe T is noncopyable struct, so receive it by 'auto ref'.
void formatValue(Writer, T, Char)(auto ref Writer w, auto ref T val, const ref FormatSpec!Char f)
if ((is(T == struct) || is(T == union)) && (hasToString!(T, Char) || !is(BuiltinTypeOf!T)) && !is(T == enum))
{
    enforceValidFormatSpec!(T, Char)(f);
    static if (hasToString!(T, Char))
    {
        formatObject(w, val, f);
    }
    else static if (isInputRange!T)
    {
        formatRange(w, val, f);
    }
    else static if (is(T == struct))
    {
        enum left = T.stringof~"(";
        enum separator = ", ";
        enum right = ")";

        put(w, left);
        foreach (i, e; val.tupleof)
        {
            static if (0 < i && val.tupleof[i-1].offsetof == val.tupleof[i].offsetof)
            {
                static if (i == val.tupleof.length - 1 || val.tupleof[i].offsetof != val.tupleof[i+1].offsetof)
                    put(w, separator~val.tupleof[i].stringof[4..$]~"}");
                else
                    put(w, separator~val.tupleof[i].stringof[4..$]);
            }
            else
            {
                static if (i+1 < val.tupleof.length && val.tupleof[i].offsetof == val.tupleof[i+1].offsetof)
                    put(w, (i > 0 ? separator : "")~"#{overlap "~val.tupleof[i].stringof[4..$]);
                else
                {
                    static if (i > 0)
                        put(w, separator);
                    formatElement(w, e, f);
                }
            }
        }
        put(w, right);
    }
    else
    {
        put(w, T.stringof);
    }
}

@safe unittest
{
    // bug 4638
    struct U8  {  string toString() const { return "blah"; } }
    struct U16 { wstring toString() const { return "blah"; } }
    struct U32 { dstring toString() const { return "blah"; } }
    formatTest( U8(), "blah" );
    formatTest( U16(), "blah" );
    formatTest( U32(), "blah" );
}

@safe unittest
{
    // 3890
    struct Int{ int n; }
    struct Pair{ string s; Int i; }
    formatTest( Pair("hello", Int(5)),
                `Pair("hello", Int(5))` );
}

@system unittest
{
    // union formatting without toString
    union U1
    {
        int n;
        string s;
    }
    U1 u1;
    formatTest( u1, "U1" );

    // union formatting with toString
    union U2
    {
        int n;
        string s;
        string toString() const { return s; }
    }
    U2 u2;
    u2.s = "hello";
    formatTest( u2, "hello" );
}

@system unittest
{
    import std.array;
    // 7230
    static struct Bug7230
    {
        string s = "hello";
        union {
            string a;
            int b;
            double c;
        }
        long x = 10;
    }

    Bug7230 bug;
    bug.b = 123;

    FormatSpec!char f;
    auto w = appender!(char[])();
    formatValue(w, bug, f);
    assert(w.data == `Bug7230("hello", #{overlap a, b, c}, 10)`);
}

@safe unittest
{
    import std.array;
    static struct S{ @disable this(this); }
    S s;

    FormatSpec!char f;
    auto w = appender!string();
    formatValue(w, s, f);
    assert(w.data == "S()");
}

/**
$(D enum) is formatted like its base value.

Params:
    w = The $(D OutputRange) to write to.
    val = The value to write.
    f = The $(D FormatSpec) defining how to write the value.
 */
void formatValue(Writer, T, Char)(auto ref Writer w, T val, const ref FormatSpec!Char f)
if (is(T == enum))
{
    if (f.spec == 's')
    {
        foreach (i, e; EnumMembers!T)
        {
            if (val == e)
            {
                formatValue(w, __traits(allMembers, T)[i], f);
                return;
            }
        }

        // val is not a member of T, output cast(T) rawValue instead.
        put(w, "cast(" ~ T.stringof ~ ")");
        static assert(!is(OriginalType!T == T));
    }
    formatValue(w, cast(OriginalType!T) val, f);
}

///
@safe pure unittest
{
    import std.array : appender;
    auto w = appender!string();
    auto spec = singleSpec("%s");

    enum A { first, second, third }

    formatElement(w, A.second, spec);

    assert(w.data == "second");
}

@safe unittest
{
    enum A { first, second, third }
    formatTest( A.second, "second" );
    formatTest( cast(A) 72, "cast(A)72" );
}
@safe unittest
{
    enum A : string { one = "uno", two = "dos", three = "tres" }
    formatTest( A.three, "three" );
    formatTest( cast(A)"mill\&oacute;n", "cast(A)mill\&oacute;n" );
}
@safe unittest
{
    enum A : bool { no, yes }
    formatTest( A.yes, "yes" );
    formatTest( A.no, "no" );
}
@safe unittest
{
    // Test for bug 6892
    enum Foo { A = 10 }
    formatTest("%s",    Foo.A, "A");
    formatTest(">%4s<", Foo.A, ">   A<");
    formatTest("%04d",  Foo.A, "0010");
    formatTest("%+2u",  Foo.A, "+10");
    formatTest("%02x",  Foo.A, "0a");
    formatTest("%3o",   Foo.A, " 12");
    formatTest("%b",    Foo.A, "1010");
}

/**
   Pointers are formatted as hex integers.
 */
void formatValue(Writer, T, Char)(auto ref Writer w, T val, const ref FormatSpec!Char f)
if (isPointer!T && !is(T == enum) && !hasToString!(T, Char))
{
    static if (isInputRange!T)
    {
        if (val !is null)
        {
            formatRange(w, *val, f);
            return;
        }
    }

    static if (is(typeof({ shared const void* p = val; })))
        alias SharedOf(T) = shared(T);
    else
        alias SharedOf(T) = T;

    const SharedOf!(void*) p = val;
    const pnum = ()@trusted{ return cast(ulong) p; }();

    if (f.spec == 's')
    {
        if (p is null)
        {
            put(w, "null");
            return;
        }
        FormatSpec!Char fs = f; // fs is copy for change its values.
        fs.spec = 'X';
        formatValue(w, pnum, fs);
    }
    else
    {
        enforceFmt(f.spec == 'X' || f.spec == 'x',
           "Expected one of %s, %x or %X for pointer type.");
        formatValue(w, pnum, f);
    }
}

@safe pure unittest
{
    // pointer
    import std.range;
    auto r = retro([1,2,3,4]);
    auto p = ()@trusted{ auto p = &r; return p; }();
    formatTest( p, "[4, 3, 2, 1]" );
    assert(p.empty);
    p = null;
    formatTest( p, "null" );

    auto q = ()@trusted{ return cast(void*) 0xFFEECCAA; }();
    formatTest( q, "FFEECCAA" );
}

@system pure unittest
{
    // Test for issue 7869
    struct S
    {
        string toString() const { return ""; }
    }
    S* p = null;
    formatTest( p, "null" );

    S* q = cast(S*) 0xFFEECCAA;
    formatTest( q, "FFEECCAA" );
}

@system unittest
{
    // Test for issue 8186
    class B
    {
        int*a;
        this(){ a = new int; }
        alias a this;
    }
    formatTest( B.init, "null" );
}

@system pure unittest
{
    // Test for issue 9336
    shared int i;
    format("%s", &i);
}

@system pure unittest
{
    // Test for issue 11778
    int* p = null;
    assertThrown(format("%d", p));
    assertThrown(format("%04d", p + 2));
}

@safe pure unittest
{
    // Test for issue 12505
    void* p = null;
    formatTest( "%08X", p, "00000000" );
}

/**
   Delegates are formatted by 'ReturnType delegate(Parameters) FunctionAttributes'
 */
void formatValue(Writer, T, Char)(auto ref Writer w, scope T, const ref FormatSpec!Char f)
if (isDelegate!T)
{
    formatValue(w, T.stringof, f);
}

///
@safe pure unittest
{
    import std.conv : to;

    int i;

    int foo(short k) @nogc
    {
        return i + k;
    }

    @system int delegate(short) @nogc bar() nothrow pure
    {
        int* p = new int;
        return &foo;
    }

    assert(to!string(&bar) == "int delegate(short) @nogc delegate() pure nothrow @system");
}

@safe unittest
{
    void func() @system { __gshared int x; ++x; throw new Exception("msg"); }
    version (linux) formatTest( &func, "void delegate() @system" );
}

@safe pure unittest
{
    int[] a = [ 1, 3, 2 ];
    formatTest( "testing %(%s & %) embedded", a,
                "testing 1 & 3 & 2 embedded");
    formatTest( "testing %((%s) %)) wyda3", a,
                "testing (1) (3) (2) wyda3" );

    int[0] empt = [];
    formatTest( "(%s)", empt,
                "([])" );
}

//------------------------------------------------------------------------------
// Fix for issue 1591
private int getNthInt(string kind, A...)(uint index, A args)
{
    return getNth!(kind, isIntegral,int)(index, args);
}

private T getNth(string kind, alias Condition, T, A...)(uint index, A args)
{
    import std.conv : text, to;

    switch (index)
    {
        foreach (n, _; A)
        {
            case n:
                static if (Condition!(typeof(args[n])))
                {
                    return to!T(args[n]);
                }
                else
                {
                    throw new FormatException(
                        text(kind, " expected, not ", typeof(args[n]).stringof,
                            " for argument #", index + 1));
                }
        }
        default:
            throw new FormatException(
                text("Missing ", kind, " argument"));
    }
}

@safe unittest
{
    // width/precision
    assert(collectExceptionMsg!FormatException(format("%*.d", 5.1, 2))
        == "integer width expected, not double for argument #1");
    assert(collectExceptionMsg!FormatException(format("%-1*.d", 5.1, 2))
        == "integer width expected, not double for argument #1");

    assert(collectExceptionMsg!FormatException(format("%.*d", '5', 2))
        == "integer precision expected, not char for argument #1");
    assert(collectExceptionMsg!FormatException(format("%-1.*d", 4.7, 3))
        == "integer precision expected, not double for argument #1");
    assert(collectExceptionMsg!FormatException(format("%.*d", 5))
        == "Orphan format specifier: %d");
    assert(collectExceptionMsg!FormatException(format("%*.*d", 5))
        == "Missing integer precision argument");

    // separatorCharPos
    assert(collectExceptionMsg!FormatException(format("%,?d", 5))
        == "separator character expected, not int for argument #1");
    assert(collectExceptionMsg!FormatException(format("%,?d", '?'))
        == "Orphan format specifier: %d");
    assert(collectExceptionMsg!FormatException(format("%.*,*?d", 5))
        == "Missing separator digit width argument");
}

/* ======================== Unit Tests ====================================== */

version (unittest)
void formatTest(T)(T val, string expected, size_t ln = __LINE__, string fn = __FILE__)
{
    import core.exception : AssertError;
    import std.array : appender;
    import std.conv : text;
    FormatSpec!char f;
    auto w = appender!string();
    formatValue(w, val, f);
    enforce!AssertError(
            w.data == expected,
            text("expected = `", expected, "`, result = `", w.data, "`"), fn, ln);
}

version (unittest)
void formatTest(T)(string fmt, T val, string expected, size_t ln = __LINE__, string fn = __FILE__) @safe
{
    import core.exception : AssertError;
    import std.array : appender;
    import std.conv : text;
    auto w = appender!string();
    formattedWrite(w, fmt, val);
    enforce!AssertError(
            w.data == expected,
            text("expected = `", expected, "`, result = `", w.data, "`"), fn, ln);
}

version (unittest)
void formatTest(T)(T val, string[] expected, size_t ln = __LINE__, string fn = __FILE__)
{
    import core.exception : AssertError;
    import std.array : appender;
    import std.conv : text;
    FormatSpec!char f;
    auto w = appender!string();
    formatValue(w, val, f);
    foreach (cur; expected)
    {
        if (w.data == cur) return;
    }
    enforce!AssertError(
            false,
            text("expected one of `", expected, "`, result = `", w.data, "`"), fn, ln);
}

version (unittest)
void formatTest(T)(string fmt, T val, string[] expected, size_t ln = __LINE__, string fn = __FILE__) @safe
{
    import core.exception : AssertError;
    import std.array : appender;
    import std.conv : text;
    auto w = appender!string();
    formattedWrite(w, fmt, val);
    foreach (cur; expected)
    {
        if (w.data == cur) return;
    }
    enforce!AssertError(
            false,
            text("expected one of `", expected, "`, result = `", w.data, "`"), fn, ln);
}

@safe /*pure*/ unittest     // formatting floating point values is now impure
{
    import std.array;

    auto stream = appender!string();
    formattedWrite(stream, "%s", 1.1);
    assert(stream.data == "1.1", stream.data);
}

@safe pure unittest
{
    import std.algorithm;
    import std.array;

    auto stream = appender!string();
    formattedWrite(stream, "%s", map!"a*a"([2, 3, 5]));
    assert(stream.data == "[4, 9, 25]", stream.data);

    // Test shared data.
    stream = appender!string();
    shared int s = 6;
    formattedWrite(stream, "%s", s);
    assert(stream.data == "6");
}

@safe pure unittest
{
    import std.array;
    auto stream = appender!string();
    formattedWrite(stream, "%u", 42);
    assert(stream.data == "42", stream.data);
}

@safe pure unittest
{
    // testing raw writes
    import std.array;
    auto w = appender!(char[])();
    uint a = 0x02030405;
    formattedWrite(w, "%+r", a);
    assert(w.data.length == 4 && w.data[0] == 2 && w.data[1] == 3
        && w.data[2] == 4 && w.data[3] == 5);
    w.clear();
    formattedWrite(w, "%-r", a);
    assert(w.data.length == 4 && w.data[0] == 5 && w.data[1] == 4
        && w.data[2] == 3 && w.data[3] == 2);
}

@safe pure unittest
{
    // testing positional parameters
    import std.array;
    auto w = appender!(char[])();
    formattedWrite(w,
            "Numbers %2$s and %1$s are reversed and %1$s%2$s repeated",
            42, 0);
    assert(w.data == "Numbers 0 and 42 are reversed and 420 repeated",
            w.data);
    assert(collectExceptionMsg!FormatException(formattedWrite(w, "%1$s, %3$s", 1, 2))
        == "Positional specifier %3$s index exceeds 2");

    w.clear();
    formattedWrite(w, "asd%s", 23);
    assert(w.data == "asd23", w.data);
    w.clear();
    formattedWrite(w, "%s%s", 23, 45);
    assert(w.data == "2345", w.data);
}

@safe unittest
{
    import core.stdc.string : strlen;
    import std.array : appender;
    import std.conv : text, octal;
    import core.stdc.stdio : snprintf;

    debug(format) printf("std.format.format.unittest\n");

    auto stream = appender!(char[])();
    //goto here;

    formattedWrite(stream,
            "hello world! %s %s ", true, 57, 1_000_000_000, 'x', " foo");
    assert(stream.data == "hello world! true 57 ",
        stream.data);

    stream.clear();
    formattedWrite(stream, "%g %A %s", 1.67, -1.28, float.nan);
    // core.stdc.stdio.fwrite(stream.data.ptr, stream.data.length, 1, stderr);

    /* The host C library is used to format floats.  C99 doesn't
    * specify what the hex digit before the decimal point is for
    * %A.  */

    version (CRuntime_Glibc)
    {
        assert(stream.data == "1.67 -0X1.47AE147AE147BP+0 nan",
                stream.data);
    }
    else version (OSX)
    {
        assert(stream.data == "1.67 -0X1.47AE147AE147BP+0 nan",
                stream.data);
    }
    else version (MinGW)
    {
        assert(stream.data == "1.67 -0XA.3D70A3D70A3D8P-3 nan",
                stream.data);
    }
    else version (CRuntime_Microsoft)
    {
        assert(stream.data == "1.67 -0X1.47AE14P+0 nan"
            || stream.data == "1.67 -0X1.47AE147AE147BP+0 nan", // MSVCRT 14+ (VS 2015)
                stream.data);
    }
    else
    {
        assert(stream.data == "1.67 -0X1.47AE147AE147BP+0 nan",
                stream.data);
    }
    stream.clear();

    formattedWrite(stream, "%x %X", 0x1234AF, 0xAFAFAFAF);
    assert(stream.data == "1234af AFAFAFAF");
    stream.clear();

    formattedWrite(stream, "%b %o", 0x1234AF, 0xAFAFAFAF);
    assert(stream.data == "100100011010010101111 25753727657");
    stream.clear();

    formattedWrite(stream, "%d %s", 0x1234AF, 0xAFAFAFAF);
    assert(stream.data == "1193135 2947526575");
    stream.clear();

    // formattedWrite(stream, "%s", 1.2 + 3.4i);
    // assert(stream.data == "1.2+3.4i");
    // stream.clear();

    formattedWrite(stream, "%a %A", 1.32, 6.78f);
    //formattedWrite(stream, "%x %X", 1.32);
    version (CRuntime_Microsoft)
        assert(stream.data == "0x1.51eb85p+0 0X1.B1EB86P+2"
            || stream.data == "0x1.51eb851eb851fp+0 0X1.B1EB860000000P+2"); // MSVCRT 14+ (VS 2015)
    else
        assert(stream.data == "0x1.51eb851eb851fp+0 0X1.B1EB86P+2");
    stream.clear();

    formattedWrite(stream, "%#06.*f",2,12.345);
    assert(stream.data == "012.35");
    stream.clear();

    formattedWrite(stream, "%#0*.*f",6,2,12.345);
    assert(stream.data == "012.35");
    stream.clear();

    const real constreal = 1;
    formattedWrite(stream, "%g",constreal);
    assert(stream.data == "1");
    stream.clear();

    formattedWrite(stream, "%7.4g:", 12.678);
    assert(stream.data == "  12.68:");
    stream.clear();

    formattedWrite(stream, "%7.4g:", 12.678L);
    assert(stream.data == "  12.68:");
    stream.clear();

    formattedWrite(stream, "%04f|%05d|%#05x|%#5x",-4.0,-10,1,1);
    assert(stream.data == "-4.000000|-0010|0x001|  0x1",
            stream.data);
    stream.clear();

    int i;
    string s;

    i = -10;
    formattedWrite(stream, "%d|%3d|%03d|%1d|%01.4f",i,i,i,i,cast(double) i);
    assert(stream.data == "-10|-10|-10|-10|-10.0000");
    stream.clear();

    i = -5;
    formattedWrite(stream, "%d|%3d|%03d|%1d|%01.4f",i,i,i,i,cast(double) i);
    assert(stream.data == "-5| -5|-05|-5|-5.0000");
    stream.clear();

    i = 0;
    formattedWrite(stream, "%d|%3d|%03d|%1d|%01.4f",i,i,i,i,cast(double) i);
    assert(stream.data == "0|  0|000|0|0.0000");
    stream.clear();

    i = 5;
    formattedWrite(stream, "%d|%3d|%03d|%1d|%01.4f",i,i,i,i,cast(double) i);
    assert(stream.data == "5|  5|005|5|5.0000");
    stream.clear();

    i = 10;
    formattedWrite(stream, "%d|%3d|%03d|%1d|%01.4f",i,i,i,i,cast(double) i);
    assert(stream.data == "10| 10|010|10|10.0000");
    stream.clear();

    formattedWrite(stream, "%.0d", 0);
    assert(stream.data == "");
    stream.clear();

    formattedWrite(stream, "%.g", .34);
    assert(stream.data == "0.3");
    stream.clear();

    stream.clear(); formattedWrite(stream, "%.0g", .34);
    assert(stream.data == "0.3");

    stream.clear(); formattedWrite(stream, "%.2g", .34);
    assert(stream.data == "0.34");

    stream.clear(); formattedWrite(stream, "%0.0008f", 1e-08);
    assert(stream.data == "0.00000001");

    stream.clear(); formattedWrite(stream, "%0.0008f", 1e-05);
    assert(stream.data == "0.00001000");

    //return;
    //core.stdc.stdio.fwrite(stream.data.ptr, stream.data.length, 1, stderr);

    s = "helloworld";
    string r;
    stream.clear(); formattedWrite(stream, "%.2s", s[0 .. 5]);
    assert(stream.data == "he");
    stream.clear(); formattedWrite(stream, "%.20s", s[0 .. 5]);
    assert(stream.data == "hello");
    stream.clear(); formattedWrite(stream, "%8s", s[0 .. 5]);
    assert(stream.data == "   hello");

    byte[] arrbyte = new byte[4];
    arrbyte[0] = 100;
    arrbyte[1] = -99;
    arrbyte[3] = 0;
    stream.clear(); formattedWrite(stream, "%s", arrbyte);
    assert(stream.data == "[100, -99, 0, 0]", stream.data);

    ubyte[] arrubyte = new ubyte[4];
    arrubyte[0] = 100;
    arrubyte[1] = 200;
    arrubyte[3] = 0;
    stream.clear(); formattedWrite(stream, "%s", arrubyte);
    assert(stream.data == "[100, 200, 0, 0]", stream.data);

    short[] arrshort = new short[4];
    arrshort[0] = 100;
    arrshort[1] = -999;
    arrshort[3] = 0;
    stream.clear(); formattedWrite(stream, "%s", arrshort);
    assert(stream.data == "[100, -999, 0, 0]");
    stream.clear(); formattedWrite(stream, "%s",arrshort);
    assert(stream.data == "[100, -999, 0, 0]");

    ushort[] arrushort = new ushort[4];
    arrushort[0] = 100;
    arrushort[1] = 20_000;
    arrushort[3] = 0;
    stream.clear(); formattedWrite(stream, "%s", arrushort);
    assert(stream.data == "[100, 20000, 0, 0]");

    int[] arrint = new int[4];
    arrint[0] = 100;
    arrint[1] = -999;
    arrint[3] = 0;
    stream.clear(); formattedWrite(stream, "%s", arrint);
    assert(stream.data == "[100, -999, 0, 0]");
    stream.clear(); formattedWrite(stream, "%s",arrint);
    assert(stream.data == "[100, -999, 0, 0]");

    long[] arrlong = new long[4];
    arrlong[0] = 100;
    arrlong[1] = -999;
    arrlong[3] = 0;
    stream.clear(); formattedWrite(stream, "%s", arrlong);
    assert(stream.data == "[100, -999, 0, 0]");
    stream.clear(); formattedWrite(stream, "%s",arrlong);
    assert(stream.data == "[100, -999, 0, 0]");

    ulong[] arrulong = new ulong[4];
    arrulong[0] = 100;
    arrulong[1] = 999;
    arrulong[3] = 0;
    stream.clear(); formattedWrite(stream, "%s", arrulong);
    assert(stream.data == "[100, 999, 0, 0]");

    string[] arr2 = new string[4];
    arr2[0] = "hello";
    arr2[1] = "world";
    arr2[3] = "foo";
    stream.clear(); formattedWrite(stream, "%s", arr2);
    assert(stream.data == `["hello", "world", "", "foo"]`, stream.data);

    stream.clear(); formattedWrite(stream, "%.8d", 7);
    assert(stream.data == "00000007");

    stream.clear(); formattedWrite(stream, "%.8x", 10);
    assert(stream.data == "0000000a");

    stream.clear(); formattedWrite(stream, "%-3d", 7);
    assert(stream.data == "7  ");

    stream.clear(); formattedWrite(stream, "%*d", -3, 7);
    assert(stream.data == "7  ");

    stream.clear(); formattedWrite(stream, "%.*d", -3, 7);
    //writeln(stream.data);
    assert(stream.data == "7");

    stream.clear(); formattedWrite(stream, "%s", "abc"c);
    assert(stream.data == "abc");
    stream.clear(); formattedWrite(stream, "%s", "def"w);
    assert(stream.data == "def", text(stream.data.length));
    stream.clear(); formattedWrite(stream, "%s", "ghi"d);
    assert(stream.data == "ghi");

here:
    @trusted void* deadBeef() { return cast(void*) 0xDEADBEEF; }
    stream.clear(); formattedWrite(stream, "%s", deadBeef());
    assert(stream.data == "DEADBEEF", stream.data);

    stream.clear(); formattedWrite(stream, "%#x", 0xabcd);
    assert(stream.data == "0xabcd");
    stream.clear(); formattedWrite(stream, "%#X", 0xABCD);
    assert(stream.data == "0XABCD");

    stream.clear(); formattedWrite(stream, "%#o", octal!12345);
    assert(stream.data == "012345");
    stream.clear(); formattedWrite(stream, "%o", 9);
    assert(stream.data == "11");

    stream.clear(); formattedWrite(stream, "%+d", 123);
    assert(stream.data == "+123");
    stream.clear(); formattedWrite(stream, "%+d", -123);
    assert(stream.data == "-123");
    stream.clear(); formattedWrite(stream, "% d", 123);
    assert(stream.data == " 123");
    stream.clear(); formattedWrite(stream, "% d", -123);
    assert(stream.data == "-123");

    stream.clear(); formattedWrite(stream, "%%");
    assert(stream.data == "%");

    stream.clear(); formattedWrite(stream, "%d", true);
    assert(stream.data == "1");
    stream.clear(); formattedWrite(stream, "%d", false);
    assert(stream.data == "0");

    stream.clear(); formattedWrite(stream, "%d", 'a');
    assert(stream.data == "97", stream.data);
    wchar wc = 'a';
    stream.clear(); formattedWrite(stream, "%d", wc);
    assert(stream.data == "97");
    dchar dc = 'a';
    stream.clear(); formattedWrite(stream, "%d", dc);
    assert(stream.data == "97");

    byte b = byte.max;
    stream.clear(); formattedWrite(stream, "%x", b);
    assert(stream.data == "7f");
    stream.clear(); formattedWrite(stream, "%x", ++b);
    assert(stream.data == "80");
    stream.clear(); formattedWrite(stream, "%x", ++b);
    assert(stream.data == "81");

    short sh = short.max;
    stream.clear(); formattedWrite(stream, "%x", sh);
    assert(stream.data == "7fff");
    stream.clear(); formattedWrite(stream, "%x", ++sh);
    assert(stream.data == "8000");
    stream.clear(); formattedWrite(stream, "%x", ++sh);
    assert(stream.data == "8001");

    i = int.max;
    stream.clear(); formattedWrite(stream, "%x", i);
    assert(stream.data == "7fffffff");
    stream.clear(); formattedWrite(stream, "%x", ++i);
    assert(stream.data == "80000000");
    stream.clear(); formattedWrite(stream, "%x", ++i);
    assert(stream.data == "80000001");

    stream.clear(); formattedWrite(stream, "%x", 10);
    assert(stream.data == "a");
    stream.clear(); formattedWrite(stream, "%X", 10);
    assert(stream.data == "A");
    stream.clear(); formattedWrite(stream, "%x", 15);
    assert(stream.data == "f");
    stream.clear(); formattedWrite(stream, "%X", 15);
    assert(stream.data == "F");

    @trusted void ObjectTest()
    {
        Object c = null;
        stream.clear(); formattedWrite(stream, "%s", c);
        assert(stream.data == "null");
    }
    ObjectTest();

    enum TestEnum
    {
        Value1, Value2
    }
    stream.clear(); formattedWrite(stream, "%s", TestEnum.Value2);
    assert(stream.data == "Value2", stream.data);
    stream.clear(); formattedWrite(stream, "%s", cast(TestEnum) 5);
    assert(stream.data == "cast(TestEnum)5", stream.data);

    //immutable(char[5])[int] aa = ([3:"hello", 4:"betty"]);
    //stream.clear(); formattedWrite(stream, "%s", aa.values);
    //core.stdc.stdio.fwrite(stream.data.ptr, stream.data.length, 1, stderr);
    //assert(stream.data == "[[h,e,l,l,o],[b,e,t,t,y]]");
    //stream.clear(); formattedWrite(stream, "%s", aa);
    //assert(stream.data == "[3:[h,e,l,l,o],4:[b,e,t,t,y]]");

    static const dchar[] ds = ['a','b'];
    for (int j = 0; j < ds.length; ++j)
    {
        stream.clear(); formattedWrite(stream, " %d", ds[j]);
        if (j == 0)
            assert(stream.data == " 97");
        else
            assert(stream.data == " 98");
    }

    stream.clear(); formattedWrite(stream, "%.-3d", 7);
    assert(stream.data == "7", ">" ~ stream.data ~ "<");
}

@safe unittest
{
    import std.array;
    import std.stdio;

    immutable(char[5])[int] aa = ([3:"hello", 4:"betty"]);
    assert(aa[3] == "hello");
    assert(aa[4] == "betty");

    auto stream = appender!(char[])();
    alias AllNumerics =
        AliasSeq!(byte, ubyte, short, ushort, int, uint, long, ulong,
                  float, double, real);
    foreach (T; AllNumerics)
    {
        T value = 1;
        stream.clear();
        formattedWrite(stream, "%s", value);
        assert(stream.data == "1");
    }

    stream.clear();
    formattedWrite(stream, "%s", aa);
}

@system unittest
{
    string s = "hello!124:34.5";
    string a;
    int b;
    double c;
    formattedRead(s, "%s!%s:%s", &a, &b, &c);
    assert(a == "hello" && b == 124 && c == 34.5);
}

version (unittest)
void formatReflectTest(T)(ref T val, string fmt, string formatted, string fn = __FILE__, size_t ln = __LINE__)
{
    import core.exception : AssertError;
    import std.array : appender;
    auto w = appender!string();
    formattedWrite(w, fmt, val);

    auto input = w.data;
    enforce!AssertError(
            input == formatted,
            input, fn, ln);

    T val2;
    formattedRead(input, fmt, &val2);
    static if (isAssociativeArray!T)
    if (__ctfe)
    {
        alias aa1 = val;
        alias aa2 = val2;
        assert(aa1 == aa2);

        assert(aa1.length == aa2.length);

        assert(aa1.keys == aa2.keys);

        assert(aa1.values == aa2.values);
        assert(aa1.values.length == aa2.values.length);
        foreach (i; 0 .. aa1.values.length)
            assert(aa1.values[i] == aa2.values[i]);

        foreach (i, key; aa1.keys)
            assert(aa1.values[i] == aa1[key]);
        foreach (i, key; aa2.keys)
            assert(aa2.values[i] == aa2[key]);
        return;
    }
    enforce!AssertError(
            val == val2,
            input, fn, ln);
}

version (unittest)
void formatReflectTest(T)(ref T val, string fmt, string[] formatted, string fn = __FILE__, size_t ln = __LINE__)
{
    import core.exception : AssertError;
    import std.array : appender;
    auto w = appender!string();
    formattedWrite(w, fmt, val);

    auto input = w.data;

    foreach (cur; formatted)
    {
        if (input == cur) return;
    }
    enforce!AssertError(
            false,
            input,
            fn,
            ln);

    T val2;
    formattedRead(input, fmt, &val2);
    static if (isAssociativeArray!T)
    if (__ctfe)
    {
        alias aa1 = val;
        alias aa2 = val2;
        assert(aa1 == aa2);

        assert(aa1.length == aa2.length);

        assert(aa1.keys == aa2.keys);

        assert(aa1.values == aa2.values);
        assert(aa1.values.length == aa2.values.length);
        foreach (i; 0 .. aa1.values.length)
            assert(aa1.values[i] == aa2.values[i]);

        foreach (i, key; aa1.keys)
            assert(aa1.values[i] == aa1[key]);
        foreach (i, key; aa2.keys)
            assert(aa2.values[i] == aa2[key]);
        return;
    }
    enforce!AssertError(
            val == val2,
            input, fn, ln);
}

@system unittest
{
    void booleanTest()
    {
        auto b = true;
        formatReflectTest(b, "%s",  `true`);
        formatReflectTest(b, "%b",  `1`);
        formatReflectTest(b, "%o",  `1`);
        formatReflectTest(b, "%d",  `1`);
        formatReflectTest(b, "%u",  `1`);
        formatReflectTest(b, "%x",  `1`);
    }

    void integerTest()
    {
        auto n = 127;
        formatReflectTest(n, "%s",  `127`);
        formatReflectTest(n, "%b",  `1111111`);
        formatReflectTest(n, "%o",  `177`);
        formatReflectTest(n, "%d",  `127`);
        formatReflectTest(n, "%u",  `127`);
        formatReflectTest(n, "%x",  `7f`);
    }

    void floatingTest()
    {
        auto f = 3.14;
        formatReflectTest(f, "%s",  `3.14`);
        version (MinGW)
            formatReflectTest(f, "%e",  `3.140000e+000`);
        else
            formatReflectTest(f, "%e",  `3.140000e+00`);
        formatReflectTest(f, "%f",  `3.140000`);
        formatReflectTest(f, "%g",  `3.14`);
    }

    void charTest()
    {
        auto c = 'a';
        formatReflectTest(c, "%s",  `a`);
        formatReflectTest(c, "%c",  `a`);
        formatReflectTest(c, "%b",  `1100001`);
        formatReflectTest(c, "%o",  `141`);
        formatReflectTest(c, "%d",  `97`);
        formatReflectTest(c, "%u",  `97`);
        formatReflectTest(c, "%x",  `61`);
    }

    void strTest()
    {
        auto s = "hello";
        formatReflectTest(s, "%s",                      `hello`);
        formatReflectTest(s, "%(%c,%)",                 `h,e,l,l,o`);
        formatReflectTest(s, "%(%s,%)",                 `'h','e','l','l','o'`);
        formatReflectTest(s, "[%(<%c>%| $ %)]",         `[<h> $ <e> $ <l> $ <l> $ <o>]`);
    }

    void daTest()
    {
        auto a = [1,2,3,4];
        formatReflectTest(a, "%s",                      `[1, 2, 3, 4]`);
        formatReflectTest(a, "[%(%s; %)]",              `[1; 2; 3; 4]`);
        formatReflectTest(a, "[%(<%s>%| $ %)]",         `[<1> $ <2> $ <3> $ <4>]`);
    }

    void saTest()
    {
        int[4] sa = [1,2,3,4];
        formatReflectTest(sa, "%s",                     `[1, 2, 3, 4]`);
        formatReflectTest(sa, "[%(%s; %)]",             `[1; 2; 3; 4]`);
        formatReflectTest(sa, "[%(<%s>%| $ %)]",        `[<1> $ <2> $ <3> $ <4>]`);
    }

    void aaTest()
    {
        auto aa = [1:"hello", 2:"world"];
        formatReflectTest(aa, "%s",                     [`[1:"hello", 2:"world"]`, `[2:"world", 1:"hello"]`]);
        formatReflectTest(aa, "[%(%s->%s, %)]",         [`[1->"hello", 2->"world"]`, `[2->"world", 1->"hello"]`]);
        formatReflectTest(aa, "{%([%s=%(%c%)]%|; %)}",  [`{[1=hello]; [2=world]}`, `{[2=world]; [1=hello]}`]);
    }

    import std.exception;
    assertCTFEable!(
    {
        booleanTest();
        integerTest();
        if (!__ctfe) floatingTest();    // snprintf
        charTest();
        strTest();
        daTest();
        saTest();
        aaTest();
        return true;
    });
}

//------------------------------------------------------------------------------
private void skipData(Range, Char)(ref Range input, const ref FormatSpec!Char spec)
{
    import std.ascii : isDigit;
    import std.conv : text;

    switch (spec.spec)
    {
        case 'c': input.popFront(); break;
        case 'd':
            if (input.front == '+' || input.front == '-') input.popFront();
            goto case 'u';
        case 'u':
            while (!input.empty && isDigit(input.front)) input.popFront();
            break;
        default:
            assert(false,
                    text("Format specifier not understood: %", spec.spec));
    }
}

private template acceptedSpecs(T)
{
         static if (isIntegral!T)       enum acceptedSpecs = "bdosuxX";
    else static if (isFloatingPoint!T)  enum acceptedSpecs = "seEfgG";
    else static if (isSomeChar!T)       enum acceptedSpecs = "bcdosuxX";    // integral + 'c'
    else                                enum acceptedSpecs = "";
}

/**
 * Reads a value from the given _input range according to spec
 * and returns it as type `T`.
 *
 * Params:
 *     T = the type to return
 *     input = the _input range to read from
 *     spec = the `FormatSpec` to use when reading from `input`
 * Returns:
 *     A value from `input` of type `T`
 * Throws:
 *     An `Exception` if `spec` cannot read a type `T`
 * See_Also:
 *     $(REF parse, std, conv) and $(REF to, std, conv)
 */
T unformatValue(T, Range, Char)(ref Range input, const ref FormatSpec!Char spec)
{
    return unformatValueImpl!T(input, spec);
}

/// Booleans
@safe pure unittest
{
    auto str = "false";
    auto spec = singleSpec("%s");
    assert(unformatValue!bool(str, spec) == false);

    str = "1";
    spec = singleSpec("%d");
    assert(unformatValue!bool(str, spec));
}

/// Null values
@safe pure unittest
{
    auto str = "null";
    auto spec = singleSpec("%s");
    assert(str.unformatValue!(typeof(null))(spec) == null);
}

/// Integrals
@safe pure unittest
{
    auto str = "123";
    auto spec = singleSpec("%s");
    assert(str.unformatValue!int(spec) == 123);

    str = "ABC";
    spec = singleSpec("%X");
    assert(str.unformatValue!int(spec) == 2748);

    str = "11610";
    spec = singleSpec("%o");
    assert(str.unformatValue!int(spec) == 5000);
}

/// Floating point numbers
@safe pure unittest
{
    import std.math : approxEqual;

    auto str = "123.456";
    auto spec = singleSpec("%s");
    assert(str.unformatValue!double(spec).approxEqual(123.456));
}

/// Character input ranges
@safe pure unittest
{
    auto str = "aaa";
    auto spec = singleSpec("%s");
    assert(str.unformatValue!char(spec) == 'a');

    // Using a numerical format spec reads a Unicode value from a string
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

/// Arrays and static arrays
@safe pure unittest
{
    string str = "aaa";
    auto spec = singleSpec("%s");
    assert(str.unformatValue!(dchar[])(spec) == "aaa"d);

    str = "aaa";
    spec = singleSpec("%s");
    dchar[3] ret = ['a', 'a', 'a'];
    assert(str.unformatValue!(dchar[3])(spec) == ret);

    str = "[1, 2, 3, 4]";
    spec = singleSpec("%s");
    assert(str.unformatValue!(int[])(spec) == [1, 2, 3, 4]);

    str = "[1, 2, 3, 4]";
    spec = singleSpec("%s");
    int[4] ret2 = [1, 2, 3, 4];
    assert(str.unformatValue!(int[4])(spec) == ret2);
}

/// Associative arrays
@safe pure unittest
{
    auto str = `["one": 1, "two": 2]`;
    auto spec = singleSpec("%s");
    assert(str.unformatValue!(int[string])(spec) == ["one": 1, "two": 2]);
}

@safe pure unittest
{
    // 7241
    string input = "a";
    auto spec = FormatSpec!char("%s");
    spec.readUpToNextSpec(input);
    auto result = unformatValue!(dchar[1])(input, spec);
    assert(result[0] == 'a');
}

private T unformatValueImpl(T, Range, Char)(ref Range input, const ref FormatSpec!Char spec)
if (isInputRange!Range && is(Unqual!T == bool))
{
    import std.algorithm.searching : find;
    import std.conv : parse, text;

    if (spec.spec == 's') return parse!T(input);

    enforce(find(acceptedSpecs!long, spec.spec).length,
            text("Wrong unformat specifier '%", spec.spec , "' for ", T.stringof));

    return unformatValue!long(input, spec) != 0;
}

private T unformatValueImpl(T, Range, Char)(ref Range input, const ref FormatSpec!Char spec)
if (isInputRange!Range && is(T == typeof(null)))
{
    import std.conv : parse, text;
    enforce(spec.spec == 's',
            text("Wrong unformat specifier '%", spec.spec , "' for ", T.stringof));

    return parse!T(input);
}

/// ditto
private T unformatValueImpl(T, Range, Char)(ref Range input, const ref FormatSpec!Char spec)
if (isInputRange!Range && isIntegral!T && !is(T == enum) && isSomeChar!(ElementType!Range))
{

    import std.algorithm.searching : find;
    import std.conv : parse, text;

    if (spec.spec == 'r')
    {
        static if (is(Unqual!(ElementEncodingType!Range) == char)
                || is(Unqual!(ElementEncodingType!Range) == byte)
                || is(Unqual!(ElementEncodingType!Range) == ubyte))
            return rawRead!T(input);
        else
            throw new Exception("The raw read specifier %r may only be used with narrow strings and ranges of bytes.");
    }

    enforce(find(acceptedSpecs!T, spec.spec).length,
            text("Wrong unformat specifier '%", spec.spec , "' for ", T.stringof));

    enforce(spec.width == 0, "Parsing integers with a width specification is not implemented");   // TODO

    immutable uint base =
        spec.spec == 'x' || spec.spec == 'X' ? 16 :
        spec.spec == 'o' ? 8 :
        spec.spec == 'b' ? 2 :
        spec.spec == 's' || spec.spec == 'd' || spec.spec == 'u' ? 10 : 0;
    assert(base != 0);

    return parse!T(input, base);

}

/// ditto
private T unformatValueImpl(T, Range, Char)(ref Range input, const ref FormatSpec!Char spec)
if (isFloatingPoint!T && !is(T == enum) && isInputRange!Range
    && isSomeChar!(ElementType!Range)&& !is(Range == enum))
{
    import std.algorithm.searching : find;
    import std.conv : parse, text;

    if (spec.spec == 'r')
    {
        static if (is(Unqual!(ElementEncodingType!Range) == char)
                || is(Unqual!(ElementEncodingType!Range) == byte)
                || is(Unqual!(ElementEncodingType!Range) == ubyte))
            return rawRead!T(input);
        else
            throw new Exception("The raw read specifier %r may only be used with narrow strings and ranges of bytes.");
    }

    enforce(find(acceptedSpecs!T, spec.spec).length,
            text("Wrong unformat specifier '%", spec.spec , "' for ", T.stringof));

    return parse!T(input);
}

/// ditto
private T unformatValueImpl(T, Range, Char)(ref Range input, const ref FormatSpec!Char spec)
if (isInputRange!Range && isSomeChar!T && !is(T == enum) && isSomeChar!(ElementType!Range))
{
    import std.algorithm.searching : find;
    import std.conv : to, text;
    if (spec.spec == 's' || spec.spec == 'c')
    {
        auto result = to!T(input.front);
        input.popFront();
        return result;
    }
    enforce(find(acceptedSpecs!T, spec.spec).length,
            text("Wrong unformat specifier '%", spec.spec , "' for ", T.stringof));

    static if (T.sizeof == 1)
        return unformatValue!ubyte(input, spec);
    else static if (T.sizeof == 2)
        return unformatValue!ushort(input, spec);
    else static if (T.sizeof == 4)
        return unformatValue!uint(input, spec);
    else
        static assert(0);
}

/// ditto
private T unformatValueImpl(T, Range, Char)(ref Range input, const ref FormatSpec!Char spec)
if (isInputRange!Range && is(StringTypeOf!T) && !isAggregateType!T && !is(T == enum))
{
    import std.conv : text;

    if (spec.spec == '(')
    {
        return unformatRange!T(input, spec);
    }
    enforce(spec.spec == 's',
            text("Wrong unformat specifier '%", spec.spec , "' for ", T.stringof));

    static if (isStaticArray!T)
    {
        T result;
        auto app = result[];
    }
    else
    {
        import std.array : appender;
        auto app = appender!T();
    }
    if (spec.trailing.empty)
    {
        for (; !input.empty; input.popFront())
        {
            static if (isStaticArray!T)
                if (app.empty)
                    break;
            app.put(input.front);
        }
    }
    else
    {
        immutable end = spec.trailing.front;
        for (; !input.empty && input.front != end; input.popFront())
        {
            static if (isStaticArray!T)
                if (app.empty)
                    break;
            app.put(input.front);
        }
    }
    static if (isStaticArray!T)
    {
        enforce(app.empty, "need more input");
        return result;
    }
    else
        return app.data;
}

/// ditto
private T unformatValueImpl(T, Range, Char)(ref Range input, const ref FormatSpec!Char spec)
if (isInputRange!Range && isArray!T && !is(StringTypeOf!T) && !isAggregateType!T && !is(T == enum))
{
    import std.conv : parse, text;
    if (spec.spec == '(')
    {
        return unformatRange!T(input, spec);
    }
    enforce(spec.spec == 's',
            text("Wrong unformat specifier '%", spec.spec , "' for ", T.stringof));

    return parse!T(input);
}

/// ditto
private T unformatValueImpl(T, Range, Char)(ref Range input, const ref FormatSpec!Char spec)
if (isInputRange!Range && isAssociativeArray!T && !is(T == enum))
{
    import std.conv : parse, text;
    if (spec.spec == '(')
    {
        return unformatRange!T(input, spec);
    }
    enforce(spec.spec == 's',
            text("Wrong unformat specifier '%", spec.spec , "' for ", T.stringof));

    return parse!T(input);
}

/**
 * Function that performs raw reading. Used by unformatValue
 * for integral and float types.
 */
private T rawRead(T, Range)(ref Range input)
if (is(Unqual!(ElementEncodingType!Range) == char)
    || is(Unqual!(ElementEncodingType!Range) == byte)
    || is(Unqual!(ElementEncodingType!Range) == ubyte))
{
    union X
    {
        ubyte[T.sizeof] raw;
        T typed;
    }
    X x;
    foreach (i; 0 .. T.sizeof)
    {
        static if (isSomeString!Range)
        {
            x.raw[i] = input[0];
            input = input[1 .. $];
        }
        else
        {
            // TODO: recheck this
            x.raw[i] = input.front;
            input.popFront();
        }
    }
    return x.typed;
}

//debug = unformatRange;

private T unformatRange(T, Range, Char)(ref Range input, const ref FormatSpec!Char spec)
in
{
    assert(spec.spec == '(');
}
body
{
    debug (unformatRange) printf("unformatRange:\n");

    T result;
    static if (isStaticArray!T)
    {
        size_t i;
    }

    const(Char)[] cont = spec.trailing;
    for (size_t j = 0; j < spec.trailing.length; ++j)
    {
        if (spec.trailing[j] == '%')
        {
            cont = spec.trailing[0 .. j];
            break;
        }
    }
    debug (unformatRange) printf("\t");
    debug (unformatRange) if (!input.empty) printf("input.front = %c, ", input.front);
    debug (unformatRange) printf("cont = %.*s\n", cast(int) cont.length, cont.ptr);

    bool checkEnd()
    {
        return input.empty || !cont.empty && input.front == cont.front;
    }

    if (!checkEnd())
    {
        for (;;)
        {
            auto fmt = FormatSpec!Char(spec.nested);
            fmt.readUpToNextSpec(input);
            enforce(!input.empty, "Unexpected end of input when parsing range");

            debug (unformatRange) printf("\t) spec = %c, front = %c ", fmt.spec, input.front);
            static if (isStaticArray!T)
            {
                result[i++] = unformatElement!(typeof(T.init[0]))(input, fmt);
            }
            else static if (isDynamicArray!T)
            {
                result ~= unformatElement!(ElementType!T)(input, fmt);
            }
            else static if (isAssociativeArray!T)
            {
                auto key = unformatElement!(typeof(T.init.keys[0]))(input, fmt);
                fmt.readUpToNextSpec(input);        // eat key separator

                result[key] = unformatElement!(typeof(T.init.values[0]))(input, fmt);
            }
            debug (unformatRange) {
            if (input.empty) printf("-> front = [empty] ");
            else             printf("-> front = %c ", input.front);
            }

            static if (isStaticArray!T)
            {
                debug (unformatRange) printf("i = %u < %u\n", i, T.length);
                enforce(i <= T.length, "Too many format specifiers for static array of length %d".format(T.length));
            }

            if (spec.sep !is null)
                fmt.readUpToNextSpec(input);
            auto sep = spec.sep !is null ? spec.sep
                         : fmt.trailing;
            debug (unformatRange) {
            if (!sep.empty && !input.empty) printf("-> %c, sep = %.*s\n", input.front, cast(int) sep.length, sep.ptr);
            else                            printf("\n");
            }

            if (checkEnd())
                break;

            if (!sep.empty && input.front == sep.front)
            {
                while (!sep.empty)
                {
                    enforce(!input.empty, "Unexpected end of input when parsing range separator");
                    enforce(input.front == sep.front, "Unexpected character when parsing range separator");
                    input.popFront();
                    sep.popFront();
                }
                debug (unformatRange) printf("input.front = %c\n", input.front);
            }
        }
    }
    static if (isStaticArray!T)
    {
        enforce(i == T.length, "Too few (%d) format specifiers for static array of length %d".format(i, T.length));
    }
    return result;
}

// Undocumented
T unformatElement(T, Range, Char)(ref Range input, const ref FormatSpec!Char spec)
if (isInputRange!Range)
{
    import std.conv : parseElement;
    static if (isSomeString!T)
    {
        if (spec.spec == 's')
        {
            return parseElement!T(input);
        }
    }
    else static if (isSomeChar!T)
    {
        if (spec.spec == 's')
        {
            return parseElement!T(input);
        }
    }

    return unformatValue!T(input, spec);
}


// Legacy implementation

enum Mangle : char
{
    Tvoid     = 'v',
    Tbool     = 'b',
    Tbyte     = 'g',
    Tubyte    = 'h',
    Tshort    = 's',
    Tushort   = 't',
    Tint      = 'i',
    Tuint     = 'k',
    Tlong     = 'l',
    Tulong    = 'm',
    Tfloat    = 'f',
    Tdouble   = 'd',
    Treal     = 'e',

    Tifloat   = 'o',
    Tidouble  = 'p',
    Tireal    = 'j',
    Tcfloat   = 'q',
    Tcdouble  = 'r',
    Tcreal    = 'c',

    Tchar     = 'a',
    Twchar    = 'u',
    Tdchar    = 'w',

    Tarray    = 'A',
    Tsarray   = 'G',
    Taarray   = 'H',
    Tpointer  = 'P',
    Tfunction = 'F',
    Tident    = 'I',
    Tclass    = 'C',
    Tstruct   = 'S',
    Tenum     = 'E',
    Ttypedef  = 'T',
    Tdelegate = 'D',

    Tconst    = 'x',
    Timmutable = 'y',
}

// return the TypeInfo for a primitive type and null otherwise.  This
// is required since for arrays of ints we only have the mangled char
// to work from. If arrays always subclassed TypeInfo_Array this
// routine could go away.
private TypeInfo primitiveTypeInfo(Mangle m)
{
    // BUG: should fix this in static this() to avoid double checked locking bug
    __gshared TypeInfo[Mangle] dic;
    if (!dic.length)
    {
        dic = [
            Mangle.Tvoid : typeid(void),
            Mangle.Tbool : typeid(bool),
            Mangle.Tbyte : typeid(byte),
            Mangle.Tubyte : typeid(ubyte),
            Mangle.Tshort : typeid(short),
            Mangle.Tushort : typeid(ushort),
            Mangle.Tint : typeid(int),
            Mangle.Tuint : typeid(uint),
            Mangle.Tlong : typeid(long),
            Mangle.Tulong : typeid(ulong),
            Mangle.Tfloat : typeid(float),
            Mangle.Tdouble : typeid(double),
            Mangle.Treal : typeid(real),
            Mangle.Tifloat : typeid(ifloat),
            Mangle.Tidouble : typeid(idouble),
            Mangle.Tireal : typeid(ireal),
            Mangle.Tcfloat : typeid(cfloat),
            Mangle.Tcdouble : typeid(cdouble),
            Mangle.Tcreal : typeid(creal),
            Mangle.Tchar : typeid(char),
            Mangle.Twchar : typeid(wchar),
            Mangle.Tdchar : typeid(dchar)
            ];
    }
    auto p = m in dic;
    return p ? *p : null;
}

private bool needToSwapEndianess(Char)(const ref FormatSpec!Char f)
{
    import std.system : endian, Endian;

    return endian == Endian.littleEndian && f.flPlus
        || endian == Endian.bigEndian && f.flDash;
}

/* ======================== Unit Tests ====================================== */

@system unittest
{
    import std.conv : octal;

    int i;
    string s;

    debug(format) printf("std.format.format.unittest\n");

    s = format("hello world! %s %s %s%s%s", true, 57, 1_000_000_000, 'x', " foo");
    assert(s == "hello world! true 57 1000000000x foo");

    s = format("%s %A %s", 1.67, -1.28, float.nan);
    /* The host C library is used to format floats.
     * C99 doesn't specify what the hex digit before the decimal point
     * is for %A.
     */
    //version (linux)
    //    assert(s == "1.67 -0XA.3D70A3D70A3D8P-3 nan");
    //else version (OSX)
    //    assert(s == "1.67 -0XA.3D70A3D70A3D8P-3 nan", s);
    //else
    version (MinGW)
        assert(s == "1.67 -0XA.3D70A3D70A3D8P-3 nan", s);
    else version (CRuntime_Microsoft)
        assert(s == "1.67 -0X1.47AE14P+0 nan"
            || s == "1.67 -0X1.47AE147AE147BP+0 nan", s); // MSVCRT 14+ (VS 2015)
    else
        assert(s == "1.67 -0X1.47AE147AE147BP+0 nan", s);

    s = format("%x %X", 0x1234AF, 0xAFAFAFAF);
    assert(s == "1234af AFAFAFAF");

    s = format("%b %o", 0x1234AF, 0xAFAFAFAF);
    assert(s == "100100011010010101111 25753727657");

    s = format("%d %s", 0x1234AF, 0xAFAFAFAF);
    assert(s == "1193135 2947526575");

    //version (X86_64)
    //{
    //    pragma(msg, "several format tests disabled on x86_64 due to bug 5625");
    //}
    //else
    //{
        s = format("%s", 1.2 + 3.4i);
        assert(s == "1.2+3.4i", s);

        //s = format("%x %X", 1.32, 6.78f);
        //assert(s == "3ff51eb851eb851f 40D8F5C3");

    //}

    s = format("%#06.*f",2,12.345);
    assert(s == "012.35");

    s = format("%#0*.*f",6,2,12.345);
    assert(s == "012.35");

    s = format("%7.4g:", 12.678);
    assert(s == "  12.68:");

    s = format("%7.4g:", 12.678L);
    assert(s == "  12.68:");

    s = format("%04f|%05d|%#05x|%#5x",-4.0,-10,1,1);
    assert(s == "-4.000000|-0010|0x001|  0x1");

    i = -10;
    s = format("%d|%3d|%03d|%1d|%01.4f",i,i,i,i,cast(double) i);
    assert(s == "-10|-10|-10|-10|-10.0000");

    i = -5;
    s = format("%d|%3d|%03d|%1d|%01.4f",i,i,i,i,cast(double) i);
    assert(s == "-5| -5|-05|-5|-5.0000");

    i = 0;
    s = format("%d|%3d|%03d|%1d|%01.4f",i,i,i,i,cast(double) i);
    assert(s == "0|  0|000|0|0.0000");

    i = 5;
    s = format("%d|%3d|%03d|%1d|%01.4f",i,i,i,i,cast(double) i);
    assert(s == "5|  5|005|5|5.0000");

    i = 10;
    s = format("%d|%3d|%03d|%1d|%01.4f",i,i,i,i,cast(double) i);
    assert(s == "10| 10|010|10|10.0000");

    s = format("%.0d", 0);
    assert(s == "");

    s = format("%.g", .34);
    assert(s == "0.3");

    s = format("%.0g", .34);
    assert(s == "0.3");

    s = format("%.2g", .34);
    assert(s == "0.34");

    s = format("%0.0008f", 1e-08);
    assert(s == "0.00000001");

    s = format("%0.0008f", 1e-05);
    assert(s == "0.00001000");

    s = "helloworld";
    string r;
    r = format("%.2s", s[0 .. 5]);
    assert(r == "he");
    r = format("%.20s", s[0 .. 5]);
    assert(r == "hello");
    r = format("%8s", s[0 .. 5]);
    assert(r == "   hello");

    byte[] arrbyte = new byte[4];
    arrbyte[0] = 100;
    arrbyte[1] = -99;
    arrbyte[3] = 0;
    r = format("%s", arrbyte);
    assert(r == "[100, -99, 0, 0]");

    ubyte[] arrubyte = new ubyte[4];
    arrubyte[0] = 100;
    arrubyte[1] = 200;
    arrubyte[3] = 0;
    r = format("%s", arrubyte);
    assert(r == "[100, 200, 0, 0]");

    short[] arrshort = new short[4];
    arrshort[0] = 100;
    arrshort[1] = -999;
    arrshort[3] = 0;
    r = format("%s", arrshort);
    assert(r == "[100, -999, 0, 0]");

    ushort[] arrushort = new ushort[4];
    arrushort[0] = 100;
    arrushort[1] = 20_000;
    arrushort[3] = 0;
    r = format("%s", arrushort);
    assert(r == "[100, 20000, 0, 0]");

    int[] arrint = new int[4];
    arrint[0] = 100;
    arrint[1] = -999;
    arrint[3] = 0;
    r = format("%s", arrint);
    assert(r == "[100, -999, 0, 0]");

    long[] arrlong = new long[4];
    arrlong[0] = 100;
    arrlong[1] = -999;
    arrlong[3] = 0;
    r = format("%s", arrlong);
    assert(r == "[100, -999, 0, 0]");

    ulong[] arrulong = new ulong[4];
    arrulong[0] = 100;
    arrulong[1] = 999;
    arrulong[3] = 0;
    r = format("%s", arrulong);
    assert(r == "[100, 999, 0, 0]");

    string[] arr2 = new string[4];
    arr2[0] = "hello";
    arr2[1] = "world";
    arr2[3] = "foo";
    r = format("%s", arr2);
    assert(r == `["hello", "world", "", "foo"]`);

    r = format("%.8d", 7);
    assert(r == "00000007");
    r = format("%.8x", 10);
    assert(r == "0000000a");

    r = format("%-3d", 7);
    assert(r == "7  ");

    r = format("%-1*d", 4, 3);
    assert(r == "3   ");

    r = format("%*d", -3, 7);
    assert(r == "7  ");

    r = format("%.*d", -3, 7);
    assert(r == "7");

    r = format("%-1.*f", 2, 3.1415);
    assert(r == "3.14");

    r = format("abc"c);
    assert(r == "abc");

    //format() returns the same type as inputted.
    wstring wr;
    wr = format("def"w);
    assert(wr == "def"w);

    dstring dr;
    dr = format("ghi"d);
    assert(dr == "ghi"d);

    void* p = cast(void*) 0xDEADBEEF;
    r = format("%s", p);
    assert(r == "DEADBEEF");

    r = format("%#x", 0xabcd);
    assert(r == "0xabcd");
    r = format("%#X", 0xABCD);
    assert(r == "0XABCD");

    r = format("%#o", octal!12345);
    assert(r == "012345");
    r = format("%o", 9);
    assert(r == "11");
    r = format("%#o", 0);   // issue 15663
    assert(r == "0");

    r = format("%+d", 123);
    assert(r == "+123");
    r = format("%+d", -123);
    assert(r == "-123");
    r = format("% d", 123);
    assert(r == " 123");
    r = format("% d", -123);
    assert(r == "-123");

    r = format("%%");
    assert(r == "%");

    r = format("%d", true);
    assert(r == "1");
    r = format("%d", false);
    assert(r == "0");

    r = format("%d", 'a');
    assert(r == "97");
    wchar wc = 'a';
    r = format("%d", wc);
    assert(r == "97");
    dchar dc = 'a';
    r = format("%d", dc);
    assert(r == "97");

    byte b = byte.max;
    r = format("%x", b);
    assert(r == "7f");
    r = format("%x", ++b);
    assert(r == "80");
    r = format("%x", ++b);
    assert(r == "81");

    short sh = short.max;
    r = format("%x", sh);
    assert(r == "7fff");
    r = format("%x", ++sh);
    assert(r == "8000");
    r = format("%x", ++sh);
    assert(r == "8001");

    i = int.max;
    r = format("%x", i);
    assert(r == "7fffffff");
    r = format("%x", ++i);
    assert(r == "80000000");
    r = format("%x", ++i);
    assert(r == "80000001");

    r = format("%x", 10);
    assert(r == "a");
    r = format("%X", 10);
    assert(r == "A");
    r = format("%x", 15);
    assert(r == "f");
    r = format("%X", 15);
    assert(r == "F");

    Object c = null;
    r = format("%s", c);
    assert(r == "null");

    enum TestEnum
    {
        Value1, Value2
    }
    r = format("%s", TestEnum.Value2);
    assert(r == "Value2");

    immutable(char[5])[int] aa = ([3:"hello", 4:"betty"]);
    r = format("%s", aa.values);
    assert(r == `["hello", "betty"]` || r == `["betty", "hello"]`);
    r = format("%s", aa);
    assert(r == `[3:"hello", 4:"betty"]` || r == `[4:"betty", 3:"hello"]`);

    static const dchar[] ds = ['a','b'];
    for (int j = 0; j < ds.length; ++j)
    {
        r = format(" %d", ds[j]);
        if (j == 0)
            assert(r == " 97");
        else
            assert(r == " 98");
    }

    r = format(">%14d<, %s", 15, [1,2,3]);
    assert(r == ">            15<, [1, 2, 3]");

    assert(format("%8s", "bar") == "     bar");
    assert(format("%8s", "b\u00e9ll\u00f4") == " b\u00e9ll\u00f4");
}

@safe unittest
{
    // bugzilla 3479
    import std.array;
    auto stream = appender!(char[])();
    formattedWrite(stream, "%2$.*1$d", 12, 10);
    assert(stream.data == "000000000010", stream.data);
}

@safe unittest
{
    // bug 6893
    import std.array;
    enum E : ulong { A, B, C }
    auto stream = appender!(char[])();
    formattedWrite(stream, "%s", E.C);
    assert(stream.data == "C");
}

// Used to check format strings are compatible with argument types
package static const checkFormatException(alias fmt, Args...) =
{
    try
        .format(fmt, Args.init);
    catch (Exception e)
        return (e.msg == ctfpMessage) ? null : e;
    return null;
}();

/*****************************************************
 * Format arguments into a string.
 *
 * Params: fmt  = Format string. For detailed specification, see $(LREF formattedWrite).
 *         args = Variadic list of arguments to _format into returned string.
 */
typeof(fmt) format(alias fmt, Args...)(Args args)
if (isSomeString!(typeof(fmt)))
{
    alias e = checkFormatException!(fmt, Args);
    static assert(!e, e.msg);
    return .format(fmt, args);
}

/// Type checking can be done when fmt is known at compile-time:
@safe unittest
{
    auto s = format!"%s is %s"("Pi", 3.14);
    assert(s == "Pi is 3.14");

    static assert(!__traits(compiles, {s = format!"%l"();}));     // missing arg
    static assert(!__traits(compiles, {s = format!""(404);}));    // surplus arg
    static assert(!__traits(compiles, {s = format!"%d"(4.03);})); // incompatible arg
}

/// ditto
immutable(Char)[] format(Char, Args...)(in Char[] fmt, Args args)
if (isSomeChar!Char)
{
    import std.array : appender;
    import std.format : formattedWrite, FormatException;
    auto w = appender!(immutable(Char)[]);
    auto n = formattedWrite(w, fmt, args);
    version (all)
    {
        // In the future, this check will be removed to increase consistency
        // with formattedWrite
        import std.conv : text;
        import std.exception : enforce;
        enforce(n == args.length, new FormatException(
            text("Orphan format arguments: args[", n, "..", args.length, "]")));
    }
    return w.data;
}

@safe pure unittest
{
    import core.exception;
    import std.exception;
    import std.format;
    assertCTFEable!(
    {
//  assert(format(null) == "");
    assert(format("foo") == "foo");
    assert(format("foo%%") == "foo%");
    assert(format("foo%s", 'C') == "fooC");
    assert(format("%s foo", "bar") == "bar foo");
    assert(format("%s foo %s", "bar", "abc") == "bar foo abc");
    assert(format("foo %d", -123) == "foo -123");
    assert(format("foo %d", 123) == "foo 123");

    assertThrown!FormatException(format("foo %s"));
    assertThrown!FormatException(format("foo %s", 123, 456));

    assert(format("hel%slo%s%s%s", "world", -138, 'c', true) ==
                  "helworldlo-138ctrue");
    });

    assert(is(typeof(format("happy")) == string));
    assert(is(typeof(format("happy"w)) == wstring));
    assert(is(typeof(format("happy"d)) == dstring));
}

// https://issues.dlang.org/show_bug.cgi?id=16661
@safe unittest
{
    assert(format("%.2f"d, 0.4) == "0.40");
    assert("%02d"d.format(1) == "01"d);
}

/*****************************************************
 * Format arguments into buffer $(I buf) which must be large
 * enough to hold the result.
 *
 * Returns:
 *     The slice of `buf` containing the formatted string.
 *
 * Throws:
 *     A `RangeError` if `buf` isn't large enough to hold the
 *     formatted string.
 *
 *     A $(LREF FormatException) if the length of `args` is different
 *     than the number of format specifiers in `fmt`.
 */
char[] sformat(alias fmt, Args...)(char[] buf, Args args)
if (isSomeString!(typeof(fmt)))
{
    alias e = checkFormatException!(fmt, Args);
    static assert(!e, e.msg);
    return .sformat(buf, fmt, args);
}

/// ditto
char[] sformat(Char, Args...)(char[] buf, in Char[] fmt, Args args)
{
    import core.exception : RangeError;
    import std.format : formattedWrite, FormatException;
    import std.utf : encode;

    size_t i;

    struct Sink
    {
        void put(dchar c)
        {
            char[4] enc;
            auto n = encode(enc, c);

            if (buf.length < i + n)
                throw new RangeError(__FILE__, __LINE__);

            buf[i .. i + n] = enc[0 .. n];
            i += n;
        }
        void put(const(char)[] s)
        {
            if (buf.length < i + s.length)
                throw new RangeError(__FILE__, __LINE__);

            buf[i .. i + s.length] = s[];
            i += s.length;
        }
        void put(const(wchar)[] s)
        {
            for (; !s.empty; s.popFront())
                put(s.front);
        }
        void put(const(dchar)[] s)
        {
            for (; !s.empty; s.popFront())
                put(s.front);
        }
    }
    auto n = formattedWrite(Sink(), fmt, args);
    version (all)
    {
        // In the future, this check will be removed to increase consistency
        // with formattedWrite
        import std.conv : text;
        import std.exception : enforce;
        enforce!FormatException(
            n == args.length,
            text("Orphan format arguments: args[", n, " .. ", args.length, "]")
        );
    }
    return buf[0 .. i];
}

/// The format string can be checked at compile-time (see $(LREF format) for details):
@system unittest
{
    char[10] buf;

    assert(buf[].sformat!"foo%s"('C') == "fooC");
    assert(sformat(buf[], "%s foo", "bar") == "bar foo");
}

@system unittest
{
    import core.exception;
    import std.format;

    debug(string) trustedPrintf("std.string.sformat.unittest\n");

    import std.exception;
    assertCTFEable!(
    {
    char[10] buf;

    assert(sformat(buf[], "foo") == "foo");
    assert(sformat(buf[], "foo%%") == "foo%");
    assert(sformat(buf[], "foo%s", 'C') == "fooC");
    assert(sformat(buf[], "%s foo", "bar") == "bar foo");
    assertThrown!RangeError(sformat(buf[], "%s foo %s", "bar", "abc"));
    assert(sformat(buf[], "foo %d", -123) == "foo -123");
    assert(sformat(buf[], "foo %d", 123) == "foo 123");

    assertThrown!FormatException(sformat(buf[], "foo %s"));
    assertThrown!FormatException(sformat(buf[], "foo %s", 123, 456));

    assert(sformat(buf[], "%s %s %s", "c"c, "w"w, "d"d) == "c w d");
    });
}

/*****************************
 * The .ptr is unsafe because it could be dereferenced and the length of the array may be 0.
 * Returns:
 *      the difference between the starts of the arrays
 */
@trusted private pure nothrow @nogc
    ptrdiff_t arrayPtrDiff(T)(const T[] array1, const T[] array2)
{
    return array1.ptr - array2.ptr;
}

@safe unittest
{
    assertCTFEable!({
    auto tmp = format("%,d", 1000);
    assert(tmp == "1,000", "'" ~ tmp ~ "'");

    tmp = format("%,?d", 'z', 1234567);
    assert(tmp == "1z234z567", "'" ~ tmp ~ "'");

    tmp = format("%10,?d", 'z', 1234567);
    assert(tmp == " 1z234z567", "'" ~ tmp ~ "'");

    tmp = format("%11,2?d", 'z', 1234567);
    assert(tmp == " 1z23z45z67", "'" ~ tmp ~ "'");

    tmp = format("%11,*?d", 2, 'z', 1234567);
    assert(tmp == " 1z23z45z67", "'" ~ tmp ~ "'");

    tmp = format("%11,*d", 2, 1234567);
    assert(tmp == " 1,23,45,67", "'" ~ tmp ~ "'");

    tmp = format("%11,2d", 1234567);
    assert(tmp == " 1,23,45,67", "'" ~ tmp ~ "'");
    });
}

@safe unittest
{
    auto tmp = format("%,f", 1000.0);
    assert(tmp == "1,000.000,000", "'" ~ tmp ~ "'");

    tmp = format("%,f", 1234567.891011);
    assert(tmp == "1,234,567.891,011", "'" ~ tmp ~ "'");

    tmp = format("%,f", -1234567.891011);
    assert(tmp == "-1,234,567.891,011", "'" ~ tmp ~ "'");

    tmp = format("%,2f", 1234567.891011);
    assert(tmp == "1,23,45,67.89,10,11", "'" ~ tmp ~ "'");

    tmp = format("%18,f", 1234567.891011);
    assert(tmp == " 1,234,567.891,011", "'" ~ tmp ~ "'");

    tmp = format("%18,?f", '.', 1234567.891011);
    assert(tmp == " 1.234.567.891.011", "'" ~ tmp ~ "'");

    tmp = format("%,?.3f", 'ä', 1234567.891011);
    assert(tmp == "1ä234ä567.891", "'" ~ tmp ~ "'");

    tmp = format("%,*?.3f", 1, 'ä', 1234567.891011);
    assert(tmp == "1ä2ä3ä4ä5ä6ä7.8ä9ä1", "'" ~ tmp ~ "'");

    tmp = format("%,4?.3f", '_', 1234567.891011);
    assert(tmp == "123_4567.891", "'" ~ tmp ~ "'");

    tmp = format("%12,3.3f", 1234.5678);
    assert(tmp == "   1,234.568", "'" ~ tmp ~ "'");

    tmp = format("%,e", 3.141592653589793238462);
    assert(tmp == "3.141,593e+00", "'" ~ tmp ~ "'");

    tmp = format("%15,e", 3.141592653589793238462);
    assert(tmp == "  3.141,593e+00", "'" ~ tmp ~ "'");

    tmp = format("%15,e", -3.141592653589793238462);
    assert(tmp == " -3.141,593e+00", "'" ~ tmp ~ "'");

    tmp = format("%.4,*e", 2, 3.141592653589793238462);
    assert(tmp == "3.14,16e+00", "'" ~ tmp ~ "'");

    tmp = format("%13.4,*e", 2, 3.141592653589793238462);
    assert(tmp == "  3.14,16e+00", "'" ~ tmp ~ "'");

    tmp = format("%,.0f", 3.14);
    assert(tmp == "3", "'" ~ tmp ~ "'");

    tmp = format("%3,g", 1_000_000.123456);
    assert(tmp == "1e+06", "'" ~ tmp ~ "'");

    tmp = format("%19,?f", '.', -1234567.891011);
    assert(tmp == " -1.234.567.891.011", "'" ~ tmp ~ "'");
}

// Test for multiple indexes
@safe unittest
{
    auto tmp = format("%2:5$s", 1, 2, 3, 4, 5);
    assert(tmp == "2345", tmp);
}

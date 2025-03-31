// Written in the D programming language.

/**
This is a submodule of $(MREF std, format).

It centers around a struct called $(LREF FormatSpec), which takes a
$(MREF_ALTTEXT format string, std,format) and provides tools for
parsing this string. Additionally this module contains a function
$(LREF singleSpec) which helps treating a single format specifier.

Copyright: Copyright The D Language Foundation 2000-2013.

License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Authors: $(HTTP walterbright.com, Walter Bright), $(HTTP erdani.com,
Andrei Alexandrescu), and Kenji Hara

Source: $(PHOBOSSRC std/format/spec.d)
 */
module std.format.spec;

import std.traits : Unqual;

template FormatSpec(Char)
if (!is(Unqual!Char == Char))
{
    alias FormatSpec = FormatSpec!(Unqual!Char);
}

/**
A general handler for format strings.

This handler centers around the function $(LREF writeUpToNextSpec),
which parses the $(MREF_ALTTEXT format string, std,format) until the
next format specifier is found. After the call, it provides
information about this format specifier in its numerous variables.

Params:
    Char = the character type of the format string
 */
struct FormatSpec(Char)
if (is(Unqual!Char == Char))
{
    import std.algorithm.searching : startsWith;
    import std.ascii : isDigit;
    import std.conv : parse, text, to;
    import std.range.primitives;

    /**
       Minimum width.

       _Default: `0`.
     */
    int width = 0;

    /**
       Precision. Its semantic depends on the format character.

       See $(MREF_ALTTEXT format string, std,format) for more details.
       _Default: `UNSPECIFIED`.
     */
    int precision = UNSPECIFIED;

    /**
       Number of elements between separators.

       _Default: `UNSPECIFIED`.
     */
    int separators = UNSPECIFIED;

    /**
       The separator charactar is supplied at runtime.

       _Default: false.
     */
    bool dynamicSeparatorChar = false;

    /**
       Set to `DYNAMIC` when the separator character is supplied at runtime.

       _Default: `UNSPECIFIED`.

       $(RED Warning:
           `separatorCharPos` is deprecated. It will be removed in 2.107.0.
           Please use `dynamicSeparatorChar` instead.)
     */
    // @@@DEPRECATED_[2.107.0]@@@
    deprecated("separatorCharPos will be removed in 2.107.0. Please use dynamicSeparatorChar instead.")
    int separatorCharPos() { return dynamicSeparatorChar ? DYNAMIC : UNSPECIFIED; }

    /// ditto
    // @@@DEPRECATED_[2.107.0]@@@
    deprecated("separatorCharPos will be removed in 2.107.0. Please use dynamicSeparatorChar instead.")
    void separatorCharPos(int value) { dynamicSeparatorChar = value == DYNAMIC; }

    /**
       Character to use as separator.

       _Default: `','`.
     */
    dchar separatorChar = ',';

    /**
       Special value for `width`, `precision` and `separators`.

       It flags that these values will be passed at runtime through
       variadic arguments.
     */
    enum int DYNAMIC = int.max;

    /**
       Special value for `precision` and `separators`.

       It flags that these values have not been specified.
     */
    enum int UNSPECIFIED = DYNAMIC - 1;

    /**
       The format character.

       _Default: `'s'`.
     */
    char spec = 's';

    /**
       Index of the argument for positional parameters.

       Counting starts with `1`. Set to `0` if not used. Default: `0`.
     */
    ushort indexStart;

    /**
       Index of the last argument for positional parameter ranges.

       Counting starts with `1`. Set to `0` if not used. Default: `0`.

       The maximum value of this field is used as a sentinel to indicate the arguments' length.
    */
    ushort indexEnd;

    version (StdDdoc)
    {
        /// The format specifier contained a `'-'`.
        bool flDash;

        /// The format specifier contained a `'0'`.
        bool flZero;

        /// The format specifier contained a space.
        bool flSpace;

        /// The format specifier contained a `'+'`.
        bool flPlus;

        /// The format specifier contained a `'#'`.
        bool flHash;

        /// The format specifier contained a `'='`.
        bool flEqual;

        /// The format specifier contained a `','`.
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
                        bool, "flEqual", 1,
                        bool, "flSeparator", 1,
                        ubyte, "", 1));
            ubyte allFlags;
        }
    }

    /// The inner format string of a nested format specifier.
    const(Char)[] nested;

    /**
       The separator of a nested format specifier.

       `null` means, there is no separator. `empty`, but not `null`,
       means zero length separator.
     */
    const(Char)[] sep;

    /// Contains the part of the format string, that has not yet been parsed.
    const(Char)[] trailing;

    /// Sequence `"["` inserted before each range or range like structure.
    enum immutable(Char)[] seqBefore = "[";

    /// Sequence `"]"` inserted after each range or range like structure.
    enum immutable(Char)[] seqAfter = "]";

    /**
       Sequence `":"` inserted between element key and element value of
       an associative array.
     */
    enum immutable(Char)[] keySeparator = ":";

    /**
       Sequence `", "` inserted between elements of a range, a range like
       structure or the elements of an associative array.
     */
    enum immutable(Char)[] seqSeparator = ", ";

    /**
       Creates a new `FormatSpec`.

       The string is lazily evaluated. That means, nothing is done,
       until $(LREF writeUpToNextSpec) is called.

       Params:
           fmt = a $(MREF_ALTTEXT format string, std,format)
     */
    this(in Char[] fmt) @safe pure
    {
        trailing = fmt;
    }

    /**
       Writes the format string to an output range until the next format
       specifier is found and parse that format specifier.

       See the $(MREF_ALTTEXT description of format strings, std,format) for more
       details about the format specifier.

       Params:
           writer = an $(REF_ALTTEXT output range, isOutputRange, std, range, primitives),
                    where the format string is written to
           OutputRange = type of the output range

       Returns:
           True, if a format specifier is found and false, if the end of the
           format string has been reached.

       Throws:
           A $(REF_ALTTEXT FormatException, FormatException, std,format)
           when parsing the format specifier did not succeed.
     */
    bool writeUpToNextSpec(OutputRange)(ref OutputRange writer) scope
    {
        import std.format : enforceFmt;

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

    private void fillUp() scope
    {
        import std.format : enforceFmt, FormatException;

        // Reset content
        if (__ctfe)
        {
            flDash = false;
            flZero = false;
            flSpace = false;
            flPlus = false;
            flEqual = false;
            flHash = false;
            flSeparator = false;
        }
        else
        {
            allFlags = 0;
        }

        width = 0;
        indexStart = 0;
        indexEnd = 0;
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
                            throw new FormatException(
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
            case '=': flEqual = true; ++i; break;
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
                        text("$ expected after '*", -width, "' in format string"));
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
                i = trailing.length - tmp.length;
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
                    i = trailing.length - tmp.length;
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
                    i = trailing.length - tmp.length;
                }
                else
                {
                    // "," was specified, but nothing after it
                    separators = 3;
                }

                if (trailing[i] == '?')
                {
                    dynamicSeparatorChar = true;
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
                    i = trailing.length - tmp.length;
                }
                else if (isDigit(trailing[i]))
                {
                    auto tmp = trailing[i .. $];
                    precision = parse!int(tmp);
                    i = trailing.length - tmp.length;
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
        throw new FormatException(text("Incorrect format specifier: ", trailing));
    }

    //--------------------------------------------------------------------------
    package bool readUpToNextSpec(R)(ref R r) scope
    {
        import std.ascii : isLower, isWhite;
        import std.format : enforceFmt;
        import std.utf : stride;

        // Reset content
        if (__ctfe)
        {
            flDash = false;
            flZero = false;
            flSpace = false;
            flPlus = false;
            flHash = false;
            flEqual = false;
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
                    assert(!r.empty, "Required at least one more input");
                    // Require a '%'
                    enforceFmt (r.front == '%',
                        text("parseToFormatSpec: Cannot find character '",
                             c2, "' in the input string."));
                    trailing = trailing[2 .. $];
                    r.popFront();
                }
                else
                {
                    enforceFmt(isLower(c2) || c2 == '*' || c2 == '(',
                        text("'%", c2, "' not supported with formatted read"));
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
                    enforceFmt(!r.empty && r.front == trailing.front,
                        text("parseToFormatSpec: Cannot find character '",
                             c, "' in the input string."));
                    r.popFront();
                }
                trailing = trailing[stride(trailing, 0) .. $];
            }
        }
        return false;
    }

    package string getCurFmtStr() const
    {
        import std.array : appender;
        import std.format.write : formatValue;

        auto w = appender!string();
        auto f = FormatSpec!Char("%s"); // for stringnize

        put(w, '%');
        if (indexStart != 0)
        {
            formatValue(w, indexStart, f);
            put(w, '$');
        }
        if (flDash) put(w, '-');
        if (flZero) put(w, '0');
        if (flSpace) put(w, ' ');
        if (flPlus) put(w, '+');
        if (flEqual) put(w, '=');
        if (flHash) put(w, '#');
        if (width != 0)
            formatValue(w, width, f);
        if (precision != FormatSpec!Char.UNSPECIFIED)
        {
            put(w, '.');
            formatValue(w, precision, f);
        }
        if (flSeparator) put(w, ',');
        if (separators != FormatSpec!Char.UNSPECIFIED)
            formatValue(w, separators, f);
        put(w, spec);
        return w.data;
    }

    /**
       Provides a string representation.

       Returns:
           The string representation.
     */
    string toString() const @safe pure
    {
        import std.array : appender;

        auto app = appender!string();
        app.reserve(200 + trailing.length);
        toString(app);
        return app.data;
    }

    /**
       Writes a string representation to an output range.

       Params:
           writer = an $(REF_ALTTEXT output range, isOutputRange, std, range, primitives),
                    where the representation is written to
           OutputRange = type of the output range
     */
    void toString(OutputRange)(ref OutputRange writer) const
    if (isOutputRange!(OutputRange, char))
    {
        import std.format.write : formatValue;

        auto s = singleSpec("%s");

        put(writer, "address = ");
        formatValue(writer, &this, s);
        put(writer, "\nwidth = ");
        formatValue(writer, width, s);
        put(writer, "\nprecision = ");
        formatValue(writer, precision, s);
        put(writer, "\nspec = ");
        formatValue(writer, spec, s);
        put(writer, "\nindexStart = ");
        formatValue(writer, indexStart, s);
        put(writer, "\nindexEnd = ");
        formatValue(writer, indexEnd, s);
        put(writer, "\nflDash = ");
        formatValue(writer, flDash, s);
        put(writer, "\nflZero = ");
        formatValue(writer, flZero, s);
        put(writer, "\nflSpace = ");
        formatValue(writer, flSpace, s);
        put(writer, "\nflPlus = ");
        formatValue(writer, flPlus, s);
        put(writer, "\nflEqual = ");
        formatValue(writer, flEqual, s);
        put(writer, "\nflHash = ");
        formatValue(writer, flHash, s);
        put(writer, "\nflSeparator = ");
        formatValue(writer, flSeparator, s);
        put(writer, "\nnested = ");
        formatValue(writer, nested, s);
        put(writer, "\ntrailing = ");
        formatValue(writer, trailing, s);
        put(writer, '\n');
    }
}

///
@safe pure unittest
{
    import std.array : appender;

    auto a = appender!(string)();
    auto fmt = "Number: %6.4e\nString: %s";
    auto f = FormatSpec!char(fmt);

    assert(f.writeUpToNextSpec(a));

    assert(a.data == "Number: ");
    assert(f.trailing == "\nString: %s");
    assert(f.spec == 'e');
    assert(f.width == 6);
    assert(f.precision == 4);

    assert(f.writeUpToNextSpec(a));

    assert(a.data == "Number: \nString: ");
    assert(f.trailing == "");
    assert(f.spec == 's');

    assert(!f.writeUpToNextSpec(a));

    assert(a.data == "Number: \nString: ");
}

@safe unittest
{
    import std.array : appender;
    import std.conv : text;
    import std.exception : assertThrown;
    import std.format : FormatException;

    auto w = appender!(char[])();
    auto f = FormatSpec!char("abc%sdef%sghi");
    f.writeUpToNextSpec(w);
    assert(w.data == "abc", w.data);
    assert(f.trailing == "def%sghi", text(f.trailing));
    f.writeUpToNextSpec(w);
    assert(w.data == "abcdef", w.data);
    assert(f.trailing == "ghi");
    // test with embedded %%s
    f = FormatSpec!char("ab%%cd%%ef%sg%%h%sij");
    w.clear();
    f.writeUpToNextSpec(w);
    assert(w.data == "ab%cd%ef" && f.trailing == "g%%h%sij", w.data);
    f.writeUpToNextSpec(w);
    assert(w.data == "ab%cd%efg%h" && f.trailing == "ij");
    // https://issues.dlang.org/show_bug.cgi?id=4775
    f = FormatSpec!char("%%%s");
    w.clear();
    f.writeUpToNextSpec(w);
    assert(w.data == "%" && f.trailing == "");
    f = FormatSpec!char("%%%%%s%%");
    w.clear();
    while (f.writeUpToNextSpec(w)) continue;
    assert(w.data == "%%%");

    f = FormatSpec!char("a%%b%%c%");
    w.clear();
    assertThrown!FormatException(f.writeUpToNextSpec(w));
    assert(w.data == "a%b%c" && f.trailing == "%");
}

// https://issues.dlang.org/show_bug.cgi?id=5237
@safe unittest
{
    import std.array : appender;

    auto w = appender!string();
    auto f = FormatSpec!char("%.16f");
    f.writeUpToNextSpec(w); // dummy eating
    assert(f.spec == 'f');
    auto fmt = f.getCurFmtStr();
    assert(fmt == "%.16f");
}

// https://issues.dlang.org/show_bug.cgi?id=14059
@safe unittest
{
    import std.array : appender;
    import std.exception : assertThrown;
    import std.format : FormatException;

    auto a = appender!(string)();

    auto f = FormatSpec!char("%-(%s%"); // %)")
    assertThrown!FormatException(f.writeUpToNextSpec(a));

    f = FormatSpec!char("%(%-"); // %)")
    assertThrown!FormatException(f.writeUpToNextSpec(a));
}

@safe unittest
{
    import std.array : appender;
    import std.format : format;

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

@safe pure unittest
{
    import std.algorithm.searching : canFind, findSplitBefore;

    auto expected = "width = 2" ~
        "\nprecision = 5" ~
        "\nspec = f" ~
        "\nindexStart = 0" ~
        "\nindexEnd = 0" ~
        "\nflDash = false" ~
        "\nflZero = false" ~
        "\nflSpace = false" ~
        "\nflPlus = false" ~
        "\nflEqual = false" ~
        "\nflHash = false" ~
        "\nflSeparator = false" ~
        "\nnested = " ~
        "\ntrailing = \n";
    auto spec = singleSpec("%2.5f");
    auto res = spec.toString();
    // make sure the address exists, then skip it
    assert(res.canFind("address"));
    assert(res.findSplitBefore("width")[1] == expected);
}

// https://issues.dlang.org/show_bug.cgi?id=15348
@safe pure unittest
{
    import std.array : appender;
    import std.exception : collectExceptionMsg;
    import std.format : FormatException;

    auto w = appender!(char[])();
    auto f = FormatSpec!char("%*10d");

    assert(collectExceptionMsg!FormatException(f.writeUpToNextSpec(w))
           == "$ expected after '*10' in format string");
}

// https://github.com/dlang/phobos/issues/10713
@safe pure unittest
{
    import std.array : appender;
    auto f = FormatSpec!char("%3$d%d");

    auto w = appender!(char[])();
    f.writeUpToNextSpec(w);
    assert(f.indexStart == 3);

    f.writeUpToNextSpec(w);
    assert(w.data.length == 0);
    assert(f.indexStart == 0);
}

// https://github.com/dlang/phobos/issues/10699
@safe pure unittest
{
    import std.array : appender;
    auto f = FormatSpec!char("%1:$d");
    auto w = appender!(char[])();

    f.writeUpToNextSpec(w);
    assert(f.indexStart == 1);
    assert(f.indexEnd == ushort.max);
}

/**
Helper function that returns a `FormatSpec` for a single format specifier.

Params:
    fmt = a $(MREF_ALTTEXT format string, std,format)
          containing a single format specifier
    Char = character type of `fmt`

Returns:
    A $(LREF FormatSpec) with the format specifier parsed.

Throws:
    A $(REF_ALTTEXT FormatException, FormatException, std,format) when the
    format string contains no format specifier or more than a single format
    specifier or when the format specifier is malformed.
  */
FormatSpec!Char singleSpec(Char)(Char[] fmt)
{
    import std.conv : text;
    import std.format : enforceFmt;
    import std.range.primitives : empty, front;

    enforceFmt(fmt.length >= 2, "fmt must be at least 2 characters long");
    enforceFmt(fmt.front == '%', "fmt must start with a '%' character");
    enforceFmt(fmt[1] != '%', "'%%' is not a permissible format specifier");

    static struct DummyOutputRange
    {
        void put(C)(scope const C[] buf) {} // eat elements
    }
    auto a = DummyOutputRange();
    auto spec = FormatSpec!Char(fmt);
    //dummy write
    spec.writeUpToNextSpec(a);

    enforceFmt(spec.trailing.empty,
        text("Trailing characters in fmt string: '", spec.trailing));

    return spec;
}

///
@safe pure unittest
{
    import std.array : appender;
    import std.format.write : formatValue;

    auto spec = singleSpec("%10.3e");
    auto writer = appender!string();
    writer.formatValue(42.0, spec);

    assert(writer.data == " 4.200e+01");
}

@safe pure unittest
{
    import std.exception : assertThrown;
    import std.format : FormatException;

    auto spec = singleSpec("%2.3e");

    assert(spec.trailing == "");
    assert(spec.spec == 'e');
    assert(spec.width == 2);
    assert(spec.precision == 3);

    assertThrown!FormatException(singleSpec(""));
    assertThrown!FormatException(singleSpec("%"));
    assertThrown!FormatException(singleSpec("%2.3"));
    assertThrown!FormatException(singleSpec("2.3e"));
    assertThrown!FormatException(singleSpec("Test%2.3e"));
    assertThrown!FormatException(singleSpec("%2.3eTest"));
    assertThrown!FormatException(singleSpec("%%"));
}

// @@@DEPRECATED_[2.107.0]@@@
deprecated("enforceValidFormatSpec was accidentally made public and will be removed in 2.107.0")
void enforceValidFormatSpec(T, Char)(scope const ref FormatSpec!Char f)
{
    import std.format.internal.write : evfs = enforceValidFormatSpec;

    evfs!T(f);
}

@safe unittest
{
    import std.exception : collectExceptionMsg;
    import std.format : format, FormatException;

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

    // dynamicSeparatorChar
    assert(collectExceptionMsg!FormatException(format("%,?d", 5))
        == "separator character expected, not int for argument #1");
    assert(collectExceptionMsg!FormatException(format("%,?d", '?'))
        == "Orphan format specifier: %d");
    assert(collectExceptionMsg!FormatException(format("%.*,*?d", 5))
        == "Missing separator digit width argument");
}


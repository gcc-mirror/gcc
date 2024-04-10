/++
    This module provides definitions to support D's
    interpolated expression sequence literal, sometimes
    called string interpolation.


    ---
    string str;
    int num;
    // the compiler uses this module to implement the
    // i"..." literal used here.
    auto a = i"$​(str) has $​(num) items.";
    ---

    The variable `a` is a sequence of expressions:

    ---
    a[0] == InterpolationHeader()
    a[$-1] == InterpolationFooter()
    ---

    First and last, you see the header and footer, to
    clearly indicate where interpolation begins and ends.
    Note that there may be nested interpolated sequences too,
    each with their own header and footer. Think of them
    as a set of balanced parenthesis around the contents.

    Inside, you will find three general categories of
    content: `InterpolatedLiteral!"string"` for string
    expressions, `InterpolatedExpression!"code"` for code
    expressions, and then the values themselves as their
    own type.

    In the example:
    ---
    auto a = i"$​(str) has $​(num) items.";
    ---

    We will find:
    ---
    a[0] == InterpolationHeader()
    a[1] == InterpolatedExpression!"str"
    a[2] == str
    a[3] == InterpolatedLiteral!" has ";
    a[4] == InterpolatedExpression!"num";
    a[5] == num
    a[6] == InterpolatedLiteral!" items.";
    a[7] == InterpolationFooter()
    a.length == 8;
    ---

    You can see the correspondence with the original
    input: when you write `$​(expression)`, the string of the
    expression is passed as `InterpolatedExpression!ThatString`,
    (excluding any parenthesis around the expression),
    and everything else is passed as `InterpolatedLiteral!str`,
    in the same sequence as they appeared in the source.

    After an `InterpolatedExpression!...`, you will find the
    actual value(s) in the tuple. (If the expression expanded
    to multiple values - for example, if it was itself a tuple,
    there will be multiple values for a single expression.)

    Library functions should NOT attempt to mixin the code
    from an `InterpolatedExpression` themselves. Doing so
    will fail, since it is coming from a different scope anyway.
    The string is provided to you only for informational purposes
    and as a sentinel to separate things the user wrote.

    Your code should be able to handle an empty code string
    in `InterpolatedExpression` or even an entirely missing
    `InterpolatedExpression`, in case an implementation decides to
    not emit these.

    The `toString` members on these return `null`, except for
    the `InterpolatedLiteral`, which returns the literal string.
    This is to ease processing by generic functions like
    `std.stdio.write` or `std.conv.text`, making them effectively
    transparently skipped.

    To extract the string from an `InterpolatedLiteral`, you can
    use an `is` expression or the `.toString` method.

    To extract the string from a `InterpolatedExpression`, you can
    use an `is` expression or the `.expression` member.

    None of these structures have runtime state.

    History:
        Added in dmd 2.10x frontend, released in late 2023.
+/
module core.interpolation;

/++
    Common implementation for returning an empty string, to avoid storing
    multiple versions of the same function based on templated types below.
+/
public string __getEmptyString() @nogc pure nothrow @safe {
    return "";
}

/++
    Sentinel values to indicate the beginning and end of an
    interpolated expression sequence.

    Note that these can nest, so while processing a sequence,
    it may be helpful to keep a nesting count if that knowledge
    is important to your application.
+/
struct InterpolationHeader {
    /++
        Returns `null` for easy compatibility with existing functions
        like `std.stdio.writeln` and `std.conv.text`.
    +/
    alias toString = __getEmptyString;
}

/// ditto
struct InterpolationFooter {
    /++
        Returns `null` for easy compatibility with existing functions
        like `std.stdio.writeln` and `std.conv.text`.
    +/
    alias toString = __getEmptyString;
}

/++
    Represents a fragment of a string literal in between expressions
    passed as part of an interpolated expression sequence.
+/
struct InterpolatedLiteral(string text) {
    /++
        Returns the text of the interpolated string literal for this
        segment of the tuple, for easy access and compatibility with
        existing functions like `std.stdio.writeln` and `std.conv.text`.
    +/
    static string toString() @nogc pure nothrow @safe {
        return text;
    }
}

/++
    Represents the source code of an expression passed as part of an
    interpolated expression sequence.
+/
struct InterpolatedExpression(string text) {
    /++
        Returns the text of an interpolated expression used in the
        original literal, if provided by the implementation.
    +/
    enum expression = text;

    /++
        Returns `null` for easy compatibility with existing functions
        like `std.stdio.writeln` and `std.conv.text`.
    +/
    alias toString = __getEmptyString;
}

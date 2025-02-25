@safe unittest
{
    import std.format;

    // Easiest way is to use `%s` everywhere:
    assert(format("I got %s %s for %s euros.", 30, "eggs", 5.27) == "I got 30 eggs for 5.27 euros.");

    // Other format characters provide more control:
    assert(format("I got %b %(%X%) for %f euros.", 30, "eggs", 5.27) == "I got 11110 65676773 for 5.270000 euros.");
}

@safe unittest
{
    import std.format;

/*
The trailing end of the sub-format string following the specifier for
each item is interpreted as the array delimiter, and is therefore
omitted following the last array item:
 */
    assert(format("My items are %(%s %).", [1,2,3]) == "My items are 1 2 3.");
    assert(format("My items are %(%s, %).", [1,2,3]) == "My items are 1, 2, 3.");

/*
The "%|" delimiter specifier may be used to indicate where the
delimiter begins, so that the portion of the format string prior to
it will be retained in the last array element:
 */
    assert(format("My items are %(-%s-%|, %).", [1,2,3]) == "My items are -1-, -2-, -3-.");

/*
These compound format specifiers may be nested in the case of a
nested array argument:
 */
    auto mat = [[1, 2, 3],
                [4, 5, 6],
                [7, 8, 9]];

    assert(format("%(%(%d %) - %)", mat), "1 2 3 - 4 5 6 - 7 8 9");
    assert(format("[%(%(%d %) - %)]", mat), "[1 2 3 - 4 5 6 - 7 8 9]");
    assert(format("[%([%(%d %)]%| - %)]", mat), "[1 2 3] - [4 5 6] - [7 8 9]");

/*
Strings and characters are escaped automatically inside compound
format specifiers. To avoid this behavior, use "%-(" instead of "%(":
 */
    assert(format("My friends are %s.", ["John", "Nancy"]) == `My friends are ["John", "Nancy"].`);
    assert(format("My friends are %(%s, %).", ["John", "Nancy"]) == `My friends are "John", "Nancy".`);
    assert(format("My friends are %-(%s, %).", ["John", "Nancy"]) == `My friends are John, Nancy.`);
}

@safe unittest
{
    import std.format;

    // Flags can be used to influence to outcome:
    assert(format("%g != %+#g", 3.14, 3.14) == "3.14 != +3.14000");

    // Width and precision help to arrange the formatted result:
    assert(format(">%10.2f<", 1234.56789) == ">   1234.57<");

    // Numbers can be grouped:
    assert(format("%,4d", int.max) == "21,4748,3647");

    // It's possible to specify the position of an argument:
    assert(format("%3$s %1$s", 3, 17, 5) == "5 3");
}

@safe unittest
{
    import std.format;

    // Width as argument
    assert(format(">%*s<", 10, "abc") == ">       abc<");

    // Precision as argument
    assert(format(">%.*f<", 5, 123.2) == ">123.20000<");

    // Grouping as argument
    assert(format("%,*d", 1, int.max) == "2,1,4,7,4,8,3,6,4,7");

    // Grouping separator as argument
    assert(format("%,3?d", '_', int.max) == "2_147_483_647");

    // All at once
    assert(format("%*.*,*?d", 20, 15, 6, '/', int.max) == "   000/002147/483647");
}

@safe unittest
{
    import std.format;

    import std.exception : assertThrown;

    assertThrown!FormatException(format("%d", "foo"));
}

@safe pure unittest
{
    import std.format;

    assert(format("Here are %d %s.", 3, "apples") == "Here are 3 apples.");

    assert("Increase: %7.2f %%".format(17.4285) == "Increase:   17.43 %");
}

@safe pure unittest
{
    import std.format;

    auto s = format!"%s is %s"("Pi", 3.14);
    assert(s == "Pi is 3.14");

    // This line doesn't compile, because 3.14 cannot be formatted with %d:
    // s = format!"%s is %d"("Pi", 3.14);
}

@safe pure unittest
{
    import std.format;

    char[20] buf;
    assert(sformat(buf[], "Here are %d %s.", 3, "apples") == "Here are 3 apples.");

    assert(buf[].sformat("Increase: %7.2f %%", 17.4285) == "Increase:   17.43 %");
}

@safe pure unittest
{
    import std.format;

    char[20] buf;

    assert(sformat!"Here are %d %s."(buf[], 3, "apples") == "Here are 3 apples.");

    // This line doesn't compile, because 3.14 cannot be formatted with %d:
    // writeln(sformat!"Here are %d %s."(buf[], 3.14, "apples"));
}


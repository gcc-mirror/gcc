//Written in the D programming language

/**
 * Implements functionality to read Comma Separated Values and its variants
 * from an input range of $(D dchar).
 *
 * Comma Separated Values provide a simple means to transfer and store
 * tabular data. It has been common for programs to use their own
 * variant of the CSV format. This parser will loosely follow the
 * $(HTTP tools.ietf.org/html/rfc4180, RFC-4180). CSV input should adhere
 * to the following criteria (differences from RFC-4180 in parentheses):
 *
 * $(UL
 *     $(LI A record is separated by a new line (CRLF,LF,CR))
 *     $(LI A final record may end with a new line)
 *     $(LI A header may be provided as the first record in input)
 *     $(LI A record has fields separated by a comma (customizable))
 *     $(LI A field containing new lines, commas, or double quotes
 *          should be enclosed in double quotes (customizable))
 *     $(LI Double quotes in a field are escaped with a double quote)
 *     $(LI Each record should contain the same number of fields)
 *   )
 *
 * Example:
 *
 * -------
 * import std.algorithm;
 * import std.array;
 * import std.csv;
 * import std.stdio;
 * import std.typecons;
 *
 * void main()
 * {
 *     auto text = "Joe,Carpenter,300000\nFred,Blacksmith,400000\r\n";
 *
 *     foreach (record; csvReader!(Tuple!(string, string, int))(text))
 *     {
 *         writefln("%s works as a %s and earns $%d per year",
 *                  record[0], record[1], record[2]);
 *     }
 *
 *     // To read the same string from the file "filename.csv":
 *
 *     auto file = File("filename.csv", "r");
 *     foreach (record;
 *         file.byLine.joiner("\n").csvReader!(Tuple!(string, string, int)))
 *     {
 *         writefln("%s works as a %s and earns $%d per year",
 *                  record[0], record[1], record[2]);
 *     }
 }
 * }
 * -------
 *
 * When an input contains a header the $(D Contents) can be specified as an
 * associative array. Passing null to signify that a header is present.
 *
 * -------
 * auto text = "Name,Occupation,Salary\r"
 *     "Joe,Carpenter,300000\nFred,Blacksmith,400000\r\n";
 *
 * foreach (record; csvReader!(string[string])
 *         (text, null))
 * {
 *     writefln("%s works as a %s and earns $%s per year.",
 *              record["Name"], record["Occupation"],
 *              record["Salary"]);
 * }
 * -------
 *
 * This module allows content to be iterated by record stored in a struct,
 * class, associative array, or as a range of fields. Upon detection of an
 * error an CSVException is thrown (can be disabled). csvNextToken has been
 * made public to allow for attempted recovery.
 *
 * Disabling exceptions will lift many restrictions specified above. A quote
 * can appear in a field if the field was not quoted. If in a quoted field any
 * quote by itself, not at the end of a field, will end processing for that
 * field. The field is ended when there is no input, even if the quote was not
 * closed.
 *
 *   See_Also:
 *      $(HTTP en.wikipedia.org/wiki/Comma-separated_values, Wikipedia
 *      Comma-separated values)
 *
 *   Copyright: Copyright 2011
 *   License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 *   Authors:   Jesse Phillips
 *   Source:    $(PHOBOSSRC std/_csv.d)
 */
module std.csv;

import std.conv;
import std.exception;  // basicExceptionCtors
import std.range.primitives;
import std.traits;

/**
 * Exception containing the row and column for when an exception was thrown.
 *
 * Numbering of both row and col start at one and corresponds to the location
 * in the file rather than any specified header. Special consideration should
 * be made when there is failure to match the header see $(LREF
 * HeaderMismatchException) for details.
 *
 * When performing type conversions, $(REF ConvException, std,conv) is stored in
 * the $(D next) field.
 */
class CSVException : Exception
{
    ///
    size_t row, col;

    // FIXME: Use std.exception.basicExceptionCtors here once bug #11500 is fixed

    this(string msg, string file = __FILE__, size_t line = __LINE__,
         Throwable next = null) @nogc @safe pure nothrow
    {
        super(msg, file, line, next);
    }

    this(string msg, Throwable next, string file = __FILE__,
         size_t line = __LINE__) @nogc @safe pure nothrow
    {
        super(msg, file, line, next);
    }

    this(string msg, size_t row, size_t col, Throwable next = null,
         string file = __FILE__, size_t line = __LINE__) @nogc @safe pure nothrow
    {
        super(msg, next, file, line);
        this.row = row;
        this.col = col;
    }

    override string toString() @safe pure const
    {
        return "(Row: " ~ to!string(row) ~
              ", Col: " ~ to!string(col) ~ ") " ~ msg;
    }
}

@safe pure unittest
{
    import std.string;
    auto e1 = new Exception("Foobar");
    auto e2 = new CSVException("args", e1);
    assert(e2.next is e1);

    size_t r = 13;
    size_t c = 37;

    auto e3 = new CSVException("argv", r, c);
    assert(e3.row == r);
    assert(e3.col == c);

    auto em = e3.toString();
    assert(em.indexOf("13") != -1);
    assert(em.indexOf("37") != -1);
}

/**
 * Exception thrown when a Token is identified to not be completed: a quote is
 * found in an unquoted field, data continues after a closing quote, or the
 * quoted field was not closed before data was empty.
 */
class IncompleteCellException : CSVException
{
    /**
     * Data pulled from input before finding a problem
     *
     * This field is populated when using $(LREF csvReader)
     * but not by $(LREF csvNextToken) as this data will have
     * already been fed to the output range.
     */
    dstring partialData;

    mixin basicExceptionCtors;
}

@safe pure unittest
{
    auto e1 = new Exception("Foobar");
    auto e2 = new IncompleteCellException("args", e1);
    assert(e2.next is e1);
}

/**
 * Exception thrown under different conditions based on the type of $(D
 * Contents).
 *
 * Structure, Class, and Associative Array
 * $(UL
 *     $(LI When a header is provided but a matching column is not found)
 *  )
 *
 * Other
 * $(UL
 *     $(LI When a header is provided but a matching column is not found)
 *     $(LI Order did not match that found in the input)
 *  )
 *
 * Since a row and column is not meaningful when a column specified by the
 * header is not found in the data, both row and col will be zero. Otherwise
 * row is always one and col is the first instance found in header that
 * occurred before the previous starting at one.
 */
class HeaderMismatchException : CSVException
{
    mixin basicExceptionCtors;
}

@safe pure unittest
{
    auto e1 = new Exception("Foobar");
    auto e2 = new HeaderMismatchException("args", e1);
    assert(e2.next is e1);
}

/**
 * Determines the behavior for when an error is detected.
 *
 * Disabling exception will follow these rules:
 * $(UL
 *     $(LI A quote can appear in a field if the field was not quoted.)
 *     $(LI If in a quoted field any quote by itself, not at the end of a
 *     field, will end processing for that field.)
 *     $(LI The field is ended when there is no input, even if the quote was
 *     not closed.)
 *     $(LI If the given header does not match the order in the input, the
 *     content will return as it is found in the input.)
 *     $(LI If the given header contains columns not found in the input they
 *     will be ignored.)
 *  )
*/
enum Malformed
{
    ignore,           /// No exceptions are thrown due to incorrect CSV.
    throwException    /// Use exceptions when input has incorrect CSV.
}

/**
 * Returns an input range for iterating over records found in $(D
 * input).
 *
 * The $(D Contents) of the input can be provided if all the records are the
 * same type such as all integer data:
 *
 * -------
 * string str = `76,26,22`;
 * int[] ans = [76,26,22];
 * auto records = csvReader!int(str);
 *
 * foreach (record; records)
 * {
 *     assert(equal(record, ans));
 * }
 * -------
 *
 * Example using a struct with modified delimiter:
 *
 * -------
 * string str = "Hello;65;63.63\nWorld;123;3673.562";
 * struct Layout
 * {
 *     string name;
 *     int value;
 *     double other;
 * }
 *
 * auto records = csvReader!Layout(str,';');
 *
 * foreach (record; records)
 * {
 *     writeln(record.name);
 *     writeln(record.value);
 *     writeln(record.other);
 * }
 * -------
 *
 * Specifying $(D ErrorLevel) as Malformed.ignore will lift restrictions
 * on the format. This example shows that an exception is not thrown when
 * finding a quote in a field not quoted.
 *
 * -------
 * string str = "A \" is now part of the data";
 * auto records = csvReader!(string,Malformed.ignore)(str);
 * auto record = records.front;
 *
 * assert(record.front == str);
 * -------
 *
 * Returns:
 *        An input range R as defined by
 *        $(REF isInputRange, std,range,primitives). When $(D Contents) is a
 *        struct, class, or an associative array, the element type of R is
 *        $(D Contents), otherwise the element type of R is itself a range with
 *        element type $(D Contents).
 *
 * Throws:
 *       $(LREF CSVException) When a quote is found in an unquoted field,
 *       data continues after a closing quote, the quoted field was not
 *       closed before data was empty, a conversion failed, or when the row's
 *       length does not match the previous length.
 *
 *       $(LREF HeaderMismatchException)  when a header is provided but a
 *       matching column is not found or the order did not match that found in
 *       the input. Read the exception documentation for specific details of
 *       when the exception is thrown for different types of $(D Contents).
 */
auto csvReader(Contents = string,Malformed ErrorLevel = Malformed.throwException, Range, Separator = char)(Range input,
                 Separator delimiter = ',', Separator quote = '"')
if (isInputRange!Range && is(Unqual!(ElementType!Range) == dchar)
    && isSomeChar!(Separator)
    && !is(Contents T : T[U], U : string))
{
    return CsvReader!(Contents,ErrorLevel,Range,
                    Unqual!(ElementType!Range),string[])
        (input, delimiter, quote);
}

/**
 * An optional $(D header) can be provided. The first record will be read in
 * as the header. If $(D Contents) is a struct then the header provided is
 * expected to correspond to the fields in the struct. When $(D Contents) is
 * not a type which can contain the entire record, the $(D header) must be
 * provided in the same order as the input or an exception is thrown.
 *
 * Read only column "b":
 *
 * -------
 * string str = "a,b,c\nHello,65,63.63\nWorld,123,3673.562";
 * auto records = csvReader!int(str, ["b"]);
 *
 * auto ans = [[65],[123]];
 * foreach (record; records)
 * {
 *     assert(equal(record, ans.front));
 *     ans.popFront();
 * }
 * -------
 *
 * Read from header of different order:
 *
 * -------
 * string str = "a,b,c\nHello,65,63.63\nWorld,123,3673.562";
 * struct Layout
 * {
 *     int value;
 *     double other;
 *     string name;
 * }
 *
 * auto records = csvReader!Layout(str, ["b","c","a"]);
 * -------
 *
 * The header can also be left empty if the input contains a header but
 * all columns should be iterated. The header from the input can always
 * be accessed from the header field.
 *
 * -------
 * string str = "a,b,c\nHello,65,63.63\nWorld,123,3673.562";
 * auto records = csvReader(str, null);
 *
 * assert(records.header == ["a","b","c"]);
 * -------
 *
 * Returns:
 *        An input range R as defined by
 *        $(REF isInputRange, std,range,primitives). When $(D Contents) is a
 *        struct, class, or an associative array, the element type of R is
 *        $(D Contents), otherwise the element type of R is itself a range with
 *        element type $(D Contents).
 *
 *        The returned range provides a header field for accessing the header
 *        from the input in array form.
 *
 * -------
 * string str = "a,b,c\nHello,65,63.63";
 * auto records = csvReader(str, ["a"]);
 *
 * assert(records.header == ["a","b","c"]);
 * -------
 *
 * Throws:
 *       $(LREF CSVException) When a quote is found in an unquoted field,
 *       data continues after a closing quote, the quoted field was not
 *       closed before data was empty, a conversion failed, or when the row's
 *       length does not match the previous length.
 *
 *       $(LREF HeaderMismatchException)  when a header is provided but a
 *       matching column is not found or the order did not match that found in
 *       the input. Read the exception documentation for specific details of
 *       when the exception is thrown for different types of $(D Contents).
 */
auto csvReader(Contents = string,
               Malformed ErrorLevel = Malformed.throwException,
               Range, Header, Separator = char)
                (Range input, Header header,
                 Separator delimiter = ',', Separator quote = '"')
if (isInputRange!Range && is(Unqual!(ElementType!Range) == dchar)
    && isSomeChar!(Separator)
    && isForwardRange!Header
    && isSomeString!(ElementType!Header))
{
    return CsvReader!(Contents,ErrorLevel,Range,
                    Unqual!(ElementType!Range),Header)
        (input, header, delimiter, quote);
}

///
auto csvReader(Contents = string,
               Malformed ErrorLevel = Malformed.throwException,
               Range, Header, Separator = char)
                (Range input, Header header,
                 Separator delimiter = ',', Separator quote = '"')
if (isInputRange!Range && is(Unqual!(ElementType!Range) == dchar)
    && isSomeChar!(Separator)
    && is(Header : typeof(null)))
{
    return CsvReader!(Contents,ErrorLevel,Range,
                    Unqual!(ElementType!Range),string[])
        (input, cast(string[]) null, delimiter, quote);
}

// Test standard iteration over input.
@safe pure unittest
{
    string str = `one,"two ""quoted"""` ~ "\n\"three\nnew line\",\nfive,six";
    auto records = csvReader(str);

    int count;
    foreach (record; records)
    {
        foreach (cell; record)
        {
            count++;
        }
    }
    assert(count == 6);
}

// Test newline on last record
@safe pure unittest
{
    string str = "one,two\nthree,four\n";
    auto records = csvReader(str);
    records.popFront();
    records.popFront();
    assert(records.empty);
}

// Test shorter row length
@safe pure unittest
{
    wstring str = "one,1\ntwo\nthree"w;
    struct Layout
    {
        string name;
        int value;
    }

    Layout[3] ans;
    ans[0].name = "one";
    ans[0].value = 1;
    ans[1].name = "two";
    ans[1].value = 0;
    ans[2].name = "three";
    ans[2].value = 0;

    auto records = csvReader!(Layout,Malformed.ignore)(str);

    int count;
    foreach (record; records)
    {
        assert(ans[count].name == record.name);
        assert(ans[count].value == record.value);
        count++;
    }
}

// Test shorter row length exception
@safe pure unittest
{
    import std.exception;

    struct A
    {
        string a,b,c;
    }

    auto strs = ["one,1\ntwo",
                 "one\ntwo,2,二\nthree,3,三",
                 "one\ntwo,2\nthree,3",
                 "one,1\ntwo\nthree,3"];

    foreach (str; strs)
    {
        auto records = csvReader!A(str);
        assertThrown!CSVException((){foreach (record; records) { }}());
    }
}


// Test structure conversion interface with unicode.
@safe pure unittest
{
    import std.math : abs;

    wstring str = "\U00010143Hello,65,63.63\nWorld,123,3673.562"w;
    struct Layout
    {
        string name;
        int value;
        double other;
    }

    Layout[2] ans;
    ans[0].name = "\U00010143Hello";
    ans[0].value = 65;
    ans[0].other = 63.63;
    ans[1].name = "World";
    ans[1].value = 123;
    ans[1].other = 3673.562;

    auto records = csvReader!Layout(str);

    int count;
    foreach (record; records)
    {
        assert(ans[count].name == record.name);
        assert(ans[count].value == record.value);
        assert(abs(ans[count].other - record.other) < 0.00001);
        count++;
    }
    assert(count == ans.length);
}

// Test input conversion interface
@safe pure unittest
{
    import std.algorithm;
    string str = `76,26,22`;
    int[] ans = [76,26,22];
    auto records = csvReader!int(str);

    foreach (record; records)
    {
        assert(equal(record, ans));
    }
}

// Test struct & header interface and same unicode
@safe unittest
{
    import std.math : abs;

    string str = "a,b,c\nHello,65,63.63\n➊➋➂❹,123,3673.562";
    struct Layout
    {
        int value;
        double other;
        string name;
    }

    auto records = csvReader!Layout(str, ["b","c","a"]);

    Layout[2] ans;
    ans[0].name = "Hello";
    ans[0].value = 65;
    ans[0].other = 63.63;
    ans[1].name = "➊➋➂❹";
    ans[1].value = 123;
    ans[1].other = 3673.562;

    int count;
    foreach (record; records)
    {
        assert(ans[count].name == record.name);
        assert(ans[count].value == record.value);
        assert(abs(ans[count].other - record.other) < 0.00001);
        count++;
    }
    assert(count == ans.length);

}

// Test header interface
@safe unittest
{
    import std.algorithm;

    string str = "a,b,c\nHello,65,63.63\nWorld,123,3673.562";
    auto records = csvReader!int(str, ["b"]);

    auto ans = [[65],[123]];
    foreach (record; records)
    {
        assert(equal(record, ans.front));
        ans.popFront();
    }

    try
    {
        csvReader(str, ["c","b"]);
        assert(0);
    }
    catch (HeaderMismatchException e)
    {
        assert(e.col == 2);
    }
    auto records2 = csvReader!(string,Malformed.ignore)
       (str, ["b","a"], ',', '"');

    auto ans2 = [["Hello","65"],["World","123"]];
    foreach (record; records2)
    {
        assert(equal(record, ans2.front));
        ans2.popFront();
    }

    str = "a,c,e\nJoe,Carpenter,300000\nFred,Fly,4";
    records2 = csvReader!(string,Malformed.ignore)
       (str, ["a","b","c","d"], ',', '"');

    ans2 = [["Joe","Carpenter"],["Fred","Fly"]];
    foreach (record; records2)
    {
        assert(equal(record, ans2.front));
        ans2.popFront();
    }
}

// Test null header interface
@safe unittest
{
    string str = "a,b,c\nHello,65,63.63\nWorld,123,3673.562";
    auto records = csvReader(str, ["a"]);

    assert(records.header == ["a","b","c"]);
}

// Test unchecked read
@safe pure unittest
{
    string str = "one \"quoted\"";
    foreach (record; csvReader!(string,Malformed.ignore)(str))
    {
        foreach (cell; record)
        {
            assert(cell == "one \"quoted\"");
        }
    }

    str = "one \"quoted\",two \"quoted\" end";
    struct Ans
    {
        string a,b;
    }
    foreach (record; csvReader!(Ans,Malformed.ignore)(str))
    {
        assert(record.a == "one \"quoted\"");
        assert(record.b == "two \"quoted\" end");
    }
}

// Test partial data returned
@safe pure unittest
{
    string str = "\"one\nnew line";

    try
    {
        foreach (record; csvReader(str))
        {}
        assert(0);
    }
    catch (IncompleteCellException ice)
    {
        assert(ice.partialData == "one\nnew line");
    }
}

// Test Windows line break
@safe pure unittest
{
    string str = "one,two\r\nthree";

    auto records = csvReader(str);
    auto record = records.front;
    assert(record.front == "one");
    record.popFront();
    assert(record.front == "two");
    records.popFront();
    record = records.front;
    assert(record.front == "three");
}


// Test associative array support with unicode separator
@safe unittest
{
  string str = "1❁2❁3\n34❁65❁63\n34❁65❁63";

  auto records = csvReader!(string[string])(str,["3","1"],'❁');
  int count;
  foreach (record; records)
  {
      count++;
      assert(record["1"] == "34");
      assert(record["3"] == "63");
  }
  assert(count == 2);
}

// Test restricted range
@safe unittest
{
    import std.typecons;
    struct InputRange
    {
        dstring text;

        this(dstring txt)
        {
            text = txt;
        }

        @property auto empty()
        {
            return text.empty;
        }

        void popFront()
        {
            text.popFront();
        }

        @property dchar front()
        {
            return text[0];
        }
    }
    auto ir = InputRange("Name,Occupation,Salary\r"d~
          "Joe,Carpenter,300000\nFred,Blacksmith,400000\r\n"d);

    foreach (record; csvReader(ir, cast(string[]) null))
        foreach (cell; record) {}
    foreach (record; csvReader!(Tuple!(string, string, int))
            (ir,cast(string[]) null)) {}
    foreach (record; csvReader!(string[string])
            (ir,cast(string[]) null)) {}
}

@safe unittest // const/immutable dchars
{
    import std.algorithm.iteration : map;
    import std.array : array;
    const(dchar)[] c = "foo,bar\n";
    assert(csvReader(c).map!array.array == [["foo", "bar"]]);
    immutable(dchar)[] i = "foo,bar\n";
    assert(csvReader(i).map!array.array == [["foo", "bar"]]);
}

/*
 * This struct is stored on the heap for when the structures
 * are passed around.
 */
private pure struct Input(Range, Malformed ErrorLevel)
{
    Range range;
    size_t row, col;
    static if (ErrorLevel == Malformed.throwException)
        size_t rowLength;
}

/*
 * Range for iterating CSV records.
 *
 * This range is returned by the $(LREF csvReader) functions. It can be
 * created in a similar manner to allow $(D ErrorLevel) be set to $(LREF
 * Malformed).ignore if best guess processing should take place.
 */
private struct CsvReader(Contents, Malformed ErrorLevel, Range, Separator, Header)
if (isSomeChar!Separator && isInputRange!Range
    && is(Unqual!(ElementType!Range) == dchar)
    && isForwardRange!Header && isSomeString!(ElementType!Header))
{
private:
    Input!(Range, ErrorLevel)* _input;
    Separator _separator;
    Separator _quote;
    size_t[] indices;
    bool _empty;
    static if (is(Contents == struct) || is(Contents == class))
    {
        Contents recordContent;
        CsvRecord!(string, ErrorLevel, Range, Separator) recordRange;
    }
    else static if (is(Contents T : T[U], U : string))
    {
        Contents recordContent;
        CsvRecord!(T, ErrorLevel, Range, Separator) recordRange;
    }
    else
        CsvRecord!(Contents, ErrorLevel, Range, Separator) recordRange;
public:
    /**
     * Header from the input in array form.
     *
     * -------
     * string str = "a,b,c\nHello,65,63.63";
     * auto records = csvReader(str, ["a"]);
     *
     * assert(records.header == ["a","b","c"]);
     * -------
     */
    string[] header;

    /**
     * Constructor to initialize the input, delimiter and quote for input
     * without a header.
     *
     * -------
     * string str = `76;^26^;22`;
     * int[] ans = [76,26,22];
     * auto records = CsvReader!(int,Malformed.ignore,string,char,string[])
     *       (str, ';', '^');
     *
     * foreach (record; records)
     * {
     *    assert(equal(record, ans));
     * }
     * -------
     */
    this(Range input, Separator delimiter, Separator quote)
    {
        _input = new Input!(Range, ErrorLevel)(input);
        _separator = delimiter;
        _quote = quote;

        prime();
    }

    /**
     * Constructor to initialize the input, delimiter and quote for input
     * with a header.
     *
     * -------
     * string str = `high;mean;low\n76;^26^;22`;
     * auto records = CsvReader!(int,Malformed.ignore,string,char,string[])
     *       (str, ["high","low"], ';', '^');
     *
     * int[] ans = [76,22];
     * foreach (record; records)
     * {
     *    assert(equal(record, ans));
     * }
     * -------
     *
     * Throws:
     *       $(LREF HeaderMismatchException)  when a header is provided but a
     *       matching column is not found or the order did not match that found
     *       in the input (non-struct).
     */
    this(Range input, Header colHeaders, Separator delimiter, Separator quote)
    {
        _input = new Input!(Range, ErrorLevel)(input);
        _separator = delimiter;
        _quote = quote;

        size_t[string] colToIndex;
        foreach (h; colHeaders)
        {
            colToIndex[h] = size_t.max;
        }

        auto r = CsvRecord!(string, ErrorLevel, Range, Separator)
            (_input, _separator, _quote, indices);

        size_t colIndex;
        foreach (col; r)
        {
            header ~= col;
            auto ptr = col in colToIndex;
            if (ptr)
                *ptr = colIndex;
            colIndex++;
        }
        // The above loop empties the header row.
        recordRange._empty = true;

        indices.length = colToIndex.length;
        int i;
        foreach (h; colHeaders)
        {
            immutable index = colToIndex[h];
            static if (ErrorLevel != Malformed.ignore)
                if (index == size_t.max)
                    throw new HeaderMismatchException
                        ("Header not found: " ~ to!string(h));
            indices[i++] = index;
        }

        static if (!is(Contents == struct) && !is(Contents == class))
        {
            static if (is(Contents T : T[U], U : string))
            {
                import std.algorithm.sorting : sort;
                sort(indices);
            }
            else static if (ErrorLevel == Malformed.ignore)
            {
                import std.algorithm.sorting : sort;
                sort(indices);
            }
            else
            {
                import std.algorithm.searching : findAdjacent;
                import std.algorithm.sorting : isSorted;
                if (!isSorted(indices))
                {
                    auto ex = new HeaderMismatchException
                           ("Header in input does not match specified header.");
                    findAdjacent!"a > b"(indices);
                    ex.row = 1;
                    ex.col = indices.front;

                    throw ex;
                }
            }
        }

        popFront();
    }

    /**
     * Part of an input range as defined by
     * $(REF isInputRange, std,range,primitives).
     *
     * Returns:
     *      If $(D Contents) is a struct, will be filled with record data.
     *
     *      If $(D Contents) is a class, will be filled with record data.
     *
     *      If $(D Contents) is a associative array, will be filled
     *      with record data.
     *
     *      If $(D Contents) is non-struct, a $(LREF CsvRecord) will be
     *      returned.
     */
    @property auto front()
    {
        assert(!empty);
        static if (is(Contents == struct) || is(Contents == class))
        {
            return recordContent;
        }
        else static if (is(Contents T : T[U], U : string))
        {
            return recordContent;
        }
        else
        {
            return recordRange;
        }
    }

    /**
     * Part of an input range as defined by
     * $(REF isInputRange, std,range,primitives).
     */
    @property bool empty() @safe @nogc pure nothrow const
    {
        return _empty;
    }

    /**
     * Part of an input range as defined by
     * $(REF isInputRange, std,range,primitives).
     *
     * Throws:
     *       $(LREF CSVException) When a quote is found in an unquoted field,
     *       data continues after a closing quote, the quoted field was not
     *       closed before data was empty, a conversion failed, or when the
     *       row's length does not match the previous length.
     */
    void popFront()
    {
        while (!recordRange.empty)
        {
            recordRange.popFront();
        }

        static if (ErrorLevel == Malformed.throwException)
            if (_input.rowLength == 0)
                _input.rowLength = _input.col;

        _input.col = 0;

        if (!_input.range.empty)
        {
            if (_input.range.front == '\r')
            {
                _input.range.popFront();
                if (!_input.range.empty && _input.range.front == '\n')
                    _input.range.popFront();
            }
            else if (_input.range.front == '\n')
                _input.range.popFront();
        }

        if (_input.range.empty)
        {
            _empty = true;
            return;
        }

        prime();
    }

    private void prime()
    {
        if (_empty)
            return;
        _input.row++;
        static if (is(Contents == struct) || is(Contents == class))
        {
            recordRange = typeof(recordRange)
                                 (_input, _separator, _quote, null);
        }
        else
        {
            recordRange = typeof(recordRange)
                                 (_input, _separator, _quote, indices);
        }

        static if (is(Contents T : T[U], U : string))
        {
            T[U] aa;
            try
            {
                for (; !recordRange.empty; recordRange.popFront())
                {
                    aa[header[_input.col-1]] = recordRange.front;
                }
            }
            catch (ConvException e)
            {
                throw new CSVException(e.msg, _input.row, _input.col, e);
            }

            recordContent = aa;
        }
        else static if (is(Contents == struct) || is(Contents == class))
        {
            static if (is(Contents == class))
                recordContent = new typeof(recordContent)();
            else
                recordContent = typeof(recordContent).init;
            size_t colIndex;
            try
            {
                for (; !recordRange.empty;)
                {
                    auto colData = recordRange.front;
                    scope(exit) colIndex++;
                    if (indices.length > 0)
                    {
                        foreach (ti, ToType; Fields!(Contents))
                        {
                            if (indices[ti] == colIndex)
                            {
                                static if (!isSomeString!ToType) skipWS(colData);
                                recordContent.tupleof[ti] = to!ToType(colData);
                            }
                        }
                    }
                    else
                    {
                        foreach (ti, ToType; Fields!(Contents))
                        {
                            if (ti == colIndex)
                            {
                                static if (!isSomeString!ToType) skipWS(colData);
                                recordContent.tupleof[ti] = to!ToType(colData);
                            }
                        }
                    }
                    recordRange.popFront();
                }
            }
            catch (ConvException e)
            {
                throw new CSVException(e.msg, _input.row, colIndex, e);
            }
        }
    }
}

@safe pure unittest
{
    import std.algorithm.comparison : equal;

    string str = `76;^26^;22`;
    int[] ans = [76,26,22];
    auto records = CsvReader!(int,Malformed.ignore,string,char,string[])
          (str, ';', '^');

    foreach (record; records)
    {
        assert(equal(record, ans));
    }
}

// Bugzilla 15545
// @system due to the catch for Throwable
@system pure unittest
{
    import std.exception : assertNotThrown;
    enum failData =
    "name, surname, age
    Joe, Joker, 99\r";
    auto r = csvReader(failData);
    assertNotThrown((){foreach (entry; r){}}());
}

/*
 * This input range is accessible through $(LREF CsvReader) when the
 * requested $(D Contents) type is neither a structure or an associative array.
 */
private struct CsvRecord(Contents, Malformed ErrorLevel, Range, Separator)
if (!is(Contents == class) && !is(Contents == struct))
{
    import std.array : appender;
private:
    Input!(Range, ErrorLevel)* _input;
    Separator _separator;
    Separator _quote;
    Contents curContentsoken;
    typeof(appender!(dchar[])()) _front;
    bool _empty;
    size_t[] _popCount;
public:
    /*
     * Params:
     *      input = Pointer to a character input range
     *      delimiter = Separator for each column
     *      quote = Character used for quotation
     *      indices = An array containing which columns will be returned.
     *             If empty, all columns are returned. List must be in order.
     */
    this(Input!(Range, ErrorLevel)* input, Separator delimiter,
         Separator quote, size_t[] indices)
    {
        _input = input;
        _separator = delimiter;
        _quote = quote;
        _front = appender!(dchar[])();
        _popCount = indices.dup;

        // If a header was given, each call to popFront will need
        // to eliminate so many tokens. This calculates
        // how many will be skipped to get to the next header column
        size_t normalizer;
        foreach (ref c; _popCount)
        {
            static if (ErrorLevel == Malformed.ignore)
            {
                // If we are not throwing exceptions
                // a header may not exist, indices are sorted
                // and will be size_t.max if not found.
                if (c == size_t.max)
                    break;
            }
            c -= normalizer;
            normalizer += c + 1;
        }

        prime();
    }

    /**
     * Part of an input range as defined by
     * $(REF isInputRange, std,range,primitives).
     */
    @property Contents front() @safe pure
    {
        assert(!empty);
        return curContentsoken;
    }

    /**
     * Part of an input range as defined by
     * $(REF isInputRange, std,range,primitives).
     */
    @property bool empty() @safe pure nothrow @nogc const
    {
        return _empty;
    }

    /*
     * CsvRecord is complete when input
     * is empty or starts with record break
     */
    private bool recordEnd()
    {
        if (_input.range.empty
           || _input.range.front == '\n'
           || _input.range.front == '\r')
        {
            return true;
        }
        return false;
    }


    /**
     * Part of an input range as defined by
     * $(REF isInputRange, std,range,primitives).
     *
     * Throws:
     *       $(LREF CSVException) When a quote is found in an unquoted field,
     *       data continues after a closing quote, the quoted field was not
     *       closed before data was empty, a conversion failed, or when the
     *       row's length does not match the previous length.
     */
    void popFront()
    {
        static if (ErrorLevel == Malformed.throwException)
            import std.format : format;
        // Skip last of record when header is depleted.
        if (_popCount.ptr && _popCount.empty)
            while (!recordEnd())
            {
                prime(1);
            }

        if (recordEnd())
        {
            _empty = true;
            static if (ErrorLevel == Malformed.throwException)
                if (_input.rowLength != 0)
                    if (_input.col != _input.rowLength)
                        throw new CSVException(
                           format("Row %s's length %s does not match "~
                                  "previous length of %s.", _input.row,
                                  _input.col, _input.rowLength));
            return;
        }
        else
        {
            static if (ErrorLevel == Malformed.throwException)
                if (_input.rowLength != 0)
                    if (_input.col > _input.rowLength)
                        throw new CSVException(
                           format("Row %s's length %s does not match "~
                                  "previous length of %s.", _input.row,
                                  _input.col, _input.rowLength));
        }

        // Separator is left on the end of input from the last call.
        // This cannot be moved to after the call to csvNextToken as
        // there may be an empty record after it.
        if (_input.range.front == _separator)
            _input.range.popFront();

        _front.shrinkTo(0);

        prime();
    }

    /*
     * Handles moving to the next skipNum token.
     */
    private void prime(size_t skipNum)
    {
        foreach (i; 0 .. skipNum)
        {
            _input.col++;
            _front.shrinkTo(0);
            if (_input.range.front == _separator)
                _input.range.popFront();

            try
                csvNextToken!(Range, ErrorLevel, Separator)
                                   (_input.range, _front, _separator, _quote,false);
            catch (IncompleteCellException ice)
            {
                ice.row = _input.row;
                ice.col = _input.col;
                ice.partialData = _front.data.idup;
                throw ice;
            }
            catch (ConvException e)
            {
                throw new CSVException(e.msg, _input.row, _input.col, e);
            }
        }
    }

    private void prime()
    {
        try
        {
            _input.col++;
            csvNextToken!(Range, ErrorLevel, Separator)
                (_input.range, _front, _separator, _quote,false);
        }
        catch (IncompleteCellException ice)
        {
            ice.row = _input.row;
            ice.col = _input.col;
            ice.partialData = _front.data.idup;
            throw ice;
        }

        auto skipNum = _popCount.empty ? 0 : _popCount.front;
        if (!_popCount.empty)
            _popCount.popFront();

        if (skipNum == size_t.max)
        {
            while (!recordEnd())
                prime(1);
            _empty = true;
            return;
        }

        if (skipNum)
            prime(skipNum);

        auto data = _front.data;
        static if (!isSomeString!Contents) skipWS(data);
        try curContentsoken = to!Contents(data);
        catch (ConvException e)
        {
            throw new CSVException(e.msg, _input.row, _input.col, e);
        }
    }
}

/**
 * Lower level control over parsing CSV
 *
 * This function consumes the input. After each call the input will
 * start with either a delimiter or record break (\n, \r\n, \r) which
 * must be removed for subsequent calls.
 *
 * Params:
 *       input = Any CSV input
 *       ans   = The first field in the input
 *       sep   = The character to represent a comma in the specification
 *       quote = The character to represent a quote in the specification
 *       startQuoted = Whether the input should be considered to already be in
 * quotes
 *
 * Throws:
 *       $(LREF IncompleteCellException) When a quote is found in an unquoted
 *       field, data continues after a closing quote, or the quoted field was
 *       not closed before data was empty.
 */
void csvNextToken(Range, Malformed ErrorLevel = Malformed.throwException,
                           Separator, Output)
                          (ref Range input, ref Output ans,
                           Separator sep, Separator quote,
                           bool startQuoted = false)
if (isSomeChar!Separator && isInputRange!Range
    && is(Unqual!(ElementType!Range) == dchar)
    && isOutputRange!(Output, dchar))
{
    bool quoted = startQuoted;
    bool escQuote;
    if (input.empty)
        return;

    if (input.front == '\n')
        return;
    if (input.front == '\r')
        return;

    if (input.front == quote)
    {
        quoted = true;
        input.popFront();
    }

    while (!input.empty)
    {
        assert(!(quoted && escQuote));
        if (!quoted)
        {
            // When not quoted the token ends at sep
            if (input.front == sep)
                break;
            if (input.front == '\r')
                break;
            if (input.front == '\n')
                break;
        }
        if (!quoted && !escQuote)
        {
            if (input.front == quote)
            {
                // Not quoted, but quote found
                static if (ErrorLevel == Malformed.throwException)
                    throw new IncompleteCellException(
                          "Quote located in unquoted token");
                else static if (ErrorLevel == Malformed.ignore)
                    ans.put(quote);
            }
            else
            {
                // Not quoted, non-quote character
                ans.put(input.front);
            }
        }
        else
        {
            if (input.front == quote)
            {
                // Quoted, quote found
                // By turning off quoted and turning on escQuote
                // I can tell when to add a quote to the string
                // escQuote is turned to false when it escapes a
                // quote or is followed by a non-quote (see outside else).
                // They are mutually exclusive, but provide different
                // information.
                if (escQuote)
                {
                    escQuote = false;
                    quoted = true;
                    ans.put(quote);
                } else
                {
                    escQuote = true;
                    quoted = false;
                }
            }
            else
            {
                // Quoted, non-quote character
                if (escQuote)
                {
                    static if (ErrorLevel == Malformed.throwException)
                        throw new IncompleteCellException(
                          "Content continues after end quote, " ~
                          "or needs to be escaped.");
                    else static if (ErrorLevel == Malformed.ignore)
                        break;
                }
                ans.put(input.front);
            }
        }
        input.popFront();
    }

    static if (ErrorLevel == Malformed.throwException)
        if (quoted && (input.empty || input.front == '\n' || input.front == '\r'))
            throw new IncompleteCellException(
                  "Data continues on future lines or trailing quote");

}

///
@safe unittest
{
    import std.array : appender;
    import std.range.primitives : popFront;

    string str = "65,63\n123,3673";

    auto a = appender!(char[])();

    csvNextToken(str,a,',','"');
    assert(a.data == "65");
    assert(str == ",63\n123,3673");

    str.popFront();
    a.shrinkTo(0);
    csvNextToken(str,a,',','"');
    assert(a.data == "63");
    assert(str == "\n123,3673");

    str.popFront();
    a.shrinkTo(0);
    csvNextToken(str,a,',','"');
    assert(a.data == "123");
    assert(str == ",3673");
}

// Test csvNextToken on simplest form and correct format.
@safe pure unittest
{
    import std.array;

    string str = "\U00010143Hello,65,63.63\nWorld,123,3673.562";

    auto a = appender!(dchar[])();
    csvNextToken!string(str,a,',','"');
    assert(a.data == "\U00010143Hello");
    assert(str == ",65,63.63\nWorld,123,3673.562");

    str.popFront();
    a.shrinkTo(0);
    csvNextToken(str,a,',','"');
    assert(a.data == "65");
    assert(str == ",63.63\nWorld,123,3673.562");

    str.popFront();
    a.shrinkTo(0);
    csvNextToken(str,a,',','"');
    assert(a.data == "63.63");
    assert(str == "\nWorld,123,3673.562");

    str.popFront();
    a.shrinkTo(0);
    csvNextToken(str,a,',','"');
    assert(a.data == "World");
    assert(str == ",123,3673.562");

    str.popFront();
    a.shrinkTo(0);
    csvNextToken(str,a,',','"');
    assert(a.data == "123");
    assert(str == ",3673.562");

    str.popFront();
    a.shrinkTo(0);
    csvNextToken(str,a,',','"');
    assert(a.data == "3673.562");
    assert(str == "");
}

// Test quoted tokens
@safe pure unittest
{
    import std.array;

    string str = `one,two,"three ""quoted""","",` ~ "\"five\nnew line\"\nsix";

    auto a = appender!(dchar[])();
    csvNextToken!string(str,a,',','"');
    assert(a.data == "one");
    assert(str == `,two,"three ""quoted""","",` ~ "\"five\nnew line\"\nsix");

    str.popFront();
    a.shrinkTo(0);
    csvNextToken(str,a,',','"');
    assert(a.data == "two");
    assert(str == `,"three ""quoted""","",` ~ "\"five\nnew line\"\nsix");

    str.popFront();
    a.shrinkTo(0);
    csvNextToken(str,a,',','"');
    assert(a.data == "three \"quoted\"");
    assert(str == `,"",` ~ "\"five\nnew line\"\nsix");

    str.popFront();
    a.shrinkTo(0);
    csvNextToken(str,a,',','"');
    assert(a.data == "");
    assert(str == ",\"five\nnew line\"\nsix");

    str.popFront();
    a.shrinkTo(0);
    csvNextToken(str,a,',','"');
    assert(a.data == "five\nnew line");
    assert(str == "\nsix");

    str.popFront();
    a.shrinkTo(0);
    csvNextToken(str,a,',','"');
    assert(a.data == "six");
    assert(str == "");
}

// Test empty data is pulled at end of record.
@safe pure unittest
{
    import std.array;

    string str = "one,";
    auto a = appender!(dchar[])();
    csvNextToken(str,a,',','"');
    assert(a.data == "one");
    assert(str == ",");

    a.shrinkTo(0);
    csvNextToken(str,a,',','"');
    assert(a.data == "");
}

// Test exceptions
@safe pure unittest
{
    import std.array;

    string str = "\"one\nnew line";

    typeof(appender!(dchar[])()) a;
    try
    {
        a = appender!(dchar[])();
        csvNextToken(str,a,',','"');
        assert(0);
    }
    catch (IncompleteCellException ice)
    {
        assert(a.data == "one\nnew line");
        assert(str == "");
    }

    str = "Hello world\"";

    try
    {
        a = appender!(dchar[])();
        csvNextToken(str,a,',','"');
        assert(0);
    }
    catch (IncompleteCellException ice)
    {
        assert(a.data == "Hello world");
        assert(str == "\"");
    }

    str = "one, two \"quoted\" end";

    a = appender!(dchar[])();
    csvNextToken!(string,Malformed.ignore)(str,a,',','"');
    assert(a.data == "one");
    str.popFront();
    a.shrinkTo(0);
    csvNextToken!(string,Malformed.ignore)(str,a,',','"');
    assert(a.data == " two \"quoted\" end");
}

// Test modifying token delimiter
@safe pure unittest
{
    import std.array;

    string str = `one|two|/three "quoted"/|//`;

    auto a = appender!(dchar[])();
    csvNextToken(str,a, '|','/');
    assert(a.data == "one"d);
    assert(str == `|two|/three "quoted"/|//`);

    str.popFront();
    a.shrinkTo(0);
    csvNextToken(str,a, '|','/');
    assert(a.data == "two"d);
    assert(str == `|/three "quoted"/|//`);

    str.popFront();
    a.shrinkTo(0);
    csvNextToken(str,a, '|','/');
    assert(a.data == `three "quoted"`);
    assert(str == `|//`);

    str.popFront();
    a.shrinkTo(0);
    csvNextToken(str,a, '|','/');
    assert(a.data == ""d);
}

// Bugzilla 8908
@safe pure unittest
{
    string csv = `  1.0, 2.0, 3.0
                    4.0, 5.0, 6.0`;

    static struct Data { real a, b, c; }
    size_t i = 0;
    foreach (data; csvReader!Data(csv)) with (data)
    {
        int[] row = [cast(int) a, cast(int) b, cast(int) c];
        if (i == 0)
            assert(row == [1, 2, 3]);
        else
            assert(row == [4, 5, 6]);
        ++i;
    }

    i = 0;
    foreach (data; csvReader!real(csv))
    {
        auto a = data.front;    data.popFront();
        auto b = data.front;    data.popFront();
        auto c = data.front;
        int[] row = [cast(int) a, cast(int) b, cast(int) c];
        if (i == 0)
            assert(row == [1, 2, 3]);
        else
            assert(row == [4, 5, 6]);
        ++i;
    }
}

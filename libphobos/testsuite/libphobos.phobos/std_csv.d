@safe unittest
{
    import std.csv;

    import std.exception : collectException;
    import std.algorithm.searching : count;
    string text = "a,b,c\nHello,65";
    auto ex = collectException!CSVException(csvReader(text).count);
    assert(ex.toString == "(Row: 0, Col: 0) Row 2's length 2 does not match previous length of 3.");
}

@safe unittest
{
    import std.csv;

    import std.exception : collectException;
    import std.algorithm.searching : count;
    import std.typecons : Tuple;
    string text = "a,b\nHello,65";
    auto ex = collectException!CSVException(csvReader!(Tuple!(string,int))(text).count);
    assert(ex.toString == "(Row: 1, Col: 2) Unexpected 'b' when converting from type string to type int");
}

@safe unittest
{
    import std.csv;

    import std.exception : assertThrown;
    string text = "a,\"b,c\nHello,65,2.5";
    assertThrown!IncompleteCellException(text.csvReader(["a","b","c"]));
}

@safe unittest
{
    import std.csv;

    import std.exception : assertThrown;
    string text = "a,b,c\nHello,65,2.5";
    assertThrown!HeaderMismatchException(text.csvReader(["b","c","invalid"]));
}

@safe unittest
{
    import std.csv;

    import std.algorithm.comparison : equal;
    import std.algorithm.searching : count;
    import std.exception : assertThrown;

    string text = "a,b,c\nHello,65,\"2.5";
    assertThrown!IncompleteCellException(text.csvReader.count);

    // ignore the exceptions and try to handle invalid CSV
    auto firstLine = text.csvReader!(string, Malformed.ignore)(null).front;
    assert(firstLine.equal(["Hello", "65", "2.5"]));
}

@safe unittest
{
    import std.csv;

    import std.algorithm.comparison : equal;
    string text = "76,26,22";
    auto records = text.csvReader!int;
    assert(records.equal!equal([
        [76, 26, 22],
    ]));
}

@safe unittest
{
    import std.csv;

    import std.algorithm.comparison : equal;
    string text = "Hello;65;2.5\nWorld;123;7.5";
    struct Layout
    {
        string name;
        int value;
        double other;
    }

    auto records = text.csvReader!Layout(';');
    assert(records.equal([
        Layout("Hello", 65, 2.5),
        Layout("World", 123, 7.5),
    ]));
}

@safe unittest
{
    import std.csv;

    string text = "A \" is now part of the data";
    auto records = text.csvReader!(string, Malformed.ignore);
    auto record = records.front;

    assert(record.front == text);
}

@safe unittest
{
    import std.csv;

    import std.algorithm.comparison : equal;
    string text = "a,b,c\nHello,65,63.63\nWorld,123,3673.562";
    auto records = text.csvReader!int(["b"]);

    assert(records.equal!equal([
        [65],
        [123],
    ]));
}

@safe unittest
{
    import std.csv;

    import std.algorithm.comparison : equal;
    string text = "a,b,c\nHello,65,2.5\nWorld,123,7.5";
    struct Layout
    {
        int value;
        double other;
        string name;
    }

    auto records = text.csvReader!Layout(["b","c","a"]);
    assert(records.equal([
        Layout(65, 2.5, "Hello"),
        Layout(123, 7.5, "World")
    ]));
}

@safe unittest
{
    import std.csv;

    string text = "a,b,c\nHello,65,63.63";
    auto records = text.csvReader(null);

    assert(records.header == ["a","b","c"]);
}

@safe unittest
{
    import std.csv;

    import std.algorithm.comparison : equal;

    string text = "76,26,22\n1,2\n3,4,5,6";
    auto records = text.csvReader!int(',', '"', true);

    assert(records.equal!equal([
        [76, 26, 22],
        [1, 2],
        [3, 4, 5, 6]
    ]));
}

@safe unittest
{
    import std.csv;

    import std.algorithm.comparison : equal;

    static struct Three
    {
        int a;
        int b;
        int c;
    }

    string text = "76,26,22\n1,2\n3,4,5,6";
    auto records = text.csvReader!Three(',', '"', true);

    assert(records.equal([
        Three(76, 26, 22),
        Three(1, 2, 0),
        Three(3, 4, 5)
    ]));
}

@safe unittest
{
    import std.csv;

    import std.algorithm.comparison : equal;

    auto text = "Name,Occupation,Salary\r" ~
        "Joe,Carpenter,300000\nFred,Blacksmith\r\n";

    auto r = csvReader!(string[string])(text, null, ',', '"', true);

    assert(r.equal([
        [ "Name" : "Joe", "Occupation" : "Carpenter", "Salary" : "300000" ],
        [ "Name" : "Fred", "Occupation" : "Blacksmith" ]
    ]));
}

@safe unittest
{
    import std.csv;

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


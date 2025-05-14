@safe unittest
{
    import std.range.interfaces;

    import std.algorithm.iteration : map;
    import std.range : iota;

    void useRange(InputRange!int range) {
        // Function body.
    }

    // Create a range type.
    auto squares = map!"a * a"(iota(10));

    // Wrap it in an interface.
    auto squaresWrapped = inputRangeObject(squares);

    // Use it.
    useRange(squaresWrapped);
}

@safe unittest
{
    import std.range.interfaces;

     import std.array;
     auto app = appender!(uint[])();
     auto appWrapped = outputRangeObject!(uint, uint[])(app);
     static assert(is(typeof(appWrapped) : OutputRange!(uint[])));
     static assert(is(typeof(appWrapped) : OutputRange!(uint)));
}


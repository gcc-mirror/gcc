pure @system unittest
{
    import std.container.array;

    auto arr = Array!int(0, 2, 3);
    assert(arr[0] == 0);
    assert(arr.front == 0);
    assert(arr.back == 3);

    // reserve space
    arr.reserve(1000);
    assert(arr.length == 3);
    assert(arr.capacity >= 1000);

    // insertion
    arr.insertBefore(arr[1..$], 1);
    assert(arr.front == 0);
    assert(arr.length == 4);

    arr.insertBack(4);
    assert(arr.back == 4);
    assert(arr.length == 5);

    // set elements
    arr[1] *= 42;
    assert(arr[1] == 42);
}

pure @system unittest
{
    import std.container.array;

    import std.algorithm.comparison : equal;
    auto arr = Array!int(1, 2, 3);

    // concat
    auto b = Array!int(11, 12, 13);
    arr ~= b;
    assert(arr.length == 6);

    // slicing
    assert(arr[1 .. 3].equal([2, 3]));

    // remove
    arr.linearRemove(arr[1 .. 3]);
    assert(arr[0 .. 2].equal([1, 11]));
}

pure @system unittest
{
    import std.container.array;

    auto arr = Array!bool([true, true, false, true, false]);
    assert(arr.length == 5);
}


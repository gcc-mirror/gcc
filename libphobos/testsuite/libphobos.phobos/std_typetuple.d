@safe unittest
{
    import std.typetuple;

    import std.typetuple;
    alias TL = TypeTuple!(int, double);

    int foo(TL td)  // same as int foo(int, double);
    {
        return td[0] + cast(int) td[1];
    }
    assert(foo(1, 2.5) == 3);
}

@safe unittest
{
    import std.typetuple;

    alias TL = TypeTuple!(int, double);

    alias Types = TypeTuple!(TL, char);
    static assert(is(Types == TypeTuple!(int, double, char)));
}


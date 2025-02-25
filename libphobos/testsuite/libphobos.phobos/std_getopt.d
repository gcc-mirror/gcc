@safe unittest
{
    import std.getopt;

    auto args = ["prog", "--foo", "-b"];

    bool foo;
    bool bar;
    auto rslt = getopt(args, "foo|f", "Some information about foo.", &foo, "bar|b",
        "Some help message about bar.", &bar);

    if (rslt.helpWanted)
    {
        defaultGetoptPrinter("Some information about the program.",
            rslt.options);
    }
}


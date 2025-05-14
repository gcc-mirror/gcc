@safe pure unittest
{
    import std.format.spec;

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

@safe pure unittest
{
    import std.format.spec;

    import std.array : appender;
    import std.format.write : formatValue;

    auto spec = singleSpec("%10.3e");
    auto writer = appender!string();
    writer.formatValue(42.0, spec);

    assert(writer.data == " 4.200e+01");
}


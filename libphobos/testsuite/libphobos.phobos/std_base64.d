pure @safe unittest
{
    import std.base64;

    ubyte[] data = [0x83, 0xd7, 0x30, 0x7a, 0x01, 0x3f];
    assert(Base64.encode(data) == "g9cwegE/");
    assert(Base64.decode("g9cwegE/") == data);
}

pure @safe unittest
{
    import std.base64;

    ubyte[] data = [0x83, 0xd7, 0x30, 0x7a, 0x01, 0x3f];
    assert(Base64URL.encode(data) == "g9cwegE_");
    assert(Base64URL.decode("g9cwegE_") == data);
}

pure @safe unittest
{
    import std.base64;

    ubyte[] data = [0x83, 0xd7, 0x30, 0x7b, 0xef];
    assert(Base64URLNoPadding.encode(data) == "g9cwe-8");
    assert(Base64URLNoPadding.decode("g9cwe-8") == data);
}

@safe unittest
{
    import std.base64;

        ubyte[] data = [0x1a, 0x2b, 0x3c, 0x4d, 0x5d, 0x6e];

        // Allocate a buffer large enough to hold the encoded string.
        auto buf = new char[Base64.encodeLength(data.length)];

        Base64.encode(data, buf);
        assert(buf == "Gis8TV1u");
    
}

@nogc nothrow @safe unittest
{
    import std.base64;

        ubyte[6] data = [0x83, 0xd7, 0x30, 0x7a, 0x01, 0x3f];
        char[32] buffer;    // much bigger than necessary

        // Just to be sure...
        auto encodedLength = Base64.encodeLength(data.length);
        assert(buffer.length >= encodedLength);

        // encode() returns a slice to the provided buffer.
        auto encoded = Base64.encode(data[], buffer[]);
        assert(encoded is buffer[0 .. encodedLength]);
        assert(encoded == "g9cwegE/");
    
}

@safe pure nothrow unittest
{
    import std.base64;

        import std.array : appender;

        auto output = appender!string();
        ubyte[] data = [0x1a, 0x2b, 0x3c, 0x4d, 0x5d, 0x6e];

        // This overload of encode() returns the number of calls to the output
        // range's put method.
        assert(Base64.encode(data, output) == 8);
        assert(output.data == "Gis8TV1u");
    
}

@safe unittest
{
    import std.base64;

        ubyte[] data = [0x1a, 0x2b, 0x3c, 0x4d, 0x5d, 0x6e];
        assert(Base64.encode(data) == "Gis8TV1u");
    
}

@safe unittest
{
    import std.base64;

        auto encoded = "Gis8TV1u";

        // Allocate a sufficiently large buffer to hold to decoded result.
        auto buffer = new ubyte[Base64.decodeLength(encoded.length)];

        Base64.decode(encoded, buffer);
        assert(buffer == [0x1a, 0x2b, 0x3c, 0x4d, 0x5d, 0x6e]);
    
}

@safe unittest
{
    import std.base64;

        auto encoded = "Gis8TV1u";
        ubyte[32] buffer;   // much bigger than necessary

        // Just to be sure...
        auto decodedLength = Base64.decodeLength(encoded.length);
        assert(buffer.length >= decodedLength);

        // decode() returns a slice of the given buffer.
        auto decoded = Base64.decode(encoded, buffer[]);
        assert(decoded is buffer[0 .. decodedLength]);
        assert(decoded == [0x1a, 0x2b, 0x3c, 0x4d, 0x5d, 0x6e]);
    
}

@system unittest
{
    import std.base64;

        struct OutputRange
        {
            ubyte[] result;
            void put(ubyte b) { result ~= b; }
        }
        OutputRange output;

        // This overload of decode() returns the number of calls to put().
        assert(Base64.decode("Gis8TV1u", output) == 6);
        assert(output.result == [0x1a, 0x2b, 0x3c, 0x4d, 0x5d, 0x6e]);
    
}

@safe unittest
{
    import std.base64;

        auto data = "Gis8TV1u";
        assert(Base64.decode(data) == [0x1a, 0x2b, 0x3c, 0x4d, 0x5d, 0x6e]);
    
}

@safe pure unittest
{
    import std.base64;

        import std.algorithm.comparison : equal;
        string encoded =
            "VGhvdSBzaGFsdCBuZXZlciBjb250aW51ZSBhZnRlciBhc3NlcnRpbmcgbnVsbA==";

        assert(Base64.decoder(encoded)
            .equal("Thou shalt never continue after asserting null"));
    
}

@safe unittest
{
    import std.base64;

    import std.string : representation;

    // pre-defined: alias Base64 = Base64Impl!('+', '/');
    ubyte[] emptyArr;
    assert(Base64.encode(emptyArr) == "");
    assert(Base64.encode("f".representation) == "Zg==");
    assert(Base64.encode("foo".representation) == "Zm9v");

    alias Base64Re = Base64Impl!('!', '=', Base64.NoPadding);
    assert(Base64Re.encode("f".representation) == "Zg");
    assert(Base64Re.encode("foo".representation) == "Zm9v");
}

@safe unittest
{
    import std.base64;

    import std.exception : assertThrown;
    assertThrown!Base64Exception(Base64.decode("ab|c"));
}


@safe unittest
{
    import std.digest.crc;

    //Template API
    import std.digest.crc;

    ubyte[4] hash = crc32Of("The quick brown fox jumps over the lazy dog");
    assert(crcHexString(hash) == "414FA339");

    //Feeding data
    ubyte[1024] data;
    CRC32 crc;
    crc.put(data[]);
    crc.start(); //Start again
    crc.put(data[]);
    hash = crc.finish();
}

@safe unittest
{
    import std.digest.crc;

    //OOP API
    import std.digest.crc;

    auto crc = new CRC32Digest();
    ubyte[] hash = crc.digest("The quick brown fox jumps over the lazy dog");
    assert(crcHexString(hash) == "414FA339"); //352441c2

    //Feeding data
    ubyte[1024] data;
    crc.put(data[]);
    crc.reset(); //Start again
    crc.put(data[]);
    hash = crc.finish();
}

@safe unittest
{
    import std.digest.crc;

    //Simple example, hashing a string using crc32Of helper function
    ubyte[4] hash32 = crc32Of("abc");
    //Let's get a hash string
    assert(crcHexString(hash32) == "352441C2");
    // Repeat for CRC64
    ubyte[8] hash64ecma = crc64ECMAOf("abc");
    assert(crcHexString(hash64ecma) == "2CD8094A1A277627");
    ubyte[8] hash64iso = crc64ISOOf("abc");
    assert(crcHexString(hash64iso) == "3776C42000000000");
}

@safe unittest
{
    import std.digest.crc;

    ubyte[1024] data;
    //Using the basic API
    CRC32 hash32;
    CRC64ECMA hash64ecma;
    CRC64ISO hash64iso;
    //Initialize data here...
    hash32.put(data);
    ubyte[4] result32 = hash32.finish();
    hash64ecma.put(data);
    ubyte[8] result64ecma = hash64ecma.finish();
    hash64iso.put(data);
    ubyte[8] result64iso = hash64iso.finish();
}

@safe unittest
{
    import std.digest.crc;

    //Let's use the template features:
    //Note: When passing a CRC32 to a function, it must be passed by reference!
    void doSomething(T)(ref T hash)
    if (isDigest!T)
    {
      hash.put(cast(ubyte) 0);
    }
    CRC32 crc32;
    crc32.start();
    doSomething(crc32);
    assert(crcHexString(crc32.finish()) == "D202EF8D");
    // repeat for CRC64
    CRC64ECMA crc64ecma;
    crc64ecma.start();
    doSomething(crc64ecma);
    assert(crcHexString(crc64ecma.finish()) == "1FADA17364673F59");
    CRC64ISO crc64iso;
    crc64iso.start();
    doSomething(crc64iso);
    assert(crcHexString(crc64iso.finish()) == "6F90000000000000");
}

@system unittest
{
    import std.digest.crc;

    ubyte[] data = [4,5,7,25];
    assert(data.crc32Of == [167, 180, 199, 131]);

    import std.utf : byChar;
    assert("hello"d.byChar.crc32Of == [134, 166, 16, 54]);

    ubyte[4] hash = "abc".crc32Of();
    assert(hash == digest!CRC32("ab", "c"));

    import std.range : iota;
    enum ubyte S = 5, F = 66;
    assert(iota(S, F).crc32Of == [59, 140, 234, 154]);
}

@system unittest
{
    import std.digest.crc;

    ubyte[] data = [4,5,7,25];
    assert(data.crc64ECMAOf == [58, 142, 220, 214, 118, 98, 105, 69]);

    import std.utf : byChar;
    assert("hello"d.byChar.crc64ECMAOf == [177, 55, 185, 219, 229, 218, 30, 155]);

    ubyte[8] hash = "abc".crc64ECMAOf();
    assert("abc".crc64ECMAOf == [39, 118, 39, 26, 74, 9, 216, 44]);
    assert(hash == digest!CRC64ECMA("ab", "c"));

    import std.range : iota;
    enum ubyte S = 5, F = 66;
    assert(iota(S, F).crc64ECMAOf == [6, 184, 91, 238, 46, 213, 127, 188]);
}

@system unittest
{
    import std.digest.crc;

    ubyte[] data = [4,5,7,25];
    assert(data.crc64ISOOf == [0, 0, 0, 80, 137, 232, 203, 120]);

    import std.utf : byChar;
    assert("hello"d.byChar.crc64ISOOf == [0, 0, 16, 216, 226, 238, 62, 60]);

    ubyte[8] hash = "abc".crc64ISOOf();
    assert("abc".crc64ISOOf == [0, 0, 0, 0, 32, 196, 118, 55]);
    assert(hash == digest!CRC64ISO("ab", "c"));

    import std.range : iota;
    enum ubyte S = 5, F = 66;

    assert(iota(S, F).crc64ISOOf == [21, 185, 116, 95, 219, 11, 54, 7]);
}

@safe unittest
{
    import std.digest.crc;

    //Simple example, hashing a string using CRC32Digest.digest helper function
    auto crc = new CRC32Digest();
    ubyte[] hash = crc.digest("abc");
    //Let's get a hash string
    assert(crcHexString(hash) == "352441C2");
}

@system unittest
{
    import std.digest.crc;

     //Let's use the OOP features:
    void test(Digest dig)
    {
      dig.put(cast(ubyte) 0);
    }
    auto crc = new CRC32Digest();
    test(crc);

    //Let's use a custom buffer:
    ubyte[4] buf;
    ubyte[] result = crc.finish(buf[]);
    assert(crcHexString(result) == "D202EF8D");
}

@safe unittest
{
    import std.digest.crc;

    //Simple example
    auto hash = new CRC32Digest();
    hash.put(cast(ubyte) 0);
    ubyte[] result = hash.finish();
}

@system unittest
{
    import std.digest.crc;

    //using a supplied buffer
    ubyte[4] buf;
    auto hash = new CRC32Digest();
    hash.put(cast(ubyte) 0);
    ubyte[] result = hash.finish(buf[]);
    //The result is now in result (and in buf. If you pass a buffer which is bigger than
    //necessary, result will have the correct length, but buf will still have it's original
    //length)
}


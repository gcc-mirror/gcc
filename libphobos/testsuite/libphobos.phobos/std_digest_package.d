@system unittest
{
    import std.digest;

    import std.digest.crc;

    //Simple example
    char[8] hexHash = hexDigest!CRC32("The quick brown fox jumps over the lazy dog");
    assert(hexHash == "39A34F41");

    //Simple example, using the API manually
    CRC32 context = makeDigest!CRC32();
    context.put(cast(ubyte[])"The quick brown fox jumps over the lazy dog");
    ubyte[4] hash = context.finish();
    assert(toHexString(hash) == "39A34F41");
}

@system unittest
{
    import std.digest;

    //Generating the hashes of a file, idiomatic D way
    import std.digest.crc, std.digest.md, std.digest.sha;
    import std.stdio;

    // Digests a file and prints the result.
    void digestFile(Hash)(string filename)
    if (isDigest!Hash)
    {
        auto file = File(filename);
        auto result = digest!Hash(file.byChunk(4096 * 1024));
        writefln("%s (%s) = %s", Hash.stringof, filename, toHexString(result));
    }

    void main(string[] args)
    {
        foreach (name; args[1 .. $])
        {
            digestFile!MD5(name);
            digestFile!SHA1(name);
            digestFile!CRC32(name);
        }
    }
}

@system unittest
{
    import std.digest;

    //Generating the hashes of a file using the template API
    import std.digest.crc, std.digest.md, std.digest.sha;
    import std.stdio;
    // Digests a file and prints the result.
    void digestFile(Hash)(ref Hash hash, string filename)
    if (isDigest!Hash)
    {
        File file = File(filename);

        //As digests imlement OutputRange, we could use std.algorithm.copy
        //Let's do it manually for now
        foreach (buffer; file.byChunk(4096 * 1024))
            hash.put(buffer);

        auto result = hash.finish();
        writefln("%s (%s) = %s", Hash.stringof, filename, toHexString(result));
    }

    void uMain(string[] args)
    {
        MD5 md5;
        SHA1 sha1;
        CRC32 crc32;

        md5.start();
        sha1.start();
        crc32.start();

        foreach (arg; args[1 .. $])
        {
            digestFile(md5, arg);
            digestFile(sha1, arg);
            digestFile(crc32, arg);
        }
    }
}

@system unittest
{
    import std.digest;

    import std.digest.crc, std.digest.md, std.digest.sha;
    import std.stdio;

    // Digests a file and prints the result.
    void digestFile(Digest hash, string filename)
    {
        File file = File(filename);

        //As digests implement OutputRange, we could use std.algorithm.copy
        //Let's do it manually for now
        foreach (buffer; file.byChunk(4096 * 1024))
          hash.put(buffer);

        ubyte[] result = hash.finish();
        writefln("%s (%s) = %s", typeid(hash).toString(), filename, toHexString(result));
    }

    void umain(string[] args)
    {
        auto md5 = new MD5Digest();
        auto sha1 = new SHA1Digest();
        auto crc32 = new CRC32Digest();

        foreach (arg; args[1 .. $])
        {
          digestFile(md5, arg);
          digestFile(sha1, arg);
          digestFile(crc32, arg);
        }
    }
}

@system unittest
{
    import std.digest;

    //Using the OutputRange feature
    import std.algorithm.mutation : copy;
    import std.digest.md;
    import std.range : repeat;

    auto oneMillionRange = repeat!ubyte(cast(ubyte)'a', 1000000);
    auto ctx = makeDigest!MD5();
    copy(oneMillionRange, &ctx); //Note: You must pass a pointer to copy!
    assert(ctx.finish().toHexString() == "7707D6AE4E027C70EEA2A935C2296F21");
}

@system unittest
{
    import std.digest;

    import std.digest.crc;
    static assert(isDigest!CRC32);
}

@system unittest
{
    import std.digest;

    import std.digest.crc;
    void myFunction(T)()
    if (isDigest!T)
    {
        T dig;
        dig.start();
        auto result = dig.finish();
    }
    myFunction!CRC32();
}

@system unittest
{
    import std.digest;

    import std.digest.crc;
    assert(is(DigestType!(CRC32) == ubyte[4]));
}

@system unittest
{
    import std.digest;

    import std.digest.crc;
    CRC32 dig;
    dig.start();
    DigestType!CRC32 result = dig.finish();
}

@system unittest
{
    import std.digest;

    import std.digest.crc, std.digest.md;
    assert(!hasPeek!(MD5));
    assert(hasPeek!CRC32);
}

@system unittest
{
    import std.digest;

    import std.digest.crc;
    void myFunction(T)()
    if (hasPeek!T)
    {
        T dig;
        dig.start();
        auto result = dig.peek();
    }
    myFunction!CRC32();
}

@system unittest
{
    import std.digest;

    import std.digest.hmac, std.digest.md;
    static assert(hasBlockSize!MD5        && MD5.blockSize      == 512);
    static assert(hasBlockSize!(HMAC!MD5) && HMAC!MD5.blockSize == 512);
}

@system unittest
{
    import std.digest;

    import std.digest.md;
    import std.range : repeat;
    auto testRange = repeat!ubyte(cast(ubyte)'a', 100);
    auto md5 = digest!MD5(testRange);
}

@system unittest
{
    import std.digest;

    import std.digest.crc, std.digest.md, std.digest.sha;
    auto md5   = digest!MD5(  "The quick brown fox jumps over the lazy dog");
    auto sha1  = digest!SHA1( "The quick brown fox jumps over the lazy dog");
    auto crc32 = digest!CRC32("The quick brown fox jumps over the lazy dog");
    assert(toHexString(crc32) == "39A34F41");
}

@system unittest
{
    import std.digest;

    import std.digest.crc;
    auto crc32 = digest!CRC32("The quick ", "brown ", "fox jumps over the lazy dog");
    assert(toHexString(crc32) == "39A34F41");
}

@system unittest
{
    import std.digest;

    import std.digest.md;
    import std.range : repeat;
    auto testRange = repeat!ubyte(cast(ubyte)'a', 100);
    assert(hexDigest!MD5(testRange) == "36A92CC94A9E0FA21F625F8BFB007ADF");
}

@system unittest
{
    import std.digest;

    import std.digest.crc;
    assert(hexDigest!(CRC32, Order.decreasing)("The quick brown fox jumps over the lazy dog") == "414FA339");
}

@system unittest
{
    import std.digest;

    import std.digest.crc;
    assert(hexDigest!(CRC32, Order.decreasing)("The quick ", "brown ", "fox jumps over the lazy dog") == "414FA339");
}

@system unittest
{
    import std.digest;

    import std.digest.md;
    auto md5 = makeDigest!MD5();
    md5.put(0);
    assert(toHexString(md5.finish()) == "93B885ADFE0DA089CDF634904FD59F71");
}

@system unittest
{
    import std.digest;

    //Using the OutputRange feature
    import std.algorithm.mutation : copy;
    import std.digest.md;
    import std.range : repeat;

    auto oneMillionRange = repeat!ubyte(cast(ubyte)'a', 1000000);
    auto ctx = new MD5Digest();
    copy(oneMillionRange, ctx);
    assert(ctx.finish().toHexString() == "7707D6AE4E027C70EEA2A935C2296F21");
}

@system unittest
{
    import std.digest;

    import std.digest.crc, std.digest.md, std.digest.sha;
    ubyte[] md5   = (new MD5Digest()).digest("The quick brown fox jumps over the lazy dog");
    ubyte[] sha1  = (new SHA1Digest()).digest("The quick brown fox jumps over the lazy dog");
    ubyte[] crc32 = (new CRC32Digest()).digest("The quick brown fox jumps over the lazy dog");
    assert(crcHexString(crc32) == "414FA339");
}

@system unittest
{
    import std.digest;

    import std.digest.crc;
    ubyte[] crc32 = (new CRC32Digest()).digest("The quick ", "brown ", "fox jumps over the lazy dog");
    assert(crcHexString(crc32) == "414FA339");
}

@system unittest
{
    import std.digest;

    void test(Digest dig)
    {
        dig.put(cast(ubyte) 0); //single ubyte
        dig.put(cast(ubyte) 0, cast(ubyte) 0); //variadic
        ubyte[10] buf;
        dig.put(buf); //buffer
    }
}

@safe unittest
{
    import std.digest;

    import std.digest.crc : CRC32;

    auto crc32 = digest!CRC32("The quick ", "brown ", "fox jumps over the lazy dog");
    assert(crc32.toHexString!(Order.decreasing) == "414FA339");
    assert(crc32.toHexString!(LetterCase.lower, Order.decreasing) == "414fa339");
}

@safe unittest
{
    import std.digest;

    import std.digest.crc;
    //Test with template API:
    auto crc32 = digest!CRC32("The quick ", "brown ", "fox jumps over the lazy dog");
    //Lower case variant:
    assert(toHexString!(LetterCase.lower)(crc32) == "39a34f41");
    //Usually CRCs are printed in this order, though:
    assert(toHexString!(Order.decreasing)(crc32) == "414FA339");
    assert(toHexString!(LetterCase.lower, Order.decreasing)(crc32) == "414fa339");
}

@safe unittest
{
    import std.digest;

    import std.digest.crc;
    // With OOP API
    auto crc32 = (new CRC32Digest()).digest("The quick ", "brown ", "fox jumps over the lazy dog");
    //Usually CRCs are printed in this order, though:
    assert(toHexString!(Order.decreasing)(crc32) == "414FA339");
}

@system unittest
{
    import std.digest;

    import std.digest.md;
    //Simple example
    auto hash = new WrapperDigest!MD5();
    hash.put(cast(ubyte) 0);
    auto result = hash.finish();
}

@system unittest
{
    import std.digest;

    //using a supplied buffer
    import std.digest.md;
    ubyte[16] buf;
    auto hash = new WrapperDigest!MD5();
    hash.put(cast(ubyte) 0);
    auto result = hash.finish(buf[]);
    //The result is now in result (and in buf). If you pass a buffer which is bigger than
    //necessary, result will have the correct length, but buf will still have it's original
    //length
}

@system pure unittest
{
    import std.digest;

    import std.digest.hmac : hmac;
    import std.digest.sha : SHA1;
    import std.string : representation;

    // a typical HMAC data integrity verification
    auto secret = "A7GZIP6TAQA6OHM7KZ42KB9303CEY0MOV5DD6NTV".representation;
    auto data = "data".representation;

    auto hex1 = data.hmac!SHA1(secret).toHexString;
    auto hex2 = data.hmac!SHA1(secret).toHexString;
    auto hex3 = "data1".representation.hmac!SHA1(secret).toHexString;

    assert( secureEqual(hex1[], hex2[]));
    assert(!secureEqual(hex1[], hex3[]));
}

@safe unittest
{
    import std.digest;

    assert(isHexString("0x0123456789ABCDEFabcdef"));
    assert(isHexString("0123456789ABCDEFabcdef"));
    assert(!isHexString("g"));
    assert(!isHexString("#"));
}

@safe unittest
{
    import std.digest;

    import std.range.primitives : ElementType, isForwardRange;
    import std.traits : ReturnType;

    // The decoder implements a forward range.
    static assert(isForwardRange!(ReturnType!(fromHexStringAsRange!string)));
    static assert(isForwardRange!(ReturnType!(fromHexStringAsRange!wstring)));
    static assert(isForwardRange!(ReturnType!(fromHexStringAsRange!dstring)));

    // The element type of the range is always `ubyte`.
    static assert(
        is(ElementType!(ReturnType!(fromHexStringAsRange!string)) == ubyte)
    );
    static assert(
        is(ElementType!(ReturnType!(fromHexStringAsRange!wstring)) == ubyte)
    );
    static assert(
        is(ElementType!(ReturnType!(fromHexStringAsRange!dstring)) == ubyte)
    );
}

@safe unittest
{
    import std.digest;

    // Single byte
    assert("0xff".fromHexString  == [255]);
    assert("0xff"w.fromHexString == [255]);
    assert("0xff"d.fromHexString == [255]);
    assert("0xC0".fromHexString  == [192]);
    assert("0x00".fromHexString  == [0]);

    // Nothing
    assert("".fromHexString  == []);
    assert(""w.fromHexString == []);
    assert(""d.fromHexString == []);

    // Nothing but a prefix
    assert("0x".fromHexString  == []);
    assert("0x"w.fromHexString == []);
    assert("0x"d.fromHexString == []);

    // Half a byte
    assert("0x1".fromHexString  == [0x01]);
    assert("0x1"w.fromHexString == [0x01]);
    assert("0x1"d.fromHexString == [0x01]);

    // Mixed case is fine.
    assert("0xAf".fromHexString == [0xAF]);
    assert("0xaF".fromHexString == [0xAF]);

    // Multiple bytes
    assert("0xfff".fromHexString     == [0x0F, 0xFF]);
    assert("0x123AaAa".fromHexString == [0x01, 0x23, 0xAA, 0xAA]);
    assert("EBBBBF".fromHexString    == [0xEB, 0xBB, 0xBF]);

    // md5 sum
    assert("d41d8cd98f00b204e9800998ecf8427e".fromHexString == [
        0xD4, 0x1D, 0x8C, 0xD9, 0x8F, 0x00, 0xB2, 0x04,
        0xE9, 0x80, 0x09, 0x98, 0xEC, 0xF8, 0x42, 0x7E,
    ]);
}

@safe unittest
{
    import std.digest;

    // Cycle self-test
    const ubyte[] initial = [0x00, 0x12, 0x34, 0xEB];
    assert(initial == initial.toHexString().fromHexString());
}


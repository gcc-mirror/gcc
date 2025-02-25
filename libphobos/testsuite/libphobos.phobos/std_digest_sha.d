@safe unittest
{
    import std.digest.sha;

    //Template API
    import std.digest.sha;

    ubyte[20] hash1 = sha1Of("abc");
    assert(toHexString(hash1) == "A9993E364706816ABA3E25717850C26C9CD0D89D");

    ubyte[28] hash224 = sha224Of("abc");
    assert(toHexString(hash224) == "23097D223405D8228642A477BDA255B32AADBCE4BDA0B3F7E36C9DA7");

    //Feeding data
    ubyte[1024] data;
    SHA1 sha1;
    sha1.start();
    sha1.put(data[]);
    sha1.start(); //Start again
    sha1.put(data[]);
    hash1 = sha1.finish();
}

@safe unittest
{
    import std.digest.sha;

    //OOP API
    import std.digest.sha;

    auto sha1 = new SHA1Digest();
    ubyte[] hash1 = sha1.digest("abc");
    assert(toHexString(hash1) == "A9993E364706816ABA3E25717850C26C9CD0D89D");

    auto sha224 = new SHA224Digest();
    ubyte[] hash224 = sha224.digest("abc");
    assert(toHexString(hash224) == "23097D223405D8228642A477BDA255B32AADBCE4BDA0B3F7E36C9DA7");

    //Feeding data
    ubyte[1024] data;
    sha1.put(data[]);
    sha1.reset(); //Start again
    sha1.put(data[]);
    hash1 = sha1.finish();
}

@safe unittest
{
    import std.digest.sha;

            //Simple example
            SHA1 hash;
            hash.start();
            hash.put(cast(ubyte) 0);
            ubyte[20] result = hash.finish();
        
}

@safe unittest
{
    import std.digest.sha;

    //Simple example, hashing a string using sha1Of helper function
    ubyte[20] hash = sha1Of("abc");
    //Let's get a hash string
    assert(toHexString(hash) == "A9993E364706816ABA3E25717850C26C9CD0D89D");

    //The same, but using SHA-224
    ubyte[28] hash224 = sha224Of("abc");
    assert(toHexString(hash224) == "23097D223405D8228642A477BDA255B32AADBCE4BDA0B3F7E36C9DA7");
}

@safe unittest
{
    import std.digest.sha;

    //Using the basic API
    SHA1 hash;
    hash.start();
    ubyte[1024] data;
    //Initialize data here...
    hash.put(data);
    ubyte[20] result = hash.finish();
}

@safe unittest
{
    import std.digest.sha;

    //Let's use the template features:
    //Note: When passing a SHA1 to a function, it must be passed by reference!
    void doSomething(T)(ref T hash)
    if (isDigest!T)
    {
      hash.put(cast(ubyte) 0);
    }
    SHA1 sha;
    sha.start();
    doSomething(sha);
    assert(toHexString(sha.finish()) == "5BA93C9DB0CFF93F52B521D7420E43F6EDA2784F");
}

@safe unittest
{
    import std.digest.sha;

    ubyte[20] hash = sha1Of("abc");
    assert(hash == digest!SHA1("abc"));

    ubyte[28] hash224 = sha224Of("abc");
    assert(hash224 == digest!SHA224("abc"));

    ubyte[32] hash256 = sha256Of("abc");
    assert(hash256 == digest!SHA256("abc"));

    ubyte[48] hash384 = sha384Of("abc");
    assert(hash384 == digest!SHA384("abc"));

    ubyte[64] hash512 = sha512Of("abc");
    assert(hash512 == digest!SHA512("abc"));

    ubyte[28] hash512_224 = sha512_224Of("abc");
    assert(hash512_224 == digest!SHA512_224("abc"));

    ubyte[32] hash512_256 = sha512_256Of("abc");
    assert(hash512_256 == digest!SHA512_256("abc"));
}

@safe unittest
{
    import std.digest.sha;

    //Simple example, hashing a string using Digest.digest helper function
    auto sha = new SHA1Digest();
    ubyte[] hash = sha.digest("abc");
    //Let's get a hash string
    assert(toHexString(hash) == "A9993E364706816ABA3E25717850C26C9CD0D89D");

    //The same, but using SHA-224
    auto sha224 = new SHA224Digest();
    ubyte[] hash224 = sha224.digest("abc");
    //Let's get a hash string
    assert(toHexString(hash224) == "23097D223405D8228642A477BDA255B32AADBCE4BDA0B3F7E36C9DA7");
}

@system unittest
{
    import std.digest.sha;

    //Let's use the OOP features:
    void test(Digest dig)
    {
      dig.put(cast(ubyte) 0);
    }
    auto sha = new SHA1Digest();
    test(sha);

    //Let's use a custom buffer:
    ubyte[20] buf;
    ubyte[] result = sha.finish(buf[]);
    assert(toHexString(result) == "5BA93C9DB0CFF93F52B521D7420E43F6EDA2784F");
}


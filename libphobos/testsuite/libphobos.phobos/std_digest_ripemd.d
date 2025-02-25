@safe unittest
{
    import std.digest.ripemd;

    //Template API
    import std.digest.md;

    ubyte[20] hash = ripemd160Of("abc");
    assert(toHexString(hash) == "8EB208F7E05D987A9B044A8E98C6B087F15A0BFC");

    //Feeding data
    ubyte[1024] data;
    RIPEMD160 md;
    md.start();
    md.put(data[]);
    md.start(); //Start again
    md.put(data[]);
    hash = md.finish();
}

@safe unittest
{
    import std.digest.ripemd;

    //OOP API
    import std.digest.md;

    auto md = new RIPEMD160Digest();
    ubyte[] hash = md.digest("abc");
    assert(toHexString(hash) == "8EB208F7E05D987A9B044A8E98C6B087F15A0BFC");

    //Feeding data
    ubyte[1024] data;
    md.put(data[]);
    md.reset(); //Start again
    md.put(data[]);
    hash = md.finish();
}

@safe unittest
{
    import std.digest.ripemd;

    //Simple example, hashing a string using ripemd160Of helper function
    ubyte[20] hash = ripemd160Of("abc");
    //Let's get a hash string
    assert(toHexString(hash) == "8EB208F7E05D987A9B044A8E98C6B087F15A0BFC");
}

@safe unittest
{
    import std.digest.ripemd;

    //Using the basic API
    RIPEMD160 hash;
    hash.start();
    ubyte[1024] data;
    //Initialize data here...
    hash.put(data);
    ubyte[20] result = hash.finish();
}

@safe unittest
{
    import std.digest.ripemd;

    //Let's use the template features:
    void doSomething(T)(ref T hash)
    if (isDigest!T)
    {
        hash.put(cast(ubyte) 0);
    }
    RIPEMD160 md;
    md.start();
    doSomething(md);
    assert(toHexString(md.finish()) == "C81B94933420221A7AC004A90242D8B1D3E5070D");
}

@safe unittest
{
    import std.digest.ripemd;

    //Simple example
    RIPEMD160 hash;
    hash.start();
    hash.put(cast(ubyte) 0);
    ubyte[20] result = hash.finish();
    assert(toHexString(result) == "C81B94933420221A7AC004A90242D8B1D3E5070D");
}

@safe unittest
{
    import std.digest.ripemd;

    ubyte[20] hash = ripemd160Of("abc");
    assert(hash == digest!RIPEMD160("abc"));
}

@safe unittest
{
    import std.digest.ripemd;

    //Simple example, hashing a string using Digest.digest helper function
    auto md = new RIPEMD160Digest();
    ubyte[] hash = md.digest("abc");
    //Let's get a hash string
    assert(toHexString(hash) == "8EB208F7E05D987A9B044A8E98C6B087F15A0BFC");
}

@system unittest
{
    import std.digest.ripemd;

    //Let's use the OOP features:
    void test(Digest dig)
    {
      dig.put(cast(ubyte) 0);
    }
    auto md = new RIPEMD160Digest();
    test(md);

    //Let's use a custom buffer:
    ubyte[20] buf;
    ubyte[] result = md.finish(buf[]);
    assert(toHexString(result) == "C81B94933420221A7AC004A90242D8B1D3E5070D");
}


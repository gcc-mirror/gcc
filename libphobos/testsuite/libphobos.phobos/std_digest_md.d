@safe unittest
{
    import std.digest.md;

    //Template API
    import std.digest.md;

    //Feeding data
    ubyte[1024] data;
    MD5 md5;
    md5.start();
    md5.put(data[]);
    md5.start(); //Start again
    md5.put(data[]);
    auto hash = md5.finish();
}

@safe unittest
{
    import std.digest.md;

    //OOP API
    import std.digest.md;

    auto md5 = new MD5Digest();
    ubyte[] hash = md5.digest("abc");
    assert(toHexString(hash) == "900150983CD24FB0D6963F7D28E17F72");

    //Feeding data
    ubyte[1024] data;
    md5.put(data[]);
    md5.reset(); //Start again
    md5.put(data[]);
    hash = md5.finish();
}

@safe unittest
{
    import std.digest.md;

            //Simple example
            MD5 hash;
            hash.start();
            hash.put(cast(ubyte) 0);
            ubyte[16] result = hash.finish();
        
}

@safe unittest
{
    import std.digest.md;

    //Simple example, hashing a string using md5Of helper function
    ubyte[16] hash = md5Of("abc");
    //Let's get a hash string
    assert(toHexString(hash) == "900150983CD24FB0D6963F7D28E17F72");
}

@safe unittest
{
    import std.digest.md;

    //Using the basic API
    MD5 hash;
    hash.start();
    ubyte[1024] data;
    //Initialize data here...
    hash.put(data);
    ubyte[16] result = hash.finish();
}

@safe unittest
{
    import std.digest.md;

    //Let's use the template features:
    void doSomething(T)(ref T hash)
    if (isDigest!T)
    {
        hash.put(cast(ubyte) 0);
    }
    MD5 md5;
    md5.start();
    doSomething(md5);
    assert(toHexString(md5.finish()) == "93B885ADFE0DA089CDF634904FD59F71");
}

@safe unittest
{
    import std.digest.md;

    ubyte[16] hash = md5Of("abc");
    assert(hash == digest!MD5("abc"));
}

@safe unittest
{
    import std.digest.md;

    //Simple example, hashing a string using Digest.digest helper function
    auto md5 = new MD5Digest();
    ubyte[] hash = md5.digest("abc");
    //Let's get a hash string
    assert(toHexString(hash) == "900150983CD24FB0D6963F7D28E17F72");
}

@system unittest
{
    import std.digest.md;

     //Let's use the OOP features:
    void test(Digest dig)
    {
      dig.put(cast(ubyte) 0);
    }
    auto md5 = new MD5Digest();
    test(md5);

    //Let's use a custom buffer:
    ubyte[16] buf;
    ubyte[] result = md5.finish(buf[]);
    assert(toHexString(result) == "93B885ADFE0DA089CDF634904FD59F71");
}


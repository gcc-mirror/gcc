@safe unittest
{
    import std.digest.hmac;

    import std.ascii : LetterCase;
    import std.digest : toHexString;
    import std.digest.sha : SHA1;
    import std.string : representation;

    auto secret = "secret".representation;
    assert("The quick brown fox jumps over the lazy dog"
            .representation
            .hmac!SHA1(secret)
            .toHexString!(LetterCase.lower) == "198ea1ea04c435c1246b586a06d5cf11c3ffcda6");
}

@safe pure nothrow @nogc unittest
{
    import std.digest.hmac;

        import std.digest.sha : SHA1;
        import std.string : representation;
        auto hmac = HMAC!SHA1("My s3cR3T keY".representation);
        hmac.put("Hello, world".representation);
        static immutable expected = [
            130, 32, 235, 44, 208, 141,
            150, 232, 211, 214, 162, 195,
            188, 127, 52, 89, 100, 68, 90, 216];
        assert(hmac.finish() == expected);
    
}

@safe pure nothrow @nogc unittest
{
    import std.digest.hmac;

        import std.digest.sha : SHA1;
        import std.string : representation;
        string data1 = "Hello, world", data2 = "Hola mundo";
        auto hmac = HMAC!SHA1("My s3cR3T keY".representation);
        hmac.put(data1.representation);
        hmac.start();                   // reset digest
        hmac.put(data2.representation); // start over
        static immutable expected = [
            122, 151, 232, 240, 249, 80,
            19, 178, 186, 77, 110, 23, 208,
            52, 11, 88, 34, 151, 192, 255];
        assert(hmac.finish() == expected);
    
}

@safe pure nothrow @nogc unittest
{
    import std.digest.hmac;

        import std.digest.hmac, std.digest.sha;
        import std.string : representation;
        string data1 = "Hello, world", data2 = "Hola mundo";
        auto hmac = HMAC!SHA1("My s3cR3T keY".representation);
        hmac.put(data1.representation)
            .put(data2.representation);
        static immutable expected = [
            197, 57, 52, 3, 13, 194, 13,
            36, 117, 228, 8, 11, 111, 51,
            165, 3, 123, 31, 251, 113];
        assert(hmac.finish() == expected);
    
}

@safe pure nothrow @nogc unittest
{
    import std.digest.hmac;

        import std.digest.sha : SHA1;
        import std.string : representation;
        string data1 = "Hello, world", data2 = "Hola mundo";
        auto hmac = HMAC!SHA1("My s3cR3T keY".representation);
        auto testDigest = hmac.put(data1.representation)
                          .put(data2.representation)
                          .finish();
        static immutable expected = [
            197, 57, 52, 3, 13, 194, 13,
            36, 117, 228, 8, 11, 111, 51,
            165, 3, 123, 31, 251, 113];
        assert(testDigest == expected);
    
}

@safe pure nothrow @nogc unittest
{
    import std.digest.hmac;

        import std.digest.sha : SHA1;
        import std.string : representation;
        string data1 = "Hello, world", data2 = "Hola mundo";
        auto digest = hmac!SHA1("My s3cR3T keY".representation)
                          .put(data1.representation)
                          .put(data2.representation)
                          .finish();
        static immutable expected = [
            197, 57, 52, 3, 13, 194, 13, 36,
            117, 228, 8, 11, 111, 51, 165,
            3, 123, 31, 251, 113];
        assert(digest == expected);
    
}

@safe pure nothrow @nogc unittest
{
    import std.digest.hmac;

        import std.algorithm.iteration : map;
        import std.digest.sha : SHA1;
        import std.string : representation;
        string data = "Hello, world";
        auto digest = data.representation
                      .map!(a => cast(ubyte)(a+1))
                      .hmac!SHA1("My s3cR3T keY".representation);
        static assert(is(typeof(digest) == ubyte[20]));
        static immutable expected = [
            163, 208, 118, 179, 216, 93,
            17, 10, 84, 200, 87, 104, 244,
            111, 136, 214, 167, 210, 58, 10];
        assert(digest == expected);
    
}

@safe pure nothrow @nogc unittest
{
    import std.digest.hmac;

    import std.digest.sha : SHA1;
    import std.string : representation;
    string data1 = "Hello, world", data2 = "Hola mundo";
    auto hmac = HMAC!SHA1("My s3cR3T keY".representation);
    auto digest = hmac.put(data1.representation)
                      .put(data2.representation)
                      .finish();
    static immutable expected = [
        197, 57, 52, 3, 13, 194, 13,
        36, 117, 228, 8, 11, 111, 51,
        165, 3, 123, 31, 251, 113];
    assert(digest == expected);
}


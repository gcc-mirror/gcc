// Written in the D programming language.

/**
This package implements the hash-based message authentication code (_HMAC)
algorithm as defined in $(HTTP tools.ietf.org/html/rfc2104, RFC2104). See also
the corresponding $(HTTP en.wikipedia.org/wiki/Hash-based_message_authentication_code, Wikipedia article).

$(SCRIPT inhibitQuickIndex = 1;)

Macros:

License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Source: $(PHOBOSSRC std/digest/hmac.d)
 */

module std.digest.hmac;

import std.digest : isDigest, hasBlockSize, isDigestibleRange, DigestType;
import std.meta : allSatisfy;

@safe:

/**
 * Template API HMAC implementation.
 *
 * This implements an _HMAC over the digest H. If H doesn't provide
 * information about the block size, it can be supplied explicitly using
 * the second overload.
 *
 * This type conforms to $(REF isDigest, std,digest).
 */

/// Compute HMAC over an input string
@safe unittest
{
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

template HMAC(H)
if (isDigest!H && hasBlockSize!H)
{
    alias HMAC = HMAC!(H, H.blockSize);
}

/**
 * Overload of HMAC to be used if H doesn't provide information about its
 * block size.
 */

struct HMAC(H, size_t hashBlockSize)
if (hashBlockSize % 8 == 0)
{
    enum blockSize = hashBlockSize;

    private H digest;
    private ubyte[blockSize / 8] key;

    /**
     * Constructs the HMAC digest using the specified secret.
     */

    this(scope const(ubyte)[] secret)
    {
        // if secret is too long, shorten it by computing its hash
        typeof(digest.finish()) buffer = void;
        typeof(secret) secretBytes = secret;

        if (secret.length > blockSize / 8)
        {
            digest.start();
            digest.put(secret);
            buffer = digest.finish();
            secretBytes = buffer[];
        }

        // if secret is too short, it will be padded with zeroes
        // (the key buffer is already zero-initialized)
        import std.algorithm.mutation : copy;
        secretBytes.copy(key[]);

        start();
    }

    ///
    @safe pure nothrow @nogc unittest
    {
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

    /**
     * Reinitializes the digest, making it ready for reuse.
     *
     * Note:
     * The constructor leaves the digest in an initialized state, so that this
     * method only needs to be called if an unfinished digest is to be reused.
     *
     * Returns:
     * A reference to the digest for convenient chaining.
     */

    ref HMAC!(H, blockSize) start() return
    {
        ubyte[blockSize / 8] ipad = void;
        foreach (immutable i; 0 .. blockSize / 8)
            ipad[i] = key[i] ^ 0x36;

        digest.start();
        digest.put(ipad[]);

        return this;
    }

    ///
    @safe pure nothrow @nogc unittest
    {
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

    /**
     * Feeds a piece of data into the hash computation. This method allows the
     * type to be used as an $(REF OutputRange, std,range).
     *
     * Returns:
     * A reference to the digest for convenient chaining.
     */

    ref HMAC!(H, blockSize) put(in ubyte[] data...) return
    {
        digest.put(data);
        return this;
    }

    ///
    @safe pure nothrow @nogc unittest
    {
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

    /**
     * Resets the digest and returns the finished hash.
     */

    DigestType!H finish()
    {
        ubyte[blockSize / 8] opad = void;
        foreach (immutable i; 0 .. blockSize / 8)
            opad[i] = key[i] ^ 0x5c;

        auto tmp = digest.finish();

        digest.start();
        digest.put(opad[]);
        digest.put(tmp);
        auto result = digest.finish();
        start();    // reset the digest
        return result;
    }

    ///
    @safe pure nothrow @nogc unittest
    {
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
}

/// ditto
template hmac(H)
if (isDigest!H && hasBlockSize!H)
{
    alias hmac = hmac!(H, H.blockSize);
}

/// ditto
template hmac(H, size_t blockSize)
if (isDigest!H)
{
    /**
     * Constructs an HMAC digest with the specified secret.
     *
     * Returns:
     * An instance of HMAC that can be fed data as desired, and finished
     * to compute the final hash when done.
     */
    auto hmac(scope const(ubyte)[] secret)
    {
        return HMAC!(H, blockSize)(secret);
    }

    ///
    @safe pure nothrow @nogc unittest
    {
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

    /**
     * Computes an _HMAC digest over the given range of data with the
     * specified secret.
     *
     * Returns:
     * The final _HMAC hash.
     */
    DigestType!H hmac(T...)(scope T data, scope const(ubyte)[] secret)
    if (allSatisfy!(isDigestibleRange, typeof(data)))
    {
        import std.range.primitives : put;
        auto hash = HMAC!(H, blockSize)(secret);
        foreach (datum; data)
            put(hash, datum);
        return hash.finish();
    }

    ///
    @safe pure nothrow @nogc unittest
    {
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
}

///
@safe pure nothrow @nogc unittest
{
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

@safe pure nothrow @nogc
unittest
{
    import std.digest.md : MD5;
    import std.range : isOutputRange;
    static assert(isOutputRange!(HMAC!MD5, ubyte));
    static assert(isDigest!(HMAC!MD5));
    static assert(hasBlockSize!(HMAC!MD5) && HMAC!MD5.blockSize == MD5.blockSize);
}

@safe pure nothrow
unittest
{
    import std.digest.md : MD5;
    import std.digest.sha : SHA1, SHA256;

    // Note, can't be UFCS because we don't want to import inside
    // version (StdUnittest).
    import std.digest : toHexString, LetterCase;
    alias hex = toHexString!(LetterCase.lower);

    ubyte[] nada;
    assert(hex(hmac!MD5   (nada, nada)) == "74e6f7298a9c2d168935f58c001bad88");
    assert(hex(hmac!SHA1  (nada, nada)) == "fbdb1d1b18aa6c08324b7d64b71fb76370690e1d");
    assert(hex(hmac!SHA256(nada, nada)) == "b613679a0814d9ec772f95d778c35fc5ff1697c493715653c6c712144292c5ad");

    import std.string : representation;
    auto key      = "key".representation,
         long_key = ("012345678901234567890123456789012345678901"
            ~"234567890123456789012345678901234567890123456789").representation,
         data1    = "The quick brown fox ".representation,
         data2    = "jumps over the lazy dog".representation,
         data     = data1 ~ data2;

    assert(hex(data.hmac!MD5   (key)) == "80070713463e7749b90c2dc24911e275");
    assert(hex(data.hmac!SHA1  (key)) == "de7c9b85b8b78aa6bc8a7a36f70a90701c9db4d9");
    assert(hex(data.hmac!SHA256(key)) == "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8");

    assert(hex(data.hmac!MD5   (long_key)) == "e1728d68e05beae186ea768561963778");
    assert(hex(data.hmac!SHA1  (long_key)) == "560d3cd77316e57ab4bba0c186966200d2b37ba3");
    assert(hex(data.hmac!SHA256(long_key)) == "a1b0065a5d1edd93152c677e1bc1b1e3bc70d3a76619842e7f733f02b8135c04");

    assert(hmac!MD5   (key).put(data1).put(data2).finish == data.hmac!MD5   (key));
    assert(hmac!SHA1  (key).put(data1).put(data2).finish == data.hmac!SHA1  (key));
    assert(hmac!SHA256(key).put(data1).put(data2).finish == data.hmac!SHA256(key));
}

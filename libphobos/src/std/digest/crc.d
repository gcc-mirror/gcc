/**
Cyclic Redundancy Check (32-bit) implementation.

$(SCRIPT inhibitQuickIndex = 1;)

$(DIVC quickindex,
$(BOOKTABLE ,
$(TR $(TH Category) $(TH Functions)
)
$(TR $(TDNW Template API) $(TD $(MYREF CRC) $(MYREF CRC32) $(MYREF CRC64ECMA) $(MYREF CRC64ISO)
)
)
$(TR $(TDNW OOP API) $(TD $(MYREF CRC32Digest) $(MYREF CRC64ECMADigest) $(MYREF CRC64ISODigest))
)
$(TR $(TDNW Helpers) $(TD $(MYREF crcHexString) $(MYREF crc32Of) $(MYREF crc64ECMAOf) $(MYREF crc64ISOOf))
)
)
)

 *
 * This module conforms to the APIs defined in `std.digest`. To understand the
 * differences between the template and the OOP API, see $(MREF std, digest).
 *
 * This module publicly imports $(MREF std, digest) and can be used as a stand-alone
 * module.
 *
 * Note:
 * CRCs are usually printed with the MSB first. When using
 * $(REF toHexString, std,digest) the result will be in an unexpected
 * order. Use $(REF toHexString, std,digest)'s optional order parameter
 * to specify decreasing order for the correct result. The $(LREF crcHexString)
 * alias can also be used for this purpose.
 *
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 *
 * Authors:   Pavel "EvilOne" Minayev, Alex Rønne Petersen, Johannes Pfau
 *
 * References:
 *      $(LINK2 http://en.wikipedia.org/wiki/Cyclic_redundancy_check, Wikipedia on CRC)
 *
 * Source: $(PHOBOSSRC std/digest/crc.d)
 *
 * Standards:
 * Implements the 'common' IEEE CRC32 variant
 * (LSB-first order, Initial value uint.max, complement result)
 *
 * CTFE:
 * Digests do not work in CTFE
 */
/*
 * Copyright (c) 2001 - 2002
 * Pavel "EvilOne" Minayev
 * Copyright (c) 2012
 * Alex Rønne Petersen
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module std.digest.crc;

public import std.digest;

///
@safe unittest
{
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

///
@safe unittest
{
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

private T[256][8] genTables(T)(T polynomial)
{
    T[256][8] res = void;

    foreach (i; 0 .. 0x100)
    {
        T crc = i;
        foreach (_; 0 .. 8)
            crc = (crc >> 1) ^ (-int(crc & 1) & polynomial);
        res[0][i] = crc;
    }

    foreach (i; 0 .. 0x100)
    {
        res[1][i] = (res[0][i] >> 8) ^ res[0][res[0][i] & 0xFF];
        res[2][i] = (res[1][i] >> 8) ^ res[0][res[1][i] & 0xFF];
        res[3][i] = (res[2][i] >> 8) ^ res[0][res[2][i] & 0xFF];
        res[4][i] = (res[3][i] >> 8) ^ res[0][res[3][i] & 0xFF];
        res[5][i] = (res[4][i] >> 8) ^ res[0][res[4][i] & 0xFF];
        res[6][i] = (res[5][i] >> 8) ^ res[0][res[5][i] & 0xFF];
        res[7][i] = (res[6][i] >> 8) ^ res[0][res[6][i] & 0xFF];
    }
    return res;
}

@system unittest
{
    auto tables = genTables(0xEDB88320);
    assert(tables[0][0] == 0x00000000 && tables[0][$ - 1] == 0x2d02ef8d && tables[7][$ - 1] == 0x264b06e6);
}

/**
 * Template API CRC32 implementation.
 * See `std.digest` for differences between template and OOP API.
 */
alias CRC32 = CRC!(32, 0xEDB88320);

/**
 * Template API CRC64-ECMA implementation.
 * See `std.digest` for differences between template and OOP API.
 */
alias CRC64ECMA = CRC!(64, 0xC96C5795D7870F42);

/**
 * Template API CRC64-ISO implementation.
 * See `std.digest` for differences between template and OOP API.
 */
alias CRC64ISO = CRC!(64, 0xD800000000000000);

/**
 * Generic Template API used for CRC32 and CRC64 implementations.
 *
 * The N parameter indicate the size of the hash in bits.
 * The parameter P specify the polynomial to be used for reduction.
 *
 * You may want to use the CRC32, CRC65ECMA and CRC64ISO aliases
 * for convenience.
 *
 * See `std.digest` for differences between template and OOP API.
 */
struct CRC(uint N, ulong P)
if (N == 32 || N == 64)
{
    private:
        static if (N == 32)
        {
            alias T = uint;
        }
        else
        {
            alias T = ulong;
        }

        static immutable T[256][8] tables = genTables!T(P);

        /**
         * Type of the finished CRC hash.
         * ubyte[4] if N is 32, ubyte[8] if N is 64.
         */
        alias R = ubyte[T.sizeof];

        // magic initialization constants
        T _state = T.max;

    public:
        /**
         * Use this to feed the digest with data.
         * Also implements the $(REF isOutputRange, std,range,primitives)
         * interface for `ubyte` and `const(ubyte)[]`.
         */
        void put(scope const(ubyte)[] data...) @trusted pure nothrow @nogc
        {
            T crc = _state;
            // process eight bytes at once
            while (data.length >= 8)
            {
                // Use byte-wise reads to support architectures without HW support
                // for unaligned reads. This can be optimized by compilers to a single
                // 32-bit read if unaligned reads are supported.
                // DMD is not able to do this optimization though, so explicitly
                // do unaligned reads for DMD's architectures.
                version (X86)
                    enum hasLittleEndianUnalignedReads = true;
                else version (X86_64)
                    enum hasLittleEndianUnalignedReads = true;
                else
                    enum hasLittleEndianUnalignedReads = false; // leave decision to optimizer

                uint one = void;
                uint two = void;

                if (!__ctfe && hasLittleEndianUnalignedReads)
                {
                    one = (cast(uint*) data.ptr)[0];
                    two = (cast(uint*) data.ptr)[1];
                }
                else
                {
                    one = (data.ptr[3] << 24 | data.ptr[2] << 16 | data.ptr[1] << 8 | data.ptr[0]);
                    two = (data.ptr[7] << 24 | data.ptr[6] << 16 | data.ptr[5] << 8 | data.ptr[4]);
                }

                static if (N == 32)
                {
                    one ^= crc;
                }
                else
                {
                    one ^= (crc & 0xffffffff);
                    two ^= (crc >> 32);
                }

                crc =
                    tables[0][two >> 24] ^
                    tables[1][(two >> 16) & 0xFF] ^
                    tables[2][(two >>  8) & 0xFF] ^
                    tables[3][two & 0xFF] ^
                    tables[4][one >> 24] ^
                    tables[5][(one >> 16) & 0xFF] ^
                    tables[6][(one >>  8) & 0xFF] ^
                    tables[7][one & 0xFF];

                data = data[8 .. $];
            }
            // remaining 1 to 7 bytes
            foreach (d; data)
                crc = (crc >> 8) ^ tables[0][(crc & 0xFF) ^ d];
            _state = crc;
        }

        /**
         * Used to initialize the CRC32 digest.
         *
         * Note:
         * For this CRC32 Digest implementation calling start after default construction
         * is not necessary. Calling start is only necessary to reset the Digest.
         *
         * Generic code which deals with different Digest types should always call start though.
         */
        void start() @safe pure nothrow @nogc
        {
            this = CRC.init;
        }

        /**
         * Returns the finished CRC hash. This also calls $(LREF start) to
         * reset the internal state.
         */
        R finish() @safe pure nothrow @nogc
        {
            auto tmp = peek();
            start();
            return tmp;
        }

        /**
         * Works like `finish` but does not reset the internal state, so it's possible
         * to continue putting data into this CRC after a call to peek.
         */
        R peek() const @safe pure nothrow @nogc
        {
            import std.bitmanip : nativeToLittleEndian;
            //Complement, LSB first / Little Endian, see http://rosettacode.org/wiki/CRC-32
            return nativeToLittleEndian(~_state);
        }
}

@safe unittest
{
    // https://issues.dlang.org/show_bug.cgi?id=13471
    static ubyte[4] foo(string str)
    {
        ubyte[4] result = str.crc32Of();
        if (result == (ubyte[4]).init)
            throw new Exception("this should not be thrown");
        return result;
    }
    enum buggy1 = foo("Hello World!");
    enum buggy2 = crc32Of("Hello World!");
    assert(buggy1 == buggy2);
    assert(buggy1 == "Hello World!".crc32Of());
}

///
@safe unittest
{
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

///
@safe unittest
{
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

///
@safe unittest
{
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

@safe unittest
{
    assert(isDigest!CRC32);
    assert(isDigest!CRC64ECMA);
    assert(isDigest!CRC64ISO);
}

@system unittest
{
    import std.conv : hexString;
    ubyte[4] digest;

    CRC32 crc;
    crc.put(cast(ubyte[])"abcdefghijklmnopqrstuvwxyz");
    assert(crc.peek() == cast(ubyte[]) hexString!"bd50274c");
    crc.start();
    crc.put(cast(ubyte[])"");
    assert(crc.finish() == cast(ubyte[]) hexString!"00000000");

    digest = crc32Of("");
    assert(digest == cast(ubyte[]) hexString!"00000000");

    //Test vector from http://rosettacode.org/wiki/CRC-32
    assert(crcHexString(crc32Of("The quick brown fox jumps over the lazy dog")) == "414FA339");

    digest = crc32Of("a");
    assert(digest == cast(ubyte[]) hexString!"43beb7e8");

    digest = crc32Of("abc");
    assert(digest == cast(ubyte[]) hexString!"c2412435");

    digest = crc32Of("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
    assert(digest == cast(ubyte[]) hexString!"5f3f1a17");

    digest = crc32Of("message digest");
    assert(digest == cast(ubyte[]) hexString!"7f9d1520");

    digest = crc32Of("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");
    assert(digest == cast(ubyte[]) hexString!"d2e6c21f");

    digest = crc32Of("1234567890123456789012345678901234567890"~
                    "1234567890123456789012345678901234567890");
    assert(digest == cast(ubyte[]) hexString!"724aa97c");

    enum ubyte[4] input = cast(ubyte[4]) hexString!"c3fcd3d7";
    assert(crcHexString(input) == "D7D3FCC3");
}

@system unittest
{
    import std.conv : hexString;
    ubyte[8] digest;

    CRC64ECMA crc;
    crc.put(cast(ubyte[])"abcdefghijklmnopqrstuvwxyz");
    assert(crc.peek() == cast(ubyte[]) hexString!"2f121b7575789626");
    crc.start();
    crc.put(cast(ubyte[])"");
    assert(crc.finish() == cast(ubyte[]) hexString!"0000000000000000");
    digest = crc64ECMAOf("");
    assert(digest == cast(ubyte[]) hexString!"0000000000000000");

    //Test vector from http://rosettacode.org/wiki/CRC-32
    assert(crcHexString(crc64ECMAOf("The quick brown fox jumps over the lazy dog")) == "5B5EB8C2E54AA1C4");

    digest = crc64ECMAOf("a");
    assert(digest == cast(ubyte[]) hexString!"052b652e77840233");

    digest = crc64ECMAOf("abc");
    assert(digest == cast(ubyte[]) hexString!"2776271a4a09d82c");

    digest = crc64ECMAOf("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
    assert(digest == cast(ubyte[]) hexString!"4b7cdce3746c449f");

    digest = crc64ECMAOf("message digest");
    assert(digest == cast(ubyte[]) hexString!"6f9b8a3156c9bc5d");

    digest = crc64ECMAOf("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");
    assert(digest == cast(ubyte[]) hexString!"2656b716e1bf0503");

    digest = crc64ECMAOf("1234567890123456789012345678901234567890"~
                         "1234567890123456789012345678901234567890");
    assert(digest == cast(ubyte[]) hexString!"bd3eb7765d0a22ae");

    enum ubyte[8] input = cast(ubyte[8]) hexString!"c3fcd3d7efbeadde";
    assert(crcHexString(input) == "DEADBEEFD7D3FCC3");
}

@system unittest
{
    import std.conv : hexString;
    ubyte[8] digest;

    CRC64ISO crc;
    crc.put(cast(ubyte[])"abcdefghijklmnopqrstuvwxyz");
    assert(crc.peek() == cast(ubyte[]) hexString!"f0494ab780989b42");
    crc.start();
    crc.put(cast(ubyte[])"");
    assert(crc.finish() == cast(ubyte[]) hexString!"0000000000000000");
    digest = crc64ISOOf("");
    assert(digest == cast(ubyte[]) hexString!"0000000000000000");

    //Test vector from http://rosettacode.org/wiki/CRC-32
    assert(crcHexString(crc64ISOOf("The quick brown fox jumps over the lazy dog")) == "4EF14E19F4C6E28E");

    digest = crc64ISOOf("a");
    assert(digest == cast(ubyte[]) hexString!"0000000000002034");

    digest = crc64ISOOf("abc");
    assert(digest == cast(ubyte[]) hexString!"0000000020c47637");

    digest = crc64ISOOf("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
    assert(digest == cast(ubyte[]) hexString!"5173f717971365e5");

    digest = crc64ISOOf("message digest");
    assert(digest == cast(ubyte[]) hexString!"a2c355bbc0b93f86");

    digest = crc64ISOOf("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");
    assert(digest == cast(ubyte[]) hexString!"598B258292E40084");

    digest = crc64ISOOf("1234567890123456789012345678901234567890"~
                        "1234567890123456789012345678901234567890");
    assert(digest == cast(ubyte[]) hexString!"760cd2d3588bf809");

    enum ubyte[8] input = cast(ubyte[8]) hexString!"c3fcd3d7efbeadde";
    assert(crcHexString(input) == "DEADBEEFD7D3FCC3");
}

/**
 * This is a convenience alias for $(REF digest, std,digest) using the
 * CRC32 implementation.
 *
 * Params:
 *      data = `InputRange` of `ElementType` implicitly convertible to
 *             `ubyte`, `ubyte[]` or `ubyte[num]` or one or more arrays
 *             of any type.
 *
 * Returns:
 *      CRC32 of data
 */
//simple alias doesn't work here, hope this gets inlined...
ubyte[4] crc32Of(T...)(T data)
{
    return digest!(CRC32, T)(data);
}

///
@system unittest
{
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

/**
 * This is a convenience alias for $(REF digest, std,digest) using the
 * CRC64-ECMA implementation.
 *
 * Params:
 *      data = `InputRange` of `ElementType` implicitly convertible to
 *             `ubyte`, `ubyte[]` or `ubyte[num]` or one or more arrays
 *             of any type.
 *
 * Returns:
 *      CRC64-ECMA of data
 */
//simple alias doesn't work here, hope this gets inlined...
ubyte[8] crc64ECMAOf(T...)(T data)
{
    return digest!(CRC64ECMA, T)(data);
}

///
@system unittest
{
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

/**
 * This is a convenience alias for $(REF digest, std,digest) using the
 * CRC64-ISO implementation.
 *
 * Params:
 *      data = `InputRange` of `ElementType` implicitly convertible to
 *             `ubyte`, `ubyte[]` or `ubyte[num]` or one or more arrays
 *             of any type.
 *
 * Returns:
 *      CRC64-ISO of data
 */
//simple alias doesn't work here, hope this gets inlined...
ubyte[8] crc64ISOOf(T...)(T data)
{
    return digest!(CRC64ISO, T)(data);
}

///
@system unittest
{
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

/**
 * producing the usual CRC32 string output.
 */
public alias crcHexString = toHexString!(Order.decreasing);
///ditto
public alias crcHexString = toHexString!(Order.decreasing, 16);

/**
 * OOP API CRC32 implementation.
 * See `std.digest` for differences between template and OOP API.
 *
 * This is an alias for $(D $(REF WrapperDigest, std,digest)!CRC32), see
 * there for more information.
 */
alias CRC32Digest = WrapperDigest!CRC32;

/**
 * OOP API CRC64-ECMA implementation.
 * See `std.digest` for differences between template and OOP API.
 *
 * This is an alias for $(D $(REF WrapperDigest, std,digest)!CRC64ECMA),
 * see there for more information.
 */
alias CRC64ECMADigest = WrapperDigest!CRC64ECMA;

/**
 * OOP API CRC64-ISO implementation.
 * See `std.digest` for differences between template and OOP API.
 *
 * This is an alias for $(D $(REF WrapperDigest, std,digest)!CRC64ISO),
 * see there for more information.
 */
alias CRC64ISODigest = WrapperDigest!CRC64ISO;

///
@safe unittest
{
    //Simple example, hashing a string using CRC32Digest.digest helper function
    auto crc = new CRC32Digest();
    ubyte[] hash = crc.digest("abc");
    //Let's get a hash string
    assert(crcHexString(hash) == "352441C2");
}

///
@system unittest
{
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

///
@safe unittest
{
    //Simple example
    auto hash = new CRC32Digest();
    hash.put(cast(ubyte) 0);
    ubyte[] result = hash.finish();
}

///
@system unittest
{
    //using a supplied buffer
    ubyte[4] buf;
    auto hash = new CRC32Digest();
    hash.put(cast(ubyte) 0);
    ubyte[] result = hash.finish(buf[]);
    //The result is now in result (and in buf. If you pass a buffer which is bigger than
    //necessary, result will have the correct length, but buf will still have it's original
    //length)
}

@system unittest
{
    import std.conv : hexString;
    import std.range;
    import std.exception;

    auto crc = new CRC32Digest();

    crc.put(cast(ubyte[])"abcdefghijklmnopqrstuvwxyz");
    assert(crc.peek() == cast(ubyte[]) hexString!"bd50274c");
    crc.reset();
    crc.put(cast(ubyte[])"");
    assert(crc.finish() == cast(ubyte[]) hexString!"00000000");

    crc.put(cast(ubyte[])"abcdefghijklmnopqrstuvwxyz");
    ubyte[20] result;
    auto result2 = crc.finish(result[]);
    assert(result[0 .. 4] == result2 && result2 == cast(ubyte[]) hexString!"bd50274c");

    debug
        assertThrown!Error(crc.finish(result[0 .. 3]));

    assert(crc.length == 4);

    assert(crc.digest("") == cast(ubyte[]) hexString!"00000000");

    assert(crc.digest("a") == cast(ubyte[]) hexString!"43beb7e8");

    assert(crc.digest("abc") == cast(ubyte[]) hexString!"c2412435");

    assert(crc.digest("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
           == cast(ubyte[]) hexString!"5f3f1a17");

    assert(crc.digest("message digest") == cast(ubyte[]) hexString!"7f9d1520");

    assert(crc.digest("abcdefghijklmnopqrstuvwxyz")
           == cast(ubyte[]) hexString!"bd50274c");

    assert(crc.digest("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
           == cast(ubyte[]) hexString!"d2e6c21f");

    assert(crc.digest("1234567890123456789012345678901234567890",
                                   "1234567890123456789012345678901234567890")
           == cast(ubyte[]) hexString!"724aa97c");

    ubyte[] onemilliona = new ubyte[1000000];
    onemilliona[] = 'a';
    auto digest = crc32Of(onemilliona);
    assert(digest == cast(ubyte[]) hexString!"BCBF25DC");

    auto oneMillionRange = repeat!ubyte(cast(ubyte)'a', 1000000);
    digest = crc32Of(oneMillionRange);
    assert(digest == cast(ubyte[]) hexString!"BCBF25DC");
}

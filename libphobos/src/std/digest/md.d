/**
 * Computes MD5 hashes of arbitrary data. MD5 hashes are 16 byte quantities that are like a
 * checksum or CRC, but are more robust.
 *
$(SCRIPT inhibitQuickIndex = 1;)

$(DIVC quickindex,
$(BOOKTABLE ,
$(TR $(TH Category) $(TH Functions)
)
$(TR $(TDNW Template API) $(TD $(MYREF MD5)
)
)
$(TR $(TDNW OOP API) $(TD $(MYREF MD5Digest))
)
$(TR $(TDNW Helpers) $(TD $(MYREF md5Of))
)
)
)

 * This module conforms to the APIs defined in $(D std.digest). To understand the
 * differences between the template and the OOP API, see $(MREF std, digest).
 *
 * This module publicly imports $(MREF std, digest) and can be used as a stand-alone
 * module.
 *
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 *
 * CTFE:
 * Digests do not work in CTFE
 *
 * Authors:
 * Piotr Szturmaj, Kai Nacke, Johannes Pfau $(BR)
 * The routines and algorithms are derived from the $(I RSA Data Security, Inc. MD5 Message-Digest Algorithm).
 *
 * References:
 *      $(LINK2 http://en.wikipedia.org/wiki/Md5, Wikipedia on MD5)
 *
 * Source: $(PHOBOSSRC std/digest/_md.d)
 *
 */

/* md5.d - RSA Data Security, Inc., MD5 message-digest algorithm
 * Derived from the RSA Data Security, Inc. MD5 Message-Digest Algorithm.
 */
module std.digest.md;

public import std.digest;

///
@safe unittest
{
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

///
@safe unittest
{
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

//rotateLeft rotates x left n bits
private uint rotateLeft(uint x, uint n) @safe pure nothrow @nogc
{
    // With recently added optimization to DMD (commit 32ea0206 at 07/28/11), this is translated to rol.
    // No assembler required.
    return (x << n) | (x >> (32-n));
}

/**
 * Template API MD5 implementation.
 * See $(D std.digest) for differences between template and OOP API.
 */
struct MD5
{
    private:
        // magic initialization constants
        uint[4] _state = [0x67452301,0xefcdab89,0x98badcfe,0x10325476]; // state (ABCD)
        ulong _count; //number of bits, modulo 2^64
        ubyte[64] _buffer; // input buffer

        static immutable ubyte[64] _padding =
        [
          0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        ];

        // F, G, H and I are basic MD5 functions
        static @safe pure nothrow @nogc
        {
            uint F(uint x, uint y, uint z) { return (x & y) | (~x & z); }
            uint G(uint x, uint y, uint z) { return (x & z) | (y & ~z); }
            uint H(uint x, uint y, uint z) { return x ^ y ^ z; }
            uint I(uint x, uint y, uint z) { return y ^ (x | ~z); }
        }


        /*
         * FF, GG, HH, and II transformations for rounds 1, 2, 3, and 4.
         * Rotation is separate from addition to prevent recomputation.
         */
        static void FF(ref uint a, uint b, uint c, uint d, uint x, uint s, uint ac)
            @safe pure nothrow @nogc
        {
            a += F (b, c, d) + x + ac;
            a = rotateLeft(a, s);
            a += b;
        }

        static void GG(ref uint a, uint b, uint c, uint d, uint x, uint s, uint ac)
            @safe pure nothrow @nogc
        {
            a += G (b, c, d) + x + ac;
            a = rotateLeft(a, s);
            a += b;
        }

        static void HH(ref uint a, uint b, uint c, uint d, uint x, uint s, uint ac)
            @safe pure nothrow @nogc
        {
            a += H (b, c, d) + x + ac;
            a = rotateLeft(a, s);
            a += b;
        }

        static void II(ref uint a, uint b, uint c, uint d, uint x, uint s, uint ac)
            @safe pure nothrow @nogc
        {
            a += I (b, c, d) + x + ac;
            a = rotateLeft(a, s);
            a += b;
        }

        /*
         * MD5 basic transformation. Transforms state based on block.
         */

        //Constants for MD5Transform routine.
        enum
        {
            S11 = 7,
            S12 = 12,
            S13 = 17,
            S14 = 22,
            S21 = 5,
            S22 = 9,
            S23 = 14,
            S24 = 20,
            S31 = 4,
            S32 = 11,
            S33 = 16,
            S34 = 23,
            S41 = 6,
            S42 = 10,
            S43 = 15,
            S44 = 21,
        }

        private void transform(const(ubyte[64])* block) pure nothrow @nogc
        {
            uint a = _state[0],
                 b = _state[1],
                 c = _state[2],
                 d = _state[3];

            uint[16] x = void;

            version (BigEndian)
            {
                import std.bitmanip : littleEndianToNative;

                for (size_t i = 0; i < 16; i++)
                {
                    x[i] = littleEndianToNative!uint(*cast(ubyte[4]*)&(*block)[i*4]);
                }
            }
            else
            {
                (cast(ubyte*) x.ptr)[0 .. 64] = (cast(ubyte*) block)[0 .. 64];
            }

            //Round 1
            FF (a, b, c, d, x[ 0], S11, 0xd76aa478); /* 1 */
            FF (d, a, b, c, x[ 1], S12, 0xe8c7b756); /* 2 */
            FF (c, d, a, b, x[ 2], S13, 0x242070db); /* 3 */
            FF (b, c, d, a, x[ 3], S14, 0xc1bdceee); /* 4 */
            FF (a, b, c, d, x[ 4], S11, 0xf57c0faf); /* 5 */
            FF (d, a, b, c, x[ 5], S12, 0x4787c62a); /* 6 */
            FF (c, d, a, b, x[ 6], S13, 0xa8304613); /* 7 */
            FF (b, c, d, a, x[ 7], S14, 0xfd469501); /* 8 */
            FF (a, b, c, d, x[ 8], S11, 0x698098d8); /* 9 */
            FF (d, a, b, c, x[ 9], S12, 0x8b44f7af); /* 10 */
            FF (c, d, a, b, x[10], S13, 0xffff5bb1); /* 11 */
            FF (b, c, d, a, x[11], S14, 0x895cd7be); /* 12 */
            FF (a, b, c, d, x[12], S11, 0x6b901122); /* 13 */
            FF (d, a, b, c, x[13], S12, 0xfd987193); /* 14 */
            FF (c, d, a, b, x[14], S13, 0xa679438e); /* 15 */
            FF (b, c, d, a, x[15], S14, 0x49b40821); /* 16 */

            //Round 2
            GG (a, b, c, d, x[ 1], S21, 0xf61e2562); /* 17 */
            GG (d, a, b, c, x[ 6], S22, 0xc040b340); /* 18 */
            GG (c, d, a, b, x[11], S23, 0x265e5a51); /* 19 */
            GG (b, c, d, a, x[ 0], S24, 0xe9b6c7aa); /* 20 */
            GG (a, b, c, d, x[ 5], S21, 0xd62f105d); /* 21 */
            GG (d, a, b, c, x[10], S22,  0x2441453); /* 22 */
            GG (c, d, a, b, x[15], S23, 0xd8a1e681); /* 23 */
            GG (b, c, d, a, x[ 4], S24, 0xe7d3fbc8); /* 24 */
            GG (a, b, c, d, x[ 9], S21, 0x21e1cde6); /* 25 */
            GG (d, a, b, c, x[14], S22, 0xc33707d6); /* 26 */
            GG (c, d, a, b, x[ 3], S23, 0xf4d50d87); /* 27 */
            GG (b, c, d, a, x[ 8], S24, 0x455a14ed); /* 28 */
            GG (a, b, c, d, x[13], S21, 0xa9e3e905); /* 29 */
            GG (d, a, b, c, x[ 2], S22, 0xfcefa3f8); /* 30 */
            GG (c, d, a, b, x[ 7], S23, 0x676f02d9); /* 31 */
            GG (b, c, d, a, x[12], S24, 0x8d2a4c8a); /* 32 */

            //Round 3
            HH (a, b, c, d, x[ 5], S31, 0xfffa3942); /* 33 */
            HH (d, a, b, c, x[ 8], S32, 0x8771f681); /* 34 */
            HH (c, d, a, b, x[11], S33, 0x6d9d6122); /* 35 */
            HH (b, c, d, a, x[14], S34, 0xfde5380c); /* 36 */
            HH (a, b, c, d, x[ 1], S31, 0xa4beea44); /* 37 */
            HH (d, a, b, c, x[ 4], S32, 0x4bdecfa9); /* 38 */
            HH (c, d, a, b, x[ 7], S33, 0xf6bb4b60); /* 39 */
            HH (b, c, d, a, x[10], S34, 0xbebfbc70); /* 40 */
            HH (a, b, c, d, x[13], S31, 0x289b7ec6); /* 41 */
            HH (d, a, b, c, x[ 0], S32, 0xeaa127fa); /* 42 */
            HH (c, d, a, b, x[ 3], S33, 0xd4ef3085); /* 43 */
            HH (b, c, d, a, x[ 6], S34,  0x4881d05); /* 44 */
            HH (a, b, c, d, x[ 9], S31, 0xd9d4d039); /* 45 */
            HH (d, a, b, c, x[12], S32, 0xe6db99e5); /* 46 */
            HH (c, d, a, b, x[15], S33, 0x1fa27cf8); /* 47 */
            HH (b, c, d, a, x[ 2], S34, 0xc4ac5665); /* 48 */

            //Round 4
            II (a, b, c, d, x[ 0], S41, 0xf4292244); /* 49 */
            II (d, a, b, c, x[ 7], S42, 0x432aff97); /* 50 */
            II (c, d, a, b, x[14], S43, 0xab9423a7); /* 51 */
            II (b, c, d, a, x[ 5], S44, 0xfc93a039); /* 52 */
            II (a, b, c, d, x[12], S41, 0x655b59c3); /* 53 */
            II (d, a, b, c, x[ 3], S42, 0x8f0ccc92); /* 54 */
            II (c, d, a, b, x[10], S43, 0xffeff47d); /* 55 */
            II (b, c, d, a, x[ 1], S44, 0x85845dd1); /* 56 */
            II (a, b, c, d, x[ 8], S41, 0x6fa87e4f); /* 57 */
            II (d, a, b, c, x[15], S42, 0xfe2ce6e0); /* 58 */
            II (c, d, a, b, x[ 6], S43, 0xa3014314); /* 59 */
            II (b, c, d, a, x[13], S44, 0x4e0811a1); /* 60 */
            II (a, b, c, d, x[ 4], S41, 0xf7537e82); /* 61 */
            II (d, a, b, c, x[11], S42, 0xbd3af235); /* 62 */
            II (c, d, a, b, x[ 2], S43, 0x2ad7d2bb); /* 63 */
            II (b, c, d, a, x[ 9], S44, 0xeb86d391); /* 64 */

            _state[0] += a;
            _state[1] += b;
            _state[2] += c;
            _state[3] += d;

            //Zeroize sensitive information.
            x[] = 0;
        }

    public:
        enum blockSize = 512;

        /**
         * Use this to feed the digest with data.
         * Also implements the $(REF isOutputRange, std,range,primitives)
         * interface for $(D ubyte) and $(D const(ubyte)[]).
         *
         * Example:
         * ----
         * MD5 dig;
         * dig.put(cast(ubyte) 0); //single ubyte
         * dig.put(cast(ubyte) 0, cast(ubyte) 0); //variadic
         * ubyte[10] buf;
         * dig.put(buf); //buffer
         * ----
         */
        void put(scope const(ubyte)[] data...) @trusted pure nothrow @nogc
        {
            uint i, index, partLen;
            auto inputLen = data.length;

            //Compute number of bytes mod 64
            index = (cast(uint)_count >> 3) & (64 - 1);

            //Update number of bits
            _count += inputLen * 8;

            partLen = 64 - index;

            //Transform as many times as possible
            if (inputLen >= partLen)
            {
                (&_buffer[index])[0 .. partLen] = data.ptr[0 .. partLen];
                transform(&_buffer);

                for (i = partLen; i + 63 < inputLen; i += 64)
                {
                    transform(cast(const(ubyte[64])*)(data[i .. i + 64].ptr));
                }

                index = 0;
            }
            else
            {
                i = 0;
            }

            /* Buffer remaining input */
            if (inputLen - i)
                (&_buffer[index])[0 .. inputLen-i] = (&data[i])[0 .. inputLen-i];
        }

        /**
         * Used to (re)initialize the MD5 digest.
         *
         * Note:
         * For this MD5 Digest implementation calling start after default construction
         * is not necessary. Calling start is only necessary to reset the Digest.
         *
         * Generic code which deals with different Digest types should always call start though.
         *
         * Example:
         * --------
         * MD5 digest;
         * //digest.start(); //Not necessary
         * digest.put(0);
         * --------
         */
        void start() @safe pure nothrow @nogc
        {
            this = MD5.init;
        }

        /**
         * Returns the finished MD5 hash. This also calls $(LREF start) to
         * reset the internal state.
          */
        ubyte[16] finish() @trusted pure nothrow @nogc
        {
            import std.bitmanip : nativeToLittleEndian;

            ubyte[16] data = void;
            ubyte[8] bits = void;
            uint index, padLen;

            //Save number of bits
            bits[0 .. 8] = nativeToLittleEndian(_count)[];

            //Pad out to 56 mod 64
            index = (cast(uint)_count >> 3) & (64 - 1);
            padLen = (index < 56) ? (56 - index) : (120 - index);
            put(_padding[0 .. padLen]);

            //Append length (before padding)
            put(bits);

            //Store state in digest
            data[0 .. 4]   = nativeToLittleEndian(_state[0])[];
            data[4 .. 8]   = nativeToLittleEndian(_state[1])[];
            data[8 .. 12]  = nativeToLittleEndian(_state[2])[];
            data[12 .. 16] = nativeToLittleEndian(_state[3])[];

            /* Zeroize sensitive information. */
            start();
            return data;
        }
        ///
        @safe unittest
        {
            //Simple example
            MD5 hash;
            hash.start();
            hash.put(cast(ubyte) 0);
            ubyte[16] result = hash.finish();
        }
}

///
@safe unittest
{
    //Simple example, hashing a string using md5Of helper function
    ubyte[16] hash = md5Of("abc");
    //Let's get a hash string
    assert(toHexString(hash) == "900150983CD24FB0D6963F7D28E17F72");
}

///
@safe unittest
{
    //Using the basic API
    MD5 hash;
    hash.start();
    ubyte[1024] data;
    //Initialize data here...
    hash.put(data);
    ubyte[16] result = hash.finish();
}

///
@safe unittest
{
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
    assert(isDigest!MD5);
}

@system unittest
{
    import std.range;

    ubyte[16] digest;

    MD5 md5;
    md5.put(cast(ubyte[])"abcdef");
    md5.start();
    md5.put(cast(ubyte[])"");
    assert(md5.finish() == cast(ubyte[]) x"d41d8cd98f00b204e9800998ecf8427e");

    digest = md5Of("");
    assert(digest == cast(ubyte[]) x"d41d8cd98f00b204e9800998ecf8427e");

    digest = md5Of("a");
    assert(digest == cast(ubyte[]) x"0cc175b9c0f1b6a831c399e269772661");

    digest = md5Of("abc");
    assert(digest == cast(ubyte[]) x"900150983cd24fb0d6963f7d28e17f72");

    digest = md5Of("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
    assert(digest == cast(ubyte[]) x"8215ef0796a20bcaaae116d3876c664a");

    digest = md5Of("message digest");
    assert(digest == cast(ubyte[]) x"f96b697d7cb7938d525a2f31aaf161d0");

    digest = md5Of("abcdefghijklmnopqrstuvwxyz");
    assert(digest == cast(ubyte[]) x"c3fcd3d76192e4007dfb496cca67e13b");

    digest = md5Of("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");
    assert(digest == cast(ubyte[]) x"d174ab98d277d9f5a5611c2c9f419d9f");

    digest = md5Of("1234567890123456789012345678901234567890"~
                    "1234567890123456789012345678901234567890");
    assert(digest == cast(ubyte[]) x"57edf4a22be3c955ac49da2e2107b67a");

    assert(toHexString(cast(ubyte[16]) x"c3fcd3d76192e4007dfb496cca67e13b")
        == "C3FCD3D76192E4007DFB496CCA67E13B");

    ubyte[] onemilliona = new ubyte[1000000];
    onemilliona[] = 'a';
    digest = md5Of(onemilliona);
    assert(digest == cast(ubyte[]) x"7707D6AE4E027C70EEA2A935C2296F21");

    auto oneMillionRange = repeat!ubyte(cast(ubyte)'a', 1000000);
    digest = md5Of(oneMillionRange);
    assert(digest == cast(ubyte[]) x"7707D6AE4E027C70EEA2A935C2296F21");
}

/**
 * This is a convenience alias for $(REF digest, std,digest) using the
 * MD5 implementation.
 */
//simple alias doesn't work here, hope this gets inlined...
auto md5Of(T...)(T data)
{
    return digest!(MD5, T)(data);
}

///
@safe unittest
{
    ubyte[16] hash = md5Of("abc");
    assert(hash == digest!MD5("abc"));
}

/**
 * OOP API MD5 implementation.
 * See $(D std.digest) for differences between template and OOP API.
 *
 * This is an alias for $(D $(REF WrapperDigest, std,digest)!MD5), see
 * there for more information.
 */
alias MD5Digest = WrapperDigest!MD5;

///
@safe unittest
{
    //Simple example, hashing a string using Digest.digest helper function
    auto md5 = new MD5Digest();
    ubyte[] hash = md5.digest("abc");
    //Let's get a hash string
    assert(toHexString(hash) == "900150983CD24FB0D6963F7D28E17F72");
}

///
@system unittest
{
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

@system unittest
{
    auto md5 = new MD5Digest();

    md5.put(cast(ubyte[])"abcdef");
    md5.reset();
    md5.put(cast(ubyte[])"");
    assert(md5.finish() == cast(ubyte[]) x"d41d8cd98f00b204e9800998ecf8427e");

    md5.put(cast(ubyte[])"abcdefghijklmnopqrstuvwxyz");
    ubyte[20] result;
    auto result2 = md5.finish(result[]);
    assert(result[0 .. 16] == result2 && result2 == cast(ubyte[]) x"c3fcd3d76192e4007dfb496cca67e13b");

    debug
    {
        import std.exception;
        assertThrown!Error(md5.finish(result[0 .. 15]));
    }

    assert(md5.length == 16);

    assert(md5.digest("") == cast(ubyte[]) x"d41d8cd98f00b204e9800998ecf8427e");

    assert(md5.digest("a") == cast(ubyte[]) x"0cc175b9c0f1b6a831c399e269772661");

    assert(md5.digest("abc") == cast(ubyte[]) x"900150983cd24fb0d6963f7d28e17f72");

    assert(md5.digest("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
           == cast(ubyte[]) x"8215ef0796a20bcaaae116d3876c664a");

    assert(md5.digest("message digest") == cast(ubyte[]) x"f96b697d7cb7938d525a2f31aaf161d0");

    assert(md5.digest("abcdefghijklmnopqrstuvwxyz")
           == cast(ubyte[]) x"c3fcd3d76192e4007dfb496cca67e13b");

    assert(md5.digest("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
           == cast(ubyte[]) x"d174ab98d277d9f5a5611c2c9f419d9f");

    assert(md5.digest("1234567890123456789012345678901234567890",
                                   "1234567890123456789012345678901234567890")
           == cast(ubyte[]) x"57edf4a22be3c955ac49da2e2107b67a");
}
